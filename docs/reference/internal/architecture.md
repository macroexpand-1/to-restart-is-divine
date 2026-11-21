# Architecture

## The Shape

My vision is a visitor opens a page and gets served a REPL backed by a real SBCL image, with nothing to install.
You type `(car 5)`, and instead of a wall of text you get the condition, a button per restart, and a backtrace.
You pick one and keep going.
That's the whole demo, and it shows how cool live image programming is.

```mermaid
flowchart TB
  browser["Browser<br/>thin JS client"]
  proxy["Reverse proxy<br/>TLS, websocket"]
  subgraph gateway["Gateway: one SBCL process"]
    ui["Web UI"]
    sessions["Session manager"]
    bridge["Debugger bridge"]
  end
  subgraph sandbox["Sandbox per session, no network"]
    image["Lesson image<br/>SBCL core"]
    agent["Debug agent"]
  end
  browser -->|websocket| proxy
  proxy --> gateway
  gateway -->|unix socket| sandbox
  class browser,proxy tone-gray
  class ui,sessions,bridge tone-blue
  class image,agent tone-amber
```

Gray is the non-Lisp edge, blue is the gateway, amber is the sandbox where visitor code runs.
The reverse proxy and the sandbox are the only parts that aren't Lisp, and I'd like to keep it that way.

**Reverse proxy.**
Terminates TLS and upgrades the websocket.
The Lisp web server never faces the internet directly.

**Gateway.**
One SBCL process with the web UI, the session manager and the debugger bridge in it.
It can use libraries (a few) and Quicklisp is fine for developing it.

**Sandbox.**
A disposable container (Linux) or jail (FreeBSD). 
Visitors get one per session and it is thrown away when the session ends.

**Lesson image.**
An SBCL core with ANSI Common Lisp, a debug agent, and one lesson's code in it.

**Debug agent.**
Zero-dependency code living in every lesson image.
It runs the REPL, ships output back, and replaces the terminal debugger with one that sends restarts to the gateway as data.
The key is that it's just transiting data.

## The Wall

There is just one security boundary: the sandbox wall.
Everything inside it belongs to the visitor, including the agent.
Visitor code runs in the same image, so it can redefine agent functions or write straight to the socket if it wants to.
I'm totally fine with that, because nothing outside the wall trusts the agent at all.

A form makes one round trip across the wall:

1. **Gateway:** the visitor's text arrives.
2. **Gateway:** it is wrapped as a string inside `:eval`.
3. **Sandbox:** the agent reads it and evaluates it.
4. **Sandbox:** the agent sends a reply message.
5. **Gateway:** a bounded read parses the reply, with `*read-eval*` off.
6. **Gateway:** the reply becomes UI state.

**Going in, visitor text is just a string.**
The gateway never calls `read` or `eval` on anything a visitor typed.
It puts the text in a string inside `:eval` and forwards it, and the only `read` that ever touches it happens inside the sandbox.

**Coming out, any message could be forged.**
Visitor code can write whatever the hell it likes to the socket, so the gateway reads agent messages the way it would read anything off the internet.
`*read-eval*` is bound to `nil` on every read (on both ends), but that alone doesn't cover it.
You can forge a message that can still intern a pile of symbols, nest a million parens deep, or claim a length of four gigabytes.
The gateway will cap the message length before it reads anything, and for now reads with only the keyword package current.
That still lets a forged `cl-user::x` intern into a real package, and every new keyword sticks around, which is why the reader isn't settled.
Whether I use `cl:read` under tight bindings or a tiny parser that only knows keywords, strings, integers and lists is open.
We lean toward the tiny parser, since the message shapes are small and fixed and then there's nothing to reason about.
Also, we just like parsers.

**No sandboxing inside Lisp.**
CL can't restrict code in a running image (`#.`, `sb-ext:run-program`, the FFI, just redefining `cl` functions), so I'm just not gonna pretend that we can.
No package locks, no symbol whitelists.
They'd only make it look safer than it is.
It's just straight up a little dangerous.

## A Session

```mermaid
stateDiagram-v2
  [*] --> Admission
  Admission --> Refused: over a cap
  Admission --> Starting: under caps
  Starting --> Live: agent ready
  Starting --> Reaped: never ready
  Live --> Reaped: idle, too old,<br/>or disconnect
  Refused --> [*]
  Reaped --> [*]
```

1. **Admission.**
   The websocket opens and asks for a lesson.
   The session manager checks the per-IP cap, the global cap, and the creation rate, and turns the session away if any of them is over.
2. **Starting.**
   It draws a random 128-bit session id, starts listening on the socket at `sandbox-socket-path`, then calls `start-sandbox`, and the sandbox runs `sbcl --core lesson-NAME.core`.
   The agent connects as soon as its image starts, the gateway accepts and waits for `(:ready)`, and the UI says "starting" until it shows up.
   If it never shows up, the session gets reaped like any other; we really don't think we need a separate failure path for that.
3. **Live.**
   The gateway relays input down and output, values and debugger state back up.
4. **Reaped.**
   Whichever comes first of the idle timeout, the max lifetime, or the browser going away.
   The session manager calls `stop-sandbox` and the whole thing is gone.

Reloading the page gets you a brand new sandbox from the same read-only core.
Nothing carries over, which is kind of the point of starting inside an image: you can always get the clean one back.

## Restarts

Here's what happens on `(car 5)`:

```mermaid
sequenceDiagram
  participant B as Browser
  participant G as Gateway
  participant A as Agent
  B->>G: types (car 5)
  G->>A: (:eval "(car 5)")
  Note over A: TYPE-ERROR,<br/>hook runs
  A->>G: (:level-opened :id 1 ...)
  G->>B: Debugger panel
  B->>G: Clicks a restart
  G->>A: (:invoke-restart 1 N)
  A->>G: (:level-closed 1)
  G->>B: Panel closes
```

The agent's debugger hook sends the condition type, its report, the `compute-restarts` list (index, name and report for each), and a backtrace.
Then it sits and waits for the gateway to say which one, and calls `invoke-restart` on it.
Roughly:

```lisp
;; sketch: NOT the real deal 
(defun agent-debugger-hook (condition old-hook)
  (declare (ignore old-hook))
  (let ((restarts (compute-restarts condition))
        (id (next-level-id)))
    (send-message
     `(:level-opened :id ,id
              :depth ,*debug-depth*
              :type ,(prin1-to-string (type-of condition))
              :report ,(princ-to-string condition)
              :restarts ,(loop for r in restarts
                               for i from 0
                               collect (list i
                                             (prin1-to-string (restart-name r))
                                             (princ-to-string r)))
              :backtrace ,(backtrace-strings)))   ; the SBCL-only bit
    ;; blocks this thread until the visitor picks something
    (debugger-loop id restarts)))
```

The agent also wraps every form it evaluates in a `retry` restart and every top-level `:eval` in the one `abort`. 
Even `(car 5)`, which should have no restarts of its own, gets two buttons instead of one.
`retry` is the one that matters: fix the function from inside the debugger, hit RETRY, and the form that failed runs again.
Lessons with a planted bug bring their own `restart-case` and those show up alongside.

**One SBCL thing to check.**
ANSI `break` binds `*debugger-hook*` to `nil`, so `(break)` would skip the hook and fall into the terminal debugger (which people can't see).
SBCL has `sb-ext:*invoke-debugger-hook*` for exactly this type of situation.
`invoke-debugger` consults it first and `break` leaves it alone which we verified against SBCL 2.6.9.

**Restarts that want a value.**
`use-value` and `store-value` need an argument, and the draft messages have nowhere to put one.
We think the panel asks for a form, sends it as a string, and the agent reads it inside the sandbox, so the gateway rule about never reading visitor text still holds.
I'm pretty open for how this plays out.

## Nested Levels

While a debugger level is open you can keep typing, same as SLIME.
That input gets evaluated inside the open level, so an error there opens level 2 on top of level 1.

```mermaid
sequenceDiagram
  participant G as Gateway
  participant A as Agent
  G->>A: (:eval "(foo)")
  A->>G: (:level-opened :id 1 :depth 1)
  G->>A: (:eval "(bar)")
  Note over A: bar errors too,<br/>inside level 1
  A->>G: (:level-opened :id 2 :depth 2)
  G->>A: ABORT on id 2
  A->>G: (:level-closed 2)
  A->>G: (:level-closed 1)
```

`ABORT` is always on the list and always goes back to the top-level REPL, and it closes each and every open level on the way out.
Other restarts just close the level that they belong to.
The bridge keeps the stack of open levels so the panel can show which level a button belongs to.

This is also why `debugger-loop` in the sketch (`serve-level` in `agent/agent.lisp`) is a loop and not a single wait.
The loop takes the next message, and if it's `:eval` it evaluates it right there (where another error recurses into the hook), and if it's `:invoke-restart` for its own id it invokes the restart and unwinds.
That's basically how SBCL's own debugger REPL works (more or less.)

## The Agent

```mermaid
flowchart TB
  sock["Agent socket"]
  reader["Reader thread"]
  inbox["Inbox queue"]
  evalt["Eval thread<br/>REPL and debugger"]
  outs["Output streams"]
  sock --> reader --> inbox --> evalt
  reader -.->|interrupt| evalt
  evalt --> outs --> sock
  class sock tone-gray
  class reader,inbox,evalt,outs tone-amber
```

Two additional threads.
`:interrupt` has to arrive while the eval thread is busy running a `(loop)`, so something other than the eval thread has to be reading the socket.

- **Reader thread.**
  Reads messages, answers `:ping` itself, and hands `:interrupt` to a short-lived thread that interrupts the eval thread (see Interrupting, below).
  Everything else goes into the inbox.
  When the socket closes, it ends the eval thread the same way an Interrupt lands, never unwinding it out of C.
  If the eval thread is still in visitor's foreign code after five seconds, the reader leaves it be; the agent's own process then exits, which ends the thread without unwinding anything.
  (In production the gateway kills the agent process outright, so this matters mostly for agents run inside a test image.)
- **Eval thread.**
  Pulls from the inbox, evaluates, sends `:values`.
  It also runs the debugger loop above, which pulls from the same inbox.
- **Output.**
  `*standard-output*` and `*error-output*` are bound to streams that send `:output` messages.
  Both threads write to the socket, so writes go through one mutex (unless we can figure out a mutexless setup).

The agent can't use a socket or JSON library (nothing beyond ANSI goes in the image), so it frames its own messages.
SBCL's socket library, `sb-bsd-sockets`, is a contrib and stays out too, so the agent calls `socket()` and `connect()` itself through `sb-alien`, which is in the base image.
Its inbox is likewise a plain list behind an `sb-thread` mutex rather than the `sb-concurrency` contrib.
Backtraces, frame inspection and thread interrupts aren't ANSI, so all of that lives in one SBCL-specific file and the rest stays portable.

**Interrupting.**
The interrupt lands in the debugger like any other condition, which we really like, because it means `(loop)` doesn't just die.
You get a panel with `continue`, and you can let it keep spinning if you want.
We mean, this is like the charm of Lisp.

The catch is that `sb-thread:interrupt-thread` runs wherever the eval thread happens to be, and that includes C.
SBCL calls into C a lot while compiling (zstd compresses debug info, and that calls malloc), so roughly one Interrupt in eight on a compile loop lands there.
Opening a debugger level in the middle of malloc, or unwinding out of it with ABORT, leaves malloc's lock held, and macOS kills the whole image.
So the interrupt first checks where it landed, with interrupts still off so a second one can't land on top:

- **Holding one of the agent's locks.**
  The Lisp half of `mailbox-receive` holds the mailbox's mutex (it's only let go during the wait itself).
  A level opened there would block the reader on its next `mailbox-send` and trip over the lock on the next `read-line`.
  Do nothing and try again; the retry lands in the wait, where the lock is free.
- **In Lisp.**
  SBCL pushed an interrupt context (`sb-kernel:*free-interrupt-context-index*` went up past what it was when the eval loop or the current level started), so it's safe: open the level.
- **In a safe wait.**
  No context was pushed, but the eval thread is blocked somewhere we marked as fine to break into: waiting on the inbox for a `read-line`, or in `cl:sleep`.
  The agent wraps the three nanosleep functions SLEEP bottoms out in, since compiled code calls SLEEP directly and wrapping SLEEP itself would miss it.
  Open the level here too.
- **In C, anywhere else.**
  Do nothing and return.
  The short-lived thread sees that, waits a few milliseconds, and tries again until the eval thread is back in Lisp.

Only one Interrupt is in flight at a time; pressing it again while one is still trying does nothing.

Nothing ever breaks into C, which is the compromise.
Visitor code sitting in a foreign call (through `sb-alien`) can't be interrupted until the call returns.
After a second of backing off, the agent sends `(:interrupt-waiting)` and the page says so.
If Lisp code keeps running after the C returns, the Interrupt lands there.
If the evaluation finishes first, the agent sends `(:interrupt-missed)`, and the page says that too.
C that never returns can only be stopped by ending the agent, so the waiting note carries a Start over button (see "When the Agent Dies" below).

Landings inside SBCL's own Lisp (the compiler, CLOS, hash tables, packages, the GC) open a level partway through SBCL's work.
SBCL keeps its critical sections closed to interrupts, and SLIME has interrupted this way for years, but that's evidence, not proof; the test suite stresses each of those workloads.

An Interrupt that lands in Lisp still stops a frame mid-instruction, where values can be halfway between registers and the stack.
Reading that frame's args crashed the image too, so it's never read.
Once in a few thousand Interrupts, a frame near the landing point points at garbage as well, and reading its args faults.
So when a level opens, the agent tries reading every frame's args and locals with memory faults caught (a bad read only faults; it doesn't write anything).
Walking down the stack can hit the same garbage and signal, so the walk stops at the first error and the level shows the frames above it.
No error can be let out while a level is being set up: SBCL turns the agent's debugger hook off while the hook runs, so it would land in SBCL's own debugger, which waits forever for a foreground thread.
So all of level setup's frame reading (the walk, the read checks, the backtrace text and both index lists) runs under one guard in `level-frame-view`.
If anything there signals anyway, the level opens with its restarts and no frames, `:level-opened` says `:frames :lost` instead of `:frames :read`, and the page says the backtrace couldn't be read.
Frames that fault, plus the mid-instruction one, are unreadable:

- the backtrace prints just the name with "args unreadable";
- `:level-opened` lists their indexes under `:unreadable`, and the page dims those rows and leaves off Locals;
- they're left out of the frames you can return from or rerun, and Locals and eval-in-frame on them are refused.

All of this leans on SBCL internals (the context index, `compiled-frame-escaped`, the nanosleep functions).
So the agent checks at startup that it's on the SBCL it was tested on (2.6.10) and that every internal it uses is still there, and refuses to start otherwise.
Moving to a new SBCL means running the Interrupt tests on it and then bumping `*tested-sbcl-version*` in `agent/sbcl.lisp`.

```mermaid
sequenceDiagram
  participant G as Gateway
  participant A as Agent
  G->>A: (:eval "(loop)")
  Note over A: spinning
  G->>A: (:interrupt)
  Note over A: reader interrupts<br/>eval thread
  A->>G: (:level-opened :id 1 ...)
  Note over G: continue or abort
```

## When the Agent Dies

The agent process can end on its own: out of memory, a crash, a sandbox limit, or visitor code calling `sb-ext:exit`.
The session's event thread then hits end of file or a stream error and, as its last act, hands the page `(:agent-gone)`.
A session the page closed itself never says that, since a closed session delivers nothing.
That event is the gateway's own; the agent can't send it, being dead.

The page answers with a note and a Start over button.
Start over closes the old session, which kills its process whether it's dead, stuck in C or fine, then clears the scrollback and the debugger panel and opens a fresh session of the same kind.
Everything the old session defined is gone, and the note says so; the REPL history stays.
A second press while one is in progress does nothing.
The same button sits under the `(:interrupt-waiting)` note, which makes it the hard stop for C that never returns.

## The Wire

Gateway and agent swap length-prefixed S-expressions over the per-session unix socket.
The browser never sees any of it.

These are fixed:

| Property | Rule |
|---|---|
| Transport | One unix socket per session, in a directory shared into the sandbox. |
| Encoding | One S-expression per message, headed by a keyword. |
| Framing | A length prefix on every message, modeled on Swank's fixed-width hex. |
| Reading | `*read-eval*` bound to `nil` on every read, both ends. |
| Opacity | Visitor code rides as a string inside `:eval`; the gateway never parses it. |

A message on the wire: six lowercase hex digits, Swank's width, then the printed form:

```
000011(:eval "(car 5)")
```

```lisp
;; sketch: NOT real code
(defun write-message (message stream)
  (let ((text (with-standard-io-syntax
                (let ((*package* (find-package :keyword)))
                  (prin1-to-string message)))))
    (format stream "~(~6,'0x~)~a" (length text) text)
    (finish-output stream)))
```

**The length counts characters.**
Both ends open the socket as a UTF-8 character stream, so the prefix counts characters, not bytes, and a `λ` counts as one.
Counting bytes would let the reader size its buffer before decoding, but it would mean encoding UTF-8 by hand or reaching past ANSI.
Since both ends are Lisp, I'm comfortable with this.
The reader still checks the length against a cap before it reads the body.

The message list itself isn't fixed.
Once the agent exists, it lives in one protocol definition shared by agent and gateway, with round-trip tests on both ends, and changing a message is just a code change.
This is where it starts:

| Direction | Message | Meaning |
|---|---|---|
| Down | `(:eval "form string")` | Read and evaluate the visitor's text. |
| Down | `(:invoke-restart ID INDEX)` | Answer the open `:level-opened` with that id. |
| Down | `(:interrupt)` | Interrupt the eval thread. |
| Down | `(:ping)` | Still there? |
| Up | `(:ready)` | Listening. |
| Up | `(:output "text")` | A chunk of output. |
| Up | `(:values ("printed value" ...))` | Printed values of a finished eval. |
| Up | `(:level-opened :id ID :depth N ...)` | A debugger level opened, with `:unwindable` and `:unreadable` frame indexes, and `:frames` saying whether the stack was `:read` or `:lost`. |
| Up | `(:level-closed ID)` | That level closed. |
| Up | `(:interrupt-waiting)` | Interrupt can't land yet: the code is in foreign (C) code. |
| Up | `(:interrupt-missed)` | The evaluation finished before a waiting Interrupt could land. |
| Up | `(:pong)` | Still here. |

`(:agent-gone)` isn't on this list because it never crosses the wire: the gateway's session makes it up when the socket ends, and drops any copy that arrives from the agent, since visitor code can write anything to the socket.

## Printing

Values come back as printed strings (never as objects!!) so nothing in the image ever has to be read back on the gateway side as Lisp data.
That moves the problem to printing, and visitors will absolutely type this (making me roll my eyes):

```lisp
(let ((x (list 1 2 3)))
  (setf (cdr (last x)) x))   ; prints forever unless we stop it
```

So the agent prints values (and backtrace frames) with `*print-circle*` on and `*print-length*` and `*print-level*` set, and cuts any one printed string (a value, a report, a frame's call, a local) at 4000 characters, saying how long it was.
A message still over the wire's limit goes as a short stand-in: values as one note, and a level with its restarts but its frames marked lost; the agent never signals about length inside the debugger hook.
A REPL line over the limit isn't sent, and the page says so.
Same idea for `:output`: `(loop (print 'hi))` is a perfectly reasonable thing to type, so the agent batches output into chunks and the gateway caps the scrollback and drops the oldest lines.
The visitor can always interrupt it.
The actual limits get picked when we can watch it happen in a browser.

## The Sandbox

Every session sandbox gets:

- No network.
- A read-only root and a small tmpfs.
- CPU, memory and process-count limits, plus a wall-clock timeout.
- A non-root user, all capabilities dropped, and seccomp (or whatever a jail's version of that is).
- An idle timeout and a max lifetime, which the gateway enforces, not the sandbox.

It's hardened when a fork bomb, a memory hog, an infinite loop and an outbound connection attempt are each boring.
If plain containers turn out too thin, gVisor or Firecracker are next, and we decide that once the prototype works.

```mermaid
flowchart TB
  sm["Session manager"]
  gf["start-sandbox<br/>stop-sandbox<br/>sandbox-socket-path"]
  docker["docker-runtime"]
  jail["jail-runtime<br/>only if FreeBSD"]
  sm --> gf
  gf --> docker
  gf -.-> jail
  class sm,gf tone-blue
  class docker,jail tone-gray
```

The gateway only ever talks to a sandbox through three generic functions, so hosting doesn't leak into it while FreeBSD jails versus Linux is still open:

```lisp
(defgeneric start-sandbox (runtime lesson session))   ; => handle
(defgeneric stop-sandbox (runtime handle))
(defgeneric sandbox-socket-path (runtime handle))     ; => pathname
```

`docker-runtime` goes first because it's easy to run on a laptop.
`jail-runtime` only gets built if the cost comparison comes out for FreeBSD.
Both shell out through `uiop:run-program`, and the Docker one is mostly a list of flags:

```lisp
;; sketch, limits come from the runtime object
(defmethod start-sandbox ((runtime docker-runtime) lesson session)
  (uiop:run-program
   (list "docker" "run" "--detach" "--rm"
         "--network" "none"
         "--read-only" "--tmpfs" "/tmp"
         "--memory" (memory-limit runtime)
         "--cpus" (cpu-limit runtime)
         "--pids-limit" (pids-limit runtime)        ; fork bombs stop here
         "--user" "lisp"
         "--cap-drop" "ALL"
         "--security-opt" "no-new-privileges"
         "--volume" (format nil "~a:/run/agent" (socket-dir session))
         (lesson-image lesson))
   :output '(:string :stripped t)))                 ; container id is the handle
```

No network means the socket can't be TCP, so it sits in a directory shared into the sandbox: a bind mount for Docker, nullfs for a jail.
The gateway listens and the agent connects.
That way the agent only needs `socket()` and `connect()`, the gateway never has to poll for a socket file to show up, and the shared directory can be mounted read-only, since the sandbox never creates anything in it.
We haven't checked the file permissions on that socket yet: the gateway creates it, and the sandbox's user needs write permission on it to connect, which is a per-backend detail to verify.
Docker also has no wall-clock limit of its own, so the gateway covers that with the max lifetime and `stop-sandbox`.

## Lessons

A lesson goes from source to sessions in six steps:

1. `lessons/NAME/` holds its source and prose.
2. A fresh SBCL loads the agent and the lesson.
3. The build checks that no contrib and no Quicklisp got loaded.
4. `save-lisp-and-die` writes the image.
5. `lesson-NAME.core` is the result, and it is read-only.
6. Every session starts from it with `sbcl --core`.

A lesson is a directory with Lisp source, optional prose, and a build step.
You start inside the running image rather than reading a program, which is the other thing (besides the debugger) that makes CL feel different, and a lesson shipped as source to read would hide it.

Only ANSI CL, the agent, splice's two contrib-free systems (`splice` and `splice/cluster`) and the lesson's own code go in the core.
No Quicklisp, no third-party library, no contribs, and the build fails if a contrib got loaded.
If a lesson needs something beyond ANSI and splice, the lesson writes it.
A visitor who watches a library do the interesting part learned about the library; splice is the exception because its interesting part is the condition system the site teaches.

Cores are built once and never written to.
Whether each lesson gets its own Docker image, or there's one SBCL image and the cores get mounted in read-only, is open; we lean toward the second, since then rebuilding a lesson doesn't mean rebuilding an image.

Core size and per-session memory decide how many sessions fit on one box, and whether sessions can share the core's pages is the number we most want to see.
SBCL is the only implementation on the server, pinned at the version in `sbcl-version` at the repo root (2.6.10).

## The Panel

The UI is a scrollback, a REPL line with history, the debugger panel, and lesson prose next to it.
The panel is the part that matters, so any front end gets judged on it first.
For a lesson with a planted bug it'd look something like:

```
+-- SIMPLE-ERROR  (level 1) ----------------------------+
| Can't parse "twelve" as a price.                      |
|                                                       |
|  [ USE-VALUE ]   Use a different price.               |
|  [ SKIP-ROW ]    Skip this row and keep going.        |
|  [ RETRY ]       Evaluate the form again.             |
|  [ ABORT ]       Back to top level.                   |
|                                                       |
|  > backtrace (click to open)                          |
+-------------------------------------------------------+
```

CLOG (drives the DOM from Lisp over a websocket) drives the page.
The one exception is the REPL line, a `<textarea>` with a short hand-written key script (`repl-line-script` in `gateway/web.lisp`).
On Enter the script takes and clears the text in the browser and sends it up as an event, so typing during a round trip is never mixed into the submitted line, and Shift+Enter inserts a newline.
History stays in Lisp, so each ArrowUp costs one round trip.

## Layout

```
to-restart-is-divine.asd ; systems: wire, agent, link, gateway, web, tests (lessons later)
sbcl-version             ; the pinned SBCL
agent/                   ; zero-dependency debug agent, and the wire framing it shares
gateway/                 ; socket link, sessions, web page, runtimes, bridge
lessons/                 ; one directory per lesson, plus the build script
deploy/                  ; container and jail definitions, proxy config
test/
```

## Rules

These hold no matter what else changes, and breaking one is a design change, not a bug fix.

- The gateway never `read`s or `eval`s visitor text.
- No socket is ever read with `*read-eval*` on.
- No sandbox gets network.
- The Lisp web server is only ever behind the proxy.
- Nothing but ANSI CL, the agent, splice's contrib-free systems and the lesson goes in a lesson image.
- Nothing about containment depends on Lisp.
- Nothing goes on the internet until the sandbox limits are in.

## Open

- **CLOG or thin JS.**
  Closed by building the debugger panel in CLOG and seeing whether it fights us.
- **Jails or Linux.**
  Closed by the hosting cost comparison; the runtime functions keep the gateway out of it until then.
- **Gateway parser.**
  `cl:read` under tight bindings or the tiny parser.
  The reading side is written and uses `cl:read` for now; closed by a decision before visitor code in a sandbox is on the far end of the socket.
- **Containers, gVisor or Firecracker.**
  Closed by the hardening tests once the prototype works.
- **Memory per session and shared core pages.**
  Closed by measuring the first real core.
- **ECL in the browser.**
  If ECL compiled to WebAssembly can run `restart-case`, signal a `type-error` and hand back the restart list, there's a mode with no gateway or sandbox at all.
  Closed by a short time-boxed spike.
  JSCL is out, since it's a subset with no documented condition system.
- **Download your own image.**
  `save-lisp-and-die` ends the process, so snapshotting a live session probably means forking a child first, and nobody's checked how that behaves with threads.
  Parked.
