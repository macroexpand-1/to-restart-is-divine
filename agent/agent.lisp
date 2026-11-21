;;;; agent/agent.lisp: the REPL, the debugger, and the thread that reads and dispatches messages

(in-package #:to-restart-is-divine/agent)

(defconstant +max-backtrace-frames+ 30
  "Most frames we'll put in one :level-opened message")

(defconstant +max-printed-length+ 4000
  "Longest printed value, call, local or report we send; anything longer gets cut")

(defconstant +interrupt-retry-seconds+ 1/200
  "How long to back off after an Interrupt lands in C before trying again")

(defconstant +landing-poll-seconds+ 1/1000
  "How often to check whether a sent Interrupt has run yet")

(defconstant +interrupt-patience-seconds+ 1
  "How long an Interrupt backs off before telling the page it's waiting on foreign code")

(defconstant +hang-up-patience-seconds+ 5
  "How long a hang-up backs off from foreign code before leaving the eval thread be")

;;; The eval thread's dynamic state, read-only.
;;; *EVAL-STATE* never gets assigned, just rebound with LET on the eval thread,
;;; so its lifetime is easy to follow. Everything else gets passed as an argument.

(defstruct (eval-state (:constructor make-eval-state
                           (&key levels top-level-abort evaluating context-depth in-safe-wait))
                       (:copier nil))
  (levels '() :read-only t)
  (top-level-abort :none :read-only t)
  (evaluating nil :read-only t)
  (context-depth 0 :read-only t)   ; interrupt contexts in effect where the current code runs
  (in-safe-wait nil :read-only t)) ; true in a wait that's fine to unwind out of, even in C

(defvar *eval-state* (make-eval-state)
  "The eval thread's state. The global value means idle: no levels, nothing evaluating")

;;; Each agent gets two threads. The reader reads every message from the gateway:
;;; it answers :ping itself, hands :interrupt to a short-lived thread that interrupts
;;; the eval thread (see Interrupt below),
;;; drops :input lines in the input mailbox, and queues :eval, :invoke-restart and
;;; the frame messages in the inbox.
;;; The eval thread works through the inbox (at the top level or inside a level)
;;; and sends everything else up to the gateway.

(defstruct (agent (:constructor %make-agent (stream)))
  (stream nil :read-only t)
  (send-lock (make-lock "agent send") :read-only t)
  (inbox (make-mailbox) :read-only t)
  (input-mailbox (make-mailbox) :read-only t) ; lines of visitor input as strings
  ;; mutable
  (connected-p t)
  (next-id 1)
  eval-thread
  reader-thread
  (interrupt-report :none)) ; LANDING-REPORT of the latest Interrupt, or :none

(defun send (agent message)
  "Send MESSAGE to the gateway, or a short stand-in if it's too long for the wire.
A stand-in that's still too long is dropped, and once a write fails, this quietly does
nothing. It never signals, since it runs inside the debugger hook"
  (call-with-lock (agent-send-lock agent)
                  (lambda ()
                    (when (agent-connected-p agent)
                      (handler-case (write-message (sendable message) (agent-stream agent))
                        (malformed-message () :dropped)
                        (stream-error ()
                          (setf (agent-connected-p agent) nil)))))))

(defun sendable (message)
  "MESSAGE, or a short stand-in when it prints over the wire's limit. Sends can happen
inside the debugger hook, where signaling about the length would take the session down"
  (if (> (length (message-text message)) *max-message-length*)
      (stand-in-message message)
      message))

(defun stand-in-message (message)
  "Short message that goes in place of MESSAGE. A level still opens, with its restarts and
its frames marked lost, and values come back as one note"
  (case (first message)
    (:values (list :values (list (too-long-note message))))
    (:level-opened (level-opened-without-frames (rest message)))
    (otherwise (list :output (format nil "~&;; ~A~%" (too-long-note message))))))

(defun too-long-note (message)
  "What a stand-in says about MESSAGE"
  (format nil "#<~(~S~) message of ~D characters, too long to send>"
          (first message) (length (message-text message))))

(defun level-opened-without-frames (fields)
  ":level-opened message with the level and restarts from FIELDS, and no frames"
  (list* :level-opened
         :id (getf fields :id)
         :depth (getf fields :depth)
         :type (getf fields :type)
         :report (getf fields :report)
         :restarts (getf fields :restarts)
         (lost-frame-fields)))

(defmethod forward-text ((agent agent) text)
  (send agent (list :output text)))

(defmethod next-input-line ((agent agent))
  (request-input agent))

(defun request-input (agent)
  "Wait for the next line the visitor types. Tells the page we want a line, and tells it
again when we stop waiting, whether a line showed up or a restart yanked us out"
  (finish-output *standard-output*)
  (send agent '(:needs-input))
  (unwind-protect (call-in-safe-wait (lambda () (mailbox-receive (agent-input-mailbox agent))))
    (send agent '(:input-done))))

(defun call-in-safe-wait (function)
  "Call FUNCTION, a wait that's fine to unwind out of, so an Interrupt landing in its C
part can still open a level"
  (let ((*eval-state* (eval-state-with *eval-state* :in-safe-wait t)))
    (funcall function)))

(defun sleep-in-safe-wait (sleep-function &rest arguments)
  "Call SLEEP-FUNCTION on ARGUMENTS as a safe wait. WRAP-SLEEP sends CL:SLEEP through here"
  (call-in-safe-wait (lambda () (apply sleep-function arguments))))

(defun eval-state-with (eval-state &key (levels (eval-state-levels eval-state))
                                     (top-level-abort (eval-state-top-level-abort eval-state))
                                     (evaluating (eval-state-evaluating eval-state))
                                     (context-depth (eval-state-context-depth eval-state))
                                     (in-safe-wait (eval-state-in-safe-wait eval-state)))
  "A copy of EVAL-STATE with whichever fields you pass swapped in"
  (make-eval-state :levels levels :top-level-abort top-level-abort :evaluating evaluating
                   :context-depth context-depth :in-safe-wait in-safe-wait))

(define-condition interrupted (condition)
  ()
  (:report "Interrupted from the page."))

(defun call-with-visitor-repl (input output function)
  "Call FUNCTION in the visitor's REPL world: every standard stream reads from INPUT and
writes to OUTPUT, we're in CL-USER, and the REPL variables start out empty"
  (let* ((terminal (make-two-way-stream input output))
         (*package* (find-package "CL-USER"))
         (*standard-output* output)
         (*error-output* output)
         (*trace-output* output)
         (*terminal-io* terminal)
         (*standard-input* terminal)
         (*query-io* terminal)
         (*debug-io* terminal)
         (- nil) (+ nil) (++ nil) (+++ nil)
         (* nil) (** nil) (*** nil)
         (/ nil) (// nil) (/// nil))
    (funcall function)))

(defun eval-loop (agent)
  "The eval thread itself: work through the inbox at the top level, forever"
  (let ((input (make-input-stream agent))
        (output (make-forwarding-stream agent))
        (*eval-state* (make-eval-state :context-depth (interrupt-context-depth))))
    (call-with-visitor-repl
     input
     output
     (lambda ()
       (call-with-debugger (lambda (condition) (enter-debugger agent condition))
                           (lambda ()
                             (loop (serve-top-level agent))))))))

(defun serve-top-level (agent)
  "Handle one inbox message at the top level. A restart or frame message aimed at a level
that's already gone just gets dropped (:STALE-RESTART or :STALE-FRAME)"
  (let ((message (mailbox-receive (agent-inbox agent))))
    (ecase (first message)
      (:eval (call-with-top-level-abort (lambda () (evaluate agent (second message)))))
      (:invoke-restart :stale-restart)
      ((:locals :eval-in-frame :return-from-frame :rerun-frame) :stale-frame))))

(defun call-with-top-level-abort (function)
  "Call FUNCTION under our own top-level ABORT, and remember that restart in *EVAL-STATE*"
  (with-simple-restart (abort "Return to top level.")
    (let ((*eval-state* (eval-state-with *eval-state* :top-level-abort (newest-abort-restart))))
      (funcall function))))

(defun newest-abort-restart ()
  "Innermost ABORT restart, or :NONE"
  (or (find 'abort (compute-restarts) :key #'restart-name) :none))

(defun evaluate (agent text)
  "Read and eval each form in TEXT, sending up each one's printed values"
  (let ((*eval-state* (eval-state-with *eval-state* :evaluating t)))
    (with-input-from-string (in text)
      (loop for form = (read in nil in)
            until (eq form in)
            do (setf - form)
               (let ((values (evaluate-form form)))
                 (shift-repl-variables form values)
                 (finish-output *standard-output*)
                 (send agent (list :values (print-values values))))))))

(defun print-values (values)
  "VALUES each printed with PRIN1. Circular stuff gets #n= labels, and long or deep
stuff gets cut short unless visitor set their own limits"
  (let ((*print-circle* t)
        (*print-length* (or *print-length* 100))
        (*print-level* (or *print-level* 10)))
    (mapcar (lambda (value) (clipped (prin1-to-string value))) values)))

(defun clipped (text)
  "TEXT, or its first +MAX-PRINTED-LENGTH+ characters and a note of how long it was.
The print limits cap lists, not strings, so a long string needs this"
  (if (> (length text) +max-printed-length+)
      (format nil "~A ... [cut, ~D characters]" (subseq text 0 +max-printed-length+) (length text))
      text))

(defun evaluate-form (form)
  "FORM's values. RETRY runs FORM again from the top"
  (loop
    (restart-case (return (multiple-value-list (eval form)))
      (retry ()
        :report "Evaluate the form again."))))

(defun shift-repl-variables (form values)
  "Shift REPL variables down one now that FORM returned VALUES"
  (setf +++ ++ ++ + + form
        /// // // / / values
        *** ** ** * * (first values)))

;; Debugger stuff

(defstruct frame-view
  "What a level knows about its frames: the frames themselves, the UNREADABLE ones among
them, and FIELDS, the frame part of its :level-opened message"
  (frames '() :read-only t)
  (unreadable '() :read-only t)
  (fields '() :read-only t))

(defun enter-debugger (agent condition)
  "Open CONDITION as new debugger level and work the inbox until a restart or frame
operation gets us out"
  (let* ((id (agent-next-id agent))
         (restarts (visible-restarts condition))
         (frame-view (level-frame-view condition))
         (frames (frame-view-frames frame-view))
         (levels (cons (list id restarts frames (frame-view-unreadable frame-view))
                       (eval-state-levels *eval-state*)))
         ;; the depth here counts whatever interrupt or error trap opened this level,
         ;; since it stays pushed while the level is open
         (*eval-state* (eval-state-with *eval-state* :levels levels :evaluating nil
                                                     :context-depth (interrupt-context-depth)
                                                     :in-safe-wait nil)))
    (setf (agent-next-id agent) (1+ id))
    (finish-output *standard-output*)
    (send agent (level-opened-message id (length levels) condition restarts frame-view))
    ;; SBCL binds the hook to NIL while it runs, so each level has to bind it again.
    ;; That's how an error inside an open level opens the next one
    (unwind-protect
         (call-with-debugger (lambda (inner) (enter-debugger agent inner))
                             (lambda () (serve-level agent)))
      (send agent (list :level-closed id)))))

(defun level-opened-message (id depth condition restarts frame-view)
  ":level-opened message for CONDITION, as level ID at DEPTH, its frames from FRAME-VIEW"
  (list* :level-opened
         :id id
         :depth depth
         :type (type-name condition)
         :report (printed #'princ-to-string condition)
         :restarts (restart-entries restarts)
         (frame-view-fields frame-view)))

(defun level-frame-view (condition)
  "Frame view for a level opening over CONDITION. SBCL turns our debugger hook off until
the level binds it again, so an error reading frames here would land in SBCL's own debugger
and hang. Any error gives the empty view, marked :LOST, and the level opens with just its
restarts"
  (handler-case (read-frame-view condition)
    (error () (make-frame-view :fields (lost-frame-fields)))))

(defun lost-frame-fields ()
  "Frame part of a :level-opened message whose frames couldn't be read or sent"
  (list :frames :lost :backtrace '() :unwindable '() :unreadable '()))

(defun read-frame-view (condition)
  "Frame view of the frames from where the debugger was entered down to EVALUATE-FORM"
  (let* ((frames (debugger-frames 'evaluate-form +max-backtrace-frames+))
         (unreadable (unreadable-frames condition frames)))
    (make-frame-view :frames frames
                     :unreadable unreadable
                     :fields (list :frames :read
                                   :backtrace (backtrace frames unreadable)
                                   :unwindable (unwindable-indexes frames unreadable)
                                   :unreadable (unreadable-indexes frames unreadable)))))

(defun restart-entries (restarts)
  "RESTARTS as (index name report) lists, the way :level-opened carries them"
  (loop for restart in restarts
        for index from 0
        collect (list index
                      (restart-name-string restart)
                      (printed #'princ-to-string restart))))

(defun serve-level (agent)
  "Work the inbox inside newest level until a restart or frame operation gets us out"
  (loop
    (let ((message (mailbox-receive (agent-inbox agent))))
      (ecase (first message)
        (:eval (evaluate agent (second message)))
        (:invoke-restart (destructuring-bind (id index) (rest message)
                           (invoke-chosen-restart id index)))
        ((:locals :eval-in-frame :return-from-frame :rerun-frame)
         (operate-on-frame agent message))))))

(defun invoke-chosen-restart (id index)
  "Invoke restart INDEX of open level ID. Unknown level or index is ignored"
  (let* ((level (find id (eval-state-levels *eval-state*) :key #'first))
         (restarts (second level)))
    (when (and level (integerp index) (< -1 index (length restarts)))
      (invoke-restart-interactively (nth index restarts)))))

(defun visible-restarts (condition)
  "CONDITION's restarts, innermost first, stopping at our top-level ABORT.
Anything past it belongs to the thread and would kill the eval thread.
If top-level ABORT is :NONE or not in there, you get all of them"
  (let* ((restarts (compute-restarts condition))
         (end (position (eval-state-top-level-abort *eval-state*) restarts)))
    (if (integerp end)
        (subseq restarts 0 (1+ end))
        restarts)))

(defun restart-name-string (restart)
  "RESTART's name as a string, empty if it's anonymous"
  (let ((name (restart-name restart)))
    (if name (symbol-name name) "")))

(defun type-name (condition)
  "CONDITION's type, with its package unless it's in CL-USER"
  (with-standard-io-syntax
    (prin1-to-string (type-of condition))))

(defun printed (printer object)
  "OBJECT printed by PRINTER and clipped, or a note if printing blows up"
  (handler-case (clipped (funcall printer object))
    (error () "#<error while printing>")))

(defun backtrace (frames unreadable)
  "FRAMES printed as calls, numbered from 0, newest first. UNREADABLE ones get just a name"
  (let ((*print-length* 10)
        (*print-level* 4)
        (*print-circle* t))
    (loop for frame in frames
          for index from 0
          collect (format nil "~D: ~A" index (frame-text frame unreadable)))))

(defun frame-text (frame unreadable)
  "FRAME printed as a call, or only its name if it's one of the UNREADABLE frames"
  (if (member frame unreadable)
      (format nil "(~A ...) ; args unreadable"
              (printed #'prin1-to-string (frame-function-name frame)))
      (printed #'prin1-to-string (frame-call frame))))

;; Interrupt
;;
;; An interrupt can land anywhere on the eval thread, including in C that holds a lock
;; (malloc, say, while SBCL compresses debug info). Opening a level there deadlocks on that
;; lock, and unwinding out of it leaves the lock held for good. So when one lands in C
;; outside a safe wait, it touches nothing and returns, and we try again a moment later.
;; Same when eval thread holds one of our own locks, like a mailbox's in the Lisp part of
;; MAILBOX-RECEIVE: a level opened there would block the reader and trip over that lock.
;; If it's still backing off after a second, it's probably sitting in visitor's foreign
;; code, so we tell the page and keep trying. Nothing breaks into C, ever.

;; One LANDING-REPORT per press of Interrupt, shared by all its tries. Eval thread writes
;; where each try landed, before it opens any level, so the reader can tell a press that's
;; still trying from one that's done without racing the level

(defstruct (landing-report (:constructor make-landing-report (purpose)))
  (purpose :interrupt :read-only t) ; :interrupt opens a level, :hang-up ends the eval thread
  ;; mutable
  (landing :pending)) ; :pending while a try is out, then what the try decided

(defun interrupt-evaluation (agent)
  "Open a debugger level on eval thread if it's running visitor code. Happens on its own
thread, since it may take a few tries and the reader shouldn't wait on that. One at a
time: :INTERRUPTING, or :ALREADY-INTERRUPTING if the last press is still trying"
  (if (interrupt-in-flight-p agent)
      :already-interrupting
      (let ((report (make-landing-report :interrupt)))
        (setf (agent-interrupt-report agent) report)
        (spawn "agent interrupt" (lambda () (land-interrupt agent report)))
        :interrupting)))

(defun interrupt-in-flight-p (agent)
  "True while the latest Interrupt hasn't landed yet"
  (let ((report (agent-interrupt-report agent)))
    (and (not (eq report :none))
         (let ((landing (landing-report-landing report)))
           (or (eq landing :pending) (backing-off-p landing))))))

(defun land-interrupt (agent report)
  "Keep interrupting eval thread until the interrupt lands somewhere it can act, noting each
try in REPORT. Tells the page if that's taking a while. Returns the last landing"
  (let ((landing (finish-interrupt agent report)))
    (setf (landing-report-landing report) landing)
    landing))

(defun finish-interrupt (agent report)
  "Retry until the interrupt lands, telling the page once if it's waiting. The landing"
  (let ((landing (retry-interrupt agent report (seconds-from-now +interrupt-patience-seconds+))))
    (if (backing-off-p landing)
        (progn (send agent '(:interrupt-waiting))
               (close-waiting-interrupt agent (retry-interrupt agent report :never)))
        landing)))

(defun close-waiting-interrupt (agent landing)
  "Page was told the Interrupt is waiting. If it never got to open a level because evaluation
finished (or stopped in a level) first, say so. Returns LANDING"
  (when (eq landing :not-evaluating)
    (send agent '(:interrupt-missed)))
  landing)

(defun retry-interrupt (agent report deadline)
  "Interrupt eval thread, backing off and trying again while that's what it says, until
DEADLINE (internal real time, or :NEVER). Returns the last landing"
  (loop for landing = (try-interrupt agent report)
        while (and (backing-off-p landing) (before-deadline-p deadline))
        do (sleep +interrupt-retry-seconds+)
        finally (return landing)))

(defun backing-off-p (landing)
  "True when LANDING means touch nothing and try again"
  (member landing '(:in-c :holding-agent-lock)))

(defun seconds-from-now (seconds)
  "Internal real time SECONDS from now"
  (+ (get-internal-real-time) (round (* seconds internal-time-units-per-second))))

(defun before-deadline-p (deadline)
  "True if DEADLINE (internal real time, or :NEVER) hasn't passed"
  (or (eq deadline :never)
      (< (get-internal-real-time) deadline)))

(defun try-interrupt (agent report)
  "Interrupt AGENT's eval thread once and wait to hear in REPORT where it landed, or
:EVAL-THREAD-GONE"
  (let ((eval-thread (agent-eval-thread agent)))
    (setf (landing-report-landing report) :pending)
    (ecase (interrupt eval-thread (lambda () (act-on-interrupt agent report)))
      (:sent (wait-for-landing report eval-thread))
      (:thread-gone :eval-thread-gone))))

(defun wait-for-landing (report eval-thread)
  "Where the interrupt behind REPORT landed, once it's run, or :EVAL-THREAD-GONE"
  (loop while (and (eq (landing-report-landing report) :pending)
                   (thread-alive-p eval-thread))
        do (sleep +landing-poll-seconds+))
  (let ((landing (landing-report-landing report)))
    (if (eq landing :pending) :eval-thread-gone landing)))

(defun act-on-interrupt (agent report)
  "The interrupt itself, on AGENT's eval thread. Does what REPORT's purpose asks if that's
safe where it landed, and notes in REPORT what it decided. Otherwise it does nothing else at
all. Interrupts stay off until it acts: a second interrupt landing in this function's own
Lisp would look like a Lisp landing even when we're sitting on top of C"
  (ecase (landing-report-purpose report)
    (:interrupt (open-level-if-safe report (interrupt-landing agent)))
    (:hang-up (stop-if-safe report (interrupt-landing agent)))))

(defun open-level-if-safe (report landing)
  "Note LANDING in REPORT, then open the Interrupt's level if that's safe there"
  (setf (landing-report-landing report) landing)
  (ecase landing
    ((:in-lisp :in-safe-wait) (call-with-interrupts #'open-interrupt-level))
    ((:in-c :holding-agent-lock :not-evaluating) landing)))

(defun stop-if-safe (report landing)
  "End the eval thread unless LANDING is visitor code in C, noting :STOPPING or :IN-C in
REPORT. A held agent lock doesn't matter here: unwinding lets go of it, and nobody's going
to want it again"
  (ecase landing
    ((:in-lisp :in-safe-wait :holding-agent-lock :not-evaluating)
     (setf (landing-report-landing report) :stopping)
     (call-with-interrupts #'abort-this-thread))
    (:in-c (setf (landing-report-landing report) :in-c))))

(defun open-interrupt-level ()
  "Open a debugger level for the Interrupt, with CONTINUE to carry on where it landed"
  (with-simple-restart (continue "Resume the interrupted evaluation.")
    (invoke-debugger (make-condition 'interrupted))))

(defun interrupt-landing (agent)
  "Where an interrupt running on AGENT's eval thread landed: :NOT-EVALUATING,
:HOLDING-AGENT-LOCK, :IN-LISP, :IN-SAFE-WAIT or :IN-C"
  (cond ((not (eval-state-evaluating *eval-state*)) :not-evaluating)
        ((holding-agent-lock-p agent) :holding-agent-lock)
        ((> (interrupt-context-depth) (eval-state-context-depth *eval-state*)) :in-lisp)
        ((eval-state-in-safe-wait *eval-state*) :in-safe-wait)
        (t :in-c)))

(defun holding-agent-lock-p (agent)
  "True when this thread holds one of AGENT's locks"
  (some #'holding-lock-p (list (agent-send-lock agent)
                               (mailbox-lock (agent-inbox agent))
                               (mailbox-lock (agent-input-mailbox agent)))))

;; Reader thread


(defun dispatch-message (agent message)
  "Act on one MESSAGE from gateway. A kind we don't know is an error"
  (ecase (first message)
    ((:eval :invoke-restart :locals :eval-in-frame :return-from-frame :rerun-frame)
     (mailbox-send (agent-inbox agent) message))
    (:input (mailbox-send (agent-input-mailbox agent) (second message)))
    (:interrupt (interrupt-evaluation agent))
    (:ping (send agent '(:pong)))))

(defun reader-loop (agent)
  "Read and dispatch gateway messages until the socket closes, then end eval thread.
:STOPPED, or :EVAL-THREAD-STUCK if it stayed in visitor's foreign code too long"
  (unwind-protect (progn (read-until-hang-up agent)
                         (stop-eval-thread agent))
    (close (agent-stream agent) :abort t)))

(defun read-until-hang-up (agent)
  "Dispatch gateway messages until the socket closes. A message we can't act on stops it too"
  (handler-case
      (loop for message = (read-message (agent-stream agent))
            until (eq message :end)
            do (dispatch-message agent message))
    (error (condition)
      (format *error-output* "~&;; Agent reader stopped: ~A~%" condition))))

(defun stop-eval-thread (agent)
  "End AGENT's eval thread the way Interrupt lands: never out of C. :STOPPED once it's done,
or :EVAL-THREAD-STUCK if it's still in C after +HANG-UP-PATIENCE-SECONDS+. Then it's left
running; in its own process, exiting gets rid of it without unwinding anything"
  (let* ((report (make-landing-report :hang-up))
         (landing (retry-interrupt agent report (seconds-from-now +hang-up-patience-seconds+))))
    (if (backing-off-p landing)
        :eval-thread-stuck
        (progn (wait-for-thread (agent-eval-thread agent))
               :stopped))))

(defun start-agent (path)
  "Make sure this SBCL is the one we tested on, raise compiler policy, mark SLEEP as a safe
wait, connect to gateway listening on PATH, send :ready and start both threads. Returns the
agent"
  (check-sbcl-internals)
  (prefer-debuggable-code)
  (wrap-sleep 'sleep-in-safe-wait)
  (let ((agent (%make-agent (connect-unix-socket path))))
    (setf (agent-eval-thread agent) (spawn "agent eval" (lambda () (eval-loop agent))))
    (send agent '(:ready))
    (setf (agent-reader-thread agent) (spawn "agent reader" (lambda () (reader-loop agent))))
    agent))

(defun wait-for-agent (agent)
  "Return once AGENT's gateway hangs up and its reader has stopped the eval thread or given
up on it: :STOPPED, :EVAL-THREAD-STUCK, or :ABORTED if the reader itself was killed"
  (wait-for-thread (agent-reader-thread agent)))

(defun run-agent (path)
  "Answer gateway at PATH until it hangs up, then exit. Agent's entry point"
  (wait-for-agent (start-agent path))
  (exit-process))
