;;;; agent/agent.lisp: the REPL, the debugger, and the thread that reads and dispatches messages

(in-package #:to-restart-is-divine/agent)

(defconstant +max-backtrace-frames+ 30
  "Most frames we'll put in one :level-opened message")

(defconstant +interrupt-retry-seconds+ 0.005
  "How long to back off after an Interrupt lands in C before trying again")

(defconstant +landing-poll-seconds+ 0.001
  "How often to check whether a sent Interrupt has run yet")

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
  reader-thread)

(defun send (agent message)
  "Send MESSAGE to the gateway. Once a write fails, this quietly does nothing"
  (call-with-lock (agent-send-lock agent)
                  (lambda ()
                    (when (agent-connected-p agent)
                      (handler-case (write-message message (agent-stream agent))
                        (stream-error ()
                          (setf (agent-connected-p agent) nil)))))))

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
    (mapcar #'prin1-to-string values)))

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

(defun enter-debugger (agent condition)
  "Open CONDITION as new debugger level and work the inbox until a restart or frame
operation gets us out"
  (let* ((id (agent-next-id agent))
         (restarts (visible-restarts condition))
         (frames (debugger-frames 'evaluate-form +max-backtrace-frames+))
         (unreadable (unreadable-frames condition frames))
         (levels (cons (list id restarts frames unreadable) (eval-state-levels *eval-state*)))
         ;; the depth here counts whatever interrupt or error trap opened this level,
         ;; since it stays pushed while the level is open
         (*eval-state* (eval-state-with *eval-state* :levels levels :evaluating nil
                                                     :context-depth (interrupt-context-depth)
                                                     :in-safe-wait nil)))
    (setf (agent-next-id agent) (1+ id))
    (finish-output *standard-output*)
    (send agent (level-opened-message id (length levels) condition restarts frames unreadable))
    ;; SBCL binds the hook to NIL while it runs, so each level has to bind it again.
    ;; That's how an error inside an open level opens the next one
    (unwind-protect
         (call-with-debugger (lambda (inner) (enter-debugger agent inner))
                             (lambda () (serve-level agent)))
      (send agent (list :level-closed id)))))

(defun level-opened-message (id depth condition restarts frames unreadable)
  ":level-opened message for CONDITION, as level ID at DEPTH over FRAMES, some UNREADABLE"
  (list :level-opened
        :id id
        :depth depth
        :type (type-name condition)
        :restarts (restart-entries restarts)
        :backtrace (backtrace frames unreadable)
        :unwindable (unwindable-indexes frames unreadable)))

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
  "OBJECT printed by PRINTER, or a note if printing blows up"
  (handler-case (funcall printer object)
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
      (format nil "(~A ...) ; interrupted mid-instruction, args unreadable"
              (printed #'prin1-to-string (frame-function-name frame)))
      (printed #'prin1-to-string (frame-call frame))))

;; Interrupt
;;
;; An interrupt can land anywhere on the eval thread, including in C that holds a lock
;; (malloc, say, while SBCL compresses debug info). Opening a level there deadlocks on that
;; lock, and unwinding out of it leaves the lock held for good. So when one lands in C
;; outside a safe wait, it touches nothing and returns, and we try again a moment later.

(defstruct (landing-report (:constructor make-landing-report ()))
  ;; mutable
  (landing :pending)) ; :pending until the interrupt runs, then what INTERRUPT-LANDING said

(defun interrupt-evaluation (agent)
  "Open a debugger level on eval thread if it's running visitor code. Happens on its own
thread, since it may take a few tries and the reader shouldn't wait on that"
  (spawn "agent interrupt" (lambda () (land-interrupt agent))))

(defun land-interrupt (agent)
  "Keep interrupting eval thread until the interrupt lands somewhere it can act, backing off
a moment each time it lands in C. Returns the last landing"
  (loop for landing = (try-interrupt (agent-eval-thread agent))
        while (eq landing :in-c)
        do (sleep +interrupt-retry-seconds+)
        finally (return landing)))

(defun try-interrupt (eval-thread)
  "Interrupt EVAL-THREAD once and wait to hear where it landed, or :EVAL-THREAD-GONE"
  (let ((report (make-landing-report)))
    (ecase (interrupt eval-thread (lambda () (act-on-interrupt report)))
      (:sent (wait-for-landing report eval-thread))
      (:thread-gone :eval-thread-gone))))

(defun wait-for-landing (report eval-thread)
  "Where the interrupt behind REPORT landed, once it's run, or :EVAL-THREAD-GONE"
  (loop while (and (eq (landing-report-landing report) :pending)
                   (thread-alive-p eval-thread))
        do (sleep +landing-poll-seconds+))
  (let ((landing (landing-report-landing report)))
    (if (eq landing :pending) :eval-thread-gone landing)))

(defun act-on-interrupt (report)
  "The interrupt itself, on eval thread. Notes in REPORT where it landed, then opens a level
if that's safe there. Landing in C, it does nothing else at all.
Interrupts stay off until the level opens: a second interrupt landing in this function's own
Lisp would look like a Lisp landing even when we're sitting on top of C"
  (let ((landing (interrupt-landing)))
    (setf (landing-report-landing report) landing)
    (ecase landing
      ((:in-lisp :in-safe-wait) (call-with-interrupts #'open-interrupt-level))
      ((:in-c :not-evaluating) landing))))

(defun open-interrupt-level ()
  "Open a debugger level for the Interrupt, with CONTINUE to carry on where it landed"
  (with-simple-restart (continue "Resume the interrupted evaluation.")
    (invoke-debugger (make-condition 'interrupted))))

(defun interrupt-landing ()
  "Where an interrupt running on eval thread landed: :NOT-EVALUATING, :IN-LISP, :IN-SAFE-WAIT
or :IN-C"
  (cond ((not (eval-state-evaluating *eval-state*)) :not-evaluating)
        ((> (interrupt-context-depth) (eval-state-context-depth *eval-state*)) :in-lisp)
        ((eval-state-in-safe-wait *eval-state*) :in-safe-wait)
        (t :in-c)))

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
A message we can't act on stops the reader"
  (unwind-protect
       (handler-case
           (loop for message = (read-message (agent-stream agent))
                 until (eq message :end)
                 do (dispatch-message agent message))
         (error (condition)
           (format *error-output* "~&;; Agent reader stopped: ~A~%" condition)))
    (kill-thread (agent-eval-thread agent))
    (close (agent-stream agent) :abort t)))

(defun start-agent (path)
  "Raise compiler policy, mark SLEEP as a safe wait, connect to gateway listening on PATH,
send :ready and start both threads. Returns the agent"
  (prefer-debuggable-code)
  (wrap-sleep 'sleep-in-safe-wait)
  (let ((agent (%make-agent (connect-unix-socket path))))
    (setf (agent-eval-thread agent) (spawn "agent eval" (lambda () (eval-loop agent))))
    (send agent '(:ready))
    (setf (agent-reader-thread agent) (spawn "agent reader" (lambda () (reader-loop agent))))
    agent))

(defun wait-for-agent (agent)
  "Return once AGENT's gateway hangs up and *both* threads are done"
  (wait-for-thread (agent-reader-thread agent))
  (wait-for-thread (agent-eval-thread agent)))

(defun run-agent (path)
  "Answer gateway at PATH until it hangs up, then exit. Agent's entry point"
  (wait-for-agent (start-agent path))
  (exit-process))
