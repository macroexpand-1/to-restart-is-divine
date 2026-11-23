;;;; gateway/agent-session.lisp: a session backed by a real agent

(in-package #:to-restart-is-divine/gateway)

;;; The agent writes lists in the shapes session.lisp lists. The event thread turns
;;; each one into an up event with WIRE-EVENT before the receiver sees it. :pong is
;;; not an event, and :agent-gone is the gateway's own to say, so both get dropped first.
;;;
;;; Each direction of the socket has one owner thread. The event thread reads. The
;;; writer thread is the only one that writes, taking messages from the outbox, so a
;;; send never waits on the socket and no lock is held across I/O.
;;;
;;; The session's phase is :OPEN, then :CLOSING, then :CLOSED, kept in a box. It passes
;;; through :GONE first when the agent dies on its own: the event thread saw the socket end,
;;; or the writer's write failed. Sends drop from then on, rather than queue for a writer
;;; that has stopped. Closing claims :OPEN or :GONE by compare-and-swap, so exactly one
;;; caller closes. Every wait has an exit that is sure to fire:
;;;   - the event thread's read ends when the agent's process dies, and closing kills it
;;;   - the writer wakes every second and stops once the phase has left :OPEN
;;;   - closing gives each thread a few seconds and logs any that didn't finish

(defclass agent-session (session)
  ((link :initarg :link :reader agent-session-link)
   (receiver :initarg :receiver :reader agent-session-receiver)
   (phase :initform (th:make-box :open) :reader agent-session-phase) ; :open, :gone, :closing or :closed
   (outbox :initform (th:make-mailbox "agent session outbox") :reader agent-session-outbox)
   ;; mutable, set once by OPEN-SESSION
   (event-thread :accessor agent-session-event-thread)
   (writer-thread :accessor agent-session-writer-thread))
  (:documentation "Session on one agent, reached over its link"))

(defmethod open-session ((kind (eql :agent)) receiver)
  "Session on a fresh agent in its own SBCL process, reporting to RECEIVER"
  (let ((agent-session (make-instance 'agent-session :link (ln:open-agent-process) :receiver receiver)))
    (setf (agent-session-event-thread agent-session)
          (th:start-thread "agent session events" (lambda () (event-loop agent-session)))
          (agent-session-writer-thread agent-session)
          (th:start-thread "agent session writer" (lambda () (writer-loop agent-session))))
    agent-session))

(defun agent-session-open-p (agent-session)
  "True until AGENT-SESSION's agent dies or something starts closing it"
  (eq :open (th:box-value (agent-session-phase agent-session))))

(defun agent-session-closing-p (agent-session)
  "True once something has started closing AGENT-SESSION"
  (member (th:box-value (agent-session-phase agent-session)) '(:closing :closed)))

(defun note-agent-gone (agent-session)
  "Move AGENT-SESSION from :OPEN to :GONE, since its agent died. :CLAIMED, or :ABANDONED when
it had already left :OPEN"
  (th:box-claim (agent-session-phase agent-session) :open :gone))

(defun gateway-only-event-p (message)
  "True when MESSAGE isn't an event the receiver should get from the agent: :pong, which
isn't an event at all, or :agent-gone, which only the gateway may say. Visitor code can write
anything to the socket, so a forged one is dropped here"
  (member (first message) '(:pong :agent-gone)))

(defun event-loop (agent-session)
  "Hand each agent message to a receive as an up event until it hangs up. Then report its gone.
A message WIRE-EVENT refuses the loop the same way. A session we closed ourselves never says so
since DELIVER drops everything once closing starts"
  (handler-case
      (loop for message = (w:read-message (ln:link-stream (agent-session-link agent-session)))
            until (eq message :end)
            unless (gateway-only-event-p message)
              do (deliver agent-session (wire-event message)))
    (error (condition)
      (when (agent-session-open-p agent-session)
        (format *error-output* "~&;; Agent session event thread stopped: ~A~%" condition))))
  (note-agent-gone agent-session)
  (deliver agent-session (make-agent-gone-event)))

(defun deliver (agent-session up-event)
  "Hand UP-EVENT to the receiver until something starts closing AGENT-SESSION. Receiver
error is logged and not fatal"
  (unless (agent-session-closing-p agent-session)
    (handler-case (receive-event (agent-session-receiver agent-session) up-event)
      (error (condition)
        (unless (agent-session-closing-p agent-session)
          (format *error-output* "~&;; Event receiver failed on ~S: ~A~%"
                  (type-of up-event) condition))))))

(defun writer-loop (agent-session)
  "Write each message from AGENT-SESSION's outbox to its agent until the phase leaves :OPEN.
A write that fails means the agent died: the session goes :GONE so later sends drop, and the
event thread reports it"
  (handler-case
      (loop while (agent-session-open-p agent-session)
            do (write-next-message agent-session))
    (error (condition)
      (when (eq (note-agent-gone agent-session) :claimed)
        (format *error-output* "~&;; Agent session writer stopped: ~A~%" condition)))))

(defun write-next-message (agent-session)
  "Write the oldest message in AGENT-SESSION's outbox, waiting up to a second for one.
:WROTE or :TIMED-OUT"
  (let ((message (th:mailbox-receive-within (agent-session-outbox agent-session) 1)))
    (cond ((eq message :timed-out) :timed-out)
          (t (w:write-message message (ln:link-stream (agent-session-link agent-session)))
             :wrote))))

(defun send-to-agent (agent-session message)
  "Queue MESSAGE for AGENT-SESSION's writer. :QUEUED, :DROPPED once the agent is gone or
closing has started, or :TOO-LONG if it's over the wire's limit (nothing gets queued)"
  (cond ((not (agent-session-open-p agent-session)) :dropped)
        ((w:text-too-long-p (w:message-text message)) :too-long)
        (t (th:mailbox-send (agent-session-outbox agent-session) message)
           :queued)))

(defmethod session-send ((agent-session agent-session) (down-message down-message))
  (send-to-agent agent-session (message-wire down-message)))

(defmethod session-close ((agent-session agent-session))
  "Kill AGENT-SESSION's agent and wait a few seconds each for its process and threads.
:CLOSED, :ALREADY-CLOSED when another caller closed it first, or :TIMED-OUT when something
didn't finish (a log line names it)"
  (ecase (th:box-step (agent-session-phase agent-session) #'step-start-close)
    (:abandoned :already-closed)
    (:claimed (finish-close agent-session))))

(defun step-start-close (phase)
  "The phase after a close is asked for in PHASE, and whether this close claimed it:
(values :CLOSING :CLAIMED) from :OPEN or :GONE, otherwise PHASE and :ABANDONED"
  (ecase phase
    ((:open :gone) (values :closing :claimed))
    ((:closing :closed) (values phase :abandoned))))

(defun finish-close (agent-session)
  "Close AGENT-SESSION's link, join its two threads and mark it :CLOSED. :CLOSED or :TIMED-OUT"
  (let ((waits (list (list "agent process" (ln:close-link (agent-session-link agent-session)))
                     (list "event thread" (th:join-within (agent-session-event-thread agent-session) 5))
                     (list "writer thread" (th:join-within (agent-session-writer-thread agent-session) 5)))))
    (th:box-claim (agent-session-phase agent-session) :closing :closed)
    (report-unfinished (unfinished-waits waits))))

(defun unfinished-waits (waits)
  "Names of the WAITS, (name outcome) pairs, whose outcome is :TIMED-OUT"
  (loop for (name outcome) in waits
        when (eq outcome :timed-out)
          collect name))

(defun report-unfinished (names)
  "Log NAMES, the waits closing gave up on. :CLOSED when there are none, otherwise :TIMED-OUT"
  (cond ((null names) :closed)
        (t (format *error-output* "~&;; Agent session close gave up waiting on: ~{~A~^, ~}~%" names)
           :timed-out)))
