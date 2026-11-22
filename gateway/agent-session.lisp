;;;; gateway/agent-session.lisp: a session backed by a real agent

(in-package #:to-restart-is-divine/gateway)

;;; Agent messages already come in the event shapes session.lisp lists, so they
;;; go to the receiver as-is. :pong isn't an event, and :agent-gone is the gateway's
;;; own to say, so both get dropped

(defclass agent-session (session)
  ((link :initarg :link :reader agent-session-link)
   (receiver :initarg :receiver :reader agent-session-receiver)
   (send-lock :initform (sb-thread:make-mutex :name "agent session send") :reader agent-session-send-lock)
   ;; mutable
   (closed :initform nil :accessor agent-session-closed-p)
   (event-thread :accessor agent-session-event-thread))
  (:documentation "Session on one agent, reached over its link"))

(defmethod open-session ((kind (eql :agent)) receiver)
  "Session on a fresh agent in its own SBCL process, reporting to RECEIVER"
  (let ((agent-session (make-instance 'agent-session :link (ln:open-agent-process) :receiver receiver)))
    (setf (agent-session-event-thread agent-session)
          (sb-thread:make-thread (lambda () (event-loop agent-session)) :name "agent session events"))
    agent-session))

(defun event-loop (agent-session)
  "Hand each agent message to receiver until the agent hangs up, then say it's gone.
A session we closed ourselves never says so, since DELIVER drops everything once closed"
  (handler-case
      (loop for message = (w:read-message (ln:link-stream (agent-session-link agent-session)))
            until (eq message :end)
            unless (gateway-only-event-p message)
              do (deliver agent-session message))
    (error (condition)
      (unless (agent-session-closed-p agent-session)
        (format *error-output* "~&;; Agent session event thread stopped: ~A~%" condition))))
  (deliver agent-session '(:agent-gone)))

(defun gateway-only-event-p (message)
  "True when MESSAGE isn't an event the receiver should get from the agent: :pong, which
isn't an event at all, or :agent-gone, which only the gateway may say. Visitor code can write
anything to the socket, so a forged one is dropped here"
  (member (first message) '(:pong :agent-gone)))

(defun deliver (agent-session event)
  "Hand EVENT to receiver unless AGENT-SESSION is closed. If receiver errors, we log it and move on"
  (unless (agent-session-closed-p agent-session)
    (handler-case (receive-event (agent-session-receiver agent-session) event)
      (error (condition)
        (format *error-output* "~&;; Event receiver failed on ~S: ~A~%" (first event) condition)))))

(defun send-to-agent (agent-session message)
  "Write MESSAGE to AGENT-SESSION's agent. :SENT, :DROPPED once the session's closed,
:TOO-LONG if it's over the wire's limit (nothing gets written), or :AGENT-GONE if the agent
died under us (the event loop tells the page about that)"
  (sb-thread:with-mutex ((agent-session-send-lock agent-session))
    (if (agent-session-closed-p agent-session)
        :dropped
        (handler-case
            (progn (w:write-message message (ln:link-stream (agent-session-link agent-session)))
                   :sent)
          (w:malformed-message () :too-long)
          (stream-error () :agent-gone)))))

(defmethod session-send ((agent-session agent-session) (down-message down-mesage))
  (send-to-agent agent-session (message-wire down-message)))

(defmethod session-eval ((agent-session agent-session) text)
  (send-to-agent agent-session (list :eval text)))

(defmethod session-invoke-restart ((agent-session agent-session) id index)
  (send-to-agent agent-session (list :invoke-restart id index)))

(defmethod session-interrupt ((agent-session agent-session))
  (send-to-agent agent-session (list :interrupt)))

(defmethod session-frame-locals ((agent-session agent-session) id frame-index)
  (send-to-agent agent-session (list :locals id frame-index)))

(defmethod session-eval-in-frame ((agent-session agent-session) id frame-index text)
  (send-to-agent agent-session (list :eval-in-frame id frame-index text)))

(defmethod session-return-from-frame ((agent-session agent-session) id frame-index text)
  (send-to-agent agent-session (list :return-from-frame id frame-index text)))

(defmethod session-rerun-frame ((agent-session agent-session) id frame-index)
  (send-to-agent agent-session (list :rerun-frame id frame-index)))

(defmethod session-input ((agent-session agent-session) text)
  (send-to-agent agent-session (list :input text)))

(defmethod session-close ((agent-session agent-session))
  (sb-thread:with-mutex ((agent-session-send-lock agent-session))
    (setf (agent-session-closed-p agent-session) t))
  (ln:close-link (agent-session-link agent-session))
  (sb-thread:join-thread (agent-session-event-thread agent-session) :default nil))
