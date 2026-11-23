;;;; gateway/recording.lisp: a session that logs everything that crosses

(in-package #:to-restart-is-divine/gateway)

;;; Recording session logs each message as the wire would carry it.
;;; down for the page's calls and up for the inner sessions events.
;;; Passes it on. Log is one value in a box, replaced whole by compare-and-swap

(defstruct (recording-kind (:constructor make-recording-kind (inner directory)))
  (inner nil :read-only t)
  (directory nil :read-only t))

(defclass recording-session (session)
  ((receiver :initarg :receiver :reader recording-receiver)
   (directory :initarg :directory :reader recording-directory)
   (log :initform (th:make-box (make-message-log)) :reader recording-log-box)
   ;; mutable
   (inner :accessor recording-inner))   ; set once by OPEN-SESSION
  (:documentation "A session that logs every message crossing it and can share the log"))

(defmethod open-session ((recording-kind recording-kind) receiver)
  "Session of RECORDING-KIND's inner kind that reports to RECEIVER. It logs every message
and shares its log into RECORDING-KIND's directory"
  (let ((recording (make-instance 'recording-session
                                  :receiver receiver
                                  :directory (recording-kind-directory recording-kind))))
    (setf (recording-inner recording)
          (open-session (recording-kind-inner recording-kind) recording))
    recording))

(defmethod receive-event ((recording recording-session) (up-event up-event))
  (when (loggable-event-p up-event)
    (record recording :up (event-wire up-event)))
  (receive-event (recording-receiver recording) up-event))

(defun record (recording direction message)
  "Add MESSAGE to RECORDING's log, traveling in DIRECTION"
  (th:box-update (recording-log-box recording)
                 (lambda (message-log) (log-append message-log direction message))))

(defun recorded-entries (recording)
  "RECORDING's log so far, oldest first"
  (log-entries (th:box-value (recording-log-box recording))))

(defgeneric loggable-event-p (up-event)
  (:documentation "True when UP-EVENT belongs in a session's log. If an event class doesn't
belong in the log, it adds a method on that class returning false. It reads the event's slots
when only some events of that class belong there")
  (:method ((up-event up-event))
    (declare (ignore up-event))
    t))

(defmethod loggable-event-p ((interrupt-waiting-event interrupt-waiting-event))
  (declare (ignore interrupt-waiting-event))
  nil)

(defmethod loggable-event-p ((interrupt-missed-event interrupt-missed-event))
  (declare (ignore interrupt-missed-event))
  nil)

(defmethod loggable-event-p ((agent-gone-event agent-gone-event))
  (declare (ignore agent-gone-event))
  nil)

(defmethod session-send ((session recording-session) (down-message down-message))
  (record session :down (message-wire down-message))
  (session-send (recording-inner session) down-message))

(defmethod session-close ((session recording-session))
  (session-close (recording-inner session)))
