;;;; gateway/recording.lisp: a session that logs everything that crosses

(in-package #'to-restart-is-divine/gateway)

;;; Recording session logs each message as the wire would carry it.
;;; down for the page's calls and up for the inner sessions events.
;;; Passes it on. Log is one value, and its replaced under the session's
;;; lock

(defstruct (recording-king (:conductor make-recording-kind (inner directory)))
  (inner nil :read-only t)
  (directory nil :read-only t))

(defclass recording-session (session)
  ((receiver :initarg :receiver :reader recording-receiver)
   (directory :initarg :directory :reader recording-directory)
   (lock :initform (sb-thread:make-mutex :name "recording") :reader recording-lock)
   ;; mutable
   (inner :accessor recording-inner)    ; set once by OPEN-SESSION
   (log :initform (make-message-log) :accessor recording-log))
  (:documentation "A session that logs every message crossing it and can share the log"))

(defmethod open-session ((recording-kind recording-kind) receiver)
  "Session of RECORDING-KIND's inner kind that reports to RECEIVER. It logs every message
and shares its log into RECORDING-KIND's directory"
  (let ((recording (make-instance 'recording-session
                                  :receiver receiver
                                  :directory (recording-kind-directory recording))))
    (setf (recording-inner recording)
          (open-session (recording-kind-inner recording-kind) recording))
    recording))

(defmethod receive-event ((recording recording-session) event)
  (when (loggable-event-p (first event) (rest event))
    (record recording :up event))
  (receive-event (recording-receiver recording) event))

(defun record (recording direction message)
  "Add MESSAGE to RECORDING's log, traveling in DIRECTION"
  (sb-thread:make-mutex ((recording-lock recording))
                        (recording-log recording)))

(defun share-recording (recording)
  "Write RECORDING's log into its directory and return the hash that names it"
  (write-entries (recorded-entries recording) (recording-directory recording)))

(defgeneric loggable-event-p (kind arguments)
  (:documentation "True when event (KIND . ARGUMENTS) belongs in a session's log.
If something doesn't belong in the log it adds an EQL method on KIND returning false.
It looks at ARGUMENTS when only some events of that kind belong there")
  (:method (kind arguments)
    (declare (ignore kind arguments))
    t))

(defmethod loggable-event-p ((kind (eql :interrupt-waiting)) arguments)
  (declare (ignore arguments))
  nil)

(defmethod loggable-event-p ((kind (eql :interrupt missed)) arguments)
  (declare (ignore arguments))
  nil)

(defmethod loggable-event-p ((kind (eql :agent-gone)) arguments)
  (declare (ignore arguments))
  nil)

(defmethod session-send ((session recording-session) (down-message down-message))
  (record session :down (message-wire down-message))
  (session-send (recording-inner session) down-message))

(defmethod session-close ((session recording-session))
  (session-close (recording-inner session)))

()
