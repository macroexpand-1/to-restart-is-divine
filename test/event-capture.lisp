;;;; test/event-capture.lisp: a session receiver that hangs onto every event it gets

(in-package #:to-restart-is-divine/tests)

(defstruct (event-capture (:constructor make-event-capture ()))
  (lock (sb-thread:make-mutex :name "event capture") :read-only t)
  (arrived (sb-thread:make-waitqueue :name "event capture") :read-only t)
  ;; mutable
  (events '()))                         ; oldest first, not yet taken

(defmethod g:receive-event ((event-capture event-capture) event)
  (sb-thread:with-mutex ((event-capture-lock event-capture))
    (setf (event-capture-events event-capture)
          (append (event-capture-events event-capture) (list event)))
    (sb-thread:condition-broadcast (event-capture-arrived event-capture))))

(defun captured-events (event-capture)
  "Every event EVENT-CAPTURE holds, oldest first"
  (sb-thread:with-mutex ((event-capture-lock event-capture))
    (copy-list (event-capture-events event-capture))))

(defun next-captured-event (event-capture)
  "Take the oldest event from EVENT-CAPTURE, waiting up to 10 seconds for one, or :TIMEOUT"
  (sb-thread:with-mutex ((event-capture-lock event-capture))
    (wait-for-captured-event event-capture)
    (if (event-capture-events event-capture)
        (pop (event-capture-events event-capture))
        :timeout)))

(defun wait-for-captured-event (event-capture)
  "Wait up to 10 seconds for EVENT-CAPTURE to have an event. Caller holds its lock"
  (loop repeat 10
        while (null (event-capture-events event-capture))
        do (wait-one-second event-capture)))

(defun wait-one-second (event-capture)
  "Wait up to a second for EVENT-CAPTURE to hear about an event. Caller holds its lock
and still holds it after, since a timed-out CONDITION-WAIT comes back without it and we grab it again"
  (let ((lock (event-capture-lock event-capture)))
    (unless (sb-thread:condition-wait (event-capture-arrived event-capture) lock :timeout 1)
      (sb-thread:grab-mutex lock))))
