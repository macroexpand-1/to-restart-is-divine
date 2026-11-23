;;;; test/event-capture.lisp: a session receiver that hangs onto every event it gets

(in-package #:to-restart-is-divine/tests)

(defstruct (event-capture (:constructor make-event-capture ()))
  (events (th:make-box '()) :read-only t)) ; wire lists, oldest first, not yet taken

(defmethod g:receive-event ((event-capture event-capture) (up-event g:up-event))
  (th:box-update (event-capture-events event-capture)
                 (lambda (events) (append events (list (g:event-wire up-event))))))

(defun captured-events (event-capture)
  "Every event EVENT-CAPTURE holds, oldest first"
  (th:box-value (event-capture-events event-capture)))

(defun next-captured-event (event-capture)
  "Take the oldest event from EVENT-CAPTURE, waiting up to 10 seconds for one, or :TIMED-OUT"
  (ecase (th:wait-until-within 10 (lambda () (captured-events event-capture)))
    (:done (th:box-step (event-capture-events event-capture)
                        (lambda (events) (values (rest events) (first events)))))
    (:timed-out :timed-out)))
