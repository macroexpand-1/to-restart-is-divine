;;;; test/fake-session-tests.lisp: the fake session keeps the session protocol

(in-package #:to-restart-is-divine/tests)

(defvar *never-set*)

(def-suite fake-session :in divine-tests)
(in-suite fake-session)

(defun fake-with-captured-events ()
  "Fake session plus the event capture it reports to"
  (let ((event-capture (make-event-capture)))
    (values (g:open-session :fake event-capture) event-capture)))

(test fake-error-opens-level-one
  (multiple-value-bind (session event-capture) (fake-with-captured-events)
    (g:session-send session (g:make-eval-message "(car 5)"))
    (let ((level (g:event-debug-level (first (captured-events event-capture)))))
      (is (= 1 (length (captured-events event-capture))))
      (is (= 1 (g:debug-level-depth level)))
      (is (plusp (length (g:debug-level-restarts level)))))))

(test fake-abort-closes-every-level
  (multiple-value-bind (session event-capture) (fake-with-captured-events)
    (g:session-send session (g:make-eval-message "(car 5)"))
    (g:session-send session (g:make-eval-message "(/ 1 0)"))
    (g:session-send session (g:make-invoke-restart-message 2 (fake-abort-index *fake-script*)))
    (is (equal '((:level-closed 2) (:level-closed 1)) (subseq (captured-events event-capture) 2)))))

(test retry-closes-own-level
  (multiple-value-bind (session event-capture) (fake-with-captured-events)
    (g:session-send session (g:make-eval-message "(car 5)"))
    (g:session-send session (g:make-eval-message "(/ 1 0)"))
    (g:session-send session (g:make-invoke-restart-message 2 0))
    (is (equal '((:level-closed 2)) (subseq (captured-events event-capture) 2)))))

(test unscripted-text-is-not-evaluated
  (multiple-value-bind (session event-capture) (fake-with-captured-events)
    (g:session-send session (g:make-eval-message "#.(setf *never-set* t)"))
    (is (eq :output (first (first (captured-events event-capture)))))
    (is (equal '(:values ()) (second (captured-events event-capture))))
    (is (not (boundp '*never-set*)))))
