;;;; test/agent-session-tests.lisp: a real agent to test against

(in-package #:to-restart-is-divine/tests)

(def-suite agent-session :in divine-tests)
(in-suite agent-session)

(defun abort-index (debug-level)
  "Index of ABORT among DEBUG-LEVEL's restarts"
  (first (find "ABORT" (g:debug-level-restarts debug-level) :key #'second :test #'string=)))

(test agent-session-recovers-from-an-error
  (let* ((event-capture (make-event-capture))
         (session (g:open-session :agent event-capture)))
    (unwind-protect
         (progn
           (is (equal '(:ready) (next-captured-event event-capture)))
           (g:session-send session (g:make-eval-message "(car 5)"))
           (let ((level (g:event-debug-level (next-captured-event event-capture))))
             (g:session-send session (g:make-invoke-restart-message (g:debug-level-id level)
                                                                     (abort-index level)))
             (is (equal (list :level-closed (g:debug-level-id level))
                        (next-captured-event event-capture)))))
      (g:session-close session))))

(test a-dying-agent-says-so
  (let* ((event-capture (make-event-capture))
         (session (g:open-session :agent event-capture)))
    (unwind-protect
         (progn
           (is (equal '(:ready) (next-captured-event event-capture)))
           (g:session-send session (g:make-eval-message "(sb-ext:exit :abort t)"))
           (is (equal '(:agent-gone) (next-captured-event event-capture)))
           ;; nothing writes to a dead agent, so a send drops rather than queue
           (is (eq :dropped (g:session-send session (g:make-eval-message "(+ 1 2)"))))
           (is (eq :closed (g:session-close session))))
      (g:session-close session))))

(test closing-starts-once-from-open-or-gone
  (is (equal '(:closing :claimed) (multiple-value-list (g::step-start-close :open))))
  (is (equal '(:closing :claimed) (multiple-value-list (g::step-start-close :gone))))
  (is (equal '(:closing :abandoned) (multiple-value-list (g::step-start-close :closing))))
  (is (equal '(:closed :abandoned) (multiple-value-list (g::step-start-close :closed)))))

(test the-agent-cannot-say-it-is-gone
  ;; visitor code can write anything to the socket, so only the gateway makes :agent-gone
  (is-true (g::gateway-only-event-p '(:agent-gone)))
  (is-true (g::gateway-only-event-p '(:pong)))
  (is-false (g::gateway-only-event-p '(:output "x"))))

(test a-line-too-long-to-send-is-refused
  (let* ((event-capture (make-event-capture))
         (session (g:open-session :agent event-capture)))
    (unwind-protect
         (progn
           (is (equal '(:ready) (next-captured-event event-capture)))
           (is (eq :too-long (g:session-send session
                                             (g:make-eval-message
                                              (make-string (1+ w:*max-message-length*)
                                                           :initial-element #\a)))))
           (g:session-send session (g:make-eval-message "(+ 1 2)"))
           (is (equal '(:values ("3")) (next-captured-event event-capture))))
      (g:session-close session))))

(test closing-a-session-is-not-the-agent-dying
  (let* ((event-capture (make-event-capture))
         (session (g:open-session :agent event-capture)))
    (is (equal '(:ready) (next-captured-event event-capture)))
    (g:session-close session)
    (is (null (captured-events event-capture)))))

(test closed-session-ignores-calls
  (let ((session (g:open-session :agent (make-event-capture))))
    (g:session-close session)
    (finishes (g:session-send session (g:make-eval-message "(+ 1 2)")))
    (finishes (g:session-send session (g:make-interrupt-message)))))
