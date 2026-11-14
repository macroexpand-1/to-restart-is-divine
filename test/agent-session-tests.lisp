;;;; test/agent-session-tests.lisp: A real agent to test against

(in-package #:to-restart-is-divine/tests)

(def-suite agent-session :in divine-tests)
(in-suite agent-session)

(defun abort-index (debug-level)
  "The index of ABORT among DEBUG-LEVEL's restarts"
  (first (find "ABORT" (g:debug-level-restarts debug-level) :key #'second :test #'string=)))

(test agent-session-recovers-from-an-error
  (let* ((event-capture (make-event-capture))
         (session (g:open-session :agent event-capture)))
    (unwind-protect
         (progn
           (is (equal '(:ready) (next-captured-event event-capture)))
           (g:session-eval session "(car 5)")
           (let ((level (g:event-debug-level (next-captured-event event-capture))))
             (g:session-invoke-restart session (g:debug-level-id level) (abort-index level))
             (is (equal (list :level-closed (g:debug-level-id level))
                        (next-captured-event event-capture)))))
      (g:session-close session))))

(test closed-session-ignores-calls
  (let ((session (g:open-session :agent (make-event-capture))))
    (g:session-close session)
    (finishes (g:session-eval session "(+ 1 2)"))
    (finishes (g:session-interrupt session))))
