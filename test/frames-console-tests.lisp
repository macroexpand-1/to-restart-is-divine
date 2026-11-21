;;;; test/frames-console-tests.lisp: the console values frame surgery adds

(in-package #:to-restart-is-divine/tests)

(def-suite frames-console :in divine-tests)
(in-suite frames-console)

(test locals-go-to-one-level
  (let* ((older (g:make-debug-level :id 1 :depth 1))
         (newer (g:make-debug-level :id 2 :depth 2))
         (levels (list newer older))
         (changed (g:fold-locals levels 2 3 '(("QTY" "12")))))
    (is (equal '(("QTY" "12")) (cdr (assoc 3 (g:debug-level-locals (first changed))))))
    (is (null (g:debug-level-locals (second changed))))
    (is (null (g:debug-level-locals newer)))))

(test unwindable-frames-are-looked-up
  (let ((level (g:make-debug-level :id 1 :depth 1 :unwindable '(0 3))))
    (is-true (g:frame-unwindable-in-p level 3))
    (is-false (g:frame-unwindable-in-p level 1))))

(test assignments-are-built-as-text
  (is (string= "(setf QTY 12)" (g:local-assignment "QTY" "12"))))
