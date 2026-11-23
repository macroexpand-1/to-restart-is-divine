;;;; test/frames-console-tests.lisp: the console values frame surgery adds

(in-package #:to-restart-is-divine/tests)

(def-suite frames-console :in divine-tests)
(in-suite frames-console)

(test locals-go-to-one-level
  (let* ((older (g:make-debug-level :id 1 :depth 1))
         (newer (g:make-debug-level :id 2 :depth 2))
         (levels (list newer older))
         (changed (g:fold-locals levels (g:make-frame-ref 2 3) '(("QTY" "12")))))
    (is (equal '(("QTY" "12")) (cdr (assoc 3 (g:debug-level-locals (first changed))))))
    (is (null (g:debug-level-locals (second changed))))
    (is (null (g:debug-level-locals newer)))))

(test unwindable-frames-are-looked-up
  (let ((level (g:make-debug-level :id 1 :depth 1 :unwindable '(0 3))))
    (is-true (g:frame-unwindable-in-p level 3))
    (is-false (g:frame-unwindable-in-p level 1))))

(test unreadable-frames-come-through
  (let* ((level (g:event-debug-level '(:level-opened :id 1 :depth 1 :unreadable (0))))
         (changed (first (g:fold-locals (list level) (g:make-frame-ref 1 2) '(("X" "1"))))))
    (is-false (g:frame-readable-in-p level 0))
    (is-true (g:frame-readable-in-p level 1))
    (is-false (g:frame-readable-in-p changed 0))))

(test a-lost-backtrace-comes-through
  (let* ((lost (g:event-debug-level '(:level-opened :id 1 :depth 1 :frames :lost)))
         (changed (first (g:fold-locals (list lost) (g:make-frame-ref 1 0) '()))))
    (is (eq :lost (g:debug-level-frames lost)))
    (is (eq :lost (g:debug-level-frames changed)))
    (is (eq :read (g:debug-level-frames (g:event-debug-level '(:level-opened :id 2 :depth 1)))))
    (signals error (g:event-debug-level '(:level-opened :id 3 :depth 1 :frames :maybe)))))

(test assignments-are-built-as-text
  (is (string= "(setf QTY 12)" (g:local-assignment "QTY" "12"))))
