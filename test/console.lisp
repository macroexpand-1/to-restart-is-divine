;;;; test/console.lisp: history and debugger-stack tests

(in-package #:to-restart-is-divine/tests)

(def-suite console-history :in divine-tests)
(in-suite console-history)

(defun recalled (history direction)
  "The line a step in DIRECTION shows from HISTORY, with nothing typed"
  (nth-value 1 (g:step-recall history direction "")))

(test previous-shows-pushed-line
  (is (equal "a" (recalled (g:fold-submitted-line (g:make-history) "a") :older))))

(test previous-stops-at-oldest
  (let ((at-oldest (g:step-recall (g:fold-submitted-line (g:make-history) "a") :older "")))
    (multiple-value-bind (history line) (g:step-recall at-oldest :older "a")
      (is (eq :none line))
      (is (eq at-oldest history)))))

(test next-without-recalling-is-none
  (is (eq :none (recalled (g:fold-submitted-line (g:make-history) "a") :newer))))

(test next-past-newest-is-empty
  (let ((recalling (g:step-recall (g:fold-submitted-line (g:make-history) "a") :older "")))
    (multiple-value-bind (history line) (g:step-recall recalling :newer "a")
      (is (equal "" line))
      (is (eq :none (g:history-cursor history))))))

(test next-past-newest-restores-draft
  (let* ((history (reduce #'g:fold-submitted-line '("a" "b") :initial-value (g:make-history)))
         (at-b (g:step-recall history :older "(car"))
         (at-a (g:step-recall at-b :older "b"))
         (back-at-b (g:step-recall at-a :newer "a")))
    (is (equal "(car" (nth-value 1 (g:step-recall back-at-b :newer "b"))))))

(test unknown-direction-is-refused
  (signals error (g:step-recall (g:make-history) :sideways "")))

(test blank-and-repeat-not-kept
  (let ((history (reduce #'g:fold-submitted-line '("a" "  " "a") :initial-value (g:make-history))))
    (is (equal '("a") (g:history-entries history)))))

(test push-resets-cursor
  (let* ((recalling (g:step-recall (g:fold-submitted-line (g:make-history) "a") :older ""))
         (pushed (g:fold-submitted-line recalling "b")))
    (is (equal "b" (recalled pushed :older)))))

(def-suite console-debugger :in divine-tests)
(in-suite console-debugger)

(defun level (id)
  "A minimal debug-level with ID"
  (g:make-debug-level :id id :depth id))

(test event-debug-level-reads-fields
  (let ((level (g:event-debug-level
                '(:level-opened :id 7 :depth 2 :type "TYPE-ERROR" :report "bad"
                  :restarts ((0 "ABORT" "Top.")) :backtrace ("0: (CAR 5)") :extra t))))
    (is (= 7 (g:debug-level-id level)))
    (is (= 2 (g:debug-level-depth level)))
    (is (equal "TYPE-ERROR" (g:debug-level-type level)))
    (is (equal "bad" (g:debug-level-report level)))
    (is (equal '((0 "ABORT" "Top.")) (g:debug-level-restarts level)))
    (is (equal '("0: (CAR 5)") (g:debug-level-backtrace level)))))

(test drop-older-drops-newer
  (let ((levels (g:push-level (g:push-level '() (level 1)) (level 2))))
    (is (null (g:drop-level levels 1)))))

(test drop-unknown-changes-nothing
  (let ((levels (g:push-level '() (level 1))))
    (is (eq levels (g:drop-level levels 99)))))

(test restart-label-format
  (is (equal "1: [ABORT] Return to top level."
             (g:restart-label '(1 "ABORT" "Return to top level.")))))

(test no-values-note
  (is (equal '(";; No values") (g:values-lines '()))))
