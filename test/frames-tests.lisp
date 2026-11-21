;;;; test/frames-tests.lisp: frame surgery and input, against an in-process agent

(in-package #:to-restart-is-divine/tests)

(def-suite frames :in divine-tests)
(in-suite frames)

(defun index-of-frame-calling (opened name)
  "Index of the first frame in OPENED (a :level-opened message) that calls NAME, or NIL"
  (position-if (lambda (frame-text) (search (format nil ": (~A " name) frame-text))
               (getf (rest opened) :backtrace)))

(defun define-order (stream)
  "Define PRICE and ORDER in the agent on STREAM, skipping past the two :values messages"
  (send-to-agent stream '(:eval "(defun price (qty) (* qty 3)) (defun order (qty) (+ 1 (price qty)))"))
  (next-past-output stream)
  (next-past-output stream))

(test failing-frame-is-unwindable
  (call-with-agent
   (lambda (stream)
     (define-order stream)
     (send-to-agent stream '(:eval "(order \"twelve\")"))
     (let ((opened (next-past-output stream)))
       (is (member (index-of-frame-calling opened "PRICE") (getf (rest opened) :unwindable)))))))

(test locals-show-the-argument
  (call-with-agent
   (lambda (stream)
     (define-order stream)
     (send-to-agent stream '(:eval "(order \"twelve\")"))
     (let* ((opened (next-past-output stream))
            (price-index (index-of-frame-calling opened "PRICE")))
       (send-to-agent stream (list :locals (level-id-of opened) price-index))
       (is (equal (list :locals (level-id-of opened) price-index '(("QTY" "\"twelve\"")))
                  (next stream)))))))

(test eval-in-frame-assigns-a-local
  (call-with-agent
   (lambda (stream)
     (define-order stream)
     (send-to-agent stream '(:eval "(order \"twelve\")"))
     (let* ((opened (next-past-output stream))
            (price-index (index-of-frame-calling opened "PRICE")))
       (send-to-agent stream (list :eval-in-frame (level-id-of opened) price-index "(setf qty 12)"))
       (is (equal '(:values ("12")) (next stream)))
       (is (equal (list :locals (level-id-of opened) price-index '(("QTY" "12")))
                  (next stream)))))))

(test rerun-frame-uses-the-new-argument
  (call-with-agent
   (lambda (stream)
     (define-order stream)
     (send-to-agent stream '(:eval "(order \"twelve\")"))
     (let* ((opened (next-past-output stream))
            (price-index (index-of-frame-calling opened "PRICE")))
       (send-to-agent stream (list :eval-in-frame (level-id-of opened) price-index "(setf qty 12)"))
       (next stream)
       (next stream)
       (send-to-agent stream (list :rerun-frame (level-id-of opened) price-index))
       (is (equal (list :level-closed (level-id-of opened)) (next stream)))
       (is (equal '(:values ("37")) (next stream)))))))

(test return-from-frame-supplies-the-value
  (call-with-agent
   (lambda (stream)
     (define-order stream)
     (send-to-agent stream '(:eval "(order \"twelve\")"))
     (let* ((opened (next-past-output stream))
            (price-index (index-of-frame-calling opened "PRICE")))
       (send-to-agent stream (list :return-from-frame (level-id-of opened) price-index "100"))
       (is (equal (list :level-closed (level-id-of opened)) (next stream)))
       (is (equal '(:values ("101")) (next stream)))))))

(test read-line-asks-the-page
  (call-with-agent
   (lambda (stream)
     (send-to-agent stream '(:eval "(read-line)"))
     (is (equal '(:needs-input) (next-past-output stream)))
     (send-to-agent stream '(:input "hello"))
     (is (equal '(:input-done) (next stream)))
     (is (equal '(:values ("\"hello\"" "NIL")) (next stream))))))

(test store-value-asks-the-page
  (call-with-agent
   (lambda (stream)
     (send-to-agent stream '(:eval "(progn (defparameter *x* \"seven\") (check-type *x* integer) *x*)"))
     (let ((opened (next-past-output stream)))
       (send-to-agent stream (list :invoke-restart (level-id-of opened) (restart-index opened "STORE-VALUE")))
       (is (equal '(:needs-input) (next-past-output stream)))
       (send-to-agent stream '(:input "7"))
       (is (equal '(:input-done) (next stream)))
       (is (equal (list :level-closed (level-id-of opened)) (next-past-output stream)))
       (is (equal '(:values ("7")) (next-past-output stream)))))))

(test stale-frame-is-ignored
  (call-with-agent
   (lambda (stream)
     (send-to-agent stream '(:eval "(car 5)"))
     (let ((opened (next-past-output stream)))
       (send-to-agent stream (list :locals (level-id-of opened) 999))
       (send-to-agent stream '(:eval "(+ 1 1)"))
       (is (equal '(:values ("2")) (next stream)))))))

(test abort-ends-a-waiting-read
  (call-with-agent
   (lambda (stream)
     (send-to-agent stream '(:eval "(read-line)"))
     (is (equal '(:needs-input) (next-past-output stream)))
     (send-to-agent stream '(:interrupt))
     (let ((opened (next-past-output stream)))
       (send-to-agent stream (list :invoke-restart (level-id-of opened) (restart-index opened "ABORT")))
       (is (equal (list :level-closed (level-id-of opened)) (next stream)))
       (is (equal '(:input-done) (next stream)))))))
