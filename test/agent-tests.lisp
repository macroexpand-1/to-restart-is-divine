;;;; test/agent-tests.lisp: the in-process agent driven over a real socket

(in-package #:to-restart-is-divine/tests)

(def-suite agent :in divine-tests)
(in-suite agent)

(defun next (stream)
  "Next message on STREAM, :END, or :TIMEOUT (plus a failed check) after five seconds"
  (handler-case (sb-sys:with-deadline (:seconds 5)
                  (w:read-message stream))
    (sb-sys:deadline-timeout ()
      (fail "No message within five seconds.")
      :timeout)))

(defun next-past-output (stream)
  "Next message on STREAM that isn't :output"
  (loop for message = (next stream)
        while (and (consp message) (eq :output (first message)))
        finally (return message)))

(defun send-to-agent (stream message)
  "Send MESSAGE to the agent on STREAM"
  (w:write-message message stream))

(defun level-id-of (opened)
  "Level id of OPENED (a :level-opened message)"
  (getf (rest opened) :id))

(defun restart-index (opened name)
  "Index of the first restart in OPENED called NAME"
  (first (find name (getf (rest opened) :restarts) :key #'second :test #'string=)))

(defun call-with-agent (function)
  "Call FUNCTION with gateway's end of an in-process agent once it says :ready.
Afterward, hang up and wait for agent's threads to finish"
  (let* ((directory (ln:create-socket-directory))
         (path (ln:socket-path directory))
         (listener (ln:make-agent-listener path))
         (agent (a:start-agent path))
         (stream (ln:accept-agent-stream listener :timeout 5)))
    (sb-bsd-sockets:socket-close listener)
    (delete-file path)
    (uiop:delete-empty-directory directory)
    (unwind-protect
         (progn
           (is (equal '(:ready) (next stream)))
           (funcall function stream))
      (close stream :abort t)
      (is (eq :stopped (a:wait-for-agent agent))))))

(test ping-answers-pong
  (call-with-agent
   (lambda (stream)
     (send-to-agent stream '(:ping))
     (is (equal '(:pong) (next stream))))))

(test unknown-kind-stops-the-reader
  (call-with-agent
   (lambda (stream)
     (send-to-agent stream '(:no-such-kind))
     (is (eq :end (next stream))))))

(test eval-sends-printed-values
  (call-with-agent
   (lambda (stream)
     (send-to-agent stream '(:eval "(+ 1 2)"))
     (is (equal '(:values ("3")) (next stream)))
     (send-to-agent stream '(:eval "(values)"))
     (is (equal '(:values ()) (next stream))))))

(test output-precedes-values
  (call-with-agent
   (lambda (stream)
     (send-to-agent stream '(:eval "(princ \"hi\")"))
     (is (equal '(:output "hi") (next stream)))
     (is (equal '(:values ("\"hi\"")) (next stream))))))

(test forms-evaluate-in-turn
  (call-with-agent
   (lambda (stream)
     (send-to-agent stream '(:eval "(+ 1 2) (* * 2)"))
     (is (equal '(:values ("3")) (next stream)))
     (is (equal '(:values ("6")) (next stream))))))

(test error-opens-level-one
  (call-with-agent
   (lambda (stream)
     (send-to-agent stream '(:eval "(car 5)"))
     (destructuring-bind (kind &key depth type restarts backtrace &allow-other-keys) (next stream)
       (is (eq :level-opened kind))
       (is (= 1 depth))
       (is (equal "TYPE-ERROR" type))
       (is (equal '(("RETRY" "Evaluate the form again.") ("ABORT" "Return to top level."))
                  (mapcar #'rest (last restarts 2))))
       (is (search "CAR" (first backtrace)))))))

(test abort-returns-to-top-level
  (call-with-agent
   (lambda (stream)
     (send-to-agent stream '(:eval "(car 5)"))
     (let ((opened (next stream)))
       (send-to-agent stream (list :invoke-restart (level-id-of opened) (restart-index opened "ABORT")))
       (is (equal (list :level-closed (level-id-of opened)) (next stream))))
     (send-to-agent stream '(:eval "(+ 1 2)"))
     (is (equal '(:values ("3")) (next stream))))))

(test abort-closes-every-level
  (call-with-agent
   (lambda (stream)
     (send-to-agent stream '(:eval "(car 5)"))
     (let ((outer (next stream)))
       (send-to-agent stream '(:eval "(/ 1 0)"))
       (let ((inner (next stream)))
         (is (= 2 (getf (rest inner) :depth)))
         (send-to-agent stream (list :invoke-restart (level-id-of inner) (restart-index inner "ABORT")))
         (is (equal (list :level-closed (level-id-of inner)) (next stream)))
         (is (equal (list :level-closed (level-id-of outer)) (next stream))))))))

(test inner-restart-closes-its-own-level
  (call-with-agent
   (lambda (stream)
     (send-to-agent stream '(:eval "(car 5)"))
     (let ((outer (next stream)))
       (send-to-agent stream '(:eval "(restart-case (error \"x\") (retreat () :report \"Back off.\" 42))"))
       (let ((inner (next stream)))
         (send-to-agent stream (list :invoke-restart (level-id-of inner) (restart-index inner "RETREAT")))
         (is (equal (list :level-closed (level-id-of inner)) (next stream)))
         (is (equal '(:values ("42")) (next stream)))
         (send-to-agent stream (list :invoke-restart (level-id-of outer) (restart-index outer "ABORT")))
         (is (equal (list :level-closed (level-id-of outer)) (next stream))))))))

(test break-enters-the-agent-debugger
  (call-with-agent
   (lambda (stream)
     (send-to-agent stream '(:eval "(break)"))
     (let ((opened (next stream)))
       (is (eq :level-opened (first opened)))
       (is (search "(BREAK)" (first (getf (rest opened) :backtrace))))
       (is (eq :read (getf (rest opened) :frames)))
       (is (string-equal "break" (getf (rest opened) :report)))
       (send-to-agent stream (list :invoke-restart (level-id-of opened) (restart-index opened "CONTINUE")))
       (is (equal (list :level-closed (level-id-of opened)) (next stream)))
       (is (equal '(:values ("NIL")) (next stream)))))))

(test a-huge-value-comes-back-cut
  (call-with-agent
   (lambda (stream)
     (send-to-agent stream '(:eval "(make-string 2000000 :initial-element #\\a)"))
     (destructuring-bind (kind (text)) (next stream)
       (is (eq :values kind))
       (is (< (length text) 5000))
       (is (search "[cut, 2000002 characters]" text)))
     (send-to-agent stream '(:eval "(+ 1 1)"))
     (is (equal '(:values ("2")) (next stream))))))

(test an-error-over-a-huge-value-still-opens-a-level
  (call-with-agent
   (lambda (stream)
     (send-to-agent stream '(:eval "(car (make-string 2000000 :initial-element #\\a))"))
     (let ((opened (next stream)))
       (is (eq :level-opened (first opened)))
       (is (< (length (getf (rest opened) :report)) 5000))
       (send-to-agent stream (list :invoke-restart (level-id-of opened) (restart-index opened "ABORT")))
       (is (equal (list :level-closed (level-id-of opened)) (next stream)))))))

(test too-many-values-send-a-stand-in
  ;; each value is under the cut, but together they're over the wire's limit
  (call-with-agent
   (lambda (stream)
     (send-to-agent stream '(:eval "(values-list (make-list 300 :initial-element (make-string 3990 :initial-element #\\a)))"))
     (destructuring-bind (kind (text)) (next stream)
       (is (eq :values kind))
       (is (search "too long to send" text)))
     (send-to-agent stream '(:eval "(+ 1 1)"))
     (is (equal '(:values ("2")) (next stream))))))

(test a-level-too-long-to-send-opens-without-frames
  (let ((message (list :level-opened :id 1 :depth 1 :type "ERROR" :report "boom"
                       :restarts '((0 "ABORT" "Return to top level."))
                       :frames :read :backtrace '("0: (F)") :unwindable '(0) :unreadable '())))
    (is (equal (list :level-opened :id 1 :depth 1 :type "ERROR" :report "boom"
                     :restarts '((0 "ABORT" "Return to top level."))
                     :frames :lost :backtrace '() :unwindable '() :unreadable '())
               (a::stand-in-message message)))))

(test interrupt-opens-a-level-over-a-running-loop
  (call-with-agent
   (lambda (stream)
     ;; SPIN gets compiled by the DEFUN, so by the time "go" shows up we're in its loop.
     ;; Interrupting while SBCL is still compiling can land inside malloc and kill the image
     (send-to-agent stream '(:eval "(defun spin () (princ \"go\") (finish-output) (loop))"))
     (next-past-output stream)
     (send-to-agent stream '(:eval "(spin)"))
     (is (equal '(:output "go") (next stream)))
     (send-to-agent stream '(:interrupt))
     (let ((opened (next stream)))
       (is (equal "TO-RESTART-IS-DIVINE/AGENT:INTERRUPTED" (getf (rest opened) :type)))
       (is (equal "CONTINUE" (second (first (getf (rest opened) :restarts)))))
       (send-to-agent stream (list :invoke-restart (level-id-of opened) (restart-index opened "ABORT")))
       (is (equal (list :level-closed (level-id-of opened)) (next stream)))))))

(defun interrupt-spin-at-once (stream)
  "Run SPIN, Interrupt it with no pause, ABORT the level. True when it all went as it should"
  (send-to-agent stream '(:eval "(spin)"))
  (next stream)
  (send-to-agent stream '(:interrupt))
  (let ((opened (next-past-output stream)))
    (send-to-agent stream (list :invoke-restart (level-id-of opened) (restart-index opened "ABORT")))
    (and (eq :level-opened (first opened))
         (equal (list :level-closed (level-id-of opened)) (next-past-output stream)))))

;;; A press that lands opens a level, and the next press can come right after. It mustn't get
;;; mistaken for the first one still trying and dropped
(test interrupt-right-after-a-level-closes
  (call-with-agent
   (lambda (stream)
     (send-to-agent stream '(:eval "(defun spin () (princ \"go\") (finish-output) (loop))"))
     (next-past-output stream)
     (is (= 30 (loop repeat 30 count (interrupt-spin-at-once stream)))))))

(test interrupted-frame-is-unreadable
  (call-with-agent
   (lambda (stream)
     (send-to-agent stream '(:eval "(defun spin () (princ \"go\") (finish-output) (loop))"))
     (next-past-output stream)
     (send-to-agent stream '(:eval "(spin)"))
     (next stream)
     (send-to-agent stream '(:interrupt))
     (let* ((opened (next-past-output stream))
            (index (position-if (lambda (line) (search "args unreadable" line))
                                (getf (rest opened) :backtrace))))
       (is (integerp index))
       (is (equal (list index) (getf (rest opened) :unreadable)))
       (is (not (member index (getf (rest opened) :unwindable))))
       (send-to-agent stream (list :locals (level-id-of opened) index))
       (send-to-agent stream '(:eval "(+ 1 1)"))
       (is (equal '(:values ("2")) (next-past-output stream)))))))

(test interrupt-breaks-a-sleep
  (call-with-agent
   (lambda (stream)
     (send-to-agent stream '(:eval "(defun nap () (princ \"go\") (finish-output) (sleep 30.0))"))
     (next-past-output stream)
     (send-to-agent stream '(:eval "(nap)"))
     (is (equal '(:output "go") (next stream)))
     (sleep 0.2)
     (send-to-agent stream '(:interrupt))
     (let ((opened (next-past-output stream)))
       (is (equal "TO-RESTART-IS-DIVINE/AGENT:INTERRUPTED" (getf (rest opened) :type)))
       (send-to-agent stream (list :invoke-restart (level-id-of opened) (restart-index opened "ABORT")))
       (is (equal (list :level-closed (level-id-of opened)) (next stream)))))))

(defparameter *busy-workloads*
  (list "(compile nil (quote (lambda (x) (list x (* x 2)))))"
        "(let ((name (gensym))) (eval (list (quote defclass) name () (list (list (quote a) :initarg :a)))) (make-instance name :a 1))"
        "(let ((table (make-hash-table :synchronized t))) (dotimes (i 5000) (setf (gethash i table) i)))"
        "(let ((package (make-package (gensym) :use ()))) (dotimes (i 2000) (intern (format nil \"S~D\" i) package)) (delete-package package))"
        "(make-list 100000)"
        "(sort (loop repeat 2000 collect (random 1000)) (function <))")
  "Visitor code that keeps SBCL busy in its own internals: compiler, CLOS, hash tables,
packages, the GC and sort. Each gets run in a loop and Interrupted")

(defun interrupt-workload (stream workload)
  "Run WORKLOAD in a loop as visitor code, Interrupt it, and ABORT the level.
True when it all went as it should"
  (send-to-agent stream (list :eval (format nil "(progn (princ \"go\") (finish-output) (loop ~A))" workload)))
  (next stream)
  (sleep (random 0.02))
  (send-to-agent stream '(:interrupt))
  (let ((opened (next-past-output stream)))
    (send-to-agent stream (list :invoke-restart (level-id-of opened) (restart-index opened "ABORT")))
    (and (equal "TO-RESTART-IS-DIVINE/AGENT:INTERRUPTED" (getf (rest opened) :type))
         (equal (list :level-closed (level-id-of opened)) (next-past-output stream)))))

;;; About one Interrupt in eight lands in C while SBCL compiles. Before Interrupt backed off
;;; from C, this killed the whole image within a few rounds (malloc's lock left held).
;;; The other workloads land in SBCL's own Lisp, which we can only stress, not prove safe
(test interrupt-lands-safely-in-busy-code
  (call-with-agent
   (lambda (stream)
     (dolist (workload *busy-workloads*)
       (is (= 5 (loop repeat 5 count (interrupt-workload stream workload))))))))

(defparameter *foreign-wait*
  "(sb-alien:with-alien ((thread sb-alien:unsigned-long))
     (sb-alien:alien-funcall
      (sb-alien:extern-alien \"pthread_create\"
                             (function sb-alien:int (* sb-alien:unsigned-long) sb-alien:unsigned-long
                                       sb-alien:system-area-pointer sb-alien:unsigned-long))
      (sb-alien:addr thread) 0 (sb-sys:foreign-symbol-sap \"usleep\") 1500000)
     (sb-alien:alien-funcall
      (sb-alien:extern-alien \"pthread_join\" (function sb-alien:int sb-alien:unsigned-long sb-alien:unsigned-long))
      thread 0))"
  "Visitor code that sits in C for 1.5 seconds and doesn't come back early when a signal
lands. pthread_join on a thread that runs usleep does that; usleep alone would just return")

(test interrupt-waits-out-foreign-code
  (call-with-agent
   (lambda (stream)
     ;; compiled up front, so the Interrupt finds us in the C and not in the compiler.
     ;; The loop after it is where the Interrupt lands once the C returns
     (send-to-agent stream (list :eval (format nil "(defun wait-in-c () (princ \"go\") (finish-output) ~A (loop))" *foreign-wait*)))
     (next-past-output stream)
     (send-to-agent stream '(:eval "(wait-in-c)"))
     (next stream)
     (loop repeat 3 do (send-to-agent stream '(:interrupt)))
     ;; three presses, one Interrupt: one note, then one level once the C returns
     (is (equal '(:interrupt-waiting) (next-past-output stream)))
     (let ((opened (next-past-output stream)))
       (is (eq :level-opened (first opened)))
       (send-to-agent stream (list :invoke-restart (level-id-of opened) (restart-index opened "ABORT")))
       (is (equal (list :level-closed (level-id-of opened)) (next-past-output stream))))
     (send-to-agent stream '(:eval "(+ 1 1)"))
     (is (equal '(:values ("2")) (next-past-output stream))))))

(test hang-up-waits-out-foreign-code
  ;; CALL-WITH-AGENT hangs up on the way out and checks the eval thread :STOPPED. Here it
  ;; hangs up while the C has most of its 1.5 seconds to go, so stopping takes over a second
  (let ((start (get-internal-real-time)))
    (call-with-agent
     (lambda (stream)
       (send-to-agent stream (list :eval (format nil "(defun wait-in-c () ~A (loop))" *foreign-wait*)))
       (next-past-output stream)
       (send-to-agent stream '(:eval "(progn (princ \"go\") (finish-output) (wait-in-c))"))
       (next stream)))
    (is (< 1.0 (/ (- (get-internal-real-time) start) internal-time-units-per-second)))))

(test holding-an-agent-lock-is-not-a-landing
  (let* ((agent (a::%make-agent (make-broadcast-stream)))
         (lock (a::mailbox-lock (a::agent-input-mailbox agent))))
    (let ((a::*eval-state* (a::make-eval-state :evaluating t)))
      (sb-thread:with-mutex (lock)
        (is (eq :holding-agent-lock (a::interrupt-landing agent))))
      (is (not (eq :holding-agent-lock (a::interrupt-landing agent)))))))

(test sbcl-internals-check-out
  (is (eq :ok (a::check-sbcl-internals))))

(test unreadable-text-opens-a-level
  (call-with-agent
   (lambda (stream)
     (send-to-agent stream '(:eval "("))
     (let ((opened (next stream)))
       (is (eq :level-opened (first opened)))
       (send-to-agent stream (list :invoke-restart (level-id-of opened) (restart-index opened "ABORT")))
       (is (equal (list :level-closed (level-id-of opened)) (next stream)))))))

(test output-is-sent-before-the-debugger-opens
  (call-with-agent
   (lambda (stream)
     (send-to-agent stream '(:eval "(progn (princ \"a\") (car 5))"))
     (is (equal '(:output "a") (next stream)))
     (is (eq :level-opened (first (next stream)))))))

(test retry-after-redefining
  (call-with-agent
   (lambda (stream)
     (send-to-agent stream '(:eval "(defun f (x) (car x))"))
     (is (equal '(:values ("F")) (next stream)))
     (send-to-agent stream '(:eval "(f 5)"))
     (let ((opened (next stream)))
       (send-to-agent stream '(:eval "(defun f (x) (if (listp x) (car x) x))"))
       (is (equal '(:values ("F")) (next-past-output stream)))
       (send-to-agent stream (list :invoke-restart (level-id-of opened) (restart-index opened "RETRY")))
       (is (equal (list :level-closed (level-id-of opened)) (next stream)))
       (is (equal '(:values ("5")) (next stream)))))))

(test circular-value-prints-with-labels
  (call-with-agent
   (lambda (stream)
     (send-to-agent stream '(:eval "(let ((x (list 1 2 3))) (setf (cdr (last x)) x))"))
     (is (equal '(:values ("#1=(1 2 3 . #1#)")) (next stream))))))
