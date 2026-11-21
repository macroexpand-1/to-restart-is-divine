;;;; test/fake-session.lisp: a scripted session that never evals anything

(in-package #:to-restart-is-divine/tests)

(defstruct (fake-script (:constructor make-fake-script (&key replies errors restarts))
                        (:copier nil))
  (replies '() :read-only t)
  (errors '() :read-only t)
  (restarts '() :read-only t))

(defparameter *fake-script*
  (make-fake-script
   :replies '(("(+ 1 2)" (:values ("3")))
              ("(values)" (:values ()))
              ("(print 'hello)" (:output "
HELLO ") (:values ("HELLO"))))
   :errors '(("(car 5)" "TYPE-ERROR" "The value
  5
is not of type
  LIST")
             ("(/ 1 0)" "DIVISION-BY-ZERO" "arithmetic error DIVISION-BY-ZERO signalled"))
   :restarts '((0 "RETRY" "Evaluate the form again.")
               (1 "ABORT" "Return to top level.")))
  "What the fake answers, which levels it opens, and the restarts each level offers")

(defun fake-abort-index (script)
  "Index of ABORT in SCRIPT's restart list, or :NONE"
  (let ((entry (find "ABORT" (fake-script-restarts script) :key #'second :test #'string=)))
    (if (consp entry) (first entry) :none)))

(defclass fake-session (g:session)
  ((receiver :initarg :receiver :reader fake-session-receiver)
   ;; mutable
   (next-id :initform 1 :accessor fake-session-next-id)
   (open-level-ids :initform '() :accessor fake-session-open-level-ids))
  (:documentation "Session that answers straight out of *FAKE-SCRIPT*"))

(defmethod g:open-session ((kind (eql :fake)) receiver)
  "Fake session reporting to RECEIVER"
  (make-instance 'fake-session :receiver receiver))

(defun deliver (fake-session event)
  "Send EVENT to FAKE-SESSION's receiver"
  (g:receive-event (fake-session-receiver fake-session) event))

(defun open-level (fake-session type report text)
  "Open a debugger level for TYPE and REPORT (raised by TEXT) and announce it"
  (let ((id (fake-session-next-id fake-session)))
    (setf (fake-session-next-id fake-session) (1+ id))
    (push id (fake-session-open-level-ids fake-session))
    (deliver fake-session (list :level-opened
                                :id id
                                :depth (length (fake-session-open-level-ids fake-session))
                                :type type
                                :report report
                                :restarts (fake-script-restarts *fake-script*)
                                :backtrace (list (format nil "0: ~A" text)
                                                 (format nil "1: (EVAL ~A)" text))))))

(defun deliver-unscripted (fake-session text)
  "Tell FAKE-SESSION's receiver there's no script for TEXT, then send empty values"
  (deliver fake-session (list :output (format nil ";; The fake session has no script for ~A~%" text)))
  (deliver fake-session (list :values '())))

(defmethod g:session-eval ((fake-session fake-session) text)
  (let ((reply (assoc text (fake-script-replies *fake-script*) :test #'string=))
        (failure (assoc text (fake-script-errors *fake-script*) :test #'string=)))
    (cond (reply (dolist (event (rest reply)) (deliver fake-session event)))
          (failure (destructuring-bind (type report) (rest failure)
                     (open-level fake-session type report text)))
          (t (deliver-unscripted fake-session text)))))

(defun closing-level-ids (open-level-ids id index)
  "Levels in OPEN-LEVEL-IDS that restart INDEX of level ID closes, newest first.
ABORT closes all of them, unknown ID closes none"
  (let ((tail (member id open-level-ids)))
    (cond ((null tail) '())
          ((eql index (fake-abort-index *fake-script*)) open-level-ids)
          (t (ldiff open-level-ids (rest tail))))))

(defmethod g:session-invoke-restart ((fake-session fake-session) id index)
  (let ((closing (closing-level-ids (fake-session-open-level-ids fake-session) id index)))
    (setf (fake-session-open-level-ids fake-session)
          (nthcdr (length closing) (fake-session-open-level-ids fake-session)))
    (dolist (closed closing)
      (deliver fake-session (list :level-closed closed)))))

(defmethod g:session-interrupt ((fake-session fake-session))
  (open-level fake-session "TO-RESTART-IS-DIVINE/AGENT:INTERRUPTED" "Interrupted from the page."
              "(interrupt)"))

(defmethod g:session-frame-locals ((fake-session fake-session) id frame-index)
  (deliver fake-session (list :locals id frame-index '(("X" "5") ("NAME" "\"fake\"")))))

(defmethod g:session-eval-in-frame ((fake-session fake-session) id frame-index text)
  (declare (ignore id frame-index text))
  (deliver fake-session '(:values ("NIL"))))

(defmethod g:session-return-from-frame ((fake-session fake-session) id frame-index text)
  (declare (ignore fake-session id frame-index text)))

(defmethod g:session-rerun-frame ((fake-session fake-session) id frame-index)
  (declare (ignore fake-session id frame-index)))

(defmethod g:session-input ((fake-session fake-session) text)
  (declare (ignore fake-session text)))

(defmethod g:session-close ((fake-session fake-session))
  (setf (fake-session-open-level-ids fake-session) '()))
