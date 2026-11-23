;;;; gateway/threads.lisp: every thread, mailbox, shared value and wait the gateway uses

(defpackage #:to-restart-is-divine/threads
  (:use #:cl)
  (:export #:start-thread #:join-within
           #:make-mailbox #:mailbox-send #:mailbox-receive-within
           #:make-box #:box-value #:box-step #:box-update #:box-claim
           #:wait-until-within #:fd-readable-within))

(in-package #:to-restart-is-divine/threads)

;;; The gateway's raw SBCL thread calls live here and nowhere else; a build test refuses
;;; them in any other gateway or test file. Every wait takes its time limit as a required
;;; argument and gives up with :TIMED-OUT, so no waiting state lacks an exit.
;;;
;;; Nothing here holds a lock while it waits. A mailbox is SB-CONCURRENCY's, which is
;;; lock-free, and a box is one slot replaced whole by compare-and-swap. Shared state
;;; goes in a box; a stream gets one owner thread fed by a mailbox

(defstruct (box (:constructor make-box (value)))
  "One shared value, replaced whole by compare-and-swap, never changed in place"
  ;; mutable
  (value nil))

(defun box-step (box step)
  "Replace BOX's value with what STEP makes of it, and return STEP's second value.
STEP takes the current value and returns (values next result). It runs again when another
thread replaced the value first, so it only computes"
  (loop
    (let ((old (box-value box)))
      (multiple-value-bind (next result) (funcall step old)
        (when (eq old (sb-ext:compare-and-swap (box-value box) old next))
          (return result))))))

(defun box-update (box function)
  "Replace BOX's value with FUNCTION of it, and return the new value. FUNCTION only computes,
since it can run more than once"
  (box-step box (lambda (old)
                  (let ((next (funcall function old)))
                    (values next next)))))

(defun box-claim (box from to)
  "Replace BOX's value with TO if it's FROM, by EQ. :CLAIMED, or :ABANDONED when it held
something else"
  (if (eq from (sb-ext:compare-and-swap (box-value box) from to))
      :claimed
      :abandoned))

(defun start-thread (name function)
  "Call FUNCTION on a new thread called NAME. The thread"
  (sb-thread:make-thread function :name name))

(defun join-within (thread seconds)
  "Wait up to SECONDS for THREAD to finish. :JOINED, or :TIMED-OUT with THREAD still running"
  (sb-thread:join-thread thread :timeout seconds :default :timed-out)
  (if (sb-thread:thread-alive-p thread)
      :timed-out
      :joined))

(defun make-mailbox (name)
  "Empty mailbox called NAME. Any number of threads may send; one should receive"
  (sb-concurrency:make-mailbox :name name))

(defun mailbox-send (mailbox item)
  "Put ITEM at the back of MAILBOX. :SENT. Refuses :TIMED-OUT, which receiving can't tell
from a wait that gave up"
  (when (eq item :timed-out)
    (error "A mailbox can't carry :TIMED-OUT."))
  (sb-concurrency:send-message mailbox item)
  :sent)

(defun mailbox-receive-within (mailbox seconds)
  "Take the oldest item in MAILBOX, waiting up to SECONDS for one. :TIMED-OUT if none came"
  (multiple-value-bind (item received) (sb-concurrency:receive-message mailbox :timeout seconds)
    (if received item :timed-out)))

(defun wait-until-within (seconds predicate)
  "Call PREDICATE every hundredth of a second until it's true. :DONE, or :TIMED-OUT once
SECONDS pass"
  (let ((deadline (+ (get-internal-real-time) (* seconds internal-time-units-per-second))))
    (loop
      (cond ((funcall predicate) (return :done))
            ((> (get-internal-real-time) deadline) (return :timed-out))
            (t (sleep 1/100))))))

(defun fd-readable-within (fd seconds)
  "Wait up to SECONDS for file descriptor FD to have input. :READABLE or :TIMED-OUT"
  (if (sb-sys:wait-until-fd-usable fd :input seconds)
      :readable
      :timed-out))
