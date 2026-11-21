;;;; agent/frames.lisp: frame surgery inside a debugger level

(in-package #:to-restart-is-divine/agent)

;;; Everything here runs on the eval thread, inside a debugger level, while
;;; the frames are still live. sbcl.lisp has the actual SBCL calls; this
;;; file just decides what to do with them.

(defconstant +max-locals+ 40
  "Most locals we'll send in one :locals message")

(defun level-frame (id index)
  "Frame INDEX of open level ID, :UNREADABLE-FRAME if its args and locals can't be trusted,
or :NO-FRAME"
  (let* ((level (find id (eval-state-levels *eval-state*) :key #'first))
         (frames (third level)))
    (cond ((not (and (integerp index) (< -1 index (length frames)))) :no-frame)
          ((member (nth index frames) (fourth level)) :unreadable-frame)
          (t (nth index frames)))))

(defun unreadable-frames (condition frames)
  "FRAMES whose args and locals can't be trusted: the one an Interrupt stopped
mid-instruction, where values may be halfway between registers and stack. A trap stops
where the compiler left good debug info, so error levels don't have this problem"
  (if (typep condition 'interrupted)
      (remove-if-not #'frame-escaped-p frames)
      '()))

(defun unwindable-indexes (frames unreadable)
  "Indexes of the FRAMES you can rerun or return from. None of the UNREADABLE ones"
  (loop for frame in frames
        for index from 0
        when (and (frame-unwindable-p frame) (not (member frame unreadable)))
          collect index))

(defun printed-locals (frame)
  "FRAME's variables as (name value) string pairs, capped at +MAX-LOCALS+. Printing is
kept short and shallow so a huge structure doesn't flood the page"
  (let ((*print-length* 10)
        (*print-level* 4)
        (*print-circle* t))
    (loop for (symbol value) in (frame-locals frame)
          repeat +max-locals+
          collect (list (printed #'prin1-to-string symbol)
                        (printed #'prin1-to-string value)))))

(defun eval-text-in-frame (text frame)
  "Evaluate each form in TEXT inside FRAME and hand back the last one's values.
This is visitor code running, so we mark it as evaluating and Interrupt works here"
  (let ((*eval-state* (eval-state-with *eval-state* :evaluating t))
        (forms (with-input-from-string (in text)
                 (loop for form = (read in nil in)
                       until (eq form in)
                       collect form))))
    (eval-form-in-frame `(progn ,@forms) frame)))

(defun operate-on-frame (agent message)
  "Do what frame MESSAGE asks. If its level or frame is gone, it's :STALE-FRAME, and a frame
we can't read is :UNREADABLE-FRAME"
  (destructuring-bind (kind id index &optional text) message
    (let ((frame (level-frame id index)))
      (cond ((eq frame :no-frame) :stale-frame)
            ((eq frame :unreadable-frame) :unreadable-frame)
            (t (operate-on-live-frame agent kind id index frame text))))))

(defun operate-on-live-frame (agent kind id index frame text)
  "Do KIND to FRAME, which is frame INDEX of level ID. Return and Rerun on a frame that
can't be unwound are :NOT-UNWINDABLE, and when they do work they never come back"
  (ecase kind
    (:locals (send-locals agent id index frame))
    (:eval-in-frame
     (send agent (list :values (print-values (eval-text-in-frame text frame))))
     (send-locals agent id index frame))
    (:return-from-frame
     (if (frame-unwindable-p frame)
         (return-from-frame frame (eval-text-in-frame text frame))
         :not-unwindable))
    (:rerun-frame
     (if (frame-unwindable-p frame)
         (rerun-frame frame)
         :not-unwindable))))

(defun send-locals (agent id index frame)
  "Send FRAME's locals up as a :locals message (it's frame INDEX of level ID)"
  (send agent (list :locals id index (printed-locals frame))))
