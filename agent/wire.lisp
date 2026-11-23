;;;; agent/wire.lisp: message framing, shared by agent and gateway

(defpackage #:to-restart-is-divine/wire
  (:use #:cl)
  (:export #:write-message #:read-message #:message-text #:text-too-long-p #:malformed-message
           #:*max-message-length*))

(in-package #:to-restart-is-divine/wire)


;;; Each message is one S-expression, and its first element (its kind) is a keyword.
;;; On the wire it's six lowercase hex digits giving its length in characters,
;;; then the text:
;;;
;;;   00000f(:values ("3"))
;;;
;;; Both ends open the socket as a UTF-8 char stream, so lengths count characters, not octets.

(defconstant +prefix-width+ 6
  "Hex digits in the length prefix")

(defparameter *max-message-length* (expt 2 20)
  "Longest printed message we'll read or write, in characters. Hard limit")

(define-condition malformed-message (error)
  ((reason :initarg :reason :reader malformed-message-reason))
  (:report (lambda (condition stream)
             (format stream "Malformed message: ~A." (malformed-message-reason condition)))))

(defun malformed (control &rest arguments)
  "Signal MALFORMED-MESSAGE with a reason from CONTROL and ARGUMENTS"
  (error 'malformed-message :reason (apply #'format nil control arguments)))

(defun message-text (message)
  "MESSAGE printed the way it goes on the wire, minus the prefix. Anything that hashes or
logs a message prints it with this, so it can't drift from what was sent"
  (with-standard-io-syntax
    (let ((*print-case* :downcase))
      (prin1-to-string message))))

(defun text-too-long-p (text)
  "True when TEXT, a message printed by MESSAGE-TEXT, is over the wire's limit"
  (> (length text) *max-message-length*))

(defun write-message (message stream)
  "Write MESSAGE to STREAM with its length prefix, then force it out"
  (let ((text (message-text message)))
    (when (text-too-long-p text)
      (malformed "~D characters is over the limit of ~D" (length text) *max-message-length*))
    (format stream "~(~v,'0X~)~A" +prefix-width+ (length text) text)
    (force-output stream)))

(defun read-chars (stream count)
  "Up to COUNT characters from STREAM. Fewer only if STREAM ends first"
  (let* ((buffer (make-string count))
         (end (read-sequence buffer stream)))
    (subseq buffer 0 end)))

(defun prefix-length (prefix)
  "Message length that PREFIX announces"
  (unless (and (= (length prefix) +prefix-width+)
               (every (lambda (char) (digit-char-p char 16)) prefix))
    (malformed "the length prefix ~S is not ~D hex digits" prefix +prefix-width+))
  (let ((announced-length (parse-integer prefix :radix 16)))
    (when (> announced-length *max-message-length*)
      (malformed "~D characters is over the limit of ~D" announced-length *max-message-length*))
    announced-length))

(defun parse-message (text)
  "Message that TEXT prints, which has to be one list headed by a keyword.
Reads with *READ-EVAL* off, in the same standard syntax WRITE-MESSAGE prints with,
so NIL comes back as NIL and not :NIL"
  (multiple-value-bind (message end)
      (handler-case (with-standard-io-syntax
                      (let ((*read-eval* nil))
                        (read-from-string text)))
        ((or reader-error end-of-file) (condition)
          (malformed "~A" condition)))
    (unless (= end (length text))
      (malformed "text follows the message"))
    (unless (and (consp message) (keywordp (first message)))
      (malformed "~S is not a list headed by a keyword" message))
    message))

(defun read-message (stream)
  "Next message on STREAM, or :END"
  (let ((prefix (read-chars stream +prefix-width+)))
    (if (zerop (length prefix))
        :end
        (let* ((length (prefix-length prefix))
               (text (read-chars stream length)))
          (unless (= (length text) length)
            (malformed "the stream ended inside a message"))
          (parse-message text)))))
