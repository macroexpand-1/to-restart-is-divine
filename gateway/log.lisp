;;;; gateway/log.lisp: a session's messages as a hash-chained log

(in-package #:to-restart-is-divine/gateway)

;;; Each entry is its own message
;;;
;;;   (:entry :seq 3 :direction :down :prev-hash "9f2c..." :message (:eval "(f 5)"))
;;;
;;; :prev-hash is the SHA-256 of the previous entry's printed text, or
;;; "genesis" for the first (just like splice). The hash of the
;;; newest entry names the whole log.

(defparameter *genesis-hash* "genesis"
  "The :prev-hash of a log's first entry")

(defstruct (message-log (:constructor %make-message-log (entries newest-hash count))
                        (:copier nil))
  (entries '() :read-only t)
  (newest-hash *genesis-hash* :read-only t)
  (count 0 :read-only t))

(defun make-message-log ()
  "Empty log"
  (%make-message-log '() *genesis-hash* 0))

(defun entry-text (entry)
  "ENTRY printed just as a log file holds it, after teh length prefix"
  (w:message-text entry))

(defun text-hash (text)
  "SHA-256 of TEXT's UTF-8 encoding as 64 lowercase hex digits"
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha256 (sb-ext:string-to-octets text :external-format :utf-8))))

(defun log-append (message-log direction message)
  "MESSAGE-LOG with MESSAGE added as the next entry. DIRECTION is :down for a message to the Lisp
and :)up for an event from it"
  (let*  ((seq (1+ (message-log-count message-log)))
          (entry (list :entry :seq seq :direction direction
                              :prev-hash (message-log-newest-hash message-log) :message message)))
    (%make-message-log (cons entry (message-log-entries message-log))
                       (text-hash (entry-text entry))
                       seq)))

(defun log-entries (message-log)
  "MESSAGE-LOG's entries, oldest first"
  (reverse (message-log-entries message-log)))

(defun chain-head (entries)
  "Head hash of ENTRIES (oldest first), if each one's :prev-hash is the has of the entry before it.
Otherwise :BROKEN and the :seq of the first entry that does not match"
  (loop with expected = *genesis-hash*
        for entry in entries
        unless (equal (getf (rest entry) :prev-hash) expected)
          do (return (values :broken (getf (rest entry) :seq)))
        do (setf expected (text-hash (entry-text entry)))
        finally (return expected)))
