;;;; gateway/session.lisp: what happens between the page and whatever runs the code

(in-package #:to-restart-is-divine/gateway)

;;; You open a session with OPEN-SESSION, handing it a kind and a receiver.
;;; It reports everything by calling RECEIVE-EVENT on that receiver, one event at a time:
;;;
;;;   (:ready)
;;;   (:output "text")
;;;   (:values ("printed value" ...))
;;;   (:level-opened :id ID :depth N :type "TYPE" :report "text"
;;;           :restarts ((INDEX "NAME" "report") ...) :backtrace ("frame" ...)
;;;           :unwindable (FRAME-INDEX ...) :unreadable (FRAME-INDEX ...)
;;;           :frames :read-or-:lost)
;;;   (:level-closed ID)
;;;   (:locals ID FRAME-INDEX (("NAME" "value") ...))
;;;   (:needs-input)
;;;   (:input-done)
;;;   (:interrupt-waiting)   ; Interrupt can't land yet, code's in foreign (C) code
;;;   (:interrupt-missed)    ; ...and evaluation finished before it could
;;;   (:agent-gone)          ; agent died without the session closing it, always last
;;;
;;; Receiver can get called on any thread, including the caller's own thread from
;;; inside SESSION-EVAL. So don't hold a lock the receiver takes while calling
;;; into a session.

(defclass session ()
  ()
  (:documentation "One visitor's connection to a Lisp that runs their code"))

(defgeneric open-session (kind receiver)
  (:documentation "New session of KIND (a keyword like :AGENT, or a kind struct) that reports to RECEIVER"))

(defgeneric receive-event (receiver event)
  (:documentation "Take one EVENT from a session, on whatever thread the session happens to deliver on"))

(defgeneric session-send (session down-message)
  (:documentation "Send DOWN-MESSAGE to SESSION. Gateway never reads the text inside.
:TOO-LONG means it was over the wire's limit and went nowhere. Any other return doesn't matter"))
