;;;; gateway/session.lisp: what happens between the page and whatever runs the code

(in-package #:to-restart-is-divine/gateway)

;;; You open a session with OPEN-SESSION, handing it a kind and a receiver.
;;; It reports everything by calling RECEIVE-EVENT on that receiver with one up
;;; event at a time, a struct from event.lisp. Each is shown beside the list
;;; EVENT-WIRE gives for it, which is what the agent writes and the log holds:
;;;
;;;   READY-EVENT              (:ready)
;;;   OUTPUT-EVENT             (:output "text")
;;;   VALUES-EVENT             (:values ("printed value" ...))
;;;   LEVEL-OPENED-EVENT       (:level-opened :id ID :depth N :type "TYPE" :report "text"
;;;                              :restarts ((INDEX "NAME" "report") ...) :backtrace ("frame" ...)
;;;                              :unwindable (FRAME-INDEX ...) :unreadable (FRAME-INDEX ...)
;;;                              :frames :read-or-:lost)
;;;   LEVEL-CLOSED-EVENT       (:level-closed ID)
;;;   LOCALS-EVENT             (:locals ID FRAME-INDEX (("NAME" "value") ...))
;;;   NEEDS-INPUT-EVENT        (:needs-input)
;;;   INPUT-DONE-EVENT         (:input-done)
;;;   INTERRUPT-WAITING-EVENT  (:interrupt-waiting)  ; Interrupt can't land yet, code's in C
;;;   INTERRUPT-MISSED-EVENT   (:interrupt-missed)   ; ...and evaluation finished before it could
;;;   AGENT-GONE-EVENT         (:agent-gone)         ; agent died without the session closing it
;;;
;;; Receiver can get called on any thread, including the caller's own thread from
;;; inside SESSION-SEND. So don't hold a lock the receiver takes while calling
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
