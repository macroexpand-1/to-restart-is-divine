;;;; gateway/session.lisp: what happens between the page and whatever runs the code

(in-package #:to-restart-is-divine/gateway)

;;; A session is opened with OPEN-SESSION, given a kind and a receiver.
;;; It reports everything by calling RECEIVE-EVENT on that receiver with one event:
;;;
;;;   (:ready)
;;;   (:output "text")
;;;   (:values ("printed value" ...))
;;;   (:level-opened :id ID :depth N :type "TYPE" :report "text"
;;;           :restarts ((INDEX "NAME" "report") ...) :backtrace ("frame" ...))
;;;   (:level-closed ID)
;;;
;;; The receiver may be called on any thread, including the caller's own
;;; thread from inside SESSION-EVAL. Callers must not hold a lock the receiver
;;; takes while calling into a session.

(defclass session ()
  ()
  (:documentation "One visitor's connection to a Lisp that runs their code"))

(defgeneric open-session (kind receiver)
  (:documentation "A new session of KIND, a keyword such as :AGENT or a kind struct, that reports to RECEIVER"))

(defgeneric receive-event (receiver event)
  (:documentation "Take one EVENT from a session. Called on whatever thread the session delivers on"))

(defgeneric session-eval (session text)
  (:documentation "Send TEXT to be read and evaluated; the gateway never reads TEXT itself. Its value
is ignored"))

(defgeneric session-invoke-restart (session id index)
  (:documentation "Answer the debugger level ID by invoking its restart number INDEX. Its value
is ignored"))

(defgeneric session-interrupt (session)
  (:documentation "Interrupt whatever SESSION is evaluating. Its value is ignored"))

(defgeneric session-close (session)
  (:documentation "End SESSION. No events are sent after this returns. Its value is ignored"))
