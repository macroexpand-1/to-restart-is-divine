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
;;;           :unwindable (FRAME-INDEX ...))
;;;   (:level-closed ID)
;;;   (:locals ID FRAME-INDEX (("NAME" "value") ...))
;;;   (:needs-input)
;;;   (:input-done)
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

(defgeneric session-eval (session text)
  (:documentation "Send TEXT off to be read and evaluated. Gateway never reads TEXT itself.
Return value doesn't matter"))

(defgeneric session-invoke-restart (session id index)
  (:documentation "Answer debugger level ID by invoking its restart number INDEX.
Return value doesn't matter"))

(defgeneric session-interrupt (session)
  (:documentation "Interrupt whatever SESSION is evaluating. Return value doesn't matter"))

(defgeneric session-close (session)
  (:documentation "End SESSION. No more events once this returns. Return value doesn't matter"))

(defgeneric session-frame-locals (session id frame-index)
  (:documentation "Ask for the live variables of frame FRAME-INDEX in level ID.
They come back as a :locals event"))

(defgeneric session-eval-in-frame (session id frame-index text)
  (:documentation "Evaluate TEXT in frame FRAME-INDEX of level ID. Gateway never reads TEXT itself.
Return value doesn't matter"))

(defgeneric session-return-from-frame (session id frame-index text)
  (:documentation "Make frame FRAME-INDEX of level ID return TEXT's values, evaluated in that frame.
Return value doesn't matter"))

(defgeneric session-rerun-frame (session id frame-index)
  (:documentation "Call frame FRAME-INDEX of level ID again, with whatever args it holds now.
Return value doesn't matter"))

(defgeneric session-input (session text)
  (:documentation "Give TEXT to visitor code as one line of input. Return value doesn't matter"))
