;;;; gateway/event.lisp: events a session reports up from a Lisp

(in-package #:to-restart-is-divine/gateway)

(defstruct (up-event (:constructor nil) (:copier nil)))

(defstruct (ready-event (:include up-event) (:constructor make-ready-event ())))
