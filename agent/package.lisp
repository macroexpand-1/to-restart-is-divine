;;;; agent/package.lisp

(defpackage #:to-restart-is-divine/agent
  (:use #:cl #:to-restart-is-divine/wire)
  (:local-nicknames (#:frozen #:to-restart-is-divine/frozen))
  (:export #:start-agent #:wait-for-agent #:run-agent #:interrupted))
