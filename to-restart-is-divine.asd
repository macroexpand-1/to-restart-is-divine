;;;; to-restart-is-divine.asd

;;; Every system here is a strict-build system: any warning from compiling or
;;; loading its own files fails the operation, style-warnings included. SBCL only
;;; style-warns about a misspelled function or class, and that's how
;;; GENT-SESSION-CLOSED-P and DOWN-MESAGE got in. The lesson image's agent loads
;;; through here too, so it builds the same way

(defsystem "to-restart-is-divine"
  :description "In-Browser Common Lisp REPL with a Showcased Debugger")

(defsystem "to-restart-is-divine/wire"
  :defsystem-depends-on ("strict-build")
  :class "strict-build:strict-system"
  :default-component-class "strict-build:strict-source-file"
  :description "Message Framing Shared by Agent and Gateway. ANSI CL Only"
  :components ((:module "agent"
                :components ((:file "wire")))))

(defsystem "to-restart-is-divine/frozen"
  :defsystem-depends-on ("strict-build")
  :class "strict-build:strict-system"
  :default-component-class "strict-build:strict-source-file"
  :description "Frozen Structs and Their Copiers. ANSI CL Only, No Deps"
  :components ((:module "frozen"
                :components ((:file "frozen")))))

(defsystem "to-restart-is-divine/agent"
  :defsystem-depends-on ("strict-build")
  :class "strict-build:strict-system"
  :default-component-class "strict-build:strict-source-file"
  :description "Debug Agent Loaded Into Every Lesson. No Deps, No Contribs"
  :depends-on ("to-restart-is-divine/wire" "to-restart-is-divine/frozen")
  :serial t
  :components ((:module "agent"
                :components ((:file "package")
                             (:file "sbcl")
                             (:file "agent")
                             (:file "frames")))))

(defsystem "to-restart-is-divine/gateway"
  :defsystem-depends-on ("strict-build")
  :class "strict-build:strict-system"
  :default-component-class "strict-build:strict-source-file"
  :description "The session protocol, console state, agent sessions, and session logs"
  :depends-on ("to-restart-is-divine/wire" "to-restart-is-divine/link" "to-restart-is-divine/frozen"
               "to-restart-is-divine/threads" "ironclad")
  :serial t
  :components ((:module "gateway"
                :components ((:file "package")
                             (:file "session")
                             (:file "message")
                             (:file "console")
                             (:file "event")
                             (:file "agent-session")
                             (:file "log")
                             (:file "recording"))))
  :in-order-to ((test-op (test-op "to-restart-is-divine/tests"))))

(defsystem "to-restart-is-divine/web"
  :defsystem-depends-on ("strict-build")
  :class "strict-build:strict-system"
  :default-component-class "strict-build:strict-source-file"
  :description "The CLOG page and the only system that loads CLOG"
  :depends-on ("to-restart-is-divine/gateway" "to-restart-is-divine/threads" "to-restart-is-divine/frozen"
               "clog")
  :components ((:module "gateway"
                :components ((:file "web")))))

(defsystem "to-restart-is-divine/threads"
  :defsystem-depends-on ("strict-build")
  :class "strict-build:strict-system"
  :default-component-class "strict-build:strict-source-file"
  :description "Every Thread, Mailbox, Shared Value and Wait the Gateway Uses. Waits Take a Time Limit"
  :depends-on ((:require "sb-concurrency"))
  :components ((:module "gateway"
                :components ((:file "threads")))))

(defsystem "to-restart-is-divine/link"
  :defsystem-depends-on ("strict-build")
  :class "strict-build:strict-system"
  :default-component-class "strict-build:strict-source-file"
  :description "The Gateway's End of the Agent Socket + the Agent in Its Own SBCL Process"
  :depends-on ((:require "sb-bsd-sockets") "to-restart-is-divine/threads")
  :components ((:module "gateway"
                :components ((:file "link")))))

(defsystem "to-restart-is-divine/tests"
  :defsystem-depends-on ("strict-build")
  :class "strict-build:strict-system"
  :default-component-class "strict-build:strict-source-file"
  :description "The fake session and every test. No CLOG"
  :depends-on ("to-restart-is-divine/agent"
               "to-restart-is-divine/link"
               "to-restart-is-divine/gateway"
               "fiveam")
  :serial t
  :components ((:module "test"
                :components ((:file "package")
                             (:file "build-tests")
                             (:file "event-capture")
                             (:file "wire-tests")
                             (:file "agent-tests")
                             (:file "link-tests")
                             (:file "agent-session-tests")
                             (:file "frames-tests")
                             (:file "frames-console-tests")
                             (:file "fake-session")
                             (:file "console")
                             (:file "fake-session-tests"))))
  :perform (test-op (o c)
                    (declare (ignore o c))
                    (unless (uiop:symbol-call :fiveam :run!
                                              (uiop:find-symbol* :divine-tests
                                                                 :to-restart-is-divine/tests))
                      (error "to-restart-is-divine tests failed!"))))
