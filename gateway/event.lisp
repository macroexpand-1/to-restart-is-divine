;;;; gateway/event.lisp: events a session reports up from a Lisp

(in-package #:to-restart-is-divine/gateway)

;;; Events are a frozen structs and receivers dispatch on its class.
;;; WIRE-EVENT turns the list the agent writes into a struct and EVENT-WIRE
;;; turns a struct back into that list, which is what the log holds.
;;; I switched to the structs because it was more natural for me.

(defstruct (up-event (:constructor nil) (:copier nil)))

(defstruct (ready-event (:include up-event) (:constructor make-ready-event ())))

(defstruct (output-event (:include up-event) (:constructor make-output-event (text)))
  (text "" :type string :read-only t))

(defstruct (values-event (:include up-event) (:constructor make-values-event (printed)))
  (printed '() :type list :read-only t))

(defstruct (level-opened-event (:include up-event)
                               (:constructor make-level-opened-event (debug-level)))
  (debug-level (error "A level opened event needs a DEBUG-LEVEL") :type debug-level :read-only t))

(defstruct (level-closed-event (:include up-event)
                               (:constructor make-level-closed-event (level-id)))
  (level-id 0 :type integer :read-only t))

(defstruct (locals-event (:include up-event) (:constructor make-locals-event (frame-ref pairs)))
  (frame-ref (error "A locals event needs a FRAME-REF") :type frame-ref :read-only t)
  (pairs '() :type list :read-only t))  ; (("NAME" "value") ...)

(defstruct (needs-input-event (:include up-event) (:constructor make-needs-input-event ())))

(defstruct (input-done-event (:include up-event) (:constructor make-input-done-event ())))

(defstruct (interrupt-waiting-event (:include up-event)
                                    (:constructor make-interrupt-waiting-event ())))

(defstruct (interrupt-missed-event (:include up-event)
                                   (:constructor make-interrupt-missed-event ())))

(defstruct (agent-gone-event (:include up-event) (:constructor make-agent-gone-event ())))

(defgeneric event-wire (up-event)
  (:documentation "UP-EVENT as the list the wire + log carry such as (:values (\"3\"))"))

(defmethod event-wire ((ready-event ready-event))
  (declare (ignore ready-event))
  (list :ready))

(defmethod event-wire ((output-event output-event))
  (list :output (output-event-text output-event)))

(defmethod event-wire ((values-event values-event))
  (list :values (values-event-printed values-event)))

(defmethod event-wire ((level-opened-event level-opened-event))
  (debug-level-wire (level-opened-event-debug-level level-opened-event)))

(defmethod event-wire ((level-closed-event level-closed-event))
  (list :level-closed (level-closed-event-level-id level-closed-event)))

(defmethod event-wire ((locals-event locals-event))
  (let ((frame-ref (locals-event-frame-ref locals-event)))
    (list :locals (frame-ref-level-id frame-ref) (frame-ref-frame-index frame-ref)
          (locals-event-pairs locals-event))))

(defmethod event-wire ((needs-input-event needs-input-event))
  (declare (ignore needs-input-event))
  (list :needs-input))

(defmethod event-wire ((input-done-event input-done-event))
  (declare (ignore input-done-event))
  (list :input-done))

(defmethod event-wire ((interrupt-waiting-event interrupt-waiting-event))
  (declare (ignore interrupt-waiting-event))
  (list :interrupt-waiting))

(defmethod event-wire ((interrupt-missed-event interrupt-missed-event))
  (declare (ignore interrupt-missed-event))
  (list :interrupt-missed))

(defmethod event-wire ((agent-gone-event agent-gone-event))
  (declare (ignore agent-gone-event))
  (list :agent-gone))

(defun wire-event (list)
  "The up event LIST carries (either agent or EVENT-WIRE wrote it). A kind it doesn't know it signals"
  (destructuring-bind (kind &rest fields) list
    (ecase kind
      (:ready (apply #'make-ready-event fields))
      (:output (apply #'make-output-event fields))
      (:values (apply #'make-values-event fields))
      (:level-opened (make-level-opened-event (event-debug-level list)))
      (:level-closed (apply #'make-level-closed-event fields))
      (:locals (wire-locals-event fields))
      (:needs-input (apply #'make-needs-input-event fields))
      (:input-done (apply #'make-input-done-event fields))
      (:interrupt-waiting (apply #'make-interrupt-waiting-event fields))
      (:interrupt-missed (apply #'make-interrupt-missed-event fields))
      (:agent-gone (apply #'make-agent-gone-event fields)))))

(defun wire-locals-event (fields)
  "Locals event make from FIELDS. Level id, frame-index, and then the pairs"
  (destructuring-bind (level-id frame-index pairs) fields
    (make-locals-event (make-frame-ref level-id frame-index) pairs)))
