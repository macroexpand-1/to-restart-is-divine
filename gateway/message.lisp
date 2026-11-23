;;;; gateway/message.lisp: the messages a session sends down to a lisp

(in-package #:to-restart-is-divine/gateway)

(defstruct (down-message (:constructor nil) (:copier nil)))

(defstruct (eval-message (:include down-message) (:constructor make-eval-message (text)))
  (text "" :type string :read-only t))

(defstruct (invoke-restart-message (:include down-message)
                                   (:constructor make-invoke-restart-message
                                       (level-id restart-index)))
  (level-id 0 :type integer :read-only t)
  (restart-index 0 :type integer :read-only t))

(defstruct (interrupt-message (:include down-message) (:constructor make-interrupt-message ())))

(defstruct (input-message (:include down-message) (:constructor make-input-message (text)))
  (text "" :type string :read-only t))

(defstruct (frame-ref (:constructor make-frame-ref (level-id frame-index)))
  (level-id 0 :type integer :read-only t)
  (frame-index 0 :type integer :read-only t))

(defstruct (frame-message (:include down-message) (:constructor nil))
  (frame-ref (error "A frame message needs a FRAME-REF.") :type frame-ref :read-only t))

(defstruct (locals-message (:include frame-message)
                           (:constructor make-locals-message (frame-ref))))

(defstruct (eval-in-frame-message (:include frame-message)
                                  (:constructor make-eval-in-frame-message (frame-ref text)))
  (text "" :type string :read-only t))

(defstruct (return-from-frame-message (:include frame-message)
                                      (:constructor make-return-from-frame-message (frame-ref text)))
  (text "" :type string :read-only t))

(defstruct (rerun-frame-message (:include frame-message)
                                (:constructor make-rerun-frame-message (frame-ref))))

(defgeneric message-wire (down-message)
  (:documentation "DOWN-MESSAGE as the list the wire and the log carry. Something like (:eval \"(f 5)\")"))

(defmethod message-wire ((eval-message eval-message))
  (list :eval (eval-message-text eval-message)))

(defmethod message-wire ((invoke-restart-message invoke-restart-message))
  (list :invoke-restart
        (invoke-restart-message-level-id invoke-restart-message)
        (invoke-restart-message-restart-index invoke-restart-message)))

(defmethod message-wire ((interrupt-message interrupt-message))
  (declare (ignore interrupt-message))
  (list :interrupt))

(defmethod message-wire ((input-message input-message))
  (list :input (input-message-text input-message)))

(defun frame-wire (kind frame-message &rest fields)
  "Wire list for FRAME-MESSAGE: KIND, its level id and frame index, then FIELDS"
  (let ((frame-ref (frame-message-frame-ref frame-message)))
    (list* kind (frame-ref-level-id frame-ref) (frame-ref-frame-index frame-ref) fields)))

(defmethod message-wire ((locals-message locals-message))
  (frame-wire :locals locals-message))

(defmethod message-wire ((eval-in-frame-message eval-in-frame-message))
  (frame-wire :eval-in-frame eval-in-frame-message
              (eval-in-frame-message-text eval-in-frame-message)))

(defmethod message-wire ((return-from-frame-message return-from-frame-message))
  (frame-wire :return-from-frame return-from-frame-message
              (return-from-frame-message-text return-from-frame-message)))

(defmethod message-wire ((rerun-frame-message rerun-frame-message))
  (frame-wire :rerun-frame rerun-frame-message))

(defun wire-message (list)
  "Down message LIST carries as MESSAGE-WIRE wrote it. A kind it doesn't know signals"
  (destructuring-bind (kind &rest fields) list
    (ecase kind
      (:eval (apply #'make-eval-message fields))
      (:invoke-restart (apply #'make-invoke-restart-message fields))
      (:interrupt (apply #'make-interrupt-message fields))
      (:input (apply #'make-input-message fields))
      (:locals (wire-frame-message #'make-locals-message fields))
      (:eval-in-frame (wire-frame-message #'make-eval-in-frame-message fields))
      (:return-from-frame (wire-frame-message #'make-return-from-frame-message fields))
      (:rerun-frame (wire-frame-message #'make-rerun-frame-message fields)))))

(defun wire-frame-message (constructor fields)
  "A frame message by CONSTRUCTOR from FIELDS. A level id, a frame index, and the rest"
  (destructuring-bind (level-id frame-index &rest more) fields
    (apply constructor (make-frame-ref level-id frame-index) more)))
