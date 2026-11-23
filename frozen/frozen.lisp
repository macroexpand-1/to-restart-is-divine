;;;; frozen/frozen.lisp: frozen structs and their copiers, from one slot list. ANSI CL only, no dependencies

(defpackage #:to-restart-is-divine/frozen
  (:use #:cl)
  (:export #:define-frozen-struct))

(in-package #:to-restart-is-divine/frozen)

;;; Reading the form

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun struct-name (name-and-options)
    "The struct's name, from NAME or (NAME OPTION...)"
    (if (symbolp name-and-options) name-and-options (first name-and-options)))

  (defun struct-options (name-and-options)
    "The defstruct options in NAME-AND-OPTIONS, '() for a bare name"
    (if (symbolp name-and-options) '() (rest name-and-options)))

  (defun option-keyword (option)
    "OPTION's keyword, whether it's written bare or as a list"
    (if (consp option) (first option) option))

  (defun find-option (keyword options)
    "The first of OPTIONS named KEYWORD, or NIL"
    (find keyword options :key #'option-keyword))

  (defun slot-list (slot-specs)
    "SLOT-SPECS without the docstring defstruct allows among them"
    (remove-if #'stringp slot-specs))

  (defun slot-name (slot-spec)
    "The slot SLOT-SPEC defines"
    (if (consp slot-spec) (first slot-spec) slot-spec))

  (defun read-only-p (slot-spec)
    "True when SLOT-SPEC says :read-only t"
    (and (consp slot-spec) (getf (cddr slot-spec) :read-only))))

;;; Refusing what the copier can't copy correctly

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun refuse-option (name options keyword reason)
    "Signal an error naming NAME when OPTIONS has KEYWORD"
    (when (find-option keyword options)
      (error "define-frozen-struct ~S: ~S is refused, because ~A." name keyword reason)))

  (defun refuse-mutable-slot (name slot-spec)
    "Signal an error naming NAME and the slot when SLOT-SPEC lacks :read-only t"
    (unless (read-only-p slot-spec)
      (error "define-frozen-struct ~S: slot ~S has no :read-only t, which every slot of a frozen struct needs."
             name (slot-name slot-spec))))

  (defun check-frozen (name options slot-specs)
    "Signal an error for anything the copier can't copy correctly. Otherwise :FROZEN"
    (refuse-option name options :include "the copier can't see the included struct's slots")
    (refuse-option name options :type "a list or vector isn't a struct the copier can name")
    (mapc (lambda (slot-spec) (refuse-mutable-slot name slot-spec)) slot-specs)
    :frozen))

;;; Naming the readers, the constructors and the copier's parameters

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun reader-prefix (name options)
    "The text defstruct puts before each slot's name to make its reader, honoring :conc-name"
    (let ((conc-name (find-option :conc-name options)))
      (cond ((null conc-name) (concatenate 'string (symbol-name name) "-"))
            ((and (consp conc-name) (second conc-name)) (string (second conc-name)))
            (t ""))))

  (defun slot-reader (name options slot)
    "SLOT's reader, interned in the current package, as defstruct interns it"
    (intern (concatenate 'string (reader-prefix name options) (symbol-name slot))))

  (defun copy-constructor (name)
    "%COPY-NAME, the private keyword constructor the copier calls"
    (intern (concatenate 'string "%COPY-" (symbol-name name))))

  (defun default-constructor (name options)
    "The MAKE-NAME option defstruct would have added itself, as a list of options. '() when
OPTIONS name a constructor"
    (if (find-option :constructor options)
        '()
        (list (list :constructor (intern (concatenate 'string "MAKE-" (symbol-name name)))))))

  (defun slot-keyword (slot)
    "The keyword a constructor takes SLOT as"
    (intern (symbol-name slot) "KEYWORD"))

  (defun copier-parameter (name options slot)
    "SLOT's keyword parameter in the copier of NAME, defaulting to the copied struct's own value"
    (list slot (list (slot-reader name options slot) name))))

(defmacro define-frozen-struct (copier name-and-options &rest slot-specs)
  "Define a frozen struct as DEFSTRUCT would from NAME-AND-OPTIONS and SLOT-SPECS, and COPIER,
a function that copies one with whichever slots you pass as keywords swapped in. Refuses a slot
without :read-only t, an :include and a :type"
  (let* ((name (struct-name name-and-options))
         (options (struct-options name-and-options))
         (slots (mapcar #'slot-name (slot-list slot-specs))))
    (check-frozen name options (slot-list slot-specs))
    `(progn
       (defstruct (,name ,@options ,@(default-constructor name options)
                   (:constructor ,(copy-constructor name) (&key ,@slots)))
         ,@slot-specs)
       (defun ,copier (,name &key ,@(mapcar (lambda (slot) (copier-parameter name options slot)) slots))
         ,(format nil "A copy of ~(~A~) with whichever slots you pass swapped in" name)
         (,(copy-constructor name) ,@(mapcan (lambda (slot) (list (slot-keyword slot) slot)) slots))))))
