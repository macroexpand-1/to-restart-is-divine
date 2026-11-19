;;;; agent/sbcl.lisp: every SBCL specific call

(in-package #:to-restart-is-divine/agent)

;;; Unix socket without sb-bsd-sockets

(defconstant +af-unix+ 1)
(defconstant +sock-stream+ 1)
(defconstant +max-socket-path+ 103
  "Octets of path that fit in sockaddr_un on every target with a
little extra for NUL")

(defconstant +output-chunk+ 4096
  "Characters buffered before sending without a newline")

(sb-alien:define-alien-routine ("socket" %socket) sb-alien:int
  (domain sb-alien:int) (type sb-alien:int) (protocol sb-alien:int))

(sb-alien:define-alien-routine ("connect" %connect) sb-alien:int
  (fd sb-alien:int) (address sb-alien:system-area-pointer) (length (sb-alien:unsigned 32)))

(defun socket-address (path)
  "PATH as a sockaddr_un: two-byte header, path, NUL-term"
  (let ((path-octets (sb-ext:string-to-octets path :external-format :utf-8 :null-terminate t)))
    (when (> (length path-octets) (1+ +max-socket-path+))
      (error "The socket path ~A is longer than ~D octets." path +max-socket-path+))
    (concatenate '(vector (unsigned-byte 8))
                 #+linux (vector +af-unix+ 0) ; sun_family, little-endian short
                 #-linux (vector (+ 2 (length path-octets)) +af-unix+) ; sun_len, then sun_family
                 path-octets)))

(defun connect-unix-socket (path)
  "Bidirectional UTF-8 character stream on unix socket at PATH"
  (let ((fd (%socket +af-unix+ +sock-stream+ 0))
        (address (socket-address path)))
    (when (minusp fd)
      (error "socket() failed: ~A" (sb-int:strerror)))
    (sb-sys:with-pinned-objects (address)
      (when (minusp (%connect fd (sb-sys:vector-sap address) (length address)))
        (let ((reason (sb-int:strerror)))
          (sb-unix:unix-close fd)
          (error "Connecting to ~A failed: ~A" path reason))))
    (sb-sys:make-fd-stream fd :input t :output t
                              :element-type 'character
                              :external-format :utf-8
                              :buffering :full
                              :dual-channel-p t
                              :name (format nil "agent socket ~A" path))))

;; Thread things with sb-thread

(defun spawn (name function)
  "Run FUNCTION in a new thread called NAME"
  (sb-thread:make-thread function :name name))

(defun wait-for-thread (thread)
  "Return once THREAD has ended"
  (sb-thread:join-thread thread :default nil))

(defun kill-thread (thread)
  "End THREAD, and unwind it"
  (sb-thread:terminate-thread thread))

(defun interrupt (thread function)
  "Run FUNCTION in THREAD as soon as it can be interrupted, with
interrupts enabled inside"
  (sb-thread:interrupt-thread thread
                              (lambda ()
                                (sb-sys:with-interrupts (funcall function)))))

;; Someone call Al Gore

(defun make-lock (name)
  "A lock called NAME"
  (sb-thread:make-mutex :name name))

(defun call-with-lock (lock function)
  "Call FUNCTION holding LOCK. Interrupts wait until LOCK is released"
  (sb-sys:without-interrupts
    (sb-thread:with-mutex (lock)
      (funcall function))))

;; Mailboxes!

(defstruct (mailbox (:constructor make-mailbox ()))
  (lock (sb-thread:make-mutex :name "mailbox") :read-only t)
  (ready (sb-thread:make-waitqueue :name "mailbox") :read-only t)
  ;; mutable
  (messages '()))                       ; oldest first

(defun mailbox-send (mailbox message)
  "Add a MESSAGE to the end of MAILBOX"
  (sb-thread:with-mutex ((mailbox-lock mailbox))
    (setf (mailbox-messages mailbox) (append (mailbox-messages mailbox) (list message)))
    (sb-thread:condition-notify (mailbox-ready mailbox))))

(defun mailbox-receive (mailbox)
  "The oldest message in MAILBOX, waiting for one if it is empty"
  (sb-thread:with-mutex ((mailbox-lock mailbox))
    (loop while (null (mailbox-messages mailbox))
          do (sb-thread:condition-wait (mailbox-ready mailbox) (mailbox-lock mailbox)))
    (pop (mailbox-messages mailbox))))

;; Stream things

(defgeneric forward-text (sink text)
  (:documentation "Take TEXT, one flushed chunk of a forwarding stream's output"))

(defclass forwarding-stream (sb-gray:fundamental-character-output-stream)
  ((sink :initarg :sink :reader stream-sink)
   (buffer :initform (make-array 0 :element-type 'character :adjustable t :fill-pointer t)
           :reader stream-buffer)
   (column :initform 0 :accessor stream-column))
  (:documentation "An output stream that hands text to SINK with FORWARD-TEXT, one line at a time"))

(defun make-forwarding-stream (sink)
  "An output stream whose text is passed to SINK with FORWARD-TEXT"
  (make-instance 'forwarding-stream :sink sink))

(defun flush-stream (forwarding-stream)
  "Hand FORWARDING-STREAM's stuff to its sink"
  (let ((buffer (stream-buffer forwarding-stream)))
    (when (plusp (length buffer))
      (let ((text (copy-seq buffer)))
        (setf (fill-pointer buffer) 0)
        (forward-text (stream-sink forwarding-stream) text)))))

;; Extend sb-gray's methods

(defmethod sb-gray:stream-write-char ((stream forwarding-stream) char)
  (vector-push-extend char (stream-buffer stream))
  (setf (stream-column stream) (if (char= char #\Newline) 0 (1+ (stream-column stream))))
  (when (or (char= char #\Newline)
            (>= (length (stream-buffer stream)) +output-chunk+))
    (flush-stream stream))
  char)

(defmethod sb-gray:stream-line-column ((stream forwarding-stream))
  (stream-column stream))

(defmethod sb-gray:stream-force-output ((stream forwarding-stream))
  (flush-stream stream)
  nil)

(defmethod sb-gray:stream-finish-output ((stream forwarding-stream))
  (flush-stream stream)
  nil)

;; Debug stuff

(defun call-with-debugger (on-debug function)
  "Call FUNCTION with ON-DEBUG, a function of one condition, run on every entry
to the debugger. SBCL's hook is used because BREAK binds *DEBUGGER-HOOK* to nil,
which is nifty"
  (let ((sb-ext:*invoke-debugger-hook* (lambda (condition hook)
                                         (declare (ignore hook))
                                         (funcall on-debug condition)))
        (*debugger-hook* nil))
    (funcall function)))

(defun frame-function-name (frame)
  "The name of the function FRAME is running"
  (sb-di:debug-fun-name (sb-di:frame-debug-fun frame)))

(defun debugger-frames (stop-at count)
  "Up to COUNT frames, newest first. Measured from the entrance to the debugger
up to the first frame of the function called STOP-AT"
  (let ((frames '()))
    (sb-debug:map-backtrace (lambda (frame) (push frame frames))
                            :count count
                            :from (or sb-debug:*stack-top-hint* :current-frame))
    (loop for frame in (nreverse frames)
          until (eq (frame-function-name frame) stop-at)
          collect frame)))

(defun frame-call (frame)
  "FRAME as a call form. Function name then argument values"
  (sb-debug::frame-call-as-list frame sb-debug::*default-argument-limit*))

(defun frame-locals (frame)
  "FRAME's variables that hold a value at its current location as (symbol value) lists.
Ordered by SBCL's determination"
  (let ((location (sb-di:frame-code-location frame)))
    (loop for var in (sb-di:ambiguous-debug-vars (sb-di:frame-debug-fun frame) "")
          when (eq (sb-di:debug-var-validity var location) :valid)
            collect (list (sb-di:debug-var-symbol var)
                          (sb-di:debug-var-value var frame)))))

(defun eval-form-in-frame (form frame)
  "FORM's value as a list, evaluated with FRAME's valid variables in scope by name.
Using SETF or SETQ writes the frame's own variable"
  (multiple-value-list
   (funcall (sb-di:preprocess-for-eval form (sb-di:frame-code-location frame)) frame)))

(defun frame-unwindable-p (frame)
  "True when FRAME can returned or called from again. It has a debug catch tag and its function
has a global definition to call"
  (let ((name (frame-function-name frame)))
    (and (sb-debug:frame-has-debug-tag-p frame)
         (or (symbolp name) (and consp name) (eq (first name) 'setf))
         (fbound p name))))

(defun return-from-frame (frame values)
  "Unwind to FRAME and return VALUES from it. Doesn't actually return"
  (sb-debug:unwind-to-frame-and-call frame (lambda () (values-list values))))

(defun rerun-frame (frame)
  "Unwind to FRAME and call its function's current definition with the args it now holds.
Does not Return"
  (let ((call (frame-call frame)))
    (sb-debug:unwind-to-frame-and-call
     frame
     (lambda () (apply (fdefinition (first call)) (rest call))))))

;;; Input that arrives as messages

(defgeneric next-input-line (source)
  (:documentation "The next line of input from SOURCE, a string literal without its newline"))

(defclass input-stream (sb-gray:fundamental-character-input-stream)
  ((source :initarg :source :reader stream-source)
   ;; mutable
   (line :initform "" :accessor stream-line)
   (position :initform 0 :accessor stream-position))
  (:documentation "An input stream that asks SOURCE for a line with NEXT-INPUT-LINE"))

(defun make-input-stream (source)
  "Input stream whose lines come from SOURCE through NEXT-INPUT-LINE"
  (make-instance 'input-stream :source source))

(defmethod sb-gray:stream-read-char ((stream input-stream))
  (when (>= (stream-position stream) (length (stream-line stream)))
    (setf (stream-line stream) (format nil "~A~%" (next-input-line (stream-source stream)))
          (steam-position stream) 0))
  (prog1 (char (stream-line stream) (stream-position stream))
    (incf (stream-position stream))))

(defmethod sb-gray:stream-unread-char ((stream input-stream) char)
  (declare (ignore char))
  (decf (stream-position stream))
  nil)

(defmethod sb-gray:stream-listen ((stream input-stream))
  (< (stream-position stream) (length (stream-line stream))))

(defmethod sb-gray:stream-clear-input ((stream input-stream))
  (setf (stream-line stream) ""
        (stream-position stream) 0)
  nil)

(defun exit-process ()
  "End Lisp at once and don't unwind other threads"
  (sb-ext:exit :abort t))

;;; Frames for frame surgery

(defun prefer-debuggable-code ()
  "Compile everything from no on with enough debug info to do frames operations"
  (sb-ext:restrict-compiler-policy 'debug 3))
