;;;; agent/sbcl.lisp: every SBCL-specific call lives here

(in-package #:to-restart-is-divine/agent)

;;; Unix socket without sb-bsd-sockets

(defconstant +af-unix+ 1)
(defconstant +sock-stream+ 1)
(defconstant +max-socket-path+ 103
  "Longest path (in octets) that fits in sockaddr_un on every target, leaving room for NUL")

(defconstant +output-chunk+ 4096
  "How many characters we buffer before sending, even without a newline")

(defparameter *sleep-functions*
  (list 'sb-unix:nanosleep 'sb-unix::nanosleep-float 'sb-unix::nanosleep-double)
  "Every function CL:SLEEP does its waiting in, depending on what kind of number it gets")

(defparameter *tested-sbcl-version* "2.6.10"
  "The SBCL the Interrupt and frame code was tested on. They lean on its internals, so the
agent won't start on any other. Bump it once the Interrupt tests pass on a new one")

(defparameter *sbcl-internals*
  (list (list :function 'sb-unix:nanosleep)
        (list :function 'sb-unix::nanosleep-float)
        (list :function 'sb-unix::nanosleep-double)
        (list :function 'sb-di::compiled-frame-escaped)
        (list :function 'sb-debug::frame-call-as-list)
        (list :variable 'sb-debug::*default-argument-limit*)
        (list :variable 'sb-kernel:*free-interrupt-context-index*))
  "SBCL internals the agent uses, as (kind symbol). Reading a :: symbol just interns it if
it's gone, so nothing fails until it's called. CHECK-SBCL-INTERNALS catches that up front")

(define-condition unsupported-sbcl (error)
  ((problems :initarg :problems :reader unsupported-sbcl-problems))
  (:report (lambda (condition stream)
             (format stream "The agent can't run on this SBCL:~{~%  ~A~}"
                     (unsupported-sbcl-problems condition)))))

(sb-alien:define-alien-routine ("socket" %socket) sb-alien:int
  (domain sb-alien:int) (type sb-alien:int) (protocol sb-alien:int))

(sb-alien:define-alien-routine ("connect" %connect) sb-alien:int
  (fd sb-alien:int) (address sb-alien:system-area-pointer) (length (sb-alien:unsigned 32)))

(defun socket-address (path)
  "PATH as a sockaddr_un: two-byte header, then path, NUL-terminated"
  (let ((path-octets (sb-ext:string-to-octets path :external-format :utf-8 :null-terminate t)))
    (when (> (length path-octets) (1+ +max-socket-path+))
      (error "The socket path ~A is longer than ~D octets." path +max-socket-path+))
    (concatenate '(vector (unsigned-byte 8))
                 #+linux (vector +af-unix+ 0) ; sun_family, little-endian short
                 #-linux (vector (+ 2 (length path-octets)) +af-unix+) ; sun_len, then sun_family
                 path-octets)))

(defun connect-unix-socket (path)
  "Two-way UTF-8 character stream on unix socket at PATH"
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
  "Return once THREAD is done: what its function returned, or :ABORTED"
  (sb-thread:join-thread thread :default :aborted))

(defun abort-this-thread ()
  "End the current thread, unwinding it on the way out"
  (sb-thread:abort-thread))

(defun interrupt (thread function)
  "Run FUNCTION in THREAD as soon as it's interruptible. FUNCTION starts with interrupts off,
so another interrupt can't land on top of it; it turns them on with CALL-WITH-INTERRUPTS
once that's safe. :SENT, or :THREAD-GONE"
  (handler-case (progn (sb-thread:interrupt-thread thread function)
                       :sent)
    (sb-thread:interrupt-thread-error () :thread-gone)))

(defun call-with-interrupts (function)
  "Call FUNCTION with interrupts turned on"
  (sb-sys:with-interrupts (funcall function)))

(defun thread-alive-p (thread)
  "True while THREAD is still running"
  (sb-thread:thread-alive-p thread))

;;; Where an interrupt landed. SBCL pushes an interrupt context when an interrupt lands in
;;; Lisp code, and pushes nothing when it lands in C. So inside the interrupt, a depth past
;;; the one the interrupted code ran at means Lisp. Reading it touches no debug info, which
;;; matters: decoding debug info calls malloc, and the C we landed in might hold malloc's lock

(defun interrupt-context-depth ()
  "How many interrupt contexts SBCL has pushed on this thread right now"
  sb-kernel:*free-interrupt-context-index*)

(defun check-sbcl-internals ()
  ":OK if this is the tested SBCL and every internal we use is there. Signals
UNSUPPORTED-SBCL otherwise"
  (let ((problems (sbcl-problems)))
    (if (null problems)
        :ok
        (error 'unsupported-sbcl :problems problems))))

(defun sbcl-problems ()
  "What's wrong with this SBCL, as lines of text. Empty when nothing is"
  (append (unless (string= (lisp-implementation-version) *tested-sbcl-version*)
            (list (format nil "it's version ~A, and the agent is tested on ~A"
                          (lisp-implementation-version) *tested-sbcl-version*)))
          (loop for (kind symbol) in *sbcl-internals*
                unless (ecase kind
                         (:function (fboundp symbol))
                         (:variable (boundp symbol)))
                  collect (format nil "~(~A~) ~S is missing" kind symbol))))

(defun wrap-sleep (wrapper)
  "Route every wait CL:SLEEP does through WRAPPER, a function of the real sleep function and
its arguments. Wrapping a second time does nothing"
  (sb-ext:without-package-locks
    (dolist (name *sleep-functions*)
      (unless (sb-int:encapsulated-p name 'agent-wrapper)
        (sb-int:encapsulate name 'agent-wrapper wrapper)))))

;; Someone call Al Gore

(defun make-lock (name)
  "A lock called NAME"
  (sb-thread:make-mutex :name name))

(defun holding-lock-p (lock)
  "True when this thread holds LOCK"
  (sb-thread:holding-mutex-p lock))

(defun call-with-lock (lock function)
  "Call FUNCTION holding LOCK. Interrupts wait until we let go"
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
  "Put MESSAGE at the back of MAILBOX"
  (sb-thread:with-mutex ((mailbox-lock mailbox))
    (setf (mailbox-messages mailbox) (append (mailbox-messages mailbox) (list message)))
    (sb-thread:condition-notify (mailbox-ready mailbox))))

(defun mailbox-receive (mailbox)
  "Oldest message in MAILBOX, waiting for one if it's empty"
  (sb-thread:with-mutex ((mailbox-lock mailbox))
    (loop while (null (mailbox-messages mailbox))
          do (sb-thread:condition-wait (mailbox-ready mailbox) (mailbox-lock mailbox)))
    (pop (mailbox-messages mailbox))))

;; Stream things

(defgeneric forward-text (sink text)
  (:documentation "Take TEXT, one flushed chunk of a forwarding stream's output, and pass it on"))

(defclass forwarding-stream (sb-gray:fundamental-character-output-stream)
  ((sink :initarg :sink :reader stream-sink)
   (buffer :initform (make-array 0 :element-type 'character :adjustable t :fill-pointer t)
           :reader stream-buffer)
   (column :initform 0 :accessor stream-column))
  (:documentation "Output stream that hands its text to SINK with FORWARD-TEXT, a line at a time"))

(defun make-forwarding-stream (sink)
  "Output stream whose text goes to SINK through FORWARD-TEXT"
  (make-instance 'forwarding-stream :sink sink))

(defun flush-stream (forwarding-stream)
  "Hand whatever FORWARDING-STREAM has buffered to its sink"
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
  "Call FUNCTION with ON-DEBUG (a function of one condition) running on every debugger
entry. Uses SBCL's hook because BREAK binds *DEBUGGER-HOOK* to NIL, which is nifty"
  (let ((sb-ext:*invoke-debugger-hook* (lambda (condition hook)
                                         (declare (ignore hook))
                                         (funcall on-debug condition)))
        (*debugger-hook* nil))
    (funcall function)))

(defun frame-function-name (frame)
  "Name of the function FRAME is running"
  (sb-di:debug-fun-name (sb-di:frame-debug-fun frame)))

(defun debugger-frames (stop-at count)
  "Up to COUNT frames, newest first, from where the debugger was entered down to (not
including) the first frame of STOP-AT. Near where an Interrupt landed, walking down can hit
a garbage frame and signal; then you get the frames above it. That error mustn't escape:
SBCL turns our hook off while it runs, so it'd land in SBCL's own debugger and hang"
  (let ((frames '()))
    (handler-case (sb-debug:map-backtrace (lambda (frame) (push frame frames))
                                          :count count
                                          :from (or sb-debug:*stack-top-hint* :current-frame))
      (error () :walk-stopped))
    (loop for frame in (nreverse frames)
          until (eq (frame-function-name frame) stop-at)
          collect frame)))

(defun frame-escaped-p (frame)
  "True when an interrupt or trap stopped FRAME mid-instruction, rather than at a call"
  (and (typep frame 'sb-di::compiled-frame)
       (sb-di::compiled-frame-escaped frame)
       t))

(defun frame-readable-p (frame)
  "True when FRAME's args and locals read without an error. Near where an Interrupt landed,
a frame can point at garbage, and reading it faults or trips a type check. Reading garbage
doesn't write anything, so catching it here is safe"
  (handler-case (progn (frame-call frame)
                       (frame-locals frame)
                       t)
    (error () nil)))

(defun frame-call (frame)
  "FRAME as a call form: function name, then its arguments as they are right now"
  (sb-debug::frame-call-as-list frame sb-debug::*default-argument-limit*))

(defun frame-locals (frame)
  "FRAME's variables that hold a value where it's stopped, as (symbol value) lists, in
whatever order SBCL gives them"
  (let ((location (sb-di:frame-code-location frame)))
    (loop for var in (sb-di:ambiguous-debug-vars (sb-di:frame-debug-fun frame) "")
          when (eq (sb-di:debug-var-validity var location) :valid)
            collect (list (sb-di:debug-var-symbol var)
                          (sb-di:debug-var-value var frame)))))

(defun eval-form-in-frame (form frame)
  "FORM's values as a list, evaluated with FRAME's live variables in scope by name.
SETF or SETQ on one writes the frame's own variable"
  (multiple-value-list
   (funcall (sb-di:preprocess-for-eval form (sb-di:frame-code-location frame)) frame)))

(defun frame-unwindable-p (frame)
  "True when you can return from FRAME or call it again: it has a debug catch tag and its
function has a global definition to call"
  (let ((name (frame-function-name frame)))
    (and (sb-debug:frame-has-debug-tag-p frame)
         (or (symbolp name) (and (consp name) (eq (first name) 'setf)))
         (fboundp name))))

(defun return-from-frame (frame values)
  "Unwind to FRAME and return VALUES (a list) from it. Never comes back here"
  (sb-debug:unwind-to-frame-and-call frame (lambda () (values-list values))))

(defun rerun-frame (frame)
  "Unwind to FRAME and call its function's current definition with whatever args it holds
now. Never comes back here"
  (let ((call (frame-call frame)))
    (sb-debug:unwind-to-frame-and-call
     frame
     (lambda () (apply (fdefinition (first call)) (rest call))))))

;;; Input that arrives as messages

(defgeneric next-input-line (source)
  (:documentation "Next line of input from SOURCE, as a string without its newline"))

(defclass input-stream (sb-gray:fundamental-character-input-stream)
  ((source :initarg :source :reader stream-source)
   ;; mutable
   (line :initform "" :accessor stream-line)
   (position :initform 0 :accessor stream-position))
  (:documentation "Input stream that asks SOURCE for each line with NEXT-INPUT-LINE"))

(defun make-input-stream (source)
  "Input stream whose lines come from SOURCE through NEXT-INPUT-LINE"
  (make-instance 'input-stream :source source))

(defmethod sb-gray:stream-read-char ((stream input-stream))
  (when (>= (stream-position stream) (length (stream-line stream)))
    (setf (stream-line stream) (format nil "~A~%" (next-input-line (stream-source stream)))
          (stream-position stream) 0))
  (prog1 (char (stream-line stream) (stream-position stream))
    (incf (stream-position stream))))

(defmethod sb-gray:stream-unread-char ((stream input-stream) char)
  (declare (ignore char))
  (decf (stream-position stream))
  nil)

(defmethod sb-gray:stream-listen ((stream input-stream))
  (< (stream-position stream) (length (stream-line stream))))

;;; SBCL's default STREAM-READ-CHAR-NO-HANG just calls STREAM-READ-CHAR, which would
;;; go ask the page for a line and sit there. NIL is Gray-speak for "nothing ready".
(defmethod sb-gray:stream-read-char-no-hang ((stream input-stream))
  (if (sb-gray:stream-listen stream)
      (sb-gray:stream-read-char stream)
      nil))

(defmethod sb-gray:stream-clear-input ((stream input-stream))
  (setf (stream-line stream) ""
        (stream-position stream) 0)
  nil)

(defun exit-process ()
  "End Lisp right now, without unwinding other threads"
  (sb-ext:exit :abort t))

;;; Frames for frame surgery

(defun prefer-debuggable-code ()
  "Compile everything from here on at (debug 3), so frames stick around (no tail-call
merging) and keep their locals for frame surgery"
  (sb-ext:restrict-compiler-policy 'debug 3))
