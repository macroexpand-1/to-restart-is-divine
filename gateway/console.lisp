;;;; gateway/console.lisp: REPL line history and debugger stack, as plain values

(in-package #:to-restart-is-divine/gateway)

;;; REPL line history

(defstruct (history (:constructor %make-history (entries cursor draft)))
  (entries '() :read-only t)            ; submitted lines, newest first
  (cursor :none :read-only t)           ; index into ENTRIES while recalling, or :NONE
  (draft "" :read-only t))              ; unsent line we were on when recalling started

(defun make-history ()
  "Empty history"
  (%make-history '() :none ""))

(defun blank-line-p (line)
  "True when LINE is nothing but whitespace"
  (every (lambda (char) (member char '(#\Space #\Tab #\Newline #\Return))) line))

;;; Fold: take one submitted line into the history
;;; (history line) -> history

(defun fold-submitted-line (history line)
  "HISTORY with LINE added and cursor reset. Blank lines and repeats of the newest entry get skipped"
  (let ((entries (history-entries history)))
    (%make-history (if (or (blank-line-p line) (equal line (first entries)))
                       entries
                       (cons line entries))
                   :none
                   "")))

;;; Step: the transition for one arrow key while recalling
;;; (history direction typed) -> (values history line). TYPED is what the REPL line
;;; held when the key got pressed. LINE is what it shows now, or :NONE to leave it alone

(defun step-recall (history direction typed)
  "Step HISTORY one entry in DIRECTION (:OLDER or :NEWER). If this starts a recall,
TYPED becomes the draft"
  (ecase direction
    (:older (step-recall-older history typed))
    (:newer (step-recall-newer history))))

(defun step-recall-older (history typed)
  "Go back one entry, keeping TYPED as the draft if this starts the recall.
Line is :NONE once we're at the oldest"
  (let* ((entries (history-entries history))
         (cursor (history-cursor history))
         (older (if (eq cursor :none) 0 (1+ cursor)))
         (draft (if (eq cursor :none) typed (history-draft history))))
    (if (< older (length entries))
        (values (%make-history entries older draft) (nth older entries))
        (values history :none))))

(defun step-recall-newer (history)
  "Go forward one entry. Past the newest you get the draft back and the cursor clears.
Line is :NONE if we're not recalling"
  (let ((entries (history-entries history))
        (cursor (history-cursor history))
        (draft (history-draft history)))
    (cond ((eq cursor :none) (values history :none))
          ((zerop cursor) (values (%make-history entries :none "") draft))
          (t (values (%make-history entries (1- cursor) draft) (nth (1- cursor) entries))))))

;;; Debugger stack

(defstruct debug-level
  (id nil :read-only t)
  (depth nil :read-only t)
  (type nil :read-only t)
  (report nil :read-only t)
  (restarts '() :read-only t)
  (backtrace '() :read-only t)
  (unwindable '() :read-only t)
  (unreadable '() :read-only t) ; frames whose args and locals can't be read
  (frames :read :read-only t)   ; :LOST when the agent couldn't read the stack at all
  (locals '() :read-only t))

(defun event-debug-level (event)
  "Debugger level that a (:level-opened ...) EVENT describes"
  (destructuring-bind (&key id depth type report restarts backtrace unwindable unreadable
                         (frames :read)
                       &allow-other-keys)
      (rest event)
    (make-debug-level :id id :depth depth :type type :report report
                      :restarts restarts :backtrace backtrace
                      :unwindable unwindable :unreadable unreadable
                      :frames (ecase frames ((:read :lost) frames)))))

(defun debug-level-wire (debug-level)
  "The (:level-opened ...) list EVENT-DEBUG-LEVEL reads from DEBUG-LEVEL.
Fetched locals stay out since no :level-opened carries them"
  (list :level-opened
        :id (debug-level-id debug-level)
        :depth (debug-level-depth debug-level)
        :type (debug-level-type debug-level)
        :report (debug-level-report debug-level)
        :restarts (debug-level-restarts debug-level)
        :backtrace (debug-level-backtrace debug-level)
        :unwindable (debug-level-unwindable debug-level)
        :unreadable (debug-level-unreadable debug-level)
        :frames (debug-level-frames debug-level)))

(defun push-level (levels debug-level)
  "LEVELS with DEBUG-LEVEL opened on top"
  (cons debug-level levels))

;;; Closing a level drops it along with every newer one
;;; (levels id) -> levels

(defun drop-level (levels id)
  "LEVELS minus level ID and everything newer. Unknown ID changes nothing"
  (let ((tail (member id levels :key #'debug-level-id)))
    (if tail
        (rest tail)
        levels)))

(defun fold-locals (levels frame-ref pairs)
  "LEVELS with the level FRAME-REF names holding PAIRS as that frame's locals, replacing any it had.
Level that isn't there changes absolutely nothing"
  (mapcar (lambda (debug-level)
            (if (eql (debug-level-id debug-level) (frame-ref-level-id frame-ref))
                (debug-level-with-locals debug-level (frame-ref-frame-index frame-ref) pairs)
                debug-level))
          levels))

(defun debug-level-with-locals (debug-level frame-index pairs)
  "Copy of DEBUG-LEVEL holding PAIRS as frame FRAME-INDEX's locals"
  (make-debug-level :id (debug-level-id debug-level)
                    :depth (debug-level-depth debug-level)
                    :type (debug-level-type debug-level)
                    :report (debug-level-report debug-level)
                    :restarts (debug-level-restarts debug-level)
                    :backtrace (debug-level-backtrace debug-level)
                    :unwindable (debug-level-unwindable debug-level)
                    :unreadable (debug-level-unreadable debug-level)
                    :frames (debug-level-frames debug-level)
                    :locals (acons frame-index pairs
                                   (remove frame-index (debug-level-locals debug-level) :key #'car))))

(defun frame-unwindable-in-p (debug-level frame-index)
  "True when you can rerun or return from frame FRAME-INDEX of DEBUG-LEVEL"
  (member frame-index (debug-level-unwindable debug-level)))

(defun frame-readable-in-p (debug-level frame-index)
  "True when frame FRAME-INDEX of DEBUG-LEVEL has locals you can look at"
  (not (member frame-index (debug-level-unreadable debug-level))))

(defun local-assignment (name text)
  "Text of a form setting variable NAME to TEXT. We don't read either string here"
  (format nil "(setf ~A ~A)" name text))

(defun restart-label (restart)
  "Button text for RESTART, an (index name report) list"
  (destructuring-bind (index name report) restart
    (format nil "~D: [~A] ~A" index name report)))

(defun values-lines (printed-values)
  "One scrollback line per printed value, or a note if there aren't any"
  (or printed-values (list ";; No values")))
