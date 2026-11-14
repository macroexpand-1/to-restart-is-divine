;;;; gateway/console.lisp: REPL line history and debugger stack (as plain values)

(in-package #:to-restart-is-divine/gateway)

;;; REPL line history

(defstruct (history (:constructor %make-history (entries cursor draft)))
  (entries '() :read-only t)            ; submitted lines, newest first
  (cursor :none :read-only t)           ; index into ENTRIES while recalling, or :NONE
  (draft "" :read-only t))              ; the unsent line recalling started from

(defun make-history ()
  "Empty history"
  (%make-history '() :none ""))

(defun blank-line-p (line)
  "True when LINE holds only whitespace"
  (every (lambda (char) (member char '(#\Space #\Tab #\Newline #\Return))) line))

;;; Fold - take one submitted line into the history.
;;; (history line) -> history

(defun fold-submitted-line (history line)
  "HISTORY with LINE added and cursor reset. Blank lines and repeats of the newest entry are not added"
  (let ((entries (history-entries history)))
    (%make-history (if (or (blank-line-p line) (equal line (first entries)))
                       entries
                       (cons line entries))
                   :none
                   "")))

;;; Step: the transition for one arrow key while recalling
;;; (history direction typed) -> (values history line). TYPED is what the REPL line
;;; held when the key was pressed. LINE is what it shows now, or :NONE to leave it

(defun step-recall (history direction typed)
  "Step HISTORY one entry in DIRECTION, :OLDER or :NEWER. TYPED becomes the draft
when recalling starts"
  (ecase direction
    (:older (step-recall-older history typed))
    (:newer (step-recall-newer history))))

(defun step-recall-older (history typed)
  "Go to the next older entry, keeping TYPED as the draft if this starts the recall.
The line is :NONE at the oldest"
  (let* ((entries (history-entries history))
         (cursor (history-cursor history))
         (older (if (eq cursor :none) 0 (1+ cursor)))
         (draft (if (eq cursor :none) typed (history-draft history))))
    (if (< older (length entries))
        (values (%make-history entries older draft) (nth older entries))
        (values history :none))))

(defun step-recall-newer (history)
  "Go to the next newer entry. Past the newest, the line is the draft and the cursor clears.
The line is :NONE when not recalling"
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
  (backtrace '() :read-only t))

(defun event-debug-level (event)
  "The debugger level a (:level-opened ...) EVENT describes"
  (destructuring-bind (&key id depth type report restarts backtrace &allow-other-keys)
      (rest event)
    (make-debug-level :id id :depth depth :type type :report report
                      :restarts restarts :backtrace backtrace)))

(defun push-level (levels debug-level)
  "LEVELS with DEBUG-LEVEL opened on top"
  (cons debug-level levels))

;;; Closing a level drops it and every newer one
;;; (levels id) -> levels

(defun drop-level (levels id)
  "LEVELS without level ID and every level newer than it. Unknown IDs change nothing"
  (let ((tail (member id levels :key #'debug-level-id)))
    (if tail
        (rest tail)
        levels)))

(defun restart-label (restart)
  "Button text for RESTART, an (index name report) list"
  (destructuring-bind (index name report) restart
    (format nil "~D: [~A] ~A" index name report)))

(defun values-lines (printed-values)
  "One scrollback line per printed value or a note when there are none"
  (or printed-values (list ";; No values")))
