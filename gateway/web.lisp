;;;; gateway/web.lisp: the CLOG page (only file that talks to CLOG)

(defpackage #:to-restart-is-divine/web
  (:use #:cl)
  (:local-nicknames (#:g #:to-restart-is-divine/gateway))
  (:export #:start-web #:stop-web))

(in-package #:to-restart-is-divine/web)

(defparameter *page-css*
  (format nil "~{~A~^ ~}"
          '("body { margin: 0; height: 100vh; display: grid; grid-template-columns: 2fr 1fr; font: 16px/1.5 ui-monospace, Menlo, monospace; }"
            ".console { display: flex; flex-direction: column; min-height: 0; padding: 1rem; gap: 0.5rem; }"
            ".scrollback { flex: 1; overflow-y: auto; white-space: pre-wrap; }"
            ".echo { font-weight: bold; }"
            ".panel { flex: none; max-height: 40vh; overflow-y: auto; }"
            ".panel:empty { display: none; }"
            ".level { border-left: 4px solid firebrick; padding: 0.5rem; margin-bottom: 0.5rem; }"
            ".condition { white-space: pre-wrap; }"
            ".restart { display: block; margin: 0.25rem 0; text-align: left; font: inherit; }"
            ".frame { color: dimgray; }"
            ".unreadable { font-style: italic; opacity: 0.6; }"
            ".call { margin-right: 0.5rem; }"
            ".frame-action { font: inherit; margin-left: 0.25rem; }"
            ".frame-value { font: inherit; width: 8rem; margin-left: 0.25rem; }"
            ".local { margin-left: 2rem; color: black; }"
            ".repl-line { font: inherit; padding: 0.25rem; resize: none; overflow: hidden; }"
            ".side { padding: 1rem; overflow-y: auto; border-left: 1px solid silver; }"))
  "Page's stylesheet, all on one line with no quote characters")

;;; One tab: its elements, its console values, its session, and the lock that
;;; keeps its DOM writes one at a time

(defstruct (page (:constructor %make-page))
  (scrollback nil :read-only t)
  (panel nil :read-only t)
  (repl-line nil :read-only t)
  (interrupt-button nil :read-only t)
  (session-kind nil :read-only t)
  (lock (sb-thread:make-mutex :name "page") :read-only t)
  (start-over-lock (sb-thread:make-mutex :name "page start over") :read-only t)
  ;; mutable
  session
  (history (g:make-history))
  (levels '())
  (needs-input :none))  ; :none, or the level a read waits in: its id, or :top

(defun start-web (session-kind &key (port 8417))
  "Serve the console on 127.0.0.1:PORT. Each tab opens a session of SESSION-KIND
(a keyword or kind struct with a G:OPEN-SESSION method)"
  (setf clog-connection:*break-on-error* nil)
  (clog:initialize (lambda (body) (on-new-window body session-kind))
                   :host "127.0.0.1"
                   :port port))

(defun stop-web ()
  "Stop serving every tab"
  (clog:shutdown))

(defun on-new-window (body session-kind)
  "Build one tab's page and run it until the tab closes, then close its session"
  (let ((page (build-page body session-kind)))
    (setf (page-session page) (g:open-session session-kind page))
    (attach-page-callbacks page)
    (clog:run body)
    ;; the lock waits out a Start over still swapping sessions, so we close the new one
    (sb-thread:with-mutex ((page-start-over-lock page))
      (g:session-close (page-session page)))))

(defun build-page (body session-kind)
  "Lay out the page in BODY and return its struct, which opens sessions of SESSION-KIND.
Callbacks get attached separately"
  (setf (clog:title (clog:html-document body)) "To Restart Is Divine")
  (clog:create-style-block body :content *page-css*)
  (let* ((console (clog:create-div body :class "console"))
         (scrollback (clog:create-div console :class "scrollback"))
         (panel (clog:create-div console :class "panel"))
         (repl-line (clog:create-text-area console :rows 1 :class "repl-line"))
         (interrupt-button (clog:create-button console :content "Interrupt" :class "interrupt"))
         (side (clog:create-div body :class "side")))
    (setf (clog:text side) "Lesson prose goes here once lessons exist.")
    (%make-page :scrollback scrollback :panel panel
                :repl-line repl-line :interrupt-button interrupt-button
                :session-kind session-kind)))

(defun attach-page-callbacks (page)
  "Hook up REPL line and Interrupt callbacks, then focus the REPL line"
  (attach-repl-line-callbacks page)
  (attach-interrupt-callback page)
  (sb-thread:with-mutex ((page-lock page))
    (clog:focus (page-repl-line page))))

(defun attach-repl-line-callbacks (page)
  "Install REPL line's key script and handle the events it sends"
  (let ((repl-line (page-repl-line page)))
    (clog:js-execute repl-line (repl-line-script (clog:script-id repl-line)))
    (clog:set-on-event-with-data repl-line "repl-submit"
                                 (lambda (repl-line text)
                                   (declare (ignore repl-line))
                                   (submit-line page text)))
    (clog:set-on-event-with-data repl-line "repl-older"
                                 (lambda (repl-line typed)
                                   (declare (ignore repl-line))
                                   (recall page :older typed)))
    (clog:set-on-event-with-data repl-line "repl-newer"
                                 (lambda (repl-line typed)
                                   (declare (ignore repl-line))
                                   (recall page :newer typed)))))

(defun attach-interrupt-callback (page)
  "Interrupt the session when someone clicks Interrupt"
  (clog:set-on-click (page-interrupt-button page)
                     (lambda (button)
                       (declare (ignore button))
                       (on-interrupt-click page))))

(defun repl-line-script (id)
  "REPL line's key handling, run in the browser on the element named ID.
Enter (no Shift) grabs the text and clears the line in one go, then sends it as a
repl-submit event, so a key typed before the server answers never gets mixed in.
Shift+Enter is a newline. ArrowUp on the first line sends repl-older and ArrowDown
on the last sends repl-newer, each with whatever the line holds. Line grows to fit"
  (format nil "(function (line) {
  function fit () { line.style.height = 'auto'; line.style.height = line.scrollHeight + 'px'; }
  function recall (e, name) {
    e.preventDefault();
    line.dataset.recallFrom = line.value;
    $(line).trigger(name, [line.value]);
  }
  line.addEventListener('input', fit);
  line.addEventListener('keydown', function (e) {
    if (e.isComposing) return;
    var before = line.value.slice(0, line.selectionStart);
    var after = line.value.slice(line.selectionEnd);
    if (e.key === 'Enter' && !e.shiftKey) {
      e.preventDefault();
      var text = line.value;
      line.value = '';
      fit();
      $(line).trigger('repl-submit', [text]);
    } else if (e.key === 'ArrowUp' && before.indexOf('\\n') < 0) {
      recall(e, 'repl-older');
    } else if (e.key === 'ArrowDown' && after.indexOf('\\n') < 0) {
      recall(e, 'repl-newer');
    }
  });
})(~A)" id))

(defun submit-line (page line)
  "Echo and record LINE (browser already took it off the REPL line), then send it.
The send happens outside the lock"
  (note-unsent-line page (send-submitted-line page line (record-submitted-line page line))))

(defun note-unsent-line (page outcome)
  "Tell the page when a line was too long to send. OUTCOME is what the send returned"
  (when (eq outcome :too-long)
    (sb-thread:with-mutex ((page-lock page))
      (append-line page "note" ";; That line is too long to send."))))

(defun record-submitted-line (page line)
  "Echo LINE and add it to history. Returns its route, :INPUT or :EVAL"
  (sb-thread:with-mutex ((page-lock page))
    (let ((route (line-route page)))
      (setf (page-history page) (g:fold-submitted-line (page-history page) line))
      (append-line page "echo" (format nil (ecase route (:input "> ~A") (:eval "* ~A")) line))
      route)))

(defun line-route (page)
  "Where Enter sends the line. :INPUT when the agent's waiting on a read and the level
it waits in is still the newest, so a level opened on top of a waiting read gets Enter
for itself. Otherwise :EVAL. Caller holds page lock"
  (if (eql (page-needs-input page) (newest-level-id page))
      :input
      :eval))

(defun newest-level-id (page)
  "Id of PAGE's newest open level, or :TOP. Caller holds page lock"
  (if (page-levels page)
      (g:debug-level-id (first (page-levels page)))
      :top))

(defun send-submitted-line (page line route)
  "Send LINE to the session the way ROUTE says. A blank :eval line is :BLANK and doesn't get sent"
  (ecase route
    (:input (g:session-input (page-session page) line))
    (:eval (if (g:blank-line-p line)
               :blank
               (g:session-eval (page-session page) line)))))

(defun recall (page direction typed)
  "Step history in DIRECTION (:OLDER or :NEWER) starting from TYPED, which is whatever
the REPL line held, and show the line we land on"
  (sb-thread:with-mutex ((page-lock page))
    (multiple-value-bind (history line) (g:step-recall (page-history page) direction typed)
      (setf (page-history page) history)
      (etypecase line
        (string (show-recalled-line (page-repl-line page) line))
        ((eql :none) :none)))))

(defun show-recalled-line (repl-line line)
  "Put LINE in REPL-LINE with the cursor at the end, unless visitor typed something since
asking for the recall. Browser does that check, so no keystroke gets clobbered"
  (clog:js-execute repl-line
                   (format nil "(function (line) {
  if (line.value !== line.dataset.recallFrom) return;
  line.value = '~A';
  line.selectionStart = line.selectionEnd = line.value.length;
  line.dispatchEvent(new Event('input'));
})(~A)"
                           (clog:escape-string line)
                           (clog:script-id repl-line))))

(defun on-restart-click (page id index)
  "Invoke restart INDEX of level ID, then put focus back on the REPL line"
  (g:session-invoke-restart (page-session page) id index)
  (sb-thread:with-mutex ((page-lock page))
    (clog:focus (page-repl-line page))))

(defun on-interrupt-click (page)
  "Interrupt the session, then put focus back on the REPL line"
  (g:session-interrupt (page-session page))
  (sb-thread:with-mutex ((page-lock page))
    (clog:focus (page-repl-line page))))

(defun on-locals-click (page id frame-index)
  "Ask for frame FRAME-INDEX's locals"
  (g:session-frame-locals (page-session page) id frame-index)
  (focus-repl-line page))

(defun on-set-click (page id frame-index name text)
  "Set local NAME of frame FRAME-INDEX to TEXT. Empty box is :BLANK and sends nothing"
  (if (g:blank-line-p text)
      :blank
      (g:session-eval-in-frame (page-session page) id frame-index (g:local-assignment name text)))
  (focus-repl-line page))

(defun on-return-click (page id frame-index text)
  "Return TEXT's values from frame FRAME-INDEX. Empty box returns NIL"
  (g:session-return-from-frame (page-session page) id frame-index
                               (if (g:blank-line-p text) "nil" text))
  (focus-repl-line page))

(defun on-rerun-click (page id frame-index)
  "Call frame FRAME-INDEX again with whatever args it holds now"
  (g:session-rerun-frame (page-session page) id frame-index)
  (focus-repl-line page))

(defun focus-repl-line (page)
  "Put focus back on PAGE's REPL line"
  (sb-thread:with-mutex ((page-lock page))
    (clog:focus (page-repl-line page))))

(defmethod g:receive-event ((page page) event)
  "Show one session EVENT"
  (sb-thread:with-mutex ((page-lock page))
    (ecase (first event)
      (:ready
       (append-line page "note" ";; Ready."))
      (:output
       (append-line page "output" (second event)))
      (:values
       (dolist (line (g:values-lines (second event)))
         (append-line page "value" line)))
      (:level-opened
       (setf (page-levels page) (g:push-level (page-levels page) (g:event-debug-level event)))
       (render-panel page))
      (:level-closed
       (setf (page-levels page) (g:drop-level (page-levels page) (second event)))
       (render-panel page))
      (:locals
       (destructuring-bind (id frame-index pairs) (rest event)
         (setf (page-levels page) (g:fold-locals (page-levels page) id frame-index pairs)))
       (render-panel page))
      (:needs-input
       (note-needs-input page))
      (:input-done
       (setf (page-needs-input page) :none))
      (:interrupt-waiting
       (append-line page "note"
                    ";; Interrupt is waiting: the code is in foreign (C) code. It lands once that returns.")
       (append-start-over page "If it never returns, Start over. That throws away everything this session defined."))
      (:interrupt-missed
       (append-line page "note" ";; Interrupt didn't land: the evaluation finished first."))
      (:agent-gone
       (append-line page "note" ";; The Lisp behind this page died.")
       (append-start-over page "Start over to get a fresh one. Nothing this session defined comes back.")))))

(defun append-start-over (page text)
  "Tack TEXT and a Start over button onto the scrollback. Caller holds page lock"
  (let ((scrollback-line (clog:create-div (page-scrollback page) :class "note")))
    (setf (clog:text (clog:create-span scrollback-line :class "call")) (format nil ";; ~A" text))
    (clog:set-on-click (clog:create-button scrollback-line :content "Start over" :class "frame-action")
                       (lambda (button)
                         (declare (ignore button))
                         (start-over page)))
    (scroll-to-end (page-scrollback page))))

(defun start-over (page)
  "Swap PAGE's session for a fresh one of the same kind and clear the page. Closing the old
session kills its process, stuck or not. :STARTED-OVER, or :ALREADY-STARTING-OVER if
another press got here first"
  (let ((started (sb-thread:with-mutex ((page-start-over-lock page) :wait-p nil)
                   (g:session-close (page-session page))
                   (sb-thread:with-mutex ((page-lock page))
                     (clear-page page))
                   (setf (page-session page) (g:open-session (page-session-kind page) page))
                   :started-over)))
    (or started :already-starting-over)))

(defun clear-page (page)
  "Empty PAGE's scrollback, panel and console values, keeping its history. Caller holds
page lock"
  (clog:destroy-children (page-scrollback page))
  (clog:destroy-children (page-panel page))
  (setf (page-levels page) '()
        (page-needs-input page) :none)
  (clog:focus (page-repl-line page)))

(defun note-needs-input (page)
  "Mark PAGE as waiting for a line in its newest level, with a note in the scrollback
the first time. Caller holds page lock"
  (when (eq (page-needs-input page) :none)
    (append-line page "note" ";; Waiting for a line of input."))
  (setf (page-needs-input page) (newest-level-id page)))

(defun append-line (page class text)
  "Tack TEXT onto the scrollback as one line of CLASS and scroll down to it.
Caller holds page lock"
  (let ((scrollback-line (clog:create-div (page-scrollback page) :class class)))
    (setf (clog:text scrollback-line) text)
    (scroll-to-end (page-scrollback page))))

(defun scroll-to-end (element)
  "Scroll ELEMENT to the bottom, no round trip to the browser"
  (let ((id (clog:script-id element)))
    (clog:js-execute element (format nil "~A.scrollTop=~A.scrollHeight" id id))))

(defun render-panel (page)
  "Redraw debugger panel from the level stack, newest first. Caller holds page lock"
  (let ((panel (page-panel page)))
    (clog:with-connection-cache (panel)
      (clog:destroy-children panel)
      (loop for debug-level in (page-levels page)
            for newest = t then nil
            do (render-level page panel debug-level newest)))))

(defun render-level (page panel debug-level newest)
  "Draw one level: its condition, a button per restart, and its frames if it's NEWEST"
  (let ((level-box (clog:create-div panel :class "level")))
    (render-condition level-box debug-level)
    (dolist (restart (g:debug-level-restarts debug-level))
      (render-restart-button page level-box (g:debug-level-id debug-level) restart))
    (when newest
      (render-frames page level-box debug-level))))

(defun render-condition (level-box debug-level)
  "Show DEBUG-LEVEL's depth, condition type and report in LEVEL-BOX"
  (setf (clog:text (clog:create-div level-box :class "condition"))
        (format nil "Debugger level ~D: ~A~%~A"
                (g:debug-level-depth debug-level)
                (g:debug-level-type debug-level)
                (g:debug-level-report debug-level))))

(defun render-restart-button (page level-box level-id restart)
  "Button in LEVEL-BOX that invokes RESTART (an (index name report) list) of level LEVEL-ID"
  (let ((button (clog:create-button level-box :class "restart"))
        (index (first restart)))
    (setf (clog:text button) (g:restart-label restart))
    (clog:set-on-click button
                       (lambda (button)
                         (declare (ignore button))
                         (on-restart-click page level-id index)))))

(defun render-frames (page level-box debug-level)
  "A row in LEVEL-BOX for each frame in DEBUG-LEVEL's backtrace, or a note if the agent
couldn't read the stack"
  (ecase (g:debug-level-frames debug-level)
    (:read (loop for frame-text in (g:debug-level-backtrace debug-level)
                 for frame-index from 0
                 do (render-frame page level-box debug-level frame-index frame-text)))
    (:lost (setf (clog:text (clog:create-div level-box :class "frame unreadable"))
                 "The backtrace couldn't be read here. The restarts still work."))))

(defun render-frame (page level-box debug-level frame-index frame-text)
  "Draw one frame row: its call, Locals, Return and Rerun if they're allowed, and its
locals if we've fetched them. A frame whose args and locals can't be read gets only its
text, dimmed"
  (let* ((id (g:debug-level-id debug-level))
         (readable (g:frame-readable-in-p debug-level frame-index))
         (row (clog:create-div level-box :class (if readable "frame" "frame unreadable"))))
    (setf (clog:text (clog:create-span row :class "call")) frame-text)
    (when readable
      (render-locals-button page row id frame-index))
    (when (g:frame-unwindable-in-p debug-level frame-index)
      (render-unwind-controls page row id frame-index))
    (render-locals page row id frame-index
                   (rest (assoc frame-index (g:debug-level-locals debug-level))))))

(defun render-locals-button (page row id frame-index)
  "Locals button in ROW that asks for frame FRAME-INDEX's locals"
  (let ((button (clog:create-button row :class "frame-action")))
    (setf (clog:text button) "Locals")
    (clog:set-on-click button
                       (lambda (button)
                         (declare (ignore button))
                         (on-locals-click page id frame-index)))))

(defun render-unwind-controls (page row id frame-index)
  "Value box, a Return button that returns the box's text from the frame, and a Rerun button"
  (let ((value-box (clog:create-form-element row :text :class "frame-value"))
        (return-button (clog:create-button row :class "frame-action"))
        (rerun-button (clog:create-button row :class "frame-action")))
    (setf (clog:text return-button) "Return"
          (clog:text rerun-button) "Rerun")
    (clog:set-on-click return-button
                       (lambda (button)
                         (declare (ignore button))
                         (on-return-click page id frame-index (clog:value value-box))))
    (clog:set-on-click rerun-button
                       (lambda (button)
                         (declare (ignore button))
                         (on-rerun-click page id frame-index)))))

(defun render-locals (page row id frame-index pairs)
  "Draw each of PAIRS (the frame's fetched locals) as NAME = value, with a Set box"
  (dolist (pair pairs)
    (destructuring-bind (name printed) pair
      (render-local page row id frame-index name printed))))

(defun render-local (page row id frame-index name printed)
  "One local as NAME = PRINTED, plus a box and a Set button that assigns whatever's in it"
  (let* ((local-row (clog:create-div row :class "local"))
         (label (clog:create-span local-row))
         (value-box (clog:create-form-element local-row :text :class "frame-value"))
         (set-button (clog:create-button local-row :class "frame-action")))
    (setf (clog:text label) (format nil "~A = ~A " name printed)
          (clog:text set-button) "Set")
    (clog:set-on-click set-button
                       (lambda (button)
                         (declare (ignore button))
                         (on-set-click page id frame-index name (clog:value value-box))))))
