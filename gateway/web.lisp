;;;; gateway/web.lisp: The CLOG page (the only file that talks to CLOG)

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
            ".repl-line { font: inherit; padding: 0.25rem; resize: none; overflow: hidden; }"
            ".side { padding: 1rem; overflow-y: auto; border-left: 1px solid silver; }"))
  "The page's stylesheet, as one line with no quote characters.")

;;; One tab has its elements, its console values, its session and the lock
;;; that serializes its DOM writes

(defstruct (page (:constructor %make-page))
  (scrollback nil :read-only t)
  (panel nil :read-only t)
  (repl-line nil :read-only t)
  (interrupt-button nil :read-only t)
  (lock (sb-thread:make-mutex :name "page") :read-only t)
  ;; mutable
  session
  (history (g:make-history))
  (levels '()))

(defun start-web (session-kind &key (port 8417))
  "Serve the console on 127.0.0.1:PORT. Each tab opens a session of SESSION-KIND,
a keyword or kind struct with a G:OPEN-SESSION method"
  (setf clog-connection:*break-on-error* nil)
  (clog:initialize (lambda (body) (on-new-window body session-kind))
                   :host "127.0.0.1"
                   :port port))

(defun stop-web ()
  "Stop serving all the tabs"
  (clog:shutdown))

(defun on-new-window (body session-kind)
  "Build one tab's page and run it until it is closed. Then close its session"
  (let ((page (build-page body)))
    (setf (page-session page) (g:open-session session-kind page))
    (attach-page-callbacks page)
    (clog:run body)
    (g:session-close (page-session page))))

(defun build-page (body)
  "Lay out the page in BODY and return its struct.
Callbacks are attached completely separately"
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
                :repl-line repl-line :interrupt-button interrupt-button)))

(defun attach-page-callbacks (page)
  "Attach the REPL line and interrupt callbacks, and focus the REPL line"
  (attach-repl-line-callbacks page)
  (attach-interrupt-callback page)
  (sb-thread:with-mutex ((page-lock page))
    (clog:focus (page-repl-line page))))

(defun attach-repl-line-callbacks (page)
  "Install the REPL line's key script and handle the events it sends"
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
  "Interrupt the session when the interrupt button is clicked"
  (clog:set-on-click (page-interrupt-button page)
                     (lambda (button)
                       (declare (ignore button))
                       (on-interrupt-click page))))

(defun repl-line-script (id)
  "The REPL line's key handling, run in the browser on the element named ID.
Enter without Shift takes the text and clears the line in one step, then sends
the text as a repl-submit event, so a key typed before the server answers is never
mixed in. Shift+Enter inserts a newline. ArrowUp on the first line sends a repl-older
event and ArrowDown on the last a repl-newer event, each with the text the line holds.
The line grows to fit its text"
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
  "Echo and record LINE, which the browser has already taken off the REPL line.
Send it, and the send is outside the lock"
  (sb-thread:with-mutex ((page-lock page))
    (setf (page-history page) (g:fold-submitted-line (page-history page) line))
    (append-line page "echo" (format nil "* ~A" line)))
  (unless (g:blank-line-p line)
    (g:session-eval (page-session page) line)))

(defun recall (page direction typed)
  "Step the history in DIRECTION, :OLDER or :NEWER, from TYPED, the text the REPL line
held, and show the line it lands on"
  (sb-thread:with-mutex ((page-lock page))
    (multiple-value-bind (history line) (g:step-recall (page-history page) direction typed)
      (setf (page-history page) history)
      (etypecase line
        (string (show-recalled-line (page-repl-line page) line))
        ((eql :none) :none)))))

(defun show-recalled-line (repl-line line)
  "Put LINE in REPL-LINE with the cursor at its end, unless the visitor has typed
since asking for the recall. The browser makes that check, so no keystroke is overwritten"
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
  "Invoke restart INDEX of level ID and then return the focus to the REPL line"
  (g:session-invoke-restart (page-session page) id index)
  (sb-thread:with-mutex ((page-lock page))
    (clog:focus (page-repl-line page))))

(defun on-interrupt-click (page)
  "Interrupt the session and then return the focus to the REPL line"
  (g:session-interrupt (page-session page))
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
       (render-panel page)))))

(defun append-line (page class text)
  "Append TEXT to the scrollback as one line of CLASS and scroll to it.
The caller holds the page lock"
  (let ((scrollback-line (clog:create-div (page-scrollback page) :class class)))
    (setf (clog:text scrollback-line) text)
    (scroll-to-end (page-scrollback page))))

(defun scroll-to-end (element)
  "Scroll ELEMENT to its bottom without a round trip to the browser"
  (let ((id (clog:script-id element)))
    (clog:js-execute element (format nil "~A.scrollTop=~A.scrollHeight" id id))))

(defun render-panel (page)
  "Redraw the debugger panel from the level stack (newest first). The caller holds
the page lock"
  (let ((panel (page-panel page)))
    (clog:with-connection-cache (panel)
      (clog:destroy-children panel)
      (loop for debug-level in (page-levels page)
            for newest = t then nil
            do (render-level page panel debug-level newest)))))

(defun render-level (page panel debug-level newest)
  "Redraw one level: its condition, a button per restart, and the backtrace if NEWEST"
  (let ((level-box (clog:create-div panel :class "level")))
    (render-condition level-box debug-level)
    (dolist (restart (g:debug-level-restarts debug-level))
      (render-restart-button page level-box (g:debug-level-id debug-level) restart))
    (when newest
      (render-backtrace level-box debug-level))))

(defun render-condition (level-box debug-level)
  "Show DEBUG-LEVEL's depth, condition type and report in LEVEL-BOX"
  (setf (clog:text (clog:create-div level-box :class "condition"))
        (format nil "Debugger level ~D: ~A~%~A"
                (g:debug-level-depth debug-level)
                (g:debug-level-type debug-level)
                (g:debug-level-report debug-level))))

(defun render-restart-button (page level-box level-id restart)
  "A button in LEVEL-BOX that invokes RESTART, an (index name report) list, of level LEVEL-ID"
  (let ((button (clog:create-button level-box :class "restart"))
        (index (first restart)))
    (setf (clog:text button) (g:restart-label restart))
    (clog:set-on-click button
                       (lambda (button)
                         (declare (ignore button))
                         (on-restart-click page level-id index)))))

(defun render-backtrace (level-box debug-level)
  "One line in LEVEL-BOX per frame of DEBUG-LEVEL's backtrace"
  (dolist (frame-text (g:debug-level-backtrace debug-level))
    (setf (clog:text (clog:create-div level-box :class "frame")) frame-text)))
