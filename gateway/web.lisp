;;;; gateway/web.lisp: the CLOG page (only file that talks to CLOG)

(defpackage #:to-restart-is-divine/web
  (:use #:cl)
  (:local-nicknames (#:g #:to-restart-is-divine/gateway)
                    (#:th #:to-restart-is-divine/threads)
                    (#:frozen #:to-restart-is-divine/frozen))
  (:export #:start-web #:stop-web))

(in-package #:to-restart-is-divine/web)

;;; One tab has one owner thread, and only the owner writes to the tab or reads its
;;; state. A CLOG callback or a session event becomes a page input in the tab's inbox,
;;; and the owner takes them one at a time with TAKE. So no lock is held across a
;;; browser write, and two writes can't interleave.
;;;
;;; The owner carries the tab's state as one frozen PAGE-STATE in its own loop variable,
;;; so nothing else can reach it. Each session reports to a fresh PAGE-FEED, and the
;;; state names the current one, so an event or a Start over press from a session the
;;; page already closed gets dropped.
;;;
;;; The owner's exits: the inbox wait wakes every minute, and closing the tab sends a
;;; TAB-CLOSED, after which the owner closes the session and stops

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

(defstruct (page (:constructor %make-page))
  "One tab's elements, the kind of session it opens, and the inbox its owner takes from"
  (scrollback nil :read-only t)
  (panel nil :read-only t)
  (repl-line nil :read-only t)
  (interrupt-button nil :read-only t)
  (session-kind nil :read-only t)
  (inbox (th:make-mailbox "page inbox") :read-only t))

(frozen:define-frozen-struct page-state-with
    (page-state (:constructor make-page-state (&key session feed history levels needs-input))
                (:copier nil))
  (session nil :read-only t)
  (feed nil :read-only t)               ; the PAGE-FEED SESSION reports to
  (history nil :read-only t)
  (levels '() :read-only t)
  (needs-input :none :read-only t))     ; :none, or the level a read waits in: its id, or :top

(defstruct (page-feed (:constructor make-page-feed (page)))
  "The receiver one session of PAGE reports to. Each session gets a fresh one"
  (page nil :read-only t))

(defmethod g:receive-event ((page-feed page-feed) (up-event g:up-event))
  "Post UP-EVENT for the page's owner, marked as coming through PAGE-FEED"
  (post (page-feed-page page-feed) (make-fed-event page-feed up-event)))

;;; What the owner takes from the inbox

(defstruct (page-input (:constructor nil)))

(defstruct (fed-event (:include page-input) (:constructor make-fed-event (feed up-event)))
  "UP-EVENT from the session that reports to FEED"
  (feed nil :read-only t)
  (up-event nil :read-only t))

(defstruct (clicked (:include page-input) (:constructor make-clicked (feed down-message)))
  "A press on a button drawn for the session reporting to FEED, which sends DOWN-MESSAGE, then
puts focus back on the REPL line"
  (feed nil :read-only t)
  (down-message nil :read-only t))

(defstruct (interrupt-request (:include page-input) (:constructor make-interrupt-request ()))
  "An Interrupt press, for whichever session is the page's now")

(defstruct (focus-request (:include page-input) (:constructor make-focus-request ()))
  "A button press that only puts focus back on the REPL line")

(defstruct (submitted-line (:include page-input) (:constructor make-submitted-line (text)))
  "TEXT the visitor entered, which the browser already took off the REPL line"
  (text nil :read-only t))

(defstruct (recall-request (:include page-input) (:constructor make-recall-request (direction typed)))
  "Step history in DIRECTION, :OLDER or :NEWER, from TYPED, what the REPL line held"
  (direction nil :read-only t)
  (typed nil :read-only t))

(defstruct (start-over-request (:include page-input) (:constructor make-start-over-request (feed)))
  "A Start over press on the button shown for the session reporting to FEED"
  (feed nil :read-only t))

(defstruct (tab-closed (:include page-input) (:constructor make-tab-closed ()))
  "The tab is gone")

(defclass unopened-session (g:session) ()
  (:documentation "Stands in for a session that failed to open. Sends go nowhere"))

(defmethod g:session-send ((unopened-session unopened-session) (down-message g:down-message))
  :dropped)

(defmethod g:session-close ((unopened-session unopened-session))
  :closed)

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
  "Build one tab's page and start its owner. Once the tab closes, tell the owner and give it
a minute to close the session. :JOINED or :TIMED-OUT"
  (let* ((page (build-page body session-kind))
         (owner (th:start-thread "page owner" (lambda () (run-page page)))))
    (clog:run body)
    (post page (make-tab-closed))
    (report-owner-join (th:join-within owner 60))))

(defun report-owner-join (outcome)
  "Log OUTCOME of joining a page's owner when it's :TIMED-OUT. OUTCOME"
  (when (eq outcome :timed-out)
    (format *error-output* "~&;; A page owner was still busy a minute after its tab closed~%"))
  outcome)

(defun post (page page-input)
  "Put PAGE-INPUT in PAGE's inbox for its owner. Safe from any thread"
  (th:mailbox-send (page-inbox page) page-input))

(defun build-page (body session-kind)
  "Lay out the page in BODY and return its struct, which opens sessions of SESSION-KIND.
The owner attaches callbacks"
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

;;; The owner

(defun run-page (page)
  "PAGE's owner: attach its callbacks, open its first session, then take inputs until its
tab closes. :TAB-CLOSED"
  (attach-page-callbacks page)
  (loop for page-state = (open-page-state page (g:make-history))
          then (take-next-input page page-state)
        until (eq page-state :tab-closed))
  :tab-closed)

(defun take-next-input (page page-state)
  "Take PAGE's next input, waiting up to a minute for one. The next state, or :TAB-CLOSED"
  (let ((page-input (th:mailbox-receive-within (page-inbox page) 60)))
    (if (eq page-input :timed-out)
        page-state
        (take-safely page page-state page-input))))

(defun take-safely (page page-state page-input)
  "TAKE PAGE-INPUT, logging an error rather than letting it stop the owner. On an error the
state stays as it was"
  (handler-case (take page page-state page-input)
    (error (condition)
      (format *error-output* "~&;; Page couldn't take ~S: ~A~%" (type-of page-input) condition)
      page-state)))

(defun open-page-state (page history)
  "Fresh state for PAGE keeping HISTORY, with a new session of PAGE's kind reporting to a new
feed. A session that fails to open leaves an UNOPENED-SESSION and a Start over button"
  (let* ((page-feed (make-page-feed page))
         (page-state (make-page-state :session (make-instance 'unopened-session)
                                      :feed page-feed :history history
                                      :levels '() :needs-input :none)))
    (handler-case (page-state-with page-state
                                   :session (g:open-session (page-session-kind page) page-feed))
      (error (condition)
        (append-line page "note" (format nil ";; The Lisp behind this page didn't start: ~A" condition))
        (append-start-over page page-state "Start over to try again")
        page-state))))

(defgeneric take (page page-state page-input)
  (:documentation "Act on PAGE-INPUT for PAGE, whose state is PAGE-STATE. Runs only on PAGE's
owner thread. The next state, or :TAB-CLOSED"))

(defmethod take ((page page) page-state (fed-event fed-event))
  (if (eq (fed-event-feed fed-event) (page-state-feed page-state))
      (show-event page page-state (fed-event-up-event fed-event))
      page-state))

(defmethod take ((page page) page-state (clicked clicked))
  (when (eq (clicked-feed clicked) (page-state-feed page-state))
    (g:session-send (page-state-session page-state) (clicked-down-message clicked)))
  (clog:focus (page-repl-line page))
  page-state)

(defmethod take ((page page) page-state (interrupt-request interrupt-request))
  (declare (ignore interrupt-request))
  (g:session-send (page-state-session page-state) (g:make-interrupt-message))
  (clog:focus (page-repl-line page))
  page-state)

(defmethod take ((page page) page-state (focus-request focus-request))
  (declare (ignore focus-request))
  (clog:focus (page-repl-line page))
  page-state)

(defmethod take ((page page) page-state (submitted-line submitted-line))
  (let ((text (submitted-line-text submitted-line))
        (route (line-route page-state)))
    (append-line page "echo" (format nil (ecase route (:input "> ~A") (:eval "* ~A")) text))
    (note-unsent-line page (send-submitted-line page-state text route))
    (page-state-with page-state
                     :history (g:fold-submitted-line (page-state-history page-state) text))))

(defmethod take ((page page) page-state (recall-request recall-request))
  (multiple-value-bind (history line)
      (g:step-recall (page-state-history page-state)
                     (recall-request-direction recall-request)
                     (recall-request-typed recall-request))
    (etypecase line
      (string (show-recalled-line (page-repl-line page) line))
      ((eql :none) :none))
    (page-state-with page-state :history history)))

(defmethod take ((page page) page-state (start-over-request start-over-request))
  (if (eq (start-over-request-feed start-over-request) (page-state-feed page-state))
      (start-over page page-state)
      page-state))

(defmethod take ((page page) page-state (tab-closed tab-closed))
  (declare (ignore tab-closed))
  (handler-case (g:session-close (page-state-session page-state))
    (error (condition)
      (format *error-output* "~&;; Closing a closed tab's session failed: ~A~%" condition)))
  :tab-closed)

;;; Callbacks: each reads what the browser sent and posts it for the owner

(defun attach-page-callbacks (page)
  "Hook up REPL line and Interrupt callbacks, then focus the REPL line"
  (attach-repl-line-callbacks page)
  (attach-interrupt-callback page)
  (clog:focus (page-repl-line page)))

(defun attach-repl-line-callbacks (page)
  "Install REPL line's key script and post the events it sends"
  (let ((repl-line (page-repl-line page)))
    (clog:js-execute repl-line (repl-line-script (clog:script-id repl-line)))
    (clog:set-on-event-with-data repl-line "repl-submit"
                                 (lambda (repl-line text)
                                   (declare (ignore repl-line))
                                   (post page (make-submitted-line text))))
    (clog:set-on-event-with-data repl-line "repl-older"
                                 (lambda (repl-line typed)
                                   (declare (ignore repl-line))
                                   (post page (make-recall-request :older typed))))
    (clog:set-on-event-with-data repl-line "repl-newer"
                                 (lambda (repl-line typed)
                                   (declare (ignore repl-line))
                                   (post page (make-recall-request :newer typed))))))

(defun attach-interrupt-callback (page)
  "Interrupt the session when someone clicks Interrupt"
  (clog:set-on-click (page-interrupt-button page)
                     (lambda (button)
                       (declare (ignore button))
                       (post page (make-interrupt-request)))))

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

(defun post-click (page-feed page-input)
  "Post PAGE-INPUT, made by a button drawn for the session reporting to PAGE-FEED"
  (post (page-feed-page page-feed) page-input))

(defun set-click-input (page-feed frame-ref name text)
  "What a Set press posts: setting local NAME of the frame FRAME-REF names to TEXT, or only
a focus when TEXT is blank. PAGE-FEED is the feed the button was drawn for"
  (if (g:blank-line-p text)
      (make-focus-request)
      (make-clicked page-feed
                    (g:make-eval-in-frame-message frame-ref (g:local-assignment name text)))))

(defun return-click-input (page-feed frame-ref text)
  "What a Return press posts: returning TEXT's values from the frame FRAME-REF names. Blank
returns NIL. PAGE-FEED is the feed the button was drawn for"
  (make-clicked page-feed
                (g:make-return-from-frame-message frame-ref (if (g:blank-line-p text) "nil" text))))

;;; Lines from the REPL line

(defun note-unsent-line (page outcome)
  "Tell the page when a line was too long to send. OUTCOME is what the send returned"
  (when (eq outcome :too-long)
    (append-line page "note" ";; That line is too long to send.")))

(defun line-route (page-state)
  "Where Enter sends the line. :INPUT when the agent's waiting on a read and the level
it waits in is still the newest, so a level opened on top of a waiting read gets Enter
for itself. Otherwise :EVAL"
  (if (eql (page-state-needs-input page-state) (newest-level-id page-state))
      :input
      :eval))

(defun newest-level-id (page-state)
  "Id of the newest open level in PAGE-STATE, or :TOP"
  (if (page-state-levels page-state)
      (g:debug-level-id (first (page-state-levels page-state)))
      :top))

(defun send-submitted-line (page-state line route)
  "Send LINE to PAGE-STATE's session the way ROUTE says. A blank :eval line is :BLANK and
doesn't get sent"
  (let ((session (page-state-session page-state)))
    (ecase route
      (:input (g:session-send session (g:make-input-message line)))
      (:eval (if (g:blank-line-p line)
                 :blank
                 (g:session-send session (g:make-eval-message line)))))))

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

;;; Session events

(defgeneric show-event (page page-state up-event)
  (:documentation "Show UP-EVENT on PAGE, whose state is PAGE-STATE. Owner thread only.
The next state"))

(defmethod show-event ((page page) page-state (ready-event g:ready-event))
  (declare (ignore ready-event))
  (append-line page "note" ";; Ready.")
  page-state)

(defmethod show-event ((page page) page-state (output-event g:output-event))
  (append-line page "output" (g:output-event-text output-event))
  page-state)

(defmethod show-event ((page page) page-state (values-event g:values-event))
  (dolist (line (g:values-lines (g:values-event-printed values-event)))
    (append-line page "value" line))
  page-state)

(defmethod show-event ((page page) page-state (level-opened-event g:level-opened-event))
  (show-levels page page-state
               (g:push-level (page-state-levels page-state)
                             (g:level-opened-event-debug-level level-opened-event))))

(defmethod show-event ((page page) page-state (level-closed-event g:level-closed-event))
  (show-levels page page-state
               (g:drop-level (page-state-levels page-state)
                             (g:level-closed-event-level-id level-closed-event))))

(defmethod show-event ((page page) page-state (locals-event g:locals-event))
  (show-levels page page-state
               (g:fold-locals (page-state-levels page-state)
                              (g:locals-event-frame-ref locals-event)
                              (g:locals-event-pairs locals-event))))

(defmethod show-event ((page page) page-state (needs-input-event g:needs-input-event))
  (declare (ignore needs-input-event))
  (note-needs-input page page-state))

(defmethod show-event ((page page) page-state (input-done-event g:input-done-event))
  (declare (ignore input-done-event))
  (page-state-with page-state :needs-input :none))

(defmethod show-event ((page page) page-state (interrupt-waiting-event g:interrupt-waiting-event))
  (declare (ignore interrupt-waiting-event))
  (append-line page "note"
               ";; Interrupt is waiting: the code is in foreign (C) code. It lands once that returns")
  (append-start-over page page-state
                     "If it never returns, start over. That throws away everything this session defined")
  page-state)

(defmethod show-event ((page page) page-state (interrupt-missed-event g:interrupt-missed-event))
  (declare (ignore interrupt-missed-event))
  (append-line page "note" ";; Interrupt didn't land: the evaluation finished first")
  page-state)

(defmethod show-event ((page page) page-state (agent-gone-event g:agent-gone-event))
  (declare (ignore agent-gone-event))
  (append-line page "note" ";; The Lisp behind this page died")
  (append-start-over page page-state
                     "Start over to get a fresh one. Nothing this session defined comes back")
  page-state)

(defun show-levels (page page-state levels)
  "Redraw PAGE's panel from LEVELS. PAGE-STATE with LEVELS in it"
  (render-panel page (page-state-feed page-state) levels)
  (page-state-with page-state :levels levels))

(defun note-needs-input (page page-state)
  "PAGE-STATE waiting for a line in its newest level, with a note in the scrollback the
first time"
  (when (eq (page-state-needs-input page-state) :none)
    (append-line page "note" ";; Waiting for a line of input."))
  (page-state-with page-state :needs-input (newest-level-id page-state)))

;;; Start over

(defun append-start-over (page page-state text)
  "Tack TEXT and a Start over button onto the scrollback. The button starts over only while
PAGE-STATE's session is still the page's"
  (let ((scrollback-line (clog:create-div (page-scrollback page) :class "note"))
        (page-feed (page-state-feed page-state)))
    (setf (clog:text (clog:create-span scrollback-line :class "call")) (format nil ";; ~A" text))
    (clog:set-on-click (clog:create-button scrollback-line :content "Start over" :class "frame-action")
                       (lambda (button)
                         (declare (ignore button))
                         (post page (make-start-over-request page-feed))))
    (scroll-to-end (page-scrollback page))))

(defun start-over (page page-state)
  "Close PAGE-STATE's session, which kills its process stuck or not, clear the page and open
a fresh session of the same kind. The new state, keeping the history"
  (append-line page "note" ";; Starting over...")
  (g:session-close (page-state-session page-state))
  (clear-page page)
  (open-page-state page (page-state-history page-state)))

(defun clear-page (page)
  "Empty PAGE's scrollback and panel and put focus on the REPL line"
  (clog:destroy-children (page-scrollback page))
  (clog:destroy-children (page-panel page))
  (clog:focus (page-repl-line page)))

;;; Drawing

(defun append-line (page class text)
  "Tack TEXT onto the scrollback as one line of CLASS and scroll down to it"
  (let ((scrollback-line (clog:create-div (page-scrollback page) :class class)))
    (setf (clog:text scrollback-line) text)
    (scroll-to-end (page-scrollback page))))

(defun scroll-to-end (element)
  "Scroll ELEMENT to the bottom, no round trip to the browser"
  (let ((id (clog:script-id element)))
    (clog:js-execute element (format nil "~A.scrollTop=~A.scrollHeight" id id))))

(defun render-panel (page page-feed levels)
  "Redraw PAGE's debugger panel from LEVELS, newest first, with buttons that press for the
session reporting to PAGE-FEED"
  (let ((panel (page-panel page)))
    (clog:with-connection-cache (panel)
      (clog:destroy-children panel)
      (loop for debug-level in levels
            for newest = t then nil
            do (render-level page-feed panel debug-level newest)))))

(defun render-level (page-feed panel debug-level newest)
  "Draw one level: its condition, a button per restart, and its frames if it's NEWEST"
  (let ((level-box (clog:create-div panel :class "level")))
    (render-condition level-box debug-level)
    (dolist (restart (g:debug-level-restarts debug-level))
      (render-restart-button page-feed level-box (g:debug-level-id debug-level) restart))
    (when newest
      (render-frames page-feed level-box debug-level))))

(defun render-condition (level-box debug-level)
  "Show DEBUG-LEVEL's depth, condition type and report in LEVEL-BOX"
  (setf (clog:text (clog:create-div level-box :class "condition"))
        (format nil "Debugger level ~D: ~A~%~A"
                (g:debug-level-depth debug-level)
                (g:debug-level-type debug-level)
                (g:debug-level-report debug-level))))

(defun render-restart-button (page-feed level-box level-id restart)
  "Button in LEVEL-BOX that invokes RESTART (an (index name report) list) of level LEVEL-ID"
  (let ((button (clog:create-button level-box :class "restart"))
        (index (first restart)))
    (setf (clog:text button) (g:restart-label restart))
    (clog:set-on-click button
                       (lambda (button)
                         (declare (ignore button))
                         (post-click page-feed
                                     (make-clicked page-feed
                                                   (g:make-invoke-restart-message level-id index)))))))

(defun render-frames (page-feed level-box debug-level)
  "A row in LEVEL-BOX for each frame in DEBUG-LEVEL's backtrace, or a note if the agent
couldn't read the stack"
  (ecase (g:debug-level-frames debug-level)
    (:read (loop for frame-text in (g:debug-level-backtrace debug-level)
                 for frame-index from 0
                 do (render-frame page-feed level-box debug-level frame-index frame-text)))
    (:lost (setf (clog:text (clog:create-div level-box :class "frame unreadable"))
                 "The backtrace couldn't be read here. The restarts still work."))))

(defun render-frame (page-feed level-box debug-level frame-index frame-text)
  "Draw one frame row: its call, Locals, Return and Rerun if they're allowed, and its
locals if we've fetched them. A frame whose args and locals can't be read gets only its
text, dimmed"
  (let* ((frame-ref (g:make-frame-ref (g:debug-level-id debug-level) frame-index))
         (readable (g:frame-readable-in-p debug-level frame-index))
         (row (clog:create-div level-box :class (if readable "frame" "frame unreadable"))))
    (setf (clog:text (clog:create-span row :class "call")) frame-text)
    (when readable
      (render-locals-button page-feed row frame-ref))
    (when (g:frame-unwindable-in-p debug-level frame-index)
      (render-unwind-controls page-feed row frame-ref))
    (render-locals page-feed row frame-ref
                   (rest (assoc frame-index (g:debug-level-locals debug-level))))))

(defun render-locals-button (page-feed row frame-ref)
  "Locals button in ROW that asks for the locals of the frame FRAME-REF names"
  (let ((button (clog:create-button row :class "frame-action")))
    (setf (clog:text button) "Locals")
    (clog:set-on-click button
                       (lambda (button)
                         (declare (ignore button))
                         (post-click page-feed
                                     (make-clicked page-feed (g:make-locals-message frame-ref)))))))

(defun render-unwind-controls (page-feed row frame-ref)
  "Value box, a Return button that returns the box's text from the frame FRAME-REF names, and a
Rerun button"
  (let ((value-box (clog:create-form-element row :text :class "frame-value"))
        (return-button (clog:create-button row :class "frame-action"))
        (rerun-button (clog:create-button row :class "frame-action")))
    (setf (clog:text return-button) "Return"
          (clog:text rerun-button) "Rerun")
    (clog:set-on-click return-button
                       (lambda (button)
                         (declare (ignore button))
                         (post-click page-feed
                                     (return-click-input page-feed frame-ref (clog:value value-box)))))
    (clog:set-on-click rerun-button
                       (lambda (button)
                         (declare (ignore button))
                         (post-click page-feed
                                     (make-clicked page-feed (g:make-rerun-frame-message frame-ref)))))))

(defun render-locals (page-feed row frame-ref pairs)
  "Draw each of PAIRS (the frame's fetched locals) as NAME = value, with a Set box"
  (dolist (pair pairs)
    (destructuring-bind (name printed) pair
      (render-local page-feed row frame-ref name printed))))

(defun render-local (page-feed row frame-ref name printed)
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
                         (post-click page-feed
                                     (set-click-input page-feed frame-ref name (clog:value value-box)))))))
