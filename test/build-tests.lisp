;;;; test/build-tests.lisp: every source file is part of some system, and raw thread
;;;; calls stay in the platform files

(in-package #:to-restart-is-divine/tests)

(defparameter *source-directories* '("agent" "frozen" "gateway" "test")
  "Directories whose Lisp files must all belong to a to-restart-is-divine system")

(defparameter *raw-thread-calls*
  '("sb-thread:" "sb-concurrency:" "sb-ext:compare-and-swap" "sb-sys:wait-until-fd-usable"
    "uiop:wait-process")
  "Calls that start threads, share state or wait. Outside the platform files, code goes
through their wrappers, whose waits all take a time limit")

(defparameter *platform-files*
  '(("agent/sbcl.lisp" "the agent's SBCL calls, ANSI CL everywhere else in the agent")
    ("gateway/threads.lisp" "the gateway's threads, mailboxes, boxes and waits")
    ("test/agent-tests.lisp" "holds one of the agent's own locks to test an interrupt landing there")
    ("test/build-tests.lisp" "names the calls to look for"))
  "Files allowed to make raw thread calls, each with the reason")

(def-suite build :in divine-tests)
(in-suite build)

(defun leaf-components (component)
  "Files under COMPONENT, looking through modules"
  (if (typep component 'asdf:parent-component)
      (mapcan #'leaf-components (asdf:component-children component))
      (list component)))

(defun listed-source-files ()
  "Truenames of every file some to-restart-is-divine system lists"
  (loop for name in (asdf:registered-systems)
        when (uiop:string-prefix-p "to-restart-is-divine" name)
          append (mapcar (lambda (component) (truename (asdf:component-pathname component)))
                         (leaf-components (asdf:find-system name)))))

(defun source-files-on-disk ()
  "Truenames of every Lisp file in *SOURCE-DIRECTORIES*"
  (loop for directory in *source-directories*
        append (directory (merge-pathnames (make-pathname :directory (list :relative directory)
                                                          :name :wild :type "lisp")
                                           (asdf:system-source-directory "to-restart-is-divine")))))

(defun unlisted-source-files ()
  "Lisp files on disk that no system loads"
  (set-difference (source-files-on-disk) (listed-source-files) :test #'equal))

(test every-source-file-is-in-a-system
  ;; finding no files at all would pass the second check for free
  (is (plusp (length (source-files-on-disk))))
  (is (equal '() (unlisted-source-files))))

(defun platform-file-p (file)
  "True when FILE is one of *PLATFORM-FILES*"
  (member (truename file)
          (mapcar (lambda (entry)
                    (truename (asdf:system-relative-pathname "to-restart-is-divine" (first entry))))
                  *platform-files*)
          :test #'equal))

(defun raw-thread-calls-in (file)
  "The *RAW-THREAD-CALLS* FILE's text contains, as (file call) pairs"
  (let ((text (string-downcase (uiop:read-file-string file))))
    (loop for call in *raw-thread-calls*
          when (search call text)
            collect (list (enough-namestring file (asdf:system-source-directory "to-restart-is-divine"))
                          call))))

(defun misplaced-raw-thread-calls ()
  "Raw thread calls in files that aren't platform files, as (file call) pairs"
  (loop for file in (source-files-on-disk)
        unless (platform-file-p file)
          append (raw-thread-calls-in file)))

(test raw-thread-calls-stay-in-the-platform-files
  ;; a platform file that moved would let its calls pass unexamined
  (is (every (lambda (entry)
               (probe-file (asdf:system-relative-pathname "to-restart-is-divine" (first entry))))
             *platform-files*))
  (is (equal '() (misplaced-raw-thread-calls))))
