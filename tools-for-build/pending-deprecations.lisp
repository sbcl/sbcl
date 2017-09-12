;;;; Determine how long things been in their current deprecation
;;;; states and whether it is time to move them to the respective
;;;; successor states.
;;;;
;;;; Use like this
;;;;
;;;;   ./run-sbcl.sh --noinform --no-userinit               \
;;;;     --script tools-for-build/pending-deprecations.lisp \
;;;;     [-- -a]
;;;;
;;;; When invoked without additional commandline arguments, the output
;;;; only contains actionable things, i.e. deprecations that should be
;;;; moved to the respective successor states. The optional -a, --all
;;;; commandline option causes information to be printed for all
;;;; deprecated things, not just actionable ones.
;;;;
;;;; The output format for individual items is as follows:
;;;;
;;;;   Namespace  Name of deprecated thing   Version/time of deprecation    successor state
;;;;   v          v                          v                              v
;;;; - FUNCTION SB-THREAD::SPINLOCK-NAME     1.0.53.11/2011-11-06 EARLY  -> LATE   2012-02-04
;;;;   SYS:SRC;CODE;THREAD.LISP                                   ^                ^
;;;;   ^                                                          current state    when to move
;;;;   Source file


;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(cl:require 'sb-introspect)

(cl:in-package #:cl-user)

;;; Git utilities

(defun split (separator string)
  (loop :for char :across string
     :with component = (make-string-output-stream)
     :if (char= char separator)
     :collect (prog1
                  (get-output-stream-string component)
                (setf component (make-string-output-stream)))
     :into components
     :else :do (write-char char component)
     :finally (return (append
                       components
                       (list (get-output-stream-string component))))))

(defun version-tag (version)
  ;; Try to handle old and new tagging schemes.
  (let* ((without-fourth (if (= 3 (count #\. version))
                             (subseq version 0 (position #\. version
                                                         :from-end t))
                             version)))
    (destructuring-bind (a b c)
        (mapcar #'parse-integer (split #\. without-fourth))
      (if (or (= a 0)
              (and (= a 1) (= b 0) (< c 50)))
          (format nil "sbcl_~A_~A_~A" a b c)
          (format nil "sbcl-~A" without-fourth)))))

(defun tag-time (tag)
  (let* ((range     (format nil "~A^..~:*~A" tag))
         (directory (make-pathname
                     :directory (append
                                 (pathname-directory *load-pathname*)
                                 '(:up))))
         (process   (run-program "git" `("--no-pager" "log" "--pretty=format:%at" ,range)
                                 :search    t
                                 :directory directory
                                 :output    :stream
                                 :error     :stream)))
    (unless (zerop (process-exit-code process))
      (warn "~@<Failed to run git log for range ~S:~@:_~A.~@:>"
            range (read-line (process-error process))))
    (parse-integer (read-line (process-output process)))))

(defun seconds-since-release (version)
  (let* ((tag      (version-tag version))
         (tag-time (tag-time tag))
         (now      (nth-value 1 (sb-unix:unix-gettimeofday))))
    (- now tag-time)))

(defun seconds-since-earliest-tagged-release ()
  (seconds-since-release ""))

;;; Time utilities

(defun print-universal-time (stream time &optional colonp atp)
  (declare (ignore colonp atp))
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time)
    (declare (ignore second minute hour))
    (format stream "~4,'0D-~2,'0D-~2,'0D" year month date)))

;;; Deprecation utilities

(defun next-state (state)
  (ecase state
    (:early :late)
    (:late  :final)
    (:final :remove)))

(defparameter *seconds-in-deprecation-state*
  `((:early . ,(* 90 24 3600))
    (:late  . ,(* 90 24 3600))
    (:final . ,(* 90 24 3600))))

(defun seconds-to-state (goal)
  (loop :for (state . time) :in *seconds-in-deprecation-state*
     :until (eq state goal)
     :sum time))

(defun process-deprecated-thing (namespace name state since
                                 &key force)
  (let* ((version      (second since))
         (now          (get-universal-time))
         (age          (ignore-errors (seconds-since-release version)))
         (next         (next-state state))
         (time-to-next (seconds-to-state next))
         (next-time    (when age (+ (- now age) time-to-next))))
    (when (or force (and next-time (>= now next-time)))
      (let ((source (sb-introspect:find-definition-sources-by-name
                     name (ecase namespace
                            (function :function)
                            (variable :variable)
                            (type     :type)))))
        (format t "- ~8A ~/sb-ext:print-symbol-with-prefix/~56T~
                     ~9@A/~:[??????????~:;~:*~/cl-user::print-universal-time/~] ~
                     ~6A ~
                     -> ~6A ~:[??????????~:;~:*~/cl-user::print-universal-time/~]~%~
                   ~2@T~{~A~^, ~}~2%"
                namespace name
                version (when age (- now age))
                state
                next (when next-time next-time)
                (mapcar #'sb-introspect:definition-source-pathname source))))))

(defun print-deprecation-info (&key (only-actionable t))
  (do-all-symbols (name1)
    (loop :for (namespace . name) :in (list (cons 'function name1)
                                            (cons 'function `(setf ,name1))
                                            (cons 'variable name1)
                                            (cons 'type     name1)) :do
       (multiple-value-bind (state since)
           (sb-int:deprecated-thing-p namespace name)
         (when state
           (process-deprecated-thing namespace name state since
                                     :force (not only-actionable)))))))

(flet ((optionp (option)
         (find option *posix-argv* :test 'string=)))
  (let ((allp (or (optionp "-a") (optionp "--all"))))
    (print-deprecation-info :only-actionable allp)))
