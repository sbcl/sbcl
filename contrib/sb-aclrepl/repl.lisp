;;;; Replicate much of the ACL toplevel functionality in SBCL. Mostly
;;;; this is portable code, but fundamentally it all hangs from a few
;;;; SBCL-specific hooks like SB-INT:*REPL-READ-FUN* and
;;;; SB-INT:*REPL-PROMPT-FUN*.
;;;;
;;;; The documentation, which may or may not apply in its entirety at
;;;; any given time, for this functionality is on the ACL website:
;;;;   <http://www.franz.com/support/documentation/6.2/doc/top-level.htm>.

(cl:in-package :sb-aclrepl)

(defstruct user-cmd
  (input nil) ; input, maybe a string or form
  (func nil)  ; cmd func entered, overloaded
              ; (:eof :null-cmd :cmd-error :history-error)
  (args nil)  ; args for cmd func
  (hnum nil)) ; history number


;;; cmd table entry
(defstruct cmd-table-entry
  (name nil) ; name of command
  (func nil) ; function handler
  (desc nil) ; short description
  (parsing nil) ; (:string :case-sensitive nil)
  (group nil) ; command group (:cmd or :alias)
  (abbr-len 0)) ; abbreviation length

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *default-prompt*
    "~:[~3*~;[~:*~D~:[~;~:*:~D~]~:[~;i~]~:[~;c~]] ~]~A(~D): "
    "The default prompt."))
(defparameter *prompt* #.*default-prompt*
  "The current prompt string or formatter function.")
(defparameter *use-short-package-name* t
  "when T, use the shortnest package nickname in a prompt")
(defparameter *dir-stack* nil
  "The top-level directory stack")
(defparameter *command-char* #\:
  "Prefix character for a top-level command")
(defvar *max-history* 100
  "Maximum number of history commands to remember")
(defvar *exit-on-eof* t
  "If T, then exit when the EOF character is entered.")
(defparameter *history* nil
  "History list")
(defparameter *cmd-number* 1
  "Number of the next command")

(defvar *input*)
(defvar *output*)

(declaim (type list *history*))

(defvar *eof-marker* :eof)
(defvar *eof-cmd* (make-user-cmd :func :eof))
(defvar *null-cmd* (make-user-cmd :func :null-cmd))

(defparameter *cmd-table-hash*
  (make-hash-table :size 30 :test #'equal))

(defun prompt-package-name ()
  (if *use-short-package-name*
      (car (sort (append
                  (package-nicknames cl:*package*)
                  (list (package-name cl:*package*)))
                 (lambda (a b) (< (length a) (length b)))))
      (package-name cl:*package*)))

(defun read-cmd (input-stream)
  ;; Reads a command from the user and returns a user-cmd object
  (let* ((next-char (peek-char-non-whitespace input-stream))
         (cmd (cond
                ((eql *command-char* next-char)
                 (dispatch-command-line input-stream))
                ((eql #\newline next-char)
                 (read-char input-stream)
                 *null-cmd*)
                ((eql :eof next-char)
                 *eof-cmd*)
                (t
                 (let* ((eof (cons nil *eof-marker*))
                        (form (read input-stream nil eof)))
                   (if (eq form eof)
                       *eof-cmd*
                       (make-user-cmd :input form :func nil :hnum *cmd-number*)))))))
    (if (and (eq cmd *eof-cmd*) (typep input-stream 'string-stream))
        (throw 'repl-catcher cmd)
        cmd)))

(defun dispatch-command-line (input-stream)
  "Processes an input line that starts with *command-char*"
  (let* ((line (string-trim-whitespace (read-line input-stream)))
         (first-space-pos (position #\space line))
         (cmd-string (subseq line 1 first-space-pos))
         (cmd-args-string
          (if first-space-pos
              (string-trim-whitespace (subseq line first-space-pos))
              "")))
    (declare (simple-string line))
    (cond
      ((or (zerop (length cmd-string))
           (whitespace-char-p (char cmd-string 0)))
       *null-cmd*)
      ((or (numberp (read-from-string cmd-string))
           (char= (char cmd-string 0) #\+)
           (char= (char cmd-string 0) #\-))
       (process-cmd-numeric cmd-string cmd-args-string))
      ((char= (char cmd-string 0) *command-char*)
       (process-history-search (subseq cmd-string 1) cmd-args-string))
      (t
       (process-cmd-text cmd-string line cmd-args-string)))))

(defun process-cmd-numeric (cmd-string cmd-args-string)
  "Process a numeric cmd, such as ':123'"
  (let* ((first-char (char cmd-string 0))
         (number-string (if (digit-char-p first-char)
                            cmd-string
                            (subseq cmd-string 1)))
         (is-minus (char= first-char #\-))
         (raw-number (read-from-string number-string))
         (number (if is-minus
                     (- *cmd-number* raw-number)
                     raw-number))
         (cmd (get-history number)))
    (when (eq cmd *null-cmd*)
      (return-from process-cmd-numeric
        (make-user-cmd :func :history-error :input (read-from-string
                                                    cmd-string))))
    (maybe-return-history-cmd cmd cmd-args-string)))

(defun maybe-return-history-cmd (cmd cmd-args-string)
  (format *output* "~A~%" (user-cmd-input cmd))
  (let ((dont-redo
         (when (and (stringp cmd-args-string)
                    (plusp (length cmd-args-string))
                    (char= #\? (char cmd-args-string 0)))
           (do ((line nil (read-line *input*)))
               ((and line (or (zerop (length line))
                              (string-equal line "Y")
                              (string-equal line "N")))
                (when (string-equal line "N")
                  t))
             (when line
               (format *output* "Type \"y\" for yes or \"n\" for no.~%"))
             (format *output* "redo? [y] ")
             (force-output *output*)))))
    (if dont-redo
        *null-cmd*
        (make-user-cmd :func (user-cmd-func cmd)
                       :input (user-cmd-input cmd)
                       :args (user-cmd-args cmd)
                       :hnum *cmd-number*))))


(defun find-history-matching-pattern (cmd-string)
  "Return history item matching cmd-string or NIL if not found"
  (dolist (his *history* nil)
    (let* ((input (user-cmd-input his))
           (string-input (if (stringp input)
                             input
                             (write-to-string input))))
      (when (search cmd-string string-input :test #'string-equal)
        (return-from find-history-matching-pattern his)))))

(defun process-history-search (pattern cmd-args-string)
  (let ((cmd (find-history-matching-pattern pattern)))
    (unless cmd
      (format *output* "No match on history list with pattern ~S~%" pattern)
      (return-from process-history-search *null-cmd*))
    (maybe-return-history-cmd cmd cmd-args-string)))


(defun process-cmd-text (cmd-string line cmd-args-string)
  "Process a text cmd, such as ':ld a b c'"
  (flet ((parse-args (parsing args-string)
           (case parsing
             (:string
              (if (zerop (length args-string))
                  nil
                  (list args-string)))
             (t
              (let ((string-stream (make-string-input-stream args-string))
                    (eof (cons nil *eof-marker*))) ;new cons for eq uniqueness
                (loop as arg = (read string-stream nil eof)
                      until (eq arg eof)
                      collect arg))))))
    (let ((cmd-entry (find-cmd cmd-string)))
      (unless cmd-entry
        (return-from process-cmd-text
          (make-user-cmd :func :cmd-error :input cmd-string)))
      (make-user-cmd :func (cmd-table-entry-func cmd-entry)
                     :input line
                     :args (parse-args (cmd-table-entry-parsing cmd-entry)
                                       cmd-args-string)
                     :hnum *cmd-number*))))

(defun make-cte (name-param func desc parsing group abbr-len)
  (let ((name (etypecase name-param
                (string
                 name-param)
                (symbol
                 (string-downcase (write-to-string name-param))))))
    (make-cmd-table-entry :name name :func func :desc desc
                          :parsing parsing :group group
                          :abbr-len (if abbr-len
                                        abbr-len
                                        (length name)))))

(defun %add-entry (cmd &optional abbr-len)
  (let* ((name (cmd-table-entry-name cmd))
         (alen (if abbr-len
                   abbr-len
                   (length name))))
    (dotimes (i (length name))
      (when (>= i (1- alen))
        (setf (gethash (subseq name 0 (1+ i)) *cmd-table-hash*)
              cmd)))))

(defun add-cmd-table-entry (cmd-string abbr-len func-name desc parsing)
  (%add-entry
   (make-cte cmd-string (symbol-function func-name) desc parsing :cmd abbr-len)
   abbr-len))

(defun find-cmd (cmdstr)
  (gethash (string-downcase cmdstr) *cmd-table-hash*))

(defun user-cmd= (c1 c2)
  "Returns T if two user commands are equal"
  (and (eq (user-cmd-func c1) (user-cmd-func c2))
       (equal (user-cmd-args c1) (user-cmd-args c2))
       (equal (user-cmd-input c1) (user-cmd-input c2))))

(defun add-to-history (cmd)
  (unless (and *history* (user-cmd= cmd (car *history*)))
    (when (>= (length *history*) *max-history*)
      (setq *history* (nbutlast *history*
                                (1+ (- (length *history*) *max-history*)))))
    (push cmd *history*)
    (incf *cmd-number*)))

(defun get-history (n)
  (let ((cmd (find n *history* :key #'user-cmd-hnum :test #'eql)))
    (if cmd
        cmd
        *null-cmd*)))

(defun get-cmd-doc-list (&optional (group :cmd))
  "Return list of all commands"
  (let ((cmds '()))
    (maphash (lambda (k v)
               (when (and
                      (= (length k) (length (cmd-table-entry-name v)))
                      (eq (cmd-table-entry-group v) group))
                 (push (list k
                             (if (= (cmd-table-entry-abbr-len v)
                                    (length k))
                                  ""
                                  (subseq k 0 (cmd-table-entry-abbr-len v)))
                             (cmd-table-entry-desc v)) cmds)))
             *cmd-table-hash*)
    (sort cmds #'string-lessp :key #'car)))

(defun cd-cmd (&optional string-dir)
  (cond
    ((or (zerop (length string-dir))
         (string= string-dir "~"))
     (setf cl:*default-pathname-defaults* (user-homedir-pathname)))
    (t
     (let ((new (truename string-dir)))
       (when (pathnamep new)
         (setf cl:*default-pathname-defaults* new)))))
  (format *output* "~A~%" (namestring cl:*default-pathname-defaults*))
  (values))

(defun pwd-cmd ()
  (format *output* "Lisp's current working directory is ~s.~%"
          (namestring cl:*default-pathname-defaults*))
  (values))

(defun trace-cmd (&rest args)
  (if args
      (format *output* "~A~%" (eval (sb-debug::expand-trace args)))
      (format *output* "~A~%" (sb-debug::%list-traced-funs)))
  (values))

(defun untrace-cmd (&rest args)
  (if args
      (format *output* "~A~%"
              (eval
               (sb-int:collect ((res))
                (let ((current args))
                  (loop
                   (unless current (return))
                   (let ((name (pop current)))
                     (res (if (eq name :function)
                              `(sb-debug::untrace-1 ,(pop current))
                              `(sb-debug::untrace-1 ',name))))))
                `(progn ,@(res) t))))
      (format *output* "~A~%" (eval (sb-debug::untrace-all))))
  (values))

#+sb-thread
(defun all-threads ()
  "Return a list of all threads"
  (sb-thread:list-all-threads))

#+sb-thread
(defun other-threads ()
  "Returns a list of all threads except the current one"
  (delete sb-thread:*current-thread* (all-threads)))

(defun exit-cmd (&optional (status 0))
  #+sb-thread
  (let ((other-threads (other-threads)))
    (when other-threads
      (format *output* "There exists the following processes~%")
      (format *output* "~{~A~%~}" other-threads)
      (format *output* "Do you want to exit lisp anyway [n]? ")
      (force-output *output*)
      (let ((input (string-trim-whitespace (read-line *input*))))
        (if (and (plusp (length input))
                 (or (char= #\y (char input 0))
                     (char= #\Y (char input 0))))
            ;; loop in case more threads get created while trying to exit
            (do ((threads other-threads (other-threads)))
                ((eq nil threads))
              (map nil #'sb-thread:terminate-thread threads)
              (sleep 0.2))
            (return-from exit-cmd)))))
  (sb-ext:exit :code status)
  (values))

(defun package-cmd (&optional pkg)
  (cond
    ((null pkg)
     (format *output* "The ~A package is current.~%"
             (package-name cl:*package*)))
    ((null (find-package (write-to-string pkg)))
     (format *output* "Unknown package: ~A.~%" pkg))
    (t
     (setf cl:*package* (find-package (write-to-string pkg)))))
  (values))

(defun string-to-list-skip-spaces (str)
  "Return a list of strings, delimited by spaces, skipping spaces."
  (declare (type (or null string) str))
  (when str
    (loop for i = 0 then (1+ j)
          as j = (position #\space str :start i)
          when (not (char= (char str i) #\space))
          collect (subseq str i j) while j)))

(let ((last-files-loaded nil))
  (defun ld-cmd (&optional string-files)
    (if string-files
        (setq last-files-loaded string-files)
        (setq string-files last-files-loaded))
    (dolist (arg (string-to-list-skip-spaces string-files))
      (let ((file
             (if (string= arg "~/" :end1 1 :end2 1)
                 (merge-pathnames (parse-namestring
                                   (string-left-trim "~/" arg))
                                  (user-homedir-pathname))
                 arg)))
        (format *output* "loading ~S~%" file)
        (load file))))
  (values))

(defun cf-cmd (string-files)
  (when string-files
    (dolist (arg (string-to-list-skip-spaces string-files))
      (compile-file arg)))
  (values))

(defun >-num (x y)
  "Return if x and y are numbers, and x > y"
  (and (numberp x) (numberp y) (> x y)))

(defun newer-file-p (file1 file2)
  "Is file1 newer (written later than) file2?"
  (>-num (if (probe-file file1) (file-write-date file1))
         (if (probe-file file2) (file-write-date file2))))

(defun compile-file-as-needed (src-path)
  "Compiles a file if needed, returns path."
  (let ((dest-path (compile-file-pathname src-path)))
    (when (or (not (probe-file dest-path))
              (newer-file-p src-path dest-path))
      (ensure-directories-exist dest-path)
      (compile-file src-path :output-file dest-path))
    dest-path))

;;;; implementation of commands

(defun apropos-cmd (string)
  (apropos (string-upcase string))
  (fresh-line *output*)
  (values))

(let ((last-files-loaded nil))
  (defun cload-cmd (&optional string-files)
    (if string-files
        (setq last-files-loaded string-files)
        (setq string-files last-files-loaded))
    (dolist (arg (string-to-list-skip-spaces string-files))
      (format *output* "loading ~a~%" arg)
      (load (compile-file-as-needed arg)))
    (values)))

(defun inspect-cmd (arg)
  (inspector-fun (eval arg) nil *output*)
  (values))

(defun istep-cmd (&optional arg-string)
  (istep (string-to-list-skip-spaces arg-string) *output*)
  (values))

(defun describe-cmd (&rest args)
  (dolist (arg args)
    (eval `(describe ,arg)))
  (values))

(defun macroexpand-cmd (arg)
  (pprint (macroexpand arg) *output*)
  (values))

(defun history-cmd ()
  (let ((n (length *history*)))
    (declare (fixnum n))
    (dotimes (i n)
      (declare (fixnum i))
      (let ((hist (nth (- n i 1) *history*)))
        (format *output* "~3A " (user-cmd-hnum hist))
        (if (stringp (user-cmd-input hist))
            (format *output* "~A~%" (user-cmd-input hist))
            (format *output* "~W~%" (user-cmd-input hist))))))
  (values))

(defun help-cmd (&optional cmd)
  (cond
    (cmd
     (let ((cmd-entry (find-cmd cmd)))
       (if cmd-entry
           (format *output* "Documentation for ~A: ~A~%"
                   (cmd-table-entry-name cmd-entry)
                   (cmd-table-entry-desc cmd-entry)))))
    (t
     (format *output* "~11A ~4A ~A~%" "COMMAND" "ABBR" "DESCRIPTION")
     (format *output* "~11A ~4A ~A~%" "<n>" ""
             "re-execute <n>th history command")
     (dolist (doc-entry (get-cmd-doc-list :cmd))
       (format *output* "~11A ~4A ~A~%" (first doc-entry)
               (second doc-entry) (third doc-entry)))))
  (values))

(defun alias-cmd ()
  (let ((doc-entries (get-cmd-doc-list :alias)))
    (typecase doc-entries
      (cons
       (format *output* "~11A ~A ~4A~%" "ALIAS" "ABBR" "DESCRIPTION")
       (dolist (doc-entry doc-entries)
         (format *output* "~11A ~4A ~A~%" (first doc-entry) (second doc-entry) (third doc-entry))))
      (t
       (format *output* "No aliases are defined~%"))))
  (values))

(defun shell-cmd (string-arg)
  (sb-ext:run-program "/bin/sh" (list "-c" string-arg)
                      :input nil :output *output*)
  (values))

(defun pushd-cmd (string-arg)
  (push string-arg *dir-stack*)
  (cd-cmd string-arg)
  (values))

(defun popd-cmd ()
  (if *dir-stack*
      (let ((dir (pop *dir-stack*)))
        (cd-cmd dir))
      (format *output* "No directory on stack to pop.~%"))
  (values))

(defun pop-cmd (&optional (n 1))
  (cond
    (*inspect-break*
     (throw 'repl-catcher (values :inspect n)))
    ((plusp *break-level*)
     (throw 'repl-catcher (values :pop n))))
  (values))

(defun bt-cmd (&optional (n most-positive-fixnum))
  (sb-debug:print-backtrace :count n))

(defun current-cmd ()
  (sb-debug::describe-debug-command))

(defun top-cmd ()
  (sb-debug::frame-debug-command 0))

(defun bottom-cmd ()
  (sb-debug::bottom-debug-command))

(defun up-cmd (&optional (n 1))
  (dotimes (i n)
    (if (and sb-debug::*current-frame*
             (sb-di:frame-up sb-debug::*current-frame*))
        (sb-debug::up-debug-command)
        (progn
          (format *output* "Top of the stack")
          (return-from up-cmd)))))

(defun dn-cmd (&optional (n 1))
  (dotimes (i n)
    (if (and sb-debug::*current-frame*
             (sb-di:frame-down sb-debug::*current-frame*))
        (sb-debug::down-debug-command)
        (progn
          (format *output* "Bottom of the stack")
          (return-from dn-cmd)))))

(defun continue-cmd (&optional (num 0))
  ;; don't look at first restart
  (let ((restarts (compute-restarts)))
    (if restarts
        (let ((restart
               (typecase num
                 (unsigned-byte
                  (if (< -1 num (length restarts))
                      (nth num restarts)
                      (progn
                        (format *output* "There is no such restart")
                        (return-from continue-cmd))))
                 (symbol
                  (find num (the list restarts)
                        :key #'restart-name
                        :test (lambda (sym1 sym2)
                                (string= (symbol-name sym1)
                                         (symbol-name sym2)))))
                 (t
                  (format *output* "~S is invalid as a restart name" num)
                  (return-from continue-cmd nil)))))
          (when restart
            (invoke-restart-interactively restart)))
    (format *output* "~&There are no restarts"))))

(defun error-cmd ()
  (when (plusp *break-level*)
    (if *inspect-break*
        (sb-debug::show-restarts (compute-restarts) *output*)
        (let ((sb-debug::*debug-restarts* (compute-restarts)))
          (sb-debug::error-debug-command)))))

(defun frame-cmd ()
  (sb-debug::print-frame-call sb-debug::*current-frame* t))

(defun zoom-cmd ()
  )

(defun local-cmd (&optional var)
  (declare (ignore var))
  (sb-debug::list-locals-debug-command))

(defun processes-cmd ()
  #+sb-thread
  (dolist (thread (all-threads))
    (format *output* "~&~A" thread)
    (when (eq thread sb-thread:*current-thread*)
      (format *output* " [current listener]")))
  #-sb-thread
  (format *output* "~&Threads are not supported in this version of sbcl")
  (values))

(defun sb-aclrepl::kill-cmd (&rest selected-threads)
  #+sb-thread
  (dolist (thread selected-threads)
    (let ((found (find thread (all-threads) :key 'sb-thread:thread-name
                       :test 'equal)))
      (if found
          (progn
            (format *output* "~&Destroying thread ~A" thread)
            (sb-thread:terminate-thread found))
          (format *output* "~&Thread ~A not found" thread))))
  #-sb-thread
  (declare (ignore selected-threads))
  #-sb-thread
  (format *output* "~&Threads are not supported in this version of sbcl")
  (values))

(defun focus-cmd (&optional process)
  #-sb-thread
  (declare (ignore process))
  #+sb-thread
  (when process
    (format *output* "~&Focusing on next thread waiting waiting for the debugger~%"))
  #+sb-thread
  (progn
    (sb-thread:release-foreground)
    (sleep 1))
  #-sb-thread
  (format *output* "~&Threads are not supported in this version of sbcl")
  (values))

(defun reset-cmd ()
  (throw 'sb-impl::toplevel-catcher nil))

(defun dirs-cmd ()
  (dolist (dir *dir-stack*)
    (format *output* "~a~%" dir))
  (values))


;;;; dispatch table for commands

(let ((cmd-table
       '(("aliases" 3 alias-cmd "show aliases")
         ("apropos" 2 apropos-cmd "show apropos" :parsing :string)
         ("bottom" 3 bottom-cmd "move to bottom stack frame")
         ("top" 3 top-cmd "move to top stack frame")
         ("bt" 2 bt-cmd "backtrace `n' stack frames, default all")
         ("up" 2 up-cmd "move up `n' stack frames, default 1")
         ("dn" 2 dn-cmd "move down `n' stack frames, default 1")
         ("cd" 2 cd-cmd "change default diretory" :parsing :string)
         ("ld" 2 ld-cmd "load a file" :parsing :string)
         ("cf" 2 cf-cmd "compile file" :parsing :string)
         ("cload" 2 cload-cmd "compile if needed and load file"
          :parsing :string)
         ("current" 3 current-cmd "print the expression for the current stack frame")
         ("continue" 4 continue-cmd "continue from a continuable error")
         ("describe" 2 describe-cmd "describe an object")
         ("macroexpand" 2 macroexpand-cmd "macroexpand an expression")
         ("package" 2 package-cmd "change current package")
         ("error" 3 error-cmd "print the last error message")
         ("exit" 2 exit-cmd "exit sbcl")
         ("frame" 2 frame-cmd "print info about the current frame")
         ("help" 2 help-cmd "print this help")
         ("history" 3 history-cmd "print the recent history")
         ("inspect" 2 inspect-cmd "inspect an object")
         ("istep" 1 istep-cmd "navigate within inspection of a lisp object" :parsing :string)
         #+sb-thread ("kill" 2 kill-cmd "kill (destroy) processes")
         #+sb-thread ("focus" 2 focus-cmd "focus the top level on a process")
         ("local" 3 local-cmd "print the value of a local variable")
         ("pwd" 3 pwd-cmd "print current directory")
         ("pushd" 2 pushd-cmd "push directory on stack" :parsing :string)
         ("pop" 3 pop-cmd "pop up `n' (default 1) break levels")
         ("popd" 4 popd-cmd "pop directory from stack")
         #+sb-thread ("processes" 3 processes-cmd "list all processes")
         ("reset" 3 reset-cmd "reset to top break level")
         ("trace" 2 trace-cmd "trace a function")
         ("untrace" 4 untrace-cmd "untrace a function")
         ("dirs" 2 dirs-cmd "show directory stack")
         ("shell" 2 shell-cmd "execute a shell cmd" :parsing :string)
         ("zoom" 2 zoom-cmd "print the runtime stack")
         )))
  (dolist (cmd cmd-table)
    (destructuring-bind (cmd-string abbr-len func-name desc &key parsing) cmd
      (add-cmd-table-entry cmd-string abbr-len func-name desc parsing))))

;;;; machinery for aliases

(defsetf alias (name &key abbr-len description) (user-func)
  `(progn
    (%add-entry
     (make-cte (quote ,name) ,user-func ,description nil :alias ,abbr-len))
    (quote ,name)))

(defmacro alias (name-param args &rest body)
  (let ((parsing nil)
        (desc "")
        (abbr-index nil)
        (name (if (atom name-param)
                  name-param
                  (car name-param))))
    (when (consp name-param)
     (dolist (param (cdr name-param))
        (cond
          ((or
            (eq param :case-sensitive)
            (eq param :string))
           (setq parsing param))
          ((stringp param)
           (setq desc param))
          ((numberp param)
           (setq abbr-index param)))))
    `(progn
      (%add-entry
       (make-cte (quote ,name) (lambda ,args ,@body) ,desc ,parsing :alias (when ,abbr-index
                                                                               (1+ ,abbr-index)))
       ,abbr-index)
      ,name)))


(defun remove-alias (&rest aliases)
  (declare (list aliases))
  (let ((keys '())
        (remove-all (not (null (find :all aliases)))))
    (unless remove-all  ;; ensure all alias are strings
      (setq aliases
            (loop for alias in aliases
                  collect
                  (etypecase alias
                    (string
                     alias)
                    (symbol
                     (symbol-name alias))))))
    (maphash
     (lambda (key cmd)
       (when (eq (cmd-table-entry-group cmd) :alias)
         (if remove-all
             (push key keys)
             (when (some
                    (lambda (alias)
                      (let ((klen (length key)))
                        (and (>= (length alias) klen)
                             (string-equal (subseq alias 0 klen)
                                           (subseq key 0 klen)))))
                    aliases)
               (push key keys)))))
     *cmd-table-hash*)
    (dolist (key keys)
      (remhash key *cmd-table-hash*))
    keys))

;;;; low-level reading/parsing functions

;;; Skip white space (but not #\NEWLINE), and peek at the next
;;; character.
(defun peek-char-non-whitespace (&optional stream)
  (do ((char (peek-char nil stream nil *eof-marker*)
             (peek-char nil stream nil *eof-marker*)))
      ((not (whitespace-char-not-newline-p char)) char)
    (read-char stream)))

(defun string-trim-whitespace (str)
  (string-trim '(#\space #\tab #\return)
               str))

(defun whitespace-char-p (x)
  (and (characterp x)
       (or (char= x #\space)
           (char= x #\tab)
           (char= x #\page)
           (char= x #\newline)
           (char= x #\return))))

(defun whitespace-char-not-newline-p (x)
  (and (whitespace-char-p x)
       (not (char= x #\newline))))

;;;; linking into SBCL hooks

(defun repl-prompt-fun (stream)
  (let ((break-level (when (plusp *break-level*)
                       *break-level*))
        (frame-number (when (and (plusp *break-level*)
                                 sb-debug::*current-frame*)
                        (sb-di::frame-number sb-debug::*current-frame*))))
    (sb-thread::get-foreground)
    (fresh-line stream)
    (if (functionp *prompt*)
        (write-string (funcall *prompt*
                               break-level
                               frame-number
                               *inspect-break*
                               *continuable-break*
                               (prompt-package-name) *cmd-number*)
                      stream)
        (handler-case
            (format nil *prompt*
                    break-level
                    frame-number
                    *inspect-break*
                    *continuable-break*
                    (prompt-package-name) *cmd-number*)
          (error ()
            (format stream "~&Prompt error>  "))
          (:no-error (prompt)
            (format stream "~A" prompt))))))

(defun process-cmd (user-cmd)
  ;; Processes a user command. Returns t if the user-cmd was a top-level
  ;; command
  (cond ((eq user-cmd *eof-cmd*)
         (when *exit-on-eof*
           (sb-ext:exit))
         (format *output* "EOF~%")
         t)
        ((eq user-cmd *null-cmd*)
         t)
        ((eq (user-cmd-func user-cmd) :cmd-error)
         (format *output* "Unknown top-level command: ~s.~%"
                 (user-cmd-input user-cmd))
         (format *output* "Type `~Ahelp' for the list of commands.~%" *command-char*)
         t)
        ((eq (user-cmd-func user-cmd) :history-error)
         (format *output* "Input numbered ~d is not on the history list~%"
                 (user-cmd-input user-cmd))
         t)
        ((functionp (user-cmd-func user-cmd))
         (add-to-history user-cmd)
         (apply (user-cmd-func user-cmd) (user-cmd-args user-cmd))
         ;;(fresh-line)
         t)
        (t
         (add-to-history user-cmd)
         nil))) ; nope, not in my job description

(defun repl-read-form-fun (input output)
  ;; Pick off all the leading ACL magic commands, then return a normal
  ;; Lisp form.
  (let ((*input* input)
        (*output* output))
    (loop for user-cmd = (read-cmd *input*) do
        (if (process-cmd user-cmd)
            (progn
              (funcall sb-int:*repl-prompt-fun* *output*)
              (force-output *output*))
            (return (user-cmd-input user-cmd))))))


(setf sb-int:*repl-prompt-fun* #'repl-prompt-fun
      sb-int:*repl-read-form-fun* #'repl-read-form-fun)

(defmacro with-new-repl-state ((&rest vars) &body forms)
  (let ((gvars (mapcar (lambda (var) (gensym (symbol-name var))) vars)))
    `(let (,@(mapcar (lambda (var gvar) `(,gvar ,var)) vars gvars))
      (lambda (noprint)
        (let ((*noprint* noprint))
          (let (,@(mapcar (lambda (var gvar) `(,var ,gvar)) vars gvars))
            (unwind-protect
                 (progn ,@forms)
              ,@(mapcar (lambda (var gvar) `(setf ,gvar ,var))
                        vars gvars))))))))

(defun make-repl-fun ()
  (with-new-repl-state (*break-level* *inspect-break* *continuable-break*
                        *dir-stack* *command-char* *prompt*
                        *use-short-package-name* *max-history* *exit-on-eof*
                        *history* *cmd-number*)
    (repl :noprint noprint :break-level 0)))

(when (boundp 'sb-impl::*repl-fun-generator*)
  (setq sb-impl::*repl-fun-generator* #'make-repl-fun))
