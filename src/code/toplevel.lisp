;;;; stuff related to the toplevel read-eval-print loop, plus some
;;;; other miscellaneous functions that we don't have any better place
;;;; for

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;;; magic specials initialized by GENESIS

;;; FIXME: The DEFVAR here is redundant with the (DECLAIM (SPECIAL ..))
;;; of all static symbols in early-impl.lisp.
(progn
  (defvar sb!vm::*current-catch-block*)
  (defvar sb!vm::*current-unwind-protect-block*)
  #!+hpux (defvar sb!vm::*c-lra*)
  (defvar *free-interrupt-context-index*))

;;; specials initialized by !COLD-INIT

;;; FIXME: These could be converted to DEFVARs.
(declaim (special #!+(or x86 x86-64) *pseudo-atomic-bits*
                  *allow-with-interrupts*
                  *interrupts-enabled*
                  *interrupt-pending*
                  *type-system-initialized*))

(defvar *cold-init-complete-p*)

;;; counts of nested errors (with internal errors double-counted)
(defvar *maximum-error-depth*)
(defvar *current-error-depth*)

;;;; default initfiles

(defun sysinit-pathname ()
  (or (let ((sbcl-homedir (sbcl-homedir-pathname)))
        (when sbcl-homedir
          (probe-file (merge-pathnames "sbclrc" sbcl-homedir))))
      #!+win32
      (merge-pathnames "sbcl\\sbclrc"
                       (sb!win32::get-folder-pathname
                        sb!win32::csidl_common_appdata))
      #!-win32
      "/etc/sbclrc"))

(defun userinit-pathname ()
  (merge-pathnames ".sbclrc" (user-homedir-pathname)))

(defvar *sysinit-pathname-function* #'sysinit-pathname
  #!+sb-doc
  "Designator for a function of zero arguments called to obtain a pathname
designator for the default sysinit file, or NIL. If the function returns NIL,
no sysinit file is used unless one has been specified on the command-line.")

(defvar *userinit-pathname-function* #'userinit-pathname
  #!+sb-doc
  "Designator for a function of zero arguments called to obtain a pathname
designator or a stream for the default userinit file, or NIL. If the function
returns NIL, no userinit file is used unless one has been specified on the
command-line.")


;;;; miscellaneous utilities for working with with TOPLEVEL

;;; Execute BODY in a context where any %END-OF-THE-WORLD (thrown e.g.
;;; by QUIT) is caught and any final processing and return codes are
;;; handled appropriately.
(defmacro handling-end-of-the-world (&body body)
  (with-unique-names (caught)
    `(let ((,caught (catch '%end-of-the-world
                      (/show0 "inside CATCH '%END-OF-THE-WORLD")
                      (unwind-protect
                           (progn ,@body)
                        (call-hooks "exit" *exit-hooks*)))))
      (/show0 "back from CATCH '%END-OF-THE-WORLD, flushing output")
      (flush-standard-output-streams)
      (sb!thread::terminate-session)
      (/show0 "calling UNIX-EXIT")
      (sb!unix:unix-exit ,caught))))

;;;; working with *CURRENT-ERROR-DEPTH* and *MAXIMUM-ERROR-DEPTH*

;;; INFINITE-ERROR-PROTECT is used by ERROR and friends to keep us out
;;; of hyperspace.
(defmacro infinite-error-protect (&rest forms)
  `(unless (infinite-error-protector)
     (/show0 "back from INFINITE-ERROR-PROTECTOR")
     (let ((*current-error-depth* (1+ *current-error-depth*)))
       (/show0 "in INFINITE-ERROR-PROTECT, incremented error depth")
       ;; arbitrary truncation
       #!+sb-show (sb!debug:backtrace 8)
       ,@forms)))

;;; a helper function for INFINITE-ERROR-PROTECT
(defun infinite-error-protector ()
  (/show0 "entering INFINITE-ERROR-PROTECTOR, *CURRENT-ERROR-DEPTH*=..")
  (/hexstr *current-error-depth*)
  (cond ((not *cold-init-complete-p*)
         (%primitive print "Argh! error in cold init, halting")
         (%primitive sb!c:halt))
        ((or (not (boundp '*current-error-depth*))
             (not (realp   *current-error-depth*))
             (not (boundp '*maximum-error-depth*))
             (not (realp   *maximum-error-depth*)))
         (%primitive print "Argh! corrupted error depth, halting")
         (%primitive sb!c:halt))
        ((> *current-error-depth* *maximum-error-depth*)
         (/show0 "*MAXIMUM-ERROR-DEPTH*=..")
         (/hexstr *maximum-error-depth*)
         (/show0 "in INFINITE-ERROR-PROTECTOR, calling ERROR-ERROR")
         (error-error "Help! "
                      *current-error-depth*
                      " nested errors. "
                      "SB-KERNEL:*MAXIMUM-ERROR-DEPTH* exceeded.")
         t)
        (t
         (/show0 "returning normally from INFINITE-ERROR-PROTECTOR")
         nil)))

;;; FIXME: I had a badly broken version of INFINITE-ERROR-PROTECTOR at
;;; one point (shown below), and SBCL cross-compiled it without
;;; warning about FORMS being undefined. Check whether that problem
;;; (missing warning) is repeatable in the final system and if so, fix
;;; it.
#|
(defun infinite-error-protector ()
  `(cond ((not *cold-init-complete-p*)
          (%primitive print "Argh! error in cold init, halting")
          (%primitive sb!c:halt))
         ((or (not (boundp '*current-error-depth*))
              (not (realp   *current-error-depth*))
              (not (boundp '*maximum-error-depth*))
              (not (realp   *maximum-error-depth*)))
          (%primitive print "Argh! corrupted error depth, halting")
          (%primitive sb!c:halt))
         ((> *current-error-depth* *maximum-error-depth*)
          (/show0 "in INFINITE-ERROR-PROTECTOR, calling ERROR-ERROR")
          (error-error "Help! "
                       *current-error-depth*
                       " nested errors. "
                       "SB-KERNEL:*MAXIMUM-ERROR-DEPTH* exceeded.")
          (progn ,@forms)
          t)
         (t
          (/show0 "in INFINITE-ERROR-PROTECTOR, returning normally")
          nil)))
|#

;;;; miscellaneous external functions

(defun sleep (seconds)
  #!+sb-doc
  "This function causes execution to be suspended for SECONDS. SECONDS may be
any non-negative real number."
  (when (or (not (realp seconds))
            (minusp seconds))
    (error 'simple-type-error
           :format-control "invalid argument to SLEEP: ~S"
           :format-arguments (list seconds)
           :datum seconds
           :expected-type '(real 0)))
  #!-win32
  (multiple-value-bind (sec nsec)
      (if (integerp seconds)
          (values seconds 0)
          (multiple-value-bind (sec frac)
              (truncate seconds)
            (values sec (truncate frac 1e-9))))
    ;; nanosleep() accepts time_t as the first argument, but on some platforms
    ;; it is restricted to 100 million seconds. Maybe someone can actually
    ;; have a reason to sleep for over 3 years?
    (loop while (> sec (expt 10 8))
          do (decf sec (expt 10 8))
             (sb!unix:nanosleep (expt 10 8) 0))
    (sb!unix:nanosleep sec nsec))
  #!+win32
  (sb!win32:millisleep (truncate (* seconds 1000)))
  nil)

;;;; the default toplevel function

(defvar / nil
  #!+sb-doc
  "a list of all the values returned by the most recent top level EVAL")
(defvar //  nil #!+sb-doc "the previous value of /")
(defvar /// nil #!+sb-doc "the previous value of //")
(defvar *   nil #!+sb-doc "the value of the most recent top level EVAL")
(defvar **  nil #!+sb-doc "the previous value of *")
(defvar *** nil #!+sb-doc "the previous value of **")
(defvar +   nil #!+sb-doc "the value of the most recent top level READ")
(defvar ++  nil #!+sb-doc "the previous value of +")
(defvar +++ nil #!+sb-doc "the previous value of ++")
(defvar -   nil #!+sb-doc "the form currently being evaluated")

(defun interactive-eval (form &key (eval #'eval))
  #!+sb-doc
  "Evaluate FORM, returning whatever it returns and adjusting ***, **, *,
+++, ++, +, ///, //, /, and -."
  (setf - form)
  (unwind-protect
       (let ((results (multiple-value-list (funcall eval form))))
         (setf /// //
               // /
               / results
               *** **
               ** *
               * (car results)))
    (setf +++ ++
          ++ +
          + -))
  (unless (boundp '*)
    ;; The bogon returned an unbound marker.
    ;; FIXME: It would be safer to check every one of the values in RESULTS,
    ;; instead of just the first one.
    (setf * nil)
    (cerror "Go on with * set to NIL."
            "EVAL returned an unbound marker."))
  (values-list /))

;;; Flush anything waiting on one of the ANSI Common Lisp standard
;;; output streams before proceeding.
(defun flush-standard-output-streams ()
  (dolist (name '(*debug-io*
                  *error-output*
                  *query-io*
                  *standard-output*
                  *trace-output*
                  *terminal-io*))
    ;; FINISH-OUTPUT may block more easily than FORCE-OUTPUT
    (force-output (symbol-value name)))
  (values))

(defun process-init-file (specified-pathname kind)
  (multiple-value-bind (context default-function)
      (ecase kind
        (:system
         (values "sysinit" *sysinit-pathname-function*))
        (:user
         (values "userinit" *userinit-pathname-function*)))
    (flet ((process-stream (stream pathname)
             (with-simple-restart (abort "Skip rest of ~A file ~S."
                                         context (native-namestring pathname))
               (loop
                 (with-simple-restart
                     (continue "Ignore error and continue processing ~A file ~S."
                               context (native-namestring pathname))
                   (let ((form (read stream nil stream)))
                     (if (eq stream form)
                         (return-from process-init-file nil)
                         (eval form))))))))
      (if specified-pathname
          (with-open-file (stream (parse-native-namestring specified-pathname)
                                  :if-does-not-exist nil)
            (if stream
                (process-stream stream (pathname stream))
                (cerror "Ignore missing init file"
                        "The specified ~A file ~A was not found."
                        context specified-pathname)))
          (let ((default (funcall default-function)))
            (when default
              (with-open-file (stream (pathname default) :if-does-not-exist nil)
                (when stream
                  (process-stream stream (pathname stream))))))))))

(defun process-eval/load-options (options)
  (/show0 "handling --eval and --load options")
  (flet ((process-1 (cons)
           (destructuring-bind (opt . value) cons
             (ecase opt
               (:eval
                (with-simple-restart (continue "Ignore runtime option --eval ~S."
                                               value)
                  (multiple-value-bind (expr pos) (read-from-string value)
                    (if (eq value (read-from-string value nil value :start pos))
                        (eval expr)
                        (error "Multiple expressions in --eval option: ~S"
                               value)))))
               (:load
                (with-simple-restart (continue "Ignore runtime option --load ~S."
                                               value)
                  (load (native-pathname value))))))
           (flush-standard-output-streams)))
    (with-simple-restart (abort "Skip rest of --eval and --load options.")
      (dolist (option options)
        (process-1 option)))))

(defun process-script (script)
  (let ((pathname (native-pathname script)))
    (handling-end-of-the-world
      (with-open-file (f pathname :element-type :default)
        (sb!fasl::maybe-skip-shebang-line f)
        (load f :verbose nil :print nil)
        (quit)))))

;; Errors while processing the command line cause the system to QUIT,
;; instead of trying to go into the Lisp debugger, because trying to
;; go into the Lisp debugger would get into various annoying issues of
;; where we should go after the user tries to return from the
;; debugger.
(defun startup-error (control-string &rest args)
  (format *error-output*
          "fatal error before reaching READ-EVAL-PRINT loop: ~%  ~?~%"
          control-string
          args)
  (quit :unix-status 1))

;;; the default system top level function
(defun toplevel-init ()
  (/show0 "entering TOPLEVEL-INIT")
  (let ( ;; value of --sysinit option
        (sysinit nil)
        ;; t if --no-sysinit option given
        (no-sysinit nil)
        ;; value of --userinit option
        (userinit nil)
        ;; t if --no-userinit option given
        (no-userinit nil)
        ;; t if --disable-debugger option given
        (disable-debugger nil)
        ;; list of (<kind> . <string>) conses representing --eval and --load
        ;; options. options. --eval options are stored as strings, so that
        ;; they can be passed to READ only after their predecessors have been
        ;; EVALed, so that things work when e.g. REQUIRE in one EVAL form
        ;; creates a package referred to in the next EVAL form. Storing the
        ;; original string also makes for easier debugging.
        (reversed-options nil)
        ;; Has a --noprint option been seen?
        (noprint nil)
        ;; Has a --script option been seen?
        (script nil)
        ;; everything in *POSIX-ARGV* except for argv[0]=programname
        (options (rest *posix-argv*)))

    (declare (type list options))

    (/show0 "done with outer LET in TOPLEVEL-INIT")

    ;; FIXME: There are lots of ways for errors to happen around here
    ;; (e.g. bad command line syntax, or READ-ERROR while trying to
    ;; READ an --eval string). Make sure that they're handled
    ;; reasonably.

    ;; Process command line options.
    (loop while options do
         (/show0 "at head of LOOP WHILE OPTIONS DO in TOPLEVEL-INIT")
         (let ((option (first options)))
           (flet ((pop-option ()
                    (if options
                        (pop options)
                        (startup-error
                         "unexpected end of command line options"))))
             (cond ((string= option "--script")
                    (pop-option)
                    (setf disable-debugger t
                          no-userinit t
                          no-sysinit t
                          script (pop-option))
                    (return))
                   ((string= option "--sysinit")
                    (pop-option)
                    (if sysinit
                        (startup-error "multiple --sysinit options")
                        (setf sysinit (pop-option))))
                   ((string= option "--no-sysinit")
                    (pop-option)
                    (setf no-sysinit t))
                   ((string= option "--userinit")
                    (pop-option)
                    (if userinit
                        (startup-error "multiple --userinit options")
                        (setf userinit (pop-option))))
                   ((string= option "--no-userinit")
                    (pop-option)
                    (setf no-userinit t))
                   ((string= option "--eval")
                    (pop-option)
                    (push (cons :eval (pop-option)) reversed-options))
                   ((string= option "--load")
                    (pop-option)
                    (push (cons :load (pop-option)) reversed-options))
                   ((string= option "--noprint")
                    (pop-option)
                    (setf noprint t))
                   ((string= option "--disable-debugger")
                    (pop-option)
                    (setf disable-debugger t))
                   ((string= option "--end-toplevel-options")
                    (pop-option)
                    (return))
                   (t
                    ;; Anything we don't recognize as a toplevel
                    ;; option must be the start of user-level
                    ;; options.. except that if we encounter
                    ;; "--end-toplevel-options" after we gave up
                    ;; because we didn't recognize an option as a
                    ;; toplevel option, then the option we gave up on
                    ;; must have been an error. (E.g. in
                    ;;  "sbcl --eval '(a)' --eval'(b)' --end-toplevel-options"
                    ;; this test will let us detect that the string
                    ;; "--eval(b)" is an error.)
                    (if (find "--end-toplevel-options" options
                              :test #'string=)
                        (startup-error "bad toplevel option: ~S"
                                       (first options))
                        (return)))))))
    (/show0 "done with LOOP WHILE OPTIONS DO in TOPLEVEL-INIT")

    ;; Delete all the options that we processed, so that only
    ;; user-level options are left visible to user code.
    (setf (rest *posix-argv*) options)

    ;; Disable debugger before processing initialization files & co.
    (when disable-debugger
      (sb!ext:disable-debugger))

    ;; Handle initialization files.
    (/show0 "handling initialization files in TOPLEVEL-INIT")
    ;; This CATCH is needed for the debugger command TOPLEVEL to
    ;; work.
    (catch 'toplevel-catcher
      ;; We wrap all the pre-REPL user/system customized startup
      ;; code in a restart.
      ;;
      ;; (Why not wrap everything, even the stuff above, in this
      ;; restart? Errors above here are basically command line
      ;; or Unix environment errors, e.g. a missing file or a
      ;; typo on the Unix command line, and you don't need to
      ;; get into Lisp to debug them, you should just start over
      ;; and do it right at the Unix level. Errors below here
      ;; are generally errors in user Lisp code, and it might be
      ;; helpful to let the user reach the REPL in order to help
      ;; figure out what's going on.)
      (restart-case
          (progn
            (unless no-sysinit
              (process-init-file sysinit :system))
            (unless no-userinit
              (process-init-file userinit :user))
            (process-eval/load-options (nreverse reversed-options))
            (when script
              (process-script script)
              (bug "PROCESS-SCRIPT returned")))
        (abort ()
          :report (lambda (s)
                    (write-string
                     (if script
                         ;; In case script calls (enable-debugger)!
                         "Abort script, exiting lisp."
                         "Skip to toplevel READ/EVAL/PRINT loop.")
                     s))
          (/show0 "CONTINUEing from pre-REPL RESTART-CASE")
          (values))                     ; (no-op, just fall through)
        (quit ()
          :report "Quit SBCL (calling #'QUIT, killing the process)."
          :test (lambda (c) (declare (ignore c)) (not script))
          (/show0 "falling through to QUIT from pre-REPL RESTART-CASE")
          (quit :unix-status 1))))

    ;; one more time for good measure, in case we fell out of the
    ;; RESTART-CASE above before one of the flushes in the ordinary
    ;; flow of control had a chance to operate
    (flush-standard-output-streams)

    (/show0 "falling into TOPLEVEL-REPL from TOPLEVEL-INIT")
    (toplevel-repl noprint)
    ;; (classic CMU CL error message: "You're certainly a clever child.":-)
    (critically-unreachable "after TOPLEVEL-REPL")))

;;; hooks to support customized toplevels like ACL-style toplevel from
;;; KMR on sbcl-devel 2002-12-21.  Altered by CSR 2003-11-16 for
;;; threaded operation: altered *REPL-FUN* to *REPL-FUN-GENERATOR*.
(defvar *repl-read-form-fun* #'repl-read-form-fun
  #!+sb-doc
  "A function of two stream arguments IN and OUT for the toplevel REPL to
call: Return the next Lisp form to evaluate (possibly handling other magic --
like ACL-style keyword commands -- which precede the next Lisp form). The OUT
stream is there to support magic which requires issuing new prompts.")
(defvar *repl-prompt-fun* #'repl-prompt-fun
  #!+sb-doc
  "A function of one argument STREAM for the toplevel REPL to call: Prompt
the user for input.")
(defvar *repl-fun-generator* (constantly #'repl-fun)
  #!+sb-doc
  "A function of no arguments returning a function of one argument NOPRINT
that provides the REPL for the system. Assumes that *STANDARD-INPUT* and
*STANDARD-OUTPUT* are set up.")

;;; read-eval-print loop for the default system toplevel
(defun toplevel-repl (noprint)
  (/show0 "entering TOPLEVEL-REPL")
  (let ((* nil) (** nil) (*** nil)
        (- nil)
        (+ nil) (++ nil) (+++ nil)
        (/// nil) (// nil) (/ nil))
    (/show0 "about to funcall *REPL-FUN-GENERATOR*")
    (let ((repl-fun (funcall *repl-fun-generator*)))
      ;; Each REPL in a multithreaded world should have bindings of
      ;; most CL specials (most critically *PACKAGE*).
      (with-rebound-io-syntax
          (handler-bind ((step-condition 'invoke-stepper))
            (loop
               (/show0 "about to set up restarts in TOPLEVEL-REPL")
               ;; CLHS recommends that there should always be an
               ;; ABORT restart; we have this one here, and one per
               ;; debugger level.
               (with-simple-restart
                   (abort "~@<Exit debugger, returning to top level.~@:>")
                 (catch 'toplevel-catcher
                   ;; In the event of a control-stack-exhausted-error, we
                   ;; should have unwound enough stack by the time we get
                   ;; here that this is now possible.
                   #!-win32
                   (sb!kernel::reset-control-stack-guard-page)
                   (funcall repl-fun noprint)
                   (critically-unreachable "after REPL")))))))))

;;; Our default REPL prompt is the minimal traditional one.
(defun repl-prompt-fun (stream)
  (fresh-line stream)
  (write-string "* " stream)) ; arbitrary but customary REPL prompt

;;; Our default form reader does relatively little magic, but does
;;; handle the Unix-style EOF-is-end-of-process convention.
(defun repl-read-form-fun (in out)
  (declare (type stream in out) (ignore out))
  ;; KLUDGE: *READ-SUPPRESS* makes the REPL useless, and cannot be
  ;; recovered from -- flip it here.
  (when *read-suppress*
    (warn "Setting *READ-SUPPRESS* to NIL to restore toplevel usability.")
    (setf *read-suppress* nil))
  (let* ((eof-marker (cons nil nil))
         (form (read in nil eof-marker)))
    (if (eq form eof-marker)
        (quit)
        form)))

(defun repl-fun (noprint)
  (/show0 "entering REPL")
  (loop
   (unwind-protect
        (progn
          ;; (See comment preceding the definition of SCRUB-CONTROL-STACK.)
          (scrub-control-stack)
          (sb!thread::get-foreground)
          (unless noprint
            (flush-standard-output-streams)
            (funcall *repl-prompt-fun* *standard-output*)
            ;; (Should *REPL-PROMPT-FUN* be responsible for doing its own
            ;; FORCE-OUTPUT? I can't imagine a valid reason for it not to
            ;; be done here, so leaving it up to *REPL-PROMPT-FUN* seems
            ;; odd. But maybe there *is* a valid reason in some
            ;; circumstances? perhaps some deadlock issue when being driven
            ;; by another process or something...)
            (force-output *standard-output*))
          (let* ((form (funcall *repl-read-form-fun*
                                *standard-input*
                                *standard-output*))
                 (results (multiple-value-list (interactive-eval form))))
            (unless noprint
              (dolist (result results)
                (fresh-line)
                (prin1 result)))))
     ;; If we started stepping in the debugger we want to stop now.
     (disable-stepping))))

;;; a convenient way to get into the assembly-level debugger
(defun %halt ()
  (%primitive sb!c:halt))
