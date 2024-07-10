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

(in-package "SB-IMPL")

;;;; default initfiles

(defun sysinit-pathname ()
  (or (let ((sbcl-homedir (sbcl-homedir-pathname)))
        (when sbcl-homedir
          (probe-file (merge-pathnames "sbclrc" sbcl-homedir))))
      #+win32
      (merge-pathnames "sbcl\\sbclrc"
                       (sb-win32::get-folder-pathname
                        sb-win32::csidl_common_appdata))
      #-win32
      "/etc/sbclrc"))

(defun userinit-pathname ()
  (merge-pathnames ".sbclrc" (user-homedir-pathname)))

(define-load-time-global *sysinit-pathname-function* #'sysinit-pathname
  "Designator for a function of zero arguments called to obtain a
pathname designator for the default sysinit file, or NIL. If the
function returns NIL, no sysinit file is used unless one has been
specified on the command-line.")

(define-load-time-global *userinit-pathname-function* #'userinit-pathname
  "Designator for a function of zero arguments called to obtain a
pathname designator or a stream for the default userinit file, or NIL.
If the function returns NIL, no userinit file is used unless one has
been specified on the command-line.")


;;;; miscellaneous utilities for working with with TOPLEVEL

;;; Execute BODY in a context where any %END-OF-THE-WORLD (thrown e.g.
;;; by QUIT) is caught and any final processing and return codes are
;;; handled appropriately.
(defmacro handling-end-of-the-world (&body body)
  `(without-interrupts
     (catch '%end-of-the-world
       (unwind-protect
            (with-local-interrupts
              (unwind-protect
                   (progn ,@body)
                (call-exit-hooks)))
         (%exit)))))

(define-load-time-global *exit-lock* nil)
(define-thread-local *exit-in-progress* nil)
(declaim (type (or null real) *exit-timeout*))
(defvar *exit-timeout* 60
  "Default amount of seconds, if any, EXIT should wait for other
threads to finish after terminating them. Default value is 60. NIL
means to wait indefinitely.")

(defun os-exit-handler (condition)
  (declare (ignore condition))
  (os-exit *exit-in-progress* :abort t))

(defvar *exit-error-handler* #'os-exit-handler)

(defun call-exit-hooks ()
  (unless *exit-in-progress*
    (setf *exit-in-progress* 0))
  (handler-bind ((serious-condition *exit-error-handler*))
    (call-hooks "exit" *exit-hooks* :on-error :warn)))

;;; TERMINATE-THREAD as it exists is basically unsuitable for anything other than
;;; interactive use, because it uses INTERRUPT-THREAD to unwind through user code.
;;; It is only slightly ok to call INTERRUPT-THREAD on a thread when you know
;;; exactly what it is doing. But even if you know what it's doing, you don't know
;;; when, so even if it's your code, it might be calling an alien destructor.
;;; It says right in the docstring of INTERRUPT-THREAD not to call it,
;;; and then what do we do? We go full steam ahead and call it.
;;;   https://www.dictionary.com/e/slang/i-cant-even/
;;;
;;; The full story here is that some users seem to think that EXIT is responsible
;;; for stopping any threads. There was a not-insignificant amount of discussion
;;; in https://groups.google.com/g/sbcl-devel/c/6Xj_d1dM1k0/m/G9OPAbOueDsJ
;;; But the mechanism employed for %EXIT-OTHER-THREADS is TERMINATE-THREAD,
;;; and that's just horrible. No matter how well the SBCL code is constructed,
;;; this approach can cause lockup.
;;;
;;; The *EXIT-HOOKS* are fine, but we can't assume that users call JOIN-THREAD in
;;; the exit hooks - they might ask threads to stop by assigning a global polled
;;; flag or by CONDITION-BROADCAST.  They may do nothing. It's arbitrary.
;;; So maybe the threads are stopping "peacefully" and would finish on their own
;;; and we {could,should} just call JOIN-THREAD on them. But there's no way to know.
;;; Instead we just abort random threads with INTERRUPT-THREAD.
;;; Some of those threads that we forcibly terminate might have already been
;;; halfway through cleanly unwinding, which very possibly (or even likely)
;;; involves calling free() to release resources. When we interrupt a thread
;;; in the midst of its voluntary call to free(), no matter how clever free() is,
;;; we can presume that it will acquire a global lock.
;;;
;;; And there you have it - some dead thread owned a global lock that can't become
;;; available because we just punched that thread in the guts and killed it.
;;; Hence *all* threads (including non-Lisp) are permanently stuck waiting on
;;; a lock that is not owned by any running thread.
;;;
;;; I tried to remedy this by having the SIGURG signal handler silently discard
;;; a signal if interrupted at a non-Lisp program counter, but that presumes the
;;; call stack is never Lisp -> C -> Lisp, which first of all means that the handler
;;; needs to be entirely in C (if called from TERMINATE-THREAD) and secondly requires
;;; TERMINATE-THREAD to keep trying to send a signal if it thinks the signal was dropped.
;;; And you can't figure out whether a signal got purposely discard because then you'd
;;; need to wait an arbitrary amount of time to decide whether it might have been.
;;; Or invent a synchronized technique to see whether the asynchronous mechanism worked.
;;;
;;; So then the solution occurred to me:
;;;  --> just don't terminate threads by force. Problem solved. <--
;;;
;;; Therefore, if you want EXIT to be as near to the C exit() call as possible,
;;; after doing the lisp exit hooks, just set this global flag to NIL.
;;; Or leave it as T if you're fine with the old lockup-prone behavior.
;;;
;;; And if you think it's up to the Lisp environment to try to make threads stop
;;; by hook or by crook as a precondition to calling OS-EXIT (with which I disagree),
;;; the right solution is possibly modeled on pthread_cancel() which has specified
;;; cancelation points (system and C library calls) which check for cancelation,
;;; perform whatever cleanups were pushed by pthread_cleanup_push() and then stop.
;;;
;;; P.S. To see that #+sb-safepoint is no magic fix - suppose you have a stack
;;; with Lisp -> C -> -> Lisp where C acquired a resource and the currently
;;; top-of-stack Lisp function took a safepoint trap. It would be fine if all it
;;; wanted to do was GC, but is not safe in general.
;;; Hence #+sb-safepoint paints a dangerously attractive veneer over an unsafe
;;; concept, making it more subtly bad instead of very obviously bad.
;;;
;;; So this is a bad default. Bad bad bad. But it's backward-compatible.
(define-load-time-global *forcibly-terminate-threads-on-exit* t)

(defun %exit ()
  ;; If anything goes wrong, we will exit immediately and forcibly.
  (handler-bind ((serious-condition *exit-error-handler*))
    (let ((ok nil)
          (code *exit-in-progress*))
      (if (consp code)
          ;; Another thread called EXIT, and passed the buck to us -- only
          ;; final call left to do.
          (os-exit (car code) :abort nil)
          (unwind-protect
               (progn
                 (flush-standard-output-streams)
                 (if *forcibly-terminate-threads-on-exit*
                     (sb-thread::%exit-other-threads)
                     (with-deadline (:seconds nil :override t)
                       #+sb-thread (finalizer-thread-stop)
                       (sb-thread:grab-mutex sb-thread::*make-thread-lock*)))
                 (setf ok t))
            (os-exit code :abort (not ok)))))))

;;;; miscellaneous external functions

(declaim (inline split-ratio-for-sleep))
(defun split-ratio-for-sleep (seconds)
  (declare (ratio seconds)
           (muffle-conditions compiler-note))
  (multiple-value-bind (quot rem) (truncate (numerator seconds)
                                            (denominator seconds))
    (values quot
            (* rem
               #.(if (sb-xc:typep 1000000000 'fixnum)
                     '(truncate 1000000000 (denominator seconds))
                     ;; Can't truncate a bignum by a fixnum without consing
                     '(* 10 (truncate 100000000 (denominator seconds))))))))

(defun split-seconds-for-sleep (seconds)
  (declare (muffle-conditions compiler-note))
  ;; KLUDGE: This whole thing to avoid consing floats
  (flet ((split-float ()
           (let ((whole-seconds (truly-the fixnum (%unary-truncate seconds))))
             (values whole-seconds
                     (truly-the (integer 0 #.(expt 10 9))
                                (%unary-truncate (* (- seconds (float whole-seconds seconds))
                                                    1f9)))))))
    (declare (inline split-float))
    (typecase seconds
      ((single-float 0f0 #.(float most-positive-fixnum 1f0))
       (split-float))
      ((double-float 0d0 #.(float most-positive-fixnum 1d0))
       (split-float))
      (ratio
       (split-ratio-for-sleep seconds))
      (t
       (multiple-value-bind (sec frac)
           (truncate seconds)
         (values sec (truncate frac 1f-9)))))))

(declaim (inline %nanosleep))
(defun %nanosleep (sec nsec)
  ;; nanosleep() accepts time_t as the first argument, but on some
  ;; platforms it is restricted to 100 million seconds. Maybe someone
  ;; can actually have a reason to sleep for over 3 years?
  (loop while (> sec (expt 10 8))
     do (decf sec (expt 10 8))
       (sb-unix:nanosleep (expt 10 8) 0))
  (sb-unix:nanosleep sec nsec))

(declaim (inline %sleep))
#-win32
(defun %sleep (seconds)
  (typecase seconds
    (double-float
     (sb-unix::nanosleep-double seconds))
    (single-float
     (sb-unix::nanosleep-float seconds))
    (integer
     (%nanosleep seconds 0))
    (t
     (multiple-value-call #'%nanosleep (split-ratio-for-sleep seconds)))))

#+(and win32 sb-thread)
(defun %sleep (seconds)
  (if (integerp seconds)
      (%nanosleep seconds 0)
      (multiple-value-call #'%nanosleep (split-seconds-for-sleep seconds))))

#+(and win32 (not sb-thread))
(defun %sleep (seconds)
  (sb-win32:millisleep (truncate (* seconds 1000))))

(defun sleep (seconds)
  "This function causes execution to be suspended for SECONDS. SECONDS may be
any non-negative real number."
  (declare (explicit-check))
  (when (or (not (realp seconds))
            (minusp seconds))
    (error 'simple-type-error
           :format-control "Invalid argument to SLEEP: ~S, ~
                            should be a non-negative real."
           :format-arguments (list seconds)
           :datum seconds
           :expected-type '(real 0)))
  (if *deadline*
      (let ((start (get-internal-real-time))
            ;; SECONDS can be too large to present as INTERNAL-TIME,
            ;; use the largest representable value in that case.
            (timeout (or (seconds-to-maybe-internal-time seconds)
                         (* safe-internal-seconds-limit
                            internal-time-units-per-second))))
        (labels ((sleep-for-a-bit (remaining)
                   (multiple-value-bind
                         (timeout-sec timeout-usec stop-sec stop-usec deadlinep)
                       (decode-timeout (/ remaining internal-time-units-per-second))
                     (declare (ignore stop-sec stop-usec))
                     ;; Sleep until either the timeout or the deadline
                     ;; expires.
                     (when (or (plusp timeout-sec) (plusp timeout-usec))
                       (%nanosleep timeout-sec (* 1000 timeout-usec)))
                     ;; If the deadline expired first, signal the
                     ;; DEADLINE-TIMEOUT. If the deadline is deferred
                     ;; or canceled, go back to sleep for the
                     ;; remaining time (if any).
                     (when deadlinep
                       (signal-deadline)
                       (let ((remaining (- timeout
                                           (- (get-internal-real-time) start))))
                         (when (plusp remaining)
                           (sleep-for-a-bit remaining)))))))
          (sleep-for-a-bit timeout)))
      (%sleep seconds))
  nil)

;;;; the default toplevel function

(defvar / nil
  "a list of all the values returned by the most recent top level EVAL")
(defvar //  nil "the previous value of /")
(defvar /// nil "the previous value of //")
(defvar *   nil "the value of the most recent top level EVAL")
(defvar **  nil "the previous value of *")
(defvar *** nil "the previous value of **")
(defvar +   nil "the value of the most recent top level READ")
(defvar ++  nil "the previous value of +")
(defvar +++ nil "the previous value of ++")
(defvar -   nil "the form currently being evaluated")

(defun interactive-eval (form &key (eval #'eval))
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
      ;; 0. Pull out the underlying stream, so we know what it is.
      ;; 1. Handle errors on it. We're doing this on entry to
      ;;    debugger, so we don't want recursive errors here.
      ;; 2. Rebind the stream symbol in case some poor sod sees
      ;;    a broken stream here while running with *BREAK-ON-ERRORS*.
  (flet ((flush (stream)
           (let ((stream (stream-output-stream stream)))
             (handler-bind ((stream-error
                             (lambda (c)
                               (when (eq (stream-error-stream c) stream)
                                 (return-from flush)))))
               (force-output stream)))))
    (let ((null-stream (load-time-value *null-broadcast-stream* t)))
      (macrolet ((flush-all (&rest streamvars)
                   `(progn
                      ,@(mapcar (lambda (streamvar)
                                  `(let ((stream ,streamvar)
                                         (,streamvar null-stream))
                                     (flush stream)))
                                streamvars))))
        (flush-all *debug-io* *error-output* *query-io* *standard-output*
                   *trace-output* *terminal-io*))))
  (values))

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
                  (load (native-pathname value))))
               (:quit
                (exit))))
           (flush-standard-output-streams)))
    (with-simple-restart (abort "Skip rest of --eval and --load options.")
      (dolist (option options)
        (process-1 option)))))

(defun process-script (script)
  (flet ((load-script (stream)
           ;; Scripts don't need to be stylish or fast, but silence is usually a
           ;; desirable quality...
           (handler-bind (((or style-warning compiler-note) #'muffle-warning)
                          ((or broken-pipe end-of-file)
                            (lambda (e)
                              ;; Shell-style.
                              (when (member (stream-error-stream e)
                                            (list *stdout* *stdin* *stderr*))
                                (exit)))))
             ;; Let's not use the *TTY* for scripts, ok? Also, normally we use
             ;; synonym streams, but in order to have the broken pipe/eof error
             ;; handling right we want to bind them for scripts.
             (let ((*terminal-io* (make-two-way-stream *stdin* *stdout*))
                   (*debug-io* (make-two-way-stream *stdin* *stderr*))
                   (*standard-input* *stdin*)
                   (*standard-output* *stdout*)
                   (*error-output* *stderr*)
                   ;; Compile/load messages from the language implementation
                   ;; are not really what one would expect from a script. And
                   ;; these messages are printed on stdout interfere when
                   ;; scripts are used as part as a UNIX pipeline.
                   (*compile-verbose* nil)
                   (*load-verbose* nil))
               (load stream :verbose nil :print nil)))))
    (handling-end-of-the-world
      (if (eq t script)
          (load-script *stdin*)
          (with-open-file (f (native-pathname script) :element-type :default)
            (sb-fasl::maybe-skip-shebang-line f)
            (load-script f))))))

;; Errors while processing the command line cause the system to EXIT,
;; instead of trying to go into the Lisp debugger, because trying to
;; go into the Lisp debugger would get into various annoying issues of
;; where we should go after the user tries to return from the
;; debugger.
(defun startup-error (control-string &rest args)
  (format *error-output*
          "fatal error before reaching READ-EVAL-PRINT loop: ~%  ~?~%"
          control-string
          args)
  (exit :code 1))


(defconstant-eqx +runtime-options+
  #("--noinform" "--core" "--help" "--version" "--dynamic-space-size"
    "--control-stack-size" "--tls-limit"
    "--debug-environment" "--disable-ldb" "--lose-on-corruption"
    "--end-runtime-options" "--merge-core-pages" "--no-merge-core-pages")
  #'equalp)

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
        ;; Quit after processing other options?
        (finally-quit nil)
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
                          script (if options (pop-option) t))
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
                   ((string= option "--quit")
                    (pop-option)
                    (setf finally-quit t))
                   ((string= option "--non-interactive")
                    ;; This option is short for --quit and --disable-debugger,
                    ;; which are needed in combination for reliable non-
                    ;; interactive startup.
                    (pop-option)
                    (setf finally-quit t)
                    (setf disable-debugger t))
                   ((string= option "--end-toplevel-options")
                    (pop-option)
                    (return))
                   ((find option +runtime-options+ :test #'string=)
                    (startup-error "C runtime option ~a in the middle of Lisp options."
                                   option))
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
    (when *posix-argv*
      (setf (rest *posix-argv*) options))

    ;; Disable debugger before processing initialization files & co.
    (when disable-debugger
      (disable-debugger))

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
          (flet ((process-init-file (kind specified-pathname default-function)
                   (awhen (or specified-pathname (funcall default-function))
                     (with-open-file (stream (if specified-pathname
                                                 (parse-native-namestring it)
                                                 (pathname it))
                                             :if-does-not-exist nil)
                       (cond (stream
                              (sb-fasl::call-with-load-bindings
                               (lambda (stream kind) (load-as-source stream :context kind))
                               stream kind stream))
                             (specified-pathname
                              (cerror "Ignore missing init file"
                                      "The specified ~A file ~A was not found."
                                      kind specified-pathname)))))))
            (unless no-sysinit
              (process-init-file "sysinit" sysinit *sysinit-pathname-function*))
            (unless no-userinit
              (process-init-file "userinit" userinit *userinit-pathname-function*))
            (when finally-quit
              (push (list :quit) reversed-options))
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
        (exit ()
          :report "Exit SBCL (calling #'EXIT, killing the process)."
          :test (lambda (c) (declare (ignore c)) (not script))
          (/show0 "falling through to EXIT from pre-REPL RESTART-CASE")
          (exit :code 1))))

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
  "A function of two stream arguments IN and OUT for the toplevel REPL to
call: Return the next Lisp form to evaluate (possibly handling other magic --
like ACL-style keyword commands -- which precede the next Lisp form). The OUT
stream is there to support magic which requires issuing new prompts.")
(defvar *repl-prompt-fun* #'repl-prompt-fun
  "A function of one argument STREAM for the toplevel REPL to call: Prompt
the user for input.")
(defvar *repl-fun-generator* (constantly #'repl-fun)
  "A function of no arguments returning a function of one argument NOPRINT
that provides the REPL for the system. Assumes that *STANDARD-INPUT* and
*STANDARD-OUTPUT* are set up.")

;;; toplevel helper
(defmacro with-rebound-io-syntax (&body body)
  `(%with-rebound-io-syntax (lambda () ,@body)))

(defun %with-rebound-io-syntax (function)
  (declare (type function function))
  (declare (dynamic-extent function))
  (let ((*package* *package*)
        (*print-array* *print-array*)
        (*print-base* *print-base*)
        (*print-case* *print-case*)
        (*print-circle* *print-circle*)
        (*print-escape* *print-escape*)
        (*print-gensym* *print-gensym*)
        (*print-length* *print-length*)
        (*print-level* *print-level*)
        (*print-lines* *print-lines*)
        (*print-miser-width* *print-miser-width*)
        (*print-pretty* *print-pretty*)
        (*print-radix* *print-radix*)
        (*print-readably* *print-readably*)
        (*print-right-margin* *print-right-margin*)
        (*read-base* *read-base*)
        (*read-default-float-format* *read-default-float-format*)
        (*read-eval* *read-eval*)
        (*read-suppress* *read-suppress*)
        (*readtable* *readtable*))
    (funcall function)))

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
        (loop
         (/show0 "about to set up restarts in TOPLEVEL-REPL")
         ;; CLHS recommends that there should always be an
         ;; ABORT restart; we have this one here, and one per
         ;; debugger level.
         (with-simple-restart
             (abort "~@<Exit debugger, returning to top level.~@:>")
           (catch 'toplevel-catcher
             (funcall repl-fun noprint)
             (critically-unreachable "after REPL"))))))))

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
        (exit)
        form)))

(defun repl-fun (noprint)
  (/show0 "entering REPL")
  (loop
   (unwind-protect
        (progn
          (scrub-control-stack)
          (sb-thread:get-foreground)
          (unless noprint
            (flush-standard-output-streams)
            (funcall *repl-prompt-fun* *standard-output*)
            ;; (Should *REPL-PROMPT-FUN* be responsible for doing its own
            ;; FORCE-OUTPUT? I can't imagine a valid reason for it not to
            ;; be done here, so leaving it up to *REPL-PROMPT-FUN* seems
            ;; odd. But maybe there *is* a valid reason in some
            ;; circumstances? perhaps some deadlock issue when being driven
            ;; by another process or something...)
            (force-output *standard-output*)
            (let ((real (maybe-resolve-synonym-stream *standard-output*)))
              ;; Because by default *standard-output* is not
              ;; *terminal-io* but STDOUT the column is not reset
              ;; after pressing enter. Reduce confusion by resetting
              ;; the column to 0
              (when (fd-stream-p real)
                (setf (fd-stream-output-column real) 0))))
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
  (%primitive sb-c:halt))


;;;; Examples of why NEVER NEVER NEVER to call TERMINATE-THREAD in production.

#|
Example 1. Stuck somewhere in free() and in JOIN-THREAD.
[stuck at https://github.com/google/tcmalloc/blob/master/tcmalloc/cpu_cache.cc#L204]
  Thread "A":
    0x98e697c [AbslInternalSpinLockDelay]
    0x9898287 [tcmalloc::CPUCache::UpdateCapacity(int, unsigned long, unsigned long, bool, tcmalloc::CPUCache::ObjectClass*, unsigned long*)]
    0x9898b38 [tcmalloc::CPUCache::Overflow(void*, unsigned long, int)]
    0x9891f39 [(anonymous namespace)::do_free_no_hooks(void*)]
    0x988cea1 [MallocBlock::ProcessFreeQueue(MallocBlock*, unsigned long, int)]
    0x26f490a [CFFI::FOREIGN-ARRAY-FREE]
  Thread "main thread":
    0x9877f23 [futex_wait]
    0x32f32c8 [(FLET SB-UNIX::BODY :IN SB-THREAD::FUTEX-WAIT)]
    0x32f301f [SB-THREAD::%%WAIT-FOR-MUTEX]
    0x2f97f5d [SB-THREAD::%WAIT-FOR-MUTEX]
    0x2f287be [SB-THREAD::JOIN-THREAD]

Example 2. Stuck somewhere else in free() and in JOIN-THREAD.
  Thread "A":
    0x7f657d1a7cb9 [syscall]
    0x98e697c [AbslInternalSpinLockDelay]
    0x995e9e7 [absl::base_internal::SpinLock::SlowLock()]
    0x988d63b [MallocBlock::CheckAndClear(int)]
    0x9a8511f [__libc_free]
    0x26f490a [CFFI::FOREIGN-ARRAY-FREE]
  Thread "main thread":
    0x9877f23 [futex_wait]
    0x32f32c8 [(FLET SB-UNIX::BODY :IN SB-THREAD::FUTEX-WAIT)]
    0x32f301f [SB-THREAD::%%WAIT-FOR-MUTEX]
    0x2f97f5d [SB-THREAD::%WAIT-FOR-MUTEX]
    0x2f287be [SB-THREAD::JOIN-THREAD]

Example 3. main thread stuck in a C library exit handler:
   0x98e697c [AbslInternalSpinLockDelay]
   0x995e9e7 [absl::base_internal::SpinLock::SlowLock()]
   0x98a43cf [tcmalloc::HugePageAwareAllocator::LockAndAlloc(unsigned long, bool*)]
   0x98a423f [tcmalloc::HugePageAwareAllocator::New(unsigned long)]
   0x9896ee8 [tcmalloc::CentralFreeList::Populate()]
   0x9896c69 [tcmalloc::CentralFreeList::RemoveRange(void**, int)]
   0x9897e74 [tcmalloc::CPUCache::Refill(int, unsigned long)]
   0x989221b [tcmalloc::dbg_do_malloc(unsigned long, unsigned long*)]
   0x9a85045 [__libc_malloc]
   0x7f25a8ad5772 [__run_exit_handlers]
   0x7f25a8ad57c5 [exit]
   0x2fa99a3 [SB-SYS::OS-EXIT]

Example 4. main thread stuck in a different C library exit handler:
   0x992ecfe [absl::synchronization_internal::Waiter::Wait(absl::synchronization_internal::KernelTimeout)]
   0x98e6865 [AbslInternalPerThreadSemWait]
   0x992fd4d [absl::Mutex::Block(absl::base_internal::PerThreadSynch*)]
   0x99324a0 [absl::Mutex::LockSlowLoop(absl::SynchWaitParams*, int)]
   0x9930b0e [absl::Mutex::LockSlow(absl::MuHowS const*, absl::Condition const*, int)]
   0x9930346 [absl::Mutex::Lock()]
   0x97ca6c0 [thread::local::internal::Var::~Var()]
   0x7fcf3fde0772 [__run_exit_handlers]
   0x7fcf3fde07c5 [exit]
   0x2fa99a3 [SB-SYS::OS-EXIT]

There are plenty more where those came from.
|#
