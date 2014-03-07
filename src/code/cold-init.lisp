;;;; cold initialization stuff, plus some other miscellaneous stuff
;;;; that we don't have any better place for

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;;; burning our ships behind us

;;; There's a fair amount of machinery which is needed only at cold
;;; init time, and should be discarded before freezing the final
;;; system. We discard it by uninterning the associated symbols.
;;; Rather than using a special table of symbols to be uninterned,
;;; which might be tedious to maintain, instead we use a hack:
;;; anything whose name matches a magic character pattern is
;;; uninterned.
;;;
;;; FIXME: Are there other tables that need to have entries removed?
;;; What about symbols of the form DEF!FOO?
(defun !unintern-init-only-stuff ()
  ;; FIXME: I can't see how this wouldn't converge in 1 pass to a fixed point
  (do ((any-changes? nil nil))
      (nil)
    (dolist (package (list-all-packages))
      (do-symbols (symbol package)
        (let ((name (symbol-name symbol)))
          (when (or (string= name "!" :end1 1 :end2 1)
                    (and (>= (length name) 2)
                         (string= name "*!" :end1 2 :end2 2)))
            (/show0 "uninterning cold-init-only symbol..")
            (/primitive-print name)
            (unintern symbol package)
            (setf any-changes? t)))))
    (unless any-changes?
      (return))))

;;;; putting ourselves out of our misery when things become too much to bear

(declaim (ftype (function (simple-string) nil) !cold-lose))
(defun !cold-lose (msg)
  (%primitive print msg)
  (%primitive print "too early in cold init to recover from errors")
  (%halt))

;;; last-ditch error reporting for things which should never happen
;;; and which, if they do happen, are sufficiently likely to torpedo
;;; the normal error-handling system that we want to bypass it
(declaim (ftype (function (simple-string) nil) critically-unreachable))
(defun critically-unreachable (where)
  (%primitive print "internal error: Control should never reach here, i.e.")
  (%primitive print where)
  (%halt))

;;;; !COLD-INIT

;;; a list of toplevel things set by GENESIS
(defvar *!reversed-cold-toplevels*)

;;; a SIMPLE-VECTOR set by GENESIS
(defvar *!load-time-values*)

(eval-when (:compile-toplevel :execute)
  ;; FIXME: Perhaps we should make SHOW-AND-CALL-AND-FMAKUNBOUND, too,
  ;; and use it for most of the cold-init functions. (Just be careful
  ;; not to use it for the COLD-INIT-OR-REINIT functions.)
  (sb!xc:defmacro show-and-call (name)
    `(progn
       (/primitive-print ,(symbol-name name))
       (,name))))

;;; called when a cold system starts up
(defun !cold-init ()
  #!+sb-doc "Give the world a shove and hope it spins."

  (/show0 "entering !COLD-INIT")

  ;; FIXME: It'd probably be cleaner to have most of the stuff here
  ;; handled by calls like !GC-COLD-INIT, !ERROR-COLD-INIT, and
  ;; !UNIX-COLD-INIT. And *TYPE-SYSTEM-INITIALIZED* could be changed to
  ;; *TYPE-SYSTEM-INITIALIZED-WHEN-BOUND* so that it doesn't need to
  ;; be explicitly set in order to be meaningful.
  (setf *after-gc-hooks* nil
        *in-without-gcing* nil
        *gc-inhibit* t
        *gc-pending* nil
        #!+sb-thread *stop-for-gc-pending* #!+sb-thread nil
        *allow-with-interrupts* t
        sb!unix::*unblock-deferrables-on-enabling-interrupts-p* nil
        *interrupts-enabled* t
        *interrupt-pending* nil
        #!+sb-thruption #!+sb-thruption *thruption-pending* nil
        *break-on-signals* nil
        *maximum-error-depth* 10
        *current-error-depth* 0
        *cold-init-complete-p* nil
        *type-system-initialized* nil
        sb!vm:*alloc-signal* nil
        sb!kernel::*gc-epoch* (cons nil nil))

  ;; I'm not sure where eval is first called, so I put this first.
  (show-and-call !eval-cold-init)
  (show-and-call !deadline-cold-init)
  (show-and-call thread-init-or-reinit)

  ;; Anyone might call RANDOM to initialize a hash value or something;
  ;; and there's nothing which needs to be initialized in order for
  ;; this to be initialized, so we initialize it right away.
  (show-and-call !random-cold-init)

  ;; Must be done before any non-opencoded array references are made.
  (show-and-call !hairy-data-vector-reffer-init)

  (show-and-call !character-database-cold-init)
  (show-and-call !character-name-database-cold-init)

  (show-and-call !early-package-cold-init)
  (show-and-call !package-cold-init)

  ;; All sorts of things need INFO and/or (SETF INFO).
  (/show0 "about to SHOW-AND-CALL !GLOBALDB-COLD-INIT")
  (show-and-call !globaldb-cold-init)

  ;; This needs to be done early, but needs to be after INFO is
  ;; initialized.
  (show-and-call !function-names-cold-init)
  (show-and-call !fdefn-cold-init)

  ;; Various toplevel forms call MAKE-ARRAY, which calls SUBTYPEP, so
  ;; the basic type machinery needs to be initialized before toplevel
  ;; forms run.
  (show-and-call !type-class-cold-init)
  (show-and-call !typedefs-cold-init)
  (show-and-call !world-lock-cold-init)
  (show-and-call !classes-cold-init)
  (show-and-call !early-type-cold-init)
  (show-and-call !late-type-cold-init)
  (show-and-call !alien-type-cold-init)
  (show-and-call !target-type-cold-init)
  (show-and-call !vm-type-cold-init)
  ;; FIXME: It would be tidy to make sure that that these cold init
  ;; functions are called in the same relative order as the toplevel
  ;; forms of the corresponding source files.

  ;;(show-and-call !package-cold-init)
  (show-and-call !policy-cold-init-or-resanify)
  (/show0 "back from !POLICY-COLD-INIT-OR-RESANIFY")

  (show-and-call !constantp-cold-init)
  (show-and-call !early-proclaim-cold-init)

  ;; KLUDGE: Why are fixups mixed up with toplevel forms? Couldn't
  ;; fixups be done separately? Wouldn't that be clearer and better?
  ;; -- WHN 19991204
  (/show0 "doing cold toplevel forms and fixups")
  (/show0 "(LISTP *!REVERSED-COLD-TOPLEVELS*)=..")
  (/hexstr (if (listp *!reversed-cold-toplevels*) "true" "NIL"))
  (/show0 "about to calculate (LENGTH *!REVERSED-COLD-TOPLEVELS*)")
  (/show0 "(LENGTH *!REVERSED-COLD-TOPLEVELS*)=..")
  #!+sb-show (let ((r-c-tl-length (length *!reversed-cold-toplevels*)))
               (/show0 "(length calculated..)")
               (let ((hexstr (hexstr r-c-tl-length)))
                 (/show0 "(hexstr calculated..)")
                 (/primitive-print hexstr)))
  (let (#!+sb-show (index-in-cold-toplevels 0))
    #!+sb-show (declare (type fixnum index-in-cold-toplevels))

    (dolist (toplevel-thing (prog1
                                (nreverse *!reversed-cold-toplevels*)
                              ;; (Now that we've NREVERSEd it, it's
                              ;; somewhat scrambled, so keep anyone
                              ;; else from trying to get at it.)
                              (makunbound '*!reversed-cold-toplevels*)))
      #!+sb-show
      (when (zerop (mod index-in-cold-toplevels 1024))
        (/show0 "INDEX-IN-COLD-TOPLEVELS=..")
        (/hexstr index-in-cold-toplevels))
      #!+sb-show
      (setf index-in-cold-toplevels
            (the fixnum (1+ index-in-cold-toplevels)))
      (typecase toplevel-thing
        (function
         (funcall toplevel-thing))
        (cons
         (case (first toplevel-thing)
           (:load-time-value
            (setf (svref *!load-time-values* (third toplevel-thing))
                  (funcall (second toplevel-thing))))
           (:load-time-value-fixup
            (setf (sap-ref-word (int-sap (get-lisp-obj-address (second toplevel-thing)))
                                (third toplevel-thing))
                  (get-lisp-obj-address
                   (svref *!load-time-values* (fourth toplevel-thing)))))
           (t
            (!cold-lose "bogus fixup code in *!REVERSED-COLD-TOPLEVELS*"))))
        (t (!cold-lose "bogus function in *!REVERSED-COLD-TOPLEVELS*")))))
  (/show0 "done with loop over cold toplevel forms and fixups")

  ;; Set sane values again, so that the user sees sane values instead
  ;; of whatever is left over from the last DECLAIM/PROCLAIM.
  (show-and-call !policy-cold-init-or-resanify)

  ;; Only do this after toplevel forms have run, 'cause that's where
  ;; DEFTYPEs are.
  (setf *type-system-initialized* t)

  ;; now that the type system is definitely initialized, fixup UNKNOWN
  ;; types that have crept in.
  (show-and-call !fixup-type-cold-init)
  ;; run the PROCLAIMs.
  (show-and-call !late-proclaim-cold-init)

  (show-and-call os-cold-init-or-reinit)
  (show-and-call !pathname-cold-init)
  (show-and-call !debug-info-cold-init)

  (show-and-call stream-cold-init-or-reset)
  (show-and-call !loader-cold-init)
  (show-and-call !foreign-cold-init)
  #!-(and win32 (not sb-thread))
  (show-and-call signal-cold-init-or-reinit)
  (/show0 "enabling internal errors")
  (setf (sb!alien:extern-alien "internal_errors_enabled" boolean) t)

  (show-and-call float-cold-init-or-reinit)

  (show-and-call !class-finalize)

  ;; The reader and printer are initialized very late, so that they
  ;; can do hairy things like invoking the compiler as part of their
  ;; initialization.
  (let ((*readtable* (make-readtable)))
    (show-and-call !reader-cold-init)
    (show-and-call !sharpm-cold-init)
    (show-and-call !backq-cold-init)
    ;; The *STANDARD-READTABLE* is assigned at last because the above
    ;; functions would operate on the standard readtable otherwise---
    ;; which would result in an error.
    (setf *standard-readtable* *readtable*))
  (setf *readtable* (copy-readtable *standard-readtable*))
  (setf sb!debug:*debug-readtable* (copy-readtable *standard-readtable*))
  (sb!pretty:!pprint-cold-init)

  ;; the ANSI-specified initial value of *PACKAGE*
  (setf *package* (find-package "COMMON-LISP-USER"))

  (/show0 "done initializing, setting *COLD-INIT-COMPLETE-P*")
  (setf *cold-init-complete-p* t)

  ; hppa heap is segmented, lisp and c uses a stub to call eachother
  #!+hpux (sb!sys:%primitive sb!vm::setup-return-from-lisp-stub)
  ;; The system is finally ready for GC.
  (/show0 "enabling GC")
  (setq *gc-inhibit* nil)
  (/show0 "doing first GC")
  (gc :full t)
  (/show0 "back from first GC")

  ;; The show is on.
  (terpri)
  (/show0 "going into toplevel loop")
  (handling-end-of-the-world
    (toplevel-init)
    (critically-unreachable "after TOPLEVEL-INIT")))

(define-deprecated-function :early "1.0.56.55" quit (exit sb!thread:abort-thread)
    (&key recklessly-p (unix-status 0))
  (if (or recklessly-p (sb!thread:main-thread-p))
      (exit :code unix-status :abort recklessly-p)
      (sb!thread:abort-thread))
  (critically-unreachable "after trying to die in QUIT"))

(declaim (ftype (sfunction (&key (:code (or null exit-code))
                                 (:timeout (or null real))
                                 (:abort t))
                           nil)
                exit))
(defun exit (&key code abort (timeout *exit-timeout*))
  #!+sb-doc
  "Terminates the process, causing SBCL to exit with CODE. CODE
defaults to 0 when ABORT is false, and 1 when it is true.

When ABORT is false (the default), current thread is first unwound,
*EXIT-HOOKS* are run, other threads are terminated, and standard
output streams are flushed before SBCL calls exit(3) -- at which point
atexit(3) functions will run. If multiple threads call EXIT with ABORT
being false, the first one to call it will complete the protocol.

When ABORT is true, SBCL exits immediately by calling _exit(2) without
unwinding stack, or calling exit hooks. Note that _exit(2) does not
call atexit(3) functions unlike exit(3).

Recursive calls to EXIT cause EXIT to behave as it ABORT was true.

TIMEOUT controls waiting for other threads to terminate when ABORT is
NIL. Once current thread has been unwound and *EXIT-HOOKS* have been
run, spawning new threads is prevented and all other threads are
terminated by calling TERMINATE-THREAD on them. The system then waits
for them to finish using JOIN-THREAD, waiting at most a total TIMEOUT
seconds for all threads to join. Those threads that do not finish
in time are simply ignored while the exit protocol continues. TIMEOUT
defaults to *EXIT-TIMEOUT*, which in turn defaults to 60. TIMEOUT NIL
means to wait indefinitely.

Note that TIMEOUT applies only to JOIN-THREAD, not *EXIT-HOOKS*. Since
TERMINATE-THREAD is asynchronous, getting multithreaded application
termination with complex cleanups right using it can be tricky. To
perform an orderly synchronous shutdown use an exit hook instead of
relying on implicit thread termination.

Consequences are unspecified if serious conditions occur during EXIT
excepting errors from *EXIT-HOOKS*, which cause warnings and stop
execution of the hook that signaled, but otherwise allow the exit
process to continue normally."
  (if (or abort *exit-in-process*)
      (os-exit (or code 1) :abort t)
      (let ((code (or code 0)))
        (with-deadline (:seconds nil :override t)
          (sb!thread:grab-mutex *exit-lock*))
        (setf *exit-in-process* code
              *exit-timeout* timeout)
        (throw '%end-of-the-world t)))
  (critically-unreachable "After trying to die in EXIT."))

;;;; initialization functions

(defun thread-init-or-reinit ()
  (sb!thread::init-initial-thread)
  (sb!thread::init-job-control)
  (sb!thread::get-foreground))

(defun reinit ()
  #!+win32
  (setf sb!win32::*ansi-codepage* nil)
  (setf *default-external-format* nil)
  (setf sb!alien::*default-c-string-external-format* nil)
  ;; WITHOUT-GCING implies WITHOUT-INTERRUPTS.
  (without-gcing
    ;; Initialize streams first, so that any errors can be printed later
    (stream-reinit t)
    (os-cold-init-or-reinit)
    (thread-init-or-reinit)
    #!-(and win32 (not sb-thread))
    (signal-cold-init-or-reinit)
    (setf (sb!alien:extern-alien "internal_errors_enabled" boolean) t)
    (float-cold-init-or-reinit))
  (gc-reinit)
  (foreign-reinit)
  (time-reinit)
  ;; If the debugger was disabled in the saved core, we need to
  ;; re-disable ldb again.
  (when (eq *invoke-debugger-hook* 'sb!debug::debugger-disabled-hook)
    (sb!debug::disable-debugger))
  (call-hooks "initialization" *init-hooks*))

;;;; some support for any hapless wretches who end up debugging cold
;;;; init code

;;; Decode THING into hexadecimal notation using only machinery
;;; available early in cold init.
#!+sb-show
(defun hexstr (thing)
  (/noshow0 "entering HEXSTR")
  (let* ((addr (get-lisp-obj-address thing))
         (nchars (* sb!vm:n-word-bytes 2))
         (str (make-string (+ nchars 2) :element-type 'base-char)))
    (/noshow0 "ADDR and STR calculated")
    (setf (char str 0) #\0
          (char str 1) #\x)
    (/noshow0 "CHARs 0 and 1 set")
    (dotimes (i nchars)
      (/noshow0 "at head of DOTIMES loop")
      (let* ((nibble (ldb (byte 4 0) addr))
             (chr (char "0123456789abcdef" nibble)))
        (declare (type (unsigned-byte 4) nibble)
                 (base-char chr))
        (/noshow0 "NIBBLE and CHR calculated")
        (setf (char str (- (1+ nchars) i)) chr
              addr (ash addr -4))))
    str))

#!+sb-show
(defun cold-print (x)
  (labels ((%cold-print (obj depthoid)
             (if (> depthoid 4)
                 (sb!sys:%primitive print "...")
                 (typecase obj
                   (simple-string
                    (sb!sys:%primitive print obj))
                   (symbol
                    (sb!sys:%primitive print (symbol-name obj)))
                   (cons
                    (sb!sys:%primitive print "cons:")
                    (let ((d (1+ depthoid)))
                      (%cold-print (car obj) d)
                      (%cold-print (cdr obj) d)))
                   (t
                    (sb!sys:%primitive print (hexstr obj)))))))
    (%cold-print x 0))
  (values))
