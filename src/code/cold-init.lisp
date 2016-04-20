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
;;; Additionally, you can specify an arbitrary way to destroy
;;; random bootstrap stuff on per-package basis.
(defun !unintern-init-only-stuff ()
  (dolist (package (list-all-packages))
    (awhen (find-symbol "UNINTERN-INIT-ONLY-STUFF" package)
      (format t "~&Calling ~/sb-impl::print-symbol-with-prefix/~%" it)
      (funcall it)
      (unintern it package)))
  (flet ((uninternable-p (symbol)
           (let ((name (symbol-name symbol)))
             (or (and (>= (length name) 1) (char= (char name 0) #\!))
                 (and (>= (length name) 2) (string= name "*!" :end1 2))
                 (memq symbol
                       '(sb!c::sb!pcl sb!c::sb!impl sb!c::sb!kernel
                         sb!c::sb!c sb!c::sb!int))))))
    ;; A structure constructor name, in particular !MAKE-SAETP,
    ;; can't be uninterned if referenced by a defstruct-description.
    ;; So loop over all structure classoids and clobber any
    ;; symbol that should be uninternable.
    (maphash (lambda (classoid layout)
               (when (structure-classoid-p classoid)
                 (let ((dd (layout-info layout)))
                   (setf (dd-constructors dd)
                         (delete-if (lambda (x)
                                      (and (consp x) (uninternable-p (car x))))
                                    (dd-constructors dd))))))
             (classoid-subclasses (find-classoid t)))
    ;; Todo: perform one pass, then a full GC, then a final pass to confirm
    ;; it worked. It shoud be an error if any uninternable symbols remain,
    ;; but at present there are about 13 other "!" symbols with referers.
    (with-package-iterator (iter (list-all-packages) :internal :external)
      (loop (multiple-value-bind (winp symbol accessibility package) (iter)
              (declare (ignore accessibility))
              (unless winp
                (return))
              (when (uninternable-p symbol)
                ;; Uninternable symbols which are referenced by other stuff
                ;; can't disappear from the image, but we don't need to preserve
                ;; their functions, so FMAKUNBOUND them. This doesn't have
                ;; the intended effect if the function shares a code-component
                ;; with non-cold-init lambdas. Though the cold-init function is
                ;; never called post-build, it is not discarded. Also, I suspect
                ;; that the following loop should print nothing, but it does:
#|
                (sb-vm::map-allocated-objects
                  (lambda (obj type size)
                    (declare (ignore size))
                    (when (= type sb-vm:code-header-widetag)
                      (let ((name (sb-c::debug-info-name
                                   (sb-kernel:%code-debug-info obj))))
                        (when (and (stringp name) (search "COLD-INIT-FORMS" name))
                          (print obj)))))
                  :dynamic)
|#
                (fmakunbound symbol)
                (unintern symbol package)))))))

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
(defvar *!cold-toplevels*)                 ; except for DEFUNs and SETF macros
(defvar *!cold-setf-macros*)               ; just SETF macros
(defvar *!cold-defconstants*)              ; just DEFCONSTANT-EQXs
(defvar *!cold-defuns*)                    ; just DEFUNs

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

(defun !encapsulate-stuff-for-cold-init (&aux names)
  (flet ((encapsulate-1 (name handler)
           (encapsulate name '!cold-init handler)
           (push name names)))
    (encapsulate-1 '%failed-aver
                   (lambda (f expr)
                     ;; output the message before signaling error,
                     ;; as it may be this is too early in the cold init.
                     (fresh-line)
                     (write-line "failed AVER:")
                     (write expr)
                     (terpri)
                     (funcall f expr)))

    (encapsulate-1
     'find-package
     (lambda (f designator)
       (cond ((packagep designator) designator)
             (t (funcall f (let ((s (string designator)))
                             (if (eql (mismatch s "SB!") 3)
                                 (concatenate 'string "SB-" (subseq s 3))
                                 s)))))))

    ;; Wrap thing-defining-functions that style-warn sufficiently early
    ;; that HANDLER-BIND can't be used to suppress the warning
    ;; (since condition classoids don't exist yet).
    (flet ((warning-suppressor (signaler)
             (lambda (f &rest args)
               (encapsulate signaler '!cold-init (constantly nil))
               (apply f args)
               (unencapsulate signaler '!cold-init)))) ; Restore it.
      ;; %DEFUN complains about everything being redefined
      (encapsulate-1 '%defun (warning-suppressor 'warn))
      ;; %DEFCONSTANT complains about all named types because of earmuffs.
      (encapsulate-1 'sb!c::%defconstant (warning-suppressor 'style-warn))
      ;; %DEFSETF ',FN warns when #'(SETF fn) also has a function binding.
      (encapsulate-1 '%defsetf (warning-suppressor 'style-warn))))
  names)

(defmacro !with-init-wrappers (&rest forms)
  `(let ((wrapped-functions (!encapsulate-stuff-for-cold-init)))
     ,@forms
     (dolist (f wrapped-functions) (unencapsulate f '!cold-init))))

;;; called when a cold system starts up
(defun !cold-init ()
  #!+sb-doc "Give the world a shove and hope it spins."

  #!+sb-show
  (sb!int::cannot-/show "Test of CANNOT-/SHOW [don't worry - this is expected]")
  (/show0 "entering !COLD-INIT")
  (setq *readtable* (make-readtable)
        *previous-case* nil
        *previous-readtable-case* nil
        *print-length* 6 *print-level* 3)
  #!-win32
  (write-string "COLD-INIT... "
                (setq *error-output* (!make-cold-stderr-stream)
                      *standard-output* *error-output*
                      *trace-output* *error-output*))

  ;; Assert that FBOUNDP doesn't choke when its answer is NIL.
  ;; It was fine if T because in that case the legality of the arg is certain.
  ;; And be extra paranoid - ensure that it really gets called.
  (locally (declare (notinline fboundp)) (fboundp '(setf !zzzzzz)))

  ;; Putting data in a synchronized hashtable (*PACKAGE-NAMES*)
  ;; requires that the main thread be properly initialized.
  (show-and-call thread-init-or-reinit)
  ;; Printing of symbols requires that packages be filled in, because
  ;; OUTPUT-SYMBOL calls FIND-SYMBOL to determine accessibility.
  (show-and-call !package-cold-init)
  ;; Fill in the printer's character attribute tables now.
  ;; If Genesis could write constant arrays into a target core,
  ;; that would be nice, and would tidy up some other things too.
  (show-and-call !printer-cold-init)
  #!-win32
  (progn (prin1 `(package = ,(package-name *package*)))
         (terpri))

  ;; *RAW-SLOT-DATA* is essentially a compile-time constant
  ;; but isn't dumpable as such because it has functions in it.
  (show-and-call sb!kernel::!raw-slot-data-init)

  ;; Anyone might call RANDOM to initialize a hash value or something;
  ;; and there's nothing which needs to be initialized in order for
  ;; this to be initialized, so we initialize it right away.
  (show-and-call !random-cold-init)

  ;; Must be done before any non-opencoded array references are made.
  (show-and-call !hairy-data-vector-reffer-init)

  (show-and-call !character-database-cold-init)
  (show-and-call !character-name-database-cold-init)
  (show-and-call sb!unicode::!unicode-properties-cold-init)

  ;; All sorts of things need INFO and/or (SETF INFO).
  (/show0 "about to SHOW-AND-CALL !GLOBALDB-COLD-INIT")
  (show-and-call !globaldb-cold-init)

  ;; Various toplevel forms call MAKE-ARRAY, which calls SUBTYPEP, so
  ;; the basic type machinery needs to be initialized before toplevel
  ;; forms run.
  (show-and-call !type-class-cold-init)
  (!with-init-wrappers (show-and-call sb!kernel::!primordial-type-cold-init))
  (show-and-call !world-lock-cold-init)
  (show-and-call !classes-cold-init)
  (show-and-call !early-type-cold-init)
  (show-and-call !late-type-cold-init)
  (show-and-call !alien-type-cold-init)
  (show-and-call !target-type-cold-init)
  ;; FIXME: It would be tidy to make sure that that these cold init
  ;; functions are called in the same relative order as the toplevel
  ;; forms of the corresponding source files.

  (show-and-call !policy-cold-init-or-resanify)
  (/show0 "back from !POLICY-COLD-INIT-OR-RESANIFY")

  ;; Must be done before toplevel forms are invoked
  ;; because a toplevel defstruct will need to add itself
  ;; to the subclasses of STRUCTURE-OBJECT.
  (show-and-call sb!kernel::!set-up-structure-object-class)

  (dolist (x *!cold-defconstants*)
    (destructuring-bind (name source-loc &optional docstring) x
      (setf (info :variable :kind name) :constant)
      (when source-loc (setf (info :source-location :constant name) source-loc))
      (when docstring (setf (fdocumentation name 'variable) docstring))))
  (!with-init-wrappers
   (dolist (x *!cold-defuns*)
     (destructuring-bind (name . inline-expansion) x
       (%defun name (fdefinition name) nil inline-expansion))))

  ;; KLUDGE: Why are fixups mixed up with toplevel forms? Couldn't
  ;; fixups be done separately? Wouldn't that be clearer and better?
  ;; -- WHN 19991204
  (/show0 "doing cold toplevel forms and fixups")
  #!-win32
  (progn (write `("Length(TLFs)= " ,(length *!cold-toplevels*)))
         (terpri))

  (!with-init-wrappers
    (loop for index-in-cold-toplevels from 0
          for toplevel-thing in (prog1 *!cold-toplevels*
                                 (makunbound '*!cold-toplevels*))
        do
      #!+sb-show
      (when (zerop (mod index-in-cold-toplevels 1024))
        (/show0 "INDEX-IN-COLD-TOPLEVELS=..")
        (/hexstr index-in-cold-toplevels))
      (typecase toplevel-thing
        (function
         (funcall toplevel-thing))
        ((cons (eql :load-time-value))
            (setf (svref *!load-time-values* (third toplevel-thing))
                  (funcall (second toplevel-thing))))
        ((cons (eql :load-time-value-fixup))
            (setf (sap-ref-word (int-sap (get-lisp-obj-address (second toplevel-thing)))
                                (third toplevel-thing))
                  (get-lisp-obj-address
                   (svref *!load-time-values* (fourth toplevel-thing)))))
        ((cons (eql defstruct))
         (apply 'sb!kernel::%defstruct (cdr toplevel-thing)))
        (t
         (!cold-lose "bogus operation in *!COLD-TOPLEVELS*")))))
  (/show0 "done with loop over cold toplevel forms and fixups")

  (show-and-call time-reinit)

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
  (setq *print-level* nil *print-length* nil) ; restore defaults

  ;; the ANSI-specified initial value of *PACKAGE*
  (setf *package* (find-package "COMMON-LISP-USER"))

  ;; Enable normal (post-cold-init) behavior of INFINITE-ERROR-PROTECT.
  (setf sb!kernel::*maximum-error-depth* 10)
  (/show0 "enabling internal errors")
  (setf (extern-alien "internal_errors_enabled" int) 1)


  ; hppa heap is segmented, lisp and c uses a stub to call eachother
  #!+hpux (%primitive sb!vm::setup-return-from-lisp-stub)
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
    (setf (extern-alien "internal_errors_enabled" int) 1)
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

;; But: you almost never need this. Just use WRITE in all its glory.
#!+sb-show
(defun cold-print (x)
  (labels ((%cold-print (obj depthoid)
             (if (> depthoid 4)
                 (%primitive print "...")
                 (typecase obj
                   (simple-string
                    (%primitive print obj))
                   (symbol
                    (%primitive print (symbol-name obj)))
                   (cons
                    (%primitive print "cons:")
                    (let ((d (1+ depthoid)))
                      (%cold-print (car obj) d)
                      (%cold-print (cdr obj) d)))
                   (t
                    (%primitive print (hexstr obj)))))))
    (%cold-print x 0))
  (values))

(in-package "SB!INT")
(defun unintern-init-only-stuff ()
  (let ((this-package (find-package "SB-INT")))
    ;; For some reason uninterning these:
    ;;    DEF!TYPE DEF!CONSTANT DEF!STRUCT
    ;; does not work, they stick around as uninterned symbols.
    ;; Some other macros must expand into them. Ugh.
    (dolist (s '(defenum defun-cached with-globaldb-name
                 .
                 #!+sb-show ()
                 #!-sb-show (/hexstr /nohexstr /noshow /noshow0 /noxhow
                             /primitive-print /show /show0 /xhow)))
      (unintern s this-package))))
