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

(in-package "SB-IMPL")

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
(defvar *!cold-toplevels*)

;;; a SIMPLE-VECTOR set by GENESIS
(defvar *!load-time-values*)

;; FIXME: Perhaps we should make SHOW-AND-CALL-AND-FMAKUNBOUND, too,
;; and use it for most of the cold-init functions. (Just be careful
;; not to use it for the COLD-INIT-OR-REINIT functions.)
(defmacro show-and-call (name)
  `(progn
     (/show "Calling" ,(symbol-name name))
     (,name)))

(defun !c-runtime-noinform-p () (/= (extern-alien "lisp_startup_options" char) 0))

;;; Allows the SIGNAL function to be called early.
(defun !signal-function-cold-init ()
  #+sb-devel
  (progn
    (setq *break-on-signals* nil)
    (setq sb-kernel::*current-error-depth* 0))
  (setq sb-debug:*stack-top-hint* nil))

(defun !printer-control-init ()
  (setq *print-readably* nil
        *print-escape* t
        *print-pretty* nil
        *print-base* 10
        *print-radix* nil
        *print-vector-length* nil
        *print-circle* nil
        *print-case* :upcase
        *print-array* t
        *print-gensym* t
        *print-lines* nil
        *print-right-margin* nil
        *print-miser-width* nil
        *print-pprint-dispatch* (sb-pretty::make-pprint-dispatch-table #() nil nil)
        *suppress-print-errors* nil
        *current-level-in-print* 0))

;;; Create a stream that works early.
(defun !make-cold-stderr-stream ()
  (let ((stderr
          #-win32 2
          #+win32 (sb-win32::get-std-handle-or-null sb-win32::+std-error-handle+))
        (buf (make-string 1 :element-type 'base-char :initial-element #\Space)))
    (%make-fd-stream
     :cout (lambda (stream ch)
             (declare (ignore stream))
             (setf (char buf 0) ch)
             (sb-unix:unix-write stderr buf 0 1)
             ch)
     :sout (lambda (stream string start end)
             (declare (ignore stream))
             (flet ((out (s start len)
                      (when (plusp len)
                        (setf (char buf 0) (char s (+ start len -1))))
                      (sb-unix:unix-write stderr s start len)))
               (if (typep string 'simple-base-string)
                   (out string start (- end start))
                   (let ((n (- end start)))
                     ;; will croak if there is any non-BASE-CHAR in the string
                     (out (replace (make-array n :element-type 'base-char)
                                   string :start2 start) 0 n)))))
     :misc (lambda (stream operation arg1)
             (declare (ignore stream arg1))
             (stream-misc-case (operation :default nil)
               (:charpos ; impart just enough smarts to make FRESH-LINE dtrt
                (if (eql (char buf 0) #\newline) 0 1)))))))

(defun !xc-sanity-checks ()
  ;; Verify on startup that some constants were dumped reflecting the
  ;; correct action of our vanilla-host-compatible functions.  For
  ;; now, just SXHASH is checked.

  ;; Parallelized build doesn't get the full set of data because the
  ;; side effect of data recording when invoking compile-time
  ;; functions aren't propagated back to process that forked the
  ;; children doing the grunt work.
  (let ((list (with-open-file (stream "output/sxhash-calls.lisp-expr"
                                      :if-does-not-exist nil)
                (when stream
                  (let ((*package* (find-package "SB-KERNEL"))) (read stream))))))
    (dolist (item list)
      (destructuring-bind (object hash) item
        (let ((calc (sxhash object)))
          (unless (= hash calc)
            (error "cross-compiler SXHASH failure on ~S: ~X vs ~X" object hash calc)))))
    (when list
      (format t "~&cross-compiler SXHASH tests passed: ~D cases~%" (length list)))))

;;; called when a cold system starts up
(defun !cold-init ()
  "Give the world a shove and hope it spins."

  (/show0 "entering !COLD-INIT")
  #+sb-show (setq */show* t)
  (setq sb-vm::*immobile-codeblob-tree* nil
        sb-vm::*dynspace-codeblob-tree* nil)
  (setq sb-kernel::*defstruct-hooks* '(sb-kernel::!bootstrap-defstruct-hook)
        sb-kernel::*struct-accesss-fragments-delayed* nil)
  (let ((stream (!make-cold-stderr-stream)))
    (setq *error-output* stream
          *standard-output* stream
          *trace-output* stream))
  (show-and-call !signal-function-cold-init)
  (show-and-call !printer-control-init) ; needed before first instance of FORMAT or WRITE-STRING
  (setq sb-unix::*unblock-deferrables-on-enabling-interrupts-p* nil) ; needed by LOAD-LAYOUT called by CLASSES-INIT
  (setq *print-length* 6
        *print-level* 3)
  (/show "testing '/SHOW" *print-length* *print-level*) ; show anything
  ;; This allows FORMAT to work, and can go as early needed for
  ;; debugging.
  (show-and-call !format-cold-init)
  (unless (!c-runtime-noinform-p)
    (write-string "COLD-INIT... "))

  ;; Anyone might call RANDOM to initialize a hash value or something;
  ;; and there's nothing which needs to be initialized in order for
  ;; this to be initialized, so we initialize it right away.
  (show-and-call !random-cold-init)

  ;; We can't do such much as a simple PROCLAIM without this global
  ;; hash-table (because of WARN-IF-INLINE-FAILED/PROCLAIM)
  (setf sb-c::*emitted-full-calls* (make-hash-table :test 'equal :synchronized t))

  ;; All sorts of things need INFO and/or (SETF INFO).
  (/show0 "about to SHOW-AND-CALL !GLOBALDB-COLD-INIT")
  (show-and-call !globaldb-cold-init)
  (show-and-call !function-names-init)
  (show-and-call !pathname-cold-init)

  ;; And now *CURRENT-THREAD*
  (sb-thread::init-main-thread)

  (show-and-call !hash-table-cold-init)

  ;; not sure why this is needed on some architectures. Dark magic.
  #-linkage-space
  (setf (fdefn-fun (find-or-create-fdefn '%coerce-callable-for-call))
        #'%coerce-callable-to-fun)
  (show-and-call !loader-cold-init)
  #+linkage-space (show-and-call sb-vm::!initialize-lisp-linkage-table)
  ;; Assert that FBOUNDP doesn't choke when its answer is NIL.
  ;; It was fine if T because in that case the legality of the arg is certain.
  ;; And be extra paranoid - ensure that it really gets called.
  (locally
      (declare (notinline fboundp) (optimize safety)) ; is unsafely flushable
    (fboundp '(setf !zzzzzz)))

  ;; Printing of symbols requires that packages be filled in, because
  ;; OUTPUT-SYMBOL calls FIND-SYMBOL to determine accessibility. Also
  ;; allows IN-PACKAGE to work.
  (show-and-call !package-cold-init)

  ;; The readtable needs to be initialized for printing symbols early,
  ;; which is useful for debugging.
  (!readtable-cold-init)
  (write-string "Checking symbol printer: ")
  (write 't)
  (terpri)

  ;; *RAW-SLOT-DATA* is essentially a compile-time constant
  ;; but isn't dumpable as such because it has functions in it.
  (show-and-call sb-kernel::!raw-slot-data-init)

  ;; Must be done before any non-opencoded array references are made.
  (show-and-call sb-vm::!hairy-data-vector-reffer-init)

  ;; It's not unreasonable to call PARSE-INTEGER in cold-init, which demands that
  ;; WHITESPACE[1]P work properly, so it needs *STANDARD-READTABLE* established.
  (let ((*readtable* (make-readtable)))
    (show-and-call !reader-cold-init)
    ;; *STANDARD-READTABLE* is assigned last to avoid an error about altering it.
    (setf *standard-readtable* *readtable*))

  ;; Various toplevel forms call MAKE-ARRAY, which calls SUBTYPEP, so
  ;; the basic type machinery needs to be initialized before toplevel
  ;; forms run.
  (show-and-call !type-class-cold-init)
  (show-and-call !classes-cold-init)
  (show-and-call sb-kernel::!primordial-type-cold-init)

  (show-and-call !type-cold-init)
  (show-and-call !policy-cold-init-or-resanify)
  (/show0 "back from !POLICY-COLD-INIT-OR-RESANIFY")

  ;; Must be done before toplevel forms are invoked
  ;; because a toplevel defstruct will need to add itself
  ;; to the subclasses of STRUCTURE-OBJECT.
  (show-and-call sb-kernel::!set-up-structure-object-class)

  (unless (!c-runtime-noinform-p)
    (write `("Length(TLFs)=" ,(length *!cold-toplevels*)) :escape nil))

  (setq sb-pcl::*!docstrings* nil) ; needed before any documentation is set
  (setq sb-c::*queued-proclaims* nil) ; needed before any proclaims are run

  (/show0 "calling cold toplevel forms and fixups")
  (let ((*package* *package*)) ; rebind to self, as if by LOAD
    (dolist (toplevel-thing *!cold-toplevels*)
      (typecase toplevel-thing
        (function
         (funcall toplevel-thing))
        ((cons (eql :load-time-value))
         (setf (svref *!load-time-values* (third toplevel-thing))
               (funcall (second toplevel-thing))))
        ((cons (eql :load-time-value-fixup))
         (destructuring-bind (object index value) (cdr toplevel-thing)
           (let ((replacement (svref *!load-time-values* value)))
             (etypecase object
               (code-component
                (aver (unbound-marker-p (code-header-ref object index)))
                (setf (code-header-ref object index) replacement))
               (cons
                (aver (= index 0))
                (aver (unbound-marker-p (car object)))
                (rplaca object replacement))))))
        ((cons (eql :named-constant))
         (destructuring-bind (object index name) (cdr toplevel-thing)
           (aver (typep object 'code-component))
           (aver (unbound-marker-p (code-header-ref object index)))
           (sb-fasl::named-constant-set object index name)))
        ((cons (eql :begin-file))
         (unless (!c-runtime-noinform-p) (print (cdr toplevel-thing))))
        (t
         (!cold-lose "bogus operation in *!COLD-TOPLEVELS*")))))
  (/show0 "done with loop over cold toplevel forms and fixups")
  (unless (!c-runtime-noinform-p) (terpri))

  (makunbound '*!cold-toplevels*) ; so it gets GC'd

  ;; Need the static-space replica of the assembly routine jump vector
  ;; filled in, and the static space vector of static fdefns.
  ;; This matters only for code that gets compiled to dynamic space,
  ;; so it's OK that it occurs somewhat late in cold-init.
  #+x86-64 (sb-vm::validate-asm-routine-vector)

  #+win32 (show-and-call reinit-internal-real-time)

  ;; Set sane values again, so that the user sees sane values instead
  ;; of whatever is left over from the last DECLAIM/PROCLAIM.
  (show-and-call !policy-cold-init-or-resanify)

  ;; Only do this after toplevel forms have run, 'cause that's where
  ;; DEFTYPEs are.
  (setf *type-system-initialized* t)

  ;; now that the type system is definitely initialized, fixup UNKNOWN
  ;; types that have crept in.
  (show-and-call !fixup-type-cold-init)

  ;; We run through queued-up type and ftype proclaims that were made
  ;; before the type system was initialized, and (since it is now
  ;; initalized) reproclaim them.
  (loop for claim in sb-c::*queued-proclaims*
        do
        (when (eq (car claim) 'ftype)
          ;; Avoid warnings about mismatched types.
          (loop for name in (cddr claim)
                do (setf (info :function :where-from name) :assumed)))
        (proclaim claim))

  (makunbound 'sb-c::*queued-proclaims*)

  (show-and-call os-cold-init-or-reinit)
  (show-and-call !lpn-cold-init)

  (show-and-call stream-cold-init-or-reset)
  (/show "Enabled buffered streams")
  (show-and-call !foreign-cold-init)
  #-(and win32 (not sb-thread))
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
    (setf *standard-readtable* *readtable*))
  (setf *readtable* (copy-readtable *standard-readtable*))
  (setf sb-debug:*debug-readtable* (copy-readtable *standard-readtable*))
  (sb-pretty:!pprint-cold-init)
  (setq *print-level* nil *print-length* nil) ; restore defaults

  ;; Enable normal (post-cold-init) behavior of INFINITE-ERROR-PROTECT.
  (setf sb-kernel:*maximum-error-depth* 10)
  (/show0 "enabling internal errors")
  (setf (extern-alien "internal_errors_enabled" int) 1)

  (show-and-call sb-disassem::!compile-inst-printers)

  ;; Toggle some readonly bits
  (dovector (sc sb-c:*backend-sc-numbers*)
    (when sc
      (logically-readonlyize (sb-c::sc-move-funs sc))
      (logically-readonlyize (sb-c::sc-load-costs sc))
      (logically-readonlyize (sb-c::sc-move-vops sc))
      (logically-readonlyize (sb-c::sc-move-costs sc))))

  (show-and-call !xc-sanity-checks)

  ;; The system is finally ready for GC.
  (/show0 "enabling GC")
  (setq *gc-inhibit* nil)
  #+sb-thread (finalizer-thread-start)

  ;; The show is on.
  (/show0 "going into toplevel loop")
  (handling-end-of-the-world
    (toplevel-init)
    (critically-unreachable "after TOPLEVEL-INIT")))

(defun quit (&key recklessly-p (unix-status 0))
  "Calls (SB-EXT:EXIT :CODE UNIX-STATUS :ABORT RECKLESSLY-P),
see documentation for SB-EXT:EXIT."
  (exit :code unix-status :abort recklessly-p))

(declaim (ftype (sfunction (&key (:code (or null exit-code))
                                 (:timeout (or null real))
                                 (:abort t))
                           nil)
                exit))
(defun exit (&key code abort (timeout *exit-timeout*))
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

Recursive calls to EXIT cause EXIT to behave as if ABORT was true.

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
  (if (or abort *exit-in-progress*)
      (os-exit (or code 1) :abort t)
      (let ((code (or code 0)))
        (with-deadline (:seconds nil :override t)
          (sb-thread:grab-mutex *exit-lock*))
        (setf *exit-in-progress* code
              *exit-timeout* timeout)
        (throw '%end-of-the-world t)))
  (critically-unreachable "After trying to die in EXIT."))

;;;; initialization functions

(defun reinit (total)
  ;; WITHOUT-GCING implies WITHOUT-INTERRUPTS.
  (without-gcing
    ;; Until *CURRENT-THREAD* has been set, nothing the slightest bit complicated
    ;; can be called, as pretty much anything can assume that it is set.
    (when total ; newly started process, and not a failed save attempt
      (sb-thread::init-main-thread)
      #+x86-64 (sb-vm::validate-asm-routine-vector)
      (rebuild-package-vector))
    ;; Initialize streams next, so that any errors can be printed
    (stream-reinit t)
    (rebuild-pathname-cache)
    (os-cold-init-or-reinit)
    #-(and win32 (not sb-thread))
    (signal-cold-init-or-reinit)
    (setf (extern-alien "internal_errors_enabled" int) 1)
    (float-cold-init-or-reinit))
  (gc-reinit)
  (finalizers-reinit)
  (foreign-reinit)
  #+win32 (reinit-internal-real-time)
  ;; If the debugger was disabled in the saved core, we need to
  ;; re-disable ldb again.
  (when (eq *invoke-debugger-hook* 'sb-debug::debugger-disabled-hook)
    (sb-debug::disable-debugger))
  (call-hooks "initialization" *init-hooks*)
  #+sb-thread (finalizer-thread-start)
  (sb-vm::!setup-cpu-specific-routines))

;;;; some support for any hapless wretches who end up debugging cold
;;;; init code

;;; Decode THING into hexadecimal notation using only machinery
;;; available early in cold init.
#+sb-show
(defun hexstr (thing)
  (/noshow0 "entering HEXSTR")
  (let* ((addr (get-lisp-obj-address thing))
         (nchars (* sb-vm:n-word-bytes 2))
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
#+sb-show
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

(push
  '("SB-INT"
    defenum defun-cached with-globaldb-name def!type def!struct
    .
    #+sb-show ()
    #-sb-show (/noshow /noshow0 /show /show0))
  *!removable-symbols*)
