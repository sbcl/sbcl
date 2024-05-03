;;;; garbage collection and allocation-related code

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

;;;; DYNAMIC-USAGE and friends

(declaim (inline dynamic-usage))
(defun dynamic-usage ()
  (extern-alien "bytes_allocated" os-vm-size-t))

(defun static-space-usage ()
  (- (sap-int sb-vm:*static-space-free-pointer*) sb-vm:static-space-start))

(defun read-only-space-usage ()
  (- (sap-int sb-vm:*read-only-space-free-pointer*) sb-vm:read-only-space-start))

(defun control-stack-usage ()
  #-stack-grows-downward-not-upward
  (sap- (control-stack-pointer-sap) (descriptor-sap sb-vm:*control-stack-start*))
  #+stack-grows-downward-not-upward
  (sap- (descriptor-sap sb-vm:*control-stack-end*) (control-stack-pointer-sap)))

(defun binding-stack-usage ()
  (sap- (binding-stack-pointer-sap) (descriptor-sap sb-vm:*binding-stack-start*)))

;;;; GET-BYTES-CONSED

;;; the total number of bytes freed so far (including any freeing
;;; which goes on in PURIFY)
;;;
;;; (We save this so that we can calculate the total number of bytes
;;; ever allocated by adding this to the number of bytes currently
;;; allocated and never freed.)
(declaim (type unsigned-byte *n-bytes-freed-or-purified*))
(define-load-time-global *n-bytes-freed-or-purified* 0)
(defun gc-reinit ()
  (setq *gc-inhibit* nil)
  ;; This GC looks utterly unnecessary, but when I tried removing it,
  ;; I got heap exhaustions sooner than when I left it in.
  ;; I think the reason is that we never set the auto-trigger at all
  ;; unless a GC happens. Maybe what we mean here is "set auto trigger"
  ;; and we need a function in C to do that.
  (gc)
  (setf *n-bytes-freed-or-purified* 0
        *gc-run-time* 0
        *gc-real-time* 0))

(declaim (ftype (sfunction () unsigned-byte) get-bytes-consed))
(defun get-bytes-consed ()
  "Return the number of bytes consed since the program began. Typically
this result will be a consed bignum, so if you have an application (e.g.
profiling) which can't tolerate the overhead of consing bignums, you'll
probably want either to hack in at a lower level (as the code in the
SB-PROFILE package does), or to design a more microefficient interface
and submit it as a patch."
  (+ (dynamic-usage)
     *n-bytes-freed-or-purified*))

(defun primitive-object-size (object)
  "Return number of bytes of heap or stack directly consumed by OBJECT"
  (cond ((not (sb-vm:is-lisp-pointer (get-lisp-obj-address object))) 0)
        ((eq object nil) (ash sb-vm::sizeof-nil-in-words sb-vm:word-shift))
        ((simple-fun-p object) (code-object-size (fun-code-header object)))
        #-(or x86 x86-64 arm64 riscv) ((lra-p object) 1)
        (t
         (with-alien ((sizer (function unsigned unsigned) :extern "primitive_object_size"))
           (with-pinned-objects (object)
             (alien-funcall sizer (get-lisp-obj-address object)))))))

;;;; GC hooks

;;; N.B.: hooks need to be sufficiently uncomplicated as to be harmless,
;;; and should not expect any particular thread context.
(define-load-time-global *after-gc-hooks* nil
  "Called after each garbage collection, except for garbage collections
triggered during thread exits. In a multithreaded environment these hooks may
run in any thread.")


;;;; internal GC

(define-alien-routine collect-garbage int (last-gen int))

(define-alien-routine gc-stop-the-world void)
(define-alien-routine gc-start-the-world void)

(declaim (inline dynamic-space-size))
(defun dynamic-space-size ()
  "Size of the dynamic space in bytes."
  (extern-alien "dynamic_space_size" os-vm-size-t))

;;;; SUB-GC

;;; SUB-GC does a garbage collection.  This is called from three places:
;;; (1) The C runtime will call here when it detects that we've consed
;;;     enough to exceed the gc trigger threshold.  This is done in
;;;     alloc() for gencgc or interrupt_maybe_gc() for cheneygc
;;; (2) The user may request a collection using GC, below
;;; (3) At the end of a WITHOUT-GCING section, we are called if
;;;     *NEED-TO-COLLECT-GARBAGE* is true
;;;
;;; This is different from the behaviour in 0.7 and earlier: it no
;;; longer decides whether to GC based on thresholds.  If you call
;;; SUB-GC you will definitely get a GC either now or when the
;;; WITHOUT-GCING is over

;;; For GENCGC all generations < GEN will be GC'ed.

(defmacro try-acquire-gc-lock (&rest forms)
  #-sb-thread `(progn ,@forms t)
  #+sb-thread
  `(when (eql (alien-funcall (extern-alien "try_acquire_gc_lock" (function int))) 1)
     ,@forms
     (alien-funcall (extern-alien "release_gc_lock" (function void)))
     t))

(defmacro get-gc-real-time ()
  ;; 64-bit is just a call to clock-gettime and some math that won't ever cons
  #+64-bit '(get-internal-real-time)
  ;; 32-bit memoizes the result to try to avoid bignum consing,
  ;; which can fail during foreign thread creation.
  #-64-bit 0)

(defun sub-gc (gen)
  (cond (*gc-inhibit*
         (setf *gc-pending* t)
         nil)
        (t
         (flet ((perform-gc ()
                  (declare (sb-c::tlab :system)) ; for *gc-epoch*
                  ;; Called from WITHOUT-GCING and WITHOUT-INTERRUPTS
                  ;; after the world has been stopped, but it's an
                  ;; awkwardly long piece of code to nest so deeply.
                  (let ((old-usage (dynamic-usage))
                        (new-usage 0)
                        (start-time (get-internal-run-time))
                        (start-real-time (get-gc-real-time)))
                    (collect-garbage gen)
                    (setf *gc-epoch* (cons 0 0))
                    (let ((run-time (- (get-internal-run-time) start-time))
                          (real-time (- (get-gc-real-time) start-real-time)))
                      (incf *gc-real-time* real-time)
                      ;; KLUDGE: Sometimes we see the second getrusage() call
                      ;; return a smaller value than the first, which can
                      ;; lead to *GC-RUN-TIME* to going negative, which in
                      ;; turn is a type-error.
                      (when (plusp run-time)
                        (incf *gc-run-time* run-time)))
                    #+(and sb-thread sb-safepoint)
                    (setf *stop-for-gc-pending* nil)
                    (setf *gc-pending* nil
                          new-usage (dynamic-usage))
                    #+sb-thread
                    (aver (not *stop-for-gc-pending*))
                    (gc-start-the-world)
                    ;; In a multithreaded environment the other threads
                    ;; will see *n-b-f-o-p* change a little late, but
                    ;; that's OK.
                    ;; N.B. the outer without-gcing prevents this
                    ;; function from being entered, so no need for
                    ;; locking.
                    (let ((freed (- old-usage new-usage)))
                      ;; GENCGC occasionally reports negative here, but
                      ;; the current belief is that it is part of the
                      ;; normal order of things and not a bug.
                      (when (plusp freed)
                        (incf *n-bytes-freed-or-purified* freed))))))
           (declare (inline perform-gc))
           ;; Let's make sure we're not interrupted and that none of
           ;; the deadline or deadlock detection stuff triggers.
           (without-interrupts
               (let ((sb-impl::*deadline* nil)
                     (epoch *gc-epoch*))
                 (loop
                  ;; GCing must be done without-gcing to avoid
                  ;; recursive GC... but we can't block on
                  ;; *already-in-gc* inside without-gcing: that would
                  ;; cause a deadlock.
                  (without-gcing
                    ;; Try to grab that mutex.  On acquisition, stop
                    ;; the world from with the mutex held, and then
                    ;; execute the remainder of the GC: stopping the
                    ;; world with interrupts disabled is the mother of
                    ;; all critical sections.
                    (cond ((try-acquire-gc-lock
                             (unsafe-clear-roots gen)
                             (gc-stop-the-world))
                           ;; Success! GC.
                           (perform-gc)
                           ;; Return, but leave *gc-pending* as is: we
                           ;; did allocate a tiny bit after GCing.  In
                           ;; theory, this could lead to a long chain
                           ;; of tail-recursive (but not in explicit
                           ;; tail position) GCs, but that doesn't
                           ;; seem likely to happen too often... And
                           ;; the old code already suffered from this
                           ;; problem.
                           (return t))
                          (t
                           ;; Some other thread is trying to GC. Clear
                           ;; *gc-pending* (we already know we want a
                           ;; GC to happen) and either let
                           ;; without-gcing figure out that the world
                           ;; is stopping, or try again.
                           (setf *gc-pending* nil))))
                  ;; we just wanted a minor GC, and a GC has
                  ;; occurred. Leave, but don't execute after-gc
                  ;; hooks.
                  ;;
                  ;; Return a 0 for easy ternary logic in the C
                  ;; runtime.
                  (when (and (eql gen 0)
                             (neq epoch *gc-pending*))
                    (return 0)))))))))

#+sb-thread
(defun post-gc ()
  (sb-impl::finalizer-thread-notify 1) ; Tell it to perform post-GC hooks
  (alien-funcall (extern-alien "empty_thread_recyclebin" (function void)))
  nil)

#-sb-thread
(defun post-gc ()
  ;; Outside the mutex, interrupts may be enabled: these may cause
  ;; another GC. FIXME: it can potentially exceed maximum interrupt
  ;; nesting by triggering GCs.
  ;;
  ;; Can that be avoided by having the hooks run only
  ;; from the outermost SUB-GC? If the nested GCs happen in interrupt
  ;; handlers that's not enough.
  ;;
  ;; KLUDGE: Don't run the hooks in GC's if:
  ;;
  ;; A) this thread is dying or just born, so that user-code never runs with
  ;;    (thread-alive-p *current-thread*) => nil.
  ;;    The just-born case can happen with foreign threads that are unlucky
  ;;    enough to be elected to perform GC just as they begin executing
  ;;    ENTER-FOREIGN-CALLBACK. This definitely seems to happen with sb-safepoint.
  ;;    I'm not sure whether it can happen without sb-safepoint.
  ;;
  ;; B) interrupts are disabled somewhere up the call chain since we
  ;;    don't want to run user code in such a case.
  ;;
  ;; Condition (A) is tested in the C runtime at can_invoke_post_gc().
  ;; Condition (B) is tested here.
  ;; The long-term solution will be to keep a separate thread for
  ;; after-gc hooks.
  ;; Finalizers are in a separate thread (usually),
  ;; but it's not permissible to invoke CONDITION-NOTIFY from a
  ;; dying thread, so we still need the guard for that, but not
  ;; the guard for whether interupts are enabled.

    ;; Here's one reason that MAX_INTERRUPTS is as high as it is.
    ;; If the thread that performed GC runs post-GC hooks which cons enough to
    ;; cause another GC while in the hooks, then as soon as interrupts are allowed
    ;; again, a GC can be invoked "recursively" - while there is a maybe_gc() on
    ;; the C call stack. It's not even true that _this_ thread had to cons a ton -
    ;; any thread could have, causing this thread to hit the GC trigger which sets the
    ;; P-A interrupted bit which causes the interrupt instruction at the end of a
    ;; pseudo-atomic sequence to take a signal hit. So with interrupts eanbled,
    ;; we get back into the GC, which calls post-GC, which might cons ...
    ;; See the example at the bottom of src/code/final for a clear picture.
    ;; Logically, the finalizer existence test belongs in src/code/final,
    ;; but the flaw in that is we're not going to call that code until unmasking
    ;; interrupts, which is precisely the thing we need to NOT do if already
    ;; in post-GC code of any kind (be it finalizer or other).
  (when (and *allow-with-interrupts*
               (or (and sb-impl::*finalizers-triggered*
                        (not sb-impl::*in-a-finalizer*))
                   *after-gc-hooks*))
      (sb-thread::without-thread-waiting-for ()
        (with-interrupts
          (run-pending-finalizers)
          (call-hooks "after-GC" *after-gc-hooks* :on-error :warn))))
  nil)

;;; This is the user-advertised garbage collection function.
(defun gc (&key (full nil) (gen 0) &allow-other-keys)
  "Initiate a garbage collection.

The default is to initiate a nursery collection, which may in turn
trigger a collection of one or more older generations as well. If FULL
is true, all generations are collected. If GEN is provided, it can be
used to specify the oldest generation guaranteed to be collected."
  (let ((gen (if full sb-vm:+pseudo-static-generation+ gen)))
    (when (eq t (sub-gc gen))
      (post-gc))))

(define-alien-routine scrub-control-stack void)

(defglobal sb-unicode::*name->char-buffers* nil)
(defun unsafe-clear-roots (gen)
  (declare (ignorable gen))
  ;; KLUDGE: Do things in an attempt to get rid of extra roots. Unsafe
  ;; as having these cons more than we have space left leads to huge
  ;; badness.
  (scrub-control-stack)
  ;; Power cache of the bignum printer: drops overly large bignums and
  ;; removes duplicate entries.
  (scrub-power-cache)
  (setf sb-unicode::*name->char-buffers* nil)
  (setf sb-c::*phash-lambda-cache* nil)
  ;; Clear caches depending on the generation being collected.
  (cond ((eql 0 gen)
         ;; Drop strings because the hash is pointer-hash
         ;; but there is no automatic cache rehashing after GC.
         (sb-format::tokenize-control-string-cache-clear))
        ((eql 1 gen)
         (sb-format::tokenize-control-string-cache-clear))
        (t
         (sb-thread:with-recursive-lock ((hash-table-lock sb-di::*uncompacted-fun-maps*)
                                         :wait-p nil)
           (clrhash sb-di::*uncompacted-fun-maps*))
         (sb-thread:with-recursive-lock ((hash-table-lock sb-di::*compiled-debug-funs*)
                                         :wait-p nil)
           (clrhash sb-di::*compiled-debug-funs*))

         (drop-all-hash-caches))))

;;;; auxiliary functions

(defun bytes-consed-between-gcs ()
  "The amount of memory that will be allocated before the next garbage
collection is initiated. This can be set with SETF.

On GENCGC platforms this is the nursery size, and defaults to 5% of dynamic
space size.

Note: currently changes to this value are lost when saving core."
  (extern-alien "bytes_consed_between_gcs" os-vm-size-t))

(defun (setf bytes-consed-between-gcs) (val)
  (declare (type (and fixnum unsigned-byte) val))
  #+(or gencgc mark-region-gc)
  (let ((current (extern-alien "bytes_consed_between_gcs" os-vm-size-t)))
    (when (< val current)
      (decf (extern-alien "auto_gc_trigger" os-vm-size-t) (- current val))))
  (setf (extern-alien "bytes_consed_between_gcs" os-vm-size-t) val))

(declaim (inline maybe-handle-pending-gc))
(defun maybe-handle-pending-gc ()
  (when (and (not *gc-inhibit*)
             (or #+sb-thread *stop-for-gc-pending*
                 *gc-pending*))
    (sb-unix::receive-pending-interrupt)))

;;;; GENCGC specifics
;;;;
(define-alien-variable ("gc_logfile" %gc-logfile) (* char))

(defun (setf gc-logfile) (pathname)
  (let ((new (when pathname
               (make-alien-string
                (native-namestring (translate-logical-pathname pathname)
                                   :as-file t))))
        (old %gc-logfile))
    (setf %gc-logfile new)
    (when old
      (free-alien old))
    pathname))
(defun gc-logfile ()
    "Return the pathname used to log garbage collections. Can be SETF.
Default is NIL, meaning collections are not logged. If non-null, the
designated file is opened before and after each collection, and generation
statistics are appended to it."
  (let ((val (cast %gc-logfile c-string)))
    (when val
      (native-pathname val))))

(deftype generation-index ()
  `(integer 0 ,sb-vm:+pseudo-static-generation+))

;;; FIXME: GENERATION (and PAGE, as seen in room.lisp) should probably be
;;; defined in Lisp, and written to header files by genesis, instead of this
;;; OAOOMiness -- this duplicates the struct definition in gencgc.c.
(define-alien-type generation
    (struct generation
            (bytes-allocated os-vm-size-t)
            (gc-trigger os-vm-size-t)
            (bytes-consed-between-gcs os-vm-size-t)
            (number-of-gcs int)
            (number-of-gcs-before-promotion int)
            (cum-sum-bytes-allocated os-vm-size-t)
            (minimum-age-before-gc double)))

(define-alien-variable generations
    (array generation #.(1+ sb-vm:+pseudo-static-generation+)))

;;; Why is PAGE-INDEX-T in SB-KERNEL but PAGE and the page table are in SB-VM?
(define-alien-type nil
    (struct sb-vm::page
            ;; To cut down the size of the page table, the scan_start_offset
            ;; - a/k/a "start" - is measured in 4-byte integers regardless
            ;; of word size. This is fine for 32-bit address space,
            ;; but if 64-bit then we have to scale the value. Additionally
            ;; there is a fallback for when even the scaled value is too big.
            (sb-vm::start #+64-bit (unsigned 32) #-64-bit signed)
            ;; Caution: The low bit of WORDS-USED* is a flag bit
            (sb-vm::words-used* (unsigned 16)) ; (* in the name is a memory aid)
            (sb-vm::flags (unsigned 8)) ; this named 'type' in C
            (sb-vm::gen (signed 8))))
(define-alien-variable ("page_table" sb-vm:page-table) (* (struct sb-vm::page)))

(declaim (ftype (sfunction (t) (integer -1 #.(floor most-positive-word sb-vm:gencgc-page-bytes)))
                sb-vm:find-page-index)
         (inline sb-vm:find-page-index))
(defun sb-vm:find-page-index (address)
  (with-alien ((find-page-index (function page-index-t unsigned) :extern "ext_find_page_index"))
    (truly-the (integer -1 #.(floor most-positive-word sb-vm:gencgc-page-bytes))
               (alien-funcall find-page-index address))))

(defun pages-allocated ()
  (loop for n below (extern-alien "next_free_page" signed)
        count (not (zerop (slot (deref sb-vm:page-table n) 'sb-vm::flags)))))

#-mark-region-gc
(defun generation-of (object)
  (with-pinned-objects (object)
    (let* ((addr (get-lisp-obj-address object))
           (page (sb-vm:find-page-index addr)))
      (cond ((>= page 0) (slot (deref sb-vm:page-table page) 'sb-vm::gen))
            #+immobile-space
            ((immobile-space-addr-p addr)
             ;; SIMPLE-FUNs don't contain a generation byte
             (when (simple-fun-p object)
               (setq addr (get-lisp-obj-address (fun-code-header object))))
             (let ((sap (int-sap (logandc2 addr sb-vm:lowtag-mask))))
               (logand (if (fdefn-p object) (sap-ref-8 sap 1) (sap-ref-8 sap 3))
                       #xF)))))))

#+mark-region-gc
(defun generation-of (object)
  (with-alien ((gc-gen-of (function char unsigned int) :extern))
    (let ((result (with-pinned-objects (object)
                    (alien-funcall gc-gen-of
                                   (get-lisp-obj-address object) 127))))
      (unless (= result 127) result))))

(export 'page-protected-p)
(macrolet ((addr->mark (addr)
             `(sap-ref-8 (extern-alien "gc_card_mark" system-area-pointer)
                         (logand (ash ,addr (- (integer-length (1- sb-vm:gencgc-card-bytes))))
                                 (extern-alien "gc_card_table_mask" int))))
           (markedp (val)
             #+soft-card-marks `(eql ,val 0)
             ;; With physical protection there are 2 single-bit fields packed in the mark byte.
             ;; The 0 bit is the card mark bit (inverse of protected-p)
             #-soft-card-marks `(oddp ,val)))
  (defun page-protected-p (object) ; OBJECT must be pinned by caller
    (not (markedp (addr->mark (get-lisp-obj-address object)))))
  (defun object-card-marks (obj)
    (aver (eq (heap-allocated-p obj) :dynamic))
    ;; Return a bit-vector with a 1 for each marked card the object is on
    ;; and a 0 for each unmarked card.
    (with-pinned-objects (obj)
      (let* ((base (logandc2 (get-lisp-obj-address obj) sb-vm:lowtag-mask))
             (last (+ base (1- (primitive-object-size obj))))
             (aligned-base (logandc2 base (1- sb-vm:gencgc-card-bytes)))
             (aligned-last (logandc2 last (1- sb-vm:gencgc-card-bytes)))
             (result (make-array (1+ (truncate (- aligned-last aligned-base)
                                               sb-vm:gencgc-card-bytes))
                                 :element-type 'bit)))
        (loop for addr from aligned-base to aligned-last by sb-vm:gencgc-card-bytes
              for i from 0
              do (setf (aref result i) (if (markedp (addr->mark addr)) 1 0)))
        result))))

(macrolet ((def (slot doc &optional adjustable)
             `(progn
                (defun ,(symbolicate "GENERATION-" slot) (generation)
                  ,doc
                  (declare (generation-index generation))
                  (slot (deref generations generation) ',slot))
                ,@(when adjustable
                    `((defun (setf ,(symbolicate "GENERATION-" slot)) (value generation)
                        (declare (generation-index generation))
                        (setf (slot (deref generations generation) ',slot) value)))))))
  (def bytes-consed-between-gcs
      "Number of bytes that can be allocated to GENERATION before that
generation is considered for garbage collection. This value is meaningless for
generation 0 (the nursery): see BYTES-CONSED-BETWEEN-GCS instead. Default is
5% of the dynamic space size divided by the number of non-nursery generations.
Can be assigned to using SETF. Available on GENCGC platforms only.

Experimental: interface subject to change."
    t)
  (def minimum-age-before-gc
      "Minimum average age of objects allocated to GENERATION before that
generation is may be garbage collected. Default is 0.75. See also
GENERATION-AVERAGE-AGE. Can be assigned to using SETF. Available on GENCGC
platforms only.

Experimental: interface subject to change."
    t)
  (def number-of-gcs-before-promotion
      "Number of times garbage collection is done on GENERATION before
automatic promotion to the next generation is triggered. Default is 1. Can be
assigned to using SETF. Available on GENCGC platforms only.

Experimental: interface subject to change."
    t)
  (def bytes-allocated
      "Number of bytes allocated to GENERATION currently. Available on GENCGC
platforms only.

Experimental: interface subject to change.")
  (def number-of-gcs
      "Number of times garbage collection has been done on GENERATION without
promotion. Available on GENCGC platforms only.

Experimental: interface subject to change.")
  (defun generation-average-age (generation)
    "Average age of memory allocated to GENERATION: average number of times
objects allocated to the generation have seen younger objects promoted to it.
Available on GENCGC platforms only.

Experimental: interface subject to change."
    (declare (generation-index generation))
    (alien-funcall (extern-alien "generation_average_age"
                                 (function double generation-index-t))
                   generation)))

(macrolet ((cases ()
             `(cond ((< sb-vm:dynamic-space-start addr
                        (sap-int (dynamic-space-free-pointer)))
                     :dynamic)
                    ((immobile-space-addr-p addr) :immobile)
                    ((< sb-vm:read-only-space-start addr
                        (sap-int sb-vm:*read-only-space-free-pointer*))
                     :read-only)
                    ;; Without immobile-space, the text range is
                    ;; more-or-less an extension of static space.
                    #-immobile-space
                    ((< sb-vm:text-space-start addr
                        (sap-int sb-vm:*text-space-free-pointer*))
                     :static)
                    ((< sb-vm:static-space-start addr
                        (sap-int sb-vm:*static-space-free-pointer*))
                     :static))))
;;; Return true if X is in any non-stack GC-managed space.
;;; (Non-stack implies not TLS nor binding stack)
;;; There's a microscopic window of time in which next_free_page for dynamic space
;;; could _decrease_ after calculating the address of X, so we'll pin X
;;; to ensure that can't happen.
(defun heap-allocated-p (x)
  (with-pinned-objects (x)
    (let ((addr (get-lisp-obj-address x)))
      (and (sb-vm:is-lisp-pointer addr)
           (cases)))))

;;; Internal use only. FIXME: I think this duplicates code that exists
;;; somewhere else which I could not find.
(defun lisp-space-p (sap &aux (addr (sap-int sap))) (cases))
) ; end MACROLET

(define-condition memory-fault-error (system-condition error) ()
  (:report
   (lambda (condition stream)
     (let* ((faultaddr (system-condition-address condition))
            (string (if (<= sb-vm:read-only-space-start
                            faultaddr
                            (sap-int (sap+ sb-vm:*read-only-space-free-pointer* -1)))
                        "Attempt to modify a read-only object at #x~X."
                        "Unhandled memory fault at #x~X.")))
       (format stream string faultaddr)))))
