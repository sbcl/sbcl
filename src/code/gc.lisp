;;;; garbage collection and allocation-related code

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;;; DYNAMIC-USAGE and friends

(eval-when (:compile-toplevel :execute)
  (sb!xc:defmacro def-c-var-fun (lisp-fun c-var-name)
    `(defun ,lisp-fun ()
       (sb!alien:extern-alien ,c-var-name (sb!alien:unsigned 32)))))

#!-sb-fluid
(declaim (inline current-dynamic-space-start))
#!+gencgc
(defun current-dynamic-space-start () sb!vm:dynamic-space-start)
#!-gencgc
(def-c-var-fun current-dynamic-space-start "current_dynamic_space")

#!-sb-fluid
(declaim (inline dynamic-usage))
#!+gencgc
(def-c-var-fun dynamic-usage "bytes_allocated")
#!-gencgc
(defun dynamic-usage ()
  (the (unsigned-byte 32)
       (- (sb!sys:sap-int (sb!c::dynamic-space-free-pointer))
          (current-dynamic-space-start))))

(defun static-space-usage ()
  (- (* sb!vm:*static-space-free-pointer* sb!vm:n-word-bytes)
     sb!vm:static-space-start))

(defun read-only-space-usage ()
  (- (* sb!vm::*read-only-space-free-pointer* sb!vm:n-word-bytes)
     sb!vm:read-only-space-start))

(defun control-stack-usage ()
  #!-stack-grows-downward-not-upward
  (- (sb!sys:sap-int (sb!c::control-stack-pointer-sap))
     (sb!sys:sap-int (sb!di::descriptor-sap sb!vm:*control-stack-start*)))
  #!+stack-grows-downward-not-upward
  (- (sb!sys:sap-int (sb!di::descriptor-sap sb!vm:*control-stack-end*))
     (sb!sys:sap-int (sb!c::control-stack-pointer-sap))))

(defun binding-stack-usage ()
  (- (sb!sys:sap-int (sb!c::binding-stack-pointer-sap))
     (sb!sys:sap-int (sb!di::descriptor-sap sb!vm:*binding-stack-start*))))

;;;; ROOM

(defun room-minimal-info ()
  (format t "Dynamic space usage is:   ~10:D bytes.~%" (dynamic-usage))
  (format t "Read-only space usage is: ~10:D bytes.~%" (read-only-space-usage))
  (format t "Static space usage is:    ~10:D bytes.~%" (static-space-usage))
  (format t "Control stack usage is:   ~10:D bytes.~%" (control-stack-usage))
  (format t "Binding stack usage is:   ~10:D bytes.~%" (binding-stack-usage))
  #!+sb-thread
  (format t
          "Control and binding stack usage is for the current thread only.~%")
  (format t "Garbage collection is currently ~:[enabled~;DISABLED~].~%"
          *gc-inhibit*))

(defun room-intermediate-info ()
  (room-minimal-info)
  (sb!vm:memory-usage :count-spaces '(:dynamic)
                      :print-spaces t
                      :cutoff 0.05f0
                      :print-summary nil))

(defun room-maximal-info ()
  ;; FIXME: SB!VM:INSTANCE-USAGE calls suppressed until bug 344 is fixed
  (room-intermediate-info)
  ;; old way, could be restored when bug 344 fixed:
  ;;x (room-minimal-info)
  ;;x (sb!vm:memory-usage :count-spaces '(:static :dynamic))
  ;;x (sb!vm:instance-usage :dynamic :top-n 10)
  ;;x (sb!vm:instance-usage :static :top-n 10)
  )

(defun room (&optional (verbosity :default))
  #!+sb-doc
  "Print to *STANDARD-OUTPUT* information about the state of internal
  storage and its management. The optional argument controls the
  verbosity of output. If it is T, ROOM prints out a maximal amount of
  information. If it is NIL, ROOM prints out a minimal amount of
  information. If it is :DEFAULT or it is not supplied, ROOM prints out
  an intermediate amount of information."
  (fresh-line)
  (ecase verbosity
    ((t)
     (room-maximal-info))
    ((nil)
     (room-minimal-info))
    (:default
     (room-intermediate-info)))
  (values))

;;;; GET-BYTES-CONSED

;;; the total number of bytes freed so far (including any freeing
;;; which goes on in PURIFY)
;;;
;;; (We save this so that we can calculate the total number of bytes
;;; ever allocated by adding this to the number of bytes currently
;;; allocated and never freed.)
(declaim (type unsigned-byte *n-bytes-freed-or-purified*))
(defvar *n-bytes-freed-or-purified* 0)
(defun gc-reinit ()
  (setq *gc-inhibit* nil)
  (gc)
  (setf *n-bytes-freed-or-purified* 0
        *gc-run-time* 0
        ;; See comment in interr.lisp
        *heap-exhausted-error-condition* (make-condition 'heap-exhausted-error)))

(declaim (ftype (sfunction () unsigned-byte) get-bytes-consed))
(defun get-bytes-consed ()
  #!+sb-doc
  "Return the number of bytes consed since the program began. Typically
this result will be a consed bignum, so if you have an application (e.g.
profiling) which can't tolerate the overhead of consing bignums, you'll
probably want either to hack in at a lower level (as the code in the
SB-PROFILE package does), or to design a more microefficient interface
and submit it as a patch."
  (+ (dynamic-usage)
     *n-bytes-freed-or-purified*))

;;;; GC hooks

(defvar *after-gc-hooks* nil
  "Called after each garbage collection, except for garbage collections
triggered during thread exits. In a multithreaded environment these hooks may
run in any thread.")


;;;; internal GC

(sb!alien:define-alien-routine collect-garbage sb!alien:int
  (#!+gencgc last-gen #!-gencgc ignore sb!alien:int))

#!+sb-thread
(progn
  (sb!alien:define-alien-routine gc-stop-the-world sb!alien:void)
  (sb!alien:define-alien-routine gc-start-the-world sb!alien:void))
#!-sb-thread
(progn
  (defun gc-stop-the-world ())
  (defun gc-start-the-world ()))


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

(defvar *already-in-gc* (sb!thread:make-mutex :name "GC lock"))

;;; A unique GC id. This is supplied for code that needs to detect
;;; whether a GC has happened since some earlier point in time. For
;;; example:
;;;
;;;   (let ((epoch *gc-epoch*))
;;;      ...
;;;      (unless (eql epoch *gc-epoch)
;;;        ....))
;;;
;;; This isn't just a fixnum counter since then we'd have theoretical
;;; problems when exactly 2^29 GCs happen between epoch
;;; comparisons. Unlikely, but the cost of using a cons instead is too
;;; small to measure. -- JES, 2007-09-30
(declaim (type cons *gc-epoch*))
(defvar *gc-epoch* (cons nil nil))

(defun sub-gc (&key (gen 0))
  (cond (*gc-inhibit*
         (setf *gc-pending* t)
         nil)
        (t
         (without-interrupts
           (setf *gc-pending* :in-progress)
           ;; Tricks to to prevent triggerring a recursive gc. This is
           ;; like a WITHOUT-GCING inside the lock except that we
           ;; cannot call MAYBE-HANDLE-PENDING-GC at the end, because
           ;; that would lead to a recursive attempt on the lock. In
           ;; case you are wondering, wrapping the lock in a
           ;; WITHOUT-GCING would also deadlock. The
           ;; *IN-WITHOUT-GCING* part is used to tell the runtime that
           ;; it's ok to have a pending gc even though *GC-INHIBIT* is
           ;; NIL.
           ;;
           ;; Now, if GET-MUTEX did not cons, that would be enough.
           ;; Because it does, we need the :IN-PROGRESS bit above to
           ;; tell the runtime not to trigger gcs.
           (let ((sb!impl::*in-without-gcing* t)
                 (sb!impl::*deadline* nil)
                 (sb!impl::*deadline-seconds* nil))
             (sb!thread:with-mutex (*already-in-gc*)
               (let ((*gc-inhibit* t))
                 (let ((old-usage (dynamic-usage))
                       (new-usage 0))
                   (unsafe-clear-roots)
                   (gc-stop-the-world)
                   (let ((start-time (get-internal-run-time)))
                     (collect-garbage gen)
                     (setf *gc-epoch* (cons nil nil))
                     (let ((run-time (- (get-internal-run-time) start-time)))
                       ;; KLUDGE: Sometimes we see the second getrusage() call
                       ;; return a smaller value than the first, which can
                       ;; lead to *GC-RUN-TIME* to going negative, which in
                       ;; turn is a type-error.
                       (when (plusp run-time)
                         (incf *gc-run-time* run-time))))
                   (setf *gc-pending* nil
                         new-usage (dynamic-usage))
                   #!+sb-thread
                   (assert (not *stop-for-gc-pending*))
                   (gc-start-the-world)
                   ;; In a multithreaded environment the other threads
                   ;; will see *n-b-f-o-p* change a little late, but
                   ;; that's OK.
                   (let ((freed (- old-usage new-usage)))
                     ;; GENCGC occasionally reports negative here, but
                     ;; the current belief is that it is part of the
                     ;; normal order of things and not a bug.
                     (when (plusp freed)
                       (incf *n-bytes-freed-or-purified* freed)))))))
           ;; While holding the mutex we were protected from
           ;; SIG_STOP_FOR_GC and recursive GCs. Now, in order to
           ;; preserve the invariant (*GC-PENDING* ->
           ;; pseudo-atomic-interrupted or *GC-INHIBIT*), let's check
           ;; explicitly for a pending gc before interrupts are
           ;; enabled again.
           (maybe-handle-pending-gc))
         t)))

(defun post-gc ()
  ;; Outside the mutex, interrupts may be enabled: these may cause
  ;; another GC. FIXME: it can potentially exceed maximum interrupt
  ;; nesting by triggering GCs.
  ;;
  ;; Can that be avoided by having the finalizers and hooks run only
  ;; from the outermost SUB-GC? If the nested GCs happen in interrupt
  ;; handlers that's not enough.
  ;;
  ;; KLUDGE: Don't run the hooks in GC's if:
  ;;
  ;; A) this thread is dying, so that user-code never runs with
  ;;    (thread-alive-p *current-thread*) => nil
  ;;
  ;; B) interrupts are disabled somewhere up the call chain since we
  ;;    don't want to run user code in such a case.
  ;;
  ;; The long-term solution will be to keep a separate thread for
  ;; finalizers and after-gc hooks.
  (when (sb!thread:thread-alive-p sb!thread:*current-thread*)
    (when *allow-with-interrupts*
      (with-interrupts
        (run-pending-finalizers)
        (call-hooks "after-GC" *after-gc-hooks* :on-error :warn)))))

;;; This is the user-advertised garbage collection function.
(defun gc (&key (gen 0) (full nil) &allow-other-keys)
  #!+(and sb-doc gencgc)
  "Initiate a garbage collection. GEN controls the number of generations
  to garbage collect."
  #!+(and sb-doc (not gencgc))
  "Initiate a garbage collection. GEN may be provided for compatibility with
  generational garbage collectors, but is ignored in this implementation."
  (when (sub-gc :gen (if full 6 gen))
    (post-gc)))

(define-alien-routine scrub-control-stack sb!alien:void)

(defun unsafe-clear-roots ()
  ;; KLUDGE: Do things in an attempt to get rid of extra roots. Unsafe
  ;; as having these cons more then we have space left leads to huge
  ;; badness.
  (scrub-control-stack)
  ;; Power cache of the bignum printer: drops overly large bignums and
  ;; removes duplicate entries.
  (scrub-power-cache)
  ;; FIXME: CTYPE-OF-CACHE-CLEAR isn't thread-safe.
  #!-sb-thread
  (ctype-of-cache-clear))


;;;; auxiliary functions

(defun bytes-consed-between-gcs ()
  #!+sb-doc
  "The amount of memory that will be allocated before the next garbage
collection is initiated. This can be set with SETF."
  (sb!alien:extern-alien "bytes_consed_between_gcs"
                         (sb!alien:unsigned 32)))

(defun (setf bytes-consed-between-gcs) (val)
  (declare (type index val))
  (setf (sb!alien:extern-alien "bytes_consed_between_gcs"
                               (sb!alien:unsigned 32))
        val))

(declaim (inline maybe-handle-pending-gc))
(defun maybe-handle-pending-gc ()
  (when (and (not *gc-inhibit*)
             (or #!+sb-thread *stop-for-gc-pending*
                 *gc-pending*))
    (sb!unix::receive-pending-interrupt)))

;;;; GENCGC specifics
;;;;
;;;; For documentation convenience, these have stubs on non-GENCGC platforms
;;;; as well.
#!+gencgc
(deftype generation-index ()
  '(integer 0 #.sb!vm:+pseudo-static-generation+))

;;; FIXME: GENERATION (and PAGE, as seen in room.lisp) should probably be
;;; defined in Lisp, and written to header files by genesis, instead of this
;;; OAOOMiness -- this duplicates the struct definition in gencgc.c.
#!+gencgc
(define-alien-type generation
    (struct generation
            (alloc-start-page page-index-t)
            (alloc-unboxed-start-page page-index-t)
            (alloc-large-start-page page-index-t)
            (alloc-large-unboxed-start-page page-index-t)
            (bytes-allocated unsigned-long)
            (gc-trigger unsigned-long)
            (bytes-consed-between-gcs unsigned-long)
            (number-of-gcs int)
            (number-of-gcs-before-promotion int)
            (cum-sum-bytes-allocated unsigned-long)
            (minimum-age-before-gc double)
            ;; `struct lutex *' or `void *', depending.
            (lutexes (* char))))

#!+gencgc
(define-alien-variable generations
    (array generation #.(1+ sb!vm:+pseudo-static-generation+)))

(macrolet ((def (slot doc &optional setfp)
             (declare (ignorable doc))
             `(progn
                (defun ,(symbolicate "GENERATION-" slot) (generation)
                  #!+sb-doc
                  ,doc
                  #!+gencgc
                  (declare (generation-index generation))
                  #!-gencgc
                  (declare (ignore generation))
                  #!-gencgc
                  (error "~S is a GENCGC only function and unavailable in this build"
                         ',slot)
                  #!+gencgc
                  (slot (deref generations generation) ',slot))
                ,@(when setfp
                        `((defun (setf ,(symbolicate "GENERATION-" slot)) (value generation)
                            #!+gencgc
                            (declare (generation-index generation))
                            #!-gencgc
                            (declare (ignore value generation))
                            #!-gencgc
                            (error "(SETF ~S) is a GENCGC only function and unavailable in this build"
                                   ',slot)
                            #!+gencgc
                            (setf (slot (deref generations generation) ',slot) value)))))))
  (def bytes-consed-between-gcs
      "Number of bytes that can be allocated to GENERATION before that
generation is considered for garbage collection. This value is meaningless for
generation 0 (the nursery): see BYTES-CONSED-BETWEEN-GCS instead. Default is
20Mb. Can be assigned to using SETF. Available on GENCGC platforms only.

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
automatic promotion to the next generation is triggered. Can be assigned to
using SETF. Available on GENCGC platforms only.

Experimental: interface subject to change."
    t)
  (def bytes-allocated
      "Number of bytes allocated to GENERATION currently. Available on GENCGC
platforms only.

Experimental: interface subject to change.")
  (def number-of-gcs
      "Number of times garbage collection has been done on GENERATION without
promotion. Available on GENCGC platforms only.

Experimental: interface subject to change."))
  (defun generation-average-age (generation)
    "Average age of memory allocated to GENERATION: average number of times
objects allocated to the generation have seen younger objects promoted to it.
Available on GENCGC platforms only.

Experimental: interface subject to change."
    #!+gencgc
    (declare (generation-index generation))
    #!-gencgc (declare (ignore generation))
    #!-gencgc
    (error "~S is a GENCGC only function and unavailable in this build."
           'generation-average-age)
    #!+gencgc
    (alien-funcall (extern-alien "generation_average_age"
                                 (function double generation-index-t))
                   generation))
