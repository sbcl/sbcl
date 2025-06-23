(in-package sb-vm)

(export '(*arena-exhaustion-handler*
          arena
          arena-p
          arena-bytes-used
          arena-bytes-wasted
          arena-length
          arena-userdata
          new-arena
          destroy-arena
          hide-arena
          unhide-arena
          switch-to-arena
          rewind-arena
          unuse-arena
          in-same-arena
          dump-arena-objects
          arena-contents
          c-find-heap->arena
          points-to-arena
          show-heap->arena))

;;; A contiguous block is described by 'struct arena_memblk' in C.
;;; There is no corresponding lisp defstruct.
(defmacro arena-memblk-freeptr (memblk) `(sap-ref-sap ,memblk 0))
(defmacro arena-memblk-limit (memblk) `(sap-ref-sap ,memblk ,(ash 1 word-shift)))
(defmacro arena-memblk-next (memblk) `(sap-ref-sap ,memblk ,(ash 2 word-shift)))
(defmacro arena-memblk-base (memblk) `(sap-ref-sap ,memblk ,(ash 3 word-shift)))

(defmacro do-arena-blocks ((blkvar arena) &body body)
  ;; bind BLK to a SAP pointing to successive 'struct memblk' in arena
  `(let ((,blkvar (int-sap (arena-first-block ,arena))))
     (loop (progn ,@body)
           (setq ,blkvar (arena-memblk-next ,blkvar))
           (if (zerop (sap-int ,blkvar)) (return)))))

;;; Not "just" define-alien-routine because we have to GET-LISP-OBJ-ADDRESS on the arg
(defun arena-bytes-used (arena)
  (alien-funcall (extern-alien "arena_bytes_used" (function unsigned unsigned))
                 (get-lisp-obj-address arena)))

(defun unuse-arena ()
  #+system-tlabs
  (when (arena-p (thread-current-arena))
    (switch-to-arena 0)))

(define-load-time-global *arena-index-generator* 0)
(declaim (fixnum *arena-index-generator*))
(define-load-time-global *arena-lock* (sb-thread:make-mutex :name "arena"))

;;; Release all memblks back to the OS, except the first one associated with this arena.
(defun rewind-arena (arena)
  #+system-tlabs
  (cond ((= (arena-token arena) most-positive-word)
         (bug "Arena token overflow. Need to implement double-precision count"))
        ((eql (arena-link arena) 0)) ; never used - do nothing
        (t
         (aver (not (eq (arena-hidden arena) t)))
         (alien-funcall (extern-alien "arena_release_memblks" (function void unsigned))
                        (get-lisp-obj-address arena))
         (setf (arena-exhausted arena) nil)
         (setf (arena-bytes-wasted arena) 0)
         (incf (arena-token arena))))
  arena)

;;; The arena structure has to be created in the arena,
;;; but the constructor can't do it (chicken-and-egg problem).
;;; There are one or more large blocks of memory associated with
;;; an arena, obtained via malloc(). Allocations within a block are
;;; contiguous but the blocks can be discontiguous.
#+system-tlabs
(declaim (ftype (sfunction (fixnum &optional fixnum fixnum &key (:hidable boolean))
                           arena) new-arena))
(defun new-arena (size &optional (growth-amount size) (max-extensions 7)
                       &key hidable)
  (declare (ignorable growth-amount max-extensions hidable))
  (assert (>= size 65536))
  (assert (>= growth-amount 32768))
  "Create a new arena of SIZE bytes which can be grown additively by GROWTH-AMOUNT
one or more times, not to exceed MAX-EXTENSIONS times"
  #-system-tlabs :placeholder
  #+system-tlabs
  (let ((huge ; a request exceeding strictly this size gets separately allocated
         (cond ((>= size 2097152) (ash size -9)) ; 1/512th = .19% of the total arena size
               ((>= size 524288) (ash size -8))  ; 1/256th = .39%
               ((>= size 131072) (ash size -7))  ; 1/128th = .78%
               (t (ash size -6))))               ;  1/64th = 1.6%
        (size-limit (+ size (the fixnum (* max-extensions growth-amount))))
        (index (with-system-mutex (*arena-lock*) (incf *arena-index-generator*)))
        (arena (truly-the arena
                (%make-lisp-obj
                 (alien-funcall (extern-alien "sbcl_new_arena"
                                              (function unsigned unsigned int))
                                size (if hidable 1 0))))))
    (%set-instance-layout arena (load-time-value (find-layout 'arena) t))
    (setf (arena-size-limit arena) size-limit
          (arena-growth-amount arena) growth-amount
          (arena-huge-object-threshold  arena) huge
          (arena-exhausted arena) nil
          (arena-index arena) index
          (arena-hidden arena) (if hidable nil 0)
          (arena-token arena) 1
          (arena-userdata arena) nil)
    arena))

;;; Destroy memory associated with ARENA, unlinking it from the global chain.
;;; Note that we do not recycle arena IDs. It would be dangerous to do so, because a thread
;;; might believe it has a valid TLAB in the deleted arena if it randomly matched on the
;;; ID and token. That's a valid argument for making arena tokens gobally unique
;;; (the way it used to work before I made tokens arena-specific)
(defun destroy-arena (arena)
  ;; C is responsible for most of the cleanup.
  #+x86-64
  (%primitive delete-arena arena)
  #-x86-64
  (alien-funcall (extern-alien "sbcl_delete_arena"
                               (function void
                                         unsigned-long))
                 (get-lisp-obj-address arena))
  ;; It is illegal to access the structure now, since that was in the arena.
  ;; So return an arbitrary success indicator. It might happen that you can accidentally
  ;; refer to the structure, but technically that constitutes a use-after-free bug.
  t)

(defmacro with-arena ((arena) &body body)
  (declare (ignorable arena))
  #-system-tlabs `(progn ,@body)
  #+system-tlabs
  `(let ((a ,arena))
     (declare (arena a))
     ;; maybe allow switching from one arena to another?
     (switch-to-arena a)
     (unwind-protect (progn ,@body) (switch-to-arena 0))))

#+system-tlabs
(progn
(declaim (inline cur-thread-stack-object-p))
(defun cur-thread-stack-object-p (x)
  (let ((a (get-lisp-obj-address x)))
    (and (< (sap-int (current-sp)) a
            (sap-int (current-thread-offset-sap thread-control-stack-end-slot))))))

(declaim (inline force-to-heap-p))
(defun force-to-heap-p (x)
  (and (not (zerop (sap-int (current-thread-offset-sap thread-arena-slot))))
       (or (dynamic-space-obj-p x)
           ;; FIXME: is checking for read-only still correct???
           (read-only-space-obj-p x)))))

(defmacro in-same-arena ((object reason) &rest forms)
  (declare (ignorable object reason))
  #-system-tlabs `(progn ,@forms)
  #+system-tlabs
  `(dx-flet ((thunk () ,@forms))
     (let ((allocating-in-arena-p
            (not (zerop (sap-int (current-thread-offset-sap thread-arena-slot)))))
           (.obj. ,object))
     ;; - if it was on stack, then don't switch to/from an arena
     ;; - or if object was in readonly space. This can happen with adjust-array
     ;;   on an initially zero-length vector that was moved into readonly space.
     (if (or (cur-thread-stack-object-p .obj.)
             (read-only-space-obj-p .obj.)
             (eq allocating-in-arena-p (not (dynamic-space-obj-p .obj.))))
         (funcall #'thunk)
         (call-using-arena #'thunk .obj. ',reason)))))

;;; backward-compatibility assuming no extension blocks. DON'T USE THIS!
(defun arena-base-address (arena) (arena-first-block arena))

;;; Possible TODO: if this needs to be efficient, then we would need
;;; a balanced tree which maps each 'struct memblk' to an arena.
;;; This would be complicated by the fact that extension occurs in C code,
;;; so C would have to maintain the balanced tree structure.
;;; It may not be terribly important to optimize.
(defun find-containing-arena (addr)
  (declare (word addr))
  (let ((chain (sap-ref-lispobj (foreign-symbol-sap "arena_chain" t) 0)))
    (unless (eql chain 0)
      (do ((arena chain (arena-link arena)))
          ((not arena))
        (do-arena-blocks (memblk arena)
          (when (< (sap-int memblk) addr (sap-int (arena-memblk-freeptr memblk)))
            (return-from find-containing-arena arena)))))))

(defun arena-mprotect (arena protect)
  (alien-funcall (extern-alien "arena_mprotect" (function void unsigned int))
                 (get-lisp-obj-address arena)
                 (if protect 1 0))
  arena)

(defun hide-arena (arena)
  (aver (not (arena-hidden arena)))
  ;; Inform GC as of now not to look in the arena
  (setf (arena-hidden arena) t)
  (arena-mprotect arena t))
;; FIXME: for the pattern "unhide / rewind / with-arena" to be safe, there has to be
;; another state introduced which is "hidden + ignored-during-GC".  This is needed
;; in case an auto-triggered collection skipped over the arena. Then there is race
;; with the next auto-triggered GC in between unhide + rewind, beacause any pointers
;; in the un-hidden arena may point to garbage.
(defun unhide-arena (arena)
  (aver (eq (arena-hidden arena) t))
  (arena-mprotect arena nil)
  ;; Inform GC as of now that it can look in the arena
  (setf (arena-hidden arena) nil)
  arena)

(defun maybe-show-arena-switch (direction reason)
  (declare (ignore direction reason)))
#+system-tlabs
(defun call-using-arena (thunk object reason)
  (if (dynamic-space-obj-p object)
      (let ((arena (thread-current-arena)))
        (aver (%instancep arena))
        (maybe-show-arena-switch "from" reason)
        (progn (switch-to-arena 0)
               (multiple-value-prog1 (funcall thunk) (switch-to-arena arena))))
      (let ((usable-arena (find-containing-arena (get-lisp-obj-address object))))
        (or usable-arena
            (bug "Object ~X looks arena-allocated but can't find where"
                 (get-lisp-obj-address object)))
        (maybe-show-arena-switch "to" reason)
        (progn (switch-to-arena usable-arena)
               (multiple-value-prog1 (funcall thunk) (switch-to-arena 0))))))

#+system-tlabs
(defun c-find-heap->arena (&optional arena)
  (declare (notinline coerce)) ; "Proclaiming SB-KERNEL:COERCE-TO-LIST to be INLINE, but 1 call to it was..."
  (let* ((result (make-array 10000))
         (n (with-pinned-objects (arena result)
              (alien-funcall
               (extern-alien "find_dynspace_to_arena_ptrs" (function int unsigned unsigned))
               (if arena (get-lisp-obj-address arena) 0)
               (get-lisp-obj-address result)))))
    ;; The AVL tree of threads is keyed by THREAD-PRIMITIVE-THREAD, which is the base
    ;; of each thread's TLS and above its binding stack. Therefore we need two separate
    ;; FIND operations, one >= and one <=. Perhaps it would make sense to key by control stack base.
    ;; FIXME: These functions should probably acquire 'all_threads_lock'. Even so, the entire
    ;; entire mechanism is still slightly unsafe because the finder returns raw addresses.
    (flet ((find-tls-ref (addr)
             (binding* ((node (sb-thread::avl-find<= addr sb-thread::*all-threads*) :exit-if-null)
                        (thread-sap (int-sap
                                     (sb-thread::thread-primitive-thread
                                      (sb-thread::avlnode-data node)))))
               (when (and (<= (sap-int thread-sap) addr)
                          (< addr (sap-int (sap+ thread-sap (ash *free-tls-index* n-fixnum-tag-bits)))))
                 (let* ((tlsindex (sap- (int-sap addr) thread-sap))
                        (symbol (symbol-from-tls-index tlsindex)))
                   (list (sap-ref-lispobj thread-sap (ash thread-lisp-thread-slot word-shift))
                         :tls symbol)))))
           (find-binding (addr)
             (binding* ((node (sb-thread::avl-find>= addr sb-thread::*all-threads*) :exit-if-null)
                        (thread-sap (int-sap
                                     (sb-thread::thread-primitive-thread
                                      (sb-thread::avlnode-data node))))
                        (bindstack-base
                         (sap-ref-word thread-sap (ash thread-binding-stack-start-slot word-shift)))
                        (bindstack-ptr
                         (sap-ref-word thread-sap (ash thread-binding-stack-pointer-slot word-shift))))
               (when (and (>= addr bindstack-base) (< addr bindstack-ptr))
                 (let* ((tlsindex (sap-ref-word (int-sap addr) (- n-word-bytes)))
                        (symbol (symbol-from-tls-index tlsindex)))
                   (list (sap-ref-lispobj thread-sap (ash thread-lisp-thread-slot word-shift))
                         :binding symbol))))))
      (dotimes (i n)
        (let ((element (aref result i)))
          (when (fixnump element) ; it's a thread memory address
            (let ((word (ash element n-fixnum-tag-bits)))
              (setf (aref result i) (or (find-tls-ref word) (find-binding word))))))))
    (coerce (subseq result 0 n) 'list)))

(defmethod print-object ((self arena) stream)
  (print-unreadable-object (self stream :type t :identity t)
    ;; Don't compute and display bytes-used, as doing so acquires the lock,
    ;; and that's an impolite thing to do in a print-object method.
    ;; (Perhaps this could _try_ to acquire the lock, computing usage when it can)
    (format stream "id=~D size=~D waste=~D"
            (arena-index self)
            (arena-length self)
            (arena-bytes-wasted self))))

(defun copy-number-to-heap (n)
  (declare (sb-c::tlab :system))
  (named-let copy ((n n))
    (if (or (typep n '(or fixnum single-float))
            (and (typep n '(or bignum double-float (complex float)))
                 (dynamic-space-obj-p n)))
        n
        (typecase n
          ;; can't use copy-bignum because that uses the active tlab
          ;; nor bignum-replace because it's a not-yet-defined macro.
          (bignum (let* ((len (sb-bignum:%bignum-length n))
                         (new (sb-bignum:%allocate-bignum len)))
                    (dotimes (i len new)
                      (declare (type sb-bignum:bignum-index i))
                      (sb-bignum:%bignum-set new i (sb-bignum:%bignum-ref n i)))))
          (double-float
           (%make-double-float (double-float-bits n)))
          ;; ratio is dynspace-p only if both parts are. copy everything to be safe
          (ratio (%make-ratio (truly-the integer (copy (%numerator n)))
                              (truly-the integer (copy (%denominator n)))))
          ;; Handle complex subtypes by hand so that a vop or IR2-converter is used
          ((complex single-float) (complex (realpart n) (imagpart n)))
          ((complex double-float) (complex (realpart n) (imagpart n)))
          (complex ; same as RATIO
           (%make-complex (truly-the rational (copy (%realpart n)))
                          (truly-the rational (copy (%imagpart n)))))
          (t (bug "~S is not a number" n))))))

;;; This variable is bound to a function of three args: arena, current request,
;;; and desired new total space consumption of the arena. It is called prior to
;;; any attempt to add an extension block to the indicated arena. During evaluation
;;; of the predicate, allocations are directed to the heap, not the arena.
;;; * If the predicate returns non-NIL, then the arena is extended by its
;;;   predetermined extension amount, regardless of the initially specified maximum
;;;   size of this arena. i.e. the predicate subsumes any given size limit.
;;; * If NIL, then we signal an uncontinuable error. The arena is _not_ in-use
;;;   during the error signaling.
;;; * Finally, nonlocal exit should bail out of whatever computation consumed
;;;   too much space, and will automatically terminate the pending allocation,
;;;   relinquishing exclusive use of the arena. The arena _will_ be in-use
;;;   during the unwind, in an "emergency overflow" mode. Further allocation
;;;   requests will not call the handler, but will succeed. The intent is to
;;;   give the application enough working memory to clean up without accidentally
;;;   creating heap-to-arena pointers, as might happen if it were to allocate
;;;   to the heap unexpectedly.
#+(and x86-64 system-tlabs)
(progn
(defun default-arena-extension-test (arena pending-allocation proposed-total-size)
  (declare (ignore pending-allocation))
  (<= proposed-total-size (arena-size-limit arena)))

(declaim (function *arena-exhaustion-handler*))
(sb-ext:define-load-time-global *arena-exhaustion-handler*
  #'default-arena-extension-test)

(defun handle-arena-request (units count &aux (arena (thread-current-arena)))
  ;; Get off the arena as soon as possible, just in case some of the code below conses
  ;; (There are 5 calls to asm routines with -BIGNUM- in their name, and while they should
  ;; not actually be invoked, better safe than sorry)
  ;; Using WITHOUT-ARENA here wouldn't necessarily be right- we have to decide
  ;; whether to restore the thread's arena on exit from this.
  (aver (%instancep arena))
  (switch-to-arena 0)
  (flet ((release-lock (arena)
           (alien-funcall (extern-alien "release_lisp_arena_lock" (function void unsigned))
                          (logandc2 (get-lisp-obj-address arena) lowtag-mask))))
    (let* ((nbytes (if (eq units 0) ; count = N cons cells, already a fixnum
                       (* (the fixnum count) 16)
                       ;; Otherwise COUNT is bytes, but the SC of the arg
                       ;; is unsigned-reg so it needs shifting to correct it.
                       (ash (the fixnum count) n-fixnum-tag-bits)))
           (hugep (and (> nbytes (arena-huge-object-threshold arena)) (= units 1)))
           (request (if hugep ; non-list oversized object will gets its own extension block
                        nbytes
                        (arena-growth-amount arena)))
           (new-size (+ request (arena-length arena)))
           (allowp nil)
           (abort t))
      #+nil
      (format t "~&Arena-handler ~S ~D ~D bytes=~D len=~x ex=~D~%"
              arena units count nbytes (arena-length arena) (arena-exhausted arena))
      (unwind-protect
           (setq allowp (or (arena-exhausted arena) ; always allow!
                            (funcall *arena-exhaustion-handler* arena nbytes new-size))
                 abort nil)
        (when (or abort (not allowp))
          ;; If predicate performed THROW to a higher frame (abort remained T),
          ;; or said to fail the request then release the arena lock.
          (release-lock arena)
          ;; User's cleanup path may not expect that allocations go to the heap,
          ;; therefore we switch back to the arena.
          (when abort
            (setf (arena-exhausted arena) t)
            (switch-to-arena arena)))) ; and we're out
      ;; Only a normal return of the predicate reaches this point.
      ;; The user either wants to extend the arena or fail, signaling overflow.
      (cond (allowp
             ;; Growing the arena releases the mutex. Return the result of grow_arena
             ;; which is the allocated address if the allocation was a huge object.
             ;; (the call to extend could go in the HANDLE-ARENA-REQUEST asm routine,
             ;; but I find this flow to be natural and easily understandable)
             (prog1
                 (%make-lisp-obj
                  ;; MUST NOT stop for GC while holding the arena mutex
                  (with-pseudo-atomic-foreign-calls
                   (alien-funcall (extern-alien "extend_lisp_arena"
                                                (function unsigned unsigned unsigned int))
                                  (logandc2 (get-lisp-obj-address arena) lowtag-mask)
                                  request (if hugep 1 0))))
               (switch-to-arena arena)))
            (t
             (release-lock arena)
             ;; There's no way to continue this because the predicate already had
             ;; a chance to do whatever it needed to do to proceed.
             ;; In this case, we do NOT set the exhausted flag, because we want
             ;; the error to persist if you attempt again to use the arena.
             ;; With the default predicate which imposes a strict bound on size,
             ;; once the error occurs, you don't get a "free pass" to use the arena
             ;; forever after. The user _must_ override the predicate to have the new
             ;; behavior of utilizing a overgrown arena in their cleanup routine.
             (unwind-protect
                  (error 'sb-kernel::arena-exhausted-error :arena arena :request request)
               ;; The arena _should_ remain in use on exit, because presuming existence
               ;; of WITH-ARENA at an outer level, it would fail the assertion in
               ;; switch_to_arena trying to unuse it.
               (switch-to-arena arena))))))) ; and we're out
) ; end PROGN
