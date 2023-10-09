;;;; Lisp-side allocation (used currently only for direct allocation
;;;; to static and immobile spaces).

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(declaim (maybe-inline allocate-system-memory))
(define-alien-routine ("os_allocate" allocate-system-memory)
  system-area-pointer
  (bytes unsigned))

(define-alien-routine ("os_deallocate" deallocate-system-memory)
  void
  (addr system-area-pointer)
  (bytes unsigned))

(define-load-time-global *allocator-mutex* (sb-thread:make-mutex :name "Allocator"))

(defun allocate-static-vector (widetag length words)
  (declare (type (unsigned-byte #.n-widetag-bits) widetag)
           (type word words)
           (type index length))
  ;; Static space starts out zeroed, so it looks a bunch of cons cells
  ;; containing (0 . 0) above the free pointer. Therefore bumping the pointer
  ;; merely exposes a new range of cons cells, if GC should happen to run
  ;; while executing this code. And because we assign the LENGTH of the vector
  ;; prior to setting the widetag, and LENGTH is a fixnum, then at worst
  ;; someone can transiently observe a cons of (0 . a-fixnum).
  (let* ((nbytes (pad-data-block (+ words vector-data-offset)))
         (pointer (alien-funcall (extern-alien "atomic_bump_static_space_free_ptr"
                                               (function system-area-pointer int))
                                 nbytes)))
    (if (/= (sap-int pointer) 0)
            ;; By storing the length prior to the widetag, the word at the old
            ;; free pointer decodes as a cons instead of a 0-length vector.
            ;; Not that it should matter, but it seems slightly better to change
            ;; the new object atomically to a correctly-sized vector rather than
            ;; a cons changing into the wrong vector into the right vector.
        (let ((v (%make-lisp-obj (sap-int (sap+ pointer other-pointer-lowtag)))))
          (setf (%array-fill-pointer v) length
            ;; then store the widetag
                (sap-ref-8 pointer (+ #+big-endian (1- n-word-bytes)))
                widetag)
          v)
        (error 'simple-storage-condition
               :format-control
               "Not enough room left in static space to allocate vector."))))

#+darwin-jit
(defun allocate-static-code-vector (widetag length words)
  (declare (type (unsigned-byte #.n-widetag-bits) widetag)
           (type word words)
           (type index length))
  (or (with-system-mutex (*allocator-mutex*)
        (let* ((pointer *static-code-space-free-pointer*)
               (nbytes (pad-data-block (+ words vector-data-offset)))
               (new-pointer (sap+ pointer nbytes)))
          (when (sap<= new-pointer (int-sap static-code-space-end))
            (setf (sap-ref-word-jit pointer (ash vector-length-slot word-shift))
                  (fixnumize length))
            (setf (sap-ref-word-jit pointer 0) widetag)
            (setf *static-code-space-free-pointer* new-pointer)
            (%make-lisp-obj (logior (sap-int pointer) other-pointer-lowtag)))))
      (error 'simple-storage-condition
             :format-control
             "Not enough room left in static code space to allocate vector.")))

#+immobile-space
(progn

(define-alien-variable text-space-size (unsigned 32))
(define-alien-variable ("FIXEDOBJ_SPACE_START" fixedobj-space-start) unsigned-long)
(define-alien-variable ("text_space_highwatermark" *text-space-free-pointer*)
  system-area-pointer)
(define-alien-variable ("fixedobj_free_pointer" *fixedobj-space-free-pointer*)
  system-area-pointer)

(eval-when (:compile-toplevel) ; FIXME: these assertions look irrelevant now
  (assert (eql code-boxed-size-slot 1))
  (assert (eql code-debug-info-slot 2)))

;;; Size-class segregation (implying which page we try to allocate to)
;;; is done from lisp now, not C. There are 3 objects types we'll see,
;;; each in its own size class (even if some are coincidentally the same size).
;;;  - Symbols
;;;  - FDEFNs
;;;  - Layouts
;;; The first two are truly fixed in size. Layouts occur in varying sizes.
(defun alloc-immobile-fixedobj (nwords header)
  (let* ((widetag (logand (truly-the fixnum header) widetag-mask))
         (aligned-nwords (truly-the fixnum (align-up nwords 2)))
         (size-class
          ;; If you change this, then be sure to update tests/immobile-space.impure
          ;; which hardcodes a size class to not conflict with anything.
          (ecase widetag
            (#.symbol-widetag 1)
            (#.fdefn-widetag  2)
            (#.instance-widetag
             (cond ((<= aligned-nwords  8) (setq aligned-nwords  8) 3)
                   ((<= aligned-nwords 16) (setq aligned-nwords 16) 4)
                   ((<= aligned-nwords 24) (setq aligned-nwords 24) 5)
                   ((<= aligned-nwords 32) (setq aligned-nwords 32) 6)
                   ((<= aligned-nwords 48) (setq aligned-nwords 48) 7)
                   (t (error "Oversized layout")))))))
    (values (%primitive !alloc-immobile-fixedobj
                        size-class
                        aligned-nwords
                        header))))

(defun make-immobile-symbol (name)
  (let ((symbol (truly-the symbol
                 (or #+x86-64 (%primitive !fast-alloc-immobile-symbol)
                     (alloc-immobile-fixedobj
                      symbol-size
                      (logior (ash (1- symbol-size) n-widetag-bits) symbol-widetag))))))
    ;; symbol-hash and package ID start out as 0
    (%primitive set-slot symbol name 'make-symbol symbol-name-slot other-pointer-lowtag)
    (%primitive set-slot symbol nil 'make-symbol symbol-info-slot other-pointer-lowtag)
    (%set-symbol-global-value symbol (make-unbound-marker))
    symbol))

) ; end PROGN

(declaim (inline immobile-space-addr-p))
(defun immobile-space-addr-p (addr)
  (declare (type word addr) (ignorable addr))
  #+immobile-space
  (or (let ((start fixedobj-space-start))
        (and (<= start addr) (< addr (truly-the word (+ start fixedobj-space-size)))))
      (let ((start text-space-start))
        (and (<= start addr) (< addr (truly-the word (+ start text-space-size)))))))

(defun immobile-space-obj-p (obj)
  (immobile-space-addr-p (get-lisp-obj-address obj)))

(define-load-time-global *immobile-codeblob-tree* nil)
(define-load-time-global *dynspace-codeblob-tree* nil)
(define-alien-variable codeblob-freelist unsigned)

;;; For the immobile code allocator:
;;; * Insertion performs low-level allocate, then tree-insert
;;; * Deletion performs tree-delete, then low-level deallocate
;;; Invariants:
;;; - a linear space walk visits at least all the objects in the tree, possibly more
;;; - tree does not contain any key that is not the base address of an object
;;; - if a tree node points to an object, then that object is either code
;;;   or a filler that has not been returned to the low-level allocator

;;; Enforce limit on boxed words based on maximum total number of words
;;; that can be indicated in the header for 32-bit words.
;;; 22 bits = 4MiB, quite generous for one code object.
;;;   (I think the reasoning behind 22 is that after adding 8 for the widetag,
;;;    it's 30, and the other 2 bits are the fixnum tag)
;;; 25 bits is the maximum unboxed size expressed in bytes,
;;; if n-word-bytes = 8 and almost the entire code object is unboxed
;;; which is, practically speaking, not really possible.
(declaim (ftype (sfunction (t (unsigned-byte 22) (unsigned-byte 25))
                           (values code-component (integer 0)))
                allocate-code-object))

(defun update-dynamic-space-code-tree (obj &optional generation-test)
  (with-pinned-objects (obj)
    (let ((addr (logandc2 (get-lisp-obj-address obj) other-pointer-lowtag))
          (tree *dynspace-codeblob-tree*))
      (loop (let ((newtree (sb-brothertree:insert addr tree)))
              ;; check that it hasn't been promoted from gen0 -> gen1 already
              ;; (very unlikely, but certainly possible).
              (when (and generation-test (not (eql (generation-of obj) 0))) (return))
              (let ((oldval (cas *dynspace-codeblob-tree* tree newtree)))
                (if (eq oldval tree) (return) (setq tree oldval))))))))

;;; Allocate a code component with BOXED words in the header
;;; followed by UNBOXED bytes of raw data.
;;; BOXED must be the exact count of boxed words desired. No adjustments
;;; are made for alignment considerations or the fixed slots.
(defun allocate-code-object (space boxed unboxed)
  (declare (ignorable space))
  (let* ((total-words
           (the (unsigned-byte 22) ; Enforce limit on total words as well
                (align-up (+ boxed (ceiling unboxed n-word-bytes)) 2))))
    #+immobile-code
    (when (member space '(:immobile :auto))
      (let (addr code holder)
        ;; CODE needs to have a heap or TLS reference to it prior to adding it to the tree
        ;; since implicit pinning uses the tree to find pinned ojects.
        (declare (special holder))
        (with-alien ((tlsf-alloc-codeblob (function unsigned system-area-pointer unsigned)
                                          :extern)
                     (tlsf-control system-area-pointer :extern))
          (with-system-mutex (*allocator-mutex* :without-gcing t)
            (unless (zerop (setq addr (alien-funcall tlsf-alloc-codeblob
                                                     tlsf-control total-words)))
              (setf code (%make-lisp-obj (logior addr other-pointer-lowtag))
                    holder code))))
        ;; GC is allowed to run now because HOLDER references CODE
        (when code
          (alien-funcall (extern-alien "memset" (function void system-area-pointer int unsigned))
                         (sap+ (int-sap addr) n-word-bytes) 0 (ash (1- boxed) word-shift))
          ;; BOXED-SIZE is a raw slot holding a byte count, but SET-SLOT takes its VALUE
          ;; arg as a descriptor-reg, so just cleverly make it right by shifting.
          (%primitive set-slot code (ash boxed (- word-shift n-fixnum-tag-bits))
                      '(setf %code-boxed-size) code-boxed-size-slot other-pointer-lowtag)
          (aver (= (sap-ref-8 (int-sap addr) 0) code-header-widetag)) ; wasn't trashed
          (let ((tree *immobile-codeblob-tree*))
            (loop (when (eq tree (setq tree (cas *immobile-codeblob-tree* tree
                                                 (sb-brothertree:insert addr tree))))
                    (return-from allocate-code-object (values code total-words)))))))
      (when (eq space :immobile)
        (error "Immobile code space exhausted")))
    (let ((code
           ;; x86-64 has a vop which wraps pseudo-atomic around the foreign call,
           ;; as is the custom for allocation trampolines.
           #+x86-64 (%primitive alloc-code total-words boxed)
           #-x86-64
           (without-gcing
               (%make-lisp-obj
                (alien-funcall (extern-alien "alloc_code_object"
                                             (function unsigned (unsigned 32) (unsigned 32)))
                               total-words boxed)))))
      ;; Small objects are always findable given an interior pointer,
      ;; but large objects require the tree.
      (when (and (neq (heap-allocated-p code) :dynamic)
                 (> total-words (/ smlgc-blocksize-max n-word-bytes)))
        (let ((tree *immobile-codeblob-tree*)
              (addr (logandc2 (get-lisp-obj-address code) lowtag-mask)))
          (loop (let ((newtree (sb-brothertree:insert addr tree)))
                  (when (eq tree (setf tree (cas *immobile-codeblob-tree* tree newtree)))
                    (return)))))
        (return-from allocate-code-object (values code total-words)))
      (update-dynamic-space-code-tree code)
    ;; FIXME: there may be random values in the unboxed payload and it's not obvious
    ;; that all callers of ALLOCATE-CODE-OBJECT always write all raw bytes.
    ;; LOAD-CODE and MAKE-CORE-COMPONENT certainly do because the representation
    ;; of simple-funs requires that the final 2 bytes be aligned to the physical end
    ;; of the object so that we can find the function table.
    ;; But what about other things that create code objects?
    ;; It could be a subtle source of nondeterministic core images.
      (values code total-words))))

;; The freelist can only be read while holding a mutex because the codeblobs
;; themselves are used to construct the freelist. It would be dangerous to
;; suppose that you could read the "link" field out of a blob (that currently
;; has FILLER_WIDETAG allegedly) if another thread got scheduled such that it
;; freed that filler and the reallocated into it instantly.
(defun immobile-code-dealloc-1 (scratchpad)
  (declare (cons scratchpad) (ignorable scratchpad))
  #+immobile-code
  (flet ((pop-1 ()
           ;; Remove an item from codeblob-freelist. Mutex must be held
           ;; and GC inhibited.
           (let ((head codeblob-freelist))
             (unless (eql head 0)
               (setf codeblob-freelist (sap-ref-word (int-sap head) n-word-bytes)))
             (%make-lisp-obj head)))) ; use funny fixnum representation
    (when (eql (car scratchpad) 0)
      ;; I'd prefer that this be done using WITH-SYSTEM-MUTEX + pseudo-atomic
      ;; rather than WITHOUT-GCING in as much as WITHOUT-GCING needs to cease to exist.
      (let ((word (if (eql codeblob-freelist 0)
                      0
                      (with-system-mutex (*allocator-mutex* :without-gcing t) (pop-1)))))
        (when (eql word 0)
          (return-from immobile-code-dealloc-1 nil))
        (setf (car scratchpad) word)))
    (let ((addr (get-lisp-obj-address (car scratchpad))))
      ;; We have to remove from the tree before removing from the TLSF pool,
      ;; because presence of a key in the tree is an assertion that there is
      ;; memory block at that address. Consider if we removed from the pool
      ;; without removing from the tree, the block could be coalesced on either side
      ;; and there would not necessarily be a block where the tree says it is.
      (let ((tree *immobile-codeblob-tree*))
        (loop (when (eq tree (setq tree (cas *immobile-codeblob-tree* tree
                                             (sb-brothertree:delete addr tree))))
                (return))))
      (with-alien ((tlsf-unalloc-codeblob (function void system-area-pointer unsigned)
                                          :extern)
                   (tlsf-control system-area-pointer :extern))
        ;; Prevent GC from walking the text space at the exact same instant
        ;; the block coalescing algorithm alters block headers.
        (with-system-mutex (*allocator-mutex* :without-gcing t)
          (alien-funcall tlsf-unalloc-codeblob tlsf-control addr)
          (setf (car scratchpad) (pop-1)))))
    t))

(define-alien-variable msegs-pending-free unsigned)

#|
(defglobal *large-object-hashset* nil)
(defun largeobj-hs-insert (node object)
  ;; NODE is an instance, OBJECT is anything.
  (let ((addr (%make-lisp-obj (logandc2 (get-lisp-obj-address object) lowtag-mask))))
    (sb-lockless:%so-eq-set-phase1-insert *large-object-hashset* node addr)))

(defun largeobj-hs-maybe-rehash (node)
  (sb-lockless:%so-eq-set-phase2-insert *large-object-hashset* node))
|#

;; GC-freed malloc-segments are chained through word 0 which happens to correspond
;; to the 'as_list' slot in the C structure definition. The actual deallocation
;; is deferred to Lisp so that we can remove from the lookup tables.
(defun release-malloc-segments (&aux (count 0))
  (flet ((segment-pop ()
           (named-let retry ((mseg msegs-pending-free))
             (if (zerop mseg)
                 0
                 (let* ((next (sap-ref-word (int-sap mseg) 0))
                        (actual (cas msegs-pending-free mseg next)))
                   (if (= actual mseg)
                       (%make-lisp-obj mseg)
                       (retry actual)))))))
    (loop
      (let ((mseg (truly-the fixnum (segment-pop))))
        (when (zerop mseg)
          #+nil
          (let ((s "Released %d large code blocks"))
            (alien-funcall (extern-alien "tprintf" (function void int system-area-pointer int))
                           1 (vector-sap s) count))
          (return))
        (let* ((mseg-sap (descriptor-sap mseg))
               (object-sap (sap+ mseg-sap smlgc-mseg-overhead-bytes))
               (addr (truly-the (unsigned-byte 56) (sap-int object-sap))))
          ;; the header was copied to wordindex 1 and then zeroized
          (aver (= (widetag@baseptr (sap+ object-sap n-word-bytes))
                   code-header-widetag))
          (let ((tree *immobile-codeblob-tree*))
            (loop (when (eq tree (setq tree (cas *immobile-codeblob-tree* tree
                                                 (sb-brothertree:delete addr tree))))
                    (return))))
          (sb-thread:barrier (:write))
          (with-alien ((delete-code-mseg (function void system-area-pointer) :extern))
            (alien-funcall delete-code-mseg mseg-sap))
          (incf count))))))

;; Display but do not pop them off the list
(defun show-releasable-malloc-segments ()
  (let ((seg msegs-pending-free))
    (loop (when (zerop seg) (return))
          ;; show 4 words of the metadata, then the lispobj header word (which should be 0)
          ;; and the word after that (which gets the header word before clobbering)
          (let ((sap (int-sap seg)))
            (format t "@ ~X:~{ ~16X~}~%"
                    seg (loop for i below 6 collect (sap-ref-word sap (ash i word-shift))))
            (setq seg (sap-ref-word sap 0))))))

#+nil
(defun call-with-sml#gc (which thunk)
  (if (listp which) (apply #'use-smlgc which) (use-smlgc which))
  (prog1 (funcall thunk)
    (setq *use-smlgc* 0)))

#+nil
(defmacro with-sml#-allocator (&body body)
  `(progn (setq *use-smlgc* -1)
          (alien-funcall (extern-alien "enable_cms_cons" (function void)))
          (multiple-value-prog1 (progn ,@body)
            (alien-funcall (extern-alien "disable_cms_cons" (function void)))
            (setq *use-smlgc* 0))))

#+nil(defun sml#-cons (a b) (with-sml#-allocator (cons a b)))

#+nil
(defun force-barrier-on ()
  (let ((phaseptr (find-dynamic-foreign-symbol-address "fake_phaseptr")))
    (let* ((vmthread (current-thread-offset-sap thread-this-slot)))
      (setf (sap-ref-word vmthread (ash thread-gc-phaseptr-slot word-shift))
            phaseptr))))

#+nil
(defun call-with-barrier-forced-on (thunk)
  (let ((phaseptr (find-dynamic-foreign-symbol-address "fake_phaseptr")))
    (let* ((vmthread (current-thread-offset-sap thread-this-slot))
           (actual-phaseptr (sap-ref-word vmthread (ash thread-gc-phaseptr-slot word-shift))))
      (setf (sap-ref-word vmthread (ash thread-gc-phaseptr-slot word-shift))
            phaseptr)
      (multiple-value-prog1
          (funcall thunk)
        (setf (sap-ref-word vmthread (ash thread-gc-phaseptr-slot word-shift))
              actual-phaseptr)))))

(defun showlarge ()
;  (sb-lockless::show-address-based-list sb-vm::*large-object-hashset*)
  )

#+nil
(defun careful-obj-to-malloc-segment (addr)
  (alien-funcall (extern-alien "careful_obj_to_malloc_segment"
                               (function system-area-pointer unsigned))
                 addr))

#+nil
(defun assert-all-large-simple-funs-findable ()
  (let ((list (sb-brothertree::codeblob-tree-to-list sb-vm::*immobile-codeblob-tree*)))
    (dolist (c list)
      (let* ((code-base-addr (logandc2 (get-lisp-obj-address c) lowtag-mask))
             (mseg (int-sap (- code-base-addr smlgc-mseg-overhead-bytes))))
        (flet ((test (obj &aux (addr (logandc2 (get-lisp-obj-address obj)
                                               lowtag-mask)))
                 ;; every lowtag wouldn't be needed but it's an OK thing to test
                 (dotimes (lowtag 16)
                   (assert (sap= (careful-obj-to-malloc-segment (logior addr lowtag))
                                 mseg)))))
          (test c)
          (dotimes (index (code-n-entries c))
            (test (%code-entry-point c index))))))))

(defun show-bitmap-aps ()
  (loop for log2size from 4 to 12
        for slot from thread-ap4-slot by 4
        do
     (format t "~2d ~16x ~8x ~16x ~4x~%"
             log2size
             (sap-int (current-thread-offset-sap slot))
             (sap-int (current-thread-offset-sap (+ 1 slot)))
             (sap-int (current-thread-offset-sap (+ 2 slot)))
             (sap-int (current-thread-offset-sap (+ 3 slot))))))

(defvar *stats*)

(defun get-consing-stats ()
  (setq *stats* (sb-kernel:make-lisp-obj (sb-vm::static-data-collection-vector)))
  ;; element 0  : number of &REST lists of length 16 or more
  ;; elt 1..15  : packed integer
  ;;              hi = number of &REST lists of length N using slow path
  ;;              lo = number of &REST lists of length N
  ;; elt 16..25 : number of lists composed of N cons cells
  (values (loop for i from 16 by 2 repeat 6
                collect (let* ((total (aref *stats* i))
                               (slow (aref *stats* (1+ i)))
                               (fast (- total slow)))
                          (cons total fast)))
          (append (loop for i from 1 to 15
                        collect
                        (let* ((element (aref *stats* i))
                               (slow (ldb (byte 32 32) element))
                               (total (ldb (byte 32 0) element))
                               (fast (- total slow)))
                          (cons total fast)))
                  (list (cons (aref *stats* 0) 0)))))

(defun print-consing-stats ()
  (multiple-value-bind (lists rest-lists) (get-consing-stats)
    (format t "~&List consing:~%")
    (let ((sum (reduce #'+ (mapcar 'car lists))))
      (loop for (count . n-fast) in lists
            for label across #("1" "2" "3" "4" "5" ">")
            do (format t " ~A | ~12d ~6,2,2f% ~6,2,2f%~%"
                       label
                       count
                       (/ count sum) ; percentage of total
                       (cond ((string= label ">") 0)
                             ((plusp count) (/ n-fast count))))))
    (format t "~&&REST consing:~%")
    (let ((sum (reduce #'+ (mapcar 'car rest-lists))))
      (loop for (count . n-fast) in rest-lists
            for label across #(" 1" " 2" " 3" " 4" " 5" " 6" " 7" " 8" " 9"
                               "10" "11" "12" "13" "14" "15" " >")
            when (plusp count)
            do (format t "~A | ~12d ~6,2,2f% ~6,2,2f%~%"
                       label
                       count
                       (/ count sum) ; percentage of total
                       (cond ((string= label " >") 0)
                             ((plusp count) (/ n-fast count))))))))

(defun sayhello ()
  (let ((s #.(format nil "Hey there~%")))
    (sb-sys:with-pinned-objects (s)
      (alien-funcall (extern-alien "printf" (function void system-area-pointer))
                     (sb-sys:vector-sap s)))))
(defun hellothread ()
  (sb-thread:make-thread #'sayhello))

#|
(defun allocate-smlgc-cons ()
  (let ((sap (int-sap (SB-THREAD::THREAD-PRIMITIVE-THREAD sb-thread:*current-thread*))))
    (setf (sap-ref-32 sap (ash (1+ thread-ap4-slot) word-shift))
|#
