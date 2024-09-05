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

(define-load-time-global *allocator-mutex* nil)

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

(define-alien-variable ("text_space_highwatermark" *text-space-free-pointer*)
  system-area-pointer)
#+immobile-space
(progn

(define-alien-variable text-space-size (unsigned 32))
(define-alien-variable ("FIXEDOBJ_SPACE_START" fixedobj-space-start) unsigned-long)
(define-alien-variable ("fixedobj_free_pointer" *fixedobj-space-free-pointer*)
  system-area-pointer)

(eval-when (:compile-toplevel)
  (aver (eql code-boxed-size-slot 1)))

;;; Size-class segregation (implying which page we try to allocate to)
;;; is done from lisp now, not C. There are 2 objects types:
;;;  - Symbols have exactly 1 size-class
;;;  - Layouts have varying size-class
(defun alloc-immobile-fixedobj (nwords header)
  (let* ((widetag (logand (truly-the fixnum header) widetag-mask))
         (aligned-nwords (truly-the fixnum (align-up nwords 2)))
         (size-class
          ;; If you change this, then be sure to update tests/immobile-space.impure
          ;; which hardcodes a size class to not conflict with anything.
          ;; There is too much magic in layout_size_class_nwords for me to
          ;; attempt to rearrange these, that's why "2" is absent below.
          ;; As a practical matter, the largest layout I've ever seen in a real
          ;; application is 20 words (7 words of raw/tagged-slot bitmap),
          ;; so we're not really hurting for more size classes.
          (ecase widetag
            (#.symbol-widetag 1)
            (#.instance-widetag
             (cond ((<= aligned-nwords  8) (setq aligned-nwords  8) 3)
                   ((<= aligned-nwords 16) (setq aligned-nwords 16) 4)
                   ((<= aligned-nwords 24) (setq aligned-nwords 24) 5)
                   ((<= aligned-nwords 32) (setq aligned-nwords 32) 6)
                   ((<= aligned-nwords 48) (setq aligned-nwords 48) 7)
                   (t (error "Oversized layout")))))))
    (values (%primitive !alloc-immobile-fixedobj size-class aligned-nwords header))))

(defun %alloc-immobile-symbol (name)
  (let ((symbol (truly-the symbol
                 (or #+x86-64 (%primitive !fast-alloc-immobile-symbol)
                     (alloc-immobile-fixedobj
                      symbol-size
                      (compute-object-header (1- symbol-size) symbol-widetag))))))
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

(defun update-dynamic-space-code-tree (obj)
  (with-pinned-objects (obj)
    (let ((addr (logandc2 (get-lisp-obj-address obj) lowtag-mask))
          (tree *dynspace-codeblob-tree*))
      (loop (let ((newtree (sb-brothertree:insert addr tree)))
              ;; check that it hasn't been promoted from gen0 -> gen1 already
              ;; (very unlikely, but certainly possible).
              (unless (eq (generation-of obj) 0) (return))
              (let ((oldval (cas *dynspace-codeblob-tree* tree newtree)))
                (if (eq oldval tree) (return) (setq tree oldval))))))))

(defglobal *code-alloc-count* 0) ; frlock: bump once on entry, again on exit
(declaim (fixnum *code-alloc-count*))

;;; Allocate a code component with BOXED words in the header
;;; followed by UNBOXED bytes of raw data.
;;; BOXED must be the exact count of boxed words desired. No adjustments
;;; are made for alignment considerations or the fixed slots.
;;; FIXME: it's not necessary that there be two different locks: *allocator-mutex*
;;; and code_allocator_lock. The Lisp mutex should subsume the C mutex,
;;; and we should acquire the Lisp one around the entire body of this function.
(defun allocate-code-object (space boxed unboxed)
  (declare (ignorable space))
  (let* ((total-words
           (the (unsigned-byte 22) ; Enforce limit on total words as well
                (align-up (+ boxed (ceiling unboxed n-word-bytes)) 2))))
    (atomic-incf *code-alloc-count*)
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
                    (atomic-incf *code-alloc-count*)
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
      (update-dynamic-space-code-tree code)
    ;; FIXME: there may be random values in the unboxed payload and it's not obvious
    ;; that all callers of ALLOCATE-CODE-OBJECT always write all raw bytes.
    ;; LOAD-CODE and MAKE-CORE-COMPONENT certainly do because the representation
    ;; of simple-funs requires that the final 2 bytes be aligned to the physical end
    ;; of the object so that we can find the function table.
    ;; But what about other things that create code objects?
    ;; It could be a subtle source of nondeterministic core images.
      (atomic-incf *code-alloc-count*)
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
      (let ((tree sb-vm::*immobile-codeblob-tree*))
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

#+permgen
(progn
(defun allocate-permgen-symbol (name)
  (with-system-mutex (*allocator-mutex* :without-gcing t)
    (let ((freeptr *permgen-space-free-pointer*))
      (setf *permgen-space-free-pointer*
            (sap+ freeptr (ash symbol-size word-shift)))
      (aver (<= (sap-int *permgen-space-free-pointer*)
                (+ permgen-space-start permgen-space-size)))
      (setf (sap-ref-word freeptr 0) symbol-widetag)
      (setf (sap-ref-lispobj freeptr (ash symbol-name-slot word-shift)) name
            (sap-ref-lispobj freeptr (ash symbol-info-slot word-shift)) nil
            (sap-ref-word freeptr (ash symbol-value-slot word-shift))
            unbound-marker-widetag)
      (%make-lisp-obj (sap-int (sap+ freeptr other-pointer-lowtag))))))
(defun sb-kernel::allocate-permgen-layout (nwords)
  (with-system-mutex (*allocator-mutex* :without-gcing t)
    (let ((freeptr *permgen-space-free-pointer*))
      (setf *permgen-space-free-pointer*
            ;; round-to-odd, add the header word
            (sap+ freeptr (ash (1+ (logior nwords 1)) word-shift)))
      (aver (<= (sap-int *permgen-space-free-pointer*)
                (+ permgen-space-start permgen-space-size)))
      (setf (sap-ref-word freeptr 0)
            (logior (ash nwords instance-length-shift) instance-widetag))
      (%make-lisp-obj (sap-int (sap+ freeptr instance-pointer-lowtag)))))))
