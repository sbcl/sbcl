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
                (sap-ref-8 pointer #+big-endian (1- sb-vm:n-word-bytes)
                                   #+little-endian 0)
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

(define-alien-variable varyobj-space-size (unsigned 32))
(define-alien-variable ("FIXEDOBJ_SPACE_START" fixedobj-space-start) unsigned-long)
(define-alien-variable ("VARYOBJ_SPACE_START" varyobj-space-start) unsigned-long)
(define-alien-variable ("varyobj_free_pointer" *varyobj-space-free-pointer*)
  system-area-pointer)
(define-alien-variable ("fixedobj_free_pointer" *fixedobj-space-free-pointer*)
  system-area-pointer)

(eval-when (:compile-toplevel)
  (assert (eql code-boxed-size-slot 1))
  (assert (eql code-debug-info-slot 2)))

(define-alien-variable "varyobj_holes" long)
(define-alien-variable "varyobj_page_touched_bits" (* (unsigned 32)))
(define-alien-variable "varyobj_pages" (* (unsigned 32)))
(define-alien-routine "find_preceding_object" long (where long))

;;; Lazily created freelist, used only when unallocate is called:
;;; A cons whose car is a sorted list of hole sizes available
;;; and whose cdr is a hashtable.
;;; The keys in the hashtable are hole sizes, values are lists of holes.
;;; A better structure would be just a sorted array of sizes
;;; with each entry pointing to the holes which are threaded through
;;; some bytes in the storage itself rather than through cons cells.
(define-load-time-global *immobile-freelist* nil)

;;; Return the zero-based index within the varyobj subspace of immobile space.
(defun varyobj-page-index (address)
  (declare (type (and fixnum unsigned-byte) address))
  (values (floor (- address varyobj-space-start) immobile-card-bytes)))

(defun varyobj-page-address (index)
  (+ varyobj-space-start (* index immobile-card-bytes)))

(declaim (inline (setf varyobj-page-scan-start-offset)))
(defun (setf varyobj-page-scan-start-offset) (newval index)
  ;; NEWVAL is passed in as a byte count but we want to store it as doublewords
  ;; so it needs right-shifting by 1+ word-shift. However because it is a field
  ;; of a packed word, it needs left-shifting by 8. We can shift by the net amount
  ;; provided that no zero bits would be right-shifted out.
  (aver (zerop (logand newval lowtag-mask)))
  (setf (deref varyobj-pages index)
        (logior (ash newval (- 8 (1+ sb-vm:word-shift)))
                (logand (deref varyobj-pages index) #xFF)))
  newval)

;;; Convert a zero-based varyobj page index into a scan start address.
(defun varyobj-page-scan-start (index)
  (- (+ varyobj-space-start (* (1+ index) immobile-card-bytes))
     (* 2 n-word-bytes (ash (deref varyobj-pages index) -8))))

(declaim (inline hole-p))
(defun hole-p (raw-address)
  ;; A code header with 0 boxed words is a hole.
  ;; See also CODE-OBJ-IS-FILLER-P
  (eql (sap-ref-64 (int-sap raw-address) 0) code-header-widetag))

(defun freed-hole-p (address)
  (and (hole-p address)
       ;; A hole is not considered to have been freed until it is
       ;; no longer in the chain of objects linked through
       ;; the debug_info slot.
       (eql (sap-ref-word (int-sap address)
                          (ash code-debug-info-slot word-shift))
            nil-value)))

(declaim (inline hole-size))
(defun hole-size (hole-address) ; in bytes
  (ash (sap-ref-32 (int-sap hole-address) 4) word-shift))

(declaim (inline (setf hole-size)))
(defun (setf hole-size) (new-size hole) ; NEW-SIZE is in bytes
  (setf (sap-ref-32 (int-sap hole) 4) (ash new-size (- word-shift)))
  new-size)

(declaim (inline hole-end-address))
(defun hole-end-address (hole-address)
  (+ hole-address (hole-size hole-address)))

(defun sorted-list-insert (item list key-fn)
  (declare (function key-fn))
  (let ((key (funcall key-fn item)) (tail list) prev)
    (loop
     (when (null tail)
       (let ((new-tail (list item)))
         (return (cond ((not prev) new-tail)
                       (t (setf (cdr prev) new-tail) list)))))
     (let ((head (car tail)))
       (when (< key (funcall key-fn head))
         (rplaca tail item)
         (rplacd tail (cons head (cdr tail)))
         (return list)))
     (setq prev tail tail (cdr tail)))))

;;; These routines are not terribly efficient, but very straightforward
;;; since we can assume the existence of hashtables.
(defun add-to-freelist (hole)
  (let* ((size (hole-size hole))
         (freelist *immobile-freelist*)
         (table (cdr freelist))
         (old (gethash (hole-size hole) table)))
    ;; Check for double-free error
    #+immobile-space-debug (aver (not (member hole (gethash size table))))
    (unless old
      (setf (car freelist)
            (sorted-list-insert size (car freelist) #'identity)))
    (setf (gethash size table) (cons hole old))))

(defun remove-from-freelist (hole)
  (let* ((key (hole-size hole))
         (freelist *immobile-freelist*)
         (table (cdr freelist))
         (list (gethash key table))
         (old-length (length list))
         (new (delete hole list :count 1)))
    (declare (ignorable old-length))
    #+immobile-space-debug (aver (= (length new) (1- old-length)))
    (cond (new
           (setf (gethash key table) new))
          (t
           (setf (car freelist) (delete key (car freelist) :count 1))
           (remhash key table)))))

(defun find-in-freelist (size test)
  (let* ((freelist *immobile-freelist*)
         (hole-size
          (if (eq test '<=)
              (let ((sizes (member size (car freelist) :test '<=)))
                (unless sizes
                  (return-from find-in-freelist nil))
                (car sizes))
              size))
         (found (car (gethash hole-size (cdr freelist)))))
    (when found
      (remove-from-freelist found))
    found))

(defun set-varyobj-space-free-pointer (free-ptr)
  (declare (type (and fixnum unsigned-byte) free-ptr))
  (setq *varyobj-space-free-pointer* (int-sap free-ptr))
  ;; When the free pointer is not page-aligned - it usually won't be -
  ;; then we create an unboxed array from the pointer to the page end
  ;; so that it appears as one contiguous object when scavenging.
  ;; instead of a bunch of cons cells.
  (when (logtest free-ptr (1- immobile-card-bytes))
    (let ((n-trailing-bytes
           (- (nth-value 1 (ceiling free-ptr immobile-card-bytes)))))
      (setf (sap-ref-word (int-sap free-ptr) 0) simple-array-fixnum-widetag
            (%array-fill-pointer
             (%make-lisp-obj (logior free-ptr sb-vm:other-pointer-lowtag)))
            ;; Convert bytes to words, subtract the header and vector length.
            (- (ash n-trailing-bytes (- word-shift)) 2)))))

(defun unallocate (hole)
  #+immobile-space-debug
  (awhen *in-use-bits* (mark-range it hole (hole-size hole) nil))
  (let* ((hole-end (hole-end-address hole))
         (end-is-free-ptr (eql hole-end (sap-int *varyobj-space-free-pointer*))))
    ;; First, ensure that no page's scan-start points to this hole.
    ;; For smaller-than-page objects, this will do nothing if the hole
    ;; was not the scan-start. For larger-than-page, we have to update
    ;; a range of pages. Example:
    ;;   |  page1 |  page2 |  page3 |  page4  |
    ;;        |-- hole A ------ | -- hole B --
    ;; If page1 had an object preceding the hole, then it is not empty,
    ;; but if it pointed to the hole, and the hole extended to the end
    ;; of the first page, then that page is empty.
    ;; Pages (1+ first-page) through (1- last-page) inclusive
    ;; must become empty. last-page may or may not be depending
    ;; on whether another object can be found on it.
    (let ((first-page (varyobj-page-index hole))
          (last-page (varyobj-page-index (1- hole-end))))
      (when (and (eql (varyobj-page-scan-start first-page) hole)
                 (< first-page last-page))
        (setf (varyobj-page-scan-start-offset first-page) 0))
      (loop for page from (1+ first-page) below last-page
            do (setf (varyobj-page-scan-start-offset page) 0))
      ;; Only touch the offset for the last page if it pointed to this hole.
      ;; If the following object is a hole that is in the pending free list,
      ;; it's ok, but if it's a hole that is already in the freelist,
      ;; it's not OK, so look beyond that object. We don't have to iterate,
      ;; since there can't be two consecutive holes - so it's either the
      ;; object after this hole, or the one after that.
      (when (eql (varyobj-page-scan-start last-page) hole)
        (let* ((page-end (varyobj-page-address (1+ last-page)))
               (new-scan-start (cond (end-is-free-ptr page-end)
                                     ((freed-hole-p hole-end)
                                      (hole-end-address hole-end))
                                     (t hole-end))))
          (setf (varyobj-page-scan-start-offset last-page)
                (if (< new-scan-start page-end)
                    ;; Compute new offset backwards relative to the page end.
                    (- page-end new-scan-start)
                    0))))) ; Page becomes empty

    (unless *immobile-freelist*
      (setf *immobile-freelist* (cons nil (make-hash-table :test #'eq))))

    ;; find-preceding is the most expensive operation in this sequence
    ;; of steps. Not sure how to improve it, but I doubt it's a problem.
    (let* ((predecessor (find-preceding-object hole))
           (pred-is-free (and (not (eql predecessor 0))
                              (freed-hole-p predecessor))))
      (when pred-is-free
        (remove-from-freelist predecessor)
        (setf hole predecessor))
      (when end-is-free-ptr
        ;; Give back space below the free pointer for better space conservation.
        ;; Consider when the hole touching the free pointer is equal in size
        ;; to another hole that could have been used instead. Taking space at
        ;; the free pointer diminishes the opportunity to use the frontier
        ;; to later allocate a larger object that would not have fit
        ;; into any existing hole.
        (set-varyobj-space-free-pointer hole)
        (return-from unallocate))
      (let* ((successor hole-end)
             (succ-is-free (freed-hole-p successor)))
        (when succ-is-free
          (setf hole-end (hole-end-address successor))
          (remove-from-freelist successor)))
      ;; The hole must be an integral number of doublewords.
      (aver (not (logtest (- hole-end hole) lowtag-mask)))
      (setf (hole-size hole) (- hole-end hole))))
  (add-to-freelist hole))

(defun alloc-immobile-code (n-bytes word0 word1 lowtag errorp)
  (declare (type (and fixnum unsigned-byte) n-bytes))
  (setq n-bytes (align-up n-bytes (* 2 n-word-bytes)))
  ;; Can't allocate fewer than 4 words due to min hole size.
  (aver (>= n-bytes (* 4 n-word-bytes)))
  (with-system-mutex (*allocator-mutex* :without-gcing t)
   (unless (zerop varyobj-holes)
     ;; If deferred sweep needs to happen, do so now.
     ;; Concurrency could potentially be improved here: at most one thread
     ;; should do this step, but it doesn't need to be exclusive with GC
     ;; as long as we can atomically pop items off the list of holes.
     (let ((hole-addr varyobj-holes))
       (setf varyobj-holes 0)
       (loop
        (let ((next (sap-ref-word (int-sap hole-addr)
                                  (ash code-debug-info-slot word-shift))))
          (setf (sap-ref-word (int-sap hole-addr)
                              (ash code-debug-info-slot word-shift))
                nil-value)
          (unallocate hole-addr)
          (if (eql (setq hole-addr next) 0) (return))))))
   (let* ((residual)
          (shrunk-size)
          (addr
           (or (and *immobile-freelist*
                    (or (find-in-freelist n-bytes '=) ; 1. Exact match?
                        ;; 2. Try splitting a hole, adding some slack so that
                        ;;    both pieces can potentially be used.
                        (let ((found (find-in-freelist (+ n-bytes 192) '<=)))
                          (when found
                            (let* ((actual-size (hole-size found))
                                   (remaining (- actual-size n-bytes)))
                              (aver (not (logtest actual-size lowtag-mask)))
                              (setq residual found ; Shorten the lower piece
                                    shrunk-size remaining)
                              (+ found remaining)))))) ; Consume the upper piece
               ;; 3. Extend the frontier.
               (let* ((addr (sap-int *varyobj-space-free-pointer*))
                      (free-ptr (+ addr n-bytes))
                      (limit (+ varyobj-space-start varyobj-space-size)))
                 (when (> free-ptr limit)
                   (cond (errorp
                          (format t "~&Immobile space exhausted~%")
                          (sb-debug:print-backtrace)
                          (sb-impl::%halt))
                         (t
                          (return-from alloc-immobile-code nil))))
                 (set-varyobj-space-free-pointer free-ptr)
                 addr))))
     (aver (not (logtest addr lowtag-mask))) ; Assert proper alignment
     ;; Compute the start and end of the first page consumed.
     (let* ((page-start (logandc2 addr (1- immobile-card-bytes)))
            (page-end (+ page-start immobile-card-bytes))
            (index (varyobj-page-index addr))
            (obj-end (+ addr n-bytes)))
       ;; Mark the page as being used by a nursery object.
       (setf (deref varyobj-pages index) (logior (deref varyobj-pages index) 1))
       ;; On the object's first page, set the scan start only if addr
       ;; is lower than the current page-scan-start object.
       ;; Note that offsets are expressed in doublewords backwards from
       ;; page end, so that we can direct the scan start to any doubleword
       ;; on the page or in the preceding 256MiB (approximately).
       (when (< addr (varyobj-page-scan-start index))
         (setf (varyobj-page-scan-start-offset index) (- page-end addr)))
       ;; On subsequent pages, always set the scan start, since there can not
       ;; be a lower-addressed object touching those pages.
       (loop
        (setq page-start page-end)
        (incf page-end immobile-card-bytes)
        (incf index)
        (when (>= page-start obj-end) (return))
        (setf (varyobj-page-scan-start-offset index) (- page-end addr))))
     #+immobile-space-debug ; "address sanitizer"
     (awhen *in-use-bits* (mark-range it addr n-bytes t))
     (setf (sap-ref-word (int-sap addr) 0) word0
           (sap-ref-word (int-sap addr) n-word-bytes) word1)
     ;; 0-fill the remainder of the object
     (alien-funcall (extern-alien "memset" (function void system-area-pointer int unsigned))
                    (sap+ (int-sap addr) (* 2 n-word-bytes)) 0 (- n-bytes (* 2 n-word-bytes)))
     ;; Only after making the new object can we reduce the size of the hole
     ;; that contained the new allocation (if it entailed chopping a hole
     ;; into parts). In this way, heap scans do not read junk.
     (when residual
       (setf (hole-size residual) shrunk-size)
       (add-to-freelist residual))
     ;; The object is live despite not having a tagged pointer yet nor
     ;; this code being pseudoatomic, because the mutex acquire has
     ;; :WITHOUT-GCING. Ideally we'd have some notion of hazard pointers
     ;; that could prevent GC from evicting objects from pointed-to pages
     ;; so that we needn't inhibit GC.
     (%make-lisp-obj (logior addr lowtag)))))

;;; Size-class segregation (implying which page we try to allocate to)
;;; is done from lisp now, not C. There are 4 objects types we'll see,
;;; each in its own size class (even if some are coincidentally the same size).
;;;  - Symbols
;;;  - FDEFNs
;;;  - Layouts
;;;  - Funcallable instances
;;; The first two are truly fixed in size. The latter two could occur in several
;;; sizes, each occupying a different size-class. This isn't implemented yet.
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
                   (t (error "Oversized layout"))))
            ;; TODO: allow different sizes of funcallable-instance
            (#.funcallable-instance-widetag 8))))
    (values (%primitive alloc-immobile-fixedobj
                        size-class
                        aligned-nwords
                        header))))

(defun make-immobile-symbol (name)
  (let ((symbol (truly-the symbol
                 (alloc-immobile-fixedobj
                  symbol-size
                  (logior (ash (1- symbol-size) n-widetag-bits) symbol-widetag)))))
    ;; symbol-hash was initialized to 0
    (%set-symbol-global-value symbol (make-unbound-marker))
    (%primitive sb-vm::set-slot symbol nil
                'make-symbol sb-vm:symbol-info-slot sb-vm:other-pointer-lowtag)
    (%primitive sb-vm::set-slot symbol name
                'make-symbol sb-vm:symbol-name-slot sb-vm:other-pointer-lowtag)
    (%set-symbol-package symbol nil)
    symbol))

) ; end PROGN

(declaim (inline immobile-space-addr-p))
(defun immobile-space-addr-p (addr)
  (declare (type word addr) (ignorable addr))
  #+immobile-space
  (or (let ((start fixedobj-space-start))
        (<= start addr (truly-the word (+ start (1- fixedobj-space-size)))))
      (let ((start varyobj-space-start))
        (<= start addr (truly-the word (+ start (1- varyobj-space-size)))))))

(defun immobile-space-obj-p (obj)
  (immobile-space-addr-p (get-lisp-obj-address obj)))

;;; Enforce limit on boxed words based on maximum total number of words
;;; that can be indicated in the header for 32-bit words.
;;; 22 bits = 4MiB, quite generous for one code object.
;;; 25 bits is the maximum unboxed size expressed in bytes,
;;; if n-word-bytes = 8 and almost the entire code object is unboxed
;;; which is, practically speaking, not really possible.
(declaim (ftype (sfunction (t (unsigned-byte 16) (unsigned-byte 22) (unsigned-byte 25))
                           code-component)
                allocate-code-object))
;;; Allocate a code component with BOXED words in the header
;;; followed by UNBOXED bytes of raw data.
;;; BOXED must be the exact count of boxed words desired. No adjustments
;;; are made for alignment considerations or the fixed slots.
(defun allocate-code-object (space n-named-calls boxed unboxed)
  (declare (ignorable space n-named-calls))
  (let* ((total-words
           (the (unsigned-byte 22) ; Enforce limit on total words as well
                (align-up (+ boxed (ceiling unboxed n-word-bytes)) 2)))
         (code
           #+gencgc
           (or #+immobile-code
               (when (member space '(:immobile :auto))
                 ;; We don't need to inhibit GC here - ALLOC-IMMOBILE-CODE does it.
                 ;; Indicate that there are initially 2 boxed words, otherwise
                 ;; immobile space GC thinks this object is freeable.
                 (alloc-immobile-code (ash total-words word-shift)
                                        (logior (ash total-words code-header-size-shift)
                                                code-header-widetag)
                                        (ash 2 n-fixnum-tag-bits)
                                        other-pointer-lowtag
                                        (eq space :immobile)))
               ;; x86-64 has a vop which wraps pseudo-atomic around the foreign call,
               ;; as is the custom for allocation trampolines.
               #+x86-64 (%primitive sb-vm::alloc-code total-words)
               #-x86-64
               (without-gcing
                 (%make-lisp-obj
                  (alien-funcall (extern-alien "alloc_code_object"
                                               (function unsigned (unsigned 32)))
                                 total-words))))
           #+cheneygc
           (%primitive var-alloc total-words 'alloc-code
                       ;; subtract 1 because var-alloc always adds 1 word
                       ;; for the header, which is not right for code objects.
                       -1 code-header-widetag other-pointer-lowtag nil)))

    (with-pinned-objects (code)
      (let ((sap (sap+ (int-sap (get-lisp-obj-address code))
                       (- other-pointer-lowtag))))
        ;; The immobile space allocator pre-zeroes, and also it needs a nonzero
        ;; value in the boxed word count because otherwise it looks like
        ;; an immobile space page filler. So don't do any more zeroing there.
        ;; (Could dead immobile objects be converted to use FILLER-WIDETAG instead?)
        (unless (immobile-space-obj-p code)
          ;; Before writing the boxed word count, zeroize up to and including 1 word
          ;; after the boxed header so that all pointers can be safely read by GC,
          ;; and so that the jump table count word is 0.
          (loop for byte-index from (ash boxed word-shift) downto (ash 2 word-shift)
                by n-word-bytes
                do (setf (sap-ref-word-jit sap byte-index) 0)))
        ;; The 1st slot beyond the header stores the boxed header size in bytes
        ;; as an untagged number, which has the same representation as a tagged
        ;; value denoting a word count if WORD-SHIFT = N-FIXNUM-TAG-BITS.
        ;; This boxed-size MUST be 0 prior to writing any pointers into the object
        ;; because the boxed words will not necessarily have been pre-zeroed;
        ;; scavenging them prior to zeroing them out would see wild pointers.
        (setf (sap-ref-word-jit sap (ash code-boxed-size-slot word-shift))
              ;; For 32-bit words, we'll have to add another primitive-object slot.
              ;; But so far nothing makes use of the n-named-calls value.
              (logior #+64-bit (ash n-named-calls 32)
                      (ash boxed word-shift)))))

    ;; FIXME: there may be random values in the unboxed payload and it's not obvious
    ;; that all callers of ALLOCATE-CODE-OBJECT always write all raw bytes.
    ;; LOAD-CODE and MAKE-CORE-COMPONENT certainly do because the representation
    ;; of simple-funs requires that the final 2 bytes be aligned to the physical end
    ;; of the object so that we can find the function table.
    ;; But what about other things that create code objects?
    ;; It could be a subtle source of nondeterministic core images.

    ;; FIXME: Sort out 64-bit and cheneygc.
    #+(and 64-bit cheneygc)
    (setf (code-header-ref code 0)
          (%make-lisp-obj
           (logior (ash total-words 32)
                   sb-vm:code-header-widetag)))
    code))
