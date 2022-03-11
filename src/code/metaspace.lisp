;;;; Allocator for LAYOUTs in the metadata space a/k/a "metaspace".
;;;; These objects are manually allocated and freed.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;; A slab "size class" holds any object of that size or smaller.
(defconstant-eqx chunk-size-classes ; measured in lispwords
  #.(sb-xc:make-array 8 :initial-contents '(0 8 12 16 24 32 48 64)
                      :element-type '(unsigned-byte 16))
  #'equalp)

;;; The most common layout size is just 8 words:
;;;    word 0: header
;;;    word 1: wrapper
;;;    word 2: clos hash
;;;    word 3: flags
;;;    word 4..6 : ID vector
;;;    word 7: bitmap
;;; Thus the smallest size the slab allocator can dole out is 8 words.

;; The largest size class nust not exceed the physical size of the slab,
;; counting the fixed overhead of 4 words.
;;  nwords | capacity | used  | waste
;;         |          |    (bytes)
;; --------+----------+---------------
;;     8   |    31    |  2016 |    32
;;    12   |    21    |  2048 |     0
;;    16   |    15    |  1952 |    96
;;    24   |    10    |  1952 |    96
;;    32   |     7    |  1824 |   224
;;    48   |     5    |  1952 |    96
;;    64   |     3    |  1472 |   480
;; "Waste" = slab size - overhead - (chunk_size_in_bytes * capacity).

(defun chunk-nwords-to-sizeclass-index (nwords)
  (let ((classes chunk-size-classes))
    (loop for i from 1 below (length classes)
          when (<= nwords (aref classes i))
          do (return i)
          finally (error "Oversized chunk: ~D words" nwords))))

(defun metaspace-sizeclass->nbytes (sizeclass)
  (* (aref chunk-size-classes sizeclass) n-word-bytes))

(define-load-time-global *metaspace-freelists* (make-array 8 :element-type 'word :initial-element 0))
(declaim (type (simple-array word (8)) *metaspace-freelists*))

;;; A completely empty slab is put back in this list for reuse,
;;; and a different chunk size can be chosen for it when next used.
;;; This is a singly-linked list threaded through SLAB-NEXT.
(defmacro metaspace-slab-recycler () '(aref *metaspace-freelists* 0))

;;; A list of memory ranges obtained directly from os_allocate.
;;; A tract is represented by a pointer to its start and a pointer
;;; to the highest address not yet used. Slabs are allocated downwards,
;;; so that when the start and the in-use pointer of a tract collide,
;;; the tract is full.
;;; In typical usage, there will be exactly 1 tract. This is enough room
;;; to hold approximately 32768 instances of LAYOUT, depending on the
;;; size of the layouts stored. It is unlikely that more room would be
;;; needed, but it does work to have a variable number of tracts.
;;;
;;; If at some point we ever figure out how to make both the static space
;;; and metaspace relocatable, and we permanently wire a CPU register to point
;;; to the boundary between the spaces, then the fact that metaspace grows
;;; downward will be halpful in as much as the register will be able to access
;;; a certain amount of either space using small positive or negative offsets.
;;;
;;; Tract: #(base free end next-tract)
(define-load-time-global *metaspace-tracts* nil)
(defmacro tract-base (tract)
  `(aref (truly-the (simple-array word (4)) ,tract) 0))
(defmacro tract-current (tract)
  `(aref (truly-the (simple-array word (4)) ,tract) 1))
(defmacro tract-end (tract)
  `(aref (truly-the (simple-array word (4)) ,tract) 2))

(defmacro do-tracts ((var) &body body)
  `(let ((,var *metaspace-tracts*))
     ;; no iteration yet
     ,@body))

;;; Try to place a tract just below the readonly space, which we know to be
;;; at a good sub-2GiB address that works for the architecture and OS.
;;; TODO: if multiple tracts exist and were allocated consecutively,
;;; can we coalesce them?
(defun os-allocate-metaspace-tract ()
  (error "Can't allocate new metaspace tracts")
  #+nil
  (let* ((request (- (if (null *metaspace-tracts*)
                         sb-vm:read-only-space-start
                         (reduce #'min *metaspace-tracts*
                                 :key (lambda (x) (sap-int (car x)))))
                     metaspace-tract-size))
         (actual (alien-funcall
                  (extern-alien "os_validate" (function system-area-pointer int unsigned unsigned))
                  #+linux 3 ; movable | MAP_32BIT
                  #-linux 1 ; movable
                  request metaspace-tract-size)))
    ;; The tract's representation is (base-address . current-pointer)
    (let ((tract (cons actual (sap+ actual metaspace-tract-size))))
      (push tract *metaspace-tracts*)
      tract)))

;;; Grab a slab out of an available tract and assign it a chunk size
;;; which is represented both as bytes and a class index.
(defun new-metaspace-slab (chunk-size-bytes sizeclass)
  (let ((slab (int-sap (metaspace-slab-recycler))))
    (if (/= (sap-int slab) 0)
        (setf (metaspace-slab-recycler) (sap-int (slab-next slab)))
        (let ((tract *metaspace-tracts*))
          (when (= (tract-base tract) (tract-current tract))
            (setq tract (os-allocate-metaspace-tract)))
          ;; Claim some new space from the tract, below CURRENT and above BASE.
          (let* ((end (int-sap (tract-current tract)))
                 (start (sap+ end (- metaspace-slab-size))))
            (setf (tract-current tract) (sap-int start)
                  slab start))))
    ;; Build a linked list of chunks in the slab. The highest-addressed chunk
    ;; in the slab aligns its end to exactly the end of the slab. In this way
    ;; any unused bytes can be lumped together with the fixed overhead at the
    ;; beginning, versus if the first chunk's start abutted the end of the fixed
    ;; words, then there may be unused space at both ends, and so the math
    ;; for checking valid pointers would get too messy.
    ;; i.e. we need an inquiry operation for "is it a valid chunk?" given any
    ;; random bit pattern, and it must be reliable and efficient.
    (let* ((chunk-end (sap+ slab metaspace-slab-size))
           (chunk (sap+ chunk-end (- chunk-size-bytes))) ; chunk start
           (usable-range-start (slab-usable-range-start slab))
           (capacity 1))
      (setf (sap-ref-word chunk 0) 0) ; terminate the freelist
      (loop (let ((previous (sap+ chunk (- chunk-size-bytes))))
              (when (sap< previous usable-range-start) (return))
              (incf capacity)
              (setf (sap-ref-sap previous 0) chunk
                    chunk-end chunk
                    chunk previous)))
      (init-slab-header slab sizeclass chunk-size-bytes capacity)
      (setf (slab-freelist slab) chunk)
      ;; Insert this as the sole slab for this sizeclass.
      ;; A new slab is never allocated if the sizeclass has any slabs
      ;; with space available.
      (setf (aref *metaspace-freelists* sizeclass) (sap-int slab))
      #+debug (format t "New slab @ ~x, capacity ~d, waste=~Db~% list=~x~%"
                      slab (slab-capacity slab)
                      (- metaspace-slab-size
                         (* slab-overhead-words n-word-bytes)
                         (* (slab-capacity slab) chunk-size-bytes))
                      (slab-freelist-as-list slab))
      slab)))

(defmacro chunk-used-p (chunk)
  `(= (sap-ref-8 ,chunk #+little-endian 0 #+big-endian ,(1- n-word-bytes))
      instance-widetag))

;;; A not-in-use chunk is linked through its 0th word, which will never
;;; resemble a lisp object header word.
(defmacro chunk-next (chunk) `(sap-ref-sap ,chunk 0))

;;; Test whether ADDRESS is a conservative (ambiguous) root.
;;; This algorithm will need to be in C of course.
(defun valid-metaspace-chunk-p (addr)
  (declare (word addr))
  (unless (= (logand addr lowtag-mask) instance-pointer-lowtag)
    (return-from valid-metaspace-chunk-p nil))
  (setq addr (logandc2 addr lowtag-mask))
  (do-tracts (tract)
    (when (and (>= addr (tract-current tract)) (< addr (tract-end tract)))
      ;; Align to containing slab
      (let ((slab (int-sap (logandc2 addr (1- metaspace-slab-size)))))
        (return-from valid-metaspace-chunk-p
          (and (not (zerop (slab-usage slab)))
               ;; Chunks are aligned such that the final chunk touches the
               ;; final word of the slab. Therefore to find a chunk index
               ;; we need to compute the displacement backwards from end.
               (multiple-value-bind (index remainder)
                   (floor (sap- (sap+ slab metaspace-slab-size) (int-sap addr))
                          (slab-chunk-size slab))
                 (and (zerop remainder) ; correctly aligned
                      (<= index (slab-capacity slab)) ; within the slab
                      (chunk-used-p (int-sap addr))))))))))

(defun allocate-metaspace-chunk (nwords)
  (let ((sizeclass (chunk-nwords-to-sizeclass-index nwords)))
    (with-system-mutex (*allocator-mutex*)
      (let* ((slab-addr (aref *metaspace-freelists* sizeclass))
             (slab (if (zerop slab-addr)
                       (new-metaspace-slab (metaspace-sizeclass->nbytes sizeclass)
                                           sizeclass)
                       (int-sap slab-addr)))
             (hole (slab-freelist slab))
             (next-hole (chunk-next hole)))
        (aver (/= (sap-int hole) 0))
        (setf (slab-freelist slab) next-hole)
        (incf (slab-usage slab))
        (when (= (sap-int next-hole) 0)
          ;; Check some invariants - this had to be the head of the list of slabs
          ;; for the sizeclass, and it has to be at max capacity.
          (aver (= 0 (sap-int (slab-prev slab))))
          (aver (= (slab-usage slab) (slab-capacity slab)))
          ;; Remove the slab from the list of slabs that have any
          ;; available space for the size class.
          (let ((next-slab (slab-next slab)))
            (setf (aref *metaspace-freelists* sizeclass)
                  (cond ((= (sap-int next-slab) 0)
                         0)
                        (t
                         (setf (slab-next slab) (int-sap 0)
                               (slab-prev next-slab) (int-sap 0))
                         (sap-int next-slab))))))
        ;; Deposit a valid instance header word
        (setf (sap-ref-word hole 0)
              (logior (ash (1- nwords) (+ sb-vm:n-widetag-bits 2))
                      instance-widetag))
        ;; Use the unchecked %MAKE-LISP-obj to return it
        (%make-lisp-obj (logior (sap-int hole) instance-pointer-lowtag))))))

;;; Address should be an aligned (not tagged) base address of a chunk.
(defun unallocate-metaspace-chunk (address)
  (declare (word address))
  #+debug (aver (metaspace-chunk-valid-p address))
  (let* ((slab (int-sap (logandc2 address (1- metaspace-slab-size))))
         (sizeclass (ash (slab-sizeclass slab) (- sb-vm:n-fixnum-tag-bits)))
         (size (slab-chunk-size slab)))
    (aver (= size (metaspace-sizeclass->nbytes sizeclass)))
    ;; Clear the memory
    (alien-funcall (extern-alien "memset" (function void unsigned int size-t))
                   address 0 size)
    (with-system-mutex (*allocator-mutex*)
      (let* ((freelist (int-sap (aref *metaspace-freelists* sizeclass)))
             (slab-was-partly-available ; true if already in freelist
              (or (sap= freelist slab) (/= 0 (sap-int (slab-prev slab)))))
             (n-objects (decf (slab-usage slab)))) ; new count
        #+debug (format t "~&Hole in slab @ ~x, sizeclass ~D, newcount=~D of ~D~%"
                        slab sizeclass n-objects (slab-capacity slab))
        ;; If slab has become completely available, add it to the recycle bin.
        (when (zerop n-objects)
          (when slab-was-partly-available
            ;; Capacity 1 can't exist in "partially available" state
            (aver (> (slab-capacity slab) 1))
            ;; Remove from size-specific doubly-linked freelist before
            ;; adding to recycle bin.
            (let ((prev (slab-prev slab))
                  (next (slab-next slab)))
              ;; prev->next = this->next
              (if (= (sap-int prev) 0) ; no previous, was the head of freelist
                  (setf (aref *metaspace-freelists* sizeclass) (sap-int next))
                  (setf (slab-next prev) next))
              ;; next->prev = this->prev
              (unless (= (sap-int next) 0)
                (setf (slab-prev next) prev))))
          ;; Like (PUSH slab recycler)
          (setf (slab-next slab) (int-sap (metaspace-slab-recycler))
                (metaspace-slab-recycler) (sap-int slab))
          ;; Clear out the unused words
          (setf (sap-ref-word slab 0) 0
                (slab-prev slab) (int-sap 0)
                (slab-freelist slab) (int-sap 0))
          #+debug (format t "~&Slab moved to recycle bin~%")
          (return-from unallocate-metaspace-chunk))

        ;; Slab has some used chunks, and gains an available chunk.
        ;; Do like (PUSH address (slab-freelist slab))
        (let ((hole (int-sap address)))
          (setf (chunk-next hole) (slab-freelist slab)
                (slab-freelist slab) hole))

        ;; If the slab was not already in the list of partly-available slabs for
        ;; its sizeclass, it goes to the head of the respective doubly-linked list.
        (unless slab-was-partly-available
          #+debug (format t "~&Insert slab in sizeclass freelist~%")
          (aver (= (sap-int (slab-prev slab)) 0))
          (setf (slab-next slab) freelist)
          (unless (= (sap-int freelist) 0)
            (setf (slab-prev freelist) slab))
          (setf (aref *metaspace-freelists* sizeclass) (sap-int slab))))))
  nil)

;;;; That's the entire allocator. These are for examination of the state.

(defun slab-freelist-as-list (slab &aux (sap (slab-freelist slab)) result)
  (loop (if (sap= sap (int-sap 0)) (return (nreverse result)))
        (push (sap-int sap) result)
        (setq sap (sap-ref-sap sap 0))))

(export 'show-slabs)
(defun show-slabs (&aux (*print-right-margin* 128))
  (do-tracts (tract)
    (let ((tract-end (int-sap (tract-end tract)))
          (slab (int-sap (tract-current tract))))
      (format t "tract: Begin = ~x Current = ~x End = ~x~%"
              (tract-base tract) (sap-int slab) (sap-int tract-end))
      (do ((slab slab (sap+ slab metaspace-slab-size)))
          ((sap>= slab tract-end))
        (let ((ptr (sap+ slab metaspace-slab-size))
              (map))
          (dotimes (i (slab-capacity slab))
            (setf ptr (sap+ ptr (- (slab-chunk-size slab))))
            (push (if (chunk-used-p ptr) #\* #\_) map))
          (if (zerop (slab-capacity slab))
              (format t "  slab @ ~x: free~%" (sap-int slab))
              (format t "  slab @ ~x: cl=~D=~3Db, cap=~d, ct=~2d [~A]~%"
                      (sap-int slab)
                      (ash (slab-sizeclass slab) (- n-fixnum-tag-bits))
                      (slab-chunk-size slab)
                      (slab-capacity slab)
                      (slab-usage slab)
                      ;; Map is collected in reverse order, so it shows in forward order
                      (coerce map 'string)))))))
  (when (find 0 *metaspace-freelists* :start 1 :test #'/=)
    (format t "Freelists:~%"))
  (do ((sizeclass 1 (1+ sizeclass)))
      ((= sizeclass (length *metaspace-freelists*)))
    (let ((addr (aref *metaspace-freelists* sizeclass)))
      (unless (zerop addr)
        (format t "  sizeclass ~d~%" sizeclass)
        (let ((slab (int-sap addr)))
          (aver (= (sap-int (slab-prev slab)) 0))
          (loop (format t "    slab @ ~x: holes=~x~%"
                        (sap-int slab) (slab-freelist-as-list slab))
                (let ((next (slab-next slab)))
                  (if (sap= next (int-sap 0)) (return))
                  (aver (sap= (slab-prev next) slab))
                  (setq slab next)))))))
  (do ((ptr (metaspace-slab-recycler))
       (list))
      ((= ptr 0)
       (when list
         (format t "Recycle: ~x~%" (nreverse list))))
    (push ptr list)
    (setq ptr (sap-int (slab-next (int-sap ptr))))))

(defun slab-chunk-at (slab-base object-index) ; for hand-testing only
  (declare (word slab-base))
  (let* ((slab (int-sap slab-base))
         (capacity (slab-capacity slab)))
    (if (< object-index capacity)
        (let ((ptr (sap+ slab
                         (- metaspace-slab-size
                            ;; Step backwards N objects
                            (* (slab-chunk-size slab)
                               (- capacity object-index))))))
          (when (chunk-used-p ptr)
            (%make-lisp-obj (logior (sap-int ptr) instance-pointer-lowtag))))
        (warn "bad index"))))

#|
(defun unallocate-lispobj (obj)
  (unallocate-chunk (- (get-lisp-obj-address obj) instance-pointer-lowtag)))

(defun unallocate-chunk-at (slab-base object-index)
  (let ((obj (slab-chunk-at slab-base object-index)))
    (when obj
      (unallocate-lispobj obj))))

(defun dealloc-metaspace ()
  (fill *metaspace-freelists* 0)
  (dolist (tract *metaspace-tracts*)
    (deallocate-system-memory (tract-base tract) metaspace-tract-size))
  (setq *metaspace-tracts* nil))
|#
