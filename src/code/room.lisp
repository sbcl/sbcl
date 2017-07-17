;;;; heap-grovelling memory usage stuff

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")


;;;; type format database

(eval-when (:compile-toplevel :load-toplevel :execute)

  (def!struct (room-info (:constructor make-room-info (name kind)))
    ;; the name of this type
    (name nil :type symbol :read-only t)
    ;; kind of type (how to reconstitute an object)
    (kind (missing-arg)
          :type (member :other :tiny-other :closure :instance :list
                        :code :vector-nil :weak-pointer)
          :read-only t))
  (!set-load-form-method room-info (:xc))

  (defun room-info-type-name (info)
    (if (specialized-array-element-type-properties-p info)
        (saetp-primitive-type-name info)
        (room-info-name info))))

(eval-when (:compile-toplevel :execute)

(defvar *meta-room-info* (make-array 256 :initial-element nil))

(dolist (obj *primitive-objects*)
  (let ((widetag (primitive-object-widetag obj))
        (lowtag (primitive-object-lowtag obj))
        (name (primitive-object-name obj)))
    (when (and (eq lowtag 'other-pointer-lowtag)
               (not (member widetag '(t nil)))
               (not (eq name 'weak-pointer)))
      (setf (svref *meta-room-info* (symbol-value widetag))
            (make-room-info name (if (member name '(fdefn symbol))
                                     :tiny-other
                                     :other))))))

(dolist (code (list #!+sb-unicode complex-character-string-widetag
                    complex-base-string-widetag simple-array-widetag
                    complex-bit-vector-widetag complex-vector-widetag
                    complex-array-widetag complex-vector-nil-widetag))
  (setf (svref *meta-room-info* code)
        (make-room-info 'array-header :other)))

(setf (svref *meta-room-info* bignum-widetag)
      (make-room-info 'bignum :other))

(setf (svref *meta-room-info* closure-widetag)
      (make-room-info 'closure :closure))

(dotimes (i (length *specialized-array-element-type-properties*))
  (let ((saetp (aref *specialized-array-element-type-properties* i)))
    (when (saetp-specifier saetp) ;; SIMPLE-ARRAY-NIL is a special case.
      (setf (svref *meta-room-info* (saetp-typecode saetp)) saetp))))

(setf (svref *meta-room-info* simple-array-nil-widetag)
      (make-room-info 'simple-array-nil :vector-nil))

(setf (svref *meta-room-info* code-header-widetag)
      (make-room-info 'code :code))

(setf (svref *meta-room-info* instance-widetag)
      (make-room-info 'instance :instance))

(setf (svref *meta-room-info* funcallable-instance-widetag)
      (make-room-info 'funcallable-instance :closure))

(setf (svref *meta-room-info* weak-pointer-widetag)
      (make-room-info 'weak-pointer :weak-pointer))

(let ((cons-info (make-room-info 'cons :list)))
  ;; A cons consists of two words, both of which may be either a
  ;; pointer or immediate data.  According to the runtime this means
  ;; either a fixnum, a character, an unbound-marker, a single-float
  ;; on a 64-bit system, or a pointer.
  (dotimes (i (ash 1 (- n-widetag-bits n-fixnum-tag-bits)))
    (setf (svref *meta-room-info* (ash i n-fixnum-tag-bits)) cons-info))

  (dotimes (i (ash 1 (- n-widetag-bits n-lowtag-bits)))
    (setf (svref *meta-room-info* (logior (ash i n-lowtag-bits)
                                          instance-pointer-lowtag))
          cons-info)
    (setf (svref *meta-room-info* (logior (ash i n-lowtag-bits)
                                          list-pointer-lowtag))
          cons-info)
    (setf (svref *meta-room-info* (logior (ash i n-lowtag-bits)
                                          fun-pointer-lowtag))
          cons-info)
    (setf (svref *meta-room-info* (logior (ash i n-lowtag-bits)
                                          other-pointer-lowtag))
          cons-info))

  (setf (svref *meta-room-info* character-widetag) cons-info)

  (setf (svref *meta-room-info* unbound-marker-widetag) cons-info)

  ;; Single-floats are immediate data on 64-bit systems.
  #!+64-bit
  (setf (svref *meta-room-info* single-float-widetag) cons-info))

) ; EVAL-WHEN

(defparameter *room-info*
  ;; SAETP instances don't dump properly from XC (or possibly
  ;; normally), and we'd rather share structure with the master copy
  ;; if we can anyway, so...
  (make-array 256
              :initial-contents
              #.`(list
                  ,@(map 'list
                         (lambda (info)
                           (if (specialized-array-element-type-properties-p info)
                               `(aref *specialized-array-element-type-properties*
                                      ,(position info *specialized-array-element-type-properties*))
                               info))
                         *meta-room-info*))))
(deftype spaces ()
  '(member :static #!+immobile-space :immobile :dynamic :read-only))

;;;; MAP-ALLOCATED-OBJECTS

;;; Since they're represented as counts of words, we should never
;;; need bignums to represent these:
(declaim (type fixnum
               *static-space-free-pointer*
               *read-only-space-free-pointer*))

#!-sb-fluid
(declaim (inline current-dynamic-space-start))
#!+gencgc
(defun current-dynamic-space-start () dynamic-space-start)
#!-gencgc
(defun current-dynamic-space-start ()
  (extern-alien "current_dynamic_space" unsigned-long))

(defun space-bounds (space)
  (declare (type spaces space))
  (ecase space
    (:static
     (values (int-sap static-space-start)
             (int-sap (ash *static-space-free-pointer* n-fixnum-tag-bits))))
    (:read-only
     (values (int-sap read-only-space-start)
             (int-sap (ash *read-only-space-free-pointer* n-fixnum-tag-bits))))
    #!+immobile-space
    (:immobile
     (values (int-sap immobile-space-start)
             (int-sap (ash *immobile-space-free-pointer* n-fixnum-tag-bits))))
    (:dynamic
     (values (int-sap (current-dynamic-space-start))
             (dynamic-space-free-pointer)))))

;;; Return the total number of bytes used in SPACE.
(defun space-bytes (space)
  (multiple-value-bind (start end) (space-bounds space)
    (- (sap-int end) (sap-int start))))

;;; Round SIZE (in bytes) up to the next dualword boundary. A dualword
;;; is eight bytes on platforms with 32-bit word size and 16 bytes on
;;; platforms with 64-bit word size.
#!-sb-fluid (declaim (inline round-to-dualword))
(defun round-to-dualword (size)
  (logand (the word (+ size lowtag-mask)) (lognot lowtag-mask)))

;;; Return the vector OBJ, its WIDETAG, and the number of octets
;;; required for its storage (including padding and alignment).
(defun reconstitute-vector (obj saetp)
  (declare (type (simple-array * (*)) obj)
           (type specialized-array-element-type-properties saetp))
  (let* ((length (+ (length obj)
                    (saetp-n-pad-elements saetp)))
         (n-bits (saetp-n-bits saetp))
         (alignment-pad (floor 7 n-bits))
         (n-data-octets (if (>= n-bits 8)
                            (* length (ash n-bits -3))
                            (ash (* (+ length alignment-pad)
                                    n-bits)
                                 -3))))
    (values obj
            (saetp-typecode saetp)
            (round-to-dualword (+ (* vector-data-offset n-word-bytes)
                                  n-data-octets)))))

;;; Given the address (untagged, aligned, and interpreted as a FIXNUM)
;;; of a lisp object, return the object, its "type code" (either
;;; LIST-POINTER-LOWTAG or a header widetag), and the number of octets
;;; required for its storage (including padding and alignment).  Note
;;; that this function is designed to NOT CONS, even if called
;;; out-of-line.
(defun reconstitute-object (address)
  (let* ((object-sap (int-sap (get-lisp-obj-address address)))
         (header (sap-ref-word object-sap 0))
         (widetag (logand header widetag-mask))
         (header-value (ash header (- n-widetag-bits)))
         (info (svref *room-info* widetag)))
    (macrolet
        ((boxed-size (header-value)
           `(round-to-dualword (ash (1+ ,header-value) word-shift)))
         (tagged-object (tag)
           `(%make-lisp-obj (logior ,tag (get-lisp-obj-address address)))))
      (cond
          ;; Pick off arrays, as they're the only plausible cause for
          ;; a non-nil, non-ROOM-INFO object as INFO.
        ((specialized-array-element-type-properties-p info)
         (reconstitute-vector (tagged-object other-pointer-lowtag) info))

        ((null info)
         (error "Unrecognized widetag #x~2,'0X in reconstitute-object"
                widetag))

        (t
         (case (room-info-kind info)
          (:list
           (values (tagged-object list-pointer-lowtag)
                   list-pointer-lowtag
                   (* 2 n-word-bytes)))

          (:closure ; also funcallable-instance
           (values (tagged-object fun-pointer-lowtag)
                   widetag
                   (boxed-size (logand header-value short-header-max-words))))

          (:instance
           (values (tagged-object instance-pointer-lowtag)
                   widetag
                   (boxed-size (logand header-value short-header-max-words))))

          (:other
           (values (tagged-object other-pointer-lowtag)
                   widetag
                   (boxed-size header-value)))

          (:tiny-other
           (values (tagged-object other-pointer-lowtag)
                   widetag
                   (boxed-size (logand header-value #xFF))))

          (:vector-nil
           (values (tagged-object other-pointer-lowtag)
                   simple-array-nil-widetag
                   (* 2 n-word-bytes)))

          (:weak-pointer
           (values (tagged-object other-pointer-lowtag)
                   weak-pointer-widetag
                   (round-to-dualword
                    (* weak-pointer-size
                       n-word-bytes))))

          (:code
           (let ((c (tagged-object other-pointer-lowtag)))
             (values c
                     code-header-widetag
                     (round-to-dualword
                      (+ (* (logand header-value short-header-max-words)
                            n-word-bytes)
                         (%code-code-size (truly-the code-component c)))))))
          (t
           (error "Unrecognized room-info-kind ~S in reconstitute-object"
                  (room-info-kind info)))))))))

;;; Iterate over all the objects in the contiguous block of memory
;;; with the low address at START and the high address just before
;;; END, calling FUN with the object, the object's type code, and the
;;; object's total size in bytes, including any header and padding.
;;; START and END are untagged, aligned memory addresses interpreted
;;; as FIXNUMs (unlike SAPs or tagged addresses, these will not cons).
(defun map-objects-in-range (fun start end)
  (declare (type function fun))
  ;; If START is (unsigned) greater than END, then we have somehow
  ;; blown past our endpoint.
  (aver (<= (get-lisp-obj-address start)
            (get-lisp-obj-address end)))
  (unless (= start end)
    (multiple-value-bind
          (obj typecode size)
        (reconstitute-object start)
      (aver (zerop (logand n-lowtag-bits size)))
      (let ((next-start
             ;; This special little dance is to add a number of octets
             ;; (and it had best be a number evenly divisible by our
             ;; allocation granularity) to an unboxed, aligned address
             ;; masquerading as a fixnum.  Without consing.
             (%make-lisp-obj
              (mask-field (byte #.n-word-bits 0)
                          (+ (get-lisp-obj-address start)
                             size)))))
        (funcall fun obj typecode size)
        (map-objects-in-range fun next-start end)))))

;;; Access to the GENCGC page table for better precision in
;;; MAP-ALLOCATED-OBJECTS
#!+gencgc
(progn
  (define-alien-type (struct page)
      (struct page
              ;; To cut down the size of the page table, the scan_start_offset
              ;; - a/k/a "start" - is measured in 4-byte integers regardless
              ;; of word size. This is fine for 32-bit address space,
              ;; but if 64-bit then we have to scale the value. Additionally
              ;; there is a fallback for when even the scaled value is too big.
              ;; (None of this matters to Lisp code for the most part)
              (start #!+64-bit (unsigned 32) #!-64-bit signed)
              ;; On platforms with small enough GC pages, this field
              ;; will be a short. On platforms with larger ones, it'll
              ;; be an int.
              ;; Measured in bytes; the low bit has to be masked off.
              (bytes-used (unsigned
                           #.(if (typep gencgc-card-bytes '(unsigned-byte 16))
                                 16
                                 32)))
              (flags (unsigned 8))
              (gen (signed 8))))
  #!+immobile-space
  (progn
    (define-alien-type (struct immobile-page)
        ;; ... and yet another place for Lisp to become out-of-sync with C.
        (struct immobile-page
                (flags (unsigned 8))
                (obj-spacing (unsigned 8))
                (obj-size (unsigned 8))
                (generations (unsigned 8))
                (free-index (unsigned 32))
                (page-link (unsigned 16))
                (prior-free-index (unsigned 16))))
    (define-alien-variable "fixedobj_pages" (* (struct immobile-page))))
  (declaim (inline find-page-index))
  (define-alien-routine ("ext_find_page_index" find-page-index)
    long (index signed))
  (define-alien-variable "last_free_page" sb!kernel::page-index-t)
  (define-alien-variable "page_table" (* (struct page))))

(declaim (inline code-header-words))
(defun code-header-words (code)
  (logand (get-header-data code) short-header-max-words))

;;; Iterate over all the objects allocated in each of the SPACES, calling FUN
;;; with the object, the object's type code, and the object's total size in
;;; bytes, including any header and padding. As a special case, if exactly one
;;; space named :ALL is requested, then map over the known spaces.
(defun map-allocated-objects (fun &rest spaces)
  (declare (type function fun))
  (when (and (= (length spaces) 1) (eq (first spaces) :all))
    (return-from map-allocated-objects
     (map-allocated-objects fun
                            :read-only :static
                            #!+immobile-space :immobile
                            :dynamic)))
  ;; You can't specify :ALL and also a list of spaces. Check that up front.
  (do-rest-arg ((space) spaces) (the spaces space))
  (flet ((do-1-space (space)
    (ecase space
      (:static
       ;; Static space starts with NIL, which requires special
       ;; handling, as the header and alignment are slightly off.
       (multiple-value-bind (start end) (space-bounds space)
         (funcall fun nil symbol-widetag (* 8 n-word-bytes))
         (map-objects-in-range fun
                               (%make-lisp-obj (+ (* 8 n-word-bytes)
                                                  (sap-int start)))
                               (%make-lisp-obj (sap-int end)))))

      ((:read-only #!-gencgc :dynamic)
       ;; Read-only space (and dynamic space on cheneygc) is a block
       ;; of contiguous allocations.
       (multiple-value-bind (start end) (space-bounds space)
         (map-objects-in-range fun
                               (%make-lisp-obj (sap-int start))
                               (%make-lisp-obj (sap-int end)))))

      #!+immobile-space
      (:immobile
       ;; Filter out filler objects. These either look like cons cells
       ;; in fixedobj subspace, or code without enough header words
       ;; in varyobj subspace. (cf 'immobile_filler_p' in gc-internal.h)
       (dx-flet ((filter (obj type size)
                   (unless (or (and (code-component-p obj)
                                    (eql (code-header-words obj) 2))
                               (consp obj))
                     (funcall fun obj type size))))
         (let ((start immobile-space-start)
               (end *immobile-fixedobj-free-pointer*))
           (dotimes (pass 2)
             (map-objects-in-range #'filter
                                   (ash start (- n-fixnum-tag-bits))
                                   end)
             (setq start (+ immobile-space-start immobile-fixedobj-subspace-size)
                   end *immobile-space-free-pointer*)))))

      #!+gencgc
      (:dynamic
       ;; Dynamic space on gencgc requires walking the GC page tables
       ;; in order to determine what regions contain objects.

       ;; We explicitly presume that any pages in an allocation region
       ;; that are in-use have a BYTES-USED of GENCGC-CARD-BYTES
       ;; (indicating a full page) or an otherwise-valid BYTES-USED.
       ;; We also presume that the pages of an open allocation region
       ;; after the first page, and any pages that are unallocated,
       ;; have a BYTES-USED of zero.  GENCGC seems to guarantee this.

       ;; Our procedure is to scan forward through the page table,
       ;; maintaining an "end pointer" until we reach a page where
       ;; BYTES-USED is not GENCGC-CARD-BYTES or we reach
       ;; LAST-FREE-PAGE.  We then MAP-OBJECTS-IN-RANGE if the range
       ;; is not empty, and proceed to the next page (unless we've hit
       ;; LAST-FREE-PAGE).  We happily take advantage of the fact that
       ;; MAP-OBJECTS-IN-RANGE will simply return if passed two
       ;; coincident pointers for the range.

       ;; FIXME: WITHOUT-GCING prevents a GC flip, but doesn't prevent
       ;; closing allocation regions and opening new ones.  This may
       ;; prove to be an issue with concurrent systems, or with
       ;; spectacularly poor timing for closing an allocation region
       ;; in a single-threaded system.

       (loop
          with page-size = (ash gencgc-card-bytes (- n-fixnum-tag-bits))
          ;; This magic dance gets us an unboxed aligned pointer as a
          ;; FIXNUM.
          with start = (%make-lisp-obj (current-dynamic-space-start))
          with end = start

          ;; This is our page range. The type constraint is far too generous,
          ;; but it does its job of producing efficient code.
          for page-index
          of-type (integer -1 (#.(/ (ash 1 n-machine-word-bits) gencgc-card-bytes)))
          from 0 below last-free-page
          for next-page-addr from (+ start page-size) by page-size
          for page-bytes-used
              ;; The low bits of bytes-used is the need-to-zero flag.
              = (logandc1 1 (slot (deref page-table page-index) 'bytes-used))

          when (< page-bytes-used gencgc-card-bytes)
          do (progn
               (incf end (ash page-bytes-used (- n-fixnum-tag-bits)))
               (map-objects-in-range fun start end)
               (setf start next-page-addr)
               (setf end next-page-addr))
          else do (incf end page-size)

          finally (map-objects-in-range fun start end))))))
  (do-rest-arg ((space) spaces)
    (if (eq space :dynamic)
        (without-gcing (do-1-space space))
        (do-1-space space)))))

;;;; MEMORY-USAGE

;;; Return a list of 3-lists (bytes object type-name) for the objects
;;; allocated in Space.
(defun type-breakdown (space)
  (declare (muffle-conditions t))
  (let ((sizes (make-array 256 :initial-element 0 :element-type '(unsigned-byte #.n-word-bits)))
        (counts (make-array 256 :initial-element 0 :element-type '(unsigned-byte #.n-word-bits))))
    (map-allocated-objects
     (lambda (obj type size)
       (declare (word size) (optimize (speed 3)) (ignore obj))
       (incf (aref sizes type) size)
       (incf (aref counts type)))
     space)

    (let ((totals (make-hash-table :test 'eq)))
      (dotimes (i 256)
        (let ((total-count (aref counts i)))
          (unless (zerop total-count)
            (let* ((total-size (aref sizes i))
                   (name (room-info-type-name (aref *room-info* i)))
                   (found (gethash name totals)))
              (cond (found
                     (incf (first found) total-size)
                     (incf (second found) total-count))
                    (t
                     (setf (gethash name totals)
                           (list total-size total-count name))))))))

      (collect ((totals-list))
        (maphash (lambda (k v)
                   (declare (ignore k))
                   (totals-list v))
                 totals)
        (sort (totals-list) #'> :key #'first)))))

;;; Handle the summary printing for MEMORY-USAGE. Totals is a list of lists
;;; (space-name . totals-for-space), where totals-for-space is the list
;;; returned by TYPE-BREAKDOWN.
(defun print-summary (spaces totals)
  (let ((summary (make-hash-table :test 'eq)))
    (dolist (space-total totals)
      (dolist (total (cdr space-total))
        (push (cons (car space-total) total)
              (gethash (third total) summary))))

    (collect ((summary-totals))
      (maphash (lambda (k v)
                 (declare (ignore k))
                 (let ((sum 0))
                   (declare (unsigned-byte sum))
                   (dolist (space-total v)
                     (incf sum (first (cdr space-total))))
                   (summary-totals (cons sum v))))
               summary)

      (format t "~2&Summary of spaces: ~(~{~A ~}~)~%" spaces)
      (let ((summary-total-bytes 0)
            (summary-total-objects 0))
        (declare (unsigned-byte summary-total-bytes summary-total-objects))
        (dolist (space-totals
                 (mapcar #'cdr (sort (summary-totals) #'> :key #'car)))
          (let ((total-objects 0)
                (total-bytes 0)
                name)
            (declare (unsigned-byte total-objects total-bytes))
            (collect ((spaces))
              (dolist (space-total space-totals)
                (let ((total (cdr space-total)))
                  (setq name (third total))
                  (incf total-bytes (first total))
                  (incf total-objects (second total))
                  (spaces (cons (car space-total) (first total)))))
              (format t "~%~A:~%    ~:D bytes, ~:D object~:P"
                      name total-bytes total-objects)
              (dolist (space (spaces))
                (format t ", ~W% ~(~A~)"
                        (round (* (cdr space) 100) total-bytes)
                        (car space)))
              (format t ".~%")
              (incf summary-total-bytes total-bytes)
              (incf summary-total-objects total-objects))))
        (format t "~%Summary total:~%    ~:D bytes, ~:D objects.~%"
                summary-total-bytes summary-total-objects)))))

;;; Report object usage for a single space.
(defun report-space-total (space-total cutoff)
  (declare (list space-total) (type (or single-float null) cutoff))
  (format t "~2&Breakdown for ~(~A~) space:~%" (car space-total))
  (let* ((types (cdr space-total))
         (total-bytes (reduce #'+ (mapcar #'first types)))
         (total-objects (reduce #'+ (mapcar #'second types)))
         (cutoff-point (if cutoff
                           (truncate (* (float total-bytes) cutoff))
                           0))
         (reported-bytes 0)
         (reported-objects 0))
    (declare (unsigned-byte total-objects total-bytes cutoff-point reported-objects
                            reported-bytes))
    (loop for (bytes objects name) in types do
      (when (<= bytes cutoff-point)
        (format t "  ~10:D bytes for ~9:D other object~2:*~P.~%"
                (- total-bytes reported-bytes)
                (- total-objects reported-objects))
        (return))
      (incf reported-bytes bytes)
      (incf reported-objects objects)
      (format t "  ~10:D bytes for ~9:D ~(~A~) object~2:*~P.~%"
              bytes objects name))
    (format t "  ~10:D bytes for ~9:D ~(~A~) object~2:*~P (space total.)~%"
            total-bytes total-objects (car space-total))))

;;; Print information about the heap memory in use. PRINT-SPACES is a
;;; list of the spaces to print detailed information for.
;;; COUNT-SPACES is a list of the spaces to scan. For either one, T
;;; means all spaces (i.e. :STATIC, :DYNAMIC and :READ-ONLY.) If
;;; PRINT-SUMMARY is true, then summary information will be printed.
;;; The defaults print only summary information for dynamic space. If
;;; true, CUTOFF is a fraction of the usage in a report below which
;;; types will be combined as OTHER.
(defun memory-usage (&key print-spaces (count-spaces '(:dynamic))
                          (print-summary t) cutoff)
  (declare (type (or single-float null) cutoff))
  (let* ((spaces (if (eq count-spaces t)
                     '(:static :dynamic :read-only)
                     count-spaces))
         (totals (mapcar (lambda (space)
                           (cons space (type-breakdown space)))
                         spaces)))

    (dolist (space-total totals)
      (when (or (eq print-spaces t)
                (member (car space-total) print-spaces))
        (report-space-total space-total cutoff)))

    (when print-summary (print-summary spaces totals)))

  (values))

;;; Print a breakdown by instance type of all the instances allocated
;;; in SPACE. If TOP-N is true, print only information for the
;;; TOP-N types with largest usage.
(defun instance-usage (space &key (top-n 15))
  (declare (type spaces space) (type (or fixnum null) top-n))
  (format t "~2&~@[Top ~W ~]~(~A~) instance types:~%" top-n space)
  (let ((totals (make-hash-table :test 'eq))
        (total-objects 0)
        (total-bytes 0))
    (declare (unsigned-byte total-objects total-bytes))
    (map-allocated-objects
     (lambda (obj type size)
       (declare (optimize (speed 3)))
       (when (eql type instance-widetag)
         (incf total-objects)
         (let* ((classoid (layout-classoid (%instance-layout obj)))
                (found (gethash classoid totals))
                (size size))
           (declare (fixnum size))
           (incf total-bytes size)
           (cond (found
                  (incf (the fixnum (car found)))
                  (incf (the fixnum (cdr found)) size))
                 (t
                  (setf (gethash classoid totals) (cons 1 size)))))))
     space)

    (collect ((totals-list))
      (maphash (lambda (classoid what)
                 (totals-list (cons (prin1-to-string
                                     (classoid-proper-name classoid))
                                    what)))
               totals)
      (let ((sorted (sort (totals-list) #'> :key #'cddr))
            (printed-bytes 0)
            (printed-objects 0))
        (declare (unsigned-byte printed-bytes printed-objects))
        (dolist (what (if top-n
                          (subseq sorted 0 (min (length sorted) top-n))
                          sorted))
          (let ((bytes (cddr what))
                (objects (cadr what)))
            (incf printed-bytes bytes)
            (incf printed-objects objects)
            (format t "  ~A: ~:D bytes, ~:D object~:P.~%" (car what)
                    bytes objects)))

        (let ((residual-objects (- total-objects printed-objects))
              (residual-bytes (- total-bytes printed-bytes)))
          (unless (zerop residual-objects)
            (format t "  Other types: ~:D bytes, ~:D object~:P.~%"
                    residual-bytes residual-objects))))

      (format t "  ~:(~A~) instance total: ~:D bytes, ~:D object~:P.~%"
              space total-bytes total-objects)))

  (values))

;;;; PRINT-ALLOCATED-OBJECTS

(defun print-allocated-objects (space &key (percent 0) (pages 5)
                                      type larger smaller count
                                      (stream *standard-output*))
  (declare (type (integer 0 99) percent) (type index pages)
           (type stream stream) (type spaces space)
           (type (or index null) type larger smaller count))
  (multiple-value-bind (start-sap end-sap) (space-bounds space)
    (let* ((space-start (sap-int start-sap))
           (space-end (sap-int end-sap))
           (space-size (- space-end space-start))
           (pagesize (get-page-size))
           (start (+ space-start (round (* space-size percent) 100)))
           (printed-conses (make-hash-table :test 'eq))
           (pages-so-far 0)
           (count-so-far 0)
           (last-page 0))
      (declare (type word last-page start)
               (fixnum pages-so-far count-so-far pagesize))
      (labels ((note-conses (x)
                 (unless (or (atom x) (gethash x printed-conses))
                   (setf (gethash x printed-conses) t)
                   (note-conses (car x))
                   (note-conses (cdr x)))))
        (map-allocated-objects
         (lambda (obj obj-type size)
           (let ((addr (get-lisp-obj-address obj)))
             (when (>= addr start)
               (when (if count
                         (> count-so-far count)
                         (> pages-so-far pages))
                 (return-from print-allocated-objects (values)))

               (unless count
                 (let ((this-page (* (the (values word t)
                                       (truncate addr pagesize))
                                     pagesize)))
                   (declare (type word this-page))
                   (when (/= this-page last-page)
                     (when (< pages-so-far pages)
                       ;; FIXME: What is this? (ERROR "Argh..")? or
                       ;; a warning? or code that can be removed
                       ;; once the system is stable? or what?
                       (format stream "~2&**** Page ~W, address ~X:~%"
                               pages-so-far addr))
                     (setq last-page this-page)
                     (incf pages-so-far))))

               (when (and (or (not type) (eql obj-type type))
                          (or (not smaller) (<= size smaller))
                          (or (not larger) (>= size larger)))
                 (incf count-so-far)
                 (case type
                   (#.code-header-widetag
                    (let ((dinfo (%code-debug-info obj)))
                      (format stream "~&Code object: ~S~%"
                              (if dinfo
                                  (sb!c::compiled-debug-info-name dinfo)
                                  "No debug info."))))
                   (#.symbol-widetag
                    (format stream "~&~S~%" obj))
                   (#.list-pointer-lowtag
                    (unless (gethash obj printed-conses)
                      (note-conses obj)
                      (let ((*print-circle* t)
                            (*print-level* 5)
                            (*print-length* 10))
                        (format stream "~&~S~%" obj))))
                   (t
                    (fresh-line stream)
                    (let ((str (write-to-string obj :level 5 :length 10
                                                :pretty nil)))
                      (unless (eql type instance-widetag)
                        (format stream "~S: " (type-of obj)))
                      (format stream "~A~%"
                              (subseq str 0 (min (length str) 60))))))))))
         space))))
  (values))

;;;; LIST-ALLOCATED-OBJECTS, LIST-REFERENCING-OBJECTS

(defvar *ignore-after* nil)

(defun valid-obj (space x)
  (or (not (eq space :dynamic))
      ;; this test looks bogus if the allocator doesn't work linearly,
      ;; which I suspect is the case for GENCGC.  -- CSR, 2004-06-29
      (< (get-lisp-obj-address x) (get-lisp-obj-address *ignore-after*))))

(defun maybe-cons (space x stuff)
  (if (valid-obj space x)
      (cons x stuff)
      stuff))

(defun list-allocated-objects (space &key type larger smaller count
                                     test)
  (declare (type spaces space)
           (type (or index null) larger smaller type count)
           (type (or function null) test))
  (unless *ignore-after*
    (setq *ignore-after* (cons 1 2)))
  (collect ((counted 0 1+))
    (let ((res ()))
      (map-allocated-objects
       (lambda (obj obj-type size)
         (when (and (or (not type) (eql obj-type type))
                    (or (not smaller) (<= size smaller))
                    (or (not larger) (>= size larger))
                    (or (not test) (funcall test obj)))
           (setq res (maybe-cons space obj res))
           (when (and count (>= (counted) count))
             (return-from list-allocated-objects res))))
       space)
      res)))

;;; Convert the descriptor into a SAP. The bits all stay the same, we just
;;; change our notion of what we think they are.
;;;
;;; Defining this here (as opposed to in 'debug-int' where it belongs)
;;; is the path of least resistance to avoiding an inlining failure warning.
#!-sb-fluid (declaim (inline sb!di::descriptor-sap))
(defun sb!di::descriptor-sap (x)
  (int-sap (get-lisp-obj-address x)))

;;; Calls FUNCTION with all objects that have (possibly conservative)
;;; references to them on current stack.
(defun map-stack-references (function)
  (let ((end
         (sb!di::descriptor-sap
          #!+stack-grows-downward-not-upward *control-stack-end*
          #!-stack-grows-downward-not-upward *control-stack-start*))
        (sp (current-sp))
        (seen nil))
    (loop until #!+stack-grows-downward-not-upward (sap> sp end)
                #!-stack-grows-downward-not-upward (sap< sp end)
          do (multiple-value-bind (obj ok) (make-lisp-obj (sap-ref-word sp 0) nil)
               (when (and ok (typep obj '(not (or fixnum character))))
                 (unless (member obj seen :test #'eq)
                   (funcall function obj)
                   (push obj seen))))
             (setf sp
                   #!+stack-grows-downward-not-upward (sap+ sp n-word-bytes)
                   #!-stack-grows-downward-not-upward (sap+ sp (- n-word-bytes))))))

;;; This interface allows one either to be agnostic of the referencing space,
;;; or specify exactly one space, but not specify a list of spaces.
;;; An upward-compatible change would be to assume a list, and call ENSURE-LIST.
(defun map-referencing-objects (fun space object)
  (declare (type (or (eql :all) spaces) space))
  (unless *ignore-after*
    (setq *ignore-after* (cons 1 2)))
  (flet ((ref-p (this widetag nwords) ; return T if 'this' references object
           (when (listp this)
             (return-from ref-p
               (or (eq (car this) object) (eq (cdr this) object))))
           (case widetag
             ;; purely boxed objects
             ((#.ratio-widetag #.complex-widetag #.value-cell-widetag
               #.symbol-widetag #.weak-pointer-widetag
               #.simple-array-widetag #.simple-vector-widetag
               #.complex-array-widetag #.complex-vector-widetag
               #.complex-bit-vector-widetag #.complex-vector-nil-widetag
               #.complex-base-string-widetag
               #!+sb-unicode #.complex-character-string-widetag))
             ;; mixed boxed/unboxed objects
             (#.code-header-widetag
              (dotimes (i (code-n-entries this))
                (let ((f (%code-entry-point this i)))
                  (when (or (eq f object)
                            (eq (%simple-fun-name f) object)
                            (eq (%simple-fun-arglist f) object)
                            (eq (%simple-fun-type f) object)
                            (eq (%simple-fun-info f) object))
                    (return-from ref-p t))))
              (setq nwords (code-header-words this)))
             (#.instance-widetag
              (return-from ref-p
                (or (eq (%instance-layout this) object)
                    (do-instance-tagged-slot (i this)
                      (when (eq (%instance-ref this i) object)
                        (return t))))))
             (#.funcallable-instance-widetag
              (let ((l (%funcallable-instance-layout this)))
                (when (eq l object)
                  (return-from ref-p t))
                (let ((bitmap (layout-bitmap l)))
                  (unless (eql bitmap -1)
                    ;; tagged slots precede untagged slots,
                    ;; so integer-length is the count of tagged slots.
                    (setq nwords (1+ (integer-length bitmap)))))))
             (#.closure-widetag
              (when (eq (%closure-fun this) object)
                (return-from ref-p t)))
             (#.fdefn-widetag
              #!+immobile-code
              (when (eq (make-lisp-obj
                         (alien-funcall
                          (extern-alien "fdefn_raw_referent" (function unsigned unsigned))
                          (logandc2 (get-lisp-obj-address this) lowtag-mask)))
                        object)
                (return-from ref-p t))
              ;; Without immobile-code the 'raw-addr' slot either holds the same thing
              ;; as the 'fun' slot, or holds a trampoline address. We'll overlook the
              ;; minor issue that due to concurrent writes, two representations of the
              ;; allegedly same referent may diverge; thus the last slot is skipped
              ;; even if it refers to a different simple-fun.
              (decf nwords))
             (t
              (return-from ref-p nil)))
           ;; gencgc has WITHOUT-GCING in map-allocated-objects over dynamic space,
           ;; so we don't have to pin each object inside REF-P.
           (#!+cheneygc with-pinned-objects #!+cheneygc (this)
            #!-cheneygc progn
            (do ((sap (int-sap (logandc2 (get-lisp-obj-address this) lowtag-mask)))
                 (i (* (1- nwords) n-word-bytes) (- i n-word-bytes)))
                ((<= i 0) nil)
              (when (eq (sap-ref-lispobj sap i) object)
                (return t))))))
    (let ((fun (%coerce-callable-to-fun fun)))
      (dx-flet ((mapfun (obj widetag size)
                  (when (and (ref-p obj widetag (/ size n-word-bytes))
                             (valid-obj space obj))
                    (funcall fun obj))))
        (map-allocated-objects #'mapfun space)))))

(defun list-referencing-objects (space object)
  (collect ((res))
    (map-referencing-objects
     (lambda (obj) (res obj)) space object)
    (res)))


#+nil ; for debugging
(defun dump-dynamic-space-code (&optional (stream *standard-output*)
                                &aux (n-pages 0) (n-code-bytes 0))
  (flet ((dump-page (page-num)
           (incf n-pages)
           (format stream "~&Page ~D~%" page-num)
           (let ((where (+ dynamic-space-start (* page-num gencgc-card-bytes)))
                 (seen-filler nil))
             (loop
               (multiple-value-bind (obj type size)
                   (reconstitute-object (ash where (- n-fixnum-tag-bits)))
                 (when (= type code-header-widetag)
                   (incf n-code-bytes size))
                 (when (if (and (consp obj) (eq (car obj) 0) (eq (cdr obj) 0))
                           (if seen-filler
                               (progn (write-char #\. stream) nil)
                               (setq seen-filler t))
                           (progn (setq seen-filler nil) t))
                   (let ((*print-pretty* nil))
                     (format stream "~&  ~X ~4X ~S " where size obj)))
                 (incf where size))
               (let ((next-page (find-page-index where)))
                 (cond ((= (logand where (1- gencgc-card-bytes)) 0)
                        (format stream "~&-- END OF PAGE --~%")
                        (return next-page))
                       ((eq next-page page-num))
                       (t
                        (incf n-pages)
                        (setq page-num next-page seen-filler nil))))))))
    (let ((i 0))
      (loop while (< i last-free-page)
            do (let ((allocation (ldb (byte 2 0)
                                      (slot (deref page-table i) 'flags))))
                 (if (= allocation 3)
                     (setq i (dump-page i))
                     (incf i)))))
    (let* ((tot (* n-pages gencgc-card-bytes))
           (waste (- tot n-code-bytes)))
      (format t "~&Used=~D Waste=~D (~F%)~%" n-code-bytes waste
              (* 100 (/ waste tot))))))
