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
  (def!struct (room-info (:make-load-form-fun just-dump-it-normally))
    ;; the name of this type
    (name nil :type symbol)
    ;; kind of type (how to reconstitute an object)
    (kind (missing-arg)
          :type (member :other :small-other :closure :instance :list
                        :code :vector-nil :weak-pointer))))

(defun room-info-type-name (info)
  (if (specialized-array-element-type-properties-p info)
      (saetp-primitive-type-name info)
      (room-info-name info)))

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
            (make-room-info :name name
                            :kind (if (eq name 'symbol)
                                      :small-other
                                      :other))))))

(dolist (code (list #!+sb-unicode complex-character-string-widetag
                    complex-base-string-widetag simple-array-widetag
                    complex-bit-vector-widetag complex-vector-widetag
                    complex-array-widetag complex-vector-nil-widetag))
  (setf (svref *meta-room-info* code)
        (make-room-info :name 'array-header
                        :kind :other)))

(setf (svref *meta-room-info* bignum-widetag)
      (make-room-info :name 'bignum
                      :kind :other))

(setf (svref *meta-room-info* closure-header-widetag)
      (make-room-info :name 'closure
                      :kind :closure))

(dotimes (i (length *specialized-array-element-type-properties*))
  (let ((saetp (aref *specialized-array-element-type-properties* i)))
    (when (saetp-specifier saetp) ;; SIMPLE-ARRAY-NIL is a special case.
      (setf (svref *meta-room-info* (saetp-typecode saetp)) saetp))))

(setf (svref *meta-room-info* simple-array-nil-widetag)
      (make-room-info :name 'simple-array-nil
                      :kind :vector-nil))

(setf (svref *meta-room-info* code-header-widetag)
      (make-room-info :name 'code
                      :kind :code))

(setf (svref *meta-room-info* instance-header-widetag)
      (make-room-info :name 'instance
                      :kind :instance))

(setf (svref *meta-room-info* funcallable-instance-header-widetag)
      (make-room-info :name 'funcallable-instance
                      :kind :closure))

(setf (svref *meta-room-info* weak-pointer-widetag)
      (make-room-info :name 'weak-pointer
                      :kind :weak-pointer))

(let ((cons-info (make-room-info :name 'cons
                                 :kind :list)))
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
(deftype spaces () '(member :static :dynamic :read-only))

;;;; MAP-ALLOCATED-OBJECTS

;;; Since they're represented as counts of words, we should never
;;; need bignums to represent these:
(declaim (type fixnum
               *static-space-free-pointer*
               *read-only-space-free-pointer*))

#!-sb-fluid
(declaim (inline current-dynamic-space-start))
#!+gencgc
(defun current-dynamic-space-start () sb!vm:dynamic-space-start)
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

          (:closure
           (values (tagged-object fun-pointer-lowtag)
                   widetag
                   (boxed-size header-value)))

          (:instance
           (values (tagged-object instance-pointer-lowtag)
                   widetag
                   (boxed-size header-value)))

          (:other
           (values (tagged-object other-pointer-lowtag)
                   widetag
                   (boxed-size header-value)))

          (:small-other
           (values (tagged-object other-pointer-lowtag)
                   widetag
                   (boxed-size (logand header-value #xff))))

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
           (values (tagged-object other-pointer-lowtag)
                   code-header-widetag
                   (round-to-dualword
                    (+ (* header-value n-word-bytes)
                       (the fixnum
                            (sap-ref-lispobj object-sap
                                             (* code-code-size-slot
                                                n-word-bytes)))))))

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
              (start signed)
              ;; On platforms with small enough GC pages, this field
              ;; will be a short. On platforms with larger ones, it'll
              ;; be an int.
              (bytes-used (unsigned
                           #.(if (typep sb!vm:gencgc-card-bytes
                                        '(unsigned-byte 16))
                                 16
                                 32)))
              (flags (unsigned 8))
              (has-dontmove-dwords (unsigned 8))
              (gen (signed 8))))
  (declaim (inline find-page-index))
  (define-alien-routine "find_page_index" long (index signed))
  (define-alien-variable "last_free_page" sb!kernel::page-index-t)
  (define-alien-variable "heap_base" (* t))
  (define-alien-variable "page_table" (* (struct page))))

;;; Iterate over all the objects allocated in SPACE, calling FUN with
;;; the object, the object's type code, and the object's total size in
;;; bytes, including any header and padding.
#!-sb-fluid (declaim (maybe-inline map-allocated-objects))
(defun map-allocated-objects (fun space)
  (declare (type function fun)
           (type spaces space))
  (without-gcing
    (ecase space
      (:static
       ;; Static space starts with NIL, which requires special
       ;; handling, as the header and alignment are slightly off.
       (multiple-value-bind (start end) (space-bounds space)
         (funcall fun nil symbol-header-widetag (* 8 n-word-bytes))
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
          with start = (sap-ref-lispobj (alien-sap (addr heap-base)) 0)
          with end = start

          ;; This is our page range.
          for page-index from 0 below last-free-page
          for next-page-addr from (+ start page-size) by page-size
          for page-bytes-used = (slot (deref page-table page-index) 'bytes-used)

          when (< page-bytes-used gencgc-card-bytes)
          do (progn
               (incf end (ash page-bytes-used (- n-fixnum-tag-bits)))
               (map-objects-in-range fun start end)
               (setf start next-page-addr)
               (setf end next-page-addr))
          else do (incf end page-size)

          finally (map-objects-in-range fun start end))))))

;;;; MEMORY-USAGE

;;; Return a list of 3-lists (bytes object type-name) for the objects
;;; allocated in Space.
(defun type-breakdown (space)
  (let ((sizes (make-array 256 :initial-element 0 :element-type '(unsigned-byte #.sb!vm:n-word-bits)))
        (counts (make-array 256 :initial-element 0 :element-type '(unsigned-byte #.sb!vm:n-word-bits))))
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
       (when (eql type instance-header-widetag)
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
      (declare (type (unsigned-byte 32) last-page start)
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
                 (let ((this-page (* (the (values (unsigned-byte 32) t)
                                       (truncate addr pagesize))
                                     pagesize)))
                   (declare (type (unsigned-byte 32) this-page))
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
                   (#.symbol-header-widetag
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
                      (unless (eql type instance-header-widetag)
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
           (type (or function null) test)
           (inline map-allocated-objects))
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

;;; Calls FUNCTION with all object that have (possibly conservative)
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

(defun map-referencing-objects (fun space object)
  (declare (type spaces space) (inline map-allocated-objects))
  (unless *ignore-after*
    (setq *ignore-after* (cons 1 2)))
  (flet ((maybe-call (fun obj)
           (when (valid-obj space obj)
             (funcall fun obj))))
    (map-allocated-objects
     (lambda (obj obj-type size)
       (declare (ignore obj-type size))
       (typecase obj
         (cons
          (when (or (eq (car obj) object)
                    (eq (cdr obj) object))
            (maybe-call fun obj)))
         (instance
          (when (or (eq (%instance-layout obj) object)
                    (do-instance-tagged-slot (i obj)
                      (when (eq (%instance-ref obj i) object)
                        (return t))))
            (maybe-call fun obj)))
         (code-component
          (let ((length (get-header-data obj)))
            (do ((i code-constants-offset (1+ i)))
                ((= i length))
              (when (eq (code-header-ref obj i) object)
                (maybe-call fun obj)
                (return)))))
         (simple-vector
          (dotimes (i (length obj))
            (when (eq (svref obj i) object)
              (maybe-call fun obj)
              (return))))
         (symbol
          (when (or (eq (symbol-name obj) object)
                    (eq (symbol-package obj) object)
                    (eq (symbol-info obj) object)
                    (and (boundp obj)
                         (eq (symbol-value obj) object)))
            (maybe-call fun obj)))))
     space)))

(defun list-referencing-objects (space object)
  (collect ((res))
    (map-referencing-objects
     (lambda (obj) (res obj)) space object)
    (res)))
