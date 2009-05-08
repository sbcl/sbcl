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

(declaim (special sb!vm:*read-only-space-free-pointer*
                  sb!vm:*static-space-free-pointer*))

;;;; type format database

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def!struct (room-info (:make-load-form-fun just-dump-it-normally))
    ;; the name of this type
    (name nil :type symbol)
    ;; kind of type (how we determine length)
    (kind (missing-arg)
          :type (member :lowtag :fixed :header :vector
                        :string :code :closure :instance))
    ;; length if fixed-length, shift amount for element size if :VECTOR
    (length nil :type (or fixnum null))))

(eval-when (:compile-toplevel :execute)

(defvar *meta-room-info* (make-array 256 :initial-element nil))

(dolist (obj *primitive-objects*)
  (let ((widetag (primitive-object-widetag obj))
        (lowtag (primitive-object-lowtag obj))
        (name (primitive-object-name obj))
        (variable (primitive-object-variable-length-p obj))
        (size (primitive-object-size obj)))
    (cond
     ((not lowtag))
     (;; KLUDGE described in dan_b message "Another one for the
      ;; collection [bug 108]" (sbcl-devel 2004-01-22)
      ;;
      ;; In a freshly started SBCL 0.8.7.20ish, (TIME (ROOM T))  causes
      ;;   debugger invoked on a SB-INT:BUG in thread 5911:
      ;;     failed AVER: "(SAP= CURRENT END)"
      ;; [WHN: Similar things happened on one but not the other of my
      ;; machines when I just run ROOM a lot in a loop.]
      ;;
      ;; This appears to be due to my [DB] abuse of the primitive
      ;; object macros to define a thread object that shares a lowtag
      ;; with fixnums and has no widetag: it looks like the code that
      ;; generates *META-ROOM-INFO* infers from this that even fixnums
      ;; are thread-sized - probably undesirable.
      ;;
      ;; This [the fix; the EQL NAME 'THREAD clause here] is more in the
      ;; nature of a workaround than a really good fix. I'm not sure
      ;; what a really good fix is: I /think/ it's probably to remove
      ;; the :LOWTAG option in DEFINE-PRIMITIVE-OBJECT THREAD, then teach
      ;; genesis to generate the necessary OBJECT_SLOT_OFFSET macros
      ;; for assembly source in the runtime/genesis/*.h files.
      (eql name 'thread))
     ((not widetag)
      (let ((info (make-room-info :name name
                                  :kind :lowtag))
            (lowtag (symbol-value lowtag)))
        (declare (fixnum lowtag))
        (dotimes (i 32)
          (setf (svref *meta-room-info* (logior lowtag (ash i 3))) info))))
     (variable)
     (t
      (setf (svref *meta-room-info* (symbol-value widetag))
            (make-room-info :name name
                            :kind :fixed
                            :length size))))))

(dolist (code (list #!+sb-unicode complex-character-string-widetag
                    complex-base-string-widetag simple-array-widetag
                    complex-bit-vector-widetag complex-vector-widetag
                    complex-array-widetag complex-vector-nil-widetag))
  (setf (svref *meta-room-info* code)
        (make-room-info :name 'array-header
                        :kind :header)))

(setf (svref *meta-room-info* bignum-widetag)
      (make-room-info :name 'bignum
                      :kind :header))

(setf (svref *meta-room-info* closure-header-widetag)
      (make-room-info :name 'closure
                      :kind :closure))

;; FIXME: This looks rather brittle. Can we get more of these numbers
;; from somewhere sensible?
(dolist (stuff '((simple-bit-vector-widetag . -3)
                 (simple-vector-widetag . #.sb!vm:word-shift)
                 (simple-array-unsigned-byte-2-widetag . -2)
                 (simple-array-unsigned-byte-4-widetag . -1)
                 (simple-array-unsigned-byte-7-widetag . 0)
                 (simple-array-unsigned-byte-8-widetag . 0)
                 (simple-array-unsigned-byte-15-widetag . 1)
                 (simple-array-unsigned-byte-16-widetag . 1)
                 (simple-array-unsigned-byte-31-widetag . 2)
                 (simple-array-unsigned-byte-32-widetag . 2)
                 (simple-array-unsigned-byte-60-widetag . 3)
                 (simple-array-unsigned-byte-63-widetag . 3)
                 (simple-array-unsigned-byte-64-widetag . 3)
                 (simple-array-signed-byte-8-widetag . 0)
                 (simple-array-signed-byte-16-widetag . 1)
                 (simple-array-unsigned-byte-29-widetag . 2)
                 (simple-array-signed-byte-30-widetag . 2)
                 (simple-array-signed-byte-32-widetag . 2)
                 (simple-array-signed-byte-61-widetag . 3)
                 (simple-array-signed-byte-64-widetag . 3)
                 (simple-array-single-float-widetag . 2)
                 (simple-array-double-float-widetag . 3)
                 (simple-array-complex-single-float-widetag . 3)
                 (simple-array-complex-double-float-widetag . 4)))
  (let* ((name (car stuff))
         (size (cdr stuff))
         (sname (string name)))
    (when (boundp name)
      (setf (svref *meta-room-info* (symbol-value name))
            (make-room-info :name (intern (subseq sname
                                                  0
                                                  (mismatch sname "-WIDETAG"
                                                            :from-end t)))
                            :kind :vector
                            :length size)))))

(setf (svref *meta-room-info* simple-base-string-widetag)
      (make-room-info :name 'simple-base-string
                      :kind :string
                      :length 0))

#!+sb-unicode
(setf (svref *meta-room-info* simple-character-string-widetag)
      (make-room-info :name 'simple-character-string
                      :kind :string
                      :length 2))

(setf (svref *meta-room-info* simple-array-nil-widetag)
      (make-room-info :name 'simple-array-nil
                      :kind :fixed
                      :length 2))

(setf (svref *meta-room-info* code-header-widetag)
      (make-room-info :name 'code
                      :kind :code))

(setf (svref *meta-room-info* instance-header-widetag)
      (make-room-info :name 'instance
                      :kind :instance))

) ; EVAL-WHEN

(defparameter *room-info* '#.*meta-room-info*)
(deftype spaces () '(member :static :dynamic :read-only))

;;;; MAP-ALLOCATED-OBJECTS

;;; Since they're represented as counts of words, we should never
;;; need bignums to represent these:
(declaim (type fixnum
               *static-space-free-pointer*
               *read-only-space-free-pointer*))

(defun space-bounds (space)
  (declare (type spaces space))
  (ecase space
    (:static
     (values (int-sap static-space-start)
             (int-sap (* *static-space-free-pointer* n-word-bytes))))
    (:read-only
     (values (int-sap read-only-space-start)
             (int-sap (* *read-only-space-free-pointer* n-word-bytes))))
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

;;; Return the total size of a vector in bytes, including any pad.
#!-sb-fluid (declaim (inline vector-total-size))
(defun vector-total-size (obj info)
  (let ((shift (room-info-length info))
        (len (+ (length (the (simple-array * (*)) obj))
                (ecase (room-info-kind info)
                  (:vector 0)
                  (:string 1)))))
    (round-to-dualword
     (+ (* vector-data-offset n-word-bytes)
        (if (minusp shift)
            (ash (+ len (1- (ash 1 (- shift))))
                 shift)
            (ash len shift))))))

;;; Access to the GENCGC page table for better precision in
;;; MAP-ALLOCATED-OBJECTS
#!+gencgc
(progn
  (define-alien-type (struct page)
      (struct page
              (start long)
              ;; On platforms with small enough GC pages, this field
              ;; will be a short. On platforms with larger ones, it'll
              ;; be an int.
              (bytes-used (unsigned
                           #.(if (typep sb!vm:gencgc-page-bytes
                                        '(unsigned-byte 16))
                                 16
                                 32)))
              (flags (unsigned 8))
              (gen (signed 8))))
  (declaim (inline find-page-index))
  (define-alien-routine "find_page_index" long (index long))
  (define-alien-variable "page_table" (* (struct page))))

;;; Iterate over all the objects allocated in SPACE, calling FUN with
;;; the object, the object's type code, and the object's total size in
;;; bytes, including any header and padding. CAREFUL makes
;;; MAP-ALLOCATED-OBJECTS slightly more accurate, but a lot slower: it
;;; is intended for slightly more demanding uses of heap groveling
;;; then ROOM.
#!-sb-fluid (declaim (maybe-inline map-allocated-objects))
(defun map-allocated-objects (fun space &optional careful)
  (declare (type function fun) (type spaces space))
  (flet ((make-obj (tagged-address)
           (if careful
               (make-lisp-obj tagged-address nil)
               (values (%make-lisp-obj tagged-address) t))))
    ;; Inlining MAKE-OBJ reduces consing on platforms where dynamic
    ;; space extends past fixnum range.
    (declare (inline make-obj))
    (without-gcing
      (multiple-value-bind (start end) (space-bounds space)
        (declare (type system-area-pointer start end))
        (declare (optimize (speed 3)))
        (let ((current start)
              #!+gencgc
              (skip-tests-until-addr 0))
          (labels ((maybe-finish-mapping ()
                     (unless (sap< current end)
                       (aver (sap= current end))
                       (return-from map-allocated-objects)))
                   ;; GENCGC doesn't allocate linearly, which means that the
                   ;; dynamic space can contain large blocks zeros that get
                   ;; accounted as conses in ROOM (and slow down other
                   ;; applications of MAP-ALLOCATED-OBJECTS). To fix this
                   ;; check the GC page structure for the current address.
                   ;; If the page is free or the address is beyond the page-
                   ;; internal allocation offset (bytes-used) skip to the
                   ;; next page immediately.
                   (maybe-skip-page ()
                     #!+gencgc
                     (when (eq space :dynamic)
                       (loop with page-mask = #.(1- sb!vm:gencgc-page-bytes)
                             for addr of-type sb!vm:word = (sap-int current)
                             while (>= addr skip-tests-until-addr)
                             do
                             ;; For some reason binding PAGE with LET
                             ;; conses like mad (but gives no compiler notes...)
                             ;; Work around the problem with SYMBOL-MACROLET
                             ;; instead of trying to figure out the real
                             ;; issue. -- JES, 2005-05-17
                             (symbol-macrolet
                                 ((page (deref page-table
                                               (find-page-index addr))))
                               ;; Don't we have any nicer way to access C struct
                               ;; bitfields?
                               (let ((alloc-flag (ldb (byte 3 2)
                                                      (slot page 'flags)))
                                     (bytes-used (slot page 'bytes-used)))
                                 ;; If the page is not free and the current
                                 ;; pointer is still below the allocation offset
                                 ;; of the page
                                 (when (and (not (zerop alloc-flag))
                                            (<= (logand page-mask addr)
                                                bytes-used))
                                   ;; Don't bother testing again until we
                                   ;; get past that allocation offset
                                   (setf skip-tests-until-addr
                                         (+ (logandc2 addr page-mask) bytes-used))
                                   ;; And then continue with the
                                   ;; scheduled mapping
                                   (return-from maybe-skip-page))
                                 ;; Move CURRENT to start of next page.
                                 (setf current (int-sap (+ (logandc2 addr page-mask)
                                                           sb!vm:gencgc-page-bytes)))
                                 (maybe-finish-mapping))))))
                   (maybe-map (obj obj-tag n-obj-bytes &optional (ok t))
                     (let ((next (typecase n-obj-bytes
                                   (fixnum (sap+ current n-obj-bytes))
                                   (integer (sap+ current n-obj-bytes)))))
                       ;; If this object would take us past END, it must
                       ;; be either bogus, or it has been allocated after
                       ;; the call to M-A-O.
                       (cond ((and ok next (sap<= next end))
                              (funcall fun obj obj-tag n-obj-bytes)
                              (setf current next))
                             (t
                              (setf current (sap+ current n-word-bytes)))))))
            (declare (inline maybe-finish-mapping maybe-skip-page maybe-map))
            (loop
              (maybe-finish-mapping)
              (maybe-skip-page)
              (let* ((header (sap-ref-word current 0))
                     (header-widetag (logand header #xFF))
                     (info (svref *room-info* header-widetag)))
                (cond
                  ((or (not info)
                       (eq (room-info-kind info) :lowtag))
                   (multiple-value-bind (obj ok)
                       (make-obj (logior (sap-int current) list-pointer-lowtag))
                     (maybe-map obj
                                list-pointer-lowtag
                                (* cons-size n-word-bytes)
                                ok)))
                  ((eql header-widetag closure-header-widetag)
                   (let* ((obj (%make-lisp-obj (logior (sap-int current)
                                                       fun-pointer-lowtag)))
                          (size (round-to-dualword
                                 (* (the fixnum (1+ (get-closure-length obj)))
                                    n-word-bytes))))
                     (maybe-map obj header-widetag size)))
                  ((eq (room-info-kind info) :instance)
                   (let* ((obj (%make-lisp-obj
                                (logior (sap-int current) instance-pointer-lowtag)))
                          (size (round-to-dualword
                                 (* (+ (%instance-length obj) 1) n-word-bytes))))
                     (aver (zerop (logand size lowtag-mask)))
                     (maybe-map obj header-widetag size)))
                  (t
                   (multiple-value-bind (obj ok)
                       (make-obj (logior (sap-int current) other-pointer-lowtag))
                     (let ((size (when ok
                                   (ecase (room-info-kind info)
                                     (:fixed
                                      (aver (or (eql (room-info-length info)
                                                     (1+ (get-header-data obj)))
                                                (floatp obj)
                                                (simple-array-nil-p obj)))
                                      (round-to-dualword
                                       (* (room-info-length info) n-word-bytes)))
                                     ((:vector :string)
                                      (vector-total-size obj info))
                                     (:header
                                      (round-to-dualword
                                       (* (1+ (get-header-data obj)) n-word-bytes)))
                                     (:code
                                      (+ (the fixnum
                                           (* (get-header-data obj) n-word-bytes))
                                         (round-to-dualword
                                          (* (the fixnum (%code-code-size obj))
                                             n-word-bytes))))))))
                       (macrolet ((frob ()
                                    '(progn
                                      (when size (aver (zerop (logand size lowtag-mask))))
                                      (maybe-map obj header-widetag size))))
                         (typecase size
                           (fixnum (frob))
                           (word (frob))
                           (null (frob))))))))))))))))


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
                   (name (room-info-name (aref *room-info* i)))
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

;;; Print info about how much code and no-ops there are in SPACE.
(defun count-no-ops (space)
  (declare (type spaces space))
  (let ((code-words 0)
        (no-ops 0)
        (total-bytes 0))
    (declare (fixnum code-words no-ops)
             (type unsigned-byte total-bytes))
    (map-allocated-objects
     (lambda (obj type size)
       (when (eql type code-header-widetag)
         (let ((words (truly-the fixnum (%code-code-size obj)))
               (sap (%primitive code-instructions obj))
               (size size))
           (declare (fixnum size))
           (incf total-bytes size)
           (incf code-words words)
           (dotimes (i words)
             (when (zerop (sap-ref-word sap (* i n-word-bytes)))
               (incf no-ops))))))
     space)

    (format t
            "~:D code-object bytes, ~:D code words, with ~:D no-ops (~D%).~%"
            total-bytes code-words no-ops
            (round (* no-ops 100) code-words)))

  (values))

(defun descriptor-vs-non-descriptor-storage (&rest spaces)
  (let ((descriptor-words 0)
        (non-descriptor-headers 0)
        (non-descriptor-bytes 0))
    (declare (type unsigned-byte descriptor-words non-descriptor-headers
                   non-descriptor-bytes))
    (dolist (space (or spaces '(:read-only :static :dynamic)))
      (declare (inline map-allocated-objects))
      (map-allocated-objects
       (lambda (obj type size)
         (case type
           (#.code-header-widetag
            (let ((inst-words (truly-the fixnum (%code-code-size obj)))
                  (size size))
              (declare (type fixnum size inst-words))
              (incf non-descriptor-bytes (* inst-words n-word-bytes))
              (incf descriptor-words
                    (- (truncate size n-word-bytes) inst-words))))
           ((#.bignum-widetag
             #.single-float-widetag
             #.double-float-widetag
             #.simple-base-string-widetag
             #!+sb-unicode #.simple-character-string-widetag
             #.simple-array-nil-widetag
             #.simple-bit-vector-widetag
             #.simple-array-unsigned-byte-2-widetag
             #.simple-array-unsigned-byte-4-widetag
             #.simple-array-unsigned-byte-8-widetag
             #.simple-array-unsigned-byte-16-widetag
             #.simple-array-unsigned-byte-32-widetag
             #.simple-array-signed-byte-8-widetag
             #.simple-array-signed-byte-16-widetag
             ;; #.simple-array-signed-byte-30-widetag
             #.simple-array-signed-byte-32-widetag
             #.simple-array-single-float-widetag
             #.simple-array-double-float-widetag
             #.simple-array-complex-single-float-widetag
             #.simple-array-complex-double-float-widetag)
            (incf non-descriptor-headers)
            (incf non-descriptor-bytes (- size n-word-bytes)))
           ((#.list-pointer-lowtag
             #.instance-pointer-lowtag
             #.ratio-widetag
             #.complex-widetag
             #.simple-array-widetag
             #.simple-vector-widetag
             #.complex-base-string-widetag
             #.complex-vector-nil-widetag
             #.complex-bit-vector-widetag
             #.complex-vector-widetag
             #.complex-array-widetag
             #.closure-header-widetag
             #.funcallable-instance-header-widetag
             #.value-cell-header-widetag
             #.symbol-header-widetag
             #.sap-widetag
             #.weak-pointer-widetag
             #.instance-header-widetag)
            (incf descriptor-words (truncate (the fixnum size) n-word-bytes)))
           (t
            (error "bogus widetag: ~W" type))))
       space))
    (format t "~:D words allocated for descriptor objects.~%"
            descriptor-words)
    (format t "~:D bytes data/~:D words header for non-descriptor objects.~%"
            non-descriptor-bytes non-descriptor-headers)
    (values)))

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
         (let* ((classoid (layout-classoid (%instance-ref obj 0)))
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
           (pagesize (sb!sys:get-page-size))
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
          (dotimes (i (%instance-length obj))
            (when (eq (%instance-ref obj i) object)
              (maybe-call fun obj)
              (return))))
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
                    (eq (symbol-plist obj) object)
                    (and (boundp obj)
                         (eq (symbol-value obj) object)))
            (maybe-call fun obj)))))
     space)))

(defun list-referencing-objects (space object)
  (collect ((res))
    (map-referencing-objects
     (lambda (obj) (res obj)) space object)
    (res)))
