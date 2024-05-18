;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

;;; ANSI doesn't guarantee the existence of specialized vectors
;;; other than T, BIT, CHARACTER.
;;; Thus, if we do
;;;   (MAKE-ARRAY 10 :ELEMENT-TYPE '(UNSIGNED-BYTE 4))
;;; in the cross-compilation host, we could easily end up with a
;;; vector of (UNSIGNED-BYTE 8) or of T, and the dumped result would
;;; reflect this.
;;;
;;; To reduce the prominence of this issue in cross-compilation, we
;;; record arrays that should be specialized in a hashtable.
;;; Fasl dumping will complain about a specialized array that does not
;;; have an entry in the table.

;;; Previously some specialized arrays were "weakened" to (ARRAY T) in the
;;; cross-compiler which served to show that the code was indifferent to
;;; specialization, but made no guarantees about what array type was dumped.
;;; Explicit code was needed to make correct constant arrays at load-time.
;;; The current approach permits use of array constants in an easy way that
;;; avoids host-Lisp-based reflection, and avoids having a DEFTYPE that
;;; changes its meaning between the host and target compilations.

;;; The motivation for this host-agnostic approach is that it supports dumping
;;; (UNSIGNED-BYTE 64) array literals in a 32-bit cross-compilation host,
;;; where that array type is almost surely upgraded to (ARRAY T).
;;; Therefore a host-reflection-based mechanism would be almost certain to fail.

;;; In case anyone wants to rewrite this yet again, here's an alternate way
;;; that was deemed elegant but difficult: in cross-compilation, all arrays
;;; were wrapped in an XC-ARRAY-WRAPPER struct consisting of one slot for
;;; the desired element-type and one slot with the real array.  All affected
;;; uses of AREF and (SETF AREF) had to be macroized so that the cross-compiler
;;; could use (AREF (XC-ARRAY-WRAPPER-DATA obj) index) where the real compiler,
;;; and all code compiled by it, would just use AREF using a single abstraction.
;;; CTYPE-OF was hacked to return ARRAY for an xc-array-wrapper which
;;; meant that the cross-compiler thought that transforms on arrays should run
;;; on wrappers, e.g. the foldable function LENGTH should look into wrappers,
;;; as could bounds-checks (ARRAY-DIMENSION). This technique led to confusing
;;; code within the compiler and was abandoned in favor of the hashtable.

;; Use this only for array specializations that are not required by ANSI.
;; Because this is not performance-critical, we can just punt to a function.
;; If no contents given, explicitly 0-fill in case element-type upgrades to T
;; and would get a default of NIL where we would use 0 in our specialization.

(defvar *array-to-specialization* (make-hash-table :test #'eq))

(defun sb-xc:make-array (dims &key (element-type 't)
                                   (initial-contents nil contentsp)
                                   (initial-element 0)
                                   (retain-specialization-for-after-xc-core))
  ;; ECL fails to compile MAKE-ARRAY when keyword args are not literal keywords. e.g.:
  ;; (DEFUN TRY (DIMS SELECT VAL)
  ;;   (MAKE-ARRAY DIMS (IF SELECT :INITIAL-CONTENTS :INITIAL-ELEMENT) VAL)) ->
  ;; "The macro form (MAKE-ARRAY DIMS (IF SELECT :INITIAL-CONTENTS :INITIAL-ELEMENT) VAL)
  ;;  was not expanded successfully.
  ;;  Error detected:
  ;;  The key (IF SELECT :INITIAL-CONTENTS :INITIAL-ELEMENT) is not allowed"
  #+host-quirks-ecl (declare (notinline cl:make-array))

  (aver element-type)
  ;; Canonicalize
  (setq element-type (type-specifier (specifier-type element-type)))
  ;; Expressed type must be _exactly_ one of the supported ones.
  (aver (find (case element-type
                #-sb-unicode (base-char 'character)
                (t element-type))
              sb-vm:*specialized-array-element-type-properties*
              :key #'sb-vm:saetp-specifier :test 'equal))

  (let ((array (cl:make-array dims
                              :element-type element-type
                              (if contentsp :initial-contents :initial-element)
                              (if contentsp initial-contents initial-element))))
    (unless (eq element-type 't)
      (setf (gethash array *array-to-specialization*)
            (cons element-type retain-specialization-for-after-xc-core)))
    array))
(defun sb-xc:array-element-type (array)
  (cond ((car (gethash array *array-to-specialization*)))
        ((bit-vector-p array) 'bit)
        ((stringp array) 'base-char)
        (t t)))

(defun target-specialized-array-p (array)
  (if (gethash array *array-to-specialization*) t nil))
(deftype sb-xc:simple-vector ()
  '(and cl:simple-vector (not (satisfies target-specialized-array-p))))

(defun %other-pointer-widetag (x)
  (if (bit-vector-p x)
      sb-vm:simple-bit-vector-widetag
      (sb-vm:saetp-typecode
       (find (array-element-type x)
             sb-vm:*specialized-array-element-type-properties*
             :key #'sb-vm:saetp-specifier :test #'equal))))

(defun sb-cold::clear-specialized-array-registry ()
  (let ((registry *array-to-specialization*))
    (maphash (lambda (key value)
               (unless (cdr value) (remhash key registry))) ; cdr = "retain"
             registry)))

(defun our-sharp-a-reader (stream char rank)
  (declare (ignore char))
  (assert (not rank))
  (let ((contents (read stream t nil t)))
    ;; Just like in src/code/sharpm
    (destructuring-bind (dimensions type &rest contents) contents
      (sb-xc:make-array dimensions :initial-contents contents :element-type type))))

(defun sb-impl::read-ub8-vector (pathname)
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (let* ((length (file-length stream))
           (array (sb-xc:make-array length :element-type '(unsigned-byte 8)
                                           :retain-specialization-for-after-xc-core t)))
      (read-sequence array stream)
      array)))

(defun sb-impl::ubN-array-from-octets (raw-bytes element-type raw-octets-per-elt)
  (let* ((n raw-octets-per-elt)
         (array (sb-xc:make-array (/ (length raw-bytes) n) :element-type element-type
                                  :retain-specialization-for-after-xc-core t)))
    (loop for i from 0 below (length raw-bytes) by n
          do (loop with element = 0
                   for offset from 0 below n
                   do (incf element (ash (aref raw-bytes (+ i offset))
                                         (* 8 (- n offset 1))))
                   finally (setf (aref array (/ i n)) element)))
    array))
