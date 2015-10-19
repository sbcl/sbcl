;;;; miscellaneous kernel-level definitions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;; Return the 24 bits of data in the header of object X, which must
;;; be an other-pointer object.
(defun get-header-data (x)
  (get-header-data x))

;;; Set the 24 bits of data in the header of object X (which must be
;;; an other-pointer object) to VAL.
(defun set-header-data (x val)
  (set-header-data x val))

;;; Return the 24 bits of data in the header of object X, which must
;;; be a fun-pointer object.
;;;
;;; FIXME: Should this not be called GET-FUN-LENGTH instead? Or even better
;;; yet, if GET-HEADER-DATA masked the lowtag instead of substracting it, we
;;; could just use it instead -- or at least this could just be a function on
;;; top of the same VOP.
(defun get-closure-length (x)
  (get-closure-length x))

(defun lowtag-of (x)
  (lowtag-of x))

(defun widetag-of (x)
  (widetag-of x))

;;; WIDETAG-OF needs extra code to handle LIST and FUNCTION lowtags. When
;;; we're only dealing with other pointers (eg. when dispatching on array
;;; element type), this is going to be faster.
(declaim (inline %other-pointer-widetag))
(defun %other-pointer-widetag (x)
  (sb!sys:sap-ref-8 (int-sap (get-lisp-obj-address x))
                    #.(ecase sb!c:*backend-byte-order*
                        (:little-endian
                         (- sb!vm:other-pointer-lowtag))
                        (:big-endian
                         (- (1- sb!vm:n-word-bytes) sb!vm:other-pointer-lowtag)))))

;;; Return a System-Area-Pointer pointing to the data for the vector
;;; X, which must be simple.
;;;
;;; FIXME: So it should be SIMPLE-VECTOR-SAP, right? (or UNHAIRY-VECTOR-SAP,
;;; if the meaning is (SIMPLE-ARRAY * 1) instead of SIMPLE-VECTOR)
;;; (or maybe SIMPLE-VECTOR-DATA-SAP or UNHAIRY-VECTOR-DATA-SAP?)
(defun vector-sap (x)
  (declare (type (simple-unboxed-array (*)) x))
  (vector-sap x))

;;; Return a System-Area-Pointer pointing to the end of the binding stack.
(defun sb!c::binding-stack-pointer-sap ()
  (sb!c::binding-stack-pointer-sap))

;;; Return a System-Area-Pointer pointing to the next free word of the
;;; current dynamic space.
(defun sb!c::dynamic-space-free-pointer ()
  (sb!c::dynamic-space-free-pointer))

;;; Return a System-Area-Pointer pointing to the end of the control stack.
(defun sb!c::control-stack-pointer-sap ()
  (sb!c::control-stack-pointer-sap))

;;; FDEFN -> FUNCTION
(defun sb!c:safe-fdefn-fun (x) (sb!c:safe-fdefn-fun x))

;;; Return the header typecode for FUNCTION. Can be set with SETF.
(defun fun-subtype (function)
  (fun-subtype function))
(defun (setf fun-subtype) (type function)
  (setf (fun-subtype function) type))

;;;; SIMPLE-FUN and accessors

(defun simple-fun-p (object)
  (simple-fun-p object))

(deftype simple-fun ()
  '(satisfies simple-fun-p))

(defun %simple-fun-doc (simple-fun)
  (declare (simple-fun simple-fun))
  (let ((info (%simple-fun-info simple-fun)))
    (cond ((typep info '(or null string))
           info)
          ((simple-vector-p info)
           nil)
          ((consp info)
           (car info))
          (t
           (bug "bogus INFO for ~S: ~S" simple-fun info)))))

(defun (setf %simple-fun-doc) (doc simple-fun)
  (declare (type (or null string) doc)
           (simple-fun simple-fun))
  (let ((info (%simple-fun-info simple-fun)))
    (setf (%simple-fun-info simple-fun)
          (cond ((typep info '(or null string))
                 doc)
                ((simple-vector-p info)
                 (if doc
                     (cons doc info)
                     info))
                ((consp info)
                 (if doc
                     (cons doc (cdr info))
                     (cdr info)))
                (t
                 (bug "bogus INFO for ~S: ~S" simple-fun info))))))

(defun %simple-fun-xrefs (simple-fun)
  (declare (simple-fun simple-fun))
  (let ((info (%simple-fun-info simple-fun)))
    (cond ((typep info '(or null string))
           nil)
          ((simple-vector-p info)
           info)
          ((consp info)
           (cdr info))
          (t
           (bug "bogus INFO for ~S: ~S" simple-fun info)))))

;;; Extract the arglist from the function header FUNC.
(defun %simple-fun-arglist (func)
  (%simple-fun-arglist func))

(defun (setf %simple-fun-arglist) (new-value func)
  (setf (%simple-fun-arglist func) new-value))

;;; Extract the name from the function header FUNC.
(defun %simple-fun-name (func)
  (%simple-fun-name func))

(defun (setf %simple-fun-name) (new-value func)
  (setf (%simple-fun-name func) new-value))

;;; Extract the type from the function header FUNC.
(defun %simple-fun-type (func)
  (%simple-fun-type func))

(defun %simple-fun-next (simple-fun)
  (%simple-fun-next simple-fun))

;; Given either a closure or a simple-fun, return the underlying simple-fun.
;; FIXME: %SIMPLE-FUN-SELF is a somewhat poor name for this function.
;; The x86[-64] code defines %CLOSURE-FUN as nothing more than %SIMPLE-FUN-SELF,
;; and it's not clear whether that's because callers need the "simple" accessor
;; to work on closures, versus reluctance to define a %CLOSURE/SIMPLE-FUN-FUN
;; reader. %FUN-FUN works on all three function subtypes, but is nontrivial.
;; Preferably at least one accessor should get a new name,
;; so that %SIMPLE-FUN-SELF can mean what it says.

(defun %simple-fun-self (simple-fun)
  (%simple-fun-self simple-fun))

;;;; CLOSURE type and accessors

(defun closurep (object)
  (closurep object))

(deftype closure ()
  '(satisfies closurep))

(defmacro do-closure-values ((value closure) &body body)
  (with-unique-names (i nclosure)
    `(let ((,nclosure ,closure))
       (declare (closure ,nclosure))
       (dotimes (,i (- (1+ (get-closure-length ,nclosure)) sb!vm:closure-info-offset))
         (let ((,value (%closure-index-ref ,nclosure ,i)))
           ,@body)))))

(defun %closure-values (closure)
  (declare (closure closure))
  (let (values)
    (do-closure-values (elt closure)
      (push elt values))
    (nreverse values)))

;;; Extract the function from CLOSURE.
(defun %closure-fun (closure)
  (%closure-fun closure))

;;; Extract the INDEXth slot from CLOSURE.
(defun %closure-index-ref (closure index)
  (%closure-index-ref closure index))

;;; Return the length of VECTOR. There is no reason to use this in
;;; ordinary code, 'cause length (the vector foo)) is the same.
(defun sb!c::vector-length (vector)
  (sb!c::vector-length vector))

;;; Allocate a unboxed, simple vector with type code TYPE, length LENGTH, and
;;; WORDS words long. Note: it is your responsibility to ensure that the
;;; relation between LENGTH and WORDS is correct.
(defun allocate-vector (type length words)
  (allocate-vector type length words))

;;; Allocate an array header with type code TYPE and rank RANK.
(defun make-array-header (type rank)
  (make-array-header type rank))

;;; Return a SAP pointing to the instructions part of CODE-OBJ.
(defun code-instructions (code-obj)
  (code-instructions code-obj))

;;; Extract the INDEXth element from the header of CODE-OBJ. Can be
;;; set with SETF.
(defun code-header-ref (code-obj index)
  (code-header-ref code-obj index))

(defun code-header-set (code-obj index new)
  (code-header-set code-obj index new))

(defun %vector-raw-bits (object offset)
  (declare (type index offset))
  (%vector-raw-bits object offset))

(defun %set-vector-raw-bits (object offset value)
  (declare (type index offset))
  (declare (type word value))
  (setf (%vector-raw-bits object offset) value))

(defun make-single-float (x) (make-single-float x))
(defun make-double-float (hi lo) (make-double-float hi lo))

(defun single-float-bits (x) (single-float-bits x))
(defun double-float-high-bits (x) (double-float-high-bits x))
(defun double-float-low-bits (x) (double-float-low-bits x))

(defun value-cell-ref (x) (value-cell-ref x))
