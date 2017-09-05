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

;;;; SIMPLE-FUN and accessors

(deftype simple-fun ()
  '(satisfies simple-fun-p))

(declaim (inline code-header-words))
(defun code-header-words (code)
  (logand (get-header-data code) sb!vm:short-header-max-words))

;;; Extract halves of SIMPLE-FUN-INFO, which is a string if it holds
;;; documentation, a SIMPLE-VECTOR if XREFS,
;;; or (CONS STRING SIMPLE-VECTOR) for both, or NIL if neither.
(macrolet ((def (name info-part if-simple-vector if-string)
             `(defun ,name (simple-fun)
                (declare (simple-fun simple-fun))
                (let ((info (%simple-fun-info simple-fun)))
                  (typecase info
                    (list (,info-part info))
                    (simple-vector ,if-simple-vector)
                    (string ,if-string)
                    (t (bug "bogus INFO for ~S: ~S" simple-fun info)))))))
  (def %simple-fun-doc   car nil info)
  (def %simple-fun-xrefs cdr info nil))

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

;;; Extract the type from the function header FUNC.
(defun %simple-fun-type (func)
  (let ((internal-type (sb!vm::%%simple-fun-type func)))
    ;; For backward-compatibility we expand SFUNCTION -> FUNCTION.
    (if (and (listp internal-type) (eq (car internal-type) 'sfunction))
        (sb!ext:typexpand-1 internal-type)
        internal-type)))

(defun %code-entry-points (code-obj) ; DO NOT USE IN NEW CODE
  (%code-entry-point code-obj 0))

(defun %simple-fun-next (simple-fun) ; DO NOT USE IN NEW CODE
  (let ((code-obj (fun-code-header simple-fun)))
    (dotimes (i (code-n-entries code-obj))
      (when (eq simple-fun (%code-entry-point code-obj i))
        (return (%code-entry-point code-obj (1+ i)))))))

;;;; CLOSURE type and accessors

(deftype closure ()
  '(satisfies closurep))

;;; FIXME: this should probably exclude the closure name slot, if named
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

(defun %set-vector-raw-bits (object offset value)
  (setf (%vector-raw-bits object offset) value))

;;; A unique GC id. This is supplied for code that needs to detect
;;; whether a GC has happened since some earlier point in time. For
;;; example:
;;;
;;;   (let ((epoch *gc-epoch*))
;;;      ...
;;;      (unless (eql epoch *gc-epoch)
;;;        ....))
;;;
;;; This isn't just a fixnum counter since then we'd have theoretical
;;; problems when exactly 2^29 GCs happen between epoch
;;; comparisons. Unlikely, but the cost of using a cons instead is too
;;; small to measure. -- JES, 2007-09-30
(declaim (type cons *gc-epoch*))
(!defglobal *gc-epoch* '(nil . nil))

(declaim (inline lowtag-of))
(defun lowtag-of (x) (logand (get-lisp-obj-address x) sb!vm:lowtag-mask))

;;; Unlike most other "Stub functions" that never called called except
;;; by the interpreter, these two do get called, by MAKE-UNPORTABLE-FLOAT
(defun make-single-float (x) (make-single-float x))
(defun make-double-float (hi lo) (make-double-float hi lo))
