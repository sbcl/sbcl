;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-PCL")

(defvar *!trivial-methods* '()) ; necessary methods for system startup
(defvar *!documentation-methods* nil) ; saved up for after PCL bootstrap
(defun !trivial-defmethod (dummy name specializer qualifiers lambda-list lambda source-loc)
  (declare (ignore dummy)) ; this would be the method class in LOAD-DEFMETHOD
  (let ((gf (assoc name *!trivial-methods*)))
    ;; Append the method but don't bother finding a predicate for it.
    ;; Methods occurring in early warm load (notably from SB-FASTEVAL)
    ;; will be properly installed when 'pcl/print-object.lisp' is loaded.
    (rplacd gf (concatenate 'vector (cdr gf)
                            (list (vector nil ; guard
                                          qualifiers specializer lambda
                                          lambda-list source-loc))))))

;;; Slow-but-correct logic for single-dispatch sans method combination,
;;; allowing exactly one primary method. Methods are sorted most-specific-first,
;;; so we can stop looking as soon as a match is found.
;;; Sorting is performed during genesis.
(defun trivial-call-a-method (gf-name specialized-arg &rest rest)
  (let* ((methods (the simple-vector
                       (cdr (or (assoc gf-name *!trivial-methods*)
                                (error "No methods on ~S" gf-name)))))
         ;; WRAPPER-OF can't be called until its constants have been patched in,
         ;; which is potentially too early in cold init especially if trying
         ;; to debug to figure out what has been patched in.
         ;; And sometimes the thing we need to print is a FMT-CONTROL,
         ;; which means we can see a funcallable-instance here.
         (arg-wrapper
          (cond ((%instancep specialized-arg) (%instance-wrapper specialized-arg))
                ((funcallable-instance-p specialized-arg) (%fun-wrapper specialized-arg))
                ;; Non-instance-like types always call a predicate.
                (t #.(find-layout 't))))
         (applicable-method
          ;; Each "method" is represented as a vector:
          ;;  #(#<GUARD> QUALIFIERS SPECIALIZER #<FMF> LAMBDA-LIST SOURCE-LOC)
          ;; SPECIALIZER is either a symbol for a classoid, or a genesis-time #<LAYOUT>.
          ;; Pick the first applicable one.
          (find-if (lambda (method)
                     (and (null (svref method 1)) ; only primary methods are candidates
                          (let ((guard (the symbol (svref method 0))))
                            (if (fboundp guard)
                                (funcall guard specialized-arg)
                                (let ((test-wrapper (svref method 2)))
                                  (and (sb-kernel::wrapper-p test-wrapper)
                                       (or (find test-wrapper (wrapper-inherits arg-wrapper))
                                           (eq arg-wrapper test-wrapper))))))))
                   methods)))
    (if applicable-method
        ;; Call using no permutation-vector / no precomputed next method.
        (apply (svref applicable-method 3) nil nil specialized-arg rest)
        (error "No applicable method for ~S on ~S~%" gf-name
               (type-of specialized-arg)))))

(defun make-load-form (object &optional environment)
  (trivial-call-a-method 'make-load-form object environment))
(defun print-object (object stream)
  (trivial-call-a-method 'print-object object stream))

(macrolet ((ensure-gfs (names)
             `(progn ,@(mapcar (lambda (name)
                                 `(defun ,name (x) (trivial-call-a-method ',name x)))
                               names))))
  (ensure-gfs (open-stream-p interactive-stream-p input-stream-p output-stream-p
               stream-element-type)))
(defun close (x &key abort) (trivial-call-a-method 'close x :abort abort))

;;; FIXME: this no longer holds methods, but it seems to have an effect
;;; on the caching of a discriminating function for PRINT-OBJECT
(defvar *!delayed-defmethod-args* nil)

;;; This exists only to show that the cross-compiler can constant-fold
;;; a constant index into a literal array without crashing.
(defun !test-svref-folding ()
  (let ((z #(42 test)))
    (if (< (svref z 0) 100) t nil)))
