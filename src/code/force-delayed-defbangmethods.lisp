;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-PCL")

(loop for (gf-name . methods) in *!trivial-methods*
      do
      (assert (generic-function-p (fdefinition gf-name)))
      (when (eq gf-name 'print-object)
        ;; It is not strictly necessary to remove CONDITION,
        ;; since its  method would just get overwritten by the
        ;; delayed "full" method, but I prefer not to accidentally
        ;; install the wrong method even temporarily.
        (setq methods
              (remove-if (lambda (x) (member x '(t condition)))
                         methods :key #'car)))
      (loop for (specializer lambda-list fmf source-loc) across methods
            do (multiple-value-bind (specializers arg-info)
                   (ecase gf-name
                     (print-object
                      (values (list (find-class specializer) (find-class t))
                              '(:arg-info (2))))
                     (make-load-form
                      (values (list (find-class specializer))
                              '(:arg-info (1 . t)))))
                 (load-defmethod
                  'standard-method gf-name '() specializers lambda-list
                  `(:function
                    ,(let ((mf (%make-method-function fmf nil)))
                       (sb-mop:set-funcallable-instance-function
                        mf
                        (method-function-from-fast-function fmf arg-info))
                       mf)
                    plist ,arg-info simple-next-method-call t)
                  source-loc))))

;;; Print-object methods on subtypes of CONDITION can't be cross-compiled
;;; until CLOS is fully working. Compile them now.
#.`(progn
     ,@(mapcar (lambda (args)
                 `(setf (slot-value (defmethod ,@(cdr args)) 'source)
                        ,(car args)))
               *!delayed-defmethod-args*))

;;; Ordinary DEFMETHOD should be used from here on out.
;;; This variable actually has some semantics to being unbound.
;;; FIXME: see if we can eliminate the associated hack in 'methods.lisp'
(makunbound '*!delayed-defmethod-args*)
