;;;; various DEFSETFs, pulled into one file for convenience in doing
;;;; them as early in the build process as possible so as to avoid
;;;; hassles with invoking SETF FOO before DEFSETF FOO and thus
;;;; compiling a call to some nonexistent function #'(SETF FOO)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;; from alieneval.lisp
(in-package "SB!ALIEN")
(defsetf slot %set-slot)
(defsetf deref (alien &rest indices) (value)
  `(%set-deref ,alien ,value ,@indices))
(defsetf %heap-alien %set-heap-alien)

;;; from arch-vm.lisp
(in-package "SB!VM")
(defsetf context-register %set-context-register)
(defsetf context-float-register %set-context-float-register)
;;; from bit-bash.lisp
(defsetf word-sap-ref %set-word-sap-ref)

;;; from debug-int.lisp
(in-package "SB!DI")
(defsetf stack-ref %set-stack-ref)
(defsetf debug-var-value %set-debug-var-value)
(defsetf debug-var-value %set-debug-var-value)
(defsetf breakpoint-info %set-breakpoint-info)

;;; from bignum.lisp
(in-package "SB!IMPL")
(defsetf %bignum-ref %bignum-set)

;;; from defstruct.lisp
(defsetf %instance-ref %instance-set)

(defsetf %raw-instance-ref/word %raw-instance-set/word)
(defsetf %raw-instance-ref/single %raw-instance-set/single)
(defsetf %raw-instance-ref/double %raw-instance-set/double)
(defsetf %raw-instance-ref/complex-single %raw-instance-set/complex-single)
(defsetf %raw-instance-ref/complex-double %raw-instance-set/complex-double)

(defsetf %instance-layout %set-instance-layout)
(defsetf %funcallable-instance-info %set-funcallable-instance-info)
(defsetf %funcallable-instance-layout %set-funcallable-instance-layout)

;;; from early-setf.lisp

;;; (setf aref/bit/sbit) are implemented using setf-functions,
;;; because they have to work with (setf (apply #'aref array subscripts))
;;; All other setfs can be done using setf-functions too, but I
;;; haven't found technical advantages or disadvantages for either
;;; scheme.
(defsetf car   %rplaca)
(defsetf first %rplaca)
(defsetf cdr   %rplacd)
(defsetf rest  %rplacd)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun %cxr-setf-expander (sub-accessor setter)
  (flet ((expand (place-reader original-form)
           (let ((temp (make-symbol "LIST"))
                 (newval (make-symbol "NEW")))
             (values (list temp)
                     `((,@place-reader ,@(cdr original-form)))
                     (list newval)
                     `(,setter ,temp ,newval)
                     `(,(if (eq setter '%rplacd) 'cdr 'car) ,temp)))))
    (if (eq sub-accessor 'nthcdr) ; random N
        (lambda (access-form env)
          (declare (ignore env))
          (declare (sb!c::lambda-list (n list)))
          (destructuring-bind (n list) (cdr access-form) ; for effect
            (declare (ignore n list)))
          (expand '(nthcdr) access-form))
        ;; NTHCDR of fixed N, or CxxxxR composition
        (lambda (access-form env)
          (declare (ignore env))
          (declare (sb!c::lambda-list (list)))
          (destructuring-bind (list) (cdr access-form) ; for effect
            (declare (ignore list)))
          (expand sub-accessor access-form))))))

(macrolet ((def (name &optional alias &aux (string (string name)))
             `(eval-when (:compile-toplevel :load-toplevel :execute)
                (let ((closure
                       (%cxr-setf-expander
                        '(,(symbolicate "C" (subseq string 2)))
                        ',(symbolicate "%RPLAC" (subseq string 1 2)))))
                  (%defsetf ',name closure nil)
                  ,@(when alias
                      `((%defsetf ',alias closure nil)))))))
  ;; Rather than expand into a DEFINE-SETF-EXPANDER, install a single closure
  ;; as the expander and capture just enough to distinguish the variations.
  (def caar)
  (def cadr second)
  (def cdar)
  (def cddr)
  (def caaar)
  (def cadar)
  (def cdaar)
  (def cddar)
  (def caadr)
  (def caddr third)
  (def cdadr)
  (def cdddr)
  (def caaaar)
  (def cadaar)
  (def cdaaar)
  (def cddaar)
  (def caadar)
  (def caddar)
  (def cdadar)
  (def cdddar)
  (def caaadr)
  (def cadadr)
  (def cdaadr)
  (def cddadr)
  (def caaddr)
  (def cadddr fourth)
  (def cdaddr)
  (def cddddr))

;; FIFTH through TENTH
(macrolet ((def (name subform)
             `(eval-when (:compile-toplevel :load-toplevel :execute)
                (%defsetf ',name (%cxr-setf-expander ',subform '%rplaca) nil))))
  (def fifth   (nthcdr 4)) ; or CDDDDR
  (def sixth   (nthcdr 5))
  (def seventh (nthcdr 6))
  (def eighth  (nthcdr 7))
  (def ninth   (nthcdr 8))
  (def tenth   (nthcdr 9)))

;; CLHS says under the entry for NTH:
;; "nth may be used to specify a place to setf. Specifically,
;;  (setf (nth n list) new-object) == (setf (car (nthcdr n list)) new-object)"
;; which means that it's wrong to use %SETNTH because in the second form,
;; (NTHCDR ...) is a subform of the CAR expression, and so must be
;; bound to a temporary variable.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (%defsetf 'nth (%cxr-setf-expander 'nthcdr '%rplaca) nil))

(defsetf elt %setelt)
(defsetf row-major-aref %set-row-major-aref)
(defsetf svref %svset)
(defsetf char %charset)
(defsetf schar %scharset)
(defsetf %array-dimension %set-array-dimension)
(defsetf %vector-raw-bits %set-vector-raw-bits)
(defsetf symbol-value set)
(defsetf symbol-global-value set-symbol-global-value)
(defsetf symbol-plist %set-symbol-plist)
(defsetf fill-pointer %set-fill-pointer)
(defsetf sap-ref-8 %set-sap-ref-8)
(defsetf signed-sap-ref-8 %set-signed-sap-ref-8)
(defsetf sap-ref-16 %set-sap-ref-16)
(defsetf signed-sap-ref-16 %set-signed-sap-ref-16)
(defsetf sap-ref-32 %set-sap-ref-32)
(defsetf signed-sap-ref-32 %set-signed-sap-ref-32)
(defsetf sap-ref-64 %set-sap-ref-64)
(defsetf signed-sap-ref-64 %set-signed-sap-ref-64)
(defsetf sap-ref-word %set-sap-ref-word)
(defsetf signed-sap-ref-word %set-signed-sap-ref-word)
(defsetf sap-ref-sap %set-sap-ref-sap)
(defsetf sap-ref-lispobj %set-sap-ref-lispobj)
(defsetf sap-ref-single %set-sap-ref-single)
(defsetf sap-ref-double %set-sap-ref-double)
#!+long-float (defsetf sap-ref-long %set-sap-ref-long)
(defsetf subseq (sequence start &optional end) (v)
  `(progn (replace ,sequence ,v :start1 ,start :end1 ,end) ,v))

;;; from fdefinition.lisp
(defsetf fdefinition %set-fdefinition)

;;; from kernel.lisp
(defsetf code-header-ref code-header-set)
