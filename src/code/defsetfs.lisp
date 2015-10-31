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

(sb!int:/show0 "entering defsetfs.lisp")

;;; from alieneval.lisp
(in-package "SB!ALIEN")
(defsetf slot %set-slot)
(defsetf deref (alien &rest indices) (value)
  `(%set-deref ,alien ,value ,@indices))
(defsetf %heap-alien %set-heap-alien)

;;; from bignum.lisp
(in-package "SB!BIGNUM")
(defsetf %bignum-ref %bignum-set)

;;; from bit-bash.lisp
(in-package "SB!VM")
(defsetf word-sap-ref %set-word-sap-ref)

;;; from debug-int.lisp
(in-package "SB!DI")
(defsetf stack-ref %set-stack-ref)
(defsetf debug-var-value %set-debug-var-value)
(defsetf debug-var-value %set-debug-var-value)
(defsetf breakpoint-info %set-breakpoint-info)

;;; from defstruct.lisp
(in-package "SB!KERNEL")
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
(in-package "SB!IMPL")

;;; (setf aref/bit/sbit) are implemented using setf-functions,
;;; because they have to work with (setf (apply #'aref array subscripts))
;;; All other setfs can be done using setf-functions too, but I
;;; haven't found technical advantages or disadvantages for either
;;; scheme.
#-sb-xc-host (defsetf car   %rplaca)
#-sb-xc-host (defsetf first %rplaca)
#-sb-xc-host (defsetf cdr   %rplacd)
#-sb-xc-host (defsetf rest  %rplacd)

(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
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

#-sb-xc-host
(macrolet ((def (name &optional alias &aux (string (string name)))
             `(eval-when (:compile-toplevel :load-toplevel :execute)
                (let ((closure
                       (%cxr-setf-expander
                        '(,(symbolicate "C" (subseq string 2)))
                        ',(symbolicate "%RPLAC" (subseq string 1 2)))))
                  (!quietly-defsetf ',name closure nil)
                  ,@(when alias
                      `((!quietly-defsetf ',alias closure nil)))))))
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
#-sb-xc-host
(macrolet ((def (name subform)
             `(eval-when (:compile-toplevel :load-toplevel :execute)
                (!quietly-defsetf ',name (%cxr-setf-expander ',subform '%rplaca)
                                  nil))))
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
#-sb-xc-host
(eval-when (:compile-toplevel :load-toplevel :execute)
  (!quietly-defsetf 'nth (%cxr-setf-expander 'nthcdr '%rplaca) nil))

#-sb-xc-host (defsetf elt %setelt)
#-sb-xc-host (defsetf row-major-aref %set-row-major-aref)
#-sb-xc-host (defsetf svref %svset)
#-sb-xc-host (defsetf char %charset)
#-sb-xc-host (defsetf schar %scharset)
(defsetf %array-dimension %set-array-dimension)
(defsetf %vector-raw-bits %set-vector-raw-bits)
#-sb-xc-host (defsetf symbol-value set)
#-sb-xc-host (defsetf symbol-global-value set-symbol-global-value)
#-sb-xc-host (defsetf symbol-plist %set-symbol-plist)
#-sb-xc-host (defsetf fill-pointer %set-fill-pointer)
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
#-sb-xc-host (defsetf subseq (sequence start &optional (end nil)) (v)
            `(progn (replace ,sequence ,v :start1 ,start :end1 ,end)
                    ,v))

;;; from fdefinition.lisp
(in-package "SB!IMPL")
#-sb-xc-host (defsetf fdefinition %set-fdefinition)

;;; from kernel.lisp
(in-package "SB!KERNEL")
(defsetf code-header-ref code-header-set)

;;; from arch-vm.lisp
(in-package "SB!VM")
(defsetf context-register %set-context-register)
(defsetf context-float-register %set-context-float-register)


(sb!int:/show0 "leaving defsetfs.lisp")
