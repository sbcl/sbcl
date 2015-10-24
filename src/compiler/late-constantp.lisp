;;;; implementation of CONSTANTP, needs both INFO and IR1-ATTRIBUTES

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; These functions are split out from 'constantp' because of the
;;; dependence on the IR1-ATTRIBUTEP macro, and the ERROR condition class.
;;; As long as we don't encounter a constantp inquiry involving a THE form
;;; or a foldable function call during cross-compilation before
;;; compiling this file, we're OK.

(!begin-collecting-cold-init-forms)

(!defconstantp the (type form)
   ;; We can't call TYPEP because the form might be (THE (FUNCTION (t) t) #<fn>)
   ;; which is valid for declaration but not for discrimination.
   ;; Instead use %%TYPEP in non-strict mode. FIXME:
   ;; (1) CAREFUL-SPECIFIER-TYPE should never fail. See lp#1395910.
   ;; (2) CONTAINS-UNKNOWN-TYPE-P should grovel into ARRAY-TYPE-ELEMENT-TYPE
   ;; so that (C-U-T-P (SPECIFIER-TYPE '(OR (VECTOR BAD) FUNCTION))) => T
   ;; and then we can parse, check for unknowns, and get rid of HANDLER-CASE.
   :test (and (constantp* form)
              (handler-case
                  ;; in case the type-spec is malformed!
                  (let ((parsed (careful-specifier-type type)))
                    ;; xc can't rely on a "non-strict" mode of TYPEP.
                    (and parsed
                         #+sb-xc-host
                         (typep (constant-form-value* form)
                                (let ((*unparse-fun-type-simplify* t))
                                  (declare (special *unparse-fun-type-simplify*))
                                  (type-specifier parsed)))
                         #-sb-xc-host
                         (%%typep (constant-form-value* form) parsed nil)))
                (error () nil)))
   :eval (constant-form-value* form))

(!defun-from-collected-cold-init-forms !constantp2-cold-init)

;;; FIXME: It would be nice to deal with inline functions
;;; too.
(defun constant-function-call-p (form environment envp)
  (let ((name (car form)))
    (if (and (legal-fun-name-p name)
             (eq :function (info :function :kind name))
             (let ((info (info :function :info name)))
               (and info (ir1-attributep (fun-info-attributes info)
                                         foldable)))
             (and (every (lambda (arg)
                           (%constantp arg environment envp))
                         (cdr form))))
        ;; Even though the function may be marked as foldable
        ;; the call may still signal an error -- eg: (CAR 1).
        (handler-case
            (values t (constant-function-call-value form environment envp))
          (error ()
            (values nil nil)))
        (values nil nil))))
