;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(defun constant-type-body-p (forms)
  (destructuring-bind (&optional first . rest) forms
    (and first (not rest)
         (or (member first '(t nil))
             (and (consp first) (eq (car first) 'quote))))))

(defun constant-type-expander (expansion)
  (declare (optimize safety))
  (lambda (whole)
    (declare (sb!c::lambda-list ())) ; for introspection of DEFTYPE lambda-list
    (if (cdr whole)
        (error 'sb!kernel::arg-count-error
               :kind 'deftype :name (car whole) :args (cdr whole)
               :lambda-list '() :minimum 0 :maximum 0)
        expansion)))

(defvar !*xc-processed-deftypes* nil)
(def!macro sb!xc:deftype (&whole form name lambda-list &body body)
  #!+sb-doc
  "Define a new type, with syntax like DEFMACRO."
  (unless (symbolp name)
    (bad-type name 'symbol "Type name is not a symbol:~%  ~S"
              form))
  (multiple-value-bind (expander-form doc source-location-form)
      (multiple-value-bind (forms decls doc) (parse-body body)
        ;; FIXME: We could use CONSTANTP here to deal with slightly more
        ;; complex deftypes using CONSTANT-TYPE-EXPANDER, but that XC:CONSTANTP
        ;; is not availble early enough.
        (if (and (not lambda-list) (not decls) (constant-type-body-p forms))
            (progn
              #-sb-xc-host (check-deprecated-type
                            (typecase forms
                              ((cons (cons (eql quote))) (cadar forms))
                              ((cons symbol)             (car forms))))
              (values `(constant-type-expander ,(car forms)) doc
                      '(sb!c:source-location)))
            ;; FIXME: it seems non-ANSI-compliant to pretend every lexenv
            ;; is nil. See also lp#309140.
            (make-macro-lambda `(type-expander ,name)
                               lambda-list body 'deftype name
                               :doc-string-allowed :external
                               :environment :ignore)))
    `(progn
       #+sb-xc-host
       (eval-when (:compile-toplevel)
         ;; This needs to be in the macroexpansion when building the xc,
         ;; but not when running the xc. But it's harmless in the latter.
         (pushnew ',name !*xc-processed-deftypes*))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (%compiler-deftype ',name ,expander-form ,source-location-form
                            ,@(when doc `(,doc)))))))
