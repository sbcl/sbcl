;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(defun constant-type-expander (expansion)
  (declare (optimize safety))
  (lambda (whole)
    (if (cdr whole)
        (sb!kernel::arg-count-error 'deftype (car whole) (cdr whole) nil 0 0)
        expansion)))

(defvar !*xc-processed-deftypes* nil)
(def!macro sb!xc:deftype (&whole form name lambda-list &body body)
  #!+sb-doc
  "Define a new type, with syntax like DEFMACRO."
  (unless (symbolp name)
    (bad-type name 'symbol "Type name is not a symbol:~%  ~S"
              form))
  (multiple-value-bind (expander-form doc lambda-list source-location-form)
      (multiple-value-bind (forms decls doc) (parse-body body)
        ;; FIXME: We could use CONSTANTP here to deal with slightly more
        ;; complex deftypes using CONSTANT-TYPE-EXPANDER, but that XC:CONSTANTP
        ;; is not availble early enough.
        (if (and (not lambda-list) (not decls) (not (cdr forms))
                 (or (member (car forms) '(t nil))
                     (and (consp (car forms)) (eq 'quote (caar forms)))))
            (values `(constant-type-expander ,(car forms)) doc '()
                    '(sb!c:source-location))
            (multiple-value-bind (def arglist doc)
                ;; FIXME: it seems non-ANSI-compliant to pretend every lexenv
                ;; is nil. See also lp#309140.
                (make-macro-lambda `(type-expander ,name)
                                   lambda-list body 'deftype name
                                   :doc-string-allowed :external
                                   :environment :ignore)
              (values def doc arglist))))
    `(progn
       #+sb-xc-host
       (eval-when (:compile-toplevel)
         ;; This needs to be in the macroexpansion when building the xc,
         ;; but not when running the xc. But it's harmless in the latter.
         (pushnew ',name !*xc-processed-deftypes*))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (%compiler-deftype ',name
                            ',lambda-list
                            ,expander-form
                            ,source-location-form
                            ,@(and doc
                                   `(,doc)))))))
