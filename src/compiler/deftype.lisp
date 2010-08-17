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

(defun %deftype (name)
  (setf (classoid-cell-pcl-class (find-classoid-cell name :create t)) nil))

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
        (if (and (not lambda-list) (not decls) (not (cdr forms))
                 (or (member (car forms) '(t nil))
                     (and (consp (car forms)) (eq 'quote (caar forms)))))
            (values `(constant-type-expander ,(car forms)) doc '(sb!c:source-location))
            (with-unique-names (whole)
              (multiple-value-bind (macro-body local-decs doc)
                  (parse-defmacro lambda-list whole body name 'deftype :default-default ''*)
                (values `(lambda (,whole)
                           ,@local-decs
                           ,macro-body)
                        doc
                        nil)))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (%compiler-deftype ',name
                            ',lambda-list
                            ,expander-form
                            ,doc
                            ,source-location-form))
       (eval-when (:load-toplevel :execute)
         (%deftype ',name))
       ',name)))
