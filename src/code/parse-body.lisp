;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;; Given a sequence of declarations (and possibly a documentation
;;; string) followed by other forms (as occurs in the bodies of DEFUN,
;;; DEFMACRO, etc.) return (VALUES FORMS DECLS DOC), where DECLS holds
;;; declarations, DOC holds a doc string (or NIL if none), and FORMS
;;; holds the other forms.
;;;
;;; If DOC-STRING-ALLOWED is NIL, then no forms will be treated as
;;; documentation strings.
(defun parse-body (body doc-string-allowed &optional silent)
  (flet ((doc-string-p (x remaining-forms doc)
           (and (stringp x) doc-string-allowed
                  ;; ANSI 3.4.11 explicitly requires that a doc string
                  ;; be followed by another form (either an ordinary form
                  ;; or a declaration). Hence:
                remaining-forms
                (if doc
                    ;; .. and says that the consequences of multiple
                    ;; doc strings are unspecified.
                    ;; That's probably not something the programmer intends.
                    (sb-c:compiler-warn "Duplicate doc string ~S" x)
                    t)))
         (declaration-p (x)
           (when (listp x)
             (let ((name (car x)))
               (cond ((eq name 'declare) t)
                     (t
                      (when (and (eq name 'declaim) (not silent))
                        ;; technically legal, but rather unlikely to
                        ;; be what the user meant to do...
                        (style-warn
                         "DECLAIM where DECLARE was probably intended"))
                      nil))))))
    (let ((forms body) (decls (list nil)) (doc nil))
      (declare (truly-dynamic-extent decls))
      (let ((decls decls))
        (loop (when (endp forms) (return))
              (let ((form (first forms)))
                (cond ((doc-string-p form (rest forms) doc)
                       (setq doc form))
                      ((declaration-p form)
                       (setq decls (setf (cdr decls) (list form))))
                      (t
                       (return))))
              (setq forms (rest forms))))
      (values forms (cdr decls) doc))))
