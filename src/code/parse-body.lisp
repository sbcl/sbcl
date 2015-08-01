;;;; functions used to parse function/macro bodies
;;;;
;;;; FIXME: In an early attempt to bootstrap SBCL, this file
;;;; was loaded before fundamental things like DEFUN and AND and OR
;;;; were defined, and it still bears scars from the attempt to
;;;; make that work. (TAGBODY, forsooth..) It should be cleaned up.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(/show0 "entering parse-body.lisp")

;;; Given a sequence of declarations (and possibly a documentation
;;; string) followed by other forms (as occurs in the bodies of DEFUN,
;;; DEFMACRO, etc.) return (VALUES FORMS DECLS DOC), where DECLS holds
;;; declarations, DOC holds a doc string (or NIL if none), and FORMS
;;; holds the other forms.
;;;
;;; If DOC-STRING-ALLOWED is NIL, then no forms will be treated as
;;; documentation strings.
(defun parse-body (body &key (doc-string-allowed t) (toplevel nil))
  (let ((reversed-decls nil)
        (forms body)
        (doc nil))
    (flet ((doc-string-p (x remaining-forms)
             (and (stringp x) doc-string-allowed
                  ;; ANSI 3.4.11 explicitly requires that a doc string
                  ;; be followed by another form (either an ordinary form
                  ;; or a declaration). Hence:
                  remaining-forms
                  (if doc
                      ;; .. and says that the consequences of multiple
                      ;; doc strings are unspecified.
                      ;; That's probably not something the programmer intends.
                      ;; We raise an error so that this won't pass unnoticed.
                      (error "duplicate doc string ~S" x)
                      t)))
           (declaration-p (x)
             (if (consp x)
                 (let ((name (car x)))
                   (case name
                     ((declare) t)
                     ((declaim)
                      (unless toplevel
                        ;; technically legal, but rather unlikely to
                        ;; be what the user meant to do...
                        (style-warn
                         "DECLAIM where DECLARE was probably intended")
                        nil))
                     (t nil))))))
      (tagbody
        :again
        (if forms
            (let ((form1 (first forms)))
              ;; Note: The (IF (IF ..) ..) stuff is because we don't
              ;; have the macro AND yet.:-|
              (if (doc-string-p form1 (rest forms))
                  (setq doc form1)
                  (if (declaration-p form1)
                      (setq reversed-decls
                            (cons form1 reversed-decls))
                      (go :done)))
              (setq forms (rest forms))
              (go :again)))
        :done)
      (values forms
              (nreverse reversed-decls)
              doc))))

(/show0 "leaving parse-body.lisp")
