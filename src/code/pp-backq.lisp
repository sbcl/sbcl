;;;; pretty-printing of backquote expansions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(defun backq-unparse-expr (form splicing)
  (ecase splicing
    ((nil)
     `(backq-comma ,form))
    ((t)
     `((backq-comma-at ,form)))
    (:nconc
     `((backq-comma-dot ,form)))
    ))

(defun backq-unparse (form &optional splicing)
  #!+sb-doc
  "Given a lisp form containing the magic functions BACKQ-LIST, BACKQ-LIST*,
  BACKQ-APPEND, etc. produced by the backquote reader macro, will return a
  corresponding backquote input form. In this form, `,' `,@' and `,.' are
  represented by lists whose cars are BACKQ-COMMA, BACKQ-COMMA-AT, and
  BACKQ-COMMA-DOT respectively, and whose cadrs are the form after the comma.
  SPLICING indicates whether a comma-escape return should be modified for
  splicing with other forms: a value of T or :NCONC meaning that an extra
  level of parentheses should be added."
  (cond
   ((atom form)
    (backq-unparse-expr form splicing))
   ((not (null (cdr (last form))))
    ;; FIXME: Shouldn't this be an ERROR?
    "### illegal dotted backquote form ###")
   (t
    (case (car form)
      (backq-list
       (mapcar #'backq-unparse (cdr form)))
      (backq-list*
       (do ((tail (cdr form) (cdr tail))
	    (accum nil))
	   ((null (cdr tail))
	    (nconc (nreverse accum)
		   (backq-unparse (car tail) t)))
	 (push (backq-unparse (car tail)) accum)))
      (backq-append
       (mapcan #'(lambda (el) (backq-unparse el t))
	       (cdr form)))
      (backq-nconc
       (mapcan #'(lambda (el) (backq-unparse el :nconc))
	       (cdr form)))
      (backq-cons
       (cons (backq-unparse (cadr form) nil)
	     (backq-unparse (caddr form) t)))
      (backq-vector
       (coerce (backq-unparse (cadr form)) 'vector))
      (quote
       (cadr form))
      (t
       (backq-unparse-expr form splicing))))))

(defun pprint-backquote (stream form &rest noise)
  (declare (ignore noise))
  (write-char #\` stream)
  (write (backq-unparse form) :stream stream))

(defun pprint-backq-comma (stream form &rest noise)
  (declare (ignore noise))
  (ecase (car form)
    (backq-comma
     (write-char #\, stream))
    (backq-comma-at
     (princ ",@" stream))
    (backq-comma-dot
     (princ ",." stream)))
  (write (cadr form) :stream stream))

;;; This is called by !PPRINT-COLD-INIT, fairly late, because
;;; SET-PPRINT-DISPATCH doesn't work until the compiler works.
;;;
;;; FIXME: It might be cleaner to just make these be toplevel forms and
;;; enforce the delay by putting this file late in the build sequence.
(defun !backq-pp-cold-init ()
  (set-pprint-dispatch '(cons (eql backq-list)) #'pprint-backquote)
  (set-pprint-dispatch '(cons (eql backq-list*)) #'pprint-backquote)
  (set-pprint-dispatch '(cons (eql backq-append)) #'pprint-backquote)
  (set-pprint-dispatch '(cons (eql backq-nconc)) #'pprint-backquote)
  (set-pprint-dispatch '(cons (eql backq-cons)) #'pprint-backquote)
  (set-pprint-dispatch '(cons (eql backq-vector)) #'pprint-backquote)

  (set-pprint-dispatch '(cons (eql backq-comma)) #'pprint-backq-comma)
  (set-pprint-dispatch '(cons (eql backq-comma-at)) #'pprint-backq-comma)
  (set-pprint-dispatch '(cons (eql backq-comma-dot)) #'pprint-backq-comma))
