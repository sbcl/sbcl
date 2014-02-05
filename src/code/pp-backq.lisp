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

(defstruct (backq-comma (:constructor make-backq-comma (form))
                        (:copier nil) (:predicate nil))
  form)
(defstruct (backq-comma-at (:include backq-comma)
                           (:constructor make-backq-comma-at (form))
                           (:copier nil) (:predicate nil)))
(defstruct (backq-comma-dot (:include backq-comma)
                            (:constructor make-backq-comma-dot (form))
                            (:copier nil) (:predicate nil)))

(defun backq-unparse-expr (form splicing)
  (ecase splicing
    ((nil) (make-backq-comma form))
    ((t) `(,(make-backq-comma-at form)))
    (:nconc `(,(make-backq-comma-dot form)))))

(defun backq-unparse (form &optional splicing)
  #!+sb-doc
  "Given a lisp form containing the magic functions BACKQ-LIST, BACKQ-LIST*,
  BACKQ-APPEND, etc. produced by the backquote reader macro, will return a
  corresponding backquote input form. In this form, `,' `,@' and `,.' are
  represented by structures of type BACKQ-COMMA, BACKQ-COMMA-AT, and
  BACKQ-COMMA-DOT respectively.
  SPLICING indicates whether a comma-escape return should be modified for
  splicing with other forms: a value of T or :NCONC meaning that an extra
  level of parentheses should be added."
  (cond
   ((atom form)
    (backq-unparse-expr form splicing))
   ((not (null (cdr (last form))))
    ;; FIXME: this probably throws a recursive error
    (bug "found illegal dotted backquote form: ~S" form))
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
       (apply #'append
              (mapcar (lambda (el) (backq-unparse el t))
                      (cdr form))))
      (backq-nconc
       (apply #'append
              (mapcar (lambda (el) (backq-unparse el :nconc))
                      (cdr form))))
      (backq-cons
       (cons (backq-unparse (cadr form) nil)
             (backq-unparse (caddr form) t)))
      (backq-vector
       ;; The special-case of empty vector isn't technically necessary,
       ;; but avoids the valid though ugly result "`#(,@NIL)"
       (acond ((cadr form) (coerce (backq-unparse it t) 'vector))
              (t #())))
      (quote
       ;; FIXME: This naively assumes that the form is exactly (QUOTE x).
       ;; Therefore (QUOTE . x) and (QUOTE x y z*) will lose.
       (let ((thing (cadr form)))
         (cond ((atom thing)
                (if (typep thing 'backq-comma)
                    (backq-unparse-expr form splicing)
                    thing))
               ((member (car thing) *backq-tokens*)
                (backq-unparse-expr form splicing))
               (t
                (cons (backq-unparse `(quote ,(car thing)))
                      (backq-unparse `(quote ,(cdr thing))))))))
      (t
       (backq-unparse-expr form splicing))))))

(defun pprint-backquote (stream form &rest noise)
  (declare (ignore noise))
  (write-char #\` stream)
  (write (backq-unparse form) :stream stream))

(defun pprint-backq-comma (stream thing &rest noise)
  (declare (ignore noise) (backq-comma thing))
  (etypecase thing
    (backq-comma-at
     (write-string ",@" stream))
    (backq-comma-dot
     (write-string ",." stream))
    (backq-comma
     (write-char #\, stream)
     (setf (sb!pretty::pretty-stream-char-out-oneshot-hook stream)
           (lambda (stream char)
             ;; Ensure a space is written before any output that would
             ;; erroneously be interpreted as a splicing frob on readback.
             (when (or (char= char #\.) (char= char #\@))
               (write-char #\Space stream))))))
  (write (backq-comma-form thing) :stream stream))

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
  (set-pprint-dispatch 'backq-comma #'pprint-backq-comma))

