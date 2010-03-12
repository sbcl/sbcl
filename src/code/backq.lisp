;;;; the backquote reader macro

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(/show0 "entering backq.lisp")

;;; The flags passed back by BACKQUOTIFY can be interpreted as follows:
;;;
;;;   |`,|: [a] => a
;;;    NIL: [a] => a            ;the NIL flag is used only when a is NIL
;;;      T: [a] => a            ;the T flag is used when a is self-evaluating
;;;  QUOTE: [a] => (QUOTE a)
;;; APPEND: [a] => (APPEND . a)
;;;  NCONC: [a] => (NCONC . a)
;;;   LIST: [a] => (LIST . a)
;;;  LIST*: [a] => (LIST* . a)
;;;
;;; The flags are combined according to the following set of rules:
;;;  ([a] means that a should be converted according to the previous table)
;;;
;;;   \ car  ||    otherwise    |    QUOTE or     |     |`,@|      |     |`,.|
;;;cdr \     ||                 |    T or NIL     |                |
;;;================================================================================
;;;  |`,|    || LIST* ([a] [d]) | LIST* ([a] [d]) | APPEND (a [d]) | NCONC  (a [d])
;;;  NIL     || LIST    ([a])   | QUOTE    (a)    | <hair>    a    | <hair>    a
;;;QUOTE or T|| LIST* ([a] [d]) | QUOTE  (a . d)  | APPEND (a [d]) | NCONC (a [d])
;;; APPEND   || LIST* ([a] [d]) | LIST* ([a] [d]) | APPEND (a . d) | NCONC (a [d])
;;; NCONC    || LIST* ([a] [d]) | LIST* ([a] [d]) | APPEND (a [d]) | NCONC (a . d)
;;;  LIST    || LIST  ([a] . d) | LIST  ([a] . d) | APPEND (a [d]) | NCONC (a [d])
;;;  LIST*   || LIST* ([a] . d) | LIST* ([a] . d) | APPEND (a [d]) | NCONC  (a [d])
;;;
;;;<hair> involves starting over again pretending you had read ".,a)" instead
;;; of ",@a)"

(defvar *backquote-count* 0 #!+sb-doc "how deep we are into backquotes")
(defvar *bq-comma-flag* '(|,|))
(defvar *bq-at-flag* '(|,@|))
(defvar *bq-dot-flag* '(|,.|))
(defvar *bq-vector-flag* '(|bqv|))
(defvar *bq-error* "Comma not inside a backquote.")

(/show0 "backq.lisp 50")

;;; the actual character macro
(defun backquote-macro (stream ignore)
  (declare (ignore ignore))
  (let ((*backquote-count* (1+ *backquote-count*)))
    (multiple-value-bind (flag thing)
        (backquotify stream (read stream t nil t))
      (when (eq flag *bq-at-flag*)
        (simple-reader-error stream ",@ after backquote in ~S" thing))
      (when (eq flag *bq-dot-flag*)
        (simple-reader-error stream ",. after backquote in ~S" thing))
      (backquotify-1 flag thing))))

(/show0 "backq.lisp 64")

(defun comma-macro (stream ignore)
  (declare (ignore ignore))
  (unless (> *backquote-count* 0)
    (when *read-suppress*
      (return-from comma-macro nil))
    (simple-reader-error stream *bq-error*))
  (let ((c (read-char stream))
        (*backquote-count* (1- *backquote-count*)))
    (cond ((char= c #\@)
           (cons *bq-at-flag* (read stream t nil t)))
          ((char= c #\.)
           (cons *bq-dot-flag* (read stream t nil t)))
          (t (unread-char c stream)
             (cons *bq-comma-flag* (read stream t nil t))))))

(/show0 "backq.lisp 83")

;;;
(defun expandable-backq-expression-p (object)
  (and (consp object)
       (let ((flag (car object)))
         (or (eq flag *bq-at-flag*)
             (eq flag *bq-dot-flag*)))))

;;; This does the expansion from table 2.
(defun backquotify (stream code)
  (cond ((atom code)
         (cond ((null code) (values nil nil))
               ((or (consp code)
                    (symbolp code))
                ;; Keywords are self-evaluating. Install after packages.
                (values 'quote code))
               (t (values t code))))
        ((or (eq (car code) *bq-at-flag*)
             (eq (car code) *bq-dot-flag*))
         (values (car code) (cdr code)))
        ((eq (car code) *bq-comma-flag*)
         (comma (cdr code)))
        ((eq (car code) *bq-vector-flag*)
         (multiple-value-bind (dflag d) (backquotify stream (cdr code))
           (values 'vector (backquotify-1 dflag d))))
        (t (multiple-value-bind (aflag a) (backquotify stream (car code))
             (multiple-value-bind (dflag d) (backquotify stream (cdr code))
               (when (eq dflag *bq-at-flag*)
                 ;; Get the errors later.
                 (simple-reader-error stream ",@ after dot in ~S" code))
               (when (eq dflag *bq-dot-flag*)
                 (simple-reader-error stream ",. after dot in ~S" code))
               (cond
                ((eq aflag *bq-at-flag*)
                 (if (null dflag)
                     (if (expandable-backq-expression-p a)
                         (values 'append (list a))
                         (comma a))
                     (values 'append
                             (cond ((eq dflag 'append)
                                    (cons a d ))
                                   (t (list a (backquotify-1 dflag d)))))))
                ((eq aflag *bq-dot-flag*)
                 (if (null dflag)
                     (if (expandable-backq-expression-p a)
                         (values 'nconc (list a))
                         (comma a))
                     (values 'nconc
                             (cond ((eq dflag 'nconc)
                                    (cons a d))
                                   (t (list a (backquotify-1 dflag d)))))))
                ((null dflag)
                 (if (member aflag '(quote t nil))
                     (values 'quote (list a))
                     (values 'list (list (backquotify-1 aflag a)))))
                ((member dflag '(quote t))
                 (if (member aflag '(quote t nil))
                     (values 'quote (cons a d ))
                     (values 'list* (list (backquotify-1 aflag a)
                                          (backquotify-1 dflag d)))))
                (t (setq a (backquotify-1 aflag a))
                   (if (member dflag '(list list*))
                       (values dflag (cons a d))
                       (values 'list*
                               (list a (backquotify-1 dflag d)))))))))))

(/show0 "backq.lisp 139")

;;; This handles the <hair> cases.
(defun comma (code)
  (cond ((atom code)
         (cond ((null code)
                (values nil nil))
               ((or (numberp code) (eq code t))
                (values t code))
               (t (values *bq-comma-flag* code))))
        ((and (eq (car code) 'quote)
              (not (expandable-backq-expression-p (cadr code))))
         (values (car code) (cadr code)))
        ((member (car code) '(append list list* nconc))
         (values (car code) (cdr code)))
        ((eq (car code) 'cons)
         (values 'list* (cdr code)))
        (t (values *bq-comma-flag* code))))

(/show0 "backq.lisp 157")

;;; This handles table 1.
(defun backquotify-1 (flag thing)
  (cond ((or (eq flag *bq-comma-flag*)
             (member flag '(t nil)))
         thing)
        ((eq flag 'quote)
         (list  'quote thing))
        ((eq flag 'list*)
         (cond ((and (null (cddr thing))
                     (not (expandable-backq-expression-p (car thing)))
                     (not (expandable-backq-expression-p (cadr thing))))
                (cons 'backq-cons thing))
               ((expandable-backq-expression-p (car (last thing)))
                (list 'backq-append
                      (cons 'backq-list (butlast thing))
                      ;; Can it be optimized further? -- APD, 2001-12-21
                      (car (last thing))))
               (t
                (cons 'backq-list* thing))))
        ((eq flag 'vector)
         (list 'backq-vector thing))
        (t (cons (ecase flag
                   ((list) 'backq-list)
                   ((append) 'backq-append)
                   ((nconc) 'backq-nconc))
                 thing))))

;;;; magic BACKQ- versions of builtin functions

(/show0 "backq.lisp 184")

;;; Define synonyms for the lisp functions we use, so that by using
;;; them, the backquoted material will be recognizable to the
;;; pretty-printer.
(macrolet ((def (b-name name)
               ;; FIXME: This function should be INLINE so that the lists
               ;; aren't consed twice, but I ran into an optimizer bug the
               ;; first time I tried to make this work for BACKQ-LIST. See
               ;; whether there's still an optimizer bug, and fix it if so, and
               ;; then make these INLINE.
               `(defun ,b-name (&rest rest)
                  (declare (truly-dynamic-extent rest))
                  (apply #',name rest))))
  (def backq-list list)
  (def backq-list* list*)
  (def backq-append append)
  (def backq-nconc nconc)
  (def backq-cons cons))

(/show0 "backq.lisp 204")

(defun backq-vector (list)
  (declare (list list))
  (coerce list 'simple-vector))

;;;; initialization

(/show0 "backq.lisp 212")

;;; Install BACKQ stuff in the current *READTABLE*.
;;;
;;; In the target Lisp, we have to wait to do this until the readtable
;;; has been created. In the cross-compilation host Lisp, we can do
;;; this right away. (You may ask: In the cross-compilation host,
;;; which already has its own implementation of the backquote
;;; readmacro, why do we do this at all? Because the cross-compilation
;;; host might -- as SBCL itself does -- express the backquote
;;; expansion in terms of internal, nonportable functions. By
;;; redefining backquote in terms of functions which are guaranteed to
;;; exist on the target Lisp, we ensure that backquote expansions in
;;; code-generating code work properly.)
(defun !backq-cold-init ()
  (set-macro-character #\` #'backquote-macro)
  (set-macro-character #\, #'comma-macro))
#+sb-xc-host (!backq-cold-init)

;;; The pretty-printer needs to know about our special tokens
(defvar *backq-tokens*
  '(backq-comma backq-comma-at backq-comma-dot backq-list
    backq-list* backq-append backq-nconc backq-cons backq-vector))

;;; Since our backquote is installed on the host lisp, and since
;;; developers make mistakes with backquotes and commas too, let's
;;; ensure that we can report errors rather than get an undefined
;;; function condition on SIMPLE-READER-ERROR.
#+sb-xc-host ; proper definition happens for the target
(defun simple-reader-error (stream format-string &rest format-args)
  (bug "READER-ERROR on stream ~S: ~?" stream format-string format-args))

(/show0 "done with backq.lisp")
