;;;; cold-boot-only readmacro syntax

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-COLD")

;;;; definition of #+ and #- as a mechanism analogous to #+/#-, but
;;;; for SB-XC:*FEATURES* instead of CL:*FEATURES*. (This is handy
;;;; when cross-compiling, so that we can make a distinction between
;;;; features of the host Common Lisp and features of the target
;;;; SBCL.)

;;; the feature list for the target system
(unless (find-package "SB-XC")
  (make-package "SB-XC" :use nil :nicknames nil))
(export (intern "*FEATURES*" "SB-XC") "SB-XC")
(declaim (type list sb-xc:*features*))
(defvar sb-xc:*features*)

(defun target-platform-keyword (&aux (features sb-xc:*features*))
  (let ((arch (intersection '(:arm :arm64 :mips :ppc :ppc64 :riscv :sparc :x86 :x86-64)
                            features)))
    (cond ((not arch) (error "No architecture selected"))
          ((> (length arch) 1) (error "More than one architecture selected")))
    (car arch)))

(defun compatible-vector-raw-bits () ; T if the host and target match on word size and endianness
  (flet ((endianness (features)
           (let ((result (intersection '(:little-endian :big-endian) features)))
             ;; some lisp implementation may not have little-endian / big-endian
             ;; features which shouldn't trigger that assert
             (assert (or (not result) (and result (not (cdr result)))))
             (car result)))
         (wordsize (features)
           (if (member :64-bit features) 64 32)))
    (and (eq (endianness sb-xc:*features*) (endianness cl:*features*))
         (= (wordsize sb-xc:*features*) (wordsize cl:*features*)))))

;;; Not necessarily the logical place to define BACKEND-ASM-PACKAGE-NAME,
;;; but a convenient one.
(defun backend-assembler-target-name ()
  (let ((keyword (target-platform-keyword)))
    (case keyword
      (:ppc :ppc64)
      (t keyword))))
(defun backend-asm-package-name ()
  (concatenate 'string "SB-" (string (backend-assembler-target-name)) "-ASM"))

;;; Like the real FEATUREP but using SB-XC:*FEATURES* instead of CL:*FEATURES*
(defun target-featurep (feature)
  (etypecase feature
    (symbol
     (if (string= feature "SBCL")
         (error "Testing SBCL as a target feature is obviously bogus")
         (member feature sb-xc:*features* :test #'eq)))
    (cons (ecase (first feature)
            (:or  (some  #'target-featurep (rest feature)))
            (:and (every #'target-featurep (rest feature)))
            (:not (destructuring-bind (subexpr) (cdr feature)
                    (not (target-featurep subexpr))))))))
(compile 'target-featurep)

(defun read-targ-feature-expr (stream sub-character infix-parameter)
  (when infix-parameter
    (error "illegal read syntax: #~D!" infix-parameter))
  (if (char= (if (let* ((*package* (find-package "KEYWORD"))
                        (*read-suppress* nil)
                        (feature (read stream t nil t)))
                   (target-featurep feature))
                 #\+ #\-)
             sub-character)
      (read stream t nil t)
      ;; Read (and discard) a form from input.
      (let ((*read-suppress* t))
        (read stream t nil t)
        (values))))
(compile 'read-targ-feature-expr)

(export '*xc-readtable*)
(defvar *xc-readtable* (copy-readtable))
(set-dispatch-macro-character #\# #\+ #'read-targ-feature-expr *xc-readtable*)
(set-dispatch-macro-character #\# #\- #'read-targ-feature-expr *xc-readtable*)

(defvar *consing-dot*)

(defun read-potential-real-number (stream char)
  (let ((buffer (load-time-value
                 ;; Assume that we don't have any potential number
                 ;; tokens that are too long.
                 (make-array 100 :element-type 'character
                                 :fill-pointer 0))))
    (setf (aref buffer 0) char)
    (setf (fill-pointer buffer) 1)
    (loop
      (let ((char (peek-char nil stream nil nil t))
            (char-skipping-whitespace (peek-char t stream nil nil t)))
        ;; Check for EOF, delimiting whitespace, or terminating macro
        ;; character.
        (when (or (null char)
                  (not (eql char char-skipping-whitespace))
                  (multiple-value-bind (function non-terminating-p)
                      (get-macro-character char)
                    (and function (not non-terminating-p))))
          (return))
        (vector-push (read-char stream t nil t) buffer)))
    (when (and (eql char #\.) (= (length buffer) 1))
      (unless (boundp '*consing-dot*)
        (if *read-suppress*
            (return-from read-potential-real-number nil)
            (error ". not inside list.")))
      (return-from read-potential-real-number *consing-dot*))
    (when *read-suppress*
      (return-from read-potential-real-number nil))
    ;; Check using the host reader whether we would get a float
    ;; literal. If so, read in the float in the target format.
    (let ((*readtable* (load-time-value (copy-readtable nil))))
      (multiple-value-bind (object position) (read-from-string buffer)
        ;; Assert that we tokenized the same number of characters as
        ;; the reader did with the standard syntax.
        (assert (= (length buffer) position))
        (if (cl:floatp object)
            (funcall 'read-target-float-from-string buffer)
            object)))))

(compile 'read-potential-real-number)

;;; Treat every potential initial character for a base-10 real number
;;; as a reader macro.
(dolist (char (coerce ".-+0123456789" 'list))
  (set-macro-character char #'read-potential-real-number t *xc-readtable*))

(defun read-maybe-nothing (stream)
  (let* ((char (read-char stream t nil t)) ; not whitespace
         (function (get-macro-character char)))
    (cond (function
           (multiple-value-call (lambda (&rest args)
                                  (if (null args)
                                      (values nil t)
                                      (values (first args) nil)))
             (funcall function stream char)))
          (t
           (unread-char char stream)
           (read stream t nil t)))))

(compile 'read-maybe-nothing)

(defun read-after-dot (stream)
  (loop
    (when (eql (peek-char t stream t nil t) #\))
      (if *read-suppress*
          (return-from read-after-dot nil)
          (error "Nothing appears after . in list.")))
    (multiple-value-bind (object skipped)
        (read-maybe-nothing stream)
      (unless skipped
        (return
          (loop
            (cond ((eql (peek-char t stream t nil t) #\))
                   (return object))
                  ((and (not (nth-value 1 (read-maybe-nothing stream)))
                        (not *read-suppress*))
                   (error "More than one object follows . in list.")))))))))

(compile 'read-after-dot)

(defun read-list (stream ignore)
  (declare (ignore ignore))
  (let* ((read-suppress *read-suppress*)
         (list (list nil))
         (tail list)
         (*consing-dot* list))
    (declare (dynamic-extent list))
    (loop
      (when (eq (peek-char t stream t nil t) #\))
        (read-char stream)
        (return (cdr list)))
      (multiple-value-bind (object skipped)
          (read-maybe-nothing stream)
        (cond ((eq object *consing-dot*)
               (when (eq list tail)
                 (unless read-suppress
                   (error "Nothing appears before . in list.")))
               (rplacd tail (read-after-dot stream)))
              ((and (not skipped) (not read-suppress))
               (setq tail
                     (cdr (rplacd tail (list object))))))))))

(compile 'read-list)

;;; We need to install our own left parenthesis reader macro to make
;;; it communicate with the dot reader macro for reading real numbers,
;;; since #\. can be used both as an initial float character as well
;;; as a consing dot in the standard syntax. Although sbcl (and cmu
;;; cl) themselves as a host lisps do not need this for consing dot to
;;; work, other implementations do, and it's ambiguous whether this is
;;; strictly necessary.
(set-macro-character #\( #'read-list nil *xc-readtable*)

;;; The reader will be defined during compilation. CLISP does not permit assignment
;;; of a symbol that currently has no functional definition, so wrap it in lambda.
(set-dispatch-macro-character #\# #\c
  (lambda (stream sub-char numarg)
    (funcall 'read-target-complex stream sub-char numarg))
  *xc-readtable*)

;;; ECL needs a bit of help:
;;; https://gitlab.com/embeddable-common-lisp/ecl/-/issues/742
#+ecl
(macrolet ((frob (char base)
             `(set-dispatch-macro-character #\# ,char
                (lambda (stream sub-char numarg)
                  (declare (ignore sub-char))
                  (declare (ignorable numarg))
                  (let ((*read-base* ,base))
                    (read stream t nil t)))
                *xc-readtable*)))
  (frob #\r numarg)
  (frob #\x 16)
  (frob #\o 8)
  (frob #\b 2))

;;;; string checker, for catching non-portability early

(defun make-quote-reader (standard-quote-reader)
  (lambda (stream char)
    (let ((result (funcall standard-quote-reader stream char)))
      (unless (every (lambda (x) (typep x 'standard-char)) result)
        (warn "Found non-STANDARD-CHAR in ~S" result))
      result)))
(compile 'make-quote-reader)
(set-macro-character #\" (make-quote-reader (get-macro-character #\" nil))
                     nil *xc-readtable*)
