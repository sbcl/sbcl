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
;;; The reader will be defined during compilation. CLISP does not permit assignment
;;; of a symbol that currently has no functional definition, so wrap it in lambda.
(set-macro-character #\$ (lambda (stream char)
                           (funcall 'read-target-float stream char))
                     t ; non-terminating so that symbols may contain a dollar sign
                     *xc-readtable*)

;;;; string checker, for catching non-portability early

;;; A note about CLISP compatibility:
;;; CLISP uses *READTABLE* when loading '.fas' files, and so we shouldn't put
;;; too much of our junk in the readtable. I'm not sure of the full extent
;;; to which it uses our macros, but it definitely was using our #\" reader.
;;; As such, it would signal warnings about strings that it wrote by its own
;;; choice, where we specifically avoided using non-standard char literals.
;;; This would happen when building + loading the cross-compiler, and CLISP
;;; compiled a format call such as this one from 'src/compiler/codegen':
;;;   (FORMAT *COMPILER-TRACE-OUTPUT* "~|~%assembly code for ~S~2%" ...))
;;; which placed into its '.fas' a quoted string containing a byte for the
;;; the literal #\Page character (and literal #\Newline, which is fine).
;;; We should not print a warning for that. We should, however, warn
;;; if we see those characters in strings as read directly from source.
;;;
;;; In case there is doubt as to the veracity of this observation, a simple
;;; experiment proves that the warnings were not exactly our fault:
;;; Given file "foo.lisp" containing (DEFUN F (S) (FORMAT S "x~|y"))
;;; Then:
;;; * (set-macro-character #\"
;;;    (let ((f (get-macro-character #\")))
;;;     (lambda (strm ch &aux (string (funcall f strm ch)))
;;;       (format t "Read ~S from ~S~%" string strm)
;;;       string)))
;;; * (load "foo.fas") shows:
;;;   ;; Loading file foo.fas ...
;;;   Read "x^Ly" from #<INPUT BUFFERED FILE-STREAM CHARACTER #P"/tmp/foo.fas" @11>
;;;
(defun make-quote-reader (standard-quote-reader)
  (lambda (stream char)
    (let ((result (funcall standard-quote-reader stream char)))
      (unless (every (lambda (x) (typep x 'standard-char)) result)
        (warn "Found non-STANDARD-CHAR in ~S" result))
      result)))
(compile 'make-quote-reader)
(set-macro-character #\" (make-quote-reader (get-macro-character #\" nil))
                     nil *xc-readtable*)
