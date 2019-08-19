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

(defun target-platform-keyword (&optional (features sb-xc:*features*))
  (let ((arch (intersection '(:alpha :arm :arm64 :hppa :mips :ppc :ppc64 :riscv :sparc :x86 :x86-64)
                            features)))
    (cond ((not arch) (error "No architecture selected"))
          ((> (length arch) 1) (error "More than one architecture selected")))
    (car arch)))

;;; Not necessarily the logical place to define BACKEND-ASM-PACKAGE-NAME,
;;; but a convenient one, because sb-xc:*features* needs to have been
;;; DEFVARed, and because 'chill' loads this and only this file.
(defun backend-assembler-target-name ()
  (let ((keyword (target-platform-keyword)))
    (case keyword
      (:ppc :ppc64)
      (t keyword))))
(defun backend-asm-package-name ()
  (concatenate 'string "SB-" (string (backend-assembler-target-name)) "-ASM"))

(defun any-vop-named-p (vop-name)
  (let ((ht (symbol-value (find-symbol "*BACKEND-PARSED-VOPS*" "SB-C"))))
    (not (null (gethash vop-name ht)))))

(defun any-vop-translates-p (fun-name)
  (let ((f (intern "INFO" "SB-INT")))
    (when (fboundp f)
      (let ((info (funcall f :function :info fun-name)))
        (if info
            (let ((f (intern "FUN-INFO-TEMPLATES" "SB-C")))
              (and (fboundp f) (not (null (funcall f info))))))))))

(defvar *feature-eval-results-file* "output/feature-tests.lisp-expr")
(defvar *feature-evaluation-results*)

(defun recording-feature-eval (expression value)
  ;; This safety check does not work for parallel build, but that produces
  ;; different code anyway due to missing derived types in any file that would
  ;; have been compiled in the serial order but was interpreted instead.
  (when (boundp '*feature-evaluation-results*)
    ; (format t "~&FEATURE EXPR: ~S -> ~S~%" expression value)
    (push (cons expression value) *feature-evaluation-results*))
  value)

(defun write-feature-eval-results ()
  (with-open-file (f *feature-eval-results-file*
                     :direction :output
                     :if-exists :supersede :if-does-not-exist :create)
    (let ((*print-readably* t))
      (format f "(~{~S~^~% ~})~%" *feature-evaluation-results*))))

(defun sanity-check-feature-evaluation ()
  (flet ((check (phase list)
           (dolist (x list)
             (let ((answer
                     (ecase (caar x)
                      (:vop-named (any-vop-named-p (cadar x)))
                      (:vop-translates (any-vop-translates-p (cadar x))))))
               (unless (eq answer (cdr x))
                 (error "make-host-~D DEFINE-VOP ordering bug:~@
 ~S should be ~S, was ~S at xc time" phase x answer (cdr x)))))))
    (check 1 (with-open-file (f *feature-eval-results-file*) (read f)))
    (check 2 *feature-evaluation-results*)))

;;; We should never call this with a selector of :HOST any more,
;;; but I'm keeping it in case of emergency.
(defun feature-in-list-p (feature selector
                          &aux (list (ecase selector
                                       (:host cl:*features*)
                                       (:target sb-xc:*features*))))
  (etypecase feature
    (symbol
     (if (and (string= feature "SBCL") (eq selector :target))
         (error "Testing SBCL as a target feature is obviously bogus")
         (member feature list :test #'eq)))
    (cons (flet ((subfeature-in-list-p (subfeature)
                   (feature-in-list-p subfeature selector)))
            (ecase (first feature)
              (:or  (some  #'subfeature-in-list-p (rest feature)))
              (:and (every #'subfeature-in-list-p (rest feature)))
              (:not (destructuring-bind (subexpr) (cdr feature)
                      (not (subfeature-in-list-p subexpr))))
              ((:vop-named :vop-translates)
               (when (eq selector :host)
                 (error "Invalid host feature test: ~S" feature))
               (destructuring-bind (subexpr) (cdr feature)
                 (case (first feature)
                   (:vop-named
                    (recording-feature-eval feature
                                      (any-vop-named-p subexpr)))
                   (:vop-translates
                    (recording-feature-eval
                     feature (any-vop-translates-p subexpr)))))))))))
(compile 'feature-in-list-p)

(defun read-targ-feature-expr (stream sub-character infix-parameter)
  (when infix-parameter
    (error "illegal read syntax: #~D!" infix-parameter))
  (if (char= (if (let* ((*package* (find-package "KEYWORD"))
                        (*read-suppress* nil)
                        (feature (read stream t nil t)))
                   (feature-in-list-p feature :target))
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

;;;; variables like SB-XC:*FEATURES* but different

;;; This variable is declared here (like SB-XC:*FEATURES*) so that
;;; things like chill.lisp work (because the variable has properties
;;; similar to SB-XC:*FEATURES*, and chill.lisp was set up to work
;;; for that). For an explanation of what it really does, look
;;; elsewhere.
;;; FIXME: Can we just assign SB-C:*BACKEND-SUBFEATURES* directly?
;;; (This has nothing whatsoever to do with the so-called "shebang" reader)
(export '*shebang-backend-subfeatures*)
(declaim (type list *shebang-backend-subfeatures*))
(defvar *shebang-backend-subfeatures*)

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
