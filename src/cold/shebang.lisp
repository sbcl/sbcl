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

;;;; definition of #!+ and #!- as a mechanism analogous to #+/#-, but
;;;; for *SHEBANG-FEATURES* instead of CL:*FEATURES*. (This is handy
;;;; when cross-compiling, so that we can make a distinction between
;;;; features of the host Common Lisp and features of the target
;;;; SBCL.)

;;; the feature list for the target system
(export '*shebang-features*)
(declaim (type list *shebang-features*))
;; FIXME: is there a reason this isn't SB!XC:*FEATURES* ?
;; We haven't set up the SB!XC package yet, but we certainly could.
(defvar *shebang-features*)

(defun target-platform-name ()
  (let ((arch (intersection '(:alpha :arm :arm64 :hppa :mips :ppc :sparc :x86 :x86-64)
                            *shebang-features*)))
    (cond ((not arch) (error "No architecture selected"))
          ((> (length arch) 1) (error "More than one architecture selected")))
    (string-downcase (car arch))))

;;; Not necessarily the logical place to define BACKEND-ASM-PACKAGE-NAME,
;;; but a convenient one, because *shebang-features* needs to have been
;;; DEFVARed, and because 'chill' loads this and only this file.
(defun backend-asm-package-name ()
  (concatenate 'string "SB!" (string-upcase (target-platform-name)) "-ASM"))

(defun feature-in-list-p (feature list)
  (labels ((sane-expr-p (x)
             (typecase x
               (symbol (and (string/= x "SB-XC") (string/= x "SB-XC-HOST")))
               ;; This allows you to write #!+(host-feature sbcl) <stuff>
               ;; to muffle conditions, bypassing the "probable XC bug" check.
               ;; Using the escape hatch is assumed never to be a mistake.
               ((cons (eql :host-feature)) t)
               (cons (every #'sane-expr-p (cdr x))))))
    (unless (sane-expr-p feature)
      (error "Target feature expression ~S looks screwy" feature)))
  (etypecase feature
    (symbol (member feature list :test #'eq))
    (cons (flet ((subfeature-in-list-p (subfeature)
                   (feature-in-list-p subfeature list)))
            (ecase (first feature)
              (:or  (some  #'subfeature-in-list-p (rest feature)))
              (:and (every #'subfeature-in-list-p (rest feature)))
              ((:host-feature :not)
               (destructuring-bind (subexpr) (cdr feature)
                 (cond ((eq (first feature) :host-feature)
                        ;; (:HOST-FEATURE :sym) looks in *FEATURES* for :SYM
                        (check-type subexpr symbol)
                        (member subexpr *features* :test #'eq))
                       (t
                        (not (subfeature-in-list-p subexpr)))))))))))
(compile 'feature-in-list-p)

(defun shebang-reader (stream sub-character infix-parameter)
  (declare (ignore sub-character))
  (when infix-parameter
    (error "illegal read syntax: #~D!" infix-parameter))
  (let ((next-char (read-char stream)))
    (unless (find next-char "+-")
      (error "illegal read syntax: #!~C" next-char))
    (if (char= (if (let* ((*package* (find-package "KEYWORD"))
                          (*read-suppress* nil)
                          (feature (read stream)))
                     (feature-in-list-p feature *shebang-features*))
                   #\+ #\-) next-char)
        (read stream t nil t)
        ;; Read (and discard) a form from input.
        (let ((*read-suppress* t))
          (read stream t nil t)
          (values)))))
(compile 'shebang-reader)

(set-dispatch-macro-character #\# #\! #'shebang-reader)
;;; while we are at it, let us write something which helps us sanity
;;; check our own code; it is too easy to write #+ when meaning #!+,
;;; and such mistakes can go undetected for a while.
;;;
;;; ideally we wouldn't use *SHEBANG-FEATURES* but
;;; *ALL-POSSIBLE-SHEBANG-FEATURES*, but maintaining that variable
;;; will not be easy.
(defun checked-feature-in-features-list-p (feature list)
  (etypecase feature
    (symbol (unless (member feature '(:ansi-cl :common-lisp :ieee-floating-point))
              (when (member feature *shebang-features* :test #'eq)
                (error "probable XC bug in host read-time conditional: ~S" feature)))
            (member feature list :test #'eq))
    (cons (flet ((subfeature-in-list-p (subfeature)
                   (checked-feature-in-features-list-p subfeature list)))
            (ecase (first feature)
              (:or  (some  #'subfeature-in-list-p (rest feature)))
              (:and (every #'subfeature-in-list-p (rest feature)))
              (:not (let ((rest (cdr feature)))
                      (if (or (null (car rest)) (cdr rest))
                        (error "wrong number of terms in compound feature ~S"
                               feature)
                        (not (subfeature-in-list-p (second feature)))))))))))
(compile 'checked-feature-in-features-list-p)

(defun she-reader (stream sub-character infix-parameter)
  (when infix-parameter
    (error "illegal read syntax: #~D~C" infix-parameter sub-character))
  (when (let* ((*package* (find-package "KEYWORD"))
               (*read-suppress* nil)
               (notp (eql sub-character #\-))
               (feature (read stream)))
          (if (checked-feature-in-features-list-p feature *features*)
              notp
              (not notp)))
    (let ((*read-suppress* t))
      (read stream t nil t)))
  (values))
(compile 'she-reader)

;;;; variables like *SHEBANG-FEATURES* but different

;;; This variable is declared here (like *SHEBANG-FEATURES*) so that
;;; things like chill.lisp work (because the variable has properties
;;; similar to *SHEBANG-FEATURES*, and chill.lisp was set up to work
;;; for that). For an explanation of what it really does, look
;;; elsewhere.
(export '*shebang-backend-subfeatures*)
(declaim (type list *shebang-backend-subfeatures*))
(defvar *shebang-backend-subfeatures*)

;;;; string checker, for catching non-portability early
(defun make-quote-reader (standard-quote-reader)
  (lambda (stream char)
    (let ((result (funcall standard-quote-reader stream char)))
      (unless (every (lambda (x) (typep x 'standard-char)) result)
        (warn "Found non-STANDARD-CHAR in ~S" result))
      result)))
(compile 'make-quote-reader)

(set-macro-character #\" (make-quote-reader (get-macro-character #\" nil)))
