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
;;;; for SB!XC:*FEATURES* instead of CL:*FEATURES*. (This is handy
;;;; when cross-compiling, so that we can make a distinction between
;;;; features of the host Common Lisp and features of the target
;;;; SBCL.)

;;; the feature list for the target system
(unless (find-package "SB!XC")
  (make-package "SB!XC" :use nil :nicknames nil))
(export (intern "*FEATURES*" "SB!XC") "SB!XC")
(declaim (type list sb!xc:*features*))
(defvar sb!xc:*features*)

(defun target-platform-keyword (&optional (features sb!xc:*features*))
  (let ((arch (intersection '(:alpha :arm :arm64 :hppa :mips :ppc :ppc64 :sparc :x86 :x86-64)
                            features)))
    (cond ((not arch) (error "No architecture selected"))
          ((> (length arch) 1) (error "More than one architecture selected")))
    (car arch)))

;;; Not necessarily the logical place to define BACKEND-ASM-PACKAGE-NAME,
;;; but a convenient one, because sb!xc:*features* needs to have been
;;; DEFVARed, and because 'chill' loads this and only this file.
(defun backend-asm-package-name ()
  (concatenate 'string "SB!" (string (target-platform-keyword)) "-ASM"))

(defun any-vop-named-p (vop-name)
  (gethash vop-name (symbol-value (find-symbol "*BACKEND-PARSED-VOPS*" "SB!C"))))

(defun any-vop-translates-p (fun-name)
  (let ((f (intern "INFO" "SB!INT")))
    (when (fboundp f)
      (let ((info (funcall f :function :info fun-name)))
        (if info
            (let ((f (intern "FUN-INFO-TEMPLATES" "SB!C")))
              (and (fboundp f) (not (null (funcall f info)))))
            (error "vop-translates: ~s is not a known function." fun-name))))))

(defun feature-in-list-p (feature list)
  (labels ((sane-expr-p (x)
             (typecase x
               (symbol (and (string/= x "SB-XC") (string/= x "SB-XC-HOST")))
               ;; This allows you to write #!+(host-feature sbcl) <stuff>
               ;; to muffle conditions, bypassing the "probable XC bug" check.
               ;; Using the escape hatch is assumed never to be a mistake.
               ((cons (member :host-feature :vop-named :vop-translates)) t)
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
              ((:host-feature :not :vop-named :vop-translates)
               (destructuring-bind (subexpr) (cdr feature)
                 (case (first feature)
                   (:host-feature
                        ;; (:HOST-FEATURE :sym) looks in *FEATURES* for :SYM
                    (check-type subexpr symbol)
                    (member subexpr *features* :test #'eq))
                   (:vop-named (any-vop-named-p subexpr))
                   (:vop-translates (any-vop-translates-p subexpr))
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
                     (feature-in-list-p feature sb!xc:*features*))
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
;;; ideally we wouldn't use SB!XC:*FEATURES* but something like
;;; *ALL-POSSIBLE-TARGET-FEATURES*, but maintaining that variable
;;; would not be easy.
(defun checked-feature-in-features-list-p (feature list)
  (etypecase feature
    (symbol (unless (member feature '(:ansi-cl :common-lisp :ieee-floating-point))
              (when (member feature sb!xc:*features* :test #'eq)
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

;;;; variables like SB!XC:*FEATURES* but different

;;; This variable is declared here (like SB!XC:*FEATURES*) so that
;;; things like chill.lisp work (because the variable has properties
;;; similar to SB!XC:*FEATURES*, and chill.lisp was set up to work
;;; for that). For an explanation of what it really does, look
;;; elsewhere.
;;; FIXME: Can we just assign SB!C:*BACKEND-SUBFEATURES* directly?
;;; (This has nothing whatsoever to do with the so-called "shebang" reader)
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
