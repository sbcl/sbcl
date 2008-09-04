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
(defvar *shebang-features*)

(defun feature-in-list-p (feature list)
  (etypecase feature
    (symbol (member feature list :test #'eq))
    (cons (flet ((subfeature-in-list-p (subfeature)
                   (feature-in-list-p subfeature list)))
            (ecase (first feature)
              (:or  (some  #'subfeature-in-list-p (rest feature)))
              (:and (every #'subfeature-in-list-p (rest feature)))
              (:not (let ((rest (cdr feature)))
                      (if (or (null (car rest)) (cdr rest))
                        (error "wrong number of terms in compound feature ~S"
                               feature)
                        (not (subfeature-in-list-p (second feature)))))))))))
(compile 'feature-in-list-p)

(defun shebang-reader (stream sub-character infix-parameter)
  (declare (ignore sub-character))
  (when infix-parameter
    (error "illegal read syntax: #~D!" infix-parameter))
  (let ((next-char (read-char stream)))
    (unless (find next-char "+-")
      (error "illegal read syntax: #!~C" next-char))
    ;; When test is not satisfied
    ;; FIXME: clearer if order of NOT-P and (NOT NOT-P) were reversed? then
    ;; would become "unless test is satisfied"..
    (when (let* ((*package* (find-package "KEYWORD"))
                 (*read-suppress* nil)
                 (not-p (char= next-char #\-))
                 (feature (read stream)))
            (if (feature-in-list-p feature *shebang-features*)
                not-p
                (not not-p)))
      ;; Read (and discard) a form from input.
      (let ((*read-suppress* t))
        (read stream t nil t))))
  (values))
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
                (error "probable XC bug in host read-time conditional")))
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

;;;; FIXME: Would it be worth implementing this?
#|
;;;; readmacro syntax to remove spaces from FORMAT strings at compile time
;;;; instead of leaving them to be skipped over at runtime

;;; a counter of the number of bytes that we think we've avoided having to
;;; compile into the system by virtue of doing compile-time processing
(defvar *shebang-double-quote--approx-bytes-saved* 0)

;;; Read a string, strip out any #\~ #\NEWLINE whitespace sequence,
;;; and return the result. (This is a subset of the processing performed
;;; by FORMAT, but we perform it at compile time instead of postponing
;;; it until run-time.
(defun shebang-double-quote (stream)
  (labels ((rc () (read-char stream))
           (white-p (char)
             ;; Putting non-standard characters in the compiler source is
             ;; generally a bad idea, since we'd like to be really portable.
             ;; It's specifically a bad idea in strings intended to be
             ;; processed by SHEBANG-DOUBLE-QUOTE, because there seems to be no
             ;; portable way to test a non-STANDARD-CHAR for whitespaceness.
             ;; (The most common problem would be to put a #\TAB -- which is
             ;; not a STANDARD-CHAR -- into the string. If this is part of the
             ;; to-be-skipped-over whitespace after a #\~ #\NEWLINE sequence in
             ;; the string, it won't work, because it won't be recognized as
             ;; whitespace.)
             (unless (typep char 'standard-char)
               (warn "non-STANDARD-CHAR in #!\": ~C" result))
             (or (char= char #\newline)
                 (char= char #\space)))
           (skip-white ()
             (do ((char (rc) (rc))
                  (count 0 (1+ count)))
                 ((not (white-p char))
                  (unread-char char stream)
                  count))))
    (do ((adj-string (make-array 0 :element-type 'char :adjustable t))
         (char (rc) (rc)))
        ((char= char #\") (coerce adj-string 'simple-string))
      (cond ((char= char #\~)
             (let ((next-char (read-char stream)))
               (cond ((char= next-char #\newline)
                      (incf *shebang-double-quote--approx-bytes-saved*
                            (+ 2 (skip-white))))
                     (t
                      (vector-push-extend      char adj-string)
                      (vector-push-extend next-char adj-string)))))
            ((char= char #\\)
             (vector-push-extend char adj-string)
             (vector-push-extend (rc) adj-string))
            (t (vector-push-extend char adj-string))))))

(setf (gethash #\" *shebang-dispatch*)
      #'shebang-double-quote)
|#