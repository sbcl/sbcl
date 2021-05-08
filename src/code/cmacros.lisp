;;;; Compiler macros that are important for the target system

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;;; We often use a source-transform to do macro-like rewriting of an
;;;; ordinary function call. Source-transforms seem to pre-date the ANSI
;;;; specification and are redundant with compiler-macros.
;;;; In the interest of not multiplying entities needlessly, it should
;;;; be feasible to get rid of source-transforms.
;;;; A problem is namespace clobbering: these must not affect the host Lisp.

;;; A sanity-checker for an extremely common programmer error.
(define-compiler-macro format (&whole form destination control &rest args)
  (declare (ignore control args))
  (when (stringp destination)
    (warn "Literal string as destination in FORMAT:~%  ~S" form))
  form)

(eval-when (:compile-toplevel :load-toplevel :execute)
(declaim (inline maybe-note-read-from-string-signature-issue))
(defun maybe-note-read-from-string-signature-issue (eof-error-p)
  ;; The interface is so unintuitive that we explicitly check for the common
  ;; error.
  (when (member eof-error-p '(:start :end :preserve-whitespace))
    (style-warn "~@<~S as EOF-ERROR-P argument to ~S: probable error. ~
               Two optional arguments must be provided before the ~
               first keyword argument.~:@>"
                eof-error-p 'read-from-string)
    t)))

(define-compiler-macro read-from-string (&whole form string &rest args
                                         &environment env)
  ;; Check this at compile-time, and rewrite it so we're silent at runtime.
  (destructuring-bind (&optional (eof-error-p t) eof-value &rest keys) args
    (if (maybe-note-read-from-string-signature-issue eof-error-p)
        `(read-from-string ,string t ,eof-value ,@keys)
        (do ((seen 0)
             ;; the :START, :END, :PRESERVE-WHITESPACE defaults respectively
             (list (list 0 nil nil))
             (bind)
             ignore)
            ((not (cdr keys))
             (if keys
                 form ; Odd number of keys, punt.
                 (let ((positionals (list (copy-symbol 'string)
                                          (copy-symbol 'eof-error-p)
                                          (copy-symbol 'eof-value)))
                       (fun-name (if (sb-c:policy env (= safety 3))
                                     '%read-from-string/safe
                                     '%read-from-string)))
                   `(let (,@(mapcar #'list positionals
                                    (list string eof-error-p eof-value))
                          ,@(nreverse bind))
                      ,@(when ignore `((declare (ignore ,@ignore))))
                      (,fun-name ,@positionals ,@list)))))
          (let* ((key (pop keys))
                 (index (case key
                          (:start 0)
                          (:end 1)
                          (:preserve-whitespace 2)
                          (otherwise (return-from read-from-string form))))
                 (var (if (logbitp index seen)
                          (let ((x (sb-xc:gensym "IGNORE")))
                            (push x ignore)
                            x)
                          (setf seen (logior (ash 1 index) seen)
                                (nth index list) (copy-symbol key)))))
            (push (list var (pop keys)) bind))))))

(defmacro def!struct (&rest args) `(defstruct ,@args))
