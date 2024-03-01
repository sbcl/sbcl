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

;;;; Source transforms are used by the compiler to make code more
;;;; canonical so that the compiler can compile it futher; they are
;;;; not optional. Compiler macros are an optional source rewriting
;;;; mechanism mainly for compile-time syntax checking and
;;;; optimizations that can be declined for any reason, but especially
;;;; through the use of NOTINLINE. Perhaps the actual mechanism
;;;; outside the decision to do rewriting could be reunified. We also
;;;; must pay special attention when writing compiler macros for the
;;;; purposes of cross-compiling. A problem is namespace clobbering:
;;;; these must not affect the host Lisp.

;;; The function that corresponds to this macro is defined in src/code/typep.
;;; This expansion is not particularly good for the interpreter, so just
;;; call the function when not compiling.
(define-compiler-macro sb-kernel::%typecase-index (layout-lists object sealed)
  (let ((exp (sb-impl::optimize-%typecase-index layout-lists object sealed)))
    exp))

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
                          (let ((x (gensym "IGNORE")))
                            (push x ignore)
                            x)
                          (setf seen (logior (ash 1 index) seen)
                                (nth index list) (copy-symbol key)))))
            (push (list var (pop keys)) bind))))))

(defmacro def!struct (&rest args) `(defstruct ,@args))
