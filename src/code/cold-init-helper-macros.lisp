;;;; This file contains machinery for collecting forms that, in the
;;;; target Lisp, must happen before top level forms are run. The
;;;; forms are stuffed into named functions which will be explicitly
;;;; called in the appropriate order by !COLD-INIT.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")
;;; FIXME: Perhaps this belongs in the %SYS package like some other
;;; cold load stuff.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *!cold-init-forms*))

(defmacro !begin-collecting-cold-init-forms ()
  #+sb-xc '(eval-when (:compile-toplevel :execute)
             (when (boundp '*!cold-init-forms*)
               (warn "discarding old *!COLD-INIT-FORMS* value"))
             (setf *!cold-init-forms* nil))
  #-sb-xc nil)

;;; Note: Unlike the analogous COLD-INIT macro in CMU CL, this macro
;;; makes no attempt to simulate a top level situation by treating
;;; EVAL-WHEN forms specially.
(defmacro !cold-init-forms (&rest forms)
  ;; In the target Lisp, stuff the forms into a named function which
  ;; will presumably be executed at the appropriate stage of cold load
  ;; (i.e. basically as soon as possible).
  #+sb-xc (progn
            (setf *!cold-init-forms*
                  (nconc *!cold-init-forms* (copy-list forms)))
            nil)
  ;; In the cross-compilation host Lisp, cold load is not a
  ;; meaningful concept. Just execute the forms at load time.
  #-sb-xc `(progn ,@forms))

(defmacro !defun-from-collected-cold-init-forms (name)
  #+sb-xc `(progn
             (defun ,name ()
               ,@*!cold-init-forms*
               (values))
             (eval-when (:compile-toplevel :execute)
               (makunbound '*!cold-init-forms*)))
  #-sb-xc (declare (ignore name)))

;;; !DEFGLOBAL, !DEFPARAMETER and !DEFVAR are named by analogy
;;; with !COLD-INIT-FORMS and (not DEF!FOO) because they are
;;; basically additional cold-init-helpers to avoid the tedious sequence:
;;;    (!begin-collecting-cold-init-forms)
;;;    (defvar *foo*)
;;;    (!cold-init-forms (setq *foo* nil))
;;;    (!defun-from-cold-init-forms !some-cold-init-fun)
;;; or the less respectable (defvar *foo*) and a random SETQ in !COLD-INIT.
;;; Each is like its namesake, but also arranges so that genesis knows
;;; the initialization form, on which it calls EVAL and dumps as a constant
;;; when writing out the cold core image.
(macrolet ((def (wrapper real-name)
             `(defmacro ,wrapper (sym value &optional (doc nil doc-p))
                `(progn (,',real-name ,sym ,value ,@(if doc-p (list doc)))
                        #-sb-xc-host (sb!fasl::setq-no-questions-asked ,sym ,value)))))
  (def !defglobal defglobal)
  (def !defparameter defparameter)
  (def !defvar defvar))

(defmacro !set-load-form-method (class-name usable-by &optional method)
  ;; If USABLE-BY is:
  ;;  :host   - the host compiler can execute this M-L-F method
  ;;  :xc     - the cross-compiler can execute this M-L-F method
  ;;  :target - the target compiler can execute this M-L-F method
  (assert (and usable-by
               (every (lambda (x) (member x '(:host :xc :target)))
                      usable-by)))
  (multiple-value-bind (host-expr target-expr)
      (case method
        ((nil) ; default
         (values '(cl:make-load-form-saving-slots obj :environment env)
                 '(sb!xc:make-load-form-saving-slots obj :environment env)))
        (:ignore-it
         (values '(bug "Can't :ignore-it for host") :ignore-it))
        (t
         (assert (not (member :host usable-by)))
         (values nil `(funcall ,method obj env))))
    `(progn
       ,@(when (or #+sb-xc-host (member :host usable-by))
           `((defmethod make-load-form ((obj ,class-name) &optional env)
               ,host-expr)))
       ,@(when (or #+sb-xc-host (member :xc usable-by))
           ;; Use the host's CLOS implementation to select the target's method.
           `((defmethod sb!xc:make-load-form ((obj ,class-name) &optional env)
               (declare (ignorable obj env))
               ,target-expr)))
       ,@(when (or #-sb-xc-host (member :target usable-by))
           ;; Use the target's CLOS implementation
           `((defmethod make-load-form ((obj ,class-name) &optional env)
               (declare (ignorable obj env))
               ,target-expr))))))

;;; Define a variable that is initialized in create_thread_struct() before any
;;; Lisp code can execute. In particular, *RESTART-CLUSTERS* and *HANDLER-CLUSTERS*
;;; should have a value before anything else happens.
;;; While thread-local vars are generally useful, this is not the implementation
;;; that would exist in the target system, if exposed more generally.
;;; (Among the issues is the very restricted initialization form)
(defmacro !define-thread-local (name initform &optional docstring)
  (check-type initform symbol)
  #!-sb-thread `(progn
                  (eval-when (:compile-toplevel :load-toplevel :execute)
                    (setf (info :variable :always-bound ',name) :always-bound))
                  (!defvar ,name ,initform ,docstring))
  #!+sb-thread `(progn
                  #-sb-xc-host (!%define-thread-local ',name ',initform)
                  (eval-when (:compile-toplevel :load-toplevel :execute)
                    (setf (info :variable :wired-tls ',name) :always-thread-local)
                    (setf (info :variable :always-bound ',name) :always-bound))
                  (defvar ,name ,initform ,docstring)))

(defvar *!thread-initial-bindings* nil)
#+sb-xc-host
(setf (get '!%define-thread-local :sb-cold-funcall-handler/for-effect)
      (lambda (name initsym)
        (push `(,name . ,initsym) *!thread-initial-bindings*)))
#-sb-xc-host
(defun !%define-thread-local (dummy1 dummy2) ; to avoid warning
  (declare (ignore dummy1 dummy2)))

;;; FIXME: Consider renaming this file asap.lisp,
;;; and the renaming the various things
;;;   *ASAP-FORMS* or *REVERSED-ASAP-FORMS*
;;;   WITH-ASAP-FORMS
;;;   ASAP or EVAL-WHEN-COLD-LOAD
;;;   DEFUN-FROM-ASAP-FORMS
;;; If so, add a comment explaining that ASAP is colloquial English for "as
;;; soon as possible", and has nothing to do with "system area pointer".
