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

(in-package "SB-KERNEL")
;;; FIXME: Perhaps this belongs in the %SYS package like some other
;;; cold load stuff.

(defvar *!cold-init-forms*)

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
  #+sb-xc `(eval-when (:compile-toplevel)
             ,@(mapcar (lambda (form) `(push ',form *!cold-init-forms*))
                       forms))
  ;; In the cross-compilation host Lisp, cold load is not a
  ;; meaningful concept. Just execute the forms at load time.
  #-sb-xc `(progn ,@forms))

(defmacro !defun-from-collected-cold-init-forms (name)
  #+sb-xc `(progn
             (defun ,name ()
               ,@(reverse *!cold-init-forms*)
               (values))
             (eval-when (:compile-toplevel :execute)
               (makunbound '*!cold-init-forms*)))
  #-sb-xc (declare (ignore name)))

;;; !DEFINE-LOAD-TIME-GLOBAL avoids this anti-pattern:
;;;    (!begin-collecting-cold-init-forms)
;;;    (define-load-time-global *foo* nil)
;;;    (!cold-init-forms (setq *foo* nil))
;;;    (!defun-from-cold-init-forms !some-cold-init-fun)
(defmacro !define-load-time-global (sym value &optional (doc nil doc-p))
  `(progn (define-load-time-global ,sym ,value ,@(if doc-p (list doc)))
          ;; The expansion of the DEFINE-L-T-G is too hairy to fopcompile,
          ;; but SETQ is ok provided that the value form is ok.
          #-sb-xc-host (setq ,sym ,value)))

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
                 '(sb-xc:make-load-form-saving-slots obj :environment env)))
        (t
         (assert (not (member :host usable-by)))
         (values nil `(funcall ,method obj env))))
    `(progn
       ,@(when (or #+sb-xc-host (member :host usable-by))
           `((defmethod make-load-form ((obj ,class-name) &optional env)
               ,host-expr)))
       ,@(when (or #+sb-xc-host (member :xc usable-by))
           ;; Use the host's CLOS implementation to select the target's method.
           `((defmethod sb-xc:make-load-form ((obj ,class-name) &optional env)
               (declare (ignorable obj env))
               ,target-expr)))
       ,@(when (or #-sb-xc-host (member :target usable-by))
           ;; Use the target's CLOS implementation
           `((defmethod make-load-form ((obj ,class-name) &optional env)
               (declare (ignorable obj env))
               ,target-expr))))))

;;; Define a variable that is assigned into TLS either in INIT-INITIAL-THREAD
;;; or NEW-LISP-THREAD-TRAMPOLINE before any other Lisp code runs.
;;; !COLD-INIT gets these assignents via INIT-INITIAL-THREAD.
;;; In particular, *RESTART-CLUSTERS* and *HANDLER-CLUSTERS* need a value prior
;;; to running into any errors, or else you've got twice the trouble.
;;;
;;; There are other thread-locals which are used by C code, and those are all
;;; defined in !PER-THREAD-C-INTERFACE-SYMBOLS. Those other symbols must have
;;; initial values of either a fixnum or a static symbol (typically T or NIL).
;;;
;;; In contrast, !DEFINE-THREAD-LOCAL allows more-or-less an arbitrary form,
;;; subject to being evaluable without need of values computed later of course.
(defvar sb-thread::*!thread-local-specials* nil)
#-sb-xc-host
(defmacro !define-thread-local (name initform &optional docstring)
  `(locally
       (declare (notinline (setf info)))
     (defvar ,name (error "~S had no value in cold-init" ',name) ,docstring)
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf sb-thread::*!thread-local-specials*
             (append sb-thread::*!thread-local-specials* '((,name ,initform))))
       ;; Does it help anything to choose the TLS index now? Probably not,
       ;; we'll use a symbol-tls fixup almost everywhere except in INITIALIZE-TLS.
       #+sb-thread (setf (info :variable :wired-tls ',name) :always-thread-local)
       (setf (info :variable :always-bound ',name) :always-bound))))
