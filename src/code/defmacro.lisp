;;;; DEFMACRO machinery

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;; the guts of the DEFMACRO macro, pulled out into a separate
;;; function in order to make it easier to express the common 
;;; bootstrap idiom
;;;   CL:DEFMACRO SB!XC:DEFMACRO
;;;   SB!XC:DEFMACRO CL:DEFMACRO
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %expander-for-defmacro (name lambda-list body)
    (let ((whole (gensym "WHOLE-"))
	  (environment (gensym "ENV-")))
      (multiple-value-bind (new-body local-decs doc)
	  (parse-defmacro lambda-list whole body name 'defmacro
			  :environment environment)
	(let ((def `(lambda (,whole ,environment)
		      ,@local-decs
		      (block ,name
			,new-body))))
	  `(sb!c::%defmacro ',name #',def ',lambda-list ,doc))))))

;;; Ordinarily this definition of SB!C:%DEFMACRO as an ordinary
;;; function is not used: the parallel (but different) definition as
;;; an IR1 transform takes precedence. However, this definition is
;;; still useful in the target interpreter, and in the
;;; cross-compilation host.
(defun sb!c::%defmacro (name definition lambda-list doc)
  (try-to-rename-interpreted-function-as-macro definition name lambda-list)
  (sb!c::%%defmacro name definition doc))

;;; (called by SB!C::%DEFMACRO)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun sb!c::%%defmacro (name definition doc)
    ;; Old note (ca. 1985, maybe:-): "Eventually %%DEFMACRO should deal with
    ;; clearing old compiler information for the functional value."
    (clear-info :function :where-from name)
    ;; FIXME: It would be nice to warn about DEFMACRO of an
    ;; already-defined macro, but that's slightly hard to do because
    ;; in common usage DEFMACRO is defined at compile time and then
    ;; redefined at load time. We'd need to make a distinction between
    ;; the defined-at-compile-time state and the defined-at-load-time
    ;; state to make this work. (Trying to warn about duplicate DEFTYPEs
    ;; runs into the same problem.)
    #+nil (when (sb!xc:macro-function name)
	    (style-warn "redefining ~S in DEFMACRO" name))
    (setf (sb!xc:macro-function name) definition
	  (fdocumentation name 'function) doc)
    name))

;;; Parse the definition and make an expander function. The actual
;;; definition is done by %DEFMACRO which we expand into, and which is
;;; handled magically by an IR1 transform. After the compiler has
;;; gotten the information it wants out of macro definition, it
;;; compiles a call to %%DEFMACRO which happens at load time.
(defmacro sb!xc:defmacro (name lambda-list &rest body)
  (%expander-for-defmacro name lambda-list body))

;;; In the cross-compiler, we not only need to support the definition
;;; of target macros at cross-compiler-build-time (with SB!XC:DEFMACRO
;;; running in the cross-compilation host), we also need to support
;;; the definition of target macros at target compilation time (with
;;; CL:DEFMACRO processed by the cross-compiler)..
#+sb-xc-host
(sb!xc:defmacro defmacro (name lambda-list &rest body)
  (%expander-for-defmacro name lambda-list body))

;;; DEFMACRO-MUNDANELY is like SB!XC:DEFMACRO, except that it doesn't
;;; have any EVAL-WHEN or IR1 magic associated with it, so it only
;;; takes effect in :LOAD-TOPLEVEL or :EXECUTE situations.
;;;
;;; KLUDGE: Currently this is only used for various special
;;; circumstances in bootstrapping, but it seems to me that it might
;;; be a good basis for reimplementation of DEFMACRO in terms of
;;; EVAL-WHEN, which might be easier to understand than the current
;;; approach based on IR1 magic. -- WHN 19990811
(def!macro defmacro-mundanely (name lambda-list &body body)
  `(progn
     (setf (sb!xc:macro-function ',name)
	   ,(let ((whole (gensym "WHOLE-"))
		  (environment (gensym "ENVIRONMENT-")))
	      (multiple-value-bind (new-body local-decs doc)
		  (parse-defmacro lambda-list whole body name 'defmacro
				  :environment environment)
		(declare (ignore doc))
		`(lambda (,whole ,environment)
		   ,@local-decs
		   (block ,name
		     ,new-body)))))
     ',name))
