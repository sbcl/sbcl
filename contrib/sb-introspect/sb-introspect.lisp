(defpackage :sb-introspect
  (:use "CL")
  (:export "FUNCTION-ARGLIST" "VALID-FUNCTION-NAME-P"))

(in-package :sb-introspect)

;; This is here as a discussion point, not yet a supported interface.  If
;; you would like to use the functions here, or you would like other
;; functions to be here, join the debate on sbcl-devel

(defun valid-function-name-p (name)
  "True if NAME denotes a function name that can be passed to MACRO-FUNCTION or FDEFINITION "
  (and (sb-int:valid-function-name-p name) t))

(defun function-arglist (function)
  "Given a function designator FUNCTION, return a description of its lambda list.  Works for macros, simple functions and generic functions"
  (cond ((valid-function-name-p function) 
         (function-arglist
	  (or (macro-function function) (fdefinition function))))
        ((typep function 'generic-function)
         (sb-pcl::generic-function-pretty-arglist function))
        (t
         (sb-impl::%simple-fun-arglist function))))

