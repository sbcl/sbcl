(in-package "SB!DI")

(defun current-thread-control-stack-start () 
  (int-sap *control-stack-start*))

(defun current-thread-control-stack-end ()
  (sap+ (int-sap *binding-stack-start*) -4))

(defun nth-interrupt-context (n)
  (sb!alien:sap-alien (nth-interrupt-context-sap n) (* os-context-t)))
