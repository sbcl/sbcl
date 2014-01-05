(in-package "SB!IMPL")

;; Cribbed from SYS:SRC;CODE;TOPLEVEL.
(defun %halt ()
  (%primitive sb!c:halt))

(defun test-closure (closure)
  (declare (function closure))
  (funcall closure)
  nil)

(defun test-uwp ()
  (let ((value 3))
    (unwind-protect
         (test-closure (lambda ()
                         (setf value 7)))
      (setf value (logand value 4)))
    value))

(defun test-tcv-result (arg1 arg2 arg3 arg4)
  (values arg1 arg2 arg3 arg4))

(defun test-tail-call-variable (fun arg1 &rest args)
  (declare (function fun))
  (apply fun arg1 args))

(defun !cold-init ()
  (test-uwp)
  (test-tail-call-variable #'test-tcv-result 42 57 69 104)
  (%primitive print "Testing %PRIMITIVE PRINT.")
  #+(or) (%halt))
