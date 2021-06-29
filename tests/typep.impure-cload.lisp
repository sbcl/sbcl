(declaim (muffle-conditions compiler-note style-warning))

(defclass myclass () ())

(defun control (x)
  (declare (type (or null otherclass) x))
  (funcall 'f2 x))

(defun experiment (x)
  (declare (type (or null myclass) x))
  (funcall 'f2 x))

(defun uses-cached-typep (f)
  (let ((c (sb-kernel:fun-code-header f)))
    (loop for i from sb-vm:code-constants-offset
          below (sb-kernel:code-header-words c)
          thereis (let ((const (sb-kernel:code-header-ref c i)))
                    (and (consp const)
                         (eq (car const) #'sb-kernel::cached-typep))))))

(with-test (:name :typep-of-a-class)
  ;; Check that we know how to detect that a function used inefficient TYPEP
  (assert (uses-cached-typep #'control))
  (assert (not (uses-cached-typep #'experiment))))
