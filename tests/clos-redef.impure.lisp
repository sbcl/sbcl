(defclass duck () ((bills :accessor duck-bills :initform 0)))
(defvar *d* (make-instance 'duck))
(defvar *intercepted* nil)
;; x86-64 optimized out the linkage table ref from the UPDATE-OBJECT-LAYOUT
;; asm routine to the lisp function. This not as performance-critical as
;; optimizing out the calls for generic-{+,-,*}, so let's assert that there
;; is a call through the linkage table by checking that ENCAPSULATE works.
(sb-int:encapsulate 'sb-kernel:update-object-layout 'test
 (compile nil '(lambda (realfun x)
                (setq *intercepted* t)
                (funcall realfun x))))
(defclass duck ()
  ((bills :accessor duck-bills :initform 0)
   (color :initform :green)))
(with-test (:name :encapsulate-update-obj-layout)
  (assert (not *intercepted*))
  (assert (not (streamp *d*)))
  (assert *intercepted*))
