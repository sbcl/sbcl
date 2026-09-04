#-tls-based-mv-return (invoke-restart 'run-tests::skip-file)

(with-test (:name :too-many-values-compile-time-error)
  (assert (nth-value 1
            (checked-compile
             `(lambda ()
                (values ,@(loop for i from 10 repeat multiple-values-limit collect i)))
             :allow-warnings t))))

(with-test (:name :values-list-too-many-values-safely-fails)
  (let ((fn (checked-compile `(lambda ()
                                (values-list
                                 (opaque-identity (make-list multiple-values-limit
                                                             :initial-element #\x)))))))
    (assert (eq :failed-safely
                (handler-case (funcall fn)
                  (program-error () :failed-safely))))))


;; Non-tail values-list returning out of a CATCH
(with-test (:name :return-multiple-too-many-values-in-catch)
  (let ((fn (checked-compile
             `(lambda ()
                (catch 'done
                  (values-list
                   (opaque-identity (make-list multiple-values-limit
                                               :initial-element #\x))))))))
    (assert (eq :failed-safely
                (handler-case (funcall fn)
                  (program-error () :failed-safely))))))

;; Non-tail values-list returning out of an UNWIND-PROTECT
(with-test (:name :return-multiple-too-many-values-in-unwind-protect)
  (let ((fn (checked-compile
             `(lambda ()
                (unwind-protect
                    (values-list
                     (opaque-identity (make-list multiple-values-limit
                                                 :initial-element #\y)))
                  (opaque-identity nil))))))
    (assert (eq :failed-safely
                (handler-case (funcall fn)
                  (program-error () :failed-safely))))))

;; Valid MULTIPLE-VALUE-CALL exceeding multiple-values-limit
;; (VALUES-LIST as an argument producer is not erroneously capped)
(with-test (:name :multiple-value-call-exceeds-mv-limit-ok)
  (let ((fn (checked-compile
             `(lambda ()
                (multiple-value-call #'list
                  (values-list (make-list 40 :initial-element 1))
                  (values-list (make-list 40 :initial-element 2)))))))
    (assert (= (length (funcall fn)) 80))))

;;;

(defun get-mv-return-count ()
  (let ((word (sb-sys:sap-int (sb-vm::current-thread-offset-sap sb-vm:thread-state-word-slot))))
    ;; assumes little-endian
    (ash (ldb (byte 8 8) word) (- sb-vm:n-fixnum-tag-bits))))

(defun hairy-result ()
  (loop for i from 0 repeat 32 collect (list 'massive-structure i)))

;; the symbol sb-vm::register-arg-count might have dropped out
(defparameter mv-return-register-count (length sb-vm::*register-arg-offsets*))

(with-test (:name :mv-area-gc-scrubbing :fails-on :interpreter)
  (let ((f1 (checked-compile `(lambda () (values-list (hairy-result)))))
        (f2 (checked-compile `(lambda () (values-list (list 'a 'b 'c 'd 'e))))))
    (funcall f1)
    (funcall f2)
    (gc :full t)
    (let ((mv-count (get-mv-return-count)))
      (assert (= mv-count 5))
      (loop for i from (+ sb-vm::thread-mv-return-values-slot
                          (- 5 mv-return-register-count))
            for word = (sb-sys:sap-int (sb-vm::current-thread-offset-sap i))
            repeat 30 ; arb
            do (assert (zerop word))))))

;;; Test pass-through returns

(declaim (ftype function g h))

(defun test-passthru-return (form)
  (let ((vops
         (ctu:ir2-vops `(lambda (x) ,form))))
    (assert (not (find 'sb-c:return-multiple vops))))
  ;; negative test
  (let ((vops
         (ctu:ir2-vops
          `(lambda (x) (multiple-value-prog1 ,form (h x))))))
    (assert (find 'sb-c:return-multiple vops))))

(with-test (:name :pass-through-dx-return)
  (test-passthru-return '(sb-int:dx-let ((y (cons x x))) (g y))))

(with-test (:name :pass-through-special-unbind)
  (test-passthru-return '(let ((*print-base* 10)) (g x))))

(defun passthru-callee-normal (n)
  (values-list (loop for i from 1 to n collect i)))
(defun passthru-callee-throw (tag n)
  (throw tag (values-list (loop for i from 1 to n collect i))))

;;; It's not as easy to assert that return-multiple doesn't occur on the
;;; normal path of CATCH - which it doesn't - because the vop is present
;;; for the nonlocal path. When thrown through, the value are on the stack.
;;; So this tests the functionality, but doesn't verify the passthru logic.
(with-test (:name :pass-through-catch-return)
  (let ((fun-normal (checked-compile
                     `(lambda (n)
                        (catch 'pt-tag
                          (passthru-callee-normal n)))))
        (fun-throw (checked-compile
                    `(lambda (n)
                       (catch 'pt-tag
                         (passthru-callee-throw 'pt-tag n))))))
    (dotimes (n cl:multiple-values-limit)
      (let ((expected (loop for i from 1 to n collect i))
            (actual-norm (multiple-value-list (funcall fun-normal n)))
            (actual-throw (multiple-value-list (funcall fun-throw n))))
        (assert (equal actual-norm expected))
        (assert (equal actual-throw expected))))))
