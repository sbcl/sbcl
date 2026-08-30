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

(with-test (:name :mv-area-gc-scrubbing :fails-on :interpreter)
  (let ((f1 (checked-compile `(lambda () (values-list (hairy-result)))))
        (f2 (checked-compile `(lambda () (values-list (list 'a 'b 'c 'd 'e))))))
    (funcall f1)
    (funcall f2)
    (gc :full t)
    (let ((mv-count (get-mv-return-count)))
      (assert (= mv-count 5))
      (loop for i from (+ sb-vm::thread-mv-return-values-slot 2)
            for word = (sb-sys:sap-int (sb-vm::current-thread-offset-sap i))
            repeat 30 ; arb
            do (assert (zerop word))))))
