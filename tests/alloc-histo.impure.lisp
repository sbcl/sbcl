
#-(and x86-64 system-tlabs) (invoke-restart 'run-tests::skip-file)

(defvar *print-histogram* nil)
(defun check-histogram (bin-index count kind)
  (let* ((h (sb-thread:allocator-histogram))
         (bins (first h))
         (ok t))
    (dotimes (i (length bins))
      (setq ok (and ok
                    (if (= i bin-index)
                        (= (aref bins bin-index) count)
                        (zerop (aref bins i))))))
    (when (or (not ok) *print-histogram*)
      (sb-thread:print-allocator-histogram h))
    (unless ok
      (error "Unexpected histogram: expected bin ~D = ~D"
             bin-index count))
    ;; this isn't using the large-object-size vector so we can't assert
    ;; about boxed/unboxed for bin >= 32
    (when (<= bin-index 31)
      (let ((nbytes (* count (* (1+ bin-index) sb-vm:cons-size sb-vm:n-word-bytes))))
        (ecase kind
          (:unboxed (assert (= (third h) nbytes)))
          (:boxed (assert (= (fourth h) nbytes))))))
    h))
(compile 'check-histogram)

(defun compile-with-histogram (lexpr)
  (let ((*features* (cons :allocation-size-histogram *features*)))
    (compile nil lexpr)))

(defvar *arena* (sb-vm:new-arena (* 512 1024 1024))) ; 512 MiB

(defun %assert-histogram (lambda bin-index count kind)
  (sb-thread:reset-allocator-histogram)
  (sb-vm:with-arena (*arena*)
    (funcall lambda))
  (sb-vm:rewind-arena *arena*)
  (check-histogram bin-index count kind))
(compile '%assert-histogram)

(defmacro assert-histogram (form bin-index count kind)
  `(%assert-histogram (compile-with-histogram '(lambda () ,form)) ,bin-index ,count ,kind))

(test-util:with-test (:name :1-cons)
  (assert-histogram (cons 1 2) 0 1 :boxed))

(test-util:with-test (:name :2-cons)
  ;; this counts _allocations_ of a given size, not objects of a given size.
  ;; The latter whould say that it was 2 cons-sized objects, but in fact
  ;; it is counted as 1 4-word object.
  (assert-histogram (list 1 2) 1 1 :boxed))

(test-util:with-test (:name :bit-vector)
  (assert-histogram (make-array 128 :element-type 'bit)
                    1 1 :unboxed))

(setf (symbol-function 'instrumented-make-array-n)
      (compile-with-histogram '(lambda (n) (make-array (the integer n)))))

;; make boxed arrays from 1 to 32 conses in length
(test-util:with-test (:name :boxed-array-small)
  (loop for array-len from 0 to 62 by 2
        for bin-index from 0
        do (let* ((constructor `(make-array ,array-len))
                  (function (compile-with-histogram `(lambda () ,constructor))))
             (assert (= (primitive-object-size (funcall function))
                        (* (+ array-len sb-vm:vector-data-offset)
                           sb-vm:n-word-bytes)))
             (let ((expected-histogram (%assert-histogram function bin-index 1 :boxed)))
               (sb-thread:reset-allocator-histogram)
               (opaque-identity (funcall 'instrumented-make-array-n array-len))
               (let ((histogram (sb-thread:allocator-histogram)))
                 (assert (equalp histogram expected-histogram)))))))

(test-util:with-test (:name :boxed-array-large-bins)
  ;; an array of 64 words exceeds the largest small bin
  (let ((array-len 64))
    (dotimes (trial 300)
      (let* ((constructor `(make-array ,array-len))
             (function (compile-with-histogram `(lambda () ,constructor)))
             (nbytes (primitive-object-size (funcall function)))
             (bin-index
              (+ 32 ; skip over the small bins
                 (integer-length nbytes)
                 -10))) ; first large bin is for 2^10.  Subtract the bias
        (assert (= nbytes
                   (* (+ array-len sb-vm:vector-data-offset)
                      sb-vm:n-word-bytes)))
        (let ((expected-histogram (%assert-histogram function bin-index 1 :boxed)))
          (sb-thread:reset-allocator-histogram)
          (opaque-identity (funcall 'instrumented-make-array-n array-len))
          (let ((histogram (sb-thread:allocator-histogram)))
            ;; (sb-thread:print-allocator-histogram histogram)
            (assert (equalp histogram expected-histogram)))))
      (incf array-len 2))))

(defun make-monster ()
  (let* ((desired-size (* 1024 1024 1024)) ; 1 GiB
         (desired-nwords (/ desired-size sb-vm:n-word-bytes))
         (nelem (- desired-nwords 2)))
    (funcall 'instrumented-make-array-n nelem)))

;(setq *print-histogram* t)
(test-util:with-test (:name :monster)
  (assert-histogram (make-monster)
                    ;; I just printed the histogram and looked at it, and wired in
                    ;; the expected bin. I dont really feel like computing it in the test.
                    53 1 :boxed))
