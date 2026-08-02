(in-package #:sb-simd-test-suite)

(defmacro define-aref-test (aref element-type simd-width &optional (unpacker 'identity))
  (let ((value-symbols (prefixed-symbols "V" simd-width))
        (zero (coerce 0 element-type))
        (one (coerce 1 element-type)))
    `(define-test ,aref
       ;; Create an array of zeros and successively replace zeros with
       ;; ones.  After each replacement, check whether a load still
       ;; produces the expected result.
       (let ((array (make-array '(,simd-width)
                                 :element-type ',element-type
                                 :initial-element ,zero)))
         (multiple-value-bind ,value-symbols (,unpacker (,aref array 0))
           ,@(loop for value-symbol in value-symbols
                   collect `(is (= ,value-symbol ,zero))))
         (loop for index below ,simd-width do
           (setf (aref array index) ,one)
           (loop for number in (multiple-value-list (,unpacker (,aref array 0)))
                 for position from 0
                 do (if (<= position index)
                        (is (= number ,one))
                        (is (= number ,zero))))))
       ;; Create an array with twice as many elements as the width of the
       ;; SIMD data type, and whose lower half consists of all zeros and
       ;; whose upper half consists of all ones.  Check that all valid
       ;; loads from this array have the expected state.
       (let ((array (make-array '(,(* 2 simd-width))
                                :element-type ',element-type
                                :initial-contents
                                (append (make-list ,simd-width :initial-element ,zero)
                                        (make-list ,simd-width :initial-element ,one)))))
         (loop for index below ,simd-width do
           (multiple-value-bind ,value-symbols
               (,unpacker (,aref array index))
             ,@(loop for value-symbol in value-symbols
                     for position from 0
                     collect `(if (< (+ ,position index) ,simd-width)
                                  (is (= ,value-symbol ,zero))
                                  (is (= ,value-symbol ,one))))))))))
