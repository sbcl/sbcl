;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Benchmarking code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(asdf:load-system :sb-simd)
(ql:quickload :alexandria :silent t)
(use-package :sb-simd-avx)

(defmacro time-total (n &body body)
  "N-average the execution time of BODY in seconds"
  (declare (optimize (speed 0)))
  (alexandria:with-gensyms (start end)
    `(let (,start ,end)
       (sb-ext:gc :full t)
       (setf ,start (get-internal-real-time))
       (loop for i below ,n
             do ,@body)
       (setf ,end (get-internal-real-time))
       (coerce (/ (- ,end ,start) internal-time-units-per-second)
               'float))))

(defun simd-sum (array &aux (n (array-total-size array)))
  (declare (type f64vec array)
           (optimize speed (safety 0)))
  (do ((index 0 (the (integer 0 #.(- array-total-size-limit 16)) (+ index 16)))
       (acc1 (f64.4 0) (f64.4+ acc1 (f64.4-aref array (+ index 0))))
       (acc2 (f64.4 0) (f64.4+ acc2 (f64.4-aref array (+ index 4))))
       (acc3 (f64.4 0) (f64.4+ acc3 (f64.4-aref array (+ index 8))))
       (acc4 (f64.4 0) (f64.4+ acc4 (f64.4-aref array (+ index 12)))))
      ((> index (- n 16))
       (do ((result (multiple-value-call #'+ (f64.4-values (f64.4+ acc1 acc2 acc3 acc4)))
                    (+ result (row-major-aref array index)))
            (index index (1+ index)))
           ((>= index n) result)))))

(defun simd-vdot (array1 array2 &aux (n (min (array-total-size array1) (array-total-size array2))))
  (declare (type f64vec array1 array2)
           (optimize speed (safety 0)))
  (do ((index 0 (the (integer 0 #.(- array-total-size-limit 16)) (+ index 16)))
       (acc1 (f64.4 0) (f64.4-incf acc1 (f64.4* (f64.4-aref array1 (+ index 0))
                                                (f64.4-aref array2 (+ index 0)))))
       (acc2 (f64.4 0) (f64.4-incf acc2 (f64.4* (f64.4-aref array1 (+ index 4))
                                                (f64.4-aref array2 (+ index 4)))))
       (acc3 (f64.4 0) (f64.4-incf acc3 (f64.4* (f64.4-aref array1 (+ index 8))
                                                (f64.4-aref array2 (+ index 8)))))
       (acc4 (f64.4 0) (f64.4-incf acc4 (f64.4* (f64.4-aref array1 (+ index 12))
                                                (f64.4-aref array2 (+ index 12))))))
      ((> index (- n 16))
       (do ((result (multiple-value-call #'+ (f64.4-values (f64.4+ acc1 acc2 acc3 acc4)))
                    (+ result (* (row-major-aref array1 index)
                                 (row-major-aref array2 index))))
            (index index (1+ index)))
           ((>= index n) result)))))

;; For some reasons it floating-point-overflows
(defun fma-vdot (array1 array2 &aux (n (min (array-total-size array1) (array-total-size array2))))
  (declare (type f64vec array1 array2)
           (optimize speed (safety 0)))
  (do ((index 0 (the (integer 0 #.(- array-total-size-limit 16)) (+ index 16)))
       (acc1 (f64.4 0) (setf acc1 (sb-simd-avx2:f64.4-fmadd acc1 (f64.4-aref array1 (+ index 0))
                                                            (f64.4-aref array2 (+ index 0)))))
       (acc2 (f64.4 0) (setf acc2 (sb-simd-avx2:f64.4-fmadd acc2 (f64.4-aref array1 (+ index 4))
                                                            (f64.4-aref array2 (+ index 4)))))
       (acc3 (f64.4 0) (setf acc3 (sb-simd-avx2:f64.4-fmadd acc3 (f64.4-aref array1 (+ index 8))
                                                            (f64.4-aref array2 (+ index 8)))))
       (acc4 (f64.4 0) (setf acc4 (sb-simd-avx2:f64.4-fmadd acc4 (f64.4-aref array1 (+ index 12))
                                                            (f64.4-aref array2 (+ index 12))))))
      ((> index (- n 16))
       (do ((result (f64.4-hsum (f64.4+ acc1 acc2 acc3 acc4))
                    (+ result (* (row-major-aref array1 index)
                                 (row-major-aref array2 index))))
            (index index (1+ index)))
           ((>= index n) result)))))

(declaim (inline f64.4-fma-vdot))
(defun f64.4-fma-vdot (array1 array2 &aux (n (min (array-total-size array1)
                                                  (array-total-size array2))))
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type f64vec array1 array2))
  (let ((n0 (- n (mod n 16))))
    (+ (loop with acc1 of-type f64.4 = (f64.4 0)
             with acc2 of-type f64.4 = (f64.4 0)
             with acc3 of-type f64.4 = (f64.4 0)
             with acc4 of-type f64.4 = (f64.4 0)
             for i of-type fixnum below n0 by 16
             do (let ((array1-i0  (f64.4-aref array1 (+ i 0)))
                      (array1-i4  (f64.4-aref array1 (+ i 4)))
                      (array1-i8  (f64.4-aref array1 (+ i 8)))
                      (array1-i12 (f64.4-aref array1 (+ i 12))))
                  (setf acc1 (sb-simd-avx2:f64.4-fmadd acc1 array1-i0
                                                       (f64.4-aref array2 (+ i 0)))
                        acc2 (sb-simd-avx2:f64.4-fmadd acc2 array1-i4
                                                       (f64.4-aref array2 (+ i 4)))
                        acc3 (sb-simd-avx2:f64.4-fmadd acc3 array1-i8
                                                       (f64.4-aref array2 (+ i 8)))
                        acc4 (sb-simd-avx2:f64.4-fmadd acc4 array1-i12
                                                       (f64.4-aref array2 (+ i 12)))))
             finally (return (f64.4-hsum (f64.4+ acc1 acc2 acc3 acc4))))
       (loop for i of-type fixnum from n0 below n
             summing (* (aref array1 i) (aref array2 i))
               into sum of-type double-float
             finally (return sum)))))

(defun benchmark-f64.4-vdot-double (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (notinline sb-simd-avx2:f64.4-vdot))
  (loop for len in v-lengths
        do (format t "Doing dot product of two ~A long double float vectors 1e6 times~%" len)
        collect (let ((u (make-array len :element-type 'double-float
                                         :initial-contents
                                         (mapcar (lambda (i) (+ i 0.1d0))
                                                 (alexandria:iota len))))
                      (v (make-array len :element-type 'double-float
                                         :initial-contents
                                         (mapcar (lambda (i) (+ i 0.2d0))
                                                 (alexandria:iota len)))))
                  (declare (type f64vec u v))
                  (time-total 1e6 (sb-simd-avx2::f64.4-vdot u v)))))

(defun benchmark-f64.4-vdot2-double (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (notinline sb-simd-avx2:f64.4-vdot2))
  (loop for len in v-lengths
        do (format t "Doing dot product of two ~A long double float vectors 1e6 times~%" len)
        collect (let ((u (make-array len :element-type 'double-float
                                         :initial-contents
                                         (mapcar (lambda (i) (+ i 0.1d0))
                                                 (alexandria:iota len))))
                      (v (make-array len :element-type 'double-float
                                         :initial-contents
                                         (mapcar (lambda (i) (+ i 0.2d0))
                                                 (alexandria:iota len)))))
                  (declare (type f64vec u v))
                  (time-total 1e6 (sb-simd-avx2::f64.4-vdot2 u v)))))

(defun benchmark-f64.4-fma-vdot-double (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (notinline f64.4-fma-vdot))
  (loop for len in v-lengths
        do (format t "Doing dot product of two ~A long double float vectors 1e6 times~%" len)
        collect (let ((u (make-array len :element-type 'double-float
                                         :initial-contents
                                         (mapcar (lambda (i) (+ i 0.1d0))
                                                 (alexandria:iota len))))
                      (v (make-array len :element-type 'double-float
                                         :initial-contents
                                         (mapcar (lambda (i) (+ i 0.2d0))
                                                 (alexandria:iota len)))))
                  (declare (type f64vec u v))
                  (time-total 1e6 (f64.4-fma-vdot u v)))))

(defun benchmark-simd-vdot-double (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (notinline f64.4-vdot))
  (loop for len in v-lengths
        do (format t "Doing dot product of two ~A long double float vectors 1e6 times~%" len)
        collect (let ((u (make-array len :element-type 'double-float
                                         :initial-contents
                                         (mapcar (lambda (i) (+ i 0.1d0))
                                                 (alexandria:iota len))))
                      (v (make-array len :element-type 'double-float
                                         :initial-contents
                                         (mapcar (lambda (i) (+ i 0.2d0))
                                                 (alexandria:iota len)))))
                  (declare (type f64vec u v))
                  (time-total 1e6 (simd-vdot u v)))))

(defun benchmark-fma-vdot-double (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (notinline fma-vdot))
  (loop for len in v-lengths
        do (format t "Doing dot product of two ~A long double float vectors 1e6 times~%" len)
        collect (let ((u (make-array len :element-type 'double-float
                                         :initial-contents
                                         (mapcar (lambda (i) (+ i 0.1d0))
                                                 (alexandria:iota len))))
                      (v (make-array len :element-type 'double-float
                                         :initial-contents
                                         (mapcar (lambda (i) (+ i 0.2d0))
                                                 (alexandria:iota len)))))
                  (declare (type f64vec u v))
                  (time-total 1e6 (fma-vdot u v)))))

(defun benchmark-avx-single (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (notinline f32.8-vdot))
  (loop for len in v-lengths
        do (format t "Doing dot product of two ~A long single float vectors 1e6 times~%" len)
        collect (let ((u (make-array len :element-type 'single-float
                                         :initial-contents
                                         (mapcar (lambda (i) (+ i 0.1f0))
                                                 (alexandria:iota len))))
                      (v (make-array len :element-type 'single-float
                                         :initial-contents
                                         (mapcar (lambda (i) (+ i 0.2f0))
                                                 (alexandria:iota len)))))
                  (declare (type f32vec u v))
                  (time-total 1e6 (f32.8-vdot u v)))))

(defun benchmark-sse-double (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (notinline f64.2-vdot))
  (loop for len in v-lengths
        do (format t "Doing dot product of two ~A long double float vectors 1e6 times~%" len)
        collect (let ((u (make-array len :element-type 'double-float
                                         :initial-contents
                                         (mapcar (lambda (i) (+ i 0.1d0))
                                                 (alexandria:iota len))))
                      (v (make-array len :element-type 'double-float
                                         :initial-contents
                                         (mapcar (lambda (i) (+ i 0.2d0))
                                                 (alexandria:iota len)))))
                  (declare (type f32vec u v))
                  (time-total 1e6 (f64.2-vdot u v)))))

(defun benchmark-sse-single (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (notinline f32.4-vdot))
  (loop for len in v-lengths
        do (format t "Doing dot product of two ~A long single float vectors 1e6 times~%" len)
        collect (let ((u (make-array len :element-type 'single-float
                                         :initial-contents
                                         (mapcar (lambda (i) (+ i 0.1f0))
                                                 (alexandria:iota len))))
                      (v (make-array len :element-type 'single-float
                                         :initial-contents
                                         (mapcar (lambda (i) (+ i 0.2f0))
                                                 (alexandria:iota len)))))
                  (declare (type f32vec u v))
                  (time-total 1e6 (f32.4-vdot u v)))))

(defun benchmark-vsum-double (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (notinline f64.4-vsum))
  (loop for len in v-lengths
        do (format t "Doing sum of a ~A long double float vector 1e6 times~%" len)
        collect (let ((u (make-array len :element-type 'double-float
                                         :initial-contents
                                         (mapcar (lambda (i) (+ i 0.1d0))
                                                 (alexandria:iota len)))))
                  (declare (type f64vec u))
                  (time-total 1e6 (f64.4-vsum u)))))

(defun benchmark-simd-sum-double (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (notinline simd-sum))
  (loop for len in v-lengths
        do (format t "Doing sum of a ~A long double float vector 1e6 times~%" len)
        collect (let ((u (make-array len :element-type 'double-float
                                         :initial-contents
                                         (mapcar (lambda (i) (+ i 0.1d0))
                                                 (alexandria:iota len)))))
                  (declare (type f64vec u))
                  (time-total 1e6 (simd-sum u)))))

(defun benchmark-vsum-single (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (notinline f32.8-vsum))
  (loop for len in v-lengths
        do (format t "Doing sum of a ~A long single float vector 1e6 x~%" len)
        collect (let ((u (make-array len :element-type 'single-float
                                         :initial-contents
                                         (mapcar (lambda (i) (+ i 0.1f0))
                                                 (alexandria:iota len)))))
                  (declare (type f32vec u))
                  (time-total 1e6 (f32.8-vsum u)))))
#+(or)
(benchmark-f64.4-vdot-double 10 100 200 400 800 1200 2400 4800 9600)
#+(or)
(benchmark-f64.4-vdot2-double 10 100 200 400 800 1200 2400 4800 9600)
#+(or)
(benchmark-avx-single 10 100 200 400 800 1200 2400 4800 9600)
#+(or)
(benchmark-sse-double 10 100 200 400 800 1200 2400 4800 9600)
#+(or)
(benchmark-sse-single 10 100 200 400 800 1200 2400 4800 9600)
#+(or)
(benchmark-vsum-double 10 100 200 400 800 1200 2400 4800 9600)
#+(or)
(benchmark-vsum-single 10 100 200 400 800 1200 2400 4800 9600)
#+(or)
(apply 'benchmark-avx-double (loop for i from 1 to 10 collect (* 1000 i)))
