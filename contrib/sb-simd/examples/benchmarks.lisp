;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Benchmarking code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(asdf:load-system :sb-simd)
(ql:quickload :alexandria :silent t)
(use-package :sb-simd-avx2)

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

(declaim (inline f64.4-hsum f32.8-hsum f64.2-hsum f32.4-hsum))
(defun f64.4-hsum (v)
  (declare (type f64.4 v) (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-call #'+ (f64.4-values v)))

(defun f32.8-hsum (v)
  (declare (type f32.8 v) (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-call #'+ (f32.8-values v)))

(defun f64.2-hsum (v)
  (declare (type f64.2 v) (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-call #'+ (f64.2-values v)))

(defun f32.4-hsum (v)
  (declare (type f32.4 v) (optimize (speed 3) (safety 0) (debug 0)))
  (multiple-value-call #'+ (f32.4-values v)))


(declaim (inline f64.4-vsum f32.8-vsum))
(defun f64.4-vsum (array &aux (n (length array)))
  (declare (type f64vec array) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((n0 (- n (mod n 16)))) 
    (+ (loop with a1 of-type f64.4 = (f64.4 0)
             with a2 of-type f64.4 = (f64.4 0)
             with a3 of-type f64.4 = (f64.4 0)
             with a4 of-type f64.4 = (f64.4 0)
             for i of-type fixnum below n0 by 16
             do (setf a1 (f64.4+ a1 (f64.4-aref array (+ i 0)))
                      a2 (f64.4+ a2 (f64.4-aref array (+ i 4)))
                      a3 (f64.4+ a3 (f64.4-aref array (+ i 8)))
                      a4 (f64.4+ a4 (f64.4-aref array (+ i 12))))
             finally (return (f64.4-hsum (f64.4+ a1 a2 a3 a4))))
       (loop for i of-type fixnum from n0 below n
             summing (aref array i) into s of-type double-float
             finally (return s)))))

(defun f32.8-vsum (array &aux (n (length array)))
  (declare (type f32vec array) (optimize (speed 3) (safety 0) (debug 0)))
  (let ((n0 (- n (mod n 32)))) 
    (+ (loop with a1 of-type f32.8 = (f32.8 0)
             with a2 of-type f32.8 = (f32.8 0)
             with a3 of-type f32.8 = (f32.8 0)
             with a4 of-type f32.8 = (f32.8 0)
             for i of-type fixnum below n0 by 32
             do (setf a1 (f32.8+ a1 (f32.8-aref array (+ i 0)))
                      a2 (f32.8+ a2 (f32.8-aref array (+ i 8)))
                      a3 (f32.8+ a3 (f32.8-aref array (+ i 16)))
                      a4 (f32.8+ a4 (f32.8-aref array (+ i 24))))
             finally (return (f32.8-hsum (f32.8+ a1 a2 a3 a4))))
       (loop for i of-type fixnum from n0 below n
             summing (aref array i) into s of-type single-float
             finally (return s)))))

(declaim (inline f64.4-vdot f32.8-vdot f64.2-vdot f32.4-vdot))

(defun f64.4-vdot
    (array1 array2 &aux (n (min (length array1) (length array2))))
  (declare (type f64vec array1 array2)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (let ((n0 (- n (mod n 16)))) 
    (+ (loop with a1 of-type f64.4 = (f64.4 0)
             with a2 of-type f64.4 = (f64.4 0)
             with a3 of-type f64.4 = (f64.4 0)
             with a4 of-type f64.4 = (f64.4 0)
             for i of-type fixnum below n0 by 16
             do (setf a1 (f64.4+ a1 (f64.4* (f64.4-aref array1 (+ i 0))
					    (f64.4-aref array2 (+ i 0))))
                      a2 (f64.4+ a2 (f64.4* (f64.4-aref array1 (+ i 4))
					    (f64.4-aref array2 (+ i 4))))
                      a3 (f64.4+ a3 (f64.4* (f64.4-aref array1 (+ i 8))
					    (f64.4-aref array2 (+ i 8))))
                      a4 (f64.4+ a4 (f64.4* (f64.4-aref array1 (+ i 12))
					    (f64.4-aref array2 (+ i 12)))))
             finally (return (f64.4-hsum (f64.4+ a1 a2 a3 a4))))
       (loop for i of-type fixnum from n0 below n
             summing (* (aref array1 i)
			(aref array2 i))
	       into s of-type double-float
             finally (return s)))))

(defun f32.8-vdot
    (array1 array2 &aux (n (min (length array1) (length array2))))
  (declare (type f32vec array1 array2)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (let ((n0 (- n (mod n 32)))) 
    (+ (loop with a1 of-type f32.8 = (f32.8 0)
             with a2 of-type f32.8 = (f32.8 0)
             with a3 of-type f32.8 = (f32.8 0)
             with a4 of-type f32.8 = (f32.8 0)
             for i of-type fixnum below n0 by 32
             do (setf a1 (f32.8+ a1 (f32.8* (f32.8-aref array1 (+ i 0))
					    (f32.8-aref array2 (+ i 0))))
                      a2 (f32.8+ a2 (f32.8* (f32.8-aref array1 (+ i 8))
					    (f32.8-aref array2 (+ i 8))))
                      a3 (f32.8+ a3 (f32.8* (f32.8-aref array1 (+ i 16))
					    (f32.8-aref array2 (+ i 16))))
                      a4 (f32.8+ a4 (f32.8* (f32.8-aref array1 (+ i 24))
					    (f32.8-aref array2 (+ i 24)))))
             finally (return (f32.8-hsum (f32.8+ a1 a2 a3 a4))))
       (loop for i of-type fixnum from n0 below n
             summing (* (aref array1 i)
			(aref array2 i))
	       into s of-type single-float
             finally (return s)))))

(defun f64.2-vdot
    (array1 array2 &aux (n (min (length array1) (length array2))))
  (declare (type f64vec array1 array2)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (let ((n0 (- n (mod n 8))))
    (+ (loop with a1 of-type f64.2 = (f64.2 0)
             with a2 of-type f64.2 = (f64.2 0)
             with a3 of-type f64.2 = (f64.2 0)
             with a4 of-type f64.2 = (f64.2 0)
             for i of-type fixnum below n0 by 8
             do (setf a1 (f64.2+ a1 (f64.2* (f64.2-aref array1 (+ i 0))
					    (f64.2-aref array2 (+ i 0))))
                      a2 (f64.2+ a2 (f64.2* (f64.2-aref array1 (+ i 2))
					    (f64.2-aref array2 (+ i 2))))
                      a3 (f64.2+ a3 (f64.2* (f64.2-aref array1 (+ i 4))
					    (f64.2-aref array2 (+ i 4))))
                      a4 (f64.2+ a4 (f64.2* (f64.2-aref array1 (+ i 6))
					    (f64.2-aref array2 (+ i 6)))))
             finally (return (f64.2-hsum (f64.2+ a1 a2 a3 a4))))
       (loop for i of-type fixnum from n0 below n
             summing (* (aref array1 i)
			(aref array2 i))
	       into s of-type double-float
             finally (return s)))))

(defun f32.4-vdot
    (array1 array2 &aux (n (min (length array1) (length array2))))
  (declare (type f32vec array1 array2)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (let ((n0 (- n (mod n 16))))
    (+ (loop with a1 of-type f32.4 = (f32.4 0)
             with a2 of-type f32.4 = (f32.4 0)
             with a3 of-type f32.4 = (f32.4 0)
             with a4 of-type f32.4 = (f32.4 0)
             for i of-type fixnum below n0 by 16
             do (setf a1 (f32.4+ a1 (f32.4* (f32.4-aref array1 (+ i 0))
					    (f32.4-aref array2 (+ i 0))))
                      a2 (f32.4+ a2 (f32.4* (f32.4-aref array1 (+ i 4))
					    (f32.4-aref array2 (+ i 4))))
                      a3 (f32.4+ a3 (f32.4* (f32.4-aref array1 (+ i 8))
					    (f32.4-aref array2 (+ i 8))))
                      a4 (f32.4+ a4 (f32.4* (f32.4-aref array1 (+ i 12))
					    (f32.4-aref array2 (+ i 12)))))
             finally (return (f32.4-hsum (f32.4+ a1 a2 a3 a4))))
       (loop for i of-type fixnum from n0 below n
             summing (* (aref array1 i)
			(aref array2 i))
	       into s of-type single-float
             finally (return s)))))


(declaim (inline f64.4-fma-vdot))
(defun f64.4-fma-vdot
    (array1 array2 &aux (n (min (length array1) (length array2))))
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type f64vec array1 array2))
  (let ((n0 (- n (mod n 16))))
    (+ (loop with a1 of-type f64.4 = (f64.4 0)
             with a2 of-type f64.4 = (f64.4 0)
             with a3 of-type f64.4 = (f64.4 0)
             with a4 of-type f64.4 = (f64.4 0)
             for i of-type fixnum below n0 by 16
             do (let ((v1-0  (f64.4-aref array1 (+ i 0)))
                      (v1-4  (f64.4-aref array1 (+ i 4)))
                      (v1-8  (f64.4-aref array1 (+ i 8)))
                      (v1-12 (f64.4-aref array1 (+ i 12))))
                  (setf a1 (sb-simd-fma:f64.4-fmadd
			    v1-0  (f64.4-aref array2 (+ i 0))  a1)
                        a2 (sb-simd-fma:f64.4-fmadd
			    v1-4  (f64.4-aref array2 (+ i 4))  a2)
                        a3 (sb-simd-fma:f64.4-fmadd
			    v1-8  (f64.4-aref array2 (+ i 8))  a3)
                        a4 (sb-simd-fma:f64.4-fmadd
			    v1-12 (f64.4-aref array2 (+ i 12)) a4)))
             finally (return (f64.4-hsum (f64.4+ a1 a2 a3 a4))))
       (loop for i of-type fixnum from n0 below n
             summing (* (aref array1 i) (aref array2 i))
               into sum of-type double-float
             finally (return sum)))))


(defun benchmark-f64.4-vdot-double (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (loop for len in v-lengths
        do (format t "Doing f64.4-vdot of two ~A long double float vectors 1e6 times~%" len)
        collect
	(let ((u (make-array len :element-type 'double-float
				 :initial-contents
				 (mapcar (lambda (i) (+ i 0.1d0))
					 (alexandria:iota len))))
              (v (make-array len :element-type 'double-float
				 :initial-contents
				 (mapcar (lambda (i) (+ i 0.2d0))
					 (alexandria:iota len)))))
          (declare (type f64vec u v))
          (time-total 1e6 (f64.4-vdot u v)))))

(defun benchmark-f64.4-fma-vdot-double (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (loop for len in v-lengths
        do (format t "Doing f64.4-fma-vdot of two ~A long double float vectors 1e6 times~%" len)
        collect
	(let ((u (make-array len :element-type 'double-float
				 :initial-contents
				 (mapcar (lambda (i) (+ i 0.1d0))
					 (alexandria:iota len))))
              (v (make-array len :element-type 'double-float
				 :initial-contents
				 (mapcar (lambda (i) (+ i 0.2d0))
					 (alexandria:iota len)))))
          (declare (type f64vec u v))
          (time-total 1e6 (f64.4-fma-vdot u v)))))

(defun benchmark-avx-single (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (loop for len in v-lengths
        do (format t "Doing f32.8-vdot of two ~A long single float vectors 1e6 times~%" len)
        collect
	(let ((u (make-array len :element-type 'single-float
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
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (loop for len in v-lengths
        do (format t "Doing f64.2-vdot of two ~A long double float vectors 1e6 times~%" len)
        collect
	(let ((u (make-array len :element-type 'double-float
				 :initial-contents
				 (mapcar (lambda (i) (+ i 0.1d0))
					 (alexandria:iota len))))
              (v (make-array len :element-type 'double-float
				 :initial-contents
				 (mapcar (lambda (i) (+ i 0.2d0))
					 (alexandria:iota len)))))
          (declare (type f64vec u v)) 
          (time-total 1e6 (f64.2-vdot u v)))))

(defun benchmark-sse-single (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (loop for len in v-lengths
        do (format t "Doing f32.4-vdot of two ~A long single float vectors 1e6 times~%" len)
        collect
	(let ((u (make-array len :element-type 'single-float
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
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (loop for len in v-lengths
        do (format t "Doing f64.4-vsum of a ~A long double float vector 1e6 times~%" len)
        collect
	(let ((u (make-array len :element-type 'double-float
				 :initial-contents
				 (mapcar (lambda (i) (+ i 0.1d0))
					 (alexandria:iota len)))))
          (declare (type f64vec u))
          (time-total 1e6 (f64.4-vsum u)))))

(defun benchmark-vsum-single (&rest v-lengths)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (loop for len in v-lengths
        do (format t "Doing f32.8-vsum of a ~A long single float vector 1e6 times~%" len)
        collect
	(let ((u (make-array len :element-type 'single-float
				 :initial-contents
				 (mapcar (lambda (i) (+ i 0.1f0))
					 (alexandria:iota len)))))
          (declare (type f32vec u))
          (time-total 1e6 (f32.8-vsum u)))))


(defun run-all-benchmarks (&rest v-lengths)
  (format t "~%==========================================================~%")
  (format t "====== AVX2 Double Float VDOT (f64.4) ======~%")
  (apply #'benchmark-f64.4-vdot-double v-lengths)
  
  (format t "~%==========================================================~%")
  (format t "====== FMA Double Float VDOT (f64.4) ======~%")
  (apply #'benchmark-f64.4-fma-vdot-double v-lengths)
  
  (format t "~%==========================================================~%")
  (format t "====== AVX2 Single Float VDOT (f32.8) ======~%")
  (apply #'benchmark-avx-single v-lengths)
  
  (format t "~%==========================================================~%")
  (format t "====== SSE Double Float VDOT (f64.2) ======~%")
  (apply #'benchmark-sse-double v-lengths)
  
  (format t "~%==========================================================~%")
  (format t "====== SSE Single Float VDOT (f32.4) ======~%")
  (apply #'benchmark-sse-single v-lengths)
  
  (format t "~%==========================================================~%")
  (format t "====== AVX2 Double Float VSUM (f64.4) ======~%")
  (apply #'benchmark-vsum-double v-lengths)
  
  (format t "~%==========================================================~%")
  (format t "====== AVX2 Single Float VSUM (f32.8) ======~%")
  (apply #'benchmark-vsum-single v-lengths)
  
  (values))

#+(or)
(apply #'run-all-benchmarks '(10 100 200 400 800 1200 2400 4800 9600))
