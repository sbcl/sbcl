(defpackage :sb-gmp-bench (:use "COMMON-LISP"))

(in-package :sb-gmp-bench)

(defparameter *stream* t)

(defparameter *state* nil)

(defun bench-+ () ; limbs: never
  (macrolet ((tstfun (f a b)
               `(lambda ()
                  (loop for i in ,a
                        for j in ,b
                        collect (,f i j)))))
    (loop for limbs fixnum from 2
          with gmp-win = 0
          until (or (= gmp-win 5)
                    (= limbs 78)) ; > 78 exhausts default heap size
          do
             (loop
               for i below 100000
               collect (sb-gmp::bassert (sb-gmp:random-bitcount *state* (* limbs sb-vm:n-word-bits)))
                 into list-a
               collect (sb-gmp::bassert (sb-gmp:random-bitcount *state* (* limbs sb-vm:n-word-bits)))
                 into list-b
               finally
                  (let (time1 time2)
                    (let ((r-sbcl (progn (sb-ext:gc)
                                         (sb-ext:call-with-timing
                                          (lambda (&rest plist)
                                            (setf time1 plist))
                                          (tstfun + list-a list-b))))
                          (r-gmp (progn (sb-ext:gc)
                                        (sb-ext:call-with-timing
                                         (lambda (&rest plist)
                                           (setf time2 plist))
                                         (tstfun sb-gmp:mpz-add list-a list-b)))))
                      (format *stream* "limbs: ~s~%Time SBCL: ~s~%Time GMP:  ~s~%"
                              limbs time1 time2)
                      (when (< (getf time2 :PROCESSOR-CYCLES)
                               (getf time1 :PROCESSOR-CYCLES))
                        (incf gmp-win))
                      (if (= (length r-sbcl) (length r-gmp))
                          (format *stream* "Test PASSED~2%")
                          (format *stream* "Test FAILED~2%"))))))))


(defun bench-* () ; limbs 6
  (macrolet ((tstfun (f a b)
               `(lambda ()
                  (loop for i in ,a
                        for j in ,b
                        collect (,f i j)))))
    (loop for limbs fixnum from 2
          with gmp-win = 0
          until (or (= gmp-win 3)
                    (= limbs 100))
          do
             (loop
               for i below 10000
               collect (sb-gmp::bassert (sb-gmp:random-bitcount *state* (* limbs sb-vm:n-word-bits)))
                 into list-a
               collect (sb-gmp::bassert (sb-gmp:random-bitcount *state* (* limbs sb-vm:n-word-bits)))
                 into list-b
               finally
                  (let (time1 time2)
                    (let ((r-sbcl (progn (sb-ext:gc)
                                         (sb-ext:call-with-timing
                                          (lambda (&rest plist)
                                            (setf time1 plist))
                                          (tstfun * list-a list-b))))
                          (r-gmp (progn (sb-ext:gc)
                                        (sb-ext:call-with-timing
                                         (lambda (&rest plist)
                                           (setf time2 plist))
                                         (tstfun sb-gmp:mpz-mul list-a list-b)))))
                      (format *stream* "limbs: ~s~%Time SBCL: ~s~%Time GMP:  ~s~%"
                              limbs time1 time2)
                      (when (< (getf time2 :PROCESSOR-CYCLES)
                               (getf time1 :PROCESSOR-CYCLES))
                        (incf gmp-win))
                      (if (= (length r-sbcl) (length r-gmp))
                          (format *stream* "Test PASSED~2%")
                          (format *stream* "Test FAILED~2%"))))))))

(defun bench-/ () ; limbs 3 / 2
  (macrolet ((tstfun (f a b)
               `(lambda ()
                  (loop for i in ,a
                        for j in ,b
                        collect (,f i j)))))
    (loop for limbs fixnum from 2
          for limbs_b fixnum from 1
          with gmp-win = 0
          until (or (= gmp-win 3)
                    (= limbs 100))
          do
             (loop
               for i below 100
               collect (sb-gmp::bassert (sb-gmp:random-bitcount *state* (* limbs sb-vm:n-word-bits)))
                 into list-a
               collect (sb-gmp::bassert (sb-gmp:random-bitcount *state* (* limbs_b sb-vm:n-word-bits)))
                 into list-b
               finally
                  (let (time1 time2)
                    (format t "bench it~%")
                    (let ((r-sbcl (progn (sb-ext:gc)
                                         (sb-ext:call-with-timing
                                          (lambda (&rest plist)
                                            (setf time1 plist))
                                          (tstfun truncate list-a list-b))))
                          (r-gmp (progn (sb-ext:gc)
                                        (sb-ext:call-with-timing
                                         (lambda (&rest plist)
                                           (setf time2 plist))
                                         (tstfun sb-gmp:mpz-tdiv list-a list-b)))))
                      (format *stream* "limbs: ~s~%Time SBCL: ~s~%Time GMP:  ~s~%"
                              limbs time1 time2)
                      (when (< (getf time2 :PROCESSOR-CYCLES)
                               (getf time1 :PROCESSOR-CYCLES))
                        (incf gmp-win))
                      (if (= (length r-sbcl) (length r-gmp))
                          (format *stream* "Test PASSED~2%")
                          (format *stream* "Test FAILED~2%"))))))))

(defun bench-gcd () ; limbs: always
  (macrolet ((tstfun (f a b)
               `(lambda ()
                  (loop for i in ,a
                        for j in ,b
                        collect (,f i j)))))
    (loop for limbs fixnum from 2
          for limbs_b fixnum from 1
          with gmp-win = 0
          until (or (= gmp-win 3)
                    (= limbs 100))
          do
             (loop
               for i below 100
               collect (sb-gmp::bassert (sb-gmp:random-bitcount *state* (* limbs sb-vm:n-word-bits)))
                 into list-a
               collect (sb-gmp::bassert (sb-gmp:random-bitcount *state* (* limbs_b sb-vm:n-word-bits)))
                 into list-b
               finally
                  (let (time1 time2)
                    (format t "bench it~%")
                    (let ((r-sbcl (progn (sb-ext:gc)
                                         (sb-ext:call-with-timing
                                          (lambda (&rest plist)
                                            (setf time1 plist))
                                          (tstfun gcd list-a list-b))))
                          (r-gmp (progn (sb-ext:gc)
                                        (sb-ext:call-with-timing
                                         (lambda (&rest plist)
                                           (setf time2 plist))
                                         (tstfun sb-gmp:mpz-gcd list-a list-b)))))
                      (format *stream* "limbs: ~s~%Time SBCL: ~s~%Time GMP:  ~s~%"
                              limbs time1 time2)
                      (when (< (getf time2 :PROCESSOR-CYCLES)
                               (getf time1 :PROCESSOR-CYCLES))
                        (incf gmp-win))
                      (if (= (length r-sbcl) (length r-gmp))
                          (format *stream* "Test PASSED~2%")
                          (format *stream* "Test FAILED~2%"))))))))

(defun bench-isqrt () ; limbs: always
  (macrolet ((tstfun (f a)
               `(lambda ()
                  (loop for i in ,a
                        collect (,f i)))))
    (loop for limbs fixnum from 1
          with gmp-win = 0
          until (or (= gmp-win 3)
                    (= limbs 100))
          do
             (loop
               for i below 100
               collect (sb-gmp::bassert (sb-gmp:random-bitcount *state* (* limbs sb-vm:n-word-bits)))
                 into list-a
               finally
                  (let (time1 time2)
                    (format t "bench it~%")
                    (let ((r-sbcl (progn (sb-ext:gc)
                                         (sb-ext:call-with-timing
                                          (lambda (&rest plist)
                                            (setf time1 plist))
                                          (tstfun isqrt list-a))))
                          (r-gmp (progn (sb-ext:gc)
                                        (sb-ext:call-with-timing
                                         (lambda (&rest plist)
                                           (setf time2 plist))
                                         (tstfun sb-gmp:mpz-sqrt list-a)))))
                      (format *stream* "limbs: ~s~%Time SBCL: ~s~%Time GMP:  ~s~%"
                              limbs time1 time2)
                      (when (< (getf time2 :PROCESSOR-CYCLES)
                               (getf time1 :PROCESSOR-CYCLES))
                        (incf gmp-win))
                      (if (= (length r-sbcl) (length r-gmp))
                          (format *stream* "Test PASSED~2%")
                          (format *stream* "Test FAILED~2%"))))))))

(defun bench-q+ () ; limbs: always
  (macrolet ((tstfun (f a b)
               `(lambda ()
                  (loop for i in ,a
                        for j in ,b
                        collect (,f i j)))))
    (loop for limbsa fixnum from 2
          for limbsb fixnum from 1
          with gmp-win = 0
          until (or (= gmp-win 5)
                    (= limbsa 50))
          do
             (loop
               for i below 10000
               collect (/ (sb-gmp:random-bitcount *state* (* limbsa sb-vm:n-word-bits))
                          (sb-gmp:random-bitcount *state* (* limbsb sb-vm:n-word-bits)))
                 into list-a
               collect (/ (sb-gmp:random-bitcount *state* (* limbsa sb-vm:n-word-bits))
                          (sb-gmp:random-bitcount *state* (* limbsb sb-vm:n-word-bits)))
                 into list-b
               finally
                  (let (time1 time2)
                    (let ((r-sbcl (progn (sb-ext:gc)
                                         (sb-ext:call-with-timing
                                          (lambda (&rest plist)
                                            (setf time1 plist))
                                          (tstfun + list-a list-b))))
                          (r-gmp (progn (sb-ext:gc)
                                        (sb-ext:call-with-timing
                                         (lambda (&rest plist)
                                           (setf time2 plist))
                                         (tstfun sb-gmp:mpq-add list-a list-b)))))
                      (format *stream* "limbs: ~s~%Time SBCL: ~s~%Time GMP:  ~s~%"
                              limbsa time1 time2)
                      (when (< (getf time2 :PROCESSOR-CYCLES)
                               (getf time1 :PROCESSOR-CYCLES))
                        (incf gmp-win))
                      (if (= (length r-sbcl) (length r-gmp))
                          (format *stream* "Test PASSED~2%")
                          (format *stream* "Test FAILED~2%"))))))))

(defun bench-q* () ; limbs: always
  (macrolet ((tstfun (f a b)
               `(lambda ()
                  (loop for i in ,a
                        for j in ,b
                        collect (,f i j)))))
    (loop for limbsa fixnum from 2
          for limbsb fixnum from 1
          with gmp-win = 0
          until (or (= gmp-win 5)
                    (= limbsa 50))
          do
             (loop
               for i below 10000
               collect (/ (sb-gmp:random-bitcount *state* (* limbsa sb-vm:n-word-bits))
                          (sb-gmp:random-bitcount *state* (* limbsb sb-vm:n-word-bits)))
                 into list-a
               collect (/ (sb-gmp:random-bitcount *state* (* limbsa sb-vm:n-word-bits))
                          (sb-gmp:random-bitcount *state* (* limbsb sb-vm:n-word-bits)))
                 into list-b
               finally
                  (let (time1 time2)
                    (let ((r-sbcl (progn (sb-ext:gc)
                                         (sb-ext:call-with-timing
                                          (lambda (&rest plist)
                                            (setf time1 plist))
                                          (tstfun * list-a list-b))))
                          (r-gmp (progn (sb-ext:gc)
                                        (sb-ext:call-with-timing
                                         (lambda (&rest plist)
                                           (setf time2 plist))
                                         (tstfun sb-gmp:mpq-mul list-a list-b)))))
                      (format *stream* "limbs: ~s~%Time SBCL: ~s~%Time GMP:  ~s~%"
                              limbsa time1 time2)
                      (when (< (getf time2 :PROCESSOR-CYCLES)
                               (getf time1 :PROCESSOR-CYCLES))
                        (incf gmp-win))
                      (if (= (length r-sbcl) (length r-gmp))
                          (format *stream* "Test PASSED~2%")
                          (format *stream* "Test FAILED~2%"))))))))

(defun bench ()
  (let ((*state* (sb-gmp:make-gmp-rstate)))
    (sb-gmp:rand-seed *state* 1234)
    (bench-q*)))
