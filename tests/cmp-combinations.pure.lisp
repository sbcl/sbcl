;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(with-test (:name :ranges)
  (let* ((ops '(< > <= >=))
         (ranges (list 0 1 -1 -2 2 most-positive-fixnum most-negative-fixnum))
         (ns (append ranges
                     (list most-positive-double-float most-negative-double-float
                           least-positive-double-float least-negative-double-float
                           most-positive-single-float most-negative-single-float
                           least-positive-single-float least-negative-single-float
                           0.0 -0.0 0d0 -0.0
                           1.0 -1.0 0.5 -0.5 1.2 -1.2
                           1.0d0 -1.0d0 0.5d0 -0.5d0 1.2d0 -1.2d0
                           1/2 -1/2
                           4/3 -4/3
                           (expt 2 300)
                           (- (expt 2 300))
                           (/ (expt 2 300))
                           (/ (- (expt 2 300)))
                           (/ 3 (expt 2 300))
                           (/ 3 (- (expt 2 300)))
                           (1- (expt 2 sb-vm:n-word-bits))
                           (1- (expt 2 (1- sb-vm:n-word-bits)))
                           (- (expt 2 (1- sb-vm:n-word-bits))))))
         #+(and slow sb-thread)
         threads)
    (macrolet ((thread (&body body)
                 (or #+(and slow sb-thread)
                     `(push (sb-thread:make-thread (lambda () ,@body))
                            threads)
                     `(progn ,@body))))
      (loop
        for logical in '(and #+slow or)
        do
        (loop
          for op1 in ops
          do
          (let ((op1 op1)
                (logical logical))
            (thread
             (loop
               for op1-not in '(progn #+slow not)
               do
               (loop
                 for op2 in ops
                 do
                 (loop
                   for op2-not in '(progn #+slow not)
                   do
                   (loop
                     for l in (list* 'l ranges)
                     do
                     (loop
                       for h in (list* 'h ranges)
                       do
                       (loop
                         for args1 in (list (list l 'n)
                                            #+slow (list 'n l))
                         do
                         (loop
                           for args2 in (list (list h 'n)
                                              #+slow (list 'n h))
                           do
                           (loop
                             for type in `(t #+slow ,@'(integer fixnum unsigned-byte (and fixnum unsigned-byte)
                                                        sb-vm:signed-word
                                                        sb-vm:word))
                             do
                             (loop
                               for l-type in (if (integerp l)
                                                 '(t)
                                                 `(fixnum #+slow ,@'((and unsigned-byte fixnum)
                                                                     (and (integer * -1) fixnum)
                                                                     sb-vm:signed-word
                                                                     sb-vm:word)))
                               do
                               (loop
                                 for h-type in (if (integerp h)
                                                   '(t)
                                                   `(fixnum #+slow ,@'((and unsigned-byte fixnum)
                                                                       (and (integer * -1) fixnum)
                                                                       sb-vm:signed-word
                                                                       sb-vm:word)))
                                 do
                                 (let* ((form `(lambda (l n h)
                                                 (declare (,l-type l)
                                                          (,h-type h)
                                                          (,type n)
                                                          (ignorable l h))
                                                 (,logical
                                                  (,op1-not (,op1 ,@args1))
                                                  (,op2-not (,op2 ,@args2)))))
                                        (fun1 (checked-compile form))
                                        (fun2 (checked-compile
                                               `(lambda (l n h)
                                                  (declare (ignorable l h)
                                                           (notinline ,@ops))
                                                  (,logical
                                                   (,op1-not (,op1 ,@args1))
                                                   (,op2-not (,op2 ,@args2)))))))
                                   (loop
                                     for l in ranges
                                     when (typep l l-type)
                                     do
                                     (loop
                                       for h in ranges
                                       when (typep h h-type)
                                       do
                                       (loop
                                         for n in ns
                                         when (typep n type)
                                         do
                                         (unless (eql (funcall fun1 l n h)
                                                      (funcall fun2 l n h))
                                           (error "~a" (list form
                                                             l n h))))))))))))))))))))))
    #+(and slow sb-thread)
    (mapc #'sb-thread:join-thread threads)))

(with-test (:name :ranges-to-fixnump)
  (let* ((ops '(< > <= >=))
         (ranges (list 0 1 -1 -2 2 most-positive-fixnum most-negative-fixnum
                       (1- (expt 2 sb-vm:n-word-bits))
                       (1- (expt 2 (1- sb-vm:n-word-bits)))
                       (- (expt 2 (1- sb-vm:n-word-bits)))))
         (ranges (or (append ranges
                             (loop for i in '(1 -1 -2 2)
                                   collect (+ most-positive-fixnum i)
                                   collect (+ most-negative-fixnum i)))
                     ranges))
         (ns (append ranges
                     (list 20 -20
                           (expt 2 300)
                           (- (expt 2 300))
                           (1- (expt 2 sb-vm:n-word-bits))
                           (1- (expt 2 (1- sb-vm:n-word-bits)))
                           (- (expt 2 (1- sb-vm:n-word-bits)))))))
    (loop
      for op1 in ops
      do
      (loop
        for op2 in ops
        do
        (loop
          for l in ranges
          do
          (loop
            for h in ranges
            do
            (loop
              for args1 in (list (list l 'n)
                                 #+slow (list 'n l))
              do
              (loop
                for args2 in (list (list h 'n)
                                   #+slow (list 'n h))
                do
                (let* ((form `(lambda (n)
                                (declare (integer n))
                                (and
                                 (,op1 ,@args1)
                                 (,op2 ,@args2))))
                       (fun1 (checked-compile form))
                       (fun2 (checked-compile
                              `(lambda (n)
                                 (declare (notinline ,@ops))
                                 (and
                                  (,op1 ,@args1)
                                  (,op2 ,@args2))))))
                  (loop
                    for n in ns
                    do
                    (unless (eql (funcall fun1 n)
                                 (funcall fun2 n))
                      (error "~a" (list form n)))))))))))))

(with-test (:name :cmp-constant-fraction)
  (let* ((ops '(< > <= >=))
         (rational (list 0 1 -1 -2 2 most-positive-fixnum most-negative-fixnum
                         1/2 -1/2
                         4/3 -4/3
                         (expt 2 300)
                         (- (expt 2 300))
                         (/ (expt 2 300))
                         (/ (- (expt 2 300)))
                         (/ 3 (expt 2 300))
                         (/ 3 (- (expt 2 300)))
                         (1- (expt 2 sb-vm:n-word-bits))
                         (1- (expt 2 (1- sb-vm:n-word-bits)))
                         (- (expt 2 (1- sb-vm:n-word-bits)))))
         (constants (list* most-positive-double-float most-negative-double-float
                           least-positive-double-float least-negative-double-float
                           most-positive-single-float most-negative-single-float
                           least-positive-single-float least-negative-single-float
                           0.0 -0.0 0d0 -0.0
                           1.0 -1.0 0.5 -0.5 1.2 -1.2
                           1.0d0 -1.0d0 0.5d0 -0.5d0 1.2d0 -1.2d0 rational)))
    (loop for op in ops
          do
          (loop for constant in constants
                do
                (loop for type in '(integer rational)
                      do
                      (let* ((form `(lambda (x)
                                      (declare (,type x))
                                      (,op x ,constant)))
                             (fun (checked-compile form)))
                        (loop for arg in rational
                              when (typep arg type)
                              do
                              (unless (eq (funcall fun arg)
                                          (funcall op arg constant))
                                (error "~a" (list form arg))))))))))
