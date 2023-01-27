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

#+slow(push :slow *features*) ;; takes more than 30 minutes

(with-test (:name :ranges)
  (let* ((ops '(< > <= >=))
         (ranges (list 0 1 -1 -2 2 most-positive-fixnum most-negative-fixnum 100 -100))
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
                           (/ 3 (- (expt 2 300))))))
         (cache (make-hash-table :test #'equal)))
    (flet ((cached-compile (form)
             (or (gethash form cache)
                 (setf (gethash form cache)
                       (checked-compile form)))))
      (loop
        for not in '(progn #+slow not)
        do
        (loop
          for logical in '(and or)
          do
          (loop
            for op1 in ops
            do
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
                              for l-type in `(fixnum #+slow ,@'((and unsigned-byte fixnum)
                                                                (and (integer * -1) fixnum)
                                                                sb-vm:signed-word
                                                                sb-vm:word))
                              do
                              (loop
                                for h-type in `(fixnum #+slow ,@'((and unsigned-byte fixnum)
                                                                  (and (integer * -1) fixnum)
                                                                  sb-vm:signed-word
                                                                  sb-vm:word))
                                do
                                (let* ((form `(lambda (l n h)
                                                (declare (,l-type l)
                                                         (,h-type h)
                                                         (,type n)
                                                         (ignorable l h))
                                                (,not
                                                 (,logical
                                                  (,op1-not (,op1 ,@args1))
                                                  (,op1-not (,op2 ,@args2))))))
                                       (fun1 (cached-compile form))
                                       (fun2 (cached-compile
                                              `(lambda (l n h)
                                                 (declare (ignorable l h)
                                                          (notinline ,@ops))
                                                 (,not
                                                  (,logical
                                                   (,op1-not (,op1 ,@args1))
                                                   (,op1-not (,op2 ,@args2))))))))
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
                                                            l n h)))))))))))))))))))))))
