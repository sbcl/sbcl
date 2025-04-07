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

(enable-test-parallelism)

;;; Can't properly decode errors where the call stack is not pinned.
#-(or arm64 mips ppc64 riscv x86 x86-64) (invoke-restart 'run-tests::skip-file)

(defun test-ops (ops types arguments &key (result-types types) (b-arguments arguments))
  (flet ((normalize-type (type)
           (sb-kernel:type-specifier (sb-kernel:specifier-type type))))
    (let ((types (mapcar #'normalize-type types))
          (result-types (mapcar #'normalize-type result-types))
          (progress 0)
          (cache (make-hash-table :test #'equal)))
      (loop for op in ops
            do
            (loop for a-type in types
                  do
                  (loop for b-type in types
                        do
                        (loop for result-type in result-types
                              do
                              (loop for a in arguments
                                    when (typep a a-type)
                                    do
                                    (loop for b in b-arguments
                                          for result = (funcall op a b)
                                          when (typep b b-type)
                                          do
                                          (loop for lambda in (list `(lambda (a b)
                                                                       (declare (,a-type a)
                                                                                (,b-type b))
                                                                       (the ,result-type (,op a b)))
                                                                    `(lambda (a)
                                                                       (declare (,a-type a))
                                                                       (the ,result-type (,op a ,b)))
                                                                    `(lambda (b)
                                                                       (declare (,b-type b))
                                                                       (the ,result-type (,op ,a b))))
                                                for args in (list (list a b)
                                                                  (list a)
                                                                  (list b))
                                                for fun = (or (gethash lambda cache)
                                                              (setf (gethash lambda cache)
                                                                    (handler-case (checked-compile lambda :allow-warnings t)
                                                                      (error (c)
                                                                        (error "Error compiling ~s:~% ~a" lambda c)))))
                                                do
                                                (when (and (zerop (mod (incf progress) (or #+(or arm x86) 100 10000)))
                                                           (interactive-stream-p *standard-output*))
                                                  (write-char #\Return)
                                                  (write progress)
                                                  (write-char #\Space)
                                                  (write (hash-table-count cache))
                                                  (force-output))
                                                (handler-case
                                                    (apply fun args)
                                                  (type-error (c)
                                                    (if (typep result result-type)
                                                        (error "~a => ~a /= ~a" (list* lambda args) c result)
                                                        (let ((x (type-error-datum c))
                                                              (type (type-error-expected-type c)))
                                                          (cond ((not (equal type result-type))
                                                                 (error "~a => type error ~a /= ~a" (list* lambda args)
                                                                        c
                                                                        result-type))
                                                                ((not (eql x result))
                                                                 (error "~a => type error ~a /= ~a" (list* lambda args)
                                                                        c
                                                                        x))))))
                                                  (error (c)
                                                    (error "~a => type error ~a /= ~a" (list* lambda args)
                                                           c
                                                           result-type))
                                                  (:no-error (x)
                                                    (if (typep result result-type)
                                                        (unless (eql x result)
                                                          (error "~a = ~a /= ~a" (list* lambda args) x result))
                                                        (error "~a => ~a /= type error" (list* lambda args) x))))))))))))))

(with-test (:name :overflow-arith)
  (test-ops '(+ - *)
            `(t fixnum (integer ,(- (expt 2 sb-vm:n-word-bits) 10)
                                ,(- (expt 2 sb-vm:n-word-bits) 1))
                (signed-byte ,sb-vm:n-word-bits)
                (unsigned-byte ,sb-vm:n-word-bits)
                (signed-byte 8)
                (unsigned-byte 8))
            (list 0 1 2 3 4 -1 -2 -3 -4
                  (- (expt 2 sb-vm:n-word-bits) 1)
                  (- (expt 2 sb-vm:n-word-bits) 5)
                  (- (expt 2 (1- sb-vm:n-word-bits)) 1)
                  (- (expt 2 (1- sb-vm:n-word-bits)) 5)
                  (- (expt 2 (1- sb-vm:n-word-bits)))
                  (- 10 (expt 2 (1- sb-vm:n-word-bits)))
                  (expt 2 (1- sb-vm:n-word-bits))
                  most-positive-fixnum
                  most-negative-fixnum
                  (1- most-positive-fixnum)
                  (1+ most-negative-fixnum)
                  (floor most-positive-fixnum 2)
                  (floor most-negative-fixnum 2))))

(with-test (:name :overflow-ash)
  (test-ops '(ash)
            `(t fixnum
                (signed-byte ,sb-vm:n-word-bits)
                (unsigned-byte ,sb-vm:n-word-bits)
                (integer ,(- 1 sb-vm:n-word-bits)
                         ,(1- sb-vm:n-word-bits))
                (integer 0
                         ,(1- sb-vm:n-word-bits))
                (and fixnum unsigned-byte))
            (list* most-positive-fixnum
                   (1- most-positive-fixnum)
                   (1+ most-positive-fixnum)
                   (1- (expt 2 sb-vm:n-word-bits))
                   #1=(list 0 1 3 4 -1 -3 -4
                            most-negative-fixnum
                            (1+ most-negative-fixnum)
                            (1- most-negative-fixnum)
                            (- (expt 2 (1- sb-vm:n-word-bits)))
                            (- 1 (expt 2 (1- sb-vm:n-word-bits)))
                            (- -1 (expt 2 (1- sb-vm:n-word-bits)))))
            :b-arguments (list* sb-vm:n-word-bits (- sb-vm:n-word-bits)
                                (- 1 sb-vm:n-word-bits) (1- sb-vm:n-word-bits)
                                300 -300 #1#)))

(with-test (:name :fixnum-integer-cmp)
  (test-ops '(> <)
            `(t fixnum
                integer
                bignum
                (integer ,(- (expt 2 sb-vm:n-word-bits) 10)
                         ,(- (expt 2 sb-vm:n-word-bits) 1))
                (signed-byte ,sb-vm:n-word-bits)
                (unsigned-byte ,sb-vm:n-word-bits)
                (signed-byte 8)
                (unsigned-byte 8)
                (integer 5 2147483647))
            (list 0 1 2 3 4 -1 -2 -3 -4
                  (- (expt 2 sb-vm:n-word-bits) 1)
                  (- (expt 2 sb-vm:n-word-bits) 5)
                  (- (expt 2 (1- sb-vm:n-word-bits)) 1)
                  (- (expt 2 (1- sb-vm:n-word-bits)) 5)
                  (- (expt 2 (1- sb-vm:n-word-bits)))
                  (- 10 (expt 2 (1- sb-vm:n-word-bits)))
                  (expt 2 (1- sb-vm:n-word-bits))
                  most-positive-fixnum
                  most-negative-fixnum
                  (1- most-positive-fixnum)
                  (1+ most-negative-fixnum)
                  (floor most-positive-fixnum 2)
                  (floor most-negative-fixnum 2)
                  2147483647)
            :result-types '(t)))

(with-test (:name :integer-ratio-float-compare)
  (test-ops '(> <)
            `(t fixnum
                integer
                bignum
                (integer ,(- (expt 2 sb-vm:n-word-bits) 10)
                         ,(- (expt 2 sb-vm:n-word-bits) 1))
                (signed-byte ,sb-vm:n-word-bits)
                (unsigned-byte ,sb-vm:n-word-bits)
                (signed-byte 8)
                (unsigned-byte 8)
                (integer 5 2147483647))
            (list 0 1 2 3 4 -1 -2 -3 -4
                  (- (expt 2 sb-vm:n-word-bits) 1)
                  (- (expt 2 sb-vm:n-word-bits) 5)
                  (- (expt 2 (1- sb-vm:n-word-bits)) 1)
                  (- (expt 2 (1- sb-vm:n-word-bits)) 5)
                  (- (expt 2 (1- sb-vm:n-word-bits)))
                  (- 10 (expt 2 (1- sb-vm:n-word-bits)))
                  (expt 2 (1- sb-vm:n-word-bits))
                  2147483647
                  most-positive-fixnum
                  most-negative-fixnum
                  (1- most-positive-fixnum)
                  (1+ most-negative-fixnum)
                  (floor most-positive-fixnum 2)
                  (floor most-negative-fixnum 2))
            :result-types '(t)))
