(defpackage "SB-GMP-TESTS"
  (:use "COMMON-LISP" "SB-GMP" "SB-RT"))

(in-package "SB-GMP-TESTS")

(defparameter *state* (make-gmp-rstate))
(rand-seed *state* 1234)

(defmacro defgenerator (name arguments &body body)
  `(defun ,name ,arguments
     (lambda () ,@body)))

(defgenerator gen-mpz (&key (limbs 5) sign nonzero)
  (let ((integer (random-bitcount *state*
                                  (* limbs sb-vm:n-word-bits))))
    (when (and nonzero (zerop integer))
      (setf integer 1))
    (ecase sign
      ((+ nil) integer)
      (- (- integer))
      ((t random) (if (zerop (random 2))
                      integer
                      (- integer))))))

(defun gen-mpq (&key (limbs 5) sign nonzero)
  (let ((numerator (gen-mpz :limbs limbs :sign sign
                            :nonzero nonzero))
        (denominator (gen-mpz :limbs limbs :nonzero t)))
    (lambda ()
      (/ (funcall numerator) (funcall denominator)))))

(defun maybe-apply (maybe-function &optional arguments)
  (if (typep maybe-function '(or function symbol))
      (apply maybe-function arguments)
      maybe-function))

(defun test-one-case (base tested &rest arguments)
  (let* ((initial-hashes (mapcar #'sxhash arguments))
         (base-values (let ((*gmp-disabled* t))
                        (multiple-value-list (maybe-apply base arguments))))
         (test-values (let ((*gmp-disabled* nil))
                        (multiple-value-list (apply tested arguments))))
         (final-hashes (mapcar #'sxhash arguments)))
    (unless (and (= (length base-values) (length test-values))
                 (every #'eql base-values test-values))
      (error "Failed test: ~S returned ~S; expected ~S"
             (cons tested arguments) test-values base-values))
    (unless (every #'eql initial-hashes final-hashes)
      (error "Failed test: ~S modified arguments ~{~A~^, ~} ~
              (printed modified values)"
             (cons tested arguments)
             (loop for i upfrom 0
                   for initial in initial-hashes
                   for final in final-hashes
                   unless (eql initial final)
                     collect i))))
  nil)

;; Really just the most basic smoke test, otherwise
;; build times ballon up a bit on slow machines.
(defvar *iteration-count* 3)

(defun test-n-cases (base tested &rest argument-generators)
  (let ((*random-state* (sb-ext:seed-random-state 54321)))
    (loop repeat *iteration-count* do
      (apply 'test-one-case base tested
             (mapcar #'maybe-apply argument-generators)))))

(defmacro define-gmp-test ((name &key (repeat 1) limbs (gmp-seed 1234))
                           &body body)
  `(deftest ,name
       (let ((*random-state* (sb-ext:seed-random-state 54321)))
         (rand-seed *state* ,gmp-seed)
         (handler-case
             (dotimes (i ,repeat)
               ;; try to get small failures first
               (let ((limbs (case i
                              (0 ,(subst `(lambda (x)
                                            x 0)
                                         'random
                                         limbs))
                              (1 ,(subst `(lambda (x)
                                            (if (> x 1) 1 0))
                                         'random
                                         limbs))
                              (t ,limbs))))
                 (declare (ignorable limbs))
                 ,@body))
           (error (c)
             (format t "~&~A~%" c)
             nil)
           (:no-error (&rest _) _ t)))
     t))

(define-gmp-test (mpz-add :repeat 7 :limbs (+ (random #xFFFFF) 2))
  (test-n-cases '+ 'mpz-add
                    (gen-mpz :limbs limbs :sign t)
                    (gen-mpz :limbs limbs :sign t)))

(define-gmp-test (mpz-sub :repeat 7 :limbs (+ (random #x1FFFF) 2))
  (test-n-cases '- 'mpz-sub
                (gen-mpz :limbs limbs :sign t)
                (gen-mpz :limbs limbs :sign t)))

(define-gmp-test (mpz-mul :repeat 7 :limbs (+ (random #x253F) 2))
  (test-n-cases '* 'mpz-mul
                (gen-mpz :limbs limbs :sign t)
                (gen-mpz :limbs limbs :sign t)))

(define-gmp-test (mpz-tdiv :repeat 7 :limbs (+ (random #x253F) 2))
  (test-n-cases 'truncate 'mpz-tdiv
                (gen-mpz :limbs limbs :sign t)
                (gen-mpz :limbs limbs :sign t :nonzero t)))

(define-gmp-test (mpz-fdiv :repeat 7 :limbs (+ (random #x253F) 2))
  (test-n-cases 'floor 'mpz-fdiv
                (gen-mpz :limbs limbs :sign t)
                (gen-mpz :limbs limbs :sign t :nonzero t)))

(define-gmp-test (mpz-cdiv :repeat 7 :limbs (+ (random #x253F) 2))
  (test-n-cases 'ceiling 'mpz-cdiv
                (gen-mpz :limbs limbs :sign t)
                (gen-mpz :limbs limbs :sign t :nonzero t)))

(define-gmp-test (mpz-gcd :repeat 7 :limbs (+ (random #x253F) 2))
  (test-n-cases 'gcd 'mpz-gcd
                (gen-mpz :limbs limbs :sign t)
                (gen-mpz :limbs limbs :sign t)))

(define-gmp-test (mpz-lcm :repeat 7 :limbs (+ (random #x253F) 2))
  (test-n-cases 'lcm 'mpz-lcm
                (gen-mpz :limbs limbs :sign t)
                (gen-mpz :limbs limbs :sign t)))

(define-gmp-test (isqrt :repeat 7 :limbs (+ (random #x253F) 2))
  (test-n-cases 'isqrt 'mpz-sqrt (gen-mpz :limbs limbs)))

(define-gmp-test (mpz-mod :repeat 7 :limbs (1+ (random #x253F)))
  (test-n-cases 'mod 'mpz-mod
                (gen-mpz :limbs limbs :sign t)
                (gen-mpz :limbs limbs :sign t :nonzero t)))

(define-gmp-test (mpz-powm :repeat 7 :limbs (1+ (random #x253F)))
  (test-n-cases (lambda (base exponent mod)
                  (let ((*gmp-disabled* nil)) ; atrociously slow otherwise
                    (mod (expt base exponent) mod)))
                'mpz-powm
                (gen-mpz :limbs limbs :sign t)
                (lambda ()
                  (1+ (random 40)))
                (gen-mpz :limbs (ceiling limbs 2) :nonzero t)))

;; bugs that have been fixed
(define-gmp-test (sign-conversion)
  (test-one-case '+ 'mpz-add #x7FFFFFFFFFFFFFFF #x7FFFFFFFFFFFFFFF))
(define-gmp-test (truncate-1)
  (test-one-case 'truncate 'mpz-tdiv
                 30951488519636377404900619671461408624764773310745985021994671444676860083493
                 200662724990805535745252242839121922075))
(define-gmp-test (truncate-2)
  (test-one-case 'truncate 'mpz-tdiv
                 320613729464106236061704728914573914390
                 -285049280629101090500613812618405407883))

(define-gmp-test (mpz-nextprime :repeat 7
                                :gmp-seed 6234
                                :limbs (1+ (random #x2F)))
  (let ((a (gen-mpz :limbs limbs)))
    (dotimes (i *iteration-count*)
      (let* ((a (funcall a))
             (p (mpz-nextprime a)))
        (assert (>= p a))
        (assert (plusp (mpz-probably-prime-p p)))))))

(define-gmp-test (mpq-add :repeat 7 :limbs (1+ (random #x3FF))
                          :gmp-seed 1235)
  (test-n-cases '+ 'mpq-add
                (gen-mpq :limbs limbs :sign t)
                (gen-mpq :limbs limbs :sign t)))

(define-gmp-test (mpq-sub :repeat 7 :limbs (1+ (random #x1FF))
                          :gmp-seed 1235)
  (test-n-cases '- 'mpq-sub
                (gen-mpq :limbs limbs :sign t)
                (gen-mpq :limbs limbs :sign t)))

(define-gmp-test (mpq-mul :repeat 7 :limbs (1+ (random #x5FF))
                          :gmp-seed 6235)
  (test-n-cases '* 'mpq-mul
                (gen-mpq :limbs limbs :sign t)
                (gen-mpq :limbs limbs :sign t)))

(define-gmp-test (mpq-div :repeat 7 :limbs (1+ (random #x3FF))
                          :gmp-seed 7235)
  (test-n-cases '/ 'mpq-div
                (gen-mpq :limbs limbs :sign t)
                (gen-mpq :limbs limbs :sign t)))

(define-gmp-test (pow)
  (test-one-case 'expt 'mpz-pow
                 16 3))

(defun fac (n)
  (loop for i from 1 to n
        for fac = 1 then (* fac i)
        finally (return fac)))

(define-gmp-test (fac1)
  (test-one-case 'fac 'mpz-fac
                 6))

(define-gmp-test (fac2)
  (test-one-case 'fac 'mpz-fac
                 63))
