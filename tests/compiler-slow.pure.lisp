(with-test (:name (compile eval the type-error))
  (checked-compile-and-assert (:optimize :safe)
      '(lambda (v)
        (list (the fixnum (the (real 0) (eval v)))))
    ((0.1) (condition 'type-error))
    ((-1)  (condition 'type-error))))

(defun pick-acceptable-default (specifier)
  (let ((parse (sb-kernel:specifier-type specifier)))
    ; (format t "~&testcase: ~s~%" specifier)
    (typecase parse
      (sb-kernel:character-set-type #\a)
      (sb-kernel:numeric-type
       (cond ((eq (sb-kernel:numeric-type-class parse) 'float)
              (ecase (sb-kernel:numeric-type-complexp parse)
                (:real
                 (ecase (sb-kernel:numeric-type-format parse)
                   (single-float 1009f0)
                   (double-float pi)))
                (:complex
                 (ecase (sb-kernel:numeric-type-format parse)
                   (single-float #c(101f0 -1f0))
                   (double-float #c(2d0 3.5d0))))))
             (t
              1)))
      (t
       (cond ((equal specifier '(or (eql 1.0d0) (eql 10.0d0))) ; KLUDGE
              1.0d0)
             ((equal specifier '(member 1 2 10))
              2)
             ((equal specifier '(complex (member 10.0 20.0)))
              (complex 10.0 10.0))
             (t
              'whatever))))))

(with-test (:name :array-type-predicates)
  (dolist (et (list* '(integer -1 200) '(integer -256 1)
                     '(integer 0 128)
                     '(integer 0 (128))
                     '(double-float 0d0 (1d0))
                     '(single-float (0s0) (1s0))
                     '(or (eql 1d0) (eql 10d0))
                     '(member 1 2 10)
                     '(complex (member 10 20))
                     '(complex (member 10d0 20d0))
                     '(complex (member 10s0 20s0))
                     '(or integer double-float)
                     '(mod 1)
                     '(member #\a #\b)
                     '(eql #\a)
                     #+sb-unicode 'extended-char
                     #+sb-unicode '(eql #\cyrillic_small_letter_yu)
                     (map 'list 'sb-vm:saetp-specifier
                          sb-vm:*specialized-array-element-type-properties*)))
    (when et
      (let* ((v (make-array 3 :element-type et
                            ;; Pick an initial element because of the (ELT ,v 0)
                              :initial-element (pick-acceptable-default et))))
        (checked-compile-and-assert ()
            `(lambda ()
               (list (if (typep ,v '(simple-array ,et (*)))
                         :good
                         ',et)
                     (if (typep (elt ,v 0) '(simple-array ,et (*)))
                         ',et
                         :good)))
          (() '(:good :good)))))))

(with-test (:name (compile equal equalp :transforms))
  (let* ((s "foo")
         (bit-vector #*11001100)
         (values `(nil 1 2 "test"
                       ;; Floats duplicated here to ensure we get newly created instances
                       (read-from-string "1.1") (read-from-string "1.2d0")
                       (read-from-string "1.1") (read-from-string "1.2d0")
                       1.1 1.2d0 '("foo" "bar" "test")
                       #(1 2 3 4) #*101010 (make-broadcast-stream) #p"/tmp/file"
                       ,s (copy-seq ,s) ,bit-vector (copy-seq ,bit-vector)
                       ,(make-hash-table) #\a #\b #\A #\C
                       ,(make-random-state) 1/2 2/3)))

    (dolist (predicate '(equal equalp))
      ;; Test all permutations of different types
      (loop for x in values
         do (loop for y in values
               do (checked-compile-and-assert (:optimize nil)
                      `(lambda (x y)
                         (,predicate (the ,(type-of x) x)
                                     (the ,(type-of y) y)))
                    ((x y) (funcall predicate x y)))))
      (checked-compile-and-assert ()
          `(lambda (x y)
             (,predicate (the (cons (or simple-bit-vector simple-base-string))
                              x)
                         (the (cons (or (and bit-vector (not simple-array))
                                        (simple-array character (*))))
                              y)))
        (((list (string 'list)) (list "LIST")) t)))))

(with-test (:name (sb-c::mask-signed-field :randomized))
  (let (result)
    (dotimes (i 1000)
      (let* ((ool (checked-compile '(lambda (s i)
                                     (sb-c::mask-signed-field s i))))
             (size (random (* sb-vm:n-word-bits 2)))
             (constant (checked-compile `(lambda (i)
                                           (sb-c::mask-signed-field ,size i))))
             (arg (- (random (* most-positive-fixnum 8)) (* most-positive-fixnum 4)))
             (declared (checked-compile `(lambda (i)
                                           (declare (type (integer ,(- (abs arg)) ,(abs arg)) i))
                                           (sb-c::mask-signed-field ,size i))))
             (ool-answer (funcall ool size arg))
             (constant-answer (funcall constant arg))
             (declared-answer (funcall declared arg)))
        (unless (= ool-answer constant-answer declared-answer)
          (push (list size arg ool-answer constant-answer declared-answer) result))))
    (assert (null result))))

(with-test (:name (multiple-value-call :type-checking-rest))
  (checked-compile-and-assert (:allow-warnings t
                               :optimize :safe)
      `(lambda (list)
         (multiple-value-call
             (lambda (&optional a &rest r)
               (declare ((satisfies eval) r)
                        (ignore r))
               (list a))
           (values-list list)))
    (('(1 list 2)) '(1))
    (('(1)) (condition 'type-error))))

(with-test (:name (multiple-value-call :type-checking-rest.2))
  (checked-compile-and-assert (:allow-warnings t
                               :optimize :safe)
      `(lambda (list)
         (multiple-value-call
             (lambda (&optional a &rest r)
               (declare (null r)
                        (ignore r))
               (list a))
           (values-list list)))
    (('(1 list 2)) (condition 'type-error))
    (('(1)) '(1))))

(with-test (:name (multiple-value-call :type-checking-rest :type-derivation))
  (checked-compile-and-assert (:allow-warnings t
                               :optimize :safe)
      `(lambda (list)
         (multiple-value-call
             (lambda (&optional a &rest r)
               (declare (cons r)
                        (ignore r))
               (list a))
           (values-list list)))
    (('(1 2)) '(1))
    (('(1)) (condition 'type-error))))
