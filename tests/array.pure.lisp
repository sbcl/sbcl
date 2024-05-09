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

;;; Array initialization has complicated defaulting for :ELEMENT-TYPE,
;;; and both compile-time and run-time logic takes a whack at it.
(with-test (:name (make-array :element-type :bug-126))
  (let ((testcases '(;; Bug 126, confusion between high-level default string
                     ;; initial element #\SPACE and low-level default array
                     ;; element #\NULL, is gone.
                     (#\null (make-array 11 :element-type 'character :initial-element #\null)
                             simple-string)
                     (#\space (make-string 11 :initial-element #\space) string)
                     (#\* (make-string 11 :initial-element #\*))
                     (#\null (make-string 11))
                     (#\null (make-string 11 :initial-element #\null))
                     (#\x (make-string 11 :initial-element #\x))
                     ;; And the other tweaks made when fixing bug 126 didn't
                     ;; mess things up too badly either.
                     (0 (make-array 11 :initial-element 0) simple-vector)
                     (nil (make-array 11 :initial-element nil))
                     (12 (make-array 11 :initial-element 12))
                     (0 (make-array 11 :element-type '(unsigned-byte 4) :initial-element 0)
                        (simple-array (unsigned-byte 4) (*)))
                     (12 (make-array 11
                          :element-type '(unsigned-byte 4)
                          :initial-element 12)))))
    (dolist (testcase testcases)
      (destructuring-bind (expected-result form &optional type) testcase
        (unless (eql expected-result (aref (eval form) 3))
          (error "expected ~S in EVAL ~S" expected-result form))
        (unless (eql expected-result
                     (aref (funcall (checked-compile `(lambda () ,form)
                                                     :allow-warnings t))
                           3))
          (error "expected ~S in FUNCALL COMPILE ~S" expected-result form))
        ;; also do some testing of compilation and verification that
        ;; errors are thrown appropriately.
        (unless (eql expected-result
                     (funcall (checked-compile `(lambda () (aref ,form 3))
                                               :allow-warnings t)))
          (error "expected ~S in COMPILED-AREF ~S" expected-result form))
        (when type
          (unless (eql expected-result
                       (funcall (checked-compile `(lambda ()
                                                    (let ((x ,form))
                                                      (declare (type ,type x))
                                                      (aref x 3)))
                                                 :allow-warnings t)))
            (error "expected ~S in COMPILED-DECLARED-AREF ~S" expected-result form)))
        (when (ignore-errors (aref (eval form) 12))
          (error "error not thrown in EVAL ~S" form))
        (when (ignore-errors (aref (funcall (checked-compile `(lambda () ,form)
                                                             :allow-warnings t))
                                   12))
          (error "error not thrown in FUNCALL COMPILE ~S" form))
        (when (ignore-errors (funcall (checked-compile `(lambda () (aref ,form 12))
                                                       :allow-warnings t)))
          (error "error not thrown in COMPILED-AREF ~S" form))
        (when type
          (when (ignore-errors (funcall
                                (checked-compile `(lambda ()
                                                    (let ((x ,form))
                                                      (declare (type ,type x))
                                                      (aref x 12)))
                                                 :allow-warnings t)))
            (error "error not thrown in COMPILED-DECLARED-AREF ~S" form)))))))

;;; On the SPARC, until sbcl-0.7.7.20, there was a bug in array
;;; references for small vector elements (spotted by Raymond Toy); the
;;; bug persisted on the PPC until sbcl-0.7.8.20.
(let (vector)
  (loop for i below 64
        for list = (make-list 64 :initial-element 1)
        do (setf (nth i list) 0)
        do (setf vector (make-array 64 :element-type 'bit
                                       :initial-contents list))
        do (checked-compile-and-assert (:optimize '(:speed 3 :safety 0))
               `(lambda (rmdr)
                  (declare (type (simple-array bit (*)) rmdr))
                  (aref rmdr ,i))
             ((vector) 0))))

;;; Following refactoring of sequence functions to detect bad type
;;; specifiers, REVERSE was left broken on vectors with fill pointers.
(with-test (:name :reverse-fill-pointer.string)
  (let ((a (make-array 10
                       :fill-pointer 5
                       :element-type 'character
                       :initial-contents "abcdefghij")))
    (assert (string= (reverse a) "edcba"))))

(with-test (:name :reverse-fill-pointer.fixnum)
  (let ((a (make-array 10
                       :fill-pointer 6
                       :element-type 'fixnum
                       :initial-contents '(0 1 2 3 4 5 7 8 9 10))))
    (assert (equalp (reverse a) #(5 4 3 2 1 0)))))

;;; ARRAY-IN-BOUNDS-P should work when given non-INDEXes as its
;;; subscripts (and return NIL, of course)
(with-test (:name array-in-bounds-p)
  (macrolet
      ((test-case (array subscript expected)
         `(progn
            (assert (,(if expected 'progn 'not)
                      (array-in-bounds-p ,array ,subscript)))
            (assert (,(if expected 'progn 'not)
                      (funcall (checked-compile `(lambda (array subscript)
                                                   (array-in-bounds-p array subscript)))
                               ,array ,subscript))))))
    (let ((a (make-array 10 :fill-pointer 5)))
      (test-case a -1                        nil)
      (test-case a  3                        t)
      (test-case a  7                        t)
      (test-case a 11                        nil)
      (test-case a (1+ most-positive-fixnum) nil))))

;;; arrays of bits should work:
(with-test (:name (make-array :element-type bit))
  (let ((a (make-array '(10 10) :element-type 'bit :adjustable t)))
    (setf (bit a 0 0) 1)
    (assert (= (bit a 0 0) 1)))
  (let ((a (make-array '(10 10) :element-type 'bit)))
    (setf (sbit a 0 0) 1)
    (assert (= (sbit a 0 0) 1))))

(with-test (:name (copy-seq bit-and equalp))
  (let ((x (copy-seq #*0011))
        (y (copy-seq #*0101)))
    (assert (equalp (bit-and x y nil) #*0001))))

;;; arrays of NIL should work, FSVO "work".
(with-test (:name (make-array upgraded-array-element-type :element-type nil))
  (let ((a (make-array '(10 10) :element-type 'nil)))
    (assert (= (array-total-size a) 100))
    (assert (equal (array-dimensions a) '(10 10)))
    (assert (eq (array-element-type a) 'nil)))

  (assert (eq (upgraded-array-element-type 'nil) 'nil)))

(with-test (:name (aref 0 :compile-time-error))
  (multiple-value-bind (fun fail)
      (checked-compile `(lambda () (aref (make-array 0) 0))
                       :allow-warnings t)
    (assert fail)
    (assert-error (funcall fun) sb-int:invalid-array-index-error)))

(with-test (:name (aref 1 :compile-time-error))
  (multiple-value-bind (fun fail)
      (checked-compile `(lambda () (aref (make-array 1) 1))
                       :allow-warnings t)
    (assert fail)
    (assert-error (funcall fun) sb-int:invalid-array-index-error)))

(with-test (:name (make-array :element-type :compile-time error))
  (multiple-value-bind (fun fail warnings style-warnings)
      (checked-compile `(lambda () (make-array 5 :element-type 'undefined-type))
                       :allow-style-warnings t)
    (declare (ignore fun fail warnings))
    (assert style-warnings)))

(with-test (:name (make-array :default :element-type :supplied :compile-time warning))
  ;; Supplied :initial-element, EQL to the default initial element,
  ;; results in full warning, even if not "used" due to 0 array total
  ;; size.
  (flet ((check (dimensions)
           (multiple-value-bind (fun fail warnings)
               (checked-compile
                `(lambda ()
                   (make-array ,dimensions
                               :initial-element 0 :element-type 'string))
                :allow-warnings t)
             (declare (ignore fun fail))
             (assert (= (length warnings) 1)))))
    (check 1)
    (check 0)))

(with-test (:name (make-array :default :element-type :implicit :compile-time style-warning))
  ;; Implicit default initial element used to initialize array
  ;; elements results in a style warning.
  (multiple-value-bind (fun fail warnings style-warnings)
      (checked-compile `(lambda () (make-array 5 :element-type 'string))
                       :allow-style-warnings t)
    (declare (ignore fun fail warnings))
    (assert (= (length style-warnings) 1)))

  ;; But not if the default initial-element is not actually used to
  ;; initialize any elements due to 0 array total size.
  (checked-compile `(lambda () (make-array 0 :element-type 'string)))
  (checked-compile `(lambda () (make-array '(0 2) :element-type 'string))))

(with-test (:name (make-array standard-char))
  ;; Maybe this is a kludge, but STANDARD-CHAR should just work,
  ;; I don't care if #\nul is nonstandard. Because, seriously?
  (checked-compile '(lambda ()
                     (make-array 5 :fill-pointer 0 :element-type 'standard-char))))

(with-test (:name :big-array)
  ;; we used to have leakage from cross-compilation hosts of the INDEX
  ;; type, which prevented us from actually using all the large array
  ;; dimensions that we promised.  Let's make sure that we can create
  ;; an array with more than 2^24 elements, since that was a symptom
  ;; from the CLISP and OpenMCL hosts.
  (let ((big-array (opaque-identity
                    (make-array (expt 2 26) :element-type 'bit))))
    (assert (= (length big-array) (expt 2 26)))))

;;; Bug reported by Kalle Olavi Niemitalo for CMUCL through Debian BTS
(with-test (:name (make-array aref :rank 0))
  (let ((array (make-array nil :initial-contents nil)))
    (assert (eql (aref array) nil))))

(with-test (:name (make-array (setf aref) length))
  (checked-compile-and-assert ()
      '(lambda ()
         (let ((a (make-array '(4)
                              :element-type 'base-char
                              :initial-element #\z)))
           (setf (aref a 0) #\a)
           (setf (aref a 1) #\b)
           (setf (aref a 2) #\c)
           a))
    (() 4 :test (lambda (values expected)
                  (= (length (first values)) (first expected))))))

;;; I have no idea how this is testing adjust-array with an initial-element !
(with-test (:name (make-array adjust-array :initial-element))
  (let ((x (make-array nil :initial-element 'foo)))
    ;; make the result look used
    (opaque-identity (adjust-array x nil))
    (assert (eql (aref x) 'foo))))

;;; BUG 315: "no bounds check for access to displaced array"
;;;  reported by Bruno Haible sbcl-devel "various SBCL bugs" from CLISP
;;;  test suite.
(with-test (:name (:displaced-to aref sb-int:invalid-array-index-error :bug-315))
  (locally (declare (optimize (safety 3) (speed 0)))
    (let* ((x (make-array 10 :fill-pointer 4 :element-type 'character
                          :initial-element #\space :adjustable t))
           (y (make-array 10 :fill-pointer 4 :element-type 'character
                          :displaced-to x)))
      (assert (eq x (adjust-array x '(5))))
      (assert (eq :error (handler-case
                             (char y 0)
                           (sb-int:invalid-array-error (e)
                             (assert (eq y (type-error-datum e)))
                             (assert (equal `(vector character 10)
                                            (type-error-expected-type e)))
                             :error)))))))

;;; MISC.527: bit-vector bitwise operations used LENGTH to get a size
;;; of a vector
(with-test (:name (bit-vector :bitwise-operations))
  (flet ((bit-vector-equal (v1 v2)
           (and (bit-vector-p v1) (bit-vector-p v2)
                (equal (array-dimension v1 0) (array-dimension v2 0))
                (loop for i below (array-dimension v1 0)
                   always (eql (aref v1 i) (aref v2 i))))))
    (let* ((length 1024)
           (v1 (make-array length :element-type 'bit :fill-pointer 0))
           (v2 (make-array length :element-type 'bit :fill-pointer 1)))
      (loop for i from 0 below length
         for x1 in '#1=(0 0 1 1 . #1#)
         and x2 in '#2=(0 1 0 1 . #2#)
         do (setf (aref v1 i) x1)
         do (setf (aref v2 i) x2))
      (loop for (bf lf) in '((bit-and logand)
                             (bit-andc1 logandc1)
                             (bit-andc2 logandc2)
                             (bit-eqv logeqv)
                             (bit-ior logior)
                             (bit-nand lognand)
                             (bit-nor lognor)
                             (bit-orc1 logorc1)
                             (bit-orc2 logorc2)
                             (bit-xor logxor)
                             ((lambda (x y) (bit-not x)) #.(lambda (x y)
                                                             (declare (ignore y))
                                                             (lognot x))))
         for fun = (checked-compile `(lambda (v)
                                       (declare (type (array bit (*)) v))
                                       (declare (optimize (speed 3) (safety 0)))
                                       (,bf v ,v2))
                                    :allow-style-warnings t)
         for r1 = (funcall fun v1)
         and r2 = (coerce (loop for i below length
                             collect (logand 1 (funcall lf (aref v1 i) (aref v2 i))))
                          'bit-vector)
         do (assert (bit-vector-equal r1 r2))))))

(with-test (:name (adjust-array fill-pointer))
  ;; CLHS, ADJUST-ARRAY: An error of type error is signaled if
  ;; fill-pointer is supplied and non-nil but array has no fill pointer.
  (assert (eq :good
              (handler-case
                  (let ((array (make-array 12)))
                    (assert (not (array-has-fill-pointer-p array)))
                    ;; make the result look used
                    (opaque-identity (adjust-array array 12 :fill-pointer t))
                    array)
                (type-error ()
                  :good)))))

(with-test (:name (adjust-array :multidimensional))
  (let ((ary (make-array '(2 2) :initial-element 0)))
    ;; SBCL used to give multidimensional arrays a bogus fill-pointer
    (assert (not (array-has-fill-pointer-p (adjust-array ary '(2 2)))))))

(with-test (:name :%set-fill-pointer/error)
  (let ((v (make-array 3 :fill-pointer 0)))
    (handler-case
        (progn
          (setf (fill-pointer v) 12)
          (error "WTF"))
      (error (e)
        (assert (eql 12 (type-error-datum e)))
        (assert (equal '(integer 0 3) (type-error-expected-type e)))))))

(with-test (:name array-storage-vector)
  (let ((vec (vector 1 2 3)))
    (assert (eq vec (sb-ext:array-storage-vector vec)))
    (assert (equalp (vector 1 2 3 4)
                    (sb-ext:array-storage-vector
                     (make-array '(2 2) :initial-contents '((1 2) (3 4))))))
    (assert (eq 'fixnum (array-element-type
                         (sb-ext:array-storage-vector (make-array '(3 4 5)
                                                                 :element-type 'fixnum)))))
    (assert (not (array-has-fill-pointer-p
                  (sb-ext::array-storage-vector
                   (make-array 5 :fill-pointer 4)))))))

(with-test (:name :invalid-array-index-error)
  (let ((array (make-array '(3 3 3))))
    (assert
     (eq :right
         (handler-case
             (eval `(aref ,array 0 1 3))
           (sb-int:invalid-array-index-error (e)
             (when (and (eq array (sb-kernel::invalid-array-index-error-array e))
                        (= 3 (type-error-datum e))
                        (equal '(integer 0 (3)) (type-error-expected-type e)))
               :right)))))))

(with-test (:name :out-of-bounds-error-details)
  (assert (eq :good
              (handler-case
                  (flet ((test (array i)
                           (aref array i)))
                    (test (eval '(vector 0 1 2 3)) 6))
                (sb-int:invalid-array-index-error (e)
                  (when (and (equal '(integer 0 (4))
                                    (type-error-expected-type e))
                             (eql 6 (type-error-datum e)))
                    :good))))))

(with-test (:name :odd-keys-for-make-array)
  (multiple-value-bind (fun fail warnings)
      (checked-compile `(lambda (m) (make-array m 1))
                       :allow-warnings 'simple-warning)
    (declare (ignore fun fail))
    (assert (= 1 (length warnings)))))


(with-test (:name :bug-1096359)
  (let ((a (make-array 1 :initial-element 5)))
    (assert (equalp (adjust-array a 2 :initial-element 10)
                    #(5 10)))))

(with-test (:name (:make-array-transform-unknown-type :bug-1156095))
  (assert (nth-value 3 (checked-compile
                        `(lambda () (make-array '(1 2) :element-type ',(gensym)))
                        :allow-style-warnings t))))

(with-test (:name :dont-make-array-bad-keywords)
  ;; This used to get a heap exhaustion error because of trying
  ;; to make the array before checking keyword validity.
  (handler-case
      (locally
          (declare (notinline make-array))
        (make-array (1- array-total-size-limit)
                    :initial-contents '(a b c) :initial-element 9))
    (simple-error (c)
      (assert
       (string= (simple-condition-format-control c)
                "Can't specify both :INITIAL-ELEMENT and :INITIAL-CONTENTS")))))

(with-test (:name (make-array :sanity-check-dims-first))
  ;; A full call to %MAKE-ARRAY will signal a TYPE-ERROR on these inputs
  ;; instead of trying to consume a massive amount of memory.
  ;; Additionally, the relevent IR1 transform should give up.
  (flet ((test (inline)
           (multiple-value-bind (fun failure-p warnings)
               (checked-compile
                `(lambda ()
                   (declare (,inline make-array))
                   (make-array `(-1 -1 ,(- (ash array-dimension-limit -2) 4))))
                :allow-failure t :allow-warnings t)
             (ecase inline
               (inline
                (assert failure-p)
                (assert (= 1 (length warnings))))
               (notinline
                (assert failure-p)
                (assert (= 1 (length warnings)))))
             (assert-error (funcall fun) type-error))))
    (test 'inline)
    (test 'notinline)))

(with-test (:name (make-array :size-overflow)
                  ;; size limit is small enough that this fails by not failing
                  ;; in the expected way
                  :skipped-on :ubsan)
  ;; 1-bit fixnum tags make array limits overflow the word length
  ;; when converted to bytes
  (when (and (= sb-vm:n-fixnum-tag-bits 1)
             (<= (- most-positive-fixnum
                    array-total-size-limit)
                 2))
    (multiple-value-bind (fun failure-p warnings)
        (checked-compile
         '(lambda ()
            (make-array (1- array-total-size-limit)))
         :allow-failure t :allow-warnings t)
      (assert failure-p)
      (assert (= 1 (length warnings)))
      (assert-error (funcall fun) type-error))))

(with-test (:name (adjust-array :non-adjustable))
  (let* ((a (make-array '(2 3) :initial-contents '((0 1 2) (3 4 5))))
         (b (adjust-array a '(2 2))))
    (setf (aref a 0 0) 11)
    (assert (zerop (aref b 0 0)))
    (assert (not (eq a b)))))

(with-test (:name :check-bound-elision)
  (checked-compile-and-assert (:optimize :safe)
      `(lambda (x)
         (char "abcd" x))
    ((4) (condition 'sb-int:invalid-array-index-error)))
  (checked-compile-and-assert (:optimize '(:safety 0))
      `(lambda (x)
         ;; Strings are null-terminated for C interoperability
         (char #.(coerce "abcd" 'simple-base-string) x))
    ((4) #\Nul)))
(defun check-bound-multiple-reads (x i)
  (let* ((x (truly-the simple-vector x))
         (l (sb-c::vector-length x)))
    (sb-kernel:%check-bound x l i)
    l))
(compile 'check-bound-multiple-reads)
(with-test (:name :check-bound-vop-optimize)
  ;; could have crashed with the bad optimizer
  (check-bound-multiple-reads #(a b c) 2))

(with-test (:name (adjust-array :transform))
  (checked-compile-and-assert ()
      `(lambda ()
         (adjust-array #(1 2 3) 3 :displaced-to #(4 5 6)))
    (() #(4 5 6) :test #'equalp)))

(with-test (:name (adjust-array :fill-pointer))
  (let ((array (make-array 10 :fill-pointer t :initial-element 0)))
    (assert (= (fill-pointer (adjust-array array 5 :fill-pointer 2))
               2))))

(with-test (:name (adjust-array :initial-element))
  (checked-compile-and-assert ()
      `(lambda (x)
         (adjust-array x 5 :initial-element #\x))
    (("abc") "abcxx")))

(with-test (:name (make-array :initial-contents 1))
  (flet ((f (x y)
           (sb-int:dx-let ((a (make-array `(,x ,y)
                                          :initial-contents
                                          '((a b c) (1 2 3)))))
             (eval a)
             nil)))
    (f 2 3)
    (assert-error (f 3 2))))

(with-test (:name (make-array :initial-contents 2))
  (labels ((compute-contents () '((a b c) (1 2 3)))
           (f (x y)
             (sb-int:dx-let ((a (make-array `(,x ,y)
                                            :initial-contents
                                            (compute-contents))))
               (eval a)
                nil)))
    (declare (notinline compute-contents))
    (f 2 3)
    (assert-error (f 3 2))))

(with-test (:name (make-array :initial-contents 3))
  (multiple-value-bind (fun failure-p warnings)
      (checked-compile
       '(lambda (z)
          (symbol-macrolet ((x (+ 1 1)) (y (* 2 1)))
            (make-array `(,x ,y)
                        :initial-contents
                        `((,z ,z 1) (,z ,z ,z)))))
       :allow-failure t :allow-warnings t)
    (assert failure-p)
    (assert (= 1 (length warnings)))
    (assert-error (funcall fun 0) error)))

(with-test (:name (adjust-array :element-type))
  (checked-compile-and-assert ()
      `(lambda (array)
         (adjust-array array 3 :element-type '(signed-byte 2)))
    ((#(1 2 3)) (condition 'error)))
  (checked-compile-and-assert ()
      `(lambda (array)
         (adjust-array array 5 :displaced-to #(1 2 3)))
    (((make-array 5 :adjustable t :element-type 'fixnum)) (condition 'error))))

(with-test (:name (make-array :transform :fill-pointer nil))
  (flet ((test (form)
           (assert (not (ctu:ir1-named-calls `(lambda () ,form))))))
    (test '(make-array 3      :fill-pointer nil))
    (test '(make-array 3      :fill-pointer nil))
    (test '(make-array 3      :fill-pointer t))
    (test '(make-array 3      :adjustable nil))
    (test '(make-array '(3 3) :adjustable nil))
    (test '(make-array '(3 3) :fill-pointer nil))))

(with-test (:name (make-array :transform :adjustable :fill-pointer))
  (multiple-value-bind (calls fun)
      (ctu:ir1-named-calls '(lambda (fp) (make-array 3 :adjustable t :fill-pointer fp)))
    (assert (not (member 'sb-kernel:%make-array calls)))
    (assert (= (length (funcall fun t)) 3))
    (assert (array-has-fill-pointer-p (funcall fun t)))
    (assert (= (length (funcall fun 2)) 2))
    (assert (= (array-total-size (funcall fun 2)) 3))
    (assert-error (funcall fun 4))
    (assert-error (funcall fun 'abc))
    (assert (not (array-has-fill-pointer-p (funcall fun nil))))
    (assert (= (length (funcall fun nil)) 3))))

(with-test (:name (make-array :transform :non-constant-fill-pointer))
  ;; Known adjustable with any fill-pointer can be inlined
  (multiple-value-bind (calls fun)
      (ctu:ir1-named-calls '(lambda (n fillp)
                                 (make-array (the (mod 20) n)
                                             :adjustable t :fill-pointer fillp)))
    (assert (not (member 'sb-kernel:%make-array calls)))
    (let ((a (funcall fun 10 3)))
      (assert (= (length a) 3))
      (assert (= (array-dimension a 0) 10))))
  ;; Non-adjustable w/ non-constant numeric fill-pointer can be inlined
  (multiple-value-bind (calls fun)
      (ctu:ir1-named-calls '(lambda (n)
                                 (make-array (the (mod 20) n)
                                             :fill-pointer (floor n 2))))
    (assert (not (member 'sb-kernel:%make-array calls)))
    (let ((a (funcall fun 10)))
      (assert (= (length a) 5))
      (assert (= (array-dimension a 0) 10)))))

(with-test (:name :check-bound-fixnum-check)
  (checked-compile-and-assert (:optimize :safe)
      `(lambda (x) (aref #100(a) x))
    ((#\Nul) (condition 'type-error))))

(with-test (:name (make-array :erroneous-type-specifiers))
  (dolist (atom '(signed-byte unsigned-byte))
    (assert (handler-case (eval `(make-array 10 :element-type '(,atom "oops")))
              (error (c) (search (format nil "bad size specified for ~A" atom)
                                 (princ-to-string c)))
              (:no-error (obj) obj nil)))))

(with-test (:name (make-array :strange-type-specifiers))
  (assert (stringp (make-array 10 :element-type (opaque-identity '(base-char)))))
  (assert (stringp (make-array 10 :element-type (opaque-identity '(standard-char)))))
  ;; If there are no extended characters (as on #-sb-unicode), then EXTENDED-CHAR is
  ;; the empty type. You'll get exactly what you ask for: an array which can hold
  ;; nothing. It's not a string, which is the right answer.
  #+sb-unicode
  (assert (stringp (make-array 10 :element-type (opaque-identity '(extended-char)))))
  (assert (bit-vector-p (make-array 10 :element-type (opaque-identity '(bit))))))

(with-test (:name :make-array-satisifies-element-type)
  (checked-compile-and-assert
      ()
      '(lambda (type)
        (make-array 3 :initial-element #\a :element-type type))
    (('(and character (satisfies eval))) "aaa" :test #'equal)
    (('(and character (or (satisfies eval) base-char))) "aaa" :test #'equal)))

(with-test (:name :make-array-or-unsigned-byte-type)
  (checked-compile-and-assert
      ()
      '(lambda (type)
        (make-array 1 :element-type type :initial-element 0))
    (('(or (eql -16) unsigned-byte)) #(0) :test #'equalp)))

(with-test (:name :check-bound-signed-bound-notes
            :fails-on (not (or :x86-64 :x86 :arm64)))
  (checked-compile-and-assert
      (:allow-notes nil)
      `(lambda (x y)
         (declare (fixnum y))
         (svref x (+ y 2)))
    ((#(1 2 3) 0) 3)))

(with-test (:name :make-array-header*-type-derivation)
  (let ((fun (checked-compile
              '(lambda (a)
                (declare ((simple-array (unsigned-byte 8) (*)) a))
                (make-array '(10 20) :element-type (array-element-type a))))))
    (assert (typep (funcall fun #A((1) (UNSIGNED-BYTE 8) 0))
                   '(simple-array (unsigned-byte 8) (10 20))))
    (assert
     (equal (sb-kernel:%simple-fun-type fun)
            '(function ((simple-array (unsigned-byte 8) (*)))
              (values (simple-array (unsigned-byte 8) (10 20)) &optional))))))

(with-test (:name :displaced-to-with-intitial)
  (checked-compile-and-assert
      ()
      `(lambda (x)
         (make-array 1 :displaced-to x :initial-element 1))
    ((#(0)) (condition 'error)))
  (assert
   (nth-value 2
              (checked-compile
               `(lambda ()
                  (lambda (x)
                    (make-array 1 :displaced-to (the vector x) :initial-contents '(1))))
               :allow-warnings t))))

(with-test (:name :check-bound-type-error)
  (assert (nth-value 2
                     (checked-compile
                      `(lambda (p)
                         (unless (svref p 0)
                           (svref p nil)))
                      :allow-warnings t))))

(with-test (:name :array-has-fill-pointer-p-folding)
  (assert (equal (sb-kernel:%simple-fun-type
                  (checked-compile `(lambda (x)
                                      (declare ((array * (* *)) x))
                                      (array-has-fill-pointer-p x))))
                 `(function ((array * (* *))) (values null &optional)))))

(with-test (:name :array-has-fill-pointer-p-transform)
  (checked-compile-and-assert
   ()
   `(lambda (n)
      (let ((a (make-array n)))
        (declare (vector a))
        (map-into a #'identity a)))
   ((0) #() :test #'equalp)))

(with-test (:name :displaced-index-offset-disallow-nil)
  (assert-error (eval '(make-array 4 :displaced-index-offset nil))))

(with-test (:name :adjust-array-copies-above-fill-pointer)
  (let ((a (make-array 4 :fill-pointer 2 :initial-contents '(a b c d))))
    (let ((b (adjust-array a 6 :initial-element 'e)))
      (assert (eq (aref b 2) 'c))
      (assert (eq (aref b 3) 'd))
      (assert (eq (aref b 4) 'e))
      (assert (eq (aref b 5) 'e)))))

(with-test (:name :test-array-dimensions-other-pointer-check)
  (checked-compile-and-assert
      ()
      `(lambda (a)
         (typep a '(simple-array t (2 1))))
    ((1) nil)
    ((#2A((1) (1))) t)))

(with-test (:name :typep-constant-%array-data-folding)
  (checked-compile-and-assert
      ()
      `(lambda ()
         (typep "abcd" '(simple-array t 2)))
    (() nil)))

(with-test (:name :vector-push-extend-specialized)
  (let ((extend (checked-compile `(lambda (e a)
                                    (vector-push-extend e a)
                                    a))))
    (loop for saetp across sb-vm:*specialized-array-element-type-properties*
          for type = (sb-vm:saetp-specifier saetp)
          when type
          do
          (let* ((value (sb-vm:saetp-initial-element-default saetp))
                 (value (if (characterp value)
                            (code-char (1+ (char-code value)))
                            (1+ value))))
            (assert (eql (aref (funcall extend value (make-array 1 :element-type type
                                                                   :adjustable t
                                                                   :fill-pointer t))
                               1)
                         value))))))

(with-test (:name :intersection-type-complexp)
  (assert (equal (caddr (sb-kernel:%simple-fun-type
                         (checked-compile `(lambda (x)
                                             (declare ((and (simple-array * (10))
                                                            (not simple-vector))
                                                       x))
                                             (length x)))))
                 `(values (integer 10 10) &optional))))

(with-test (:name :vector-length-intersection-types)
  (assert (equal (caddr (sb-kernel:%simple-fun-type
                         (checked-compile `(lambda (x)
                                             (declare ((and (or (simple-array * (11))
                                                                (simple-array * (12)))
                                                            (not simple-vector))
                                                       x))
                                             (length x)))))
                 `(values (integer 11 12) &optional))))

(with-test (:name :aref-dimension-checking)
  (checked-compile-and-assert
      (:optimize :safe)
      `(lambda (x)
         (aref x 0))
    ((#2A((1 2) (3 4))) (condition 'type-error))))

(with-test (:name :aref-constant-type-derive)
  (flet ((test (form type)
           (assert
            (type-specifiers-equal
             (caddr
              (sb-kernel:%simple-fun-type
               (checked-compile
                `(lambda (a)
                   ,form))))
             `(values ,type &optional)))))
    (test `(aref #(1 2 3) a)
          '(integer 1 3))
    (test `(svref #(1 2 3.0) a)
          '(or (integer 1 2) single-float))
    (test `(aref ,(make-array 3 :fill-pointer 2 :initial-contents #(1 2 3.0)) a)
          '(or (integer 1 2) single-float))))

(with-test (:name :make-array-initial-contents-zero-dimensions)
  (checked-compile-and-assert
      (:optimize :safe)
      `(lambda (d)
         (make-array d :initial-contents 1))
    ((nil) #0a1 :test #'equalp)))

(with-test (:name :negative-fill-pointer)
  (checked-compile-and-assert
   (:optimize :safe)
   `(lambda (a f)
      (setf (fill-pointer a) f))
   (((make-array 0 :fill-pointer 0) -1) (condition 'type-error))))

(with-test (:name :large-index
            :skipped-on (not :64-bit))
  (checked-compile
   `(lambda ()
      (make-array
       (1+ (ash 1 32))
       :element-type 'base-char
       :initial-element #\a)))
  (checked-compile
   `(lambda (x a)
      (setf (sbit x (ash 1 34)) a)))
  (checked-compile
   `(lambda (fn)
      (let ((s (make-string 536870910)))
        (declare (dynamic-extent s))
        (funcall fn s))))
  (checked-compile
   `(lambda (fn)
      (let ((s (make-string 536870910 :element-type 'base-char)))
        (declare (dynamic-extent s))
        (funcall fn s)))))

(with-test (:name :hairy-aref-check-bounds)
  (assert (= (count 'sb-kernel:%check-bound
                    (ctu:ir1-named-calls
                     `(lambda (x)
                        (declare ((vector t) x))
                        (aref x 0))
                     nil))
             0)))

(with-test (:name :setf-aref-simple-vector-from-new-value)
  (assert (not
           (ctu:ir1-named-calls
            `(lambda (x)
               (declare ((simple-array * (*)) x))
               (setf (aref x 0) 'm))))))

(with-test (:name :typep-displaced)
  (checked-compile-and-assert
      ()
      `(lambda (a)
         (typep a '(vector double-float)))
    (((make-array 1 :element-type 'double-float :displaced-to (make-array '(1 1) :element-type 'double-float))) t))
  (checked-compile-and-assert
      ()
      `(lambda (a)
         (typep a '(vector t 2)))
    (((make-array 2 :displaced-to (make-array '(2 1)))) t)))

(defun aindex (i array) (aref array i))
(defun 2d-aindex (i array) (aref array i i))
(defun seqindex (i seq) (elt seq i))

(with-test (:name :array-index-error-wording)
  ;; message contains a "should be ... below"
  (macrolet ((try (form)
               `(handler-case ,form
                  (:no-error (x) (error "Got ~S instead an error" x))
                  (condition (c)
                    (let ((str (princ-to-string c)))
                      (assert (search "Invalid index" str))
                      (assert (search "should be" str)))))))
    (try (aindex 5 #(1)))
    (try (2d-aindex 5 (make-array '(10 1))))
    (try (seqindex 5 #(1)))
    (try (seqindex 5 (make-array 9 :fill-pointer 1))))
  ;; message does not contains a "should be"
  (macrolet ((try (form)
               `(handler-case ,form
                  (:no-error (x) (error "Got ~S instead of an error" x))
                  (condition (c)
                    (let ((str (princ-to-string c)))
                      (assert (search "Invalid index" str))
                      (assert (not (search "should be" str)))
                      (assert (not (search "below 0" str))))))))
    (try (aindex 5 #()))
    (try (2d-aindex 5 (make-array '(10 0))))
    (try (seqindex 5 #()))
    (try (seqindex 5 (make-array 9 :fill-pointer 0)))))

(with-test (:name :fill-pointer-derive-type)
  (assert-type
   (lambda (n)
     (make-array n :element-type 'character :fill-pointer 0))
   (and (vector character) (not simple-array)))
  (assert-type
   (lambda (n f)
     (make-array n :element-type 'character :fill-pointer f))
   (array character)))
