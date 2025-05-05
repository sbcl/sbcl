;;;; miscellaneous tests of STRING-related stuff

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

;;; basic non-destructive case operations
(with-test (:name (string-upcase string-downcase string-capitalize :smoke))
  (assert (string= (string-upcase     "This is a test.") "THIS IS A TEST."))
  (assert (string= (string-downcase   "This is a test.") "this is a test."))
  (assert (string= (string-capitalize "This is a test.") "This Is A Test."))
  (assert (string= (string-upcase "Is this 900-Sex-hott, please?" :start 3)
                   "Is THIS 900-SEX-HOTT, PLEASE?"))
  (assert (string= (string-downcase "Is this 900-Sex-hott, please?"
                                    :start 10 :end 16)
                   "Is this 900-sex-hott, please?"))
  (assert (string= (string-capitalize "Is this 900-Sex-hott, please?")
                   "Is This 900-Sex-Hott, Please?")))

;;; The non-destructive case operations accept string designators, not
;;; just strings.

(with-test (:name (string-upcase string-downcase string-capitalize :designators))
  (assert (string= (string-upcase '|String designator|) "STRING DESIGNATOR"))
  (assert (string= (string-downcase #\S) "s"))
  (assert (string= (string-downcase #\.) "."))
  (assert (string= (string-capitalize 'ya-str-desig :end 5) "Ya-StR-DESIG")))

;;; basic destructive case operations
(with-test (:name (nstring-upcase nstring-downcase nstring-capitalize :smoke))
  (let ((nstring (make-array 5 :element-type 'character :fill-pointer 0)))
    (vector-push-extend #\c nstring)
    (vector-push-extend #\a nstring)
    (vector-push-extend #\t nstring)
    (nstring-upcase nstring)
    (assert (string= nstring "CAT"))
    (setf (fill-pointer nstring) 2)
    (nstring-downcase nstring :start 1)
    (setf (fill-pointer nstring) 3)
    (assert (string= nstring "CaT"))
    (nstring-capitalize nstring)
    (assert (string= nstring "Cat"))))

#|
(VECTOR NIL)s are strings only if you believe that the definition involving union
types prevails over the ample amount of exposition in the spec, as well as utility
of the resulting strings, and common sense.

I tested 9 other implementations, and 7 of them agree that:
  (SUBTYPEP '(SIMPLE-ARRAY NIL (*)) 'STRING) => NIL and T

They have differing behaviors as to effect of make-string and make-array as follows:

Allegro (10.1 Free Express Edition):
 (SIMPLE-ARRAY NIL (*)) exists but does not satisfy STRINGP
 (make-string 1 :element-type NIL) returns a (SIMPLE-ARRAY CHARACTER (1))

Clasp (built from git rev 6b8047c2 @ 2020-10-22):
 (make-array 1 :element-type NIL) signals error
 (make-string 1 :element-type NIL) returns a (SIMPLE-ARRAY CHARACTER (1))

Clozure (v1.12):
 (make-array 1 :element-type NIL) returns (SIMPLE-VECTOR 1)
 (make-string 1 :element-type NIL) returns a (SIMPLE-BASE-STRING 1)

CMUCL (version 20d Unicode):
 (make-array 1 :element-type nil) returns (SIMPLE-BASE-STRING 1)
 as does MAKE-STRING.

ECL (version 20.4.24):
 (make-array 1 :element-type NIL) says
  "ECL does not support arrays with element type NIL"
 (make-string 1 :element-type NIL) returns a (SIMPLE-ARRAY BASE-CHAR (1))

GCL (version 2.6.12 CLtL1):
 (type-of (make-array 1 :element-type nil)) => VECTOR
 (array-element-type (make-string 1 :element-type nil)) => STRING-CHAR

MKCL (version 1.1.11):
 (make-array 1 :element-type NIL) returns a (VECTOR NIL 1)
 (make-string 1 :element-type NIL) returns a (SIMPLE-BASE-STRING 1)

Of the dissenters, they return T and T from the subtypep expression above
though their behavioral differences from MAKE-ARRAY to MAKE-STRING
present compelling evidence that at least in their API they disfavor considering
an object a string if it has element type NIL, hence MAKE-STRING upgrades NIL
to a nonempty type.
ABCL (version 1.7.1):
 (type-of (make-array 1 :element-type nil)) => (NIL-VECTOR 1)
 (type-of (make-string 1 :element-type nil)) => (SIMPLE-BASE-STRING 1)
 Also: (make-string 1 :element-type 'ratio) => "^@" ; the type is just ignored
CLISP (version 2.49.92):
 (type-of (make-array 1 :element-type nil)) => (SIMPLE-ARRAY NIL (1))
 (type-of (make-string 1 :element-type nil)) => (SIMPLE-BASE-STRING 1)

And one more Lisp implementation which matches none of the above-
Lispworks personal edition 7.1:
  (make-array 1 :element-type nil) signals
    ERROR: (ARRAY NIL) is an illegal type specifier.
  (make-string 1 :element-type nil) signals
    ERROR: (ARRAY NIL) is an illegal type specifier.
  (subtypep '(vector nil) 'string) signals
    ERROR: (ARRAY NIL (*)) is an illegal type specifier.

Hence (SIMPLE-ARRAY NIL (*)) is most plausibly not regarded as a string,
the ANSI compliance test suite notwithstanding.
And the range of implementation choices suggest that users can not reasonably
claim that any particular result from these edge cases constitutes a bug.
|#
(with-test (:name (vector nil))
  (assert (not (stringp (make-array 0 :element-type nil))))
  (assert (not (subtypep (class-of (make-array 1 :element-type nil))
                         (find-class 'string)))))

(with-test (:name (make-string :element-type t type-error))
  (multiple-value-bind (fun failure-p warnings)
      (checked-compile '(lambda () (make-string 5 :element-type t))
                       :allow-failure t :allow-warnings t)
    (assert failure-p)
    (assert (= 1 (length warnings)))
    ;; It's not clear why this function should be expected to return a TYPE-ERROR.
    ;; There is no object created which is of the wrong type,
    ;; and the type of the legal value of ELEMENT-TYPE isn't really in question
    ;; nor is the best way to describe what you can't pass.
    (assert-error (funcall fun) #|type-error|#)))

;; MISC.574
(with-test (:name (string<= base-string :optimized))
  (assert (= (funcall (lambda (a)
                        (declare (optimize (speed 3) (safety 1)
                                           (debug 1) (space 2))
                                 (fixnum a))
                        (string<= (coerce "e99mo7yAJ6oU4" 'base-string)
                                  (coerce "aaABAAbaa" 'base-string)
                                  :start1 a))
                      9)
             9)))

;; String trimming.
(with-test (:name (string-trim 1))
  (flet ((make-test (string left right both)
           (macrolet ((check (fun wanted)
                        `(let ((result (,fun " " string)))
                           (assert (equal result ,wanted))
                           (when (equal string ,wanted)
                             ;; Check that the original string is
                             ;; returned when no changes are needed. Not
                             ;; required by the spec, but a desireable
                             ;; feature for performance.
                             (assert (eql result string))))))
             ;; Check the functional implementations
             (locally
                 (declare (notinline string-left-trim string-right-trim
                                     string-trim))
               (check string-left-trim left)
               (check string-right-trim right)
               (check string-trim both))
             ;; Check the transforms
             (locally
                 (declare (type simple-string string))
               (check string-left-trim left)
               (check string-right-trim right)
               (check string-trim both)))))
    (make-test "x " "x " "x" "x")
    (make-test " x" "x" " x" "x")
    (make-test " x " "x " " x" "x")
    (make-test " x x " "x x " " x x" "x x")))


;;; Trimming should respect fill-pointers
(with-test (:name (string-trim :fill-pointer))
  (let* ((s (make-array 9 :initial-contents "abcdabadd" :element-type
                        'character :fill-pointer 7))
         (s2 (string-left-trim "ab" s))
         (s3 (string-right-trim "ab" s)))
    (assert (equal "abcdaba" s))
    (assert (equal "cdaba" s2))
    (assert (equal "abcd" s3))))

;;; Trimming should replace displacement offsets
(with-test (:name (string-trim :displaced))
 (let* ((etype 'base-char)
        (s0
          (make-array '(6) :initial-contents "abcaeb" :element-type etype))
        (s
          (make-array '(3) :element-type etype :displaced-to s0 :displaced-index-offset 1)))
   (assert (equal "bc" (string-right-trim "ab" s)))
   (assert (equal "bca" s))
   (assert (equal "abcaeb" s0))))

(with-test (:name (string-trim :nothing-to-do))
  ;; Trimming non-simple-strings when there is nothing to do
  (let ((a (make-array 10 :element-type 'character :initial-contents "abcde00000" :fill-pointer 5)))
    (assert (equal "abcde" (string-right-trim "Z" a))))

  ;; Trimming non-strings when there is nothing to do.
  (string-right-trim " " #\a))

(with-test (:name :nil-vector-access)
  (let ((nil-vector (make-array 10 :element-type nil)))
    ;; Nowhere is it specified that PRINT-OBJECT on a VECTOR-NIL must
    ;; access the contents and signal an error. So WRITE works fine.
    (assert (write-to-string nil-vector))
    (let ((*read-eval* nil))
      (assert-error (write-to-string nil-vector :readably t)))
    (flet ((test (accessor)
             (assert-error
              (funcall (checked-compile
                        `(lambda ()
                           (,accessor (make-array 10 :element-type nil) 0))))
              sb-kernel:nil-array-accessed-error)
             (assert-error
              (funcall (checked-compile `(lambda (x) (,accessor x 0)))
                       nil-vector)
              sb-kernel:nil-array-accessed-error)))
      (test 'aref)
      ;; (test 'char)  ; you'll get a TYPE-ERROR instead.
      ;; (test 'schar) ; ditto
      (test 'row-major-aref))))

(with-test (:name :nil-array-access)
  (let ((nil-array (make-array '(10 10) :element-type nil)))
    (assert (write-to-string nil-array))
    (let ((*read-eval* nil))
      (assert-error (write-to-string nil-array :readably t)))
    (flet ((test (accessor args)
             (assert-error
              (funcall (checked-compile
                        `(lambda ()
                           (,accessor (make-array '(10 10) :element-type nil)
                                      ,@(make-list args :initial-element 0)))))
              sb-kernel:nil-array-accessed-error)
             (assert-error
              (funcall (checked-compile
                        `(lambda (x)
                           (,accessor x ,@(make-list args :initial-element 0))))
                       nil-array)
              sb-kernel:nil-array-accessed-error)))
      (test 'aref 2)
      (test 'row-major-aref 1))))

(with-test (:name (string-equal :two-arg))
  (flet ((check (function expected)
           (assert (eq (funcall
                        (checked-compile `(lambda (x y) (,function x y)))
                        "a"
                        (make-array 1 :element-type 'character
                                    :displaced-to "bA"
                                    :displaced-index-offset 1))
                       expected))))
    (check 'string-equal     t)
    (check 'string-not-equal nil)))

(with-test (:name :write-to-string-base-stringize)
  (let ((result (funcall (checked-compile `(lambda (x) (write-to-string x))) 33d0)))
    (assert (equal result "33.0d0"))
    (assert (typep result 'simple-base-string))))

(with-test (:name :make-string-fail-early)
  ;; This used to get "Heap exhausted"
  (assert-error
   (funcall (opaque-identity 'make-string) (truncate array-total-size-limit 2)
            :element-type '(unsigned-byte 8))))

(with-test (:name :make-string-initial-element-mismatch)
  ;; This used to be silently accepted (at least in the interpreter)
  (assert-error
   (funcall (opaque-identity 'make-string) 10
            :element-type '(member #\a #\b #\c) :initial-element nil)))

(with-test (:name :%sp-string-compare-argument-order)
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (string/= "_" (the simple-string x)))
   (("_") nil)
   (("a") 0)))

(with-test (:name :string=-derive-type)
  (macrolet
      ((check (fun expected)
         `(assert
           (equal (second
                   (third
                    (sb-kernel:%simple-fun-type
                     (checked-compile '(lambda (x y)
                                        (declare (ignorable x y))
                                        ,fun)))))
                  ',expected))))
    (check (string= (the (simple-string 1) x)
                    (the (simple-string 2) y)) null)
    (check (string= (the (simple-base-string 1) x)
                    (the (simple-base-string 2) y)) null)
    (check (string= (the (simple-array character (1)) x)
                    (the (simple-array character (2)) y)) null)))

(with-test (:name :string/=-derive-type)
  (macrolet
      ((check (fun expected)
         `(assert
           (type-specifiers-equal
            (second
             (third
              (sb-kernel:%simple-fun-type
               (checked-compile '(lambda (x y)
                                  (declare (ignorable x y))
                                  ,fun)))))
            ',expected))))
    (check (string/= (the (simple-string 4) x)
                     (the (simple-string 1) y)
                     :start1 1 :end1 * :end2 0) (or (integer 1 1) null))))

#+sb-unicode
(with-test (:name :possibly-base-stringize)
  ;;  simple-base, base non-simple
  (let* ((str (make-string 4 :element-type 'base-char))
         (res (sb-int:possibly-base-stringize str)))
    (assert (eq res str)))
  (let* ((str (make-array 4 :element-type 'base-char
                          :displaced-to (make-string 4 :element-type 'base-char)))
         (res (sb-int:possibly-base-stringize str)))
    (assert (and (sb-int:neq res str) (typep res 'simple-base-string))))
  ;;  simple-character w/ASCII only, non-simple character w/ASCII only
  (let* ((str (make-string 4))
         (res (sb-int:possibly-base-stringize str)))
    (assert (and (sb-int:neq res str) (typep res 'simple-base-string))))
  (let* ((str (make-array 4 :element-type 'character
                          :displaced-to (make-string 4)))
         (res (sb-int:possibly-base-stringize str)))
    (assert (and (sb-int:neq res str) (typep res 'simple-base-string))))
  ;; simple-character w/Unicode, non-simple character w/Unicode
  (let* ((str (make-string 4 :initial-element #\u3f4))
         (res (sb-int:possibly-base-stringize str)))
    (assert (and (eq res str) (typep res 'sb-kernel:simple-character-string))))
  (let* ((str (make-array 4 :element-type 'character
                          :displaced-to
                          (make-string 4 :initial-element #\u3f5)))
         (res (sb-int:possibly-base-stringize str)))
    (assert (and (sb-int:neq res str) (typep res 'sb-kernel:simple-character-string)))))
(with-test (:name :possibly-base-stringize-dx :skipped-on :interpreter)
  (let* ((str (make-string 4 :element-type 'base-char))
         (res (sb-int:possibly-base-stringize-to-heap str)))
    (declare (dynamic-extent str))
    (assert (if (sb-kernel:dynamic-space-obj-p str)
                (eq res str)
                (not (eq res str))))))

(with-test (:name :string-case-type)
  (macrolet
      ((check (fun expected)
         `(assert
           (type-specifiers-equal
            (second
             (third
              (sb-kernel:%simple-fun-type
               (checked-compile '(lambda (x)
                                  (declare (ignorable x))
                                  ,fun)))))
            ',expected))))
    (check (string-upcase nil)
           (simple-base-string 3))
    (check (string-upcase (the symbol x))
           simple-string)
    (check (string-upcase (the character x))
           (simple-string 1))))

(with-test (:name :downcase-ascii)
  (assert (string= (string-downcase (code-char 192))
                   (string (char-downcase (code-char 192))))))

(with-test (:name :find-on-string)
  (checked-compile-and-assert
   ()
   `(lambda (s)
      (declare (string s))
      (find #\1 s :test #'char/=))
   ((" ") #\Space)
   (("1") nil)))

(with-test (:name :inline-fill)
  (assert (equal (ctu:ir1-named-calls
                  `(lambda (x)
                     (make-string 2 :element-type x :initial-element #\a)))
                 '(sb-vm::%string-widetag-and-n-bits-shift))))
