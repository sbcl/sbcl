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

;;; (VECTOR NIL)s are strings.  Tests for that and issues uncovered in
;;; the process.
(with-test (:name (vector nil))
  (assert (typep (make-array 1 :element-type nil) 'string))
  (assert (not (typep (make-array 2 :element-type nil) 'base-string)))
  (assert (typep (make-string 3 :element-type nil) 'simple-string))
  (assert (not (typep (make-string 4 :element-type nil) 'simple-base-string)))

  (assert (subtypep (class-of (make-array 1 :element-type nil))
                    (find-class 'string)))
  (assert (subtypep (class-of (make-array 2 :element-type nil :fill-pointer 1))
                    (find-class 'string)))

  (assert (string= "" (make-array 0 :element-type nil)))
  (assert (string/= "a" (make-array 0 :element-type nil)))
  (assert (string= "" (make-array 5 :element-type nil :fill-pointer 0)))

  (assert (= (sxhash "")
             (sxhash (make-array 0 :element-type nil))
             (sxhash (make-array 5 :element-type nil :fill-pointer 0))
             (sxhash (make-string 0 :element-type nil))))
  (assert (subtypep (type-of (make-array 2 :element-type nil)) 'simple-string))
  (assert (subtypep (type-of (make-array 4 :element-type nil :fill-pointer t))
                    'string))

  (assert (eq (intern "") (intern (make-array 0 :element-type nil))))
  (assert (eq (intern "")
              (intern (make-array 5 :element-type nil :fill-pointer 0)))))

(with-test (:name (make-string :element-type t type-error))
  (multiple-value-bind (fun failure-p warnings)
      (checked-compile '(lambda () (make-string 5 :element-type t))
                       :allow-failure t :allow-warnings t)
    (assert failure-p)
    (assert (= 1 (length warnings)))
    (assert-error (funcall fun) type-error)))

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
    (assert-error (write-to-string nil-vector)
                  sb-kernel:nil-array-accessed-error)
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
      (test 'char)
      (test 'schar)
      (test 'row-major-aref))))

(with-test (:name :nil-array-access)
  (let ((nil-array (make-array '(10 10) :element-type nil)))
    (assert-error (write-to-string nil-array)
                  sb-kernel:nil-array-accessed-error)
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
