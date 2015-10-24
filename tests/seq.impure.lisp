;;;; tests related to sequences

;;;; This file is impure because we want to be able to use DEFUN.

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

(load "test-util.lisp")
(load "assertoid.lisp")

(defpackage :seq-test
  (:use :cl :assertoid :test-util))

(in-package :seq-test)

;;; user-defined mock sequence class for testing generic versions of
;;; sequence functions.
(defclass list-backed-sequence (standard-object
                                sequence)
  ((elements :initarg :elements :type list :accessor %elements)))

(defmethod sequence:make-sequence-like ((sequence list-backed-sequence) length
                                        &rest args &key
                                        initial-element initial-contents)
  (declare (ignore initial-element initial-contents))
  (make-instance 'list-backed-sequence
                 :elements (apply #'sequence:make-sequence-like
                                  '() length args)))

(defmethod sequence:length ((sequence list-backed-sequence))
  (length (%elements sequence)))

(defmethod sequence:elt
    ((sequence list-backed-sequence) index)
  (nth index (%elements sequence)))

(defmethod (setf sequence:elt)
    (new-value (sequence list-backed-sequence) index)
  (setf (nth index (%elements sequence)) new-value))

;;; helper functions for exercising SEQUENCE code on data of many
;;; specialized types, and in many different optimization scenarios
(defun for-every-seq-1 (base-seq snippet)
  (labels
      ((entirely (eltype)
         (every (lambda (el) (typep el eltype)) base-seq))
       (make-sequence-for-type (type)
         (etypecase type
           ((member list list-backed-sequence)
            (coerce base-seq type))
           ((cons (eql simple-array) (cons * (cons (eql 1) null)))
            (destructuring-bind (eltype one) (rest type)
              (declare (ignore one))
              (when (entirely eltype)
                (coerce base-seq type))))
           ((cons (eql vector))
            (destructuring-bind (eltype) (rest type)
              (when (entirely eltype)
                (let ((initial-element
                        (cond ((subtypep eltype 'character)
                               #\!)
                              ((subtypep eltype 'number)
                               0)
                                (t #'error))))
                  (replace (make-array
                            (+ (length base-seq)
                               (random 3))
                            :element-type eltype
                            :fill-pointer
                            (length base-seq)
                            :initial-element
                            initial-element)
                           base-seq))))))))
    (dolist (seq-type '(list
                        (simple-array t 1)
                        (vector t)
                        (simple-array character 1)
                        (vector character)
                        (simple-array (signed-byte 4) 1)
                        (vector (signed-byte 4))
                        list-backed-sequence))
      (dolist (declaredness '(nil t))
        (dolist (optimization '(((speed 3) (space 0))
                                ((speed 2) (space 2))
                                ((speed 1) (space 2))
                                ((speed 0) (space 1))))
          (let ((seq (make-sequence-for-type seq-type))
                (lambda-expr `(lambda (seq)
                                (declare (sb-ext:muffle-conditions
                                          sb-ext:compiler-note))
                                ,@(when declaredness
                                    `((declare (type ,seq-type seq))))
                                (declare (optimize ,@optimization))
                                ,snippet)))
            (when (not seq)
              (return))
            ;(format t "~&~S~%" lambda-expr)
            (multiple-value-bind (fun warnings-p failure-p)
                (compile nil lambda-expr)
              (when (or warnings-p failure-p)
                (error "~@<failed compilation:~2I ~_LAMBDA-EXPR=~S ~_WARNINGS-P=~S ~_FAILURE-P=~S~:@>"
                       lambda-expr warnings-p failure-p))
              ;(format t "~&~S ~S~%~S~%~S ~S~%"
              ;        base-seq snippet seq-type declaredness optimization)
              ;(format t "~&(TYPEP SEQ 'SIMPLE-ARRAY)=~S~%"
              ;        (typep seq 'simple-array))
              (unless (funcall fun seq)
                (error "~@<failed test:~2I ~_BASE-SEQ=~S ~_SNIPPET=~S ~_SEQ-TYPE=~S ~_DECLAREDNESS=~S ~_OPTIMIZATION=~S~:@>"
                       base-seq
                       snippet
                       seq-type
                       declaredness
                       optimization)))))))))
(defun for-every-seq (base-seq snippets)
  (dolist (snippet snippets)
    (for-every-seq-1 base-seq snippet)))

;;; a wrapper to hide declared type information from the compiler, so
;;; we don't get stopped by compiler warnings about e.g. compiling
;;; (POSITION 1 #() :KEY #'ABS) when #() has been coerced to a string.
(defun indiscriminate (fun)
  (lambda (&rest rest) (apply fun rest)))

;;; asymmetric test arg order example from ANSI FIND definition page
(assert (eql #\space ; original example, depends on ASCII character ordering
             (find #\d "here are some letters that can be looked at"
                   :test #'char>)))
(assert (eql #\e ; modified example, depends only on standard a-z ordering
             (find #\f "herearesomeletters" :test #'char>)))
(assert (eql 4 ; modified more, avoids charset technicalities completely
             (find 5 '(6 4) :test '>)))

(with-test (:name sequence:emptyp)
  (for-every-seq #()
    '((eq t (sequence:emptyp seq))))
  (for-every-seq #(1)
    '((eq nil (sequence:emptyp seq)))))

;;; tests of FIND, POSITION, FIND-IF, and POSITION-IF (and a few for
;;; deprecated FIND-IF-NOT and POSITION-IF-NOT too)
(for-every-seq #()
  '((null (find 1 seq))
    (null (find 1 seq :from-end t))
    (null (position 1 seq :key (indiscriminate #'abs)))
    (null (position nil seq :test (constantly t)))
    (null (position nil seq :test nil))
    (null (position nil seq :test-not nil))
    (null (find-if #'1+ seq :key (indiscriminate #'log)))
    (null (position-if #'identity seq :from-end t))
    (null (find-if-not #'packagep seq))
    (null (position-if-not #'packagep seq :key nil))))
(for-every-seq #(1)
  '((null (find 2 seq))
    ;; Get the argument ordering for asymmetric tests like #'> right.
    ;; (bug reported and fixed by Alexey Dejneka sbcl-devel 2001-10-17)
    (eql 1 (find 2 seq :test #'>))
    (find 2 seq :key #'1+)
    (find 1 seq :from-end t)
    (null (find 1 seq :from-end t :start 1))
    (null (find 0 seq :from-end t))
    (eql 0 (position 1 seq :key #'abs))
    (null (position nil seq :test 'equal))
    (eql 1 (find-if #'1- seq :key #'log))
    (eql 0 (position-if #'identity seq :from-end t))
    (null (find-if-not #'sin seq))
    (eql 0 (position-if-not #'packagep seq :key 'identity))))
(for-every-seq #(1 2 3 2 1)
  '((find 3 seq)
    (find 3 seq :from-end 'yes)
    (eql 1 (position 1.5 seq :test #'<))
    (eql 0 (position 0 seq :key '1-))
    (eql 4 (position 0 seq :key '1- :from-end t))
    (eql 2 (position 4 seq :key '1+))
    (eql 2 (position 4 seq :key '1+ :from-end t))
    (eql 1 (position 2 seq))
    (eql 1 (position 2 seq :start 1))
    (null (find 2 seq :start 1 :end 1))
    (eql 3 (position 2 seq :start 2))
    (eql 3 (position 2 seq :key nil :from-end t))
    (eql 2 (position 3 seq :test '=))
    (eql 0 (position 3 seq :test-not 'equalp))
    (eql 2 (position 3 seq :test 'equal :from-end t))
    (null (position 4 seq :test #'eql))
    (null (find-if #'packagep seq))
    (eql 1 (find-if #'plusp seq))
    (eql 3 (position-if #'plusp seq :key #'1- :from-end t))
    (eql 1 (position-if #'evenp seq))
    (eql 3 (position-if #'evenp seq :from-end t))
    (eql 2 (position-if #'plusp seq :from-end nil :key '1- :start 2))
    (eql 3 (position-if #'plusp seq :from-end t :key '1- :start 2))
    (null (position-if #'plusp seq :from-end t :key '1- :start 2 :end 2))
    (null (find-if-not #'plusp seq))
    (eql 0 (position-if-not #'evenp seq))
    (eql 0 (search #(1) seq))
    (eql 1 (search #(4 5) seq :key 'oddp))
    (eql 1 (search #(-2) seq :test (lambda (a b) (= (- a) b))))
    (eql 4 (search #(1) seq :start2 1))
    (null (search #(3) seq :start2 3))
    (eql 2 (search #(3) seq :start2 2))
    (eql 0 (search #(1 2) seq))
    (null (search #(2 1 3) seq))
    (eql 0 (search #(0 1 2 4) seq :start1 1 :end1 3))
    (eql 3 (search #(0 2 1 4) seq :start1 1 :end1 3))
    (eql 4 (search #(1) seq :from-end t))
    (eql 0 (search #(1 2) seq :from-end t))
    (null (search #(1 2) seq :from-end t :start2 1))
    (eql 0 (search #(0 1 2 4) seq :from-end t :start1 1 :end1 3))
    (eql 3 (search #(0 2 1 4) seq :from-end t :start1 1 :end1 3))
    (null (search #(2 1 3) seq :from-end t))))
(for-every-seq "string test"
  '((null (find 0 seq))
    (null (find #\D seq :key #'char-upcase))
    (find #\E seq :key #'char-upcase)
    (null (find #\e seq :key #'char-upcase))
    (eql 3 (position #\i seq))
    (eql 0 (position #\s seq :key #'char-downcase))
    (eql 1 (position #\s seq :key #'char-downcase :test #'char/=))
    (eql 9 (position #\s seq :from-end t :test #'char=))
    (eql 10 (position #\s seq :from-end t :test #'char/=))
    (eql 4 (position #\N seq :from-end t :key 'char-upcase :test #'char-equal))
    (eql 5 (position-if (lambda (c) (equal #\g c)) seq))
    (eql 5 (position-if (lambda (c) (equal #\g c)) seq :from-end t))
    (find-if #'characterp seq)
    (find-if (lambda (c) (typep c 'base-char)) seq :from-end t)
    (null (find-if 'upper-case-p seq))))

;;; SUBSEQ
(with-test (:name :subseq)
  (let ((avec (make-array 10
                          :fill-pointer 4
                          :initial-contents '(0 1 2 3 iv v vi vii iix ix))))
    ;; These first five always worked AFAIK.
    (assert (equalp (subseq avec 0 3) #(0 1 2)))
    (assert (equalp (subseq avec 3 3) #()))
    (assert (equalp (subseq avec 1 3) #(1 2)))
    (assert (equalp (subseq avec 1) #(1 2 3)))
    (assert (equalp (subseq avec 1 4) #(1 2 3)))
    ;; SBCL bug found ca. 2002-05-01 by OpenMCL's correct handling of
    ;; SUBSEQ, CSR's driving portable cross-compilation far enough to
    ;; reach the SUBSEQ calls in assem.lisp, and WHN's sleazy
    ;; translation of old CMU CL new-assem.lisp into sufficiently grotty
    ;; portable Lisp that it passed suitable illegal values to SUBSEQ to
    ;; exercise the bug:-|
    ;;
    ;; SUBSEQ should check its END value against logical LENGTH, not
    ;; physical ARRAY-DIMENSION 0.
    ;;
    ;; fixed in sbcl-0.7.4.22 by WHN
    (assert (null (ignore-errors (aref (subseq avec 1 5) 0))))))

;;; FILL
(defun test-fill-typecheck (x)
  (declare (optimize (safety 3) (space 2) (speed 1)))
  (fill (make-string 10) x))

(assert (string= (test-fill-typecheck #\@) "@@@@@@@@@@"))
;;; BUG 186, fixed in sbcl-0.7.5.5
(assert (null (ignore-errors (test-fill-typecheck 4097))))

;;; MAKE-SEQUENCE, COERCE, CONCATENATE, MERGE, MAP and requested
;;; result type (BUGs 46a, 46b, 66)
(with-test (:name :sequence-functions)
  (macrolet ((assert-type-error (form)
               `(assert (typep (nth-value 1 (ignore-errors ,form))
                               'type-error))))
    (dolist (type-stub '((simple-vector)
                         (vector *)
                         (vector (signed-byte 8))
                         (vector (unsigned-byte 16))
                         (vector (signed-byte 32))
                         (simple-bit-vector)))
      (declare (optimize safety))
      (format t "~&~S~%" type-stub)
      ;; MAKE-SEQUENCE
      (assert (= (length (make-sequence `(,@type-stub) 10)) 10))
      (assert (= (length (make-sequence `(,@type-stub 10) 10)) 10))
      (assert-type-error (make-sequence `(,@type-stub 10) 11))
      ;; COERCE
      (assert (= (length (coerce '(0 0 0) `(,@type-stub))) 3))
      (assert (= (length (coerce #(0 0 0) `(,@type-stub 3))) 3))
      (assert-type-error (coerce #*111 `(,@type-stub 4)))
      ;; CONCATENATE
      (assert (= (length (concatenate `(,@type-stub) #(0 0 0) #*111)) 6))
      (assert (equalp (concatenate `(,@type-stub) #(0 0 0) #*111)
                      (coerce #(0 0 0 1 1 1) `(,@type-stub))))
      (assert (= (length (concatenate `(,@type-stub 6) #(0 0 0) #*111)) 6))
      (assert (equalp (concatenate `(,@type-stub 6) #(0 0 0) #*111)
                      (coerce #(0 0 0 1 1 1) `(,@type-stub 6))))
      (assert-type-error (concatenate `(,@type-stub 5) #(0 0 0) #*111))
      ;; MERGE
      (macrolet ((test (type)
                   `(merge ,type (copy-seq #(0 1 0)) (copy-seq #*111) #'>)))
        (assert (= (length (test `(,@type-stub))) 6))
        (assert (equalp (test `(,@type-stub))
                        (coerce #(1 1 1 0 1 0) `(,@type-stub))))
        (assert (= (length (test `(,@type-stub 6))) 6))
        (assert (equalp (test `(,@type-stub 6))
                        (coerce #(1 1 1 0 1 0) `(,@type-stub 6))))
        (assert-type-error (test `(,@type-stub 4))))
      ;; MAP
      (assert (= (length (map `(,@type-stub) #'logxor #(0 0 1 1) '(0 1 0 1))) 4))
      (assert (equalp (map `(,@type-stub) #'logxor #(0 0 1 1) '(0 1 0 1))
                      (coerce #(0 1 1 0) `(,@type-stub))))
      (assert (= (length (map `(,@type-stub 4) #'logxor #(0 0 1 1) '(0 1 0 1)))
                 4))
      (assert (equalp (map `(,@type-stub 4) #'logxor #(0 0 1 1) '(0 1 0 1))
                      (coerce #(0 1 1 0) `(,@type-stub 4))))
      (assert-type-error (map `(,@type-stub 5) #'logxor #(0 0 1 1) '(0 1 0 1))))
    ;; some more CONCATENATE tests for strings
    (locally
        (declare (optimize safety))
      (assert (string= (concatenate 'string "foo" " " "bar") "foo bar"))
      (assert (string= (concatenate '(string 7) "foo" " " "bar") "foo bar"))
      (assert-type-error (concatenate '(string 6) "foo" " " "bar"))
      (assert (string= (concatenate '(string 6) "foo" #(#\b #\a #\r)) "foobar"))
      (assert (string= (concatenate '(string 6) #(#\b #\a #\r) "foo") "barfoo"))
      (assert-type-error (concatenate '(string 7) "foo" #(#\b #\a #\r))))
    ;; Non-VECTOR ARRAY types aren't allowed as vector type specifiers.
    (locally
        (declare (optimize safety))
      (assert-type-error (concatenate 'simple-array "foo" "bar"))
      (assert-type-error (map 'simple-array #'identity '(1 2 3)))
      (assert (equalp #(11 13)
                      (map '(simple-array fixnum (*)) #'+ '(1 2 3) '(10 11))))
      (assert-type-error (coerce '(1 2 3) 'simple-array))
      (assert-type-error (merge 'simple-array (list 1 3) (list 2 4) '<))
      (assert (equalp #(3 2 1) (coerce '(3 2 1) '(vector fixnum))))
      (assert-type-error (map 'array #'identity '(1 2 3)))
      (assert-type-error (map '(array fixnum) #'identity '(1 2 3)))
      (assert (equalp #(1 2 3) (coerce '(1 2 3) '(vector fixnum))))
      ;; but COERCE has an exemption clause:
      (assert (string= "foo" (coerce "foo" 'simple-array)))
      ;; ... though not in all cases.
      (assert-type-error (coerce '(#\f #\o #\o) 'simple-array)))))

;; CONCATENATE used to fail for generic sequences for result-type NULL.
(with-test (:name (concatenate :result-type-null :bug-1162301))
  (assert (sequence:emptyp (concatenate 'null)))

  (for-every-seq #()
    '((sequence:emptyp (concatenate 'null seq))
      (sequence:emptyp (concatenate 'null seq seq))
      (sequence:emptyp (concatenate 'null seq #()))
      (sequence:emptyp (concatenate 'null seq ""))))

  (for-every-seq #(1)
    (mapcar (lambda (form)
              `(typep (nth-value 1 (ignore-errors ,form)) 'type-error))
            '((concatenate 'null seq)
              (concatenate 'null seq seq)
              (concatenate 'null seq #())
              (concatenate 'null seq "2")))))

;;; As pointed out by Raymond Toy on #lisp IRC, MERGE had some issues
;;; with user-defined types until sbcl-0.7.8.11
(deftype list-typeoid () 'list)
(with-test (:name :merge-user-types)
 (assert (equal '(1 2 3 4) (merge 'list-typeoid (list 1 3) (list 2 4) '<)))
;;; and also with types that weren't precicely LIST
 (assert (equal '(1 2 3 4) (merge 'cons (list 1 3) (list 2 4) '<))))

;;; but wait, there's more! The NULL and CONS types also have implicit
;;; length requirements:
(with-test (:name :sequence-functions-list-types)
  (macrolet ((assert-type-error (form)
               `(assert (typep (nth-value 1 (ignore-errors ,form))
                               'type-error))))
    (locally
        (declare (optimize safety))
      ;; MAKE-SEQUENCE
      (assert-type-error (make-sequence 'cons 0))
      (assert-type-error (make-sequence 'null 1))
      (assert-type-error (make-sequence '(cons t null) 0))
      (assert-type-error (make-sequence '(cons t null) 2))
      ;; KLUDGE: I'm not certain that this test actually tests for what
      ;; it should test, in that the type deriver and optimizers might
      ;; be too smart for the good of an exhaustive test system.
      ;; However, it makes me feel good.  -- CSR, 2002-10-18
      (assert (null (make-sequence 'null 0)))
      (assert (= (length (make-sequence 'cons 3)) 3))
      (assert (= (length (make-sequence '(cons t null) 1)) 1))
      ;; and NIL is not a valid type for MAKE-SEQUENCE
      (assert-type-error (make-sequence 'nil 0))
      ;; COERCE
      (assert-type-error (coerce #(1) 'null))
      (assert-type-error (coerce #() 'cons))
      (assert-type-error (coerce #() '(cons t null)))
      (assert-type-error (coerce #(1 2) '(cons t null)))
      (assert (null (coerce #() 'null)))
      (assert (= (length (coerce #(1) 'cons)) 1))
      (assert (= (length (coerce #(1) '(cons t null))) 1))
      (assert-type-error (coerce #() 'nil))
      ;; MERGE
      (assert-type-error (merge 'null (list 1 3) (list 2 4) '<))
      (assert-type-error (merge 'cons () () '<))
      (assert (null (merge 'null () () '<)))
      (assert (= (length (merge 'cons (list 1 3) (list 2 4) '<)) 4))
      (assert (= (length (merge '(cons t (cons t (cons t (cons t null))))
                                (list 1 3) (list 2 4)
                                '<))
                 4))
      (assert-type-error (merge 'nil () () '<))
      ;; CONCATENATE
      (assert-type-error (concatenate 'cons #() ()))
      (assert-type-error (concatenate '(cons t null) #(1 2 3) #(4 5 6)))
      (assert (= (length (concatenate 'cons #() '(1) "2 3")) 4))
      (assert (= (length (concatenate '(cons t cons) '(1) "34")) 3))
      (assert-type-error (concatenate 'nil '(3)))
      ;; FIXME: tests for MAP to come when some brave soul implements
      ;; the analogous type checking for MAP/%MAP.
      )))

;;; ELT should signal an error of type TYPE-ERROR if its index
;;; argument isn't a valid sequence index for sequence:
(defun test-elt-signal (x)
  (elt x 3))
(assert-error (test-elt-signal "foo") type-error)
(assert (eql (test-elt-signal "foob") #\b))
(locally
    (declare (optimize (safety 3)))
  (assert-error (elt (list 1 2 3) 3) type-error))

;;; confusion in the refactoring led to this signalling an unbound
;;; variable, not a type error.
(defun svrefalike (x)
  (svref x 0))
(assert-error (svrefalike #*0) type-error)

;;; checks for uniform bounding index handling.
;;;
;;; This used to be SAFETY 3 only, but bypassing these checks with
;;; above-zero speed when SPEED > SAFETY is not The SBCL Way.
;;;
;;; KLUDGE: not all in one big form because that causes SBCL to spend
;;; an absolute age trying to compile it.
(defmacro sequence-bounding-indices-test (&body body)
  `(progn
     (locally
    ;; See Issues 332 [and 333(!)] in the CLHS
    (declare (optimize (speed 3) (safety 1)))
    (let ((string (make-array 10
                              :fill-pointer 5
                              :initial-element #\a
                              :element-type 'base-char)))
        ,(car body)
        (format t "... BASE-CHAR")
        (finish-output)
        (flet ((reset ()
                 (setf (fill-pointer string) 10)
                 (fill string #\a)
                 (setf (fill-pointer string) 5)))
          (declare (ignorable #'reset))
          ,@(cdr body))))
    (locally
      ;; See Issues 332 [and 333(!)] in the CLHS
      (declare (optimize (speed 3) (safety 1)))
      (let ((string (make-array 10
                                :fill-pointer 5
                                :initial-element #\a
                                :element-type 'character)))
        ,(car body)
        (format t "... CHARACTER")
        (finish-output)
      (flet ((reset ()
               (setf (fill-pointer string) 10)
               (fill string #\a)
               (setf (fill-pointer string) 5)))
        (declare (ignorable #'reset))
          ,@(cdr body))))))

(declaim (notinline opaque-identity))
(defun opaque-identity (x) x)
;;; Accessor SUBSEQ
(sequence-bounding-indices-test
 (format t "~&/Accessor SUBSEQ")
 (assert (string= (subseq string 0 5) "aaaaa"))
 (assert-error (subseq string 0 6))
 (assert-error (subseq string (opaque-identity -1) 5))
 (assert-error (subseq string 4 2))
 (assert-error (subseq string 6))
 (assert (string= (setf (subseq string 0 5) "abcde") "abcde"))
 (assert (string= (subseq string 0 5) "abcde"))
 (reset)
 (assert-error (setf (subseq string 0 6) "abcdef"))
 (assert-error (setf (subseq string (opaque-identity -1) 5) "abcdef"))
 (assert-error (setf (subseq string 4 2) ""))
 (assert-error (setf (subseq string 6) "ghij")))

;;; Function COUNT, COUNT-IF, COUNT-IF-NOT
(sequence-bounding-indices-test
 (format t "~&/Function COUNT, COUNT-IF, COUNT-IF-NOT")
 (assert (= (count #\a string :start 0 :end nil) 5))
 (assert (= (count #\a string :start 0 :end 5) 5))
 (assert-error (count #\a string :start 0 :end 6))
 (assert-error (count #\a string :start (opaque-identity -1) :end 5))
 (assert-error (count #\a string :start 4 :end 2))
 (assert-error (count #\a string :start 6 :end 9))
 (assert (= (count-if #'alpha-char-p string :start 0 :end nil) 5))
 (assert (= (count-if #'alpha-char-p string :start 0 :end 5) 5))
 (assert-error
  (count-if #'alpha-char-p string :start 0 :end 6))
 (assert-error
  (count-if #'alpha-char-p string :start (opaque-identity -1) :end 5))
 (assert-error
  (count-if #'alpha-char-p string :start 4 :end 2))
 (assert-error
  (count-if #'alpha-char-p string :start 6 :end 9))
 (assert (= (count-if-not #'alpha-char-p string :start 0 :end nil) 0))
 (assert (= (count-if-not #'alpha-char-p string :start 0 :end 5) 0))
 (assert-error
  (count-if-not #'alpha-char-p string :start 0 :end 6))
 (assert-error
  (count-if-not #'alpha-char-p string :start (opaque-identity -1) :end 5))
 (assert-error
  (count-if-not #'alpha-char-p string :start 4 :end 2))
 (assert-error
  (count-if-not #'alpha-char-p string :start 6 :end 9)))

;;; Function FILL
(sequence-bounding-indices-test
 (format t "~&/Function FILL")
 (assert (string= (fill string #\b :start 0 :end 5) "bbbbb"))
 (assert (string= (fill string #\c :start 0 :end nil) "ccccc"))
 (assert-error (fill string #\d :start 0 :end 6))
 (assert-error (fill string #\d :start (opaque-identity -1) :end 5))
 (assert-error (fill string #\d :start 4 :end 2))
 (assert-error (fill string #\d :start 6 :end 9)))

;;; Function FIND, FIND-IF, FIND-IF-NOT
(sequence-bounding-indices-test
 (format t "~&/Function FIND, FIND-IF, FIND-IF-NOT")
 (assert (char= (find #\a string :start 0 :end nil) #\a))
 (assert (char= (find #\a string :start 0 :end 5) #\a))
 (assert-error (find #\a string :start 0 :end 6))
 (assert-error (find #\a string :start (opaque-identity -1) :end 5))
 (assert-error (find #\a string :start 4 :end 2))
 (assert-error (find #\a string :start 6 :end 9))
 (assert (char= (find-if #'alpha-char-p string :start 0 :end nil) #\a))
 (assert (char= (find-if #'alpha-char-p string :start 0 :end 5) #\a))
 (assert-error
  (find-if #'alpha-char-p string :start 0 :end 6))
 (assert-error
  (find-if #'alpha-char-p string :start (opaque-identity -1) :end 5))
 (assert-error
  (find-if #'alpha-char-p string :start 4 :end 2))
 (assert-error
  (find-if #'alpha-char-p string :start 6 :end 9))
 (assert (eq (find-if-not #'alpha-char-p string :start 0 :end nil) nil))
 (assert (eq (find-if-not #'alpha-char-p string :start 0 :end 5) nil))
 (assert-error
  (find-if-not #'alpha-char-p string :start 0 :end 6))
 (assert-error
  (find-if-not #'alpha-char-p string :start (opaque-identity -1) :end 5))
 (assert-error
  (find-if-not #'alpha-char-p string :start 4 :end 2))
 (assert-error
  (find-if-not #'alpha-char-p string :start 6 :end 9)))

;;; Function MISMATCH
(sequence-bounding-indices-test
 (format t "~&/Function MISMATCH")
 (assert (null (mismatch string "aaaaa" :start1 0 :end1 nil)))
 (assert (= (mismatch "aaab" string :start2 0 :end2 4) 3))
 (assert-error (mismatch "aaaaaa" string :start2 0 :end2 6))
 (assert-error (mismatch string "aaaaaa" :start1 (opaque-identity -1) :end1 5))
 (assert-error (mismatch string "" :start1 4 :end1 2))
 (assert-error (mismatch "aaaa" string :start2 6 :end2 9)))

;;; Function PARSE-INTEGER
(sequence-bounding-indices-test
 (format t "~&/Function PARSE-INTEGER")
 (setf (fill-pointer string) 10)
 (setf (subseq string 0 10) "1234567890")
 (setf (fill-pointer string) 5)
 (assert (= (parse-integer string :start 0 :end 5) 12345))
 (assert (= (parse-integer string :start 0 :end nil) 12345))
 (assert-error (parse-integer string :start 0 :end 6))
 (assert-error (parse-integer string :start (opaque-identity -1) :end 5))
 (assert-error (parse-integer string :start 4 :end 2))
 (assert-error (parse-integer string :start 6 :end 9)))

;;; Function PARSE-NAMESTRING
(sequence-bounding-indices-test
 (format t "~&/Function PARSE-NAMESTRING")
 (setf (fill-pointer string) 10)
 (setf (subseq string 0 10)
       #-win32 "/dev/ /tmp"
       #+win32 "C:/   NUL")
 (setf (fill-pointer string) 5)
 (assert (truename (parse-namestring string nil *default-pathname-defaults*
                                     :start 0 :end 5)))
 (assert (truename (parse-namestring string nil *default-pathname-defaults*
                                     :start 0 :end nil)))
 (assert-error (parse-namestring string nil
                                 *default-pathname-defaults*
                                 :start 0 :end 6))
 (assert-error (parse-namestring string nil
                                 *default-pathname-defaults*
                                 :start (opaque-identity -1) :end 5))
 (assert-error (parse-namestring string nil
                                 *default-pathname-defaults*
                                 :start 4 :end 2))
 (assert-error (parse-namestring string nil
                                 *default-pathname-defaults*
                                 :start 6 :end 9)))

;;; Function POSITION, POSITION-IF, POSITION-IF-NOT
(sequence-bounding-indices-test
 (format t "~&/Function POSITION, POSITION-IF, POSITION-IF-NOT")

 (assert (= (position #\a string :start 0 :end nil) 0))
 (assert (= (position #\a string :start 0 :end 5) 0))
 (assert-error (position #\a string :start 0 :end 6))
 (assert-error (position #\a string :start (opaque-identity -1) :end 5))
 (assert-error (position #\a string :start 4 :end 2))
 (assert-error (position #\a string :start 6 :end 9))
 (assert (= (position-if #'alpha-char-p string :start 0 :end nil) 0))
 (assert (= (position-if #'alpha-char-p string :start 0 :end 5) 0))
 (assert-error
  (position-if #'alpha-char-p string :start 0 :end 6))
 (assert-error
  (position-if #'alpha-char-p string :start (opaque-identity -1) :end 5))
 (assert-error
  (position-if #'alpha-char-p string :start 4 :end 2))
 (assert-error
  (position-if #'alpha-char-p string :start 6 :end 9))
 (assert (eq (position-if-not #'alpha-char-p string :start 0 :end nil) nil))
 (assert (eq (position-if-not #'alpha-char-p string :start 0 :end 5) nil))
 (assert-error
  (position-if-not #'alpha-char-p string :start 0 :end 6))
 (assert-error
  (position-if-not #'alpha-char-p string :start (opaque-identity -1) :end 5))
 (assert-error
  (position-if-not #'alpha-char-p string :start 4 :end 2))
 (assert-error
  (position-if-not #'alpha-char-p string :start 6 :end 9)))

;;; Function READ-FROM-STRING
(sequence-bounding-indices-test
 (format t "~&/Function READ-FROM-STRING")
 (setf (subseq string 0 5) "(a b)")
 (assert (equal (read-from-string string nil nil :start 0 :end 5) '(a b)))
 (assert (equal (read-from-string string nil nil :start 0 :end nil) '(a b)))
 (assert-error (read-from-string string nil nil :start 0 :end 6))
 (assert-error (read-from-string string nil nil :start (opaque-identity -1) :end 5))
 (assert-error (read-from-string string nil nil :start 4 :end 2))
 (assert-error (read-from-string string nil nil :start 6 :end 9)))

;;; Function REDUCE
(sequence-bounding-indices-test
 (format t "~&/Function REDUCE")
 (setf (subseq string 0 5) "abcde")
 (assert (equal (reduce #'list* string :from-end t :start 0 :end nil)
                '(#\a #\b #\c #\d . #\e)))
 (assert (equal (reduce #'list* string :from-end t :start 0 :end 5)
                '(#\a #\b #\c #\d . #\e)))
 (assert-error (reduce #'list* string :start 0 :end 6))
 (assert-error (reduce #'list* string :start (opaque-identity -1) :end 5))
 (assert-error (reduce #'list* string :start 4 :end 2))
 (assert-error (reduce #'list* string :start 6 :end 9)))

;;; Function REMOVE, REMOVE-IF, REMOVE-IF-NOT, DELETE, DELETE-IF,
;;; DELETE-IF-NOT
(sequence-bounding-indices-test
 (format t "~&/Function REMOVE, REMOVE-IF, REMOVE-IF-NOT, ...")
 (assert (equal (remove #\a string :start 0 :end nil) ""))
 (assert (equal (remove #\a string :start 0 :end 5) ""))
 (assert-error (remove #\a string :start 0 :end 6))
 (assert-error (remove #\a string :start (opaque-identity -1) :end 5))
 (assert-error (remove #\a string :start 4 :end 2))
 (assert-error (remove #\a string :start 6 :end 9))
 (assert (equal (remove-if #'alpha-char-p string :start 0 :end nil) ""))
 (assert (equal (remove-if #'alpha-char-p string :start 0 :end 5) ""))
 (assert-error
  (remove-if #'alpha-char-p string :start 0 :end 6))
 (assert-error
  (remove-if #'alpha-char-p string :start (opaque-identity -1) :end 5))
 (assert-error
  (remove-if #'alpha-char-p string :start 4 :end 2))
 (assert-error
  (remove-if #'alpha-char-p string :start 6 :end 9))
 (assert (equal (remove-if-not #'alpha-char-p string :start 0 :end nil)
                "aaaaa"))
 (assert (equal (remove-if-not #'alpha-char-p string :start 0 :end 5)
                "aaaaa"))
 (assert-error
  (remove-if-not #'alpha-char-p string :start 0 :end 6))
 (assert-error
  (remove-if-not #'alpha-char-p string :start (opaque-identity -1) :end 5))
 (assert-error
  (remove-if-not #'alpha-char-p string :start 4 :end 2))
 (assert-error
  (remove-if-not #'alpha-char-p string :start 6 :end 9)))
(sequence-bounding-indices-test
 (format t "~&/... DELETE, DELETE-IF, DELETE-IF-NOT")
 (assert (equal (delete #\a string :start 0 :end nil) ""))
 (reset)
 (assert (equal (delete #\a string :start 0 :end 5) ""))
 (reset)
 (assert-error (delete #\a string :start 0 :end 6))
 (reset)
 (assert-error (delete #\a string :start (opaque-identity -1) :end 5))
 (reset)
 (assert-error (delete #\a string :start 4 :end 2))
 (reset)
 (assert-error (delete #\a string :start 6 :end 9))
 (reset)
 (assert (equal (delete-if #'alpha-char-p string :start 0 :end nil) ""))
 (reset)
 (assert (equal (delete-if #'alpha-char-p string :start 0 :end 5) ""))
 (reset)
 (assert-error
  (delete-if #'alpha-char-p string :start 0 :end 6))
 (reset)
 (assert-error
  (delete-if #'alpha-char-p string :start (opaque-identity -1) :end 5))
 (reset)
 (assert-error
  (delete-if #'alpha-char-p string :start 4 :end 2))
 (reset)
 (assert-error
  (delete-if #'alpha-char-p string :start 6 :end 9))
 (reset)
 (assert (equal (delete-if-not #'alpha-char-p string :start 0 :end nil)
                "aaaaa"))
 (reset)
 (assert (equal (delete-if-not #'alpha-char-p string :start 0 :end 5)
                "aaaaa"))
 (reset)
 (assert-error
  (delete-if-not #'alpha-char-p string :start 0 :end 6))
 (reset)
 (assert-error
  (delete-if-not #'alpha-char-p string :start (opaque-identity -1) :end 5))
 (reset)
 (assert-error
  (delete-if-not #'alpha-char-p string :start 4 :end 2))
 (reset)
 (assert-error
  (delete-if-not #'alpha-char-p string :start 6 :end 9)))

;;; Function REMOVE-DUPLICATES, DELETE-DUPLICATES
(sequence-bounding-indices-test
 (format t "~&/Function REMOVE-DUPLICATES, DELETE-DUPLICATES")
 (assert (string= (remove-duplicates string :start 0 :end 5) "a"))
 (assert (string= (remove-duplicates string :start 0 :end nil) "a"))
 (assert-error (remove-duplicates string :start 0 :end 6))
 (assert-error (remove-duplicates string :start (opaque-identity -1) :end 5))
 (assert-error (remove-duplicates string :start 4 :end 2))
 (assert-error (remove-duplicates string :start 6 :end 9))
 (assert (string= (delete-duplicates string :start 0 :end 5) "a"))
 (reset)
 (assert (string= (delete-duplicates string :start 0 :end nil) "a"))
 (reset)
 (assert-error (delete-duplicates string :start 0 :end 6))
 (reset)
 (assert-error (delete-duplicates string :start (opaque-identity -1) :end 5))
 (reset)
 (assert-error (delete-duplicates string :start 4 :end 2))
 (reset)
 (assert-error (delete-duplicates string :start 6 :end 9)))

;;; Function REPLACE
(sequence-bounding-indices-test
 (format t "~&/Function REPLACE")
 (assert (string= (replace string "bbbbb" :start1 0 :end1 5) "bbbbb"))
 (assert (string= (replace (copy-seq "ccccc")
                           string
                           :start2 0 :end2 nil) "bbbbb"))
 (assert-error (replace string "ccccc" :start1 0 :end1 6))
 (assert-error (replace string "ccccc" :start2 (opaque-identity -1) :end2 5))
 (assert-error (replace string "ccccc" :start1 4 :end1 2))
 (assert-error (replace string "ccccc" :start1 6 :end1 9)))

;;; Function SEARCH
(sequence-bounding-indices-test
 (format t "~&/Function SEARCH")
 (assert (= (search "aa" string :start2 0 :end2 5) 0))
 (assert (null (search string "aa" :start1 0 :end2 nil)))
 (assert-error (search "aa" string :start2 0 :end2 6))
 (assert-error (search "aa" string :start2 (opaque-identity -1) :end2 5))
 (assert-error (search "aa" string :start2 4 :end2 2))
 (assert-error (search "aa" string :start2 6 :end2 9)))

;;; Function STRING-UPCASE, STRING-DOWNCASE, STRING-CAPITALIZE,
;;; NSTRING-UPCASE, NSTRING-DOWNCASE, NSTRING-CAPITALIZE
(defmacro string-case-frob (fn)
  `(progn
     (assert-error (,fn string :start 0 :end 6))
     (assert-error (,fn string :start (opaque-identity -1) :end 5))
     (assert-error (,fn string :start 4 :end 2))
     (assert-error (,fn string :start 6 :end 9))))

(sequence-bounding-indices-test
 (format t "~&/Function STRING-UPCASE, STRING-DOWNCASE, STRING-CAPITALIZE, ...")
 (string-case-frob string-upcase)
 (string-case-frob string-downcase)
 (string-case-frob string-capitalize)
 (format t "~&/... NSTRING-UPCASE, NSTRING-DOWNCASE, NSTRING-CAPITALIZE")
 (string-case-frob nstring-upcase)
 (string-case-frob nstring-downcase)
 (string-case-frob nstring-capitalize))

;;; Function STRING=, STRING/=, STRING<, STRING>, STRING<=, STRING>=,
;;; STRING-EQUAL, STRING-NOT-EQUAL, STRING-LESSP, STRING-GREATERP,
;;; STRING-NOT-GREATERP, STRING-NOT-LESSP
(defmacro string-predicate-frob (fn)
  `(progn
     (,fn string "abcde" :start1 0 :end1 5)
     (,fn "fghij" string :start2 0 :end2 nil)
     (assert-error (,fn string "klmno"
                        :start1 0 :end1 6))
     (assert-error (,fn "pqrst" string
                        :start2 (opaque-identity -1) :end2 5))
     (assert-error (,fn "uvwxy" string
                        :start1 4 :end1 2))
     (assert-error (,fn string "z" :start2 6 :end2 9))))
(sequence-bounding-indices-test
 (format t "~&/Function STRING=, STRING/=, STRING<, STRING>, STRING<=, STRING>=, ...")
 (string-predicate-frob string=)
 (string-predicate-frob string/=)
 (string-predicate-frob string<)
 (string-predicate-frob string>)
 (string-predicate-frob string<=)
 (string-predicate-frob string>=))
(sequence-bounding-indices-test
 (format t "~&/... STRING-EQUAL, STRING-NOT-EQUAL, STRING-LESSP, ...")
 (string-predicate-frob string-equal)
 (string-predicate-frob string-not-equal)
 (string-predicate-frob string-lessp))
(sequence-bounding-indices-test
 (format t "~&/... STRING-GREATERP, STRING-NOT-GREATERP, STRING-NOT-LESSP")
 (string-predicate-frob string-greaterp)
 (string-predicate-frob string-not-greaterp)
 (string-predicate-frob string-not-lessp))

;;; Function SUBSTITUTE, SUBSTITUTE-IF, SUBSTITUTE-IF-NOT,
;;; NSUBSTITUTE, NSUBSTITUTE-IF, NSUBSTITUTE-IF-NOT
(sequence-bounding-indices-test
 (format t "~&/Function SUBSTITUTE, SUBSTITUTE-IF, SUBSTITUTE-IF-NOT, ...")
 (assert (string= (substitute #\b #\a string :start 0 :end 5) "bbbbb"))
 (assert (string= (substitute #\c #\a string :start 0 :end nil)
                  "ccccc"))
 (assert-error (substitute #\b #\a string :start 0 :end 6))
 (assert-error (substitute #\b #\a string :start (opaque-identity -1) :end 5))
 (assert-error (substitute #\b #\a string :start 4 :end 2))
 (assert-error (substitute #\b #\a string :start 6 :end 9))
 (assert (string= (substitute-if #\b #'alpha-char-p string
                                 :start 0 :end 5)
                  "bbbbb"))
 (assert (string= (substitute-if #\c #'alpha-char-p string
                                 :start 0 :end nil)
                  "ccccc"))
 (assert-error (substitute-if #\b #'alpha-char-p string
                              :start 0 :end 6))
 (assert-error (substitute-if #\b #'alpha-char-p string
                              :start (opaque-identity -1) :end 5))
 (assert-error (substitute-if #\b #'alpha-char-p string
                              :start 4 :end 2))
 (assert-error (substitute-if #\b #'alpha-char-p string
                              :start 6 :end 9))
 (assert (string= (substitute-if-not #\b #'alpha-char-p string
                                     :start 0 :end 5)
                  "aaaaa"))
 (assert (string= (substitute-if-not #\c #'alpha-char-p string
                                     :start 0 :end nil)
                  "aaaaa"))
 (assert-error (substitute-if-not #\b #'alpha-char-p string
                                  :start 0 :end 6))
 (assert-error (substitute-if-not #\b #'alpha-char-p string
                                  :start (opaque-identity -1) :end 5))
 (assert-error (substitute-if-not #\b #'alpha-char-p string
                                  :start 4 :end 2))
 (assert-error (substitute-if-not #\b #'alpha-char-p string
                                  :start 6 :end 9)))
(sequence-bounding-indices-test
 (format t "~&/... NSUBSTITUTE, NSUBSTITUTE-IF, NSUBSTITUTE-IF-NOT")
 (assert (string= (nsubstitute #\b #\a string :start 0 :end 5) "bbbbb"))
 (reset)
 (assert (string= (nsubstitute #\c #\a string :start 0 :end nil)
                  "ccccc"))
 (reset)
 (assert-error (nsubstitute #\b #\a string :start 0 :end 6))
 (reset)
 (assert-error (nsubstitute #\b #\a string :start (opaque-identity -1) :end 5))
 (reset)
 (assert-error (nsubstitute #\b #\a string :start 4 :end 2))
 (reset)
 (assert-error (nsubstitute #\b #\a string :start 6 :end 9))
 (reset)
 (assert (string= (nsubstitute-if #\b #'alpha-char-p string
                                  :start 0 :end 5)
                  "bbbbb"))
 (reset)
 (assert (string= (nsubstitute-if #\c #'alpha-char-p string
                                  :start 0 :end nil)
                  "ccccc"))
 (reset)
 (assert-error (nsubstitute-if #\b #'alpha-char-p string
                               :start 0 :end 6))
 (reset)
 (assert-error (nsubstitute-if #\b #'alpha-char-p string
                               :start (opaque-identity -1) :end 5))
 (reset)
 (assert-error (nsubstitute-if #\b #'alpha-char-p string
                               :start 4 :end 2))
 (reset)
 (assert-error (nsubstitute-if #\b #'alpha-char-p string
                               :start 6 :end 9))
 (reset)
 (assert (string= (nsubstitute-if-not #\b #'alpha-char-p string
                                      :start 0 :end 5)
                  "aaaaa"))
 (reset)
 (assert (string= (nsubstitute-if-not #\c #'alpha-char-p string
                                      :start 0 :end nil)
                  "aaaaa"))
 (reset)
 (assert-error (nsubstitute-if-not #\b #'alpha-char-p string
                                   :start 0 :end 6))
 (reset)
 (assert-error (nsubstitute-if-not #\b #'alpha-char-p string
                                   :start (opaque-identity -1) :end 5))
 (reset)
 (assert-error (nsubstitute-if-not #\b #'alpha-char-p string
                                   :start 4 :end 2))
 (reset)
 (assert-error (nsubstitute-if-not #\b #'alpha-char-p string
                                   :start 6 :end 9)))
;;; Function WRITE-STRING, WRITE-LINE
(sequence-bounding-indices-test
 (format t "~&/Function WRITE-STRING, WRITE-LINE")
 (write-string string *standard-output* :start 0 :end 5)
 (write-string string *standard-output* :start 0 :end nil)
 (assert-error (write-string string *standard-output*
                             :start 0 :end 6))
 (assert-error (write-string string *standard-output*
                             :start (opaque-identity -1) :end 5))
 (assert-error (write-string string *standard-output*
                             :start 4 :end 2))
 (assert-error (write-string string *standard-output*
                             :start 6 :end 9))
 (write-line string *standard-output* :start 0 :end 5)
 (write-line string *standard-output* :start 0 :end nil)
 (assert-error (write-line string *standard-output*
                           :start 0 :end 6))
 (assert-error (write-line string *standard-output*
                           :start (opaque-identity -1) :end 5))
 (assert-error (write-line string *standard-output*
                           :start 4 :end 2))
 (assert-error (write-line string *standard-output*
                           :start 6 :end 9)))

;;; Macro WITH-INPUT-FROM-STRING
(sequence-bounding-indices-test
 (format t "~&/Macro WITH-INPUT-FROM-STRING")
 (with-input-from-string (s string :start 0 :end 5)
   (assert (char= (read-char s) #\a)))
 (with-input-from-string (s string :start 0 :end nil)
   (assert (char= (read-char s) #\a)))
 (assert-error
  (with-input-from-string (s string :start 0 :end 6)
    (read-char s)))
 (assert-error
  (with-input-from-string (s string :start (opaque-identity -1) :end 5)
    (read-char s)))
 (assert-error
  (with-input-from-string (s string :start 4 :end 2)
    (read-char s)))
 (assert-error
  (with-input-from-string (s string :start 6 :end 9)
    (read-char s))))

;;; testing bit-bashing according to _The Practice of Programming_
(defun fill-bytes-for-testing (bitsize)
  "Return a list of 'bytes' of type (MOD BITSIZE)."
  (remove-duplicates (list 0
                           (1- (ash 1 (1- bitsize)))
                           (ash 1 (1- bitsize))
                           (1- (ash 1 bitsize)))))

(defun fill-with-known-value (value size &rest vectors)
  (dolist (vec vectors)
    (dotimes (i size)
      (setf (aref vec i) value))))

(defun collect-fill-amounts (n-power)
  (remove-duplicates
   (loop for i from 0 upto n-power
         collect (1- (expt 2 i))
         collect (expt 2 i)
         collect (1+ (expt 2 i)))))

(defun test-fill-bashing (bitsize padding-amount n-power)
  (let* ((size (+ (* padding-amount 2) (expt 2 n-power) (* padding-amount 2)))
         (standard (make-array size :element-type `(unsigned-byte ,bitsize)))
         (bashed (make-array size :element-type `(unsigned-byte ,bitsize)))
         (fill-amounts (collect-fill-amounts n-power))
         (bash-function (intern (format nil "UB~A-BASH-FILL" bitsize)
                                (find-package "SB-KERNEL"))))
    (format t "~&/Function ~A..." bash-function)
    (loop for offset from padding-amount below (* 2 padding-amount) do
          (dolist (c (fill-bytes-for-testing bitsize))
            (dolist (n fill-amounts)
              (fill-with-known-value (mod (lognot c) (ash 1 bitsize)) n
                                     standard bashed)
              ;; fill vectors
              ;; a) the standard slow way
              (locally (declare (notinline fill))
                (fill standard c :start offset :end (+ offset n)))
              ;; b) the blazingly fast way
              (let ((value (loop for i from 0 by bitsize
                                 until (= i sb-vm:n-word-bits)
                                 sum (ash c i))))
                (funcall bash-function value bashed offset n))
              ;; check for errors
              (when (mismatch standard bashed)
                (format t "Test with offset ~A, fill ~A and length ~A failed.~%"
                        offset c n)
                (format t "Mismatch: ~A ~A~%"
                        (subseq standard 0 (+ offset n 1))
                        (subseq bashed 0 (+ offset n 1)))
                (return-from test-fill-bashing nil))))
          finally (return t))))

(defun test-copy-bashing (bitsize padding-amount n-power)
  (let* ((size (+ (* padding-amount 2) (expt 2 n-power) (* padding-amount 2)))
         (standard-dst (make-array size :element-type `(unsigned-byte ,bitsize)))
         (bashed-dst (make-array size :element-type `(unsigned-byte ,bitsize)))
         (source (make-array size :element-type `(unsigned-byte ,bitsize)))
         (fill-amounts (collect-fill-amounts n-power))
         (bash-function (intern (format nil "UB~A-BASH-COPY" bitsize)
                                (find-package "SB-KERNEL"))))
    (format t "~&/Function ~A..." bash-function)
    (do ((source-offset padding-amount (1+ source-offset)))
        ((>= source-offset (* padding-amount 2))
         ;; success!
         t)
     (do ((target-offset padding-amount (1+ target-offset)))
         ((>= target-offset (* padding-amount 2)))
       (dolist (c (fill-bytes-for-testing bitsize))
         (dolist (n fill-amounts)
           (fill-with-known-value (mod (lognot c) (ash 1 bitsize)) size
                                  source standard-dst bashed-dst)
           ;; fill with test data
           (fill source c :start source-offset :end (+ source-offset n))
           ;; copy filled test data to test vectors
           ;; a) the slow way
           (replace standard-dst source
                    :start1 target-offset :end1 (+ target-offset n)
                    :start2 source-offset :end2 (+ source-offset n))
           ;; b) the blazingly fast way
           (funcall bash-function source source-offset
                    bashed-dst target-offset n)
           ;; check for errors
           (when (mismatch standard-dst bashed-dst)
             (format t "Test with target-offset ~A, source-offset ~A, fill ~A, and length ~A failed.~%"
                     target-offset source-offset c n)
             (format t "Mismatch:~% correct ~A~% actual  ~A~%"
                     standard-dst
                     bashed-dst)
             (return-from test-copy-bashing nil))))))))

;; Too slow for the interpreter
#+#.(cl:if (cl:eq sb-ext:*evaluator-mode* :compile) '(and) '(or))
(loop for i = 1 then (* i 2) do
     ;; the bare '13' here is fairly arbitrary, except that it's been
     ;; reduced from '32', which made the tests take aeons; '8' provides
     ;; a good range of lengths over which to fill and copy, which
     ;; should tease out most errors in the code (if any exist).  (It
     ;; also makes this part of the test suite finish reasonably
     ;; quickly.)
     (assert (time (test-fill-bashing i 13 8)))
     (assert (time (test-copy-bashing i 13 8)))
     until (= i sb-vm:n-word-bits))

(defun test-inlined-bashing (bitsize)
  ;; We have to compile things separately for each bitsize so the
  ;; compiler will work out the array type and trigger the REPLACE
  ;; transform.
  (let ((lambda-form
         `(lambda ()
            (let* ((n-elements-per-word ,(truncate sb-vm:n-word-bits bitsize))
                   (size (* 3 n-elements-per-word))
                   (standard-dst (make-array size :element-type '(unsigned-byte ,bitsize)))
                   (bashed-dst (make-array size :element-type '(unsigned-byte ,bitsize)))
                   (source (make-array size :element-type '(unsigned-byte ,bitsize))))
              (declare (type (simple-array (unsigned-byte ,bitsize) (*))
                             source standard-dst bashed-dst))
              (do ((i 0 (1+ i))
                   (offset n-elements-per-word (1+ offset)))
                  ((>= offset (* 2 n-elements-per-word)) t)
                (dolist (c (fill-bytes-for-testing ,bitsize))
                  (fill-with-known-value (mod (lognot c) (ash 1 ,bitsize)) size
                                         source standard-dst bashed-dst)
                  ;; fill with test-data
                  (fill source c :start offset :end (+ offset n-elements-per-word))
                  ;; copy filled data to test vectors
                  ;;
                  ;; a) the slow way (which is actually fast, since this
                  ;; should be transformed into UB*-BASH-COPY)
                  (replace standard-dst source
                           :start1 (- offset n-elements-per-word i)
                           :start2 (- offset n-elements-per-word i)
                           :end1 offset :end2 offset)
                  ;; b) the fast way--we fold the
                  ;; :START{1,2} arguments above ourselves
                  ;; to trigger the REPLACE transform
                  (replace bashed-dst source
                           :start1 0 :start2 0 :end1 offset :end2 offset)
                  ;; check for errors
                  (when (or (mismatch standard-dst bashed-dst)
                            ;; trigger COPY-SEQ transform
                            (mismatch (copy-seq standard-dst) bashed-dst)
                            ;; trigger SUBSEQ transform
                            (mismatch (subseq standard-dst (- offset n-elements-per-word i))
                                      bashed-dst))
                    (format t "Test with target-offset ~A, source-offset ~A, fill ~A, and length ~A failed.~%"
                            0 0 c offset)
                    (format t "Mismatch:~% correct ~A~% actual  ~A~%"
                            standard-dst
                            bashed-dst)
                    (return-from nil nil))))))))
    (funcall (compile nil lambda-form))))

#+#.(cl:if (cl:eq sb-ext:*evaluator-mode* :compile) '(and) '(or))
(loop for i = 1 then (* i 2) do
      (assert (test-inlined-bashing i))
      until (= i sb-vm:n-word-bits))

;;; tests from the Sacla test suite via Eric Marsden, 2007-05-07
(remove-duplicates (vector 1 2 2 1) :test-not (lambda (a b) (not (= a b))))

(delete-duplicates (vector #\a #\b #\c #\a)
                   :test-not (lambda (a b) (not (char-equal a b))))

;;; FILL on lists
(let ((l (list 1 2 3)))
  (assert (eq l (fill l 0 :start 1 :end 2)))
  (assert (equal l '(1 0 3)))
  (assert (eq l (fill l 'x :start 2 :end 3)))
  (assert (equal l '(1 0 x)))
  (assert (eq l (fill l 'y :start 1)))
  (assert (equal l '(1 y y)))
  (assert (eq l (fill l 'z :end 2)))
  (assert (equal l '(z z y)))
  (assert (eq l (fill l 1)))
  (assert (equal l '(1 1 1)))
  (assert-error (fill l 0 :start 4))
  (assert-error (fill l 0 :end 4))
  (assert-error (fill l 0 :start 2 :end 1)))

;;; Both :TEST and :TEST-NOT provided
(with-test (:name :test-and-test-not-to-adjoin)
  (multiple-value-bind (fun failure-p warnings)
      (checked-compile `(lambda (item test test-not)
                          (adjoin item '(1 2 3 :foo)
                                  :test test
                                  :test-not test-not))
                       :allow-warnings t)
    (declare (ignore failure-p))
    (assert (= 1 (length warnings)))
    (assert-error (funcall fun 1 #'eql (complement #'eql)))))

;;; tests of deftype types equivalent to STRING or SIMPLE-STRING
(deftype %string () 'string)
(deftype %simple-string () 'simple-string)
(deftype string-3 () '(string 3))
(deftype simple-string-3 () '(simple-string 3))

(with-test (:name :user-defined-string-types-map-etc)
  (dolist (type '(%string %simple-string string-3 simple-string-3))
    (assert (string= "foo" (coerce '(#\f #\o #\o) type)))
    (assert (string= "foo" (map type 'identity #(#\f #\o #\o))))
    (assert (string= "foo" (merge type (copy-seq '(#\o)) (copy-seq '(#\f #\o))
                                  'char<)))
    (assert (string= "foo" (concatenate type '(#\f) "oo")))
    (assert (string= "ooo" (make-sequence type 3 :initial-element #\o)))))
(with-test (:name :user-defined-string-types-map-etc-error)
  (dolist (type '(string-3 simple-string-3))
    (assert-error (coerce '(#\q #\u #\u #\x) type))
    (assert-error (map type 'identity #(#\q #\u #\u #\x)))
    (assert-error (merge type (copy-seq '(#\q #\x)) (copy-seq "uu") 'char<))
    (assert-error (concatenate type "qu" '(#\u #\x)))
    (assert-error (make-sequence type 4 :initial-element #\u))))

(defun test-bit-position (size set start end from-end res)
  (let ((v (make-array size :element-type 'bit :initial-element 0)))
    (dolist (i set)
      (setf (bit v i) 1))
    (dolist (f (list (compile nil
                              `(lambda (b v s e fe)
                                 (position b (the bit-vector v) :start s :end e :from-end fe)))
                     (compile nil
                              `(lambda (b v s e fe)
                                 (assert (eql b 1))
                                 (position 1 (the bit-vector v) :start s :end e :from-end fe)))
                     (compile nil
                              `(lambda (b v s e fe)
                                 (position b (the vector v) :start s :end e :from-end fe)))))
      (let ((got (funcall f 1 v start end from-end)))
        (unless (eql res got)
          (cerror "Continue" "POSITION 1, Wanted ~S, got ~S.~%  size = ~S, set = ~S, from-end = ~S"
                  res got
                  size set from-end)))))
  (let ((v (make-array size :element-type 'bit :initial-element 1)))
    (dolist (i set)
      (setf (bit v i) 0))
    (dolist (f (list (compile nil
                              `(lambda (b v s e fe)
                                 (position b (the bit-vector v) :start s :end e :from-end fe)))
                     (compile nil
                              `(lambda (b v s e fe)
                                 (assert (eql b 0))
                                 (position 0 (the bit-vector v) :start s :end e :from-end fe)))
                     (compile nil
                              `(lambda (b v s e fe)
                                 (position b (the vector v) :start s :end e :from-end fe)))))
      (let ((got (funcall f 0 v start end from-end)))
        (unless (eql res got)
          (cerror "Continue" "POSITION 0, Wanted ~S, got ~S.~%  size = ~S, set = ~S, from-end = ~S"
                  res got
                  size set from-end))))))

(defun random-test-bit-position (n)
  (loop repeat n
        do (let* ((vector (make-array (+ 2 (random 5000)) :element-type 'bit))
                  (offset (random (1- (length vector))))
                  (size (1+ (random (- (length vector) offset))))
                  (disp (make-array size :element-type 'bit :displaced-to vector
                                         :displaced-index-offset offset)))
             (assert (plusp size))
             (loop repeat 10
                   do (setf (bit vector (random (length vector))) 1))
             (flet ((test (orig)
                      (declare (bit-vector orig))
                      (let ((copy (coerce orig 'simple-vector))
                            (p0 (random (length orig)))
                            (p1 (1+ (random (length orig)))))
                        (multiple-value-bind (s e)
                            (if (> p1 p0)
                                (values p0 p1)
                                (values p1 p0))
                          (assert (eql (position 1 copy :start s :end e)
                                       (position 1 orig :start s :end e)))
                          (assert (eql (position 1 copy :start s :end e :from-end t)
                                       (position 1 orig :start s :end e :from-end t)))))))
               (test vector)
               (test disp)))))

(with-test (:name :bit-position)
  (test-bit-position 0 (list) 0 0 nil nil)
  (test-bit-position 0 (list) 0 0 t nil)
  (test-bit-position 1 (list 0) 0 0 nil nil)
  (test-bit-position 1 (list 0) 0 0 t nil)
  (test-bit-position 1 (list 0) 0 1 nil 0)
  (test-bit-position 1 (list 0) 0 1 t 0)
  (test-bit-position 10 (list 0 1) 0 1 nil 0)
  (test-bit-position 10 (list 0 1) 1 1 nil nil)
  (test-bit-position 10 (list 0 1) 0 1 t 0)
  (test-bit-position 10 (list 0 1) 1 1 t nil)
  (test-bit-position 10 (list 0 3) 1 4 nil 3)
  (test-bit-position 10 (list 0 3) 1 4 t 3)
  (test-bit-position 10 (list 0 3 6) 1 10 nil 3)
  (test-bit-position 10 (list 0 3 6) 1 10 t 6)
  (test-bit-position 1000 (list 128 700) 20 500 nil 128)
  (test-bit-position 1000 (list 128 700) 20 500 t 128)
  (test-bit-position 1000 (list 423 762) 200 800 nil 423)
  (test-bit-position 1000 (list 423 762) 200 800 t 762)
  (test-bit-position 1000 (list 298 299) 100 400 nil 298)
  (test-bit-position 1000 (list 298 299) 100 400 t 299))

(with-test (:name (:bit-position :random-test))
  (random-test-bit-position 10000))

;; REVERSE and NREVERSE should assert that the returned value
;; from a generic sequence operation is of type SEQUENCE.
(defclass bogus-reversal-seq (sequence standard-object) ())
(defmethod sequence:reverse ((self bogus-reversal-seq))
  #2a((x y) (1 2)))
(with-test (:name :generic-sequence-reverse)
  (assert-error (reverse (make-instance 'bogus-reversal-seq))))

(with-test (:name :abstract-base-sequence-satisfies-sequencep)
  (assert (typep (sb-pcl::class-prototype (find-class 'sequence)) 'sequence)))

(defvar *macro-invocations* 0)
;; in case someone adds more tests after this, don't mess up OPAQUE-IDENTITY
(defun opaque-id-again (x) x)
(define-compiler-macro opaque-id-again (x) (incf *macro-invocations*) x)
(with-test (:name :mapfoo-admits-compiler-macros)
  (compile nil '(lambda (l) (mapcar #'opaque-id-again l)))
  (assert (= *macro-invocations* 1))
  (compile nil '(lambda (l) (some #'opaque-id-again l)))
  (assert (= *macro-invocations* 2)))

;;; success
