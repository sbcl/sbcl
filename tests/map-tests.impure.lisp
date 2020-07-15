;;;; side-effectful tests of MAP-related stuff

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

;;; tests of MAP
(with-test (:name (map :smoke))
  (assertoid (map 'vector #'+ '(1 2 3) '(30 20))
             :expected-equalp #(31 22))
  (assertoid (map 'list #'+ #(1 2) '(100) #(0) #(100 100))
             :expected-equal '(201)))

;;; tests of MAP-INTO

(with-test (:name (map-into :smoke))
  (assertoid (map-into (vector) #'+ '(1 2 3) '(30 20))
             :expected-equalp #())
  (assertoid (map-into (vector 99) #'+ '(1 2 3) '(30 20))
             :expected-equalp #(31))
  (assertoid (map-into (vector 99 88) #'+ '(1 2 3) '(30 20))
             :expected-equalp #(31 22))
  (assertoid (map-into (vector 99 88 77) #'+ '(1 2 3) '(30 20))
             :expected-equalp #(31 22 77))

  (assertoid (map-into (list) #'+ '(1 2 3) '(30 20))
             :expected-equalp '())
  (assertoid (map-into (list 99) #'+ '(1 2 3) '(30 20))
             :expected-equalp '(31))
  (assertoid (map-into (list 99 88) #'+ '(1 2 3) '(30 20))
             :expected-equalp '(31 22))
  (assertoid (map-into (list 99 88 77) #'+ '(1 2 3) '(30 20))
             :expected-equalp '(31 22 77))

  (assertoid (map-into (vector 99 99 99) (constantly 5))
             :expected-equalp #(5 5 5))
  (assertoid (map-into (vector 99 99 99) (let ((x 0)) (lambda () (incf x))))
             :expected-equalp #(1 2 3))

  (assertoid (map-into (list 99 99 99) (constantly 5))
             :expected-equalp '(5 5 5))
  (assertoid (map-into (list 99 99 99) (let ((x 0)) (lambda () (incf x))))
             :expected-equalp '(1 2 3))

  (assertoid (map-into (make-array 0 :element-type 'fixnum)
                       #'+ '(1 2 3) '(30 20))
             :expected-equalp #())
  (assertoid (map-into (make-array 1 :element-type 'fixnum :initial-element 99)
                       #'+ '(1 2 3) '(30 20))
             :expected-equalp #(31))
  (assertoid (map-into (make-array 2 :element-type 'fixnum :initial-element 99)
                       #'+ '(1 2 3) '(30 20))
             :expected-equalp #(31 22))
  (assertoid (map-into (make-array 3 :element-type 'fixnum :initial-element 99)
                       #'+ '(1 2 3) '(30 20))
             :expected-equalp #(31 22 99))

  (assertoid (map-into (make-array 0 :fill-pointer 0 :initial-element 99)
                       #'+ '(1 2 3) '(30 20))
             :expected-equalp #())
  (assertoid (map-into (make-array 1 :fill-pointer 0 :initial-element 99)
                       #'+ '(1 2 3) '(30 20))
             :expected-equalp #(31))
  (assertoid (map-into (make-array 2 :fill-pointer 0 :initial-element 99)
                       #'+ '(1 2 3) '(30 20))
             :expected-equalp #(31 22))
  (assertoid (map-into (make-array 3 :fill-pointer 0 :initial-element 99)
                       #'+ '(1 2 3) '(30 20))
             :expected-equalp #(31 22))

  (assertoid (map-into (make-array 9 :fill-pointer 9 :initial-element 99)
                       #'+ '(1 2 3) '(30 20))
             :expected-equalp #(31 22))
  (assertoid (map-into (make-array 9 :fill-pointer 5 :initial-element 99)
                       #'+ '(1 2 3) '(30 20))
             :expected-equalp #(31 22)))

(defmacro with-mapnil-test-fun (fun-name &body body)
  `(let ((reversed-result nil))
     (flet ((,fun-name (&rest rest)
              (push rest reversed-result)))
       ,@body
       (nreverse reversed-result))))

(with-test (:name (map nil))
  (assertoid (with-mapnil-test-fun fun
               (map nil #'fun #(1)))
             :expected-equal '((1)))
  (assertoid (with-mapnil-test-fun fun
               (map nil #'fun #() '(1 2 3)))
             :expected-equal '())
  (assertoid (with-mapnil-test-fun fun
               (map nil #'fun #(a b c) '(alpha beta) '(aleph beth)))
             :expected-equal '((a alpha aleph) (b beta beth))))

;;; Exercise MAP repeatedly on the same dataset by providing various
;;; combinations of sequence type arguments, declarations, and so
;;; forth.
(defvar *list-1* '(1))
(defvar *list-2* '(1 2))
(defvar *list-3* '(1 2 3))
(defvar *list-4* '(1 2 3 4))
(defvar *vector-10* #(10))
(defvar *vector-20* #(10 20))
(defvar *vector-30* #(10 20 30))
(defmacro maptest (&key
                   result-seq
                   fun-name
                   arg-seqs
                   arg-types
                   (result-element-types '(t)))
  (let ((reversed-assertoids nil))
    (dotimes (arg-type-index (expt 2 (length arg-types)))
      (labels (;; Arrange for EXPR to be executed.
               (arrange (expr)
                 (push expr reversed-assertoids))
               ;; We toggle the various type declarations on and
               ;; off depending on the bit pattern in ARG-TYPE-INDEX,
               ;; so that we get lots of different things to test.
               (eff-arg-type (i)
                 (if (and (< i (length arg-types))
                          (plusp (logand (expt 2 i)
                                         arg-type-index)))
                     (nth i arg-types)
                     t))
               (args-with-type-decls ()
                 (let ((reversed-result nil))
                   (dotimes (i (length arg-seqs) (nreverse reversed-result))
                     (push `(the ,(eff-arg-type i)
                              ,(nth i arg-seqs))
                           reversed-result)))))
        (dolist (fun `(',fun-name #',fun-name))
          (dolist (result-type (cons 'list
                                     (mapcan (lambda (et)
                                               `((vector ,et)
                                                 (simple-array ,et 1)))
                                             result-element-types)))
            (arrange
             `(assertoid (map ',result-type ,fun ,@(args-with-type-decls))
                         :expected-equalp (coerce ,result-seq
                                                  ',result-type))))
          (arrange
           `(assertoid (map-into (fill (copy-seq ,result-seq) 9999)
                                 ,fun ,@(args-with-type-decls))
                       :expected-equalp ,result-seq)))
        (arrange
         `(assertoid (mapcar (lambda (args) (apply #',fun-name args))
                             (with-mapnil-test-fun mtf
                               (map nil
                                    ;; (It would be nice to test MAP
                                    ;; NIL with function names, too,
                                    ;; but I can't see any concise way
                                    ;; to do it..)
                                    #'mtf
                                    ,@(args-with-type-decls))))
                     :expected-equal (coerce ,result-seq 'list)))))
    `(progn ,@(nreverse reversed-assertoids))))

(with-test (:name :maptest)
  (maptest :result-seq '(2 3)
           :fun-name 1+
           :arg-seqs (*list-2*)
           :arg-types (list))
  (maptest :result-seq '(nil nil nil)
           :fun-name oddp
           :arg-seqs (*vector-30*)
           :arg-types (vector))
  (maptest :result-seq '(12 24)
           :fun-name +
           :arg-seqs (*list-2* *list-2* *vector-30*)
           :arg-types (list list vector)))

(with-test (:name (map-into vector :from list))
  (map-into (eval (make-array 10))
            #'list
            (make-list 10))
  (checked-compile-and-assert ()
      `(lambda (a) (map-into (make-array 3) #'identity a))
    (('(1 2 3)) #(1 2 3) :test #'equalp)))

(with-test (:name (map-into type-error))
  (checked-compile-and-assert (:optimize :safe)
      `(lambda (x)
         (map-into (make-array 1 :element-type '(signed-byte 16)) x))
    (((constantly nil)) (condition 'type-error)))
  (checked-compile-and-assert (:optimize :safe)
      `(lambda (array x) (map-into array x))
    (((make-array 1 :element-type '(signed-byte 16)) (constantly nil))
     (condition 'type-error)))
  (checked-compile-and-assert (:optimize :safe)
      `(lambda (array x) (map-into array x))
    (((cons 1 2) (constantly nil)) (condition 'type-error))))

(with-test (:name (map type-error))
  (checked-compile-and-assert (:optimize :safe)
      `(lambda (x) (map '(vector (signed-byte 16) 1) #'identity x))
    (('(1.0)) (condition 'type-error)))
  (checked-compile-and-assert (:optimize :safe)
      `(lambda (type x) (map type #'identity x))
    (('(vector (signed-byte 16) 1) '(1.0)) (condition 'type-error))))

(with-test (:name (map :out-of-line))
  (flet ((test (map args expected)
           (let ((fun (checked-compile `(lambda (&rest args)
                                          (declare (notinline ,map))
                                          (apply #',map args)))))
             (assert (equal (apply fun args) expected)))))
    (test 'mapcar (list #'+ '(1 2 3) '(3 2 1)) '(4 4 4))
    (test 'maplist (list #'cons '(1 2 3) '(3 2 1))
          '(((1 2 3) 3 2 1) ((2 3) 2 1) ((3) 1)))
    (test 'mapcan (list #'cons '(1 2 3) '(3 2 1)) '(1 2 3 . 1))
    (test 'mapcon (list #'list '(1 2 3) '(3 2 1))
          '((1 2 3) (3 2 1) (2 3) (2 1) (3) (1)))
    (test 'mapcan (list  #'identity '((3 4 . 5) nil (1 . 5)))
          '(3 4 1 . 5))
    (test 'mapcar (list  #'list '(1 2 3) '(4 5 6) '(7 8 9))
          '((1 4 7) (2 5 8) (3 6 9)))
    (test 'mapcan (list #'identity '(1)) 1)))

(with-test (:name (map :initial-element-style-warnin))
  (checked-compile-and-assert
   ()
   `(lambda (x)
      (map '(vector hash-table) #'identity x))))
