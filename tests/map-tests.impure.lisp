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

(load "assertoid.lisp")
(use-package "ASSERTOID")

;;; tests of MAP
;;; FIXME: Move these into their own file.
(assertoid (map 'vector #'+ '(1 2 3) '(30 20))
           :expected-equalp #(31 22))
(assertoid (map 'list #'+ #(1 2) '(100) #(0) #(100 100))
           :expected-equal '(201))

(defmacro with-mapnil-test-fun (fun-name &body body)
  `(let ((reversed-result nil))
     (flet ((,fun-name (&rest rest)
              (push rest reversed-result)))
       ,@body
       (nreverse reversed-result))))
(assertoid (with-mapnil-test-fun fun
             (map nil #'fun #(1)))
           :expected-equal '((1)))
(assertoid (with-mapnil-test-fun fun
             (map nil #'fun #() '(1 2 3)))
           :expected-equal '())
(assertoid (with-mapnil-test-fun fun
             (map nil #'fun #(a b c) '(alpha beta) '(aleph beth)))
           :expected-equal '((a alpha aleph) (b beta beth)))

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
                                                  ',result-type)))))
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
         :arg-types (list list vector))

;;; success
