;;;; miscellaneous tests of printing stuff

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

;;; We should be able to output X readably (at least when *READ-EVAL*).
(defun assert-readable-output (x)
  (assert (eql x
               (let ((*read-eval* t))
                 (read-from-string (with-output-to-string (s)
                                     (write x :stream s :readably t)))))))

;;; Even when *READ-EVAL* is NIL, we should be able to output some
;;; (not necessarily readable) representation without signalling an
;;; error.
(defun assert-unreadable-output (x)
  (let ((*read-eval* nil))
    (with-output-to-string (s) (write x :stream s :readably nil))))

(defun assert-output (x)
  (assert-readable-output x)
  (assert-unreadable-output x))

;;; Nathan Froyd reported that sbcl-0.6.11.34 screwed up output of
;;; floating point infinities.
(dolist (x (list short-float-positive-infinity short-float-negative-infinity
                 single-float-positive-infinity single-float-negative-infinity
                 double-float-positive-infinity double-float-negative-infinity
                 long-float-positive-infinity long-float-negative-infinity))
  (assert-output x))

;;; Eric Marsden reported that this would blow up in CMU CL (even
;;; though ANSI says that the mismatch between ~F expected type and
;;; provided string type is supposed to be handled without signalling
;;; an error) and provided a fix which was ported to sbcl-0.6.12.35.
(assert (null (format t "~F" "foo")))

;;; This was a bug in SBCL until 0.6.12.40 (originally reported as a
;;; CMU CL bug by Erik Naggum on comp.lang.lisp).
(loop for base from 2 to 36
      with *print-radix* = t
      do (let ((*print-base* base))
           (assert (string= "#*101" (format nil "~S" #*101)))))

;;; bug in sbcl-0.7.1.25, reported by DB sbcl-devel 2002-02-25
(assert (string= "0.5" (format nil "~2D" 0.5)))

;;; we want malformed format strings to cause errors rather than have
;;; some DWIM "functionality".
(assert-error (format nil "~:2T"))

;;; bug reported, with fix, by Robert Strandh, sbcl-devel 2002-03-09,
;;; fixed in sbcl-0.7.1.36:
(assert (string= (format nil "~2,3,8,'0$" 1234567.3d0) "1234567.30"))

;;; checks that other FORMAT-DOLLAR output remains sane after the
;;; 0.7.1.36 change
(assert (string= (format nil "~$" 0) "0.00"))
(assert (string= (format nil "~$" 4) "4.00"))
(assert (string= (format nil "~$" -4.0) "-4.00"))
(assert (string= (format nil "~2,7,11$" -4.0) "-0000004.00"))
(assert (string= (format nil "~2,7,11,' $" 1.1) " 0000001.10"))
(assert (string= (format nil "~1,7,11,' $" 1.1) "  0000001.1"))
(assert (string= (format nil "~1,3,8,' $" 7.3) "   007.3"))
(assert (string= (format nil "~2,3,8,'0$" 7.3) "00007.30"))

;;; Check for symbol lookup in ~/ / directive -- double-colon was
;;; broken in 0.7.1.36 and earlier
(defun print-foo (stream arg colonp atsignp &rest params)
  (declare (ignore colonp atsignp params))
  (format stream "~d" arg))

(assert (string= (format nil "~/print-foo/" 2) "2"))
(assert (string= (format nil "~/cl-user:print-foo/" 2) "2"))
(assert (string= (format nil "~/cl-user::print-foo/" 2) "2"))
(assert-error (format nil "~/cl-user:::print-foo/" 2))
(assert-error (format nil "~/cl-user:a:print-foo/" 2))
(assert-error (format nil "~/a:cl-user:print-foo/" 2))
(assert-error (format nil "~/cl-user:print-foo:print-foo/" 2))

;;; better make sure that we get this one right, too
(defun print-foo\:print-foo (stream arg colonp atsignp &rest params)
  (declare (ignore colonp atsignp params))
  (format stream "~d" arg))

(assert (string= (format nil "~/cl-user:print-foo:print-foo/" 2) "2"))
(assert (string= (format nil "~/cl-user::print-foo:print-foo/" 2) "2"))

;;; Check for error detection of illegal directives in a~<..~> justify
;;; block (see ANSI section 22.3.5.2)
(assert-error (format nil "~<~W~>" 'foo))
(assert-error (format nil "~<~<~A~:>~>" '(foo)))
(assert (string= (format nil "~<~<~A~>~>" 'foo) "FOO"))

(with-test (:name (:format :justification-atsign-check))
  (assert-error (format nil "~<~@>"))
  (assert-error (eval '(format nil "~<~@>"))))

;;; Check that arrays that we print while *PRINT-READABLY* is true are
;;; in fact generating similar objects.
(assert (equal (array-dimensions
                (read-from-string
                 (with-output-to-string (s)
                   (let ((*print-readably* t))
                     (print (make-array '(1 2 0)) s)))))
               '(1 2 0)))

(dolist (array (list (make-array '(1 0 1))
                     (make-array 0 :element-type nil)
                     (make-array 1 :element-type 'base-char)
                     (make-array 1 :element-type 'character)))
  (assert (multiple-value-bind (result error)
              (ignore-errors (read-from-string
                              (with-output-to-string (s)
                                (let ((*print-readably* t))
                                  (print array s)))))
            ;; it might not be readably-printable
            (or (typep error 'print-not-readable)
                (and
                 ;; or else it had better have the same dimensions
                 (equal (array-dimensions result) (array-dimensions array))
                 ;; and the same element-type
                 (equal (array-element-type result) (array-element-type array)))))))

;;; before 0.8.0.66 it signalled UNBOUND-VARIABLE
(write #(1 2 3) :pretty nil :readably t)

;;; another UNBOUND-VARIABLE, this time due to a bug in FORMATTER
;;; expanders.
(funcall (formatter "~@<~A~:*~A~:>") nil 3)

;;; the PPC floating point backend was at one point sufficiently
;;; broken that this looped infinitely or caused segmentation
;;; violations through stack corruption.
(print 0.0001)

;;; In sbcl-0.8.7, the ~W format directive interpreter implemented the
;;; sense of the colon and at-sign modifiers exactly backwards.
;;;
;;; (Yes, the test for this *is* substantially hairier than the fix;
;;; wanna make something of it?)
(cl:in-package :cl-user)
(defstruct wexerciser-0-8-7)
(defun wexercise-0-8-7-interpreted (wformat)
  (format t wformat (make-wexerciser-0-8-7)))
(defmacro define-compiled-wexercise-0-8-7 (wexercise wformat)
  `(defun ,wexercise ()
    (declare (optimize (speed 3) (space 1)))
    (format t ,wformat (make-wexerciser-0-8-7))
    (values)))
(define-compiled-wexercise-0-8-7 wexercise-0-8-7-compiled-without-atsign "~W")
(define-compiled-wexercise-0-8-7 wexercise-0-8-7-compiled-with-atsign "~@W")
(defmethod print-object :before ((wexerciser-0-8-7 wexerciser-0-8-7) stream)
  (unless (and *print-level* *print-length*)
    (error "gotcha coming")))
(let ((*print-level* 11)
      (*print-length* 12))
  (wexercise-0-8-7-interpreted "~W")
  (wexercise-0-8-7-compiled-without-atsign))
(remove-method #'print-object
               (find-method #'print-object
                            '(:before)
                            (mapcar #'find-class '(wexerciser-0-8-7 t))))
(defmethod print-object :before ((wexerciser-0-8-7 wexerciser-0-8-7) stream)
  (when (or *print-level* *print-length*)
    (error "gotcha going")))
(let ((*print-level* 11)
      (*print-length* 12))
  (wexercise-0-8-7-interpreted "~@W")
  (wexercise-0-8-7-compiled-with-atsign))

;;; WRITE-TO-STRING was erroneously DEFKNOWNed as FOLDABLE
;;;
;;; This bug from PFD
(defpackage "SCRATCH-WRITE-TO-STRING" (:use))
(with-standard-io-syntax
  (let* ((*package* (find-package "SCRATCH-WRITE-TO-STRING"))
         (answer (write-to-string 'scratch-write-to-string::x :readably nil)))
    (assert (string= answer "X"))))
;;; and a couple from Bruno Haible
(defun my-pprint-reverse (out list)
  (write-char #\( out)
  (when (setq list (reverse list))
    (loop
     (write (pop list) :stream out)
     (when (endp list) (return))
     (write-char #\Space out)))
  (write-char #\) out))
(with-standard-io-syntax
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
    (set-pprint-dispatch '(cons (member foo)) 'my-pprint-reverse 0)
    (let ((answer (write-to-string '(foo bar :boo 1) :pretty t :escape t)))
      (assert (string= answer "(1 :BOO BAR FOO)")))))
(defun my-pprint-logical (out list)
  (pprint-logical-block (out list :prefix "(" :suffix ")")
    (when list
      (loop
       (write-char #\? out)
       (write (pprint-pop) :stream out)
       (write-char #\? out)
       (pprint-exit-if-list-exhausted)
       (write-char #\Space out)))))
(with-standard-io-syntax
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
    (set-pprint-dispatch '(cons (member bar)) 'my-pprint-logical 0)
    (let ((answer (write-to-string '(bar foo :boo 1) :pretty t :escape t)))
      (assert (string= answer "(?BAR? ?FOO? ?:BOO? ?1?)")))))

;;; FORMAT string compile-time checker failure, reported by Thomas
;;; F. Burdick
(multiple-value-bind (f w-p f-p)
    (compile nil '(lambda () (format nil "~{")))
  (assert (and w-p f-p))
  (assert (nth-value 1 (ignore-errors (funcall f)))))

;;; floating point print/read consistency
(let ((x (/ -9.349640046247849d-21 -9.381494249123696d-11)))
  (let ((y (read-from-string (write-to-string x :readably t))))
    (assert (eql x y))))

(let ((x1 (float -5496527/100000000000000000))
      (x2 (float -54965272/1000000000000000000)))
  (assert (or (equal (multiple-value-list (integer-decode-float x1))
                     (multiple-value-list (integer-decode-float x2)))
              (string/= (prin1-to-string x1) (prin1-to-string x2)))))

;;; readable printing of arrays with *print-radix* t
(let ((*print-radix* t)
      (*print-readably* t)
      (*print-pretty* nil))
  (let ((output (with-output-to-string (s)
                  (write #2a((t t) (nil nil)) :stream s))))
    (assert (equalp (read-from-string output) #2a((t t) (nil nil))))))

;;; NIL parameters to "interpreted" FORMAT directives
(assert (string= (format nil "~v%" nil) (string #\Newline)))

;;; PRINC-TO-STRING should bind print-readably
(let ((*print-readably* t))
  (assert (string= (princ-to-string #\7)
                   (write-to-string #\7 :escape nil :readably nil))))

;;; in FORMAT, ~^ inside ~:{ should go to the next case, not break
;;; iteration, even if one argument is just a one-element list.
(assert (string= (format nil "~:{~A~^~}" '((A) (C D))) "AC"))

;;; errors should be raised if pprint and justification are mixed
;;; injudiciously...
(dolist (x (list "~<~:;~>~_" "~<~:;~>~I" "~<~:;~>~W"
                 "~<~:;~>~:T" "~<~:;~>~<~:>" "~_~<~:;~>"
                 "~I~<~:;~>" "~W~<~:;~>" "~:T~<~:;~>" "~<~:>~<~:;~>"))
  (assert-error (format nil x nil))
  (assert-error (format nil (eval `(formatter ,x)) nil)))
;;; ...but not in judicious cases.
(dolist (x (list "~<~;~>~_" "~<~;~>~I" "~<~;~>~W"
                 "~<~;~>~:T" "~<~;~>~<~>" "~_~<~;~>"
                 "~I~<~;~>" "~W~<~;~>" "~:T~<~;~>" "~<~>~<~;~>"
                 "~<~:;~>~T" "~T~<~:;~>"))
  (assert (format nil x nil))
  (assert (format nil (eval `(formatter ,x)) nil)))

;;; bug 350: bignum printing so memory-hungry that heap runs out
;;; -- just don't stall here forever on a slow box
(with-test (:name :bug-350)
  (handler-case
      (with-timeout 10
        (print (ash 1 1000000) (make-broadcast-stream)))
    (timeout ()
      (print 'timeout!))))

;;; bug 371: bignum print/read inconsistency
(defvar *bug-371* -7043009959286724629649270926654940933664689003233793014518979272497911394287216967075767325693021717277238746020477538876750544587281879084559996466844417586093291189295867052594478662802691926547232838591510540917276694295393715934079679531035912244103731582711556740654671309980075069010778644542022/670550434139267031632063192770201289106737062379324644110801846820471752716238484923370056920388400273070254958650831435834503195629325418985020030706879602898158806736813101434594805676212779217311897830937606064579213895527844045511878668289820732425014254579493444623868748969110751636786165152601)
(let ((*print-base* 5)
      (*read-base* 5)
      (*print-radix* nil))
  (assert (= *bug-371* (read-from-string (prin1-to-string *bug-371*)))))

;;; a spot of random-testing for rational printing
(defvar *seed-state* (make-random-state))
(write *seed-state* :pretty nil) ; so that we can reproduce errors
(let ((seed (make-random-state *seed-state*)))
  (loop repeat 42
     do (let ((n (random (ash 1 1000) seed))
              (d (random (ash 1 1000) seed)))
          (when (zerop (random 2 seed))
            (setf n (- n)))
          (let ((r (/ n d)))
            (loop for base from 2 to 36
               do (let ((*print-base* base)
                        (*read-base* base)
                        (*print-radix* nil))
                    (assert (= r (read-from-string (prin1-to-string r))))
                    (if (= 36 base)
                        (decf *read-base*)
                        (incf *read-base*))
                    (assert (not (eql r (read-from-string (prin1-to-string r)))))
                    (let ((*print-radix* t))
                      (assert (= r (read-from-string
                                    (princ-to-string r)))))))))
       (write-char #\.)
       (finish-output)))

;;;; Bugs, found by PFD
;;; NIL parameter for ~^ means `not supplied'
(loop for (format arg result) in
      '(("~:{~D~v^~D~}" ((3 1 4) (1 0 2) (7 nil) (5 nil 6)) "341756")
        ("~:{~1,2,v^~A~}" ((nil 0) (3 1) (0 2)) "02"))
      do (assert (string= (funcall #'format nil format arg) result))
      do (assert (string= (with-output-to-string (s)
                            (funcall (eval `(formatter ,format)) s arg))
                          result)))

;;; NIL first parameter for ~R is equivalent to no parameter.
(assert (string= (format nil "~VR" nil 5) "five"))
(assert (string= (format nil (formatter "~VR") nil 6) "six"))

;;; CSR inserted a bug into Burger & Dybvig's float printer.  Caught
;;; by Raymond Toy
(assert (string= (format nil "~E" 1d23) "1.d+23"))

;;; Fixed-format bugs from CLISP's test suite (reported by Bruno
;;; Haible, bug 317)
(assert (string= (format nil "~1F" 10) "10."))
(assert (string= (format nil "~0F" 10) "10."))
(assert (string= (format nil "~2F" 1234567.1) "1234567."))

;;; here's one that seems to fail most places.  I think this is right,
;;; and most of the other answers I've seen are definitely wrong.
(assert (string= (format nil "~G" 1d23) "100000000000000000000000.    "))

;;; Adam Warner's test case
(assert (string= (format nil "~@F" 1.23) "+1.23"))


;;; New (2005-11-08, also known as CSR House day) float format test
;;; cases.  Simon Alexander, Raymond Toy, and others
(assert (string= (format nil "~9,4,,-7E" pi) ".00000003d+8"))
(assert (string= (format nil "~9,4,,-5E" pi) ".000003d+6"))
(assert (string= (format nil "~5,4,,7E" pi) "3141600.d-6"))
(assert (string= (format nil "~11,4,,3E" pi) "  314.16d-2"))
(assert (string= (format nil "~11,4,,5E" pi) "  31416.d-4"))
(assert (string= (format nil "~11,4,,0E" pi) "  0.3142d+1"))
(assert (string= (format nil "~9,,,-1E" pi) ".03142d+2"))
(assert (string= (format nil "~,,,-2E" pi) "0.003141592653589793d+3"))
(assert (string= (format nil "~,,,2E" pi) "31.41592653589793d-1"))
(assert (string= (format nil "~E" pi) "3.141592653589793d+0"))
(assert (string= (format nil "~9,5,,-1E" pi) ".03142d+2"))
(assert (string= (format nil "~11,5,,-1E" pi) " 0.03142d+2"))
(assert (string= (format nil "~G" pi) "3.141592653589793    "))
(assert (string= (format nil "~9,5G" pi) "3.1416    "))
(assert (string= (format nil "|~13,6,2,7E|" pi) "| 3141593.d-06|"))
(assert (string= (format nil "~9,3,2,0,'%E" pi) "0.314d+01"))
(assert (string= (format nil "~9,0,6f" pi) " 3141593."))
(assert (string= (format nil "~6,2,1,'*F" pi) " 31.42"))
(assert (string= (format nil "~6,2,1,'*F" (* 100 pi)) "******"))
(assert (string= (format nil "~9,3,2,-2,'%@E" pi) "+.003d+03"))
(assert (string= (format nil "~10,3,2,-2,'%@E" pi) "+0.003d+03"))
(assert (string= (format nil "~15,3,2,-2,'%,'=@E" pi) "=====+0.003d+03"))
(assert (string= (format nil "~9,3,2,-2,'%E" pi) "0.003d+03"))
(assert (string= (format nil "~8,3,2,-2,'%@E" pi) "%%%%%%%%"))

(assert (string= (format nil "~g" 1e0) "1.    "))
(assert (string= (format nil "~g" 1.2d40) "12000000000000000000000000000000000000000.    "))

(assert (string= (format nil "~e" 0) "0.0e+0"))
(assert (string= (format nil "~e" 0d0) "0.0d+0"))
(assert (string= (format nil "~9,,4e" 0d0) "0.0d+0000"))

(let ((table (make-hash-table)))
  (setf (gethash 1 table) t)
  (assert-error (with-standard-io-syntax
                  (let ((*read-eval* nil)
                        (*print-readably* t))
                    (with-output-to-string (*standard-output*)
                      (prin1 table))))
                print-not-readable))

;; Test that we can print characters readably regardless of the external format
;; of the stream.

(defun test-readable-character (character external-format)
  (let ((file "print.impure.tmp"))
    (unwind-protect
         (progn
           (with-open-file (stream file
                                   :direction :output
                                   :external-format external-format
                                   :if-exists :supersede)
             (write character :stream stream :readably t))
           (with-open-file (stream file
                                   :direction :input
                                   :external-format external-format
                                   :if-does-not-exist :error)
             (assert (char= (read stream) character))))
      (ignore-errors
        (delete-file file)))))

(with-test (:name (:print-readable :character :utf-8) :skipped-on '(not :sb-unicode))
  (test-readable-character (code-char #xfffe) :utf-8))

(with-test (:name (:print-readable :character :iso-8859-1) :skipped-on '(not :sb-unicode))
  (test-readable-character (code-char #xfffe) :iso-8859-1))

(assert (string= (eval '(format nil "~:C" #\a)) "a"))
(assert (string= (format nil (formatter "~:C") #\a) "a"))

;;; This used to trigger an AVER instead.
(assert-error (eval '(formatter "~>")) sb-format:format-error)
(assert-error (eval '(format t "~>")) sb-format:format-error)

;;; readably printing hash-tables, check for circularity
(let ((x (cons 1 2))
      (h (make-hash-table))
      (*print-readably* t)
      (*print-circle* t)
      (*read-eval* t))
  (setf (gethash x h) h)
  (destructuring-bind (x2 . h2) (read-from-string (write-to-string (cons x h)))
    (assert (equal x x2))
    (assert (eq h2 (gethash x2 h2)))))

;;; an off-by-one error in the ~R format directive until 1.0.15.20
;;; prevented printing cardinals and ordinals between (expt 10 63) and
;;; (1- (expt 10 66))
(assert (string= (format nil "~R" (expt 10 63)) "one vigintillion"))
(assert (string= (format nil "~:R" (expt 10 63)) "one vigintillionth"))

;;; too-clever cacheing for PRINT-OBJECT resulted in a bogus method
;;; for printing RESTART objects.  Check also CONTROL-STACK-EXHAUSTED
;;; and HEAP-EXHAUSTED-ERROR.
(let ((result (with-output-to-string (*standard-output*)
                (princ (find-restart 'abort)))))
  (assert (string/= result "#<" :end1 2)))
(let ((result (with-output-to-string (*standard-output*)
                (princ (make-condition 'sb-kernel::control-stack-exhausted)))))
  (assert (string/= result "#<" :end1 2)))
(let ((result (with-output-to-string (*standard-output*)
                (princ (make-condition 'sb-kernel::heap-exhausted-error)))))
  (assert (string/= result "#<" :end1 2)))

(with-test (:name (:with-standard-io-syntax :bind-print-pprint-dispatch))
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil)))
    (set-pprint-dispatch 'symbol #'(lambda (stream obj)
                                     (declare (ignore obj))
                                     (write-string "FOO" stream)))
    (with-standard-io-syntax
      (let ((*print-pretty* t))
        (assert (string= (princ-to-string 'bar) "BAR"))))))

;;; bug-lp#488979

(defclass a-class-name () ())

(assert (find #\Newline
              (let ((*print-pretty* t)
                    (*print-right-margin* 10))
                (format nil "~A" (make-instance 'a-class-name)))
              :test #'char=))

(assert (not (find #\Newline
                   (let ((*print-pretty* nil)
                         (*print-right-margin* 10))
                     (format nil "~A" (make-instance 'a-class-name)))
                   :test #'char=)))

;;; The PRINT-OBJECT method for RANDOM-STATE used to have a bogus
;;; dimension argument for MAKE-ARRAY.
(with-test (:name :print-random-state)
  (assert (equalp *random-state*
                  (read-from-string
                   (write-to-string *random-state*)))))

(with-test (:name :write-return-value)
  ;; COMPILE is called explicitly because there was a bug in the
  ;; compiler-macro for WRITE, which isn't expanded by the evaluator.
  (assert (= 123 (funcall (compile nil '(lambda () (write 123)))))))

(with-test (:name :write/write-to-string-compiler-macro-lp/598374+581564)
  (let ((test (compile nil
                       `(lambda (object &optional output-stream)
                          (write object
                                 :stream output-stream)))))
    (assert (equal "(HELLO WORLD)"
                   (with-output-to-string (*standard-output*)
                     (let ((list '(hello world)))
                       (assert (eq list (funcall test list)))))))
    (assert (equal "12"
                   (with-output-to-string (*standard-output*)
                     (assert (eql 12 (funcall test 12)))))))
  (let ((test (compile nil
                       `(lambda ()
                          (let ((*print-length* 42))
                            (write-to-string *print-length* :length nil))))))
    (assert (equal "42" (funcall test)))))

(with-test (:name (:format :compile-literal-dest-string))
  (assert (eq :warned
              (handler-case
                  (compile nil
                           `(lambda (x) (format "~A" x)))
                ((and warning (not style-warning)) ()
                  :warned)))))

(with-test (:name :bug-308961)
  (assert (string= (format nil "~4,1F" 0.001) " 0.0"))
  (assert (string= (format nil "~4,1@F" 0.001) "+0.0"))
  (assert (string= (format nil "~E" 0.01) "1.e-2"))
  (assert (string= (format nil "~G" 0.01) "1.00e-2")))

(with-test (:name (:fp-print-read-consistency single-float))
  (let ((*random-state* (make-random-state t))
        (oops))
    (loop for f = most-positive-single-float then (/ f 2.0)
          while (> f 0.0)
          do (loop repeat 10
                   for fr = (random f)
                   do (unless (eql fr (read-from-string (prin1-to-string fr)))
                        (push fr oops)
                        (return))))
    (loop for f = most-negative-single-float then (/ f 2.0)
          while (< f -0.0)
          do (loop repeat 10
                   for fr = (- (random (- f)))
                   do (unless (eql fr (read-from-string (prin1-to-string fr)))
                        (push fr oops)
                        (return))))
    (when oops
      (error "FP print-read inconsistencies:~%~:{  ~S => ~S~%~}"
             (mapcar (lambda (f)
                       (list f (read-from-string (prin1-to-string f))))
                     oops)))))

(with-test (:name (:fp-print-read-consistency double-float))
  (let ((*random-state* (make-random-state t))
        (oops))
    ;; FIXME skipping denormalized floats due to bug 793774.
    (loop for f = most-positive-double-float then (/ f 2d0)
          while (> f 0d0)
          do (loop repeat 10
                   for fr = (random f)
                   do (unless (float-denormalized-p fr)
                        (unless (eql fr (read-from-string (prin1-to-string fr)))
                          (push fr oops)
                          (return)))))
    (loop for f = most-negative-double-float then (/ f 2d0)
          while (< f -0d0)
          do (loop repeat 10
                   for fr = (- (random (- f)))
                   do (unless (float-denormalized-p fr)
                        (unless (eql fr (read-from-string (prin1-to-string fr)))
                          (push fr oops)
                          (return)))))
    (when oops
      (error "FP print-read inconsistencies:~%~:{  ~S => ~S~%~}"
             (mapcar (lambda (f)
                       (list f (read-from-string (prin1-to-string f))))
                     oops)))))

(with-test (:name :bug-811386)
  (assert (equal "   0.00" (format nil "~7,2,-2f" 0)))
  (assert (equal "   0.00" (format nil "~7,2,2f" 0)))
  (assert (equal "   0.01" (format nil "~7,2,-2f" 1)))
  (assert (equal " 100.00" (format nil "~7,2,2f" 1)))
  (assert (equal "   0.00" (format nil "~7,2,-2f" 0.1)))
  (assert (equal "  10.00" (format nil "~7,2,2f" 0.1)))
  (assert (equal "   0.01" (format nil "~7,2,-2f" 0.5))))

(with-test (:name :bug-867684)
  (assert (equal "ab" (format nil "a~0&b"))))

(with-test (:name :print-unreadably-function)
  (assert (equal "\"foo\""
                 (handler-bind ((print-not-readable #'sb-ext:print-unreadably))
                   (write-to-string (coerce "foo" 'base-string) :readably t)))))

(with-test (:name :printing-specialized-arrays-readably)
  (let ((*read-eval* t)
        (dimss (loop repeat 10
                     collect (loop repeat (1+ (random 3))
                                   collect (1+ (random 10)))))
        (props sb-vm::*specialized-array-element-type-properties*))
    (labels ((random-elt (type)
               (case type
                 (base-char
                  (code-char (random 128)))
                 (character
                  (code-char (random char-code-limit)))
                 (single-float
                  (+ least-positive-normalized-single-float
                     (random most-positive-single-float)))
                 (double-float
                  (+ least-positive-normalized-double-float
                         (random most-positive-double-float)))
                 (bit
                  (random 2))
                 (fixnum
                  (random most-positive-fixnum))
                 ((t)
                  t)
                 (otherwise
                  (destructuring-bind (type x) type
                    (ecase type
                      (unsigned-byte
                       (random (1- (expt 2 x))))
                      (signed-byte
                       (- (random (expt 2 (1- x)))))
                      (complex
                       (complex (random-elt x) (random-elt x)))))))))
      (dotimes (i (length props))
        (let ((et (sb-vm::saetp-specifier (aref props i))))
          (when et
            (when (eq 'base-char et)
              ;; base-strings not included in the #. printing.
              (go :next))
            (dolist (dims dimss)
              (let ((a (make-array dims :element-type et)))
                (assert (equal et (array-element-type a)))
                (dotimes (i (array-total-size a))
                  (setf (row-major-aref a i) (random-elt et)))
                (let ((copy (read-from-string (write-to-string a :readably t))))
                  (assert (equal dims (array-dimensions copy)))
                  (assert (equal et (array-element-type copy)))
                  (assert (equal (array-total-size a) (array-total-size copy)))
                  (dotimes (i (array-total-size a))
                    (assert (equal (row-major-aref a i) (row-major-aref copy i)))))))))
        :next))))

(with-test (:name (:format :negative-colinc-and-mincol))
  (assert-error (format nil "~-2a" 1))
  (assert-error (format nil "~,0a" 1)))

(with-test (:name :bug-905817)
  ;; The bug manifests itself in an endless loop in FORMAT.
  ;; Correct behaviour is to signal an error.
  (handler-case
      (with-timeout 5
        (assert-error (format nil "e~8,0s" 12395)))
    (timeout ()
      (error "Endless loop in FORMAT"))))

(with-test (:name :format-type-check)
  (assert (equal "1/10" (format nil "~2r" 1/2)))
  (assert-error (format nil "~r" 1.32) sb-format:format-error)
  (assert-error (format nil "~c" 1.32) sb-format:format-error)
  (assert (equal "1/10" (eval '(format nil "~2r" 1/2))))
  (assert-error (eval '(format nil "~r" 1.32)) sb-format:format-error)
  (assert-error (eval '(format nil "~c" 1.32)) sb-format:format-error))

;; Setup for test of print-object on a class with no proper name.
;; There are various PRINT-OBJECT tests strewn throughout, all of which
;; are not in the obvious place to me, except for perhaps 'clos.impure'
;; which is also not the right place because that file concerns CLOS
;; behavior in general, not to mention being far too noisy in its output.
(defclass fruit () (a))
(defvar *fruit1* (find-class 'fruit))
(setf (find-class 'fruit) nil)
(defclass fruit () (n o))
(defvar *fruit2* (find-class 'fruit))
(setf (find-class 'fruit) nil)
(defclass fruit () (x))
(with-test (:name (print-object :improper-class-name))
  (assert (string/= (write-to-string *fruit1*) (write-to-string *fruit2*)))
  (assert (string/= (write-to-string (find-class 'fruit))
                    (write-to-string *fruit1*))))

(with-test (:name :format-readably)
  (let ((*print-readably* t))
    (assert (format nil "~$" #'format))
    (assert (format nil "~d" #'format))
    (assert (format nil "~x" #'format))
    (assert (format nil "~b" #'format))
    (assert (format nil "~3r" #'format))
    (locally
        (declare (notinline format))
      (assert (format nil "~$" #'format))
      (assert (format nil "~d" #'format))
      (assert (format nil "~x" #'format))
      (assert (format nil "~b" #'format))
      (assert (format nil "~3r" #'format)))))

(with-test (:name :format-print-base)
  (let ((*print-base* 3))
    (assert (equal (format nil "~g" '(123)) "(123)"))
    (assert (equal (format nil "~f" '(123)) "(123)"))
    (assert (equal (format nil "~e" '(123)) "(123)"))
    (assert (equal (format nil "~$" '(123)) "(123)"))
    (locally
        (declare (notinline format))
      (assert (equal (format nil "~g" '(123)) "(123)"))
      (assert (equal (format nil "~f" '(123)) "(123)"))
      (assert (equal (format nil "~e" '(123)) "(123)"))
      (assert (equal (format nil "~$" '(123)) "(123)")))))

(with-test (:name :format-concatenate)
  (assert (equal
           (funcall (compile nil `(lambda (x) (format nil "~s" (the string x))))
                    "\\")
           (prin1-to-string "\\"))))

(with-test (:name :write-stream-nil)
  (assert
   (equal
    (with-output-to-string (*standard-output*)
      (funcall (compile nil `(lambda () (write "xx" :stream nil)))))
    "\"xx\"")))

(define-condition foo () (a))
(defvar *ccc* (make-condition 'foo))
(define-condition foo (warning) (a))
(with-test (:name :write-obsolete-condition)
  (assert (search "UNPRINTABLE" (write-to-string *ccc*))))
