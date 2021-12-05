;;;; tests for problems in the interface presented to the user/programmer

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

(defmacro silently (&rest things)
  `(let ((*standard-output* (make-broadcast-stream))) ,@things))

;; Interpreted closure is a problem for COMPILE
(with-test (:name (disassemble function) :skipped-on :interpreter)
  ;; DISASSEMBLE shouldn't fail on closures or unpurified functions
  (defun disassemble-fun (x) x)
  (silently (disassemble 'disassemble-fun)))

(with-test (:name (disassemble :closure) :skipped-on :interpreter)
  (let ((x 1)) (defun disassemble-closure (y) (if y (setq x y) x)))
  (silently (disassemble 'disassemble-closure)))

(defun interpreted-function-p (x) (typep x 'sb-kernel:interpreted-function))

#+(or sb-eval sb-fasteval)
(with-test (:name (disassemble :interpreted))
    ;; Nor should it fail on interpreted functions
    (let ((sb-ext:*evaluator-mode* :interpret))
      (eval `(defun disassemble-eval (x) x))
      (silently (disassemble 'disassemble-eval)))

    ;; disassemble-eval should still be an interpreted function.
    ;; clhs disassemble: "(If that function is an interpreted function,
    ;; it is first compiled but the result of this implicit compilation
    ;; is not installed.)"
    (assert (interpreted-function-p (symbol-function 'disassemble-eval))))

(with-test (:name (disassemble generic-function))
  ;; nor should it fail on generic functions or other funcallable instances
  (defgeneric disassemble-generic (x))
  (silently (disassemble 'disassemble-generic))
  (let ((fin (make-instance 'sb-mop:funcallable-standard-object)))
    (silently (disassemble fin))))

;;; while we're at it, much the same applies to
;;; FUNCTION-LAMBDA-EXPRESSION:
(defun fle-fun (x) x)

(let ((x 1)) (defun fle-closure (y) (if y (setq x y) x)))

(defclass non-standard-generic-function (generic-function) ()
  (:metaclass sb-mop:funcallable-standard-class))
(defmethod sb-mop:generic-function-name ((generic-function non-standard-generic-function))
  'name)

(with-test (:name function-lambda-expression)
  (flet ((fle-name (x)
           (nth-value 2 (function-lambda-expression x))))
    (assert (eql (fle-name #'fle-fun) 'fle-fun))
    (assert (eql (fle-name #'fle-closure) 'fle-closure))
    (assert (eql (fle-name #'disassemble-generic) 'disassemble-generic))
    (function-lambda-expression
     (make-instance 'sb-mop:funcallable-standard-object))
    (function-lambda-expression
     (make-instance 'non-standard-generic-function))
    (function-lambda-expression
     (make-instance 'standard-generic-function))
    #+(or sb-eval sb-fasteval)
    (progn
      (let ((sb-ext:*evaluator-mode* :interpret))
        (eval `(defun fle-eval (x) x))
        (assert (eql (fle-name (symbol-function 'fle-eval)) 'fle-eval)))

      ;; fle-eval should still be an interpreted function.
      (assert (interpreted-function-p (symbol-function 'fle-eval))))))

(with-test (:name :nested-function-lambda-expression)
  (let* ((lexpr
          '(lambda ()
            (labels ((f1 (z &rest r &key (b (eval 'foo)) (a 3) w)
                       (- a w (length r) (f2 (/  z b))))
                     (f2 (w) (* w .3)))
              (values #'f1 #'f2))))
         (f (compile nil lexpr)))
    (assert (equal (function-lambda-expression f) lexpr))
    (assert (equal (function-lambda-expression (funcall f))
                   '(lambda (z &rest r &key (b (eval 'foo)) (a 3) w)
                     (block f1 (- a w (length r) (f2 (/ z b)))))))
    (assert (equal (function-lambda-expression
                    (nth-value 1 (funcall f)))
                   '(lambda (w) (block f2 (* w 0.3)))))))

;;; Tests of documentation on types and classes

(defun assert-documentation (thing doc-type expected)
  ;; This helper function makes ASSERT errors print THING, DOC-TYPE,
  ;; the return value of DOCUMENTATION and EXPECTED.
  (flet ((assert-documentation-helper (thing doc-type documentation expected)
           (declare (ignore thing doc-type))
           (equal documentation expected)))
    (assert (assert-documentation-helper
             thing doc-type (documentation thing doc-type) expected))))

(defpackage #:documentation.package
  (:documentation "PACKAGE"))

(with-test (:name (documentation package))
  (assert-documentation (find-package '#:documentation.package) t "PACKAGE")
  (setf (documentation (find-package '#:documentation.package) t) "PACKAGE2")
  (assert-documentation (find-package '#:documentation.package) t "PACKAGE2"))

(defclass foo ()
  ()
  (:documentation "FOO"))

(defclass documentation.funcallable-instance ()
  ()
  (:metaclass sb-mop:funcallable-standard-class)
  (:documentation "FEZ"))

(defstruct bar "BAR")

(define-condition baz ()
  ()
  (:documentation "BAZ"))

(macrolet
    ((do-class (name expected &optional structurep)
       `(progn
          (assert-documentation ',name 'type ,expected)
          (assert-documentation (find-class ',name) 'type ,expected)
          (assert-documentation (find-class ',name) 't ,expected)
          ,@(when structurep
              `((assert-documentation ',name 'structure ,expected)))

          (let ((new1 (symbol-name (gensym "NEW1")))
                (new2 (symbol-name (gensym "NEW2")))
                (new3 (symbol-name (gensym "NEW3")))
                (new4 (symbol-name (gensym "NEW4"))))
            (declare (ignorable new4))
            (setf (documentation ',name 'type) new1)
            (assert-documentation (find-class ',name) 'type new1)
            (setf (documentation (find-class ',name) 'type) new2)
            (assert-documentation (find-class ',name) 't new2)
            (setf (documentation (find-class ',name) 't) new3)
            (assert-documentation ',name 'type new3)
            ,@(when structurep
                `((assert-documentation ',name 'structure new3)
                  (setf (documentation ',name 'structure) new4)
                  (assert-documentation ',name 'structure new4)))))))

  (with-test (:name (documentation class standard-class))
    (do-class foo "FOO"))

  (with-test (:name (documentation class sb-mop:funcallable-standard-class))
    (do-class documentation.funcallable-instance "FEZ"))

  (with-test (:name (documentation struct 1))
    (do-class bar "BAR" t))

  (with-test (:name (documentation condition))
    (do-class baz "BAZ")))

(defclass documentation-metaclass (standard-class)
  ()
  (:documentation "metaclass with methods on DOCUMENTATION."))

(defmethod documentation ((thing documentation-metaclass)
                          (doc-type (eql 't)))
  (sb-int:awhen (call-next-method)
    (concatenate 'string ":" sb-int:it)))

(defmethod (setf documentation) (new-value
                                 (thing documentation-metaclass)
                                 (doc-type (eql 't)))
  (call-next-method (when new-value
                      (substitute #\! #\. new-value))
                    thing doc-type))

(defmethod sb-mop:validate-superclass ((class documentation-metaclass)
                                       (superclass standard-class))
  t)

(defclass documentation-class ()
  ()
  (:metaclass documentation-metaclass)
  (:documentation "normal"))

(with-test (:name (documentation :non-stanadard :metaclass))
  (flet ((check (expected class-name)
           (let ((class (find-class class-name)))
             (assert-documentation class-name 'type expected)
             (assert-documentation class 'type expected)
             (assert-documentation class t expected))))
    ;; Make sure methods specialized on the metaclass are not bypassed
    ;; when retrieving and modifying class documentation.
    (check ":normal" 'documentation-class)
    (setf (documentation 'documentation-class 'type) "2.")
    (check ":2!" 'documentation-class)
    (setf (documentation 'documentation-class 'type) nil)
    (check nil 'documentation-class)

    ;; Sanity check: make sure the metaclass has its own documentation
    ;; and is not affected by the above modifications.
    (check "metaclass with methods on DOCUMENTATION."
           'documentation-metaclass)))

(defstruct (frob (:type vector)) "FROB")

(with-test (:name (documentation struct 2))
  (assert-documentation 'frob 'structure "FROB")
  (setf (documentation 'frob 'structure) "NEW5")
  (assert-documentation 'frob 'structure "NEW5"))

(deftype quux ()
  "QUUX"
  't)

(with-test (:name (documentation type))
  (assert-documentation 'quux 'type "QUUX")
  (setf (documentation 'quux 'type) "NEW4")
  (assert-documentation 'quux 'type "NEW4"))

(define-compiler-macro cmacro (x)
  "compiler macro"
  x)

(define-compiler-macro (setf cmacro) (y x)
  "setf compiler macro"
  (declare (ignore x))
  y)

(with-test (:name (documentation compiler-macro))
  (assert-documentation 'cmacro 'compiler-macro "compiler macro")
  (assert-documentation '(setf cmacro) 'compiler-macro "setf compiler macro"))

(defun (setf documentation.setf) (x)
  "(setf foo) documentation"
  x)

(with-test (:name (documentation function setf))
  (flet ((expect (documentation)
           (assert-documentation
            '(setf documentation.setf) 'function documentation)
           (assert-documentation
            #'(setf documentation.setf) 'function documentation)
           (assert-documentation
            #'(setf documentation.setf) t documentation)))
    (expect "(setf foo) documentation")
    ;; The original test checked this twice. No idea why.
    (expect "(setf foo) documentation")

    ;; Modification
    (setf (documentation '(setf documentation.setf) 'function)
          "(setf bar) documentation")
    (expect "(setf bar) documentation")

    (setf (documentation #'(setf documentation.setf) 'function)
          "(setf baz) documentation")
    (expect "(setf baz) documentation")

    (setf (documentation #'(setf documentation.setf) t)
          "(setf fez) documentation")
    (expect "(setf fez) documentation")))

(with-test (:name (documentation lambda))
  (let ((f (lambda () "aos the zos" t))
        (g (sb-int:named-lambda fii () "zoot the fruit" t)))
    (dolist (doc-type '(t function))
      (assert-documentation f doc-type "aos the zos")
      (assert-documentation g doc-type "zoot the fruit"))
    (setf (documentation f t) "fire")
    (assert-documentation f t "fire")
    (assert-documentation g t "zoot the fruit")))

(with-test (:name (documentation flet))
  (assert
   (string= (documentation
             (flet ((quux (x)
                      "this is FLET quux"
                      (/ x 2)))
               #'quux)
             t)
            "this is FLET quux")))

(with-test (:name (documentation labels))
  (assert
   (string= (documentation
             (labels ((rec (x)
                        "this is LABELS rec"
                        (if (plusp x)
                            (* x (rec (1- x)))
                            1)))
               #'rec)
             t)
            "this is LABELS rec")))

(let ((x 1))
  (defun docfoo (y)
    "bar"
    (incf x y)))

(with-test (:name (documentation :closure))
  (assert-documentation 'docfoo 'function "bar")
  (assert (string= (setf (documentation 'docfoo 'function) "baz") "baz"))
  (assert-documentation 'docfoo 'function "baz")
  (assert-documentation #'docfoo t "baz")
  (assert (string= (setf (documentation #'docfoo t) "zot") "zot"))
  (assert-documentation #'docfoo t "zot")
  (assert-documentation 'docfoo 'function "zot")
  (assert (not (setf (documentation 'docfoo 'function) nil)))
  (assert-documentation 'docfoo 'function nil))

(with-test (:name (documentation :built-in-macro) :skipped-on (not :sb-doc))
  (assert (documentation 'trace 'function)))

(with-test (:name (documentation :built-in-function) :skipped-on (not :sb-doc))
  (assert (documentation 'cons 'function)))

(defvar documentation.variable nil
  "foo variable documentation")

(with-test (:name (documentation variable))
  (assert-documentation 'documentation.variable 'variable
                        "foo variable documentation")
  (setf (documentation 'documentation.variable 'variable)
        "baz variable documentation")
  (assert-documentation 'documentation.variable 'variable
                        "baz variable documentation"))

(with-test (:name (documentation :mismatch-for-function))
  (defun test ()
    "X"
    nil)
  (setf (symbol-function 'test2) #'test)
  (setf (documentation 'test 'function) "Y")
  (assert (equal (documentation #'test t)
                 (documentation 'test 'function)))
  (setf (documentation 'test2 'function) "Z")
  (assert (not
           (equal (documentation 'test 'function)
                  (documentation 'test2 'function)))))

(with-test (:name (documentation setf :on nil))
  (assert
   (handler-case
       (assert (equal (setf (documentation nil 'function) "foo") "foo"))
     (style-warning () t)
     (:no-error (x)
       (declare (ignore x))
       nil))))

(with-test (:name (describe generic-function :assumed-type))
  ;; Signalled an error at one point
  (let ((fun (checked-compile '(lambda ()
                                 (flet ((zoo () (gogo)))
                                   (defmethod gogo () nil)
                                   (describe 'gogo)))
                              :allow-style-warnings t)))
    (handler-bind ((warning #'muffle-warning)) ; implicit gf
      (silently (funcall fun)))))

(defmacro bug-643958-test ()
  "foo"
  :ding!)

(with-test (:name :bug-643958)
  (assert (equal "foo" (documentation 'bug-643958-test 'function)))
  (setf (documentation 'bug-643958-test 'function) "bar")
  (assert (equal "bar" (documentation 'bug-643958-test 'function))))

(with-test (:name (:endianness :in *features*))
  (assert
   (or (member :big-endian *features*)
       (member :little-endian *features*))))

(with-test (:name (apropos :inherited :bug-1364413))
  (let* ((package (make-package "BUGGALO" :use nil))
         (symbol (intern "BUGGALO" package)))
    (export (list symbol) package)
    (let ((inherits (make-package "BUGGALO-INHERITS" :use (list package))))
      (assert (= (length (apropos-list "BUGGALO" package)) 1))
      (assert (= (length (apropos-list "BUGGALO" inherits)) 1))
      (delete-package inherits))
    (delete-package package)))

(with-test (:name (apropos :inherited :external-only :bug-1364413))
  (let* ((package (make-package "BUGGALO" :use nil))
         (symbol (intern "BUGGALO" package)))
    (export (list symbol) package)
    (let ((inherits (make-package "BUGGALO-INHERITS" :use (list package))))
      (assert (= (length (apropos-list "BUGGALO" package t)) 1))
      (assert (= (length (apropos-list "BUGGALO" inherits t)) 0))
      (delete-package inherits))
    (delete-package package)))

(with-test (:name (apropos :once-only))
  (assert (= (length (apropos-list "UPDATE-INSTANCE-FOR-REDEFINED-CLASS")) 1)))

(defgeneric gf-arglist-1 (x &key y))
(defmethod gf-arglist-1 (x &key (y nil) (z nil z-p))
  (list x y z z-p))

(defgeneric gf-arglist-2 (x &key y))
(defmethod gf-arglist-2 ((x integer) &key (y nil) ((z f) nil z-p)) (list x y f z-p))
(defmethod gf-arglist-2 ((x string) &key (y nil) ((z w) nil z-p)) (list x y w z-p))

(defgeneric gf-arglist-3 (x &key ((:y y))))

(defgeneric gf-arglist-4 (x &key ((:y z))))

(defgeneric gf-arglist-5 (x &key y))
(defmethod gf-arglist-5 ((x integer) &key z &allow-other-keys) (list x z))

(with-test (:name (:generic-function-pretty-arglist 1))
  (assert (equal (sb-pcl::generic-function-pretty-arglist #'gf-arglist-1)
                 '(x &key y z))))
(with-test (:name (:generic-function-pretty-arglist 2))
  (assert (or (equal (sb-pcl::generic-function-pretty-arglist #'gf-arglist-2)
                     '(x &key y ((z w))))
              (equal (sb-pcl::generic-function-pretty-arglist #'gf-arglist-2)
                     '(x &key y ((z f)))))))
(with-test (:name (:generic-function-pretty-arglist 3))
  (assert (equal (sb-pcl::generic-function-pretty-arglist #'gf-arglist-3)
                 '(x &key y))))
(with-test (:name (:generic-function-pretty-arglist 4))
  (assert (equal (sb-pcl::generic-function-pretty-arglist #'gf-arglist-4)
                 '(x &key ((:y z))))))
(with-test (:name (:generic-function-pretty-arglist 5))
  (assert (equal (sb-pcl::generic-function-pretty-arglist #'gf-arglist-5)
                 '(x &key y z &allow-other-keys))))

(defgeneric traced-gf (x))
(defmethod traced-gf (x) (1+ x))

(with-test (:name (trace generic-function))
  (assert (= (traced-gf 3) 4))
  (trace traced-gf)
  (let ((output (with-output-to-string (*trace-output*)
                  (assert (= (traced-gf 3) 4)))))
    (assert (> (length output) 0))
    (assert (search "0: (TRACED-GF 3)" output))
    (assert (search "0: TRACED-GF returned 4" output)))
  (assert (typep #'traced-gf 'standard-generic-function))
  (untrace traced-gf)
  (let ((output (with-output-to-string (*trace-output*)
                  (assert (= (traced-gf 3) 4)))))
    (assert (= (length output) 0))))

(untrace traced-gf)
(with-test (:name (trace generic-function :methods t))
  (trace :methods t traced-gf)
  (let ((output (with-output-to-string (*trace-output*)
                  (assert (= (traced-gf 4) 5)))))
    (assert (> (length output) 0))
    (assert (search "0: (TRACED-GF 4)" output))
    (assert (search "0: TRACED-GF returned 5" output))
    (assert (search "1: ((METHOD TRACED-GF (T)) 4)" output))
    (assert (search "1: (METHOD TRACED-GF (T)) returned 5" output)))
  (untrace traced-gf)
  (let ((output (with-output-to-string (*trace-output*)
                  (assert (= (traced-gf 3) 4)))))
    (assert (= (length output) 0))))

(defmethod traced-gf :before (x) :before)
(defmethod traced-gf :after ((x integer)) :after)

(untrace traced-gf)
(with-test (:name (trace generic-function :methods :combined))
  (trace :methods t traced-gf)
  (let ((output (with-output-to-string (*trace-output*)
                  (assert (= (traced-gf 5) 6)))))
    (assert (> (length output) 0))
    (assert (search "0: (TRACED-GF 5)" output))
    (assert (search "0: TRACED-GF returned 6" output))
    (assert (search "1: ((SB-PCL::COMBINED-METHOD TRACED-GF) 5)" output))
    (assert (search "1: (SB-PCL::COMBINED-METHOD TRACED-GF) returned 6" output))
    (assert (search "2: ((METHOD TRACED-GF :BEFORE (T)) 5)" output))
    (assert (search "2: (METHOD TRACED-GF :BEFORE (T)) returned :BEFORE" output))
    (assert (search "2: ((METHOD TRACED-GF :AFTER (INTEGER)) 5)" output))
    (assert (search "2: (METHOD TRACED-GF :AFTER (INTEGER)) returned :AFTER" output))
    (assert (search "2: ((METHOD TRACED-GF (T)) 5)" output))
    (assert (search "2: (METHOD TRACED-GF (T)) returned 6" output)))
  (untrace traced-gf)
  (let ((output (with-output-to-string (*trace-output*)
                  (assert (= (traced-gf 3) 4)))))
    (assert (= (length output) 0))))

(let* ((mf (lambda (args nms)
             (declare (ignore nms))
             (* 2 (car args))))
       (m (make-instance 'standard-method
                         :specializers (list (find-class 'integer))
                         :qualifiers nil
                         :lambda-list '(x)
                         :function mf)))
  (add-method #'traced-gf m))

(untrace traced-gf)
(with-test (:name (trace generic-function :methods :method-function))
  (trace :methods t traced-gf)
  (let ((output (with-output-to-string (*trace-output*)
                  (assert (= (traced-gf 5) 10)))))
    (assert (> (length output) 0))
    (assert (search "0: (TRACED-GF 5)" output))
    (assert (search "0: TRACED-GF returned 10" output))
    (assert (search "1: ((SB-PCL::COMBINED-METHOD TRACED-GF) 5)" output))
    (assert (search "1: (SB-PCL::COMBINED-METHOD TRACED-GF) returned 10" output))
    (assert (search "2: ((METHOD TRACED-GF :BEFORE (T)) 5)" output))
    (assert (search "2: (METHOD TRACED-GF :BEFORE (T)) returned :BEFORE" output))
    (assert (search "2: ((METHOD TRACED-GF :AFTER (INTEGER)) 5)" output))
    (assert (search "2: (METHOD TRACED-GF :AFTER (INTEGER)) returned :AFTER" output))
    (assert (search "2: ((METHOD TRACED-GF (INTEGER)) 5)" output))
    (assert (search "2: (METHOD TRACED-GF (INTEGER)) returned 10" output)))
  (untrace traced-gf)
  (let ((output (with-output-to-string (*trace-output*)
                  (assert (= (traced-gf 5) 10)))))
    (assert (= (length output) 0))))

(with-test (:name :undefined-fun-macro-error)
  (assert (search "is a macro" (princ-to-string (make-condition 'undefined-function :name 'cond)))))

(defun testme (a b) (values "nice" (+ a b)))
(compile 'testme)
(defparameter trace-this-f1 #'testme)
(sb-int:encapsulate-funobj trace-this-f1 (sb-int:find-fdefn 'testme))

(defun funky (a b c) (lambda (z) (values "nice" a b (+ (incf a) (decf c) z))))
(compile 'funky)
(defparameter trace-this-f2 (funky 10 'wat 19))
(setf (symbol-function 'funky-closure) trace-this-f2)
(sb-int:encapsulate-funobj trace-this-f2 (sb-int:find-fdefn 'trace-this-f2))

(with-test (:name :trace-funobj-encapsulation)
  (assert (search "returned \"nice\""
                  (with-output-to-string (*trace-output*) (funcall trace-this-f1 1 2))))
  (assert (search "returned \"nice\""
                  (with-output-to-string (*trace-output*) (testme 3 4))))
  (assert (search "returned \"nice\""
                  (with-output-to-string (*trace-output*) (funcall trace-this-f2 1))))
  (assert (search "returned \"nice\""
                  (with-output-to-string (*trace-output*) (funky-closure 5)))))

;;; https://bugs.launchpad.net/sbcl/+bug/1850531
(with-test (:name :describe-function-not-named-by-designator)
  (describe (formatter "~&~A~A") (make-broadcast-stream))) ; should not crash

(defun test-intercepted-load (arg) (apply #'load arg (list :foo :bar :allow-other-keys t)))
(compile 'test-intercepted-load)
(sb-int:encapsulate 'load 'interceptor
 (compile nil '(lambda (realfun pathname &rest things)
                (if (eq pathname :testme)
                    :yes
                    (apply realfun pathname things)))))

(with-test (:name :load-encapsulatable)
  (assert (eq (test-intercepted-load :testme) :yes)))

;;;; success
