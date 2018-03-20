;;;; miscellaneous compiler tests with side effects (e.g. DEFUN
;;;; changing FDEFINITIONs and globaldb stuff)

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

(declaim (optimize (debug 3) (speed 2) (space 1)))

;;; Until version 0.6.9 or so, SBCL's version of Python couldn't do
;;; this correctly, due to the bug patched by Rob MacLachlan on the
;;; cmucl-imp list 2000-06-21, and applied to SBCL by Martin Atzmueller.
;;; (The effectiveness of the test also depends on the implicit
;;; function typing of Python (where DEFUN is like DECLAIM FTYPE),
;;; which violates the ANSI spec, and should be fixed. Once that
;;; unrelated bug is fixed, this code will no longer test the type
;;; inference behavior it's intended to test.)
(defun emptyvalues (&rest rest) (declare (ignore rest)) (values))
(defstruct foo x y)
(defun bar ()
  (let ((res (emptyvalues)))
    (unless (typep res 'foo)
      'expected-value)))
(assert (eq (bar) 'expected-value))

(declaim (ftype (function (real) (values integer single-float)) valuesify))
(defun valuesify (x)
  (values (round x)
          (coerce x 'single-float)))
(defun exercise-valuesify (x)
  (multiple-value-bind (i f) (valuesify x)
    (declare (type integer i))
    (declare (type single-float f))
    (+ i f)))
(assert (= (exercise-valuesify 1.25) 2.25))

;;; An early version (sbcl-0.6.11.33) of code to check FTYPEs from DEFUN
;;; against DECLAIMed FTYPEs blew up when an FTYPE was DECLAIMed
;;; to be pure FUNCTION, because the internal representation of
;;; FUNCTION itself (as opposed to subtypes of FUNCTION, such as
;;; (FUNCTION () T)) is a BUILT-IN-CLASS object, not a FUN-TYPE
;;; object.
(declaim (ftype function i-am-just-a-function))
(defun i-am-just-a-function (x y) (+ x y 1))

;;; Stig E Sandoe reported in cclan-Bugs-431263 that SBCL couldn't
;;; compile this. sbcl-0.6.12.26 died in CIRCULAR-LIST-P with "The
;;; value \"EST\" is not of type LIST." Dan Barlow fixed it.
(defvar +time-zones+
  '((5 "EDT" . "EST") (6 "CDT" . "CST") (7 "MDT" .
"MST") (8 "PDT" . "PST")
    (0 "GMT" . "GDT") (-2 "MET" . "MET DST"))
  "*The string representations of the time zones.")

(declaim (optimize (debug 1) (speed 1) (space 1)))

;;; The old CMU CL Python compiler assumed that it was safe to infer
;;; function types (including return types) from function definitions
;;; and then use them to optimize code later [and it was almost
;;; right!]. This is of course bad when functions are redefined. The
;;; problem was fixed in sbcl-0.6.12.57.
(defun foo (x)
          (if (plusp x)
              1.0
              0))
(eval '(locally
        (defun bar (x)
          (typecase (foo x)
            (fixnum :fixnum)
            (real :real)
            (string :string)
            (t :t)))
        (compile 'bar)))
(assert (eql (bar 11) :real))
(assert (eql (bar -11) :fixnum))
(setf (symbol-function 'foo) #'identity)
(assert (eql (bar 11) :fixnum))
(assert (eql (bar -11.0) :real))
(assert (eql (bar "this is a test") :string))
(assert (eql (bar (make-hash-table)) :t))

;;; bug reported by Brian Spilsbury sbcl-devel 2001-09-30, fixed by
;;; Alexey Dejneka patch sbcl-devel 2001-10-02
(defun pixarray-element-size (pixarray)
  (let ((eltype (array-element-type pixarray)))
    (cond ((eq eltype 'bit) 1)
          ((and (listp eltype)
                (eq (first eltype) 'unsigned-byte))
           (second eltype))
          (t
           (error "Invalid pixarray: ~S." pixarray)))))
(assert (eql 1 (pixarray-element-size #*110)))

;;; bug 31 turned out to be a manifestation of non-ANSI array type
;;; handling, fixed by CSR in sbcl-0.7.3.8.
(defun array-element-type-handling (x)
  (declare (optimize safety))
  (declare (type (vector cons) x))
  (when (consp (aref x 0))
    (aref x 0)))
(assert-error
 (array-element-type-handling
  (make-array 3 :element-type t :initial-element 0))
 type-error)

;;; bug 220: type check inserted after all arguments in MV-CALL caused
;;; failure of stack analysis
(defun bug220-helper ()
  13)
(assert (equal (multiple-value-call #'list
                 (the integer (bug220-helper))
                 nil)
               '(13 nil)))

;;; bug 221: sbcl 0.7.9.13 failed to compile the following function
(declaim (ftype (function (fixnum) (values package boolean)) bug221-f1))
(declaim (ftype (function (t) (values package boolean)) bug221-f2))
(defun bug221 (b x)
  (funcall (if b #'bug221-f1 #'bug221-f2) x))

;;; bug 166: compiler failure
(defstruct bug166s)
(defmethod permanentize ((uustk bug166s))
  (flet ((frob (hash-table test-for-deletion)
           )
         (obj-entry.stale? (oe)
           (destructuring-bind (key . datum) oe
             (declare (type simple-vector key))
             (deny0 (void? datum))
             (some #'stale? key))))
    (declare (inline frob obj-entry.stale?))
    (frob (uustk.args-hash->obj-alist uustk)
          #'obj-entry.stale?)
    (frob (uustk.hash->memoized-objs-list uustk)
          #'objs.stale?))
  (call-next-method))

;;; bugs 115, 226: compiler failure in lifetime analysis
(defun bug115-1 ()
  (declare (optimize (speed 2) (debug 3)))
  (flet ((m1 ()
           (unwind-protect nil)))
    (if (catch nil)
        (m1)
        (m1))))

(defun bug115-2 ()
  (declare (optimize (speed 2) (debug 3)))
  (flet ((m1 ()
           (bar (if (foo) 1 2))
           (let ((x (foo)))
             (bar x (list x)))))
    (if (catch nil)
        (m1)
        (m1))))

(defun bug226 ()
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (flet ((safe-format (stream string &rest r)
           (unless (ignore-errors (progn
                                    (apply #'format stream string r)
                                    t))
             (format stream "~&foo ~S" string))))
    (cond
      ((eq my-result :ERROR)
       (cond
         ((ignore-errors (typep condition result))
          (safe-format t "~&bar ~S" result))
         (t
          (safe-format t "~&baz ~S (~A) ~S" condition condition result)))))))

;;; bug 231: SETQ did not check the type of the variable being set
(defun bug231a-1 (x)
  (declare (optimize safety) (type (integer 0 8) x))
  (incf x))
(assert-error (bug231a-1 8) type-error)

(defun bug231a-2 (x)
  (declare (optimize safety) (type (integer 0 8) x))
  (list (lambda (y) (setq x y))
        (lambda () x)))
(destructuring-bind (set get) (bug231a-2 0)
  (funcall set 8)
  (assert (eql (funcall get) 8))
  (assert-error (funcall set 9) type-error)
  (assert (eql (funcall get) 8)))

(defun bug231b (x z)
  (declare (optimize safety) (type integer x))
  (locally
      (declare (type (real 1) x))
    (setq x z))
  (list x z))
(assert-error (bug231b nil 1) type-error)
(assert-error (bug231b 0 1.5) type-error)
(assert-error (bug231b 0 0) type-error)

;;; A bug appeared in flaky7_branch. Python got lost in unconverting
;;; embedded tail calls during let-conversion.
(defun bug239 (bit-array-2 &optional result-bit-array)
  (declare (type (array bit) bit-array-2)
           (type (or (array bit) (member t nil)) result-bit-array))
  (unless (simple-bit-vector-p bit-array-2)
    (multiple-value-call
        (lambda (data1 start1)
          (multiple-value-call
              (lambda (data2 start2)
                (multiple-value-call
                    (lambda (data3 start3)
                      (declare (ignore start3))
                      (print (list data1 data2)))
                  (values 0 0)))
            (values bit-array-2 0)))
      (values 444 0))))
(assert (equal (bug239 (make-array 4 :element-type 'bit
                                   :adjustable t
                                   :initial-element 0)
                       nil)
               '(444 #*0000)))

(defstruct some-structure a)
(eval-when (:compile-toplevel)
  ;; in the big CLASS reorganization in pre8, this would fail with
  ;; SOME-STRUCTURE-A is not FBOUNDP.  Fixed in 0.pre8.64
  (find-class 'some-structure nil))
(eval-when (:load-toplevel)
  (assert (typep (find-class 'some-structure) 'class)))

;; When loading from fasl, if a form in that file tried referencing
;; compiled-debug-info for a function provided earlier in that same file,
;; it wouldn't work because the earlier function's #<code> component is
;; present but without its debug-info-source, which is dumped later.
(defun f1 (x) (+ x 3))
;; There's nothing to assert other than that this works.
(print (multiple-value-list (function-lambda-expression #'f1)))

;; It's possible for an instance to refer to a LAYOUT that has no classoid.
;; This can arise from one file compiling a defstruct and another file compiling
;; a function that refers to an instance of it as a literal either from
;; LOAD-TIME-VALUE or MAKE-LOAD-FORM. Then you restart lisp and load the second
;; but not the first file.  A function to recreate the instance was correctly
;; dumped, but everything including the disassembler would crash trying to view
;; the resultant object.

(eval-when (:compile-toplevel)
  (declaim (inline make-my-awesome-struct))
  (defstruct (my-awesome-struct (:predicate nil) (:copier nil)) a b c)
  (defmethod make-load-form ((self my-awesome-struct) &optional env)
    (declare (ignore env))
    ;; Can't use MAKE-LOAD-FORM-SAVING-SLOTS
    ;; because that goes (ALLOCATE-INSTANCE (FIND-CLASS 'MY-AWESOME-STRUCT).
    ;; Our named constructor is inlined and doesn't need the class.
    (with-slots (a b c) self `(make-my-awesome-struct :a ',a :b ',b :c ',c)))
  (defvar *my-awesome-instance*
    (make-my-awesome-struct :a 1 :b '(foo . #*101) :c #(4 5 3))))

(defun trythis (x)
  ;; use a macro to return the object itself, not the symbol naming it.
  (macrolet ((it () *my-awesome-instance*))
    (list x (it))))

(defun get-foo-val (x) (my-awesome-struct-b x))

;; This approximates the test case - no class's proper name is MY-AWESOME-STRUCT.
;; The old metaobjects are detritus in the object hierarchy but don't affect
;; corectness of the test.
(eval-when (:compile-toplevel)
  (dolist (sym '("MY-AWESOME-STRUCT" "MY-AWESOME-STRUCT-A"
                 "MY-AWESOME-STRUCT-B" "MY-AWESOME-STRUCT-C"))
    (unintern (find-symbol sym))))

(assert (not (find-class 'my-awesome-struct nil)))

(let ((x (trythis 9))
      (*print-pretty* nil))
  (format t "~&Hi! ~S~%" x)
  (assert (search "UNPRINTABLE" (write-to-string x)))
  (assert (equal (get-foo-val (second x)) '(foo . #*101))))
