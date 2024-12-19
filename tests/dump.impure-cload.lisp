;;;; tests related to the way objects are dumped into fasl files

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "compiler-test-util.lisp"))

(declaim (optimize (debug 3) (speed 2) (space 1)))
(declaim (muffle-conditions compiler-note))

;;; this would fail an AVER in NOTE-POTENTIAL-CIRCULARITY
(defparameter *circular-2d-array* #1=#2A((a b) (#1# x)))

;;; Don Geddis reported this test case 25 December 1999 on a CMU CL
;;; mailing list: dumping circular lists caused the compiler to enter
;;; an infinite loop. Douglas Crosher reported a patch 27 Dec 1999.
;;; The patch was tested on SBCL by Martin Atzmueller 2 Nov 2000, and
;;; merged in sbcl-0.6.8.11.
(defun q-dg1999-1 () (dolist (x '#1=("A" "B" . #1#)) (progn x)))
(defun q-dg1999-2 () (dolist (x '#1=("C" "D" . #1#)) (progn x)))
(defun q-dg1999-3 () (dolist (x '#1=("E" "F" . #1#)) (progn x)))
(defun q-dg1999-4 () (dolist (x '#1=("C" "D" . #1#)) (progn x)))
(defun useful-dg1999 (keys)
  (declare (type list keys))
  (loop
      for c in '#1=("Red" "Blue" . #1#)
      for key in keys))

;;; sbcl-0.6.11.25 or so had DEF!STRUCT/MAKE-LOAD-FORM/HOST screwed up
;;; so that the compiler couldn't dump pathnames.
(format t "Now the compiler can dump pathnames again: ~S ~S~%" #p"" #p"/x/y/z")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct foo x y)
  (defmethod make-load-form ((foo foo) &optional env)
    (declare (ignore env))
    ;; an extremely meaningless MAKE-LOAD-FORM method whose only point
    ;; is to exercise the mechanism a little bit
    (values `(make-foo :x (list ',(foo-x foo)))
            `(setf (foo-y ,foo) ',foo))))

(defparameter *foo*
  #.(make-foo :x "X" :y "Y"))

(assert (equalp (foo-x *foo*) '("X")))
(assert (locally (declare (notinline eql)) ; noise suppression
                 (eql (foo-y *foo*) *foo*)))

;;; Logical pathnames should be dumpable, too, but what does it mean?
;;; As of sbcl-0.7.7.16, we've taken dumping the host part to mean
;;; dumping a reference to the name of the host (much as dumping a
;;; symbol involves dumping a reference to the name of its package).
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (logical-pathname-translations "MY-LOGICAL-HOST")
        (list '("**;*.*.*" "/tmp/*.*"))))

(defparameter *path* #p"MY-LOGICAL-HOST:FOO;BAR.LISP")

;;; Non-SIMPLE-ARRAY VECTORs should be dumpable, though they can lose
;;; their complex attributes.

(defparameter *string* #.(make-array 3 :initial-element #\a
                                       :fill-pointer 2
                                       :element-type 'character))

;;; SBCL 0.7.8 incorrectly read high bits of (COMPLEX DOUBLE-FLOAT)
;;; components as unsigned bytes.
(defparameter *numbers*
  '(-1s0 -1f0 -1d0 -1l0
    #c(-1s0 -1s0) #c(-1f0 -1f0) #c(-1d0 -1d0) #c(-1l0 -1l0)))

;;; tests for MAKE-LOAD-FORM-SAVING-SLOTS
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct savable-structure
    (a nil :type symbol)
    (b nil :type symbol :read-only t)
    (c nil :read-only t)
    (d 0 :type fixnum)
    (e 17 :type (unsigned-byte 32) :read-only t))
  (defmethod make-load-form ((s savable-structure) &optional env)
    (make-load-form-saving-slots s :environment env)))
(defparameter *savable-structure*
  #.(make-savable-structure :a t :b 'frob :c 1 :d 39 :e 19))
(assert (eql (savable-structure-a *savable-structure*) t))
(assert (eql (savable-structure-b *savable-structure*) 'frob))
(assert (eql (savable-structure-c *savable-structure*) 1))
(assert (eql (savable-structure-d *savable-structure*) 39))
(assert (eql (savable-structure-e *savable-structure*) 19))

;;; null :SLOT-NAMES /= unsupplied
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass savable-class ()
    ((a :initform t :initarg :a)))
  (defmethod make-load-form ((s savable-class) &optional env)
    (make-load-form-saving-slots s :environment env :slot-names '())))
(defparameter *savable-class*
  #.(make-instance 'savable-class :a 3))
(assert (not (slot-boundp *savable-class* 'a)))


;;; ensure that we can dump and reload specialized arrays whose element
;;; size is smaller than a byte (caused a few problems circa SBCL
;;; 0.8.14.4)

(defvar *1-bit* #.(make-array 5 :element-type 'bit :initial-element 0))
(defvar *2-bit* #.(make-array 5 :element-type '(unsigned-byte 2) :initial-element 0))
(defvar *4-bit* #.(make-array 5 :element-type '(unsigned-byte 4) :initial-element 1))

;;; tests for constant coalescing (and absence of such) in the
;;; presence of strings.
(progn
  (defvar *character-string-1* #.(make-string 5 :initial-element #\a))
  (defvar *character-string-2* #.(make-string 5 :initial-element #\a))
  (assert (eq *character-string-1* *character-string-2*))
  (assert (typep *character-string-1* '(simple-array character (5)))))

(progn
  (defvar *base-string-1*
    #.(make-string 5 :initial-element #\b :element-type 'base-char))
  (defvar *base-string-2*
    #.(make-string 5 :initial-element #\b :element-type 'base-char))
  (assert (eq *base-string-1* *base-string-2*))
  (assert (typep *base-string-1* '(simple-base-string 5))))

#-#.(cl:if (cl:subtypep 'cl:character 'cl:base-char) '(and) '(or))
(progn
  (defvar *base-string*
    #.(make-string 5 :element-type 'base-char :initial-element #\x))
  (defvar *character-string*
    #.(make-string 5 :initial-element #\x))
  (assert (not (eq *base-string* *character-string*)))
  (assert (typep *base-string* 'base-string))
  (assert (typep *character-string* '(vector character))))

;; Preparation for more MAKE-LOAD-FORM tests
(eval-when (:compile-toplevel :load-toplevel :execute)

 (locally
  ;; this file's global SPEED proclamation generates a lot of unwanted noise
  (declare (optimize (speed 1)))
  (defstruct airport
    name code
    (latitude nil :type double-float)
    (longitude nil :type double-float))

  (defmethod make-load-form ((self airport) &optional env)
    (make-load-form-saving-slots self :environment env))

  (defun compute-airports (n)
    (let ((a (make-array n)))
      (dotimes (i n a)
        (setf (aref a i) (make-airport :code (format nil "~36,3,'0R" i)
                                       :name (format nil "airport~d" i)
                                       :latitude (+ 40 (/ i 1000.0d0))
                                       :longitude (+  100 (/ i 1000.0d0)))))))
  (defstruct s1
    (w 0 :type sb-ext:word)
    (sf 0f0 :type single-float)
    (df 0d0 :type double-float)
    (csf #c(0f0 0f0) :type (complex single-float))
    (cdf #c(0d0 0d0) :type (complex double-float))
    (kids nil)
    (friends nil))

  (defstruct s2
    (id)
    (friends)
    (parent))

  (defmethod make-load-form ((self s1) &optional env)
    (declare (ignore env))
    (ecase (s1-w self)
      (1
       ;; return gratuitously modified expressions
       (multiple-value-bind (alloc init)
           (make-load-form-saving-slots self)
         (values (list 'progn alloc) (list 'progn init))))
      (2
       ;; omit the (complex double-float) slot
       (make-load-form-saving-slots self
                                    ;; nonexistent names are ignored
                                    :slot-names '(w sf df csf bogus
                                                  kids friends)))
      (3
       (make-load-form-saving-slots self)))) ; normal

  (defmethod make-load-form ((self s2) &optional env)
    (declare (ignore env))
    (make-load-form-saving-slots self))

  (defun compute-tangled-stuff ()
    (flet ((circular-list (x)
             (let ((list (list x)))
               (rplacd list list))))
      (let* ((a (make-s1 :w 1
                         :sf 1.25f-9
                         :df 1048d50
                         :csf #c(8.45f1 -9.35f2)
                         :cdf #c(-5.430005d10 2.875d0)))
             (b (make-s1 :w 2
                         :sf 2f0
                         :df 3d0
                         :csf #c(4f0 5f0)
                         :cdf #c(6d0 7d0)))
             (c (make-s1 :w 3
                         :sf -2f0
                         :df -3d0
                         :csf #c(-4f0 -5f0)
                         :cdf #c(-6d0 -7d0)))
             (k1 (make-s2 :id 'b-kid1 :parent b))
             (k2 (make-s2 :id 'c-kid1 :parent c)))
        (setf (s2-friends k1) (list k2)
              (s2-friends k2) (list k1))
        (setf (s1-kids b) (list k1 (make-s2 :id 'b-kid2 :parent b))
              (s1-kids c) (list k2)
              (s1-friends a) (list* b c (circular-list a))
              (s1-friends b) (list c a)
              (s1-friends c) (list a b))
        (list a b c))))

)) ; end EVAL-WHEN

(defun load-form-is-default-mlfss-p (object)
  (multiple-value-bind (creation-form init-form)
      (make-load-form object)
    (multiple-value-bind (ss-creation-form ss-init-form)
        (make-load-form-saving-slots object)
      (and (equal creation-form ss-creation-form)
           (equal init-form ss-init-form)))))

;; Redefine the MAKE-LOAD-FORM method on FOO.
(remove-method #'make-load-form (find-method #'make-load-form nil (list 'foo)))
(defvar *foo-save-slots* nil)
(defmethod make-load-form ((self foo) &optional env)
  (declare (ignore env))
  (if (eq *foo-save-slots* :all)
      (make-load-form-saving-slots self)
      (make-load-form-saving-slots self :slot-names *foo-save-slots*)))
(with-test (:name :load-form-canonical-p)
  (let ((foo (make-foo :x 'x :y 'y)))
    (flet ((assert-canonical (slots)
             (let ((*foo-save-slots* slots))
               (assert (load-form-is-default-mlfss-p foo)))))
      (assert-canonical :all)
      (assert-canonical '(x y)) ; specifying all slots explicitly is still canonical
      (assert-canonical '(y x)))
    ;; specifying only one slot is not canonical
    (let ((*foo-save-slots* '(x)))
      (assert (not (load-form-is-default-mlfss-p foo))))))

;; A huge constant vector. This took 9 seconds to compile (on a MacBook Pro)
;; prior to the optimization for using fops to dump.
;; This assertion is simply whether it comes out correctly, not the time taken.
(defparameter *airport-vector* #.(compute-airports 4000))

;; a tangled forest of structures,
(defparameter *metadata* '#.(compute-tangled-stuff))

(with-test (:name :make-load-form-huge-vector)
  (assert (equalp (compute-airports (length (the vector *airport-vector*)))
                  *airport-vector*)))

(with-test (:name :make-load-form-circular-hair)
  (let ((testcase (compute-tangled-stuff)))
    (declare (optimize (speed 1)))
    ;; MAKE-LOAD-FORM discards the value of the CDF slot of one structure.
    ;; This probably isn't something "reasonable" to do, but it indicates
    ;; that SB-FASL::FOP-STRUCT was correctly not used.
    (setf (s1-cdf (second testcase)) #c(0d0 0d0))
    (assert (string= (write-to-string testcase :circle t :pretty nil)
                     (write-to-string *metadata* :circle t :pretty nil)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass twp ()
    ((node-name :initarg :name :accessor node-name)
     (parent :accessor node-parent :initform nil)
     (children :initarg :children :accessor node-children)))

  (defmethod print-object ((self twp) stream)
    (declare (optimize (speed 0))) ; silence noise
    (format stream "#<Node ~A~@[->~A~]>"
            (node-name self)
            (handler-case (mapcar 'node-name (node-children self))
              (unbound-slot () nil))))

  (defmethod make-load-form ((x twp) &optional environment)
    (declare (ignore environment))
    (values
     ;; creation form
     `(make-instance ',(class-of x)
                     ,@(if (slot-boundp x 'children)
                           `(:children ',(slot-value x 'children))))
     ;; initialization form
     `(setf (node-parent ',x) ',(slot-value x 'parent))))

  (defun make-tree-from-spec (node-class specs)
    (let ((tree (make-hash-table)))
      (dolist (node-name (remove-duplicates (apply #'append specs)))
        (setf (gethash node-name tree)
              (make-instance node-class :name node-name)))
      (dolist (node-spec specs)
        (let ((par (gethash (car node-spec) tree))
              (kids (mapcar (lambda (x) (gethash x tree)) (cdr node-spec))))
          (dolist (kid kids)
            (assert (not (node-parent kid)))
            (setf (slot-value kid 'parent) par))
          (setf (slot-value par 'children) kids)))
      (values (gethash 'root tree)))))

(defun verify-tree (node)
  (dolist (kid (if (slot-boundp node 'children) (node-children node) nil))
    (unless (eq (node-parent kid) node)
      (error "Node ~S shoud have ~S as parent but has ~S~%"
             (node-name kid)
             (node-name node)
             (node-parent kid)))
    (verify-tree kid)))

(defvar *x*
  #.(make-tree-from-spec
      'twp
      '((root a b c f)
        (a x y)
        (b p q r s)
        (c d e g))))

(with-test (:name :tree-with-parent-hand-made-load-form)
  (verify-tree *x*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass twp2 (twp) ())
  (defmethod make-load-form ((x twp2) &optional environment)
    (declare (ignore environment))
    (make-load-form-saving-slots x)))

;; Track the make-load-form FOPs as they fly by at load-time.
(defvar *call-tracker* nil)
(defvar *fop-funs*
  (car (ctu:find-code-constants #'sb-fasl::load-fasl-group :type '(simple-vector 128))))
(dolist (fop-name '(sb-fasl::fop-instance))
  (let* ((index (position fop-name *fop-funs*
                          :key
                          (lambda (x) (and (functionp x) (sb-kernel:%fun-name x)))))
         (fun (aref *fop-funs* index)))
    (setf (aref *fop-funs* index)
          (lambda (&rest args)
            (push fop-name *call-tracker*)
            (apply fun args)))))

;; Same as *X* but the MAKE-LOAD-FORM method is different
(defvar *y*
  #.(make-tree-from-spec
      'twp2
      '((root a b c f)
        (a x y)
        (b p q r s)
        (c d e g))))

(assert (= 14 (count 'sb-fasl::fop-instance *call-tracker*)))

(with-test (:name :tree-with-parent-m-l-f-s-s)
  (verify-tree *y*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass class-with-shared-slot ()
    ((a-slot :allocation :class :initarg :a :accessor thing-a)))
  (defmethod make-load-form ((self class-with-shared-slot) &optional environment)
    (declare (ignore environment))
    (make-load-form-saving-slots self :slot-names '(a-slot))))

(defvar *fool1* (make-instance 'class-with-shared-slot :a 42))
(defvar *fool2* #.(let ((i (make-instance 'class-with-shared-slot)))
                    (slot-makunbound i 'a-slot)
                    i))

;; The CLHS writeup is slightly ambiguous about what to with unbound
;; standard-object slots. Assuming that "initialized" and "uninitialized"
;; correspond to slots for which SLOT-BOUNDP would return T
;; and NIL respectively, the meaning of
;;  "initialized slots in object are initialized ..."
;; can only mean that you write values into slots of the reconstructed
;; object that were bound in the compile-time object.
;;
;; However "Uninitialized slots in object are not initialized" has two
;; opposing meanings depending on whether the verb is "are" which
;; expresses state versus "are [not] initialized" which expresses inaction.
;; For a similar grammatical construction, DEFINE-METHOD-COMBINATION
;; says in the "Short Form" description that:
;;   "that method serves as the effective method and operator is not called."
;; In that sentence "is [not] called" means that "calling" does NOT happen.
;; Analogously, "is [not] initialized" would imply that initializing
;; does NOT happen; it does NOT imply that "uninitializing" DOES happen.
;;
;; It seems though, that "are [not] initialized" actually means
;; SHALL be made to become uninitialized. This is based on the Notes
;; below the main description referencing SLOT-MAKUNBOUND.
;; (Though muddied by use of weasel-words "could" and "might")
;;
;; Ultimately the two end states (doing something / not doing something)
;; agree when the slot is local to the object, and no behavior is imparted
;; by ALLOCATE-INSTANCE to cause slots to be other than unbound.
;; This tests the edge case: that we DO call slot-makunbound.
(with-test (:name :mlfss-slot-makunbound)
  (assert (not (slot-boundp *fool1* 'a-slot))))

(defun try-literal-layout () #.(sb-kernel:find-layout 'class-with-shared-slot))
(with-test (:name :dump-std-obj-literal-layout)
  (assert (eq (try-literal-layout)
              (sb-kernel:find-layout 'class-with-shared-slot))))

(defparameter *some-hash-table*
  #.(let ((ht (make-hash-table)))
      (setf (gethash 'first ht) :a)
      (setf (gethash 'second ht) :b)
      ht))
;;; In the interest of producing repeatable results from externalized
;;; hash-tables, the make-load-form method iterates in such a way that
;;; the k/v vector should be ordered the same as it originally was.
;;; This also has implications on the order of items in each bucket
;;; when there are collisions.
(with-test (:name :reconstructed-hash-table)
  (let ((pairs (sb-impl::hash-table-pairs *some-hash-table*)))
    (assert (eq (aref pairs 2) 'first))
    (assert (eq (aref pairs 4) 'second))))

(defun int= (a b) (= (the integer a) (the integer b)))
(define-hash-table-test int= sb-impl::number-sxhash)
(defun get-sync-hash-table () #.(make-hash-table :synchronized t))
(with-test (:name :dump-hash-tables)
  ;; Don't lose the :synchronized option.
  (assert (hash-table-synchronized-p (get-sync-hash-table)))
  ;; Disallow nonstandard hash that disagrees with test.
  (assert-error (make-load-form (make-hash-table :test 'int= :hash-function 'sxhash)))
  ;; Allow nonstandard test as long as it was registered
  (assert (make-load-form (make-hash-table :test 'int=))))

(defun list-coalescing-test-fun-1 ()
  (declare (optimize (debug 1)))
  ;; base coalesces with base, non-base coalesces with non-base
  (values '#.`(foo ,(coerce "a" 'base-string))
          '#.`(foo ,(coerce "a" '(array character)))
          '#.`(foo ,(coerce "a" 'base-string))
          '#.`(foo ,(coerce "a" '(array character)))))

(defun list-coalescing-test-fun-2 ()
  (declare (optimize (debug 1)))
  (values '#.`(foo ,(coerce "a" '(array character)))
          '#.`(foo ,(coerce "a" 'base-string))))

(with-test (:name :more-code-constant-coalescing
                  :skipped-on (:not :sb-unicode))
  (let ((l1 (ctu:find-code-constants #'list-coalescing-test-fun-1))
        (l2 (ctu:find-code-constants #'list-coalescing-test-fun-2)))
    (assert (equal (length l1) 2))
    (assert (equal (length l2) 2))
    (multiple-value-bind (c1 c2 c3 c4) (list-coalescing-test-fun-1)
      (assert (typep (second c1) 'simple-base-string))
      (assert (typep (second c2) 'sb-kernel:simple-character-string))
      (assert (typep (second c3) 'simple-base-string))
      (assert (typep (second c4) 'sb-kernel:simple-character-string)))
    (assert (or (equal l1 l2)
                (equal l1 (reverse l2))))))

(defun cons-on-list-p (cons list)
  (assert (consp cons))
  (loop for cdr on list thereis (eq cons cdr)))

(defun constant-folding-cdr-test-fun ()
  (let ((list '(a b c)))
    (dolist (x (list list (cdr list) (cddr list)))
      (assert (cons-on-list-p x list)))))

(with-test (:name (:cdr-eq-detection :lp1583753))
  (constant-folding-cdr-test-fun))

(locally
    (declare (optimize (debug 2) (safety 3)))
  (defconstant +consy-constant+ (if (boundp '+consy-constant+) (symbol-value '+consy-constant+) '(message-id CAOrNaszM=eSufoqA4KFVNq4CUpjSJym-ktQnufQgj7a5g2sHmg@mail.gmail.com)))
  (defun test-constant-identity (x)
    (list '(message-id CAOrNaszM=eSufoqA4KFVNq4CUpjSJym-ktQnufQgj7a5g2sHmg@mail.gmail.com)
          (eq x +consy-constant+))))

(with-test (:name (defconstant :reference :identity))
  (let ((z (eval `(test-constant-identity ,(intern "+CONSY-CONSTANT+")))))
    (assert (equal (car z) +consy-constant+))
    (assert (cadr z))))

(macrolet ((expand ()
             (declare (muffle-conditions compiler-note)) ; why is DECLAIM not enough?
             `(progn
                ,@(loop for i from 0 below sb-vm:n-word-bits
                        collect
                        `(sb-int:defconstant-eqx ,(intern (format nil "MYSAP~d" i))
                             ,(sb-sys:int-sap (ash 1 i))
                           #'sb-sys:sap=)))))
  (expand))
(with-test (:name :dump-sap)
  (dotimes (i sb-vm:n-word-bits)
    (let ((s (intern (format nil "MYSAP~d" i))))
      (assert (= (sb-sys:sap-int (symbol-value s))
                 (ash 1 i))))))

(eval-when (:compile-toplevel :load-toplevel)
  (defstruct monkey
    (x t)
    (y 1 :type fixnum)
    (data (cons 1 2))
    (str "hi"))

  (defmethod make-load-form ((self monkey) &optional e)
    (make-load-form-saving-slots self :slot-names '(data) :environment e)))

(defvar *amonkey* #.(make-monkey :x nil :y 3 :data '(ok)))
(eval-when (:compile-toplevel) (makunbound '*amonkey*))
(with-test (:name :dump-monkey)
  (let ((a *amonkey*))
    (assert (sb-kernel::unbound-marker-p (monkey-x a)))
    (assert (sb-kernel::unbound-marker-p (monkey-y a)))
    (assert (sb-kernel::unbound-marker-p (monkey-str a)))
    (assert (equal (monkey-data a) '(ok)))))

(defun use-numeric-vector-a ()
  #.(make-array 5 :element-type '(signed-byte 8) :initial-contents '(90 100 5 -2 3)))

(defun use-numeric-vector-b ()
  #.(make-array 5 :element-type '(signed-byte 8) :initial-contents '(90 100 5 -2 3)))

(with-test (:name :coalesce-numeric-arrays)
  (assert (eq (use-numeric-vector-a) (use-numeric-vector-b))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defstruct person id)
(defstruct (spy (:include person (id "   " :type (simple-string 2)))) gadgetry)
(defmethod make-load-form ((self spy) &optional environment)
  (declare (ignore environment))
  (make-load-form-saving-slots self)))

(defvar *agent* #.(let ((s (make-spy :id "86" :gadgetry '(shoe-phone))))
                    (setf (person-id s) "max")
                    s))

;;; If lp#1969318 is fixed, then *AGENT* should not be reconstructed
;;; from its load form, let alone dumped in the first place.
;;; But thankfully we trap the read of the ID slot.
(with-test (:name :illegal-typed-slot-value)
  (assert-error (spy-id *agent*)))

(defun eq-cdr-constants-coalescing ()
  (values
   '(a (x . #1=(c)) #1# a)
   '(b (x . #2=(c)) #2# b)))

(with-test (:name :eq-cdr-constants-coalescing)
  (multiple-value-bind (a b)
      (funcall 'eq-cdr-constants-coalescing)
    (assert (eq (cdadr a) (caddr a)))
    (assert (eq (cdadr b) (caddr b)))))

(with-test (:name :circularity-within-non-simple-array)
  (let ((a #.(make-array 2 :adjustable t
                           :initial-element '#1=(#1#))))
    (assert (eq (aref (opaque-identity a) 0)
                (car (aref (opaque-identity a) 0))))
    (assert (eq (aref (opaque-identity a) 0)
                (aref (opaque-identity a) 1)))))

(with-test (:name :non-simple-array-constant-folding)
  (let ((x #.(make-array 10 :fill-pointer 5)))
    (assert (= (array-total-size x)
               (array-total-size (opaque-identity x))))))
