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

(cl:in-package :cl-user)

(load "assertoid.lisp")

;;;; examples from, or close to, the Common Lisp DEFSTRUCT spec

;;; Type mismatch of slot default init value isn't an error until the
;;; default init value is actually used. (The justification is
;;; somewhat bogus, but the requirement is clear.)
(defstruct person age (name 007 :type string)) ; not an error until 007 used
(make-person :name "James") ; not an error, 007 not used
(assert (raises-error? (make-person) type-error))
;;; FIXME: broken structure slot type checking in sbcl-0.pre7.62
#+nil (assert (raises-error? (setf (person-name (make-person "Q")) 1) type-error))

;;; basic inheritance
(defstruct (astronaut (:include person)
		      (:conc-name astro-))
  helmet-size
  (favorite-beverage 'tang))
(let ((x (make-astronaut :name "Buzz" :helmet-size 17.5)))
  (assert (equal (person-name x) "Buzz"))
  (assert (equal (astro-name x) "Buzz"))
  (assert (eql (astro-favorite-beverage x) 'tang))
  (assert (null (astro-age x))))
(defstruct (ancient-astronaut (:include person (age 77)))
  helmet-size
  (favorite-beverage 'tang))
(assert (eql (ancient-astronaut-age (make-ancient-astronaut :name "John")) 77))

;;; interaction of :TYPE and :INCLUDE and :INITIAL-OFFSET
(defstruct (binop (:type list) :named (:initial-offset 2))
  (operator '? :type symbol)   
  operand-1
  operand-2)
(defstruct (annotated-binop (:type list)
			    (:initial-offset 3)
			    (:include binop))
  commutative associative identity)
(assert (equal (make-annotated-binop :operator '*
				     :operand-1 'x
				     :operand-2 5
				     :commutative t
				     :associative t
				     :identity 1)
	       '(nil nil binop * x 5 nil nil nil t t 1)))

;;; effect of :NAMED on :TYPE
(defstruct (named-binop (:type list) :named)
  (operator '? :type symbol)
  operand-1
  operand-2)
(let ((named-binop (make-named-binop :operator '+ :operand-1 'x :operand-2 5)))
  ;; The data representation is specified to look like this.
  (assert (equal named-binop '(named-binop + x 5)))
  ;; A meaningful NAMED-BINOP-P is defined.
  (assert (named-binop-p named-binop))
  (assert (named-binop-p (copy-list named-binop)))
  (assert (not (named-binop-p (cons 11 named-binop))))
  (assert (not (named-binop-p (find-package :cl)))))

;;; example 1
(defstruct town
  area
  watertowers
  (firetrucks 1 :type fixnum)
  population 
  (elevation 5128 :read-only t))
(let ((town1 (make-town :area 0 :watertowers 0)))
  (assert (town-p town1))
  (assert (not (town-p 1)))
  (assert (eql (town-area town1) 0))
  (assert (eql (town-elevation town1) 5128))
  (assert (null (town-population town1)))
  (setf (town-population town1) 99)
  (assert (eql (town-population town1) 99))
  (let ((town2 (copy-town town1)))
    (dolist (slot-accessor-name '(town-area
				  town-watertowers
				  town-firetrucks
				  town-population
				  town-elevation))
      (assert (eql (funcall slot-accessor-name town1)
		   (funcall slot-accessor-name town2))))
    (assert (not (fboundp '(setf town-elevation)))))) ; 'cause it's :READ-ONLY

;;; example 2
(defstruct (clown (:conc-name bozo-))
  (nose-color 'red)         
  frizzy-hair-p
  polkadots)
(let ((funny-clown (make-clown)))
  (assert (eql (bozo-nose-color funny-clown) 'red)))
(defstruct (klown (:constructor make-up-klown)
		  (:copier clone-klown)
		  (:predicate is-a-bozo-p))
  nose-color
  frizzy-hair-p
  polkadots)
(assert (is-a-bozo-p (make-up-klown)))

;;;; systematically testing variants of DEFSTRUCT:
;;;;   * native, :TYPE LIST, and :TYPE VECTOR

;;; FIXME: things to test:
;;;   * Slot readers work.
;;;   * Slot writers work.
;;;   * Predicates work.

;;; FIXME: things that would be nice to test systematically someday:
;;;   * constructors (default, boa..)
;;;   * copiers
;;;   * no type checks when (> SPEED SAFETY)
;;;   * Tests of inclusion would be good. (It's tested very lightly
;;;     above, and then tested a fair amount by the system compiling
;;;     itself.)

(defun string+ (&rest rest)
  (apply #'concatenate 'string
	 (mapcar #'string rest)))
(defun symbol+ (&rest rest)
  (values (intern (apply #'string+ rest))))

(defun accessor-name (conc-name slot-name)
  (symbol+ conc-name slot-name))

;;; Use the ordinary FDEFINITIONs of accessors (not inline expansions)
;;; to read and write a structure slot.
(defun read-slot-notinline (conc-name slot-name instance)
  (funcall (accessor-name conc-name slot-name) instance))
(defun write-slot-notinline (new-value conc-name slot-name instance)
  (funcall (fdefinition `(setf ,(accessor-name conc-name slot-name)))
	   new-value instance))

;;; Use inline expansions of slot accessors, if possible, to read and
;;; write a structure slot.
(defun read-slot-inline (conc-name slot-name instance)
  (funcall (compile nil
		    `(lambda (instance)
		       (,(accessor-name conc-name slot-name) instance)))
	   instance))
(defun write-slot-inline (new-value conc-name slot-name instance)
  (funcall (compile nil
		    `(lambda (new-value instance)
		       (setf (,(accessor-name conc-name slot-name) instance)
			     new-value)))
	   new-value
	   instance))

;;; Read a structure slot, checking that the inline and out-of-line
;;; accessors give the same result.
(defun read-slot (conc-name slot-name instance)
  (let ((inline-value (read-slot-inline conc-name slot-name instance))
	(notinline-value (read-slot-notinline conc-name slot-name instance)))
    (assert (eql inline-value notinline-value))
    inline-value))

;;; Write a structure slot, using INLINEP argument to decide
;;; on inlineness of accessor used.
(defun write-slot (new-value conc-name slot-name instance inlinep)
  (if inlinep
      (write-slot-inline new-value conc-name slot-name instance)
      (write-slot-notinline new-value conc-name slot-name instance)))

;;; bound during the tests so that we can get to it even if the
;;; debugger is having a bad day
(defvar *instance*)
  
(defmacro test-variant (defstructname &key colontype boa-constructor-p)
  `(progn

     (format t "~&/beginning PROGN for COLONTYPE=~S~%" ',colontype)

     (defstruct (,defstructname
		  ,@(when colontype `((:type ,colontype)))
                  ,@(when boa-constructor-p
                          `((:constructor ,(symbol+ "CREATE-" defstructname)
                             (id
			      &optional
			      (optional-test 2 optional-test-p)
                              &key
                              (home nil home-p)
                              (no-home-comment "Home package CL not provided.")
                              (comment (if home-p "" no-home-comment))
                              (refcount (if optional-test-p optional-test nil))
                              hash
                              weight)))))
       
       ;; some ordinary tagged slots
       id
       (home nil :type package :read-only t)
       (comment "" :type simple-string)
       ;; some raw slots
       (weight 1.0 :type single-float)
       (hash 1 :type (integer 1 #.(* 3 most-positive-fixnum)) :read-only t)
       ;; more ordinary tagged slots
       (refcount 0 :type (and unsigned-byte fixnum)))

     (format t "~&/done with DEFSTRUCT~%")

     (let* ((cn (string+ ',defstructname "-")) ; conc-name
	    (ctor (symbol-function ',(symbol+ (if boa-constructor-p
                                               "CREATE-"
                                               "MAKE-")
                                             defstructname)))
	    (*instance* (funcall ctor
				 ,@(unless boa-constructor-p
                                           `(:id)) "some id"
				 ,@(when boa-constructor-p
					 '(1))
				 :home (find-package :cl)
				 :hash (+ 14 most-positive-fixnum)
				 ,@(unless boa-constructor-p
					   `(:refcount 1)))))

       ;; Check that ctor set up slot values correctly. 
       (format t "~&/checking constructed structure~%")
       (assert (string= "some id" (read-slot cn "ID" *instance*)))
       (assert (eql (find-package :cl) (read-slot cn "HOME" *instance*)))
       (assert (string= "" (read-slot cn "COMMENT" *instance*)))
       (assert (= 1.0 (read-slot cn "WEIGHT" *instance*)))
       (assert (eql (+ 14 most-positive-fixnum)
		    (read-slot cn "HASH" *instance*)))
       (assert (= 1 (read-slot cn "REFCOUNT" *instance*)))

       ;; There should be no writers for read-only slots.
       (format t "~&/checking no read-only writers~%")
       (assert (not (fboundp `(setf ,(symbol+ cn "HOME")))))
       (assert (not (fboundp `(setf ,(symbol+ cn "HASH")))))
       ;; (Read-only slot values are checked in the loop below.)

       (dolist (inlinep '(t nil))
	 (format t "~&/doing INLINEP=~S~%" inlinep)
	 ;; Fiddle with writable slot values.
	 (let ((new-id (format nil "~S" (random 100)))
	       (new-comment (format nil "~X" (random 5555)))
	       (new-weight (random 10.0)))
	   (write-slot new-id cn "ID" *instance* inlinep)
	   (write-slot new-comment cn "COMMENT" *instance* inlinep)
	   (write-slot new-weight cn "WEIGHT" *instance* inlinep)
	   (assert (eql new-id (read-slot cn "ID" *instance*)))
	   (assert (eql new-comment (read-slot cn "COMMENT" *instance*)))
	   ;;(unless (eql new-weight (read-slot cn "WEIGHT" *instance*))
	   ;;  (error "WEIGHT mismatch: ~S vs. ~S"
	   ;;         new-weight (read-slot cn "WEIGHT" *instance*)))
	   (assert (eql new-weight (read-slot cn "WEIGHT" *instance*)))))
       (format t "~&/done with INLINEP loop~%")

       ;; :TYPE FOO objects don't go in the Lisp type system, so we
       ;; can't test TYPEP stuff for them.
       ;;
       ;; FIXME: However, when they're named, they do define
       ;; predicate functions, and we could test those. 
       ,@(unless colontype 
	   `(;; Fiddle with predicate function.
	     (let ((pred-name (symbol+ ',defstructname "-P")))
	       (format t "~&/doing tests on PRED-NAME=~S~%" pred-name)
	       (assert (funcall pred-name *instance*))
	       (assert (not (funcall pred-name 14)))
	       (assert (not (funcall pred-name "test")))
	       (assert (not (funcall pred-name (make-hash-table))))
	       (let ((compiled-pred
		      (compile nil `(lambda (x) (,pred-name x)))))
		 (format t "~&/doing COMPILED-PRED tests~%")
		 (assert (funcall compiled-pred *instance*))
		 (assert (not (funcall compiled-pred 14)))
		 (assert (not (funcall compiled-pred #()))))
	       ;; Fiddle with TYPEP.
	       (format t "~&/doing TYPEP tests, COLONTYPE=~S~%" ',colontype)
	       (assert (typep *instance* ',defstructname))
	       (assert (not (typep 0 ',defstructname)))
	       (assert (funcall (symbol+ "TYPEP") *instance* ',defstructname))
	       (assert (not (funcall (symbol+ "TYPEP") nil ',defstructname)))
	       (let* ((typename ',defstructname)
		      (compiled-typep
		       (compile nil `(lambda (x) (typep x ',typename)))))
		 (assert (funcall compiled-typep *instance*))
		 (assert (not (funcall compiled-typep nil))))))))
     
     (format t "~&/done with PROGN for COLONTYPE=~S~%" ',colontype)))
      
(test-variant vanilla-struct)
(test-variant vector-struct :colontype vector)
(test-variant list-struct :colontype list)
(test-variant vanilla-struct :boa-constructor-p t)
(test-variant vector-struct :colontype vector :boa-constructor-p t)
(test-variant list-struct :colontype list :boa-constructor-p t)


;;;; testing raw slots harder
;;;;
;;;; The offsets of raw slots need to be rescaled during the punning
;;;; process which is used to access them. That seems like a good
;;;; place for errors to lurk, so we'll try hunting for them by
;;;; verifying that all the raw slot data gets written successfully
;;;; into the object, can be copied with the object, and can then be
;;;; read back out (with none of it ending up bogusly outside the
;;;; object, so that it couldn't be copied, or bogusly overwriting
;;;; some other raw slot).

(defstruct manyraw
  (a (expt 2 30) :type (unsigned-byte 32))
  (b 0.1 :type single-float)
  (c 0.2d0 :type double-float)
  (d #c(0.3 0.3) :type (complex single-float))
  unraw-slot-just-for-variety
  (e #c(0.4d0 0.4d0) :type (complex double-float))
  (aa (expt 2 30) :type (unsigned-byte 32))
  (bb 0.1 :type single-float)
  (cc 0.2d0 :type double-float)
  (dd #c(0.3 0.3) :type (complex single-float))
  (ee #c(0.4d0 0.4d0) :type (complex double-float)))

(defvar *manyraw* (make-manyraw))

(assert (eql (manyraw-a *manyraw*) (expt 2 30)))
(assert (eql (manyraw-b *manyraw*) 0.1))
(assert (eql (manyraw-c *manyraw*) 0.2d0))
(assert (eql (manyraw-d *manyraw*) #c(0.3 0.3)))
(assert (eql (manyraw-e *manyraw*) #c(0.4d0 0.4d0)))
(assert (eql (manyraw-aa *manyraw*) (expt 2 30)))
(assert (eql (manyraw-bb *manyraw*) 0.1))
(assert (eql (manyraw-cc *manyraw*) 0.2d0))
(assert (eql (manyraw-dd *manyraw*) #c(0.3 0.3)))
(assert (eql (manyraw-ee *manyraw*) #c(0.4d0 0.4d0)))

(setf (manyraw-aa *manyraw*) (expt 2 31)
      (manyraw-bb *manyraw*) 0.11
      (manyraw-cc *manyraw*) 0.22d0
      (manyraw-dd *manyraw*) #c(0.33 0.33)
      (manyraw-ee *manyraw*) #c(0.44d0 0.44d0))

(let ((copy (copy-manyraw *manyraw*)))
  (assert (eql (manyraw-a copy) (expt 2 30)))
  (assert (eql (manyraw-b copy) 0.1))
  (assert (eql (manyraw-c copy) 0.2d0))
  (assert (eql (manyraw-d copy) #c(0.3 0.3)))
  (assert (eql (manyraw-e copy) #c(0.4d0 0.4d0)))
  (assert (eql (manyraw-aa copy) (expt 2 31)))
  (assert (eql (manyraw-bb copy) 0.11))
  (assert (eql (manyraw-cc copy) 0.22d0))
  (assert (eql (manyraw-dd copy) #c(0.33 0.33)))
  (assert (eql (manyraw-ee copy) #c(0.44d0 0.44d0))))

;;; success
(format t "~&/returning success~%")
(quit :unix-status 104)
