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

;;; success
(quit :unix-status 104)
