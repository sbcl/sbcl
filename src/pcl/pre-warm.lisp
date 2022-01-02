;;;; In support of PCL we compile some things into the cold image.
;;;; Not only does this simplify the PCL bootstrap ever so slightly,
;;;; it is nice to be able to test for types SB-PCL::%METHOD-FUNCTION
;;;; and CLASS (neither of which will have any instances too early).

;;;; This software is part of the SBCL system. See the README file for more
;;;; information.

;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

;;;; copyright information from original PCL sources:
;;;;
;;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;;; All rights reserved.
;;;;
;;;; Use and copying of this software and preparation of derivative works based
;;;; upon this software are permitted. Any distribution of this software or
;;;; derivative works must comply with all applicable United States export
;;;; control laws.
;;;;
;;;; This software is made available AS IS, and Xerox Corporation makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.

(in-package "SB-PCL")

(declaim (type (member nil early braid complete) **boot-state**))
(define-load-time-global **boot-state** nil)

;;; method function stuff.
;;;
;;; PCL historically included a so-called method-fast-function, which
;;; is essentially a method function but with (a) a precomputed
;;; continuation for CALL-NEXT-METHOD and (b) a permutation vector for
;;; slot access.  [ FIXME: see if we can understand these two
;;; optimizations before commit. ]  However, the presence of the
;;; fast-function meant that we violated AMOP and the effect of the
;;; :FUNCTION initarg, and furthermore got to potentially confusing
;;; situations where the function and the fast-function got out of
;;; sync, so that calling (method-function method) with the defined
;;; protocol would do different things from (call-method method) in
;;; method combination.
;;;
;;; So we define this internal method function structure, which we use
;;; when we create a method function ourselves.  This means that we
;;; can hang the various bits of information that we want off the
;;; method function itself, and also that if a user overrides method
;;; function creation there is no danger of having the system get
;;; confused.

;;; FIXME: these all go in dynamic space and then allocate a trampoline when
;;; assigned into an FDEFN. Figure out how to put them in immobile space,
;;; or can we allocate them _anywhere_ with embedded code now? I think so!
;;; And why do we assign these info FDEFNs? What calls them via their names?
#-sb-xc-host ; host doesn't need
(progn
(!defstruct-with-alternate-metaclass %method-function
  :slot-names (fast-function)
  :constructor %make-method-function
  :superclass-name function
  :metaclass-name static-classoid
  :metaclass-constructor make-static-classoid
  :dd-type funcallable-structure)
;;; Note: for x8-64 with #+immobile-code there are 2 additional raw slots which
;;; hold machine instructions to load the funcallable-instance-fun and jump to
;;; it, so that funcallable-instances can act like simple-funs, in as much as
;;; there's an address you can jump to without loading a register.
(sb-kernel:!defstruct-with-alternate-metaclass standard-funcallable-instance
  :slot-names (clos-slots hash-code)
  :constructor %make-standard-funcallable-instance
  :superclass-name function
  :metaclass-name static-classoid
  :metaclass-constructor make-static-classoid
  :dd-type funcallable-structure)
)

;;; Set up fake standard-classes.
;;; This is enough to fool the compiler into optimizing TYPEP into
;;; %INSTANCE-TYPEP.
;;; I'll bet that at least half of these we don't need at all.
(defparameter *!early-class-predicates*
  '((specializer specializerp)
    (standard-specializer standard-specializer-p)
    (exact-class-specializer exact-class-specializer-p)
    (class-eq-specializer class-eq-specializer-p)
    (eql-specializer eql-specializer-p)
    (class classp)
    (slot-class slot-class-p)
    (std-class std-class-p)
    (standard-class standard-class-p)
    (funcallable-standard-class funcallable-standard-class-p)
    (condition-class condition-class-p)
    (structure-class structure-class-p)
    (forward-referenced-class forward-referenced-class-p)
    (method method-p) ; shouldn't this be spelled METHODP? (like CLASSP)
    (standard-method standard-method-p)
    (accessor-method accessor-method-p)
    (standard-accessor-method standard-accessor-method-p)
    (standard-reader-method standard-reader-method-p)
    (standard-writer-method standard-writer-method-p)
    (standard-boundp-method standard-boundp-method-p)
    (global-reader-method global-reader-method-p)
    (global-writer-method global-writer-method-p)
    (global-boundp-method global-boundp-method-p)
    (generic-function generic-function-p)
    (standard-generic-function standard-generic-function-p)
    (method-combination method-combination-p)
    (long-method-combination long-method-combination-p)
    (short-method-combination short-method-combination-p)))

#+sb-xc-host
(progn
;;; Create #<SB-KERNEL::CONDITION-CLASSOID CONDITION>
;;; so that we can successfully parse the type specifier
;;; CONDITION-DESIGNATOR-HEAD which expands to
;;; (or format-control symbol condition sb-pcl::condition-class).
;;; Compiling any ERROR or WARN call eagerly looks up and re-parses
;;; the global ftype, and we don't want to see unknown types.
(let* ((name 'condition)
       (classoid (sb-kernel::make-condition-classoid :name name))
       (cell (sb-kernel::make-classoid-cell name classoid))
       (layout (make-layout (hash-layout-name name)
                             classoid
                             :depthoid 1
                             :inherits (vector (find-layout 't))
                             :length (+ sb-vm:instance-data-start 1)
                             :flags +condition-layout-flag+
                             :invalid nil)))
  (setf (classoid-wrapper classoid) layout
        (info :type :classoid-cell name) cell
        (info :type :kind name) :instance))

;;; Create classoids that correspond with some CLOS classes
(flet ((create-fake-classoid (name fun-p)
         (let* ((classoid (make-standard-classoid :name name))
                (cell (sb-kernel::make-classoid-cell name classoid))
                (layout
                  (make-layout (hash-layout-name name)
                               classoid
                               :depthoid -1
                               :inherits (map 'vector #'find-layout
                                              (cons t (if fun-p '(function))))
                               :length 0 ; don't care
                               :invalid nil)))
           (setf (classoid-wrapper classoid) layout
                 (info :type :classoid-cell name) cell
                 (info :type :kind name) :instance))))
  ;; Because we don't wire into %INSTANCE-TYPEP any assumptions about
  ;; the superclass/subclass relationships, these can all trivially be faked.
  (dolist (x *!early-class-predicates*)
    (let ((name (car x)))
      ;; GENERIC-FUNCTION and STANDARD-GENERIC-FUNCTION must contain
      ;; FUNCTION in their layouts so that their type predicates
      ;; optimize into FUNCALLABLE-INSTANCE-P (followed by a layout check),
      ;; rather than testing both that and INSTANCEP.
      (create-fake-classoid name
                            (memq name '(standard-generic-function
                                         generic-function))))))
) ; end PROGN

;;; BIG FAT WARNING: These predicates can't in general be called prior to the
;;; definition of the class which they test. However in carefully controlled
;;; circumstances they can be called when their class under test is not defined.
;;; The exact requirement is that the lowtag test must fail.
;;; So for example you can call GENERIC-FUNCTION-P on a HASH-TABLE,
;;; and CLASSP on a STRING, but you can't call CLASSP on anything that is either
;;; a FUNCALLABLE-INSTANCE or INSTANCE.
;;; With that caveat in mind, these are nifty things to have ASAP.
#-sb-xc-host
(macrolet ((define-class-predicates ()
             `(progn
                ,@(mapcar (lambda (x)
                           (destructuring-bind (class-name predicate) x
                             `(defun ,predicate (x) (typep x ',class-name))))
                         *!early-class-predicates*))))
  (define-class-predicates))

(defun safe-code-p (&optional env)
  (sb-c::policy (or env (sb-c::make-null-lexenv)) (eql safety 3)))
