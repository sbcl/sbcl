;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

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


;;; This is the early definition of LOAD-DEFCLASS. It just collects up
;;; all the class definitions in a list. Later, in braid1.lisp, these
;;; are actually defined.

;;; Each entry in *EARLY-CLASS-DEFINITIONS* is an EARLY-CLASS-DEFINITION.
(defparameter *!early-class-definitions* ())

(defun !early-class-definition (class-name)
  (or (find class-name *!early-class-definitions* :key #'ecd-class-name)
      (error "~S is not a class in *early-class-definitions*." class-name)))

(defun !make-early-class-definition
       (name source-location metaclass
        superclass-names canonical-slots other-initargs)
  (list 'early-class-definition
        name source-location metaclass
        superclass-names canonical-slots other-initargs))

(defun ecd-class-name        (ecd) (nth 1 ecd))
(defun ecd-source-location   (ecd) (nth 2 ecd))
(defun ecd-metaclass         (ecd) (nth 3 ecd))
(defun ecd-superclass-names  (ecd) (nth 4 ecd))
(defun ecd-canonical-slots   (ecd) (nth 5 ecd))
(defun ecd-other-initargs    (ecd) (nth 6 ecd))

(defvar *!early-class-slots* nil)

(defun canonical-slot-name (canonical-slot)
  (getf canonical-slot :name))

(defun !early-class-slots (class-name)
  (cdr (or (assoc class-name *!early-class-slots*)
           (let ((a (cons class-name
                          (mapcar #'canonical-slot-name
                                  (!early-collect-inheritance class-name)))))
             (push a *!early-class-slots*)
             a))))

(defun !early-class-size (class-name)
  (length (!early-class-slots class-name)))

(defun !early-collect-inheritance (class-name)
  ;;(declare (values slots cpl default-initargs direct-subclasses))
  (let ((cpl (!early-collect-cpl class-name)))
    (values (!early-collect-slots cpl)
            cpl
            (!early-collect-default-initargs cpl)
            (let (collect)
              (dolist (definition *!early-class-definitions*)
                (when (memq class-name (ecd-superclass-names definition))
                  (push (ecd-class-name definition) collect)))
              (nreverse collect)))))

(defun !early-collect-slots (cpl)
  (let* ((definitions (mapcar #'!early-class-definition cpl))
         (super-slots (mapcar #'ecd-canonical-slots definitions))
         (slots (apply #'append (reverse super-slots))))
    (dolist (s1 slots)
      (let ((name1 (canonical-slot-name s1)))
        (dolist (s2 (cdr (memq s1 slots)))
          (when (eq name1 (canonical-slot-name s2))
            (error "More than one early class defines a slot with the~%~
                    name ~S. This can't work because the bootstrap~%~
                    object system doesn't know how to compute effective~%~
                    slots."
                   name1)))))
    slots))

(defun !early-collect-cpl (class-name)
  (labels ((walk (c)
             (let* ((definition (!early-class-definition c))
                    (supers (ecd-superclass-names definition)))
               (cons c
                     (apply #'append (mapcar #'!early-collect-cpl supers))))))
    (remove-duplicates (walk class-name) :from-end nil :test #'eq)))

(defun !early-collect-default-initargs (cpl)
  (let ((default-initargs ()))
    (dolist (class-name cpl)
      (let* ((definition (!early-class-definition class-name))
             (others (ecd-other-initargs definition)))
        (loop (when (null others) (return nil))
              (let ((initarg (pop others)))
                (unless (eq initarg :direct-default-initargs)
                 (error "~@<The defclass option ~S is not supported by ~
                        the bootstrap object system.~:@>"
                        initarg)))
              (setq default-initargs
                    (nconc default-initargs (reverse (pop others)))))))
    (reverse default-initargs)))

(defun !bootstrap-slot-index (class-name slot-name)
  (or (position slot-name (!early-class-slots class-name))
      (error "~S not found" slot-name)))

;;; !BOOTSTRAP-GET-SLOT and !BOOTSTRAP-SET-SLOT are used to access and
;;; change the values of slots during bootstrapping. During
;;; bootstrapping, there are only two kinds of objects whose slots we
;;; need to access, CLASSes and SLOT-DEFINITIONs. The first argument
;;; to these functions tells whether the object is a CLASS or a
;;; SLOT-DEFINITION.
;;;
;;; Note that the way this works it stores the slot in the same place
;;; in memory that the full object system will expect to find it
;;; later. This is critical to the bootstrapping process, the whole
;;; changeover to the full object system is predicated on this.
;;;
;;; One important point is that the layout of standard classes and
;;; standard slots must be computed the same way in this file as it is
;;; by the full object system later.
(defmacro !bootstrap-get-slot (type object slot-name)
  `(clos-slots-ref (get-slots ,object)
                   (!bootstrap-slot-index ,type ,slot-name)))
(defun !bootstrap-set-slot (type object slot-name new-value)
  (setf (!bootstrap-get-slot type object slot-name) new-value))

(defun early-class-precedence-list (class)
  (!bootstrap-get-slot 'pcl-class class '%class-precedence-list))

(defun early-class-slotds (class)
  (!bootstrap-get-slot 'slot-class class 'slots))

(defun early-slot-definition-name (slotd)
  (!bootstrap-get-slot 'standard-effective-slot-definition slotd 'name))

(defun early-slot-definition-location (slotd)
  (!bootstrap-get-slot 'standard-effective-slot-definition slotd 'location))

(defun early-slot-definition-info (slotd)
  (!bootstrap-get-slot 'standard-effective-slot-definition slotd 'info))

(defun early-class-direct-subclasses (class)
  (!bootstrap-get-slot 'class class 'direct-subclasses))

(declaim (notinline load-defclass))
(defun load-defclass (name metaclass supers canonical-slots canonical-options
                      readers writers slot-names source-location &optional safe-p)
  ;; SAFE-P is used by REAL-LOAD-DEFCLASS, but can be ignored here, since
  ;; during the bootstrap we won't have (SAFETY 3).
  (declare (ignore safe-p))
  (sb-kernel::%%compiler-defclass name readers writers slot-names)
  (let ((ecd (!make-early-class-definition name
                                          source-location
                                          metaclass
                                          (copy-tree supers)
                                          (copy-tree canonical-slots)
                                          (copy-tree canonical-options)))
        (existing
         (find name *!early-class-definitions* :key #'ecd-class-name)))
    (setq *!early-class-definitions*
          (cons ecd (remove existing *!early-class-definitions*)))
    ecd))
