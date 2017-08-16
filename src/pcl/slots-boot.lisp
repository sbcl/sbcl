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

(defvar *!temporary-ensure-accessor-functions* nil)
(defun ensure-accessor (fun-name)
  (when (member fun-name *!temporary-ensure-accessor-functions* :test 'equal)
    (error "ENSURE-ACCESSOR ~S called more than once!?" fun-name))
  (push fun-name *!temporary-ensure-accessor-functions*)
  #| We don't really need "fast" global slot accessors while building PCL.
  ;; With few exceptions, all methods use a permutation vector for slot access.
  ;; In a pinch, these would suffice, should it become utterly necessary:
  (destructuring-bind (slot-name method) (cddr fun-name)
    (setf (fdefinition fun-name)
          (ecase method
            (reader (lambda (object) (slot-value object slot-name)))
            (writer (lambda (newval object) (setf (slot-value object slot-name) newval)))
            (boundp (lambda (object) (slot-boundp object slot-name))))))|#
  (setf (fdefinition fun-name)
        (lambda (&rest args)
          (error "Nooooo! ~S accidentally invoked on ~S" fun-name args))))

(defun make-structure-slot-boundp-function (slotd)
  (declare (ignore slotd))
  (named-lambda always-bound (object)
    (declare (ignore object))
    t))

(define-condition instance-structure-protocol-error
    (reference-condition error)
  ((slotd :initarg :slotd :reader instance-structure-protocol-error-slotd)
   (fun :initarg :fun :reader instance-structure-protocol-error-fun))
  (:report
   (lambda (c s)
     (format s "~@<The slot ~S has neither ~S nor ~S ~
                allocation, so it can't be ~A by the default ~
                ~S method.~@:>"
             (instance-structure-protocol-error-slotd c)
             :instance :class
             (cond
               ((member (instance-structure-protocol-error-fun c)
                        '(slot-value-using-class slot-boundp-using-class))
                "read")
               (t "written"))
             (instance-structure-protocol-error-fun c)))))

(defun instance-structure-protocol-error (slotd fun)
  (error 'instance-structure-protocol-error
         :slotd slotd :fun fun
         :references `((:amop :generic-function ,fun)
                       (:amop :section (5 5 3)))))

(defun get-optimized-std-accessor-method-function (class slotd name)
  (cond
    ((structure-class-p class)
     (ecase name
       (reader (slot-definition-internal-reader-function slotd))
       (writer (slot-definition-internal-writer-function slotd))
       (boundp (make-structure-slot-boundp-function slotd))))
    ((condition-class-p class)
     (let ((info (the slot-info (slot-definition-info slotd))))
       (ecase name
         (reader (slot-info-reader info))
         (writer (slot-info-writer info))
         (boundp (slot-info-boundp info)))))
    (t
     (let* ((fsc-p (cond ((standard-class-p class) nil)
                         ((funcallable-standard-class-p class) t)
                         ((std-class-p class)
                          ;; Shouldn't be using the optimized-std-accessors
                          ;; in this case.
                          #+nil (format t "* warning: ~S ~S~%   ~S~%"
                                        name slotd class)
                          nil)
                         (t (error "~S is not a STANDARD-CLASS." class))))
            (slot-name (slot-definition-name slotd))
            (location (slot-definition-location slotd))
            (function (ecase name
                        (reader #'make-optimized-std-reader-method-function)
                        (writer #'make-optimized-std-writer-method-function)
                        (boundp #'make-optimized-std-boundp-method-function)))
            ;; KLUDGE: we need this slightly hacky calling convention
            ;; for these functions for bootstrapping reasons: see
            ;; !BOOTSTRAP-MAKE-SLOT-DEFINITION in braid.lisp.  -- CSR,
            ;; 2004-07-12
            (value (funcall function fsc-p slotd slot-name location)))
       (declare (type function function))
       (values value (slot-definition-location slotd))))))

(defun make-optimized-std-reader-method-function
    (fsc-p slotd slot-name location)
  (set-fun-name
   (etypecase location
     (fixnum
      (if fsc-p
          (lambda (instance)
            (check-obsolete-instance instance)
            (let ((value (clos-slots-ref (fsc-instance-slots instance)
                                         location)))
              (if (unbound-marker-p value)
                  (values
                   (slot-unbound (class-of instance) instance slot-name))
                  value)))
          (lambda (instance)
            (check-obsolete-instance instance)
            (let ((value (clos-slots-ref (std-instance-slots instance)
                                         location)))
              (if (unbound-marker-p value)
                  (values
                   (slot-unbound (class-of instance) instance slot-name))
                  value)))))
     (cons
      (lambda (instance)
        (check-obsolete-instance instance)
        (let ((value (cdr location)))
          (if (unbound-marker-p value)
              (values (slot-unbound (class-of instance) instance slot-name))
              value))))
     (null
      (lambda (instance)
        (declare (ignore instance))
        (instance-structure-protocol-error slotd 'slot-value-using-class))))
   `(reader ,slot-name)))

(defun make-optimized-std-writer-method-function (fsc-p slotd slot-name location)
  ;; The (WHEN SLOTD ...) gunk is for building early slot definitions.
  (let* ((class (when slotd (slot-definition-class slotd)))
         (safe-p (when slotd (safe-p class)))
         (orig-wrapper (when safe-p (class-wrapper class)))
         (info (when safe-p (slot-definition-info slotd)))
         (writer-fun (etypecase location
                       ;; In SAFE-P case the typechecking already validated the instance.
                       (fixnum
                        (if fsc-p
                            (if safe-p
                                (lambda (nv instance)
                                  (setf (clos-slots-ref (fsc-instance-slots instance)
                                                        location)
                                        nv))
                                (lambda (nv instance)
                                  (check-obsolete-instance instance)
                                  (setf (clos-slots-ref (fsc-instance-slots instance)
                                                        location)
                                        nv)))
                            (if safe-p
                                (lambda (nv instance)
                                  (setf (clos-slots-ref (std-instance-slots instance)
                                                        location)
                                        nv))
                                (lambda (nv instance)
                                  (check-obsolete-instance instance)
                                  (setf (clos-slots-ref (std-instance-slots instance)
                                                        location)
                                        nv)))))
                       (cons
                        (if safe-p
                            (lambda (nv instance)
                              (declare (ignore instance))
                              (setf (cdr location) nv))
                            (lambda (nv instance)
                              (check-obsolete-instance instance)
                              (setf (cdr location) nv))))
                       (null
                        (lambda (nv instance)
                          (declare (ignore nv instance))
                          (instance-structure-protocol-error
                           slotd
                           '(setf slot-value-using-class))))))
         (checking-fun (when safe-p
                         (lambda (new-value instance)
                           ;; If we have a TYPE-CHECK-FUNCTION, call it.
                           (let* (;; Note that the class of INSTANCE here is not
                                  ;; neccessarily the SLOT-DEFINITION-CLASS of
                                  ;; the SLOTD passed to M-O-S-W-M-F, since it's
                                  ;; e.g. possible for a subclass to define a
                                  ;; slot of the same name but with no
                                  ;; accessors. So we may need to fetch the
                                  ;; right SLOT-INFO from the wrapper instead of
                                  ;; just closing over it.
                                  (wrapper (valid-wrapper-of instance))
                                  (typecheck
                                   (slot-info-typecheck
                                    (if (eq wrapper orig-wrapper)
                                        info
                                        (cdr (find-slot-cell wrapper slot-name))))))
                             (when typecheck
                               (funcall typecheck new-value)))
                           ;; Then call the real writer.
                           (funcall writer-fun new-value instance)))))
    (set-fun-name (if safe-p
                      checking-fun
                      writer-fun)
                  `(writer ,slot-name))))

(defun make-optimized-std-boundp-method-function
    (fsc-p slotd slot-name location)
  (set-fun-name
   (etypecase location
     (fixnum (if fsc-p
                 (lambda (instance)
                   (check-obsolete-instance instance)
                   (not (unbound-marker-p (clos-slots-ref (fsc-instance-slots instance)
                                                          location))))
                 (lambda (instance)
                   (check-obsolete-instance instance)
                   (not (unbound-marker-p (clos-slots-ref (std-instance-slots instance)
                                                          location))))))
     (cons (lambda (instance)
             (check-obsolete-instance instance)
             (not (unbound-marker-p (cdr location)))))
     (null
      (lambda (instance)
        (declare (ignore instance))
        (instance-structure-protocol-error slotd 'slot-boundp-using-class))))
   `(boundp ,slot-name)))

(defun make-optimized-structure-slot-value-using-class-method-function
    (function)
  (declare (type function function))
  (lambda (class object slotd)
    (declare (ignore class slotd))
    (funcall function object)))

(defun make-optimized-structure-setf-slot-value-using-class-method-function
    (function)
  (declare (type function function))
  (lambda (nv class object slotd)
    (declare (ignore class slotd))
    (funcall function nv object)))

(defun make-optimized-structure-slot-boundp-using-class-method-function ()
  (lambda (class object slotd)
    (declare (ignore class object slotd))
    t))

(defun get-optimized-std-slot-value-using-class-method-function
    (class slotd name)
  (cond
    ((structure-class-p class)
     (ecase name
       (reader (make-optimized-structure-slot-value-using-class-method-function
                (slot-definition-internal-reader-function slotd)))
       (writer (make-optimized-structure-setf-slot-value-using-class-method-function
                (slot-definition-internal-writer-function slotd)))
       (boundp (make-optimized-structure-slot-boundp-using-class-method-function))))
    ((condition-class-p class)
     (let ((info (slot-definition-info slotd)))
       (ecase name
         (reader
          (let ((fun (slot-info-reader info)))
            (lambda (class object slotd)
              (declare (ignore class slotd))
              (funcall fun object))))
         (writer
          (let ((fun (slot-info-writer info)))
            (lambda (new-value class object slotd)
              (declare (ignore class slotd))
              (funcall fun new-value object))))
         (boundp
          (let ((fun (slot-info-boundp info)))
            (lambda (class object slotd)
              (declare (ignore class slotd))
              (funcall fun object)))))))
    (t
     (let* ((fsc-p (cond ((standard-class-p class) nil)
                         ((funcallable-standard-class-p class) t)
                         (t (error "~S is not a standard-class" class))))
            (function
             (ecase name
               (reader
                #'make-optimized-std-slot-value-using-class-method-function)
               (writer
                #'make-optimized-std-setf-slot-value-using-class-method-function)
               (boundp
                #'make-optimized-std-slot-boundp-using-class-method-function))))
       (declare (type function function))
       (values (funcall function fsc-p slotd)
               (slot-definition-location slotd))))))

(defun make-optimized-std-slot-value-using-class-method-function (fsc-p slotd)
  (let ((location (slot-definition-location slotd))
        (slot-name (slot-definition-name slotd)))
    (etypecase location
      (fixnum (if fsc-p
                  (lambda (class instance slotd)
                    (declare (ignore slotd))
                    (check-obsolete-instance instance)
                    (let ((value (clos-slots-ref (fsc-instance-slots instance)
                                                 location)))
                      (if (unbound-marker-p value)
                          (values (slot-unbound class instance slot-name))
                          value)))
                  (lambda (class instance slotd)
                    (declare (ignore slotd))
                    (check-obsolete-instance instance)
                    (let ((value (clos-slots-ref (std-instance-slots instance)
                                                 location)))
                      (if (unbound-marker-p value)
                          (values (slot-unbound class instance slot-name))
                          value)))))
      (cons (lambda (class instance slotd)
              (declare (ignore slotd))
              (check-obsolete-instance instance)
              (let ((value (cdr location)))
                (if (unbound-marker-p value)
                    (values (slot-unbound class instance slot-name))
                    value))))
      (null
       (lambda (class instance slotd)
         (declare (ignore class instance))
         (instance-structure-protocol-error slotd 'slot-value-using-class))))))

(defun make-optimized-std-setf-slot-value-using-class-method-function
    (fsc-p slotd)
  (let* ((location (slot-definition-location slotd))
         (class (slot-definition-class slotd))
         (typecheck
          (when (safe-p class)
            (slot-info-typecheck (slot-definition-info slotd)))))
    (macrolet ((make-mf-lambda (&body body)
                 `(lambda (nv class instance slotd)
                    (declare (ignore class slotd))
                    (check-obsolete-instance instance)
                    ,@body))
               (make-mf-lambdas (&body body)
                 ;; Having separate lambdas for the NULL / not-NULL cases of
                 ;; TYPE-CHECK-FUNCTION is done to avoid runtime overhead
                 ;; for CLOS typechecking when it's not in use.
                 `(if typecheck
                      (make-mf-lambda
                       (funcall (the function typecheck) nv)
                       ,@body)
                      (make-mf-lambda
                       ,@body))))
      (etypecase location
        (fixnum
         (if fsc-p
             (make-mf-lambdas
              (setf (clos-slots-ref (fsc-instance-slots instance) location)
                    nv))
             (make-mf-lambdas
              (setf (clos-slots-ref (std-instance-slots instance) location)
                    nv))))
        (cons
         (make-mf-lambdas (setf (cdr location) nv)))
        (null (lambda (nv class instance slotd)
                (declare (ignore nv class instance))
                (instance-structure-protocol-error
                 slotd '(setf slot-value-using-class))))))))

(defun make-optimized-std-slot-boundp-using-class-method-function
    (fsc-p slotd)
  (let ((location (slot-definition-location slotd)))
    (etypecase location
      (fixnum
       (if fsc-p
           (lambda (class instance slotd)
             (declare (ignore class slotd))
             (check-obsolete-instance instance)
             (not (unbound-marker-p
                   (clos-slots-ref (fsc-instance-slots instance) location))))
           (lambda (class instance slotd)
             (declare (ignore class slotd))
             (check-obsolete-instance instance)
             (not (unbound-marker-p
                   (clos-slots-ref (std-instance-slots instance) location))))))
      (cons (lambda (class instance slotd)
              (declare (ignore class slotd))
              (check-obsolete-instance instance)
              (not (unbound-marker-p (cdr location)))))
      (null
       (lambda (class instance slotd)
         (declare (ignore class instance))
         (instance-structure-protocol-error slotd
                                            'slot-boundp-using-class))))))

(defun get-accessor-from-svuc-method-function (class slotd sdfun name)
  (macrolet ((emf-funcall (emf &rest args)
               `(invoke-effective-method-function ,emf nil
                                                  :required-args ,args)))
    (set-fun-name
     (case name
       (reader (lambda (instance)
                 (emf-funcall sdfun class instance slotd)))
       (writer (lambda (nv instance)
                 (emf-funcall sdfun nv class instance slotd)))
       (boundp (lambda (instance)
                 (emf-funcall sdfun class instance slotd))))
     `(,name ,(class-name class) ,(slot-definition-name slotd)))))

(defun maybe-class (class-or-name)
  (when (eq **boot-state** 'complete)
    (if (typep class-or-name 'class)
        class-or-name
        (find-class class-or-name nil))))

(flet ((make-initargs (slot-name kind method-function)
         (let ((initargs (copy-tree method-function))
               (slot-names (list slot-name)))
           (setf (getf (getf initargs 'plist) :slot-name-lists)
                 (ecase kind
                   ((:reader :boundp) (list slot-names))
                   (:writer (list '() slot-names))))
           initargs)))

  (defun make-std-reader-method-function (class-or-name slot-name)
    (let ((class (maybe-class class-or-name)))
      (make-initargs
       slot-name :reader
       (ecase (slot-access-strategy class slot-name 'reader t)
         (:standard
          (make-method-function
           (lambda (instance)
             (pv-binding1 ((bug "Please report this")
                           (instance) (instance-slots))
               (instance-read-standard
                .pv. instance-slots 0
                (slot-value instance slot-name))))))
         ((:custom :accessor)
          (make-method-function
           (lambda (instance)
             (pv-binding1 ((bug "Please report this")
                           (instance) nil)
               (instance-read-custom .pv. 0 instance)))))))))

  (defun make-std-writer-method-function (class-or-name slot-name)
    (let ((class (maybe-class class-or-name)))
      (make-initargs
       slot-name :writer
       (ecase (slot-access-strategy class slot-name 'writer t)
         (:standard
          (macrolet ((writer-method-function (safe)
                       `(make-method-function
                         (lambda (nv instance)
                           (pv-binding1 ((bug "Please report this")
                                         (instance) (instance-slots))
                             (instance-write-standard
                              .pv. instance-slots 0 nv
                              (setf (slot-value instance slot-name)
                                    .good-new-value.)
                              ,@(when safe '(nil t))))))))
            (if (and class (safe-p class))
                (writer-method-function t)
                (writer-method-function nil))))
         ((:custom :accessor)
          (make-method-function
           (lambda (nv instance)
             (pv-binding1 ((bug "Please report this")
                           (instance) nil)
               (instance-write-custom .pv. 0 instance nv)))))))))

  (defun make-std-boundp-method-function (class-or-name slot-name)
    (let ((class (maybe-class class-or-name)))
      (make-initargs
       slot-name :boundp
       (ecase (slot-access-strategy class slot-name 'boundp t)
         (:standard
          (make-method-function
           (lambda (instance)
             (pv-binding1 ((bug "Please report this")
                           (instance) (instance-slots))
               (instance-boundp-standard
                .pv. instance-slots 0
                (slot-boundp instance slot-name))))))
         ((:custom :accessor)
          (make-method-function
           (lambda (instance)
             (pv-binding1 ((bug "Please report this")
                           (instance) nil)
               (instance-boundp-custom .pv. 0 instance))))))))))

;;;; FINDING SLOT DEFINITIONS
;;;
;;; Historical PCL found slot definitions by iterating over
;;; CLASS-SLOTS, which is O(N) for number of slots, and moreover
;;; requires a GF call (for SLOT-DEFINITION-NAME) for each slot in
;;; list up to the desired one.
;;;
;;; Current SBCL hashes the effective slot definitions, and some
;;; information pulled out from them into a simple-vector, with bucket
;;; chains made out of plists keyed by the slot names. This fixes
;;; gives O(1) performance, and avoid the GF calls.
;;;
;;; MAKE-SLOT-TABLE constructs the hashed vector out of a list of
;;; effective slot definitions and the class they pertain to, and
;;; FIND-SLOT-DEFINITION knows how to look up slots in that vector.
;;;
;;; The only bit of cleverness in the implementation is to make the
;;; vectors fairly tight, but always longer then 0 elements:
;;;
;;; -- We don't want to waste huge amounts of space no these vectors,
;;;    which are mostly required by things like SLOT-VALUE with a
;;;    variable slot name, so a constant extension over the minimum
;;;    size seems like a good choise.
;;;
;;; -- As long as the vector always has a length > 0
;;;    FIND-SLOT-DEFINITION doesn't need to handle the rare case of an
;;;    empty vector separately: it just returns a NIL.
;;;
;;; In addition to the slot-definition we also store the slot-location
;;; and type-check function for instances of standard metaclasses, so
;;; that SLOT-VALUE &co using variable slot names can get at them
;;; without additional GF calls.
;;;
;;; Notes:
;;;   It would also be nice to have STANDARD-INSTANCE-STRUCTURE-P
;;;   generic instead of checking versus STANDARD-CLASS and
;;;   FUNCALLABLE-STANDARD-CLASS.
;;;
;;;   Uh, the comments above talking about how FIND-SLOT-DEFINITION
;;;   does something with slot vectors has no basis in reality.
;;;   Probably the comments need fixing, rather than the code.

(defun find-slot-definition (class slot-name &optional errorp)
  (unless (class-finalized-p class)
    (or (try-finalize-inheritance class)
        (if errorp
            (error "Cannot look up slot-definition for ~S in ~S (too early to finalize.)"
                   slot-name class)
            (return-from find-slot-definition (values nil nil)))))
  (dolist (slotd (class-slots class)
           (if errorp
               (error "No slot called ~S in ~S." slot-name class)
               (values nil t)))
    (when (eq slot-name (slot-definition-name slotd))
      (return (values slotd t)))))

(defun find-slot-cell (wrapper slot-name)
  (declare (symbol slot-name))
  (declare (optimize (sb-c::insert-array-bounds-checks 0)))
  (let* ((vector (layout-slot-table wrapper))
         (modulus (truly-the index (svref vector 0)))
         ;; Can elide the 'else' branch of (OR symbol-hash ensure-symbol-hash)
         ;; because every symbol in the slot-table already got a nonzero hash.
         (index (rem (symbol-hash slot-name) modulus))
         (probe (svref vector (1+ index))))
    (declare (simple-vector vector) (index index))
    (cond ((fixnump probe)
           (do* ((count (svref vector (1- (truly-the index probe))))
                 (end (truly-the index (+ probe count)))
                 (j probe (1+ j)))
                ((>= j end))
             (declare (index count j))
             (when (eq (svref vector j) slot-name)
               (return (svref vector (truly-the index (+ j count)))))))
          ((eq (car (truly-the list probe)) slot-name)
           (cdr probe)))))

(defun make-slot-table (class slots &optional bootstrap)
  (unless slots
    ;; *** If changing this empty table value to something else,
    ;;     be sure to make a similar change to MAKE-COLD-LAYOUT in
    ;;     compiler/generic/genesis as well as in DEFSTRUCT LAYOUT.
    ;;     A DEFCONSTANT for this would only transfer the problem
    ;;     to cold-init in a different sort of way. :-(
    (return-from make-slot-table #(1 nil)))
  (let* ((n (+ (logior (length slots) 1) 2)) ; an odd divisor is preferred
         (vector (make-array n :initial-element nil)))
    (flet ((add-to-vector (name slot)
             (declare (symbol name)
                      (optimize (sb-c::insert-array-bounds-checks 0)))
             (let ((index (rem (ensure-symbol-hash name) n)))
               (setf (svref vector index)
                     (acons name
                            (cons (when (or bootstrap
                                            (and (standard-class-p class)
                                                 (slot-accessor-std-p slot 'all)))
                                    (if bootstrap
                                        (early-slot-definition-location slot)
                                        (slot-definition-location slot)))
                                  (the slot-info
                                    (if bootstrap
                                        (early-slot-definition-info slot)
                                        (slot-definition-info slot))))
                            (svref vector index))))))
      (if (eq 'complete **boot-state**)
          (dolist (slot slots)
            (add-to-vector (slot-definition-name slot) slot))
          (dolist (slot slots)
            (add-to-vector (early-slot-definition-name slot) slot))))
    ;; The VECTOR as computed above implements a hash table with chaining.
    ;; Rather than store chains using cons cells, chains can be stored in the
    ;; vector itself at the end, with the table entry pointing to another
    ;; index in the vector. The chain length is stored first, then all keys,
    ;; then all values. The resulting structure takes less memory than
    ;; linked lists, and can be scanned faster. As an exception, for lists
    ;; of length 1, the table cell holds a (key . value) pair directly.
    (let* ((final-n
            (+ 1 n
               ;; number of additional cells needed to represent linked lists
               ;; as length-prefixed subsequences in the final vector.
               (loop for cell across vector
                     for count = (length cell)
                     sum (if (<= count 1) 0 (1+ (* count 2))))))
           (final-vector (make-array final-n))
           (data-index (1+ n))) ; after the hashtable portion of the vector
      (setf (aref final-vector 0) n) ; the modulus
      (dotimes (i n final-vector)
        (let ((alist (aref vector i)))
          (if (not (cdr alist)) ; store it in the final vector as-is
              (setf (aref final-vector (1+ i)) (car alist))
              (let ((count (length alist)))
                ;; Probed cell holds the index of the first symbol.
                ;; The symbol count precedes the first symbol cell.
                (setf (aref final-vector (1+ i)) (1+ data-index)
                      (aref final-vector data-index) count)
                (dolist (cell alist)
                  (setf (aref final-vector (incf data-index)) (car cell)))
                (dolist (cell alist)
                  (setf (aref final-vector (incf data-index)) (cdr cell)))
                (incf data-index))))))))
