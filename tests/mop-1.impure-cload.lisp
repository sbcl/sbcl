;;;; miscellaneous side-effectful tests of the MOP

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

;;;; Note that the MOP is not in an entirely supported state.
;;;; However, this seems a good a way as any of ensuring that we have
;;;; no regressions.

;;; This is basically the DYNAMIC-SLOT-CLASS example from AMOP, with
;;; fixups for running in the full MOP rather than closette: SLOTDs
;;; instead of slot-names, and so on.

(defclass dynamic-slot-class (standard-class) ())

(defmethod sb-mop:validate-superclass
    ((class dynamic-slot-class) (super standard-class))
  t)

(defmethod sb-mop:compute-effective-slot-definition
           ((class dynamic-slot-class) name direct-slots)
  (let ((slot (call-next-method)))
    (setf (sb-mop:slot-definition-allocation slot) :dynamic)
    slot))

(defun dynamic-slot-p (slot)
  (eq (sb-mop:slot-definition-allocation slot) :dynamic))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (let ((table (make-hash-table)))

    (defun allocate-table-entry (instance)
      (setf (gethash instance table) ()))

    (defun read-dynamic-slot-value (instance slot-name)
      (let* ((alist (gethash instance table))
             (entry (assoc slot-name alist)))
        (if (null entry)
            (error "slot ~S unbound in ~S" slot-name instance)
            (cdr entry))))

    (defun write-dynamic-slot-value (new-value instance slot-name)
      (let* ((alist (gethash instance table))
             (entry (assoc slot-name alist)))
        (if (null entry)
            (push `(,slot-name . ,new-value)
                  (gethash instance table))
            (setf (cdr entry) new-value))
        new-value))

    (defun dynamic-slot-boundp (instance slot-name)
      (let* ((alist (gethash instance table))
             (entry (assoc slot-name alist)))
        (not (null entry))))

    (defun dynamic-slot-makunbound (instance slot-name)
      (let* ((alist (gethash instance table))
             (entry (assoc slot-name alist)))
        (unless (null entry)
          (setf (gethash instance table) (delete entry alist))))
      instance)

    ))

(defmethod allocate-instance ((class dynamic-slot-class) &key)
  (let ((instance (call-next-method)))
    (allocate-table-entry instance)
    instance))

(defmethod sb-mop:slot-value-using-class ((class dynamic-slot-class)
                                          instance slotd)
  (let ((slot (find slotd (sb-mop:class-slots class))))
    (if slot
        (read-dynamic-slot-value instance (sb-mop:slot-definition-name slotd))
        (call-next-method))))

(defmethod (setf sb-mop:slot-value-using-class) (new-value (class dynamic-slot-class)
                                                 instance slotd)
  (let ((slot (find slotd (sb-mop:class-slots class))))
    (if slot
        (write-dynamic-slot-value new-value instance (sb-mop:slot-definition-name slotd))
        (call-next-method))))

(defmethod sb-mop:slot-boundp-using-class ((class dynamic-slot-class)
                                           instance slotd)
  (let ((slot (find slotd (sb-mop:class-slots class))))
    (if slot
        (dynamic-slot-boundp instance (sb-mop:slot-definition-name slotd))
        (call-next-method))))

(defmethod sb-mop:slot-makunbound-using-class ((class dynamic-slot-class)
                                               instance slotd)
  (let ((slot (find slotd (sb-mop:class-slots class))))
    (if slot
        (dynamic-slot-makunbound instance (sb-mop:slot-definition-name slotd))
        (call-next-method))))

(defclass test-class-1 ()
  ((slot1 :initarg :slot1)
   (slot2 :initarg :slot2 :initform nil))
  (:metaclass dynamic-slot-class))

(defclass test-class-2 (test-class-1)
  ((slot2 :initarg :slot2 :initform t)
   (slot3 :initarg :slot3))
  (:metaclass dynamic-slot-class))

(defvar *one* (make-instance 'test-class-1))
(defvar *two* (make-instance 'test-class-2 :slot3 1))

(with-test (:name :mop-1)
  (assert (not (slot-boundp *one* 'slot1)))
  (assert (null (slot-value *one* 'slot2)))
  (assert (eq t (slot-value *two* 'slot2)))
  (assert (= 1 (slot-value *two* 'slot3))))
