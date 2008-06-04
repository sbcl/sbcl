;;;; various extensions (including SB-INT "internal extensions")
;;;; available both in the cross-compilation host Lisp and in the
;;;; target SBCL, but which can't be defined on the target until until
;;;; some significant amount of machinery (e.g. error-handling) is
;;;; defined

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;; Is X a list for which LENGTH is meaningful, i.e. a list which is
;;; not improper and which is not circular?
(defun list-with-length-p (x)
  (values (ignore-errors (list-length x))))

;;; not used in 0.7.8, but possibly useful for defensive programming
;;; in e.g. (COERCE ... 'VECTOR)
;;;(defun list-length-or-die (x)
;;;  (or (list-length x)
;;;      ;; not clear how to do this best:
;;;      ;;   * Should this be a TYPE-ERROR? Colloquially that'd make
;;;      ;;     lots of sense, but since I'm not sure how to express
;;;      ;;     "noncircular list" as a Lisp type expression, coding
;;;      ;;     it seems awkward.
;;;      ;;   * Should the ERROR object include the offending value?
;;;      ;;     Ordinarily that's helpful, but if the user doesn't have
;;;      ;;     his printer set up to deal with cyclicity, we might not
;;;      ;;     be doing him a favor by printing the object here.
;;;      ;; -- WHN 2002-10-19
;;;      (error "can't calculate length of cyclic list")))

;;; This is used in constructing arg lists for debugger printing,
;;; and when needing to print unbound slots in PCL.
(defstruct (unprintable-object
            (:constructor make-unprintable-object (string))
            (:print-object (lambda (x s)
                             (print-unreadable-object (x s)
                               (write-string (unprintable-object-string x) s))))
            (:copier nil))
  string)

;;; Used internally, but it would be nice to provide something
;;; like this for users as well.

#!+sb-thread
(defmacro define-structure-slot-addressor (name &key structure slot)
  (let* ((dd (find-defstruct-description structure t))
         (slotd (when dd (find slot (dd-slots dd) :key #'dsd-name)))
         (index (when slotd (dsd-index slotd))))
    (unless index
      (error "Slot ~S not found in ~S." slot structure))
    `(progn
       (declaim (inline ,name))
       (defun ,name (instance)
         (declare (type ,structure instance) (optimize speed))
         (sb!ext:truly-the
          sb!vm:word
          (+ (sb!kernel:get-lisp-obj-address instance)
             (- (* ,(+ sb!vm:instance-slots-offset index) sb!vm:n-word-bytes)
                sb!vm:instance-pointer-lowtag)))))))

(defmacro compare-and-swap (place old new)
  "Atomically stores NEW in PLACE if OLD matches the current value of PLACE.
Two values are considered to match if they are EQ. Returns the previous value
of PLACE: if the returned value if EQ to OLD, the swap was carried out.

PLACE must be an accessor form whose CAR is one of the following:

 CAR, CDR, FIRST, REST, SYMBOL-PLIST, SYMBOL-VALUE, SVREF

or the name of a DEFSTRUCT created accessor for a slot whose declared type is
either FIXNUM or T. Results are unspecified if the slot has a declared type
other then FIXNUM or T.

EXPERIMENTAL: Interface subject to change."
  (flet ((invalid-place ()
           (error "Invalid first argument to COMPARE-AND-SWAP: ~S" place)))
    (unless (consp place)
      (invalid-place))
  ;; FIXME: Not the nicest way to do this...
  (destructuring-bind (op &rest args) place
    (case op
      ((car first)
       `(%compare-and-swap-car (the cons ,@args) ,old ,new))
      ((cdr rest)
       `(%compare-and-swap-cdr (the cons ,@args) ,old ,new))
      (symbol-plist
       `(%compare-and-swap-symbol-plist (the symbol ,@args) ,old ,new))
      (symbol-value
       `(%compare-and-swap-symbol-value (the symbol ,@args) ,old ,new))
      (svref
       (let ((vector (car args))
             (index (cadr args)))
         (unless (and vector index (not (cddr args)))
           (invalid-place))
         (with-unique-names (v)
           `(let ((,v ,vector))
              (declare (simple-vector ,v))
              (%compare-and-swap-svref ,v (%check-bound ,v (length ,v) ,index) ,old ,new)))))
      (t
       (let ((dd (info :function :structure-accessor op)))
         (if dd
             (let* ((structure (dd-name dd))
                    (slotd (find op (dd-slots dd) :key #'dsd-accessor-name))
                    (index (dsd-index slotd))
                    (type (dsd-type slotd)))
               (unless (eq t (dsd-raw-type slotd))
                 (error "Cannot use COMPARE-AND-SWAP with structure accessor for a typed slot: ~S"
                        place))
               (when (dsd-read-only slotd)
                 (error "Cannot use COMPARE-AND-SWAP with structure accessor for a read-only slot: ~S"
                        place))
               `(truly-the (values ,type &optional)
                           (%compare-and-swap-instance-ref (the ,structure ,@args)
                                                           ,index
                                                           (the ,type ,old) (the ,type ,new))))
             (error "Invalid first argument to COMPARE-AND-SWAP: ~S" place))))))))

(macrolet ((def (name lambda-list ref &optional set)
             #!+compare-and-swap-vops
             (declare (ignore ref set))
             `(defun ,name (,@lambda-list old new)
                #!+compare-and-swap-vops
                (,name ,@lambda-list old new)
                #!-compare-and-swap-vops
                (let ((current (,ref ,@lambda-list)))
                  (when (eq current old)
                    ,(if set
                         `(,set ,@lambda-list new)
                         `(setf (,ref ,@lambda-list) new)))
                  current))))
  (def %compare-and-swap-car (cons) car)
  (def %compare-and-swap-cdr (cons) cdr)
  (def %compare-and-swap-instance-ref (instance index) %instance-ref %instance-set)
  (def %compare-and-swap-symbol-plist (symbol) symbol-plist)
  (def %compare-and-swap-symbol-value (symbol) symbol-value)
  (def %compare-and-swap-svref (vector index) svref))

(defun call-hooks (kind hooks &key (on-error :error))
  (dolist (hook hooks)
    (handler-case
        (funcall hook)
      (serious-condition (c)
        (if (eq :warn on-error)
            (warn "Problem running ~A hook ~S:~%  ~A" kind hook c)
            (with-simple-restart (continue "Skip this ~A hook." kind)
              (error "Problem running ~A hook ~S:~%  ~A" kind hook c)))))))
