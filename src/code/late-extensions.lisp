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
;;;
;;; FIXME / IMPORTANT: If the slot is raw, the address is correct only for
;;; instances of the specified class, not its subclasses!
#!+sb-thread
(defmacro define-structure-slot-addressor (name &key structure slot)
  (let* ((dd (find-defstruct-description structure t))
         (slotd (when dd (find slot (dd-slots dd) :key #'dsd-name)))
         (index (when slotd (dsd-index slotd)))
         (raw-type (dsd-raw-type slotd)))
    (unless index
      (error "Slot ~S not found in ~S." slot structure))
    `(progn
       (declaim (inline ,name))
       (defun ,name (instance)
         (declare (type ,structure instance) (optimize speed))
         (sb!ext:truly-the
          sb!vm:word
          (+ (sb!kernel:get-lisp-obj-address instance)
             (- (* ,(if (eq t raw-type)
                        (+ sb!vm:instance-slots-offset index)
                        (- (1+ (sb!kernel::dd-instance-length dd)) sb!vm:instance-slots-offset index
                           (1- (sb!kernel::raw-slot-words raw-type))))
                   sb!vm:n-word-bytes)
                sb!vm:instance-pointer-lowtag)))))))

(defmacro compare-and-swap (place old new &environment env)
  "Atomically stores NEW in PLACE if OLD matches the current value of PLACE.
Two values are considered to match if they are EQ. Returns the previous value
of PLACE: if the returned value is EQ to OLD, the swap was carried out.

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
       `(%compare-and-swap-symbol-plist (the symbol ,@args) ,old (the list ,new)))
      (symbol-value
       (destructuring-bind (name) args
         (flet ((slow (symbol)
                  (with-unique-names (n-symbol n-old n-new)
                    `(let ((,n-symbol ,symbol)
                           (,n-old ,old)
                           (,n-new ,new))
                       (declare (symbol ,n-symbol))
                       (about-to-modify-symbol-value ,n-symbol 'compare-and-swap ,n-new)
                       (%compare-and-swap-symbol-value ,n-symbol ,n-old ,n-new)))))
           (if (sb!xc:constantp name env)
               (let ((cname (constant-form-value name env)))
                 (if (eq :special (info :variable :kind cname))
                     ;; Since we know the symbol is a special, we can just generate
                     ;; the type check.
                     `(%compare-and-swap-symbol-value
                       ',cname ,old (the ,(info :variable :type cname) ,new))
                     (slow (list 'quote cname))))
               (slow name)))))
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

(defun expand-atomic-frob (name place diff)
  (flet ((invalid-place ()
           (error "Invalid first argument to ~S: ~S" name place)))
    (unless (consp place)
      (invalid-place))
    (destructuring-bind (op &rest args) place
      (case op
        (aref
         (when (cddr args)
           (invalid-place))
         #!+(or x86 x86-64 ppc)
         (with-unique-names (array)
           `(let ((,array (the (simple-array sb!ext:word (*)) ,(car args))))
              (%array-atomic-incf/word
               ,array
               (%check-bound ,array (array-dimension ,array 0) ,(cadr args))
               (logand #.(1- (ash 1 sb!vm:n-word-bits))
                       ,(ecase name
                               (atomic-incf
                                `(the sb!vm:signed-word ,diff))
                               (atomic-decf
                                `(- (the sb!vm:signed-word ,diff))))))))
         #!-(or x86 x86-64 ppc)
         (with-unique-names (array index old-value)
           (let ((incremented-value
                  (ecase name
                         (atomic-incf
                          `(+ ,old-value (the sb!vm:signed-word ,diff)))
                         (atomic-decf
                          `(- ,old-value (the sb!vm:signed-word ,diff))))))
             `(sb!sys:without-interrupts
               (let* ((,array ,(car args))
                      (,index ,(cadr args))
                      (,old-value (aref ,array ,index)))
                 (setf (aref ,array ,index)
                       (logand #.(1- (ash 1 sb!vm:n-word-bits))
                               ,incremented-value))
                 ,old-value)))))
        (t
         (when (cdr args)
           (invalid-place))
         (let ((dd (info :function :structure-accessor op)))
           (if dd
               (let* ((structure (dd-name dd))
                      (slotd (find op (dd-slots dd) :key #'dsd-accessor-name))
                      (index (dsd-index slotd))
                      (type (dsd-type slotd)))
                 (declare (ignorable structure index))
                 (unless (and (eq 'sb!vm:word (dsd-raw-type slotd))
                              (type= (specifier-type type) (specifier-type 'sb!vm:word)))
                   (error "~S requires a slot of type (UNSIGNED-BYTE ~S), not ~S: ~S"
                          name sb!vm:n-word-bits type place))
                 (when (dsd-read-only slotd)
                   (error "Cannot use ~S with structure accessor for a read-only slot: ~S"
                          name place))
                 #!+(or x86 x86-64 ppc)
                 `(truly-the sb!vm:word
                             (%raw-instance-atomic-incf/word
                              (the ,structure ,@args) ,index
                              (logand #.(1- (ash 1 sb!vm:n-word-bits))
                                      ,(ecase name
                                              (atomic-incf
                                               `(the sb!vm:signed-word ,diff))
                                              (atomic-decf
                                               `(- (the sb!vm:signed-word ,diff)))))))
                 ;; No threads outside x86 and x86-64 for now, so this is easy...
                 #!-(or x86 x86-64 ppc)
                 (with-unique-names (structure old)
                                    `(sb!sys:without-interrupts
                                      (let* ((,structure ,@args)
                                             (,old (,op ,structure)))
                                        (setf (,op ,structure)
                                              (logand #.(1- (ash 1 sb!vm:n-word-bits))
                                                      ,(ecase name
                                                              (atomic-incf
                                                               `(+ ,old (the sb!vm:signed-word ,diff)))
                                                              (atomic-decf
                                                               `(- ,old (the sb!vm:signed-word ,diff))))))
                                        ,old))))
             (invalid-place))))))))

(defmacro atomic-incf (place &optional (diff 1))
  #!+sb-doc
  "Atomically increments PLACE by DIFF, and returns the value of PLACE before
the increment.

The incrementation is done using word-size modular arithmetic: on 32 bit
platforms ATOMIC-INCF of #xFFFFFFFF by one results in #x0 being stored in
PLACE.

PLACE must be an accessor form whose CAR is the name of a DEFSTRUCT accessor
whose declared type is (UNSIGNED-BYTE 32) on 32 bit platforms,
and (UNSIGNED-BYTE 64) on 64 bit platforms or an AREF of a (SIMPLE-ARRAY
SB-EXT:WORD (*) -- the type SB-EXT:WORD can be used for this purpose.

DIFF defaults to 1, and must be a (SIGNED-BYTE 32) on 32 bit platforms,
and (SIGNED-BYTE 64) on 64 bit platforms.

EXPERIMENTAL: Interface subject to change."
  (expand-atomic-frob 'atomic-incf place diff))

(defmacro atomic-decf (place &optional (diff 1))
  #!+sb-doc
  "Atomically decrements PLACE by DIFF, and returns the value of PLACE before
the increment.

The decrementation is done using word-size modular arithmetic: on 32 bit
platforms ATOMIC-DECF of #x0 by one results in #xFFFFFFFF being stored in
PLACE.

PLACE must be an accessor form whose CAR is the name of a DEFSTRUCT accessor
whose declared type is (UNSIGNED-BYTE 32) on 32 bit platforms,
and (UNSIGNED-BYTE 64) on 64 bit platforms or an AREF of a (SIMPLE-ARRAY
SB-EXT:WORD (*) -- the type SB-EXT:WORD can be used for this purpose.

DIFF defaults to 1, and must be a (SIGNED-BYTE 32) on 32 bit platforms,
and (SIGNED-BYTE 64) on 64 bit platforms.

EXPERIMENTAL: Interface subject to change."
  (expand-atomic-frob 'atomic-decf place diff))

;; Interpreter stubs for ATOMIC-INCF.
#!+(or x86 x86-64 ppc)
(defun %array-atomic-incf/word (array index diff)
  (declare (type (simple-array word (*)) array)
           (fixnum index)
           (type sb!vm:signed-word diff))
  (%array-atomic-incf/word array index diff))

(defun call-hooks (kind hooks &key (on-error :error))
  (dolist (hook hooks)
    (handler-case
        (funcall hook)
      (serious-condition (c)
        (if (eq :warn on-error)
            (warn "Problem running ~A hook ~S:~%  ~A" kind hook c)
            (with-simple-restart (continue "Skip this ~A hook." kind)
              (error "Problem running ~A hook ~S:~%  ~A" kind hook c)))))))

;;;; DEFGLOBAL

(defmacro-mundanely defglobal (name value &optional (doc nil docp))
  #!+sb-doc
  "Defines NAME as a global variable that is always bound. VALUE is evaluated
and assigned to NAME both at compile- and load-time, but only if NAME is not
already bound.

Global variables share their values between all threads, and cannot be
locally bound, declared special, defined as constants, and neither bound
nor defined as symbol macros.

See also the declarations SB-EXT:GLOBAL and SB-EXT:ALWAYS-BOUND."
  `(progn
     (eval-when (:compile-toplevel)
       (let ((boundp (boundp ',name)))
         (%compiler-defglobal ',name (unless boundp ,value) boundp)))
     (eval-when (:load-toplevel :execute)
       (let ((boundp (boundp ',name)))
         (%defglobal ',name (unless boundp ,value) boundp ',doc ,docp
                     (sb!c:source-location))))))

(defun %compiler-defglobal (name value boundp)
  (sb!xc:proclaim `(global ,name))
  (unless boundp
    #-sb-xc-host
    (set-symbol-global-value name value)
    #+sb-xc-host
    (set name value))
  (sb!xc:proclaim `(always-bound ,name)))

(defun %defglobal (name value boundp doc docp source-location)
  (%compiler-defglobal name value boundp)
  (when docp
    (setf (fdocumentation name 'variable) doc))
  (sb!c:with-source-location (source-location)
    (setf (info :source-location :variable name) source-location))
  name)
