;;;; This file contains parts of the Alien implementation that
;;;; are not part of the compiler.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-ALIEN")

(/show0 "target-alieneval.lisp 15")

;;;; alien variables

;;; Make a string out of the symbol, converting all uppercase letters to
;;; lower case and hyphens into underscores.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun guess-alien-name-from-lisp-name (lisp-name)
    (declare (type symbol lisp-name))
    (nsubstitute #\_ #\- (string-downcase (symbol-name lisp-name)))))

;;; The opposite of GUESS-ALIEN-NAME-FROM-LISP-NAME. Make a symbol out
;;; of the string, converting all lowercase letters to uppercase and
;;; underscores into hyphens.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun guess-lisp-name-from-alien-name (alien-name)
    (declare (type simple-string alien-name))
    (intern (nsubstitute #\- #\_ (string-upcase alien-name)))))

;;; Extract the Lisp and alien names from NAME. If only one is given,
;;; guess the other.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun pick-lisp-and-alien-names (name)
    (flet ((oops ()
             (error "~@<~:IMalformed alien name. Acceptable formats are:~
                     ~:@_  (\"alien_name\" LISP-NAME)~
                     ~:@_  FOO-BAR                - equivalent to (\"foo_bar\" FOO-BAR)~
                     ~:@_  \"foo_bar\"              - equivalent to (\"foo_bar\" FOO-BAR)~:@>")))
      (typecase name
       (string
        (values (guess-lisp-name-from-alien-name name)
                (coerce name 'simple-string)))
       (symbol
        (values name (guess-alien-name-from-lisp-name name)))
       (list
        (unless (and (proper-list-of-length-p name 2)
                     (symbolp (second name))
                     (stringp (first name)))
          (oops))
        (values (second name) (coerce (first name) 'simple-string)))
       (t
        (oops))))))

(defmacro define-alien-variable (name type &environment env)
  "Define NAME as an external alien variable of type TYPE. NAME should
be a list of a string holding the alien name and a symbol to use as
the Lisp name. If NAME is just a symbol or string, then the other name
is guessed from the one supplied."
  (multiple-value-bind (lisp-name alien-name) (pick-lisp-and-alien-names name)
    (with-auxiliary-alien-types env
      (let ((alien-type (parse-alien-type type env)))
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           ,@(when *new-auxiliary-types*
               `((%def-auxiliary-alien-types ',*new-auxiliary-types*
                                             (sb-c:source-location))))
           (%define-alien-variable ',lisp-name
                                   ',alien-name
                                   ',alien-type
                                   (sb-c:source-location)))))))

;;; Do the actual work of DEFINE-ALIEN-VARIABLE.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %define-alien-variable (lisp-name alien-name type location)
    (setf (info :variable :kind lisp-name) :alien)
    (setf (info :variable :where-from lisp-name) :defined)
    (setf (info :variable :alien-info lisp-name)
          (make-heap-alien-info :type type
                                :alien-name alien-name
                                :datap t))
    (setf (info :source-location :variable lisp-name) location)
    lisp-name))

(defun alien-value (symbol)
  "Returns the value of the alien variable bound to SYMBOL. Signals an
error if SYMBOL is not bound to an alien variable, or if the alien
variable is undefined."
  (%heap-alien (or (info :variable :alien-info symbol)
                   (error 'unbound-variable :name symbol))))

(defmacro extern-alien (name type &environment env)
  "Access the alien variable named NAME, assuming it is of type TYPE.
This is SETFable."
  (let* ((alien-name (possibly-base-stringize
                      (etypecase name
                       (symbol (guess-alien-name-from-lisp-name name))
                       (string name))))
         (alien-type (parse-alien-type type env))
         (datap (not (alien-fun-type-p alien-type))))
    `(%alien-value (foreign-symbol-sap ,alien-name ,datap) 0 ',alien-type)))

(defmacro with-alien (bindings &body body &environment env)
  "Establish some local alien variables. Each BINDING is of the form:
     VAR TYPE [ ALLOCATION ] [ INITIAL-VALUE | EXTERNAL-NAME ]
   ALLOCATION should be one of:
     :LOCAL (the default)
       The alien is allocated on the stack, and has dynamic extent.
     :EXTERN
       No alien is allocated, but VAR is established as a local name for
       the external alien given by EXTERNAL-NAME."
  ;; FIXME:
  ;;      :STATIC
  ;;        The alien is allocated on the heap, and has infinite extent. The alien
  ;;        is allocated at load time, so the same piece of memory is used each time
  ;;        this form executes.
  (let (bind-alien-stack-pointer)
    (with-auxiliary-alien-types env
      (dolist (binding (reverse bindings))
        (destructuring-bind
            (symbol type &optional opt1 (opt2 nil opt2p))
            binding
          (let* ((alien-type (parse-alien-type type env))
                 (datap (not (alien-fun-type-p alien-type))))
            (multiple-value-bind (allocation initial-value)
                (if opt2p
                    (values opt1 opt2)
                    (case opt1
                      (:extern
                       (values opt1 (guess-alien-name-from-lisp-name symbol)))
                      (:static
                       (values opt1 nil))
                      (t
                       (values :local opt1))))
              (setf body
                    (ecase allocation
                      #+nil
                      (:static
                       (let ((sap
                               (make-symbol (concatenate 'string "SAP-FOR-"
                                                         (symbol-name symbol)))))
                         `((let ((,sap (load-time-value (%make-alien ...))))
                             (declare (type system-area-pointer ,sap))
                             (symbol-macrolet
                                 ((,symbol (sap-alien ,sap ,type)))
                               ,@(when initial-value
                                   `((setq ,symbol ,initial-value)))
                               ,@body)))))
                      (:extern
                       `((symbol-macrolet
                             ((,symbol
                                (%alien-value
                                 (foreign-symbol-sap ,initial-value ,datap) 0 ,alien-type)))
                           ,@body)))
                      (:local
                       (let* ((var (gensym "VAR"))
                              (initval (if initial-value (gensym "INITVAL")))
                              (info (make-local-alien-info :type alien-type))
                              (inner-body
                                `((note-local-alien-type ',info ,var)
                                  (symbol-macrolet ((,symbol (local-alien ',info ,var)))
                                    ,@(when initial-value
                                        `((setq ,symbol ,initval)))
                                    ,@body)))
                              (body-forms
                                (if initial-value
                                    `((let ((,initval ,initial-value))
                                        ,@inner-body))
                                    inner-body)))
                         (setf bind-alien-stack-pointer t)
                         `((let ((,var (make-local-alien ',info)))
                             ,@body-forms))))))))))
      (verify-local-auxiliaries-okay)
      `(symbol-macrolet ((&auxiliary-type-definitions&
                           ,(append *new-auxiliary-types*
                                    (auxiliary-type-definitions env))))
         ,@(cond
             (bind-alien-stack-pointer
              ;; The LET IR1-translator will actually turn this into
              ;; RESTORING-NSP on #-c-stack-is-control-stack to avoid
              ;; expanding into non-standard special forms.
              ;; And the LET has to look exactly like this, not LET*
              ;; and no other bindings.
              `((let ((sb-c:*alien-stack-pointer* sb-c:*alien-stack-pointer*))
                  ,@body)))
             (t
              body))))))

;;;; runtime C values that don't correspond directly to Lisp types

(defmethod print-object ((value alien-value) stream)
  ;; Don't use ":TYPE T" here - TYPE-OF isn't what we want.
  (print-unreadable-object (value stream)
    (format stream "~S ~S #X~8,'0X ~S ~/sb-impl:print-type-specifier/"
            'alien-value
            :sap (sap-int (alien-value-sap value))
            :type (unparse-alien-type (alien-value-type value)))))

(declaim (inline null-alien))
(defun null-alien (x)
  "Return true if X (which must be an ALIEN pointer) is null, false otherwise."
  (zerop (sap-int (alien-sap x))))

(defmacro sap-alien (sap type &environment env)
  "Convert the system area pointer SAP to an ALIEN of the specified TYPE (not
   evaluated.) TYPE must be pointer-like."
  (let ((alien-type (parse-alien-type type env)))
    (if (eq (compute-alien-rep-type alien-type) 'system-area-pointer)
        `(%sap-alien ,sap ',alien-type)
        (error "cannot make an alien of type ~S out of a SAP" type))))

(defun alien-sap (alien)
  "Return a System-Area-Pointer pointing to Alien's data."
  (declare (type alien-value alien))
  (alien-value-sap alien))

;;;; allocation/deallocation of heap aliens

(defmacro make-alien (type &optional size &environment env)
  "Allocate an alien of type TYPE in foreign heap, and return an alien
pointer to it. The allocated memory is not initialized, and may
contain garbage. The memory is allocated using malloc(3), so it can be
passed to foreign functions which use free(3), or released using
FREE-ALIEN.

For alien stack allocation, see macro WITH-ALIEN.

The TYPE argument is not evaluated. If SIZE is supplied, how it is
interpreted depends on TYPE:

  * When TYPE is a foreign array type, an array of that type is
    allocated, and a pointer to it is returned. Note that you
    must use DEREF to first access the array through the pointer.

    If supplied, SIZE is used as the first dimension for the array.

  * When TYPE is any other foreign type, then an object for that
    type is allocated, and a pointer to it is returned. So
    (make-alien int) returns a (* int).

    If SIZE is specified, then a block of that many objects is
    allocated, with the result pointing to the first one.

Examples:

  (defvar *foo* (make-alien (array char 10)))
  (type-of *foo*)                   ; => (alien (* (array (signed 8) 10)))
  (setf (deref (deref *foo*) 0) 10) ; => 10

  (make-alien char 12)              ; => (alien (* (signed 8)))"
  (let ((alien-type (if (alien-type-p type)
                        type
                        (parse-alien-type type env))))
    (multiple-value-bind (size-expr element-type)
        (if (alien-array-type-p alien-type)
            (let ((dims (alien-array-type-dimensions alien-type)))
              (cond
                (size
                 (unless dims
                   (error
                    "cannot override the size of zero-dimensional arrays"))
                 (when (constantp size)
                   (setf alien-type
                         (make-alien-array-type
                          :dimensions (cons (constant-form-value size) (cdr dims))
                          :element-type (alien-array-type-element-type alien-type)
                          :bits (alien-type-bits alien-type)
                          :alignment (alien-type-alignment alien-type)))))
                (dims
                 (setf size (car dims)))
                (t
                 (setf size 1)))
              (values `(* ,size ,@(cdr dims))
                      (alien-array-type-element-type alien-type)))
            (values (or size 1) alien-type))
      (let ((bits (alien-type-bits element-type))
            (alignment (alien-type-alignment element-type)))
        (unless bits
          (error "The size of ~S is unknown."
                 (unparse-alien-type element-type)))
        (unless alignment
          (error "The alignment of ~S is unknown."
                 (unparse-alien-type element-type)))
        ;; This is the one place where the %SAP-ALIEN note is quite
        ;; undesirable, in most uses of MAKE-ALIEN the %SAP-ALIEN
        ;; cannot be optimized away.
        `(locally (declare (muffle-conditions compiler-note))
           ;; FIXME: Do we really need the ASH/+7 here after ALIGN-OFFSET?
           (%sap-alien (%make-alien (* ,(ash (+ 7 (align-offset bits alignment)) -3)
                                       (the index ,size-expr)))
                       ',(make-alien-pointer-type :to alien-type)))))))

(defun malloc-error (bytes)
  (error 'simple-storage-condition
         :format-control "~A: malloc() of ~S bytes failed."
         :format-arguments (list (strerror (get-errno)) bytes)))

;;; Allocate a block of memory at least BYTES bytes long and return a
;;; system area pointer to it.
(declaim (inline %make-alien))
(defun %make-alien (bytes)
  (declare (type index bytes)
           (optimize (sb-c:alien-funcall-saves-fp-and-pc 0)))
  (let ((sap (alien-funcall (extern-alien "malloc"
                                          (function system-area-pointer size-t))
                            bytes)))
    (if (and (eql 0 (sap-int sap)) (not (eql 0 bytes)))
        (malloc-error bytes)
        sap)))

#+c-stack-is-control-stack
(declaim (inline invoke-with-saved-fp))
;;; On :c-stack-is-control-stack platforms, this DEFUN must appear prior to the
;;; first cross-compile-time use of ALIEN-FUNCALL, the transform of which is
;;; an invocation of INVOKE-WITH-SAVED-FP, which should be inlined.
#+c-stack-is-control-stack
(defun invoke-with-saved-fp (fn)
  (declare (muffle-conditions compiler-note)
           (optimize (speed 3)))
  ;; No need to link to the previous value, it can be fetched from the binding stack.
  (let ((*saved-fp* (sb-c::current-fp-fixnum)))
    (funcall fn)))

(declaim (inline free-alien))
(defun free-alien (alien)
  "Dispose of the storage pointed to by ALIEN. The ALIEN must have been
allocated by MAKE-ALIEN, MAKE-ALIEN-STRING or malloc(3)."
  (alien-funcall (extern-alien "free" (function (values) system-area-pointer))
                 (alien-sap alien))
  nil)

(declaim (type (function * (values system-area-pointer index))
               %make-alien-string))
(defun %make-alien-string (string &key (start 0) end
                                       (external-format :default)
                                       (null-terminate t))
  ;; FIXME: This is slow. We want a function to get the length of the
  ;; encoded string so we can allocate the foreign memory first and
  ;; encode directly there.
  (let* ((octets (string-to-octets string
                                   :start start :end end
                                   :external-format external-format
                                   :null-terminate null-terminate))
         (count (length octets))
         (buf (%make-alien count)))
    (sb-kernel:copy-ub8-to-system-area octets 0 buf 0 count)
    (values buf count)))

(defun make-alien-string (string &rest rest
                                 &key (start 0) end
                                      (external-format :default)
                                      (null-terminate t))
  "Copy part of STRING delimited by START and END into freshly
allocated foreign memory, freeable using free(3) or FREE-ALIEN.
Returns the allocated string as a (* CHAR) alien, and the number of
bytes allocated as secondary value.

The string is encoded using EXTERNAL-FORMAT. If NULL-TERMINATE is
true (the default), the alien string is terminated by an additional
null byte."
  (declare (ignore start end external-format null-terminate))
  (multiple-value-bind (sap bytes)
      (apply #'%make-alien-string string rest)
    (values (%sap-alien sap (parse-alien-type '(* char) nil))
            bytes)))

(define-compiler-macro make-alien-string (&rest args)
  `(multiple-value-bind (sap bytes) (%make-alien-string ,@args)
     (values (%sap-alien sap ',(parse-alien-type '(* char) nil))
             bytes)))

;;;; the SLOT operator

;;; Find the field named SLOT, or die trying.
(defun slot-or-lose (type slot)
  (declare (type alien-record-type type)
           (type symbol slot))
  (or (find slot (alien-record-type-fields type)
            :key #'alien-record-field-name)
      (error "There is no slot named ~S in ~S." slot type)))

;;; Extract the value from the named slot from the record ALIEN. If
;;; ALIEN is actually a pointer, then DEREF it first.
(defun slot (alien slot)
  "Extract SLOT from the Alien STRUCT or UNION ALIEN. May be set with SETF."
  (declare (type alien-value alien)
           (type symbol slot))
  (let ((type (alien-value-type alien)))
    (etypecase type
      (alien-pointer-type
       (slot (deref alien) slot))
      (alien-record-type
       (let ((field (slot-or-lose type slot)))
         (%alien-value (alien-value-sap alien)
                       (alien-record-field-offset field)
                       (alien-record-field-type field)))))))

;;; Deposit the value in the specified slot of the record ALIEN. If
;;; the ALIEN is really a pointer, DEREF it first. The compiler uses
;;; this when it can't figure out anything better.
(defun %set-slot (alien slot value)
  (declare (type alien-value alien)
           (type symbol slot))
  (let ((type (alien-value-type alien)))
    (etypecase type
      (alien-pointer-type
       (%set-slot (deref alien) slot value))
      (alien-record-type
       (let ((field (slot-or-lose type slot)))
         (setf (%alien-value (alien-value-sap alien)
                             (alien-record-field-offset field)
                             (alien-record-field-type field))
               value))))))

;;; Compute the address of the specified slot and return a pointer to it.
(defun %slot-addr (alien slot)
  (declare (type alien-value alien)
           (type symbol slot))
  (let ((type (alien-value-type alien)))
    (etypecase type
      (alien-pointer-type
       (%slot-addr (deref alien) slot))
      (alien-record-type
       (let* ((field (slot-or-lose type slot))
              (offset (alien-record-field-offset field))
              (field-type (alien-record-field-type field)))
         (%sap-alien (sap+ (alien-sap alien) (/ offset sb-vm:n-byte-bits))
                     (make-alien-pointer-type :to field-type)))))))

;;;; the DEREF operator

;;; This function does most of the work of the different DEREF
;;; methods. It returns two values: the type and the offset (in bits)
;;; of the referred-to alien.
(defun deref-guts (alien indices)
  (declare (type alien-value alien)
           (type list indices)
           (values alien-type integer))
  (let ((type (alien-value-type alien)))
    (etypecase type
      (alien-pointer-type
       (when (cdr indices)
         (error "too many indices when DEREF'ing ~S: ~W"
                type
                (length indices)))
       (let ((element-type (alien-pointer-type-to type)))
         (values element-type
                 (if indices
                     (* (align-offset (alien-type-bits element-type)
                                      (alien-type-alignment element-type))
                        (car indices))
                     0))))
      (alien-array-type
       (unless (= (length indices) (length (alien-array-type-dimensions type)))
         (error "incorrect number of indices when DEREF'ing ~S: ~W"
                type (length indices)))
       (labels ((frob (dims indices offset)
                  (if (null dims)
                      offset
                      (frob (cdr dims) (cdr indices)
                        (+ (if (zerop offset)
                               0
                               (* offset (car dims)))
                           (car indices))))))
         (let ((element-type (alien-array-type-element-type type)))
           (values element-type
                   (* (align-offset (alien-type-bits element-type)
                                    (alien-type-alignment element-type))
                      (frob (alien-array-type-dimensions type)
                        indices 0)))))))))

;;; Dereference the alien and return the results.
(defun deref (alien &rest indices)
  "Dereference an Alien pointer or array. If an array, the indices are used
   as the indices of the array element to access. If a pointer, one index can
   optionally be specified, giving the equivalent of C pointer arithmetic."
  (declare (type alien-value alien)
           (type list indices))
  (multiple-value-bind (target-type offset) (deref-guts alien indices)
    (%alien-value (alien-value-sap alien)
                  offset
                  target-type)))

(defun %set-deref (alien value &rest indices)
  (declare (type alien-value alien)
           (type list indices))
  (multiple-value-bind (target-type offset) (deref-guts alien indices)
    (setf (%alien-value (alien-value-sap alien)
                        offset
                        target-type)
          value)))

(defun %deref-addr (alien &rest indices)
  (declare (type alien-value alien)
           (type list indices))
  (multiple-value-bind (target-type offset) (deref-guts alien indices)
    (%sap-alien (sap+ (alien-value-sap alien) (/ offset sb-vm:n-byte-bits))
                (make-alien-pointer-type :to target-type))))

;;;; accessing heap alien variables

(defun %heap-alien (info)
  (declare (type heap-alien-info info))
  (%alien-value (heap-alien-info-sap info)
                0
                (heap-alien-info-type info)))

(defun %set-heap-alien (info value)
  (declare (type heap-alien-info info))
  (setf (%alien-value (heap-alien-info-sap info)
                      0
                      (heap-alien-info-type info))
        value))

(defun %heap-alien-addr (info)
  (declare (type heap-alien-info info))
  (%sap-alien (heap-alien-info-sap info)
              (make-alien-pointer-type :to (heap-alien-info-type info))))

;;;; accessing local aliens

(defun make-local-alien (info)
  (let* ((alien (eval `(make-alien ,(local-alien-info-type info))))
         (alien-sap (alien-sap alien)))
    (finalize
     alien
     (lambda ()
       (alien-funcall
        (extern-alien "free" (function (values) system-area-pointer))
        alien-sap))
     :dont-save t)
    alien))

(defun note-local-alien-type (info alien)
  (declare (ignore info alien))
  nil)

(defun local-alien (info alien)
  (declare (ignore info))
  (deref alien))

(defun %set-local-alien (info alien value)
  (declare (ignore info))
  (setf (deref alien) value))

(define-setf-expander local-alien (&whole whole info alien)
  (let ((value (gensym))
        (info-var (gensym))
        (alloc-tmp (gensym))
        (info (if (and (consp info)
                       (eq (car info) 'quote))
                  (second info)
                  (error "Something is wrong; local-alien-info not found: ~S"
                         whole))))
    (values nil
            nil
            (list value)
            `(if (%local-alien-forced-to-memory-p ',info)
                 (%set-local-alien ',info ,alien ,value)
                   (let* ((,info-var ',(local-alien-info-type info))
                          (,alloc-tmp (deport-alloc ,value ,info-var)))
                     (maybe-with-pinned-objects (,alloc-tmp) (,(local-alien-info-type info))
                       (setf ,alien (deport ,alloc-tmp ,info-var)))))
            whole)))

(defun %local-alien-forced-to-memory-p (info)
  (local-alien-info-force-to-memory-p info))

(defun %local-alien-addr (info alien)
  (declare (type local-alien-info info))
  (unless (local-alien-info-force-to-memory-p info)
    (error "~S isn't forced to memory. Something went wrong." alien))
  alien)


;;;; the ADDR macro

(defmacro addr (expr &environment env)
  "Return an Alien pointer to the data addressed by Expr, which must be a call
   to SLOT or DEREF, or a reference to an Alien variable."
  (let ((form (%macroexpand expr env)))
    (or (typecase form
          (cons
           (case (car form)
             (slot
              (cons '%slot-addr (cdr form)))
             (deref
              (cons '%deref-addr (cdr form)))
             (%heap-alien
              (cons '%heap-alien-addr (cdr form)))
             (local-alien
              (let ((info (let ((info-arg (second form)))
                            (and (consp info-arg)
                                 (eq (car info-arg) 'quote)
                                 (second info-arg)))))
                (unless (local-alien-info-p info)
                  (error "Something is wrong, LOCAL-ALIEN-INFO not found: ~S"
                         form))
                (setf (local-alien-info-force-to-memory-p info) t))
              (cons '%local-alien-addr (cdr form)))))
          (symbol
           (let ((kind (info :variable :kind form)))
             (when (eq kind :alien)
               `(%heap-alien-addr ',(info :variable :alien-info form))))))
        (error "~S is not a valid L-value." form))))

(push '("SB-ALIEN" define-alien-type-class define-alien-type-method)
      *!removable-symbols*)


;;;; the CAST macro

(defmacro cast (alien type &environment env)
  "Convert ALIEN to an Alien of the specified TYPE (not evaluated.)  Both types
   must be Alien array, pointer or function types."
  `(%cast ,alien ',(parse-alien-type type env)))

(defun %cast (alien target-type)
  (declare (type alien-value alien)
           (type alien-type target-type)
           (optimize (safety 2)))
  (if (or (alien-pointer-type-p target-type)
          (alien-array-type-p target-type)
          (alien-fun-type-p target-type))
      (let ((alien-type (alien-value-type alien)))
        (if (or (alien-pointer-type-p alien-type)
                (alien-array-type-p alien-type)
                (alien-fun-type-p alien-type))
            (naturalize (alien-value-sap alien) target-type)
            (error "~S cannot be casted." alien)))
      (error "cannot cast to alien type ~S" (unparse-alien-type target-type))))

;;;; the ALIEN-SIZE macro

(defmacro alien-size (type &optional (units :bits) &environment env)
  "Return the size of the alien type TYPE. UNITS specifies the units to
   use and can be either :BITS, :BYTES, or :WORDS."
  (let* ((alien-type (parse-alien-type type env))
         (bits (alien-type-bits alien-type)))
    (if bits
        (values (ceiling bits
                         (ecase units
                           (:bits 1)
                           (:bytes sb-vm:n-byte-bits)
                           (:words sb-vm:n-word-bits))))
        (error "unknown size for alien type ~S"
               (unparse-alien-type alien-type)))))

;;;; NATURALIZE, DEPORT, EXTRACT-ALIEN-VALUE, DEPOSIT-ALIEN-VALUE

;;; There is little cost to making an interpreted function,
;;; however it is even better if we can share the function object,
;;; especially with sb-fasteval which avoids work on repeated invocations.
;;; sb-eval doesn't optimize its IR in the same way,
;;; but this is still a boon from a memory consumption stance.
;;;
;;; GLOBALDB-SXHASHOID serves as a nice hash function for this purpose anyway.
;;; Arguably we could key the cache off the internalized alien-type object
;;; which induced creation of an interpreted lambda, rather than the s-expr,
;;; but we'd need to get the TYPE and the method {NATURALIZE, DEPORT, etc} here,
;;; so it would be a more invasive change.
;;;
(defun-cached (coerce-to-interpreted-function
               :hash-bits 8 :hash-function #'globaldb-sxhashoid)
    ((lambda-form equal))
  (let (#+(or sb-eval sb-fasteval)
        (*evaluator-mode* :interpret))
    (coerce lambda-form 'function)))

(defun naturalize (alien type)
  (declare (type alien-type type))
  (funcall (coerce-to-interpreted-function (compute-naturalize-lambda type))
           alien type))

(defun deport (value type)
  (declare (type alien-type type))
  (funcall (coerce-to-interpreted-function (compute-deport-lambda type))
           value type))

(defun deport-alloc (value type)
  (declare (type alien-type type))
  (funcall (coerce-to-interpreted-function (compute-deport-alloc-lambda type))
           value type))

(defun %alien-value (sap offset type)
  (declare (type system-area-pointer sap)
           (type unsigned-byte offset)
           (type alien-type type))
  (funcall (coerce-to-interpreted-function (compute-extract-lambda type))
           sap offset type))

(defun (setf %alien-value) (value sap offset type)
  (declare (type system-area-pointer sap)
           (type unsigned-byte offset)
           (type alien-type type))
  (funcall (coerce-to-interpreted-function (compute-deposit-lambda type))
           value sap offset type))

;;;; ALIEN-FUNCALL, DEFINE-ALIEN-ROUTINE

(defun alien-funcall (alien &rest args)
  "Call the foreign function ALIEN with the specified arguments. ALIEN's
type specifies the argument and result types."
  (declare (type alien-value alien))
  (let ((type (alien-value-type alien)))
    (typecase type
      (alien-pointer-type
       (apply #'alien-funcall (deref alien) args))
      (alien-fun-type
       (unless (= (length (alien-fun-type-arg-types type))
                  (length args))
         (error "wrong number of arguments for ~S~%expected ~W, got ~W"
                type
                (length (alien-fun-type-arg-types type))
                (length args)))
       (let ((stub (alien-fun-type-stub type)))
         (unless stub
           (setf stub
                 (let ((fun (gensym "FUN"))
                       (parms (make-gensym-list (length args))))
                   (compile nil
                            `(lambda (,fun ,@parms)
                               (declare (optimize (sb-c:insert-step-conditions 0)))
                               (declare (type (alien ,type) ,fun))
                               (alien-funcall ,fun ,@parms)))))
           (setf (alien-fun-type-stub type) stub))
         (apply stub alien args)))
      (t
       (error "~S is not an alien function." alien)))))

(defmacro define-alien-routine (name result-type
                                     &rest args
                                     &environment lexenv)
  "DEFINE-ALIEN-ROUTINE Name Result-Type {(Arg-Name Arg-Type [Style])}*

Define a foreign interface function for the routine with the specified NAME.
Also automatically DECLAIM the FTYPE of the defined function.

NAME may be either a string, a symbol, or a list of the form (string symbol).

RETURN-TYPE is the alien type for the function return value. VOID may be
used to specify a function with no result.

The remaining forms specify individual arguments that are passed to the
routine. ARG-NAME is a symbol that names the argument, primarily for
documentation. ARG-TYPE is the C type of the argument. STYLE specifies the
way that the argument is passed.

:IN
      An :IN argument is simply passed by value. The value to be passed is
      obtained from argument(s) to the interface function. No values are
      returned for :In arguments. This is the default mode.

:OUT
      The specified argument type must be a pointer to a fixed sized object.
      A pointer to a preallocated object is passed to the routine, and the
      the object is accessed on return, with the value being returned from
      the interface function. :OUT and :IN-OUT cannot be used with pointers
      to arrays, records or functions.

:COPY
      This is similar to :IN, except that the argument values are stored
      on the stack, and a pointer to the object is passed instead of
      the value itself.

:IN-OUT
      This is a combination of :OUT and :COPY. A pointer to the argument is
      passed, with the object being initialized from the supplied argument
      and the return value being determined by accessing the object on
      return."
  (binding* (((lisp-name alien-name) (pick-lisp-and-alien-names name))
             ;; The local name is uninterned so that we don't preclude
             ;;   (defconstant kill 9)
             ;;   (define-alien-routine "kill" int (pid int) (sig int))
             ;; which, if we didn't hide the local name, would get:
             ;;  "Attempt to bind a constant variable with SYMBOL-MACROLET: KILL"
             (local-name (copy-symbol lisp-name)))
    (collect ((docs) (lisp-args) (lisp-arg-types)
              (lisp-result-types
               (cond ((eql result-type 'void)
                      ;; What values does a function return, if it
                      ;; returns no values? Exactly one - NIL. -- APD,
                      ;; 2003-03-02
                      (list 'null))
                     (t
                      ;; FIXME: Check for VALUES.
                      (list `(alien ,result-type)))))
              (arg-types) (alien-vars)
              (alien-args) (results))
      (dolist (arg args)
        (cond ((stringp arg)
               (docs arg))
              ((eq arg '&optional)
               (arg-types arg))
              (t
               (destructuring-bind (name type &optional (style :in)) arg
                 (unless (member style '(:in :copy :out :in-out))
                   (error "bogus argument style ~S in ~S" style arg))
                 (when (and (member style '(:out :in-out))
                            (typep (parse-alien-type type lexenv)
                                   'alien-pointer-type))
                   (error "can't use :OUT or :IN-OUT on pointer-like type:~%  ~S"
                          type))
                 (let (arg-type)
                   (cond ((eq style :in)
                          (setq arg-type type)
                          (alien-args name))
                         (t
                          (setq arg-type `(* ,type))
                          (if (eq style :out)
                              (alien-vars `(,name ,type))
                              (alien-vars `(,name ,type ,name)))
                          (alien-args `(addr ,name))))
                   (arg-types arg-type)
                   (unless (eq style :out)
                     (lisp-args name)
                     (lisp-arg-types t
                                     ;; FIXME: It should be something
                                     ;; like `(ALIEN ,ARG-TYPE), except
                                     ;; for we also accept SAPs where
                                     ;; pointers are required.
                                     )))
                 (when (or (eq style :out) (eq style :in-out))
                   (results name)
                   (lisp-result-types `(alien ,type)))))))
      `(progn
         ;; The theory behind this automatic DECLAIM is that (1) if
         ;; you're calling C, static typing is what you're doing
         ;; anyway, and (2) such a declamation can be (especially for
         ;; alien values) both messy to do by hand and very important
         ;; for performance of later code which uses the return value.
         (declaim (ftype (function ,(lisp-arg-types)
                                   ;; c-string also accepts aliens and
                                   ;; byte-arrays, but on output it
                                   ;; produces strings.
                                   (values ,@(substitute '(or simple-string null)
                                                         '(alien c-string)
                                                         (lisp-result-types)
                                                         :test #'equal) &optional))
                         ,lisp-name))
         (defun ,lisp-name ,(lisp-args)
           ,@(docs)
           (with-alien
            ((,local-name (function ,result-type ,@(arg-types))
                         :extern ,alien-name)
             ,@(alien-vars))
             ,@(if (eq 'void result-type)
                   `((alien-funcall ,local-name ,@(alien-args))
                     (values nil ,@(results)))
                   `((values (alien-funcall ,local-name ,@(alien-args))
                             ,@(results))))))))))

(defun alien-typep (object type)
  "Return T iff OBJECT is an alien of type TYPE."
  (let ((lisp-rep-type (compute-lisp-rep-type type)))
    (if lisp-rep-type
        (typep object lisp-rep-type)
        (and (alien-value-p object)
             (alien-subtype-p (alien-value-type object) type)))))

(defun alien-value-typep (object type)
  (when (alien-value-p object)
    (alien-subtype-p (alien-value-type object) type)))

(defun alien-void-type-p (type)
  (and (alien-values-type-p type) (not (alien-values-type-values type))))

;;; Assert that two important types aren't messed up
(eval-when (:compile-toplevel)
  (flet ((check-size (tag)
           (let ((alien-type (parse-alien-type `(struct ,tag) nil)))
             (unless (= (alien-type-bits alien-type)
                        (* (symbol-value (package-symbolicate "SB-UNIX" "SIZEOF-" tag))
                           sb-vm:n-byte-bits))
               (error "(STRUCT ~S) has unexpected size" tag)))))
    (check-size 'sb-unix::timespec)
    (check-size 'sb-unix::timeval)))
