;;;; This file contains parts of the ALIEN implementation that
;;;; are not part of the compiler.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!ALIEN")

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
      (etypecase name
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
  #!+sb-doc
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
                                             (sb!c:source-location))))
           (%define-alien-variable ',lisp-name
                                   ',alien-name
                                   ',alien-type
                                   (sb!c:source-location)))))))

;;; Do the actual work of DEFINE-ALIEN-VARIABLE.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %define-alien-variable (lisp-name alien-name type location)
    (setf (info :variable :kind lisp-name) :alien)
    (setf (info :variable :where-from lisp-name) :defined)
    (setf (info :variable :alien-info lisp-name)
          (make-heap-alien-info :type type
                                :alien-name alien-name
                                :datap t))
    (setf (info :source-location :variable lisp-name) location)))

(defun alien-value (symbol)
  #!+sb-doc
  "Returns the value of the alien variable bound to SYMBOL. Signals an
error if SYMBOL is not bound to an alien variable, or if the alien
variable is undefined."
  (%heap-alien (or (info :variable :alien-info symbol)
                   (error 'unbound-variable :name symbol))))

(defmacro extern-alien (name type &environment env)
  #!+sb-doc
  "Access the alien variable named NAME, assuming it is of type TYPE.
This is SETFable."
  (let* ((alien-name (etypecase name
                       (symbol (guess-alien-name-from-lisp-name name))
                       (string name)))
         (alien-type (parse-alien-type type env))
         (datap (not (alien-fun-type-p alien-type))))
    `(%alien-value (foreign-symbol-sap ,alien-name ,datap) 0 ',alien-type)))

(defmacro with-alien (bindings &body body &environment env)
  #!+sb-doc
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
  (/show "entering WITH-ALIEN" bindings)
  (with-auxiliary-alien-types env
    (dolist (binding (reverse bindings))
      (/show binding)
      (destructuring-bind
          (symbol type &optional opt1 (opt2 nil opt2p))
          binding
        (/show symbol type opt1 opt2)
        (let* ((alien-type (parse-alien-type type env))
               (datap (not (alien-fun-type-p alien-type))))
          (/show alien-type)
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
            (/show allocation initial-value)
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
                     (/show0 ":EXTERN case")
                     `((symbol-macrolet
                           ((,symbol
                              (%alien-value
                               (foreign-symbol-sap ,initial-value ,datap) 0 ,alien-type)))
                         ,@body)))
                    (:local
                     (/show0 ":LOCAL case")
                     (let* ((var (sb!xc:gensym "VAR"))
                            (initval (if initial-value (sb!xc:gensym "INITVAL")))
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
                       (/show var initval info)
                       #!+(or x86 x86-64)
                       `((let ((,var (make-local-alien ',info)))
                           ,@body-forms))
                       ;; FIXME: This version is less efficient then it needs to be, since
                       ;; it could just save and restore the number-stack pointer once,
                       ;; instead of doing multiple decrements if there are multiple bindings.
                       #!-(or x86 x86-64)
                       `((let ((,var (make-local-alien ',info)))
                           (multiple-value-prog1
                               (progn ,@body-forms)
                             ;; No need for unwind protect here, since
                             ;; allocation involves modifying NSP, and
                             ;; NSP is saved and restored during NLX.
                             ;; And in non-transformed case it
                             ;; performs finalization.
                             (dispose-local-alien ',info ,var))))))))))))
    (/show "revised" body)
    (verify-local-auxiliaries-okay)
    (/show0 "back from VERIFY-LOCAL-AUXILIARIES-OK, returning")
    `(symbol-macrolet ((&auxiliary-type-definitions&
                        ,(append *new-auxiliary-types*
                                 (auxiliary-type-definitions env))))
       #!+(or x86 x86-64)
       (let ((sb!vm::*alien-stack-pointer* sb!vm::*alien-stack-pointer*))
         ,@body)
       #!-(or x86 x86-64)
       ,@body)))

;;;; runtime C values that don't correspond directly to Lisp types

;;; Note: The DEFSTRUCT for ALIEN-VALUE lives in a separate file
;;; 'cause it has to be real early in the cold-load order.
#!-sb-fluid (declaim (freeze-type alien-value))
(def!method print-object ((value alien-value) stream)
  (print-unreadable-object (value stream)
    ;; See identical kludge in host-alieneval.
    (let ((sb!pretty:*pprint-quote-with-syntactic-sugar* nil))
      (declare (special sb!pretty:*pprint-quote-with-syntactic-sugar*))
      (format stream
            "~S ~S #X~8,'0X ~S ~S"
            'alien-value
            :sap (sap-int (alien-value-sap value))
            :type (unparse-alien-type (alien-value-type value))))))

#!-sb-fluid (declaim (inline null-alien))
(defun null-alien (x)
  #!+sb-doc
  "Return true if X (which must be an ALIEN pointer) is null, false otherwise."
  (zerop (sap-int (alien-sap x))))

(defmacro sap-alien (sap type &environment env)
  #!+sb-doc
  "Convert the system area pointer SAP to an ALIEN of the specified TYPE (not
   evaluated.) TYPE must be pointer-like."
  (let ((alien-type (parse-alien-type type env)))
    (if (eq (compute-alien-rep-type alien-type) 'system-area-pointer)
        `(%sap-alien ,sap ',alien-type)
        (error "cannot make an alien of type ~S out of a SAP" type))))

(defun %sap-alien (sap type)
  (declare (type system-area-pointer sap)
           (type alien-type type))
  (make-alien-value :sap sap :type type))

(defun alien-sap (alien)
  #!+sb-doc
  "Return a System-Area-Pointer pointing to Alien's data."
  (declare (type alien-value alien))
  (alien-value-sap alien))

;;;; allocation/deallocation of heap aliens

(defmacro make-alien (type &optional size &environment env)
  #!+sb-doc
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
  (type-of *foo*)                 ; => (alien (* (array (signed 8) 10)))
  (setf (deref (deref foo) 0) 10) ; => 10

  (make-alien char 12)            ; => (alien (* (signed 8)))"
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
                   (setf alien-type (copy-alien-array-type alien-type))
                   (setf (alien-array-type-dimensions alien-type)
                         (cons (constant-form-value size) (cdr dims)))))
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

(defun malloc-error (bytes errno)
  (error 'simple-storage-condition
         :format-control "~A: malloc() of ~S bytes failed."
         :format-arguments (list (strerror errno) bytes)))

;;; Allocate a block of memory at least BYTES bytes long and return a
;;; system area pointer to it.
#!-sb-fluid (declaim (inline %make-alien))
(defun %make-alien (bytes)
  (declare (type index bytes)
           (optimize (sb!c:alien-funcall-saves-fp-and-pc 0)))
  (let ((sap (alien-funcall (extern-alien "malloc"
                                          (function system-area-pointer size-t))
                            bytes)))
    (if (and (not (eql 0 bytes)) (eql 0 (sap-int sap)))
        (malloc-error bytes (get-errno))
        sap)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *saved-fp-and-pcs* nil)
  ;; Can't use DECLAIM since always-bound is a non-standard declaration
  (sb!xc:proclaim '(sb!ext:always-bound *saved-fp-and-pcs*)))

#!+c-stack-is-control-stack
(declaim (inline invoke-with-saved-fp-and-pc))
;;; On :c-stack-is-control-stack platforms, this DEFUN must appear prior to the
;;; first cross-compile-time use of ALIEN-FUNCALL, the transform of which is
;;; an invocation of INVOKE-WITH-SAVED-FP-AND-PC, which should be inlined.
;;; Makes no sense when compiling for the host.
#!+(and c-stack-is-control-stack (host-feature sb-xc))
(defun invoke-with-saved-fp-and-pc (fn)
  (declare #-sb-xc-host (muffle-conditions compiler-note)
           (optimize (speed 3)))
  (dx-let ((fp-and-pc (make-array 2 :element-type 'word)))
    (setf (aref fp-and-pc 0) (sb!kernel:get-lisp-obj-address
                              (sb!kernel:%caller-frame))
          (aref fp-and-pc 1) (sap-int (sb!kernel:%caller-pc)))
    (dx-let ((*saved-fp-and-pcs* (cons fp-and-pc *saved-fp-and-pcs*)))
      (funcall fn))))

#!-sb-fluid (declaim (inline free-alien))
(defun free-alien (alien)
  #!+sb-doc
  "Dispose of the storage pointed to by ALIEN. The ALIEN must have been
allocated by MAKE-ALIEN, MAKE-ALIEN-STRING or malloc(3)."
  (alien-funcall (extern-alien "free" (function (values) system-area-pointer))
                 (alien-sap alien))
  nil)

(declaim (type (sfunction * system-area-pointer) %make-alien-string))
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
    (sb!kernel:copy-ub8-to-system-area octets 0 buf 0 count)
    buf))

(defun make-alien-string (string &rest rest
                                 &key (start 0) end
                                      (external-format :default)
                                      (null-terminate t))
  #!+sb-doc
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
  #!+sb-doc
  "Extract SLOT from the Alien STRUCT or UNION ALIEN. May be set with SETF."
  (declare (type alien-value alien)
           (type symbol slot)
           (optimize (inhibit-warnings 3)))
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
           (type symbol slot)
           (optimize (inhibit-warnings 3)))
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
           (type symbol slot)
           (optimize (inhibit-warnings 3)))
  (let ((type (alien-value-type alien)))
    (etypecase type
      (alien-pointer-type
       (%slot-addr (deref alien) slot))
      (alien-record-type
       (let* ((field (slot-or-lose type slot))
              (offset (alien-record-field-offset field))
              (field-type (alien-record-field-type field)))
         (%sap-alien (sap+ (alien-sap alien) (/ offset sb!vm:n-byte-bits))
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
  #!+sb-doc
  "Dereference an Alien pointer or array. If an array, the indices are used
   as the indices of the array element to access. If a pointer, one index can
   optionally be specified, giving the equivalent of C pointer arithmetic."
  (declare (type alien-value alien)
           (type list indices)
           (optimize (inhibit-warnings 3)))
  (multiple-value-bind (target-type offset) (deref-guts alien indices)
    (%alien-value (alien-value-sap alien)
                  offset
                  target-type)))

(defun %set-deref (alien value &rest indices)
  (declare (type alien-value alien)
           (type list indices)
           (optimize (inhibit-warnings 3)))
  (multiple-value-bind (target-type offset) (deref-guts alien indices)
    (setf (%alien-value (alien-value-sap alien)
                        offset
                        target-type)
          value)))

(defun %deref-addr (alien &rest indices)
  (declare (type alien-value alien)
           (type list indices)
           (optimize (inhibit-warnings 3)))
  (multiple-value-bind (target-type offset) (deref-guts alien indices)
    (%sap-alien (sap+ (alien-value-sap alien) (/ offset sb!vm:n-byte-bits))
                (make-alien-pointer-type :to target-type))))

;;;; accessing heap alien variables

(defun %heap-alien (info)
  (declare (type heap-alien-info info)
           (optimize (inhibit-warnings 3)))
  (%alien-value (heap-alien-info-sap info)
                0
                (heap-alien-info-type info)))

(defun %set-heap-alien (info value)
  (declare (type heap-alien-info info)
           (optimize (inhibit-warnings 3)))
  (setf (%alien-value (heap-alien-info-sap info)
                      0
                      (heap-alien-info-type info))
        value))

(defun %heap-alien-addr (info)
  (declare (type heap-alien-info info)
           (optimize (inhibit-warnings 3)))
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

;; It's not mandatory that this function not exist for x86[-64],
;; however for sanity, it should not, because no call to it can occur.
#!-(or x86 x86-64)
(defun dispose-local-alien (info alien)
  (declare (ignore info))
  (cancel-finalization alien)
  (free-alien alien))

;;;; the CAST macro

(defmacro cast (alien type &environment env)
  #!+sb-doc
  "Convert ALIEN to an Alien of the specified TYPE (not evaluated.)  Both types
   must be Alien array, pointer or function types."
  `(%cast ,alien ',(parse-alien-type type env)))

(defun %cast (alien target-type)
  (declare (type alien-value alien)
           (type alien-type target-type)
           (optimize (safety 2))
           (optimize (inhibit-warnings 3)))
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
  #!+sb-doc
  "Return the size of the alien type TYPE. UNITS specifies the units to
   use and can be either :BITS, :BYTES, or :WORDS."
  (let* ((alien-type (parse-alien-type type env))
         (bits (alien-type-bits alien-type)))
    (if bits
        (values (ceiling bits
                         (ecase units
                           (:bits 1)
                           (:bytes sb!vm:n-byte-bits)
                           (:words sb!vm:n-word-bits))))
        (error "unknown size for alien type ~S"
               (unparse-alien-type alien-type)))))

;;;; NATURALIZE, DEPORT, EXTRACT-ALIEN-VALUE, DEPOSIT-ALIEN-VALUE

(defun coerce-to-interpreted-function (lambda-form)
  (let (#!+sb-eval
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
  #!+sb-doc
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
                 (let ((fun (sb!xc:gensym "FUN"))
                       (parms (make-gensym-list (length args))))
                   (compile nil
                            `(lambda (,fun ,@parms)
                               (declare (optimize (sb!c::insert-step-conditions 0)))
                               (declare (type (alien ,type) ,fun))
                               (alien-funcall ,fun ,@parms)))))
           (setf (alien-fun-type-stub type) stub))
         (apply stub alien args)))
      (t
       (error "~S is not an alien function." alien)))))

(defmacro define-alien-routine (name result-type
                                     &rest args
                                     &environment lexenv)
  #!+sb-doc
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
  (multiple-value-bind (lisp-name alien-name)
      (pick-lisp-and-alien-names name)
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
        (if (stringp arg)
            (docs arg)
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
                (lisp-result-types `(alien ,type))))))
      `(progn
         ;; The theory behind this automatic DECLAIM is that (1) if
         ;; you're calling C, static typing is what you're doing
         ;; anyway, and (2) such a declamation can be (especially for
         ;; alien values) both messy to do by hand and very important
         ;; for performance of later code which uses the return value.
         (declaim (ftype (function ,(lisp-arg-types)
                                   (values ,@(lisp-result-types) &optional))
                         ,lisp-name))
         (defun ,lisp-name ,(lisp-args)
           ,@(docs)
           (with-alien
            ((,lisp-name (function ,result-type ,@(arg-types))
                         :extern ,alien-name)
             ,@(alien-vars))
             ,@(if (eq 'void result-type)
                   `((alien-funcall ,lisp-name ,@(alien-args))
                     (values nil ,@(results)))
                   `((values (alien-funcall ,lisp-name ,@(alien-args))
                             ,@(results))))))))))

(defun alien-typep (object type)
  #!+sb-doc
  "Return T iff OBJECT is an alien of type TYPE."
  (let ((lisp-rep-type (compute-lisp-rep-type type)))
    (if lisp-rep-type
        (typep object lisp-rep-type)
        (and (alien-value-p object)
             (alien-subtype-p (alien-value-type object) type)))))

(defun alien-value-typep (object type)
  (when (alien-value-p object)
    (alien-subtype-p (alien-value-type object) type)))

;;;; ALIEN CALLBACKS
;;;;
;;;; See "Foreign Linkage / Callbacks" in the SBCL Internals manual.

(defvar *alien-callback-info* nil
  #!+sb-doc
  "Maps SAPs to corresponding CALLBACK-INFO structures: contains all the
information we need to manipulate callbacks after their creation. Used for
changing the lisp-side function they point to, invalidation, etc.")

(defstruct callback-info
  specifier
  function ; NULL if invalid
  wrapper
  index)

(defun callback-info-key (info)
  (cons (callback-info-specifier info) (callback-info-function info)))

(defun alien-callback-info (alien)
  (cdr (assoc (alien-sap alien) *alien-callback-info* :test #'sap=)))

(defvar *alien-callbacks* (make-hash-table :test #'equal)
  #!+sb-doc
  "Cache of existing callback SAPs, indexed with (SPECIFER . FUNCTION). Used for
memoization: we don't create new callbacks if one pointing to the correct
function with the same specifier already exists.")

(defvar *alien-callback-wrappers* (make-hash-table :test #'equal)
  #!+sb-doc
  "Cache of existing lisp wrappers, indexed with SPECIFER. Used for memoization:
we don't create new wrappers if one for the same specifier already exists.")

(defvar *alien-callback-trampolines* (make-array 32 :fill-pointer 0 :adjustable t)
  #!+sb-doc
  "Lisp trampoline store: assembler wrappers contain indexes to this, and
ENTER-ALIEN-CALLBACK pulls the corresponding trampoline out and calls it.")

(defun %alien-callback-sap (specifier result-type argument-types function wrapper
                            &optional call-type)
  (declare #!-x86 (ignore call-type))
  (let ((key (list specifier function)))
    (or (gethash key *alien-callbacks*)
        (setf (gethash key *alien-callbacks*)
              (let* ((index (fill-pointer *alien-callback-trampolines*))
                     ;; Aside from the INDEX this is known at
                     ;; compile-time, which could be utilized by
                     ;; having the two-stage assembler tramp &
                     ;; wrapper mentioned in [1] above: only the
                     ;; per-function tramp would need assembler at
                     ;; runtime. Possibly we could even pregenerate
                     ;; the code and just patch the index in later.
                     (assembler-wrapper
                      (alien-callback-assembler-wrapper
                       index result-type argument-types
                       #!+x86
                       (if (eq call-type :stdcall)
                           (ceiling
                            (apply #'+
                                   (mapcar 'alien-type-word-aligned-bits
                                           argument-types))
                            8)
                           0))))
                (vector-push-extend
                 (alien-callback-lisp-trampoline wrapper function)
                 *alien-callback-trampolines*)
                ;; Assembler-wrapper is static, so sap-taking is safe.
                (let ((sap (vector-sap assembler-wrapper)))
                  (push (cons sap (make-callback-info :specifier specifier
                                                      :function function
                                                      :wrapper wrapper
                                                      :index index))
                        *alien-callback-info*)
                  sap))))))

(defun alien-callback-lisp-trampoline (wrapper function)
  (declare (function wrapper) (optimize speed))
  (lambda (args-pointer result-pointer)
    (funcall wrapper args-pointer result-pointer function)))

(defun alien-callback-lisp-wrapper-lambda (specifier result-type argument-types env)
  (let* ((arguments (make-gensym-list (length argument-types)))
         (argument-names arguments)
         (argument-specs (cddr specifier)))
    `(lambda (args-pointer result-pointer function)
       ;; FIXME: the saps are not gc safe
       (let ((args-sap (int-sap
                        (sb!kernel:get-lisp-obj-address args-pointer)))
             (res-sap (int-sap
                       (sb!kernel:get-lisp-obj-address result-pointer))))
         (declare (ignorable args-sap res-sap))
         (with-alien
             ,(loop
                 with offset = 0
                 for spec in argument-specs
                 ;; KLUDGE: At least one platform requires additional
                 ;; alignment beyond a single machine word for certain
                 ;; arguments.  Accept an additional delta (for the
                 ;; alignment) to apply to subsequent arguments to
                 ;; account for the alignment gaps as a secondary
                 ;; value, so that we don't have to update unaffected
                 ;; backends.
                 for (accessor-form alignment)
                   = (multiple-value-list
                      (alien-callback-accessor-form spec 'args-sap offset))
                 collect `(,(pop argument-names) ,spec
                            :local ,accessor-form)
                 do (incf offset (+ (alien-callback-argument-bytes spec env)
                                    (or alignment 0))))
           ,(flet ((store (spec real-type)
                          (if spec
                              `(setf (deref (sap-alien res-sap (* ,spec)))
                                     ,(if real-type
                                          `(the ,real-type
                                             (funcall function ,@arguments))
                                          `(funcall function ,@arguments)))
                              `(funcall function ,@arguments))))
                  (cond ((alien-void-type-p result-type)
                         (store nil nil))
                        ((alien-integer-type-p result-type)
                         ;; Integer types should be padded out to a full
                         ;; register width, to comply with most ABI calling
                         ;; conventions, but should be typechecked on the
                         ;; declared type width, hence the following:
                         (if (alien-integer-type-signed result-type)
                             (store `(signed
                                      ,(alien-type-word-aligned-bits result-type))
                                    `(signed-byte ,(alien-type-bits result-type)))
                             (store
                              `(unsigned
                                ,(alien-type-word-aligned-bits result-type))
                              `(unsigned-byte ,(alien-type-bits result-type)))))
                        (t
                         (store (unparse-alien-type result-type) nil))))))
       (values))))

(defun invalid-alien-callback (&rest arguments)
  (declare (ignore arguments))
  (error "Invalid alien callback called."))

(defun parse-callback-specification (result-type lambda-list)
  (values
   `(function ,result-type ,@(mapcar #'second lambda-list))
   (mapcar #'first lambda-list)))

(defun parse-alien-ftype (specifier env)
  (destructuring-bind (function result-type &rest argument-types)
      specifier
    (aver (eq 'function function))
    (multiple-value-bind (bare-result-type calling-convention)
        (typecase result-type
          ((cons calling-convention *)
             (values (second result-type) (first result-type)))
          (t result-type))
      (values (let ((*values-type-okay* t))
                (parse-alien-type bare-result-type env))
              (mapcar (lambda (spec)
                        (parse-alien-type spec env))
                      argument-types)
              calling-convention))))

(defun alien-void-type-p (type)
  (and (alien-values-type-p type) (not (alien-values-type-values type))))

(defun alien-type-word-aligned-bits (type)
  (align-offset (alien-type-bits type) sb!vm:n-word-bits))

(defun alien-callback-argument-bytes (spec env)
  (let ((type (parse-alien-type spec env)))
    (if (or (alien-integer-type-p type)
            (alien-float-type-p type)
            (alien-pointer-type-p type)
            (alien-system-area-pointer-type-p type))
        (ceiling (alien-type-word-aligned-bits type) sb!vm:n-byte-bits)
        (error "Unsupported callback argument type: ~A" type))))

(defun enter-alien-callback (index return arguments)
  (funcall (aref *alien-callback-trampolines* index)
           return
           arguments))

;;; To ensure that callback wrapper functions continue working even
;;; if #'ENTER-ALIEN-CALLBACK moves in memory, access to it is indirected
;;; through the *ENTER-ALIEN-CALLBACK* static symbol. -- JES, 2006-01-01
(defvar *enter-alien-callback* #'enter-alien-callback)

;;;; interface (not public, yet) for alien callbacks

(defmacro alien-callback (specifier function &environment env)
  #!+sb-doc
  "Returns an alien-value with of alien ftype SPECIFIER, that can be passed to
an alien function as a pointer to the FUNCTION. If a callback for the given
SPECIFIER and FUNCTION already exists, it is returned instead of consing a new
one."
  ;; Pull out as much work as is convenient to macro-expansion time, specifically
  ;; everything that can be done given just the SPECIFIER and ENV.
  (multiple-value-bind (result-type argument-types call-type)
      (parse-alien-ftype specifier env)
    `(%sap-alien
      (%alien-callback-sap ',specifier ',result-type ',argument-types
                           ,function
                           (or (gethash ',specifier *alien-callback-wrappers*)
                               (setf (gethash ',specifier *alien-callback-wrappers*)
                                     (compile nil
                                              ',(alien-callback-lisp-wrapper-lambda
                                                 specifier result-type argument-types env))))
                           ,call-type)
      ',(parse-alien-type specifier env))))

(defun alien-callback-p (alien)
  #!+sb-doc
  "Returns true if the alien is associated with a lisp-side callback,
and a secondary return value of true if the callback is still valid."
  (let ((info (alien-callback-info alien)))
    (when info
      (values t (and (callback-info-function info) t)))))

(defun alien-callback-function (alien)
  #!+sb-doc
  "Returns the lisp function designator associated with the callback."
  (let ((info (alien-callback-info alien)))
    (when info
      (callback-info-function info))))

(defun (setf alien-callback-function) (function alien)
  #!+sb-doc
  "Changes the lisp function designated by the callback."
  (let ((info (alien-callback-info alien)))
    (unless info
      (error "Not an alien callback: ~S" alien))
    ;; sap cache
    (let ((key (callback-info-key info)))
      (remhash key *alien-callbacks*)
      (setf (gethash key *alien-callbacks*) (alien-sap alien)))
    ;; trampoline
    (setf (aref *alien-callback-trampolines* (callback-info-index info))
          (alien-callback-lisp-trampoline (callback-info-wrapper info) function))
    ;; metadata
    (setf (callback-info-function info) function)
    function))

(defun invalidate-alien-callback (alien)
  #!+sb-doc
  "Invalidates the callback designated by the alien, if any, allowing the
associated lisp function to be GC'd, and causing further calls to the same
callback signal an error."
  (let ((info (alien-callback-info alien)))
    (when (and info (callback-info-function info))
      ;; sap cache
      (remhash (callback-info-key info) *alien-callbacks*)
      ;; trampoline
      (setf (aref *alien-callback-trampolines* (callback-info-index info))
            #'invalid-alien-callback)
      ;; metadata
      (setf (callback-info-function info) nil)
      t)))

;;; FIXME: This call assembles a new callback for every closure,
;;; which sucks hugely. ...not that I can think of an obvious
;;; solution. Possibly maybe we could write a generalized closure
;;; callback analogous to closure_tramp, and share the actual wrapper?
;;;
;;; For lambdas that result in simple-funs we get the callback from
;;; the cache on subsequent calls.
(defmacro alien-lambda (result-type typed-lambda-list &body forms)
  (multiple-value-bind (specifier lambda-list)
      (parse-callback-specification result-type typed-lambda-list)
    `(alien-callback ,specifier (lambda ,lambda-list ,@forms))))

;;; FIXME: Should subsequent (SETF FDEFINITION) affect the callback or not?
;;; What about subsequent DEFINE-ALIEN-CALLBACKs? My guess is that changing
;;; the FDEFINITION should invalidate the callback, and redefining the
;;; callback should change existing callbacks to point to the new defintion.
(defmacro define-alien-callback (name result-type typed-lambda-list &body forms)
  #!+sb-doc
  "Defines #'NAME as a function with the given body and lambda-list, and NAME as
the alien callback for that function with the given alien type."
  (declare (symbol name))
  (multiple-value-bind (specifier lambda-list)
      (parse-callback-specification result-type typed-lambda-list)
    `(progn
       (defun ,name ,lambda-list ,@forms)
       (defparameter ,name (alien-callback ,specifier #',name)))))
