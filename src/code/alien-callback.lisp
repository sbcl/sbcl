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

(in-package "SB-ALIEN")

;;; These are supposed to be external in SB-ALIEN, but the export gets
;;; lost (as this is now a warm-loaded file).
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export (intern "DEFINE-ALIEN-CALLABLE" "SB-ALIEN")
          "SB-ALIEN")
  (export (intern "ALIEN-CALLABLE-FUNCTION" "SB-ALIEN")
          "SB-ALIEN")
  (export (intern "ALIEN-CALLABLE" "SB-ALIEN")
          "SB-ALIEN")
  (export (intern "WITH-ALIEN-CALLABLE" "SB-ALIEN")
          "SB-ALIEN"))

;;;; ALIEN CALLBACKS
;;;;
;;;; See "Foreign Linkage / Callbacks" in the SBCL Internals manual.

(define-load-time-global *alien-callbacks*
  (make-array 32 :fill-pointer 0 :adjustable t
                 :initial-element nil)
  "An array of all callback aliens indexed by their order of creation.")

(defun alien-callback-index (alien)
  (position alien *alien-callbacks* :test #'eq))

(defun invalid-alien-callback (&rest arguments)
  (declare (ignore arguments))
  (error "Invalid alien callback called."))

(define-load-time-global *alien-callback-functions*
    (make-array 32 :fill-pointer 0 :adjustable t
                :initial-element #'invalid-alien-callback)
  "An array of Lisp callback functions associated to aliens of the same index.")

(define-load-time-global *free-alien-callback-indices*
    (make-hash-table :test #'equal)
  "A cache of free alien callback indices keyed by type specifier.")

(defun adjustable-sap (sap)
  #+x86-64
  (with-pinned-objects (sap)
    (setf (sap-ref-32 (int-sap (sb-kernel:get-lisp-obj-address sap))
                      (- 4 sb-vm:other-pointer-lowtag))
          (truly-the (unsigned-byte 32)
                     (sap- sap (int-sap sb-vm:static-space-start)))))
  sap)

(defun %alien-callback-alien (alien-fun-type specifier function)
  (let ((free-indices (gethash specifier *free-alien-callback-indices*)))
    (cond
      (free-indices
       (let ((index (pop (gethash specifier *free-alien-callback-indices*))))
         (setf (aref *alien-callback-functions* index) function)
         (aref *alien-callbacks* index)))
      (t
       (let* ((index (fill-pointer *alien-callback-functions*))
              (argument-types (alien-fun-type-arg-types alien-fun-type))
              ;; Aside from the INDEX this is known at
              ;; compile-time, which could be utilized by
              ;; having the two-stage assembler tramp &
              ;; wrapper mentioned in [1] above: only the
              ;; per-function tramp would need assembler at
              ;; runtime. Possibly we could even pregenerate
              ;; the code and just patch the index in later.
              (assembler-wrapper
                (alien-callback-assembler-wrapper
                 index
                 (alien-fun-type-result-type alien-fun-type)
                 argument-types
                 #+x86
                 (if (eq (alien-fun-type-convention alien-fun-type) :stdcall)
                     (ceiling
                      (apply #'+
                             (mapcar 'alien-type-word-aligned-bits
                                     argument-types))
                      8)
                     0))))
         (vector-push-extend function *alien-callback-functions*)
         ;; Assembler-wrapper is static, so sap-taking is safe.
         (let ((alien (%sap-alien (adjustable-sap (vector-sap assembler-wrapper))
                                  alien-fun-type)))
           (vector-push-extend alien *alien-callbacks*)
           alien))))))

(defun alien-type-word-aligned-bits (type)
  (align-offset (alien-type-bits type) sb-vm:n-word-bits))

(defun alien-callback-argument-bytes (spec env)
  (let ((type (parse-alien-type spec env)))
    (if (or (alien-integer-type-p type)
            (alien-float-type-p type)
            (alien-pointer-type-p type)
            (alien-system-area-pointer-type-p type))
        (ceiling (alien-type-word-aligned-bits type) sb-vm:n-byte-bits)
        (error "Unsupported callback argument type: ~A" type))))

(defun enter-alien-callback (index arguments return)
  (declare (optimize speed (safety 0)))
  (funcall (truly-the function
                      (svref (sb-kernel:%array-data *alien-callback-functions*)
                             index))
           arguments
           return))

(defun alien-callback-p (alien)
  "Returns true if the alien is associated with a Lisp-side callback."
  (alien-callback-index alien))

;;; Invalidate the alien function ALIEN allocated by Lisp and allow
;;; its entry point to be reused by other callables of the same type.
(defun free-alien-callable (alien specifier)
  (let ((index (alien-callback-index alien)))
    (unless index
      (error "Trying to free a callable alien ~A not allocated by Lisp." alien))
    (when (member index (gethash specifier *free-alien-callback-indices*))
      (error "Double free of callable alien ~A." alien))
    (setf (aref *alien-callback-functions* index) #'invalid-alien-callback)
    (push index
          (gethash specifier *free-alien-callback-indices*))))

(defun parse-alien-fun-type (result-type typed-lambda-list env)
  (parse-alien-type
   `(function ,result-type ,@(mapcar #'second typed-lambda-list))
   env))

;;; Needed for old versions of CFFI.
(defmacro alien-lambda (result-type typed-lambda-list &body body
                                                      &environment env)
  (let ((fun-type (parse-alien-fun-type result-type typed-lambda-list env)))
    `(%alien-callback-alien ',fun-type
                            ',(unparse-alien-type fun-type)
                            (alien-lambda2 ,result-type ,typed-lambda-list
                              ,@body))))

(defmacro alien-lambda2 (result-type typed-lambda-list &body body
                                                       &environment env)
  (let* ((result-type
           (alien-fun-type-result-type
            (parse-alien-fun-type result-type typed-lambda-list env)))
         (arguments (mapcar #'first typed-lambda-list))
         (argument-specs (mapcar #'second typed-lambda-list)))
    (multiple-value-bind (body decls doc) (parse-body body t)
      `(lambda (args-pointer result-pointer)
         ,@(and doc (list doc))
         (declare (optimize speed (sb-c::verify-arg-count 0)))
         (let ((args-sap (descriptor-sap args-pointer))
               (res-sap (descriptor-sap result-pointer)))
           (declare (ignorable args-sap res-sap))
           (let
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
                  collect `(,(pop arguments) ,accessor-form)
                  do (incf offset (+ (alien-callback-argument-bytes spec env)
                                     (or alignment 0))))
             ,(flet ((store (spec real-type)
                       (if spec
                           `(setf (deref (sap-alien res-sap (* ,spec)))
                                  ,(if real-type
                                       `(the ,real-type
                                             (locally ,@decls
                                               ,@body))
                                       `(locally ,@decls
                                          ,@body)))
                           `(locally ,@decls ,@body))))
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
         0))))

;;;; Alien callables

(define-load-time-global *alien-callables* (make-hash-table :test #'eq)
    "Map from Lisp symbols to the alien callable functions they name.")

(defmethod documentation ((object alien-value) (doc-type (eql 'alien-callable)))
  (let ((index (alien-callback-index object)))
    (when index
      (documentation (aref *alien-callback-functions* index) 'function))))

(defmethod (setf documentation) (new-value (object alien-value)
                                 (doc-type (eql 'alien-callable)))
  (let ((index (alien-callback-index object)))
    (when index
      (setf (documentation (aref *alien-callback-functions* index)
                           'function)
            new-value))))

(defmethod documentation ((name symbol) (doc-type (eql 'alien-callable)))
  (documentation (alien-callable-function name) doc-type))

(defmethod (setf documentation) (new-value (name symbol)
                                 (doc-type (eql 'alien-callable)))
  (setf (documentation (alien-callable-function name) doc-type)
        new-value))

(defun %define-alien-callable (name function alien-fun-type specifier)
  (declare (type symbol name)
           (type function function))
  (flet ((register-new-callable ()
           (setf (gethash name *alien-callables*)
                 (%alien-callback-alien alien-fun-type specifier function))))
    (let ((existing (alien-callable-function name)))
      (if existing
          (let ((old-type (alien-value-type existing)))
            ;; TODO: Use a weaker predicate like the CMU CL
            ;; COMPATIBLE-FUNCTION-TYPES-P.
            (cond ((alien-type-= old-type alien-fun-type)
                   (let ((index (alien-callback-index existing)))
                     (aver index)
                     (setf (aref *alien-callback-functions* index)
                           function)))
                  (t
                   (let ((e (format nil "~
Attempt to redefine alien callable with incompatible return type.
   Old type was: ~A
    New type is: ~A" old-type alien-fun-type))
                         (c (format nil "~
Create new alien callable (old alien callable gets freed)."))
                         (existing-specifier (unparse-alien-type
                                              (alien-value-type existing))))
                     (cerror c e)
                     (free-alien-callable existing existing-specifier)
                     (register-new-callable)))))
          (register-new-callable)))))

(defmacro define-alien-callable (name result-type typed-lambda-list
                                 &body body
                                 &environment env)
  "(define-alien-callable NAME RESULT-TYPE {(ARG-NAME ARG-TYPE)}*
     {doc-string} {decls}* {FORM}*)

Define an alien function which can be called by alien code. The alien
function returned by (alien-callable-function NAME) expects alien
arguments of the specified ARG-TYPEs and returns an alien of type
RESULT-TYPE.

If (alien-callable-function NAME) already exists, its value is not
changed (though it is arranged that an updated version of the Lisp
callable function will be called, provided that the new type and the
existing type are compatible). This feature allows for incremental
redefinition of callable functions."
  (multiple-value-bind (lisp-name alien-name)
      (pick-lisp-and-alien-names name)
    (declare (ignore alien-name))
    (let ((fun-type (parse-alien-fun-type result-type typed-lambda-list env)))
      `(%define-alien-callable
        ',lisp-name
        (alien-lambda2 ,result-type ,typed-lambda-list
          ,@body)
        ',fun-type
        ',(unparse-alien-type fun-type)))))

(defmacro with-alien-callable (definitions
                               &body body
                               &environment env)
    "Establish some local alien functions. Each DEFINITION is of the form:
     NAME RESULT-TYPE {(ARG-NAME ARG-TYPE)}*
       {doc-string} {decls}* {FORM}*

     The resulting alien callable value has dynamic extent."
  (collect ((bindings)
            (declarations)
            (cleanup))
    (dolist (definition definitions)
      (destructuring-bind (name result-type typed-lambda-list &body body)
          definition
        (let* ((fun-type (parse-alien-fun-type result-type
                                               typed-lambda-list
                                               env))
               (specifier (unparse-alien-type fun-type)))
          (bindings `(,name (%alien-callback-alien
                             ',fun-type
                             ',specifier
                             (alien-lambda2 ,result-type ,typed-lambda-list
                               ,@body))))
          (declarations `(declare ((alien ,fun-type) ,name)))
          (cleanup `(free-alien-callable ,name ',specifier)))))
    `(let ,(bindings)
       ,@(declarations)
       (unwind-protect (progn ,@body)
         ,@(cleanup)))))

(defun alien-callable-function (name)
  "Return the alien callable function associated with NAME."
  (gethash name *alien-callables*))

(defun make-alien-callable-function-unbound (name)
  "Make NAME in the alien callable namespace unbound and frees the
callable designated by the alien."
  (multiple-value-bind (lisp-name alien-name)
      (pick-lisp-and-alien-names name)
    (declare (ignore alien-name))
    (let ((alien (alien-callable-function lisp-name)))
      (when alien
        (free-alien-callable alien (unparse-alien-type (alien-value-type alien)))))
    (remhash lisp-name *alien-callables*)))

(defun initialize-alien-callable-symbol (name)
  "Initialize the alien symbol named by NAME with its alien callable
function value."
  (multiple-value-bind (lisp-name alien-name)
      (pick-lisp-and-alien-names name)
    (setf (%alien-value (foreign-symbol-sap alien-name t)
                        0
                        (make-alien-pointer-type))
          (cast (alien-callable-function lisp-name) (* t)))))

(in-package "SB-THREAD")
#+sb-thread
(defun enter-foreign-callback (index return arguments)
  (let ((thread (init-thread-local-storage (make-foreign-thread))))
    (dx-let ((startup-info (vector nil ; trampoline is n/a
                                   nil ; cell in *STARTING-THREADS* is n/a
                                   #'sb-alien::enter-alien-callback
                                   (list index return arguments)
                                   nil nil))) ; sigmask + fpu state bits
      (copy-primitive-thread-fields thread)
      (setf (thread-startup-info thread) startup-info)
      (update-all-threads (thread-primitive-thread thread) thread)
      (run))))
