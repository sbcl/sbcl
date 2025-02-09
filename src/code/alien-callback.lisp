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
          "SB-ALIEN"))

;;;; ALIEN CALLBACKS
;;;;
;;;; See "Foreign Linkage / Callbacks" in the SBCL Internals manual.

(define-load-time-global *alien-callback-saps*
  (make-array 32 :fill-pointer 0 :adjustable t
                 :initial-element nil)
  "An array of all callback SAPs indexed by their order of creation.")

(defun alien-callback-index (alien)
  (position (alien-sap alien) *alien-callback-saps* :test #'sap=))

(define-load-time-global *alien-callbacks* (make-hash-table :test #'eq)
  "Cache of existing callback SAPs, indexed by FUNCTION. Used for
memoization: we don't create new callbacks if one pointing to the same
function already exists.")

(defun invalid-alien-callback (&rest arguments)
  (declare (ignore arguments))
  (error "Invalid alien callback called."))

(define-load-time-global *alien-callback-functions*
    (make-array 32 :fill-pointer 0 :adjustable t
                :initial-element #'invalid-alien-callback)
  "Lisp function store: assembler wrappers contain indexes to this, and
ENTER-ALIEN-CALLBACK pulls the corresponding function out and calls it.")

(defun %alien-callback-sap (result-type argument-types function
                            &optional call-type)
  (declare #-x86 (ignore call-type))
  (ensure-gethash
   function *alien-callbacks*
   (let* ((index (fill-pointer *alien-callback-functions*))
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
            #+x86
            (if (eq call-type :stdcall)
                (ceiling
                 (apply #'+
                        (mapcar 'alien-type-word-aligned-bits
                                argument-types))
                 8)
                0))))
     (vector-push-extend function *alien-callback-functions*)
     ;; Assembler-wrapper is static, so sap-taking is safe.
     (let ((sap (vector-sap assembler-wrapper)))
       (vector-push-extend sap *alien-callback-saps*)
       sap))))

(defun alien-callback-lambda-expression (specifier arguments body result-type env)
  (let ((argument-names arguments)
        (argument-specs (cddr specifier)))
    `(lambda (args-pointer result-pointer)
       (declare (optimize speed))
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
                 collect `(,(pop argument-names) ,accessor-form)
                 do (incf offset (+ (alien-callback-argument-bytes spec env)
                                    (or alignment 0))))
           ,(flet ((store (spec real-type)
                     (if spec
                         `(setf (deref (sap-alien res-sap (* ,spec)))
                                ,(if real-type
                                     `(the ,real-type
                                           (progn
                                             ,@body))
                                     `(progn ,@body)))
                         `(progn ,@body))))
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
  (declare (optimize (safety 0) speed))
  (funcall (truly-the function
                      (svref (sb-kernel:%array-data *alien-callback-functions*)
                             index))
           arguments
           return))

;;;; interface (not public, yet) for alien callbacks

(defun alien-callback-p (alien)
  "Returns true if the alien is associated with a lisp-side callback,
and a secondary return value of true if the callback is still valid."
  (let ((index (alien-callback-index alien)))
    (when index
      (values t (not (eq (aref *alien-callback-functions* index)
                         #'invalid-alien-callback))))))

(defun alien-callback-function (alien)
  "Returns the Lisp function associated with the callback."
  (let ((index (alien-callback-index alien)))
    (when index
      (aref *alien-callback-functions* index))))

(defun (setf alien-callback-function) (function alien)
  "Changes the Lisp function designated by the callback."
  (let ((index (alien-callback-index alien)))
    (unless index
      (error "Not an alien callback: ~S" alien))
    ;; sap cache
    (let ((function (aref *alien-callback-functions* index)))
      (remhash function *alien-callbacks*)
      (setf (gethash function *alien-callbacks*) (alien-sap alien)))
    (setf (aref *alien-callback-functions* index) function)
    function))

(defun invalidate-alien-callback (alien)
  "Invalidates the callback designated by the alien, if any, allowing the
associated lisp function to be GC'd, and causing further calls to the same
callback to signal an error."
  (let ((index (alien-callback-index alien)))
    (when index
      (let ((function (aref *alien-callback-functions* index)))
        (unless (eq function #'invalid-alien-callback)
          ;; sap cache
          (remhash function *alien-callbacks*)
          (setf (aref *alien-callback-functions* index)
                #'invalid-alien-callback)
          t)))))

;;; FIXME: This call assembles a new callback for every closure,
;;; which sucks hugely. ...not that I can think of an obvious
;;; solution. Possibly maybe we could write a generalized closure
;;; callback analogous to closure_tramp, and share the actual wrapper?
;;;
;;; For lambdas that result in simple-funs we get the callback from
;;; the cache on subsequent calls.
(defmacro alien-lambda (result-type typed-lambda-list &body body
                                                      &environment env)
  (multiple-value-bind (specifier lambda-list)
      (parse-callback-specification result-type typed-lambda-list)
    (multiple-value-bind (result-type argument-types call-type)
        (parse-alien-ftype specifier env)
      (let ((lambda-expression
              (alien-callback-lambda-expression
               specifier lambda-list
               body result-type env)))
        `(%sap-alien
          (%alien-callback-sap ',result-type ',argument-types
                               ,lambda-expression
                               ,call-type)
          ',(parse-alien-type specifier env))))))

;;;; Alien callables

(define-load-time-global *alien-callables* (make-hash-table :test #'eq)
    "Map from Lisp symbols to the alien callable functions they name.")

(defmacro define-alien-callable (name result-type typed-lambda-list &body body)
  "Define an alien callable function in the alien callable namespace with result
type RESULT-TYPE and with lambda list specifying the alien types of the
arguments."
  (multiple-value-bind (lisp-name alien-name)
      (pick-lisp-and-alien-names name)
    (declare (ignore alien-name))
    `(progn
       (invalidate-alien-callable ',lisp-name)
       (setf (gethash ',lisp-name *alien-callables*)
             (alien-lambda ,result-type ,typed-lambda-list ,@body)))))

(defun alien-callable-function (name)
  "Return the alien callable function associated with NAME."
  (gethash name *alien-callables*))

(defun invalidate-alien-callable (name)
  "Invalidates the callable designated by the alien, if any, allowing the
associated lisp function to be GC'd, and causing further calls to the same
callable to signal an error."
  (multiple-value-bind (lisp-name alien-name)
      (pick-lisp-and-alien-names name)
    (declare (ignore alien-name))
    (let ((alien (alien-callable-function lisp-name)))
      (when alien
        (invalidate-alien-callback alien)))
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
