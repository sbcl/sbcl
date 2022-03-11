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

;;; ALIEN-CALLBACK is supposed to be external in SB-ALIEN-INTERNALS,
;;; but the export gets lost (as this is now a warm-loaded file), and
;;; then 'chill' gets a conflict with SB-ALIEN over it.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export (intern "ALIEN-CALLBACK" "SB-ALIEN-INTERNALS")
          "SB-ALIEN-INTERNALS")
  (export (intern "DEFINE-ALIEN-CALLABLE" "SB-ALIEN")
          "SB-ALIEN")
  (export (intern "ALIEN-CALLABLE-FUNCTION" "SB-ALIEN")
          "SB-ALIEN"))

;;;; ALIEN CALLBACKS
;;;;
;;;; See "Foreign Linkage / Callbacks" in the SBCL Internals manual.

(defvar *alien-callback-info* nil
  "Maps SAPs to corresponding CALLBACK-INFO structures: contains all the
information we need to manipulate callbacks after their creation. Used for
changing the lisp-side function they point to, invalidation, etc.")

(defstruct (callback-info
            (:predicate nil)
            (:copier nil))
  (specifier nil :read-only t)
  function ; NULL if invalid
  (wrapper nil :read-only t)
  index)

(defun callback-info-key (info)
  (cons (callback-info-specifier info) (callback-info-function info)))

(defun alien-callback-info (alien)
  (cdr (assoc (alien-sap alien) *alien-callback-info* :test #'sap=)))

(define-load-time-global *alien-callbacks* (make-hash-table :test #'equal)
  "Cache of existing callback SAPs, indexed with (SPECIFER . FUNCTION). Used for
memoization: we don't create new callbacks if one pointing to the correct
function with the same specifier already exists.")

(define-load-time-global *alien-callback-wrappers* (make-hash-table :test #'equal)
  "Cache of existing lisp wrappers, indexed with SPECIFER. Used for memoization:
we don't create new wrappers if one for the same specifier already exists.")

(defun invalid-alien-callback (&rest arguments)
  (declare (ignore arguments))
  (error "Invalid alien callback called."))

(define-load-time-global *alien-callback-trampolines*
    (make-array 32 :fill-pointer 0 :adjustable t
                :initial-element #'invalid-alien-callback)
  "Lisp trampoline store: assembler wrappers contain indexes to this, and
ENTER-ALIEN-CALLBACK pulls the corresponding trampoline out and calls it.")

(defun %alien-callback-sap (specifier result-type argument-types function wrapper
                            &optional call-type)
  (declare #-x86 (ignore call-type))
  (ensure-gethash
   (list specifier function) *alien-callbacks*
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
            #+x86
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
       sap))))

(defun alien-callback-lisp-trampoline (wrapper function)
  (declare (function wrapper) (optimize speed))
  (lambda (args-pointer result-pointer)
    (funcall wrapper args-pointer result-pointer function)))

(defun alien-callback-lisp-wrapper-lambda (specifier result-type argument-types env)
  (let* ((arguments (make-gensym-list (length argument-types)))
         (argument-names arguments)
         (argument-specs (cddr specifier)))
    `(lambda (args-pointer result-pointer function)
       ;; KLUDGE: the SAP shouldn't be consed but they are, don't
       ;; bother anyone about that sad fact
       (declare (muffle-conditions compiler-note)
                (optimize speed))
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

(defun enter-alien-callback (index return arguments)
  (funcall (truly-the function
                      (svref (sb-kernel:%array-data *alien-callback-trampolines*)
                             index))
           return
           arguments))

;;;; interface (not public, yet) for alien callbacks

(defmacro alien-callback (specifier function &environment env)
  "Returns an alien-value of alien ftype SPECIFIER, that can be passed to
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
                           (ensure-gethash
                            ',specifier *alien-callback-wrappers*
                            ,(alien-callback-lisp-wrapper-lambda
                              specifier result-type argument-types env))
                           ,call-type)
      ',(parse-alien-type specifier env))))

(defun alien-callback-p (alien)
  "Returns true if the alien is associated with a lisp-side callback,
and a secondary return value of true if the callback is still valid."
  (let ((info (alien-callback-info alien)))
    (when info
      (values t (and (callback-info-function info) t)))))

(defun alien-callback-function (alien)
  "Returns the lisp function designator associated with the callback."
  (let ((info (alien-callback-info alien)))
    (when info
      (callback-info-function info))))

(defun (setf alien-callback-function) (function alien)
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
  "Invalidates the callback designated by the alien, if any, allowing the
associated lisp function to be GC'd, and causing further calls to the same
callback to signal an error."
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
    #+pauseless-threadstart
    (dx-let ((startup-info (vector nil ; trampoline is n/a
                                   nil ; cell in *STARTING-THREADS* is n/a
                                   #'sb-alien::enter-alien-callback
                                   (list index return arguments)
                                   nil nil))) ; sigmask + fpu state bits
      (copy-primitive-thread-fields thread)
      (setf (thread-startup-info thread) startup-info)
      (update-all-threads (thread-primitive-thread thread) thread)
      (run))
    #-pauseless-threadstart
    (dx-let ((args (list index return arguments)))
      (run thread nil #'sb-alien::enter-alien-callback args))))
