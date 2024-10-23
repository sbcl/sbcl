;;;; This file defines all of the internal errors. How they are
;;;; handled is defined in .../code/interr.lisp. How they are signaled
;;;; depends on the machine.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

;;; This is for generating runtime/genesis/constants.h
;;; (The strings are only used in C code and are relatively unimportant)
(defun !c-stringify-internal-error (interr)
  (destructuring-bind (description symbol . nargs) interr
    (declare (ignore nargs))
    (if (stringp description)
        description
        ;; Isn't this rewording a bit pointless? OBJECT-NOT-FOO-ERROR is
        ;; descriptive enough for its use in describe_internal_error()
        (format nil "Object is not of type ~A."
                ;; Given a string "OBJECT-NOT-foo-ERROR", pull out the "foo"
                (subseq (string symbol) 11 (- (length (string symbol)) 6))))))

;; Define SB-C:+BACKEND-INTERNAL-ERRORS+ as a vector of pairs.
;; General errors have the form ("description-of-foo" . foo-ERROR)
;; and type errors are (type-spec . OBJECT-NOT-<type-spec>-ERRROR)
(macrolet
   ((compute-it (general-errors &rest type-errors)
      (let ((list
             (append
              general-errors
              ;; All simple vector specializations
              (map 'list
                   (lambda (saetp)
                     ;; Convert from specifier -> type -> specifier
                     ;; because some specializations have particular names:
                     ;;  (SIMPLE-ARRAY BASE-CHAR (*)) -> SIMPLE-BASE-STRING
                     ;;  (SIMPLE-ARRAY BIT (*)) -> SIMPLE-BIT-VECTOR
                     ;;  (SIMPLE-ARRAY T (*)) -> SIMPLE-VECTOR
                     (list
                      (type-specifier
                       (specifier-type
                        `(simple-array ,(sb-vm:saetp-specifier saetp) (*))))
                      (symbolicate "OBJECT-NOT-"
                                   (sb-vm:saetp-primitive-type-name saetp))))
                   sb-vm:*specialized-array-element-type-properties*)
              (let ((unboxed-vectors
                     (map 'list
                          (lambda (saetp)
                            (type-specifier
                             (specifier-type
                              `(simple-array ,(sb-vm:saetp-specifier saetp) (*)))))
                          (remove-if (lambda (x) (member x '(nil t)))
                                     sb-vm:*specialized-array-element-type-properties*
                                     :key 'sb-vm:saetp-specifier))))
                `(((mod ,(1+ array-dimension-limit))
                   object-not-array-dimension)
                  ;; Union of all unboxed array specializations,
                  ;; for type-checking the argument to VECTOR-SAP
                  ((or ,@unboxed-vectors) object-not-simple-specialized-vector)
                  ;; For type-checking the argument to array blt functions
                  ;; that take either a SAP or an unboxed vector.
                  ;; KLUDGE: fragile, as the order of OR terms has to match
                  ;; exactly the type constraint in the blt functions.
                  ((or ,@unboxed-vectors system-area-pointer)
                   object-not-sap-or-simple-specialized-vector)))
              type-errors)))
        ;; Error number must be of type (unsigned-byte 8).
        (assert (<= (length list) 256))
        `(defconstant-eqx sb-c:+backend-internal-errors+
             ,(map 'vector
                   (lambda (x)
                     (flet ((normalize-type (type)
                              (if (stringp type)
                                  type
                                  (type-specifier (specifier-type type)))))
                       (if (symbolp x)
                           (list* (normalize-type x) (symbolicate "OBJECT-NOT-" x "-ERROR") 1)
                           (list* (normalize-type (car x)) (symbolicate (second x) "-ERROR")
                                  (if (stringp (car x))
                                      (third x)
                                      1)))))
                   list)
           #'equalp))))
 (compute-it
  ;; Keep the following two subsets of internal errors in this order:
  ;;
  ;; (I) all the errors which are not a TYPE-ERROR.
  ;; FIXME: These should either consistently be sentences beginning with
  ;; a capital letter and ending with a period, or consistently not that,
  ;; instead of a random mix of both.
  (("unknown system lossage" unknown 0)
   ("An attempt was made to use an undefined FDEFINITION." undefined-fun 1)
   #+(or arm arm64 x86-64)
   ("An attempt was made to use an undefined alien function" undefined-alien-fun 1)
   ;; Only x86-64 can detect uninitialized C memory
   ;; but there's no harm in defining this error.
   ("Read of uninitialized memory" uninitialized-memory 0)
   ("invalid argument count" invalid-arg-count 1)
   ("invalid argument count" local-invalid-arg-count 2)
   ("bogus argument to VALUES-LIST" bogus-arg-to-values-list 1)
   ("An attempt was made to use an undefined SYMBOL-VALUE." unbound-symbol 1)
   ("attempt to RETURN-FROM a block that no longer exists" invalid-unwind 0)
   ("attempt to THROW to a non-existent tag" unseen-throw-tag 1)
   ("division by zero" division-by-zero 1)
   ("Object is of the wrong type." object-not-type 2)
   ("check-type error" check-type 3)
   ("ECASE failure" ecase-failure 2)
   ("ETYPECASE failure" etypecase-failure 2)
   ("odd number of &KEY arguments" odd-key-args 0)
   ("unknown &KEY argument" unknown-key-arg 1)
   ("invalid array index" invalid-array-index 3)
   ("invalid vector index" invalid-vector-index 2)
   ("uninitialized element" uninitialized-element 2)
   ("A function with declared result type NIL returned." nil-fun-returned 1)
   ("An array with element-type NIL was accessed." nil-array-accessed 1)
   ("Object layout is invalid. (indicates obsolete instance)" layout-invalid 2)
   ("Thread local storage exhausted." tls-exhausted 0)
   ("Stack allocated object overflows stack." stack-allocated-object-overflows-stack 1)
   ("Unreachable code reached" unreachable 0)
   ("Failed aver" failed-aver 1)
   ("Multiplication overflow" mul-overflow 2)
   ("Add overflow" add-sub-overflow 1)
   #+x86-64
   ("Sub overflow" sub-overflow 1)
   ("Add overflow" signed-unsigned-add-overflow 1)
   ("Add overflow" add-overflow2 2)
   ("Sub overflow" sub-overflow2 2)
   ("Mul overflow" mul-overflow2 2)
   ("ASH overflow" ash-overflow2 2)
   ("Negate overflow" negate-overflow 1))
  ;; (II) All the type specifiers X for which there is a unique internal
  ;;      error code corresponding to a primitive object-not-X-error.
  function
  list
  bignum
  ratio
  single-float
  double-float
  #+long-float long-float
  simple-string
  fixnum
  vector
  string
  base-string
  ((vector nil) object-not-vector-nil)
  #+sb-unicode ((vector character) object-not-character-string)
  bit-vector
  array
  number
  rational
  float
  real
  integer
  cons
  symbol
  (system-area-pointer object-not-sap)
  simple-array
  ((signed-byte 32) object-not-signed-byte-32)
  ((signed-byte 64) object-not-signed-byte-64) ; regardless of word size
  unsigned-byte
  ((unsigned-byte 8) object-not-unsigned-byte-8)
  ;; ANSI-STREAM-IN-BUFFER-LENGTH bounds check type
  ((unsigned-byte 9) object-not-unsigned-byte-9)
  ((unsigned-byte 32) object-not-unsigned-byte-32)
  ((unsigned-byte 64) object-not-unsigned-byte-64) ; regardless of word size
  complex
  ((complex rational) object-not-complex-rational)
  ((complex float) object-not-complex-float)
  ((complex single-float) object-not-complex-single-float)
  ((complex double-float) object-not-complex-double-float)
  #+long-float ((complex long-float) object-not-complex-long-float)
  #+sb-simd-pack simd-pack
  #+sb-simd-pack-256 simd-pack-256
  weak-pointer
  instance
  #+sb-unicode
  character
  base-char
  ((and vector (not simple-array)) object-not-complex-vector)

  ;; This "type" is used for checking that a structure slot has a value,
  ;; which it may not if a BOA constructor failed to initialize it.
  ;; The compiler knows to translate the SATISFIES test with a vop,
  ;; and knows to emit the specific error number for this type
  ;; rather than using the strange type name. The INTERNAL-ERROR function
  ;; receives the trap number as if it were a type error,
  ;; but prints a better message than "is not a (not satisfies)"
  ((not (satisfies sb-vm::unbound-marker-p)) slot-not-initialized)

  ;; Now, in approximate order of descending popularity.
  ;; If we exceed 255 error numbers, trailing ones can be deleted arbitrarily.
  sb-c:storage-class ; the single most popular type
  sb-c:tn-ref
  index
  ctype
  sb-impl::buffer
  sb-c::vop
  sb-c::basic-combination
  sb-sys:fd-stream
  layout
  (sb-assem:segment object-not-assem-segment)
  sb-c::cblock
  sb-disassem:disassem-state
  sb-c::ctran
  sb-c::clambda
  sb-c:tn
  ((or function symbol) object-not-callable)
  sb-c:component
  ((or index null) object-not-index-or-null)
  stream
  sb-c::ir2-block
  sb-c::ir2-component
  type-class
  sb-c::lvar
  sb-c::vop-info
  (sb-disassem:instruction object-not-disassembler-instruction)
  ((mod 1114112) object-not-unicode-code-point)
  (sb-c::node object-not-compiler-node)
  sequence
  sb-c::functional
  ((member t nil) object-not-boolean)
  sb-c::lambda-var
  sb-alien::alien-type-class
  lexenv
  ;; simple vector-of-anything is called a "rank-1-array"
  ;; because "simple-vector" means (simple-array t (*))
  ((simple-array * (*)) object-not-simple-rank-1-array)
  hash-table
  sb-c::combination
  numeric-type
  defstruct-description
  sb-format::format-directive
  package
  form-tracking-stream
  ansi-stream
  ((unsigned-byte 16) object-not-unsigned-byte-16)
  ((signed-byte 8) object-not-signed-byte-8)
  ((signed-byte 16) object-not-signed-byte-16)
  ((or index list) object-not-index-or-list)
  condition
  sb-pcl::fast-method-call
  ((or symbol string) object-not-symbol-or-string)
  ((or symbol string character) object-not-string-designator)
  ((and unsigned-byte fixnum) object-not-unsigned-fixnum)
  bit-index))

(defun error-number-or-lose (name)
  (or (position name sb-c:+backend-internal-errors+
                :key #'cadr :test #'eq)
      (error "unknown internal error: ~S" name)))

(defun error-length (error-number)
  (if (array-in-bounds-p sb-c:+backend-internal-errors+ error-number)
      (cddr (svref sb-c:+backend-internal-errors+ error-number))
      0))
