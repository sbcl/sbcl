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

(in-package "SB!KERNEL")

(defun error-number-or-lose (name)
  (or (position name sb!c:*backend-internal-errors* :key #'cdr)
      (error "unknown internal error: ~S" name)))

;; These used to be stored as  (error-symbol . string) but now they're
;; (error-string|type-spec . error-symbol) for two reasons:
;; 1. I dislike seeing (object-not-unsigned-byte-8 unsigned-byte 8)
;;    instead of (object-not-unsigned-byte . (unsigned-byte 8))
;; 2. It's conceptually a mapping FROM a type-spec TO an error-symbol.
;;    Envisioned that way, the alist keys want to be in the car.
;;    The fact that a few "keys" are strings is largely irrelevant
;;    and I'd probably like to discard them after genesis.
;;
(defun !c-stringify-internal-error (interr)
  (destructuring-bind (description . symbol) interr
    (if (stringp description)
        description
        (format nil "Object is not of type ~A."
                ;; KLUDGE - the C descriptions of these type is too messy
                (case symbol
                  (object-not-sap-or-simple-specialized-vector-error
                   "SAP-OR-SIMPLE-SPECIALIZED-VECTOR")
                  (object-not-simple-specialized-vector-error
                   "SIMPLE-SPECIALIZED-VECTOR")
                  (t
                   description))))))

(setf sb!c:*backend-internal-errors*
(map 'vector (lambda (x) (cons (cadr x) (symbolicate (car x) "-ERROR")))
`((unknown "unknown system lossage")
  (object-not-function function)
  (object-not-list list)
  (object-not-bignum bignum)
  (object-not-ratio ratio)
  (object-not-single-float single-float)
  (object-not-double-float double-float)
  #!+long-float (object-not-long-float long-float)
  (object-not-simple-string simple-string)
  (object-not-fixnum fixnum)
  (object-not-vector vector)
  (object-not-string string)
  (object-not-base-string base-string)
  (object-not-vector-nil (vector nil))
  #!+sb-unicode
  (object-not-character-string (vector character))
  (object-not-bit-vector bit-vector)
  (object-not-array array)
  (object-not-number number)
  (object-not-rational rational)
  (object-not-float float)
  (object-not-real real)
  (object-not-integer integer)
  (object-not-cons cons)
  (object-not-symbol symbol)
  (undefined-fun
   ;; FIXME: Isn't this used for calls to unbound (SETF FOO) too? If so, revise
   ;; the name.
   "An attempt was made to use an undefined FDEFINITION.")
  #!+(or arm x86-64)
  (undefined-alien-fun
   "An attempt was made to use an undefined alien function")
  (invalid-arg-count "invalid argument count")
  (bogus-arg-to-values-list "bogus argument to VALUES-LIST")
  (unbound-symbol "An attempt was made to use an undefined SYMBOL-VALUE.")
  (object-not-sap system-area-pointer)
  (invalid-unwind "attempt to RETURN-FROM a block that no longer exists")
  (unseen-throw-tag "attempt to THROW to a non-existent tag")
  (division-by-zero "division by zero")
  (object-not-type "Object is of the wrong type.")
  (odd-key-args "odd number of &KEY arguments")
  (unknown-key-arg "unknown &KEY argument")
  (invalid-array-index"invalid array index")
  (wrong-number-of-indices "wrong number of indices")
  (object-not-simple-array simple-array)
  (object-not-signed-byte-32 (signed-byte 32))
  (object-not-signed-byte-64 (signed-byte 64)) ; regardless of word size
  (object-not-unsigned-byte unsigned-byte)
  (object-not-unsigned-byte-8 (unsigned-byte 8))
  ;; not sure where this is used, but it ranks high on the popularity poll
  (object-not-unsigned-byte-9 (unsigned-byte 9))
  (object-not-unsigned-byte-32 (unsigned-byte 32))
  (object-not-unsigned-byte-64 (unsigned-byte 64)) ; regardless of word size
  (object-not-complex complex)
  (object-not-complex-rational (complex rational))
  (object-not-complex-float (complex float))
  (object-not-complex-single-float (complex single-float))
  (object-not-complex-double-float (complex double-float))
  #!+long-float (object-not-complex-long-float (complex long-float))
  #!+sb-simd-pack (object-not-simd-pack simd-pack)
  (object-not-weak-pointer weak-pointer)
  (object-not-instance instance)
  (object-not-character character)
  (nil-fun-returned "A function with declared result type NIL returned.")
  (nil-array-accessed "An array with element-type NIL was accessed.")
  (layout-invalid "Object layout is invalid. (indicates obsolete instance)")
  (object-not-complex-vector (and vector (not simple-array)))
  (tls-exhausted "Thread local storage exhausted.")

  ;; now, in descending order by popularity.
  ;; The reasoning is that if we exceed 255 error numbers we can delete
  ;; harmlessly from the end. Error numbers must not be more than one byte.
  (object-not-storage-class sb!c:sc) ; the single most popular type
  (object-not-array-dimension (integer 0 ,sb!xc:array-dimension-limit))
  (object-not-index sb!int:index) ; the second-most popular type for checkgen
  (object-not-tn-ref sb!c:tn-ref)
  (object-not-ctype sb!kernel:ctype)
  (object-not-buffer sb!impl::buffer)
  (object-not-vop sb!c::vop)
  (object-not-basic-combination sb!c::basic-combination)
  (object-not-fd-stream sb!sys:fd-stream)
  (object-not-layout layout)
  (object-not-assem-segment sb!assem:segment)
  (object-not-cblock sb!c::cblock)
  (object-not-disassem-state sb!disassem:disassem-state)
  (object-not-ctran sb!c::ctran)
  (object-not-clambda sb!c::clambda)
  (object-not-tn sb!c:tn)
  (object-not-callable (or function symbol))
  (object-not-component sb!c:component)
  (object-not-index-or-null (or index null))
  (object-not-stream stream)
  (object-not-ir2-block sb!c::ir2-block)
  (object-not-lvar sb!c::lvar)
  (object-not-vop-info sb!c::vop-info)
  (object-not-disassembler-instruction sb!disassem:instruction)
  (object-not-unicode-code-point (mod 1114112))
  (object-not-compiler-node sb!c::node)
  (object-not-sequence sequence)
  (object-not-functional sb!c::functional)
  (object-not-boolean (member t nil)) ; also have (member nil t). Why?
  (object-not-lambda-var sb!c::lambda-var)
  (object-not-alien-type-class sb!alien::alien-type-class)
  (object-not-lexenv sb!kernel:lexenv)
  (object-not-hash-table hash-table)
  (object-not-package package)
  ;; Most of these array types are unimportant to have as primitive traps.
  ;; It can't hurt to keep them all unless the entire list is too long.
  ;; [And it would be difficult not to keep them all
  ;; because compiler/generic/late-type-vops refers to the error number names]
  ,@(map 'list
         (lambda (saetp)
           (list
            (symbolicate "OBJECT-NOT-" (sb!vm:saetp-primitive-type-name saetp))
            ;; back and forth from specifier to type to specifier because:
            ;;  (SIMPLE-ARRAY BASE-CHAR (*)) -> SIMPLE-BASE-STRING
            ;;  (SIMPLE-ARRAY BIT (*)) -> SIMPLE-BIT-VECTOR
            ;;  (SIMPLE-ARRAY T (*)) -> SIMPLE-VECTOR
            (type-specifier
             (specifier-type
              `(simple-array ,(sb!vm:saetp-specifier saetp) (*))))))
         sb!vm:*specialized-array-element-type-properties*)
  ,@(let ((array-types
           (remove
            'simple-vector
            (map 'list (lambda (saetp)
                         (type-specifier
                          (specifier-type
                           `(simple-array ,(sb!vm:saetp-specifier saetp) (*)))))
                 sb!vm:*specialized-array-element-type-properties*))))
    ;; An error number for SIMPLE-SPECIALIZED-VECTOR wins in multiple ways.
    ;; Every safe use of (VECTOR-SAP x) that had to type-check X also dumped
    ;; a huge s-expression solely for the ERROR call. The conses alone are
    ;; nearly 2KB, not to mention all the fasl ops to construct the monster.
    `((object-not-simple-specialized-vector (or ,@array-types))
      ;; KLUDGE: Slightly fragile as the order of OR terms makes a difference
      (object-not-sap-or-simple-specialized-vector
       (or ,@array-types system-area-pointer))))
  ;; And finally, simple vector-of-anything is called a "rank-1-array"
  ;; because "simple-vector" means (simple-array t (*))
  (object-not-simple-rank-1-array (simple-array * (*))))))
