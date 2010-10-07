;;;; signatures of machine-specific functions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;;; internal type predicates

;;; Simple TYPEP uses that don't have any standard predicate are
;;; translated into non-standard unary predicates.
(defknown (fixnump bignump ratiop
           short-float-p single-float-p double-float-p long-float-p
           complex-rational-p complex-float-p complex-single-float-p
           complex-double-float-p #!+long-float complex-long-float-p
           complex-vector-p
           base-char-p %standard-char-p %instancep %other-pointer-p
           base-string-p simple-base-string-p
           #!+sb-unicode character-string-p
           #!+sb-unicode simple-character-string-p
           array-header-p
           sequencep extended-sequence-p
           simple-array-p simple-array-nil-p vector-nil-p
           simple-array-unsigned-byte-2-p
           simple-array-unsigned-byte-4-p simple-array-unsigned-byte-7-p
           simple-array-unsigned-byte-8-p simple-array-unsigned-byte-15-p
           simple-array-unsigned-byte-16-p
           #!+#.(cl:if (cl:= 32 sb!vm:n-word-bits) '(and) '(or))
           simple-array-unsigned-byte-29-p
           simple-array-unsigned-byte-31-p
           simple-array-unsigned-byte-32-p
           #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
           simple-array-unsigned-byte-60-p
           #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
           simple-array-unsigned-byte-63-p
           #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
           simple-array-unsigned-byte-64-p
           simple-array-signed-byte-8-p simple-array-signed-byte-16-p
           #!+#.(cl:if (cl:= 32 sb!vm:n-word-bits) '(and) '(or))
           simple-array-signed-byte-30-p
           simple-array-signed-byte-32-p
           #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
           simple-array-signed-byte-61-p
           #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
           simple-array-signed-byte-64-p
           simple-array-single-float-p simple-array-double-float-p
           #!+long-float simple-array-long-float-p
           simple-array-complex-single-float-p
           simple-array-complex-double-float-p
           #!+long-float simple-array-complex-long-float-p
           system-area-pointer-p realp
           ;; #!+#.(cl:if (cl:= 32 sb!vm:n-word-bits) '(and) '(or))
           unsigned-byte-32-p
           ;; #!+#.(cl:if (cl:= 32 sb!vm:n-word-bits) '(and) '(or))
           signed-byte-32-p
           #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
           unsigned-byte-64-p
           #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
           signed-byte-64-p
           weak-pointer-p code-component-p lra-p
           funcallable-instance-p)
  (t) boolean (movable foldable flushable))
(defknown #.(loop for (name) in *vector-without-complex-typecode-infos*
                  collect name)
  (t) boolean (movable foldable flushable))

;;;; miscellaneous "sub-primitives"

(defknown pointer-hash (t) hash (flushable))

(defknown %sp-string-compare
  (simple-string index index simple-string index index)
  (or index null)
  (foldable flushable))

(defknown %sxhash-simple-string (simple-string) hash
  (foldable flushable))

(defknown %sxhash-simple-substring (simple-string index) hash
  (foldable flushable))

(defknown symbol-hash (symbol) hash
  (flushable movable))

(defknown %set-symbol-hash (symbol hash)
  t (unsafe))

(defknown initialize-vector ((simple-array * (*)) &rest t)
  (simple-array * (*))
  (always-translatable flushable)
  :result-arg 0)

(defknown vector-fill* (t t t t) vector
  (unsafe)
  :result-arg 0)

(defknown vector-length (vector) index (flushable))

(defknown vector-sap ((simple-unboxed-array (*))) system-area-pointer
  (flushable))

(defknown lowtag-of (t) (unsigned-byte #.sb!vm:n-lowtag-bits)
  (flushable movable))
(defknown widetag-of (t) (unsigned-byte #.sb!vm:n-widetag-bits)
  (flushable movable))

(defknown (get-header-data get-closure-length) (t) (unsigned-byte 24)
  (flushable))
(defknown set-header-data (t (unsigned-byte 24)) t
  (unsafe))

(defknown %array-dimension (t index) index
  (flushable))
(defknown %set-array-dimension (t index index) index
  ())
(defknown %array-rank (t) index
  (flushable))

(defknown %make-instance (index) instance
  (flushable))
(defknown %make-structure-instance (defstruct-description list &rest t) instance
  (flushable always-translatable))
(defknown %instance-layout (instance) layout
  (foldable flushable))
(defknown %set-instance-layout (instance layout) layout
  (unsafe))
(defknown %instance-length (instance) index
  (foldable flushable))
(defknown %instance-ref (instance index) t
  (flushable always-translatable))
(defknown %instance-set (instance index t) t
  (unsafe always-translatable))
(defknown %layout-invalid-error (t layout) nil)

(defknown %raw-instance-ref/word (instance index) sb!vm:word
  (flushable always-translatable))
(defknown %raw-instance-set/word (instance index sb!vm:word) sb!vm:word
  (unsafe always-translatable))
(defknown %raw-instance-ref/single (instance index) single-float
  (flushable always-translatable))
(defknown %raw-instance-set/single (instance index single-float) single-float
  (unsafe always-translatable))
(defknown %raw-instance-ref/double (instance index) double-float
  (flushable always-translatable))
(defknown %raw-instance-set/double (instance index double-float) double-float
  (unsafe always-translatable))
(defknown %raw-instance-ref/complex-single (instance index)
  (complex single-float)
  (flushable always-translatable))
(defknown %raw-instance-set/complex-single
    (instance index (complex single-float))
  (complex single-float)
  (unsafe always-translatable))
(defknown %raw-instance-ref/complex-double (instance index)
  (complex double-float)
  (flushable always-translatable))
(defknown %raw-instance-set/complex-double
    (instance index (complex double-float))
  (complex double-float)
  (unsafe always-translatable))

#!+(or x86 x86-64 ppc)
(defknown %raw-instance-atomic-incf/word (instance index sb!vm:word) sb!vm:word
    (unsafe always-translatable))
#!+(or x86 x86-64 ppc)
(defknown %array-atomic-incf/word (t index sb!vm:word) sb!vm:word
  (unsafe always-translatable))

;;; These two are mostly used for bit-bashing operations.
(defknown %vector-raw-bits (t fixnum) sb!vm:word
  (flushable))
(defknown (%set-vector-raw-bits) (t fixnum sb!vm:word) sb!vm:word
  (unsafe))


(defknown allocate-vector ((unsigned-byte 8) index index) (simple-array * (*))
  (flushable movable))

(defknown make-array-header ((unsigned-byte 8) (unsigned-byte 24)) array
  (flushable movable))


(defknown make-weak-pointer (t) weak-pointer
  (flushable))

(defknown %make-complex (real real) complex
  (flushable movable))
(defknown %make-ratio (rational rational) ratio
  (flushable movable))
(defknown make-value-cell (t) t
  (flushable movable))

;;;; threading

#!+(and sb-lutex sb-thread)
(progn
  (defknown sb!vm::%make-lutex () sb!vm::lutex ())
  (defknown sb!vm::lutexp (t) boolean (foldable flushable)))

(defknown (dynamic-space-free-pointer binding-stack-pointer-sap
                                      control-stack-pointer-sap)  ()
  system-area-pointer
  (flushable))

;;;; debugger support

(defknown current-sp () system-area-pointer (movable flushable))
(defknown current-fp () system-area-pointer (movable flushable))
(defknown stack-ref (system-area-pointer index) t (flushable))
(defknown %set-stack-ref (system-area-pointer index t) t (unsafe))
(defknown lra-code-header (t) t (movable flushable))
(defknown fun-code-header (t) t (movable flushable))
(defknown %make-lisp-obj (sb!vm:word) t (movable flushable))
(defknown get-lisp-obj-address (t) sb!vm:word (movable flushable))
(defknown fun-word-offset (function) index (movable flushable))

;;;; 32-bit logical operations

(defknown word-logical-not (sb!vm:word) sb!vm:word
  (foldable flushable movable))

(defknown (word-logical-and word-logical-nand
           word-logical-or word-logical-nor
           word-logical-xor word-logical-eqv
           word-logical-andc1 word-logical-andc2
           word-logical-orc1 word-logical-orc2)
          (sb!vm:word sb!vm:word) sb!vm:word
  (foldable flushable movable))

(defknown (shift-towards-start shift-towards-end) (sb!vm:word fixnum)
  sb!vm:word
  (foldable flushable movable))

;;;; bignum operations

(defknown %allocate-bignum (bignum-index) bignum-type
  (flushable))

(defknown %bignum-length (bignum-type) bignum-index
  (foldable flushable movable))

(defknown %bignum-set-length (bignum-type bignum-index) bignum-type
  (unsafe))

(defknown %bignum-ref (bignum-type bignum-index) bignum-element-type
  (flushable))
#!+(or x86 x86-64)
(defknown %bignum-ref-with-offset (bignum-type bignum-index (signed-byte 24))
  bignum-element-type (flushable always-translatable))

(defknown %bignum-set (bignum-type bignum-index bignum-element-type)
  bignum-element-type
  (unsafe))
#!+(or x86 x86-64)
(defknown %bignum-set-with-offset
  (bignum-type bignum-index (signed-byte 24) bignum-element-type)
  bignum-element-type (unsafe always-translatable))

(defknown %digit-0-or-plusp (bignum-element-type) boolean
  (foldable flushable movable))

(defknown (%add-with-carry %subtract-with-borrow)
          (bignum-element-type bignum-element-type (mod 2))
  (values bignum-element-type (mod 2))
  (foldable flushable movable))

(defknown %multiply-and-add
          (bignum-element-type bignum-element-type bignum-element-type
                               &optional bignum-element-type)
  (values bignum-element-type bignum-element-type)
  (foldable flushable movable))

(defknown %multiply (bignum-element-type bignum-element-type)
  (values bignum-element-type bignum-element-type)
  (foldable flushable movable))

(defknown %lognot (bignum-element-type) bignum-element-type
  (foldable flushable movable))

(defknown (%logand %logior %logxor) (bignum-element-type bignum-element-type)
  bignum-element-type
  (foldable flushable movable))

(defknown %fixnum-to-digit (fixnum) bignum-element-type
  (foldable flushable movable))

(defknown %floor (bignum-element-type bignum-element-type bignum-element-type)
  (values bignum-element-type bignum-element-type)
  (foldable flushable movable))

(defknown %fixnum-digit-with-correct-sign (bignum-element-type)
  (signed-byte #.sb!vm:n-word-bits)
  (foldable flushable movable))

(defknown (%ashl %ashr %digit-logical-shift-right)
          (bignum-element-type (mod #.sb!vm:n-word-bits)) bignum-element-type
  (foldable flushable movable))

;;;; bit-bashing routines

;;; FIXME: there's some ugly duplication between the (INTERN (FORMAT ...))
;;; magic here and the same magic in src/code/bit-bash.lisp.  I don't know
;;; of any good way to clean it up, but it's definitely violating OAOO.
(macrolet ((define-known-copiers ()
            `(progn
              ,@(loop for i = 1 then (* i 2)
                      collect `(defknown ,(intern (format nil "UB~D-BASH-COPY" i)
                                                  (find-package "SB!KERNEL"))
                                ((simple-unboxed-array (*)) index (simple-unboxed-array (*)) index index)
                                (values)
                                ())
                      collect `(defknown ,(intern (format nil "SYSTEM-AREA-UB~D-COPY" i)
                                                  (find-package "SB!KERNEL"))
                                (system-area-pointer index system-area-pointer index index)
                                (values)
                                ())
                      collect `(defknown ,(intern (format nil "COPY-UB~D-TO-SYSTEM-AREA" i)
                                                  (find-package "SB!KERNEL"))
                                ((simple-unboxed-array (*)) index system-area-pointer index index)
                                (values)
                                ())
                      collect `(defknown ,(intern (format nil "COPY-UB~D-FROM-SYSTEM-AREA" i)
                                                  (find-package "SB!KERNEL"))
                                (system-area-pointer index (simple-unboxed-array (*)) index index)
                                (values)
                                ())
                      until (= i sb!vm:n-word-bits)))))
  (define-known-copiers))

;;; (not really a bit-bashing routine, but starting to take over from
;;; bit-bashing routines in byte-sized copies as of sbcl-0.6.12.29:)
(defknown %byte-blt
  ((or (simple-unboxed-array (*)) system-area-pointer) index
   (or (simple-unboxed-array (*)) system-area-pointer) index index)
  (values)
  ())

;;;; code/function/fdefn object manipulation routines

(defknown code-instructions (t) system-area-pointer (flushable movable))
(defknown code-header-ref (t index) t (flushable))
(defknown code-header-set (t index t) t ())

(defknown fun-subtype (function) (unsigned-byte #.sb!vm:n-widetag-bits)
  (flushable))
(defknown ((setf fun-subtype))
          ((unsigned-byte #.sb!vm:n-widetag-bits) function)
  (unsigned-byte #.sb!vm:n-widetag-bits)
  ())

(defknown make-fdefn (t) fdefn (flushable movable))
(defknown fdefn-p (t) boolean (movable foldable flushable))
(defknown fdefn-name (fdefn) t (foldable flushable))
(defknown fdefn-fun (fdefn) (or function null) (flushable))
(defknown (setf fdefn-fun) (function fdefn) t (unsafe))
(defknown fdefn-makunbound (fdefn) t ())

(defknown %simple-fun-self (function) function
  (flushable))
(defknown (setf %simple-fun-self) (function function) function
  (unsafe))

(defknown %closure-fun (function) function
  (flushable))

(defknown %closure-index-ref (function index) t
  (flushable))

(defknown %make-funcallable-instance (index) function
  (unsafe))

(defknown %funcallable-instance-info (function index) t (flushable))
(defknown %set-funcallable-instance-info (function index t) t (unsafe))

;;;; mutator accessors

(defknown mutator-self () system-area-pointer (flushable movable))

(defknown %data-vector-and-index (array index)
                                 (values (simple-array * (*)) index)
                                 (foldable flushable))
