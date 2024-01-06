(in-package #:cl-user)

(defpackage #:sb-simd-internals
  (:use #:common-lisp)
  (:export
   ;; constants.lisp
   #:most-positive-f32
   #:most-negative-f32
   #:most-positive-f64
   #:most-negative-f64
   #:most-positive-u1
   #:most-positive-u2
   #:most-positive-u4
   #:most-positive-u8
   #:most-positive-u16
   #:most-positive-u32
   #:most-positive-u64
   #:most-positive-s8
   #:most-negative-s8
   #:most-positive-s16
   #:most-negative-s16
   #:most-positive-s32
   #:most-negative-s32
   #:most-positive-s64
   #:most-negative-s64
   #:+u8-true+
   #:+u16-true+
   #:+u32-true+
   #:+u64-true+
   #:+u8-false+
   #:+u16-false+
   #:+u32-false+
   #:+u64-false+
   #:+s8-true+
   #:+s16-true+
   #:+s32-true+
   #:+s64-true+
   #:+s8-false+
   #:+s16-false+
   #:+s32-false+
   #:+s64-false+
   #:+f32-true+
   #:+f64-true+
   #:+f32-false+
   #:+f64-false+
   ;; utilities.lisp
   #:type-specifier
   #:non-nil-symbol
   #:function-name
   #:index+
   #:index-
   #:index*
   #:ensure-package
   #:mksym
   #:prefixed-symbols
   #:touch
   #:required-argument
   #:macroexpand-all
   #:ensure-list
   #:lambda-expression-p
   #:parse-function-name
   #:define-inline
   #:define-notinline
   ;; printable.lisp
   #:printable
   #:printable-slot-plist
   ;; instruction-set.lisp
   #:*instruction-sets*
   #:instruction-set
   #:instruction-set-p
   #:instruction-set-name
   #:instruction-set-package
   #:instruction-set-includes
   #:instruction-set-available-p
   #:find-instruction-set
   #:included-instruction-sets
   #:define-instruction-set
   #:instruction-set-vectorizers
   ;; record.lisp
   #:record
   #:record-p
   #:record-name
   #:record-instruction-set
   #:value-record
   #:value-record-p
   #:value-record-name
   #:value-record-instruction-set
   #:value-record-type
   #:value-record-primitive-type
   #:value-record-bits
   #:value-record-scs
   #:value-record-simd-width
   #:value-record-cast-record
   #:find-value-record
   #:filter-value-records
   #:simd-record
   #:simd-record-p
   #:scalar-record-p
   #:simd-record-name
   #:simd-record-instruction-set
   #:simd-record-type
   #:simd-record-primitive-type
   #:simd-record-bits
   #:simd-record-scs
   #:simd-record-scalar-record
   #:function-record
   #:function-record-p
   #:function-record-name
   #:function-record-instruction-set
   #:function-record-result-records
   #:function-record-result-record
   #:function-record-required-argument-records
   #:function-record-rest-argument-record
   #:function-record-simd-width
   #:function-record-scalar-variant
   #:find-function-record
   #:filter-function-records
   #:filter-available-function-records
   #:scalar-function-record-p
   #:simd-function-record-p
   #:aref-record
   #:aref-record-p
   #:setf-aref-record
   #:setf-aref-record-p
   #:row-major-aref-record
   #:row-major-aref-record-p
   #:setf-row-major-aref-record
   #:setf-row-major-aref-record-p
   #:instruction-record
   #:instruction-record-p
   #:instruction-record-name
   #:instruction-record-instruction-set
   #:instruction-record-vop
   #:instruction-record-mnemonic
   #:instruction-record-result-records
   #:instruction-record-argument-records
   #:instruction-record-cost
   #:instruction-record-pure
   #:instruction-record-always-translatable
   #:instruction-record-associative
   #:instruction-record-encoding
   #:instruction-record-prefix
   #:instruction-record-suffix
   #:vref-record
   #:vref-record-p
   #:vref-record-name
   #:vref-record-instruction-set
   #:vref-record-vop
   #:vref-record-vop-c
   #:vref-record-mnemonic
   #:vref-record-value-record
   #:vref-record-vector-record
   #:vref-record-aref
   #:vref-record-row-major-aref
   #:load-record
   #:load-record-p
   #:load-record-name
   #:load-record-instruction-set
   #:load-record-vop
   #:load-record-vop-c
   #:load-record-mnemonic
   #:load-record-value-record
   #:load-record-vector-record
   #:load-record-aref
   #:load-record-row-major-aref
   #:store-record
   #:store-record-p
   #:store-record-name
   #:store-record-instruction-set
   #:store-record-vop
   #:store-record-vop-c
   #:store-record-mnemonic
   #:store-record-value-record
   #:store-record-vector-record
   #:store-record-aref
   #:store-record-row-major-aref
   #:reffer-record
   #:reffer-record-p
   #:reffer-record-name
   #:reffer-record-instruction-set
   #:reffer-record-array-record
   #:reffer-record-primitive
   #:associative-record
   #:associative-record-p
   #:associative-record-name
   #:associative-record-instruction-set
   #:associative-record-binary-operation
   #:associative-record-identity-element
   #:reducer-record
   #:reducer-record-p
   #:reducer-record-name
   #:reducer-record-instruction-set
   #:reducer-record-binary-operation
   #:reducer-record-initial-element
   #:comparison-record
   #:comparison-record-p
   #:comparison-record-name
   #:comparison-record-instruction-set
   #:comparison-record-cmp
   #:comparison-record-and
   #:comparison-record-truth
   #:unequal-record
   #:unequal-record-p
   #:unequal-record-name
   #:unequal-record-instruction-set
   #:unequal-record-neq
   #:unequal-record-and
   #:unequal-record-truth
   #:if-record
   #:if-record-p
   #:if-record-name
   #:if-record-instruction-set
   #:if-record-blend
   #:cast-record
   #:cast-record-p
   #:cast-record-name
   #:cast-record-instruction-set
   #:scalar-cast-record
   #:scalar-cast-record-p
   #:scalar-cast-record-name
   #:scalar-cast-record-instruction-set
   #:simd-cast-record
   #:simd-cast-record-p
   #:simd-cast-record-name
   #:simd-cast-record-instruction-set
   #:simd-cast-record-broadcast
   #:reinterpret-cast-record
   #:reinterpret-cast-record-p
   #:reinterpret-cast-record-name
   #:reinterpret-cast-record-instruction-set
   #:reinterpret-cast-record-reinterpreters
   ;; Macros
   #:define-fake-vop
   #:define-trivial-fake-vop
   #:with-primitive-arguments
   #:with-primitive-argument
   #:instruction-set-case
   ;; CPU Identification
   #:sse-supported-p
   #:sse2-supported-p
   #:sse3-supported-p
   #:ssse3-supported-p
   #:sse4.1-supported-p
   #:sse4.2-supported-p
   #:avx-supported-p
   #:avx2-supported-p
   #:fma-supported-p))

(progn
  (defpackage #:sb-simd
    (:use #:common-lisp #:sb-simd-internals)
    #0=
    (:export
     ;; Re-exports from sb-simd-internals.
     #:most-positive-f32
     #:most-negative-f32
     #:most-positive-f64
     #:most-negative-f64
     #:most-positive-u1
     #:most-positive-u2
     #:most-positive-u4
     #:most-positive-u8
     #:most-positive-u16
     #:most-positive-u32
     #:most-positive-u64
     #:most-positive-s8
     #:most-negative-s8
     #:most-positive-s16
     #:most-negative-s16
     #:most-positive-s32
     #:most-negative-s32
     #:most-positive-s64
     #:most-negative-s64
     #:+u8-true+
     #:+u16-true+
     #:+u32-true+
     #:+u64-true+
     #:+u8-false+
     #:+u16-false+
     #:+u32-false+
     #:+u64-false+
     #:+s8-true+
     #:+s16-true+
     #:+s32-true+
     #:+s64-true+
     #:+s8-false+
     #:+s16-false+
     #:+s32-false+
     #:+s64-false+
     #:+f32-true+
     #:+f64-true+
     #:+f32-false+
     #:+f64-false+
     #:index
     #:index+
     #:index-
     #:index*
     ;; Macros
     #:define-inline
     #:instruction-set-case
     ;; f32
     #:f32
     #:f32vec
     #:f32-array
     #:f32-if
     #:f32-and
     #:f32-or
     #:f32-xor
     #:f32-andc1
     #:f32-not
     #:f32-max
     #:f32-min
     #:f32+
     #:f32-
     #:f32*
     #:f32/
     #:f32=
     #:f32/=
     #:f32<
     #:f32<=
     #:f32>
     #:f32>=
     #:f32-incf
     #:f32-decf
     #:f32-aref
     #:f32-row-major-aref
     ;; f64
     #:f64
     #:f64vec
     #:f64-array
     #:f64-if
     #:f64-and
     #:f64-or
     #:f64-xor
     #:f64-andc1
     #:f64-not
     #:f64-max
     #:f64-min
     #:f64+
     #:f64-
     #:f64*
     #:f64/
     #:f64=
     #:f64/=
     #:f64<
     #:f64<=
     #:f64>
     #:f64>=
     #:f64-incf
     #:f64-decf
     #:f64-aref
     #:f64-row-major-aref
     ;; u1
     #:u1
     ;; u2
     #:u2
     ;; u4
     #:u4
     ;; u8
     #:u8
     #:u8vec
     #:u8-array
     #:u8-if
     #:u8-and
     #:u8-or
     #:u8-xor
     #:u8-andc1
     #:u8-not
     #:u8-max
     #:u8-min
     #:u8+
     #:u8-
     #:u8=
     #:u8/=
     #:u8<
     #:u8<=
     #:u8>
     #:u8>=
     #:u8-incf
     #:u8-decf
     #:u8-aref
     #:u8-row-major-aref
     ;; u16
     #:u16
     #:u16vec
     #:u16-array
     #:u16-if
     #:u16-and
     #:u16-or
     #:u16-xor
     #:u16-andc1
     #:u16-not
     #:u16-max
     #:u16-min
     #:u16+
     #:u16-
     #:u16=
     #:u16/=
     #:u16<
     #:u16<=
     #:u16>
     #:u16>=
     #:u16-incf
     #:u16-decf
     #:u16-aref
     #:u16-row-major-aref
     ;; u32
     #:u32
     #:u32vec
     #:u32-array
     #:u32-if
     #:u32-and
     #:u32-or
     #:u32-xor
     #:u32-andc1
     #:u32-not
     #:u32-max
     #:u32-min
     #:u32+
     #:u32-
     #:u32=
     #:u32/=
     #:u32<
     #:u32<=
     #:u32>
     #:u32>=
     #:u32-incf
     #:u32-decf
     #:u32-aref
     #:u32-row-major-aref
     ;; u64
     #:u64
     #:u64vec
     #:u64-array
     #:u64-if
     #:u64-and
     #:u64-or
     #:u64-xor
     #:u64-andc1
     #:u64-not
     #:u64-max
     #:u64-min
     #:u64+
     #:u64-
     #:u64=
     #:u64/=
     #:u64<
     #:u64<=
     #:u64>
     #:u64>=
     #:u64-incf
     #:u64-decf
     #:u64-aref
     #:u64-row-major-aref
     ;; s8
     #:s8
     #:s8vec
     #:s8-array
     #:s8-if
     #:s8-and
     #:s8-or
     #:s8-xor
     #:s8-andc1
     #:s8-not
     #:s8-max
     #:s8-min
     #:s8+
     #:s8-
     #:s8=
     #:s8/=
     #:s8<
     #:s8<=
     #:s8>
     #:s8>=
     #:s8-incf
     #:s8-decf
     #:s8-aref
     #:s8-row-major-aref
     ;; s16
     #:s16
     #:s16vec
     #:s16-array
     #:s16-if
     #:s16-and
     #:s16-or
     #:s16-xor
     #:s16-andc1
     #:s16-not
     #:s16-max
     #:s16-min
     #:s16+
     #:s16-
     #:s16=
     #:s16/=
     #:s16<
     #:s16<=
     #:s16>
     #:s16>=
     #:s16-incf
     #:s16-decf
     #:s16-aref
     #:s16-row-major-aref
     ;; s32
     #:s32
     #:s32vec
     #:s32-array
     #:s32-if
     #:s32-and
     #:s32-or
     #:s32-xor
     #:s32-andc1
     #:s32-not
     #:s32-max
     #:s32-min
     #:s32+
     #:s32-
     #:s32=
     #:s32/=
     #:s32<
     #:s32<=
     #:s32>
     #:s32>=
     #:s32-incf
     #:s32-decf
     #:s32-aref
     #:s32-row-major-aref
     ;; s64
     #:s64
     #:s64vec
     #:s64-array
     #:s64-if
     #:s64-and
     #:s64-or
     #:s64-xor
     #:s64-andc1
     #:s64-not
     #:s64-max
     #:s64-min
     #:s64+
     #:s64-
     #:s64=
     #:s64/=
     #:s64<
     #:s64<=
     #:s64>
     #:s64>=
     #:s64-incf
     #:s64-decf
     #:s64-aref
     #:s64-row-major-aref
     ;; Simple Strings
     #+sb-unicode
     #:charvec
     #:char-array
     ;; Integer Packers
     #:u64-from-u8s
     #:u64-from-u16s
     #:u64-from-u32s
     #:u64-from-s8s
     #:u64-from-s16s
     #:u64-from-s32s
     #:u64-from-s64
     ;; Integer Unpackers
     #:u8s-from-u64
     #:u16s-from-u64
     #:u32s-from-u64
     #:s8s-from-u64
     #:s16s-from-u64
     #:s32s-from-u64
     #:s64-from-u64
     ;; Boolean Conversion
     #:u8-from-boolean
     #:u16-from-boolean
     #:u32-from-boolean
     #:u64-from-boolean
     ;; Odd Bits
     #:u16-odd-bits
     #:u32-odd-bits))

  (defpackage #:sb-simd-x86-64
    (:use #:common-lisp #:sb-simd-internals #:sb-simd)
    #0#
    #1=
    (:export #:imm1 #:imm2 #:imm3 #:imm4 #:imm5 #:imm6 #:imm7 #:imm8))

  (defpackage #:sb-simd-sse
    (:use #:common-lisp #:sb-simd-internals #:sb-simd-x86-64)
    (:shadow
     ;; f32
     #:f32
     #:f32-and
     #:f32-or
     #:f32-xor
     #:f32-andc1
     #:f32-not
     #:f32-max
     #:f32-min
     #:f32+
     #:f32-
     #:f32*
     #:f32/
     #:f32=
     #:f32/=
     #:f32<
     #:f32<=
     #:f32>
     #:f32>=
     #:f32-incf
     #:f32-decf
     #:f32-aref
     #:f32-row-major-aref)
    #0#
    #1#
    #2=
    (:export
     #:f32!
     #:p128
     ;; f32.4
     #:make-f32.4
     #:f32.4
     #:f32.4!
     #:f32.4-values
     #:f32.4-broadcast
     #:f32.4!-from-f32
     #:f32.4-and
     #:f32.4-or
     #:f32.4-xor
     #:f32.4-max
     #:f32.4-min
     #:f32.4+
     #:f32.4-
     #:f32.4*
     #:f32.4/
     #:f32.4-horizontal-and
     #:f32.4-horizontal-or
     #:f32.4-horizontal-xor
     #:f32.4-horizontal-max
     #:f32.4-horizontal-min
     #:f32.4-horizontal+
     #:f32.4-horizontal*
     #:f32.4-andc1
     #:f32.4-not
     #:f32.4-reciprocal
     #:f32.4-rsqrt
     #:f32.4-sqrt
     #:f32.4-movemask
     #:f32.4-shuffle
     #:f32.4-incf
     #:f32.4-decf
     #:f32.4-aref #:f32.4-row-major-aref
     #:f32.4-non-temporal-aref #:f32.4-non-temporal-row-major-aref))

  (defpackage #:sb-simd-sse2
    (:use #:common-lisp #:sb-simd-internals #:sb-simd-sse)
    (:shadow
     ;; f64
     #:f64
     #:f64-and
     #:f64-or
     #:f64-xor
     #:f64-andc1
     #:f64-not
     #:f64-max
     #:f64-min
     #:f64+
     #:f64-
     #:f64*
     #:f64/
     #:f64=
     #:f64/=
     #:f64<
     #:f64<=
     #:f64>
     #:f64>=
     #:f64-incf
     #:f64-decf
     #:f64-aref
     #:f64-row-major-aref)
    #0#
    #1#
    #2#
    #3=
    (:export
     #:f64!
     #:u8!
     #:u16!
     #:u32!
     #:u64!
     ;; f32.4
     #:f32.4=
     #:f32.4/=
     #:f32.4<
     #:f32.4<=
     #:f32.4>
     #:f32.4>=
     ;; f64.2
     #:make-f64.2
     #:f64.2
     #:f64.2!
     #:f64.2-values
     #:f64.2-broadcast
     #:f64.2-and
     #:f64.2-or
     #:f64.2-xor
     #:f64.2-max
     #:f64.2-min
     #:f64.2+
     #:f64.2-
     #:f64.2*
     #:f64.2/
     #:f64.2=
     #:f64.2/=
     #:f64.2<
     #:f64.2<=
     #:f64.2>
     #:f64.2>=
     #:f64.2-horizontal-and
     #:f64.2-horizontal-or
     #:f64.2-horizontal-xor
     #:f64.2-horizontal-max
     #:f64.2-horizontal-min
     #:f64.2-horizontal+
     #:f64.2-horizontal*
     #:f64.2-andc1
     #:f64.2-not
     #:f64.2-sqrt
     #:f64.2-shuffle
     #:f64.2-unpackhi
     #:f64.2-unpacklo
     #:f64.2-movemask
     #:f64.2-incf
     #:f64.2-decf
     #:f64.2-aref #:f64.2-row-major-aref
     #:f64.2-non-temporal-aref #:f64.2-non-temporal-row-major-aref
     ;; u8.16
     #:make-u8.16
     #:u8.16
     #:u8.16!
     #:u8.16-values
     #:u8.16-broadcast
     #:u8.16-and
     #:u8.16-or
     #:u8.16-xor
     #:u8.16-andc1
     #:u8.16-not
     #:u8.16+
     #:u8.16-
     #:u8.16=
     #:u8.16-unpackhi
     #:u8.16-unpacklo
     #:u8.16-movemask
     #:u8.16-average
     ;; TODO Having 8-bit shifts would be nice, but there is no instruction
     ;; for that.  But they could be implemented as a fake VOP.
     ;; #:u8.16-shiftl
     ;; #:u8.16-shiftr
     #:u8.16-incf
     #:u8.16-decf
     #:u8.16-aref #:u8.16-row-major-aref
     #:u8.16-non-temporal-aref #:u8.16-non-temporal-row-major-aref
     ;; u16.8
     #:make-u16.8
     #:u16.8
     #:u16.8!
     #:u16.8-values
     #:u16.8-broadcast
     #:u16.8-and
     #:u16.8-or
     #:u16.8-xor
     #:u16.8-andc1
     #:u16.8-not
     #:u16.8+
     #:u16.8-
     #:u16.8=
     #:u16.8-unpackhi
     #:u16.8-unpacklo
     #:u16.8-movemask
     #:u16.8-average
     #:u16.8-shiftl
     #:u16.8-shiftr
     #:u16.8-incf
     #:u16.8-decf
     #:u16.8-aref #:u16.8-row-major-aref
     #:u16.8-non-temporal-aref #:u16.8-non-temporal-row-major-aref
     ;; u32.4
     #:make-u32.4
     #:u32.4
     #:u32.4!
     #:u32.4-values
     #:u32.4-broadcast
     #:u32.4-and
     #:u32.4-or
     #:u32.4-xor
     #:u32.4-andc1
     #:u32.4-not
     #:u32.4+
     #:u32.4-
     #:u32.4=
     #:u32.4-unpackhi
     #:u32.4-unpacklo
     #:u32.4-movemask
     #:u32.4-shuffle
     #:u32.4-shiftl
     #:u32.4-shiftr
     #:u32.4-incf
     #:u32.4-decf
     #:u32.4-aref #:u32.4-row-major-aref
     #:u32.4-non-temporal-aref #:u32.4-non-temporal-row-major-aref
     #+sb-unicode
     #:u32.4-string-ref
     #+sb-unicode
     #:u32.4-row-major-string-ref
     ;; u64.2
     #:make-u64.2
     #:u64.2
     #:u64.2!
     #:u64.2-values
     #:u64.2-broadcast
     #:u64.2-and
     #:u64.2-or
     #:u64.2-xor
     #:u64.2-andc1
     #:u64.2-not
     #:u64.2+
     #:u64.2-
     #:u64.2-unpackhi
     #:u64.2-unpacklo
     #:u64.2-movemask
     #:u64.2-shiftl
     #:u64.2-shiftr
     #:u64.2-incf
     #:u64.2-decf
     #:u64.2-aref #:u64.2-row-major-aref
     #:u64.2-non-temporal-aref #:u64.2-non-temporal-row-major-aref
     ;; s8.16
     #:make-s8.16
     #:s8.16
     #:s8.16!
     #:s8.16-values
     #:s8.16-broadcast
     #:s8.16-and
     #:s8.16-or
     #:s8.16-xor
     #:s8.16-andc1
     #:s8.16-not
     #:s8.16+
     #:s8.16-
     #:s8.16=
     #:s8.16-unpackhi
     #:s8.16-unpacklo
     #:s8.16-movemask
     #:s8.16-aref #:s8.16-row-major-aref
     #:s8.16-non-temporal-aref #:s8.16-non-temporal-row-major-aref
     ;; s16.8
     #:make-s16.8
     #:s16.8
     #:s16.8!
     #:s16.8-values
     #:s16.8-broadcast
     #:s16.8-and
     #:s16.8-or
     #:s16.8-xor
     #:s16.8-andc1
     #:s16.8-not
     #:s16.8+
     #:s16.8-
     #:s16.8=
     #:s16.8-unpackhi
     #:s16.8-unpacklo
     #:s16.8-movemask
     #:s16.8-mullo
     #:s16.8-elt
     #:s16.8-shufflehi
     #:s16.8-shufflelo
     #:s16.8-shiftl
     #:s16.8-shiftr
     #:s16.8-aref #:s16.8-row-major-aref
     #:s16.8-non-temporal-aref #:s16.8-non-temporal-row-major-aref
     ;; s32.4
     #:make-s32.4
     #:s32.4
     #:s32.4!
     #:s32.4-values
     #:s32.4-broadcast
     #:s32.4-and
     #:s32.4-or
     #:s32.4-xor
     #:s32.4-andc1
     #:s32.4-not
     #:s32.4+
     #:s32.4-
     #:s32.4=
     #:s32.4-unpackhi
     #:s32.4-unpacklo
     #:s32.4-movemask
     #:s32.4-shuffle
     #:s32.4-shiftl
     #:s32.4-shiftr
     #:s32.4-aref #:s32.4-row-major-aref
     #:s32.4-non-temporal-aref #:s32.4-non-temporal-row-major-aref
     ;; s64.2
     #:make-s64.2
     #:s64.2
     #:s64.2!
     #:s64.2-values
     #:s64.2-broadcast
     #:s64.2-and
     #:s64.2-or
     #:s64.2-xor
     #:s64.2-andc1
     #:s64.2-not
     #:s64.2+
     #:s64.2-
     #:s64.2-unpackhi
     #:s64.2-unpacklo
     #:s64.2-movemask
     #:s64.2-shiftl
     #:s64.2-shiftr
     #:s64.2-aref #:s64.2-row-major-aref
     #:s64.2-non-temporal-aref #:s64.2-non-temporal-row-major-aref))

  (defpackage #:sb-simd-sse3
    (:use #:common-lisp #:sb-simd-internals #:sb-simd-sse2)
    #0#
    #1#
    #2#
    #3#
    #4=
    (:export
     #:f32.4-hadd
     #:f32.4-hdup
     #:f32.4-ldup))

  (defpackage #:sb-simd-ssse3
    (:use #:common-lisp #:sb-simd-internals #:sb-simd-sse3)
    #0#
    #1#
    #2#
    #3#
    #4#
    #5=
    (:export
     #:s16.8-mulhrs
     #:u16.8-hadd
     #:u32.4-hadd
     #:u16.8-hsub
     #:u32.4-hsub
     #:s8.16-shuffle
     #:s8.16-abs
     #:s8.16-sign
     #:s16.8-abs
     #:s16.8-maddubs
     #:s16.8-sign
     #:s16.8-hadd
     #:s16.8-hsub
     #:s32.4-abs
     #:s32.4-sign
     #:s32.4-hadd
     #:s32.4-hsub))

  (defpackage #:sb-simd-sse4.1
    (:use #:common-lisp #:sb-simd-internals #:sb-simd-ssse3)
    (:shadow
     #:f32.4-non-temporal-aref #:f32.4-non-temporal-row-major-aref
     #:f64.2-non-temporal-aref #:f64.2-non-temporal-row-major-aref
     #:u8.16-non-temporal-aref #:u8.16-non-temporal-row-major-aref
     #:u16.8-non-temporal-aref #:u16.8-non-temporal-row-major-aref
     #:u32.4-non-temporal-aref #:u32.4-non-temporal-row-major-aref
     #:u64.2-non-temporal-aref #:u64.2-non-temporal-row-major-aref
     #:s8.16-non-temporal-aref #:s8.16-non-temporal-row-major-aref
     #:s16.8-non-temporal-aref #:s16.8-non-temporal-row-major-aref
     #:s32.4-non-temporal-aref #:s32.4-non-temporal-row-major-aref
     #:s64.2-non-temporal-aref #:s64.2-non-temporal-row-major-aref)
    #0#
    #1#
    #2#
    #3#
    #4#
    #5#
    #6=
    (:export
     #:f32.4-if
     #:f32.4-round
     #:f32.4-floor
     #:f32.4-ceiling
     #:f32.4-truncate
     ;; #:f32.4-elt ;; TODO
     #:f32.4-insert
     #:f64.2-if
     #:f64.2-round
     #:f64.2-floor
     #:f64.2-ceiling
     #:f64.2-truncate
     #:u8.16-if
     #:u8.16-elt
     #:u8.16-insert
     #:u16.8-if
     #:u16.8-max
     #:u16.8-min
     #:u16.8-minpos
     #:u32.4-if
     #:u32.4-max
     #:u32.4-min
     #:u32.4-elt
     #:u32.4-insert
     #:u64.2-if
     #:u64.2=
     #:u64.2/=
     #:u64.2-elt
     #:s8.16-if
     #:s8.16-max
     #:s8.16-min
     #:s8.16-elt
     #:s8.16-insert
     #:s16.8-if
     #:s16.8-from-u8.16
     #:s16.8-from-s8.16
     #:s16.8-pack
     #:s32.4-if
     #:s32.4-max
     #:s32.4-min
     #:s32.4-from-u8.16
     #:s32.4-from-s8.16
     #:s32.4-from-u16.8
     #:s32.4-from-s16.8
     #:s32.4-mullo
     #:s32.4-elt
     #:s32.4-insert
     #:s64.2-if
     #:s64.2-from-u8.16
     #:s64.2-from-s8.16
     #:s64.2-from-u16.8
     #:s64.2-from-s16.8
     #:s64.2-from-u32.4
     #:s64.2-from-s32.4
     #:s64.2=
     #:s64.2/=
     #:s64.2-elt))

  (defpackage #:sb-simd-sse4.2
    (:use #:common-lisp #:sb-simd-internals #:sb-simd-sse4.1)
    #0#
    #1#
    #2#
    #3#
    #4#
    #5#
    #6#
    #7=
    (:export
     #:u64.2>
     #:u64.2>=
     #:u64.2<
     #:u64.2<=))

  (defpackage #:sb-simd-avx
    (:use #:common-lisp #:sb-simd-internals #:sb-simd-x86-64)
    #0#
    #1#
    (:shadow
     ;; f32
     #:f32
     #:f32-and
     #:f32-or
     #:f32-xor
     #:f32-andc1
     #:f32-not
     #:f32-max
     #:f32-min
     #:f32+
     #:f32-
     #:f32*
     #:f32/
     #:f32=
     #:f32/=
     #:f32<
     #:f32<=
     #:f32>
     #:f32>=
     #:f32-incf
     #:f32-decf
     #:f32-aref
     #:f32-row-major-aref
     ;; f64
     #:f64
     #:f64-and
     #:f64-or
     #:f64-xor
     #:f64-andc1
     #:f64-not
     #:f64-max
     #:f64-min
     #:f64+
     #:f64-
     #:f64*
     #:f64/
     #:f64=
     #:f64/=
     #:f64<
     #:f64<=
     #:f64>
     #:f64>=
     #:f64-incf
     #:f64-decf
     #:f64-aref
     #:f64-row-major-aref)
    #8=
    (:export
     #:p128
     #:p256
     #:vzeroupper
     #:vzeroall
     ;; f32.4
     #:make-f32.4
     #:f32.4
     #:f32.4!
     #:f32.4-values
     #:f32.4-broadcast
     #:f32.4-if
     #:f32.4-from-f64.4
     #:f32.4-and
     #:f32.4-or
     #:f32.4-xor
     #:f32.4-andc1
     #:f32.4-not
     #:f32.4-max
     #:f32.4-min
     #:f32.4+
     #:f32.4-
     #:f32.4*
     #:f32.4/
     #:f32.4-horizontal-and
     #:f32.4-horizontal-or
     #:f32.4-horizontal-xor
     #:f32.4-horizontal-max
     #:f32.4-horizontal-min
     #:f32.4-horizontal+
     #:f32.4-horizontal*
     #:f32.4=
     #:f32.4/=
     #:f32.4<
     #:f32.4<=
     #:f32.4>
     #:f32.4>=
     #:f32.4-addsub
     #:f32.4-hadd
     #:f32.4-hsub
     #:f32.4-reciprocal
     #:f32.4-rsqrt
     #:f32.4-sqrt
     #:f32.4-unpackhi
     #:f32.4-unpacklo
     #:f32.4-round
     #:f32.4-floor
     #:f32.4-ceiling
     #:f32.4-truncate
     #:f32.4-permute
     #:f32.4-shuffle
     #:f32.4-movemask
     #:f32.4-incf
     #:f32.4-decf
     #:f32.4-aref #:f32.4-row-major-aref
     #:f32.4-non-temporal-aref #:f32.4-non-temporal-row-major-aref
     ;; f64.2
     #:make-f64.2
     #:f64.2
     #:f64.2!
     #:f64.2-values
     #:f64.2-broadcast
     #:f64.2-if
     #:f64.2-and
     #:f64.2-or
     #:f64.2-xor
     #:f64.2-andc1
     #:f64.2-not
     #:f64.2-max
     #:f64.2-min
     #:f64.2+
     #:f64.2-
     #:f64.2*
     #:f64.2/
     #:f64.2-horizontal-and
     #:f64.2-horizontal-or
     #:f64.2-horizontal-xor
     #:f64.2-horizontal-max
     #:f64.2-horizontal-min
     #:f64.2-horizontal+
     #:f64.2-horizontal*
     #:f64.2=
     #:f64.2/=
     #:f64.2<
     #:f64.2<=
     #:f64.2>
     #:f64.2>=
     #:f64.2-addsub
     #:f64.2-hadd
     #:f64.2-hsub
     #:f64.2-sqrt
     #:f64.2-unpackhi
     #:f64.2-unpacklo
     #:f64.2-round
     #:f64.2-floor
     #:f64.2-ceiling
     #:f64.2-truncate
     #:f64.2-permute
     #:f64.2-shuffle
     #:f64.2-movemask
     #:f64.2-incf
     #:f64.2-decf
     #:f64.2-aref #:f64.2-row-major-aref
     #:f64.2-non-temporal-aref #:f64.2-non-temporal-row-major-aref
     ;; f32.8
     #:make-f32.8
     #:f32.8
     #:f32.8!
     #:f32.8-values
     #:f32.8-broadcast
     #:f32.8-if
     #:f32.8-from-u32.8
     #:f32.8-and
     #:f32.8-or
     #:f32.8-xor
     #:f32.8-andc1
     #:f32.8-not
     #:f32.8-max
     #:f32.8-min
     #:f32.8+
     #:f32.8-
     #:f32.8*
     #:f32.8/
     #:f32.8-horizontal-and
     #:f32.8-horizontal-or
     #:f32.8-horizontal-xor
     #:f32.8-horizontal-max
     #:f32.8-horizontal-min
     #:f32.8-horizontal+
     #:f32.8-horizontal*
     #:f32.8=
     #:f32.8/=
     #:f32.8<
     #:f32.8<=
     #:f32.8>
     #:f32.8>=
     #:f32.8-dupeven
     #:f32.8-dupodd
     #:f32.8-hadd
     #:f32.8-hsub
     #:f32.8-reciprocal
     #:f32.8-rsqrt
     #:f32.8-sqrt
     #:f32.8-unpackhi
     #:f32.8-unpacklo
     #:f32.8-round
     #:f32.8-floor
     #:f32.8-ceiling
     #:f32.8-truncate
     #:f32.8-permute
     #:f32.8-permute128
     #:f32.8-shuffle
     #:f32.8-movemask
     #:f32.4-from-f32.8
     #:f32.8-insert-f32.4
     #:f32.8-round
     #:f32.8-incf
     #:f32.8-decf
     #:f32.8-aref #:f32.8-row-major-aref
     #:f32.8-non-temporal-aref #:f32.8-non-temporal-row-major-aref
     ;; f64.4
     #:make-f64.4
     #:f64.4
     #:f64.4!
     #:f64.4-values
     #:f64.4-broadcast
     #:f64.4-if
     #:f64.4-from-f32.4
     #:f64.4-from-u32.4
     #:f64.4-from-s32.4
     #:f64.4-and
     #:f64.4-or
     #:f64.4-xor
     #:f64.4-andc1
     #:f64.4-not
     #:f64.4-max
     #:f64.4-min
     #:f64.4+
     #:f64.4-
     #:f64.4*
     #:f64.4/
     #:f64.4-horizontal-and
     #:f64.4-horizontal-or
     #:f64.4-horizontal-xor
     #:f64.4-horizontal-max
     #:f64.4-horizontal-min
     #:f64.4-horizontal+
     #:f64.4-horizontal*
     #:f64.4=
     #:f64.4/=
     #:f64.4<
     #:f64.4<=
     #:f64.4>
     #:f64.4>=
     #:f64.4-dupeven
     #:f64.4-hadd
     #:f64.4-hsub
     #:f64.4-sqrt
     #:f64.4-unpackhi
     #:f64.4-unpacklo
     #:f64.4-ceiling
     #:f64.4-permute
     #:f64.4-permute128
     #:f64.4-shuffle
     #:f64.4-movemask
     #:f64.4-reverse
     #:f64.2-from-f64.4
     #:f64.4-insert-f64.2
     #:f64.4-set128
     #:f64.4-round
     #:f64.4-incf
     #:f64.4-decf
     #:f64.4-aref #:f64.4-row-major-aref
     #:f64.4-non-temporal-aref #:f64.4-non-temporal-row-major-aref
     ;; u8.16
     #:make-u8.16
     #:u8.16
     #:u8.16!
     #:u8.16-values
     #:u8.16-broadcast
     #:u8.16-if
     #:u8.16-and
     #:u8.16-or
     #:u8.16-xor
     #:u8.16-andc1
     #:u8.16-not
     #:u8.16+
     #:u8.16-
     #:u8.16=
     #:u8.16/=
     #:u8.16>
     #:u8.16<
     #:u8.16>=
     #:u8.16<=
     #:u8.16-unpackhi
     #:u8.16-unpacklo
     #:u8.16-movemask
     #:u8.16-aref #:u8.16-row-major-aref
     #:u8.16-non-temporal-aref #:u8.16-non-temporal-row-major-aref
     ;; u16.8
     #:make-u16.8
     #:u16.8
     #:u16.8!
     #:u16.8-values
     #:u16.8-broadcast
     #:u16.8-if
     #:u16.8-and
     #:u16.8-or
     #:u16.8-xor
     #:u16.8-andc1
     #:u16.8-not
     #:u16.8+
     #:u16.8-
     #:u16.8=
     #:u16.8/=
     #:u16.8>
     #:u16.8<
     #:u16.8>=
     #:u16.8<=
     #:u16.8-shiftl
     #:u16.8-shiftr
     #:u16.8-unpackhi
     #:u16.8-unpacklo
     #:u16.8-movemask
     #:u16.8-aref #:u16.8-row-major-aref
     #:u16.8-non-temporal-aref #:u16.8-non-temporal-row-major-aref
     ;; u32.4
     #:make-u32.4
     #:u32.4
     #:u32.4!
     #:u32.4-values
     #:u32.4-broadcast
     #:u32.4-if
     #:u32.4-and
     #:u32.4-or
     #:u32.4-xor
     #:u32.4-andc1
     #:u32.4-not
     #:u32.4+
     #:u32.4-
     #:u32.4=
     #:u32.4/=
     #:u32.4>
     #:u32.4<
     #:u32.4>=
     #:u32.4<=
     #:u32.4-unpackhi
     #:u32.4-unpacklo
     #:u32.4-movemask
     #:u32.4-permute
     #:u32.4-aref #:u32.4-row-major-aref
     #:u32.4-non-temporal-aref #:u32.4-non-temporal-row-major-aref
     #+sb-unicode
     #:u32.4-string-ref
     #+sb-unicode
     #:u32.4-row-major-string-ref
     ;; u64.2
     #:make-u64.2
     #:u64.2
     #:u64.2!
     #:u64.2-values
     #:u64.2-broadcast
     #:u64.2-if
     #:u64.2-and
     #:u64.2-or
     #:u64.2-xor
     #:u64.2-andc1
     #:u64.2-not
     #:u64.2+
     #:u64.2-
     #:u64.2=
     #:u64.2/=
     #:u64.2>
     #:u64.2<
     #:u64.2>=
     #:u64.2<=
     #:u64.2-unpackhi
     #:u64.2-unpacklo
     #:u64.2-movemask
     #:u64.2-permute
     #:u64.2-aref #:u64.2-row-major-aref
     #:u64.2-non-temporal-aref #:u64.2-non-temporal-row-major-aref
     ;; u8.32
     #:make-u8.32
     #:u8.32
     #:u8.32!
     #:u8.32-values
     #:u8.32-broadcast
     #:u8.16-from-u8.32
     #:u8.32-insert-u8.16
     #:u8.32-aref #:u8.32-row-major-aref
     #:u8.32-non-temporal-aref #:u8.32-non-temporal-row-major-aref
     ;; u16.16
     #:make-u16.16
     #:u16.16
     #:u16.16!
     #:u16.16-values
     #:u16.16-broadcast
     #:u16.8-from-u16.16
     #:u16.16-insert-u16.8
     #:u16.16-aref #:u16.16-row-major-aref
     #:u16.16-non-temporal-aref #:u16.16-non-temporal-row-major-aref
     ;; u32.8
     #:make-u32.8
     #:u32.8
     #:u32.8!
     #:u32.8-values
     #:u32.8-broadcast
     #:u32.8-permute
     #:u32.8-insert-u32.4
     #:u32.8-aref #:u32.8-row-major-aref
     #:u32.8-non-temporal-aref #:u32.8-non-temporal-row-major-aref
     #+sb-unicode
     #:u32.8-string-ref
     #+sb-unicode
     #:u32.8-row-major-string-ref
     ;; u64.4
     #:make-u64.4
     #:u64.4
     #:u64.4!
     #:u64.4-values
     #:u64.4-broadcast
     #:u64.4-permute
     #:u64.2-from-u64.4
     #:u64.4-insert-u64.2
     #:u64.4-aref #:u64.4-row-major-aref
     #:u64.4-non-temporal-aref #:u64.4-non-temporal-row-major-aref
     ;; s8.16
     #:make-s8.16
     #:s8.16
     #:s8.16!
     #:s8.16-values
     #:s8.16-broadcast
     #:s8.16-if
     #:s8.16-and
     #:s8.16-or
     #:s8.16-xor
     #:s8.16-andc1
     #:s8.16-not
     #:s8.16+
     #:s8.16-
     #:s8.16=
     #:s8.16/=
     #:s8.16>
     #:s8.16<
     #:s8.16>=
     #:s8.16<=
     #:s8.16-unpackhi
     #:s8.16-unpacklo
     #:s8.16-movemask
     #:s8.16-aref #:s8.16-row-major-aref
     #:s8.16-non-temporal-aref #:s8.16-non-temporal-row-major-aref
     ;; s16.8
     #:make-s16.8
     #:s16.8
     #:s16.8!
     #:s16.8-values
     #:s16.8-broadcast
     #:s16.8-if
     #:s16.8-and
     #:s16.8-or
     #:s16.8-xor
     #:s16.8-andc1
     #:s16.8-not
     #:s16.8+
     #:s16.8-
     #:s16.8-mullo
     #:s16.8=
     #:s16.8/=
     #:s16.8>
     #:s16.8<
     #:s16.8>=
     #:s16.8<=
     #:s16.8-mullo
     #:s16.8-shiftl
     #:s16.8-shiftr
     #:s16.8-unpackhi
     #:s16.8-unpacklo
     #:s16.8-movemask
     #:s16.8-aref #:s16.8-row-major-aref
     #:s16.8-non-temporal-aref #:s16.8-non-temporal-row-major-aref
     ;; s32.4
     #:make-s32.4
     #:s32.4
     #:s32.4!
     #:s32.4-values
     #:s32.4-broadcast
     #:s32.4-if
     #:s32.4-from-f64.4
     #:s32.4-and
     #:s32.4-or
     #:s32.4-xor
     #:s32.4-andc1
     #:s32.4-not
     #:s32.4+
     #:s32.4-
     #:s32.4-mullo
     #:s32.4=
     #:s32.4/=
     #:s32.4>
     #:s32.4<
     #:s32.4>=
     #:s32.4<=
     #:s32.4-mullo
     #:s32.4-unpackhi
     #:s32.4-unpacklo
     #:s32.4-movemask
     #:s32.4-permute
     #:s32.4-aref #:s32.4-row-major-aref
     #:s32.4-non-temporal-aref #:s32.4-non-temporal-row-major-aref
     ;; s64.2
     #:s64.2
     #:make-s64.2
     #:s64.2
     #:s64.2!
     #:s64.2-values
     #:s64.2-broadcast
     #:s64.2-if
     #:s64.2-and
     #:s64.2-or
     #:s64.2-xor
     #:s64.2-andc1
     #:s64.2-not
     #:s64.2+
     #:s64.2-
     #:s64.2=
     #:s64.2/=
     #:s64.2>
     #:s64.2<
     #:s64.2>=
     #:s64.2<=
     #:s64.2-shiftl
     #:s64.2-shiftr
     #:s64.2-unpackhi
     #:s64.2-unpacklo
     #:s64.2-movemask
     #:s64.2-permute
     #:s64.2-aref #:s64.2-row-major-aref
     #:s64.2-non-temporal-aref #:s64.2-non-temporal-row-major-aref
     ;; s8.32
     #:make-s8.32
     #:s8.32
     #:s8.32!
     #:s8.32-values
     #:s8.32-broadcast
     #:s8.16-from-s8.32
     #:s8.32-insert-s8.16
     #:s8.32-permute128
     #:s8.32-aref #:s8.32-row-major-aref
     #:s8.32-non-temporal-aref #:s8.32-non-temporal-row-major-aref
     ;; s16.16
     #:make-s16.16
     #:s16.16
     #:s16.16!
     #:s16.16-values
     #:s16.16-broadcast
     #:s16.8-from-s16.16
     #:s16.16-insert-s16.8
     #:s16.16-permute128
     #:s16.16-aref #:s16.16-row-major-aref
     #:s16.16-non-temporal-aref #:s16.16-non-temporal-row-major-aref
     ;; s32.8
     #:make-s32.8
     #:s32.8
     #:s32.8!
     #:s32.8-values
     #:s32.8-broadcast
     #:s32.4-from-s32.8
     #:s32.8-insert-s32.4
     #:s32.8-permute128
     #:s32.8-aref #:s32.8-row-major-aref
     #:s32.8-non-temporal-aref #:s32.8-non-temporal-row-major-aref
     ;; s64.4
     #:make-s64.4
     #:s64.4
     #:s64.4!
     #:s64.4-values
     #:s64.4-broadcast
     #:s64.2-from-s64.4
     #:s64.4-insert-s64.2
     #:s64.4-permute
     #:s64.4-permute128
     #:s64.4-aref #:s64.4-row-major-aref
     #:s64.4-non-temporal-aref #:s64.4-non-temporal-row-major-aref))

  (defpackage #:sb-simd-avx2
    (:use #:common-lisp #:sb-simd-internals #:sb-simd-avx)
    (:shadow
     #:u8.16!-from-p256 #:u16.8!-from-p256 #:u32.4!-from-p256 #:u64.2!-from-p256
     #:s8.16!-from-p256 #:s16.8!-from-p256 #:s32.4!-from-p256 #:s64.2!-from-p256
     #:u8.16! #:u16.8! #:u32.4! #:u64.2!
     #:s8.16! #:s16.8! #:s32.4! #:s64.2!
     #:make-u8.32 #:make-u16.16 #:make-u32.8 #:make-u64.4
     #:make-s8.32 #:make-s16.16 #:make-s32.8 #:make-s64.4
     #:u8.32 #:u16.16 #:u32.8 #:u64.4
     #:s8.32 #:s16.16 #:s32.8 #:s64.4
     #:u8.32-values #:u16.16-values #:u32.8-values #:u64.4-values
     #:s8.32-values #:s16.16-values #:s32.8-values #:s64.4-values
     #:u8.16-from-u8.32 #:u16.8-from-u16.16 #:u32.4-from-u32.8 #:u64.2-from-u64.4
     #:s8.16-from-s8.32 #:s16.8-from-s16.16 #:s32.4-from-s32.8 #:s64.2-from-s64.4
     #:u8.32-insert-u8.16 #:u16.16-insert-u16.8 #:u32.8-insert-u32.4 #:u64.4-insert-u64.2
     #:s8.32-insert-s8.16 #:s16.16-insert-s16.8 #:s32.8-insert-s32.4 #:s64.4-insert-s64.2
     #:u8.16-broadcast #:u16.8-broadcast  #:u32.4-broadcast #:u64.2-broadcast
     #:s8.16-broadcast #:s16.8-broadcast  #:s32.4-broadcast #:s64.2-broadcast
     #:u8.32-broadcast #:u16.16-broadcast #:u32.8-broadcast #:u64.4-broadcast
     #:s8.32-broadcast #:s16.16-broadcast #:s32.8-broadcast #:s64.4-broadcast
     #:u8.32-permute128 #:s8.32-permute128
     #:u32.8-permute128 #:s32.8-permute128
     #:s16.16-permute128
     #:s64.4-permute128
     #:f64.4-reverse
     #:s64.2-shiftl
     #:s64.2-shiftr
     #:f32.4-non-temporal-aref #:f32.4-non-temporal-row-major-aref
     #:f64.2-non-temporal-aref #:f64.2-non-temporal-row-major-aref
     #:f32.8-non-temporal-aref #:f32.8-non-temporal-row-major-aref
     #:f64.4-non-temporal-aref #:f64.4-non-temporal-row-major-aref
     #:u8.16-non-temporal-aref #:u8.16-non-temporal-row-major-aref
     #:u16.8-non-temporal-aref #:u16.8-non-temporal-row-major-aref
     #:u32.4-non-temporal-aref #:u32.4-non-temporal-row-major-aref
     #:u64.2-non-temporal-aref #:u64.2-non-temporal-row-major-aref
     #:s8.16-non-temporal-aref #:s8.16-non-temporal-row-major-aref
     #:s16.8-non-temporal-aref #:s16.8-non-temporal-row-major-aref
     #:s32.4-non-temporal-aref #:s32.4-non-temporal-row-major-aref
     #:s64.2-non-temporal-aref #:s64.2-non-temporal-row-major-aref
     #:u8.32-non-temporal-aref  #:u8.32-non-temporal-row-major-aref
     #:u16.16-non-temporal-aref #:u16.16-non-temporal-row-major-aref
     #:u32.8-non-temporal-aref  #:u32.8-non-temporal-row-major-aref
     #:u64.4-non-temporal-aref  #:u64.4-non-temporal-row-major-aref
     #:s8.32-non-temporal-aref  #:s8.32-non-temporal-row-major-aref
     #:s16.16-non-temporal-aref #:s16.16-non-temporal-row-major-aref
     #:s32.8-non-temporal-aref  #:s32.8-non-temporal-row-major-aref
     #:s64.4-non-temporal-aref  #:s64.4-non-temporal-row-major-aref)
    #0#
    #1#
    #8#
    #9=
    (:export
     ;; f32.8
     ;; f64.4
     #:f64.4-reverse
     ;; u8.16
     ;; u16.8
     ;; u32.4
     #:u32.4-shiftl
     #:u32.4-shiftr
     ;; u64.2
     #:u64.2-shiftl
     #:u64.2-shiftr
     ;; s8.16
     ;; s16.8
     ;; s32.4
     #:s32.4-shiftl
     #:s32.4-shiftr
     ;; s64.2
     #:s64.2-shiftl
     #:s64.2-shiftr
     ;; u8.32
     #:u8.32-if
     #:u8.32-and
     #:u8.32-or
     #:u8.32-xor
     #:u8.32-andc1
     #:u8.32-not
     #:u8.32-max
     #:u8.32-min
     #:u8.32+
     #:u8.32-
     #:u8.32=
     #:u8.32/=
     #:u8.32>
     #:u8.32<
     #:u8.32>=
     #:u8.32<=
     #:u8.32-avg
     #:u8.32-packus
     #:u8.32-unpackhi
     #:u8.32-unpacklo
     #:u8.32-movemask
     #:u8.32-permute128
     #:u8.16-from-u8.32
     #:u8.32-insert-u8.16
     ;; u16.16
     #:u16.16-from-u8.16
     #:u16.16-if
     #:u16.16-and
     #:u16.16-or
     #:u16.16-xor
     #:u16.16-andc1
     #:u16.16-not
     #:u16.16-max
     #:u16.16-min
     #:u16.16+
     #:u16.16-
     #:s16.16-mulhi
     #:u16.16=
     #:u16.16/=
     #:u16.16>
     #:u16.16<
     #:u16.16>=
     #:u16.16<=
     #:u16.16-shiftl
     #:u16.16-shiftr
     #:u16.16-avg
     #:u16.16-packus
     #:u16.16-unpacklo
     #:u16.16-unpackhi
     #:u16.16-movemask
     #:u16.8-from-u16.16
     #:u16.16-insert-u16.8
     #:u16.16-permute128
     ;; u32.8
     #:u32.8-from-u16.8
     #:u32.8-from-u8.16
     #:u32.8-if
     #:u32.8-and
     #:u32.8-or
     #:u32.8-xor
     #:u32.8-andc1
     #:u32.8-not
     #:u32.8-max
     #:u32.8-min
     #:u32.8+
     #:u32.8-
     #:u32.8=
     #:u32.8/=
     #:u32.8>
     #:u32.8<
     #:u32.8>=
     #:u32.8<=
     #:u32.8-shiftl
     #:u32.8-shiftr
     #:u32.8-unpacklo
     #:u32.8-unpackhi
     #:u32.8-movemask
     #:u32.4-from-u32.8
     #:u32.8-insert-u32.4
     #:u32.8-permute128
     #:u32.8-incf
     #:u32.8-decf
     ;; u64.4
     #:u64.4-from-u16.8
     #:u64.4-from-u32.4
     #:u64.4-from-u8.16
     #:u64.4-if
     #:u64.4-and
     #:u64.4-or
     #:u64.4-xor
     #:u64.4-andc1
     #:u64.4-not
     #:u64.4+
     #:u64.4-
     #:u64.4-mul
     #:u64.4=
     #:u64.4/=
     #:u64.4>
     #:u64.4<
     #:u64.4>=
     #:u64.4<=
     #:u64.4-shiftl
     #:u64.4-shiftr
     #:u64.4-unpacklo
     #:u64.4-unpackhi
     #:u64.4-movemask
     #:u64.2-from-u64.4
     #:u64.4-insert-u64.2
     #:u64.4-permute128
     #:u64.4-incf
     #:u64.4-decf
     ;; s8.32
     #:s8.32-if
     #:s8.32-and
     #:s8.32-or
     #:s8.32-xor
     #:s8.32-andc1
     #:s8.32-not
     #:s8.32-max
     #:s8.32-max
     #:s8.32+
     #:s8.32-
     #:s32.8-mullo
     #:s8.32=
     #:s8.32/=
     #:s8.32>
     #:s8.32<
     #:s8.32>=
     #:s8.32<=
     #:s8.32-abs
     #:s8.32-packs
     #:s8.32-unpacklo
     #:s8.32-unpackhi
     #:s8.32-movemask
     #:s8.32-shuffle
     #:s8.32-sign
     #:s8.16-from-s8.32
     #:s8.32-insert-s8.16
     #:s8.32-permute128
     ;; s16.16
     #:s16.16-from-s8.16
     #:s16.16-from-u8.16
     #:s16.16-if
     #:s16.16-and
     #:s16.16-or
     #:s16.16-xor
     #:s16.16-andc1
     #:s16.16-not
     #:s16.16-max
     #:s16.16-min
     #:s16.16+
     #:s16.16-
     #:s16.16-mulhi
     #:s16.16-mullo
     #:s16.16-mulhrs
     #:s16.16=
     #:s16.16/=
     #:s16.16>
     #:s16.16<
     #:s16.16>=
     #:s16.16<=
     #:s16.16-abs
     #:s16.16-hadd
     #:s16.16-hadds
     #:s16.16-madd
     #:s16.16-maddubs
     #:s16.16-hsub
     #:s16.16-hsubs
     #:s16.16-packs
     #:s16.16-unpackhi
     #:s16.16-unpacklo
     #:s16.16-movemask
     #:s16.16-shiftl
     #:s16.16-shiftr
     #:s16.16-sign
     #:s16.8-from-s16.16
     #:s16.16-insert-s16.8
     #:s16.16-permute128
     ;; s32.8
     #:s32.8-from-s16.8
     #:s32.8-from-u16.8
     #:s32.8-from-s8.16
     #:s32.8-from-u8.16
     #:s32.8-if
     #:s32.8-and
     #:s32.8-or
     #:s32.8-xor
     #:s32.8-andc1
     #:s32.8-not
     #:s32.8-max
     #:s32.8-min
     #:s32.8+
     #:s32.8-
     #:s32.8-mullo
     #:s32.8=
     #:s32.8/=
     #:s32.8>
     #:s32.8<
     #:s32.8>=
     #:s32.8<=
     #:s32.8-abs
     #:s32.8-hadd
     #:s32.8-hsub
     #:s32.8-shiftl
     #:s32.8-shiftr
     #:s32.8-sign
     #:s32.8-unpacklo
     #:s32.8-unpackhi
     #:s32.8-movemask
     #:s32.4-from-s32.8
     #:s32.8-insert-s32.4
     #:s32.8-permute128
     #:s32.8-incf
     #:s32.8-decf
     ;; s64.4
     #:s64.4-from-s16.8
     #:s64.4-from-u16.8
     #:s64.4-from-s32.4
     #:s64.4-from-u32.4
     #:s64.4-from-s8.16
     #:s64.4-from-u8.16
     #:s64.4-if
     #:s64.4-and
     #:s64.4-or
     #:s64.4-xor
     #:s64.4-andc1
     #:s64.4-not
     #:s64.4+
     #:s64.4-
     #:s64.4=
     #:s64.4/=
     #:s64.4>
     #:s64.4<
     #:s64.4>=
     #:s64.4<=
     #:s64.4-shiftl
     #:s64.4-shiftr
     #:s64.4-unpackhi
     #:s64.4-unpacklo
     #:s64.4-movemask
     #:s64.2-from-s64.4
     #:s64.4-insert-s64.2
     #:s64.4-permute128
     #:s64.4-incf
     #:s64.4-decf))

  (defpackage #:sb-simd-fma
    (:use #:common-lisp #:sb-simd-internals #:sb-simd-avx2)
    #0#
    #1#
    #8#
    #9#
    #10=
    (:export
     ;; f32
     #:f32-fmadd
     #:f32-fnmadd
     #:f32-fmsub
     #:f32-fmaddsub
     #:f32-fmsubadd
     ;; f64
     #:f64-fmadd
     #:f64-fnmadd
     #:f64-fmsub
     #:f64-fmaddsub
     #:f64-fmsubadd
     ;; f32.4
     #:f32.4-fmadd
     #:f32.4-fnmadd
     #:f32.4-fmsub
     #:f32.4-fmaddsub
     #:f32.4-fmsubadd
     ;; f32.8
     #:f32.8-fmadd
     #:f32.8-fnmadd
     #:f32.8-fmsub
     #:f32.8-fmaddsub
     #:f32.8-fmsubadd
     ;; f64.2
     #:f64.2-fmadd
     #:f64.2-fnmadd
     #:f64.2-fmsub
     #:f64.2-fmaddsub
     #:f64.2-fmsubadd
     ;; f64.4
     #:f64.4-fmadd
     #:f64.4-fnmadd
     #:f64.4-fmsub
     #:f64.4-fmaddsub
     #:f64.4-fmsubadd)))

(dolist (p '("SB-SIMD" "SB-SIMD-AVX" "SB-SIMD-AVX2" "SB-SIMD-FMA"
             "SB-SIMD-INTERNALS" "SB-SIMD-SSE" "SB-SIMD-SSE2"
             "SB-SIMD-SSE3" "SB-SIMD-SSE4.1" "SB-SIMD-SSE4.2"
             "SB-SIMD-SSSE3" "SB-SIMD-X86-64"))
  (setf (sb-int:system-package-p (find-package p)) t))
