;;;; machine-independent aspects of the object representation

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; KLUDGE: The primitive objects here may look like self-contained
;;;; definitions, but in general they're not. In particular, if you
;;;; try to add a slot to them, beware of the following:
;;;;   * The GC scavenging code (and for all I know other GC code too)
;;;;     is not automatically generated from these layouts, but instead
;;;;     was hand-written to correspond to them. The offsets are
;;;;     automatically propagated into the GC scavenging code, but the
;;;;     existence of slots, and whether they should be scavenged, is
;;;;     not automatically propagated. Thus e.g. if you add a
;;;;     SIMPLE-FUN-DEBUG-INFO slot holding a tagged object which needs
;;;;     to be GCed, you need to tweak scav_code_blob() and
;;;;     verify_space() in gencgc.c, and the corresponding code in gc.c.
;;;;   * Various code (e.g. STATIC-FSET in genesis.lisp) is hard-wired
;;;;     to know the name of the last slot of the object the code works
;;;;     with, and implicitly to know that the last slot is special (being
;;;;     the beginning of an arbitrary-length sequence of bytes following
;;;;     the fixed-layout slots).
;;;; -- WHN 2001-12-29

;;;; the primitive objects themselves

(define-primitive-object (cons :type cons :lowtag list-pointer-lowtag)
  (car :ref-trans car :set-trans %rplaca :init :arg
       :cas-trans %compare-and-swap-car)
  (cdr :ref-trans cdr :set-trans %rplacd :init :arg
       :cas-trans %compare-and-swap-cdr))

(define-primitive-object (instance :lowtag instance-pointer-lowtag
                                   :widetag instance-widetag
                                   :alloc-trans %make-instance)
  (slots :rest-p t))

(define-primitive-object (bignum :lowtag other-pointer-lowtag
                                 :widetag bignum-widetag
                                 :alloc-trans sb-bignum:%allocate-bignum)
  (digits :rest-p t :c-type "sword_t"))

(define-primitive-object (ratio :type ratio
                                :lowtag other-pointer-lowtag
                                :widetag ratio-widetag
                                :alloc-trans %make-ratio)
  (numerator :type (and integer (not (eql 0)))
             :ref-known (flushable movable)
             :ref-trans %numerator
             :init :arg)
  (denominator :type (integer 2)
               :ref-known (flushable movable)
               :ref-trans %denominator
               :init :arg))

#-64-bit
(define-primitive-object (single-float :lowtag other-pointer-lowtag
                                       :widetag single-float-widetag)
  (value :c-type "float"))

(define-primitive-object (double-float :lowtag other-pointer-lowtag
                                       :widetag double-float-widetag)
  #-64-bit (filler)
  (value :c-type "double" :length #.(/ 64 n-word-bits)))

#+long-float
(define-primitive-object (long-float :lowtag other-pointer-lowtag
                                     :widetag long-float-widetag)
  #+sparc (filler)
  (value :c-type "long double" :length #+x86 3 #+sparc 4))

;;; FIXME: the primitive-type should probably be named COMPLEX-RATIONAL
;;; but that was more invasive than renaming just the widetag
(define-primitive-object (complex :type complex
                                  :lowtag other-pointer-lowtag
                                  :widetag complex-rational-widetag
                                  :alloc-trans %make-complex)
  (real :type real
        :ref-known (flushable movable)
        :ref-trans %realpart
        :init :arg)
  (imag :type real
        :ref-known (flushable movable)
        :ref-trans %imagpart
        :init :arg))

(define-primitive-object (array :lowtag other-pointer-lowtag
                                :widetag t)
  ;; FILL-POINTER of an ARRAY is in the same place as LENGTH of a
  ;; VECTOR -- see SHRINK-VECTOR.
  (fill-pointer :type index
                :ref-trans %array-fill-pointer
                :ref-known (flushable)
                :set-trans (setf %array-fill-pointer)
                :set-known ())
  (elements :type index
            :ref-trans %array-available-elements
            :ref-known (flushable)
            :set-trans (setf %array-available-elements)
            :set-known ())
  (data :type array
        :ref-trans %array-data ; might be a vector, might not be
        :ref-known (flushable)
        :set-trans (setf %array-data)
        :set-known ())
  (displacement :type index
                :ref-trans %array-displacement
                :ref-known (flushable)
                :set-trans (setf %array-displacement)
                :set-known ())
  (displaced-p :type t
               :ref-trans %array-displaced-p
               :ref-known (flushable)
               :set-trans (setf %array-displaced-p)
               :set-known ())
  (displaced-from :type list
                  :ref-trans %array-displaced-from
                  :ref-known (flushable)
                  :set-trans (setf %array-displaced-from)
                  :set-known ())
  (dimensions :rest-p t))

(define-primitive-object (vector :type vector
                                 :lowtag other-pointer-lowtag
                                 :widetag t)
  ;; FILL-POINTER of an ARRAY is in the same place as LENGTH of a
  ;; VECTOR -- see SHRINK-VECTOR.
  (length :ref-trans sb-c::vector-length
          :type index)
  (data :rest-p t :c-type "uword_t"))

#|
Code header representation:

  |       total words     | widetag |
  | (3 bytes less 2 bits) |         |
  +-----------------------+---------+  [32-bit words]
  |      N boxed header bytes       |
  +---------------------------------+
  max total payload size in words = #x3fffff

  |            total words          | gc_gen | 0 | 0 | widetag |
  |            (4 bytes)            |        |   |   |         |
  +------------------------------------------------------------+  [64-bit words]
  |                                 |   N boxed header bytes   |
  |                                 |        (4 bytes)         |
  +------------------------------------------------------------+

  the two zero bytes are reserved for future use
  max total payload size in words = uint_max
    (should probably made the same as for 32-bit word size for consistency)

For both:
  code-size = total words * n-word-bytes - boxed bytes
  text-size = code-size - simple-fun table size - padding bytes
  bit 30           = touched since last GC bit
  The boxed byte count is stored "raw" (i.e. it's not a tagged value,
  but it has fixnum nature)

Since most architectures can not atomically store and load 2 words at once,
it is essential that code size be computable by loading a single word
to make backtrace reliable (i.e. in the heap search step).

Note that vector objects require reading their length from a non-header word,
but this not subject to a data race because only 1 word conveys the size.
In addition there are no vectors on code pages which are the pages scanned
during backtrace.
|#

;;; The header contains the total size of the object (including
;;; the header itself) in words.
;;; NB: while we have in fact done a fairly thorough job of eradicating
;;; hidden dependencies on primitive object sizes for the most part,
;;; 'ppc-assem.S' contains a literal constant that relies on knowing
;;; the precise size of a code object. Yes, there is a FIXME there :-)
;;; So, if you touch this, then fix that. REALLY REALLY.
(define-primitive-object (code :type code-component
                                :lowtag other-pointer-lowtag
                                :widetag code-header-widetag)
  ;; This is the length of the boxed section, in bytes, not tagged.
  ;; It will be a multiple of the word size.
  ;; It can be accessed as a tagged value in Lisp by shifting.
  (boxed-size :type fixnum ; see above figure
              :ref-known (flushable movable)
              :ref-trans %code-boxed-size)
  ;;
  ;; Caution! the first few slots have a slight amount of order-sensitivity.
  ;; When saving a core we try to defer the scavenge of FIXUPS and DEBUG-INFO
  ;; so that they reside relatively near the end of the consumed space.
  ;; This is similar to what 'purify.c' would do.  It's tricky to express a range
  ;; without either hardwiring the indices or else assuming that you know the order
  ;; in which they appear, as in "&obj->firstslot" up to "&obj->lastslot".
  ;; See in particular how scav_code_blob() and delay_code_metadata_scavenge()
  ;; conspire to skip over 2 slots and then come back to visit them later.
  ;;
  ;; Not all architectures use fixups. The slot is always present for consistency.
  ;; The corresponding SETF function is defined using code-header-set
  ;; on the slot index.
  (fixups :type t :ref-known (flushable) :ref-trans %code-fixups)
  ;; This can be either a DEBUG-INFO object, the symbol :BPT-LRA, or,
  ;; as a special case for the assembler code component, a cons
  ;; holding a hash-table. (the cons points from read-only to static
  ;; space, but the hash-table wants to be in dynamic space) The
  ;; corresponding SETF function is defined using code-header-set on
  ;; the slot index; and there's a special variant if #+darwin-jit.
  (debug-info :type t
              :ref-known (flushable)
              :ref-trans %code-debug-info)
  (constants :rest-p t))

(define-primitive-object (fdefn :type fdefn
                                :lowtag other-pointer-lowtag
                                :widetag fdefn-widetag)
  (name :ref-trans fdefn-name)
  (fun :type (or function null) :ref-trans fdefn-fun)
  ;; raw-addr is used differently by the various backends:
  ;; - Sparc, ARM, and RISC-V store the same object as 'fun'
  ;;   unless the function is non-simple, in which case
  ;;   they store a descriptorized (fun-pointer lowtag)
  ;;   pointer to the closure tramp
  ;; - all others store a native pointer to the function entry address
  ;;   or closure tramp. x86-64 with immobile-code constrains this
  ;;   to holding the address of a SIMPLE-FUN or an object that
  ;;   has the simple-fun call convention- either a generic-function with
  ;;   a self-contained trampoline, or closure or funcallable-instance
  ;;   wrapped in a simplifying trampoline.
  (raw-addr :c-type "char *"))

;;; Reader for FDEFN-RAW-ADDR. The usual IR2 converter would return
;;; descriptor-reg and so its result would need shifting by n-fixnum-tag-bits.
#-sb-xc-host
(defun fdefn-raw-addr (fdefn)
  (with-pinned-objects (fdefn)
    (sap-ref-word (int-sap (get-lisp-obj-address fdefn))
                  (- (ash fdefn-raw-addr-slot word-shift) other-pointer-lowtag))))

;;; a simple function (as opposed to hairier things like closures
;;; which are also subtypes of Common Lisp's FUNCTION type)
(define-primitive-object (simple-fun :type function
                                     :lowtag fun-pointer-lowtag
                                     :widetag simple-fun-widetag)
  ;; All three function primitive-objects have the first word after the header
  ;; as some kind of entry point, either the address to jump to, in the case
  ;; of x86oids and arm64, or the Lisp function to jump to, for everybody else.
  (self :set-known ()
        :set-trans (setf %simple-fun-self))
  ;; This slot used to be named CODE, but that was misleaing because the
  ;; generated constant SIMPLE-FUN-CODE-OFFSET did not mean the offset from here
  ;; back to the containing object (which isn't constant), but instead the offset
  ;; forward to the first instruction, i.e. what is now SIMPLE-FUN-INSTS-OFFSET.
  (insts :rest-p t :c-type "unsigned char"))

(defconstant code-slots-per-simple-fun 4)
;;; These are word numbers beyond the base of the simple-fun's metadata
;;; in the code header. The mnemonic device here is that the first 3 slots
;;; essentially comprise the function-lambda-expression,
;;; and the last is a derived piece of information.
(defconstant simple-fun-name-slot    0)
(defconstant simple-fun-arglist-slot 1)
(defconstant simple-fun-source-slot  2) ; form and/or docstring
(defconstant simple-fun-info-slot    3) ; type and possibly xref

#-(or x86 x86-64 arm64 riscv)
(define-primitive-object (return-pc :lowtag other-pointer-lowtag :widetag t)
  (return-point :c-type "unsigned char" :rest-p t))

(define-primitive-object (closure :lowtag fun-pointer-lowtag
                                  :widetag closure-widetag
                                  ;; This allocator is used when renaming or cloning
                                  ;; a closure. The compiler has its own way of making
                                  ;; closures which requires that the length be
                                  ;; a compile-time constant.
                                  :alloc-trans %alloc-closure)
  (fun :init :arg :ref-trans #+(or x86 x86-64 arm64) %closure-callee
                             #-(or x86 x86-64 arm64) %closure-fun)
  (info :rest-p t))

(define-primitive-object (funcallable-instance
                          :lowtag fun-pointer-lowtag
                          :widetag funcallable-instance-widetag
                          :alloc-trans %make-funcallable-instance)
  (trampoline)
  ;; self-contained trampoline instructions go in the words following
  ;; the trampoline address. Currently the use of 2 words is based on
  ;; the requirement for x86-64 but it is relatively easy to change.
  ;; It's best if the displacement from the instructions to the FUNCTION
  ;; slot is independent of whether there is a LAYOUT slot. Otherwise,
  ;; were the LAYOUT to intrude between the instructions and the FUNCTION,
  ;; then the instruction bytes would depend on whether #+compact-instance-header
  ;; is enabled, which is an extra and unnecessary complication.
  #+executable-funinstances (instword1)
  #+executable-funinstances (instword2)
  (function :type function
            :ref-known (flushable) :ref-trans %funcallable-instance-fun
            :set-known () :set-trans (setf %funcallable-instance-fun))
  #-compact-instance-header
  (layout :set-trans %set-fun-layout :ref-trans %fun-layout)
  (info :rest-p t))

(define-primitive-object (value-cell :lowtag other-pointer-lowtag
                                     :widetag value-cell-widetag
                                     ;; FIXME: We also have an explicit VOP
                                     ;; for this. Is this needed as well?
                                     :alloc-trans make-value-cell)
  (value :set-trans value-cell-set
         :set-known ()
         :ref-trans value-cell-ref
         :ref-known (flushable)
         :init :arg))

(define-primitive-object (sap :lowtag other-pointer-lowtag
                              :widetag sap-widetag)
  (pointer :c-type "char *" :pointer t))


(define-primitive-object (weak-pointer :type weak-pointer
                                       :lowtag other-pointer-lowtag
                                       :widetag weak-pointer-widetag
                                       :alloc-trans make-weak-pointer)
  (value :ref-trans %weak-pointer-value :ref-known (flushable)
         :init :arg)
  ;; 64-bit uses spare header bytes to store the 'next' link
  #-64-bit (next :c-type "struct weak_pointer *"))

;;;; other non-heap data blocks

(define-primitive-object (binding)
  value
  symbol) ;; on sb-thread, this is actually a tls-index

(define-primitive-object (unwind-block)
  (uwp :c-type "struct unwind_block *")
  (cfp :c-type "lispobj *")
  #-(or x86 x86-64 arm64) code
  entry-pc
  #+(and win32 x86) next-seh-frame
  #+(and win32 x86) seh-frame-handler
  #+(and unbind-in-unwind (not c-stack-is-control-stack)) nfp
  #+(and unbind-in-unwind (not c-stack-is-control-stack)) nsp
  #+unbind-in-unwind bsp
  #+unbind-in-unwind current-catch)

(define-primitive-object (catch-block)
  (uwp :c-type "struct unwind_block *")
  (cfp :c-type "lispobj *")
  #-(or x86 x86-64 arm64) code
  entry-pc
  #+(and win32 x86) next-seh-frame
  #+(and win32 x86) seh-frame-handler
  #+(and unbind-in-unwind (not c-stack-is-control-stack)) nfp
  #+(and unbind-in-unwind (not c-stack-is-control-stack)) nsp
  #+unbind-in-unwind bsp
  (previous-catch :c-type "struct catch_block *")
  tag)

;;;; symbols

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *symbol-primobj-defn-properties*
    '(:lowtag other-pointer-lowtag
      :widetag symbol-widetag
      :alloc-trans %alloc-symbol
      :type symbol)))

#+(and 64-bit (not relocatable-static-space))
(define-primitive-object (symbol . #.*symbol-primobj-defn-properties*)
  ;; Beware when changing this definition.  NIL-the-symbol is defined
  ;; using this layout, and NIL-the-end-of-list-marker is the cons
  ;; ( NIL . NIL ), living in the first two slots of NIL-the-symbol
  ;; (conses have no header).  Careful selection of lowtags ensures
  ;; that the same pointer can be used for both purposes.
  ;; HASH and VALUE are the first two slots.
  ;; Traditionally VALUE was the first slot, corresponding to the CAR of
  ;; NIL-as-end-of-list; and HASH was the second, corresponding to CDR.
  ;; By storing HASH in the word after the object header it becomes a memory-safe
  ;; operation to read SYMBOL-HASH-SLOT from _any_ object whatsoever
  ;; using lisp code equivalent to "native_pointer(ptr)[1]".
  ;; This improves the code for CASE and ECASE over symbols
  ;; regardless of whether the object being tested is known to be a symbol.
  (hash :set-trans %set-symbol-hash)
  (value :init :unbound
         :set-trans %set-symbol-global-value
         :set-known ())

  ;; This slot holds an FDEFN. It's almost unnecessary to have FDEFNs at all
  ;; for symbols. If we ensured that any function bound to a symbol had a
  ;; call convention rendering it callable in the manner of a SIMPLE-FUN,
  ;; then we would only need to store that function's raw entry address here,
  ;; thereby removing the FDEFN for any global symbol. Any closure assigned
  ;; to a symbol would need a tiny trampoline, which is already the case
  ;; for #+immobile-code.
  (fdefn :ref-trans %symbol-fdefn :ref-known ()
         :cas-trans cas-symbol-fdefn)
  ;; The private accessor for INFO reads the slot verbatim.
  ;; In contrast, the SYMBOL-INFO function always returns a PACKED-INFO
  ;; instance (see info-vector.lisp) or NIL. The slot itself may hold a cons
  ;; of the user's PLIST and a PACKED-INFO or just a PACKED-INFO.
  ;; It can't hold a PLIST alone without wrapping in an extra cons cell.
  (info :ref-trans symbol-%info :ref-known (flushable)
        ;; IR2-CONVERT-CASSER only knows the arg order as (OBJECT OLD NEW),
        ;; so as much as I'd like to name this (CAS SYMBOL-%INFO),
        ;; it can't be that, because it'd need args of (OLD NEW OBJECT).
        ;; This is a pretty close approximation of the desired name.
        :cas-trans sb-impl::cas-symbol-%info
        :type (or instance list)
        :init :null)
  (name :init :arg))

;;; 64-bit relocatable-static is a little like 64-bit, a little like 32-bit.
;;; Refer to comments above for details on each slot.
#+(and 64-bit relocatable-static-space)
(define-primitive-object (symbol . #.*symbol-primobj-defn-properties*)
  (fdefn :ref-trans %symbol-fdefn :ref-known () :cas-trans cas-symbol-fdefn)
  (value :init :unbound :set-trans %set-symbol-global-value :set-known ())
  (info :ref-trans symbol-%info :ref-known (flushable)
        :cas-trans sb-impl::cas-symbol-%info
        :type (or instance list)
        :init :null)
  (hash :set-trans %set-symbol-hash)
  (name :init :arg))

#-64-bit
(define-primitive-object (symbol . #.*symbol-primobj-defn-properties*)
  ;; As described in the comments above for #+64-bit, the first two slots of SYMBOL
  ;; have to work for NIL-as-cons, so they have to be NIL and NIL, which have to
  ;; also be the correct value when reading the slot of NIL-as-symbol.
  (fdefn :ref-trans %symbol-fdefn :ref-known () :cas-trans cas-symbol-fdefn)
  (value :init :unbound :set-trans %set-symbol-global-value :set-known ())
  (info :ref-trans symbol-%info :ref-known (flushable)
        :cas-trans sb-impl::cas-symbol-%info
        :type (or instance list)
        :init :null)
  (name :init :arg :ref-trans symbol-name)
  ;; The remaining slots can be ignored by GC
  #+salted-symbol-hash (hash)
  #-salted-symbol-hash (hash :set-trans %set-symbol-hash :ref-trans symbol-hash)
  (package-id :type index ; actually 16 bits. (Could go in the header)
              :ref-trans symbol-package-id
              :set-trans sb-impl::set-symbol-package-id :set-known ())
  ;; 0 tls-index means no tls-index is allocated
  ;; For the 32-bit architectures, reading this slot as a descriptor
  ;; makes it "off" by N-FIXNUM-TAG-BITS, which is bothersome,
  ;; so there's a transform on SYMBOL-TLS-INDEX to make it sane.
  ;; The issue with not having this in the header - supposing we used
  ;; the high 2 bytes for it - is that NIL's 0th word would suggest
  ;; that it has a nonzero TLS index, namely:
  ;; * (sb-vm:hexdump nil)
  ;;   1100008: 0110000B = NIL ; looks like NIL's TLS index is 0x110
  ;;   110000C: 0110000B = NIL
  #+sb-thread
  (tls-index :type (and fixnum unsigned-byte) ; too generous still?
             :ref-known (flushable)
             :ref-trans %symbol-tls-index))

(define-primitive-object (complex-single-float
                          :lowtag other-pointer-lowtag
                          :widetag complex-single-float-widetag)
  #+64-bit
  (data :c-type "struct { float data[2]; } ")
  #-64-bit
  (real :c-type "float")
  #-64-bit
  (imag :c-type "float"))

(define-primitive-object (complex-double-float
                          :lowtag other-pointer-lowtag
                          :widetag complex-double-float-widetag)
  (filler)
  (real :c-type "double" :length #.(/ 64 n-word-bits))
  (imag :c-type "double" :length #.(/ 64 n-word-bits)))

#+sb-simd-pack
(define-primitive-object (simd-pack
                          :lowtag other-pointer-lowtag
                          :widetag simd-pack-widetag)
  (tag :ref-trans %simd-pack-tag
       :attributes (movable flushable)
       :type (unsigned-byte 4))
  (lo-value :c-type "long" :type (unsigned-byte 64))
  (hi-value :c-type "long" :type (unsigned-byte 64)))

#+sb-simd-pack-256
(define-primitive-object (simd-pack-256
                          :lowtag other-pointer-lowtag
                          :widetag simd-pack-256-widetag)
  (tag :ref-trans %simd-pack-256-tag
       :attributes (movable flushable)
       :type (unsigned-byte 4))
  (p0 :c-type "long" :type (unsigned-byte 64))
  (p1 :c-type "long" :type (unsigned-byte 64))
  (p2 :c-type "long" :type (unsigned-byte 64))
  (p3 :c-type "long" :type (unsigned-byte 64)))

;;; Define some slots that precede 'struct thread' so that each may be read
;;; using a small negative 1-byte displacement.
;;; These slots hold frequently-referenced constants.
;;; If we can't do that for some reason - like, say, the safepoint page
;;; is located prior to 'struct thread', then these just become ordinary slots.
(defconstant-eqx +thread-header-slot-names+
    `#(#+x86-64
       ,@'(t-nil-constants
           alien-linkage-table-base
           msan-xor-constant
           ;; The following slot's existence must NOT be conditional on #+msan
           msan-param-tls) ; = &__msan_param_tls
       #+permgen
       ,@'(function-layout)
       #+immobile-space
       ,@'(function-layout
           text-space-addr
           text-card-count
           text-card-marks))
  #'equalp)

(macrolet ((assign-header-slot-indices ()
             (let ((i 0))
               `(progn
                  ,@(map 'list (lambda (x)
                                 `(defconstant ,(symbolicate "THREAD-" x "-SLOT") ,(decf i)))
                            +thread-header-slot-names+)))))
  (assign-header-slot-indices))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; allocator histogram capacity
  (defconstant n-histogram-bins-small 32)
  (defconstant n-histogram-bins-large 32))
;;; the #+allocation-size-histogram has an exact count of objects allocated
;;; for all sizes up to (* cons-size n-word-bytes n-histogram-bins-small).
;;; Larger allocations are grouped by the binary log of the size.
;;; It seems that 99.5% of all allocations are less than the small bucket limit,
;;; making the histogram exact except for the tail.
(defconstant first-large-histogram-bin-log2size
  (integer-length (* n-histogram-bins-small cons-size n-word-bytes)))

;;; this isn't actually a lisp object at all, it's a c structure that lives
;;; in c-land.  However, we need sight of so many parts of it from Lisp that
;;; it makes sense to define it here anyway, so that the GENESIS machinery
;;; can take care of maintaining Lisp and C versions.
(define-primitive-object (thread :size primitive-thread-object-length)
  ;; no_tls_value_marker is borrowed very briefly at thread startup to
  ;; pass the address of the start routine into new_thread_trampoline.
  ;; tls[0] = NO_TLS_VALUE_MARKER because a the tls index slot
  ;; of a symbol is initialized to zero
  (no-tls-value-marker)

  (stepping)

  ;; Keep this first bunch of slots from binding-stack-pointer through alloc-region
  ;; near the beginning of the structure so that x86[-64] assembly code
  ;; can use single-byte displacements from thread-base-tn.
  ;; Doing so reduces code size for allocation sequences and special variable
  ;; manipulations by fixing their TLS offsets to be < 2^7, the largest
  ;; aligned displacement fitting in a signed byte.
  ;;
  (binding-stack-pointer :c-type "lispobj *" :pointer t
                         :special *binding-stack-pointer*)
  ;; next two not used in C, but this wires the TLS offsets to small values
  #+(and (or riscv x86-64 arm64) sb-thread)
  (current-catch-block :special *current-catch-block*)
  #+(and (or riscv x86-64 arm64) sb-thread)
  (current-unwind-protect-block :special *current-unwind-protect-block*)
  #+(or sb-thread sparc ppc)
  (pseudo-atomic-bits #+(or x86 x86-64) :special #+(or x86 x86-64) *pseudo-atomic-bits*
                      :c-type "pa_bits_t")
  (alien-stack-pointer :c-type "lispobj *" :pointer t
                       :special *alien-stack-pointer*)
  ;; Deterministic consing profile recording area.
  (profile-data :c-type "uword_t *" :pointer t)
  ;; Thread-local allocation buffers
  (boxed-tlab :c-type "struct alloc_region" :length 3)
  (cons-tlab :c-type "struct alloc_region" :length 3)
  (mixed-tlab :c-type "struct alloc_region" :length 3)
  ;; END of slots to keep near the beginning.

  ;; This is the original address at which the memory was allocated,
  ;; which may have different alignment then what we prefer to use.
  ;; Kept here so that when the thread dies we can release the whole
  ;; memory we reserved.
  (os-address :c-type "void *" :pointer t)
  ;; Technically this slot violates our requirement that the size of the thread
  ;; primitive object be computable by assuming one word per slot. POSIX says
  ;; "IEEE Std 1003.1-2001/Cor 2-2004, item XBD/TC2/D6/26 is applied,
  ;;  adding pthread_t to the list of types that are not required to be arithmetic
  ;;  types, thus allowing pthread_t to be defined as a structure."
  ;;
  ;; Furthermore, it is technically possibly for a pthread_t to be smaller than a word
  ;; (a 4-byte identifier, not a pointer, on 64-bit) but that seems not to be true
  ;; for any system that we care about.
  (os-thread :c-type #+(or win32 (not sb-thread)) "lispobj" ; actually is HANDLE
                     #-(or win32 (not sb-thread)) "pthread_t")
  (os-kernel-tid) ; the kernel's thread identifier, 32 bits on linux

  ;; These aren't accessed (much) from Lisp, so don't really care
  ;; if it takes a 4-byte displacement.
  (alien-stack-start :c-type "lispobj *" :pointer t)
  (binding-stack-start :c-type "lispobj *" :pointer t
                       :special *binding-stack-start*)

  (control-stack-start :c-type "lispobj *" :pointer t
                       :special *control-stack-start*)
  (control-stack-end :c-type "lispobj *" :pointer t
                     :special *control-stack-end*)
  (this :c-type "struct thread *" :pointer t)
  (prev :c-type "struct thread *" :pointer t)
  (next :c-type "struct thread *" :pointer t)
  ;; a struct containing {starting, running, suspended, dead}
  ;; and some other state fields.
  (state-word :c-type "struct thread_state_word")
  ;; Statistical CPU profiler data recording buffer
  (sprof-data)
  ;;
  (arena)

  #+x86 (tls-cookie)                          ;  LDT index
  #+sb-thread (tls-size)
  ;; For various reasons related to pseudo-atomic and interrupt
  ;; handling, we need to know if the machine context is in Lisp code
  ;; or not.  On non-threaded targets, this is a global variable in
  ;; the runtime, but it's clearly a per-thread value.
  ;; This is a true/false flag. Since there are about 47 different competing
  ;; definitions of the C type "boolean", just use "lispobj" for its known size.
  ;; The name is visually distinct from the global flag of similar purpose
  ;; so that 'grep' can find the one you want more easily.
  #+(and sb-thread (not arm64)) (ffcall-active-p)
  ;; Same as above for the location of the current control stack frame.
  #+(and sb-thread (not (or x86 x86-64)))
  (control-frame-pointer :c-type "lispobj *")
  ;; Same as above for the location of the current control stack
  ;; pointer.  This is also used on threaded x86oids to allow LDB to
  ;; print an approximation of the CSP as needed.
  #+sb-thread
  (control-stack-pointer :c-type "lispobj *")
  (card-table)

  ;; A few extra thread-local allocation buffers for special purposes
  ;; #-sb-thread probably won't use these, to be determined...
  (symbol-tlab :c-type "struct alloc_region" :length 3)
  (sys-mixed-tlab :c-type "struct alloc_region" :length 3)
  (sys-cons-tlab :c-type "struct alloc_region" :length 3)
  (remset)
  ;; allocation instrumenting
  (tot-bytes-alloc-boxed)
  (tot-bytes-alloc-unboxed)
  (slow-path-allocs)
  (et-allocator-mutex-acq) ; elapsed times
  (et-find-freeish-page)
  (et-bzeroing)
  (allocator-histogram :c-type "size_histogram"
                       ;; small bins store just a count
                       ;; large bins store a count and size
                       :length #.(+ (* 2 n-histogram-bins-large)
                                    n-histogram-bins-small))

  ;; The *current-thread* MUST be the last slot in the C thread structure.
  ;; It it the only slot that needs to be noticed by the garbage collector.
  (lisp-thread :pointer t :special sb-thread:*current-thread*))

(defconstant code-header-size-shift #+64-bit 32 #-64-bit n-widetag-bits)
(defconstant-eqx code-serialno-byte
  #+64-bit (byte 32 32) #-64-bit (byte 18 14)
  #'equalp)
(declaim (inline code-object-size code-header-words %code-code-size))
#-sb-xc-host
(progn
  (defun code-object-size (code)
    (declare (code-component code))
    #-64-bit (ash (logand (get-header-data code) #x3FFFFF) word-shift)
    #+64-bit (ash (ash (get-header-data code) -24) word-shift))

  (defun code-header-words (code)
    (declare (code-component code))
    ;; The values stored is an untagged byte count.  If N-FIXNUM-TAG-BITS is the same
    ;; as WORD-SHIFT, then it can be conveniently read as word count with no shifting.
    ;; Putting an upper bound on the word count improves the generated code, no matter
    ;; that it's an excessive bound. 22 bits expresses the maximum object size
    ;; for 32-bit words. The boxed count can't in practice be that large.
    (ldb (byte 22 0) (ash (%code-boxed-size code) (- n-fixnum-tag-bits word-shift))))

  (defun %code-code-size (code)
    (declare (code-component code))
    (- (code-object-size code) (ash (code-header-words code) word-shift)))

  ;; Possibly not the best place for this definition, but other accessors
  ;; for primitive objects are here too.
  (defun code-jump-table-words (code)
    (declare (code-component code))
    (if (eql (code-header-ref code code-boxed-size-slot) 0)
        0
        (with-pinned-objects (code)
          (ldb (byte 14 0) (sap-ref-word (code-instructions code) 0)))))

  ;; Serial# is stored in 18 bits of the first unboxed data word, the same word
  ;; which holds the count of jump table entries. The primary purpose of the serial#
  ;; is to uniquely identify code objects independent of any naming.
  ;; The serial# is globally unique unless the global counter wraps around.
  (defun %code-serialno (code)
    (declare (code-component code))
    (if (eql (code-header-ref code code-boxed-size-slot) 0)
        0
        (with-pinned-objects (code)
          (ldb code-serialno-byte (sap-ref-word (code-instructions code) 0)))))

) ; end PROGN

;;; The offset of NIL in static space, including the tag.
(defconstant nil-value-offset
  (+ ;; Make space for the different regions, if they exist.
     ;; If you change this, then also change zero_all_free_ranges() in
     ;; gencgc.
     #+(and gencgc (not sb-thread) (not 64-bit))
     (* 10 n-word-bytes)
     ;; This offset of #x100 has to do with some edge cases where a vop
     ;; might treat UNBOUND-MARKER as a pointer. So it has an address
     ;; that is somewhere near NIL which makes it sort of "work"
     ;; to dereference it. See git rev f1a956a6a771 for more info.
     #+64-bit #x100
     ;; magic padding because of NIL's symbol/cons-like duality
     (* 2 n-word-bytes)
     list-pointer-lowtag))

;;; The definitions below want to use ALIGN-UP, which is not defined
;;; in time to put these in early-objdef, but it turns out that we don't
;;; need them there.
(#-relocatable-static-space defconstant #+relocatable-static-space define-symbol-macro nil-value (+ static-space-start nil-value-offset))

#+sb-xc-host (defun get-nil-taggedptr () nil-value)

;;; Start of static objects:
;;;
;;;   32-bit w/threads     |   32-bit no threads     |      64-bit
;;;  --------------------  | --------------------    | ---------------------
;;;       padding          |      padding            |      padding
;;;  NIL: header (#x07__)  | NIL: header (#x06__)    | NIL: header (#x05__)
;;;       hash             |      hash               |      hash
;;;       value            |      value              |      value
;;;       info             |      info               |      info
;;;       name             |      name               |      name
;;;       fdefn            |      fdefn              |      fdefn
;;;       package          |      package            |      (unused)
;;;       tls_index        |   T: header             |   T: header
;;;       (unused)         |                         |
;;;    T: header           |                         |
;;;  -------------------   | --------------------    | ---------------------
;;;    SYMBOL_SIZE=8       |   SYMBOL_SIZE=7         |   SYMBOL_SIZE=6
;;;    NIL is 10 words     |   NIL is 8 words        |   NIL is 8 words

;;; This constant is the address at which to scan NIL as a root.
;;; To ensure that scav_symbol is invoked, we have to see the widetag
;;; in the 0th word, which is at 1 word prior to the word containing the CAR of
;;; nil-as-a-list. So subtract the lowtag and then go back one more word.
;;; This address is NOT double-lispword-aligned, but the scavenge method
;;; does not assert that.
(defconstant nil-symbol-slots-offset
  (- nil-value-offset list-pointer-lowtag n-word-bytes))

;;; NIL as a symbol contains the usual number of words for a symbol,
;;; aligned to a double-lispword. This will NOT end at a double-lispword boundary.
;;; In all 3 scenarios depicted above, the number of slots that the 'scav' function
;;; returns suggests that it would examine the header word of the *next* symbol.
;;; But it does not, because it confines itself to looking only at the number of
;;; words indicated in the symbol header of NIL. But we have to pass in the aligned
;;; count because of the assertion in heap_scavenge that the scan ends as expected,
;;; and scavenge methods must return an even number because nothing can be smaller
;;; than 1 cons cell or not a multiple thereof.
(defconstant nil-symbol-slots-end-offset
  (+ nil-symbol-slots-offset
     (ash (align-up symbol-size 2) word-shift)))

;;; This constant is the number of words to report that NIL consumes
;;; when Lisp asks for its primitive-object-size. So we say that it consumes
;;; all words from the start of static-space objects up to the next object.
(defconstant sizeof-nil-in-words (+ 2 (sb-int:align-up (1- symbol-size) 2)))

;;; Address at which to start scanning static symbols when heap-walking.
;;; Basically skip over MIXED-REGION (if it's in static space) and NIL.
;;; Or: go to NIL's header word, subtract 1 word, and add in the physical
;;; size of NIL in bytes that we report for primitive-object-size.
(defconstant static-space-objects-offset
  (+ nil-symbol-slots-offset
     (ash (1- sizeof-nil-in-words) word-shift)))

(defconstant lockfree-list-tail-value-offset
  (+ static-space-objects-offset
     (* (length +static-symbols+) (ash (align-up symbol-size 2) word-shift))
     instance-pointer-lowtag))
