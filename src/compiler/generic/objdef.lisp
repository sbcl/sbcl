;;;; machine-independent aspects of the object representation

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

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
;;;;     to be GCed, you need to tweak scav_code_header() and
;;;;     verify_space() in gencgc.c, and the corresponding code in gc.c.
;;;;   * Various code (e.g. STATIC-FSET in genesis.lisp) is hard-wired
;;;;     to know the name of the last slot of the object the code works
;;;;     with, and implicitly to know that the last slot is special (being
;;;;     the beginning of an arbitrary-length sequence of bytes following
;;;;     the fixed-layout slots).
;;;; -- WHN 2001-12-29

;;;; the primitive objects themselves

(!define-primitive-object (cons :type cons
                               :lowtag list-pointer-lowtag
                               :alloc-trans cons)
  (car :ref-trans car :set-trans sb!c::%rplaca :init :arg
       :cas-trans %compare-and-swap-car)
  (cdr :ref-trans cdr :set-trans sb!c::%rplacd :init :arg
       :cas-trans %compare-and-swap-cdr))

(!define-primitive-object (instance :lowtag instance-pointer-lowtag
                                   :widetag instance-widetag
                                   :alloc-trans %make-instance)
  (slots :rest-p t))

(!define-primitive-object (bignum :lowtag other-pointer-lowtag
                                 :widetag bignum-widetag
                                 :alloc-trans sb!bignum::%allocate-bignum)
  (digits :rest-p t :c-type #!-alpha "sword_t" #!+alpha "u32"))

(!define-primitive-object (ratio :type ratio
                                :lowtag other-pointer-lowtag
                                :widetag ratio-widetag
                                :alloc-trans %make-ratio)
  (numerator :type integer
             :ref-known (flushable movable)
             :ref-trans %numerator
             :init :arg)
  (denominator :type integer
               :ref-known (flushable movable)
               :ref-trans %denominator
               :init :arg))

#!-64-bit
(!define-primitive-object (single-float :lowtag other-pointer-lowtag
                                       :widetag single-float-widetag)
  (value :c-type "float"))

(!define-primitive-object (double-float :lowtag other-pointer-lowtag
                                       :widetag double-float-widetag)
  #!-64-bit (filler)
  (value :c-type "double" :length #.(/ 64 n-word-bits)))

#!+long-float
(!define-primitive-object (long-float :lowtag other-pointer-lowtag
                                     :widetag long-float-widetag)
  #!+sparc (filler)
  (value :c-type "long double" :length #!+x86 3 #!+sparc 4))

(!define-primitive-object (complex :type complex
                                  :lowtag other-pointer-lowtag
                                  :widetag complex-widetag
                                  :alloc-trans %make-complex)
  (real :type real
        :ref-known (flushable movable)
        :ref-trans %realpart
        :init :arg)
  (imag :type real
        :ref-known (flushable movable)
        :ref-trans %imagpart
        :init :arg))

(!define-primitive-object (array :lowtag other-pointer-lowtag
                                :widetag t)
  ;; FILL-POINTER of an ARRAY is in the same place as LENGTH of a
  ;; VECTOR -- see SHRINK-VECTOR.
  (fill-pointer :type index
                :ref-trans %array-fill-pointer
                :ref-known (flushable foldable)
                :set-trans (setf %array-fill-pointer)
                :set-known ())
  (fill-pointer-p :type (member t nil)
                  :ref-trans %array-fill-pointer-p
                  :ref-known (flushable foldable)
                  :set-trans (setf %array-fill-pointer-p)
                  :set-known ())
  (elements :type index
            :ref-trans %array-available-elements
            :ref-known (flushable foldable)
            :set-trans (setf %array-available-elements)
            :set-known ())
  (data :type array
        :ref-trans %array-data ; might be a vector, might not be
        :ref-known (flushable foldable)
        :set-trans (setf %array-data)
        :set-known ())
  (displacement :type index
                :ref-trans %array-displacement
                :ref-known (flushable foldable)
                :set-trans (setf %array-displacement)
                :set-known ())
  (displaced-p :type t
               :ref-trans %array-displaced-p
               :ref-known (flushable foldable)
               :set-trans (setf %array-displaced-p)
               :set-known ())
  (displaced-from :type list
                  :ref-trans %array-displaced-from
                  :ref-known (flushable)
                  :set-trans (setf %array-displaced-from)
                  :set-known ())
  (dimensions :rest-p t))

(!define-primitive-object (vector :type vector
                                 :lowtag other-pointer-lowtag
                                 :widetag t)
  ;; FILL-POINTER of an ARRAY is in the same place as LENGTH of a
  ;; VECTOR -- see SHRINK-VECTOR.
  (length :ref-trans sb!c::vector-length
          :type index)
  (data :rest-p t :c-type #!-alpha "uword_t" #!+alpha "u32"))

;;; The header contains the size of slots and constants in words.
(!define-primitive-object (code :type code-component
                                :lowtag other-pointer-lowtag
                                :widetag code-header-widetag)
  ;; This is the size of instructions in bytes, not aligned.
  ;; Adding the size from the header and aligned code-size will yield
  ;; the total size of the code-object.
  ;; The upper 4 bytes in 8-byte words can be used for ancillary data.
  (code-size :type index
             :ref-known (flushable movable)
             :ref-trans #!+64-bit %%code-code-size ; serialno + size
                        #!-64-bit %code-code-size)
  (debug-info :type t
              :ref-known (flushable)
              :ref-trans %code-debug-info
              :set-known ()
              :set-trans (setf %code-debug-info))
  #!+(or x86 immobile-space)
  (fixups :type t
          :ref-known (flushable)
          :ref-trans %code-fixups
          :set-known ()
          :set-trans (setf %code-fixups))
  (constants :rest-p t))

(!define-primitive-object (fdefn :type fdefn
                                :lowtag other-pointer-lowtag
                                :widetag fdefn-widetag)
  (name :ref-trans fdefn-name
        :set-trans %set-fdefn-name :set-known ())
  (fun :type (or function null) :ref-trans fdefn-fun)
  ;; raw-addr is used differently by the various backends:
  ;; - Sparc and ARM store the same object as 'fun'
  ;;   unless the function is non-simple, in which case
  ;;   they store a descriptorized (fun-pointer lowtag)
  ;;   pointer to the closure tramp
  ;; - x86-64 with immobile-code feature stores a JMP instruction
  ;;   to the function entry address. Special considerations
  ;;   pertain to undefined functions, FINs, and closures.
  ;; - all others store a native pointer to the function entry address
  ;;   or closure tramp
  (raw-addr :c-type #!-alpha "char *" #!+alpha "u32"))

;;; a simple function (as opposed to hairier things like closures
;;; which are also subtypes of Common Lisp's FUNCTION type)
(!define-primitive-object (simple-fun :type function
                                     :lowtag fun-pointer-lowtag
                                     :widetag simple-fun-widetag)
  ;; All three function primitive-objects have the first word after the header
  ;; as some kind of entry point, either the address to jump to, in the case
  ;; of x86, or the Lisp function to jump to, for everybody else.
  (self :set-known ()
        :set-trans (setf %simple-fun-self))
  (name :ref-known (flushable)
        :ref-trans %simple-fun-name
        :set-known ()
        :set-trans (setf %simple-fun-name))
  (arglist :type list
           :ref-known (flushable)
           :ref-trans %simple-fun-arglist
           :set-known ()
           :set-trans (setf %simple-fun-arglist))
  (type :ref-known (flushable)
        ;; %%SIMPLE-FUN-TYPE is used only by %SIMPLE-FUN-TYPE.
        ;; Nobody should care that %SIMPLE-FUN-TYPE isn't open-coded.
        :ref-trans %%simple-fun-type
        :set-known ()
        :set-trans (setf %simple-fun-type))
  ;; NIL for empty, STRING for a docstring, SIMPLE-VECTOR for XREFS, and (CONS
  ;; STRING SIMPLE-VECTOR) for both.
  (info :init :null
        :ref-trans %simple-fun-info
        :ref-known (flushable)
        :set-trans (setf %simple-fun-info)
        :set-known ())
  (code :rest-p t :c-type "unsigned char"))

#!-(or x86 x86-64)
(!define-primitive-object (return-pc :lowtag other-pointer-lowtag :widetag t)
  (return-point :c-type "unsigned char" :rest-p t))

(!define-primitive-object (closure :lowtag fun-pointer-lowtag
                                   :widetag closure-widetag
                                   ;; This allocator is %COPY-foo because it's only
                                   ;; used when renaming a closure. The compiler has
                                   ;; its own way of making closures, which requires
                                   ;; that the length be a compile-time constant.
                                   :alloc-trans %copy-closure)
  (fun :init :arg :ref-trans #!+(or x86 x86-64) %closure-callee
                             #!-(or x86 x86-64) %closure-fun)
  (info :rest-p t))

(!define-primitive-object (funcallable-instance
                          :lowtag fun-pointer-lowtag
                          :widetag funcallable-instance-widetag
                          :alloc-trans %make-funcallable-instance)
  (trampoline :init :funcallable-instance-tramp)
  ;; TODO: if we can switch places of 'function' and 'fsc-instance-slots'
  ;; (at least for the builds with compact-instance-header)
  ;; then for both funcallable and non-funcallable instances,
  ;; the CLOS slot vector will be in the word 5 bytes past the tagged pointer.
  ;; This shouldn't be too hard to arrange, since nothing needs to know where
  ;; the tagged function lives except the funcallable instance trampoline.
  (function :ref-known (flushable) :ref-trans %funcallable-instance-function
            :set-known () :set-trans (setf %funcallable-instance-function))
  (info :rest-p t))

(!define-primitive-object (value-cell :lowtag other-pointer-lowtag
                                     :widetag value-cell-widetag
                                     ;; FIXME: We also have an explicit VOP
                                     ;; for this. Is this needed as well?
                                     :alloc-trans make-value-cell)
  (value :set-trans value-cell-set
         :set-known ()
         :ref-trans value-cell-ref
         :ref-known (flushable)
         :init :arg))

(!define-primitive-object (sap :lowtag other-pointer-lowtag
                              :widetag sap-widetag)
  (pointer :c-type "char *" :pointer t))


(!define-primitive-object (weak-pointer :type weak-pointer
                                       :lowtag other-pointer-lowtag
                                       :widetag weak-pointer-widetag
                                       :alloc-trans make-weak-pointer)
  (value :ref-trans %weak-pointer-value :ref-known (flushable)
         :init :arg)
  (next :c-type #!-alpha "struct weak_pointer *" #!+alpha "u32"))

;;;; other non-heap data blocks

(!define-primitive-object (binding)
  value
  symbol) ;; on sb-thread, this is actually a tls-index

(!define-primitive-object (unwind-block)
  (uwp :c-type #!-alpha "struct unwind_block *" #!+alpha "u32")
  (cfp :c-type #!-alpha "lispobj *" #!+alpha "u32")
  #!-(or x86 x86-64) code
  entry-pc
  #!+(and win32 x86) next-seh-frame
  #!+(and win32 x86) seh-frame-handler
  #!+x86-64 bsp
  #!+x86-64
  current-catch)

(!define-primitive-object (catch-block)
  (uwp :c-type #!-alpha "struct unwind_block *" #!+alpha "u32")
  (cfp :c-type #!-alpha "lispobj *" #!+alpha "u32")
  #!-(or x86 x86-64) code
  entry-pc
  #!+(and win32 x86) next-seh-frame
  #!+(and win32 x86) seh-frame-handler
  #!+x86-64 bsp
  (previous-catch :c-type #!-alpha "struct catch_block *" #!+alpha "u32")
  tag)

;;;; symbols

(!define-primitive-object (symbol :lowtag other-pointer-lowtag
                                 :widetag symbol-widetag
                                 :alloc-trans %%make-symbol
                                 :type symbol)

  ;; Beware when changing this definition.  NIL-the-symbol is defined
  ;; using this layout, and NIL-the-end-of-list-marker is the cons
  ;; ( NIL . NIL ), living in the first two slots of NIL-the-symbol
  ;; (conses have no header).  Careful selection of lowtags ensures
  ;; that the same pointer can be used for both purposes:
  ;; OTHER-POINTER-LOWTAG is 7, LIST-POINTER-LOWTAG is 3, so if you
  ;; subtract 3 from (SB-KERNEL:GET-LISP-OBJ-ADDRESS 'NIL) you get the
  ;; first data slot, and if you subtract 7 you get a symbol header.

  ;; also the CAR of NIL-as-end-of-list
  (value :init :unbound
         :set-trans %set-symbol-global-value
         :set-known ())
  ;; also the CDR of NIL-as-end-of-list.  Its reffer needs special
  ;; care for this reason, as hash values must be fixnums.
  (hash :set-trans %set-symbol-hash)

  (info :ref-trans symbol-info :ref-known (flushable)
        :set-trans (setf symbol-info)
        :set-known ()
        :cas-trans %compare-and-swap-symbol-info
        :type (or simple-vector list)
        :init :null)
  (name :ref-trans symbol-name :init :arg)
  (package :ref-trans symbol-package
           :set-trans %set-symbol-package
           :init :null)
  ;; 0 tls-index means no tls-index is allocated
  ;; 64-bit put the tls-index in the header word.
  #!+(and sb-thread (not 64-bit))
  (tls-index :ref-known (flushable) :ref-trans symbol-tls-index))

(!define-primitive-object (complex-single-float
                          :lowtag other-pointer-lowtag
                          :widetag complex-single-float-widetag)
  #!+64-bit
  (data :c-type "struct { float data[2]; } ")
  #!-64-bit
  (real :c-type "float")
  #!-64-bit
  (imag :c-type "float"))

(!define-primitive-object (complex-double-float
                          :lowtag other-pointer-lowtag
                          :widetag complex-double-float-widetag)
  (filler)
  (real :c-type "double" :length #.(/ 64 n-word-bits))
  (imag :c-type "double" :length #.(/ 64 n-word-bits)))

#!+sb-simd-pack
(!define-primitive-object (simd-pack
                          :lowtag other-pointer-lowtag
                          :widetag simd-pack-widetag)
  (tag :ref-trans %simd-pack-tag
       :attributes (movable flushable)
       :type fixnum)
  (lo-value :c-type "long" :type (unsigned-byte 64))
  (hi-value :c-type "long" :type (unsigned-byte 64)))

;;; this isn't actually a lisp object at all, it's a c structure that lives
;;; in c-land.  However, we need sight of so many parts of it from Lisp that
;;; it makes sense to define it here anyway, so that the GENESIS machinery
;;; can take care of maintaining Lisp and C versions.
(!define-primitive-object (thread :size primitive-thread-object-length)
  ;; no_tls_value_marker is borrowed very briefly at thread startup to
  ;; pass the address of initial-function into new_thread_trampoline.
  ;; tls[0] = NO_TLS_VALUE_MARKER_WIDETAG because a the tls index slot
  ;; of a symbol is initialized to zero
  (no-tls-value-marker)
  (os-thread :c-type "os_thread_t")
  ;; This is the original address at which the memory was allocated,
  ;; which may have different alignment then what we prefer to use.
  ;; Kept here so that when the thread dies we can release the whole
  ;; memory we reserved.
  (os-address :c-type "void *" :pointer t)

  ;; Keep these next six slots (alloc-region being figured in as 1 slot)
  ;; near the beginning of the structure so that x86[-64] assembly code
  ;; can use single-byte displacements from thread-base-tn.
  ;; Doing so reduces code size for allocation sequences and special variable
  ;; manipulations by fixing their TLS offsets to be < 2^7, the largest
  ;; aligned displacement fitting in a signed byte.
  ;;
  ;; Information for constructing deterministic consing profile.
  (profile-data :c-type "uword_t *" :pointer t)
  #!+gencgc (alloc-region :c-type "struct alloc_region" :length 4)
  #!+sb-thread (pseudo-atomic-bits #!+(or x86 x86-64) :special #!+(or x86 x86-64) *pseudo-atomic-bits*)
  ;; next two not used in C, but this wires the TLS offsets to small values
  #!+(and x86-64 sb-thread)
  (current-catch-block :special *current-catch-block*)
  #!+(and x86-64 sb-thread)
  (current-unwind-protect-block :special *current-unwind-protect-block*)
  (alien-stack-pointer :c-type "lispobj *" :pointer t
                       :special *alien-stack-pointer*)
  (binding-stack-pointer :c-type "lispobj *" :pointer t
                         :special *binding-stack-pointer*)
  (stepping)
  ;; END of slots to keep near the beginning.

  ;; TODO: these slots should be accessible using (SIGNED-BYTE 8) displacement
  ;; from the thread base. We've nearly exhausted small positive indices
  ;; so the slots will have to precede 'struct thread' in memory.
  (varyobj-space-addr)
  (varyobj-card-count)
  (varyobj-card-marks)
  (dynspace-addr)
  (dynspace-card-count)
  (dynspace-pte-base)

  ;; These aren't accessed (much) from Lisp, so don't really care
  ;; if it takes a 4-byte displacement.
  (alien-stack-start :c-type "lispobj *" :pointer t)
  (binding-stack-start :c-type "lispobj *" :pointer t
                       :special *binding-stack-start*)

  #!+(and sb-thread (not sb-safepoint))
  (state-sem :c-type "os_sem_t *" :pointer t)
  #!+(and sb-thread (not sb-safepoint))
  (state-not-running-sem :c-type "os_sem_t *" :pointer t)
  #!+(and sb-thread (not sb-safepoint))
  (state-not-running-waitcount :c-type "int" :length 1)
  #!+(and sb-thread (not sb-safepoint))
  (state-not-stopped-sem :c-type "os_sem_t *" :pointer t)
  #!+(and sb-thread (not sb-safepoint))
  (state-not-stopped-waitcount :c-type "int" :length 1)
  (control-stack-start :c-type "lispobj *" :pointer t
                       :special *control-stack-start*)
  (control-stack-end :c-type "lispobj *" :pointer t
                     :special *control-stack-end*)
  (control-stack-guard-page-protected)
  #!+win32 (private-events :c-type "struct private_events" :length 2)
  (this :c-type "struct thread *" :pointer t)
  (prev :c-type "struct thread *" :pointer t)
  (next :c-type "struct thread *" :pointer t)
  ;; starting, running, suspended, dead
  (state :c-type "lispobj")

  #!+x86 (tls-cookie)                          ;  LDT index
  (interrupt-data :c-type "struct interrupt_data *"
                  :pointer t)
  ;; For various reasons related to pseudo-atomic and interrupt
  ;; handling, we need to know if the machine context is in Lisp code
  ;; or not.  On non-threaded targets, this is a global variable in
  ;; the runtime, but it's clearly a per-thread value.
  #!+sb-thread
  (foreign-function-call-active :c-type "boolean")
  ;; Same as above for the location of the current control stack frame.
  #!+(and sb-thread (not (or x86 x86-64)))
  (control-frame-pointer :c-type "lispobj *")
  ;; Same as above for the location of the current control stack
  ;; pointer.  This is also used on threaded x86oids to allow LDB to
  ;; print an approximation of the CSP as needed.
  #!+sb-thread
  (control-stack-pointer :c-type "lispobj *")
  #!+mach-exception-handler
  (mach-port-name :c-type "mach_port_name_t")
  ;; Context base pointer for running on top of system libraries built using
  ;; -fomit-frame-pointer.  Currently truly required and implemented only
  ;; for (and win32 x86-64), but could be generalized to other platforms if
  ;; needed:
  #!+win32 (carried-base-pointer :c-type "os_context_register_t")
  #!+sb-safepoint (csp-around-foreign-call :c-type "lispobj *")
  #!+win32 (synchronous-io-handle-and-flag :c-type "HANDLE" :length 1)
  #!+(and sb-safepoint-strictly (not win32))
  (sprof-alloc-region :c-type "struct alloc_region" :length 4)
  ;; The following slot's existence must NOT be conditional on #+msan
  #!+x86-64 (msan-param-tls) ; = &__msan_param_tls
  ;; function-layout is needed for closure creation. it's constant,
  ;; but we need somewhere to read it from.
  #!+(and immobile-space 64-bit sb-thread) (function-layout))

;;; Compute the smallest TLS index that will be assigned to a special variable
;;; that does not map onto a thread slot.
;;; Given N thread slots, the tls indices are 0..N-1 scaled by word-shift.
;;; This constant is the index prior to scaling.
(defconstant sb!thread::tls-index-start primitive-thread-object-length)

(defmacro make-code-header-word (boxed-nwords)
  `(logior (ash ,boxed-nwords #!+64-bit 32 #!-64-bit n-widetag-bits)
           code-header-widetag))
