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
;;;;   * The src/runtime/print.c code (used by LDB) is implemented
;;;;     using hand-written lists of slot names, which aren't automatically
;;;;     generated from the code in this file.
;;;;   * Various code (e.g. STATIC-FSET in genesis.lisp) is hard-wired
;;;;     to know the name of the last slot of the object the code works
;;;;     with, and implicitly to know that the last slot is special (being
;;;;     the beginning of an arbitrary-length sequence of bytes following
;;;;     the fixed-layout slots).
;;;; -- WHN 2001-12-29

;;;; the primitive objects themselves

(define-primitive-object (cons :lowtag list-pointer-lowtag
                               :alloc-trans cons)
  (car :ref-trans car :set-trans sb!c::%rplaca :init :arg
       :cas-trans %compare-and-swap-car)
  (cdr :ref-trans cdr :set-trans sb!c::%rplacd :init :arg
       :cas-trans %compare-and-swap-cdr))

(define-primitive-object (instance :lowtag instance-pointer-lowtag
                                   :widetag instance-header-widetag
                                   :alloc-trans %make-instance)
  (slots :rest-p t))

(define-primitive-object (bignum :lowtag other-pointer-lowtag
                                 :widetag bignum-widetag
                                 :alloc-trans sb!bignum::%allocate-bignum)
  (digits :rest-p t :c-type #!-alpha "long" #!+alpha "u32"))

(define-primitive-object (ratio :type ratio
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

#!+#.(cl:if (cl:= sb!vm:n-word-bits 32) '(and) '(or))
(define-primitive-object (single-float :lowtag other-pointer-lowtag
                                       :widetag single-float-widetag)
  (value :c-type "float"))

(define-primitive-object (double-float :lowtag other-pointer-lowtag
                                       :widetag double-float-widetag)
  #!-x86-64 (filler)
  (value :c-type "double" :length #!-x86-64 2 #!+x86-64 1))

#!+long-float
(define-primitive-object (long-float :lowtag other-pointer-lowtag
                                     :widetag long-float-widetag)
  #!+sparc (filler)
  (value :c-type "long double" :length #!+x86 3 #!+sparc 4))

(define-primitive-object (complex :type complex
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

(define-primitive-object (array :lowtag other-pointer-lowtag
                                :widetag t)
  ;; FILL-POINTER of an ARRAY is in the same place as LENGTH of a
  ;; VECTOR -- see SHRINK-VECTOR.
  (fill-pointer :type index
                :ref-trans %array-fill-pointer
                :ref-known (flushable foldable)
                :set-trans (setf %array-fill-pointer)
                :set-known (unsafe))
  (fill-pointer-p :type (member t nil)
                  :ref-trans %array-fill-pointer-p
                  :ref-known (flushable foldable)
                  :set-trans (setf %array-fill-pointer-p)
                  :set-known (unsafe))
  (elements :type index
            :ref-trans %array-available-elements
            :ref-known (flushable foldable)
            :set-trans (setf %array-available-elements)
            :set-known (unsafe))
  (data :type array
        :ref-trans %array-data-vector
        :ref-known (flushable foldable)
        :set-trans (setf %array-data-vector)
        :set-known (unsafe))
  (displacement :type (or index null)
                :ref-trans %array-displacement
                :ref-known (flushable foldable)
                :set-trans (setf %array-displacement)
                :set-known (unsafe))
  (displaced-p :type t
               :ref-trans %array-displaced-p
               :ref-known (flushable foldable)
               :set-trans (setf %array-displaced-p)
               :set-known (unsafe))
  (displaced-from :type list
                  :ref-trans %array-displaced-from
                  :ref-known (flushable)
                  :set-trans (setf %array-displaced-from)
                  :set-known (unsafe))
  (dimensions :rest-p t))

(define-primitive-object (vector :type vector
                                 :lowtag other-pointer-lowtag
                                 :widetag t)
  ;; FILL-POINTER of an ARRAY is in the same place as LENGTH of a
  ;; VECTOR -- see SHRINK-VECTOR.
  (length :ref-trans sb!c::vector-length
          :type index)
  (data :rest-p t :c-type #!-alpha "unsigned long" #!+alpha "u32"))

(define-primitive-object (code :type code-component
                               :lowtag other-pointer-lowtag
                               :widetag t)
  (code-size :type index
             :ref-known (flushable movable)
             :ref-trans %code-code-size)
  (entry-points :type (or function null)
                :ref-known (flushable)
                :ref-trans %code-entry-points
                :set-known (unsafe)
                :set-trans (setf %code-entry-points))
  (debug-info :type t
              :ref-known (flushable)
              :ref-trans %code-debug-info
              :set-known (unsafe)
              :set-trans (setf %code-debug-info))
  (trace-table-offset)
  (constants :rest-p t))

(define-primitive-object (fdefn :type fdefn
                                :lowtag other-pointer-lowtag
                                :widetag fdefn-widetag)
  (name :ref-trans fdefn-name)
  (fun :type (or function null) :ref-trans fdefn-fun)
  (raw-addr :c-type #!-alpha "char *" #!+alpha "u32"))

;;; a simple function (as opposed to hairier things like closures
;;; which are also subtypes of Common Lisp's FUNCTION type)
(define-primitive-object (simple-fun :type function
                                     :lowtag fun-pointer-lowtag
                                     :widetag simple-fun-header-widetag)
  #!-(or x86 x86-64) (self :ref-trans %simple-fun-self
               :set-trans (setf %simple-fun-self))
  #!+(or x86 x86-64) (self
          ;; KLUDGE: There's no :SET-KNOWN, :SET-TRANS, :REF-KNOWN, or
          ;; :REF-TRANS here in this case. Instead, there's separate
          ;; DEFKNOWN/DEFINE-VOP/DEFTRANSFORM stuff in
          ;; compiler/x86/system.lisp to define and declare them by
          ;; hand. I don't know why this is, but that's (basically)
          ;; the way it was done in CMU CL, and it works. (It's not
          ;; exactly the same way it was done in CMU CL in that CMU
          ;; CL's allows duplicate DEFKNOWNs, blithely overwriting any
          ;; previous data associated with the previous DEFKNOWN, and
          ;; that property was used to mask the definitions here. In
          ;; SBCL as of 0.6.12.64 that's not allowed -- too confusing!
          ;; -- so we have to explicitly suppress the DEFKNOWNish
          ;; stuff here in order to allow this old hack to work in the
          ;; new world. -- WHN 2001-08-82
          )
  (next :type (or function null)
        :ref-known (flushable)
        :ref-trans %simple-fun-next
        :set-known (unsafe)
        :set-trans (setf %simple-fun-next))
  (name :ref-known (flushable)
        :ref-trans %simple-fun-name
        :set-known (unsafe)
        :set-trans (setf %simple-fun-name))
  (arglist :type list
           :ref-known (flushable)
           :ref-trans %simple-fun-arglist
           :set-known (unsafe)
           :set-trans (setf %simple-fun-arglist))
  (type :ref-known (flushable)
        :ref-trans %simple-fun-type
        :set-known (unsafe)
        :set-trans (setf %simple-fun-type))
  ;; NIL for empty, STRING for a docstring, SIMPLE-VECTOR for XREFS, and (CONS
  ;; STRING SIMPLE-VECTOR) for both.
  (info :init :null
        :ref-trans %simple-fun-info
        :ref-known (flushable)
        :set-trans (setf %simple-fun-info)
        :set-known (unsafe))
  ;; the SB!C::DEBUG-FUN object corresponding to this object, or NIL for none
  #+nil ; FIXME: doesn't work (gotcha, lowly maintenoid!) See notes on bug 137.
  (debug-fun :ref-known (flushable)
             :ref-trans %simple-fun-debug-fun
             :set-known (unsafe)
             :set-trans (setf %simple-fun-debug-fun))
  (code :rest-p t :c-type "unsigned char"))

(define-primitive-object (return-pc :lowtag other-pointer-lowtag :widetag t)
  (return-point :c-type "unsigned char" :rest-p t))

(define-primitive-object (closure :lowtag fun-pointer-lowtag
                                  :widetag closure-header-widetag)
  (fun :init :arg :ref-trans %closure-fun)
  (info :rest-p t))

(define-primitive-object (funcallable-instance
                          :lowtag fun-pointer-lowtag
                          :widetag funcallable-instance-header-widetag
                          :alloc-trans %make-funcallable-instance)
  (trampoline :init :funcallable-instance-tramp)
  (function :ref-known (flushable) :ref-trans %funcallable-instance-function
            :set-known (unsafe) :set-trans (setf %funcallable-instance-function))
  (info :rest-p t))

(define-primitive-object (value-cell :lowtag other-pointer-lowtag
                                     :widetag value-cell-header-widetag
                                     ;; FIXME: We also have an explicit VOP
                                     ;; for this. Is this needed as well?
                                     :alloc-trans make-value-cell)
  (value :set-trans value-cell-set
         :set-known (unsafe)
         :ref-trans value-cell-ref
         :ref-known (flushable)
         :init :arg))

#!+alpha
(define-primitive-object (sap :lowtag other-pointer-lowtag
                              :widetag sap-widetag)
  (padding)
  (pointer :c-type "char *" :length 2))

#!-alpha
(define-primitive-object (sap :lowtag other-pointer-lowtag
                              :widetag sap-widetag)
  (pointer :c-type "char *"))


(define-primitive-object (weak-pointer :type weak-pointer
                                       :lowtag other-pointer-lowtag
                                       :widetag weak-pointer-widetag
                                       :alloc-trans make-weak-pointer)
  (value :ref-trans sb!c::%weak-pointer-value :ref-known (flushable)
         :init :arg)
  (broken :type (member t nil)
          :ref-trans sb!c::%weak-pointer-broken :ref-known (flushable)
          :init :null)
  (next :c-type #!-alpha "struct weak_pointer *" #!+alpha "u32"))

;;;; other non-heap data blocks

(define-primitive-object (binding)
  value
  symbol)

(define-primitive-object (unwind-block)
  (current-uwp :c-type #!-alpha "struct unwind_block *" #!+alpha "u32")
  (current-cont :c-type #!-alpha "lispobj *" #!+alpha "u32")
  #!-(or x86 x86-64) current-code
  entry-pc
  #!+win32 next-seh-frame
  #!+win32 seh-frame-handler)

(define-primitive-object (catch-block)
  (current-uwp :c-type #!-alpha "struct unwind_block *" #!+alpha "u32")
  (current-cont :c-type #!-alpha "lispobj *" #!+alpha "u32")
  #!-(or x86 x86-64) current-code
  entry-pc
  #!+win32 next-seh-frame
  #!+win32 seh-frame-handler
  tag
  (previous-catch :c-type #!-alpha "struct catch_block *" #!+alpha "u32"))

;;; (For an explanation of this, see the comments at the definition of
;;; KLUDGE-NONDETERMINISTIC-CATCH-BLOCK-SIZE.)
(aver (= kludge-nondeterministic-catch-block-size catch-block-size))

;;;; symbols

(define-primitive-object (symbol :lowtag other-pointer-lowtag
                                 :widetag symbol-header-widetag
                                 :alloc-trans %make-symbol)

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
         :set-known (unsafe))
  ;; also the CDR of NIL-as-end-of-list.  Its reffer needs special
  ;; care for this reason, as hash values must be fixnums.
  (hash :set-trans %set-symbol-hash)

  (plist :ref-trans symbol-plist
         :set-trans %set-symbol-plist
         :cas-trans %compare-and-swap-symbol-plist
         :type list
         :init :null)
  (name :ref-trans symbol-name :init :arg)
  (package :ref-trans symbol-package
           :set-trans %set-symbol-package
           :init :null)
  #!+sb-thread (tls-index :ref-known (flushable) :ref-trans symbol-tls-index))

(define-primitive-object (complex-single-float
                          :lowtag other-pointer-lowtag
                          :widetag complex-single-float-widetag)
  #!+x86-64
  (data :c-type "struct { float data[2]; } ")
  #!-x86-64
  (real :c-type "float")
  #!-x86-64
  (imag :c-type "float"))

(define-primitive-object (complex-double-float
                          :lowtag other-pointer-lowtag
                          :widetag complex-double-float-widetag)
  (filler)
  (real :c-type "double" :length #!-x86-64 2 #!+x86-64 1)
  (imag :c-type "double" :length #!-x86-64 2 #!+x86-64 1))

#!+(and sb-lutex sb-thread)
(define-primitive-object (lutex
                          :lowtag other-pointer-lowtag
                          :widetag lutex-widetag
                          :alloc-trans %make-lutex)
  (gen :c-type "long" :length 1)
  (live :c-type "long" :length 1)
  (next :c-type "struct lutex *" :length 1)
  (prev :c-type "struct lutex *" :length 1)
  (mutex :c-type "pthread_mutex_t *"
         :length 1)
  (mutexattr :c-type "pthread_mutexattr_t *"
             :length 1)
  (condition-variable :c-type "pthread_cond_t *"
                      :length 1))

;;; this isn't actually a lisp object at all, it's a c structure that lives
;;; in c-land.  However, we need sight of so many parts of it from Lisp that
;;; it makes sense to define it here anyway, so that the GENESIS machinery
;;; can take care of maintaining Lisp and C versions.
;;; Hence the even-fixnum lowtag just so we don't get odd(sic) numbers
;;; added to the slot offsets
(define-primitive-object (thread :lowtag even-fixnum-lowtag)
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
  (os-address :c-type "void *" :length #!+alpha 2 #!-alpha 1)
  #!+sb-thread
  (os-attr :c-type "pthread_attr_t *" :length #!+alpha 2 #!-alpha 1)
  #!+sb-thread
  (state-lock :c-type "pthread_mutex_t *" :length #!+alpha 2 #!-alpha 1)
  #!+sb-thread
  (state-cond :c-type "pthread_cond_t *" :length #!+alpha 2 #!-alpha 1)
  (binding-stack-start :c-type "lispobj *" :length #!+alpha 2 #!-alpha 1)
  (binding-stack-pointer :c-type "lispobj *" :length #!+alpha 2 #!-alpha 1)
  (control-stack-start :c-type "lispobj *" :length #!+alpha 2 #!-alpha 1)
  (control-stack-end :c-type "lispobj *" :length #!+alpha 2 #!-alpha 1)
  (control-stack-guard-page-protected)
  (alien-stack-start :c-type "lispobj *" :length #!+alpha 2 #!-alpha 1)
  (alien-stack-pointer :c-type "lispobj *" :length #!+alpha 2 #!-alpha 1)
  #!+gencgc (alloc-region :c-type "struct alloc_region" :length 5)
  (this :c-type "struct thread *" :length #!+alpha 2 #!-alpha 1)
  (prev :c-type "struct thread *" :length #!+alpha 2 #!-alpha 1)
  (next :c-type "struct thread *" :length #!+alpha 2 #!-alpha 1)
  ;; starting, running, suspended, dead
  (state :c-type "lispobj")
  (tls-cookie)                          ;  on x86, the LDT index
  #!+(or x86 x86-64 sb-thread) (pseudo-atomic-bits)
  (interrupt-data :c-type "struct interrupt_data *"
                  :length #!+alpha 2 #!-alpha 1)
  (stepping)
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
  #!+(and sb-thread)
  (control-stack-pointer :c-type "lispobj *")
  ;; KLUDGE: On alpha, until STEPPING we have been lucky and the 32
  ;; bit slots came in pairs. However the C compiler will align
  ;; interrupt_contexts on a double word boundary. This logic should
  ;; be handled by DEFINE-PRIMITIVE-OBJECT.
  #!+alpha
  (padding)
  (interrupt-contexts :c-type "os_context_t *" :rest-p t))
