;;;; miscellaneous primitive stubs (ordinary FDEFINITIONs for full
;;;; call defined in terms of fundamental definitions of inline
;;;; expansions)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

(macrolet ((def (name &optional (args '(x)))
             `(defun ,name ,args
                (,@(if (listp name) `(funcall #',name) `(,name)) ,@args)))
           (def* (&rest defs)
             `(progn ,@(mapcar (lambda (x) `(def ,@x)) defs))))
  ;; SAP arithmetic and accessors
  (def sap< (x y))
  (def sap<= (x y))
  (def sap= (x y))
  (def sap>= (x y))
  (def sap> (x y))
  (def sap+ (sap offset))
  (def sap- (x y))
  (def sap-int)
  (def int-sap)
  (macrolet ((def-accessor (name)
               `(progn (def ,(symbolicate "%SET-" name) (sap offset value))
                       (def ,name (sap offset)))))
    (def-accessor sap-ref-8)
    (def-accessor sap-ref-16)
    (def-accessor sap-ref-32)
    (def-accessor sap-ref-64)
    (def-accessor sap-ref-word)
    (def-accessor signed-sap-ref-8)
    (def-accessor signed-sap-ref-16)
    (def-accessor signed-sap-ref-32)
    (def-accessor signed-sap-ref-64)
    (def-accessor signed-sap-ref-word)
    (def-accessor sap-ref-sap)
    (def-accessor sap-ref-lispobj)
    (def-accessor sap-ref-single)
    (def-accessor sap-ref-double))
  (def %byte-blt (src src-start dst dst-start dst-end))
  (def get-header-data)
  (def set-header-data (x val))
  (def get-closure-length)
  (def widetag-of)
  (def %other-pointer-widetag)
  (def vector-sap)
  (def binding-stack-pointer-sap  ())
  ;; x86 uses a plain old inline function for 'dynamic_space_free_pointer'
  ;; so there's no stub function for DYNAMIC-SPACE-FREE-POINTER.
  #-(or x86 x86-64) (def dynamic-space-free-pointer ())
  (def control-stack-pointer-sap ())
  (def sb-c:safe-fdefn-fun)
  (def fun-subtype)
  (def simple-fun-p)
  (def %simple-fun-arglist)
  (def (setf %simple-fun-arglist) (new-value func))
  (def %simple-fun-name)
  (def (setf %simple-fun-name) (new-value func))
  (def %simple-fun-info)
  (def closurep)
  (def %closure-fun)
  (def %closure-index-ref (closure index))
  (def sb-c::vector-length)
  (def make-array-header (type rank))
  (def code-instructions)
  (def code-header-ref (code-obj index))
  (def code-header-set (code-obj index new))
  (def %vector-raw-bits (object offset))
  (def %set-vector-raw-bits (object offset value))
  (def single-float-bits)
  (def double-float-high-bits)
  (def double-float-low-bits)
  (def value-cell-ref)
  (def %caller-frame ())
  (def %caller-pc ())
  ;; %code-code-size is an inline fun on 64-bit
  #-64-bit (def %code-code-size)
  (def %code-debug-info)
  #+(or x86 immobile-space) (def sb-vm::%code-fixups)
  (def %funcallable-instance-layout)
  (def %set-funcallable-instance-layout (x new-value))
  #+sb-simd-pack
  (def* (%make-simd-pack (tag low high))
        (%make-simd-pack-single (x y z w))
        (%make-simd-pack-double (low high))
        (%make-simd-pack-ub64 (low high))
        (%simd-pack-tag)
        (%simd-pack-low)
        (%simd-pack-high))
  #+sb-thread (def sb-vm::current-thread-offset-sap)
  (def current-sp ())
  (def current-fp ())
  (def stack-ref (s n))
  (def %set-stack-ref (s n value))
  (def fun-code-header)
  #-(or x86 x86-64) (def lra-code-header)
  (def %make-lisp-obj)
  (def get-lisp-obj-address))

(defun spin-loop-hint ()
  "Hints the processor that the current thread is spin-looping."
  (spin-loop-hint))


(defun %other-pointer-subtype-p (x choices)
  (and (%other-pointer-p x)
       (member (%other-pointer-widetag x) choices)
       t))
