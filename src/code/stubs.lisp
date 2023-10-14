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

(defun (cas symbol-value) (old new symbol)
  (cas (symbol-value symbol) old new))
(defun (cas svref) (old new vector index)
  (cas (svref vector index) old new))
#+(or ppc64 x86-64)
(macrolet ((def (name)
             `(defun (cas ,name) (old new sap index)
                (funcall #'(cas ,name) old new sap index))))
  (def sb-sys:sap-ref-8)
  (def sb-sys:sap-ref-16)
  (def sb-sys:sap-ref-32)
  (def sb-sys:sap-ref-64)
  (def sb-sys:signed-sap-ref-64)
  (def sb-sys:sap-ref-sap)
  (def sb-sys:sap-ref-lispobj))

(macrolet ((def (name &rest args)
             `(defun ,name ,args
                (,name ,@args))))
  (def word-logical-not x)
  (def word-logical-and x y)
  (def word-logical-or x y)
  (def word-logical-xor x y)
  (def word-logical-nor x y)
  (def word-logical-eqv x y)
  (def word-logical-nand x y)
  (def word-logical-andc1 x y)
  (def word-logical-andc2 x y)
  (def word-logical-orc1 x y)
  (def word-logical-orc2 x y))

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
               ;; the low-level %SET functions should not need stubs
               `(progn (def (setf ,name) (value sap offset))
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
  (def %byte-blt (src src-start dst dst-start count))
  (def get-header-data)
  (def set-header-data (x val))
  (def widetag-of)
  (def %other-pointer-widetag)
  (def vector-sap)
  (def binding-stack-pointer-sap  ())
  #+cheneygc (def dynamic-space-free-pointer ())
  (def control-stack-pointer-sap ())
  (def sb-c:safe-fdefn-fun)
  (def %fun-pointer-widetag)
  (def %closure-fun)
  (def %closure-index-ref (closure index))
  (def make-fdefn)
  (def fdefn-name)
  (def fdefn-fun)
  (def fdefn-makunbound)
  (def sb-c::vector-length)
  (def make-array-header (type rank))
  (def code-instructions)
  #-untagged-fdefns (def code-header-ref (code-obj index))
  (def %vector-raw-bits (object offset))
  (def %set-vector-raw-bits (object offset value))
  #-weak-vector-readbarrier (def weak-vector-len)
  (def single-float-bits)
  (def double-float-high-bits)
  #+64-bit
  (def double-float-bits)
  (def double-float-low-bits)
  (def value-cell-ref)
  (def %caller-frame ())
  (def %caller-pc ())
  (def %code-debug-info)
  (def sb-vm::%code-fixups)
  (def sb-bignum:%bignum-length)

  ;; instances
  (def %make-instance) ; Allocate a new instance with X data slots.
  (def %make-instance/mixed)
  (def %instance-length) ; Given an instance, return its length.
  (def %instance-layout)
  (def %set-instance-layout (instance new-value))
  ; (def %instance-ref (instance index)) ; defined in 'target-defstruct'
  (def %instance-set (instance index new-value))
  ;; funcallable instances
  ;(def %make-funcallable-instance)
  (def %fun-layout)
  (def %set-fun-layout (fin new-value))
  (def %funcallable-instance-fun)
  (def (setf %funcallable-instance-fun) (fin new-value))
  (def %funcallable-instance-info (fin i))
  #+compact-instance-header (progn (def layout-of)
                                   (def %instanceoid-layout))
  #+64-bit (def layout-depthoid)

  ;; lists
  (def %rplaca (x val))
  (def %rplacd (x val))

  #+compare-and-swap-vops
  (def* (%array-atomic-incf/word (array index diff))
        (%raw-instance-atomic-incf/word (instance index diff)))
  #+(or x86 x86-64)
  (def* (sb-vm::%cpu-identification (arg1 arg2))
        (sb-vm::%vector-cas-pair (vector index old1 old2 new1 new2))
        (sb-vm::%instance-cas-pair (instance index old1 old2 new1 new2))
        (sb-vm::%cons-cas-pair (cons old1 old2 new1 new2)))

  #+sb-simd-pack
  (def* (%make-simd-pack (tag low high))
        (%make-simd-pack-single (x y z w))
        (%make-simd-pack-double (low high))
        (%make-simd-pack-ub64 (low high))
        (%simd-pack-tag))
  #+sb-simd-pack-256
  (def* (%make-simd-pack-256 (tag p0 p1 p2 p3))
        (%make-simd-pack-256-single (a b c d e f g h))
        (%make-simd-pack-256-double (a b c d))
        (%make-simd-pack-256-ub64 (a b c d))
        (%simd-pack-256-tag))
  #+(or sb-thread x86-64) (def sb-vm::current-thread-offset-sap)
  (def current-sp ())
  (def current-fp ())
  (def stack-ref (s n))
  (def fun-code-header)
  (def symbol-package-id)
  (def symbol-hash)
  (def symbol-%info) ; primitive reader always needs a stub
  #-(or x86 x86-64) (def lra-code-header)
  (def %make-lisp-obj)
  (def get-lisp-obj-address)
  #+x86-64
  (def single-float-copysign (float float2))
  #+x86-64
  (def single-float-sign))

#+sb-simd-pack
(macrolet ((def (name)
             `(defun ,name (pack)
                (sb-vm::simd-pack-dispatch pack
                  (,name pack)))))
  (def %simd-pack-low)
  (def %simd-pack-high))

#+sb-simd-pack-256
(macrolet ((def (name)
             `(defun ,name (pack)
                (sb-vm::simd-pack-256-dispatch pack
                  (,name pack)))))
  (def %simd-pack-256-0)
  (def %simd-pack-256-1)
  (def %simd-pack-256-2)
  (def %simd-pack-256-3))

(defun spin-loop-hint ()
  "Hints the processor that the current thread is spin-looping."
  (spin-loop-hint))

(defun %other-pointer-subtype-p (x choices)
  (and (%other-pointer-p x)
       (member (%other-pointer-widetag x) choices)
       t))

;;; TYPECASE could expand to contain a call to this function.
;;; The interpreter can ignore it, it is just compiler magic.
(defun sb-c::%type-constraint (var type)
  (declare (ignore var type))
  nil)
(eval-when (:compile-toplevel)
  ;; Defining %TYPE-CONSTRAINT issues a full warning because TYPE's type
  ;; is (OR TYPE-SPECIFIER CTYPE), and TYPE-SPECIFIER is
  ;; (OR LIST SYMBOL CLASSOID CLASS), and CLASS isn't known, and you can't
  ;; define it because it's a standard symbol.
  (setq sb-c::*undefined-warnings* nil))

(setf (fdefinition 'unaligned-dx-cons) #'list)
