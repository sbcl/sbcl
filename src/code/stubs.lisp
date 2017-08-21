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
                (,@(if (listp name) `(funcall #',name) `(,name)) ,@args))))
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
  (def closurep)
  (def %closure-fun)
  (def %closure-index-ref (closure index))
  (def sb-c::vector-length)
  (def make-array-header (type rank))
  (def code-instructions)
  (def code-header-ref (code-obj index))
  (def code-header-set (code-obj index new))
  (def %vector-raw-bits (object offset))
  (def single-float-bits)
  (def double-float-high-bits)
  (def double-float-low-bits)
  (def value-cell-ref)
  (def %caller-frame ())
  (def %caller-pc ())
  (def %code-code-size)
  (def %code-debug-info)
  #+(or x86 immobile-space) (def sb-vm::%code-fixups)
  (def %funcallable-instance-layout)
  (def %set-funcallable-instance-layout (x new-value)))
