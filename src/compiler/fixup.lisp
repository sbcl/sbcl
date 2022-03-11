;;;; fixups, extracted from codegen.lisp by WHN 19990227 in order
;;;; to help with cross-compiling bootstrapping

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;; a fixup of some kind
(defstruct (fixup
            (:constructor make-fixup (name flavor &optional offset))
            (:copier nil))
  ;; the name and flavor of the fixup. The assembler makes no
  ;; assumptions about the contents of these fields; their semantics
  ;; are imposed by the dumper.
  (name nil :read-only t)
  ;; FIXME: "-flavor" and "-kind" are completedly devoid of meaning.
  ;; They former should probably be "fixup-referent-type" or "fixup-source"
  ;; to indicate that it denotes a namespace for NAME, and latter should be
  ;; "fixup-how" as it conveys a manner in which to modify encoded bytes.
  (flavor nil :read-only t)
  ;; OFFSET is an optional offset from whatever external label this
  ;; fixup refers to. Or in the case of the :CODE-OBJECT flavor of
  ;; fixups on the :X86 architecture, NAME is always NIL, so this
  ;; fixup doesn't refer to an external label, and OFFSET is an offset
  ;; from the beginning of the current code block.
  ;; A LABEL can also be used for ppc or ppc64 in which case the value
  ;; of the fixup will be the displacement to the label from CODE-TN.
  (offset 0 :type (or sb-vm:signed-word label)
            :read-only t))

(defstruct (fixup-note
             (:constructor make-fixup-note (kind fixup position))
             (:copier nil))
  kind
  fixup
  position)
(declaim (freeze-type fixup fixup-note))
