;;;; Support for memory barriers required for multithreaded operation

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-THREAD")


;;;; Interpreter stubs for the various barrier functions

#-(vop-named sb-vm:%memory-barrier)
(progn
;;; Assert correctness of build order. (Need not be exhaustive)
(eval-when (:compile-toplevel) #+x86-64 (error "Expected %memory-barrier vop"))
(declaim (inline sb-vm:%compiler-barrier sb-vm:%memory-barrier
                 sb-vm:%read-barrier sb-vm:%write-barrier
                 sb-vm:%data-dependency-barrier)))
(macrolet ((def (name)
             `(defun ,name ()
                #+(vop-named sb-vm:%memory-barrier) (,name)
                (values))))
  (def sb-vm:%compiler-barrier)
  (def sb-vm:%memory-barrier)
  (def sb-vm:%read-barrier)
  (def sb-vm:%write-barrier)
  (def sb-vm:%data-dependency-barrier))

;;;; The actual barrier macro and support
(defmacro barrier ((kind) &body forms)
  "Insert a barrier in the code stream, preventing some sort of
reordering.

KIND should be one of:

  :COMPILER
    Prevent the compiler from reordering memory access across the
    barrier.
  :MEMORY
    Prevent the cpu from reordering any memory access across the
    barrier.
  :READ
    Prevent the cpu from reordering any read access across the
    barrier.
  :WRITE
    Prevent the cpu from reordering any write access across the
    barrier.
  :DATA-DEPENDENCY
    Prevent the cpu from reordering dependent memory reads across the
    barrier (requiring reads before the barrier to complete before any
    reads after the barrier that depend on them).  This is a weaker
    form of the :READ barrier.

FORMS is an implicit PROGN, evaluated before the barrier.  BARRIER
returns the values of the last form in FORMS.

The file \"memory-barriers.txt\" in the Linux kernel documentation is
highly recommended reading for anyone programming at this level."
  `(multiple-value-prog1
    (progn ,@forms)
    (,(or (getf '(:compiler        sb-vm:%compiler-barrier
                  :memory          sb-vm:%memory-barrier
                  :read            sb-vm:%read-barrier
                  :write           sb-vm:%write-barrier
                  :data-dependency sb-vm:%data-dependency-barrier)
                kind)
          (error "Unknown barrier kind ~S" kind)))))
