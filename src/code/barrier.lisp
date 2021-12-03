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

;;; If no memory barrier vops exist, then the %{mumble}-BARRIER function is an inline
;;; function that does nothing. If the vops exist, then the same function
;;; is always translated with a vop, and the DEFUN is merely an interpreter stub.
(eval-when (:compile-toplevel)
  (sb-xc:defmacro def-barrier (name)
    (if (sb-c::vop-existsp :named sb-vm:%memory-barrier)
        `(defun ,name () (,name))
        `(progn (declaim (inline ,name)) (defun ,name () (values))))))
(def-barrier sb-vm:%compiler-barrier)
(def-barrier sb-vm:%memory-barrier)
(def-barrier sb-vm:%read-barrier)
(def-barrier sb-vm:%write-barrier)
(def-barrier sb-vm:%data-dependency-barrier)

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
