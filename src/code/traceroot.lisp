;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-EXT")

(define-alien-variable  "gc_object_watcher" unsigned)

;;; This is the more accurate of the object liveness proof generators,
;;; as there is no chance for other code to execute in between the
;;; garbage collection and production of the chain of referencing objects.
(defun gc-and-search-roots (wps)
  (let ((list (ensure-list wps)))
    (sb-sys:with-pinned-objects (list)
      (setf gc-object-watcher (sb-kernel:get-lisp-obj-address list)))
    (gc :full t))
  (setf gc-object-watcher 0))

;;; This object liveness proof generator works well enough,
;;; but might be adversely affected by actions of concurrent threads.
(defun search-roots (wps)
  (let ((list (ensure-list wps)))
    (sb-sys:without-gcing
      (alien-funcall (extern-alien "prove_liveness" (function void unsigned))
                     (sb-kernel:get-lisp-obj-address list)))))
