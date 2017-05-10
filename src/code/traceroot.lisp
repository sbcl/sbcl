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
(define-alien-variable  "gc_traceroot_criterion" int)

(flet ((criterion-value (criterion)
         (ecase criterion
           ;; CRITERION determines just how rooty (how deep) a root must be.
           ;; :OLDEST says we can stop upon seeing an object in the oldest
           ;; gen to GC, or older. This is the easiest test to satisfy.
           ;; To find a root of an image-backed object, you want to stop only
           ;; at a truly :STATIC object, because everything dumped was
           ;; effectively :PSEUDO-STATIC, which is usually the same
           ;; as :OLDEST, unless the oldest gen to GC has been decreased.
           (:oldest 0)
           (:pseudo-static 1)
           (:static 2))))

;;; This is the more accurate of the object liveness proof generators,
;;; as there is no chance for other code to execute in between the
;;; garbage collection and production of the chain of referencing objects.
(defun gc-and-search-roots (wps &optional (criterion :oldest))
  (let ((list (ensure-list wps)))
    (setf gc-traceroot-criterion (criterion-value criterion))
    (sb-sys:with-pinned-objects (list)
      (setf gc-object-watcher (sb-kernel:get-lisp-obj-address list)))
    (gc :full t))
  (setf gc-object-watcher 0))

;;; This object liveness proof generator works well enough,
;;; but might be adversely affected by actions of concurrent threads.
(defun search-roots (wps &optional (criterion :oldest))
  (let ((list (ensure-list wps)))
    (sb-sys:without-gcing
      (alien-funcall (extern-alien "prove_liveness" (function void unsigned int))
                     (sb-kernel:get-lisp-obj-address list)
                     (criterion-value criterion))))))
