;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

(file-comment
  "$Header$")

(sb!alien:def-alien-routine ("purify" %purify) sb!c-call:void
  (static-roots sb!c-call:unsigned-long)
  (read-only-roots sb!c-call:unsigned-long))

;;; Compact the info environment. This is written with gratuitous
;;; recursion to make sure that our (and compact-info-environment's)
;;; local variables are above the stack top when purify runs.
(defun compact-environment-aux (name n)
  (cond
   ((zerop n)
    (let ((old-ie (car *info-environment*)))
      (setq *info-environment*
	    (list* (make-info-environment :name "Working")
		   (compact-info-environment (first *info-environment*)
					     :name name)
		   (rest *info-environment*)))
      (shrink-vector (sb!c::volatile-info-env-table old-ie) 0)))
   (t
    (compact-environment-aux name (1- n))
    n)))

(defun purify (&key root-structures (environment-name "Auxiliary"))
  #!+sb-doc
  "This function optimizes garbage collection by moving all currently live
   objects into non-collected storage. ROOT-STRUCTURES is an optional list of
   objects which should be copied first to maximize locality.

   DEFSTRUCT structures defined with the (:PURE T) option are moved into
   read-only storage, further reducing GC cost. List and vector slots of pure
   structures are also moved into read-only storage.

   ENVIRONMENT-NAME is gratuitous documentation for compacted version of the
   current global environment (as seen in SB!C::*INFO-ENVIRONMENT*.) If NIL is
   supplied, then environment compaction is inhibited."

  (when environment-name (compact-environment-aux environment-name 200))

  (let ((*gc-notify-before*
	 #'(lambda (notify-stream bytes-in-use)
	     (declare (ignore bytes-in-use))
	     (write-string "[doing purification: " notify-stream)
	     (force-output notify-stream)))
	(*internal-gc*
	 #'(lambda ()
	     (%purify (get-lisp-obj-address root-structures)
		      (get-lisp-obj-address nil))))
	(*gc-notify-after*
	 #'(lambda (notify-stream &rest ignore)
	     (declare (ignore ignore))
	     (write-line "done]" notify-stream))))
    #!-gencgc (gc t)
    #!+gencgc (gc :verbose t))
  nil)
