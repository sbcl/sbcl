;;;; that part of the parms.lisp file from original CMU CL which is defined in
;;;; terms of the BACKEND structure
;;;;
;;;; FIXME: When we break up the BACKEND structure, this might be mergeable
;;;; back into the parms.lisp file.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.



(in-package "SB!VM")


;;;; compiler constants

(setf *backend-fasl-file-type* "axpf")
(setf *backend-fasl-file-implementation* :alpha)
(setf *backend-fasl-file-version* 2)
;;;(setf *backend-fasl-file-version* 8)
;;; 8 = sbcl-0.6.10.4 revived Gray stream support, changing stream layouts


(setf *backend-register-save-penalty* 3)

(setf *backend-byte-order* :little-endian)

;;; XXX the C runtime gets page size using getpagesize() - can't we
;;; look at that instead of hardcoding it here too?
(setf *backend-page-size* 8192)

