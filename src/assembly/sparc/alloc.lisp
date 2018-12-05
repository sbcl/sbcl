;;;; stuff to handle allocation of stuff we don't want to do inline

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;; (Given that the pseudo-atomic sequence is so short, there is
;;; nothing that qualifies.  But we want to keep the file around
;;; in case we decide to add something later.)
