;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;; SC-OFFSETs are needed by sparc-vm.lisp

(in-package "SB!C")

;;;; SC-OFFSETs
;;;;
;;;; We represent the place where some value is stored with a SC-OFFSET,
;;;; which is the SC number and offset encoded as an integer.

(defconstant-eqx sc-offset-scn-byte (byte 5 0) #'equalp)
(defconstant-eqx sc-offset-offset-byte (byte 22 5) #'equalp)
(def!type sc-offset () '(unsigned-byte 27))

(defmacro make-sc-offset (scn offset)
  `(dpb ,scn sc-offset-scn-byte
        (dpb ,offset sc-offset-offset-byte 0)))

(defmacro sc-offset-scn (sco) `(ldb sc-offset-scn-byte ,sco))
(defmacro sc-offset-offset (sco) `(ldb sc-offset-offset-byte ,sco))
