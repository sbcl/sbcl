;;;; the STREAM structure

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;; the size of a stream in-buffer
;;;
;;; KLUDGE: The EVAL-WHEN wrapper isn't needed except when using CMU
;;; CL as a cross-compilation host. cmucl-2.4.19 issues full WARNINGs
;;; (not just STYLE-WARNINGs!) when processing this file, and when
;;; processing other files which use LISP-STREAM. -- WHN 2000-12-13
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant in-buffer-length 512))

(deftype in-buffer-type ()
  `(simple-array (unsigned-byte 8) (,in-buffer-length)))

(defstruct (lisp-stream (:constructor nil))
  ;; Buffered input.
  (in-buffer nil :type (or in-buffer-type null))
  (in-index in-buffer-length :type index)	; index into IN-BUFFER
  (in #'ill-in :type function)			; READ-CHAR function
  (bin #'ill-bin :type function)		; byte input function
  (n-bin #'ill-bin :type function)		; n-byte input function
  (out #'ill-out :type function)		; WRITE-CHAR function
  (bout #'ill-bout :type function)		; byte output function
  (sout #'ill-out :type function)		; string output function
  (misc #'do-nothing :type function))		; less-used methods
(def!method print-object ((x lisp-stream) stream)
  (print-unreadable-object (x stream :type t :identity t)))
