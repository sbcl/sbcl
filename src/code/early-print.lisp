;;;; printer stuff which has to be defined early (e.g. DEFMACROs)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;;; level and length abbreviations

;;; The current level we are printing at, to be compared against
;;; *PRINT-LEVEL*. See the macro DESCEND-INTO for a handy interface to
;;; depth abbreviation.
(defvar *current-level-in-print* 0)

;;; Automatically handle *PRINT-LEVEL* abbreviation. If we are too
;;; deep, then a #\# is printed to STREAM and BODY is ignored.
(defmacro descend-into ((stream) &body body)
  (let ((flet-name (gensym)))
    `(flet ((,flet-name ()
	      ,@body))
       (cond ((and (null *print-readably*)
		   *print-level*
		   (>= *current-level-in-print* *print-level*))
	      (write-char #\# ,stream))
	     (t
	      (let ((*current-level-in-print* (1+ *current-level-in-print*)))
		(,flet-name)))))))

;;; Punt if INDEX is equal or larger then *PRINT-LENGTH* (and
;;; *PRINT-READABLY* is NIL) by outputting \"...\" and returning from
;;; the block named NIL.
(defmacro punt-print-if-too-long (index stream)
  `(when (and (not *print-readably*)
	      *print-length*
	      (>= ,index *print-length*))
     (write-string "..." ,stream)
     (return)))
