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

(defvar *current-level* 0
  #!+sb-doc
  "The current level we are printing at, to be compared against *PRINT-LEVEL*.
   See the macro DESCEND-INTO for a handy interface to depth abbreviation.")

(defmacro descend-into ((stream) &body body)
  #!+sb-doc
  "Automatically handle *PRINT-LEVEL* abbreviation. If we are too deep, then
   a # is printed to STREAM and BODY is ignored."
  (let ((flet-name (gensym)))
    `(flet ((,flet-name ()
	      ,@body))
       (cond ((and (null *print-readably*)
		   *print-level*
		   (>= *current-level* *print-level*))
	      (write-char #\# ,stream))
	     (t
	      (let ((*current-level* (1+ *current-level*)))
		(,flet-name)))))))

(defmacro punt-if-too-long (index stream)
  #!+sb-doc
  "Punt if INDEX is equal or larger then *PRINT-LENGTH* (and *PRINT-READABLY*
   is NIL) by outputting \"...\" and returning from the block named NIL."
  `(when (and (not *print-readably*)
	      *print-length*
	      (>= ,index *print-length*))
     (write-string "..." ,stream)
     (return)))
