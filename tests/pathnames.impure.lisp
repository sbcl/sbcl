;;;; miscellaneous tests of pathname-related stuff

;;;; This file is naturally impure because we mess with
;;;; LOGICAL-PATHNAME-TRANSLATIONS.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;; 
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(in-package "CL-USER")

(setf (logical-pathname-translations "foo")
      '(("REL;*.*.*"       "/tmp/")
	("MAIL;**;*.MAIL"  "/tmp/subdir/")
	("PROGGIES;*"      "/tmp/")))

(assert (string= (format nil
			 "~S"
			 (translate-logical-pathname "foo:proggies;save"))
		 "#P\"/tmp/save\""))

(compile-file-pathname "foo:proggies;save")

;;; success
(quit :unix-status 104)
