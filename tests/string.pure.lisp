;;;; miscellaneous tests of STRING-related stuff

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

(assert (string= (string-upcase     "This is a test.") "THIS IS A TEST."))
(assert (string= (string-downcase   "This is a test.") "this is a test."))
(assert (string= (string-capitalize "This is a test.") "This Is A Test."))
(assert (string= (string-upcase "Is this 900-Sex-hott, please?" :start 3)
		 "Is THIS 900-SEX-HOTT, PLEASE?"))
(assert (string= (string-downcase "Is this 900-Sex-hott, please?"
				  :start 10 :end 16)
		 "Is this 900-sex-hott, please?"))
(assert (string= (string-capitalize "Is this 900-Sex-hott, please?")
		 "Is This 900-Sex-Hott, Please?"))
