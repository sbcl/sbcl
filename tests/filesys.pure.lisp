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

;;; In sbcl-0.6.9 FOO-NAMESTRING functions  returned "" instead of NIL.
(let ((pathname0 (make-pathname :host nil 
				:directory
				(pathname-directory
				 *default-pathname-defaults*)
				:name "getty"))
      (pathname1 (make-pathname :host nil 
				:directory nil
				:name nil)))
  (assert (equal (file-namestring pathname0) "getty"))
  (assert (equal (directory-namestring pathname0)
		 (directory-namestring *default-pathname-defaults*)))
  (assert (equal (file-namestring pathname1) ""))
  (assert (equal (directory-namestring pathname1) "")))

;;; In sbcl-0.6.9 DIRECTORY failed on paths with :WILD or
;;; :WILD-INFERIORS in their directory components.
(let ((dir (directory "../**/*.*")))
  ;; We know a little bit about the structure of this result;
  ;; let's test to make sure that this test file is in it.
  (assert (find-if (lambda (pathname)
		     (search "tests/filesys.pure.lisp"
			     (namestring pathname)))
		   dir)))

;;; ANSI: FILE-LENGTH should signal an error of type TYPE-ERROR if
;;; STREAM is not a stream associated with a file.
;;;
;;; (Peter Van Eynde's ansi-test suite caught this, and Eric Marsden
;;; reported a fix for CMU CL, which was ported to sbcl-0.6.12.35.)
(assert (typep (nth-value 1 (ignore-errors (file-length *terminal-io*)))
	       'type-error))
