;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-COLD")

;;; RENAME-PACKAGE in two steps in order to avoid the possibility of undefined
;;; behavior when one of the new names is the same as one of the old names.
;;; (ANSI on RENAME-PACKAGE: "The consequences are undefined if new-name or any
;;; new-nickname conflicts with any existing package names.")
(defun rename-package-carefully (package-designator
                                 new-name
                                 &optional new-nicknames)
  (let ((package (find-package package-designator))
        (unused-name "UNUSED-PACKAGE-NAME"))
    (assert (not (find-package unused-name)))
    (assert (not (string= unused-name new-name)))
    (assert (not (find unused-name new-nicknames :test #'string=)))
    (assert (not (find new-name new-nicknames :test #'string=)))
    (rename-package package unused-name)
    (rename-package package new-name new-nicknames)))
(compile 'rename-package-carefully)
