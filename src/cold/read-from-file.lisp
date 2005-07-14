;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-COLD")

;;; Return an expression read from the file named PATHNAME-DESIGNATOR.
(export 'read-from-file)
(defun read-from-file (pathname-designator)
  (with-open-file (s pathname-designator)
    (let* ((result (read s))
           (eof-result (cons nil nil))
           (after-result (read s nil eof-result)))
      (unless (eq after-result eof-result)
        (error "more than one expression in file ~S" pathname-designator))
      result)))
(compile 'read-from-file)
