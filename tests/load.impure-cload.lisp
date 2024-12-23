;;;; miscellaneous side-effectful tests of LOAD

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

(defvar *foo* '#:bar)

(assert (boundp 'sb-fasl::*current-fasl-group*))
(let* ((label
        (sb-fasl::fasl-group-header-label sb-fasl::*current-fasl-group*))
       (pn (pathname label)))
  (assert (and (string= (pathname-name pn) "load.impure-cload")
               (string= (pathname-type pn) "lisp"))))

;; INTERN always computes hashes, so the only thing we need to test
;; is whether uninterned symbols always have a hash.
(with-test (:name :loader-computes-symbol-hash-always)
  (assert (not (zerop (sb-kernel:symbol-hash *foo*)))))
