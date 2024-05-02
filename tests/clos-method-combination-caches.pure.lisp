;;;; testing the consistency of method combination sets

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

(let (problems)
  (flet ((check (gf)
           (let* ((mc (sb-mop:generic-function-method-combination gf))
                  (gfs (sb-pcl::method-combination-%generic-functions mc)))
             (unless (sb-pcl::weak-hashset-memberp gf gfs)
               (push gf problems)))))
    (sb-pcl::map-all-generic-functions #'check))
  (assert (null problems)))
