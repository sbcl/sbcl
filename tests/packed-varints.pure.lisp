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

(with-test (:name :pack-compiled-debug-info-tlf-num+offset)
  (flet ((test (a b)
           (let ((cdi
                  (sb-c::make-compiled-debug-info
                   :name "test" :fun-map (sb-c::make-compiled-debug-fun :name 'test
                                                                        :encoded-locs 0
                                                                        :allow-other-keys t
                                                                        :return-pc 0
                                                                        :return-pc-pass 0
                                                                        :elsewhere-pc 0
                                                                        :old-fp 0)
                   :tlf-num+offset (let ((sb-c::*adjustable-vectors* nil))
                                     (sb-c::pack-tlf-num+offset a b)))))
             (assert (eql a (sb-c::compiled-debug-info-tlf-number cdi)))
             (assert (eql b (sb-c::compiled-debug-info-char-offset cdi))))))
    (dolist (i '(nil 0 1 2 313 542))
      (dolist (j '(nil 0 1 2 3 9898989 405344332))
        (test i j)))))

#+(and x86-64 immobile-space sb-unicode (not interpreter)) ; missing symbols otherwise
(with-test (:name :bignum-unpacker-no-consing)
  (flet ((try ()
           (let ((result 0)
                 ;; This happens to be a fairly large bignum (>2500 bits)
                 (b (sb-vm::%code-fixups
                     (sb-kernel:fun-code-header
                      #'sb-unicode::line-break-annotate))))
             (sb-int:do-packed-varints (int b)
               (setq result (logxor result int)))
             result)))
    (ctu:assert-no-consing (try))))
