;;;; tests of the INFO/globaldb system
;;;;
;;;; KLUDGE: Unlike most of the system's tests, these are not in the
;;;; problem domain, but in the implementation domain, so modification
;;;; of the system could cause these tests to fail even if the system
;;;; was still a correct implementation of ANSI Common Lisp + SBCL
;;;; extensions. Perhaps such tests should be separate from tests in
;;;; the problem domain. -- WHN 2001-02-11

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

(in-package :cl-user)

(defun foo (a) (list a))
(let ((x 1)) (foo x))

(assert (eq (sb-int:info :function :where-from 'foo)
            :defined))

(defun foo (a b) (list a b))
(let ((x 1)) (foo x 2))

(flet ((foo (a b c)
         (list a b c)))
  (foo 1 2 3))

;;; FIXME: This one is commented out since it doesn't work when
;;; the DEFUN is just LOADed instead of COMPILE-FILEd, and it's
;;; not immediately obvious what's the best way to set up
;;; the COMPILE-FILE test.
#||
(assert
  (equal
   (format nil "~A" (sb-int:info :function :type 'foo))
   "#<FUN-TYPE (FUNCTION (T T) LIST)>"))
||#

(with-test (:name :bug-458015)
  ;; Make sure layouts have sane source-locations
  (dolist (env sb-c::*info-environment*)
    (sb-c::do-info (env :class class :type type :name info-name :value value)
      (when (and (symbolp info-name)
                 (eql class :type)
                 (eql type :kind))
        (let* ((classoid (sb-kernel:find-classoid info-name nil))
               (layout (and classoid (sb-kernel:classoid-layout classoid)))
               (srcloc (and layout (sb-kernel::layout-source-location layout))))
          (when (and layout)
            (assert (or (sb-c::definition-source-location-p srcloc)
                        (null srcloc)))))))))

;;; success
