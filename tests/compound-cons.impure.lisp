;;;; ANSI requires CONS be supported as a compound type. The CMU CL
;;;; version which SBCL was forked from didn't support this, but
;;;; various patches made around May 2000 added support for this to
;;;; CMU CL. This file contains tests of their functionality.

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

(cl:in-package :cl-user)

;;; This block of eight assertions is taken directly from
;;; 'Issue CONS-TYPE-SPECIFIER Writeup' in the ANSI spec.
(assert (typep '(a b c) '(cons t)))
(assert (typep '(a b c) '(cons symbol)))
(assert (not (typep '(a b c) '(cons integer))))
(assert (typep '(a b c) '(cons t t)))
(assert (not (typep '(a b c) '(cons symbol symbol))))
(assert (typep '(a b c) '(cons symbol (cons symbol (cons symbol)))))
(assert (not (typep '(a b c) '(cons symbol (cons symbol (cons symbol nil))))))
(assert (typep '(a b c) '(cons symbol (cons symbol (cons symbol null)))))

(assert (not (typep 11 'cons)))
(assert (not (typep 11 '(cons *))))
(assert (not (typep 11 '(cons t t))))

(assert (not (typep '() 'cons)))
(assert (typep '(100) 'cons))
(assert (typep '(100) '(cons t)))
(assert (typep '(100) '(cons number)))
(assert (not (typep '(100) '(cons character))))
(assert (typep '(100) '(cons number t)))
(assert (typep '(100) '(cons number null)))
(assert (not (typep '(100) '(cons number string))))

(assert (typep '("yes" . no) '(cons string symbol)))
(assert (not (typep '(yes . no) '(cons string symbol))))
(assert (not (typep '(yes . "no") '(cons string symbol))))
(assert (typep '(yes . "no") '(cons symbol)))
(assert (typep '(yes . "no") '(cons symbol t)))
(assert (typep '(yes . "no") '(cons t string)))
(assert (not (typep '(yes . "no") '(cons t null))))

(assert (subtypep '(cons t) 'cons))
(assert (subtypep 'cons '(cons t)))
(assert (subtypep '(cons t *) 'cons))
(assert (subtypep 'cons '(cons t *)))
(assert (subtypep '(cons * *) 'cons))
(assert (subtypep 'cons '(cons * *)))

(assert (subtypep '(cons number *) 'cons))
(assert (not (subtypep 'cons '(cons number *))))
(assert (subtypep '(cons * number) 'cons))
(assert (not (subtypep 'cons '(cons * number))))
(assert (subtypep '(cons structure-object number) 'cons))
(assert (not (subtypep 'cons '(cons structure-object number))))

(assert (subtypep '(cons null fixnum) (type-of '(nil 44))))
