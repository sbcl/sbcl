;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

;;;; copyright information from original PCL sources:
;;;;
;;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;;; All rights reserved.
;;;;
;;;; Use and copying of this software and preparation of derivative works based
;;;; upon this software are permitted. Any distribution of this software or
;;;; derivative works must comply with all applicable United States export
;;;; control laws.
;;;;
;;;; This software is made available AS IS, and Xerox Corporation makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.

(in-package "SB-PCL")

;;; The built-in method combination types as taken from page 1-31 of
;;; 88-002R. Note that the STANDARD method combination type is defined
;;; by hand in the file combin.lisp.
(define-method-combination +      :identity-with-one-argument t)
(define-method-combination and    :identity-with-one-argument t)
(define-method-combination append :identity-with-one-argument nil)
(define-method-combination list   :identity-with-one-argument nil)
(define-method-combination max    :identity-with-one-argument t)
(define-method-combination min    :identity-with-one-argument t)
(define-method-combination nconc  :identity-with-one-argument t)
(define-method-combination or     :identity-with-one-argument t)
;;; we made OR (:MOST-SPECIFIC-FIRST) earlier in the build; hook it in
(let ((info (gethash 'or **method-combinations**)))
  (aver info)
  (aver (null (method-combination-info-cache info)))
  (setf (method-combination-info-cache info)
        (list (cons '(:most-specific-first) *or-method-combination*))))
(define-method-combination progn  :identity-with-one-argument t)

