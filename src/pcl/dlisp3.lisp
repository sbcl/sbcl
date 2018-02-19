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

;;; Rather than compiling the constructors here, just tickle the range
;;; of shapes defined above, leaving the generation of the
;;; constructors to precompile-dfun-constructors.
(let ((checking-or-caching-list
        '((t nil (class) nil)
          (t nil (class class) nil)
          (t nil (class class class) nil)
          (t nil (class class t) nil)
          (t nil (class class t t) nil)
          (t nil (class class t t t) nil)
          (t nil (class t) nil)
          (t nil (class t t) nil)
          (t nil (class t t t) nil)
          (t nil (class t t t t) nil)
          (t nil (class t t t t t) nil)
          (t nil (class t t t t t t) nil)
          (t nil (t class) nil)
          (t nil (t class t) nil)
          (t nil (t t class) nil)
          (t nil (class) t)
          (t nil (class class) t)
          (t nil (class t) t)
          (t nil (class t t) t)
          (t nil (class t t t) t)
          (t nil (t class) t)
          (t t (class) nil)
          (t t (class class) nil)
          (t t (class class class) nil)
          (nil nil (class) nil)
          (nil nil (class class) nil)
          (nil nil (class class t) nil)
          (nil nil (class class t t) nil)
          (nil nil (class t) nil)
          (nil nil (t class t) nil)
          (nil nil (class) t)
          (nil nil (class class) t))))
 (dolist (key checking-or-caching-list)
   (destructuring-bind (cached-emf-p return-value-p metatypes applyp) key
     (multiple-value-bind (args generator)
         (if cached-emf-p
             (if return-value-p
                 (values (list metatypes) 'emit-constant-value)
                 (values (list metatypes applyp) 'emit-caching))
             (if return-value-p
                 (values (list metatypes) 'emit-in-checking-p)
                 (values (list metatypes applyp) 'emit-checking)))
       (apply #'get-dfun-constructor generator args)))))
