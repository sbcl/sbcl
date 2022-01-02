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

;; This choice of naming structure is perhaps unfortunate, because were the
;; names 2-lists, the globaldb hack to support this would instead be
;; a natural use of the (SETF <x>) style naming that globaldb favors.
;; But this naming is documented, and changing it would be incompatible.
;; The 4-part name can be thought of as a 2-part name because
;; half of it is composed of constants:
;; (SB-PCL::SLOT-ACCESSOR :GLOBAL <foo> SB-PCL::{READER|WRITER|BOUNDP})
;; -> ({READER|WRITER|BOUNDP} <foo>)
;;
(defun slot-reader-name (slot-name)
  (list 'slot-accessor :global slot-name 'reader))

(defun slot-writer-name (slot-name)
  (list 'slot-accessor :global slot-name 'writer))

(defun slot-boundp-name (slot-name)
  (list 'slot-accessor :global slot-name 'boundp))

(define-function-name-syntax slot-accessor (list)
  (when (= (length list) 4)
    (destructuring-bind (class slot rwb) (cdr list)
      (when (and (member rwb '(sb-pcl::reader sb-pcl::writer sb-pcl::boundp))
                 (symbolp slot)
                 (symbolp class))
        (values t slot)))))

;;; This is the object that we stick into a slot to tell us that it is
;;; unbound. It is the same as the marker for unbound symbols.
;;; There are two ways to check whether a slot is unbound:
;;;   (EQ <val> +slot-unbound+) ; ordinary object equality test
;;;   (UNBOUND-MARKER-P <val>)  ; (potentially) faster test
;;;
;;; It seems only reasonable to also export this for users, since
;;; otherwise dealing with STANDARD-INSTANCE-ACCESS becomes harder
;;; -- and slower -- than it needs to be.
#-sb-xc-host (define-symbol-macro +slot-unbound+ (make-unbound-marker))

