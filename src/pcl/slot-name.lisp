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

(in-package "SB!PCL")

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

