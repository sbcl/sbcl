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

(defmacro slot-symbol (slot-name type)
  `(if (and (symbolp ,slot-name) (symbol-package ,slot-name))
       (or (get ,slot-name ',(ecase type
			       (reader 'reader-symbol)
			       (writer 'writer-symbol)
			       (boundp 'boundp-symbol)))
	   (intern (format nil "~A ~A slot ~A"
			   (package-name (symbol-package ,slot-name))
			   (symbol-name ,slot-name)
			   ,(symbol-name type))
		   *slot-accessor-name-package*))
       (progn
	 (error "Non-symbol and non-interned symbol slot name accessors~
		 are not yet implemented.")
	 ;;(make-symbol (format nil "~A ~A" ,slot-name ,type))
	 )))

(defun slot-reader-symbol (slot-name)
  (slot-symbol slot-name reader))

(defun slot-writer-symbol (slot-name)
  (slot-symbol slot-name writer))

(defun slot-boundp-symbol (slot-name)
  (slot-symbol slot-name boundp))

