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

;;; Pre-allocate generic function caches. The hope is that this will put
;;; them nicely together in memory, and that that may be a win. Of course
;;; the first gc copy will probably blow that out, this really wants to be
;;; wrapped in something that declares the area static.
;;;
;;; This preallocation only creates about 25% more caches than PCL itself
;;; uses need. Some ports may want to preallocate some more of these.
(flet ((allocate (n size)
		 (mapcar #'free-cache-vector
			 (mapcar #'get-cache-vector
				 (make-list n :initial-element size)))))
  (allocate 128 4)
  (allocate 64 8)
  (allocate 64 9)
  (allocate 32 16)
  (allocate 16 17)
  (allocate 16 32)
  (allocate 1  64))
