;;;; optimizations for SAP operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;;; DEFKNOWNs

(defknown foreign-symbol-address (simple-string) system-area-pointer
  (movable flushable))

(defknown (sap< sap<= sap= sap>= sap>)
	  (system-area-pointer system-area-pointer) boolean
  (movable flushable))

(defknown sap+ (system-area-pointer integer) system-area-pointer
  (movable flushable))
(defknown sap- (system-area-pointer system-area-pointer) (signed-byte 32)
  (movable flushable))

(defknown sap-int (system-area-pointer) (unsigned-byte #!-alpha 32 #!+alpha 64)
  (movable flushable))
(defknown int-sap ((unsigned-byte #!-alpha 32 #!+alpha 64))
  system-area-pointer (movable))

(defknown sap-ref-8 (system-area-pointer fixnum) (unsigned-byte 8)
  (flushable))
(defknown %set-sap-ref-8 (system-area-pointer fixnum (unsigned-byte 8))
  (unsigned-byte 8)
  ())

(defknown sap-ref-16 (system-area-pointer fixnum) (unsigned-byte 16)
  (flushable))
(defknown %set-sap-ref-16 (system-area-pointer fixnum (unsigned-byte 16))
  (unsigned-byte 16)
  ())

(defknown sap-ref-32 (system-area-pointer fixnum) (unsigned-byte 32)
  (flushable))
(defknown %set-sap-ref-32 (system-area-pointer fixnum (unsigned-byte 32))
  (unsigned-byte 32)
  ())

#!+alpha
(defknown sap-ref-64 (system-area-pointer fixnum) (unsigned-byte 64)
  (flushable))
#!+alpha
(defknown %set-sap-ref-64 (system-area-pointer fixnum (unsigned-byte 64))
  (unsigned-byte 64)
  ())

(defknown signed-sap-ref-8 (system-area-pointer fixnum) (signed-byte 8)
  (flushable))
(defknown %set-signed-sap-ref-8 (system-area-pointer fixnum (signed-byte 8))
  (signed-byte 8)
  ())

(defknown signed-sap-ref-16 (system-area-pointer fixnum) (signed-byte 16)
  (flushable))
(defknown %set-signed-sap-ref-16 (system-area-pointer fixnum (signed-byte 16))
  (signed-byte 16)
  ())

(defknown signed-sap-ref-32 (system-area-pointer fixnum) (signed-byte 32)
  (flushable))
(defknown %set-signed-sap-ref-32 (system-area-pointer fixnum (signed-byte 32))
  (signed-byte 32)
  ())

#!+alpha
(defknown signed-sap-ref-64 (system-area-pointer fixnum) (signed-byte 64)
  (flushable))
#!+alpha
(defknown %set-signed-sap-ref-64 (system-area-pointer fixnum (signed-byte 64))
  (signed-byte 64)
  ())

(defknown sap-ref-sap (system-area-pointer fixnum) system-area-pointer
  (flushable))
(defknown %set-sap-ref-sap (system-area-pointer fixnum system-area-pointer)
  system-area-pointer
  ())

(defknown sap-ref-single (system-area-pointer fixnum) single-float
  (flushable))
(defknown sap-ref-double (system-area-pointer fixnum) double-float
  (flushable))
#!+(or x86 long-float)
(defknown sap-ref-long (system-area-pointer fixnum) long-float
  (flushable))

(defknown %set-sap-ref-single
	  (system-area-pointer fixnum single-float) single-float
  ())
(defknown %set-sap-ref-double
	  (system-area-pointer fixnum double-float) double-float
  ())
#!+long-float
(defknown %set-sap-ref-long
	  (system-area-pointer fixnum long-float) long-float
  ())

;;;; transforms for converting sap relation operators

(dolist (info '((sap< <) (sap<= <=) (sap= =) (sap>= >=) (sap> >)))
  (destructuring-bind (sap-fun int-fun) info
    (deftransform sap-fun ((x y) '* '* :eval-name t)
      `(,int-fun (sap-int x) (sap-int y)))))

;;;; transforms for optimizing SAP+

(deftransform sap+ ((sap offset))
  (cond ((and (constant-continuation-p offset)
	      (eql (continuation-value offset) 0))
	 'sap)
	(t
	 (extract-function-args sap 'sap+ 2)
	 '(lambda (sap offset1 offset2)
	    (sap+ sap (+ offset1 offset2))))))

(dolist (fun '(sap-ref-8 %set-sap-ref-8
	       signed-sap-ref-8 %set-signed-sap-ref-8
	       sap-ref-16 %set-sap-ref-16
	       signed-sap-ref-16 %set-signed-sap-ref-16
	       sap-ref-32 %set-sap-ref-32
	       signed-sap-ref-32 %set-signed-sap-ref-32
	       sap-ref-sap %set-sap-ref-sap
	       sap-ref-single %set-sap-ref-single
	       sap-ref-double %set-sap-ref-double
	       #!+(or x86 long-float) sap-ref-long
	       #!+long-float %set-sap-ref-long))
  (deftransform fun ((sap offset) '* '* :eval-name t)
    (extract-function-args sap 'sap+ 2)
    `(lambda (sap offset1 offset2)
       (,fun sap (+ offset1 offset2)))))
