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

;; FIXME These are supported natively on alpha and using deftransforms
;; in compiler/x86/sap.lisp, which in OAO$n$ style need copying to
;; other 32 bit systems
(defknown sap-ref-64 (system-area-pointer fixnum) (unsigned-byte 64)
  (flushable))
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

(defknown signed-sap-ref-64 (system-area-pointer fixnum) (signed-byte 64)
  (flushable))
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

(macrolet ((def (sap-fun int-fun)
             `(deftransform ,sap-fun ((x y) * *)
                `(,',int-fun (sap-int x) (sap-int y)))))
  (def sap< <)
  (def sap<= <=)
  (def sap= =)
  (def sap>= >=)
  (def sap> >))

;;;; transforms for optimizing SAP+

(deftransform sap+ ((sap offset))
  (cond ((and (constant-continuation-p offset)
	      (eql (continuation-value offset) 0))
	 'sap)
	(t
	 (extract-fun-args sap 'sap+ 2)
	 '(lambda (sap offset1 offset2)
	    (sap+ sap (+ offset1 offset2))))))

(macrolet ((def (fun)
             `(deftransform ,fun ((sap offset) * *)
                (extract-fun-args sap 'sap+ 2)
                 `(lambda (sap offset1 offset2)
                   (,',fun sap (+ offset1 offset2))))))
  (def sap-ref-8)
  (def %set-sap-ref-8)
  (def signed-sap-ref-8)
  (def %set-signed-sap-ref-8)
  (def sap-ref-16)
  (def %set-sap-ref-16)
  (def signed-sap-ref-16)
  (def %set-signed-sap-ref-16)
  (def sap-ref-32)
  (def %set-sap-ref-32)
  (def signed-sap-ref-32)
  (def %set-signed-sap-ref-32)
  (def sap-ref-64)
  (def %set-sap-ref-64)
  (def signed-sap-ref-64)
  (def %set-signed-sap-ref-64)
  (def sap-ref-sap)
  (def %set-sap-ref-sap)
  (def sap-ref-single)
  (def %set-sap-ref-single)
  (def sap-ref-double)
  (def %set-sap-ref-double)
  ;; The original CMUCL code had #!+(and x86 long-float) for this first one,
  ;; but only #!+long-float for the second.  This was redundant, since the
  ;; LONG-FLOAT target feature only exists on X86.  So we removed the
  ;; redundancy.  --njf 2002-01-08
  #!+long-float (def sap-ref-long)
  #!+long-float (def %set-sap-ref-long))
