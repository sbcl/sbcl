;;;; dumping functionality which isn't needed in cross-compilation
;;;; (and, typically, which is awkward to implement in the
;;;; cross-compilation host)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

(file-comment
  "$Header$")

;;; Dump the first N bytes of VEC out to FILE. VEC is some sort of unboxed
;;; vector-like thing that we can BLT from.
(defun dump-raw-bytes (vec n fasl-file)
  (declare (type index n) (type fasl-file fasl-file))
  (sb!sys:output-raw-bytes (fasl-file-stream fasl-file) vec 0 n)
  (values))

;;; Dump a multi-dimensional array. Note: any displacements are folded out.
;;;
;;; This isn't needed at cross-compilation time because SBCL doesn't
;;; use multi-dimensional arrays internally. It's hard to implement
;;; at cross-compilation time because it uses WITH-ARRAY-DATA. If it ever
;;; becomes necessary to implement it at cross-compilation time, it might
;;; possible to use ROW-MAJOR-AREF stuff to do it portably.
(defun dump-multi-dim-array (array file)
  (let ((rank (array-rank array)))
    (dotimes (i rank)
      (dump-integer (array-dimension array i) file))
    (sb!impl::with-array-data ((vector array) (start) (end))
      (if (and (= start 0) (= end (length vector)))
	  (sub-dump-object vector file)
	  (sub-dump-object (subseq vector start end) file)))
    (dump-fop 'sb!impl::fop-array file)
    (dump-unsigned-32 rank file)
    (eq-save-object array file)))

(defun dump-single-float-vector (vec file)
  (let ((length (length vec)))
    (dump-fop 'sb!impl::fop-single-float-vector file)
    (dump-unsigned-32 length file)
    (dump-raw-bytes vec (* length sb!vm:word-bytes) file)))

(defun dump-double-float-vector (vec file)
  (let ((length (length vec)))
    (dump-fop 'sb!impl::fop-double-float-vector file)
    (dump-unsigned-32 length file)
    (dump-raw-bytes vec (* length sb!vm:word-bytes 2) file)))

#!+long-float
(defun dump-long-float-vector (vec file)
  (let ((length (length vec)))
    (dump-fop 'sb!impl::fop-long-float-vector file)
    (dump-unsigned-32 length file)
    (dump-raw-bytes vec (* length sb!vm:word-bytes #!+x86 3 #!+sparc 4) file)))

(defun dump-complex-single-float-vector (vec file)
  (let ((length (length vec)))
    (dump-fop 'sb!impl::fop-complex-single-float-vector file)
    (dump-unsigned-32 length file)
    (dump-raw-bytes vec (* length sb!vm:word-bytes 2) file)))

(defun dump-complex-double-float-vector (vec file)
  (let ((length (length vec)))
    (dump-fop 'sb!impl::fop-complex-double-float-vector file)
    (dump-unsigned-32 length file)
    (dump-raw-bytes vec (* length sb!vm:word-bytes 2 2) file)))

#!+long-float
(defun dump-complex-long-float-vector (vec file)
  (let ((length (length vec)))
    (dump-fop 'sb!impl::fop-complex-long-float-vector file)
    (dump-unsigned-32 length file)
    (dump-raw-bytes vec (* length sb!vm:word-bytes #!+x86 3 #!+sparc 4 2) file)))

#!+(and long-float x86)
(defun dump-long-float (float file)
  (declare (long-float float))
  (let ((exp-bits (long-float-exp-bits float))
	(high-bits (long-float-high-bits float))
	(low-bits (long-float-low-bits float)))
    (dump-unsigned-32 low-bits file)
    (dump-unsigned-32 high-bits file)
    (dump-integer-as-n-bytes exp-bits 2 file)))

#!+(and long-float sparc)
(defun dump-long-float (float file)
  (declare (long-float float))
  (let ((exp-bits (long-float-exp-bits float))
	(high-bits (long-float-high-bits float))
	(mid-bits (long-float-mid-bits float))
	(low-bits (long-float-low-bits float)))
    (dump-unsigned-32 low-bits file)
    (dump-unsigned-32 mid-bits file)
    (dump-unsigned-32 high-bits file)
    (dump-integer-as-n-bytes exp-bits 4 file)))

;;; Or a complex...

(defun dump-complex (x file)
  (typecase x
    ((complex single-float)
     (dump-fop 'sb!impl::fop-complex-single-float file)
     (dump-integer-as-n-bytes (single-float-bits (realpart x)) 4 file)
     (dump-integer-as-n-bytes (single-float-bits (imagpart x)) 4 file))
    ((complex double-float)
     (dump-fop 'sb!impl::fop-complex-double-float file)
     (let ((re (realpart x)))
       (declare (double-float re))
       (dump-unsigned-32 (double-float-low-bits re) file)
       (dump-integer-as-n-bytes (double-float-high-bits re) 4 file))
     (let ((im (imagpart x)))
       (declare (double-float im))
       (dump-unsigned-32 (double-float-low-bits im) file)
       (dump-integer-as-n-bytes (double-float-high-bits im) 4 file)))
    #!+long-float
    ((complex long-float)
     (dump-fop 'sb!impl::fop-complex-long-float file)
     (dump-long-float (realpart x) file)
     (dump-long-float (imagpart x) file))
    (t
     (sub-dump-object (realpart x) file)
     (sub-dump-object (imagpart x) file)
     (dump-fop 'sb!impl::fop-complex file))))

