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

(in-package "SB!FASL")

;;; a helper function shared by DUMP-SIMPLE-CHARACTER-STRING and
;;; DUMP-SYMBOL (in the target compiler: the cross-compiler uses the
;;; portability knowledge and always dumps BASE-STRINGS).
#!+sb-unicode
(defun dump-characters-of-string (s fasl-output)
  (declare (type string s) (type fasl-output fasl-output))
  (dovector (c s)
    (dump-unsigned-byte-32 (char-code c) fasl-output))
  (values))
#!+sb-unicode
(defun dump-simple-character-string (s file)
  (declare (type (simple-array character (*)) s))
  (dump-fop* (length s) fop-small-character-string fop-character-string file)
  (dump-characters-of-string s file)
  (values))

;;; Dump the first N bytes of VEC out to FILE. VEC is some sort of unboxed
;;; vector-like thing that we can BLT from.
(defun dump-raw-bytes (vec n fasl-output)
  (declare (type index n) (type fasl-output fasl-output))
  ;; FIXME: Why not WRITE-SEQUENCE?
  (sb!impl::buffer-output (fasl-output-stream fasl-output) vec 0 n)
  (values))

;;; Dump a multi-dimensional array. Note: any displacements are folded out.
;;;
;;; This isn't needed at cross-compilation time because SBCL doesn't
;;; use multi-dimensional arrays internally. And it's hard to
;;; implement at cross-compilation time because it uses
;;; WITH-ARRAY-DATA. If it ever becomes necessary to implement it at
;;; cross-compilation time, it might possible to use ROW-MAJOR-AREF
;;; stuff to do it portably.
(defun dump-multi-dim-array (array file)
  (let ((rank (array-rank array)))
    (dotimes (i rank)
      (dump-integer (array-dimension array i) file))
    (with-array-data ((vector array) (start) (end))
      (if (and (= start 0) (= end (length vector)))
          (sub-dump-object vector file)
          (sub-dump-object (subseq vector start end) file)))
    (dump-fop 'fop-array file)
    (dump-word rank file)
    (eq-save-object array file)))

;;;; various dump-a-number operations

(defun dump-single-float-vector (vec file)
  (let ((length (length vec)))
    (dump-fop 'fop-single-float-vector file)
    (dump-word length file)
    (dump-raw-bytes vec (* length 4) file)))

(defun dump-double-float-vector (vec file)
  (let ((length (length vec)))
    (dump-fop 'fop-double-float-vector file)
    (dump-word length file)
    (dump-raw-bytes vec (* length 8) file)))

#!+long-float
(defun dump-long-float-vector (vec file)
  (let ((length (length vec)))
    (dump-fop 'fop-long-float-vector file)
    (dump-word length file)
    (dump-raw-bytes vec
                    (* length sb!vm:n-word-bytes #!+x86 3 #!+sparc 4)
                    file)))

(defun dump-complex-single-float-vector (vec file)
  (let ((length (length vec)))
    (dump-fop 'fop-complex-single-float-vector file)
    (dump-word length file)
    (dump-raw-bytes vec (* length 8) file)))

(defun dump-complex-double-float-vector (vec file)
  (let ((length (length vec)))
    (dump-fop 'fop-complex-double-float-vector file)
    (dump-word length file)
    (dump-raw-bytes vec (* length 16) file)))

#!+long-float
(defun dump-complex-long-float-vector (vec file)
  (let ((length (length vec)))
    (dump-fop 'fop-complex-long-float-vector file)
    (dump-word length file)
    (dump-raw-bytes vec
                    (* length sb!vm:n-word-bytes #!+x86 3 #!+sparc 4 2)
                    file)))

#!+(and long-float x86)
(defun dump-long-float (float file)
  (declare (long-float float))
  (let ((exp-bits (long-float-exp-bits float))
        (high-bits (long-float-high-bits float))
        (low-bits (long-float-low-bits float)))
    ;; We could get away with DUMP-WORD here, since the x86 has 4-byte words,
    ;; but we prefer to make things as explicit as possible.
    ;;     --njf, 2004-08-16
    (dump-integer-as-n-bytes low-bits 4 file)
    (dump-integer-as-n-bytes high-bits 4 file)
    (dump-integer-as-n-bytes exp-bits 2 file)))

#!+(and long-float sparc)
(defun dump-long-float (float file)
  (declare (long-float float))
  (let ((exp-bits (long-float-exp-bits float))
        (high-bits (long-float-high-bits float))
        (mid-bits (long-float-mid-bits float))
        (low-bits (long-float-low-bits float)))
    ;; We could get away with DUMP-WORD here, since the sparc has 4-byte
    ;; words, but we prefer to make things as explicit as possible.
    ;;     --njf, 2004-08-16
    (dump-integer-as-n-bytes low-bits 4 file)
    (dump-integer-as-n-bytes mid-bits 4 file)
    (dump-integer-as-n-bytes high-bits 4 file)
    (dump-integer-as-n-bytes exp-bits 4 file)))
