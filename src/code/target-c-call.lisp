;;;; FIXME: This file and host-c-call.lisp are separate from the
;;;; rest of the alien source code for historical reasons: CMU CL
;;;; made a distinction between the stuff in the C-CALL package and
;;;; stuff in the ALIEN package. There's no obvious boundary
;;;; there, though, and SBCL doesn't try to make this distinction,
;;;; so it might make sense to just merge these files in with the
;;;; rest of the SB-ALIEN code.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!ALIEN")

;;;; extra types

(define-alien-type char (integer 8))
(define-alien-type short (integer 16))
(define-alien-type int (integer 32))
(define-alien-type long (integer #!-alpha 32 #!+alpha 64))

(define-alien-type unsigned-char (unsigned 8))
(define-alien-type unsigned-short (unsigned 16))
(define-alien-type unsigned-int (unsigned 32))
(define-alien-type unsigned-long (unsigned #!-alpha 32 #!+alpha 64))

(define-alien-type float single-float)
(define-alien-type double double-float)

(define-alien-type-translator void ()
  (parse-alien-type '(values) (sb!kernel:make-null-lexenv)))

;;; FIXME: %NATURALIZE-C-STRING (and the UTF8 siblings below) would
;;; appear to be vulnerable to the lisp string moving from underneath
;;; them if the world undergoes a GC, possibly triggered by another
;;; thread.  Ugh.
(defun %naturalize-c-string (sap)
  (declare (type system-area-pointer sap))
  (locally
      (declare (optimize (speed 3) (safety 0)))
    (let ((length (loop for offset of-type fixnum upfrom 0
                        until (zerop (sap-ref-8 sap offset))
                        finally (return offset))))
      (let ((result (make-string length :element-type 'base-char)))
	(sb!kernel:copy-from-system-area sap 0
                                         result (* sb!vm:vector-data-offset
                                                   sb!vm:n-word-bits)
                                         (* length sb!vm:n-byte-bits))
	result))))

(defun %naturalize-utf8-string (sap)
  (declare (type system-area-pointer sap))
  (locally
    (declare (optimize (speed 3) (safety 0)))
    (let ((length (do* ((offset 0)
                        (byte (sap-ref-8 sap offset) (sap-ref-8 sap offset))
                        (index 0 (1+ index)))
                       ((zerop byte) index)
                    (declare (type fixnum offset index))
                    (cond
                      ;; FIXME: Here, and below, we don't defend
                      ;; against malformed utf-8 with any degree of
                      ;; rigour.
                      ((< byte #x80) (incf offset))
                      ((< byte #xe0) (incf offset 2))
                      ((< byte #xf0) (incf offset 3))
                      (t (incf offset 4))))))
      (let ((result (make-string length :element-type 'character)))
        (do* ((offset 0)
              (byte (sap-ref-8 sap offset) (sap-ref-8 sap offset))
              (index 0 (1+ index)))
            ((>= index length) result)
          (declare (type fixnum offset index))
          (setf (char result index)
                (cond
                  ((< byte #x80)
                   (prog1 (code-char byte) (incf offset)))
                  ((< byte #xe0)
                   (prog1 (code-char (dpb byte (byte 5 6)
                                          (sap-ref-8 sap (1+ offset))))
                     (incf offset 2)))
                  ((< byte #xf0)
                   (prog1 (code-char
                           (dpb byte (byte 4 12)
                                (dpb (sap-ref-8 sap (1+ offset)) (byte 6 6)
                                     (sap-ref-8 sap (+ 2 offset)))))
                     (incf offset 3)))
                  (t
                   (prog1
                       (code-char
                        (dpb byte (byte 3 18)
                             (dpb (sap-ref-8 sap (1+ offset)) (byte 6 12)
                                  (dpb (sap-ref-8 sap (+ 2 offset)) (byte 6 6)
                                       (sap-ref-8 sap (+ 3 offset))))))
                     (incf offset 4))))))))))

(defun %deport-utf8-string (string)
  (declare (type simple-string string))
  (locally
    (declare (optimize (speed 3) (safety 0)))
    (let ((length (1+ (do* ((offset 0)
                            (length (length string))
                            (index 0 (1+ index)))
                           ((= index length) offset)
                        (declare (type fixnum offset))
                        (let ((bits (char-code (char string index))))
                          (cond
                            ((< bits #x80) (incf offset 1))
                            ((< bits #x800) (incf offset 2))
                            ((< bits #x10000) (incf offset 3))
                            (t (incf offset 4))))))))
      (let ((vector (make-array length :element-type '(unsigned-byte 8)
                                :initial-element 0)))
        (do* ((offset 0)
              (length (length string))
              (index 0 (1+ index)))
             ((= index length) vector)
          (declare (type fixnum offset))
          (let ((bits (char-code (char string index))))
            (cond
              ((< bits #x80)
               (setf (aref vector offset) bits)
               (incf offset))
              ((< bits #x800)
               (setf (aref vector offset) (logior #xc0 (ldb (byte 5 6) bits)))
               (setf (aref vector (1+ offset))
                     (logior #x80 (ldb (byte 6 0) bits)))
               (incf offset 2))
              ((< bits #x10000)
               (setf (aref vector offset) (logior #xe0 (ldb (byte 4 12) bits)))
               (setf (aref vector (1+ offset))
                     (logior #x80 (ldb (byte 6 6) bits)))
               (setf (aref vector (+ offset 2))
                     (logior #x80 (ldb (byte 6 0) bits)))
               (incf offset 3))
              (t
               (setf (aref vector offset) (logior #xf0 (ldb (byte 3 18) bits)))
               (setf (aref vector (1+ offset))
                     (logior #x80 (ldb (byte 6 12) bits)))
               (setf (aref vector (+ offset 2))
                     (logior #x80 (ldb (byte 6 6) bits)))
               (setf (aref vector (+ offset 3))
                     (logior #x80 (ldb (byte 6 0) bits)))
               (incf offset 4)))))))))
