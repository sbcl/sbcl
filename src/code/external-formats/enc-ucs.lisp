;;;; Universal Character Set (UCS) encodings
;;;;
;;;; In our interpretation, these are distinct from UTF encodings: the
;;;; UCS encodings are a direct encoding of the code point, in 16- and
;;;; 32-bit variants; by contrast, the UTF encodings handle Unicode
;;;; surrogate code points specially.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;; TODO Macro for generating different variants:
;;; :ucs-2le (little endian)    sap-ref-16le
;;; :ucs-2be (big endian)       sap-ref-16be
;;; :ucs-2   (native)           sap-ref-16

;;;  Utilities

(declaim
 (inline sap-ref-16le (setf sap-ref-16le) sap-ref-16be (setf sap-ref-16be)
         sap-ref-32le (setf sap-ref-32le) sap-ref-32be (setf sap-ref-32be)))

;;; Define feature LITTLE-ENDIAN-AND-MISALIGNED-READ?
(defun sap-ref-16le (sap offset)
  #+(or x86 x86-64)
  (sap-ref-16 sap offset)
  #-(or x86 x86-64)
  (dpb (sap-ref-8 sap (1+ offset)) (byte 8 8)
       (sap-ref-8 sap offset)))

(defun (setf sap-ref-16le) (value sap offset)
  #+(or x86 x86-64)
  (setf (sap-ref-16 sap offset) value)
  #-(or x86 x86-64)
  (setf (sap-ref-8 sap offset) (logand value #xff)
        (sap-ref-8 sap (1+ offset)) (ldb (byte 8 8) value)))

(defun sap-ref-16be (sap offset)
  (dpb (sap-ref-8 sap offset) (byte 8 8)
       (sap-ref-8 sap (1+ offset))))

(defun (setf sap-ref-16be) (value sap offset)
  (setf (sap-ref-8 sap (1+ offset)) (logand value #xff)
        (sap-ref-8 sap offset) (ldb (byte 8 8) value)))

(defun sap-ref-32le (sap offset)
  #+(or x86 x86-64)
  (sap-ref-32 sap offset)
  #-(or x86 x86-64)
  (dpb (sap-ref-8 sap (+ offset 3)) (byte 8 24)
       (dpb (sap-ref-8 sap (+ offset 2)) (byte 8 16)
            (sap-ref-16le sap offset))))

(defun (setf sap-ref-32le) (value sap offset)
  #+(or x86 x86-64)
  (setf (sap-ref-32 sap offset) value)
  #-(or x86 x86-64)
  (setf (sap-ref-8 sap offset) (logand value #xff)
        (sap-ref-8 sap (1+ offset)) (ldb (byte 8 8) value)
        (sap-ref-8 sap (+ offset 2)) (ldb (byte 8 16) value)
        (sap-ref-8 sap (+ offset 3)) (ldb (byte 8 24) value)))

(defun sap-ref-32be (sap offset)
  (dpb (sap-ref-8 sap offset) (byte 8 24)
       (dpb (sap-ref-8 sap (1+ offset)) (byte 8 16)
            (dpb (sap-ref-8 sap (+ offset 2)) (byte 8 8)
                 (sap-ref-8 sap (+ offset 3))))))

(defun (setf sap-ref-32be) (value sap offset)
  (setf (sap-ref-8 sap offset) (ldb (byte 8 24) value)
        (sap-ref-8 sap (1+ offset)) (ldb (byte 8 16) value)
        (sap-ref-8 sap (+ offset 2)) (ldb (byte 8 8) value)
        (sap-ref-8 sap (+ offset 3)) (logand value #xff)))

;;;
;;;   octets
;;;

;;; Conversion to UCS-2{LE,BE}
(declaim (inline char->ucs-2le))
(defun char->ucs-2le (char dest string pos)
  (declare (optimize speed #.*safety-0*)
           (type (array (unsigned-byte 8) (*)) dest))
  (let ((code (char-code char)))
    (if (< code #x10000)
        (flet ((add-byte (b)
                 (declare (type (unsigned-byte 8) b))
                 (vector-push-extend b dest)))
          (declare (inline add-byte))
          (add-byte (ldb (byte 8 0) code))
          (add-byte (ldb (byte 8 8) code)))
        (let ((replacement (encoding-error :ucs-2le string pos)))
          (declare (type (simple-array (unsigned-byte 8) (*)) replacement))
          (dotimes (i (length replacement))
            (vector-push-extend (aref replacement i) dest))))))

(declaim (inline char->ucs-2be))
(defun char->ucs-2be (char dest string pos)
  (declare (optimize speed #.*safety-0*)
           (type (array (unsigned-byte 8) (*)) dest))
  (let ((code (char-code char)))
    (if (< code #x10000)
        (flet ((add-byte (b)
                 (declare (type (unsigned-byte 8) b))
                 (vector-push-extend b dest)))
          (declare (inline add-byte))
          (add-byte (ldb (byte 8 8) code))
          (add-byte (ldb (byte 8 0) code)))
        (let ((replacement (encoding-error :ucs-2be string pos)))
          (declare (type (simple-array (unsigned-byte 8) (*)) replacement))
          (dotimes (i (length replacement))
            (vector-push-extend (aref replacement i) dest))))))

(defun string->ucs-2le (string sstart send additional-space)
  (declare (optimize speed #.*safety-0*)
           (type simple-string string)
           (type array-range sstart send additional-space))
  (let ((array (make-array (* 2 (+ additional-space (- send sstart)))
                           :element-type '(unsigned-byte 8)
                           :fill-pointer 0 :adjustable t)))
    (loop for i from sstart below send
          do (char->ucs-2le (char string i) array string i))
    (dotimes (i (* 2 additional-space))
      (vector-push-extend 0 array))
    (coerce array '(simple-array (unsigned-byte 8) (*)))))

(defun string->ucs-2be (string sstart send additional-space)
  (declare (optimize speed #.*safety-0*)
           (type simple-string string)
           (type array-range sstart send additional-space))
  (let ((array (make-array (* 2 (+ additional-space (- send sstart)))
                           :element-type '(unsigned-byte 8)
                           :fill-pointer 0 :adjustable t)))
    (loop for i from sstart below send
          do (char->ucs-2be (char string i) array string i))
    (dotimes (i (* 2 additional-space))
      (vector-push-extend 0 array))
    (coerce array '(simple-array (unsigned-byte 8) (*)))))

;; Conversion from UCS-2{LE,BE}
(defmacro define-bytes-per-ucs2-character (accessor type)
  (declare (ignore type))
  (let ((name-le (make-od-name 'bytes-per-ucs-2le-character accessor))
        (name-be (make-od-name 'bytes-per-ucs-2be-character accessor)))
    `(progn
      (defun ,name-le (array pos end)
        (declare (ignore array pos end))
        (values 2 nil))
      (defun ,name-be (array pos end)
        (declare (ignore array pos end))
        (values 2 nil)))))
(instantiate-octets-definition define-bytes-per-ucs2-character)

(defmacro define-simple-get-ucs2-character (accessor type)
  (let ((name-le (make-od-name 'simple-get-ucs-2le-char accessor))
        (name-be (make-od-name 'simple-get-ucs-2be-char accessor)))
    `(progn
      (defun ,name-le (array pos bytes)
        (declare (optimize speed #.*safety-0*)
                 (type ,type array)
                 (type array-range pos)
                 (type (integer 1 4) bytes)
                 (ignore bytes))
        ;; Optimization for SYSTEM-AREA-POINTER: use SAP-REF-16LE that
        ;; reads two bytes at once on some architectures.
        ,(if (and (eq accessor 'sap-ref-8)
                  (eq type 'system-area-pointer))
             '(code-char (sap-ref-16le array pos))
             `(flet ((cref (x)
                      (,accessor array (the array-range (+ pos x)))))
               (declare (inline cref))
               (code-char (dpb (cref 1) (byte 8 8) (cref 0))))))
      (defun ,name-be (array pos bytes)
        (declare (optimize speed #.*safety-0*)
                 (type ,type array)
                 (type array-range pos)
                 (type (integer 1 4) bytes)
                 (ignore bytes))
        ;; Use SAP-REF-16BE even if it is not optimized
        ,(if (and (eq accessor 'sap-ref-8)
                  (eq type 'system-area-pointer))
             '(code-char (sap-ref-16be array pos))
             `(flet ((cref (x)
                      (,accessor array (the array-range (+ pos x)))))
               (declare (inline cref))
               (code-char (dpb (cref 0) (byte 8 8) (cref 1)))))))))

(instantiate-octets-definition define-simple-get-ucs2-character)

(defmacro define-ucs-2->string (accessor type)
  (let ((name-le (make-od-name 'ucs-2le->string accessor))
        (name-be (make-od-name 'ucs-2be->string accessor)))
    `(progn
      (defun ,name-le (array astart aend)
        (declare (optimize speed #.*safety-0*)
                 (type ,type array)
                 (type array-range astart aend))
        (let ((string (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character)))
          (loop with pos = astart
                while (< pos aend)
                do (multiple-value-bind (bytes invalid)
                       (,(make-od-name 'bytes-per-ucs-2le-character accessor) array pos aend)
                     (declare (type (or null string) invalid))
                     (aver (null invalid))
                     (vector-push-extend
                      (,(make-od-name 'simple-get-ucs-2le-char accessor)
                        array pos bytes)
                      string)
                     (incf pos bytes)))
          string))
      (defun ,name-be (array astart aend)
        (declare (optimize speed #.*safety-0*)
                 (type ,type array)
                 (type array-range astart aend))
        (let ((string (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character)))
          (loop with pos = astart
                while (< pos aend)
                do (multiple-value-bind (bytes invalid)
                       (,(make-od-name 'bytes-per-ucs-2be-character accessor) array pos aend)
                     (declare (type (or null string) invalid))
                     (aver (null invalid))
                     (vector-push-extend
                      (,(make-od-name 'simple-get-ucs-2be-char accessor)
                        array pos bytes)
                      string)
                     (incf pos bytes)))
          string)))))

(instantiate-octets-definition define-ucs-2->string)

(define-external-format/variable-width (:ucs-2le :ucs2le #+win32 :ucs2 #+win32 :ucs-2) t
  (code-char #xfffd)
  2
  (if (< bits #x10000)
      (setf (sap-ref-16le sap tail) bits)
      (external-format-encoding-error stream bits))
  2
  (code-char (sap-ref-16le sap head))
  ucs-2le->string-aref
  string->ucs-2le)

(define-external-format/variable-width (:ucs-2be :ucs2be) t
  (code-char #xfffd)
  2
  (if (< bits #x10000)
      (setf (sap-ref-16be sap tail) bits)
      (external-format-encoding-error stream bits))
  2
  (code-char (sap-ref-16be sap head))
  ucs-2be->string-aref
  string->ucs-2be)

(declaim (inline char->ucs-4le))
(defun char->ucs-4le (char dest string pos)
  (declare (optimize speed #.*safety-0*)
           (type (array (unsigned-byte 8) (*)) dest)
           (ignore string pos))
  (let ((code (char-code char)))
    (flet ((add-byte (b)
             (declare (type (unsigned-byte 8) b))
             (vector-push-extend b dest)))
      (declare (inline add-byte))
      (add-byte (ldb (byte 8 0) code))
      (add-byte (ldb (byte 8 8) code))
      (add-byte (ldb (byte 8 16) code))
      (add-byte (ldb (byte 8 24) code)))))

(declaim (inline char->ucs-4be))
(defun char->ucs-4be (char dest string pos)
  (declare (optimize speed #.*safety-0*)
           (type (array (unsigned-byte 8) (*)) dest)
           (ignore string pos))
  (let ((code (char-code char)))
    (flet ((add-byte (b)
             (declare (type (unsigned-byte 8) b))
             (vector-push-extend b dest)))
      (declare (inline add-byte))
      (add-byte (ldb (byte 8 24) code))
      (add-byte (ldb (byte 8 16) code))
      (add-byte (ldb (byte 8 8) code))
      (add-byte (ldb (byte 8 0) code)))))

(defun string->ucs-4le (string sstart send additional-space)
  (declare (optimize speed #.*safety-0*)
           (type simple-string string)
           (type array-range sstart send additional-space))
  (let ((array (make-array (* 4 (+ additional-space (- send sstart)))
                           :element-type '(unsigned-byte 8)
                           :fill-pointer 0 :adjustable t)))
    (loop for i from sstart below send
          do (char->ucs-4le (char string i) array string i))
    (dotimes (i (* 4 additional-space))
      (vector-push-extend 0 array))
    (coerce array '(simple-array (unsigned-byte 8) (*)))))

(defun string->ucs-4be (string sstart send additional-space)
  (declare (optimize speed #.*safety-0*)
           (type simple-string string)
           (type array-range sstart send additional-space))
  (let ((array (make-array (* 4 (+ additional-space (- send sstart)))
                           :element-type '(unsigned-byte 8)
                           :fill-pointer 0 :adjustable t)))
    (loop for i from sstart below send
          do (char->ucs-4be (char string i) array string i))
    (dotimes (i (* 4 additional-space))
      (vector-push-extend 0 array))
    (coerce array '(simple-array (unsigned-byte 8) (*)))))

;; Conversion from UCS-4{LE,BE}
(defmacro define-bytes-per-ucs4-character (accessor type)
  (declare (ignore type))
  (let ((name-le (make-od-name 'bytes-per-ucs-4le-character accessor))
        (name-be (make-od-name 'bytes-per-ucs-4be-character accessor)))
    `(progn
      (defun ,name-le (array pos end)
        (declare (ignore array pos end))
        (values 4 nil))
      (defun ,name-be (array pos end)
        (declare (ignore array pos end))
        (values 4 nil)))))
(instantiate-octets-definition define-bytes-per-ucs4-character)

(defmacro define-simple-get-ucs4-character (accessor type)
  (let ((name-le (make-od-name 'simple-get-ucs-4le-char accessor))
        (name-be (make-od-name 'simple-get-ucs-4be-char accessor)))
    `(progn
      (defun ,name-le (array pos bytes)
        (declare (optimize speed #.*safety-0*)
                 (type ,type array)
                 (type array-range pos)
                 (type (integer 1 4) bytes))
        ;; Optimization for SYSTEM-AREA-POINTER: use SAP-REF-32LE that
        ;; reads four bytes at once on some architectures.
        (let ((code ,(if (and (eq accessor 'sap-ref-8)
                              (eq type 'system-area-pointer))
                         '(sap-ref-32le array pos)
                         `(flet ((cref (x)
                                   (,accessor array (the array-range (+ pos x)))))
                            (declare (inline cref))
                            (dpb (cref 3) (byte 8 24)
                                 (dpb (cref 2) (byte 8 16)
                                      (dpb (cref 1) (byte 8 8) (cref 0))))))))
          (if (< code char-code-limit)
              (code-char code)
              (decoding-error array pos (+ pos bytes) :ucs-4le
                              'octet-decoding-error pos))))
      (defun ,name-be (array pos bytes)
        (declare (optimize speed #.*safety-0*)
                 (type ,type array)
                 (type array-range pos)
                 (type (integer 1 4) bytes))
        ;; Use SAP-REF-32BE even if it is not optimized
        (let ((code ,(if (and (eq accessor 'sap-ref-8)
                              (eq type 'system-area-pointer))
                         '(sap-ref-32be array pos)
                         `(flet ((cref (x)
                                   (,accessor array (the array-range (+ pos x)))))
                            (declare (inline cref))
                            (dpb (cref 0) (byte 8 24)
                                 (dpb (cref 1) (byte 8 16)
                                      (dpb (cref 2) (byte 8 8) (cref 3))))))))
          (if (< code char-code-limit)
              (code-char code)
              (decoding-error array pos (+ pos bytes) :ucs-4be
                              'octet-decoding-error pos)))))))

(eval-when (:compile-toplevel)
  (proclaim '(muffle-conditions compiler-note)))
(instantiate-octets-definition define-simple-get-ucs4-character)

(defmacro define-ucs-4->string (accessor type)
  (let ((name-le (make-od-name 'ucs-4le->string accessor))
        (name-be (make-od-name 'ucs-4be->string accessor)))
    `(progn
      (defun ,name-le (array astart aend)
        (declare (optimize speed #.*safety-0*)
                 (type ,type array)
                 (type array-range astart aend))
        (let ((string (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character)))
          (loop with pos = astart
                while (< pos aend)
                do (multiple-value-bind (bytes invalid)
                       (,(make-od-name 'bytes-per-ucs-4le-character accessor) array pos aend)
                     (declare (type (or null string) invalid))
                     (aver (null invalid))
                     (let ((thing (,(make-od-name 'simple-get-ucs-4le-char accessor) array pos bytes)))
                       (typecase thing
                         (character (vector-push-extend thing string))
                         (string (dotimes (i (length thing))
                                   (vector-push-extend (char thing i) string)))))
                     (incf pos bytes)))
          string))
      (defun ,name-be (array astart aend)
        (declare (optimize speed #.*safety-0*)
                 (type ,type array)
                 (type array-range astart aend))
        (let ((string (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character)))
          (loop with pos = astart
                while (< pos aend)
                do (multiple-value-bind (bytes invalid)
                       (,(make-od-name 'bytes-per-ucs-4be-character accessor) array pos aend)
                     (declare (type (or null string) invalid))
                     (aver (null invalid))
                     (let ((thing (,(make-od-name 'simple-get-ucs-4be-char accessor) array pos bytes)))
                       (typecase thing
                         (character (vector-push-extend thing string))
                         (string (dotimes (i (length thing))
                                   (vector-push-extend (char thing i) string)))))
                     (incf pos bytes)))
          string)))))

(instantiate-octets-definition define-ucs-4->string)

(define-external-format/variable-width (:ucs-4le :ucs4le) nil
  (code-char #xfffd)
  4
  (setf (sap-ref-32le sap tail) bits)
  4
  (let ((code (sap-ref-32le sap head)))
    (if (< code char-code-limit)
        (code-char code)
        (return-from decode-break-reason 4)))
  ucs-4le->string-aref
  string->ucs-4le)

(define-external-format/variable-width (:ucs-4be :ucs4be) nil
  (code-char #xfffd)
  4
  (setf (sap-ref-32be sap tail) bits)
  4
  (let ((code (sap-ref-32be sap head)))
    (if (< code char-code-limit)
        (code-char code)
        (return-from decode-break-reason 4)))
  ucs-4be->string-aref
  string->ucs-4be)
