(in-package #:sb!impl)

;;; TODO Macro for generating different variants:
;;; :ucs-2le (little endian)    sap-ref-16le
;;; :ucs-2be (big endian)       sap-ref-16be
;;; :ucs-2   (native)           sap-ref-16

;;;  Utilities

(declaim (inline sap-ref-16le (setf sap-ref-16le)
                 sap-ref-16be (setf sap-ref-16be)))

;;; Define feature LITTLE-ENDIAN-AND-MISALIGNED-READ?
(defun sap-ref-16le (sap offset)
  #!+(or x86 x86-64)
  (sap-ref-16 sap offset)
  #!-(or x86 x86-64)
  (dpb (sap-ref-8 sap (1+ offset)) (byte 8 8)
       (sap-ref-8 sap offset)))

(defun (setf sap-ref-16le) (value sap offset)
  #!+(or x86 x86-64)
  (setf (sap-ref-16 sap offset) value)
  #!-(or x86 x86-64)
  (setf (sap-ref-8 sap offset) (logand #xFF value)
        (sap-ref-8 sap (1+ offset)) (ldb (byte 8 8) value)))

(defun sap-ref-16be (sap offset)
  (dpb (sap-ref-8 sap offset) (byte 8 8)
       (sap-ref-8 sap (1+ offset))))

(defun (setf sap-ref-16be) (value sap offset)
  (setf (sap-ref-8 sap (1+ offset)) (logand #xFF value)
        (sap-ref-8 sap offset) (ldb (byte 8 8) value)))

;;;
;;;   Define external format: fd-stream
;;;
(define-external-format/variable-width (:ucs-2le :ucs2le #!+win32 :ucs2 #!+win32 :ucs-2) nil
  2
  (if (< bits #x10000)
      (setf (sap-ref-16le sap tail) bits)
      (external-format-encoding-error stream bits))
  2
  (code-char (sap-ref-16le sap head)))

(define-external-format/variable-width (:ucs-2be :ucs2be) nil
  2
  (if (< bits #x10000)
      (setf (sap-ref-16be sap tail) bits)
      (external-format-encoding-error stream bits))
  2
  (code-char (sap-ref-16be sap head)))


;;;
;;;   octets
;;;

;;; Conversion to UCS-2{LE,BE}
(declaim (inline char->ucs-2le))
(defun char->ucs-2le (char dest string pos)
  (declare (optimize speed (safety 0))
           (type (array (unsigned-byte 8) (*)) dest))
  (let ((code (char-code char)))
    (if (< code #x10000)
        (flet ((add-byte (b)
                 (declare (type (unsigned-byte 8) b))
                 (vector-push b dest)))
          (declare (inline add-byte))
          (add-byte (ldb (byte 8 0) code))
          (add-byte (ldb (byte 8 8) code)))
        ; signal error
        (encoding-error :ucs-2le string pos))))

(declaim (inline char->ucs-2be))
(defun char->ucs-2be (char dest string pos)
  (declare (optimize speed (safety 0))
           (type (array (unsigned-byte 8) (*)) dest))
  (let ((code (char-code char)))
    (if (< code #x10000)
        (flet ((add-byte (b)
                 (declare (type (unsigned-byte 8) b))
                 (vector-push b dest)))
          (declare (inline add-byte))
          (add-byte (ldb (byte 8 8) code))
          (add-byte (ldb (byte 8 0) code)))
        ; signal error
        (encoding-error :ucs-16be string pos))))

(defun string->ucs-2le (string sstart send additional-space)
  (declare (optimize speed (safety 0))
           (type simple-string string)
           (type array-range sstart send additional-space))
  (let ((array (make-array (* 2 (+ additional-space (- send sstart)))
                           :element-type '(unsigned-byte 8)
                           :fill-pointer 0)))
    (loop for i from sstart below send
          do (char->ucs-2le (char string i) array string i))
    (dotimes (i additional-space)
      (vector-push 0 array)
      (vector-push 0 array))
    (coerce array '(simple-array (unsigned-byte 8) (*)))))

(defun string->ucs-2be (string sstart send additional-space)
  (declare (optimize speed (safety 0))
           (type simple-string string)
           (type array-range sstart send additional-space))
  (let ((array (make-array (* 2 (+ additional-space (- send sstart)))
                           :element-type '(unsigned-byte 8)
                           :fill-pointer 0)))
    (loop for i from sstart below send
          do (char->ucs-2be (char string i) array string i))
    (dotimes (i additional-space)
      (vector-push 0 array)
      (vector-push 0 array))
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
        (declare (optimize speed (safety 0))
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
               (code-char (dpb (cref 1) (byte 8 8)
                          (cref 0))))))
      (defun ,name-be (array pos bytes)
        (declare (optimize speed (safety 0))
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
               (code-char (dpb (cref 0) (byte 8 8)
                               (cref 1)))))))))

(instantiate-octets-definition define-simple-get-ucs2-character)

(defmacro define-ucs-2->string (accessor type)
  (let ((name-le (make-od-name 'ucs-2le->string accessor))
        (name-be (make-od-name 'ucs-2be->string accessor)))
    `(progn
      (defun ,name-le (array astart aend)
        (declare (optimize speed (safety 0))
                 (type ,type array)
                 (type array-range astart aend))
        (let ((string (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character)))
          (loop with pos = astart
                while (< pos aend)
                do (multiple-value-bind (bytes invalid)
                       (,(make-od-name 'bytes-per-ucs-2le-character accessor) array pos aend)
                     (declare (type (or null string) invalid))
                     (assert (null invalid))
                     (vector-push-extend
                      (,(make-od-name 'simple-get-ucs-2le-char accessor)
                        array pos bytes)
                      string)
                     (incf pos bytes)))
          string))
      (defun ,name-be (array astart aend)
        (declare (optimize speed (safety 0))
                 (type ,type array)
                 (type array-range astart aend))
        (let ((string (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character)))
          (loop with pos = astart
                while (< pos aend)
                do (multiple-value-bind (bytes invalid)
                       (,(make-od-name 'bytes-per-ucs-2be-character accessor) array pos aend)
                     (declare (type (or null string) invalid))
                     (assert (null invalid))
                     (vector-push-extend
                      (,(make-od-name 'simple-get-ucs-2be-char accessor)
                        array pos bytes)
                      string)
                     (incf pos bytes)))
          string)))))

(instantiate-octets-definition define-ucs-2->string)

(pushnew '((:ucs-2le :ucs2le #!+win32 :ucs2 #!+win32 :ucs-2)
           ucs-2le->string-aref string->ucs-2le)
         *external-format-functions*)

(pushnew '((:ucs-2be :ucs2be)
           ucs-2be->string-aref string->ucs-2be)
         *external-format-functions*)
