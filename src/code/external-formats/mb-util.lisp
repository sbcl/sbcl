(in-package "SB-IMPL")

(defmacro define-multibyte-mapper (name list)
  (let ((list (sort (copy-list list) #'< :key #'car))
        (hi (loop for x in list maximize (max (car x) (cadr x)))))
    `(defconstant-eqx ,name
       (make-array '(,(length list) 2)
                   :element-type '(integer 0 ,hi)
                   :initial-contents ',list)
       #'equalp)))

(defun get-multibyte-mapper (table code)
  (declare (optimize speed #.*safety-0*)
           (type (array * (* 2)) table)
           (type fixnum code))
  (labels ((recur (start end)
             (declare (type fixnum start end))
             (let* ((m (ash (+ start end) -1))
                    (x (aref table m 0)))
               (declare (type fixnum m x))
               (cond ((= x code)
                      (aref table m 1))
                     ((and (< x code) (< m end))
                      (recur (1+ m) end))
                     ((and (> x code) (> m start))
                      (recur start (1- m)))))))
    (recur 0 (1- (array-dimension table 0)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; FIXME: better to change make-od-name() to accept multiple
  ;; arguments in octets.lisp?
  (defun make-od-name-list (&rest syms)
    (reduce #'make-od-name syms))

  (defun define-bytes-per-mb-character-1 (accessor type format
                                          mb-len mb-continuation-byte-p)
    (let ((name (make-od-name-list 'bytes-per format 'character accessor))
          (invalid-mb-starter-byte
           (make-od-name-list 'invalid format 'starter-byte))
          (invalid-mb-continuation-byte
           (make-od-name-list 'invalid format 'continuation-byte)))
      `(progn
         ;;(declaim (inline ,name))
         (defun ,name (array pos end)
           (declare (optimize speed #.*safety-0*)
                    (type ,type array)
                    (type array-range pos end))
           ;; returns the number of bytes consumed and nil if it's a
           ;; valid character or the number of bytes consumed and a
           ;; replacement string if it's not.
           (let ((initial-byte (,accessor array pos))
                 (reject-reason nil)
                 (reject-position pos)
                 (remaining-bytes (- end pos)))
             (declare (type array-range reject-position remaining-bytes))
             (labels ((valid-starter-byte-p (b)
                        (declare (type (unsigned-byte 8) b))
                        (let ((ok (,mb-len b)))
                          (unless ok
                            (setf reject-reason ',invalid-mb-starter-byte))
                          ok))
                      (enough-bytes-left-p (x)
                        (let ((ok (> end (+ pos (1- x)))))
                          (unless ok
                            (setf reject-reason 'end-of-input-in-character))
                          ok))
                      (valid-secondary-p (x)
                        (let* ((idx (the array-range (+ pos x)))
                               (b (,accessor array idx))
                               (ok (,mb-continuation-byte-p b)))
                          (unless ok
                            (setf reject-reason ',invalid-mb-continuation-byte)
                            (setf reject-position idx))
                          ok))
                      (preliminary-ok-for-length (maybe-len len)
                        (and (eql maybe-len len)
                             ;; Has to be done in this order so that
                             ;; certain broken sequences (e.g., the
                             ;; two-byte sequence `"initial (length 3)"
                             ;; "non-continuation"' -- `#xef #x32')
                             ;; signal only part of that sequence as
                             ;; erroneous.
                             (loop for i from 1 below (min len remaining-bytes)
                                always (valid-secondary-p i))
                             (enough-bytes-left-p len))))
               (declare (inline valid-starter-byte-p
                                enough-bytes-left-p
                                valid-secondary-p
                                preliminary-ok-for-length))
               (let ((maybe-len (valid-starter-byte-p initial-byte)))
                 (cond ((eql maybe-len 1)
                        (values 1 nil))
                       ((preliminary-ok-for-length maybe-len 2)
                        (values 2 nil))
                       ((preliminary-ok-for-length maybe-len 3)
                        (values 3 nil))
                       (t
                        (let* ((bad-end (ecase reject-reason
                                          (,invalid-mb-starter-byte
                                           (1+ pos))
                                          (end-of-input-in-character
                                           end)
                                          (,invalid-mb-continuation-byte
                                           reject-position)))
                               (bad-len (- bad-end pos)))
                          (declare (type array-range bad-end bad-len))
                          (let ((replacement (decoding-error array pos bad-end ,format reject-reason reject-position)))
                            (values bad-len replacement))))))))))))

  (defun define-simple-get-mb-char-1 (accessor type format mb-to-ucs)
    (let ((name (make-od-name-list 'simple-get format 'char accessor))
          (malformed (make-od-name 'malformed format)))
      `(progn
         (declaim (inline ,name))
         (defun ,name (array pos bytes)
           (declare (optimize speed #.*safety-0*)
                    (type ,type array)
                    (type array-range pos)
                    (type (integer 1 3) bytes))
           (flet ((cref (x)
                    (,accessor array (the array-range (+ pos x)))))
             (declare (inline cref))
             (let ((code (,mb-to-ucs (ecase bytes
                                       (1 (cref 0))
                                       (2 (logior (ash (cref 0) 8) (cref 1)))
                                       (3 (logior (ash (cref 0) 16)
                                                  (ash (cref 1) 8)
                                                  (cref 2)))))))
               (if code
                   (code-char code)
                   (decoding-error array pos (+ pos bytes) ,format
                                   ',malformed pos))))))))

  (defun define-mb->string-1 (accessor type format)
    (let ((name
           (make-od-name-list format '>string accessor))
          (bytes-per-mb-character
           (make-od-name-list 'bytes-per format 'character accessor))
          (simple-get-mb-char
           (make-od-name-list 'simple-get format 'char accessor)))
      `(progn
         (defun ,name (array astart aend)
           (declare (optimize speed #.*safety-0*)
                    (type ,type array)
                    (type array-range astart aend))
           (let ((string (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character)))
             (loop with pos = astart
                while (< pos aend)
                do (multiple-value-bind (bytes invalid)
                       (,bytes-per-mb-character array pos aend)
                     (declare (type (or null string) invalid))
                     (cond
                       ((null invalid)
                        (let ((thing (,simple-get-mb-char array pos bytes)))
                          (typecase thing
                            (character (vector-push-extend thing string))
                            (string
                               (dotimes (i (length thing))
                                 (vector-push-extend (char thing i) string))))))
                       (t
                        (dotimes (i (length invalid))
                          (vector-push-extend (char invalid i) string))))
                     (incf pos bytes)))
             (coerce string 'simple-string))))))

  (declaim (inline mb-char-len))
  (defun mb-char-len (code)
    (declare (optimize speed #.*safety-0*)
             (type fixnum code))
    (cond ((< code 0) (bug "can't happen"))
          ((< code #x100) 1)
          ((< code #x10000) 2)
          ((< code #x1000000) 3)
          (t (bug "can't happen"))))
  )

(defmacro define-multibyte-encoding (format aliases
                                     ucs-to-mb mb-to-ucs
                                     mb-len mb-continuation-byte-p)
  (let ((char->mb (make-od-name 'char-> format))
        (string->mb (make-od-name 'string-> format))
        (define-bytes-per-mb-character
         (make-od-name-list 'define-bytes-per format 'character))
        (define-simple-get-mb-char
         (make-od-name-list 'define-simple-get format 'char))
        (define-mb->string
         (make-od-name-list 'define format '>string)))
    `(progn
       ;; for octets.lisp
       (define-condition ,(make-od-name 'malformed format)
           (octet-decoding-error) ())
       (define-condition ,(make-od-name-list 'invalid format 'starter-byte)
           (octet-decoding-error) ())
       (define-condition ,(make-od-name-list 'invalid format 'continuation-byte)
           (octet-decoding-error) ())

       (declaim (inline ,char->mb))
       (defun ,char->mb (char dest string pos)
         (declare (optimize speed #.*safety-0*)
                  (type (array (unsigned-byte 8) (*)) dest))
         (let ((code (,ucs-to-mb (char-code char))))
           (if code
               (flet ((add-byte (b)
                        (declare (type (unsigned-byte 8) b))
                        (vector-push-extend b dest)))
                 (declare (inline add-byte))
                 (setf code (the fixnum code))
                 (ecase (mb-char-len code)
                   (1
                    (add-byte code))
                   (2
                    (add-byte (ldb (byte 8 8) code))
                    (add-byte (ldb (byte 8 0) code)))
                   (3
                    (add-byte (ldb (byte 8 16) code))
                    (add-byte (ldb (byte 8 8) code))
                    (add-byte (ldb (byte 8 0) code)))))
               (encoding-error ,format string pos))))

       (defun ,string->mb (string sstart send additional-space)
         (declare (optimize speed #.*safety-0*)
                  (type simple-string string)
                  (type array-range sstart send additional-space))
         (let ((array (make-array (+ additional-space (- send sstart))
                                  :element-type '(unsigned-byte 8)
                                  :adjustable t
                                  :fill-pointer 0)))
           (loop for i from sstart below send
              do (,char->mb (char string i) array string i))
           (dotimes (i additional-space)
             (vector-push-extend 0 array))
           (coerce array '(simple-array (unsigned-byte 8) (*)))))

       (defmacro ,define-bytes-per-mb-character (accessor type)
         (define-bytes-per-mb-character-1 accessor type ',format
                                          ',mb-len ',mb-continuation-byte-p))

       (instantiate-octets-definition ,define-bytes-per-mb-character)

       (defmacro ,define-simple-get-mb-char (accessor type)
         (define-simple-get-mb-char-1 accessor type ',format ',mb-to-ucs))

       (instantiate-octets-definition ,define-simple-get-mb-char)

       (defmacro ,define-mb->string (accessor type)
         (define-mb->string-1 accessor type ',format))

       (instantiate-octets-definition ,define-mb->string)

       ;; for fd-stream.lisp
       (define-external-format/variable-width ,aliases t
         ;; KLUDGE: it so happens that at present (2009-10-22) none of
         ;; the external formats defined with
         ;; define-multibyte-encoding can encode the unicode
         ;; replacement character, so we hardcode the preferred
         ;; replacement here.
         #\?
         (block size
           (mb-char-len (or (,ucs-to-mb (char-code byte))
                            (return-from size 0))))
         (let ((mb (,ucs-to-mb bits)))
           (if (null mb)
               (external-format-encoding-error stream byte)
               (ecase size
                 (1 (setf (sap-ref-8 sap tail) mb))
                 (2 (setf (sap-ref-8 sap tail) (ldb (byte 8 8) mb)
                          (sap-ref-8 sap (1+ tail)) (ldb (byte 8 0) mb)))
                 (3 (setf (sap-ref-8 sap tail) (ldb (byte 8 16) mb)
                          (sap-ref-8 sap (1+ tail)) (ldb (byte 8 8) mb)
                          (sap-ref-8 sap (+ 2 tail)) (ldb (byte 8 0) mb))))))
         (1 (,mb-len byte))
         (let* ((mb (ecase size
                      (1 byte)
                      (2 (let ((byte2 (sap-ref-8 sap (1+ head))))
                           (unless (,mb-continuation-byte-p byte2)
                             (return-from decode-break-reason 2))
                           (dpb byte (byte 8 8) byte2)))
                      (3 (let ((byte2 (sap-ref-8 sap (1+ head)))
                               (byte3 (sap-ref-8 sap (+ 2 head))))
                           (unless (,mb-continuation-byte-p byte2)
                             (return-from decode-break-reason 2))
                           (unless (,mb-continuation-byte-p byte3)
                             (return-from decode-break-reason 3))
                           (dpb byte (byte 8 16) (dpb byte2 (byte 8 8) byte3))))))
                (ucs (,mb-to-ucs mb)))
           (if (null ucs)
               (return-from decode-break-reason 1)
               (code-char ucs)))
         ,(make-od-name format '>string-aref)
         ,string->mb))))
