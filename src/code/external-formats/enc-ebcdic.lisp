(in-package "SB-IMPL")

(defmacro !define-unibyte-permutation-mapper (byte-code-name code-byte-name table)
  (let ((byte-to-code-table
         (make-array 256 :element-type '(unsigned-byte 8) :initial-contents table))
        (code-to-byte-table
         (make-array 256 :element-type '(unsigned-byte 8))))
    (dotimes (i 256)
      (setf (aref code-to-byte-table (aref byte-to-code-table i)) i))
    `(progn
       (defun ,byte-code-name (byte)
         (declare (optimize speed #.*safety-0*)
                  (type (unsigned-byte 8) byte))
         (aref ,byte-to-code-table byte))
       (defun ,code-byte-name (code)
         (declare (optimize speed #.*safety-0*)
                  (type char-code code))
         (if (> code 255)
             nil
             (aref ,code-to-byte-table code))))))

(!define-unibyte-permutation-mapper ebcdic-us->code-mapper code->ebcdic-us-mapper
  (#x00 #x01 #x02 #x03 #x9c #x09 #x86 #x7f #x97 #x8d #x8e #x0b #x0c #x0d #x0e #x0f
   #x10 #x11 #x12 #x13 #x9d #x85 #x08 #x87 #x18 #x19 #x92 #x8f #x1c #x1d #x1e #x1f
   #x80 #x81 #x82 #x83 #x84 #x0a #x17 #x1b #x88 #x89 #x8a #x8b #x8c #x05 #x06 #x07
   #x90 #x91 #x16 #x93 #x94 #x95 #x96 #x04 #x98 #x99 #x9a #x9b #x14 #x15 #x9e #x1a
   #x20 #xa0 #xe2 #xe4 #xe0 #xe1 #xe3 #xe5 #xe7 #xf1 #xa2 #x2e #x3c #x28 #x2b #x7c
   #x26 #xe9 #xea #xeb #xe8 #xed #xee #xef #xec #xdf #x21 #x24 #x2a #x29 #x3b #xac
   #x2d #x2f #xc2 #xc4 #xc0 #xc1 #xc3 #xc5 #xc7 #xd1 #xa6 #x2c #x25 #x5f #x3e #x3f
   #xf8 #xc9 #xca #xcb #xc8 #xcd #xce #xcf #xcc #x60 #x3a #x23 #x40 #x27 #x3d #x22
   #xd8 #x61 #x62 #x63 #x64 #x65 #x66 #x67 #x68 #x69 #xab #xbb #xf0 #xfd #xfe #xb1
   #xb0 #x6a #x6b #x6c #x6d #x6e #x6f #x70 #x71 #x72 #xaa #xba #xe6 #xb8 #xc6 #xa4
   #xb5 #x7e #x73 #x74 #x75 #x76 #x77 #x78 #x79 #x7a #xa1 #xbf #xd0 #xdd #xde #xae
   #x5e #xa3 #xa5 #xb7 #xa9 #xa7 #xb6 #xbc #xbd #xbe #x5b #x5d #xaf #xa8 #xb4 #xd7
   #x7b #x41 #x42 #x43 #x44 #x45 #x46 #x47 #x48 #x49 #xad #xf4 #xf6 #xf2 #xf3 #xf5
   #x7d #x4a #x4b #x4c #x4d #x4e #x4f #x50 #x51 #x52 #xb9 #xfb #xfc #xf9 #xfa #xff
   #x5c #xf7 #x53 #x54 #x55 #x56 #x57 #x58 #x59 #x5a #xb2 #xd4 #xd6 #xd2 #xd3 #xd5
   #x30 #x31 #x32 #x33 #x34 #x35 #x36 #x37 #x38 #x39 #xb3 #xdb #xdc #xd9 #xda #x9f))

(declaim (inline get-ebcdic-us-bytes))
(defun get-ebcdic-us-bytes (string pos)
  (declare (optimize speed #.*safety-0*)
           (type simple-string string)
           (type array-range pos))
  (get-latin-bytes #'code->ebcdic-us-mapper :ebcdic-us string pos))

(defun string->ebcdic-us (string sstart send null-padding)
    (declare (optimize speed #.*safety-0*)
             (type simple-string string)
             (type array-range sstart send))
  (values (string->latin% string sstart send #'get-ebcdic-us-bytes null-padding)))

(defmacro define-ebcdic-us->string* (accessor type)
  (declare (ignore type))
  (let ((name (make-od-name 'ebcdic-us->string* accessor)))
    `(progn
       (defun ,name (string sstart send array astart aend)
         (,(make-od-name 'latin->string* accessor) string sstart send array astart aend #'ebcdic-us->code-mapper)))))
(instantiate-octets-definition define-ebcdic-us->string*)

(defmacro define-ebcdic-us->string (accessor type)
  (declare (ignore type))
  `(defun ,(make-od-name 'ebcdic-us->string accessor) (array astart aend)
     (,(make-od-name 'latin->string accessor) array astart aend #'ebcdic-us->code-mapper)))
(instantiate-octets-definition define-ebcdic-us->string)

(define-unibyte-external-format :ebcdic-us (:cp037 :|cp037| :ibm-037 :ibm037)
  (let ((ebcdic-us-byte (code->ebcdic-us-mapper bits)))
    (if ebcdic-us-byte
        (setf (sap-ref-8 sap tail) ebcdic-us-byte)
        (external-format-encoding-error stream bits)))
  (code-char (ebcdic-us->code-mapper byte))
  ebcdic-us->string-aref
  string->ebcdic-us)
