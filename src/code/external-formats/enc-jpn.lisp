(in-package "SB-IMPL")

;;; EUC-JP
(declaim (inline ucs-to-eucjp eucjp-to-ucs
                 mb-len-as-eucjp eucjp-continuation-byte-p))

(defun ucs-to-eucjp (code)
  (declare (optimize speed #.*safety-0*)
           (type fixnum code))
  (if (<= code #x7F) code
      (get-multibyte-mapper +ucs-to-eucjp-table+ code)))

(defun eucjp-to-ucs (code)
  (declare (optimize speed #.*safety-0*)
           (type fixnum code))
  (if (<= code #x7F) code
      (get-multibyte-mapper +eucjp-to-ucs-table+ code)))

(defun mb-len-as-eucjp (code)
  (declare (optimize speed #.*safety-0*)
           (type (unsigned-byte 8) code))
  (cond ((< code #x80) 1)
        ((or (= code #x8E) (<= #xA1 code #xFE)) 2)
        ((= code #x8F) 3)))

(defun eucjp-continuation-byte-p (code)
  (declare (optimize speed #.*safety-0*)
           (type (unsigned-byte 8) code))
  (<= #xA1 code #xFE))

(declaim (muffle-conditions compiler-note))
(define-multibyte-encoding :euc-jp (:euc-jp :eucjp :|eucJP|)
  ucs-to-eucjp eucjp-to-ucs mb-len-as-eucjp eucjp-continuation-byte-p)

;;; Shift_JIS
(declaim (inline ucs-to-sjis sjis-to-ucs
                 mb-len-as-sjis sjis-continuation-byte-p))

(defun ucs-to-sjis (code)
  (declare (optimize speed #.*safety-0*)
           (type fixnum code))
  (if (<= code #x7F) code
      (get-multibyte-mapper +ucs-to-sjis-table+ code)))

(defun sjis-to-ucs (code)
  (declare (optimize speed #.*safety-0*)
           (type fixnum code))
  (if (<= code #x7F) code
      (get-multibyte-mapper +sjis-to-ucs-table+ code)))

(defun mb-len-as-sjis (code)
  (declare (optimize speed #.*safety-0*)
           (type (unsigned-byte 8) code))
  (cond ((or (< code #x80) (<= #xA1 code #xDF)) 1)
        ((or (<= #x81 code #x9F) (<= #xE0 code #xFC)) 2)))

(defun sjis-continuation-byte-p (code)
  (declare (optimize speed #.*safety-0*)
           (type (unsigned-byte 8) code))
  (or (<= #x40 code #x7E) (<= #x80 code #xFC)))

(define-multibyte-encoding :shift_jis (:shift_jis :sjis :|Shift_JIS| :cp932)
  ucs-to-sjis sjis-to-ucs mb-len-as-sjis sjis-continuation-byte-p)
