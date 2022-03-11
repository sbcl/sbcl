;;; enc-cn.lisp: CP936 encoding support in SBCL
;;; Chun Tian (binghe) <binghe.lisp@gmail.com>
;;; Sat Dec 23 02:45:12 CST 2006

(in-package "SB-IMPL")

;;; GBK
(declaim (inline ucs-to-gbk gbk-to-ucs
                 mb-len-as-gbk gbk-continuation-byte-p))

(defun ucs-to-gbk (code)
  (declare (optimize speed #.*safety-0*)
           (type fixnum code))
  (if (<= code #x7f) code
      (get-multibyte-mapper +ucs-to-gbk-table+ code)))

(defun gbk-to-ucs (code)
  (declare (optimize speed #.*safety-0*)
           (type fixnum code))
  (if (<= code #x7f) code
      (get-multibyte-mapper +gbk-to-ucs-table+ code)))

(defun mb-len-as-gbk (code)
  (declare (optimize speed #.*safety-0*)
           (type (unsigned-byte 8) code))
  (if (< code #x80) 1 2))

(defun gbk-continuation-byte-p (code)
  (declare (optimize speed #.*safety-0*)
           (type (unsigned-byte 8) code)
           (ignore code))
  t)

(declaim (muffle-conditions compiler-note))
(define-multibyte-encoding :gbk (:gbk :cp936)
  ucs-to-gbk gbk-to-ucs mb-len-as-gbk gbk-continuation-byte-p)
