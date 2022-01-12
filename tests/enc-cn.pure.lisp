;;; -*- coding: utf-8 -*-
;;; enc-cn.impure.lisp: test case for enc-cn.lisp and enc-cn-tbl.lisp
#+(or (not sb-unicode) unicode-lite) (invoke-restart 'run-tests::skip-file)

(let ((str (coerce '(#\u0031 #\u0041 #\uff21 #\u3042 #\u80e1 #\u73a5 #\u59ee)
                   'string))
      (gbk '(#x31 #x41 #xa3 #xc1 #xa4 #xa2 #xba #xfa #xab #x68 #x8a #xac))
      (file (scratch-file-name)))
  (dolist (pair (list (list gbk :gbk)))
    (destructuring-bind (bytes enc) pair
      ;; check if output works
      (with-open-file (s file :direction :output
                         :if-exists :supersede :external-format enc)
        (write-string str s))
      (with-open-file (s file :direction :input
                         :element-type '(unsigned-byte 8))
        (loop for c in bytes
           do (assert (eql (read-byte s) c))))

      ;; check if input works
      (with-open-file (s file :direction :input :external-format enc)
        (loop for c across str
           do (assert (eql (read-char s) c))))

      ;; check if string conversion works
      (assert (equal (coerce (string-to-octets str :external-format enc)
                             'list)
                     bytes))
      (assert (equal (octets-to-string (coerce bytes
                                               '(vector (unsigned-byte 8)))
                                       :external-format enc)
                     str))))
  (delete-file file))
;;; success
