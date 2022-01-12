;; -*- coding: utf-8 -*-
#+(or (not sb-unicode) unicode-lite) (invoke-restart 'run-tests::skip-file)

(let ((str (coerce '(#\u0041 #\uff71 #\uff21 #\u3042 #\u6f3e #\u71f9 #\u91ed)
                   'string))
      (eucjp '(#x41 #x8e #xb1 #xa3 #xc1 #xa4 #xa2 #xdf #xa1 #xe0 #xa1
               #x8f #xe3 #xaf))
      (sjis '(#x41 #xb1 #x82 #x60 #x82 #xa0 #xe0 #x40 #xe0 #x9f #xfb #xbd))
      (file (scratch-file-name)))
  (dolist (pair (list (list eucjp :euc-jp)
                      (list sjis :shift_jis)))
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
