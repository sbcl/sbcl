#-sb-unicode
(sb-ext:quit :unix-status 104)

(let ((p "eucjp-test.data")
      (eucjp "eucjp-test-eucjp.data")
      (utf8 "eucjp-test-utf8.data"))

  ;; generate test data
  (with-open-file (in "eucjp-test.lisp-expr" :direction :input)
    (with-open-file (out-eucjp eucjp :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-exists :supersede)
      (with-open-file (out-utf8 utf8 :direction :output
                                :external-format :utf-8
                                :if-exists :supersede)
        (do ((euc (read in nil) (read in nil))
             (ucs (read in nil) (read in nil))
             (i 0 (1+ i)))
            ((or (null euc) (null ucs)))
          ;; write EUC-JP data as binary
          (let ((out out-eucjp))
            (when (>= euc #x10000)
              (write-byte (ldb (byte 8 16) euc) out))
            (when (>= euc #x100)
              (write-byte (ldb (byte 8 8) euc) out))
            (write-byte (ldb (byte 8 0) euc) out)
            (when (= (mod i 32) 31)
              (write-byte #x0a out)))
          ;; trust UTF-8 external format
          (let ((out out-utf8))
            (write-char (code-char ucs) out)
            (when (= (mod i 32) 31)
              (write-char (code-char #x0a) out)))))))

  ;; check if input works
  (with-open-file (in1 eucjp :direction :input
                       :external-format :euc-jp)
    (with-open-file (in2 utf8 :direction :input
                         :external-format :utf-8)
      (do ((c1 (read-char in1 nil) (read-char in1 nil))
           (c2 (read-char in2 nil) (read-char in2 nil)))
          ((and (null c1) (null c2)))
        (assert (eql c1 c2)))))

  ;; check if output works
  (with-open-file (in utf8 :direction :input
                      :external-format :utf-8)
    (with-open-file (out p :direction :output
                         :external-format :euc-jp
                         :if-exists :supersede)
      (do ((c (read-char in nil) (read-char in nil)))
          ((null c))
        (write-char c out))))
  (with-open-file (in1 eucjp :direction :input
                       :element-type '(unsigned-byte 8))
    (with-open-file (in2 p :direction :input
                         :element-type '(unsigned-byte 8))
      (do ((b1 (read-byte in1 nil) (read-byte in1 nil))
           (b2 (read-byte in2 nil) (read-byte in2 nil)))
          ((and (null b1) (null b2)))
        (assert (eql b1 b2)))))
  (delete-file p)
  (delete-file eucjp)
  (delete-file utf8))

;; check if string conversion works
(with-open-file (in "eucjp-test.lisp-expr" :direction :input)
  (do ((euc (read in nil) (read in nil))
       (ucs (read in nil) (read in nil))
       (i 0 (1+ i)))
      ((or (null euc) (null ucs)))
    (let ((o (coerce (cond ((>= euc #x10000)
                            (list (ldb (byte 8 16) euc)
                                  (ldb (byte 8 8) euc)
                                  (ldb (byte 8 0) euc)))
                           ((>= euc #x100)
                            (list (ldb (byte 8 8) euc)
                                  (ldb (byte 8 0) euc)))
                           (t (list euc)))
                     '(vector (unsigned-byte 8))))
          (s (string (code-char ucs))))
      (assert (equal (octets-to-string o :external-format :euc-jp) s))
      (assert (equal (coerce (string-to-octets s :external-format :euc-jp)
                             'list)
                     (coerce o 'list))))))
;;; success
(sb-ext:quit :unix-status 104)
