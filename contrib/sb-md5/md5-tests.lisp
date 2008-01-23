(defpackage #:sb-md5-tests
  (:use #:sb-md5 #:cl #:sb-rt))
(in-package #:sb-md5-tests)

(defun byte-array-to-hex-string (bytevec)
  (format nil "~(~{~2,'0X~}~)" (coerce bytevec 'list)))

(defun one-shot-test (string)
  (md5sum-string string :external-format :ascii))

(defun incremental-test (string)
  (let ((bytevec (sb-ext:string-to-octets string :external-format :ascii))
        (state (sb-md5:make-md5-state)))
    (dotimes (i (length bytevec) (sb-md5:finalize-md5-state state))
      (sb-md5:update-md5-state state bytevec :start i :end (1+ i)))))

(defun fill-pointer-test (string)
  (let* ((bytevec (sb-ext:string-to-octets string :external-format :ascii))
         (fillvec (let ((x (make-array (* 2 (length bytevec))
                                       :fill-pointer 0
                                       :element-type '(unsigned-byte 8))))
                    (dotimes (i (length bytevec) x)
                      (vector-push (aref bytevec i) x)))))
    (sb-md5:md5sum-sequence fillvec)))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun tests-for-test-suite (basename index string expected-result)
  (loop for (test-kind testfun) in '(("ONE-SHOT" one-shot-test)
                                     ("INCREMENTAL" incremental-test)
                                     ("FILL-POINTER" fill-pointer-test))
     collect `(deftest ,(intern (format nil "~A.~A.~A" basename test-kind index))
                  (string= (byte-array-to-hex-string (funcall ',testfun ,string))
                           ,expected-result)
                t) into test-forms
       finally (return `(progn ,@test-forms))))
) ; EVAL-WHEN

(macrolet
    ((define-rfc1321-tests (test-list)
         `(progn
           ,@(loop for i upfrom 0
                   for (string . expected-result) in test-list
                   collect (tests-for-test-suite "SB-MD5.RFC1321" i string expected-result)))))
  (define-rfc1321-tests
      (("" . "d41d8cd98f00b204e9800998ecf8427e")
       ("a" ."0cc175b9c0f1b6a831c399e269772661")
       ("abc" . "900150983cd24fb0d6963f7d28e17f72")
       ("message digest" . "f96b697d7cb7938d525a2f31aaf161d0")
       ("abcdefghijklmnopqrstuvwxyz" . "c3fcd3d76192e4007dfb496cca67e13b")
       ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" . "d174ab98d277d9f5a5611c2c9f419d9f")
       ("12345678901234567890123456789012345678901234567890123456789012345678901234567890" . "57edf4a22be3c955ac49da2e2107b67a"))))

(macrolet
    ((define-other-tests (test-list)
         `(progn
           ,@(loop for i upfrom 0
                   for (string . expected-result) in test-list
                   collect (tests-for-test-suite "SB-MD5.OTHER" i string expected-result)))))
  (define-other-tests
      (;; From padding bug report by Edi Weitz
       ("1631901HERR BUCHHEISTERCITROEN NORD1043360796beckenbauer" . "d734945e5930bb28859ccd13c830358b")
       ;; Test padding for strings from 0 to 69*8 bits in size.
       ("" . "d41d8cd98f00b204e9800998ecf8427e")
       ("a" . "0cc175b9c0f1b6a831c399e269772661")
       ("aa" . "4124bc0a9335c27f086f24ba207a4912")
       ("aaa" . "47bce5c74f589f4867dbd57e9ca9f808")
       ("aaaa" . "74b87337454200d4d33f80c4663dc5e5")
       ("aaaaa" . "594f803b380a41396ed63dca39503542")
       ("aaaaaa" . "0b4e7a0e5fe84ad35fb5f95b9ceeac79")
       ("aaaaaaa" . "5d793fc5b00a2348c3fb9ab59e5ca98a")
       ("aaaaaaaa" . "3dbe00a167653a1aaee01d93e77e730e")
       ("aaaaaaaaa" . "552e6a97297c53e592208cf97fbb3b60")
       ("aaaaaaaaaa" . "e09c80c42fda55f9d992e59ca6b3307d")
       ("aaaaaaaaaaa" . "d57f21e6a273781dbf8b7657940f3b03")
       ("aaaaaaaaaaaa" . "45e4812014d83dde5666ebdf5a8ed1ed")
       ("aaaaaaaaaaaaa" . "c162de19c4c3731ca3428769d0cd593d")
       ("aaaaaaaaaaaaaa" . "451599a5f9afa91a0f2097040a796f3d")
       ("aaaaaaaaaaaaaaa" . "12f9cf6998d52dbe773b06f848bb3608")
       ("aaaaaaaaaaaaaaaa" . "23ca472302f49b3ea5592b146a312da0")
       ("aaaaaaaaaaaaaaaaa" . "88e42e96cc71151b6e1938a1699b0a27")
       ("aaaaaaaaaaaaaaaaaa" . "2c60c24e7087e18e45055a33f9a5be91")
       ("aaaaaaaaaaaaaaaaaaa" . "639d76897485360b3147e66e0a8a3d6c")
       ("aaaaaaaaaaaaaaaaaaaa" . "22d42eb002cefa81e9ad604ea57bc01d")
       ("aaaaaaaaaaaaaaaaaaaaa" . "bd049f221af82804c5a2826809337c9b")
       ("aaaaaaaaaaaaaaaaaaaaaa" . "ff49cfac3968dbce26ebe7d4823e58bd")
       ("aaaaaaaaaaaaaaaaaaaaaaa" . "d95dbfee231e34cccb8c04444412ed7d")
       ("aaaaaaaaaaaaaaaaaaaaaaaa" . "40edae4bad0e5bf6d6c2dc5615a86afb")
       ("aaaaaaaaaaaaaaaaaaaaaaaaa" . "a5a8bfa3962f49330227955e24a2e67c")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaa" . "ae791f19bdf77357ff10bb6b0e97e121")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaa" . "aaab9c59a88bf0bdfcb170546c5459d6")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "b0f0545856af1a340acdedce23c54b97")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "f7ce3d7d44f3342107d884bfa90c966a")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "59e794d45697b360e18ba972bada0123")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "3b0845db57c200be6052466f87b2198a")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "5eca9bd3eb07c006cd43ae48dfde7fd3")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "b4f13cb081e412f44e99742cb128a1a5")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "4c660346451b8cf91ef50f4634458d41")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "11db24dc3f6c2145701db08625dd6d76")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "80dad3aad8584778352c68ab06250327")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "1227fe415e79db47285cb2689c93963f")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "8e084f489f1bdf08c39f98ff6447ce6d")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "08b2f2b0864bac1ba1585043362cbec9")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "4697843037d962f62a5a429e611e0f5f")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "10c4da18575c092b486f8ab96c01c02f")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "af205d729450b663f48b11d839a1c8df")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "0d3f91798fac6ee279ec2485b25f1124")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "4c3c7c067634daec9716a80ea886d123")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "d1e358e6e3b707282cdd06e919f7e08c")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "8c6ded4f0af86e0a7e301f8a716c4363")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "4c2d8bcb02d982d7cb77f649c0a2dea8")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "bdb662f765cd310f2a547cab1cfecef6")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "08ff5f7301d30200ab89169f6afdb7af")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "6eb6a030bcce166534b95bc2ab45d9cf")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "1bb77918e5695c944be02c16ae29b25e")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "b6fe77c19f0f0f4946c761d62585bfea")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "e9e7e260dce84ffa6e0e7eb5fd9d37fc")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "eced9e0b81ef2bba605cbc5e2e76a1d0")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "ef1772b6dff9a122358552954ad0df65")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "3b0c8ac703f828b04c6c197006d17218")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "652b906d60af96844ebd21b674f35e93")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "dc2f2f2462a0d72358b2f99389458606")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "762fc2665994b217c52c3c2eb7d9f406")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "cc7ed669cf88f201c3297c6a91e1d18d")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "cced11f7bbbffea2f718903216643648")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "24612f0ce2c9d2cf2b022ef1e027a54f")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "b06521f39153d618550606be297466d5")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "014842d480b571495a4a0363793f7367")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "c743a45e0d2e6a95cb859adae0248435")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "def5d97e01e1219fb2fc8da6c4d6ba2f")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "92cb737f8687ccb93022fdb411a77cca")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "a0d1395c7fb36247bfe2d49376d9d133")
       ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" . "ab75504250558b788f99d1ebd219abf2"))))

(deftest sb-md5.md5sum-file.0
    (let ((file
           (loop with ret
                 for filename = (format nil "md5-test-~6,'0D" (random 100000))
                 do (with-open-file (stream filename :direction :output
                                            :if-exists nil
                                            :if-does-not-exist :create)
                      (when stream
                        (setf ret stream)))
                 when ret return ret)))
      (unwind-protect
          (string= (format nil "~(~{~2,'0X~}~)"
                           (coerce (md5sum-file file) 'list))
                   "d41d8cd98f00b204e9800998ecf8427e")
        (delete-file file)))
      t)

(deftest sb-md5.md5sum-sequence.error.0
    (handler-case (md5sum-sequence "foo")
      (type-error () :good))
  :good)
