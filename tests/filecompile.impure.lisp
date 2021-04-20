(with-test (:name :compile-file-error-position-reporting
            :serial t)
  (dolist (input '("data/wonky1.lisp" "data/wonky2.lisp" "data/wonky3.lisp"))
    (let ((expect (with-open-file (f input) (read f))))
      (assert (stringp expect))
      (let ((err-string (with-output-to-string (*error-output*)
                          (compile-file input :print nil
                                              :output-file
                                              (scratch-file-name "fasl")))))
        (assert (search expect err-string))))))

#-sb-thread (sb-ext:exit :code 104)

(unlock-package "SB-INT")
(unlock-package "SB-IMPL")
(unlock-package "CL")
(rename-package "COMMON-LISP" "COMMON-LISP" '("CL"  "SB-XC"))

(defun slurp (filename)
  (with-open-file (stream filename :element-type '(unsigned-byte 8))
    (let ((res (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence res stream)
      res)))

;;; Assert that concurrent threads doing the same thing
;;; produce identical output.
(with-test (:name :parallel-compile-file)
  (with-scratch-file (thr1-out "fasl")
  (with-scratch-file (thr2-out "fasl")
  (with-scratch-file (thr3-out "fasl")
    (flet ((work (result)
             (let* ((*standard-output* (make-string-output-stream))
                    (*error-output* *standard-output*))
               (compile-file "../src/code/primordial-extensions"
                             :output-file result)
               (get-output-stream-string *standard-output*))))
      (dotimes (i 1)
        (let ((thr1 (sb-thread:make-thread #'work :arguments (list thr1-out)))
              (thr2 (sb-thread:make-thread #'work :arguments (list thr2-out)))
              (thr3 (sb-thread:make-thread #'work :arguments (list thr3-out))))
          (let ((string1 (sb-thread:join-thread thr1))
                (string2 (sb-thread:join-thread thr2))
                (string3 (sb-thread:join-thread thr3)))
            (declare (ignorable string1 string2 string3))
            #+nil
            (progn
              (format t "1:~%~A~%" string1)
              (format t "2:~%~A~%" string2)
              (format t "3:~%~A~%" string3)))
          (let ((bytes1 (slurp thr1-out))
                (bytes2 (slurp thr2-out))
                (bytes3 (slurp thr3-out)))
            (assert (equalp bytes1 bytes2))
            (assert (equalp bytes2 bytes3)))
          (delete-file thr1-out)
          (delete-file thr2-out)
          (delete-file thr3-out))))))))
