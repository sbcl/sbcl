#-arm64 (invoke-restart 'run-tests::skip-file)

(load "compiler-test-util.lisp")

(with-test (:name :alien-deref-indexed)
  (flet ((try (type left-shift)
           (let ((lines
                  (ctu:disassembly-lines
                   `(lambda (i)
                      (declare (optimize (debug 0)))
                      (let ((a (alien-funcall (extern-alien "f" (function (* ,type))))))
                        (deref a (sb-ext:truly-the sb-int:index i))))))
                 (look-for
                  (format nil ", LSL #~D]" left-shift)))
             ;; some line should have an LDR with an "extended" index
             (assert
              (= (count-if (lambda (line)
                             (and (search "LDR" line) (search look-for line)))
                           lines)
                 1)))))
    (try 'unsigned-short 1)
    (try 'unsigned-int   2)
    (try 'unsigned-long  3)))
