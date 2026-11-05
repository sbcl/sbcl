#-arm64 (invoke-restart 'run-tests::skip-file)

(load "compiler-test-util.lisp")

(with-test (:name :alien-deref-indexed)
  (flet ((try (type left-shift)
           (let ((lines
                  (ctu:disassembly-lines
                   `(lambda (i)
                      (declare (optimize (debug 0)))
                      (let ((a (alien-funcall (extern-alien "f" (function (* (unsigned ,type)))))))
                        (deref a (sb-ext:truly-the sb-int:index i))))))
                 (look-for
                  (format nil ", LSL #~D]" left-shift)))
             ;; some line should have an LDR with an "extended" index
             (unless
                 (= (count-if (lambda (line)
                                (and (search "LDR" line) (search look-for line)))
                              lines)
                    1)
               (error "~{~a~%~}" lines)
               ))))
    (try 16 1)
    (try 32 2)
    (try 64 3)))

(with-test (:name (:ldp-stp :aligned))
  (dolist (line (ctu:disassembly-lines
                 '(lambda (a b c f)
                    (declare (function f))
                    (funcall f)
                    (list a b c))))
    (let ((p (search "[CFP, #" line)))
      (when (and p (or (search "LDP" line) (search "STP" line)))
        (assert (zerop (mod (parse-integer line :start (+ p 7) :junk-allowed t)
                            16)))))))
