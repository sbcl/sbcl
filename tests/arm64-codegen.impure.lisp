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

;;; Stack slots stored or loaded as a pair are not accessed the other way
;;; nearby: a value saved before a short call and restored after it would
;;; otherwise wait for the store, its forwarding failing.
(with-test (:name (:ldp-stp :forwarding))
  (flet ((mismatched (form)
           (let ((stored (make-hash-table)) (bad '()))
             (dolist (line (ctu:disassembly-lines form) bad)
               ;; A label starts another straight-line run.
               (when (search ": " line)
                 (clrhash stored))
               (let ((p (search "[CFP, #" line)))
                 (when p
                   (let ((offset (parse-integer line :start (+ p 7) :junk-allowed t)))
                     (cond ((search "STP " line)
                            (setf (gethash offset stored) :pair
                                  (gethash (+ offset 8) stored) :pair))
                           ((search "STR " line)
                            (setf (gethash offset stored) :single))
                           ((search "LDP " line)
                            (when (or (eq (gethash offset stored) :single)
                                      (eq (gethash (+ offset 8) stored) :single))
                              (push line bad)))
                           ((search "LDR " line)
                            (when (eq (gethash offset stored) :pair)
                              (push line bad)))))))))))
    (assert (null (mismatched
                   '(lambda (f n)
                     (declare (function f) (fixnum n))
                     (let ((a 0) (b 0) (c 0))
                       (declare (fixnum a b c))
                       (dotimes (i n)
                         (funcall f)
                         (incf a)
                         (incf b 2)
                         (incf c 3))
                       (list a b c))))))
    (assert (null (mismatched
                   '(lambda (f v n)
                     (declare (function f) (simple-vector v) (fixnum n))
                     (let ((k 0))
                       (declare (fixnum k))
                       (dotimes (i n k)
                         (when (funcall f (svref v i))
                           (incf k))))))))))
