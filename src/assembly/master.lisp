;; If you change this, note that 'slam.lisp' has to be able to parse this form
(mapc (lambda (x) (load (sb-cold:stem-source-path x) :verbose nil :print nil))
      '("src/assembly/{arch}/tramps"
        "src/assembly/{arch}/assem-rtns"
        "src/assembly/{arch}/array"
        "src/assembly/{arch}/arith"
        "src/assembly/{arch}/alloc"))

(format t "~&Asm routine hit counters:~%")
(let ((v sb-vm::*asm-routine-hit-counter-map*))
  (dotimes (i (length v))
    (format t " ~3d = ~a~%" i (aref v i))))
