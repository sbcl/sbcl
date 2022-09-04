(require :sb-md5)

(let ((stream (make-string-output-stream)))
  (with-package-iterator (iter "SB-MD5" :internal :external)
    (loop
     (multiple-value-bind (flag symbol) (iter)
       (unless flag (return))
       (when (and (fboundp symbol) (not (macro-function symbol)))
         (let ((code (sb-kernel:fun-code-header (fdefinition symbol))))
           (disassemble code :stream stream)
           (let ((text (get-output-stream-string stream)))
             (when (search "ALIEN-FUNCALL" text)
               (error "Compiler bug on ~S" code)))))))))

(setq run-tests::*allowed-inputs* :any) ; makes random pathnames without aid of WITH-SCRATCH-FILE
(load "../contrib/sb-md5/md5-tests.lisp")
