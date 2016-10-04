(defun load-sbcl-file (file)
  (labels ((exit-sbcl (code)
             #+sbcl (sb-ext:exit :code code)
             #+clisp (ccl:quit code)
             #+abcl (ext:quit :status code)
             #+cmucl (unix:unix-exit code)
             #+ecl (si:quit code)
             (return-from load-sbcl-file)))
    (restart-case
        (load file)
      (abort ()
        :report "Abort building SBCL."
        (exit-sbcl 1)))
    (exit-sbcl 0)))
