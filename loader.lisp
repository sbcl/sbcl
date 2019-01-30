(defun load-sbcl-file (file &optional (exit t))
  (labels ((exit-sbcl (code)
             #+sbcl #.(if (eq :external
                              (nth-value 1 (find-symbol "EXIT" :sb-ext)))
                          `(,(find-symbol "EXIT" :sb-ext) :code code)
                          `(,(find-symbol "QUIT" :sb-ext) :unix-status code))
             #+ccl (ccl:quit code)
             #+abcl (ext:quit :status code)
             #+cmucl (unix:unix-exit code)
             #+ecl (si:quit code)
             #+clisp (ext:quit code)
             (return-from load-sbcl-file)))
    (restart-case
        (load file)
      (abort-build ()
        :report "Abort building SBCL."
        (exit-sbcl 1)))
    (when exit (exit-sbcl 0))))
