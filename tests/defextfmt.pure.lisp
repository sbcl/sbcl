
;; test for lp#659107
(with-test (:name :cmdline-setq-external-format
                  :skipped-on (not :sb-unicode))
  (with-scratch-file (script "lisp")
    (dolist (ef-name '(:utf8 :utf16le :utf16be :utf32le :utf32be))
      (with-open-file (stream script :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create
                              :external-format ef-name)
        (format stream "(defvar s \"what? ~A\"~%)" (name-char "GRINNING_FACE"))
        (format stream "(sb-sys:os-exit
 (if (and (string= (subseq s 0 6) \"what? \") (char= (char s 6) #\\grinning_face)) 0 1))~%"))
      (let ((process (run-program
                      sb-ext:*runtime-pathname*
                      (list "--core" sb-int:*core-string*
                            "--noinform" "--no-sysinit" "--no-userinit" "--noprint"
                            "--disable-debugger"
                            "--eval" (format nil "(setq *default-external-format* ~s)" ef-name)
                            "--load" script)
                      :error t)))
        #+win32
        (process-close process)
        (assert (zerop (process-exit-code process)))))))
