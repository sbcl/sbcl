;;;
;;; invocation: "$SBCL --script genmakefile.lisp >Makefile"
;;;
(let ((files (append (directory "*.test.sh")
                     (directory "*.*pure*.lisp"))))
  (format t "~%all:")
  (dolist (file (mapcar 'pathname-name files))
    (format t " \\~% $(LOGDIR)/~a.log" file))
  (terpri)
  (terpri)
  (dolist (file files)
    (let ((in (format nil "~a.~a" (pathname-name file) (pathname-type file)))
          (out (format nil "$(LOGDIR)/~a.log" (pathname-name file))))
      (format t "~a: ~a~%~a./run-tests.sh ~a >~a 2>&1~%"
              out in #\tab in out))))
