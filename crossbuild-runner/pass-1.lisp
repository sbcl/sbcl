;; (setq *default-pathname-defaults* #p"") ; FIXME: causes redefinition warnings!
(let* ((target-name (second sb-ext:*posix-argv*))
       (build-dir (format nil "obj/xbuild/~A/" target-name))
       (ltf (format nil "~A/local-target-features" build-dir))
       (objroot (format nil "~A/from-host/" build-dir)))
  (ensure-directories-exist objroot)
  (defvar *sbcl-host-obj-prefix* objroot)
  (setq sb-sys:*stdout* (open (format nil "~A/stdout" objroot) :direction :output
                              :if-does-not-exist :create :if-exists :supersede)
        sb-sys:*stderr* (open (format nil "~A/stderr" objroot) :direction :output
                              :if-does-not-exist :create :if-exists :supersede))
  (defvar *sbcl-local-target-features-file* ltf)
  (with-open-file (*standard-output* ltf :direction :output
                                     :if-does-not-exist :create
                                     :if-exists :supersede)
    (let ((target-symbol (intern (string-upcase target-name) "KEYWORD")))
      (format t "(lambda (features) (union features (list :crossbuild-test :os-provides-dlopen ~s ~a~%"
              target-symbol
              (case target-symbol
                ((:x86 :x86-64) ":win32 :sb-thread :sb-safepoint")
                (t ":unix :linux :elf"))))
    ;; "features" contains the mandatory set of symbols for any build of that target.
    (dolist (features-filename '("features" "local-target-features"))
      (with-open-file (f (format nil "crossbuild-runner/backends/~a/~a"
                                 target-name features-filename))
        (let ((string (make-string (file-length f))))
          (read-sequence string f)
          (write-string string))))
    (format t ")))~%"))
  (load "make-host-1.lisp")
  (finish-output sb-sys:*stdout*)
  (finish-output sb-sys:*stderr*)
  (save-lisp-and-die (format nil "~A/xc.core" build-dir)))
