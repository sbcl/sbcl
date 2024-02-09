;; (setq *default-pathname-defaults* #p"") ; FIXME: causes redefinition warnings!
(sb-int:binding*
      (((configuration-name target-name features)
        (destructuring-bind (first second third) (cdr *posix-argv*)
          (values first second third)))
       (arch-symbol (intern (string-upcase target-name) "KEYWORD"))
       (features (read-from-string features))
       (subtract-features
        (mapcan (lambda (x) (if (typep x '(cons (eql not))) (list (cadr x))))
                features))
       (add-features (remove-if-not #'symbolp features))
       (build-dir (format nil "obj/xbuild/~A/" configuration-name))
       (ltf (format nil "~A/local-target-features" build-dir))
       (objroot (format nil "~A/from-host/" build-dir)))
  (ensure-directories-exist objroot)
  (defvar *sbcl-host-obj-prefix* objroot)
  (let ((makeflags (sb-ext:posix-getenv "MAKEFLAGS")))
    (when (search "--jobserver" makeflags)
      (setq sb-sys:*stdout* (open (format nil "~A/stdout" objroot) :direction :output
                                  :if-does-not-exist :create :if-exists :supersede)
            sb-sys:*stderr* (open (format nil "~A/stderr" objroot) :direction :output
                                  :if-does-not-exist :create :if-exists :supersede))))
  (defvar *sbcl-local-target-features-file* ltf)
  (with-open-file (*standard-output* ltf :direction :output
                                     :if-does-not-exist :create
                                     :if-exists :supersede)
    (format t "(lambda (features)
(set-difference (union features (list :crossbuild-test :os-provides-dlopen ~s~{ ~s~}~%"
            arch-symbol add-features)
    ;; "features" contains the mandatory set of symbols for any build of that target.
    (let ((features-filename "features"))
      (with-open-file (f (format nil "crossbuild-runner/backends/~a/~a"
                                 target-name features-filename))
        (let ((string (make-string (file-length f))))
          (read-sequence string f)
          (write-string string))))
    (format t ")) '~S))~%" subtract-features))
  (load "make-host-1.lisp")
  (finish-output sb-sys:*stdout*)
  (finish-output sb-sys:*stderr*)
  (save-lisp-and-die (format nil "~A/xc.core" build-dir)))
