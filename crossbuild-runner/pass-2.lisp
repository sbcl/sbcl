(defvar *sbcl-local-target-features-file*
  (format nil "obj/xbuild/~A/local-target-features" (second sb-ext:*posix-argv*)))
(load "src/cold/shared.lisp")
(in-package "SB-COLD")
(let* ((target-name (second sb-ext:*posix-argv*))
       (build-dir (format nil "obj/xbuild/~A/" target-name))
       (objroot (format nil "~A/from-xc/" build-dir)))
  (ensure-directories-exist objroot)
  (defvar *host-obj-prefix* (format nil "~A/from-host/" build-dir))
  (defvar *target-obj-prefix* objroot)
  (setq sb-sys:*stdout* (open (format nil "~A/stdout" objroot) :direction :output
                              :if-does-not-exist :create :if-exists :supersede)
        sb-sys:*stderr* (open (format nil "~A/stderr" objroot) :direction :output
                              :if-does-not-exist :create :if-exists :supersede)))
(load "src/cold/set-up-cold-packages.lisp")
(load "src/cold/defun-load-or-cload-xcompiler.lisp")
(load-or-cload-xcompiler #'host-load-stem)

;;; Redefine STEM-SOURCE-PATH to take 'stuff-groveled-from-headers' from the
;;; architecture-dependent location, but otherwise the normal location.
(host-sb-int:encapsulate 'stem-source-path 'wrap
  (lambda (realfun stem)
    (if (string= stem "output/stuff-groveled-from-headers")
        (format nil "crossbuild-runner/backends/~a/stuff-groveled-from-headers.lisp"
                (string-downcase (sb-cold::target-platform-keyword)))
        (funcall realfun stem))))

(format t "~&Target features: ~S~%" sb-xc:*features*)
(let ((warnings (sb-xc:with-compilation-unit ()
                  (load "src/cold/compile-cold-sbcl.lisp")
                  sb-c::*undefined-warnings*)))
  (finish-output host-sb-sys:*stdout*)
  (finish-output host-sb-sys:*stderr*)
  (when (and warnings (not (target-featurep :win32)))
    (error "Fail")))
