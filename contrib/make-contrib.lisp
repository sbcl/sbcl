(setf (sb-ext:readtable-base-char-preference *readtable*) :both)
(defvar *system* (second sb-ext:*posix-argv*))
;; Convert dep filenames into module names. Depending on whether each Make step
;; saw the fasls as already-existing, or is making them, the dep is either the
;; unadorned fasl name, or containing the 'vpath' in it already.
(defvar *deps*
  (mapcar 'string-upcase (mapcar 'pathname-name (cddr sb-ext:*posix-argv*))))
 (format t "; Note: Building ~S~@[, deps=~S~]~%" *system* *deps*)
(mapc 'require *deps*)

(declaim (muffle-conditions (and compiler-note (not sb-c::unknown-typep-note))))

(defun run-defs-to-lisp (inputs output)
  (flet ((invoke (string &rest args)
           (apply (find-symbol string "SB-GROVEL") args)))
    (let ((c-file (merge-pathnames "runme.c" output))
          (all-headers)
          (all-definitions)
          (package))
      (dolist (input inputs)
        (if package
            (assert (eq (cdr input) package))
            (setf package (cdr input)))
        ;; Combine input specifications so that we run the C compiler once only
        (multiple-value-bind (headers definitions)
            (with-open-file (stream (merge-pathnames (make-pathname :type "lisp")
                                                     (car input))
                                    :direction :input)
              (values (read stream) (read stream)))
          (setf all-headers (nconc all-headers headers)) ; can be order-sensitive!
          (setf all-definitions (nconc all-definitions definitions))))
      (with-open-file (stream c-file :direction :output :if-exists :supersede)
        (invoke "PRINT-C-SOURCE" stream all-headers all-definitions package))
      (let* ((c-compiler-output (merge-pathnames #+unix "a.out" #+win32 "a.exe" output))
             (result (invoke "RUN-C-COMPILER" c-file c-compiler-output)))
        (unless (= result 0) (error "C compilation failed"))
        (let ((result
               (process-exit-code
                (run-program (namestring c-compiler-output) (list (namestring output))
                             :search nil :input nil :output *trace-output*))))
          (unless (= result 0) (error "C execution failed")))))))

(defparameter +genfile+ "generated-constants")
(defun perform (defsystem)
  (let* ((specified-sources (getf defsystem :components))
         ;; This path is basically arbitrary. I wanted to avoid creating
         ;; another directory under "obj/" but alas ...
         (objdir (format nil "../../obj/from-self/contrib/~A/" *system*))
         (*features* (append '(:sb-building-contrib) *features*
                             sb-impl:+internal-features+)))
    (ensure-directories-exist objdir)
    (sb-int:collect ((alien-constants) (flattened-sources) (fasls))
      (with-open-file (f (merge-pathnames "module-setup.lisp" objdir)
                         :direction :output :if-exists :supersede)
        (format f "~{(require \"~A\")~%~}" *deps*))
      (flattened-sources `(t "module-setup"))
      ;; Compile all files serially. :depends-on is just documentation for the user
      (sb-int:named-let flatten ((prefix "") (sources specified-sources))
        (dolist (source sources)
          (ecase (car source)
            (:module
             (let* ((subdir (cadr source))
                    (pathname (getf source :pathname subdir))
                    (newprefix (if (string= pathname  "")
                                   prefix
                                   (concatenate 'string prefix subdir "/"))))
               (unless (string= pathname "")
                 (ensure-directories-exist (format nil "~A~A" objdir newprefix)))
               (flatten newprefix (getf source :components))))
            (:file
             (let ((if-feature (getf source :if-feature)))
               (when (or (not if-feature) (sb-int:featurep if-feature))
                 (flattened-sources
                  `(nil ,(concatenate 'string prefix (cadr source)))))))
            (:sb-grovel-constants-file
             ;; We don't run sb-grovel as a contrib module for building other modules.
             ;; sb-grovel interacts with ASDF when REQUIREd, but this script doesn't.
             (destructuring-bind (specfile &key package if-feature &allow-other-keys)
                 (cdr source)
               (assert package)
               (when (or (not if-feature) (sb-int:featurep if-feature))
                 (unless (alien-constants) ; add in a source file
                   (flattened-sources `(t ,+genfile+)))
                 (alien-constants (cons specfile package))))))))
      (with-open-file (f (merge-pathnames "module-provide.lisp" objdir)
                         :direction :output :if-exists :supersede)
        (format f "(provide \"~A\")~%" (string-upcase *system*)))
      (flattened-sources `(t "module-provide"))

      (when (alien-constants)
        (load "../sb-grovel/defpackage") ; faster to interpret than compile
        (let ((*evaluator-mode* :compile)) (load "../sb-grovel/def-to-lisp"))
        (run-defs-to-lisp (alien-constants) ; specification files
                          (format nil "~A~A.lisp" objdir +genfile+)) ; file to generate
        ;; foreign-glue contains macros needed to compile the generated file
        (let ((*evaluator-mode* :compile)) (load "../sb-grovel/foreign-glue")))

      (with-compilation-unit ()
        (loop for (generated-p stem) in (flattened-sources)
              do
          (format t "Compile-File ~S~%" stem)
          (multiple-value-bind (output warnings errors)
              (compile-file  (if generated-p
                                 (format nil "~A~A.lisp" objdir stem)
                                 (format nil "~A.lisp" stem))
                             :output-file (format nil "~A~A.fasl" objdir stem))
            (declare (ignore warnings))
            (when errors (sb-sys:os-exit 1))
            (fasls output)
            (load output))))
      (let ((outputs (mapcar 'namestring (fasls)))
            (joined (format nil "../../obj/sbcl-home/contrib/~A.fasl" *system*)))
        (ensure-directories-exist joined)
        (with-open-file (asd (merge-pathnames (make-pathname :type "asd") joined)
                             :direction :output :if-exists :supersede
                             :if-does-not-exist :create)
          (format asd "(defsystem :~A :class require-system)~%" *system*))
        (sb-sys:os-exit
         (process-status
          (run-program #+unix "/bin/cat" #-unix "cat" outputs
                       ;; probably is /usr/bin/cat.exe but let's not assume that
                       #-unix :search #-unix t
                       :output joined :if-output-exists :supersede)))))))

(compile 'perform)
(let ((form (with-open-file (f (format nil "~A.asd" *system*))
              (let ((form (read f)))
                ;; each .asd file has an ERROR form preventing users from LOADing it
                (assert (eq (car form) 'error))
                (read f)))))
  (let ((eval (getf form :eval)))
    (when eval (eval eval)))
  (let ((bindings (getf form :bind))
        (*compile-verbose* nil)) ; set the default
    (progv (mapcar 'first bindings) (mapcar 'second bindings)
      (perform form))))
