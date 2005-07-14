(load "../asdf/asdf")

(setf asdf::*central-registry*
      '((merge-pathnames "systems/" (truename (sb-ext:posix-getenv "SBCL_HOME")))))
(push :sb-building-contrib *features*)
(asdf:operate 'asdf:load-op *system*)

(defvar *system-stub* (make-pathname :name *system* :type "lisp"))

(when (probe-file (compile-file-pathname *system-stub*))
  (error "fasl file exists"))

(with-open-file (s *system-stub* :direction :output :if-exists :error)
  (print '(unless (member "ASDF" *modules* :test #'string=)
           (load (merge-pathnames "asdf/asdf.fasl" (truename (sb-ext:posix-getenv "SBCL_HOME")))))
         s)
  ;; This addition to *central-registry* allows us to find contribs
  ;; even if the user has frobbed the original contents.
  (print `(let ((asdf:*central-registry* (cons (merge-pathnames "systems/"
                                                                (truename (sb-ext:posix-getenv "SBCL_HOME")))
                                               asdf:*central-registry*)))
           (asdf::module-provide-asdf ,*system*))
         s))

(compile-file *system-stub*)
(delete-file *system-stub*)
