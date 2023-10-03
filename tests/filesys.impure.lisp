(defvar *scratchdir* (pathname (test-util:scratch-dir-name)))
(ensure-directories-exist *scratchdir*)
(setq *default-pathname-defaults* *scratchdir*)

(with-test (:name (open :if-does-not-exist restart))
  (flet ((do-open (restart)
           (let ((filename "does-not-exist"))
             (unwind-protect
                  (handler-bind
                      ((file-does-not-exist
                         (lambda (condition)
                           (let ((restart (find-restart restart condition)))
                             (invoke-restart restart)))))
                    (close (open filename :direction :output)))
               (assert (probe-file filename))
               (delete-file filename)))))
    (do-open 'sb-impl::create)))

(with-test (:name (open :if-exists restart))
  (labels ((read-file (filename)
             (with-open-file (stream filename)
               (let ((result (make-string (file-length stream))))
                 (read-sequence result stream)
                 result)))
           (do-open (restart expected-content)
             (let ((filename "exists"))
               (with-open-file (stream filename :direction :output
                                                :if-does-not-exist :create)
                 (write-string "foo" stream))
               (unwind-protect
                    (progn
                      (handler-bind
                          ((file-exists
                             (lambda (condition)
                               (let ((restart (find-restart restart condition)))
                                 (invoke-restart restart)))))
                        (let ((stream (open filename :direction :output)))
                          (write-string "bar" stream)
                          (close stream)))
                      (assert (equal expected-content (read-file filename))))
                 (delete-file filename)
                 (ignore-errors
                  (delete-file (concatenate 'string filename ".bak")))))))
    (do-open 'sb-impl::supersede "bar")
    (do-open 'sb-impl::overwrite "bar")
    (do-open 'sb-impl::rename "bar")
    (do-open 'append "foobar")))

(delete-directory *scratchdir*)

