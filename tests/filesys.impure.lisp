;;; DIRECTORY used to treat */** as **.
(with-test (:name (directory :*/**))
  ;; FIXME: this test should be redone to construct a controlled file
  ;; hierarchy for listing. If test files or test runners ever get
  ;; reorganized and it turns out there are no subdirectories under
  ;; the *DEFAULT-PATHNAME-DEFAULTS* when this gets run, this test
  ;; will pass but fail to test the behavior it's supposed to.
  ;;
  ;; and FIXME: I've observed this test to fail in a few ways
  ;; which I've never been able to recreate by trial and error:
  ;; (1) it gets "NIL is not a (OR PATHNAME STRING ...)"
  ;;     which presumably means it was looking for a pathname-designator
  ;; (2) Somehow the REDUCE expression winds up with "run-sbcl.sh" in its
  ;;     output where the DIRECTORY expression does not.
  ;;     So the fact is that something is *changing* the directory up 1 level,
  ;;     otherwise there is no way that run-sbcl.sh appears. But what?
  (assert (equal (directory "*/**/*.*")
                 ;; Each call to DIRECTORY sorts its results, but if
                 ;; our files' truenames are random (e.g., if files
                 ;; here are symlinks to hashes of file content), then
                 ;; we need to merge the results of the inner
                 ;; DIRECTORY calls.
                 (reduce
                  (lambda (list1 list2)
                    ;; Depends on all truenames having namestrings.
                    ;; (Which they do; noted for future reference.)
                    (merge 'list list1 list2 'string< :key 'namestring))
                  (mapcar (lambda (directory)
                            (directory (merge-pathnames "**/*.*" directory)))
                          (directory "*/"))))))

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

