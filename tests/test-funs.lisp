(defvar *test-evaluator-mode* :compile)

;;; Set to T to always print summary, or :SLOW to print only if > 1 second.
;;; (Additionally the summary will be printed if the overhead exceeds
;;; 1 microsecond, so that I might figure out where it's occuring)
(defvar *summarize-test-times* nil)

(defun summarize-within-file-elapsed-times (file start-time)
  (let* ((actual-total (- (get-internal-real-time) start-time))
         (accounted-total (reduce #'+ test-util:*elapsed-times* :key #'car))
         (unaccounted-total (- actual-total accounted-total)))
    (when (plusp unaccounted-total)
      (push (cons unaccounted-total "(overhead)") test-util:*elapsed-times*))
    (when (or (eq *summarize-test-times* t)
              (and (eq *summarize-test-times* :slow-tests)
                   (> actual-total internal-time-units-per-second)))
      (format t "~2&Tests ordered by descending elapsed time:~%")
      (dolist (x (sort test-util:*elapsed-times* #'> :key #'car))
        (let ((*print-pretty* nil))
          (format t "~6d ~a~%" (car x) (cdr x))))
      (format t "~6d TOTAL TIME (~a)~%" actual-total file))))

(defun clear-test-status ()
  (with-open-file (stream #.(merge-pathnames "test-status.lisp-expr"
                                             *load-pathname*)
                          :direction :output
                          :if-exists :supersede)
    (write-line "NIL" stream)))

;;; Only the pure load-only (vs. compile+load) tests will be run concurrently.
;;; The impure ones won't because we don't know what side-effects they depend on
;;; (they are stateful with regard to order within the file).
;;; Potentially the pure '.cload' tests could work.
(defun load-test (file)
  (let ((test-util::*deferred-test-forms*)
        (test-util:*elapsed-times*)
        (start-time (get-internal-real-time)))
    (declare (special test-util::*deferred-test-forms*))
    (makunbound 'test-util::*deferred-test-forms*)
    (let ((*features* (append *features* sb-impl:+internal-features+)))
      (load file :external-format :utf-8))
    (when (boundp 'test-util::*deferred-test-forms*)
      ;; Execute all tests that were wrapped in WITH-TEST
      (let ((holder test-util::*deferred-test-forms*))
        ;; Sort the slow tests in front of non-slow tests
        (setf (elt holder 1) (nconc (nreverse (elt holder 1))
                                    (nreverse (elt holder 2)))
              (elt holder 2) nil)
        (let ((n (elt holder 0))
              (threads))
          (format t "~&// Enabling ~D threads~%" n)
          (dotimes (i n)
            (push (sb-thread:make-thread
                   (lambda ()
                     (loop
                      (let ((test (atomic-pop (svref holder 1))))
                        (unless test (return))
                        (test-util::run-test-concurrently test)))))
                  threads))
          (dolist (thr threads)
            (sb-thread:join-thread thr)))))
    (summarize-within-file-elapsed-times file start-time)))

(defun cload-test (file)
  (let ((test-util:*elapsed-times*)
        (start-time (get-internal-real-time)))
    (with-scratch-file (fasl "fasl")
      (let ((*features* (append *features* sb-impl:+internal-features+)))
        (compile-file file :print nil :output-file fasl))
      (test-util::record-test-elapsed-time "(compile-file)" start-time)
      (load fasl)
      ;; TODO: as above, execute queued tests if within-file concurrency was enabled.
      )
    (summarize-within-file-elapsed-times file start-time)))

(defun sh-test (file)
  (clear-test-status)
  (progn
    (test-util::setenv "TEST_SBCL_EVALUATOR_MODE"
                        (string-downcase *test-evaluator-mode*))
    (let ((process (sb-ext:run-program (or #+sunos (posix-getenv "SHELL")
                                           "/bin/sh")
                                       (list (native-namestring file))
                                       :output *error-output*)))
      (let ((*failures* nil))
        (test-util:report-test-status))
      (sb-ext:exit :code (process-exit-code process)))))
