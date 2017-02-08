(defun find-test-files ()
  (append (directory "*.pure.lisp") (directory "*.impure.lisp")))

(defun file-tests (file)
  (with-simple-restart (continue "Skip file ~S" file)
    (let ((string (with-open-file (stream file
                                          :direction :input
                                          :element-type 'character
                                          :external-format :utf-8)
                    (with-output-to-string (output)
                      (let ((buffer (make-string 4096)))
                        (loop for characters-read = (read-sequence buffer stream)
                           do (write-sequence buffer output :end characters-read)
                           while (= characters-read 4096)))))))
      (loop with prefix = "(with-test "
         for match = (search prefix string) then (search prefix string :start2 position)
         for position = (when match (+ match (length prefix)))
         while position
         appending (with-simple-restart (continue "Skip WITH-TEST form")
                     (let* ((options (read-from-string string nil nil :start position))
                            (name    (getf options :name)))
                       (list (cons name file))))))))

(defun all-tests ()
  (let ((skip-count 0))
    (handler-bind ((error (lambda (condition)
                            (declare (ignore condition))
                            (incf skip-count)
                            (continue))))
      (prog1
          (mapcan #'file-tests (find-test-files))
        (when (plusp skip-count)
          (warn "Skipped ~D WITH-TEST form~:P" skip-count))))))

(defun in-name-p (query name)
  (let ((query (let ((*package* (find-package :cl-user)))
                 (with-standard-io-syntax
                   (read-from-string query)))))
    (labels ((in-name-p (name)
               (typecase name
                 (cons   (some #'in-name-p name))
                 (t      (equal query name)))))
      (in-name-p name))))

(defun in-name-p/fuzzy (query name)
  (let ((query (string-downcase query)))
    (labels ((in-name-p (name)
               (typecase name
                 (cons   (some #'in-name-p name))
                 (symbol (search query (string-downcase name)))
                 (t      (search query (princ-to-string name))))))
      (in-name-p name))))

(defun tests-for (query &key (test #'in-name-p/fuzzy))
  (remove query (all-tests) :test-not test :key #'car))

(defun print-matches (matches &key (stream *standard-output*))
  (let ((*print-right-margin* most-positive-fixnum)
        (*print-miser-width* most-positive-fixnum))
    (loop for (name . file) in matches
       do (format stream "~32@<~A:~> ~S~%"
                  (enough-namestring file *default-pathname-defaults*) name))))

(multiple-value-bind (predicate queries)
    (let ((args (rest sb-ext:*posix-argv*)))
      (if (equal (first args) "--fuzzy")
          (values #'in-name-p/fuzzy (rest args))
          (values #'in-name-p args)))
  (dolist (query queries)
    (print-matches (tests-for query :test predicate))))
