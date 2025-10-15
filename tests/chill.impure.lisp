
(load "../src/cold/chill.lisp")

(defvar *build-order-data*
  (with-open-file (s "../src/cold/build-order.lisp-expr")
    (cons (read s) (read s))))

(defvar *chill-compatible-readtable*
  (let ((readtable (copy-readtable nil)))
    (let ((sharp-dot (get-dispatch-macro-character #\# #\. readtable)))
      (flet ((sharp-dot-suppressed (&rest args)
               (let ((*read-suppress* t))
                 (apply sharp-dot args))))
        (set-dispatch-macro-character #\# #\. #'sharp-dot-suppressed readtable)))
    readtable))

(defun read-file (stem &aux (*readtable* *chill-compatible-readtable*))
  (with-open-file (s (merge-pathnames stem (make-pathname
                                            :directory '(:relative :up)
                                            :type "lisp")))
    (let ((eof (cons nil nil)))
      (loop
       (let ((o (read s nil eof)))
         (when (eql o eof)
           (return)))))))

(with-test (:name (:chill :read-xc-files))
  (flet ((try-replacing (stem this that)
           (let ((position (search this stem)))
             (when position
               (concatenate 'string
                            (subseq stem 0 (1+ position))
                            (string-downcase that)
                            (subseq stem (+ position (length this) -1)))))))
    (let ((xc-stems-and-flags (car *build-order-data*)))
      (dolist (stem-and-flags xc-stems-and-flags)
        (unless (member :not-target (cdr stem-and-flags))
          (let* ((stem (car stem-and-flags))
                 (name (or (try-replacing stem "/{arch}/" (sb-cold::target-platform-keyword))
                           (try-replacing stem "/asm-target/" (sb-cold::backend-assembler-target-name))
                           stem)))
            (read-file name)))))))

(with-test (:name (:chill :read-target-2-files))
  (let ((target-2-stems-lists (cdr *build-order-data*)))
    (dolist (stems-list target-2-stems-lists)
      (dolist (stem stems-list)
        (read-file stem)))))
