(with-test (:name :posix-tmpfile)
  (let ((tmpfile (sb-unix:unix-tmpfile)))
    (multiple-value-bind (output warn err)
        (sb-c:compile-form-to-file '(progn (defvar *foo* 3)) tmpfile)
      (assert (and output (not warn) (not err)))
      (assert (eq (sb-int:info :variable :kind '*foo*) :special))
      (sb-int:clear-info :variable :kind '*foo*)
      (let ((stream (sb-impl::stream-from-stdio-file tmpfile :input t)))
        ;; compile-form-to-file leaves the file positioned to its end.
        (file-position stream 0)
        (sb-fasl::load-as-fasl stream nil nil)
        (sb-unix:unix-fclose tmpfile)
        (assert (eq (sb-int:info :variable :kind '*foo*) :special))
        (assert (= (symbol-value '*foo*) 3))))))

(defun load-from-tempfile (stdio-file)
  (sb-unix:unix-lseek (sb-impl::stdio-file-fd stdio-file) 0 sb-unix:l_set)
  (let ((stream (sb-impl::stream-from-stdio-file stdio-file)))
    (prog1 (load stream :print t) (sb-unix:unix-fclose stdio-file))))

(with-test (:name :compile-to-tmpfile-and-load)
  (with-scratch-file (srcfilename "lisp")
    (with-open-file (f srcfilename :if-does-not-exist :create :direction :output)
      (format f "(defun negatefoo (x) (- x))"))
    ;; source is a named temp file but the correspoding fasl is anonymous.
    ;; In order to compile from anonymous tempfiles, we'd have to somehow circumvent
    ;; the CLHS convention that streams as pathname designators are coerced to pathnames
    ;; and then opened. LOAD is (astonishingly?) exempt from that treatment though.
    (let ((tempfile (sb-c:compile-file-to-tempfile srcfilename)))
      (load-from-tempfile tempfile))
    (assert (= (funcall 'negatefoo 1) -1))))
