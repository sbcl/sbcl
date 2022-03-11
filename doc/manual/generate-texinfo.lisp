(map nil #'require '("asdf" "uiop"))
(asdf:initialize-source-registry
 '(:source-registry :ignore-inherited-configuration))

(with-compilation-unit ()
  (load "docstrings.lisp"))

;;;; Generating documentation strings

(defvar *contrib-directory* #P"../../contrib/")

(defvar *asdf-object-cache-directory* #P"../../obj/asdf-cache/")

(defvar *documented-packages*
  '("COMMON-LISP" "SB-ALIEN" "SB-DEBUG" "SB-EXT" "SB-GRAY" "SB-MOP"
    "SB-PCL" "SB-SYS" "SB-SEQUENCE" "SB-UNICODE" "SB-PROFILE"
    "SB-THREAD"))

(defun documented-contribs (&key (exclude '("asdf")))
  (loop for texinfo-file in (directory (merge-pathnames
                                        "*/*.texinfo" *contrib-directory*))
        for name = (car (last (pathname-directory texinfo-file)))
        for package = (string-upcase name)
        when (cond
               ((find name exclude :test #'string=)
                nil)
               ((find name result :test #'string= :key #'car)
                nil)
               (t
                t))
        collect (cons name package) into result
        finally (return result)))

(defun generate-docstrings-texinfo (runtime
                                    &key (docstring-directory "docstrings/"))
  (let* ((contribs (sort (documented-contribs) #'string< :key #'car))
         (packages (sort (append *documented-packages*
                                 (map 'list #'cdr contribs))
                         #'string<)))
    (format t "/creating docstring snippets~@
               ~2@Tfrom SBCL=\'~A\'~@
               ~2@Tfor documented contribs~%~4@T~A~@
               ~2@Tfor packages~%~4@T~A~%"
            runtime (map 'list #'car contribs) packages)
    (map nil (lambda (contrib) (require (car contrib))) contribs)
    (apply #'sb-texinfo:generate-includes docstring-directory packages)))

;;;; Special cases: external formats list, package locks, variables.template

(defun replace-all (new old string)
  (with-output-to-string (stream)
    (loop with old-length = (length old)
       for start = 0 then (+ offset old-length)
       for offset = (search old string :start2 start)
       while offset
       do (write-string (subseq string start offset) stream)
         (write-string new stream)
       finally (write-string (subseq string start) stream))))

(defun expand-variables (&key (input-file "variables.template")
                           (output-file "variables.texinfo"))
  (format t "/expanding variables in ~A~%" output-file)
  (let* ((version (lisp-implementation-version))
         (date (multiple-value-bind (second minute hour day month year)
                   (decode-universal-time (get-universal-time))
                 (declare (ignore second minute hour day))
                 (format nil "~D-~2,'0D" year month)))
         (template (uiop:read-file-string input-file))
         (expanded (replace-all version "@VERSION@"
                                (replace-all date "@MONTH@" template))))
    (with-open-file (output output-file
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string expanded output))))

(defun generate-external-format-texinfo (&optional (output-file "encodings.texi-temp"))
  (format t "/creating ~A~%" output-file)
  (with-open-file (stream output-file :direction :output :if-exists :supersede)
    (flet ((table (items)
             (format stream "@table @code~%~%")
             (loop for (canonical-name . names) in items
                do (format stream "@item ~S~%~{@code{~S}~^, ~}~%~%"
                           canonical-name names))
             (format stream "@end table~%")))
      (let (result)
        (loop for ef across sb-impl::*external-formats*
              when (sb-impl::external-format-p ef)
              do
              (pushnew (sb-impl::ef-names ef) result :test #'equal))
        (table (sort result #'string< :key #'car))))))

;;;; Entry point

(destructuring-bind (program runtime docstring-directory) *posix-argv*
  (declare (ignore program))
  (generate-docstrings-texinfo
   runtime :docstring-directory docstring-directory)

  (expand-variables)
  (generate-external-format-texinfo))
