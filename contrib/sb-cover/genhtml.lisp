;;; A frontend for the SBCL code coverage facility. Written by Juho
;;; Snellman, and placed under public domain.

(in-package #:sb-cover)

(defun clear-coverage ()
  "Clear all files from the coverage database. The files will be re-entered
into the database when the FASL files (produced by compiling
STORE-COVERAGE-DATA optimization policy set to 3) are loaded again into the
image."
  (clrhash (code-coverage-hashtable))
  (setf (cdr *code-coverage-info*) nil))

(defun save-coverage ()
  "Returns an opaque representation of the current code coverage state.
The only operation that may be done on the state is passing it to
RESTORE-COVERAGE. The representation is guaranteed to be readably printable.
A representation that has been printed and read back will work identically
in RESTORE-COVERAGE."
  (refresh-coverage-bits)
  (loop for file being the hash-keys of (code-coverage-hashtable)
        using (hash-value states)
        ;; a regression test fails without a COPY-SEQ here even though IRL we expect
        ;; to store these in a file right away (making the copy immaterial)
        collect (list* file (covered-file-paths states)
                       (copy-seq (covered-file-executed states)))))

(flet ((set-image-states (coverage-state operation)
         ;; This does not update coverage stored in code object headers
         (loop for (filename paths . bits) in coverage-state
               do (let ((image-states (gethash filename (code-coverage-hashtable))))
                    (cond ((not image-states) ; use all the new data
                           (let ((info (sb-c::make-coverage-instrumented-file paths nil nil)))
                             (replace (covered-file-executed info) bits)
                             (setf (gethash filename (code-coverage-hashtable)) info)))
                          ((equal (covered-file-paths image-states) paths)
                           (if (eq operation boole-ior)
                               (bit-ior (covered-file-executed image-states) bits t)
                               (replace (covered-file-executed image-states) bits)))
                          (t
                           (warn "Can't replace coverage for ~S - source has changed"
                                 filename)))))))

(defun merge-coverage (coverage-state)
  "Merge the code coverage data to include covered code from an earlier
state produced by SAVE-COVERAGE."
  (set-image-states coverage-state boole-ior))

(defun restore-coverage (coverage-state)
  "Restore the code coverage data back to an earlier state produced by
SAVE-COVERAGE."
  (set-image-states coverage-state boole-2)))

(defun save-coverage-in-file (pathname)
  "Call SAVE-COVERAGE and write the results of that operation into the
file designated by PATHNAME."
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (with-standard-io-syntax
      (let ((*package* (find-package :sb-cover)))
        (write (save-coverage) :stream stream)))
    (values)))

(defun merge-coverage-from-file (pathname)
  "READ the contents of the file designated by PATHNAME and pass the
result to MERGE-COVERAGE."
  (with-open-file (stream pathname :direction :input)
    (with-standard-io-syntax
      (let ((*package* (find-package :sb-cover)))
        (merge-coverage (read stream))))
    (values)))

(defun restore-coverage-from-file (pathname)
  "READ the contents of the file designated by PATHNAME and pass the
result to RESTORE-COVERAGE."
  (with-open-file (stream pathname :direction :input)
    (with-standard-io-syntax
      (let ((*package* (find-package :sb-cover)))
        (restore-coverage (read stream))))
    (values)))

(defun pathname-as-directory (pathname &optional (errorp t))
  (let ((pathname (merge-pathnames pathname)))
    (if (and (member (pathname-name pathname) '(nil :unspecific))
             (member (pathname-type pathname) '(nil :unspecific)))
        pathname
        (if errorp
            (error "~S does not designate a directory" pathname)
            (make-pathname :directory (append (or (pathname-directory pathname)
                                                  (list :relative))
                                              (list (file-namestring pathname)))
                           :name nil :type nil :version nil
                           :defaults pathname)))))

(defun report (directory &key ((:form-mode *source-path-mode*) :whole)
               (if-matches 'identity)
               (external-format :default))
  "Print a code coverage report of all instrumented files into DIRECTORY.
If DIRECTORY does not exist, it will be created. The main report will be
printed to the file cover-index.html. The external format of the source
files can be specified with the EXTERNAL-FORMAT parameter.

If the keyword argument FORM-MODE has the value :CAR, the annotations in
the coverage report will be placed on the CARs of any cons-forms, while if
it has the value :WHOLE the whole form will be annotated (the default).
The former mode shows explicitly which forms were instrumented, while the
latter mode is generally easier to read.

The keyword argument IF-MATCHES should be a designator for a function
of one argument, called for the namestring of each file with code
coverage info. If it returns true, the file's info is included in the
report, otherwise ignored. The default value is CL:IDENTITY.
"
  (let* ((paths)
         (directory (pathname-as-directory directory))
         (defaults (translate-logical-pathname directory)))
    (ensure-directories-exist defaults)
    (refresh-coverage-bits)
    (maphash (lambda (k v)
               (declare (ignore v))
               (when (funcall if-matches k)
                 (let* ((pk (translate-logical-pathname k))
                        (n (format nil "~(~{~2,'0X~}~)"
                                   (coerce (sb-md5:md5sum-string
                                            (sb-ext:native-namestring pk))
                                           'list))))
                   (when (probe-file k)
                     (with-open-file (stream (make-pathname :name n :type "html"
                                                            :defaults directory)
                                             :direction :output
                                             :if-exists :supersede
                                             :if-does-not-exist :create)
                       (push (list* k n (report-file k stream external-format))
                             paths))))))
             (code-coverage-hashtable))
    (let ((report-file (make-pathname :name "cover-index" :type "html" :defaults directory)))
      (with-open-file (stream report-file
                              :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
        (write-styles stream)
        (unless paths
          (warn "No coverage data found for any file, producing an empty report. Maybe you~%forgot to (DECLAIM (OPTIMIZE SB-COVER:STORE-COVERAGE-DATA))?")
          (format stream "<h3>No code coverage data found.</h3>")
          (close stream)
          (return-from report))
        (format stream "<table class='summary'>")
        (format stream "<tr class='head-row'><td></td><td class='main-head' colspan='3'>Expression</td><td class='main-head' colspan='3'>Branch</td></tr>")
        (format stream "<tr class='head-row'>~{<td width='80px'>~A</td>~}</tr>"
                (list "Source file"
                      "Covered" "Total" "%"
                      "Covered" "Total" "%"))
        (setf paths (sort paths #'string< :key #'car))
        (loop for prev = nil then source-file
              for (source-file report-file expression branch) in paths
              for even = nil then (not even)
              do (when (or (null prev)
                           (not (equal (pathname-directory (pathname source-file))
                                       (pathname-directory (pathname prev)))))
                   (format stream "<tr class='subheading'><td colspan='7'>~A</td></tr>~%"
                           (namestring (make-pathname
                                        :host (pathname-host (pathname source-file))
                                        :directory (pathname-directory (pathname source-file))))))
              do (format stream "<tr class='~:[odd~;even~]'><td class='text-cell'><a href='~a.html'>~a</a></td>~{<td>~:[-~;~:*~a~]</td><td>~:[-~;~:*~a~]</td><td>~:[-~;~:*~5,1f~]</td>~}</tr>"
                         even
                         report-file
                         (enough-namestring (pathname source-file)
                                            (pathname source-file))
                         (list (ok-of expression)
                               (all-of expression)
                               (percent expression)
                               (ok-of branch)
                               (all-of branch)
                               (percent branch))))
        (format stream "</table>"))
      report-file)))

(defun percent (count)
  (unless (zerop (all-of count))
    (* 100
       (/ (ok-of count) (all-of count)))))

(defun report-file (file html-stream external-format)
  "Print a code coverage report of FILE into the stream HTML-STREAM."
  (format html-stream "<html><head>")
  (write-styles html-stream)
  (format html-stream "</head><body>")
  (multiple-value-bind (counts states source)
      (compute-file-info file external-format)
    #+nil ; maybe cross-check both techniques for producing states
    (multiple-value-bind (counts2 states2) (compute-file-states file)
      (when states2
        (assert (equalp counts counts2))
        (assert (equalp states states2))))
    (print-report html-stream file counts states source)
    (format html-stream "</body></html>")
    (list (getf counts :expression) (getf counts :branch))))

(defvar *counts* nil)

(defun print-report (html-stream file counts states source)
  ;; Just used for testing
  (setf *counts* counts)
  (let ((*print-case* :downcase))
    (format html-stream
            "<h3>Coverage report: ~a <br />~%</h3>~%" file)
    (when (zerop (all-of (getf counts :expression)))
      (format html-stream "<b>File has no instrumented forms</b>")
      (return-from print-report))
    (format html-stream "<table class='summary'><tr class='head-row'>~{<td width='80px'>~a</td>~}"
            (list "Kind" "Covered" "All" "%"))
    (dolist (mode '(:expression :branch))
      (let ((count (getf counts mode)))
        (format html-stream "<tr class='~:[odd~;even~]'><td>~A</td><td>~a</td><td>~a</td><td>~5,1F</td></tr>~%"
                (eql mode :branch)
                mode
                (ok-of count)
                (all-of count)
                (percent count))))
    (format html-stream "</table>"))
  (format html-stream "<div class='key'><b>Key</b><br />~%")
  (format html-stream "<div class='state-0'>Not instrumented</div>")
  (format html-stream "<div class='state-15'>Conditionalized out</div>")
  (format html-stream "<div class='state-1'>Executed</div>")
  (format html-stream "<div class='state-2'>Not executed</div>")
  (format html-stream "<div>&#160;</div>")
  (format html-stream "<div class='state-5'>Both branches taken</div>")
  (format html-stream "<div class='state-6'>One branch taken</div>")
  (format html-stream "<div class='state-10'>Neither branch taken</div>")
  (format html-stream "</div>")
  (format html-stream "<nobr><div><code>~%")
  (flet ((line (line)
           (format html-stream "</code></div></nobr>~%<nobr><div class='source'><div class='line-number'><code>~A</code></div><code>&#160;" line)
           line))
    (loop for last-state = nil then state
          with line = (line 1)
          for col from 1
          for char across source
          for state across states
          do (unless (eq state last-state)
               (when last-state
                 (format html-stream "</span>"))
               (format html-stream "<span class='state-~a'>" state))
          do (case char
               ((#\Newline)
                (setf state nil)
                (setf col 0)
                (format html-stream "</span>")
                (line (incf line)))
               ((#\Space)
                (format html-stream "&#160;"))
               ((#\Tab)
                (error "tab"))
               (t
                (if (alphanumericp char)
                    (write-char char html-stream)
                    (format html-stream "&#~A;" (char-code char))))))
    (format html-stream "</code></div>")))

(defun write-styles (html-stream)
  (format html-stream "<style type='text/css'>
*.state-0 { background-color: #eeeeee }
*.state-1 { background-color: #aaffaa }
*.state-5 { background-color: #44dd44 }
*.state-2 { background-color: #ffaaaa }
*.state-10 { background-color: #ee6666 }
*.state-15 { color: #aaaaaa; background-color: #eeeeee }
*.state-9,*.state-6 { background-color: #ffffaa }
div.key { margin: 20px; width: 200px }
div.source { width: 88ex; background-color: #eeeeee; padding-left: 5px;
             /* border-style: solid none none none; border-width: 1px;
             border-color: #dddddd */ }

*.line-number { color: #666666; float: left; width: 6ex; text-align: right; margin-right: 1ex; }

table.summary tr.head-row { background-color: #aaaaff }
table.summary tr td.text-cell { text-align: left }
table.summary tr td.main-head { text-align: center }
table.summary tr td { text-align: right }
table.summary tr.even { background-color: #eeeeff }
table.summary tr.subheading { background-color: #aaaaff}
table.summary tr.subheading td { text-align: left; font-weight: bold; padding-left: 5ex; }
</style>"))


#|
As a quick way to view source maps, do:
(progn (write (read-and-record-source-maps (read-source "myfile.lisp" :default))
              :pprint-dispatch *ppd*)
       (values))
|#
(sb-ext:define-load-time-global *ppd*
    (let ((ppd (copy-pprint-dispatch)))
      (set-pprint-dispatch
       'sb-sys:system-area-pointer
       (lambda (stream object)
         (let ((int (sb-sys:sap-int object))
               (sigil-bit (ash 1 (1- sb-vm:n-word-bits))))
           (if (logtest int sigil-bit)
               (format stream "s~d" (logxor int sigil-bit))
               (write object :stream stream :pretty nil))))
       1 ppd)
      ppd))

;;; Debugging helpers
#+nil (progn
(defun show-states (source states &aux (srclen (length source)))
  (let ((linecount (+ (count #\newline source)
                      (if (char= (char source (1- srclen)) #\newline) 0 1)))
        (lines))
    (let ((line-start-pos 0))
      (dotimes (i linecount)
        (let* ((newline-pos (or (position #\newline source :start line-start-pos) srclen))
               (count (- newline-pos line-start-pos)))
          (push (cons line-start-pos count) lines)
          (setq line-start-pos (1+ newline-pos)))))
    (loop for line-num from 1 to linecount
          for (start . char-count) in (nreverse lines)
          do (format t "~4d |" line-num)
             (loop for i from start repeat char-count do (format t " ~c" (char source i)))
             (format t "~%      ")
             (loop for i from start repeat char-count do (format t " ~X" (aref states i)))
             (format t "~%"))))

(defun show-file-source-maps (pathname &optional tlf-num)
  (let* ((source (read-source pathname :default))
         (maps (read-and-record-source-maps source))
         (*print-pprint-dispatch* *ppd*)
         (i -1))
    (dolist (cell (if tlf-num (list (nth tlf-num maps)) maps))
      (let ((form (car cell)))
        (format t "TLF ~d:~%" (incf i))
        (write form)
        (terpri)
        (maphash
         (lambda (subform locs)
           (let ((repr (substitute #\space #\newline
                                   (write-to-string subform :pretty t)))
                 (source
                  (when (singleton-p locs)
                    (substitute #\space #\newline
                                (subseq source (1- (caar locs)) (cadar locs))))))
             (format t "  @ ~A = ~A~@[ = {~A}~]~%" locs repr source)))
         (cdr cell))))))

(defun show-fun-covg-paths (simple-fun)
  (let ((branches (make-hash-table :test 'equal)))
    (binding* (((paths code) (%find-coverage-map simple-fun))
               (marks (code-coverage-marks code)))
      (dotimes (i (length paths))
        (let ((list (aref paths i)))
          (format t "~3d~A = ~a~%"
                  i
                  (if (/= (sb-sys:sap-ref-8 marks i) 0) "*" " ")
                  (mapcar #'reverse list))
          (dolist (path list)
            (when (member (car path) '(:then :else))
              (push (car path) (gethash (cdr path) branches)))))))
    (maphash (lambda (path ways) (format t "~a -> ~a~%" path ways))
             branches))))
