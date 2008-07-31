;;; A frontend for the SBCL code coverage facility. Written by Juho
;;; Snellman, and placed under public domain.

;;; This module includes a modified version of the source path parsing
;;; routines from Swank. That code was written by Helmut Eller, and
;;; was placed under Public Domain

(defpackage #:sb-cover
  (:use #:cl #:sb-c)
  (:export #:report
           #:reset-coverage #:clear-coverage
           #:restore-coverage #:restore-coverage-from-file
           #:save-coverage #:save-coverage-in-file
           #:store-coverage-data))

(in-package #:sb-cover)

(declaim (type (member :whole :car) *source-path-mode*))
(defvar *source-path-mode* :whole)

(defclass sample-count ()
  ((mode :accessor mode-of :initarg :mode)
   (all :accessor all-of :initform 0)
   (ok :accessor ok-of :initform 0)))

(defun clear-coverage ()
  "Clear all files from the coverage database. The files will be re-entered
into the database when the FASL files (produced by compiling
STORE-COVERAGE-DATA optimization policy set to 3) are loaded again into the
image."
  (sb-c::clear-code-coverage))

(defun reset-coverage ()
  "Reset all coverage data back to the `Not executed` state."
  (sb-c::reset-code-coverage))

(defun save-coverage ()
  "Returns an opaque representation of the current code coverage state.
The only operation that may be done on the state is passing it to
RESTORE-COVERAGE. The representation is guaranteed to be readably printable.
A representation that has been printed and read back will work identically
in RESTORE-COVERAGE."
  (loop for file being the hash-keys of sb-c::*code-coverage-info*
        using (hash-value states)
        collect (cons file states)))

(defun restore-coverage (coverage-state)
  "Restore the code coverage data back to an earlier state produced by
SAVE-COVERAGE."
  (loop for (file . states) in coverage-state
        do (let ((image-states (gethash file sb-c::*code-coverage-info*))
                 (table (make-hash-table :test 'equal)))
             (when image-states
               (loop for cons in image-states
                     do (setf (gethash (car cons) table) cons))
               (loop for (key . value) in states
                     do (let ((state (gethash key table)))
                          (when state
                            (setf (cdr state) value))))))))

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
                           :name nil :type nil
                           :defaults pathname)))))

(defun report (directory &key ((:form-mode *source-path-mode*) :whole)
               (external-format :default))
  "Print a code coverage report of all instrumented files into DIRECTORY.
If DIRECTORY does not exist, it will be created. The main report will be
printed to the file cover-index.html. The external format of the source
files can be specified with the EXTERNAL-FORMAT parameter.

If the keyword argument FORM-MODE has the value :CAR, the annotations in
the coverage report will be placed on the CARs of any cons-forms, while if
it has the value :WHOLE the whole form will be annotated (the default).
The former mode shows explicitly which forms were instrumented, while the
latter mode is generally easier to read."
  (let ((paths)
        (*default-pathname-defaults* (pathname-as-directory directory)))
    (ensure-directories-exist *default-pathname-defaults*)
    (maphash (lambda (k v)
               (declare (ignore v))
               (let* ((n (format nil "~(~{~2,'0X~}~)"
                                (coerce (sb-md5:md5sum-string
                                         (sb-ext:native-namestring k))
                                        'list)))
                      (path (make-pathname :name n :type "html")))
                 (when (probe-file k)
                   (with-open-file (stream path
                                           :direction :output
                                           :if-exists :supersede
                                           :if-does-not-exist :create)
                     (push (list* k n (report-file k stream external-format))
                           paths)))))
             *code-coverage-info*)
    (let ((report-file (make-pathname :name "cover-index" :type "html")))
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
                           (namestring (make-pathname :directory (pathname-directory (pathname source-file))))))
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
  (let* ((source (detabify (read-file file external-format)))
         (states (make-array (length source)
                             :initial-element 0
                             :element-type '(unsigned-byte 4)))
         ;; Convert the code coverage records to a more suitable format
         ;; for this function.
         (expr-records (convert-records (gethash file *code-coverage-info*)
                                        :expression))
         (branch-records (convert-records (gethash file *code-coverage-info*)
                                          :branch))
         ;; Cache the source-maps
         (maps (with-input-from-string (stream source)
                 (loop with map = nil
                       with form = nil
                       with eof = nil
                       for i from 0
                       do (setf (values form map)
                                (handler-case
                                    (read-and-record-source-map stream)
                                  (end-of-file ()
                                    (setf eof t))
                                  (error (error)
                                    (warn "Error when recording source map for toplevel form ~A:~%  ~A" i error)
                                    (values nil
                                            (make-hash-table)))))
                       until eof
                       when map
                       collect (cons form map)))))
    (mapcar (lambda (map)
              (maphash (lambda (k locations)
                         (declare (ignore k))
                         (dolist (location locations)
                           (destructuring-bind (start end suppress) location
                             (when suppress
                               (fill-with-state source states 15 (1- start)
                                                end)))))
                       (cdr map)))
            maps)
    ;; Go through all records, find the matching source in the file,
    ;; and update STATES to contain the state of the record in the
    ;; indexes matching the source location. We do this in two stages:
    ;; the first stage records the character ranges, and the second stage
    ;; does the update, in order from shortest to longest ranges. This
    ;; ensures that for each index in STATES will reflect the state of
    ;; the innermost containing form.
    (let ((counts (list :branch (make-instance 'sample-count :mode :branch)
                        :expression (make-instance 'sample-count
                                                   :mode :expression))))
      (let ((records (append branch-records expr-records))
            (locations nil))
        (dolist (record records)
          (destructuring-bind (mode path state) record
            (let* ((path (reverse path))
                   (tlf (car path))
                   (source-form (car (nth tlf maps)))
                   (source-map (cdr (nth tlf maps)))
                   (source-path (cdr path)))
              (cond ((eql mode :branch)
                     (let ((count (getf counts :branch)))
                       ;; For branches mode each record accounts for two paths
                       (incf (ok-of count)
                             (ecase state
                               (5 2)
                               ((6 9) 1)
                               (10 0)))
                       (incf (all-of count) 2)))
                    (t
                     (let ((count (getf counts :expression)))
                       (when (eql state 1)
                         (incf (ok-of count)))
                       (incf (all-of count)))))
              (if source-map
                  (handler-case
                      (multiple-value-bind (start end)
                          (source-path-source-position (cons 0 source-path)
                                                       source-form
                                                       source-map)
                        (push (list start end source state) locations))
                    (error ()
                      (warn "Error finding source location for source path ~A in file ~A~%" source-path file)))
                  (warn "Unable to find a source map for toplevel form ~A in file ~A~%" tlf file)))))
        ;; Now process the locations, from the shortest range to the longest
        ;; one. If two locations have the same range, the one with the higher
        ;; state takes precedence. The latter condition ensures that if
        ;; there are both normal- and a branch-states for the same form,
        ;; the branch-state will be used.
        (setf locations (sort locations #'> :key #'fourth))
        (dolist (location (stable-sort locations #'<
                                       :key (lambda (location)
                                              (- (second location)
                                                 (first location)))))
          (destructuring-bind (start end source state) location
            (fill-with-state source states state start end))))
      (print-report html-stream file counts states source)
      (format html-stream "</body></html>")
      (list (getf counts :expression)
            (getf counts :branch)))))

(defun fill-with-state (source states state start end)
  (let* ((pos (position #\Newline source
                        :end start
                        :from-end t))
         (start-column (if pos
                           (- start 1 pos)
                           0))
         (end-column 0))
    (loop for i from start below end
          for col from start-column
          for char = (aref source i)
          do (cond ((eql char #\Newline)
                    (setf col -1))
                   ((not (eql char #\Space))
                    (setf end-column (max end-column col)))))
    (loop for i from start below end
          for col from start-column
          for char = (aref source i)
          do (if (eql char #\Newline)
                 (setf col -1)
                 (when (and (zerop (aref states i))
                            #+nil (<= col end-column)
                            (>= col start-column))
                   (setf (aref states i) state))))))

;;; Convert tabs to spaces
(defun detabify (source)
  (with-output-to-string (stream)
    (loop for char across source
          for col from 0
          for i from 0
          do (if (eql char #\Tab)
                 (loop repeat (- 8 (mod col 8))
                       do (write-char #\Space stream)
                       do (incf col)
                       finally (decf col))
                 (progn
                   (when (eql char #\Newline)
                     ;; Filter out empty last line
                     (when (eql i (1- (length source)))
                       (return))
                     (setf col -1))
                   (write-char char stream))))))

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
  (format html-stream "<div class='state-10''>Neither branch taken</div>")
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

(defun convert-records (records mode)
  (ecase mode
    (:expression
     (loop for record in records
           unless (member (caar record) '(:then :else))
           collect (list mode
                         (car record)
                         (if (sb-c::code-coverage-record-marked record)
                             1
                             2))))
    (:branch
     (let ((hash (make-hash-table :test 'equal)))
       (dolist (record records)
         (let ((path (car record)))
           (when (member (car path) '(:then :else))
             (setf (gethash (cdr path) hash)
                   (logior (gethash (cdr path) hash 0)
                           (ash (if (sb-c::code-coverage-record-marked record)
                                    1
                                    2)
                                (if (eql (car path) :then)
                                    0
                                    2)))))))
       (let ((list nil))
         (maphash (lambda (k v)
                    (push (list mode k v) list))
                  hash)
         list)))))

;;;; A mutant version of swank-source-path-parser from Swank/Slime.

(defun read-file (filename external-format)
  "Return the entire contents of FILENAME as a string."
  (with-open-file (s filename :direction :input
                     :external-format external-format)
    (let ((string (make-string (file-length s))))
      (read-sequence string s)
      string)))

(defun make-source-recorder (fn source-map)
  "Return a macro character function that does the same as FN, but
additionally stores the result together with the stream positions
before and after of calling FN in the hashtable SOURCE-MAP."
  (declare (type function fn))
  (lambda (stream char)
    (declare (optimize debug safety))
    (let ((start (file-position stream))
          (values (multiple-value-list (funcall fn stream char)))
          (end (file-position stream)))
      (unless (null values)
        (push (list start end *read-suppress*)
              (gethash (car values) source-map)))
      (values-list values))))

(defun make-source-recording-readtable (readtable source-map)
  "Return a source position recording copy of READTABLE.
The source locations are stored in SOURCE-MAP."
  (let* ((tab (copy-readtable readtable))
         (*readtable* tab))
    (dotimes (code 128)
      (let ((char (code-char code)))
        (multiple-value-bind (fn term) (get-macro-character char tab)
          (when fn
            (set-macro-character char (make-source-recorder fn source-map)
                                 term tab)))))
    (suppress-sharp-dot tab)
    (set-macro-character #\(
                         (make-source-recorder
                          (make-recording-read-list source-map)
                          source-map))
    tab))

;;; Ripped from SB-IMPL, since location recording on a cons-cell level
;;; can't be done just by simple read-table tricks.
(defun make-recording-read-list (source-map)
  (lambda (stream ignore)
    (block return
      (when (eql *package* (find-package :keyword))
        (return-from return
          (sb-impl::read-list stream ignore)))
      (let* ((thelist (list nil))
             (listtail thelist))
        (do ((firstchar (sb-impl::flush-whitespace stream)
                        (sb-impl::flush-whitespace stream)))
            ((char= firstchar #\) ) (cdr thelist))
          (when (char= firstchar #\.)
            (let ((nextchar (read-char stream t)))
              (cond ((sb-impl::token-delimiterp nextchar)
                     (cond ((eq listtail thelist)
                            (unless *read-suppress*
                              (sb-int:simple-reader-error
                               stream
                               "Nothing appears before . in list.")))
                           ((sb-impl::whitespace[2]p nextchar)
                            (setq nextchar (sb-impl::flush-whitespace stream))))
                     (rplacd listtail
                             ;; Return list containing last thing.
                             (car (sb-impl::read-after-dot stream nextchar)))
                     (return (cdr thelist)))
                    ;; Put back NEXTCHAR so that we can read it normally.
                    (t (unread-char nextchar stream)))))
          ;; Next thing is not an isolated dot.
          (let ((start (file-position stream))
                (listobj (sb-impl::read-maybe-nothing stream firstchar))
                (end (file-position stream)))
            ;; allows the possibility that a comment was read
            (when listobj
             (unless (consp (car listobj))
               (setf (car listobj) (gensym))
                (push (list start end *read-suppress*)
                      (gethash (car listobj) source-map)))
              (rplacd listtail listobj)
              (setq listtail listobj))))))))

(defun suppress-sharp-dot (readtable)
  (when (get-macro-character #\# readtable)
    (let ((sharp-dot (get-dispatch-macro-character #\# #\. readtable)))
      (set-dispatch-macro-character #\# #\.
                                    (lambda (&rest args)
                                      (let ((*read-suppress* t))
                                        (apply sharp-dot args)))
                                    readtable))))

(defun read-and-record-source-map (stream)
  "Read the next object from STREAM.
Return the object together with a hashtable that maps
subexpressions of the object to stream positions."
  (let* ((source-map (make-hash-table :test #'eq))
         (*readtable* (make-source-recording-readtable *readtable* source-map))
         (start (file-position stream))
         (form (read stream))
         (end (file-position stream)))
    ;; ensure that at least FORM is in the source-map
    (unless (gethash form source-map)
      (push (list start end nil)
            (gethash form source-map)))
    (values form source-map)))

(defun read-source-form (n stream)
  "Read the Nth toplevel form number with source location recording.
Return the form and the source-map."
  (let ((*read-suppress* t))
    (dotimes (i n)
      (read stream)))
  (let ((*read-suppress* nil)
        (*read-eval* nil))
    (read-and-record-source-map stream)))

(defun source-path-stream-position (path stream)
  "Search the source-path PATH in STREAM and return its position."
  (check-source-path path)
  (destructuring-bind (tlf-number . path) path
    (multiple-value-bind (form source-map) (read-source-form tlf-number stream)
      (source-path-source-position (cons 0 path) form source-map))))

(defun check-source-path (path)
  (unless (and (consp path)
               (every #'integerp path))
    (error "The source-path ~S is not valid." path)))

(defun source-path-string-position (path string)
  (with-input-from-string (s string)
    (source-path-stream-position path s)))

(defun source-path-file-position (path filename)
  (with-open-file (file filename)
    (source-path-stream-position path file)))

(defun source-path-source-position (path form source-map)
  "Return the start position of PATH from FORM and SOURCE-MAP.  All
subforms along the path are considered and the start and end position
of the deepest (i.e. smallest) possible form is returned."
  ;; compute all subforms along path
  (let ((forms (loop for n in path
                     for f = form then (nth n f)
                     collect f into forms
                     finally (return forms))))
    ;; select the first subform present in source-map
    (loop for real-form in (reverse forms)
          for form = (if (or (eql *source-path-mode* :whole)
                             (not (consp real-form)))
                         real-form
                         (car real-form))
          for positions = (gethash form source-map)
          until (and positions (null (cdr positions)))
          finally (destructuring-bind ((start end suppress)) positions
                    (declare (ignore suppress))
                    (return (values (1- start) end))))))
