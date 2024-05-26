;;; A frontend for the SBCL code coverage facility. Written by Juho
;;; Snellman, and placed under public domain.

;;; This module includes a modified version of the source path parsing
;;; routines from Swank. That code was written by Helmut Eller, and
;;; was placed under Public Domain

(defpackage #:sb-cover
  (:use #:cl #:sb-c)
  (:export #:report
           #:get-coverage
           #:reset-coverage #:clear-coverage
           #:restore-coverage #:restore-coverage-from-file
           #:save-coverage #:save-coverage-in-file
           #:store-coverage-data))

(in-package #:sb-cover)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (sb-int:system-package-p *package*) t))

(defmacro code-coverage-hashtable () `(car sb-int:*code-coverage-info*))

(defun reset-code-coverage ()
  (maphash (lambda (info cc)
             (declare (ignore info))
             (dolist (cc-entry cc)
               (setf (cdr cc-entry) nil)))
           (car sb-int:*code-coverage-info*)))

;;;; New coverage representation.
;;;; One byte per coverage mark is stored in the unboxed constants of the code.
;;;; x86[-64] use a slightly different but not significantly different
;;; representation of the marks from other architectures.
(defun %find-coverage-map (code)
  (declare (type (or sb-kernel:simple-fun
                     sb-kernel:code-component
                     symbol)
                 code))
  (etypecase code
   (sb-kernel:simple-fun
    (%find-coverage-map (sb-kernel:fun-code-header code)))
   (symbol
    (%find-coverage-map (or (macro-function code) (fdefinition code))))
   (sb-kernel:code-component
    (let ((n (sb-kernel:code-header-words code)))
      (let ((map (sb-kernel:code-header-ref code (1- n))))
        (when (typep map '(cons (eql sb-c::coverage-map)))
          (return-from %find-coverage-map (values (cdr map) code))))
      ;; if code-boxed-words can't be an odd number, try one more slot
      #-(or x86 x86-64)
      (let ((map (sb-kernel:code-header-ref code (- n 2))))
        (when (typep map '(cons (eql sb-c::coverage-map)))
          (return-from %find-coverage-map (values (cdr map) code))))))))

#+arm64
(declaim (ftype (sb-int:sfunction (t) (simple-array (unsigned-byte 8) (*))) code-coverage-marks))
;;; Coverage marks are in the raw bytes following the jump tables
;;; preceding any other unboxed constants. This way we don't have to store
;;; a pointer to the coverage marks since their location is implicit.
(defun code-coverage-marks (code)
  #-arm64
  (let ((insts (sb-kernel:code-instructions code)))
    (sb-sys:sap+ insts (ash (sb-kernel:code-jump-table-words code)
                            sb-vm:word-shift)))
  #+arm64
  (let* ((words (sb-kernel:code-header-words code))
         (last (sb-kernel:code-header-ref code (- words 2))))
    (if (vectorp last)
        last
        (sb-kernel:code-header-ref code (- words 3)))))

;;;;

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
  (clrhash (car sb-int:*code-coverage-info*))
  (setf (cdr sb-int:*code-coverage-info*) nil))

(macrolet
    ((do-instrumented-code ((var &optional result) &body body)
       ;; Scan list of weak-pointers to all coverage-instrumented code,
       ;; binding VAR to each object, and removing broken weak-pointers.
       `(let ((predecessor sb-int:*code-coverage-info*))
          (loop
           (let ((cell (cdr predecessor)))
             (unless cell (return ,result))
             (let ((,var (sb-ext:weak-pointer-value (car cell))))
               (if ,var
                   (progn ,@body (setq predecessor cell))
                   (rplacd predecessor (cdr cell))))))))
     ;; Using different values here isn't great, but a 1 bit seemed
     ;; the natural choice for "marked" which is fine for x86 which can
     ;; store any immediate byte. But the architectures which can't
     ;; have either a ZERO-TN or NULL-TN, and can store a byte from
     ;; that register into the coverage mark. So they expect a 0
     ;; in the low bit and therefore a 1 in the unmarked state.
     (empty-mark-word ()
       #+(or x86-64 x86) 0
       #-(or x86-64 x86) sb-ext:most-positive-word)
     (byte-marked-p (byte)
       #+(or x86-64 x86) `(/= ,byte 0)
       #-(or x86-64 x86) `(/= ,byte #xFF)))

;;; Retun just the list of soure paths in CODE that are marked covered.
(defun get-coverage (code)
  (multiple-value-bind (map code) (%find-coverage-map code)
    (when map
      (sb-int:collect ((paths))
        #-arm64
        (sb-sys:with-pinned-objects (code)
          (let ((sap (code-coverage-marks code)))
            (dotimes (i (length map) (paths))
              (when (byte-marked-p (sb-sys:sap-ref-8 sap i))
                (paths (svref map i))))))
        #+arm64
        (let ((marks (code-coverage-marks code)))
          (dotimes (i (length map) (paths))
            (when (byte-marked-p (aref marks i))
              (paths (svref map i)))))))))

(defun reset-coverage (&optional object)
  "Reset all coverage data back to the `Not executed` state."
  (cond (object ; reset only this object
         (multiple-value-bind (map code)
             (%find-coverage-map (the sb-kernel:code-component object))
           (when map
             #-arm64
             (sb-sys:with-pinned-objects (code)
               (sb-alien:alien-funcall
                (sb-alien:extern-alien "memset"
                                       (function sb-alien:void sb-sys:system-area-pointer
                                                 sb-alien:int sb-alien:unsigned))
                (code-coverage-marks code)
                (logand (empty-mark-word) #xFF)
                (length map)))
             #+arm64
             (fill (code-coverage-marks code) #xFF))))
        (t                              ; reset everything
         (do-instrumented-code (code)
           (reset-coverage code))
         (reset-code-coverage))))

;;; Transfer data from new-style coverage marks into old-style.
;;; Update only data for FILENAME if supplied, or all files if NIL.
;;; Ideally we could report directly from the new data where applicable,
;;; however this is, for the time being, perfectly backward-compatibile.
(defun refresh-coverage-info (&optional filename)
  (declare (ignorable filename))
  ;; NAMESTRING->PATH-TABLES maps a namestring to a hashtable which maps
  ;; source paths to the legacy coverage record for that path in that file,
  ;;   e.g. (1 4 1) -> ((1 4 1) . NIL)
  (let ((namestring->path-tables (make-hash-table :test 'equal))
        (coverage-records (code-coverage-hashtable))
        (n-marks 0))
    (do-instrumented-code (code)
      (sb-int:binding* ((map (%find-coverage-map code) :exit-if-null)
                        (namestring
                         (sb-c::debug-source-namestring
                          (sb-c::debug-info-source (sb-kernel:%code-debug-info code)))
                         :exit-if-null)
                        (legacy-coverage-marks
                         (and (or (null filename) (string= namestring filename))
                              (gethash namestring coverage-records))
                         :exit-if-null)
                        (path-lookup-table
                         (gethash namestring namestring->path-tables)))
        ;; Build the source path -> marked map for this file if not seen yet.
        ;; It is of course redundant to have both representations.
        (unless path-lookup-table
          (setf path-lookup-table (make-hash-table :test 'equal)
                (gethash namestring namestring->path-tables) path-lookup-table)
          (dolist (item legacy-coverage-marks)
            (setf (gethash (car item) path-lookup-table) item)))
        #-arm64
        (sb-sys:with-pinned-objects (code)
          (let ((sap (code-coverage-marks code)))
            (dotimes (i (length map))   ; for each recorded mark
              (when (byte-marked-p (sb-sys:sap-ref-8 sap i))
                (incf n-marks)
                ;; Set the legacy coverage mark for each path it touches
                (dolist (path (svref map i))
                  (let ((found (gethash path path-lookup-table)))
                    (if found
                        (rplacd found t)
                        #+nil
                        (warn "Missing coverage entry for ~S in ~S"
                              path namestring))))))))
        #+arm64
        (let ((marks (code-coverage-marks code)))
          (dotimes (i (length map))     ; for each recorded mark
            (when (byte-marked-p (aref marks i))
              (incf n-marks)
              (dolist (path (svref map i))
                (let ((found (gethash path path-lookup-table)))
                  (if found
                      (rplacd found t)))))))))
    (values coverage-records n-marks)))

) ; end MACROLET

(defun save-coverage ()
  "Returns an opaque representation of the current code coverage state.
The only operation that may be done on the state is passing it to
RESTORE-COVERAGE. The representation is guaranteed to be readably printable.
A representation that has been printed and read back will work identically
in RESTORE-COVERAGE."
  (refresh-coverage-info)
  (loop for file being the hash-keys of (code-coverage-hashtable)
        using (hash-value states)
        collect (cons file states)))

(defun restore-coverage (coverage-state)
  "Restore the code coverage data back to an earlier state produced by
SAVE-COVERAGE."
  ;; This does not update coverage stored in code object headers
  (loop for (file . states) in coverage-state
        do (let ((image-states (gethash file (code-coverage-hashtable)))
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
    (when (eq if-matches 'identity)
      (refresh-coverage-info)) ; update all
    (maphash (lambda (k v)
               (declare (ignore v))
               (when (funcall if-matches k)
                 (unless (eq if-matches 'identity)
                   (refresh-coverage-info k)) ; update one file
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

;; This special is bound to the package in which forms will be read,
;; and reassigned as read-and-record-source-map detects what look like
;; "IN-PACKAGE" forms.
(defvar *current-package*)

(defun report-file (file html-stream external-format)
  "Print a code coverage report of FILE into the stream HTML-STREAM."
  (format html-stream "<html><head>")
  (write-styles html-stream)
  (format html-stream "</head><body>")
  (let* ((source (detabify (read-file file external-format)))
         (states (make-array (length source)
                             :initial-element 0
                             :element-type '(unsigned-byte 4)))
         (hashtable (code-coverage-hashtable))
         ;; Convert the code coverage records to a more suitable format
         ;; for this function.
         (expr-records (convert-records (gethash file hashtable) :expression))
         (branch-records (convert-records (gethash file hashtable) :branch))
         ;; Cache the source-maps
         (maps (with-input-from-string (stream source)
                 (loop with *current-package* = (find-package "CL-USER")
                       with map = nil
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
                         (if (cdr record)
                             1
                             2))))
    (:branch
     (let ((hash (make-hash-table :test 'equal)))
       (dolist (record records)
         (let ((path (car record)))
           (when (member (car path) '(:then :else))
             (setf (gethash (cdr path) hash)
                   (logior (gethash (cdr path) hash 0)
                           (ash (if (cdr record)
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
  (declare (type (or function symbol) fn))
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
    ;; It is unspecified whether doing (SET-MACRO-CHARACTER c1 fn)
    ;; and then (SET-DISPATCH-MACRO-CHARACTER c1 c2 fn) should be allowed.
    ;; Portability concerns aside, it doesn't work in the latest code,
    ;; but first changing the function for #. and then # in that order works.
    (suppress-sharp-dot tab)
    (dotimes (code 128)
      (let ((char (code-char code)))
        (multiple-value-bind (fn term) (get-macro-character char tab)
          (when fn
            (set-macro-character char (make-source-recorder fn source-map)
                                 term tab)))))
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
             (rt *readtable*)
             (listtail thelist))
        (do ((firstchar (sb-impl::flush-whitespace stream rt)
                        (sb-impl::flush-whitespace stream rt)))
            ((char= firstchar #\) ) (cdr thelist))
          (when (char= firstchar #\.)
            (let ((nextchar (read-char stream t)))
              (cond ((sb-impl::token-delimiterp nextchar)
                     (cond ((eq listtail thelist)
                            (unless *read-suppress*
                              (sb-int:simple-reader-error
                               stream
                               "Nothing appears before . in list.")))
                           ((sb-impl::whitespace[2]p nextchar rt)
                            (setq nextchar (sb-impl::flush-whitespace stream rt))))
                     (rplacd listtail
                             (sb-impl::read-after-dot
                              stream nextchar (if *read-suppress* 0 -1)))
                     (return (cdr thelist)))
                    ;; Put back NEXTCHAR so that we can read it normally.
                    (t (unread-char nextchar stream)))))
          ;; Next thing is not an isolated dot.
          (sb-int:binding*
              ((start (file-position stream))
               ((winp obj) (sb-impl::read-maybe-nothing stream firstchar))
               (listobj (if (not (zerop winp)) (list obj)))
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

;;; The detection logic for "IN-PACKAGE" is stolen from swank's
;;; source-path-parser.lisp.
;;;
;;; We look for lines that start with "(in-package " or
;;; "(cl:in-package ", without leading whitespace.
(defun starts-with-p (string prefix)
  (declare (type string string prefix))
  (not (mismatch string prefix
                 :end1 (min (length string) (length prefix))
                 :test #'char-equal)))

(defun extract-package (line)
  (declare (type string line))
  (let ((*package* *current-package*))
    (second (read-from-string line))))

(defun look-for-in-package-form (string)
  (when (or (starts-with-p string "(in-package ")
            (starts-with-p string "(cl:in-package "))
    (let ((package (find-package (extract-package string))))
      (when package
        (setf *current-package* package)))))

(defun look-for-in-package-form-in-stream (stream start-position end-position)
  "Scans the stream between start-position up to end-position for
   something that looks like an in-package form. If it does find
   something, the function updates *current-package*. In all cases,
   the stream is reset to end-position on exit."
  (assert (file-position stream start-position))  ; rewind the stream
  (loop until (>= (file-position stream) end-position)
     do (look-for-in-package-form (or (read-line stream nil)
                                      (return))))
  (assert (file-position stream end-position)))

(defun read-and-record-source-map (stream)
  "Read the next object from STREAM.
Return the object together with a hashtable that maps
subexpressions of the object to stream positions."
  (let* ((source-map (make-hash-table :test #'eq))
         (start (file-position stream))
         (form (let ((*readtable* (make-source-recording-readtable *readtable* source-map))
                     (*package* *current-package*))
                 (read stream)))
         (end (file-position stream)))
    (look-for-in-package-form-in-stream stream start end)
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
