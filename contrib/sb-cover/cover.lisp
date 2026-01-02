;;; A frontend for the SBCL code coverage facility. Written by Juho
;;; Snellman, and placed under public domain.

;;; This module includes a modified version of the source path parsing
;;; routines from Swank. That code was written by Helmut Eller, and
;;; was placed under Public Domain

(defpackage #:sb-cover
  (:use #:cl #:sb-c #:sb-int)
  (:export #:report #:lcov-report
           #:enable-coverage-logging
           #:get-coverage
           #:reset-coverage #:clear-coverage
           #:merge-coverage #:merge-coverage-from-file
           #:restore-coverage #:restore-coverage-from-file
           #:save-coverage #:save-coverage-in-file
           #:store-coverage-data))

(in-package #:sb-cover)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (system-package-p *package*) t))

(defmacro code-coverage-hashtable () `(car *code-coverage-info*))

;;;; New coverage representation.
;;;; One byte per coverage mark is stored in the unboxed constants of the code.
;;;; x86[-64] use a slightly different but not significantly different
;;; representation of the marks from other architectures.
(defun %find-coverage-map (code)
  (etypecase code
   (sb-kernel:simple-fun
    (%find-coverage-map (sb-kernel:fun-code-header code)))
   (symbol
    (%find-coverage-map (or (macro-function code) (fdefinition code))))
   (sb-kernel:code-component
    (values (sb-c::code-coverage-map code) code))))

#+arm64
(declaim (ftype (sfunction (t) (simple-array (unsigned-byte 8) (*))) code-coverage-marks))
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

(defstruct (sample-count (:constructor make-sample-count (mode))
                         (:conc-name ""))
  (mode nil :read-only t) ; never read. Should we just delete the slot?
  (all-of 0)
  (ok-of 0))

(macrolet
    ((do-instrumented-code ((var) &body body)
       ;; Scan coverage-instrumented codeblobs, binding VAR to each
       `(dolist (#1=#:v (cdr *code-coverage-info*))
          (dotimes (#2=#:i (weak-vector-len #1#))
            (let ((,var (weak-vector-ref #1# #2#)))
              (when ,var ,@body)))))
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
(defun get-coverage (code) ; UNUSED. Can we delete this?
  (multiple-value-bind (map code) (%find-coverage-map code)
    (when map
      (collect ((paths))
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
         (maphash (lambda (filename file-coverage)
                    (declare (ignore filename))
                    (fill (covered-file-executed file-coverage) 0))
                  (code-coverage-hashtable))
         (do-instrumented-code (code)
           (reset-coverage code)))))

;;; Transfer marks from code-component into a per-file bitmap, making subsequent steps simpler.
;;; An unnecessarily inefficient detail is that the mapping from index in a code header's byte
;;; array to the bit array is predetermined at compile-time, yet we wait until now to do it.
;;; The fact is that paths should not be stored in the code headers- just bit indices.
(defun refresh-coverage-bits ()
  ;; NAMESTRING->PATH-TABLES maps a namestring to a hashtable which maps
  ;; source paths to the per-file parallel arrays
  ;;   e.g. (1 4 1) -> index into COVERED-FILE-PATHS, COVERED-FILE-EXECUTED
  (let ((namestring->path-tables (make-hash-table :test 'equal)))
    (do-instrumented-code (code)
      (binding* ((map (the (not null) (%find-coverage-map code)))
                 (namestring
                  (sb-c::debug-source-namestring
                          (sb-c::debug-info-source (sb-kernel:%code-debug-info code)))
                         :exit-if-null)
                 (covered-file (gethash namestring (code-coverage-hashtable)) :exit-if-null)
                 (path-lookup-table (gethash namestring namestring->path-tables)))
        ;; Build the source path -> index table for this file if not seen yet.
        (unless path-lookup-table
          (setf path-lookup-table (make-hash-table :test 'equal)
                (gethash namestring namestring->path-tables) path-lookup-table)
          (dotimes (i (length (covered-file-paths covered-file)))
            (let ((path (svref (covered-file-paths covered-file) i)))
              (setf (gethash path path-lookup-table) i))))
        #-arm64
        (sb-sys:with-pinned-objects (code)
          (let ((sap (code-coverage-marks code)))
            (dotimes (i (length map))   ; for each recorded mark
              (when (byte-marked-p (sb-sys:sap-ref-8 sap i))
                ;; Set the file's EXECUTED bit for each affected source path
                (dolist (path (svref map i))
                  (let ((found (gethash path path-lookup-table)))
                    (if found
                        (setf (bit (covered-file-executed covered-file) found) 1)
                        #+nil
                        (warn "Missing coverage entry for ~S in ~S"
                              path namestring))))))))
        #+arm64
        (let ((marks (code-coverage-marks code)))
          (dotimes (i (length map))     ; for each recorded mark
            (when (byte-marked-p (aref marks i))
              (dolist (path (svref map i))
                (let ((found (gethash path path-lookup-table)))
                  (if found
                      (setf (bit (covered-file-executed covered-file) found) 1)))))))))))

) ; end MACROLET

;; This special is bound to the package in which forms will be read,
;; and reassigned as read-and-record-source-map detects what look like
;; "IN-PACKAGE" forms.
(defvar *current-package*)

(defun gen-moniker (source-map+idgen)
  ;; This is like GENSYM but cheaper and with no string-space pollution.
  ;; A gensym is 6 or 8 words, plus at least 4 words for chars of its name.
  ;; But we don't need these unique objects to be symbols necessarily- SAPs will
  ;; work, and get at least 5x space reduction. Setting the high bit makes these
  ;; SAPs somewhat unmistakable for genuine ones though it doesn't matter too much
  ;; since they're just cheap substitutes for a source form.
  ;; I'd like to use only integers, for futher shrinkage, but then a SOURCE-MAP
  ;; may have to be split into two tables: one whose keys were created as unique
  ;; identifiers, and one for literal integers as read.
  (let ((sigil-bit (ash 1 (1- sb-vm:n-word-bits))))
    (sb-sys:int-sap (logior sigil-bit (incf (cadr source-map+idgen))))))

(defun compute-file-info (filename external-format)
  ;; Go through all records, find the matching source in the file,
  ;; and update STATES to contain the state of the record in the
  ;; indexes matching the source location.
  (let* ((source (read-source filename external-format))
         ;; Cache the source-maps
         (maps (read-and-record-source-maps source))
         (states (initial-states source maps))
         (counts (list :branch (make-sample-count :branch)
                       :expression (make-sample-count :expression)))
         (records (get-records filename counts))
         ;; We do this in two stages: the first stage records the
         ;; character ranges, and the second stage does the update, in
         ;; order from shortest to longest ranges. This ensures that
         ;; for each index in STATES will reflect the state of the
         ;; innermost containing form.
         (locations (get-locations records maps filename)))
    ;; Now process the locations, from the shortest range to the longest
    ;; one. If two locations have the same range, the one with the higher
    ;; state takes precedence. The latter condition ensures that if
    ;; there are both normal- and branch-states for the same form,
    ;; the branch-state will be used.
    (fill-states-from-locations source states locations)
    (values counts states source)))

;;; Two varint-encoded integers can usually be stuffed into one fixnum.
;;; For 30-bit fixnums it's slightly more likely to be a bignum, but if
;;; the START is under 1MiB, it's very possibly a fixnum.
(defun pack-pair (start end &optional octets)
  (aver (>= end start))
  (unless octets ; a convenience for manual use and testing
    (setf octets (make-array 4 :element-type '(unsigned-byte 8) :fill-pointer 0)))
  ;; Delta-encoded as with code fixups, biased up by 1 because 0 signifies no data
  (write-var-integer (1+ start) octets)
  (write-var-integer (- end start) octets)
  (prog1 (sb-c::integer-from-octets octets) (setf (fill-pointer octets) 0)))

(defun unpack-pair (packed-pair)
  (let ((list (sb-c::unpack-code-fixup-locs packed-pair)))
    (values (1- (car list)) ; 1 as encoded means 0, etc
            ;; If START and END were =, then the delta is 0, which can't be encoded,
            ;; so the pair reads back as only one integer, which we just repeat.
            (1- (if (singleton-p list) (car list) (cadr list))))))

(defconstant +suppressed+ 15)
(defun source-position-line-number (source position)
  ;; Positions start at 1, so subtract 1 to get an array index
  (let ((array-index (1- position)))
    (aver (char/= (schar source array-index) #\newline))
    ;; This is line 1 if there are 0 newlines to the left; line 2 if 1, and so on
    (1+ (count #\newline source :end array-index))))

;;; Produce the STATES array without re-reading FILENAME.
;;; After doing this, it is possible to produce LCOV output using a few simple rules:
;;; - Lines will be reported as "feasible" only if at least one non-suppressed state
;;;   exists (i.e. some executable source form must be present) in the containing form.
;;;   KNOWN BUG: lines consisting of nothing but whitespace and comments within a
;;;   a larger form attract the coverage state of the containing form. This problem
;;;   is not specific to LCOV output, but HTML output as well.
;;; - Lines are considered "hit" only if they have zero unexecuted states.
;;;   Branch states other than "neither branch taken" are considered executed.
;;; - A line's branch count (taken/total) is computed over all :THEN or :ELSE paths
;;;   that start on the line. e.g. the denominator would be 4 on this line:
;;;     (values (if x (bar) (baz)) (if y (foo) (quux)))
(defun compute-file-states (filename)
  (binding* ((file (gethash filename (code-coverage-hashtable)) :exit-if-null)
             (loc-vec (covered-file-locations file) :exit-if-null)
             (linelengths (covered-file-lines file))
             (mock-source ; there is 1 fewer #\newline than there are lines
              (make-string (+ (reduce #'+ linelengths) (1- (length linelengths)))
                           :initial-element #\z))
             (states (make-array (length mock-source) :element-type '(unsigned-byte 4)
                                                      :initial-element 0))
             (counts (list :branch (make-sample-count :branch)
                           :expression (make-sample-count :expression)))
             (branch-lines (make-array (1+ (length linelengths)) :initial-element nil))
             (records (get-records filename counts)))
    ;; Insert #\newline chars. These are the only characters that FILL-WITH-STATE
    ;; cares about. Revision b40ce7df50 did away with looking for #\Space, possibly
    ;; a relic of an abandoned attempt to avoid coloring leading whitespace.
    (let ((pos 0))
      (dotimes (i (1- (length linelengths)))
        (let ((len (aref linelengths i)))
          (setf (char mock-source (incf pos len)) #\newline)
          (incf pos))))
    ;; Compute branch coverage for LCOV format
    (dolist (record records)
      (destructuring-bind (state . loc) (cdr record)
        (when (and (logtest state #b1100) ; a branch record
                   (not (eql loc 0)))
          (let* ((line (source-position-line-number mock-source (unpack-pair loc)))
                 ;; lines conventionally are numbered from 1
                 (cell (svref branch-lines (1- line)))
                 (then-taken (if (= (ldb (byte 2 0) state) 2) 0 1))
                 (else-taken (if (= (ldb (byte 2 2) state) 2) 0 1))
                 (ways (+ then-taken else-taken)))
            (unless cell
              (setf cell (cons 0 0) (svref branch-lines (1- line)) cell))
            (incf (car cell) ways)
            (incf (cdr cell) 2)))))
    ;; Elements of LOC-VEC at indices exceeding the length of COVERED-FILE-PATHS
    ;; are infeasible to execute.
    (loop for i from (length (covered-file-paths file)) below (length loc-vec)
          do (multiple-value-bind (start end) (unpack-pair (aref loc-vec i))
               (fill-with-state mock-source states +suppressed+ (1- start) end)))
    (fill-states-from-locations
     mock-source states
     (mapcan (lambda (record)
               (destructuring-bind (state . loc) (cdr record)
                 (if (/= loc 0)
                     (multiple-value-bind (start end) (unpack-pair loc)
                       (list (list start end state))))))
             records))
    (values counts states linelengths branch-lines)))

(defun get-records (filename counts)
  ;; If BRANCHING-LINES is specified, then it is an array with one element
  ;; per line such that if branching occurs at a line N, its cell will be
  ;; updated to hold a cons expressing the number of ways taken and the
  ;; number of possible ways. (So 1/2 is not the same as 2/4)
  (let* ((file (gethash filename (code-coverage-hashtable)))
         (paths (covered-file-paths file))
         (bits (covered-file-executed file))
         (locs (covered-file-locations file))
         (branch-locs (make-hash-table :test 'equal))
         (branch-recs (make-hash-table :test 'equal))
         (branch-count (getf counts :branch))
         (expression-count (getf counts :expression)))
    (collect ((records))
      (dotimes (i (length paths))
        (let ((state (if (zerop (sbit bits i)) 2 1))
              (path (aref paths i))
              (location (if (< i (length locs)) (aref locs i))))
          (cond ((member (car path) '(:then :else))
                 (when location
                   (pushnew location (gethash (cdr path) branch-locs)))
                 (setf (gethash (cdr path) branch-recs)
                       (logior (gethash (cdr path) branch-recs 0)
                               (ash state (if (eql (car path) :then) 0 2)))))
                (t
                 (when (eql state 1) (incf (ok-of expression-count)))
                 (incf (all-of expression-count))
                 (records (list* path state location))))))
      (maphash (lambda (path state)
                 ;; Each branch record accounts for two paths
                 (incf (ok-of branch-count)
                       (ecase state
                         (5 2)      ; #b0101 = both ways taken
                         ((6 9) 1)  ; #b0110 = taken/not-taken, #b1001 = not-taken/taken
                         (10 0)))   ; #b1010 = neither way taken
                 (incf (all-of branch-count) 2)
                 (let ((location (gethash path branch-locs)))
                   ;; :THEN and :ELSE must have the identical locations
                   ;; (it's the location of the value that picks the branch direction)
                   (when location
                     (aver (singleton-p location))
                     (setq location (car location)))
                   (records (list* path state location))))
               branch-recs)
      (records))))

(defun read-and-record-source-maps (source &aux (id-generator (list 0)))
  (with-input-from-string (stream source)
    (loop with *current-package* = (find-package "CL-USER")
          with map = nil
          with form = nil
          for i from 0
          do (setf (values form map)
                   (handler-case (read-and-record-source-map stream id-generator)
                     (error (error)
                       (warn "Error when recording source map for toplevel form ~A:~%  ~A" i error)
                       (values nil (make-hash-table)))))
          when map collect (cons form map)
          when (eql form *eof-object*) do (loop-finish))))

(defun initial-states (source maps)
  (let ((states (make-array (length source) :initial-element 0 :element-type '(unsigned-byte 4))))
    ;; we have read the source with our location-tracking reader; we
    ;; now know what parts of the source were *READ-SUPPRESS*ed.
    (note-suppressions source states maps)
    states))

(defun note-suppressions (source states maps)
  (dolist (tlf maps) ; = (form . hash-table)
    (dohash ((k locations) (cdr tlf))
      (declare (ignore k))
      (dolist (location locations)
        (destructuring-bind (start end &optional suppress) location
          ;; STATES array is 0-origin but locations are 1-origin, so the array
          ;; range to fill is (1- START) to (1- END) inclusive
          (when suppress
            (fill-with-state source states +suppressed+ (1- start) end)))))))

;;; Change most elements of STATES between START (inclusive) and END (exclusive)
;;; to STATE. Some elements will remain unaffected:
;;; - those which initially contain a nonzero value
;;; - those corresponding to a display column to to the left of the START column
(defun fill-with-state (source states state start end)
  (let* ((pos (position #\Newline source :end start :from-end t))
         (start-column (if pos (- start 1 pos) 0)))
    (loop for i from start below end
          for col from start-column
          for char = (aref source i)
          do (if (eql char #\Newline)
                 (setf col -1)
                 (when (and (zerop (aref states i)) (>= col start-column))
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

(defun get-locations (records maps filename)
  (let (locations)
    (dolist (record records locations)
      (destructuring-bind (rpath state . dummy) record
        (declare (ignore dummy))
        (let* ((path (reverse rpath))
               (tlf-num (car path))
               (tlf (nth tlf-num maps))
               (source-form (car tlf))
               (source-map (cdr tlf))
               (source-path (cdr path)))
          (if source-map
              (handler-case
                  (multiple-value-bind (start end)
                      (source-path-source-position (cons 0 source-path) source-form source-map)
                    (when (and start end)
                      (push (list start end state) locations)))
                (error (e)
                  (warn "~@<Error finding source location for source path ~A in file ~A: ~2I~_~A~@:>"
                        source-path filename e)))
              (warn "Unable to find a source map for toplevel form ~A in file ~A~%"
                    tlf-num filename)))))))

(defun fill-states-from-locations (source states locations)
  (setf locations (sort (copy-list locations) #'> :key #'third))
  (dolist (location (stable-sort locations #'<
                                 :key (lambda (location)
                                        (- (second location)
                                           (first location)))))
    (destructuring-bind (start end state) location
      (fill-with-state source states state start end))))

;;;; A mutant version of swank-source-path-parser from Swank/Slime.

(defun read-source (filename external-format)
  (detabify (read-file filename external-format)))

;;; Character handling in this file is largely agnostic of external formats.
;;; We achieve this feat by slurping each source file into a huge string and
;;; performing all processing in terms of string positions, not file-position
;;; on the underlying source file. That's the theory, but I would not be
;;; surprised if there are some minor formatting glitches in the HTML output
;;; whenever non-ASCII chars are present.
(defun read-file (filename external-format)
  "Return the entire contents of FILENAME as a string."
  (with-open-file (s filename :direction :input
                     :external-format external-format)
    (let* ((string (make-string (file-length s)))
           (nchars (read-sequence string s)))
      ;; FILE-LENGTH was an upper bound on the length in characters.
      (when (< nchars (length string))
        (sb-kernel:%shrink-vector string nchars))
      string)))

;;; Return a cons of (LOCATIONS . LINE-LENGTHS) where each element of LOCATIONS
;;; describes the bounding box of a corresponding element in PATHS, and
;;; elements of LINE-LENGTHS indicate where all the #\newline chars go.
(defun coverage-augmentation (stream paths)
  (file-position stream 0)
  (let* ((string (make-string (file-length stream)))
         (nchars (read-sequence string stream)))
    (when (< nchars (length string))
      (sb-kernel:%shrink-vector string nchars))
    (setq string (detabify string))
    (let* ((source-maps (let ((*package* (find-package "CL-USER")))
                          (read-and-record-source-maps string)))
           (lines
            (collect ((lines))
             (let ((start 0))
               (loop
                 (let ((end (position #\newline string :start start)))
                   (lines (- (or end (length string)) start))
                   (if end (setq start (1+ end)) (return (lines))))))))
           (locations (make-array (length paths) :initial-element 0))
           (suppressions)
           (octets (make-array 4 :element-type '(unsigned-byte 8) :fill-pointer 0)))
      ;; just like NOTE-SUPPRESSIONS but different
      (dolist (tlf source-maps) ; = (form . hash-table)
        (dohash ((k locations) (cdr tlf))
          (declare (ignore k))
          (dolist (location locations)
            (when (third location)
              (push (pack-pair (first location) (second location) octets) suppressions)))))
      ;; just like GET-LOCATIONS but different
      (dotimes (i (length paths))
        (binding* ((rpath (aref paths i))
                   (path (reverse (if (fixnump (car rpath)) rpath (cdr rpath))))
                   (tlf-num (car path))
                   (tlf (nth tlf-num source-maps))
                   (source-form (car tlf))
                   (source-map (cdr tlf))
                   (source-path (cdr path))
                   ((start end)
                    (source-path-source-position (cons 0 source-path) source-form source-map)))
          (when (and start end)
            (setf (aref locations i) (pack-pair start end)))))
      (cons (sb-c::coerce-to-smallest-eltype (concatenate 'vector locations suppressions))
            (sb-c::coerce-to-smallest-eltype lines)))))

;;; In the usual way of invoking SB-COVER:REPORT, it first re-reads all source files,
;;; without which, we lack a way for code under test to produce side-channel artifacts
;;; describing source forms hit in a way that most coverage aggregation tooling expects.
;;; (e.g. "lines 1 through 9 are comments; lines 10 through 20 were executed")
;;; At best we could output source paths reached. Those tend to be not user-friendly.
;;; To improve things so that tests can emit descriptive files usable for later
;;; consumption by non-Lisp tooling (think LCOV,GCOV), we have a few options:
;;; (1) in the code being run, feed it all ita sources (from in-memory streams perhaps?)
;;;     to re-parse and derive so-called "source maps" just-in-time, OR
;;; (2) invent a compact way to represent source-maps in the code under test, OR
;;; (3) translate source paths to physical bounding boxes at compile-time.
;;; This enhacement takess the third approach, storing more data in *CODE-COVERAGE-INFO*
;;; if SB-COVER:ENABLE-COVERAGE-LOGGING is called prior to compiling anything.
;;; Coverage-instrumented functions gain enough metadata to describe the forms reached
;;; by line and column. Thus we separate analysim from presentation, and only the UI needs
;;; access to the source code for purposes of rendering it in different colors.
(defun enable-coverage-logging ()
  (setq sb-c::*coverage-augmentation-hook* #'coverage-augmentation))

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
        (push (list* start end (if *read-suppress* '(t)))
              (gethash (car values) source-map)))
      (values-list values))))

(defun make-source-recording-readtable (readtable source-map+idgen)
  "Return a source position recording copy of READTABLE.
The source locations are stored in (CAR SOURCE-MAP+IDGEN)"
  (declare (type (cons hash-table (cons integer null)) source-map+idgen))
  (let* ((tab (copy-readtable readtable))
         (source-map (car source-map+idgen))
         (*readtable* tab))
    ;; Preserve sharp-dot in feature conditional expressions (so do
    ;; this before SUPPRESS-SHARP-DOT).
    (preserve-sharp-dot-in-sharp-plus-minus tab)
    ;; It is unspecified whether doing (SET-MACRO-CHARACTER c1 fn)
    ;; and then (SET-DISPATCH-MACRO-CHARACTER c1 c2 fn) should be allowed.
    ;; Portability concerns aside, it doesn't work in the latest code,
    ;; but first changing the function for #. and then # in that order works.
    (suppress-sharp-dot tab)
    (suppress-sharp-a tab)
    (suppress-sharp-c tab)
    (dotimes (code 128)
      (let ((char (code-char code)))
        (multiple-value-bind (fn term) (get-macro-character char tab)
          (when fn
            (set-macro-character char (make-source-recorder fn source-map)
                                 term tab)))))
    (set-macro-character #\` (make-source-recorder #'read-backq source-map))
    (set-macro-character #\, (make-source-recorder #'read-comma source-map))
    (set-macro-character #\(
                         (make-source-recorder
                          (make-recording-read-list source-map+idgen)
                          source-map))
    tab))

(defvar *backquote-level* 0)
(defun read-backq (stream ignore)
  (declare (ignore ignore))
  (let ((*backquote-level* (1+ *backquote-level*)))
    (list 'backquote (read stream t nil t))))
(defun read-comma (stream ignore)
  (declare (ignore ignore))
  (unless (> *backquote-level* 0)
    (when *read-suppress*
      (return-from read-comma nil))
    (error "comma found not within a corresponding backquote"))
  (let ((flag-char (read-char stream)))
    (case flag-char
      ((#\. #\@))
      (t (unread-char flag-char stream)))
    (let ((*backquote-level* (1- *backquote-level*)))
      (list 'comma (read stream t nil t)))))

(defstruct read-eval-marker)

;;; Ripped from SB-IMPL, since location recording on a cons-cell level
;;; can't be done just by simple read-table tricks.
(defun make-recording-read-list (source-map+idgen)
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
            ((char= firstchar #\)) (cdr thelist))
          (when (char= firstchar #\.)
            (let ((nextchar (read-char stream t)))
              (cond ((sb-impl:token-delimiterp nextchar rt)
                     (cond ((eq listtail thelist)
                            (unless *read-suppress*
                              (simple-reader-error stream
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
          (binding* ((start (file-position stream))
                     ((winp obj) (sb-impl::read-maybe-nothing stream firstchar))
                     (end (file-position stream)))
            ;; allows the possibility that a comment was read
            (unless (eql winp 0)
              (let ((listobj (list obj))
                    (source-map (car source-map+idgen)))
                (unless (or (consp obj) (read-eval-marker-p obj))
                  (setf (car listobj) (gen-moniker source-map+idgen))
                  (push (list* start end (if *read-suppress* '(t)))
                        (gethash (car listobj) source-map)))
                (rplacd listtail listobj)
                (setq listtail listobj)))))))))

(defun preserve-sharp-dot-in-sharp-plus-minus (readtable)
  (when (get-macro-character #\# readtable)
    (let ((sharp-dot (get-dispatch-macro-character #\# #\. readtable))
          (sharp-plus (get-dispatch-macro-character #\# #\+ readtable))
          (sharp-minus (get-dispatch-macro-character #\# #\- readtable)))
      (when (and sharp-dot sharp-plus sharp-minus)
        (let ((copy (copy-readtable readtable)))
          (flet ((sharp-plus-minus (stream sub-char numarg)
                   (declare (ignore numarg))
                   (if (char= sub-char
                              (if (featurep (let ((*package* *keyword-package*)
                                                  (sb-impl::*reader-package* nil)
                                                  (*read-suppress* nil)
                                                  (*readtable* copy))
                                              (read stream t nil t)))
                                  #\+ #\-))
                       (read stream t nil t)
                       (let ((*read-suppress* t))
                         (read stream t nil t)
                         (values)))))
            (set-dispatch-macro-character #\# #\+ #'sharp-plus-minus)
            (set-dispatch-macro-character #\# #\- #'sharp-plus-minus)))))))

(defun suppress-sharp-dot (readtable)
  (when (get-macro-character #\# readtable)
    (let ((sharp-dot (get-dispatch-macro-character #\# #\. readtable)))
      (when sharp-dot
        (set-dispatch-macro-character #\# #\.
                                      (lambda (stream &rest args)
                                        (declare (ignore args))
                                        (let ((*backquote-level* 0))
                                          (read stream t nil t)
                                          (make-read-eval-marker))))
                                      readtable))))

(defun suppress-sharp-c (readtable)
  (when (get-macro-character #\# readtable)
    (let ((sharp-c (get-dispatch-macro-character #\# #\c readtable)))
      (when sharp-c
        ;; we don't actually use *READ-SUPPRESS* here because we don't
        ;; want to annotate the list part of the complex as
        ;; conditionalized out.
        (flet ((sharp-c-replacement (stream subchar numarg)
                 (declare (ignore subchar numarg))
                 (let ((thing (read stream t nil t)))
                   (cond
                     (*read-suppress* nil)
                     ((and (listp thing) (= (length thing) 2)) #c(1 1))
                     (t (simple-reader-error stream "illegal complex number format: #C~S" thing))))))
          (set-dispatch-macro-character #\# #\c #'sharp-c-replacement readtable))))))

(defun suppress-sharp-a (readtable)
  (when (get-macro-character #\# readtable)
    (let ((sharp-a (get-dispatch-macro-character #\# #\a readtable)))
      (when sharp-a
        (flet ((sharp-a-replacement (stream subchar numarg)
                 (declare (ignore subchar))
                 (let ((thing (read stream t nil t)))
                   (cond
                     (*read-suppress* nil)
                     ;; regular #2A(...) syntax
                     ((and numarg (typep thing 'sequence)) #())
                     ;; extended #A(dims element-type &rest contents) syntax
                     ((not numarg) #())
                     (t (simple-reader-error stream "illegal literal array format: #~DA~S"
                                             numarg thing))))))
          (set-dispatch-macro-character #\# #\a #'sharp-a-replacement readtable))))))

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

;;; A SOURCE-MAP is a hash-table which maps each subform of FORM to a list of
;;; locations where it appears in the stream. Subforms other than cons cells are replaced
;;; by unique gensyms.
;;; In most situations, each subform will have exactly one location at which it appears,
;;; however, indistinguishable objects have multiple locations.
;;; Consider "(defvar v #(#x0 #x000 zot))". In it, the atoms #x0 and #x000 are identical,
;;; so the source map will say that the integer 0 appears at locations 13-15 and 17-21.
;;; Note also two conventions with regard to location representation:
;;;  - indexing uses an origin of 1.  This is the same as for 'point' in Emacs.
;;;  - ranges are _inclusive_ of the upper bound
;;;
;;; The full set of subforms for this example would resemble:
;;;    #:G216 -> ((2 7 NIL))                    ; symbol DEFVAR
;;;    #:G217 -> ((9 9 NIL))                    ; symbol V
;;;    0 -> ((17 21 NIL) (13 15 NIL))           ; integer zero
;;;    #(0 0 ZOT) -> ((11 26 NIL))              ; vector as read
;;;    #:G218 -> ((11 26 NIL))                  ; same vector after gensym substitution
;;;    (#:G216 #:G217 #:G218) -> ((1 27 NIL))   ; entire form
;;;
;;; Had the #x reader macro not been used - like in "(defvar v #(0 0 zot))" - then
;;; there would be 1 fewer key in the hash table, something like:
;;;    #:G219 -> ((2 7 NIL))                    ; symbol DEFVAR
;;;    #:G220 -> ((9 9 NIL))                    ; symbol V
;;;    #(0 0 ZOT) -> ((11 20 NIL))              ; vector as read
;;;    #:G221 -> ((11 20 NIL))                  ; after gensym substitution
;;;    (#:G219 #:G220 #:G221) -> ((1 21 NIL))   ; entire form
;;;
(defun read-and-record-source-map (stream id-generator)
  "Read the next object from STREAM.
Return the object together with a hashtable that maps
subexpressions of the object to stream positions."
  (let* ((source-map (make-hash-table :test #'eq))
         ;; I suspect that we only need to ensure uniqueness of the subforms
         ;; which represent original forms within a single hash-table,
         ;; but by using an ID generator that increments monotonically
         ;; across all hash-tables is probably a little easier to understand.
         (source-map+idgen (cons source-map id-generator))
         (start (file-position stream))
         ;; Maybe build just one readtable across this whole run?
         ;; (This conses at least 2KiB just to make the readtable)
         (form (let ((*readtable* (make-source-recording-readtable
                                   *readtable* source-map+idgen))
                     (*package* *current-package*))
                 (read stream nil *eof-object*)))
         (end (file-position stream)))
    (when (eql form *eof-object*)
      ;; we might have suppressed some content under #+ or similar
      (return-from read-and-record-source-map (values form source-map)))
    (look-for-in-package-form-in-stream stream start end)
    ;; ensure that at least FORM is in the source-map
    (unless (gethash form source-map)
      (push (list start end) (gethash form source-map)))
    (values form source-map)))

(defun nth-or-marker (n list)
  (cond
    ((read-eval-marker-p list) list)
    ((= n 0) (car list))
    (t (nth-or-marker (1- n) (cdr list)))))

(defun source-path-source-position (path form source-map)
  "Return the start and end positions of PATH from FORM and SOURCE-MAP.  All
subforms along the path are considered and the start and end position
of the deepest (i.e. smallest) possible form is returned."
  ;; compute all subforms along path
  (let ((forms (loop for ns on path
                     for n = (car ns)
                     for f = form then (nth-or-marker n f)
                     collect f into forms
                     if (read-eval-marker-p f)
                       do (return-from source-path-source-position (values nil nil))
                     finally (return forms))))
    ;; select the first subform present in source-map
    (loop for real-form in (reverse forms)
          for form = (if (or (eql *source-path-mode* :whole)
                             (not (consp real-form)))
                         real-form
                         (car real-form))
          for positions = (gethash form source-map)
          until positions
          finally (destructuring-bind ((start end &optional suppress)) (last positions)
                    (declare (ignore suppress))
                    (return (values (1- start) end))))))

;;; Return 1 for hit, 0 for not hit, or NIL for not executable (comments, read-suppressions)
(defun summarize-states-between (states start end)
  (let ((executed nil) (not-executed nil))
    (loop for i from start below end
          do (let ((state (aref states i)))
               (ecase state
                 ((0 #.+suppressed+)) ; ignore
                 ((2 #b1010) (setq not-executed t))
                 ((1 5 6 9) (setq executed t)))))
    (if (or executed not-executed) (if not-executed 0 1))))

(defun lcov-report (pathname)
  (refresh-coverage-bits)
  (with-open-file (stream pathname :direction :output :if-does-not-exist :create
                                   :if-exists :supersede)
    (dohash ((filename info) (code-coverage-hashtable))
      (declare (ignore info))
      (multiple-value-bind (counts states linelengths branch-lines)
          (compute-file-states filename)
        (declare (ignore counts))
        (format stream "SF:~A~%" filename)
        ;; For each line (1 more than LINELENGTHS), figure out an aggregate
        ;; value for whether it was hit or not.
        (let ((state-index 0) (line 1))
          (loop
            (let* ((nchars (aref linelengths (1- line)))
                   (summary
                    (summarize-states-between states state-index (+ state-index nchars))))
              (if summary (format stream "DA:~D,~D~%" line summary))
              (if (>= (incf state-index (1+ nchars)) (length states)) (return))
              (incf line))))
        (loop for line from 1 to (length (the simple-vector branch-lines))
              do (awhen (svref branch-lines (1- line))
                   (destructuring-bind (taken . ways) it
                   (dotimes (i (- ways taken)) (format stream "BA:~D,1~%" line))
                   (dotimes (i taken) (format stream "BA:~D,2~%" line)) )))
        (format stream "end_of_record~%")))))
