;;;; that part of the loader is only needed on the target system
;;;; (which is basically synonymous with "that part of the loader
;;;; which is not needed by GENESIS")

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-FASL")

(defvar *load-source-default-type* "lisp"
  "The source file types which LOAD looks for by default.")

(defvar *load-verbose* nil
  ;; Note that CMU CL's default for this was T, and ANSI says it's
  ;; implementation-dependent. We choose NIL on the theory that it's
  ;; a nicer default behavior for Unix programs.
  "the default for the :VERBOSE argument to LOAD")

(defvar *load-print* nil
  "the default for the :PRINT argument to LOAD")

#+ansi-compliant-load-truename
(defvar *load-truename* nil
  "the TRUENAME of the file that LOAD is currently loading")
#-ansi-compliant-load-truename
(setf (documentation '*load-truename* 'variable)
  "the TRUENAME of the file that LOAD is currently loading")

(defvar *load-pathname* nil
  "the defaulted pathname that LOAD is currently loading")

(declaim (type (or pathname null) *load-truename* *load-pathname*))


;;;; LOAD-AS-SOURCE

;;; something not EQ to anything we might legitimately READ
(define-load-time-global *eof-object* (make-symbol "EOF-OBJECT"))

(macrolet ((make-file-stream-source-info (s)
             `(sb-c::make-source-info
               :file-info (sb-c::make-file-info
                           :%truename :defer
                           ;; This T-L-P has been around since at least 2011.
                           ;; It's unclear why an LPN isn't good enough.
                           :pathname (translate-logical-pathname ,s)
                           :external-format (stream-external-format ,s)
                           :write-date (file-write-date ,s)))))

;;; Load a text stream.  (Note that load-as-fasl is in another file.)
;; We'd like, when entering the debugger as a result of an EVAL error,
;; that the condition be annotated with the stream position.
;; One way to do it is catch all conditions and encapsulate them in
;; something new such as a LOADER-EVAL-ERROR and re-signal.
;; The printer for the encapsulated condition has the data it needs to
;; show the original condition and the line/col. That would unfortunately
;; interfere with handlers that were bound around LOAD, since they would
;; only receive the encapsulated condition, and not be able to test for
;; things they're interested in, such as which redefinition warnings to ignore.
;; Instead, printing a herald for any SERIOUS-CONDITION approximates
;; the desired behavior closely enough without printing something for warnings.
;; TODO: It would be supremely cool if, for toplevel PROGN, we could
;; indicate the position of the specific subform that failed
(defun load-as-source (stream &key verbose print (context "loading"))
  (maybe-announce-load stream verbose)
  (let* ((pathname (ignore-errors (translate-logical-pathname stream)))
         (native (when pathname (native-namestring pathname))))
    (with-simple-restart (abort "Abort ~A file ~S." context native)
     (labels ((condition-herald (c)
                (declare (ignore c)) ; propagates up
                (when (form-tracking-stream-p stream)
                  (let* ((startpos
                          (form-tracking-stream-form-start-char-pos stream))
                         (point (line/col-from-charpos stream startpos)))
                    (format *error-output* "~&While evaluating the form ~
 starting at line ~D, column ~D~%  of ~S:"
                            (car point) (cdr point)
                            (or pathname stream)))))
              (eval-form (form index)
               (with-simple-restart (continue "Ignore error and continue ~A file ~S."
                                              context native)
                 (loop
                  (handler-bind ((serious-condition #'condition-herald))
                   (with-simple-restart (retry "Retry EVAL of current toplevel form.")
                     (if print
                         (let ((results (multiple-value-list (eval-tlf form index))))
                           (load-fresh-line)
                           (format t "~{~S~^, ~}~%" results))
                         (eval-tlf form index)))
                    (return))))))
       (let ((sb-c::*current-path* nil)
             (sb-impl::*eval-source-info* nil)
             (sb-impl::*eval-tlf-index* nil)
             (sb-impl::*eval-source-context* nil))
         (locally (declare (optimize (sb-c::type-check 0)))
           (setf sb-c::*current-path* (make-unbound-marker)))
         (if pathname
             (let* ((info (make-file-stream-source-info stream))
                    (sb-c::*source-info* info))
               (locally (declare (optimize (sb-c::type-check 0)))
                 (setf sb-c::*current-path* (make-unbound-marker)))
               (setf (sb-c::source-info-stream info) stream)
               (sb-c:do-forms-from-info ((form current-index) info
                                          'sb-c::input-error-in-load)
                 (sb-c::with-source-paths
                   (sb-c::find-source-paths form current-index)
                   (eval-form form current-index))))
             (let ((sb-c::*source-info* nil))
               (loop for form =
                     (handler-case (read stream nil *eof-object*)
                       ((or reader-error end-of-file) (c)
                         (error 'sb-c::input-error-in-load :stream stream
                                                           :condition c)))
                     until (eq form *eof-object*)
                     do (sb-c::with-source-paths
                          (eval-form form nil)))))))))
  t)
) ; end MACROLET

;;;; LOAD itself

(define-condition fasl-header-missing (invalid-fasl)
  ((fhsss :reader invalid-fasl-fhsss :initarg :fhsss))
  (:report
   (lambda (condition stream)
     (format stream "~@<File ~S has a fasl file type, but no fasl header:~%~
                     Expected ~S, but got ~S.~:@>"
             (invalid-fasl-stream condition)
             (invalid-fasl-expected condition)
             (invalid-fasl-fhsss condition)))))

(defun call-with-load-bindings (function stream arg pathname-designator)
  (let* (;; FIXME: we should probably document the circumstances
         ;; where *LOAD-PATHNAME* and *LOAD-TRUENAME* aren't
         ;; pathnames during LOAD.  ANSI makes no exceptions here.
         (*load-pathname* (handler-case (pathname pathname-designator)
                            ;; FIXME: it should probably be a type
                            ;; error to try to get a pathname for a
                            ;; stream that doesn't have one, but I
                            ;; don't know if we guarantee that.
                            (error () nil)))
         ;; FIXME: this ANSI-specified nonsense should have been an accessor call
         ;; because eager binding might force dozens of syscalls to occur.
         ;; The non-ansi-compliant code resolves *LOAD-TRUENAME* only if referenced.
         #-ansi-compliant-load-truename (%load-truename (make-unbound-marker))
         #+ansi-compliant-load-truename
         (*load-truename* (when *load-pathname*
                            (handler-case (truename stream)
                              (file-error () nil))))
         ;; Bindings used internally.
         (*load-depth* (1+ *load-depth*)))
    (funcall function stream arg)))

(defun resolve-load-truename (symbol untruename)
  (if (boundp symbol)
      (symbol-value symbol)
      (set symbol (when untruename
                    (handler-case (truename untruename)
                      (file-error () nil))))))

;;; Returns T if the stream is a binary input stream with a FASL header.
(defun fasl-header-p (stream &key errorp)
  (unless (and (member (stream-element-type stream) '(character base-char))
               ;; give up if it's not a file stream, or it's an
               ;; fd-stream but it's either not bivalent or not
               ;; seekable (doesn't really have a file)
               (or (not (typep stream 'file-stream))
                   (and (typep stream 'fd-stream)
                        (or (not (sb-impl::fd-stream-bivalent-p stream))
                            (not (sb-impl::fd-stream-file stream))
                            (neq (sb-impl::fd-stream-fd-type stream) :regular)))))
    (let ((p (file-position stream)))
      (when p ; Can't do it non-destructively on non-seekable streams.
        (unwind-protect
             (let* ((header *fasl-header-string-start-string*)
                    ;; MISMATCH is called no matter how few octets are read in,
                    ;; so start with BUFFER completely 0-initialized.
                    (buffer (make-array (length header) :element-type '(unsigned-byte 8)
                                                        :initial-element 0))
                    (n 0))
               (flet ((scan ()
                        (maybe-skip-shebang-line stream)
                        (setf n (read-sequence buffer stream))))
                 (if errorp
                     (scan)
                     (or (ignore-errors (scan))
                         ;; no a binary input stream
                         (return-from fasl-header-p nil))))
               (cond ((not (mismatch buffer header
                                     :test #'(lambda (code char) (= code (char-code char))))))
                     ((zerop n)
                      ;; Immediate EOF is valid -- we want to match what
                      ;; CHECK-FASL-HEADER does...
                      nil)
                     (errorp
                      (error 'fasl-header-missing
                             :stream stream
                             :fhsss buffer
                             :expected header))))
          (file-position stream p))))))

(defun load (filespec &key (verbose *load-verbose*) (print *load-print*)
                           (if-does-not-exist :error) (external-format :default))
  "Load the file given by FILESPEC into the Lisp environment, returning T on
   success. The file type (a.k.a extension) is defaulted if missing. These
   options are defined:

   :IF-DOES-NOT-EXIST
       If :ERROR (the default), signal an error if the file can't be located.
       If NIL, simply return NIL (LOAD normally returns T.)

   :VERBOSE
       If true, print a line describing each file loaded.

   :PRINT
       If true, print information about loaded values.  When loading the
       source, the result of evaluating each top-level form is printed.

   :EXTERNAL-FORMAT
       The external-format to use when opening the FILENAME. The default is
       :DEFAULT which uses the SB-EXT:*DEFAULT-EXTERNAL-FORMAT*."
  (labels ((load-stream (stream faslp)
             (if (and (fd-stream-p stream)
                      (eq (sb-impl::fd-stream-fd-type stream) :directory))
                 (error 'simple-file-error
                        :pathname (pathname stream)
                        :format-control "Can't LOAD a directory: ~s."
                        :format-arguments (list (pathname stream)))
                 (call-with-load-bindings
                  #'load-stream-1 stream
                  faslp
                  ;; If you prefer *LOAD-PATHNAME* to reflect what the user specified prior
                  ;; to merging, then CALL-WITH-LOAD-BINDINGS is passed FILESPEC,
                  ;; otherwise it is passed STREAM.
                  (cond ((and (not sb-c::*merge-pathnames*)
                              (typep filespec '(or string pathname)))
                         filespec)
                        (t stream)))))
           (load-stream-1 (stream faslp)
             (let (;; Bindings required by ANSI.
                   (*readtable* *readtable*)
                   (*package* (sane-package))
                   ;; KLUDGE: I can't find in the ANSI spec where it says
                   ;; that DECLAIM/PROCLAIM of optimization policy should
                   ;; have file scope. CMU CL did this, and it seems
                   ;; reasonable, but it might not be right; after all,
                   ;; things like (PROCLAIM '(TYPE ..)) don't have file
                   ;; scope, and I can't find anything under PROCLAIM or
                   ;; COMPILE-FILE or LOAD or OPTIMIZE which justifies this
                   ;; behavior. Hmm. -- WHN 2001-04-06
                   (sb-c::*policy* sb-c::*policy*)
                   (sb-c::*handled-conditions* sb-c::*handled-conditions*))
               (if faslp
                   (load-as-fasl stream verbose (if print t nil))
                   ;; FIXME: if *EVALUATOR-MODE* is :INTERPRET,
                   ;; then this should have nothing whatsoever to do with
                   ;; compiler-error-resignaling. That's an artifact
                   ;; of using the compiler to perform interpretation.
                   (sb-c:with-compiler-error-resignalling
                       (load-as-source stream :verbose verbose :print print))))))
    (declare (dynamic-extent #'load-stream-1))

    ;; Case 1: stream.
    (when (streamp filespec)
      (return-from load (load-stream filespec (fasl-header-p filespec))))

    (let ((pathname (pathname filespec)))
      ;; Case 2: Open as binary, try to process as a fasl.
      (with-open-stream
          (stream (or (open filespec :element-type '(unsigned-byte 8)
                                     :if-does-not-exist nil)
                      (when (null (pathname-type filespec))
                        (let ((defaulted-pathname
                                (probe-load-defaults filespec)))
                          (if defaulted-pathname
                              (progn (setq pathname defaulted-pathname)
                                     (open pathname
                                           :if-does-not-exist
                                           (if if-does-not-exist :error nil)
                                           :element-type '(unsigned-byte 8))))))
                      (if if-does-not-exist
                          (error 'simple-file-error
                                 :pathname filespec
                                 :format-control
                                 "~@<Couldn't load ~S: file does not exist.~@:>"
                                 :format-arguments (list filespec))
                          (return-from load nil))))
        ;; This is the understandable logic.
        #-ansi-compliant-load-truename
        (if (string-equal (pathname-type pathname) *fasl-file-type*)
            (load-stream stream t)
            (with-open-file (stream pathname :external-format external-format
                                             :class 'form-tracking-stream)
              (load-stream stream nil)))
        ;; I have no idea what "don't allow empty .fasls" was supposed to mean.
        ;; The more natural interpretation to me would be that if a file is empty,
        ;; we don't treat the file as fasl, but instead treat it as source (and do nothing)
        ;; regardless of the name. But the actual effect seems to be to force an error.
        ;; So I would have commented that as "force an error on empty fasls", especially
        ;; as there is ambiguity in "don't X and Y" - does it mean "don't X" and "don't Y"?
        ;; Because surely that would be illogical - "don't assume empty files are source".
        ;; Anyway, reasonable users should never test the edge cases of this,
        ;; and so all it effectively achieves is slowing down the loading of files.
        #+ansi-compliant-load-truename
        (let* ((real (probe-file stream))
               (should-be-fasl-p
                 (and real (string-equal (pathname-type real) *fasl-file-type*))))
          ;; Don't allow empty .fasls, and assume other empty files
          ;; are source files.
          (cond ((or (and should-be-fasl-p (eql 0 (file-length stream)))
                     (fasl-header-p stream :errorp should-be-fasl-p))
                 (load-stream stream t))
                (t
                 ;; Case 3: Open using the given external format, process as source.
                 (with-open-file (stream pathname :external-format external-format
                                                  :class 'form-tracking-stream)
                   (load-stream stream nil)))))))))

;; This implements the defaulting SBCL seems to have inherited from
;; CMU.  This routine does not try to perform any loading; all it does
;; is return the pathname (not the truename) of a file to be loaded,
;; or NIL if no such file can be found.  This routine is supposed to
;; signal an error if a fasl's timestamp is older than its source
;; file, but we protect against errors in PROBE-FILE, because any of
;; the ways that we might fail to find a defaulted file are reasons
;; not to load it, but not worth exposing to the user who didn't
;; expicitly ask us to load a file with a made-up name (e.g., the
;; defaulted filename might exceed filename length limits).
(defun probe-load-defaults (pathname)
  (multiple-value-bind (source-existsp defaulted-source-pathname
                        fasl-existsp defaulted-fasl-pathname)
      (flet ((probe (type &aux (candidate (make-pathname :type type
                                                         :defaults pathname)))
               ;; %query-file-system doesn't like LPNs. Maybe this should just
               ;; always call (%query-file-system (translate-logical-pathname))
               ;; since that works in either case. The right thing may be to translate
               ;; before calling PROBE-LOAD-DEFAULTS but I'm not sure, because the
               ;; translation of the name could differ by the type.
               ;; In no case should this function ask for truename of a physical
               ;; pathname on #+unix but maybe on #+win32 it has to, for reasons
               ;; that I don't know. If so, we can just turn this test into
               ;; (if (or #+win32 t (logical-pathname-p candidate)))
               (values (if (logical-pathname-p candidate)
                           (probe-file candidate)
                           (sb-impl::%query-file-system candidate :existence nil))
                       candidate)))
        (multiple-value-call #'values
          (probe *load-source-default-type*) (probe *fasl-file-type*)))
    (cond ((and fasl-existsp
                source-existsp
                (> (file-write-date defaulted-source-pathname)
                   (file-write-date defaulted-fasl-pathname)))
           (restart-case
               (error "The object file ~A is~@
                       older than the presumed source:~%  ~A."
                      defaulted-fasl-pathname
                      defaulted-source-pathname)
             (source () :report "load source file"
                     defaulted-source-pathname)
             (object () :report "load object file"
                     defaulted-fasl-pathname)))
          (fasl-existsp defaulted-fasl-pathname)
          (source-existsp defaulted-source-pathname))))

;;;; linkage fixups

(defun %asm-routine-table (code-obj)
   (#+darwin-jit car #-darwin-jit identity (%code-debug-info code-obj)))

(defun calc-asm-routine-bounds ()
  (loop for v being each hash-value of (%asm-routine-table *assembler-routines*)
        minimize (car v) into min
        maximize (cadr v) into max
        ;; min/max are inclusive byte ranges, but return the answer
        ;; using standard convention of exclusive upper bound.
        finally (return (values min (1+ max)))))

;;; how we learn about assembler routines at startup
(defvar *!initial-assembler-routines*)

(defun get-asm-routine (name &aux (code *assembler-routines*))
  (awhen (the list (gethash (the symbol name) (%asm-routine-table code)))
    (sap-int (sap+ (code-instructions code) (car it)))))

(defun asm-routine-index-from-addr (addr)
  (let* ((code *assembler-routines*)
         (max (hash-table-count (%asm-routine-table code)))
         (min 1)
         (table (code-instructions code)))
    (loop
     (let* ((guess-index (floor (+ min max) 2))
            (guess-val (sap-ref-word table (ash guess-index sb-vm:word-shift))))
       (cond ((= guess-val addr) (return guess-index))
             ((< guess-val addr) (setq min (1+ guess-index))) ; guess too small
             (t (setq max (1- guess-index)))) ; guess too large
       (if (< max min) (return nil))))))

(defun !loader-cold-init ()
  (let* ((code *assembler-routines*)
         (size (%code-text-size code))
         (vector (the simple-vector *!initial-assembler-routines*))
         (count (length vector))
         (offsets (make-array (1+ count) :element-type '(unsigned-byte 16)))
         (ht (make-hash-table))) ; keys are symbols
    (#+darwin-jit rplaca #-darwin-jit setf (%code-debug-info code) ht)
    (setf sb-c::*asm-routine-offsets* offsets)
    (dotimes (i count)
      (destructuring-bind (name . offset) (svref vector i)
        (let ((next-offset (if (< (1+ i) count) (cdr (svref vector (1+ i))) size)))
          (setf (aref offsets (1+ i)) offset)
          ;; Must be in ascending order, but one address can have more than one name.
          (aver (>= next-offset offset))
          ;; store inclusive bounds on PC offset range and the function index
          (setf (gethash name ht) (list* offset (1- next-offset) (1+ i))))))))

(defun !warm-load (file)
  (restart-case (let ((sb-c::*source-namestring*
                       (format nil "SYS:~A" (substitute #\; #\/ file))))
                  (load file))
    (abort-build ()
      :report "Abort building SBCL."
      (sb-ext:exit :code 1))))
