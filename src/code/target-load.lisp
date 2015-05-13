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

(in-package "SB!FASL")

(defvar *load-source-default-type* "lisp"
  #!+sb-doc
  "The source file types which LOAD looks for by default.")

(declaim (type (or pathname null) *load-truename* *load-pathname*))
(defvar *load-truename* nil
  #!+sb-doc
  "the TRUENAME of the file that LOAD is currently loading")
(defvar *load-pathname* nil
  #!+sb-doc
  "the defaulted pathname that LOAD is currently loading")

;;;; LOAD-AS-SOURCE

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
        (if pathname
            (let* ((info (sb!c::make-file-source-info
                          pathname (stream-external-format stream)))
                   (sb!c::*source-info* info))
              (setf (sb!c::source-info-stream info) stream)
              (sb!c::do-forms-from-info ((form current-index) info
                                         'sb!c::input-error-in-load)
                (sb!c::with-source-paths
                  (sb!c::find-source-paths form current-index)
                  (eval-form form current-index))))
            (let ((sb!c::*source-info* nil))
              (do ((form (read stream nil *eof-object*)
                         (read stream nil *eof-object*)))
                  ((eq form *eof-object*))
                (sb!c::with-source-paths
                  (eval-form form nil))))))))
  t)

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


;;; The following comment preceded the pre 1.0.12.36 definition of
;;; LOAD; it may no longer be accurate:

;; FIXME: Daniel Barlow's ilsb.tar ILISP-for-SBCL patches contain an
;; implementation of "DEFUN SOURCE-FILE" which claims, in a comment,
;; that CMU CL does not correctly record source file information when
;; LOADing a non-compiled file. Check whether this bug exists in SBCL
;; and fix it if so.

(defun load (pathspec &key (verbose *load-verbose*) (print *load-print*)
             (if-does-not-exist t) (external-format :default))
  #!+sb-doc
  "Load the file given by FILESPEC into the Lisp environment, returning
   T on success."
  (flet ((load-stream (stream faslp)
           (when (and (fd-stream-p stream)
                      (eq (sb!impl::fd-stream-fd-type stream) :directory))
             (error 'simple-file-error
                    :pathname (pathname stream)
                    :format-control
                    "Can't LOAD a directory: ~s."
                    :format-arguments (list (pathname stream))))
           (let* (;; Bindings required by ANSI.
                  (*readtable* *readtable*)
                  (*package* (sane-package))
                  ;; FIXME: we should probably document the circumstances
                  ;; where *LOAD-PATHNAME* and *LOAD-TRUENAME* aren't
                  ;; pathnames during LOAD.  ANSI makes no exceptions here.
                  (*load-pathname* (handler-case (pathname stream)
                                     ;; FIXME: it should probably be a type
                                     ;; error to try to get a pathname for a
                                     ;; stream that doesn't have one, but I
                                     ;; don't know if we guarantee that.
                                     (error () nil)))
                  (*load-truename* (when *load-pathname*
                                     (handler-case (truename stream)
                                       (file-error () nil))))
                  ;; Bindings used internally.
                  (*load-depth* (1+ *load-depth*))
                  ;; KLUDGE: I can't find in the ANSI spec where it says
                  ;; that DECLAIM/PROCLAIM of optimization policy should
                  ;; have file scope. CMU CL did this, and it seems
                  ;; reasonable, but it might not be right; after all,
                  ;; things like (PROCLAIM '(TYPE ..)) don't have file
                  ;; scope, and I can't find anything under PROCLAIM or
                  ;; COMPILE-FILE or LOAD or OPTIMIZE which justifies this
                  ;; behavior. Hmm. -- WHN 2001-04-06
                  (sb!c::*policy* sb!c::*policy*))
             (return-from load
               (if faslp
                   (load-as-fasl stream verbose print)
                   (sb!c:with-compiler-error-resignalling
                     (load-as-source stream :verbose verbose
                                            :print print)))))))
    ;; Case 1: stream.
    (when (streamp pathspec)
      (return-from load (load-stream pathspec (fasl-header-p pathspec))))
    (let ((pathname (pathname pathspec)))
      ;; Case 2: Open as binary, try to process as a fasl.
      (with-open-stream
          (stream (or (open pathspec :element-type '(unsigned-byte 8)
                            :if-does-not-exist nil)
                      (when (null (pathname-type pathspec))
                        (let ((defaulted-pathname
                               (probe-load-defaults pathspec)))
                          (if defaulted-pathname
                              (progn (setq pathname defaulted-pathname)
                                     (open pathname
                                           :if-does-not-exist
                                           (if if-does-not-exist :error nil)
                                           :element-type '(unsigned-byte 8))))))
                      (if if-does-not-exist
                          (error 'simple-file-error
                                 :pathname pathspec
                                 :format-control
                                 "~@<Couldn't load ~S: file does not exist.~@:>"
                                 :format-arguments (list pathspec)))))
        (unless stream
          (return-from load nil))
        (let* ((real (probe-file stream))
               (should-be-fasl-p
                (and real (string-equal (pathname-type real) *fasl-file-type*))))
          ;; Don't allow empty .fasls, and assume other empty files
          ;; are source files.
          (when (and (or should-be-fasl-p (not (eql 0 (file-length stream))))
                     (fasl-header-p stream :errorp should-be-fasl-p))
            (return-from load (load-stream stream t)))))
      ;; Case 3: Open using the given external format, process as source.
      (with-open-file (stream pathname :external-format external-format
                              :class 'form-tracking-stream)
        (load-stream stream nil)))))

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
  (destructuring-bind (defaulted-source-pathname
                       defaulted-source-truename
                       defaulted-fasl-pathname
                       defaulted-fasl-truename)
      (loop for type in (list *load-source-default-type*
                              *fasl-file-type*)
            as probe-pathname = (make-pathname :type type
                                               :defaults pathname)
            collect probe-pathname
            collect (handler-case (probe-file probe-pathname)
                      (file-error () nil)))
    (cond ((and defaulted-fasl-truename
                defaulted-source-truename
                (> (file-write-date defaulted-source-truename)
                   (file-write-date defaulted-fasl-truename)))
           (restart-case
               (error "The object file ~A is~@
                       older than the presumed source:~%  ~A."
                      defaulted-fasl-truename
                      defaulted-source-truename)
             (source () :report "load source file"
                     defaulted-source-pathname)
             (object () :report "load object file"
                     defaulted-fasl-pathname)))
          (defaulted-fasl-truename defaulted-fasl-pathname)
          (defaulted-source-truename defaulted-source-pathname))))

;;; Load a code object. BOX-NUM objects are popped off the stack for
;;; the boxed storage section, then CODE-LENGTH bytes of code are read in.
(defun load-code (box-num code-length stack ptr input-stream)
  (declare (fixnum box-num code-length))
  (declare (simple-vector stack) (type index ptr))
  (let ((code (sb!c:allocate-code-object box-num code-length)))
    (setf (%code-debug-info code) (svref stack (+ ptr box-num)))
    (loop for i of-type index from sb!vm:code-constants-offset
          for j of-type index from ptr below (+ ptr box-num)
          do (setf (code-header-ref code i) (svref stack j)))
    (without-gcing
      (read-n-bytes input-stream (code-instructions code) 0 code-length))
    code))

;;;; linkage fixups

;;; how we learn about assembler routines at startup
(defvar *!initial-assembler-routines*)

(defun !loader-cold-init ()
  (/show0 "/!loader-cold-init")
  (dolist (routine *!initial-assembler-routines*)
    (setf (gethash (car routine) *assembler-routines*) (cdr routine))))
