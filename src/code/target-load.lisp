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

;;; Load a text file.  (Note that load-as-fasl is in another file.)
(defun load-as-source (stream verbose print)
  (maybe-announce-load stream verbose)
  (do ((sexpr (read stream nil *eof-object*)
              (read stream nil *eof-object*)))
      ((eq sexpr *eof-object*)
       t)
    (if print
        (let ((results (multiple-value-list (eval sexpr))))
          (load-fresh-line)
          (format t "~{~S~^, ~}~%" results))
      (eval sexpr))))

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

;; Pretty well any way of doing LOAD will expose race conditions: for
;; example, a file might get deleted or renamed after we open it but
;; before we find its truename.  It seems useful to say that
;; detectible ways the file system can fail to be static are good
;; enough reason to stop loading, but to stop in a way that
;; distinguishes errors that occur mid-way through LOAD from the
;; initial failure to OPEN the file, so that handlers can try do
;; defaulting only when the file didn't exist at the start of LOAD,
;; while allowing race conditions to get through.
(define-condition load-race-condition (error)
  ((pathname :reader load-race-condition-pathname :initarg :pathname))
  (:report (lambda (condition stream)
             (format stream "~@<File~S was deleted or renamed during LOAD.~:>"
                     (load-race-condition-pathname condition)))))

(defmacro resignal-race-condition (&body body)
  `(handler-case (progn ,@body)
     (file-error (error)
       (error 'load-race-condition :pathname (file-error-pathname error)))))

;;; The following comment preceded the pre 1.0.12.36 definition of
;;; LOAD; it may no longer be accurate:

;; FIXME: Daniel Barlow's ilsb.tar ILISP-for-SBCL patches contain an
;; implementation of "DEFUN SOURCE-FILE" which claims, in a comment,
;; that CMU CL does not correctly record source file information when
;; LOADing a non-compiled file. Check whether this bug exists in SBCL
;; and fix it if so.

;;; This is our real LOAD.  The LOAD below is just a wrapper that does
;;; some defaulting in case the user asks us to load a file that
;;; doesn't exist at the time we start.
(defun %load (pathspec &key (verbose *load-verbose*) (print *load-print*)
             (if-does-not-exist t) (external-format :default))
  (when (streamp pathspec)
    (let* (;; Bindings required by ANSI.
           (*readtable* *readtable*)
           (*package* (sane-package))
           ;; FIXME: we should probably document the circumstances
           ;; where *LOAD-PATHNAME* and *LOAD-TRUENAME* aren't
           ;; pathnames during LOAD.  ANSI makes no exceptions here.
           (*load-pathname* (handler-case (pathname pathspec)
                              ;; FIXME: it should probably be a type
                              ;; error to try to get a pathname for a
                              ;; stream that doesn't have one, but I
                              ;; don't know if we guarantee that.
                              (error () nil)))
           (*load-truename* (when *load-pathname*
                              (handler-case (truename *load-pathname*)
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
      (return-from %load
        (if (equal (stream-element-type pathspec) '(unsigned-byte 8))
            (load-as-fasl pathspec verbose print)
            (load-as-source pathspec verbose print)))))
  ;; If we're here, PATHSPEC isn't a stream, so must be some other
  ;; kind of pathname designator.
  (with-open-file (stream pathspec
                          :element-type '(unsigned-byte 8)
                          :if-does-not-exist
                          (if if-does-not-exist :error nil))
    (unless stream
      (return-from %load nil))
    (let* ((header-line (make-array
                         (length *fasl-header-string-start-string*)
                         :element-type '(unsigned-byte 8))))
      (read-sequence header-line stream)
      (if (mismatch header-line *fasl-header-string-start-string*
                    :test #'(lambda (code char) (= code (char-code char))))
          (let ((truename (resignal-race-condition (probe-file stream))))
            (when (and truename
                       (string= (pathname-type truename) *fasl-file-type*))
              (error 'fasl-header-missing
                     :stream (namestring truename)
                     :fhsss header-line
                     :expected *fasl-header-string-start-string*)))
          (progn
            (file-position stream :start)
            (return-from %load
              (%load stream :verbose verbose :print print))))))
  (resignal-race-condition
    (with-open-file (stream pathspec
                            :external-format
                            external-format)
      (%load stream :verbose verbose :print print))))

;; Given a simple %LOAD like the above, one can implement any
;; particular defaulting strategy with a wrapper like this one:
(defun load (pathspec &key (verbose *load-verbose*) (print *load-print*)
            (if-does-not-exist :error) (external-format :default))
  #!+sb-doc
  "Load the file given by FILESPEC into the Lisp environment, returning
   T on success."
  (handler-bind ((file-error
                  #'(lambda (error)
                      ;; This handler will run if %LOAD failed to OPEN
                      ;; the file to look for a fasl header.
                      (let ((pathname (file-error-pathname error)))
                        ;; As PROBE-FILE returned NIL, the file
                        ;; doesn't exist.  If the filename we tried to
                        ;; open lacked a type, try loading a filename
                        ;; determined by our defaulting.
                        (when (null (handler-case (probe-file pathname)
                                      (file-error (error) error)))
                          (when (null (pathname-type pathname))
                            (let ((default (probe-load-defaults pathname)))
                              (when default
                                (return-from load
                                  (resignal-race-condition
                                    (%load default
                                           :verbose verbose
                                           :print print
                                           :external-format
                                           external-format
                                           :if-does-not-exist
                                           if-does-not-exist))))))))
                      ;; If we're here, one of three things happened:
                      ;; (1) %LOAD errored and PROBE-FILE succeeded,
                      ;; in which case the file must be a bad symlink,
                      ;; unreadable, or it was created between %LOAD
                      ;; and PROBE-FILE; (2) %LOAD errored and
                      ;; PROBE-FILE errored, and so things are amiss
                      ;; in the file system (albeit possibly
                      ;; differently now than when OPEN errored); (3)
                      ;; our defaulting did not find a file.  In any
                      ;; of these cases, decline to handle the
                      ;; original error or return NIL, depending on
                      ;; IF-DOES-NOT-EXIST.
                      (if if-does-not-exist
                          nil
                          (return-from load nil)))))
    (%load pathspec :verbose verbose :print print
           :external-format external-format)))

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
                (> (resignal-race-condition
                     (file-write-date defaulted-source-truename))
                   (resignal-race-condition
                     (file-write-date defaulted-fasl-truename))))
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
;;; the boxed storage section, then SIZE bytes of code are read in.
#!-x86
(defun load-code (box-num code-length)
  (declare (fixnum box-num code-length))
  (with-fop-stack t
    (let ((code (%primitive sb!c:allocate-code-object box-num code-length))
          (index (+ sb!vm:code-trace-table-offset-slot box-num)))
      (declare (type index index))
      (setf (%code-debug-info code) (pop-stack))
      (dotimes (i box-num)
        (declare (fixnum i))
        (setf (code-header-ref code (decf index)) (pop-stack)))
      (sb!sys:without-gcing
        (read-n-bytes *fasl-input-stream*
                      (code-instructions code)
                      0
                      code-length))
      code)))

;;; Moving native code during a GC or purify is not so trivial on the
;;; x86 port.
;;;
;;; Our strategy for allowing the loading of x86 native code into the
;;; dynamic heap requires that the addresses of fixups be saved for
;;; all these code objects. After a purify these fixups can be
;;; dropped. In CMU CL, this policy was enabled with
;;; *ENABLE-DYNAMIC-SPACE-CODE*; in SBCL it's always used.
#!+x86
(defun load-code (box-num code-length)
  (declare (fixnum box-num code-length))
  (with-fop-stack t
    (let ((stuff (list (pop-stack))))
      (dotimes (i box-num)
        (declare (fixnum i))
        (push (pop-stack) stuff))
      (let* ((dbi (car (last stuff)))   ; debug-info
             (tto (first stuff)))       ; trace-table-offset

        (setq stuff (nreverse stuff))

        ;; FIXME: *LOAD-CODE-VERBOSE* should probably be #!+SB-SHOW.
        (when *load-code-verbose*
              (format t "stuff: ~S~%" stuff)
              (format t
                      "   : ~S ~S ~S ~S~%"
                      (sb!c::compiled-debug-info-p dbi)
                      (sb!c::debug-info-p dbi)
                      (sb!c::compiled-debug-info-name dbi)
                      tto)
              (format t "   loading to the dynamic space~%"))

        (let ((code (%primitive sb!c:allocate-code-object
                                box-num
                                code-length))
              (index (+ sb!vm:code-trace-table-offset-slot box-num)))
          (declare (type index index))
          (when *load-code-verbose*
            (format t
                    "  obj addr=~X~%"
                    (sb!kernel::get-lisp-obj-address code)))
          (setf (%code-debug-info code) (pop stuff))
          (dotimes (i box-num)
            (declare (fixnum i))
            (setf (code-header-ref code (decf index)) (pop stuff)))
          (sb!sys:without-gcing
           (read-n-bytes *fasl-input-stream*
                         (code-instructions code)
                         0
                         code-length))
          code)))))

;;;; linkage fixups

;;; how we learn about assembler routines at startup
(defvar *!initial-assembler-routines*)

(defun !loader-cold-init ()
  (/show0 "/!loader-cold-init")
  (dolist (routine *!initial-assembler-routines*)
    (setf (gethash (car routine) *assembler-routines*) (cdr routine))))
