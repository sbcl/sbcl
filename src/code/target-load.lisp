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
(defun load-as-source (stream verbose print)
  (maybe-announce-load stream verbose)
  (macrolet ((do-sexprs ((sexpr stream) &body body)
               (aver (symbolp sexpr))
               (with-unique-names (source-info)
                 (once-only ((stream stream))
                   `(if (handler-case (pathname stream)
                          (error () nil))
                        (let ((,source-info (sb!c::make-file-source-info
                                            (pathname ,stream)
                                            (stream-external-format ,stream))))
                          (setf (sb!c::source-info-stream ,source-info) ,stream)
                          (sb!c::do-forms-from-info ((,sexpr) ,source-info)
                            ,@body))
                        (do ((,sexpr (read ,stream nil *eof-object*)
                                     (read ,stream nil *eof-object*)))
                            ((eq ,sexpr *eof-object*))
                          ,@body))))))
    (do-sexprs (sexpr stream)
      (if print
          (let ((results (multiple-value-list (eval sexpr))))
            (load-fresh-line)
            (format t "~{~S~^, ~}~%" results))
          (eval sexpr)))
    t))

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
                   (load-as-source stream verbose print))))))
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
      ;; Case 3: Open using the gived external format, process as source.
      (with-open-file (stream pathname :external-format external-format)
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
;;; the boxed storage section, then SIZE bytes of code are read in.
#!-x86
(defun load-code (box-num code-length)
  (declare (fixnum box-num code-length))
  (with-fop-stack t
    (let ((code (sb!c:allocate-code-object box-num code-length))
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

        (let ((code (sb!c:allocate-code-object box-num code-length))
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
