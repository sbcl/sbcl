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

;;; Load a text file.
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

;;; a helper function for LOAD: Load the stuff in a file when we have
;;; the name.
;;;
;;; FIXME: with the addition of the EXTERNAL-FORMAT argument, this
;;; interface has become truly sucky.
(defun internal-load (pathname truename if-does-not-exist verbose print
		      &optional contents external-format)
  (declare (type (member nil :error) if-does-not-exist))
  (unless truename
    (if if-does-not-exist
	(error 'simple-file-error
	       :pathname pathname
	       :format-control "~S does not exist."
	       :format-arguments (list (namestring pathname)))
	(return-from internal-load nil)))

  (let ((*load-truename* truename)
	(*load-pathname* (merge-pathnames pathname)))
    (case contents
      (:source
       (with-open-file (stream truename
			       :direction :input
			       :if-does-not-exist if-does-not-exist
                               :external-format external-format)
	 (load-as-source stream verbose print)))
      (:binary
       (with-open-file (stream truename
			       :direction :input
			       :if-does-not-exist if-does-not-exist
			       :element-type '(unsigned-byte 8))
	 (load-as-fasl stream verbose print)))
      (t
       (let* ((fhsss *fasl-header-string-start-string*)
	      (first-line (make-array (length fhsss)
				      :element-type '(unsigned-byte 8)))
	      (read-length
	       (with-open-file (stream truename
				       :direction :input
				       :element-type '(unsigned-byte 8))
		 (read-sequence first-line stream))))
	 (cond
	   ((and (= read-length (length fhsss))
		 (do ((i 0 (1+ i)))
		     ((= i read-length) t)
		   (when (/= (char-code (aref fhsss i)) (aref first-line i))
		     (return))))
            (internal-load pathname truename if-does-not-exist verbose print
                           :binary))
           (t
            (when (string= (pathname-type truename) *fasl-file-type*)
              (error 'fasl-header-missing
                     :stream (namestring truename)
                     :fhsss first-line
                     :expected fhsss))
            (internal-load pathname truename if-does-not-exist verbose print
                           :source external-format))))))))

;;; a helper function for INTERNAL-LOAD-DEFAULT-TYPE: Try the default
;;; file type TYPE and return (VALUES PATHNAME TRUENAME) for a match,
;;; or (VALUES PATHNAME NIL) if the file doesn't exist.
;;;
;;; This is analogous to CMU CL's TRY-DEFAULT-TYPES, but we only try a
;;; single type. By avoiding CMU CL's generality here, we avoid having
;;; to worry about some annoying ambiguities. (E.g. what if the
;;; possible types are ".lisp" and ".cl", and both "foo.lisp" and
;;; "foo.cl" exist?)
(defun try-default-type (pathname type)
  (let ((pn (translate-logical-pathname (make-pathname :type type :defaults pathname))))
    (values pn (probe-file pn))))

;;; a helper function for LOAD: Handle the case of INTERNAL-LOAD where
;;; the file does not exist.
(defun internal-load-default-type
    (pathname if-does-not-exist verbose print external-format)
  (declare (type (member nil :error) if-does-not-exist))
  (multiple-value-bind (src-pn src-tn)
      (try-default-type pathname *load-source-default-type*)
    (multiple-value-bind (obj-pn obj-tn)
	(try-default-type pathname *fasl-file-type*)
      (cond
       ((and obj-tn
	     src-tn
	     (> (file-write-date src-tn) (file-write-date obj-tn)))
	(restart-case
	 (error "The object file ~A is~@
                 older than the presumed source:~%  ~A."
		(namestring obj-tn)
		(namestring src-tn))
	 (source () :report "load source file"
	   (internal-load src-pn src-tn if-does-not-exist verbose print
			  :source external-format))
	 (object () :report "load object file"
	    (internal-load src-pn obj-tn if-does-not-exist verbose print
			   :binary))))
       (obj-tn
	(internal-load obj-pn obj-tn if-does-not-exist verbose print :binary))
       (src-pn
	(internal-load src-pn src-tn if-does-not-exist
                       verbose print :source external-format))
       (t
	(internal-load pathname nil if-does-not-exist
                       verbose print nil external-format))))))

;;; This function mainly sets up special bindings and then calls
;;; sub-functions. We conditionally bind the switches with PROGV so
;;; that people can set them in their init files and have the values
;;; take effect. If the compiler is loaded, we make the
;;; compiler-policy local to LOAD by binding it to itself.
;;;
;;; FIXME: Daniel Barlow's ilsb.tar ILISP-for-SBCL patches contain an
;;; implementation of "DEFUN SOURCE-FILE" which claims, in a comment, that CMU
;;; CL does not correctly record source file information when LOADing a
;;; non-compiled file. Check whether this bug exists in SBCL and fix it if so.
(defun load (filespec
	     &key
	     (verbose *load-verbose*)
	     (print *load-print*)
	     (if-does-not-exist t)
	     (external-format :default))
  #!+sb-doc
  "Load the file given by FILESPEC into the Lisp environment, returning
   T on success."
  (let ((*load-depth* (1+ *load-depth*))
	;; KLUDGE: I can't find in the ANSI spec where it says that
	;; DECLAIM/PROCLAIM of optimization policy should have file
	;; scope. CMU CL did this, and it seems reasonable, but it
	;; might not be right; after all, things like (PROCLAIM '(TYPE
	;; ..)) don't have file scope, and I can't find anything under
	;; PROCLAIM or COMPILE-FILE or LOAD or OPTIMIZE which
	;; justifies this behavior. Hmm. -- WHN 2001-04-06
	(sb!c::*policy* sb!c::*policy*)
	;; The ANSI spec for LOAD says "LOAD binds *READTABLE* and
	;; *PACKAGE* to the values they held before loading the file."
	(*package* (sane-package))
	(*readtable* *readtable*)
	;; The old CMU CL LOAD function used an IF-DOES-NOT-EXIST
	;; argument of (MEMBER :ERROR NIL) type. ANSI constrains us to
	;; accept a generalized boolean argument value for this
	;; externally-visible function, but the internal functions
	;; still use the old convention.
	(internal-if-does-not-exist (if if-does-not-exist :error nil)))
    ;; FIXME: This VALUES wrapper is inherited from CMU CL. Once SBCL
    ;; gets function return type checking right, we can achieve a
    ;; similar effect better by adding FTYPE declarations.
    (values
     (if (streamp filespec)
	 (if (or (equal (stream-element-type filespec)
			'(unsigned-byte 8)))
	     (load-as-fasl filespec verbose print)
	     (load-as-source filespec verbose print))
	 (let* ((pathname (pathname filespec))
		(physical-pathname (translate-logical-pathname pathname))
		(probed-file (probe-file physical-pathname)))
	   (if (or probed-file
		   (pathname-type physical-pathname))
	       (internal-load
                physical-pathname probed-file internal-if-does-not-exist
                verbose print nil external-format)
	       (internal-load-default-type
                pathname internal-if-does-not-exist
                verbose print external-format)))))))

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
      (let* ((dbi (car (last stuff)))	; debug-info
	     (tto (first stuff)))	; trace-table-offset

	(setq stuff (nreverse stuff))

	;; FIXME: *LOAD-CODE-VERBOSE* should probably be #!+SB-SHOW.
	(when *load-code-verbose*
	      (format t "stuff: ~S~%" stuff)
	      (format t
		      "   : ~S ~S ~S ~S~%"
		      (sb!c::compiler-debug-info-p dbi)
		      (sb!c::debug-info-p dbi)
		      (sb!c::compiler-debug-info-name dbi)
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
