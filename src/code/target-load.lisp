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

(in-package "SB!IMPL")

(defvar *load-source-default-type* "lisp"
  #!+sb-doc
  "The source file types which LOAD looks for by default.")

(defvar *load-truename* nil
  #!+sb-doc
  "the TRUENAME of the file that LOAD is currently loading")

(defvar *load-pathname* nil
  #!+sb-doc
  "the defaulted pathname that LOAD is currently loading")

(declaim (type (or pathname null) *load-truename* *load-pathname*))

;;;; SLOLOAD

;;; Load a text file.
(defun sloload (stream verbose print)
  (do-load-verbose stream verbose)
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

;;; a helper function for LOAD: Load the stuff in a file when we have the name.
(defun internal-load (pathname truename if-does-not-exist verbose print
		      &optional contents)
  (declare (type (member nil :error) if-does-not-exist))
  (unless truename
    (if if-does-not-exist
	(error 'simple-file-error
	       :pathname pathname
	       :format-control "~S does not exist."
	       :format-arguments (list (namestring pathname)))
	(return-from internal-load nil)))

  (let ((*load-truename* truename)
	(*load-pathname* pathname))
    (case contents
      (:source
       (with-open-file (stream truename
			       :direction :input
			       :if-does-not-exist if-does-not-exist)
	 (sloload stream verbose print)))
      (:binary
       (with-open-file (stream truename
			       :direction :input
			       :if-does-not-exist if-does-not-exist
			       :element-type '(unsigned-byte 8))
	 (fasload stream verbose print)))
      (t
       (let ((first-line (with-open-file (stream truename :direction :input)
			   (read-line stream nil)))
	     (fhs sb!c:*fasl-header-string-start-string*))
	 (cond
	  ((and first-line
		(>= (length (the simple-string first-line))
		    (length fhs))
		(string= first-line fhs :end1 (length fhs)))
	   (internal-load pathname truename if-does-not-exist verbose print
			  :binary))
	  (t
	   (when (string= (pathname-type truename)
			  sb!c:*backend-fasl-file-type*)
	     (error "File has a fasl file type, but no fasl file header:~%  ~S"
		    (namestring truename)))
	   (internal-load pathname truename if-does-not-exist verbose print
			  :source))))))))

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
  (let ((pn (make-pathname :type type :defaults pathname)))
    (values pn (probe-file pn))))

;;; a helper function for LOAD: Handle the case of INTERNAL-LOAD where
;;; the file does not exist.
(defun internal-load-default-type (pathname if-does-not-exist verbose print)
  (declare (type (member nil :error) if-does-not-exist))
  (multiple-value-bind (src-pn src-tn)
      (try-default-type pathname *load-source-default-type*)
    (multiple-value-bind (obj-pn obj-tn)
	(try-default-type pathname sb!c:*backend-fasl-file-type*)
      (cond
       ((and obj-tn
	     src-tn
	     (> (file-write-date src-tn) (file-write-date obj-tn)))
	(restart-case
	 (error "The object file ~A is~@
		older than the presumed source:~%  ~A."
		(namestring obj-tn)
		(namestring src-tn))
	 ;; FIXME: In CMU CL one of these was a CONTINUE case.
	 ;; There's not one now. I don't remember how restart-case
	 ;; works very well, make sure that it doesn't do anything
	 ;; weird when we don't specify the CONTINUE case.
	 (source () :report "load source file"
	   (internal-load src-pn src-tn if-does-not-exist verbose print
			  :source))
	 (object () :report "load object file"
	    (internal-load src-pn obj-tn if-does-not-exist verbose print
			   :binary))))
       (obj-tn
	(internal-load obj-pn obj-tn if-does-not-exist verbose print :binary))
       (src-pn
	(internal-load src-pn src-tn if-does-not-exist verbose print :source))
       (t
	(internal-load pathname nil if-does-not-exist verbose print nil))))))

;;; This function mainly sets up special bindings and then calls
;;; sub-functions. We conditionally bind the switches with PROGV so
;;; that people can set them in their init files and have the values
;;; take effect. If the compiler is loaded, we make the
;;; compiler-policy local to LOAD by binding it to itself.
;;;
;;; FIXME: ANSI specifies a &KEY :EXTERNAL-FORMAT argument.
;;;
;;; FIXME: Daniel Barlow's ilsb.tar ILISP-for-SBCL patches contain an
;;; implementation of "DEFUN SOURCE-FILE" which claims, in a comment, that CMU
;;; CL does not correctly record source file information when LOADing a
;;; non-compiled file. Check whether this bug exists in SBCL and fix it if so.
(defun load (filespec
	     &key
	     (verbose *load-verbose*)
	     (print *load-print*)
	     (if-does-not-exist t))
  #!+sb-doc
  "Loads the file given by FILESPEC into the Lisp environment, returning
   T on success. These options are defined:

   :IF-DOES-NOT-EXIST
       What should we do if the file can't be located? If true (the
       default), signal an error. If NIL, simply return NIL.

   :VERBOSE
       If true, print a line describing each file loaded. The default
       is *LOAD-VERBOSE*.

   :PRINT
       If true, print information about loaded values. When loading the
       source, the result of evaluating each top-level form is printed.
       The default is *LOAD-PRINT*."

  (let ((sb!c::*policy* sb!c::*policy*)
	(sb!c::*interface-policy* sb!c::*interface-policy*)
	(*package* (sane-package))
	(*readtable* *readtable*)
	(*load-depth* (1+ *load-depth*))
	;; The old CMU CL LOAD function used an IF-DOES-NOT-EXIST argument of
	;; (MEMBER :ERROR NIL) type. ANSI constrains us to accept a generalized
	;; boolean argument value for this externally-visible function, but the
	;; internal functions still use the old convention.
	(internal-if-does-not-exist (if if-does-not-exist :error nil)))
    ;; FIXME: This VALUES wrapper is inherited from CMU CL.
    ;; Once SBCL gets function return type checking right, we can
    ;; achieve a similar effect better by adding FTYPE declarations.
    (values
     (if (streamp filespec)
	 (if (or (equal (stream-element-type filespec)
			'(unsigned-byte 8)))
	     (fasload filespec verbose print)
	     (sloload filespec verbose print))
	 (let ((pn (merge-pathnames (pathname filespec)
				    *default-pathname-defaults*)))
	   (if (wild-pathname-p pn)
	       (let ((files (directory pn)))
		 #!+high-security
		 (when (null files)
		   (error 'file-error :pathname filespec))
		 (dolist (file files t)
		   (internal-load pn
				  file
				  internal-if-does-not-exist
				  verbose
				  print)))
	       (let ((tn (probe-file pn)))
		 (if (or tn (pathname-type pn))
		     (internal-load pn
				    tn
				    internal-if-does-not-exist
				    verbose
				    print)
		     (internal-load-default-type
		      pn
		      internal-if-does-not-exist
		      verbose
		      print)))))))))

;;; Load a code object. BOX-NUM objects are popped off the stack for
;;; the boxed storage section, then SIZE bytes of code are read in.
#!-x86
(defun load-code (box-num code-length)
  (declare (fixnum box-num code-length))
  (with-fop-stack t
    (let ((code (%primitive sb!c:allocate-code-object box-num code-length))
	  (index (+ #!-gengc sb!vm:code-trace-table-offset-slot
		    #!+gengc sb!vm:code-debug-info-slot
		    box-num)))
      (declare (type index index))
      #!-gengc (setf (%code-debug-info code) (pop-stack))
      (dotimes (i box-num)
	(declare (fixnum i))
	(setf (code-header-ref code (decf index)) (pop-stack)))
      (sb!sys:without-gcing
	(read-n-bytes *fasl-file*
		      (code-instructions code)
		      0
		      #!-gengc code-length
		      #!+gengc (* code-length sb!vm:word-bytes)))
      code)))

#!+x86
(defun load-code (box-num code-length)
  (declare (fixnum box-num code-length))
  (with-fop-stack t
    (let ((stuff (list (pop-stack))))
      (dotimes (i box-num)
	(declare (fixnum i))
	(push (pop-stack) stuff))
      (let* ((dbi (car (last stuff)))	; debug-info
	     (tto (first stuff))	; trace-table-offset
	     (load-to-dynamic-space
	      (or *enable-dynamic-space-code*
		  ;; definitely byte-compiled code?
		  (and *load-byte-compiled-code-to-dynamic-space*
		       (sb!c::debug-info-p dbi)
		       (not (sb!c::compiled-debug-info-p dbi)))
		  ;; or a x86 top level form?
		  (and *load-x86-tlf-to-dynamic-space*
		       (sb!c::compiled-debug-info-p dbi)
		       (string= (sb!c::compiled-debug-info-name dbi)
				"top-level form")))) )

	(setq stuff (nreverse stuff))

	;; Check that tto is always a list for byte-compiled
	;; code. Could be used an alternate check.
	(when (and (typep tto 'list)
		   (not (and (sb!c::debug-info-p dbi)
			     (not (sb!c::compiled-debug-info-p dbi)))))
	  ;; FIXME: What is this for?
	  (format t "* tto list on non-bc code: ~S~% ~S ~S~%"
		  stuff dbi tto))
	
	;; FIXME: *LOAD-CODE-VERBOSE* should probably be #!+SB-SHOW.
	(when *load-code-verbose*
	      (format t "stuff: ~S~%" stuff)
	      (format t
		      "   : ~S ~S ~S ~S~%"
		      (sb!c::compiled-debug-info-p dbi)
		      (sb!c::debug-info-p dbi)
		      (sb!c::compiled-debug-info-name dbi)
		      tto)
	      (if load-to-dynamic-space
		  (format t "   loading to the dynamic space~%")
		  (format t "   loading to the static space~%")))

	(let ((code
	       (if load-to-dynamic-space
		   (%primitive sb!c:allocate-dynamic-code-object
			       box-num
			       code-length)
		   (%primitive sb!c:allocate-code-object
			       box-num
			       code-length)))
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
	   (read-n-bytes *fasl-file* (code-instructions code) 0 code-length))
	  code)))))

;;;; linkage fixups

;;; how we learn about assembler routines and foreign symbols at startup
(defvar *!initial-assembler-routines*)
(defvar *!initial-foreign-symbols*)
(defun !loader-cold-init ()
  (dolist (routine *!initial-assembler-routines*)
    (setf (gethash (car routine) *assembler-routines*) (cdr routine)))
  (dolist (symbol *!initial-foreign-symbols*)
    (setf (gethash (car symbol) *static-foreign-symbols*) (cdr symbol))))

(declaim (ftype (function (string) sb!vm:word) foreign-symbol-address-as-integer))
(defun foreign-symbol-address-as-integer (foreign-symbol)
  (or (gethash foreign-symbol *static-foreign-symbols*)
      (gethash (concatenate 'simple-string
			    #!+linux "ldso_stub__"
			    #!+openbsd "_"
			    #!+freebsd "ldso_stub__"
			    foreign-symbol)
	       *static-foreign-symbols*)
      (sb!sys:get-dynamic-foreign-symbol-address foreign-symbol)
      (error "unknown foreign symbol: ~S" foreign-symbol)))

(defun foreign-symbol-address (symbol)
  (int-sap (foreign-symbol-address-as-integer (sb!vm:extern-alien-name symbol))))
