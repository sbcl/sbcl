;;;; stuff which is not specific to any particular build phase, but
;;;; used by most of them
;;;;
;;;; Note: It's specifically not used when bootstrapping PCL, because
;;;; we do SAVE-LISP after that, and we don't want to save extraneous
;;;; bootstrapping machinery into the frozen image which will
;;;; subsequently be used as the mother of all Lisp sessions.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;; GC tuning has little effect on the x86 due to the generational
;;; collector.  For the older stop & copy collector, it assuredly
;;; does.  GC time is proportional to the amount of non-garbage
;;; needing collection and copying; when the application involved is
;;; the SBCL compiler, it doesn't take any longer to collect 20 Mb than
;;; to collect 2 Mb. -dan, 20000819
;;;
;;; Actually, tweaking *BYTES-CONSED-BETWEEN-GCS* to 20Mb instead of
;;; the default 2 seemed to make SBCL rebuild O(25%) faster on my 256
;;; Mb K6/3, so I think it does have some effect on X86/GENCGC. I
;;; haven't looked into why this would be, though. Also, I'm afraid
;;; that using 20Mb here might be unfriendly to people using more-reasonable
;;; machines (like old laptops with 48Mb of memory..) so I've
;;; suppressed this tweak except for Alpha. -- WHN 2001-05-11
#+(and sbcl alpha) ; SBCL/Alpha uses stop-and-copy, and Alphas have lotso RAM.
(progn
  (sb-ext:gc-off)
  (setf (sb-ext:bytes-consed-between-gcs) (* 20 (expt 10 6)))
  (sb-ext:gc-on)
  (sb-ext:gc))

;;; SB-COLD holds stuff used to build the initial SBCL core file
;;; (including not only the final construction of the core file, but
;;; also the preliminary steps like e.g. building the cross-compiler
;;; and running the cross-compiler to produce target FASL files).
(defpackage "SB-COLD" (:use "CL"))

(in-package "SB-COLD")

;;; prefixes for filename stems when cross-compiling. These are quite arbitrary
;;; (although of course they shouldn't collide with anything we don't want to
;;; write over). In particular, they can be either relative path names (e.g.
;;; "host-objects/" or absolute pathnames (e.g. "/tmp/sbcl-xc-host-objects/").
;;;
;;; The cross-compilation process will force the creation of these directories
;;; by executing CL:ENSURE-DIRECTORIES-EXIST (on the xc host Common Lisp).
(defvar *host-obj-prefix*)
(defvar *target-obj-prefix*)

;;; suffixes for filename stems when cross-compiling
(defvar *host-obj-suffix* 
  (or
   ;; On some xc hosts, it's impossible to LOAD a fasl file unless it
   ;; has the same extension that the host uses for COMPILE-FILE
   ;; output, so we have to be careful to use the xc host's preferred
   ;; extension.
   ;;
   ;; FIXME: This is a little ugly and annoying to maintain. And
   ;; there's very likely some way to rearrange the build process so
   ;; that we never explicitly refer to host object file suffixes,
   ;; only to the result of CL:COMPILE-FILE-PATHNAME.
   #+lispworks ".ufsl" ; as per Lieven Marchand sbcl-devel 2002-02-01
   ;; On most xc hosts, any old extension works, so we use an
   ;; arbitrary one.
   ".lisp-obj"))
(defvar *target-obj-suffix*
  ;; Target fasl files are LOADed (actually only quasi-LOADed, in
  ;; GENESIS) only by SBCL code, and it doesn't care about particular
  ;; extensions, so we can use something arbitrary.
  ".lisp-obj")

;;; a function of one functional argument, which calls its functional argument
;;; in an environment suitable for compiling the target. (This environment
;;; includes e.g. a suitable *FEATURES* value.)
(defvar *in-target-compilation-mode-fn*)

;;; designator for a function with the same calling convention as
;;; CL:COMPILE-FILE, to be used to translate ordinary Lisp source files into
;;; target object files
(defvar *target-compile-file*)

;;; designator for a function with the same calling convention as
;;; SB-C:ASSEMBLE-FILE, to be used to translate assembly files into target
;;; object files
(defvar *target-assemble-file*)

;;;; some tools

;;; Take the file named X and make it into a file named Y. Sorta like
;;; UNIX, and unlike Common Lisp's bare RENAME-FILE, we don't allow
;;; information from the original filename to influence the final
;;; filename. (The reason that it's only sorta like UNIX is that in
;;; UNIX "mv foo bar/" will work, but the analogous
;;; (RENAME-FILE-A-LA-UNIX "foo" "bar/") should fail.)
;;;
;;; (This is a workaround for the weird behavior of Debian CMU CL
;;; 2.4.6, where (RENAME-FILE "dir/x" "dir/y") tries to create a file
;;; called "dir/dir/y". If that behavior goes away, then we should be
;;; able to get rid of this function and use plain RENAME-FILE in the
;;; COMPILE-STEM function above. -- WHN 19990321
(defun rename-file-a-la-unix (x y)

  ;; CLISP signals an error when the target file exists, which
  ;; seems unjustified by the ANSI definition of RENAME-FILE.
  ;; Work around it.
  #+clisp (ignore-errors (delete-file y))

  (rename-file x
	       ;; (Note that the TRUENAME expression here is lifted
	       ;; from an example in the ANSI spec for TRUENAME.)
	       (with-open-file (stream y :direction :output)
		 (close stream)
		 ;; From the ANSI spec: "In this case, the file is
		 ;; closed when the truename is tried, so the truename
		 ;; information is reliable."
		 (truename stream))))
(compile 'rename-file-a-la-unix)

;;; a wrapper for compilation/assembly, used mostly to centralize
;;; the procedure for finding full filenames from "stems"
;;;
;;; Compile the source file whose basic name is STEM, using some
;;; standard-for-the-SBCL-build-process procedures to generate the
;;; full pathnames of source file and object file. Return the pathname
;;; of the object file for STEM. Several &KEY arguments are accepted:
;;;   :SRC-PREFIX, :SRC-SUFFIX =
;;;      strings to be concatenated to STEM to produce source filename
;;;   :OBJ-PREFIX, :OBJ-SUFFIX =
;;;      strings to be concatenated to STEM to produce object filename
;;;   :TMP-OBJ-SUFFIX-SUFFIX =
;;;      string to be appended to the name of an object file to produce 
;;;      the name of a temporary object file
;;;   :COMPILE-FILE, :IGNORE-FAILURE-P =
;;;     :COMPILE-FILE is a function to use for compiling the file
;;;     (with the same calling conventions as ANSI CL:COMPILE-FILE).
;;;     If the third return value (FAILURE-P) of this function is
;;;     true, a continuable error will be signalled, unless
;;;     :IGNORE-FAILURE-P is set, in which case only a warning will be
;;;     signalled.
(defun compile-stem (stem
		     &key
		     (obj-prefix "")
		     (obj-suffix (error "missing OBJ-SUFFIX"))
		     (tmp-obj-suffix-suffix "-tmp")
		     (src-prefix "")
		     (src-suffix ".lisp")
		     (compile-file #'compile-file)
		     ignore-failure-p)

  (let* (;; KLUDGE: Note that this CONCATENATE 'STRING stuff is not The Common
	 ;; Lisp Way, although it works just fine for common UNIX environments.
	 ;; Should it come to pass that the system is ported to environments
	 ;; where version numbers and so forth become an issue, it might become
	 ;; urgent to rewrite this using the fancy Common Lisp PATHNAME
	 ;; machinery instead of just using strings. In the absence of such a
	 ;; port, it might or might be a good idea to do the rewrite.
	 ;; -- WHN 19990815
	 (src (concatenate 'string src-prefix stem src-suffix))
	 (obj (concatenate 'string obj-prefix stem obj-suffix))
	 (tmp-obj (concatenate 'string obj tmp-obj-suffix-suffix)))

    (ensure-directories-exist obj :verbose t)

    ;; We're about to set about building a new object file. First, we
    ;; delete any preexisting object file in order to avoid confusing
    ;; ourselves later should we happen to bail out of compilation
    ;; with an error.
    (when (probe-file obj)
      (delete-file obj))

    ;; Work around a bug in CLISP 1999-01-08 #'COMPILE-FILE: CLISP
    ;; mangles relative pathnames passed as :OUTPUT-FILE arguments,
    ;; but works OK with absolute pathnames.
    #+clisp
    (setf tmp-obj
	  ;; (Note that this idiom is taken from the ANSI
	  ;; documentation for TRUENAME.)
	  (with-open-file (stream tmp-obj :direction :output)
	    (close stream)
	    (truename stream)))

    ;; Try to use the compiler to generate a new temporary object file.
    (multiple-value-bind (output-truename warnings-p failure-p)
	(funcall compile-file src :output-file tmp-obj)
      (declare (ignore warnings-p))
      (cond ((not output-truename)
	     (error "couldn't compile ~S" src))
	    (failure-p
	     (if ignore-failure-p
		 (warn "ignoring FAILURE-P return value from compilation of ~S"
		       src)
		 (unwind-protect
		     (progn
		       ;; FIXME: This should have another option,
		       ;; redoing compilation.
		       (cerror "Continue, using possibly-bogus ~S."
			       "FAILURE-P was set when creating ~S."
			       obj)
		       (setf failure-p nil))
		   ;; Don't leave failed object files lying around.
		   (when (and failure-p (probe-file tmp-obj))
		     (delete-file tmp-obj)
		     (format t "~&deleted ~S~%" tmp-obj)))))
	    ;; Otherwise: success, just fall through.
	    (t nil)))

    ;; If we get to here, compilation succeeded, so it's OK to rename
    ;; the temporary output file to the permanent object file.
    (rename-file-a-la-unix tmp-obj obj)

    ;; nice friendly traditional return value
    (pathname obj)))
(compile 'compile-stem)

;;; other miscellaneous tools
(load "src/cold/read-from-file.lisp")
(load "src/cold/rename-package-carefully.lisp")
(load "src/cold/with-stuff.lisp")

;;; Try to minimize/conceal any non-standardness of the host Common Lisp.
(load "src/cold/ansify.lisp")

;;;; special read-macros for building the cold system (and even for
;;;; building some of our tools for building the cold system)

(load "src/cold/shebang.lisp")

;;; When cross-compiling, the *FEATURES* set for the target Lisp is
;;; not in general the same as the *FEATURES* set for the host Lisp.
;;; In order to refer to target features specifically, we refer to
;;; *SHEBANG-FEATURES* instead of *FEATURES*, and use the #!+ and #!-
;;; readmacros instead of the ordinary #+ and #- readmacros.
(setf *shebang-features*
      (let* ((default-features
	       (append (read-from-file "base-target-features.lisp-expr")
		       (read-from-file "local-target-features.lisp-expr")))
	     (customizer-file-name "customize-target-features.lisp")
	     (customizer (if (probe-file customizer-file-name)
			     (compile nil 
				      (read-from-file customizer-file-name))
			     #'identity)))
	(funcall customizer default-features)))
(let ((*print-length* nil)
      (*print-level* nil))
  (format t
	  "target features *SHEBANG-FEATURES*=~@<~S~:>~%"
	  *shebang-features*))

;;;; cold-init-related PACKAGE and SYMBOL tools

;;; Once we're done with possibly ANSIfying the COMMON-LISP package,
;;; it's probably a mistake if we change it (beyond changing the
;;; values of special variables such as *** and +, anyway). Set up
;;; machinery to warn us when/if we change it.
;;;
;;; FIXME: All this machinery should probably be conditional on
;;; #!+SB-SHOW, i.e. we should be able to wrap #!+SB-SHOW around both
;;; the LOAD and the DEFVAR here. 
(load "src/cold/snapshot.lisp")
(defvar *cl-snapshot* (take-snapshot "COMMON-LISP"))

;;;; master list of source files and their properties

;;; flags which can be used to describe properties of source files
(defparameter
  *expected-stem-flags*
  '(;; meaning: This file is not to be compiled when building the
    ;; cross-compiler which runs on the host ANSI Lisp.
    :not-host
    ;; meaning: This file is not to be compiled as part of the target
    ;; SBCL.
    :not-target
    ;; meaning: This file is to be processed with the SBCL assembler,
    ;; not COMPILE-FILE. (Note that this doesn't make sense unless
    ;; :NOT-HOST is also set, since the SBCL assembler doesn't exist
    ;; while the cross-compiler is being built in the host ANSI Lisp.)
    :assem
    ;; meaning: The #'COMPILE-STEM argument called :IGNORE-FAILURE-P
    ;; should be true. (This is a KLUDGE: I'd like to get rid of it.
    ;; For now, it exists so that compilation can proceed through the
    ;; legacy warnings in src/compiler/x86/array.lisp, which I've
    ;; never figured out but which were apparently acceptable in CMU
    ;; CL. Eventually, it would be great to just get rid of all
    ;; warnings and remove support for this flag. -- WHN 19990323)
    :ignore-failure-p))

(defparameter *stems-and-flags* (read-from-file "build-order.lisp-expr"))

(defmacro do-stems-and-flags ((stem flags) &body body)
  (let ((stem-and-flags (gensym "STEM-AND-FLAGS-")))
    `(dolist (,stem-and-flags *stems-and-flags*)
       (let ((,stem (first ,stem-and-flags))
	     (,flags (rest ,stem-and-flags)))
	 ,@body))))

;;; Check for stupid typos in FLAGS list keywords.
(let ((stems (make-hash-table :test 'equal)))
  (do-stems-and-flags (stem flags)
    (if (gethash stem stems)
      (error "duplicate stem ~S in *STEMS-AND-FLAGS*" stem)
      (setf (gethash stem stems) t))
    (let ((set-difference (set-difference flags *expected-stem-flags*)))
      (when set-difference
	(error "found unexpected flag(s) in *STEMS-AND-FLAGS*: ~S"
	       set-difference)))))

;;;; tools to compile SBCL sources to create the cross-compiler

;;; Execute function FN in an environment appropriate for compiling the
;;; cross-compiler's source code in the cross-compilation host.
(defun in-host-compilation-mode (fn)
  (let ((*features* (cons :sb-xc-host *features*))
	;; the CROSS-FLOAT-INFINITY-KLUDGE, as documented in
	;; base-target-features.lisp-expr:
	(*shebang-features* (set-difference *shebang-features*
					    '(:sb-propagate-float-type
					      :sb-propagate-fun-type))))
    (with-additional-nickname ("SB-XC" "SB!XC")
      (funcall fn))))
(compile 'in-host-compilation-mode)

;;; Process a file as source code for the cross-compiler, compiling it
;;; (if necessary) in the appropriate environment, then loading it
;;; into the cross-compilation host Common lisp.
(defun host-cload-stem (stem &key ignore-failure-p)
  (let ((compiled-filename (in-host-compilation-mode
			    (lambda ()
			      (compile-stem
			       stem
			       :obj-prefix *host-obj-prefix*
			       :obj-suffix *host-obj-suffix*
			       :compile-file #'cl:compile-file
			       :ignore-failure-p ignore-failure-p)))))
    (load compiled-filename)))
(compile 'host-cload-stem)

;;; like HOST-CLOAD-STEM, except that we don't bother to compile
(defun host-load-stem (stem &key ignore-failure-p)
  (declare (ignore ignore-failure-p)) ; (It's only relevant when
  ;; compiling.) KLUDGE: It's untidy to have the knowledge of how to
  ;; construct complete filenames from stems in here as well as in
  ;; COMPILE-STEM. It should probably be factored out somehow. -- WHN
  ;; 19990815
  (load (concatenate 'simple-string *host-obj-prefix* stem *host-obj-suffix*)))
(compile 'host-load-stem)

;;;; tools to compile SBCL sources to create object files which will
;;;; be used to create the target SBCL .core file

;;; Run the cross-compiler on a file in the source directory tree to
;;; produce a corresponding file in the target object directory tree.
(defun target-compile-stem (stem &key assem-p ignore-failure-p)
  (funcall *in-target-compilation-mode-fn*
	   (lambda ()
	     (compile-stem stem
			   :obj-prefix *target-obj-prefix*
			   :obj-suffix *target-obj-suffix*
			   :ignore-failure-p ignore-failure-p
			   :compile-file (if assem-p
					     *target-assemble-file*
					     *target-compile-file*)))))
(compile 'target-compile-stem)

;;; (This function is not used by the build process, but is intended
;;; for interactive use when experimenting with the system. It runs
;;; the cross-compiler on test files with arbitrary filenames, not
;;; necessarily in the source tree, e.g. in "/tmp".)
(defun target-compile-file (filename)
  (funcall *in-target-compilation-mode-fn*
	   (lambda ()
	     (funcall *target-compile-file* filename))))
(compile 'target-compile-file)
