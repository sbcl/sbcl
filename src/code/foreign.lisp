;;;; support for dynamically loading foreign object files and
;;;; resolving symbols therein

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-ALIEN") ; (SB-ALIEN, not SB!ALIEN, since we're in warm load.)

;;; SEMI-KLUDGE: Preferable would be to use something like O_NOFOLLOW
;;; which will refuse to open() a file if it is a symlink; but I've
;;; been told that is a FreeBSD/Linux-only thing.  Meanwhile, this will
;;; make our filenames a lot less predictable.
;;; (The man file for open() says O_EXCL should treat even a symlink as
;;; an existing file.  I wonder if it really does that.)
;;; Also, no more dependence on ASCII character ordering.
;;; -- mrd 20021101
(defun generate-random-string (&optional (len 6))
  (let* ((characters "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
	 (num (length characters))
	 (string (make-string len)))
    (dotimes (i len string)
      (setf (char string i)
	    (char characters (random num))))))

(defun pick-temporary-file-name (&optional
				 (base "/tmp/sbcl-tmp-~D~A"))
  (let ((code (generate-random-string)))
    (loop
      (let ((name (format nil base (sb-unix:unix-getpid) code)))
	(multiple-value-bind (fd errno)
	    (sb-unix:unix-open name
			       (logior sb-unix:o_wronly
				       sb-unix:o_creat
				       sb-unix:o_excl)
			       #o666)
	  (cond ((not (null fd))
		 (sb-unix:unix-close fd)
		 (return name))
		((not (= errno sb-unix:eexist))
		 (simple-file-perror "couldn't create temporary file ~S"
				     name
				     errno))
		(t
		 (setf code (generate-random-string)))))))))

;;; On any OS where we don't support foreign object file loading, any
;;; query of a foreign symbol value is answered with "no definition
;;; known", i.e. NIL.
#-(or linux sunos FreeBSD OpenBSD)
(defun get-dynamic-foreign-symbol-address (symbol)
  (declare (type simple-string symbol) (ignore symbol))
  nil)

;;; dlsym()-based implementation of GET-DYNAMIC-FOREIGN-SYMBOL-ADDRESS
;;; and functions (e.g. LOAD-FOREIGN) which affect it.  This should 
;;; work on any ELF system with dlopen(3) and dlsym(3)
;;; It also works on OpenBSD, which isn't ELF, but is otherwise modern
;;; enough to have a fairly well working dlopen/dlsym implementation.
#-(or linux sunos FreeBSD OpenBSD)
(macrolet ((define-unsupported-fun (fun-name)
	     `(defun ,fun-name (&rest rest)
		"unsupported on this system"
                (declare (ignore rest))
		(error 'unsupported-operator :name ',fun-name))))
  (define-unsupported-fun load-1-foreign)
  (define-unsupported-fun load-foreign))
#+(or linux sunos FreeBSD OpenBSD)
(progn

;;; flags for dlopen()
(defconstant rtld-lazy 1)		; lazy function call binding?
(defconstant rtld-now 2)		; immediate function call binding?
(defconstant rtld-global #x100)		; symbols of loaded obj file
					; (and its dependencies) made
					; visible (as though the
					; obj file were linked directly
					; into the program)?

;;; a list of handles returned from dlopen(3) (or possibly some
;;; bogus value temporarily during initialization)
(defvar *handles-from-dlopen* nil)

;;; Dynamically loaded stuff isn't there upon restoring from a save.
;;; Clearing the variable this way was originally done primarily for
;;; Irix, which resolves tzname at runtime, resulting in
;;; *HANDLES-FROM-DLOPEN* (which was then called *TABLES-FROM-DLOPEN*)
;;; being set in the saved core image, resulting in havoc upon
;;; restart; but it seems harmless and tidy for other OSes too.
;;;
;;; Of course, it can be inconvenient that dynamically loaded stuff
;;; goes away when we save and restore. However,
;;;  (1) trying to avoid it by system programming here could open a
;;;      huge can of worms, since e.g. now we would need to worry about
;;;      libraries possibly being in different locations (file locations
;;;      or memory locations) at restore time than at save time; and
;;;  (2) by the time the application programmer is so deep into the
;;;      the use of hard core extension features as to be doing
;;;      dynamic loading of foreign files and saving/restoring cores,
;;;      he probably has the sophistication to write his own after-save
;;;      code to reload the libraries without much difficulty.

;;; dan 2001.05.10 suspects that objection (1) is bogus for
;;; dlsym()-enabled systems

(push (lambda () (setq *handles-from-dlopen* nil))
      *after-save-initializations*)

(defvar *dso-linker* "/usr/bin/ld")
(defvar *dso-linker-options* '("-shared" "-o"))

(sb-alien:define-alien-routine dlopen system-area-pointer
  (file sb-alien:c-string) (mode sb-alien:int))
(sb-alien:define-alien-routine dlsym system-area-pointer
  (lib system-area-pointer)
  (name sb-alien:c-string))
(sb-alien:define-alien-routine dlerror sb-alien:c-string)

;;; Ensure that we've opened our own binary so we can dynamically resolve 
;;; symbols in the C runtime.  
;;;
;;; Old comment: This used to happen only in
;;; GET-DYNAMIC-FOREIGN-SYMBOL-ADDRESS, and only if no libraries were
;;; dlopen()ed already, but that didn't work if something was
;;; dlopen()ed before any problem global vars were used.  So now we do
;;; this in any function that can add to the *HANDLES-FROM-DLOPEN*, as
;;; well as in GET-DYNAMIC-FOREIGN-SYMBOL-ADDRESS.
;;;
;;; FIXME: It would work just as well to do it once at startup, actually.
;;; Then at least we know it's done.    -dan 2001.05.10
(defun ensure-runtime-symbol-table-opened ()
  (unless *handles-from-dlopen*
    ;; Prevent recursive call if dlopen() isn't defined.
    (setf *handles-from-dlopen* (int-sap 0))
    (setf *handles-from-dlopen* (list (dlopen nil rtld-lazy)))
    (when (zerop (sb-sys:sap-int (first *handles-from-dlopen*)))
      (error "can't open our own binary's symbol table: ~S" (dlerror)))))

(defun load-1-foreign (file)
  "the primitive upon which the more general LOAD-FOREIGN is built: load
  a single foreign object file

  To use LOAD-1-FOREIGN, at the Unix command line do this:
    echo 'int summish(int x, int y) { return 1 + x + y; }' > /tmp/ffi-test.c
    make /tmp/ffi-test.o # i.e. cc -c -o /tmp/ffi-test.o /tmp/ffi-test.c
    ld -shared -o /tmp/ffi-test.so /tmp/ffi-test.o
  then in SBCL do this:
    (LOAD-1-FOREIGN \"/tmp/ffi-test.so\")
    (DEFINE-ALIEN-ROUTINE SUMMISH INT (X INT) (Y INT))
  Now running (SUMMISH 10 20) should return 31.
"
  (ensure-runtime-symbol-table-opened)
  ;; Note: We use RTLD-GLOBAL so that it can find all the symbols
  ;; previously loaded. We use RTLD-NOW so that dlopen() will fail if
  ;; not all symbols are defined.
  (let* ((real-file (or (unix-namestring file) file))
         (sap (dlopen real-file (logior rtld-now rtld-global))))
       (if (zerop (sap-int sap))
	   (error "can't open object ~S: ~S" real-file (dlerror))
	   (pushnew sap *handles-from-dlopen* :test #'sap=)))
  (values))

(defun get-dynamic-foreign-symbol-address (symbol)
  (ensure-runtime-symbol-table-opened)
  ;; Find the symbol in any of the loaded object files. Search in
  ;; reverse order of loading, so that later loadings take precedence.
  ;;
  ;; FIXME: The way that we use PUSHNEW SAP in LOAD-1-FOREIGN means
  ;; that the list isn't guaranteed to be in reverse order of loading,
  ;; at least not if a file is loaded more than once. Is this the
  ;; right thing? (In what cases does it matter?)
  (dolist (handle *handles-from-dlopen*)
    ;; KLUDGE: We implicitly exclude the possibility that the variable
    ;; could actually be NULL, but the man page for dlsym(3) 
    ;; recommends doing a more careful test. -- WHN 20000825
    (let ((possible-result (sap-int (dlsym handle symbol))))
      (unless (zerop possible-result)
	(return possible-result)))))

(defun load-foreign (files
		     &key
		     (libraries '("-lc"))
		     ;; FIXME: The old documentation said
		     ;;   The BASE-FILE argument is used to specify a
		     ;;   file to use as the starting place for
		     ;;   defined symbols. The default is the C start
		     ;;   up code for Lisp.
		     ;; But the code ignored the BASE-FILE argument.
		     ;; The comment above
		     ;;   (DECLARE (IGNORE BASE-FILE))
		     ;; said
		     ;;   dlopen() remembers the name of an object,
		     ;;   when dlopen()ing the same name twice, the
		     ;;   old object is reused.
		     ;; So I deleted all reference to BASE-FILE,
		     ;; including the now-bogus reference to the
		     ;; BASE-FILE argument in the documentation. But
		     ;; are there any other subtleties of the new code
		     ;; which need to be documented in its place?
		     (env nil env-p)
		     (environment (if env-p
				      (unix-environment-sbcl-from-cmu env)
				      (posix-environ))
				  environment-p))
  #+sb-doc
  "LOAD-FOREIGN loads a list of C object files into a running Lisp. The FILES
  argument should be a single file or a list of files. The files may be
  specified as namestrings or as pathnames. The libraries argument should be a
  list of library files as would be specified to ld. They will be searched in
  the order given. The default is just \"-lc\", i.e., the C library. The
  ENVIRONMENT argument is a list of SIMPLE-STRINGs corresponding to the Unix
  environment (\"man environ\") definitions for the invocation of the linker.
  The default is the environment that Lisp is itself running in. Instead of
  using the ENVIRONMENT argument, it is also possible to use the ENV argument,
  using the older, lossy CMU CL representation."
  (when (and env-p environment-p)
    (error "can't specify :ENV and :ENVIRONMENT simultaneously"))
  (let ((output-file (pick-temporary-file-name
		      (concatenate 'string "/tmp/~D~A" (string (gensym)))))
	(error-output (make-string-output-stream)))

    (/show "running" *dso-linker*)
    (force-output)
    (unwind-protect
	(let ((proc (sb-ext:run-program
		     *dso-linker*
		     (append *dso-linker-options*
			     (list output-file)
			     (append (mapcar (lambda (name)
					       (unix-namestring name nil))
					     (if (atom files)
						 (list files)
					       files))
				     libraries))
		     :environment environment
		     :input nil
		     :output error-output
		     :error :output)))
	  (unless proc
	    (error "could not run ~A" *dso-linker*))
	  (unless (zerop (sb-ext:process-exit-code proc))
	    (sb-sys:serve-all-events 0)
	    (error "~A failed:~%~A" *dso-linker*
		   (get-output-stream-string error-output)))
	  (load-1-foreign output-file))
      #-sb-show (sb-unix:unix-unlink output-file)
      #+sb-show (/show "not unlinking" output-file)))) ; so we can look at it

) ; PROGN
