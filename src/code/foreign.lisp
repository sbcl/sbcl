;;;; support for dynamically loading foreign object files

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-SYS")

(defun pick-temporary-file-name (&optional
				 ;; KLUDGE: There are various security
				 ;; nastyisms associated with easily
				 ;; guessable temporary file names,
				 ;; and we haven't done anything to
				 ;; work around them here. -- pointed
				 ;; out by Dan Barlow on sbcl-devel
				 ;; 20000702
				 (base "/tmp/sbcl-tmp-~D~C"))
  (let ((code (char-code #\A)))
    (loop
      (let ((name (format nil base (sb-unix:unix-getpid) (code-char code))))
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
		 (error "could not create temporary file ~S: ~A"
			name
			(sb-unix:get-unix-error-msg errno)))
		;; KLUDGE: depends on ASCII character ordering -- WHN 20000128
		((= code (char-code #\Z))
		 (setf code (char-code #\a)))
		((= code (char-code #\z))
		 (return nil))
		(t
		 (incf code))))))))


;;; On any OS where we don't support foreign object file loading, any
;;; query of a foreign symbol value is answered with "no definition
;;; known", i.e. NIL.
;;;
;;; (On any OS which *does* support foreign object file loading, this
;;; placeholder implementation is overwritten by a subsequent real
;;; implementation.)
(defun get-dynamic-foreign-symbol-address (symbol)
  (declare (type simple-string symbol) (ignore symbol))
  nil)

;;; Linux implementation of GET-DYNAMIC-FOREIGN-SYMBOL-ADDRESS
;;; and functions (e.g. LOAD-FOREIGN) which affect it
#+(or linux FreeBSD)
(progn

;;; flags for dlopen()
(defconstant rtld-lazy 1)		; lazy function call binding?
(defconstant rtld-now 2)		; immediate function call binding?
(defconstant rtld-global #x100)		; symbols of loaded obj file
					; (and its dependencies) made
					; visible (as though the
					; obj file were linked directly
					; into the program)?

;;; a list of tables returned from dlopen(3) (or possibly some
;;; bogus value temporarily during initialization)
(defvar *tables-from-dlopen* nil)
;;; Dynamically loaded stuff isn't there upon restoring from a save.
;;; Clearing the variable this way was originally done primarily for
;;; Irix, which resolves tzname at runtime, resulting in
;;; *TABLES-FROM-DLOPEN* being set in the saved core image, resulting
;;; in havoc upon restart; but it seems harmless and tidy for other
;;; OSes too.
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
(push (lambda () (setq *tables-from-dlopen* nil))
      sb-int:*after-save-initializations*)

(defvar *dso-linker* "/usr/bin/ld")
(defvar *dso-linker-options* '("-G" "-o"))


(sb-alien:def-alien-routine dlopen system-area-pointer
  (file sb-c-call:c-string) (mode sb-c-call:int))
(sb-alien:def-alien-routine dlsym system-area-pointer
  (lib system-area-pointer)
  (name sb-c-call:c-string))
(sb-alien:def-alien-routine dlerror sb-c-call:c-string)

;;; Ensure that we've opened our own binary so we can resolve global
;;; variables in the Lisp image that come from libraries. This used to
;;; happen only in GET-DYNAMIC-FOREIGN-SYMBOL-ADDRESS, and only if no
;;; libraries were dlopen()ed already, but that didn't work if
;;; something was dlopen()ed before any problem global vars were used.
;;; So now we do this in any function that can add to the
;;; *TABLES-FROM-DLOPEN*, as well as in
;;; GET-DYNAMIC-FOREIGN-SYMBOL-ADDRESS.
(defun ensure-lisp-table-opened ()
  (unless *tables-from-dlopen*
    ;; Prevent recursive call if dlopen() isn't defined.
    (setf *tables-from-dlopen* (int-sap 0))
    (setf *tables-from-dlopen* (list (dlopen nil rtld-lazy)))
    (when (zerop (sb-sys:sap-int (first *tables-from-dlopen*)))
      (error "can't open global symbol table: ~S" (dlerror)))))

(defun load-1-foreign (file)
  "the primitive upon which the more general LOAD-FOREIGN is built: load
  a single foreign object file

  To use LOAD-1-FOREIGN, at the Unix command line do this:
    echo 'int summish(int x, int y) { return 1 + x + y; }' > /tmp/ffi-test.c
    make /tmp/ffi-test.o # i.e. cc -c -o /tmp/ffi-test.o /tmp/ffi-test.c
    ld -shared -o /tmp/ffi-test.so /tmp/ffi-test.o
  then in SBCL do this:
    (LOAD-1-FOREIGN \"/tmp/ffi-test.so\")
    (DEF-ALIEN-ROUTINE SUMMISH INT (X INT) (Y INT))
  Now running (SUMMISH 10 20) should return 31.
"
  (ensure-lisp-table-opened)
  ;; Note: We use RTLD-GLOBAL so that it can find all the symbols
  ;; previously loaded. We use RTLD-NOW so that dlopen() will fail if
  ;; not all symbols are defined.
  (let* ((real-file (or (unix-namestring file) file))
         (sap (dlopen real-file (logior rtld-now rtld-global))))
       (if (zerop (sap-int sap))
	   (error "can't open object ~S: ~S" real-file (dlerror))
	   (pushnew sap *tables-from-dlopen* :test #'sap=)))
  (values))

(defun get-dynamic-foreign-symbol-address (symbol)
  (ensure-lisp-table-opened)
  ;; Find the symbol in any of the loaded object files. Search in
  ;; reverse order of loading, so that later loadings take precedence.
  ;;
  ;; FIXME: The way that we use PUSHNEW SAP in LOAD-1-FOREIGN means
  ;; that the list isn't guaranteed to be in reverse order of loading,
  ;; at least not if a file is loaded more than once. Is this the
  ;; right thing? (In what cases does it matter?)
  (dolist (table *tables-from-dlopen*)
    ;; KLUDGE: We implicitly exclude the possibility that the variable
    ;; could actually be NULL, but the man page for dlsym(3) 
    ;; recommends doing a more careful test. -- WHN 20000825
    (let ((possible-result (sap-int (dlsym table symbol))))
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
  using the alternate, lossy representation used by CMU CL."
  (when (and env-p environment-p)
    (error "can't specify :ENV and :ENVIRONMENT simultaneously"))
  (let ((output-file (pick-temporary-file-name
		      (concatenate 'string "/tmp/~D~C" (string (gensym)))))
	(error-output (make-string-output-stream)))

    (/show "running" *dso-linker*)
    (force-output)
    (unwind-protect
	(let ((proc (sb-ext:run-program
		     *dso-linker*
		     (append *dso-linker-options*
			     (list output-file)
			     (append (mapcar #'(lambda (name)
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
