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

;;; On any OS where we don't support foreign object file loading, any
;;; query of a foreign symbol value is answered with "no definition
;;; known", i.e. NIL.
#-(or linux sunos FreeBSD OpenBSD NetBSD darwin)
(defun get-dynamic-foreign-symbol-address (symbol)
  (declare (type simple-string symbol) (ignore symbol))
  nil)

;;; dlsym()-based implementation of GET-DYNAMIC-FOREIGN-SYMBOL-ADDRESS
;;; and functions (e.g. LOAD-FOREIGN) which affect it.  This should 
;;; work on any ELF system with dlopen(3) and dlsym(3)
;;; It also works on OpenBSD, which isn't ELF, but is otherwise modern
;;; enough to have a fairly well working dlopen/dlsym implementation.
(macrolet ((define-unsupported-fun (fun-name &optional (error-message "unsupported on this system"))
	     `(defun ,fun-name (&rest rest)
		,error-message
                (declare (ignore rest))
		(error 'unsupported-operator :name ',fun-name))))
  #-(or linux sunos FreeBSD OpenBSD NetBSD darwin)
  (define-unsupported-fun load-shared-object)
  #+(or linux sunos FreeBSD OpenBSD NetBSD darwin)
  (progn

    (define-unsupported-fun load-foreign "Unsupported as of SBCL 0.8.13.")
    (define-unsupported-fun load-1-foreign "Unsupported as of SBCL 0.8.13. Please use LOAD-SHARED-OBJECT.")

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

    (define-alien-routine dlopen system-area-pointer
      (file c-string) (mode int))
    
    (define-alien-routine dlsym system-area-pointer
      (lib system-area-pointer) (name c-string))
    
    (define-alien-routine dlerror c-string)
    
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

    (defun load-shared-object (file)
      "Load a shared library/dynamic shared object file/general
  dlopenable alien container.

  To use LOAD-SHARED-OBJECT, at the Unix command line do this:
    echo 'int summish(int x, int y) { return 1 + x + y; }' > /tmp/ffi-test.c
    make /tmp/ffi-test.o # i.e. cc -c -o /tmp/ffi-test.o /tmp/ffi-test.c
    ld -shared -o /tmp/ffi-test.so /tmp/ffi-test.o
  then in SBCL do this:
    (LOAD-SHARED-OBJECT \"/tmp/ffi-test.so\")
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
      (setf symbol (coerce symbol 'simple-base-string))
      ;; Find the symbol in any of the loaded object files. Search in
      ;; reverse order of loading, so that later loadings take precedence.
      ;;
      ;; FIXME: The way that we use PUSHNEW SAP in LOAD-SHARED-OBJECT means
      ;; that the list isn't guaranteed to be in reverse order of loading,
      ;; at least not if a file is loaded more than once. Is this the
      ;; right thing? (In what cases does it matter?)
      (dolist (handle (reverse *handles-from-dlopen*))
	;; KLUDGE: We implicitly exclude the possibility that the variable
	;; could actually be NULL, but the man page for dlsym(3) 
	;; recommends doing a more careful test. -- WHN 20000825
	(let ((possible-result (sap-int (dlsym handle symbol))))
	  (unless (zerop possible-result)
	    (return possible-result)))))

    (defun foreign-symbol-in-address (sap)
      (declare (ignore sap)))

    (when (ignore-errors (foreign-symbol-address "dladdr"))
      (setf (symbol-function 'foreign-symbol-in-address)
	    ;; KLUDGE: This COMPILE trick is to avoid trying to
	    ;; compile a reference to dladdr on platforms without it.
	    (compile nil
	     '(lambda (sap)
	       (let ((addr (sap-int sap)))
		 (with-alien ((info
			       (struct dl-info
				       (filename c-string)
				       (base unsigned)
				       (symbol c-string)
				       (symbol-address unsigned)))
			      (dladdr
			       (function unsigned
					 unsigned (* (struct dl-info)))
			       :extern "dladdr"))
		   (let ((err (alien-funcall dladdr addr (addr info))))
		     (if (zerop err)
			 nil
			 (values (slot info 'symbol)
				 (slot info 'filename)
				 addr
				 (- addr (slot info 'symbol-address)))))))))))
    
    ))					; PROGN, MACROLET
