;;;; Foreign symbol linkage

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;; *STATIC-FOREIGN-SYMBOLS* are static as opposed to "dynamic" (not
;;; as opposed to C's "extern"). The table contains symbols known at
;;; the time that the program was built, but not symbols defined in
;;; object files which have been loaded dynamically since then.
(declaim (type hash-table *static-foreign-symbols*))
(defvar *static-foreign-symbols* (make-hash-table :test 'equal))

(defun find-foreign-symbol-in-table (name table)
  (some (lambda (prefix)
	  (gethash (concatenate 'string prefix name) table))
	#("" "ldso_stub__")))

(defun foreign-symbol-address-as-integer-or-nil (name &optional datap)
  (declare (ignorable datap))
  (or (find-foreign-symbol-in-table name  *static-foreign-symbols*)
      #!+os-provides-dlopen
      (progn
        #-sb-xc-host
        (values #!-linkage-table
                (get-dynamic-foreign-symbol-address name)
                #!+linkage-table
                (ensure-foreign-symbol-linkage name datap)
                t))))

(defun foreign-symbol-address-as-integer (name &optional datap)
  (or (foreign-symbol-address-as-integer-or-nil name datap)
      (error "Unknown foreign symbol: ~S" name)))

(defun foreign-symbol-address (symbol &optional datap)
  (declare (ignorable datap))
  (let ((name (sb!vm:extern-alien-name symbol)))
    #!-linkage-table
    (int-sap (foreign-symbol-address-as-integer name))
    #!+linkage-table
    (multiple-value-bind (addr sharedp)
        (foreign-symbol-address-as-integer name datap)
      #+sb-xc-host
      (aver (not sharedp))
      ;; If the address is from linkage-table and refers to data
      ;; we need to do a bit of juggling.
      (if (and sharedp datap)
          ;; FIXME: 64bit badness here
          (int-sap (sap-ref-32 (int-sap addr) 0))
          (int-sap addr)))))

(defun foreign-reinit ()
  #!+os-provides-dlopen
  (reopen-shared-objects)
  #!+linkage-table
  (linkage-table-reinit))

;;; Cleanups before saving a core
(defun foreign-deinit ()
  #!+(and os-provides-dlopen (not linkage-table))
  (let ((shared (remove-if #'null (mapcar #'sb!alien::shared-object-file
					  *shared-objects*))))
    (when shared
      (warn "~@<Saving cores with shared objects loaded is unsupported on ~
            this platform: calls to foreign functions in shared objects ~
            from the restarted core will not work. You may be able to ~
            work around this limitation by reloading all foreign definitions ~
            and code using them in the restarted core, but no guarantees.~%~%~
            Shared objects in this image:~% ~{~A~^, ~}~:@>"
	    shared)))
  #!+os-provides-dlopen
  (close-shared-objects))

(defun foreign-symbol-in-address (sap)
  (declare (ignorable sap))
  #-sb-xc-host
  (let ((addr (sap-int sap)))
    (declare (ignorable addr))
    #!+linkage-table
    (when (<= sb!vm:linkage-table-space-start
	      addr
	      sb!vm:linkage-table-space-end)
      (maphash (lambda (name info)
		 (let ((table-addr (linkage-info-address info)))
		   (when (<= table-addr
			     addr
			     (+ table-addr sb!vm:linkage-table-entry-size))
		     (return-from foreign-symbol-in-address name))))
	       *linkage-info*))
    #!+os-provides-dladdr
    (with-alien ((info (struct dl-info
			       (filename c-string)
			       (base unsigned)
			       (symbol c-string)
			       (symbol-address unsigned)))
		 (dladdr (function unsigned unsigned (* (struct dl-info)))
			 :extern "dladdr"))
      (let ((err (alien-funcall dladdr addr (addr info))))
	(if (zerop err)
	    nil
	    (slot info 'symbol))))
    ;; FIXME: Even in the absence of dladdr we could search the
    ;; static foreign symbols (and *linkage-info*, for that matter).
    ))

;;; How we learn about foreign symbols and dlhandles initially
(defvar *!initial-foreign-symbols*)

(defun !foreign-cold-init ()
  (dolist (symbol *!initial-foreign-symbols*)
    (setf (gethash (car symbol) *static-foreign-symbols*) (cdr symbol)))
  #!+os-provides-dlopen
  (setf *runtime-dlhandle* (dlopen-or-lose nil)
        *shared-objects* nil))

#!-os-provides-dlopen
(define-unsupported-fun load-shared-object)
