;;;; stuff that knows how to load compiled code directly into core

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; A CORE-OBJECT structure holds the state needed to resolve cross-component
;;; references during in-core compilation.
(defstruct (core-object
	    (:constructor make-core-object ())
	    #-no-ansi-print-object
	    (:print-object (lambda (x s)
			     (print-unreadable-object (x s :type t))))
	    (:copier nil))
  ;; A hashtable translating ENTRY-INFO structures to the corresponding actual
  ;; FUNCTIONs for functions in this compilation.
  (entry-table (make-hash-table :test 'eq) :type hash-table)
  ;; A hashtable translating ENTRY-INFO structures to a list of pairs
  ;; (<code object> . <offset>) describing the places that need to be
  ;; backpatched to point to the function for ENTRY-INFO.
  (patch-table (make-hash-table :test 'eq) :type hash-table)
  ;; A list of all the DEBUG-INFO objects created, kept so that we can
  ;; backpatch with the source info.
  (debug-info () :type list))

;;; Note the existence of FUNCTION.
(defun note-function (info function object)
  (declare (type function function)
	   (type core-object object))
  (let ((patch-table (core-object-patch-table object)))
    (dolist (patch (gethash info patch-table))
      (setf (code-header-ref (car patch) (the index (cdr patch))) function))
    (remhash info patch-table))
  (setf (gethash info (core-object-entry-table object)) function)
  (values))

;;; Do "load-time" fixups on the code vector.
(defun do-core-fixups (code fixups)
  (declare (list fixups))
  (dolist (info fixups)
    (let* ((kind (first info))
	   (fixup (second info))
	   (name (fixup-name fixup))
	   (flavor (fixup-flavor fixup))
	   (offset (third info))
	   (value (ecase flavor
		    (:assembly-routine
		     (aver (symbolp name))
		     (or (gethash name *assembler-routines*)
			 (error "undefined assembler routine: ~S" name)))
		    (:foreign
		     (aver (stringp name))
		     (or (sb!impl::foreign-symbol-address-as-integer name)
			 (error "unknown foreign symbol: ~S")))
		    #!+x86
		    (:code-object
		     (aver (null name))
		     (values (get-lisp-obj-address code) t)))))
      (sb!vm:fixup-code-object code offset value kind))))

;;; Stick a reference to the function Fun in Code-Object at index I. If the
;;; function hasn't been compiled yet, make a note in the Patch-Table.
(defun reference-core-function (code-obj i fun object)
  (declare (type core-object object) (type functional fun)
	   (type index i))
  (let* ((info (leaf-info fun))
	 (found (gethash info (core-object-entry-table object))))
    (if found
	(setf (code-header-ref code-obj i) found)
	(push (cons code-obj i)
	      (gethash info (core-object-patch-table object)))))
  (values))

;;; Call the top-level lambda function dumped for Entry, returning the
;;; values. Entry may be a :TOP-LEVEL-XEP functional.
(defun core-call-top-level-lambda (entry object)
  (declare (type functional entry) (type core-object object))
  (funcall (or (gethash (leaf-info entry)
			(core-object-entry-table object))
	       (error "Unresolved forward reference."))))

;;; Backpatch all the DEBUG-INFOs dumped so far with the specified
;;; SOURCE-INFO list. We also check that there are no outstanding forward
;;; references to functions.
(defun fix-core-source-info (info object source-info)
  (declare (type source-info info) (type core-object object))
  (aver (zerop (hash-table-count (core-object-patch-table object))))
  (let ((res (debug-source-for-info info)))
    (dolist (sinfo res)
      (setf (debug-source-info sinfo) source-info))
    (dolist (info (core-object-debug-info object))
      (setf (compiled-debug-info-source info) res))
    (setf (core-object-debug-info object) ()))
  (values))
