;;;; miscellaneous system hacking macros

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;; This checks to see whether the array is simple and the start and
;;; end are in bounds. If so, it proceeds with those values.
;;; Otherwise, it calls %WITH-ARRAY-DATA. Note that %WITH-ARRAY-DATA
;;; may be further optimized.
;;;
;;; Given any ARRAY, bind DATA-VAR to the array's data vector and
;;; START-VAR and END-VAR to the start and end of the designated
;;; portion of the data vector. SVALUE and EVALUE are any start and
;;; end specified to the original operation, and are factored into the
;;; bindings of START-VAR and END-VAR. OFFSET-VAR is the cumulative
;;; offset of all displacements encountered, and does not include
;;; SVALUE.
;;;
;;; When FORCE-INLINE is set, the underlying %WITH-ARRAY-DATA form is
;;; forced to be inline, overriding the ordinary judgment of the
;;; %WITH-ARRAY-DATA DEFTRANSFORMs. Ordinarily the DEFTRANSFORMs are
;;; fairly picky about their arguments, figuring that if you haven't
;;; bothered to get all your ducks in a row, you probably don't care
;;; that much about speed anyway! But in some cases it makes sense to
;;; do type testing inside %WITH-ARRAY-DATA instead of outside, and
;;; the DEFTRANSFORM can't tell that that's going on, so it can make
;;; sense to use FORCE-INLINE option in that case.
(defmacro with-array-data (((data-var array &key offset-var)
			    (start-var &optional (svalue 0))
			    (end-var &optional (evalue nil))
			    &key force-inline)
			   &body forms)
  (once-only ((n-array array)
	      (n-svalue `(the index ,svalue))
	      (n-evalue `(the (or index null) ,evalue)))
    `(multiple-value-bind (,data-var
			   ,start-var
			   ,end-var
			   ,@(when offset-var `(,offset-var)))
	 (if (not (array-header-p ,n-array))
	     (let ((,n-array ,n-array))
	       (declare (type (simple-array * (*)) ,n-array))
	       ,(once-only ((n-len `(length ,n-array))
			    (n-end `(or ,n-evalue ,n-len)))
		  `(if (<= ,n-svalue ,n-end ,n-len)
		       ;; success
		       (values ,n-array ,n-svalue ,n-end 0)
		       ;; failure: Make a NOTINLINE call to
		       ;; %WITH-ARRAY-DATA with our bad data
		       ;; to cause the error to be signalled.
		       (locally
			 (declare (notinline %with-array-data))
			 (%with-array-data ,n-array ,n-svalue ,n-evalue)))))
	     (,(if force-inline '%with-array-data-macro '%with-array-data)
	      ,n-array ,n-svalue ,n-evalue))
       ,@forms)))

;;; This is the fundamental definition of %WITH-ARRAY-DATA, for use in
;;; DEFTRANSFORMs and DEFUNs.
(defmacro %with-array-data-macro (array
				  start
				  end
				  &key
				  (element-type '*)
				  unsafe?
				  fail-inline?)
  (let ((size (gensym "SIZE-"))
	(data (gensym "DATA-"))
	(cumulative-offset (gensym "CUMULATIVE-OFFSET-")))
    `(let* ((,size (array-total-size ,array))
	    (,end (cond (,end
			 (unless (or ,unsafe? (<= ,end ,size))
			   ,(if fail-inline?
				`(error "End ~D is greater than total size ~D."
					,end ,size)
				`(failed-%with-array-data ,array ,start ,end)))
			 ,end)
			(t ,size))))
       (unless (or ,unsafe? (<= ,start ,end))
	 ,(if fail-inline?
	      `(error "Start ~D is greater than end ~D." ,start ,end)
	      `(failed-%with-array-data ,array ,start ,end)))
       (do ((,data ,array (%array-data-vector ,data))
	    (,cumulative-offset 0
				(+ ,cumulative-offset
				   (%array-displacement ,data))))
	   ((not (array-header-p ,data))
	    (values (the (simple-array ,element-type 1) ,data)
		    (the index (+ ,cumulative-offset ,start))
		    (the index (+ ,cumulative-offset ,end))
		    (the index ,cumulative-offset)))
	 (declare (type index ,cumulative-offset))))))

#!-gengc
(defmacro without-gcing (&rest body)
  #!+sb-doc
  "Executes the forms in the body without doing a garbage collection."
  `(unwind-protect
       (let ((*gc-inhibit* t))
	 ,@body)
     (when (and *need-to-collect-garbage* (not *gc-inhibit*))
       (maybe-gc nil))))

#!+gengc
(defmacro without-gcing (&rest body)
  #!+sb-doc
  "Executes the forms in the body without doing a garbage collection."
  `(without-interrupts ,@body))

;;; EOF-OR-LOSE is a useful macro that handles EOF.
(defmacro eof-or-lose (stream eof-error-p eof-value)
  `(if ,eof-error-p
       (error 'end-of-file :stream ,stream)
       ,eof-value))

;;; These macros handle the special cases of T and NIL for input and
;;; output streams.
;;;
;;; FIXME: Shouldn't these be functions instead of macros?
(defmacro in-synonym-of (stream &optional check-type)
  (let ((svar (gensym)))
    `(let ((,svar ,stream))
       (cond ((null ,svar) *standard-input*)
	     ((eq ,svar t) *terminal-io*)
	     (T ,@(when check-type `((enforce-type ,svar ,check-type)))
		#!+high-security
		(unless (input-stream-p ,svar)
		  (error 'simple-type-error
			 :datum ,svar
			 :expected-type '(satisfies input-stream-p)
			 :format-control "~S isn't an input stream"
			 :format-arguments ,(list  svar)))		
		,svar)))))
(defmacro out-synonym-of (stream &optional check-type)
  (let ((svar (gensym)))
    `(let ((,svar ,stream))
       (cond ((null ,svar) *standard-output*)
	     ((eq ,svar t) *terminal-io*)
	     (T ,@(when check-type `((check-type ,svar ,check-type)))
		#!+high-security
		(unless (output-stream-p ,svar)
		  (error 'simple-type-error
			 :datum ,svar
			 :expected-type '(satisfies output-stream-p)
			 :format-control "~S isn't an output stream."
			 :format-arguments ,(list  svar)))
		,svar)))))

;;; WITH-mumble-STREAM calls the function in the given SLOT of the
;;; STREAM with the ARGS for LISP-STREAMs, or the FUNCTION with the
;;; ARGS for FUNDAMENTAL-STREAMs.
(defmacro with-in-stream (stream (slot &rest args) &optional stream-dispatch)
  `(let ((stream (in-synonym-of ,stream)))
    ,(if stream-dispatch
	 `(if (lisp-stream-p stream)
	      (funcall (,slot stream) stream ,@args)
	      ,@(when stream-dispatch
		  `(,(destructuring-bind (function &rest args) stream-dispatch
		       `(,function stream ,@args)))))
	 `(funcall (,slot stream) stream ,@args))))

(defmacro with-out-stream (stream (slot &rest args) &optional stream-dispatch)
  `(let ((stream (out-synonym-of ,stream)))
    ,(if stream-dispatch
	 `(if (lisp-stream-p stream)
	      (funcall (,slot stream) stream ,@args)
	      ,@(when stream-dispatch
		  `(,(destructuring-bind (function &rest args) stream-dispatch
					 `(,function stream ,@args)))))
	 `(funcall (,slot stream) stream ,@args))))

;;;; These are hacks to make the reader win.

;;; This macro sets up some local vars for use by the
;;; FAST-READ-CHAR macro within the enclosed lexical scope. The stream
;;; is assumed to be a LISP-STREAM.
(defmacro prepare-for-fast-read-char (stream &body forms)
  `(let* ((%frc-stream% ,stream)
	  (%frc-method% (lisp-stream-in %frc-stream%))
	  (%frc-buffer% (lisp-stream-in-buffer %frc-stream%))
	  (%frc-index% (lisp-stream-in-index %frc-stream%)))
     (declare (type index %frc-index%)
	      (type lisp-stream %frc-stream%))
     ,@forms))

;;; This macro must be called after one is done with FAST-READ-CHAR
;;; inside its scope to decache the lisp-stream-in-index.
(defmacro done-with-fast-read-char ()
  `(setf (lisp-stream-in-index %frc-stream%) %frc-index%))

;;; a macro with the same calling convention as READ-CHAR, to be used
;;; within the scope of a PREPARE-FOR-FAST-READ-CHAR
(defmacro fast-read-char (&optional (eof-error-p t) (eof-value ()))
  `(cond
    ((not %frc-buffer%)
     (funcall %frc-method% %frc-stream% ,eof-error-p ,eof-value))
    ((= %frc-index% +in-buffer-length+)
     (prog1 (fast-read-char-refill %frc-stream% ,eof-error-p ,eof-value)
	    (setq %frc-index% (lisp-stream-in-index %frc-stream%))))
    (t
     (prog1 (code-char (aref %frc-buffer% %frc-index%))
	    (incf %frc-index%)))))

;;;; And these for the fasloader...

;;; Just like PREPARE-FOR-FAST-READ-CHAR except that we get the BIN
;;; method. The stream is assumed to be a LISP-STREAM.
;;;
;;; KLUDGE: It seems weird to have to remember to explicitly call
;;; DONE-WITH-FAST-READ-BYTE at the end of this, given that we're
;;; already wrapping the stuff inside in a block. Why not rename this
;;; macro to WITH-FAST-READ-BYTE, do the DONE-WITH-FAST-READ-BYTE stuff
;;; automatically at the end of the block, and eliminate
;;; DONE-WITH-FAST-READ-BYTE as a separate entity? (and similarly
;;; for the FAST-READ-CHAR stuff) -- WHN 19990825
(defmacro prepare-for-fast-read-byte (stream &body forms)
  `(let* ((%frc-stream% ,stream)
	  (%frc-method% (lisp-stream-bin %frc-stream%))
	  (%frc-buffer% (lisp-stream-in-buffer %frc-stream%))
	  (%frc-index% (lisp-stream-in-index %frc-stream%)))
     (declare (type index %frc-index%)
	      (type lisp-stream %frc-stream%))
     ,@forms))

;;; Similar to fast-read-char, but we use a different refill routine & don't
;;; convert to characters. If ANY-TYPE is true, then this can be used on any
;;; integer streams, and we don't assert the result type.
(defmacro fast-read-byte (&optional (eof-error-p t) (eof-value ()) any-type)
  ;; KLUDGE: should use ONCE-ONLY on EOF-ERROR-P and EOF-VALUE -- WHN 19990825
  `(truly-the
    ,(if (and (eq eof-error-p t) (not any-type)) '(unsigned-byte 8) t)
    (cond
     ((not %frc-buffer%)
      (funcall %frc-method% %frc-stream% ,eof-error-p ,eof-value))
     ((= %frc-index% +in-buffer-length+)
      (prog1 (fast-read-byte-refill %frc-stream% ,eof-error-p ,eof-value)
	(setq %frc-index% (lisp-stream-in-index %frc-stream%))))
     (t
      (prog1 (aref %frc-buffer% %frc-index%)
	(incf %frc-index%))))))
(defmacro done-with-fast-read-byte ()
  `(done-with-fast-read-char))
