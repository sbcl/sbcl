;;;; parts of the loader which make sense in the cross-compilation
;;;; host (and which are useful in the host, because they're used by
;;;; GENESIS)
;;;;
;;;; based on the CMU CL load.lisp code, written by Skef Wholey and
;;;; Rob Maclachlan

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!FASL")

;;;; miscellaneous load utilities

;;; Output the current number of semicolons after a fresh-line.
;;; FIXME: non-mnemonic name
(defun load-fresh-line ()
  (fresh-line)
  (let ((semicolons ";;;;;;;;;;;;;;;;"))
    (do ((count *load-depth* (- count (length semicolons))))
	((< count (length semicolons))
	 (write-string semicolons *standard-output* :end count))
      (declare (fixnum count))
      (write-string semicolons))
    (write-char #\space)))

;;; If VERBOSE, output (to *STANDARD-OUTPUT*) a message about how
;;; we're loading from STREAM-WE-ARE-LOADING-FROM.
(defun maybe-announce-load (stream-we-are-loading-from verbose)
  (when verbose
    (load-fresh-line)
    (let ((name #-sb-xc-host (file-name stream-we-are-loading-from)
		#+sb-xc-host nil))
      (if name
	  (format t "loading ~S~%" name)
	  (format t "loading stuff from ~S~%" stream-we-are-loading-from)))))

;;;; utilities for reading from fasl files

#!-sb-fluid (declaim (inline read-byte))

;;;    Expands into code to read an N-byte unsigned integer using
;;; fast-read-byte.
(defmacro fast-read-u-integer (n)
  (declare (optimize (speed 0)))
  (do ((res '(fast-read-byte)
	    `(logior (fast-read-byte)
		     (ash ,res 8)))
       (cnt 1 (1+ cnt)))
      ((>= cnt n) res)))

;;; Like Fast-Read-U-Integer, but the size may be determined at run time.
(defmacro fast-read-var-u-integer (n)
  (let ((n-pos (gensym))
	(n-res (gensym))
	(n-cnt (gensym)))
    `(do ((,n-pos 8 (+ ,n-pos 8))
	  (,n-cnt (1- ,n) (1- ,n-cnt))
	  (,n-res
	   (fast-read-byte)
	   (dpb (fast-read-byte) (byte 8 ,n-pos) ,n-res)))
	 ((zerop ,n-cnt) ,n-res)
       (declare (type index ,n-pos ,n-cnt)))))

;;; Read a signed integer.
(defmacro fast-read-s-integer (n)
  (declare (optimize (speed 0)))
  (let ((n-last (gensym)))
    (do ((res `(let ((,n-last (fast-read-byte)))
		 (if (zerop (logand ,n-last #x80))
		     ,n-last
		     (logior ,n-last #x-100)))
	      `(logior (fast-read-byte)
		       (ash (the (signed-byte ,(* cnt 8)) ,res) 8)))
	 (cnt 1 (1+ cnt)))
	((>= cnt n) res))))

;;; Read an N-byte unsigned integer from the *FASL-INPUT-STREAM*
(defmacro read-arg (n)
  (declare (optimize (speed 0)))
  (if (= n 1)
      `(the (unsigned-byte 8) (read-byte *fasl-input-stream*))
      `(prepare-for-fast-read-byte *fasl-input-stream*
	 (prog1
	  (fast-read-u-integer ,n)
	  (done-with-fast-read-byte)))))

;;; FIXME: This deserves a more descriptive name, and should probably
;;; be implemented as an ordinary function, not a macro.
;;;
;;; (for the names: There seem to be only two cases, so it could be
;;; named READ-U-INTEGER-8 and READ-U-INTEGER-32 or something.)

;;;; the fop table

;;; The table is implemented as a simple-vector indexed by the table
;;; offset. We may need to have several, since LOAD can be called
;;; recursively.

;;; a list of free fop tables for the fasloader
;;;
;;; FIXME: Is it really a win to have this permanently bound?
;;; Couldn't we just bind it on entry to LOAD-AS-FASL?
(defvar *free-fop-tables* (list (make-array 1000)))

;;; the current fop table
(defvar *current-fop-table*)
(declaim (simple-vector *current-fop-table*))

;;; the length of the current fop table
(defvar *current-fop-table-size*)
(declaim (type index *current-fop-table-size*))

;;; the index in the fop-table of the next entry to be used
(defvar *current-fop-table-index*)
(declaim (type index *current-fop-table-index*))

(defun grow-fop-table ()
  (let* ((new-size (* *current-fop-table-size* 2))
	 (new-table (make-array new-size)))
    (declare (fixnum new-size) (simple-vector new-table))
    (replace new-table (the simple-vector *current-fop-table*))
    (setq *current-fop-table* new-table)
    (setq *current-fop-table-size* new-size)))

(defmacro push-fop-table (thing)
  (let ((n-index (gensym)))
    `(let ((,n-index *current-fop-table-index*))
       (declare (fixnum ,n-index))
       (when (= ,n-index (the fixnum *current-fop-table-size*))
	 (grow-fop-table))
       (setq *current-fop-table-index* (1+ ,n-index))
       (setf (svref *current-fop-table* ,n-index) ,thing))))

;;;; the fop stack

;;; (This is in a SIMPLE-VECTOR, but it grows down, since it is
;;; somewhat cheaper to test for overflow that way.)
(defvar *fop-stack* (make-array 100))
(declaim (simple-vector *fop-stack*))

;;; the index of the most recently pushed item on the fop stack
(defvar *fop-stack-pointer* 100)

;;; the current index into the fop stack when we last recursively
;;; entered LOAD
(defvar *fop-stack-pointer-on-entry*)
(declaim (type index *fop-stack-pointer* *fop-stack-pointer-on-entry*))

(defun grow-fop-stack ()
  (let* ((size (length (the simple-vector *fop-stack*)))
	 (new-size (* size 2))
	 (new-stack (make-array new-size)))
    (declare (fixnum size new-size) (simple-vector new-stack))
    (replace new-stack (the simple-vector *fop-stack*) :start1 size)
    (incf *fop-stack-pointer-on-entry* size)
    (setq *fop-stack-pointer* size)
    (setq *fop-stack* new-stack)))

;;; Cache information about the fop stack in local variables. Define a
;;; local macro to pop from the stack. Push the result of evaluation
;;; if specified.
(defmacro with-fop-stack (pushp &body forms)
  (aver (member pushp '(nil t :nope)))
  (let ((n-stack (gensym))
	(n-index (gensym))
	(n-res (gensym)))
    `(let ((,n-stack *fop-stack*)
	   (,n-index *fop-stack-pointer*))
       (declare (simple-vector ,n-stack) (type index ,n-index))
       (macrolet ((pop-stack ()
		    `(prog1
		      (svref ,',n-stack ,',n-index)
		      (incf ,',n-index)))
		  (call-with-popped-things (fun n)
		    (let ((n-start (gensym)))
		      `(let ((,n-start (+ ,',n-index ,n)))
			 (declare (type index ,n-start))
			 (setq ,',n-index ,n-start)
			 (,fun ,@(make-list n :initial-element
					    `(svref ,',n-stack
						    (decf ,n-start))))))))
	 ,(if pushp
	      `(let ((,n-res (progn ,@forms)))
		 (when (zerop ,n-index)
		   (grow-fop-stack)
		   (setq ,n-index *fop-stack-pointer*
			 ,n-stack *fop-stack*))
		 (decf ,n-index)
		 (setq *fop-stack-pointer* ,n-index)
		 (setf (svref ,n-stack ,n-index) ,n-res))
	      `(prog1
		(progn ,@forms)
		(setq *fop-stack-pointer* ,n-index)))))))

;;;; LOAD-AS-FASL
;;;;
;;;; Note: LOAD-AS-FASL is used not only by LOAD, but also (with
;;;; suitable modification of the fop table) in GENESIS. Therefore,
;;;; it's needed not only in the target Lisp, but also in the
;;;; cross-compilation host.

;;; a helper function for LOAD-FASL-GROUP
;;;
;;; Return true if we successfully read a FASL header from the stream,
;;; or NIL if EOF was hit before anything was read. Signal an error if
;;; we encounter garbage.
(defun check-fasl-header (stream)

  (let ((byte (read-byte stream nil)))
    (when byte

      ;; Read the string part of the fasl header, or die.
      (let* ((fhsss *fasl-header-string-start-string*)
	     (fhsss-length (length fhsss)))
	(unless (= byte (char-code (schar fhsss 0)))
	  (error "illegal first byte in fasl file header"))
	(do ((byte (read-byte stream) (read-byte stream))
	     (count 1 (1+ count)))
	    ((= byte +fasl-header-string-stop-char-code+)
	     t)
	  (declare (fixnum byte count))
	  (when (and (< count fhsss-length)
		     (not (eql byte (char-code (schar fhsss count)))))
	    (error
	     "illegal subsequent (not first) byte in fasl file header"))))

      ;; Read and validate implementation and version, or die.
      (let* ((implementation-length (read-arg 4))
	     (implementation-string (make-string implementation-length))
	     (ignore (read-string-as-bytes stream implementation-string))
	     (implementation (keywordicate implementation-string))
	     ;; FIXME: The logic above to read a keyword from the fasl file
	     ;; could probably be shared with the read-a-keyword fop.
	     (version (read-arg 4)))
	(declare (ignore ignore))
	(flet ((check-version (variant possible-implementation needed-version)
		 (when (string= possible-implementation implementation)
		   (unless (= version needed-version)
		     (error "~@<~S is in ~A fasl file format version ~W, ~
                             but this version of SBCL uses ~
                             format version ~W.~:@>"
			    stream
			    variant
			    version
			    needed-version))
		   t)))
	  (or (check-version "native code"
			     +backend-fasl-file-implementation+
			     +fasl-file-version+)
	      (error "~S was compiled for implementation ~A, but this is a ~A."
		     stream
		     implementation
		     +backend-fasl-file-implementation+)))))))

;; Setting this variable gives you a trace of fops as they are loaded and
;; executed.
#!+sb-show
(defvar *show-fops-p* nil)

;;; a helper function for LOAD-AS-FASL
;;;
;;; Return true if we successfully load a group from the stream, or
;;; NIL if EOF was encountered while trying to read from the stream.
;;; Dispatch to the right function for each fop. Special-case
;;; FOP-BYTE-PUSH since it is real common.
(defun load-fasl-group (stream)
  (when (check-fasl-header stream)
    (catch 'fasl-group-end
      (let ((*current-fop-table-index* 0))
	(loop
	  (let ((byte (read-byte stream)))

	    ;; Do some debugging output.
	    #!+sb-show
	    (when *show-fops-p*
	      (let ((ptr *fop-stack-pointer*)
		    (stack *fop-stack*))
		(fresh-line *trace-output*)
		;; The FOP operations are stack based, so it's sorta
		;; logical to display the operand before the operator.
		;; ("reverse Polish notation")
		(unless (= ptr (length stack))
		  (write-char #\space *trace-output*)
		  (prin1 (svref stack ptr) *trace-output*)
		  (terpri *trace-output*))
		;; Display the operator.
		(format *trace-output*
			"~&~S (#X~X at ~D) (~S)~%"
			(svref *fop-names* byte)
			byte
			(1- (file-position stream))
			(svref *fop-funs* byte))))

	    ;; Actually execute the fop.
	    (if (eql byte 3)
	      ;; FIXME: This is the special case for FOP-BYTE-PUSH.
	      ;; Benchmark to see whether it's really worth special
	      ;; casing it. If it is, at least express the test in
	      ;; terms of a symbolic name for the FOP-BYTE-PUSH code,
	      ;; not a bare '3' (!). Failing that, remove the special
	      ;; case (and the comment at the head of this function
	      ;; which mentions it).
	      (let ((index *fop-stack-pointer*))
		(declare (type index index))
		(when (zerop index)
		  (grow-fop-stack)
		  (setq index *fop-stack-pointer*))
		(decf index)
		(setq *fop-stack-pointer* index)
		(setf (svref *fop-stack* index)
		      (svref *current-fop-table* (read-byte stream))))
	      (funcall (the function (svref *fop-funs* byte))))))))))

(defun load-as-fasl (stream verbose print)
  ;; KLUDGE: ANSI says it's good to do something with the :PRINT
  ;; argument to LOAD when we're fasloading a file, but currently we
  ;; don't. (CMU CL did, but implemented it in a non-ANSI way, and I
  ;; just disabled that instead of rewriting it.) -- WHN 20000131
  (declare (ignore print))
  (when (zerop (file-length stream))
    (error "attempt to load an empty FASL file:~%  ~S" (namestring stream)))
  (maybe-announce-load stream verbose)
  (let* ((*fasl-input-stream* stream)
	 (*current-fop-table* (or (pop *free-fop-tables*) (make-array 1000)))
	 (*current-fop-table-size* (length *current-fop-table*))
	 (*fop-stack-pointer-on-entry* *fop-stack-pointer*))
    (unwind-protect
	;; FIXME: This should probably become
	;;   (LOOP WHILE (LOAD-FASL-GROUP-STREAM))
	;; but as a LOOP newbie I don't want to do that until I can
	;; test it.
	(do ((loaded-group (load-fasl-group stream) (load-fasl-group stream)))
	    ((not loaded-group)))
      (setq *fop-stack-pointer* *fop-stack-pointer-on-entry*)
      (push *current-fop-table* *free-fop-tables*)
      ;; NIL out the stack and table, so that we don't hold onto garbage.
      ;;
      ;; FIXME: Couldn't we just get rid of the free fop table pool so
      ;; that some of this NILing out would go away?
      (fill *fop-stack* nil :end *fop-stack-pointer-on-entry*)
      (fill *current-fop-table* nil)))
  t)

;;; This is used in in target-load and also genesis, using
;;; *COLD-FOREIGN-SYMBOL-TABLE*. All the speculative prefix-adding
;;; code for foreign symbol lookup should be here.
(defun find-foreign-symbol-in-table (name table)
  (let ((prefixes
         #!+(or linux freebsd) #("" "ldso_stub__")
	 #!+openbsd #("" "_")))    
    (some (lambda (prefix)
	    (gethash (concatenate 'string prefix name)
		     table
		     nil))
	  prefixes)))


;;;; stuff for debugging/tuning by collecting statistics on FOPs (?)

#|
(defvar *fop-counts* (make-array 256 :initial-element 0))
(defvar *fop-times* (make-array 256 :initial-element 0))
(defvar *print-fops* nil)

(defun clear-counts ()
  (fill (the simple-vector *fop-counts*) 0)
  (fill (the simple-vector *fop-times*) 0)
  t)

(defun analyze-counts ()
  (let ((counts ())
	(total-count 0)
	(times ())
	(total-time 0))
    (macrolet ((breakdown (lvar tvar vec)
		 `(progn
		   (dotimes (i 255)
		     (declare (fixnum i))
		     (let ((n (svref ,vec i)))
		       (push (cons (svref *fop-names* i) n) ,lvar)
		       (incf ,tvar n)))
		   (setq ,lvar (subseq (sort ,lvar (lambda (x y)
						     (> (cdr x) (cdr y))))
				       0 10)))))

      (breakdown counts total-count *fop-counts*)
      (breakdown times total-time *fop-times*)
      (format t "Total fop count is ~D~%" total-count)
      (dolist (c counts)
	(format t "~30S: ~4D~%" (car c) (cdr c)))
      (format t "~%Total fop time is ~D~%" (/ (float total-time) 60.0))
      (dolist (m times)
	(format t "~30S: ~6,2F~%" (car m) (/ (float (cdr m)) 60.0))))))
|#

