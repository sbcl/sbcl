;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!FORMAT")

(file-comment
  "$Header$")

(define-condition format-error (error)
  ((complaint :reader format-error-complaint :initarg :complaint)
   (arguments :reader format-error-arguments :initarg :arguments :initform nil)
   (control-string :reader format-error-control-string
		   :initarg :control-string
		   :initform *default-format-error-control-string*)
   (offset :reader format-error-offset :initarg :offset
	   :initform *default-format-error-offset*)
   (print-banner :reader format-error-print-banner :initarg :print-banner
		 :initform t))
  (:report %print-format-error))

(defun %print-format-error (condition stream)
  (format stream
	  "~:[~;error in format: ~]~
		 ~?~@[~%  ~A~%  ~V@T^~]"
	  (format-error-print-banner condition)
	  (format-error-complaint condition)
	  (format-error-arguments condition)
	  (format-error-control-string condition)
	  (format-error-offset condition)))

(def!struct format-directive
  (string (required-argument) :type simple-string)
  (start (required-argument) :type (and unsigned-byte fixnum))
  (end (required-argument) :type (and unsigned-byte fixnum))
  (character (required-argument) :type base-char)
  (colonp nil :type (member t nil))
  (atsignp nil :type (member t nil))
  (params nil :type list))
(def!method print-object ((x format-directive) stream)
  (print-unreadable-object (x stream)
    (write-string (format-directive-string x)
		  stream
		  :start (format-directive-start x)
		  :end (format-directive-end x))))

;;;; TOKENIZE-CONTROL-STRING

(defun tokenize-control-string (string)
  (declare (simple-string string))
  (let ((index 0)
	(end (length string))
	(result nil))
    (loop
      (let ((next-directive (or (position #\~ string :start index) end)))
	(when (> next-directive index)
	  (push (subseq string index next-directive) result))
	(when (= next-directive end)
	  (return))
	(let ((directive (parse-directive string next-directive)))
	  (push directive result)
	  (setf index (format-directive-end directive)))))
    (nreverse result)))

(defun parse-directive (string start)
  (let ((posn (1+ start)) (params nil) (colonp nil) (atsignp nil)
	(end (length string)))
    (flet ((get-char ()
	     (if (= posn end)
		 (error 'format-error
			:complaint "String ended before directive was found."
			:control-string string
			:offset start)
		 (schar string posn))))
      (loop
	(let ((char (get-char)))
	  (cond ((or (char<= #\0 char #\9) (char= char #\+) (char= char #\-))
		 (multiple-value-bind (param new-posn)
		     (parse-integer string :start posn :junk-allowed t)
		   (push (cons posn param) params)
		   (setf posn new-posn)
		   (case (get-char)
		     (#\,)
		     ((#\: #\@)
		      (decf posn))
		     (t
		      (return)))))
		((or (char= char #\v) (char= char #\V))
		 (push (cons posn :arg) params)
		 (incf posn)
		 (case (get-char)
		   (#\,)
		   ((#\: #\@)
		    (decf posn))
		   (t
		    (return))))
		((char= char #\#)
		 (push (cons posn :remaining) params)
		 (incf posn)
		 (case (get-char)
		   (#\,)
		   ((#\: #\@)
		    (decf posn))
		   (t
		    (return))))
		((char= char #\')
		 (incf posn)
		 (push (cons posn (get-char)) params)
		 (incf posn)
		 (unless (char= (get-char) #\,)
		   (decf posn)))
		((char= char #\,)
		 (push (cons posn nil) params))
		((char= char #\:)
		 (if colonp
		     (error 'format-error
			    :complaint "too many colons supplied"
			    :control-string string
			    :offset posn)
		     (setf colonp t)))
		((char= char #\@)
		 (if atsignp
		     (error 'format-error
			    :complaint "too many #\\@ characters supplied"
			    :control-string string
			    :offset posn)
		     (setf atsignp t)))
		(t
		 (when (char= (schar string (1- posn)) #\,)
		   (push (cons (1- posn) nil) params))
		 (return))))
	(incf posn))
      (let ((char (get-char)))
	(when (char= char #\/)
	  (let ((closing-slash (position #\/ string :start (1+ posn))))
	    (if closing-slash
		(setf posn closing-slash)
		(error 'format-error
		       :complaint "no matching closing slash"
		       :control-string string
		       :offset posn))))
	(make-format-directive
	 :string string :start start :end (1+ posn)
	 :character (char-upcase char)
	 :colonp colonp :atsignp atsignp
	 :params (nreverse params))))))

;;;; FORMATTER stuff

(sb!xc:defmacro formatter (control-string)
  `#',(%formatter control-string))

(defun %formatter (control-string)
  (block nil
    (catch 'need-orig-args
      (let* ((*simple-args* nil)
	     (*only-simple-args* t)
	     (guts (expand-control-string control-string))
	     (args nil))
	(dolist (arg *simple-args*)
	  (push `(,(car arg)
		  (error
		   'format-error
		   :complaint "required argument missing"
		   :control-string ,control-string
		   :offset ,(cdr arg)))
		args))
	(return `(lambda (stream &optional ,@args &rest args)
		   ,guts
		   args))))
    (let ((*orig-args-available* t)
	  (*only-simple-args* nil))
      `(lambda (stream &rest orig-args)
	 (let ((args orig-args))
	   ,(expand-control-string control-string)
	   args)))))

(defun expand-control-string (string)
  (let* ((string (etypecase string
		   (simple-string
		    string)
		   (string
		    (coerce string 'simple-string))))
	 (*default-format-error-control-string* string)
	 (directives (tokenize-control-string string)))
    `(block nil
       ,@(expand-directive-list directives))))

(defun expand-directive-list (directives)
  (let ((results nil)
	(remaining-directives directives))
    (loop
      (unless remaining-directives
	(return))
      (multiple-value-bind (form new-directives)
	  (expand-directive (car remaining-directives)
			    (cdr remaining-directives))
	(push form results)
	(setf remaining-directives new-directives)))
    (reverse results)))

(defun expand-directive (directive more-directives)
  (etypecase directive
    (format-directive
     (let ((expander
	    (aref *format-directive-expanders*
		  (char-code (format-directive-character directive))))
	   (*default-format-error-offset*
	    (1- (format-directive-end directive))))
       (if expander
	   (funcall expander directive more-directives)
	   (error 'format-error
		  :complaint "unknown directive"))))
    (simple-string
     (values `(write-string ,directive stream)
	     more-directives))))

(defmacro-mundanely expander-next-arg (string offset)
  `(if args
       (pop args)
       (error 'format-error
	      :complaint "no more arguments"
	      :control-string ,string
	      :offset ,offset)))

(defun expand-next-arg (&optional offset)
  (if (or *orig-args-available* (not *only-simple-args*))
      `(,*expander-next-arg-macro*
	,*default-format-error-control-string*
	,(or offset *default-format-error-offset*))
      (let ((symbol (gensym "FORMAT-ARG-")))
	(push (cons symbol (or offset *default-format-error-offset*))
	      *simple-args*)
	symbol)))

(defmacro expand-bind-defaults (specs params &body body)
  (once-only ((params params))
    (if specs
	(collect ((expander-bindings) (runtime-bindings))
		 (dolist (spec specs)
		   (destructuring-bind (var default) spec
		     (let ((symbol (gensym)))
		       (expander-bindings
			`(,var ',symbol))
		       (runtime-bindings
			`(list ',symbol
			       (let* ((param-and-offset (pop ,params))
				      (offset (car param-and-offset))
				      (param (cdr param-and-offset)))
				 (case param
				   (:arg `(or ,(expand-next-arg offset)
					      ,,default))
				   (:remaining
				    (setf *only-simple-args* nil)
				    '(length args))
				   ((nil) ,default)
				   (t param))))))))
		 `(let ,(expander-bindings)
		    `(let ,(list ,@(runtime-bindings))
		       ,@(if ,params
			     (error
			      'format-error
			      :complaint
			      "too many parameters, expected no more than ~D"
			      :arguments (list ,(length specs))
			      :offset (caar ,params)))
		       ,,@body)))
	`(progn
	   (when ,params
	     (error 'format-error
		    :complaint "too many parameters, expected none"
		    :offset (caar ,params)))
	   ,@body))))

;;;; format directive machinery

;;; FIXME: only used in this file, could be SB!XC:DEFMACRO in EVAL-WHEN
(defmacro def-complex-format-directive (char lambda-list &body body)
  (let ((defun-name (intern (format nil
				    "~:@(~:C~)-FORMAT-DIRECTIVE-EXPANDER"
				    char)))
	(directive (gensym))
	(directives (if lambda-list (car (last lambda-list)) (gensym))))
    `(progn
       (defun ,defun-name (,directive ,directives)
	 ,@(if lambda-list
	       `((let ,(mapcar #'(lambda (var)
				   `(,var
				     (,(intern (concatenate
						'string
						"FORMAT-DIRECTIVE-"
						(symbol-name var))
					       (symbol-package 'foo))
				      ,directive)))
			       (butlast lambda-list))
		   ,@body))
	       `((declare (ignore ,directive ,directives))
		 ,@body)))
       (%set-format-directive-expander ,char #',defun-name))))

;;; FIXME: only used in this file, could be SB!XC:DEFMACRO in EVAL-WHEN
(defmacro def-format-directive (char lambda-list &body body)
  (let ((directives (gensym))
	(declarations nil)
	(body-without-decls body))
    (loop
      (let ((form (car body-without-decls)))
	(unless (and (consp form) (eq (car form) 'declare))
	  (return))
	(push (pop body-without-decls) declarations)))
    (setf declarations (reverse declarations))
    `(def-complex-format-directive ,char (,@lambda-list ,directives)
       ,@declarations
       (values (progn ,@body-without-decls)
	       ,directives))))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun %set-format-directive-expander (char fn)
  (setf (aref *format-directive-expanders* (char-code (char-upcase char))) fn)
  char)

(defun %set-format-directive-interpreter (char fn)
  (setf (aref *format-directive-interpreters*
	      (char-code (char-upcase char)))
	fn)
  char)

(defun find-directive (directives kind stop-at-semi)
  (if directives
      (let ((next (car directives)))
	(if (format-directive-p next)
	    (let ((char (format-directive-character next)))
	      (if (or (char= kind char)
		      (and stop-at-semi (char= char #\;)))
		  (car directives)
		  (find-directive
		   (cdr (flet ((after (char)
				 (member (find-directive (cdr directives)
							 char
							 nil)
					 directives)))
			  (case char
			    (#\( (after #\)))
			    (#\< (after #\>))
			    (#\[ (after #\]))
			    (#\{ (after #\}))
			    (t directives))))
		   kind stop-at-semi)))
	    (find-directive (cdr directives) kind stop-at-semi)))))

) ; EVAL-WHEN

;;;; format directives for simple output

(def-format-directive #\A (colonp atsignp params)
  (if params
      (expand-bind-defaults ((mincol 0) (colinc 1) (minpad 0)
			     (padchar #\space))
		     params
	`(format-princ stream ,(expand-next-arg) ',colonp ',atsignp
		       ,mincol ,colinc ,minpad ,padchar))
      `(princ ,(if colonp
		   `(or ,(expand-next-arg) "()")
		   (expand-next-arg))
	      stream)))

(def-format-directive #\S (colonp atsignp params)
  (cond (params
	 (expand-bind-defaults ((mincol 0) (colinc 1) (minpad 0)
				(padchar #\space))
			params
	   `(format-prin1 stream ,(expand-next-arg) ,colonp ,atsignp
			  ,mincol ,colinc ,minpad ,padchar)))
	(colonp
	 `(let ((arg ,(expand-next-arg)))
	    (if arg
		(prin1 arg stream)
		(princ "()" stream))))
	(t
	 `(prin1 ,(expand-next-arg) stream))))

(def-format-directive #\C (colonp atsignp params)
  (expand-bind-defaults () params
    (if colonp
	`(format-print-named-character ,(expand-next-arg) stream)
	(if atsignp
	    `(prin1 ,(expand-next-arg) stream)
	    `(write-char ,(expand-next-arg) stream)))))

(def-format-directive #\W (colonp atsignp params)
  (expand-bind-defaults () params
    (if (or colonp atsignp)
	`(let (,@(when colonp
		   '((*print-pretty* t)))
	       ,@(when atsignp
		   '((*print-level* nil)
		     (*print-length* nil))))
	   (output-object ,(expand-next-arg) stream))
	`(output-object ,(expand-next-arg) stream))))

;;;; format directives for integer output

(defun expand-format-integer (base colonp atsignp params)
  (if (or colonp atsignp params)
      (expand-bind-defaults
	  ((mincol 0) (padchar #\space) (commachar #\,) (commainterval 3))
	  params
	`(format-print-integer stream ,(expand-next-arg) ,colonp ,atsignp
			       ,base ,mincol ,padchar ,commachar
			       ,commainterval))
      `(write ,(expand-next-arg) :stream stream :base ,base :radix nil
	      :escape nil)))

(def-format-directive #\D (colonp atsignp params)
  (expand-format-integer 10 colonp atsignp params))

(def-format-directive #\B (colonp atsignp params)
  (expand-format-integer 2 colonp atsignp params))

(def-format-directive #\O (colonp atsignp params)
  (expand-format-integer 8 colonp atsignp params))

(def-format-directive #\X (colonp atsignp params)
  (expand-format-integer 16 colonp atsignp params))

(def-format-directive #\R (colonp atsignp params)
  (if params
      (expand-bind-defaults
	  ((base 10) (mincol 0) (padchar #\space) (commachar #\,)
	   (commainterval 3))
	  params
	`(format-print-integer stream ,(expand-next-arg) ,colonp ,atsignp
			       ,base ,mincol
			       ,padchar ,commachar ,commainterval))
      (if atsignp
	  (if colonp
	      `(format-print-old-roman stream ,(expand-next-arg))
	      `(format-print-roman stream ,(expand-next-arg)))
	  (if colonp
	      `(format-print-ordinal stream ,(expand-next-arg))
	      `(format-print-cardinal stream ,(expand-next-arg))))))

;;;; format directive for pluralization

(def-format-directive #\P (colonp atsignp params end)
  (expand-bind-defaults () params
    (let ((arg (cond
		((not colonp)
		 (expand-next-arg))
		(*orig-args-available*
		 `(if (eq orig-args args)
		      (error 'format-error
			     :complaint "no previous argument"
			     :offset ,(1- end))
		      (do ((arg-ptr orig-args (cdr arg-ptr)))
			  ((eq (cdr arg-ptr) args)
			   (car arg-ptr)))))
		(*only-simple-args*
		 (unless *simple-args*
		   (error 'format-error
			  :complaint "no previous argument"))
		 (caar *simple-args*))
		(t
		 (throw 'need-orig-args nil)))))
      (if atsignp
	  `(write-string (if (eql ,arg 1) "y" "ies") stream)
	  `(unless (eql ,arg 1) (write-char #\s stream))))))

;;;; format directives for floating point output

(def-format-directive #\F (colonp atsignp params)
  (when colonp
    (error 'format-error
	   :complaint
	   "The colon modifier cannot be used with this directive."))
  (expand-bind-defaults ((w nil) (d nil) (k nil) (ovf nil) (pad #\space)) params
    `(format-fixed stream ,(expand-next-arg) ,w ,d ,k ,ovf ,pad ,atsignp)))

(def-format-directive #\E (colonp atsignp params)
  (when colonp
    (error 'format-error
	   :complaint
	   "The colon modifier cannot be used with this directive."))
  (expand-bind-defaults
      ((w nil) (d nil) (e nil) (k 1) (ovf nil) (pad #\space) (mark nil))
      params
    `(format-exponential stream ,(expand-next-arg) ,w ,d ,e ,k ,ovf ,pad ,mark
			 ,atsignp)))

(def-format-directive #\G (colonp atsignp params)
  (when colonp
    (error 'format-error
	   :complaint
	   "The colon modifier cannot be used with this directive."))
  (expand-bind-defaults
      ((w nil) (d nil) (e nil) (k nil) (ovf nil) (pad #\space) (mark nil))
      params
    `(format-general stream ,(expand-next-arg) ,w ,d ,e ,k ,ovf ,pad ,mark ,atsignp)))

(def-format-directive #\$ (colonp atsignp params)
  (expand-bind-defaults ((d 2) (n 1) (w 0) (pad #\space)) params
    `(format-dollars stream ,(expand-next-arg) ,d ,n ,w ,pad ,colonp
		     ,atsignp)))

;;;; format directives for line/page breaks etc.

(def-format-directive #\% (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
	   :complaint
	   "The colon and atsign modifiers cannot be used with this directive."
	   ))
  (if params
      (expand-bind-defaults ((count 1)) params
	`(dotimes (i ,count)
	   (terpri stream)))
      '(terpri stream)))

(def-format-directive #\& (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
	   :complaint
	   "The colon and atsign modifiers cannot be used with this directive."
	   ))
  (if params
      (expand-bind-defaults ((count 1)) params
	`(progn
	   (fresh-line stream)
	   (dotimes (i (1- ,count))
	     (terpri stream))))
      '(fresh-line stream)))

(def-format-directive #\| (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
	   :complaint
	   "The colon and atsign modifiers cannot be used with this directive."
	   ))
  (if params
      (expand-bind-defaults ((count 1)) params
	`(dotimes (i ,count)
	   (write-char (code-char form-feed-char-code) stream)))
      '(write-char (code-char form-feed-char-code) stream)))

(def-format-directive #\~ (colonp atsignp params)
  (when (or colonp atsignp)
    (error 'format-error
	   :complaint
	   "The colon and atsign modifiers cannot be used with this directive."
	   ))
  (if params
      (expand-bind-defaults ((count 1)) params
	`(dotimes (i ,count)
	   (write-char #\~ stream)))
      '(write-char #\~ stream)))

(def-complex-format-directive #\newline (colonp atsignp params directives)
  (when (and colonp atsignp)
    (error 'format-error
	   :complaint "both colon and atsign modifiers used simultaneously"))
  (values (expand-bind-defaults () params
	    (if atsignp
		'(write-char #\newline stream)
		nil))
	  (if (and (not colonp)
		   directives
		   (simple-string-p (car directives)))
	      (cons (string-left-trim *format-whitespace-chars*
				      (car directives))
		    (cdr directives))
	      directives)))

;;;; format directives for tabs and simple pretty printing

(def-format-directive #\T (colonp atsignp params)
  (if colonp
      (expand-bind-defaults ((n 1) (m 1)) params
	`(pprint-tab ,(if atsignp :section-relative :section)
		     ,n ,m stream))
      (if atsignp
	  (expand-bind-defaults ((colrel 1) (colinc 1)) params
	    `(format-relative-tab stream ,colrel ,colinc))
	  (expand-bind-defaults ((colnum 1) (colinc 1)) params
	    `(format-absolute-tab stream ,colnum ,colinc)))))

(def-format-directive #\_ (colonp atsignp params)
  (expand-bind-defaults () params
    `(pprint-newline ,(if colonp
			  (if atsignp
			      :mandatory
			      :fill)
			  (if atsignp
			      :miser
			      :linear))
		     stream)))

(def-format-directive #\I (colonp atsignp params)
  (when atsignp
    (error 'format-error
	   :complaint
	   "cannot use the at-sign modifier with this directive"))
  (expand-bind-defaults ((n 0)) params
    `(pprint-indent ,(if colonp :current :block) ,n stream)))

;;;; format directive for ~*

(def-format-directive #\* (colonp atsignp params end)
  (if atsignp
      (if colonp
	  (error 'format-error
		 :complaint
		 "both colon and atsign modifiers used simultaneously")
	  (expand-bind-defaults ((posn 0)) params
	    (unless *orig-args-available*
	      (throw 'need-orig-args nil))
	    `(if (<= 0 ,posn (length orig-args))
		 (setf args (nthcdr ,posn orig-args))
		 (error 'format-error
			:complaint "Index ~D out of bounds. Should have been ~
				    between 0 and ~D."
			:arguments (list ,posn (length orig-args))
			:offset ,(1- end)))))
      (if colonp
	  (expand-bind-defaults ((n 1)) params
	    (unless *orig-args-available*
	      (throw 'need-orig-args nil))
	    `(do ((cur-posn 0 (1+ cur-posn))
		  (arg-ptr orig-args (cdr arg-ptr)))
		 ((eq arg-ptr args)
		  (let ((new-posn (- cur-posn ,n)))
		    (if (<= 0 new-posn (length orig-args))
			(setf args (nthcdr new-posn orig-args))
			(error 'format-error
			       :complaint
			       "Index ~D is out of bounds; should have been ~
				between 0 and ~D."
			       :arguments
			       (list new-posn (length orig-args))
			       :offset ,(1- end)))))))
	  (if params
	      (expand-bind-defaults ((n 1)) params
		(setf *only-simple-args* nil)
		`(dotimes (i ,n)
		   ,(expand-next-arg)))
	      (expand-next-arg)))))

;;;; format directive for indirection

(def-format-directive #\? (colonp atsignp params string end)
  (when colonp
    (error 'format-error
	   :complaint "cannot use the colon modifier with this directive"))
  (expand-bind-defaults () params
    `(handler-bind
	 ((format-error
	   #'(lambda (condition)
	       (error 'format-error
		      :complaint
		      "~A~%while processing indirect format string:"
		      :arguments (list condition)
		      :print-banner nil
		      :control-string ,string
		      :offset ,(1- end)))))
       ,(if atsignp
	    (if *orig-args-available*
		`(setf args (%format stream ,(expand-next-arg) orig-args args))
		(throw 'need-orig-args nil))
	    `(%format stream ,(expand-next-arg) ,(expand-next-arg))))))

;;;; format directives for capitalization

(def-complex-format-directive #\( (colonp atsignp params directives)
  (let ((close (find-directive directives #\) nil)))
    (unless close
      (error 'format-error
	     :complaint "no corresponding close parenthesis"))
    (let* ((posn (position close directives))
	   (before (subseq directives 0 posn))
	   (after (nthcdr (1+ posn) directives)))
      (values
       (expand-bind-defaults () params
	 `(let ((stream (make-case-frob-stream stream
					       ,(if colonp
						    (if atsignp
							:upcase
							:capitalize)
						    (if atsignp
							:capitalize-first
							:downcase)))))
	    ,@(expand-directive-list before)))
       after))))

(def-complex-format-directive #\) ()
  (error 'format-error
	 :complaint "no corresponding open parenthesis"))

;;;; format directives and support functions for conditionalization

(def-complex-format-directive #\[ (colonp atsignp params directives)
  (multiple-value-bind (sublists last-semi-with-colon-p remaining)
      (parse-conditional-directive directives)
    (values
     (if atsignp
	 (if colonp
	     (error 'format-error
		    :complaint
		    "both colon and atsign modifiers used simultaneously")
	     (if (cdr sublists)
		 (error 'format-error
			:complaint
			"Can only specify one section")
		 (expand-bind-defaults () params
		   (expand-maybe-conditional (car sublists)))))
	 (if colonp
	     (if (= (length sublists) 2)
		 (expand-bind-defaults () params
		   (expand-true-false-conditional (car sublists)
						  (cadr sublists)))
		 (error 'format-error
			:complaint
			"must specify exactly two sections"))
	     (expand-bind-defaults ((index (expand-next-arg))) params
	       (setf *only-simple-args* nil)
	       (let ((clauses nil))
		 (when last-semi-with-colon-p
		   (push `(t ,@(expand-directive-list (pop sublists)))
			 clauses))
		 (let ((count (length sublists)))
		   (dolist (sublist sublists)
		     (push `(,(decf count)
			     ,@(expand-directive-list sublist))
			   clauses)))
		 `(case ,index ,@clauses)))))
     remaining)))

(defun parse-conditional-directive (directives)
  (let ((sublists nil)
	(last-semi-with-colon-p nil)
	(remaining directives))
    (loop
      (let ((close-or-semi (find-directive remaining #\] t)))
	(unless close-or-semi
	  (error 'format-error
		 :complaint "no corresponding close bracket"))
	(let ((posn (position close-or-semi remaining)))
	  (push (subseq remaining 0 posn) sublists)
	  (setf remaining (nthcdr (1+ posn) remaining))
	  (when (char= (format-directive-character close-or-semi) #\])
	    (return))
	  (setf last-semi-with-colon-p
		(format-directive-colonp close-or-semi)))))
    (values sublists last-semi-with-colon-p remaining)))

(defun expand-maybe-conditional (sublist)
  (flet ((hairy ()
	   `(let ((prev-args args)
		  (arg ,(expand-next-arg)))
	      (when arg
		(setf args prev-args)
		,@(expand-directive-list sublist)))))
    (if *only-simple-args*
	(multiple-value-bind (guts new-args)
	    (let ((*simple-args* *simple-args*))
	      (values (expand-directive-list sublist)
		      *simple-args*))
	  (cond ((eq *simple-args* (cdr new-args))
		 (setf *simple-args* new-args)
		 `(when ,(caar new-args)
		    ,@guts))
		(t
		 (setf *only-simple-args* nil)
		 (hairy))))
	(hairy))))

(defun expand-true-false-conditional (true false)
  (let ((arg (expand-next-arg)))
    (flet ((hairy ()
	     `(if ,arg
		  (progn
		    ,@(expand-directive-list true))
		  (progn
		    ,@(expand-directive-list false)))))
      (if *only-simple-args*
	  (multiple-value-bind (true-guts true-args true-simple)
	      (let ((*simple-args* *simple-args*)
		    (*only-simple-args* t))
		(values (expand-directive-list true)
			*simple-args*
			*only-simple-args*))
	    (multiple-value-bind (false-guts false-args false-simple)
		(let ((*simple-args* *simple-args*)
		      (*only-simple-args* t))
		  (values (expand-directive-list false)
			  *simple-args*
			  *only-simple-args*))
	      (if (= (length true-args) (length false-args))
		  `(if ,arg
		       (progn
			 ,@true-guts)
		       ,(do ((false false-args (cdr false))
			     (true true-args (cdr true))
			     (bindings nil (cons `(,(caar false) ,(caar true))
						 bindings)))
			    ((eq true *simple-args*)
			     (setf *simple-args* true-args)
			     (setf *only-simple-args*
				   (and true-simple false-simple))
			     (if bindings
				 `(let ,bindings
				    ,@false-guts)
				 `(progn
				    ,@false-guts)))))
		  (progn
		    (setf *only-simple-args* nil)
		    (hairy)))))
	  (hairy)))))

(def-complex-format-directive #\; ()
  (error 'format-error
	 :complaint
	 "~~; directive not contained within either ~~[...~~] or ~~<...~~>"))

(def-complex-format-directive #\] ()
  (error 'format-error
	 :complaint
	 "no corresponding open bracket"))

;;;; format directive for up-and-out

(def-format-directive #\^ (colonp atsignp params)
  (when atsignp
    (error 'format-error
	   :complaint "cannot use the at-sign modifier with this directive"))
  (when (and colonp (not *up-up-and-out-allowed*))
    (error 'format-error
	   :complaint "attempt to use ~~:^ outside a ~~:{...~~} construct"))
  `(when ,(case (length params)
	    (0 (if colonp
		   '(null outside-args)
		   (progn
		     (setf *only-simple-args* nil)
		     '(null args))))
	    (1 (expand-bind-defaults ((count 0)) params
		 `(zerop ,count)))
	    (2 (expand-bind-defaults ((arg1 0) (arg2 0)) params
		 `(= ,arg1 ,arg2)))
	    (t (expand-bind-defaults ((arg1 0) (arg2 0) (arg3 0)) params
		 `(<= ,arg1 ,arg2 ,arg3))))
     ,(if colonp
	  '(return-from outside-loop nil)
	  '(return))))

;;;; format directives for iteration

(def-complex-format-directive #\{ (colonp atsignp params string end directives)
  (let ((close (find-directive directives #\} nil)))
    (unless close
      (error 'format-error
	     :complaint "no corresponding close brace"))
    (let* ((closed-with-colon (format-directive-colonp close))
	   (posn (position close directives)))
      (labels
	  ((compute-insides ()
	     (if (zerop posn)
		 (if *orig-args-available*
		     `((handler-bind
			   ((format-error
			     #'(lambda (condition)
				 (error 'format-error
					:complaint
			"~A~%while processing indirect format string:"
					:arguments (list condition)
					:print-banner nil
					:control-string ,string
					:offset ,(1- end)))))
			 (setf args
			       (%format stream inside-string orig-args args))))
		     (throw 'need-orig-args nil))
		 (let ((*up-up-and-out-allowed* colonp))
		   (expand-directive-list (subseq directives 0 posn)))))
	   (compute-loop-aux (count)
	     (when atsignp
	       (setf *only-simple-args* nil))
	     `(loop
		,@(unless closed-with-colon
		    '((when (null args)
			(return))))
		,@(when count
		    `((when (and ,count (minusp (decf ,count)))
			(return))))
		,@(if colonp
		      (let ((*expander-next-arg-macro* 'expander-next-arg)
			    (*only-simple-args* nil)
			    (*orig-args-available* t))
			`((let* ((orig-args ,(expand-next-arg))
				 (outside-args args)
				 (args orig-args))
			    (declare (ignorable orig-args outside-args args))
			    (block nil
			      ,@(compute-insides)))))
		      (compute-insides))
		,@(when closed-with-colon
		    '((when (null args)
			(return))))))
	   (compute-loop ()
	     (if params
		 (expand-bind-defaults ((count nil)) params
		   (compute-loop-aux count))
		 (compute-loop-aux nil)))
	   (compute-block ()
	     (if colonp
		 `(block outside-loop
		    ,(compute-loop))
		 (compute-loop)))
	   (compute-bindings ()
	     (if atsignp
		 (compute-block)
		 `(let* ((orig-args ,(expand-next-arg))
			 (args orig-args))
		    (declare (ignorable orig-args args))
		    ,(let ((*expander-next-arg-macro* 'expander-next-arg)
			   (*only-simple-args* nil)
			   (*orig-args-available* t))
		       (compute-block))))))
	(values (if (zerop posn)
		    `(let ((inside-string ,(expand-next-arg)))
		       ,(compute-bindings))
		    (compute-bindings))
		(nthcdr (1+ posn) directives))))))

(def-complex-format-directive #\} ()
  (error 'format-error
	 :complaint "no corresponding open brace"))

;;;; format directives and support functions for justification

(def-complex-format-directive #\< (colonp atsignp params string end directives)
  (multiple-value-bind (segments first-semi close remaining)
      (parse-format-justification directives)
    (values
     (if (format-directive-colonp close)
	 (multiple-value-bind (prefix per-line-p insides suffix)
	     (parse-format-logical-block segments colonp first-semi
					 close params string end)
	   (expand-format-logical-block prefix per-line-p insides
					suffix atsignp))
	 (expand-format-justification segments colonp atsignp
				      first-semi params))
     remaining)))

(def-complex-format-directive #\> ()
  (error 'format-error
	 :complaint "no corresponding open bracket"))

(defun parse-format-logical-block
       (segments colonp first-semi close params string end)
  (when params
    (error 'format-error
	   :complaint "No parameters can be supplied with ~~<...~~:>."
	   :offset (caar params)))
  (multiple-value-bind (prefix insides suffix)
      (multiple-value-bind (prefix-default suffix-default)
	  (if colonp (values "(" ")") (values nil ""))
	(flet ((extract-string (list prefix-p)
		 (let ((directive (find-if #'format-directive-p list)))
		   (if directive
		       (error 'format-error
			      :complaint
			      "cannot include format directives inside the ~
			       ~:[suffix~;prefix~] segment of ~~<...~~:>"
			      :arguments (list prefix-p)
			      :offset (1- (format-directive-end directive)))
		       (apply #'concatenate 'string list)))))
	(case (length segments)
	  (0 (values prefix-default nil suffix-default))
	  (1 (values prefix-default (car segments) suffix-default))
	  (2 (values (extract-string (car segments) t)
		     (cadr segments) suffix-default))
	  (3 (values (extract-string (car segments) t)
		     (cadr segments)
		     (extract-string (caddr segments) nil)))
	  (t
	   (error 'format-error
		  :complaint "too many segments for ~~<...~~:>")))))
    (when (format-directive-atsignp close)
      (setf insides
	    (add-fill-style-newlines insides
				     string
				     (if first-semi
					 (format-directive-end first-semi)
					 end))))
    (values prefix
	    (and first-semi (format-directive-atsignp first-semi))
	    insides
	    suffix)))

(defun add-fill-style-newlines (list string offset)
  (if list
      (let ((directive (car list)))
	(if (simple-string-p directive)
	    (nconc (add-fill-style-newlines-aux directive string offset)
		   (add-fill-style-newlines (cdr list)
					    string
					    (+ offset (length directive))))
	    (cons directive
		  (add-fill-style-newlines (cdr list)
					   string
					   (format-directive-end directive)))))
      nil))

(defun add-fill-style-newlines-aux (literal string offset)
  (let ((end (length literal))
	(posn 0))
    (collect ((results))
      (loop
	(let ((blank (position #\space literal :start posn)))
	  (when (null blank)
	    (results (subseq literal posn))
	    (return))
	  (let ((non-blank (or (position #\space literal :start blank
					 :test #'char/=)
			       end)))
	    (results (subseq literal posn non-blank))
	    (results (make-format-directive
		      :string string :character #\_
		      :start (+ offset non-blank) :end (+ offset non-blank)
		      :colonp t :atsignp nil :params nil))
	    (setf posn non-blank))
	  (when (= posn end)
	    (return))))
      (results))))

(defun parse-format-justification (directives)
  (let ((first-semi nil)
	(close nil)
	(remaining directives))
    (collect ((segments))
      (loop
	(let ((close-or-semi (find-directive remaining #\> t)))
	  (unless close-or-semi
	    (error 'format-error
		   :complaint "no corresponding close bracket"))
	  (let ((posn (position close-or-semi remaining)))
	    (segments (subseq remaining 0 posn))
	    (setf remaining (nthcdr (1+ posn) remaining)))
	  (when (char= (format-directive-character close-or-semi)
		       #\>)
	    (setf close close-or-semi)
	    (return))
	  (unless first-semi
	    (setf first-semi close-or-semi))))
      (values (segments) first-semi close remaining))))

(sb!xc:defmacro expander-pprint-next-arg (string offset)
  `(progn
     (when (null args)
       (error 'format-error
	      :complaint "no more arguments"
	      :control-string ,string
	      :offset ,offset))
     (pprint-pop)
     (pop args)))

(defun expand-format-logical-block (prefix per-line-p insides suffix atsignp)
  `(let ((arg ,(if atsignp 'args (expand-next-arg))))
     ,@(when atsignp
	 (setf *only-simple-args* nil)
	 '((setf args nil)))
     (pprint-logical-block
	 (stream arg
		 ,(if per-line-p :per-line-prefix :prefix) ,prefix
		 :suffix ,suffix)
       (let ((args arg)
	     ,@(unless atsignp
		 `((orig-args arg))))
	 (declare (ignorable args ,@(unless atsignp '(orig-args))))
	 (block nil
	   ,@(let ((*expander-next-arg-macro* 'expander-pprint-next-arg)
		   (*only-simple-args* nil)
		   (*orig-args-available* t))
	       (expand-directive-list insides)))))))

(defun expand-format-justification (segments colonp atsignp first-semi params)
  (let ((newline-segment-p
	 (and first-semi
	      (format-directive-colonp first-semi))))
    (expand-bind-defaults
	((mincol 0) (colinc 1) (minpad 0) (padchar #\space))
	params
      `(let ((segments nil)
	     ,@(when newline-segment-p
		 '((newline-segment nil)
		   (extra-space 0)
		   (line-len 72))))
	 (block nil
	   ,@(when newline-segment-p
	       `((setf newline-segment
		       (with-output-to-string (stream)
			 ,@(expand-directive-list (pop segments))))
		 ,(expand-bind-defaults
		      ((extra 0)
		       (line-len '(or (sb!impl::line-length stream) 72)))
		      (format-directive-params first-semi)
		    `(setf extra-space ,extra line-len ,line-len))))
	   ,@(mapcar #'(lambda (segment)
			 `(push (with-output-to-string (stream)
				  ,@(expand-directive-list segment))
				segments))
		     segments))
	 (format-justification stream
			       ,@(if newline-segment-p
				     '(newline-segment extra-space line-len)
				     '(nil 0 0))
			       segments ,colonp ,atsignp
			       ,mincol ,colinc ,minpad ,padchar)))))

;;;; format directive and support function for user-defined method

(def-format-directive #\/ (string start end colonp atsignp params)
  (let ((symbol (extract-user-function-name string start end)))
    (collect ((param-names) (bindings))
      (dolist (param-and-offset params)
	(let ((param (cdr param-and-offset)))
	  (let ((param-name (gensym)))
	    (param-names param-name)
	    (bindings `(,param-name
			,(case param
			   (:arg (expand-next-arg))
			   (:remaining '(length args))
			   (t param)))))))
      `(let ,(bindings)
	 (,symbol stream ,(expand-next-arg) ,colonp ,atsignp
		  ,@(param-names))))))

(defun extract-user-function-name (string start end)
  (let ((slash (position #\/ string :start start :end (1- end)
			 :from-end t)))
    (unless slash
      (error 'format-error
	     :complaint "malformed ~~/ directive"))
    (let* ((name (string-upcase (let ((foo string))
				  ;; Hack alert: This is to keep the compiler
				  ;; quiet about deleting code inside the
				  ;; subseq expansion.
				  (subseq foo (1+ slash) (1- end)))))
	   (first-colon (position #\: name))
	   (last-colon (if first-colon (position #\: name :from-end t)))
	   (package-name (if last-colon
			     (subseq name 0 first-colon)
			     "COMMON-LISP-USER"))
	   (package (find-package package-name)))
      (unless package
	;; FIXME: should be PACKAGE-ERROR? Could we just use
	;; FIND-UNDELETED-PACKAGE-OR-LOSE?
	(error 'format-error
	       :complaint "no package named ~S"
	       :arguments (list package-name)))
      (intern (if first-colon
		  (subseq name (1+ first-colon))
		  name)
	      package))))
