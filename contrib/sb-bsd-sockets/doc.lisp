(eval-when (:load-toplevel :compile-toplevel :execute)
  (defpackage :db-doc (:use  :cl :asdf #+sbcl :sb-ext #+cmu :ext )))
(in-package :db-doc)
;;; turn water into wine ^W^W^W lisp into HTML

#|
OK.  We need a design

1) The aim is to document the current package, given a system.
2) The assumption is that the system is loaded; this makes it easier to
do cross-references and stuff
3) We output HTML on *standard-output*
4) Hyperlink wherever useful
5) We're allowed to intern symbols all over the place if we like

|#

;;; note: break badly on multiple packages


(defvar *symbols* nil
  "List of external symbols to print; derived from parsing DEFPACKAGE form")


(defun worth-documenting-p (symbol)
  (and symbol
       (eql (symbol-package symbol) *package*)
       (or (ignore-errors (find-class symbol))
	   (boundp symbol) (fboundp symbol))))

(defun linkable-symbol-p (word)
  (labels ((symbol-char (c) (or (upper-case-p c) (digit-char-p c)
				(eql c #\-))))
    (and (every  #'symbol-char word)
	 (some #'upper-case-p word)
	 (worth-documenting-p (find-symbol word)))))

(defun markup-word (w)
  (if (symbolp w) (setf w (princ-to-string w)))
  (cond ((linkable-symbol-p w) 
	 (format nil "<a href=\"#~A\">~A</a>"
		 w  w))
	((and (> (length w) 0)
	      (eql (elt w 0) #\_)
	      (eql (elt w (1- (length w))) #\_))
	 (format nil "<b>~A</b>" (subseq w 1 (1- (length w)))))
	(t w)))
(defun markup-space (w)
  (let ((para (search (coerce '(#\Newline #\Newline) 'string) w)))
    (if para
	(format nil "~A<P>~A"
		(subseq w 0 (1+ para))
		(markup-space (subseq w (1+ para) nil)))
	w)))

(defun text-markup (text)
  (let ((start-word 0) (end-word 0))
    (labels ((read-word ()
	       (setf end-word
		     (position-if
		      (lambda (x) (member x '(#\Space #\, #\.  #\Newline)))
		      text :start start-word))
	       (subseq text start-word end-word))
	     (read-space ()
	       (setf start-word
		     (position-if-not
		      (lambda (x) (member x '(#\Space #\, #\.  #\Newline)))
		      text :start end-word ))
	       (subseq text end-word start-word)))
      (with-output-to-string (o)
	(loop for inword = (read-word)
	      do (princ (markup-word inword) o)
	      while (and start-word end-word)
	      do (princ (markup-space (read-space)) o)
	      while (and start-word end-word))))))


(defun do-defpackage (form stream)
  (setf *symbols* nil)
  (destructuring-bind (defn name &rest options) form
    (when (string-equal name (package-name *package*))
      (format stream "<h1>Package ~A</h1>~%" name)
      (when (documentation *package* t)
	(princ (text-markup (documentation *package* t))))
      (let ((exports (assoc :export options)))
        (when exports
          (setf *symbols* (mapcar #'symbol-name (cdr exports)))))
      1)))

(defun do-defclass (form stream)
  (destructuring-bind (defn name super slots &rest options) form
    (when (interesting-name-p name)
      (let ((class  (find-class name)))
	(format stream "<p><a name=\"~A\"><i>Class: </i><b>~A</b></a>~%"
		name  name)
	#+nil (format stream "<p><b>Superclasses: </b> ~{~A ~}~%"
		(mapcar (lambda (x) (text-markup (class-name x)))
			(mop:class-direct-superclasses class)))
	(if (documentation class 'type)
	    (format stream "<blockquote>~A</blockquote>~%"
		    (text-markup (documentation class  'type))))
	(when slots
	  (princ "<p><b>Slots:</b><ul>" stream)
	  (dolist (slot slots)
	    (destructuring-bind
		  (name &key reader writer accessor initarg initform type
			documentation)
		(if (consp slot) slot (list slot))
	      (format stream "<li>~A : ~A</li>~%" name
		      (if documentation (text-markup documentation) "")))) 
	  (princ "</ul>" stream))
	t))))
	

(defun interesting-name-p (name)
  (cond ((consp name)
	 (and (eql (car name) 'setf)
	      (interesting-name-p (cadr name))))
	(t (member (symbol-name name) *symbols* :test #'string=))))

(defun markup-lambdalist (l)
  (let (key-p)
    (loop for i in l
	  if (eq '&key i) do (setf key-p t)
	  end
	  if (and (not key-p) (consp i))
	  collect (list (car i) (markup-word (cadr i)))
	  else collect i)))

(defun do-defunlike (form label stream)
  (destructuring-bind (defn name lambdalist &optional doc &rest code) form
    (when (interesting-name-p name)
      (when (symbolp name)
	(setf *symbols* (remove (symbol-name name) *symbols* :test #'string=)))
      (format stream "<p><a name=\"~A\"><table width=\"100%\"><tr><td width=\"80%\">(~A <i>~A</i>)</td><td align=right>~A</td></tr></table>~%"
              name  (string-downcase (princ-to-string name))
	      (string-downcase
	       (format nil "~{ ~A~}" (markup-lambdalist lambdalist)))
	      label)
      (if (stringp doc)
          (format stream "<blockquote>~A</blockquote>~%"
		  (text-markup doc)))
      t)))

(defun do-defun (form stream) (do-defunlike form "Function" stream))
(defun do-defmethod (form stream) (do-defunlike form "Method" stream))
(defun do-defgeneric (form stream) (do-defunlike form "Generic Function" stream))
(defun do-boolean-sockopt (form stream)
  (destructuring-bind (type lisp-name level c-name) form
    (pushnew (symbol-name lisp-name) *symbols*)

    (do-defunlike `(defun  ,lisp-name ((socket socket) argument)
		    ,(format nil "Return the value of the ~A socket option for SOCKET.  This can also be updated with SETF." (symbol-name c-name) ) 'empty)
      "Accessor" stream)))
    
(defun do-form (form output-stream)
  (cond ((not (listp form)) nil)
	((string= (symbol-name (car form)) "DEFINE-SOCKET-OPTION-BOOL")
	 (do-boolean-sockopt form output-stream))
	((eq (car form) 'defclass)
	 (do-defclass form output-stream))
	((eq (car form) 'eval-when)
	 (do-form (third form) output-stream))
	((eq (car form) 'defpackage)
	 (do-defpackage form output-stream))
	((eq (car form) 'defun)
	 (do-defun form output-stream))
	((eq (car form) 'defmethod)
	 (do-defmethod form output-stream))
	((eq (car form) 'defgeneric)
	 (do-defgeneric form output-stream))
	(t nil)))

(defun do-file (input-stream output-stream)
  "Read in a Lisp program on INPUT-STREAM and make semi-pretty HTML on OUTPUT-STREAM"
  (let ((eof-marker (gensym)))
    (if (< 0 
	 (loop for form =  (read input-stream nil eof-marker)
	       until (eq form eof-marker)
	       if (do-form form output-stream)
	       count 1 #| and
	       do (princ "<hr width=\"20%\">" output-stream) |# ))
	(format output-stream "<hr>"
		))))

(defvar *standard-sharpsign-reader*
  (get-dispatch-macro-character #\# #\|))

(defun document-system (system &key
                               (output-stream *standard-output*)
                               (package *package*))
  "Produce HTML documentation for all files defined in SYSTEM, covering
symbols exported from PACKAGE"
  (let ((*package* (find-package package))
	(*readtable* (copy-readtable)) 
	(*standard-output* output-stream))
    (set-dispatch-macro-character
     #\# #\|
     (lambda (s c n)
       (if (eql (peek-char nil s t nil t) #\|)
	   (princ
	    (text-markup
	     (coerce 
	      (loop with discard = (read-char s t nil t)
		    ;initially (princ "<P>")
		    for c = (read-char s t nil t)
		    until (and (eql c #\|)
			       (eql (peek-char nil s t nil t) #\#))
		    collect c
		    finally (read-char s t nil t))
	      'string)))
	   (funcall *standard-sharpsign-reader* s c n))))
    (dolist (c (cclan:all-components 'db-sockets))
      (when (and (typep c 'cl-source-file)
		 (not (typep c 'db-sockets-system::constants-file)))
	(with-open-file (in (component-pathname c) :direction :input)
	    (do-file in *standard-output*))))))

(defun start ()
  (with-open-file (*standard-output* "index.html" :direction :output)
      (format t "<html><head><title>SBCL BSD-Sockets API Reference</title></head><body>~%")
    (asdf:operate 'asdf:load-op 'sb-bsd-sockets)
    (document-system 'sb-bsd-sockets :package :sb-bsd-sockets)))

(start)
