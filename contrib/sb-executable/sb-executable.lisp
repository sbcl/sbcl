(cl:defpackage :sb-executable
  (:use :cl :sb-ext :sb-alien)
  (:export :make-executable :copy-stream)
  ;; (what else should we be exporting?)
  )

(cl:in-package :sb-executable)

(defvar *stream-buffer-size* 8192)
(defun copy-stream (from to)
  "Copy into TO from FROM until end of the input stream, in blocks of
*stream-buffer-size*.  The streams should have the same element type."
  (unless (subtypep (stream-element-type to) (stream-element-type from))
    (error "Incompatible streams ~A and ~A." from to))
  (let ((buf (make-array *stream-buffer-size*
			 :element-type (stream-element-type from))))
    (loop
     (let ((pos (read-sequence buf from)))
       (when (zerop pos) (return))
       (write-sequence buf to :end pos)))))

(defvar *exec-header*
  "#!/bin/sh --
exec sbcl --noinform ~{~A ~}--eval \"(with-open-file (i \\\"$0\\\" :element-type '(unsigned-byte 8)) (read-line i) (read-line i) (load i) (quit))\" --end-toplevel-options ${1+\"$@\"}
")

(defun make-executable (output-file fasls
			&key (runtime-flags '("--disable-debugger"
					      "--userinit /dev/null"
					      "--sysinit /dev/null")))
  "Write an executable called OUTPUT-FILE which can be run from the shell, by 'linking' together code from FASLS.  Actually works by concatenating them and prepending a #! header"
  (with-open-file (out output-file :direction :output
		       :element-type '(unsigned-byte 8))
    (write-sequence (map 'vector #'char-code
			 (format nil *exec-header* runtime-flags)) out)
    (dolist (input-file (if (listp fasls) fasls (list fasls)))
      (with-open-file (in (merge-pathnames input-file
					   (make-pathname :type "fasl"))
			  :element-type '(unsigned-byte 8))
	(copy-stream in out))))
  (let* ((out-name (namestring output-file))
	 (prot (elt (multiple-value-list (sb-unix:unix-stat out-name)) 3)))
    (sb-unix::void-syscall ("chmod" c-string int)
			   out-name
			   (logior prot
				   (if (logand prot #o400) #o100)
				   (if (logand prot  #o40)  #o10)
				   (if (logand prot   #o4)   #o1)))))
			 
  