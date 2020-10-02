(cl:defpackage :sb-executable
  (:use :cl :sb-ext :sb-alien)
  (:export :make-executable :copy-stream)
  ;; (what else should we be exporting?)
  )

(cl:in-package :sb-executable)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (sb-int:system-package-p *package*) t))

(defvar *stream-buffer-size* 8192)
(defun copy-stream (from to &key (element-type (stream-element-type from) element-type-passed-p))
  "Copy into TO from FROM until end of the input stream, in blocks of
*stream-buffer-size*.  The streams should have the same element type.

The argument :element-type indicates the element type of the
buffer used to copy data from FROM to TO.

If one of the streams has an element type that is different from
what (stream-element-type stream) reports, that is, if it was
opened with :element-type :default, the argument :element-type is
required in order to select the correct stream decoding/encoding
strategy."
  (unless (or element-type-passed-p
              (subtypep (stream-element-type to) element-type))
    (error "Incompatible streams ~A and ~A:" from to))
  (let ((buf (make-array *stream-buffer-size*
                         :element-type element-type)))
    (loop
      (let ((pos (read-sequence buf from)))
        (when (zerop pos) (return))
        (write-sequence buf to :end pos)))))


(defvar *exec-header*
  "#!/bin/sh --
exec sbcl --noinform ~{~A ~}--eval \"(with-open-file (i \\\"$0\\\" :element-type '(unsigned-byte 8)) (loop while (< ret 2) when (= (read-byte i) 10) count 1 into ret) (load i) (funcall (quote ~A)) (exit))\" --end-toplevel-options ${1+\"$@\"}
")

(defun make-executable (output-file fasls
                        &key (runtime-flags '("--disable-debugger"
                                              "--no-userinit"
                                              "--no-sysinit"))
                        initial-function)
  "Write an executable called OUTPUT-FILE which can be run from the shell, by 'linking' together code from FASLS.  Actually works by concatenating them and prepending a #! header"
  (with-open-file (out output-file
                       :direction :output
                       :if-exists :supersede
                       :element-type '(unsigned-byte 8))
    (write-sequence (map 'vector #'char-code
                         (format nil *exec-header* runtime-flags
                                 (or initial-function 'values))) out)
    (dolist (input-file (sb-int:ensure-list fasls))
      (with-open-file (in (merge-pathnames input-file
                                           (make-pathname :type "fasl"))
                          :element-type '(unsigned-byte 8))
        (copy-stream in out))))
  #-win32
  (let* (;; FIXME: use OUT as the pathname designator
         (out-name (namestring (translate-logical-pathname output-file)))
         (prot (elt (multiple-value-list (sb-unix:unix-stat out-name)) 3)))
    (if prot
        (sb-unix:void-syscall ("chmod" c-string int)
                               out-name
                               (logior prot
                                       (if (logand prot #o400) #o100)
                                       (if (logand prot  #o40)  #o10)
                                       (if (logand prot   #o4)   #o1)))
        (error "stat() call failed"))))

(provide 'sb-executable)
