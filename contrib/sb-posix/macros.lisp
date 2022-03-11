(in-package :sb-posix)

(define-designator filename (string c-string)
  ("A STRING designating a filename in native namestring syntax.

Note that native namestring syntax is distinct from Lisp namestring syntax:

  \(pathname \"/foo*/bar\")

is a wild pathname with a pattern-matching directory component.
SB-EXT:PARSE-NATIVE-NAMESTRING may be used to construct Lisp pathnames that
denote POSIX filenames as understood by system calls, and
SB-EXT:NATIVE-NAMESTRING can be used to coerce them into strings in the native
namestring syntax.

Note also that POSIX filename syntax does not distinguish the names of files
from the names of directories: in order to parse the name of a directory in
POSIX filename syntax into a pathname MY-DEFAULTS for which

  \(merge-pathnames (make-pathname :name \"FOO\" :case :common)
                    my-defaults)

returns a pathname that denotes a file in the directory, supply a true
:AS-DIRECTORY argument to SB-EXT:PARSE-NATIVE-NAMESTRING. Likewise, to supply
the name of a directory to a POSIX function in non-directory syntax, supply a
true :AS-FILE argument to SB-EXT:NATIVE-NAMESTRING."
   "Designator for a FILENAME: a STRING designating itself, or a
designator for a PATHNAME designating the corresponding native namestring."
   "Converts FILENAME-DESIGNATOR into a FILENAME.")
  (pathname
   (sb-ext:native-namestring (translate-logical-pathname filename)
                             :as-file t))
  (string
   filename)
  (stream
   (filename (pathname filename))))

(define-designator file-descriptor (fixnum (integer 32))
    ("A FIXNUM designating a native file descriptor.

SB-SYS:MAKE-FD-STREAM can be used to construct a FILE-STREAM associated with a
native file descriptor.

Note that mixing I/O operations on a FILE-STREAM with operations directly on its
descriptor may produce unexpected results if the stream is buffered."
     "Designator for a FILE-DESCRIPTOR: either a fixnum designating itself, or
a FILE-STREAM designating the underlying file-descriptor."
     "Converts FILE-DESCRIPTOR-DESIGNATOR into a FILE-DESCRIPTOR.")
  (file-stream
   (sb-sys:fd-stream-fd file-descriptor))
  (fixnum
   file-descriptor))

(define-designator sap-or-nil (sb-sys:system-area-pointer sb-sys:system-area-pointer)
    ()
  (null (sb-sys:int-sap 0))
  (sb-sys:system-area-pointer sap-or-nil))

(define-designator alien-pointer-to-anything-or-nil (sb-alien-internals::alien-value (* t))
    ()
  (null (sb-alien:sap-alien (sb-sys:int-sap 0) (* t)))
  ((alien (* t)) alien-pointer-to-anything-or-nil))

(defun lisp-for-c-symbol (name)
  (etypecase name
    (list
     (lisp-for-c-symbol (car name)))
    (string
     (let ((root (if (eql #\_ (char name 0)) (subseq name 1) name)))
       (intern (substitute #\- #\_ (string-upcase root)) :sb-posix)))))

;; Note: this variable is set in interface.lisp.  defined here for
;; clarity and so the real-c-name compile as desired.
(defparameter *c-functions-in-runtime* nil)

(defun real-c-name (name)
  (let  ((maybe-name
          (etypecase name
            (list
             (destructuring-bind (name &key c-name options) name
               (declare (ignorable options))
               (if c-name
                   c-name
                   (cond #+largefile
                         ((or (eql options :largefile)
                              (member :largefile options))
                          (format nil "~a_largefile" name))
                         (t
                          name)))))
            (string
             name))))
    (if (member maybe-name *c-functions-in-runtime*
                :test #'string=)
        (format nil "_~A" maybe-name)
        maybe-name)))

(defmacro define-call-internally (lisp-name c-name return-type error-predicate
                                  &rest arguments)
  (if (sb-sys:find-foreign-symbol-address c-name)
      `(progn
        (declaim (inline ,lisp-name))
        (defun ,lisp-name ,(mapcar #'car (remove '&optional arguments))
          (let ((r (alien-funcall
                    (extern-alien
                     ,c-name
                     (function ,return-type
                               ,@(mapcar
                                  (lambda (x)
                                    (if (eq x '&optional)
                                        x
                                        (gethash (cadr x)
                                                 *designator-types*
                                                 (cadr x))))
                                  arguments)))
                    ,@(mapcar (lambda (x)
                                (if (nth-value 1
                                               (gethash (cadr x)
                                                        *designator-types*))
                                    `(,(intern (symbol-name (cadr x))
                                               :sb-posix)
                                      ,(car x))
                                    (car x)))
                              (remove '&optional arguments)))))
            (if (,error-predicate r) (syscall-error ',lisp-name) r))))
      `(sb-int:style-warn "Didn't find definition for ~S" ,c-name)))

(defmacro define-call (name return-type error-predicate &rest arguments)
  (let ((lisp-name (lisp-for-c-symbol name))
        (real-c-name (real-c-name name)))
    `(progn
       (export ',lisp-name :sb-posix)
       (define-call-internally ,lisp-name
           ,real-c-name
         ,return-type
         ,error-predicate
         ,@arguments))))

(defmacro define-entry-point (name arglist &body body)
  (let ((lisp-name (lisp-for-c-symbol name)))
    `(progn
      (export ',lisp-name :sb-posix)
      (declaim (inline ,lisp-name))
      (defun ,lisp-name ,arglist
        ,@body))))

(defmacro define-simple-call (name return-type &rest arguments)
  (multiple-value-bind (lisp-name c-name)
      (values name (substitute #\_ #\- (string-downcase name)))
    `(progn
       (export ',lisp-name :sb-posix)
       (defun ,lisp-name ,(mapcar #'first arguments)
         (alien-funcall (extern-alien ,c-name (function ,return-type
                                                        ,@(mapcar #'second arguments)))
                        ,@(mapcar #'first arguments))))))
