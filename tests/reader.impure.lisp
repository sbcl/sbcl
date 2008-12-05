;;;; tests related to the Lisp reader

;;;; This file is impure because we want to modify the readtable and stuff.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(load "assertoid.lisp")
(use-package "ASSERTOID")

;;; Bug 30, involving mistakes in binding the read table, made this
;;; code fail.
(defun read-vector (stream char)
  (declare (ignorable char))
  (coerce (read-delimited-list #\] stream t) 'vector))
(set-macro-character #\[ #'read-vector nil)
(set-macro-character #\] (get-macro-character #\)) nil)
(multiple-value-bind (res pos)
    (read-from-string "[1 2 3]") ; ==> #(1 2 3), 7
  (assert (equalp res #(1 2 3)))
  (assert (= pos 7)))
(multiple-value-bind (res pos)
    (read-from-string "#\\x") ; ==> #\x, 3
  (assert (equalp res #\x))
  (assert (= pos 3)))
(multiple-value-bind (res pos)
    (read-from-string "[#\\x]")
  (assert (equalp res #(#\x)))
  (assert (= pos 5)))

;;; Bug 51b. (try to throw READER-ERRORs when the reader encounters
;;; dubious input)
(assert (raises-error? (read-from-string "1e1000") reader-error))
(assert (raises-error? (read-from-string "1/0") reader-error))

;;; Bug reported by Antonio Martinez on comp.lang.lisp 2003-02-03 in
;;; message <b32da960.0302030640.7d6fc610@posting.google.com>: reading
;;; circular instances of CLOS classes didn't work:
(defclass box ()
  ((value :initarg :value :reader value)))
(defun read-box (stream char)
  (declare (ignore char))
  (let ((objects (read-delimited-list #\] stream t)))
    (unless (= 1 (length objects))
      (error "Unknown box reader syntax"))
    (make-instance 'box :value (first objects))))
(set-macro-character #\[ 'read-box)
(set-syntax-from-char #\] #\))
(multiple-value-bind (res pos)
    (read-from-string "#1=[#1#]")
  (assert (eq (value res) res))
  (assert (= pos 8)))
;;; much, much, later (in Feb 2007), CSR noticed that the problem
;;; still exists for funcallable instances.
(defclass funcallable-box (box sb-mop:funcallable-standard-object) ()
  (:metaclass sb-mop:funcallable-standard-class))
(defun read-funcallable-box (stream char)
  (declare (ignore char))
  (let ((objects (read-delimited-list #\} stream t)))
    (unless (= 1 (length objects))
      (error "Unknown box reader syntax"))
    (make-instance 'funcallable-box :value (first objects))))
(set-macro-character #\{ 'read-funcallable-box)
(set-syntax-from-char #\} #\))
(multiple-value-bind (res pos)
    (read-from-string "#1={#1#}")
  (assert (eq (value res) res))
  (assert (= pos 8)))

;;; CSR managed to break the #S reader macro in the process of merging
;;; SB-PCL:CLASS and CL:CLASS -- make sure it works
(defstruct readable-struct a)
(macrolet
    ((frob (string)
       `(assert (eq (readable-struct-a (read-from-string ,string)) t))))
  (frob "#S(READABLE-STRUCT :A T)")
  (frob "#S(READABLE-STRUCT A T)")
  (frob "#S(READABLE-STRUCT \"A\" T)")
  (frob "#S(READABLE-STRUCT #\\A T)")
  (frob "#S(READABLE-STRUCT #\\A T :A NIL)"))
(macrolet
    ((frob (string)
       `(assert (raises-error? (read-from-string ,string) reader-error))))
  (frob "#S(READABLE-STRUCT . :A)")
  (frob "#S(READABLE-STRUCT :A . T)")
  (frob "#S(READABLE-STRUCT :A T . :A)")
  (frob "#S(READABLE-STRUCT :A T :A . T)"))

;;; reported by Henrik Motakef
(defpackage "")
(assert (eq (symbol-package (read-from-string "||::FOO"))
            (find-package "")))

;;; test nested reads, test case by Helmut Eller for cmucl
(defclass my-in-stream (sb-gray:fundamental-character-input-stream)
  ((last-char :initarg :last-char)))

(let ((string " a ")
      (i 0))
  (defmethod sb-gray:stream-read-char ((s my-in-stream))
    (with-input-from-string (s "b") (read s))
    (with-slots (last-char) s
      (cond (last-char (prog1 last-char (setf last-char nil)))
             (t (prog1 (aref string i)
                  (setq i (mod (1+ i) (length string)))))))))

(defmethod sb-gray:stream-unread-char ((s my-in-stream) char)
  (setf (slot-value s 'last-char) char)
  nil)

(assert (eq 'a (read (make-instance 'my-in-stream :last-char nil))))

;;; NIL as the last argument to SET-SYNTAX-FROM-CHAR in compiled code,
;;; reported by Levente Mészáros
(let ((fun (compile nil '(lambda ()
                          (set-syntax-from-char #\{ #\( *readtable* nil)))))
  (funcall fun)
  (assert (equal '(:ok) (read-from-string "{:ok)"))))

(with-test (:name bad-recursive-read)
  ;; This use to signal an unbound-variable error instead.
  (assert (eq :error
              (handler-case
                  (with-input-from-string (s "42")
                    (read s t nil t))
                (reader-error (e)
                  :error)))))

(with-test (:name standard-readtable-modified)
  (macrolet ((test (form &optional op)
               `(assert
                 (eq :error
                     (handler-case
                         (progn ,form t)
                       (sb-int:standard-readtable-modified-error (e)
                         ,@(when op
                            `((assert
                               (equal ,op (sb-kernel::standard-readtable-modified-operation e)))))
                         :error))))))
    (let ((rt *readtable*))
     (with-standard-io-syntax
       (let ((srt *readtable*))
         (test (setf (readtable-case srt) :preserve) '(setf readtable-case))
         (test (copy-readtable rt srt) 'copy-readtable)
         (test (set-syntax-from-char #\a #\a srt rt) 'set-syntax-from-char)
         (test (set-macro-character #\a (constantly t) t srt) 'set-macro-character)
         (test (make-dispatch-macro-character #\! t srt))
         (test (set-dispatch-macro-character #\# #\a (constantly t) srt) 'set-dispatch-macro-character))))))

;;; THIS SHOULD BE LAST as it frobs the standard readtable
(with-test (:name set-macro-character-nil)
  (handler-bind ((sb-int:standard-readtable-modified-error #'continue))
    (let ((fun (lambda (&rest args) 'ok)))
      ;; NIL means the standard readtable.
      (assert (eq t (set-macro-character #\~ fun nil nil)))
      (assert (eq fun (get-macro-character #\~ nil)))
      (assert (eq t (set-dispatch-macro-character #\# #\~ fun nil)))
      (assert (eq fun (get-dispatch-macro-character #\# #\~ nil))))))

;;; success
