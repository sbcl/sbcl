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

;;; Test that symbols are properly normalized in SB-UNICODE builds
#+sb-unicode
(with-test (:name (:normalizing-reader)
                  :skipped-on (not :sb-unicode))
  (labels ((str (&rest chars)
             (coerce chars 'string))
           (symbol (&rest chars)
             (read-from-string (apply #'str chars))))
    (assert (eq :a :a))
    (assert (eq :a :A))
    (assert (eq (symbol #\UF984) (symbol #\U6FFE)))
    (make-package "BAFFLE")
    (intern "C" "BAFFLE")
    (assert (eq (symbol #\b #\a #\f #\f #\l #\e #\: #\: #\c)
                (symbol #\b #\a #\UFB04 #\e #\: #\: #\c)))
    (assert (not (eq (symbol #\| #\f #\f #\l #\|) (symbol #\| #\UFB04 #\|))))
    (assert (not (eq (symbol #\\ #\U32C0) (symbol #\1 #\U6708))))
    (assert (eq (symbol #\U32C0) (symbol #\1 #\U6708)))
    (let ((*readtable* (copy-readtable)))
      (setf (sb-ext:readtable-normalization *readtable*) nil)
      (assert (not (eq (symbol #\b #\a #\f #\f #\l #\e)
                       (symbol #\b #\a #\UFB04 #\e))))
      (assert (not (eq (symbol #\U32C0) (symbol #\1 #\U6708)))))))

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
(assert-error (read-from-string "1e1000") reader-error)
(assert-error (read-from-string "1/0") reader-error)

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
(assert (eq (get-macro-character #\[) 'read-box)) ; not #'READ-BOX
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
       `(handler-bind ((warning #'muffle-warning))
          (assert (eq (readable-struct-a (read-from-string ,string)) t)))))
  (frob "#S(READABLE-STRUCT :A T)")
  (frob "#S(READABLE-STRUCT A T)")
  (frob "#S(READABLE-STRUCT \"A\" T)")
  (frob "#S(READABLE-STRUCT #\\A T)")
  (frob "#S(READABLE-STRUCT #\\A T :A NIL)"))
(macrolet
    ((frob (string)
       `(assert-error (read-from-string ,string) reader-error)))
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

(with-test (:name :bad-recursive-read)
  ;; This use to signal an unbound-variable error instead.
  (assert (eq :error
              (handler-case
                  (with-input-from-string (s "42")
                    (read s t nil t))
                (reader-error ()
                  :error)))))

(with-test (:name :standard-readtable-modified)
  (macrolet ((test (form &optional op)
               `(assert
                 (eq :error
                     (handler-case
                         (progn ,form t)
                       (sb-int:standard-readtable-modified-error (e)
                         (declare (ignorable e))
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

(with-test (:name :reader-package-errors)
  (flet ((test (string)
           (handler-case
               (progn (read-from-string string) :feh)
             (error (e)
               (when (and (typep e 'reader-error) (typep e 'package-error))
                 (package-error-package e))))))
    (assert (equal "NO-SUCH-PKG" (test "no-such-pkg::foo")))
    (assert (eq (find-package :cl) (test "cl:no-such-sym")))))

;; lp# 1012335 - also tested by 'READ-BOX above
(handler-bind ((condition #'continue))
    (defun nil (stream char) (declare (ignore stream char)) 'foo!))
(with-test (:name :set-macro-char-lazy-coerce-to-fun)
  (set-macro-character #\$ #'nil) ; #'NIL is a function
  (assert (eq (read-from-string "$") 'foo!))

  (make-dispatch-macro-character #\$)
  (assert (set-dispatch-macro-character #\$ #\( 'read-metavar))
  (assert (eq (get-dispatch-macro-character #\$ #\() 'read-metavar))
  (assert (eq (handler-case (read-from-string "$(x)")
                (undefined-function (c)
                  (if (eq (cell-error-name c) 'read-metavar) :win)))
              :win))
  (defun read-metavar (stream subchar arg)
    (declare (ignore subchar arg))
    (list :metavar (read stream t nil t)))
  (assert (equal (read-from-string "$(x)") '(:metavar x)))

  (set-macro-character #\$ nil) ; 'NIL never designates a function
  (assert (eq (read-from-string "$") '$))

  ;; Do not accept extended-function-designators.
  ;; (circumlocute to prevent a compile-time error)
  (let ((designator (eval ''(setf no-no-no))))
    (assert (eq (handler-case (set-macro-character #\$ designator)
                  (type-error () :ok))
                :ok))
    (assert (eq (handler-case
                    (set-dispatch-macro-character #\# #\$ designator)
                  (type-error () :ok))
                :ok))))

(defun cl-user::esoteric-load-thing ()
  ;; This LOAD-AS-SOURCE will fail if SET reads as the keyword :SET
  (let ((s (make-string-input-stream
            "(cl:in-package :cl-user) (set 'okey-dokey 3)")))
    (let ((*package* *package*))
      (sb-impl::load-as-source s :print nil :verbose nil))
    (assert (eql (symbol-value 'cl-user::okey-dokey) 3))))

(with-test (:name :reader-package-in-conditional)
  ;; Sharp-plus binds *package* but not *reader-package* so that if,
  ;; while reading the conditional expression itself, a read-time eval occurs
  ;; expressly changing *package*, it should do whan you mean,
  ;; though such usage is a little insane.
  (let ((value
         (read-from-string
          "(#+#.(cl:progn (cl-user::esoteric-load-thing) 'sbcl) hiyya hoho)")))
    (assert (equal value '(hiyya hoho)))))

#+sb-unicode
(with-test (:name :unicode-dispatch-macros)
  ;; Smoke test: (set-syntax-from-char unicode-char ordinary-constituent-char)
  ;; should not fail
  (set-syntax-from-char (code-char 300) #\a)
  ;;
  (let ((*readtable* (copy-readtable)))
    (make-dispatch-macro-character (code-char #x266F)) ; musical sharp
    (set-dispatch-macro-character
     (code-char #x266F) (code-char #x221E) ; #\Infinity
     (lambda (stream char arg)
       (declare (ignore stream char arg))
       :infinity))
    (let ((x (read-from-string
              (map 'string #'code-char '(#x266F #x221E)))))
      (assert (eq x :infinity))
      ;; I don't know what this was testing, and it's "noisy". Can we fix that?
      ;; I think we used to treat NIL as *removing* the macro function, which is not
      ;; a specified action. But neither could NIL ever be a function designator.
      (set-dispatch-macro-character (code-char #x266F) (code-char #x221E) nil)
      (assert (zerop (hash-table-count
                      (cdr (sb-impl::%dispatch-macro-char-table
                            (get-macro-character (code-char #x266F)))))))))

  (let ((*readtable* (copy-readtable)))
    (make-dispatch-macro-character (code-char #xbeef))
    (set-dispatch-macro-character (code-char #xbeef) (code-char #xf00d)
                                  'beef-f00d)
    (set-dispatch-macro-character (code-char #xbeef) (code-char #xd00d)
                                  'beef-d00d)
    (set-syntax-from-char (code-char #xfeed) (code-char #xbeef)
                          *readtable* *readtable*)
    (assert (eq (get-dispatch-macro-character (code-char #xfeed)
                                              (code-char #xf00d))
                'beef-f00d))
    (set-dispatch-macro-character (code-char #xfeed) (code-char #xf00d)
                                  'read-feed-food)
    (assert (eq (get-dispatch-macro-character (code-char #xbeef)
                                              (code-char #xf00d))
                'beef-f00d))
    (set-dispatch-macro-character (code-char #xbeef) #\W 'read-beef-w)
    (assert (null (get-dispatch-macro-character (code-char #xfeed) #\W)))
    (set-syntax-from-char (code-char #xbeef) #\a)
    (set-syntax-from-char (code-char #xfeed) #\b)
    (set-syntax-from-char (code-char 35) #\a) ; sharp is dead
    (assert (null (sb-impl::dispatch-tables *readtable*))))

  ;; Ensure the interface provided for named-readtables remains somewhat intact.
  (let ((*readtable* (copy-readtable)))
    (make-dispatch-macro-character #\@)
    (set-dispatch-macro-character #\@ #\a 'read-at-a)
    (set-dispatch-macro-character #\@ #\$ 'read-at-dollar)
    (set-dispatch-macro-character #\@ #\* #'sb-impl::sharp-star)
    ;; Enter exactly one character in the Unicode range because
    ;; iteratation order is arbitrary and assert would be fragile.
    ;; ASCII characters are naturally ordered by code.
    (set-dispatch-macro-character #\@ (code-char #x2010) 'read-blah)
    (let ((rt (copy-readtable *readtable*)))
      ;; Don't want to assert about all the standard noise,
      ;; and also don't want to kill the ability to write #\char
      (set-syntax-from-char #\# #\a rt)
      (assert (equal (sb-impl::dispatch-tables rt nil)
                     `((#\@ (#\A . read-at-a)
                            (#\* . ,#'sb-impl::sharp-star)
                            (#\$ . read-at-dollar)
                            (#\hyphen . read-blah))))))
    ;; this removes one entry rather than entering NIL in the hashtable
    (set-dispatch-macro-character #\@ (code-char #x2010) nil)
    (let ((rt (copy-readtable *readtable*)))
      (set-syntax-from-char #\# #\a rt)
      (assert (equal (sb-impl::dispatch-tables rt nil)
                     `((#\@ (#\A . read-at-a)
                            (#\* . ,#'sb-impl::sharp-star)
                            (#\$ . read-at-dollar))))))))

(with-test (:name :copy-dispatching-macro)
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\$ (get-macro-character #\#) t)
    (let ((foo (read-from-string "$(a b c)")))
      (assert (equalp foo #(a b c))))
    (set-dispatch-macro-character #\$ #\[
      (lambda (stream char arg)
        (declare (ignore char arg))
        (append '(:start) (read-delimited-list #\] stream t) '(:end))))
    (set-syntax-from-char #\] #\))
    (let ((foo (read-from-string "$[a b c]")))
      (assert (equal foo '(:start a b c :end))))
    ;; dispatch tables get shared. This behavior is SBCL-specific.
    (let ((foo (read-from-string "#[a b c]")))
      (assert (equal foo '(:start a b c :end))))))

;;; THIS SHOULD BE LAST as it frobs the standard readtable
(with-test (:name :set-macro-character-nil)
  (handler-bind ((sb-int:standard-readtable-modified-error #'continue))

    (let ((fun (lambda (&rest args) (declare (ignore args)) 'ok)))
      ;; NIL means the standard readtable.
      (assert (eq t (set-macro-character #\~ fun nil nil)))
      (assert (eq fun (get-macro-character #\~ nil)))
      (assert (eq t (set-dispatch-macro-character #\# #\~ fun nil)))
      (assert (eq fun (get-dispatch-macro-character #\# #\~ nil))))))

;;; success
