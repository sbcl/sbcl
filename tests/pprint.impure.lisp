;;;; test of the pretty-printer

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

(in-package :cl-user)

;;;; Assert that entries inserted into a dispatch table with equal priority
;;;; are order-preserving - unless they are of the form (CONS (EQL x)).
;;;; This is not a requirement in general, but is quite reasonable.
(with-test (:name :pprint-dispatch-order-preserving)
  (let ((tbl (sb-pretty::make-pprint-dispatch-table)))
    (handler-bind ((warning #'muffle-warning)) ; nonexistent types
      (set-pprint-dispatch 'foo1 #'pprint-fill 5 tbl)
      (set-pprint-dispatch 'fool #'pprint-fill 0 tbl)
      (set-pprint-dispatch 'foo2 #'pprint-fill 5 tbl))
    (let ((entries (sb-pretty::pprint-dispatch-table-entries tbl)))
      (assert (equal (mapcar #'sb-pretty::pprint-dispatch-entry-type entries)
                     '(foo1 foo2 fool))))))

;;;; tests for former BUG 99, where pretty-printing was pretty messed
;;;; up, e.g. PPRINT-LOGICAL-BLOCK - because of CHECK-FOR-CIRCULARITY
;;;; - didn't really work:
;;;;   "DESCRIBE interacts poorly with *PRINT-CIRCLE*, e.g. the output from
;;;;    (let ((*print-circle* t)) (describe (make-hash-table)))
;;;;  is weird, [...] #<HASH-TABLE :TEST EQL :COUNT 0 {90BBFC5}> is an . (EQL)
;;;; ..."
;;;; So, this was mainly a pretty printing problem.

;;; Create a circular list.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *circ-list* '(1 1))
  (prog1 nil
    (setf (cdr *circ-list*) *circ-list*)))

;;; I think this test is bogus. PPRINT-LOGICAL-BLOCK needs to print
;;; the #1= and mark *CIRC-LIST* as having been printed for the first
;;; time. After that any attempt to print *CIRC-LIST* must result in
;;; in a #1# being printed. Thus the right output is (for once)
;;; #1=#1#. -- JES, 2005-06-05
#+nil
;;; circular lists are still being printed correctly?
(assert (equal
         (with-output-to-string (*standard-output*)
           (let ((*print-circle* t))
             (pprint-logical-block (*standard-output* *circ-list*)
                                 (format *standard-output* "~S" *circ-list*))))
         "#1=(1 . #1#)"))

;;; test from CLHS
(with-test (:name :pprint-clhs-example)
  (assert (equal
           (with-output-to-string (*standard-output*)
             (let ((a (list 1 2 3)))
               (setf (cdddr a) a)
               (let ((*print-circle* t))
                 (write a :stream *standard-output*))
               :done))
           "#1=(1 2 3 . #1#)")))

(with-test (:name (:pprint :bug-99))
  (assert (equal
           (with-output-to-string (*standard-output*)
             (let* ((*print-circle* t))
               (format *standard-output* "~@<~S ~_is ~S. This was not seen!~:>"
                       'eql 'eql)))
           "EQL is EQL. This was not seen!"))

  (assert (equal
           (with-output-to-string (*standard-output*)
             (let* ((*print-circle* t))
               (format *standard-output*
                       "~@<~S ~_is ~S and ~S. This was not seen!~:>"
                       'eql 'eql 'eql)))
           "EQL is EQL and EQL. This was not seen!")))

;;; the original test for BUG 99 (only interactive), no obvious
;;; way to make an automated test:
;;;  (LET ((*PRINT-CIRCLE* T)) (DESCRIBE (MAKE-HASH-TABLE)))

;;; bug 263: :PREFIX, :PER-LINE-PREFIX and :SUFFIX arguments of
;;; PPRINT-LOGICAL-BLOCK may be complex strings
(with-test (:name :pprint-logical-block-arguments-complex-strings)
  (let ((list '(1 2 3))
        (prefix (make-array 2
                            :element-type 'character
                            :displaced-to ";x"
                            :fill-pointer 1))
        (suffix (make-array 2
                            :element-type 'character
                            :displaced-to ">xy"
                            :displaced-index-offset 1
                            :fill-pointer 1)))
    (assert (equal (with-output-to-string (s)
                     (pprint-logical-block (s list
                                              :per-line-prefix prefix
                                              :suffix suffix)
                       (format s "~{~W~^~:@_~}" list)))
                   (format nil ";1~%~
                              ;2~%~
                              ;3x")))))

;;; bug 141b: not enough care taken to disambiguate ,.FOO and ,@FOO
;;; from , .FOO and , @FOO
(with-test (:name :pprint-backquote-magic)
  (assert (equal
           (with-output-to-string (s)
             (write '`(,  .foo) :stream s :pretty t :readably t))
           "`(, .FOO)"))
  (assert (equal
           (with-output-to-string (s)
             (write '`(,  @foo) :stream s :pretty t :readably t))
           "`(, @FOO)"))
  (assert (equal
           (with-output-to-string (s)
             (write '`(,  ?foo) :stream s :pretty t :readably t))
           "`(,?FOO)")))

;;; bug reported by Paul Dietz on sbcl-devel: unquoted lambda lists
;;; were leaking the SB-IMPL::BACKQ-COMMA implementation.
(with-test (:name :pprint-leaking-backq-comma)
  (assert (equal
           (with-output-to-string (s)
             (write '`(foo ,x) :stream s :pretty t :readably t))
           "`(FOO ,X)"))
  (assert (equal
           (with-output-to-string (s)
             (write '`(foo ,@x) :stream s :pretty t :readably t))
           "`(FOO ,@X)"))
  (assert (equal
           (with-output-to-string (s)
             (write '`(foo ,.x) :stream s :pretty t :readably t))
           "`(FOO ,.X)"))
  (assert (equal
           (with-output-to-string (s)
             (write '`(lambda ,x) :stream s :pretty t :readably t))
           "`(LAMBDA ,X)"))
  (assert (equal
           (with-output-to-string (s)
             (write '`(lambda ,@x) :stream s :pretty t :readably t))
           "`(LAMBDA ,@X)"))
  (assert (equal
           (with-output-to-string (s)
             (write '`(lambda ,.x) :stream s :pretty t :readably t))
           "`(LAMBDA ,.X)"))
  (assert (equal
           (with-output-to-string (s)
             (write '`(lambda (,x)) :stream s :pretty t :readably t))
           "`(LAMBDA (,X))")))

(defun unwhitespaceify (string)
  (let ((string (substitute #\Space #\Newline string)))
    ;; highly inefficient. this is not how you'd do this in real life.
    (loop (let ((p (search "  " string)))
            (when (not p) (return string))
            (setq string
                  (concatenate 'string
                               (subseq string 0 p)
                               (subseq string (1+ p))))))))

;;; more backquote printing brokenness, fixed quasi-randomly by CSR.
;;; and fixed a little more by DPK.
(with-test (:name :pprint-more-backquote-brokeness)
  (flet ((try (input expect)
           (assert (equalp (read-from-string expect) input))
           (let ((actual (unwhitespaceify (write-to-string input :pretty t))))
             (unless (equal actual expect)
             (error "Failed test for ~S. Got ~S~%"
                    expect actual)))))
    (try '``(foo ,@',@bar) "``(FOO ,@',@BAR)")
    (try '``(,,foo ,',foo foo) "``(,,FOO ,',FOO FOO)")
    (try '``(((,,foo) ,',foo) foo) "``(((,,FOO) ,',FOO) FOO)")
    (try '`#() "`#()")
    (try '`#(,bar) "`#(,BAR)")
    (try '`#(,(bar)) "`#(,(BAR))")
    (try '`#(,@bar) "`#(,@BAR)")
    (try '`#(,@(bar)) "`#(,@(BAR))")
    (try '`#(a ,b c) "`#(A ,B C)")
    (try '`#(,@A ,b c) "`#(,@A ,B C)")
    (try '`(,a . #(foo #() #(,bar) ,bar)) "`(,A . #(FOO #() #(,BAR) ,BAR))")
    (try '(let ((foo (x))) `(let (,foo) (setq ,foo (y)) (baz ,foo)))
           "(LET ((FOO (X))) `(LET (,FOO) (SETQ ,FOO (Y)) (BAZ ,FOO)))")
    (try '(let `((,a ,b)) :forms) "(LET `((,A ,B)) :FORMS)")
    (try '(lambda `(,x ,y) :forms) "(LAMBDA `(,X ,Y) :FORMS)")
    (try '(defun f `(,x ,y) :forms) "(DEFUN F `(,X ,Y) :FORMS)")))


;;; SET-PPRINT-DISPATCH should accept function name arguments, and not
;;; rush to coerce them to functions.
(set-pprint-dispatch '(cons (eql frob)) 'ppd-function-name)
(defun ppd-function-name (s o)
  (print (length o) s))

(with-test (:name (:set-pprint-dispatch :no-function-coerce)))
(let ((s (with-output-to-string (s)
           (pprint '(frob a b) s))))
  (assert (position #\3 s)))

;; Test that circularity detection works with pprint-logical-block
;; (including when called through pprint-dispatch).
(with-test (:name :pprint-circular-detection)
  (let ((*print-pretty* t)
        (*print-circle* t)
        (*print-pprint-dispatch* (copy-pprint-dispatch)))
    (labels ((pprint-a (stream form &rest rest)
               (declare (ignore rest))
               (pprint-logical-block (stream form :prefix "<" :suffix ">")
                 (pprint-exit-if-list-exhausted)
                 (loop
                   (write (pprint-pop) :stream stream)
                   (pprint-exit-if-list-exhausted)
                   (write-char #\space stream)))))
      (set-pprint-dispatch '(cons (eql a)) #'pprint-a)
      (assert (string= "<A 1 2 3>"
                       (with-output-to-string (s)
                         (write '(a 1 2 3) :stream s))))
      (assert (string= "#1=<A 1 #1# #2=#(2) #2#>"
                       (with-output-to-string (s)
                         (write '#2=(a 1 #2# #5=#(2) #5#) :stream s))))
      (assert (string= "#1=(B #2=<A 1 #1# 2 3> #2#)"
                       (with-output-to-string (s)
                         (write '#3=(b #4=(a 1 #3# 2 3) #4#) :stream s)))))))

;; Test that a circular improper list inside a logical block works.
(with-test (:name :pprint-circular-improper-lists-inside-logical-blocks)
  (let ((*print-circle* t)
        (*print-pretty* t))
    (assert (string= "#1=(#2=(#2# . #3=(#1# . #3#)))"
                     (with-output-to-string (s)
                       (write '#1=(#2=(#2# . #3=(#1# . #3#))) :stream s))))))

;;; Printing malformed defpackage forms without errors.
(with-test (:name :pprint-defpackage)
  (let ((*standard-output* (make-broadcast-stream)))
    (pprint '(defpackage :foo nil))
    (pprint '(defpackage :foo 42))))

(with-test (:name :standard-pprint-dispatch-modified)
  (assert
   (eq :error
       (handler-case (with-standard-io-syntax
                       (set-pprint-dispatch 'symbol (constantly nil))
                       :no-error)
         (sb-int:standard-pprint-dispatch-table-modified-error ()
           :error)))))

(defun pprint-to-string (form)
  (let ((string (with-output-to-string (s) (pprint form s))))
    (assert (eql #\newline (char string 0)))
    (subseq string 1)))
(with-test (:name :pprint-defmethod-lambda-list-function)
  (assert (equal "(DEFMETHOD FOO ((FUNCTION CONS)) FUNCTION)"
                 (pprint-to-string `(defmethod foo ((function cons)) function))))
  (assert (equal "(DEFMETHOD FOO :AFTER (FUNCTION CONS) FUNCTION)"
                 (pprint-to-string `(defmethod foo :after (function cons) function))))
  (assert (equal "(DEFMETHOD FOO :BEFORE ((FUNCTION (EQL #'FOO))) FUNCTION)"
                 (pprint-to-string `(DEFMETHOD FOO :BEFORE ((FUNCTION (EQL #'FOO))) FUNCTION)))))

(with-test (:name :pprint-lambda-list-quote)
  (assert (equal "(LAMBDA (&KEY (BAR 'BAZ)))"
                 (pprint-to-string '(lambda (&key (bar 'baz)))))))

(defclass frob () ())

(defmethod print-object ((obj frob) stream)
  (print-unreadable-object (obj stream :type nil :identity nil)
    (format stream "FRABOTZICATOR")))

;;; SBCL < 1.0.38 printed #<\nFRABOTIZICATOR>
(with-test (:name (:pprint-unreadable-object :no-ugliness-when-type=nil))
  (assert (equal "#<FRABOTZICATOR>"
                 (let ((*print-right-margin* 5)
                       (*print-pretty* t))
                   (format nil "~@<~S~:>" (make-instance 'frob))))))

(with-test (:name :pprint-logical-block-code-deletion-node
                  :skipped-on '(not :stack-allocatable-closures))
  (handler-case
      (compile nil
               `(lambda (words &key a b c)
                  (pprint-logical-block (nil words :per-line-prefix (or a b c))
                    (pprint-fill *standard-output* (sort (copy-seq words) #'string<) nil))))
    ((or sb-ext:compiler-note warning) (c)
      (error "~A" c))))

(with-test (:name :pprint-logical-block-multiple-per-line-prefix-eval)
  (funcall (compile nil
                    `(lambda ()
                       (let ((n 0))
                         (with-output-to-string (s)
                           (pprint-logical-block (s nil :per-line-prefix (if (eql 1 (incf n))
                                                                             "; "
                                                                             (error "oops")))
                             (pprint-newline :mandatory s)
                             (pprint-newline :mandatory s)))
                         n)))))

(with-test (:name :can-restore-orig-pprint-dispatch-table)
  (let* ((orig (pprint-dispatch 'some-symbol))
         (alt (lambda (&rest args) (apply orig args))))
    (set-pprint-dispatch 'symbol alt)
    (assert (eq alt (pprint-dispatch 'some-symbol)))
    (setf *print-pprint-dispatch* (copy-pprint-dispatch nil))
    (assert (eq orig (pprint-dispatch 'some-symbol)))
    (assert (not (eq alt orig)))))

(with-test (:name :pprint-improper-list)
  (let* ((max-length 10)
         (stream (make-broadcast-stream))
         (errors
           (loop for symbol being the symbol in :cl
                 nconc
                 (loop for i from 1 below max-length
                       for list = (cons symbol 10) then (cons symbol list)
                       when (nth-value 1 (ignore-errors (pprint list stream)))
                       collect (format nil "(~{~a ~}~a . 10)" (butlast list) symbol)))))
    (when errors
      (error "Can't PPRINT improper lists: ~a" errors))))

(with-test (:name :pprint-circular-backq-comma)
  ;; LP 1161218 reported by James M. Lawrence
  (let ((string (write-to-string '(let ((#1=#:var '(99)))
                                   `(progn ,@(identity #1#)))
                                 :circle t :pretty t)))
    (assert (not (search "#2#" string)))))

(with-test (:name :pprint-dotted-setf)
  (let ((*print-pretty* t))
    (equal (format nil "~a" '(setf . a))
           "(SETF . A)")))

(with-test (:name :literal-fun-nested-lists)
  (assert (search "EQUALP" (format nil "~:w" `((((((,#'equalp)))))))
                  :test #'char-equal)))

(defvar *a* (make-array 3 :fill-pointer 0))
(with-test (:name :pprint-logical-block-eval-order)
  (flet ((vp (x) (vector-push x *a*)))
    (pprint-logical-block (nil (progn (vp 1) '(foo))
                               :suffix (progn (vp 2) "}")
                               :prefix (progn (vp 3) "{"))
      (write (pprint-pop))))
  (assert (equalp *a* #(1 2 3))))

;; these warn, but "work" in as much as they don't kill the machinery
(with-test (:name (:pprint-dispatch :set-ppd-unknown-type))
  (handler-bind ((warning #'muffle-warning))
    (assert-signal
     (set-pprint-dispatch 'frood
                          (lambda (stream obj)
                            (let ((*print-pretty* nil))
                              (format stream "[frood: ~D]" obj))))
     warning)
    ;; We expect multiple warnings since the type specifier references
    ;; multiple undefined things.
    (assert-signal
     (set-pprint-dispatch '(or weasel (and woodle (satisfies thing)))
                          (lambda (stream obj)
                            (format stream "hi ~A!" (type-of obj))))
     warning 2)
    (write-to-string (macroexpand '(setf (values a b) (floor x y)))
                     :pretty t)
    ;; yay, we're not dead
    ))

(with-test (:name (:pprint-dispatch :unknown-type-1a))
  (assert (string= (write-to-string (list 1 2 3 1006) :pretty t)
                   "(1 2 3 1006)")))
(deftype frood () '(integer 1005 1006))
(with-test (:name (:pprint-dispatch :unknown-type-1b))
  (assert (string= (write-to-string (list 1 2 3 1006) :pretty t)
                   "(1 2 3 [frood: 1006])")))
(defstruct weasel)
(with-test (:name (:pprint-dispatch :unknown-type-2a))
  ;; still can't use the dispatch entry because of the OR
  ;; even though WEASEL "could have" eagerly returned T.
  (assert (string= (write-to-string (make-weasel) :pretty t)
                   "#S(WEASEL)")))
(defstruct woodle)
(with-test (:name (:pprint-dispatch :unknown-type-2b))
  ;; still no, because #'THING is not boundp
  (assert (string= (write-to-string (make-weasel) :pretty t)
                   "#S(WEASEL)"))
  (defun thing (x) x)
  (assert (string= (write-to-string (make-weasel) :pretty t)
                   "hi WEASEL!")))

(deftype known-cons ()
  '(cons (member known-cons other-known-cons other-other)))
(with-test (:name (:pprint-dispatch :known-cons-type))
  (flet ((pprint-known-cons (stream obj)
           (format stream "#<KNOWN-CONS ~S>" (cdr obj))))
    (set-pprint-dispatch 'known-cons #'pprint-known-cons))
  (let ((hashtable (sb-pretty::pprint-dispatch-table-cons-entries
                    *print-pprint-dispatch*)))
    ;; First ensure that the CONS table was used
    (assert (gethash 'known-cons hashtable))
    ;; Check that dispatch entries are shared. In practice it is not "useful"
    ;; but it is a consequence of the general approach of allowing any MEMBER
    ;; type versus disallowing MEMBER types of more than one element.
    (assert (eq (gethash 'known-cons hashtable)
                (gethash 'other-known-cons hashtable)))
    (assert (eq (gethash 'known-cons hashtable)
                (gethash 'other-other hashtable))))
  (assert (string= (write-to-string (cons 'known-cons t) :pretty t)
                   "#<KNOWN-CONS T>"))
  (assert (string= (write-to-string (cons 'known-cons (cons 'known-cons t)) :pretty t)
                   "#<KNOWN-CONS #<KNOWN-CONS T>>")))

;; force MACDADDY to be a closure over X.
(let ((x 3)) (defmacro macdaddy (a b &body z) a b z `(who-cares ,x)) (incf x))

(with-test (:name :closure-macro-arglist)
  ;; assert correct test setup - MACDADDY is a closure if compiling,
  ;; or a funcallable-instance if not
  (assert (eq (sb-kernel:fun-subtype (macro-function 'macdaddy))
              #-interpreter sb-vm:closure-header-widetag
              #+interpreter sb-vm:funcallable-instance-header-widetag))
  ;; MACRO-INDENTATION used %simple-fun-arglist instead of %fun-arglist.
  ;; Depending on your luck it would either not return the right answer,
  ;; or crash, depending on what lay at 4 words past the function address.
  (assert (= (sb-pretty::macro-indentation 'macdaddy) 2)))

(defmacro try1 (a b &body fool) `(baz ,a ,b ,fool))
(defmacro try2 (a b &optional &body fool) `(baz ,a ,b ,fool))
(defmacro try3 (a b &optional c &body fool) `(baz ,a ,b ,c ,fool))
(defmacro try4 (a b . fool) `(baz ,a ,b ,fool))
(defmacro try5 (a b &optional . fool) `(baz ,a ,b ,fool))
(defmacro try6 (a b &optional c . fool) `(baz ,a ,b ,c ,fool))
(with-test (:name :macro-indentation)
  (assert (= (sb-pretty::macro-indentation 'try1) 2))
  (assert (= (sb-pretty::macro-indentation 'try2) 2))
  (assert (= (sb-pretty::macro-indentation 'try3) 3))
  (assert (= (sb-pretty::macro-indentation 'try4) 2))
  (assert (= (sb-pretty::macro-indentation 'try5) 2))
  (assert (= (sb-pretty::macro-indentation 'try6) 3)))

;;; success
