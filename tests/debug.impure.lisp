;;;; This file is for testing debugging functionality, using
;;;; test machinery which might have side effects (e.g.
;;;; executing DEFUN).

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

(cl:in-package :cl-user)

;;; The debugger doesn't have any native knowledge of the interpreter
(when (eq sb-ext:*evaluator-mode* :interpret)
  (sb-ext:exit :code 104))

#+(or x86 x86-64)
(with-test (:name :legal-bpt-lra-object)
  (sb-int:binding* ((code (sb-kernel:fun-code-header #'read))
                    (sap (sb-sys:sap+ (sb-kernel:code-instructions code) 13)) ; random
                    ((bpt-sap bpt-code-obj) (sb-di::make-bpt-lra sap)))
    (declare (ignore bpt-sap))
    ;; This was causing heap corruption
    (assert (zerop (sb-kernel:code-jump-table-words bpt-code-obj)))))


;;;; Check that we get debug arglists right.

(defun zoop (zeep &key beep)
  (declare (ignore zeep beep) (special blurp))
  blurp)
(assert (equal (sb-kernel:%fun-lambda-list #'zoop) '(zeep &key beep)))

;;; Check some predefined functions too.
;;;
;;; (We don't know exactly what the arguments are, e.g. the first
;;; argument of PRINT might be SB-IMPL::OBJECT or SB-KERNEL::OBJ or
;;; whatever. But we do know the general structure that a correct
;;; answer should have, so we can safely do a lot of checks.)
(with-test (:name :predefined-functions-1)
  (destructuring-bind (object-sym andoptional-sym stream-sym)
      (sb-kernel:%fun-lambda-list #'print)
    (assert (symbolp object-sym))
    (assert (eql andoptional-sym '&optional))
    (assert (symbolp stream-sym))))

(with-test (:name :predefined-functions-2)
  (destructuring-bind (dest-sym control-sym andrest-sym format-args-sym)
      (sb-kernel:%fun-lambda-list #'format)
    (assert (symbolp dest-sym))
    (assert (symbolp control-sym))
    (assert (eql andrest-sym '&rest))
    (assert (symbolp format-args-sym))))

;;;; test TRACE

(defmacro with-traced-function ((name &rest options) &body body)
  `(with-output-to-string (*trace-output*)
     (unwind-protect
          (progn
            (trace ,name ,@options)
            ,@body)
       (ignore-errors (untrace ,name)))))

(defun trace-this (&optional arg)
  (declare (ignore arg))
  'ok)

(defun trace-fact (n)
  (if (zerop n)
      1
      (* n (trace-fact (1- n)))))

(with-test (:name (trace :simple))
  (let ((output (with-traced-function (trace-this)
                  (assert (eq 'ok (trace-this))))))
    (assert (search "TRACE-THIS" output))
    (assert (search "returned OK" output))))

(with-test (:name (trace :print-readably))
  (let ((output (with-traced-function (trace-this)
                  (let ((*print-readably* t))
                    (assert (eq 'ok (trace-this (sb-int:make-unprintable-object "foo"))))))))
    (assert (search "TRACE-THIS" output))
    (assert (search "foo" output))
    (assert (search "returned OK" output))))

;;; The following should not work:
;;;  (DEFUN G () (M))
;;;  (DEFMACRO M (&REST r) (format t "M invoked: ~S~%" r))
;;;  (ENCAPSULATE 'M 'foo (lambda (f &rest r) (format t "encapsulation: ~S ~S~%" f r)))
;;;  (G)
;;; If (ENCAPSULATE) were permitted, M's guard trampoline would be replaced by the
;;; encapsulation which is not meaningful. Most uses of the macro M will NOT invoke
;;; the encapsulation since we don't store macro functions in the symbol-function location.
;;; Only a consumer of M being accidentally compiled first and resolving M to an
;;; fdefinition would see the encapulation. That should just be an error.
(with-test (:name :no-macro-encapsulation)
  (assert-error (sb-int:encapsulate 'cond 'tryme
                                    (lambda (f &rest stuff)
                                      (declare (ignore f stuff))))))

;;; bug 379
(with-test (:name (trace :encapsulate nil)
            :fails-on (or (and :ppc (not :linux)) :sparc :arm64)
            :broken-on (or :freebsd))
  (let ((output (with-traced-function (trace-this :encapsulate nil)
                  (assert (eq 'ok (trace-this))))))
    (assert (search "TRACE-THIS" output))
    (assert (search "returned OK" output))))

(with-test (:name (trace :encapsulate nil :recursive)
            :fails-on (or (and :ppc (not :linux)) :sparc :arm64)
            :broken-on (or :freebsd))
  (let ((output (with-traced-function (trace-fact :encapsulate nil)
                  (assert (= 120 (trace-fact 5))))))
    (assert (search "TRACE-FACT" output))
    (assert (search "returned 1" output))
    (assert (search "returned 120" output))))

(defun trace-and-fmakunbound-this (x)
  x)

(with-test (:name (trace fmakunbound :bug-667657))
  (trace trace-and-fmakunbound-this)
  (fmakunbound 'trace-and-fmakunbound-this)
  ;; FIXME: this generates a pointless warning that we can't untrace a formerly
  ;; traced function. Function redefinition knows to untrace/re-trace because of
  ;; the setf-fdefn hook. fmakunbound can do something similar - drop the trace.
  (untrace)
  (assert (not (trace))))

(with-test (:name (trace :report nil :smoke))
  (let ((output (with-traced-function (trace-this :report nil)
                  (assert (eq 'ok (trace-this))))))
    (assert (sequence:emptyp output))))

(with-test (:name (trace :report nil :print))
  (let ((output (with-traced-function
                    (trace-fact :report nil :print (sb-debug:arg 0))
                  (assert (eq '2 (trace-fact 2))))))
    (assert (string= output (format nil "2~@
                                         1~@
                                         0~%")))))

(with-test (:name :bug-414)
  (handler-bind ((warning #'error))
    (with-scratch-file (output "fasl")
      (load (compile-file "bug-414.lisp" :output-file output
                                         :verbose nil :print nil)))
    (with-output-to-string (s)
      (disassemble 'bug-414 :stream s))))

;; A known function can be stored as a code constant in lieu of the
;; usual mode of storing an #<fdefn> and looking up the function from it.
;; One such usage occurs with TAIL-CALL-VARIABLE (e.g. via APPLY).
;; Show that declaring the function locally notinline uses the #<fdefn>
;; by first compiling a call that would have elided the #<fdefn>
;; and then TRACE.
;; XXX: what purpose has the JUNK argument?
(defun test-compile-then-load (filename junk)
  (declare (notinline compile-file load))
  (with-scratch-file (output "fasl")
    (apply 'load (apply 'compile-file filename :output-file output junk)
           junk)))
(compile 'test-compile-then-load)
(with-test (:name :traceable-known-fun)
  (let ((s (make-string-output-stream)))
    (trace compile-file load)
    (let ((*trace-output* s))
      (test-compile-then-load "bug-414.lisp" nil))
    (untrace)
    (assert (>= (count #\Newline (get-output-stream-string s)) 4))))

(with-test (:name :bug-310175)
  ;; KLUDGE: Not all DX-enabled platforms DX CONS, and the compiler
  ;; transforms two-arg-LIST* (and one-arg-LIST) to CONS.  Therefore,
  ;; use two-arg-LIST, which should get through to VOP LIST, and thus
  ;; stack-allocate on a predictable set of machines.
  (let ((dx-arg (list t t)))
    (declare (dynamic-extent dx-arg))
    (flet ((dx-arg-backtrace (x)
             (declare (optimize (debug 2)))
             (prog1 (sb-debug:list-backtrace :count 10)
               (assert (sb-debug::stack-allocated-p x)))))
      (declare (notinline dx-arg-backtrace))
      (assert (member-if (lambda (frame)
                           (and (consp frame)
                                (consp (car frame))
                                (equal '(flet dx-arg-backtrace :in) (butlast (car frame)))
                                (notany #'sb-debug::stack-allocated-p (cdr frame))))
                         (dx-arg-backtrace dx-arg))))))

(with-test (:name :bug-795245)
  (assert
   (eq :ok
       (catch 'done
         (handler-bind
             ((error (lambda (e)
                       (declare (ignore e))
                       (handler-case
                           (sb-debug:print-backtrace :count 100
                                                     :stream (make-broadcast-stream))
                         (error ()
                           (throw 'done :error))
                         (:no-error ()
                           (throw 'done :ok))))))
           (apply '/= nil 1 2 nil))))))

;;;; test infinite error protection

(defmacro nest-errors (n-levels error-form)
  (if (< 0 n-levels)
      `(handler-bind ((error (lambda (condition)
                               (declare (ignore condition))
                               ,error-form)))
        (nest-errors ,(1- n-levels) ,error-form))
      error-form))

(defun erroring-debugger-hook (condition old-debugger-hook)
  (let ((*debugger-hook* old-debugger-hook))
    (format t "recursive condition: ~A~%" condition) (force-output)
    (error "recursive condition: ~A" condition)))

(defun test-infinite-error-protection ()
  ;; after 50 successful throws to SB-IMPL::TOPLEVEL-CATCHER sbcl used
  ;; to halt, it produces so much garbage that's hard to suppress that
  ;; it is tested only once
  (write-line "--HARMLESS BUT ALARMING BACKTRACE COMING UP--")
  (let ((*debugger-hook* #'erroring-debugger-hook))
    (loop repeat 1 do
          (let ((error-counter 0)
                (*terminal-io* (make-broadcast-stream)))
            (assert
             (not (eq
                   :normal-exit
                   (catch 'sb-impl::toplevel-catcher
                     (nest-errors 20 (error "infinite error ~s"
                                            (incf error-counter)))
                     :normal-exit)))))))
  (write-line "--END OF H-B-A-B--"))

;;; *debugger-hook* is now cleared after trying to enter the debugger
;;; *once in ERROR-ERROR, breaking these tests.
(with-test (:name :infinite-error-protection
            :skipped-on :sbcl)
  (enable-debugger)
  (test-infinite-error-protection))

(with-test (:name (:infinite-error-protection :thread)
            :skipped-on (or :sbcl (not :sb-thread)))
  (enable-debugger)
  (let ((thread (sb-thread:make-thread #'test-infinite-error-protection)))
    (loop while (sb-thread:thread-alive-p thread))))

;; unconditional, in case either previous left it enabled
(disable-debugger)

;;;; test some limitations of MAKE-LISP-OBJ

;;; Older GENCGC systems had a bug in the pointer validation used by
;;; MAKE-LISP-OBJ that made SIMPLE-FUN objects always fail to
;;; validate.
(with-test (:name (:make-lisp-obj :simple-funs))
  (sb-sys:without-gcing
    (assert (eq #'identity
                (sb-kernel:make-lisp-obj
                 (sb-kernel:get-lisp-obj-address
                  #'identity))))))

;;; Older CHENEYGC systems didn't perform any real pointer validity
;;; checks beyond "is this pointer to somewhere in heap space".
(with-test (:name (:make-lisp-obj :pointer-validation))
  ;; Fun and games: We need to test MAKE-LISP-OBJ with a known-bogus
  ;; address, but we also need the GC to not pitch a fit if it sees an
  ;; object with said bogus address.  Thus, construct our known-bogus
  ;; object within an area of unboxed storage (a vector) in static
  ;; space.  We'll make it a simple object, (CONS 0 0), which has an
  ;; in-memory representation of two consecutive zero words.  We
  ;; allocate a three-word vector so that we can guarantee a
  ;; double-word aligned double-word of zeros no matter what happens
  ;; with the vector-data-offset (currently double-word aligned).
  (let* ((memory (sb-int:make-static-vector 3 :element-type `(unsigned-byte ,sb-vm:n-word-bits)
                                            :initial-element 0))
         (vector-data-address (sb-sys:sap-int (sb-kernel::vector-sap memory)))
         (object-base-address (logandc2 (+ vector-data-address sb-vm:lowtag-mask) sb-vm:lowtag-mask))
         (object-tagged-address (+ object-base-address sb-vm:list-pointer-lowtag)))
    (multiple-value-bind (object valid-p)
        (sb-kernel:make-lisp-obj object-tagged-address nil)
      (declare (ignore object))
      (assert (not valid-p)))))

(defun test-debugger (control form &rest targets)
  (let ((out (make-string-output-stream))
        (oops t))
    (unwind-protect
         (progn
           (with-simple-restart (debugger-test-done! "Debugger Test Done!")
             (let* ((*debug-io* (make-two-way-stream
                                 (make-string-input-stream control)
                                 (make-broadcast-stream out #+nil *standard-output*)))
                    ;; Initial announcement goes to *ERROR-OUTPUT*
                    (*error-output* *debug-io*)
                    (*invoke-debugger-hook* nil))
               (handler-bind ((error #'invoke-debugger))
                 (eval form))))
           (setf oops nil))
      (when oops
        (error "Uncontrolled unwind from debugger test.")))
    ;; For sanity's sake this is outside the *debug-io* rebinding -- otherwise
    ;; it could swallow our asserts!
    (with-input-from-string (s (get-output-stream-string out))
      (loop for line = (read-line s nil)
            while line
            do (assert targets nil "Line = ~a" line)
               #+nil
               (format *error-output* "Got: ~A~%" line)
               (let ((match (pop targets)))
                 (if (eq '* match)
                     ;; Whatever, till the next line matches.
                     (let ((text (pop targets)))
                       #+nil
                       (format *error-output* "Looking for: ~A~%" text)
                       (unless (search text line)
                         (push text targets)
                         (push match targets)))
                     (unless (search match line)
                       (format *error-output* "~&Wanted: ~S~%   Got: ~S~%" match line)
                       (setf oops t))))))
    ;; Check that we saw everything we wanted
    (when targets
      (error "Missed: ~S" targets))
    (assert (not oops))))

(with-test (:name (:debugger :source 1))
  (test-debugger
   "d
    source 0
    debugger-test-done!"
   `(progn
      (defun this-will-break (x)
               (declare (optimize debug))
               (let* ((y (- x x))
                      (z (/ x y)))
                 (+ x z)))
      (this-will-break 1))
   '*
   "debugger invoked"
   '*
   "DIVISION-BY-ZERO"
   "Operation was (/ 1 0)"
   '*
   "INTEGER-/-INTEGER"
   "(THIS-WILL-BREAK 1)"
   "1]"
   "(/ X Y)"
   "1]"))

(with-test (:name (:debugger :source 2))
  (test-debugger
   "d
    source 0
    debugger-test-done!"
   `(locally (declare (optimize (speed 0) (safety 3) (debug 3)))
      (let ((f #'(lambda (x cont)
                   (print x (make-broadcast-stream))
                   (if (zerop x)
                       (error "~%foo")
                       (funcall cont (1- x) cont)))))
        (funcall f 10 f)))
   '*
   "debugger"
   '*
   "foo"
   '*
   "source: (ERROR \"~%foo\")"
   '*
   "(LAMBDA (X CONT)"
   '*
   "(FUNCALL CONT (1- X) CONT)"
   "1]"))

(with-test (:name (:debugger :bogus-debug-fun :source))
  (test-debugger
   "d
    debugger-test-done!"
   `(let ()
      (#.(gensym)))
   '*
   "undefined function"
   '*
   "1]"))

(with-test (:name (disassemble :high-debug-eval))
  (eval `(defun this-will-be-disassembled (x)
           (declare (optimize debug))
           (+ x x)))
  (let* ((oopses (make-string-output-stream))
         (disassembly
           (let ((*error-output* oopses))
             (with-output-to-string (*standard-output*)
               (disassemble 'this-will-be-disassembled)))))
    (with-input-from-string (s disassembly)
      (assert (search "; disassembly for THIS-WILL-BE-DISASSEMBLED"
                      (read-line s))))
    (let ((problems (get-output-stream-string oopses)))
      (unless (zerop (length problems))
        (error problems)))))

(defun this-too-will-be-disasssembled (x)
  (declare (optimize debug))
  (+ x x))

(with-test (:name (disassemble :high-debug-load))
  (let* ((oopses (make-string-output-stream))
         (disassembly
           (let ((*error-output* oopses))
             (with-output-to-string (*standard-output*)
               (disassemble 'this-too-will-be-disasssembled)))))
    (with-input-from-string (s disassembly)
      (assert (equal "; disassembly for THIS-TOO-WILL-BE-DISASSSEMBLED"
                     (read-line s))))
    (let ((problems (get-output-stream-string oopses)))
      (unless (zerop (length problems))
        (error problems)))))

(with-test (:name (:xep-arglist-clean-up :bug-1192929))
  (assert
   (block nil
     (handler-bind ((error (lambda (e)
                             (declare (ignore e))
                             (return (< (length (car (sb-debug:list-backtrace :count 1)))
                                        10)))))
       (funcall (compile nil `(lambda (i) (declare ((mod 65536) i)) i)) nil)))))

;;; bug-1261646

(defun print-backtrace-to-string/debug-print-variable-alist (x)
  (values
   (with-output-to-string (stream)
     (let ((*debug-print-variable-alist* '((*print-length* . 5)
                                           (*print-level* . 3))))
       (sb-debug:print-backtrace :stream stream :count 5)))
   x)) ; Force use of X to prevent flushing

(with-test (:name (:print-frame-call :respect *debug-print-variable-alist*
                   *print-length* :bug-1261646))
  (let* ((printed (print-backtrace-to-string/debug-print-variable-alist
                   (make-array 200 :initial-element 0)))
         (call "(PRINT-BACKTRACE-TO-STRING/DEBUG-PRINT-VARIABLE-ALIST ")
         (position (+ (search call printed) (length call))))
    (assert (eql position (search "#(0 0 0 0 0 ...)" printed :start2 position)))))

(with-test (:name (:print-frame-call :respect *debug-print-variable-alist*
                   *print-level* :bug-1261646))
  (let* ((printed (print-backtrace-to-string/debug-print-variable-alist
                   '(((((1)))))))
         (call "(PRINT-BACKTRACE-TO-STRING/DEBUG-PRINT-VARIABLE-ALIST ")
         (position (+ (search call printed) (length call))))
    (assert (eql position (search "((#))" printed :start2 position)))))


(defvar *x* nil)
(defun foo (a) a)

(with-test (:name :trace-debug-arg)
  (trace foo :print-after (setf *x* (sb-debug:arg 0)))
  (foo 1)
  (assert (eql *x* 1))

  (trace foo :print (setf *x* (sb-debug:arg 0)))
  (foo 2)
  (assert (eql *x* 2))

  (trace foo :condition (eql (setf *x* (sb-debug:arg 0)) 0))
  (foo 3)
  (assert (eql *x* 3))

  (trace foo :condition-after (setf *x* (sb-debug:arg 0)))
  (foo 4)
  (assert (eql *x* 4))

  (trace foo :break (and (setf *x* (sb-debug:arg 0)) nil))
  (foo 5)
  (assert (eql *x* 5))

  (trace foo :break-all (and (setf *x* (sb-debug:arg 0)) nil))
  (foo 6)
  (assert (eql *x* 6))
  (trace foo :break-after (and (setf *x* (sb-debug:arg 0)) nil))
  (foo 7))

(defun frobbleize (arg) (declare (ignore arg)) (sb-debug:list-backtrace) 'win)
(defmethod low-debug-method ((self t))
  (declare (optimize (debug 0)))
  (frobbleize 'me) ; make this not a tail call, so it remains on stack
  5)
(with-test (:name :clean-fast-method-frame-lossage)
  (low-debug-method 42)) ; no need to assert. it either crashes or doesn't

(defun return-65535 ()
  65535)

(with-test (:name :indirect-closure-values)
  (let ((count 0))
    (block nil
      (handler-bind ((error (lambda (c)
                              (declare (ignore c))
                              (sb-debug::map-backtrace
                               (lambda (frame)
                                 (let ((sb-debug::*current-frame* frame)
                                       (name (sb-debug::frame-call frame)))
                                   (when (or (eq name 'test)
                                             (and (consp name)
                                                  (or (eql (search '(labels f1) name) 0)
                                                      (eql (search '(labels f2) name) 0))))
                                     (incf count)
                                     (assert (eql (var 'a) 2))))))
                              (return))))
        (funcall
         (compile nil
                  `(sb-int:named-lambda test ()
                     (declare (optimize debug))
                     (let ((a 1))
                       (labels
                           ((f1 ()
                              (incf a)
                              (signal 'error))
                            (f2 ()
                              (f1)))
                         (f2))))))))
    (assert (= count 3))))

(with-test (:name :indirect-closure-values.2)
  (let ((count 0))
    (block nil
      (handler-bind ((error (lambda (c)
                              (declare (ignore c))
                              (sb-debug::map-backtrace
                               (lambda (frame)
                                 (let ((sb-debug::*current-frame* frame)
                                       (name (sb-debug::frame-call frame)))
                                   (when (or (eq name 'test)
                                             (and (consp name)
                                                  (or (eql (search '(labels f1) name) 0)
                                                      (eql (search '(labels f2) name) 0))))
                                     (incf count)
                                     (assert (eql (var 'a) 65535))))))
                              (return))))
        (funcall
         (compile nil
                  `(sb-int:named-lambda test ()
                     (declare (optimize debug))
                     (let ((a (return-65535)))
                       (declare ((unsigned-byte 16) a))
                       (labels
                           ((f1 ()
                              (incf a)
                              (signal 'error))
                            (f2 ()
                              (f1)))
                         (f2))))))))
    (assert (= count 3))))

(with-test (:name :non-tail-self-call-bad-variables)
  (let ((count 0))
    (block nil
      (handler-bind ((error (lambda (c)
                              (declare (ignore c))
                              (sb-debug::map-backtrace
                               (lambda (frame)
                                 (let ((sb-debug::*current-frame* frame))
                                   (multiple-value-bind (name args)
                                       (sb-debug::frame-call frame)
                                     (when (eq name 'test)
                                       (assert (or (null args)
                                                   (equal args '(nil))))
                                       (incf count))))))
                              (return))))
        (funcall
         (compile nil `(sb-int:named-lambda test (&optional x)
                         (declare (optimize sb-c::recognize-self-calls))
                         (signal 'error :format-control "~a" :format-arguments (list x))
                         (test 1)
                         1)))))
    (assert (= count 1))))

(with-test (:name :local-tail-call-variables)
  (let ((count 0))
    (block nil
      (handler-bind ((error (lambda (c)
                              (declare (ignore c))
                              (sb-debug::map-backtrace
                               (lambda (frame)
                                 (let ((sb-debug::*current-frame* frame))
                                   (multiple-value-bind (name args)
                                       (sb-debug::frame-call frame)
                                     (when (eq name 'test)
                                       (assert (equal args '(error)))
                                       (incf count))))))
                              (return))))
        (funcall
         (compile nil `(sb-int:named-lambda test (x)
                         (signal x)
                         ;; If :local-tail-call fails, this will fail
                         ;; too, because there's no jump between
                         ;; SIGNAL and the call to TAIL and it will
                         ;; show (flet tail) in the backtrace.
                         (flet ((tail ()))
                           (declare (notinline tail))
                           (tail))))
         'error)))
    (assert (= count 1))))

(with-test (:name :variables-surrounding-inlined-code)
  (let ((count 0))
    (block nil
      (handler-bind ((error (lambda (c)
                              (declare (ignore c))
                              (sb-debug::map-backtrace
                               (lambda (frame)
                                 (let ((sb-debug::*current-frame* frame))
                                   (multiple-value-bind (name)
                                       (sb-debug::frame-call frame)
                                     (when (eq name 'test)
                                       (assert (equal (sb-debug:var 'l) '(1 2 3)))
                                       (incf count))))))
                              (return))))
        (funcall
         (compile nil `(sb-int:named-lambda test (a i)
                         (declare (optimize (debug 3)))
                         (let ((l (list 1 2 3)))
                           (aref a i)
                           l)))
         #(1) 2)))
    (assert (= count 1))))

(with-test (:name :variables-surrounding-inlined-code.2)
  (let ((count 0))
    (block nil
      (handler-bind ((error (lambda (c)
                              (declare (ignore c))
                              (sb-debug::map-backtrace
                               (lambda (frame)
                                 (let ((sb-debug::*current-frame* frame))
                                   (multiple-value-bind (name)
                                       (sb-debug::frame-call frame)
                                     (when (eq name 'test)
                                       (assert (equal (sb-debug:var 'l) '(1 2 3)))
                                       (incf count))))))
                              (return))))
        (funcall
         (compile nil `(sb-int:named-lambda test (c)
                         (declare (optimize (debug 3)))
                         (let ((l (list 1 2 3)))
                           (map 'list #'signal c)
                           l)))
         '(error))))
    (assert (= count 1))))

(with-test (:name :properly-tagged-p-internal)
  ;; Pick a code component that has a ton of restarts.
  (let* ((code (sb-kernel:fun-code-header #'sb-impl::update-package-with-variance))
         (n (sb-kernel:code-n-entries code)))
    (sb-sys:with-pinned-objects (code)
      (let* ((base (logandc2 (sb-kernel:get-lisp-obj-address code)
                             sb-vm:lowtag-mask))
             (limit (+ base (sb-ext:primitive-object-size code))))
        (flet ((properly-tagged-p (ptr)
                 (eql (alien-funcall (extern-alien "properly_tagged_p_internal"
                                                   (function int unsigned unsigned))
                                     ptr base)
                      1)))
          ;; For architectures that don't use LRAs, there are exactly 'n-entries'
          ;; properly tagged interior pointers. For those which do use LRAs,
          ;; there are at least that many, because we allow pointing to LRAs,
          ;; but they aren't enumerable so we don't know the actual count.
          (assert (#+(or x86 x86-64 arm64) =
                   #-(or x86 x86-64 arm64) >
                     (loop for ptr from (+ base (* 2 sb-vm:n-word-bytes))
                           below limit count (properly-tagged-p ptr))
                     n))
          ;; Verify that the binary search algorithm for simple-fun-index works.
          (let ((count 0))
            (loop for ptr from base below limit
                  do
               (let ((index (alien-funcall
                             (extern-alien "simple_fun_index"
                                           (function int unsigned unsigned))
                             base ptr)))
                 (unless (eql index -1)
                   (let ((tagged-fun (logior ptr sb-vm:fun-pointer-lowtag)))
                     (assert (properly-tagged-p tagged-fun))
                     (incf count)
                     #+nil
                     (format t "~x -> ~d (~a)~%"
                             ptr index (sb-kernel:make-lisp-obj tagged-fun))))))
            (assert (= count n))))))))

(with-test (:name :repeatable-fasl)
  (with-scratch-file (output1 "fasl")
    (compile-file "bug-414.lisp" ; compile this file, why not
                  ::output-file output1 :verbose nil :print nil)
    (with-scratch-file (output2 "fasl")
      (compile-file "bug-414.lisp" ; compile this file, why not
                    ::output-file output2 :verbose nil :print nil)
      (with-open-file (fasl1 output1 :element-type '(unsigned-byte 8))
        (with-open-file (fasl2 output2 :element-type '(unsigned-byte 8))
          (assert (= (file-length fasl1) (file-length fasl2)))
          (loop repeat (file-length fasl1)
                do (assert (= (read-byte fasl1) (read-byte fasl2)))))))))

;; lp#1901781
(defun ll-unknown (x y) (declare (optimize (debug 0))) (+ x y))
(compile 'll-unknown)
(with-test (:name :unknown-lambda-list)
  (assert (eq (sb-kernel:%fun-lambda-list #'ll-unknown) :unknown)))
