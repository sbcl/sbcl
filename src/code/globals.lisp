;;;; This file contains special proclamations for variables that are
;;;; referenced in the code sources before they are defined.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;; Define a variable that is assigned into TLS either in INIT-INITIAL-THREAD
;;; or NEW-LISP-THREAD-TRAMPOLINE before any other Lisp code runs.
;;; !COLD-INIT gets these assignents via INIT-INITIAL-THREAD.
;;; In particular, *RESTART-CLUSTERS* and *HANDLER-CLUSTERS* need a value prior
;;; to running into any errors, or else you've got twice the trouble.
;;;
;;; There are other thread-locals which are used by C code, and those are all
;;; defined in PER-THREAD-C-INTERFACE-SYMBOLS. Those other symbols must have
;;; initial values of either a fixnum or a static symbol (typically T or NIL).
;;;
;;; In contrast, DEFINE-THREAD-LOCAL allows more-or-less an arbitrary form,
;;; subject to being evaluable without need of values computed later of course.
;;;
;;; This macro represents a departure from our normal modus operandi with respect
;;; to getting the cross-compiler to be born knowing certain aspects of the target.
;;; Traditionally we process some defining macros (such as SB-XC:DEFMACRO) in both
;;; make-host passes. Doing so makes recompilation of pass 2 depend on all of pass 1.
;;; Here we avoid such depedency by imparting knowlege into genesis built up during
;;; make-host-2, namely the mapping of symbol to TLS index for symbols that are
;;; neither thread slots nor the blessed "C interface" symbols.
(defmacro define-thread-local
    (name &optional (initform '(make-unbound-marker) always-boundp)
                    docstring)
  `(locally
       (declare (notinline (setf info)))
     ;; We don't error here on ALWAYS-BOUND variables as they may have
     ;; variable reference initforms that aren't initialized until
     ;; later in cold-init.
     (defvar ,name ,initform ,docstring)
     ;; :EXECUTE is for parallelized make-host-2 to see the compile-toplevel effect
     (eval-when (:compile-toplevel :execute)
       (%define-thread-local ',name ',initform ',always-boundp))
     (eval-when (:load-toplevel)
       #+sb-thread (setf (info :variable :wired-tls ',name) :always-thread-local)
       ,@(when always-boundp
           `((setf (info :variable :always-bound ',name) :always-bound))))))

(eval-when (:compile-toplevel :execute)
  (defvar sb-thread::*thread-local-specials* (list :not-final))
  (defun %define-thread-local (symbol initform always-boundp)
    (declare (notinline (setf info)))
    (let ((list sb-thread::*thread-local-specials*))
      (aver (eq (car list) :not-final))
      (let ((found (assq symbol (cdr list))))
        (if found
            (setf (cadr found) initform)
            (let* ((thread-struct (sb-vm::primitive-object 'sb-vm::thread))
                   (n-fixed (+ (sb-vm:primitive-object-length thread-struct)
                               (count-if-not
                                (lambda (x)
                                  (find (car (ensure-list x))
                                        (sb-vm:primitive-object-slots thread-struct)
                                        :key #'sb-vm:slot-special))
                                sb-vm::per-thread-c-interface-symbols)))
                   (tls-slot (+ n-fixed (length (cdr list)))))
              (nconc list (list (list symbol initform)))
              (setf (info :variable :wired-tls symbol) (ash tls-slot sb-vm:word-shift))
              (when always-boundp
                (setf (info :variable :always-bound symbol) :always-bound))))))))

;;; a list of lists of currently active RESTART instances. maintained
;;; by RESTART-BIND.
(define-thread-local *restart-clusters* nil)

(define-load-time-global sb-kernel::**initial-handler-clusters** '(nil))
;;; a list of handlers maintained by HANDLER-BIND
(define-thread-local *handler-clusters* sb-kernel::**initial-handler-clusters**)

(declaim (special sb-debug:*in-the-debugger*
                  sb-debug:*stack-top-hint*
                  *gc-inhibit* *gc-pending*
                  #+sb-thread *stop-for-gc-pending*
                  *posix-argv*
                  *default-external-format*
                  *default-source-external-format*
                  *free-interrupt-context-index*))
(declaim (always-bound *default-external-format* *default-source-external-format*))

;;; A unique GC id. This is supplied for code that needs to detect
;;; whether a GC has happened since some earlier point in time. For
;;; example:
;;;
;;;   (let ((epoch *gc-epoch*))
;;;      ...
;;;      (unless (eql epoch *gc-epoch)
;;;        ....))
;;;
;;; This isn't just a fixnum counter since then we'd have theoretical
;;; problems when exactly 2^29 GCs happen between epoch
;;; comparisons. Unlikely, but the cost of using a cons instead is too
;;; small to measure. -- JES, 2007-09-30
(declaim (type cons sb-kernel::*gc-epoch*))
(define-load-time-global sb-kernel::*gc-epoch* '(nil . nil))

;;; Stores the code coverage instrumentation results. The CAR is a
;;; hashtable. The CDR is a list of weak pointers to code objects
;;; having coverage marks embedded in the unboxed constants. Keys in
;;; the hashtable are namestrings, the value is a list of (CONS PATH
;;; VISITED).
(define-load-time-global *code-coverage-info*
    (list (make-hash-table :test 'equal :synchronized t)))
(declaim (type (cons hash-table) *code-coverage-info*))

(in-package "SB-DEBUG")

;;; This is a list of conses (fun-end-cookie . condition-satisfied),
;;; which we use to note distinct dynamic entries into functions. When
;;; we enter a traced function, we add a entry to this list holding
;;; the new end-cookie and whether the trace condition was satisfied.
;;; We must save the trace condition so that the after breakpoint
;;; knows whether to print. The length of this list tells us the
;;; indentation to use for printing TRACE messages.
;;;
;;; This list also helps us synchronize the TRACE facility dynamically
;;; for detecting non-local flow of control. Whenever execution hits a
;;; :FUN-END breakpoint used for TRACE'ing, we look for the
;;; FUN-END-COOKIE at the top of *TRACED-ENTRIES*. If it is not
;;; there, we discard any entries that come before our cookie.
;;;
;;; When we trace using encapsulation, we bind this variable and add
;;; (NIL . CONDITION-SATISFIED), so a NIL "cookie" marks an
;;; encapsulated tracing.
(sb-impl::define-thread-local *traced-entries* ())
(declaim (list *traced-entries*))
