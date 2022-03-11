;;;; aimed optimization qualities

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

(define-optimization-quality check-constant-modification
    safety
  ("no" "maybe" "yes" "yes")
  "Control whether the compiler should check for constant
modification. Defaults to SAFETY.")

(define-optimization-quality type-check
    ;; FIXME: grepping the tree for "policy.*safety" yields some
    ;; places which might want to use this instead -- or
    ;; some other derived policy.
    (cond ((= safety 0) 0)
          (t 3))
  ("no" "maybe" "weak" "full")
  "Control the way to perform runtime type checking:
0: declared types are simply trusted; no runtime checks are performed;
2: fast checks are performed: declared types are weakened to
   FIXNUM/SINGLE-FLOAT/FLOAT/NUMBER/structure/specialized array etc.;
3: declared types are fully checked (several exceptions exist;
   see \"SBCL User Manual\", Compiler->Handling of Types->
   Implementation Limitations for details).")

(define-optimization-quality check-tag-existence
    (cond ((= safety 0) 0)
          (t 3))
  ("no" "maybe" "yes" "yes")
  "Control whether GO and RETURN-FROM check liveness of the destination tag.
Enabling this option can increase heap consing of closures.")

(define-optimization-quality let-conversion
    (if (<= debug speed) 3 0)
  ("off" "maybe" "on" "on")
  "Control inline-substitution of used-once local functions.")

(define-optimization-quality alien-funcall-saves-fp-and-pc
    (if (<= speed debug) 3 0)
  ("no" "maybe" "yes" "yes")
  "Control ALIEN-FUNCALL saving frame-pointer and program counter for
more reliable bactracing across foreign calls.")

(define-optimization-quality verify-arg-count
    (if (zerop safety) 0 3)
  ("no" "maybe" "yes" "yes"))

(define-optimization-quality insert-debug-catch
  (cond ((and (= debug 3)
              (> debug speed))
         3)
        #+unwind-to-frame-and-call-vop
        ((and (> debug 0)
              (>= debug speed))
         1)
        (t
         0))
  ("no" "maybe" "yes" "yes")
  "Enables possibility of returning from stack frames with the debugger.
The default value 1 doesn't prevent tail call optimization, while >1 does.")

(define-optimization-quality recognize-self-calls
    (if (> (max speed space) debug)
        3
        0)
  ("no" "maybe" "yes" "yes")
  "When enabled, reference to a function FOO inside the body of (DEFUN
FOO ...) is considered to be the reference to the function being
defined. Calls to FOO are compiled as local. This allows better
optimization and type checking, but TRACE will not show recursive
calls. If the function object is bound to another name BAR, and FOO is
bound to another function, calls to FOO inside BAR will remain to be
recursive.

When disabled, internal references to a function FOO will be
considered ti be a call of a function, bound to the symbol at
run-time, which is less efficient. TRACE will show recursive calls. In
case of renaming described above, calls to FOO will not be recursive
and will refer to the new function, bound to FOO.")

(define-optimization-quality float-accuracy
    3
  ("degraded" "full" "full" "full"))

(define-optimization-quality insert-step-conditions
    (if (> debug (max speed space compilation-speed))
        debug
        0)
  ("no" "no" "partial" "full")
  "Control instrumentation of code, enabling single-stepping through
it in the debugger.

This option has no effect without COMPUTE-DEBUG-FUN.")

(define-optimization-quality compute-debug-fun
    debug
  ("minimal" "yes" "yes" "yes"))

(define-optimization-quality store-source-form
    debug
  ("no" "maybe" "yes" "yes"))

(define-optimization-quality preserve-single-use-debug-variables
    (if (and (>= debug 2)
             (< speed 3))
        3
        0)
  ("no" "no" "no" "yes")
  "When disabled, LET variable, which is never set and is referenced
exactly once, is eliminated and the reference is substituted with the
initial value. This allows better type inference and some algebraic
optimizations.

When enabled, the variable is preserved and can be seen in the
debugger.")

(define-optimization-quality insert-array-bounds-checks
    (if (= safety 0) 0 3)
  ("no" "yes" "yes" "yes"))
(define-optimization-quality aref-trapping
    #-ubsan (if (= safety 3) 3 0) ; equiv. to safety unless expressed otherwise
    #+ubsan 2 ; default to yes
  ("no" "yes" "yes" "yes"))

(define-optimization-quality store-xref-data
    (if (= space 3)
        0
        3)
  ("no" "yes" "yes" "yes"))

(define-optimization-quality store-coverage-data
    0
  ("no" "no" "yes" "yes"))

(define-optimization-quality instrument-consing
    1
  ("no" "no" "yes" "yes"))

#+sb-safepoint
(define-optimization-quality insert-safepoints
    1
  ("no" "yes" "yes" "yes")
  "When enabled, the compiler will insert safepoints at strategic
points (loop edges, function prologues) to ensure that potentially
long-running code can be interrupted.

When enabled, no safepoints will be inserted explicitly.  Note that
this declaration does not prevent out-of-line function calls, which
will encounter safepoints unless the target function has also been
compiled with this declaration in effect.")

(define-optimization-quality store-closure-debug-pointer
    0
  ("no" "no" "yes" "yes"))

;;; ALLOW-NON-RETURNING-TAIL-CALL unsupresses the supression of tail-call
;;; optimization of nil-returning functions.
;;;
;;; At present this is used only by the ARG-COUNT-ERROR function, but would be
;;; useful in similar functions whose sole purpose is to accept positional
;;; arguments, shaping them into keyword arguments with which to call ERROR.
;;; Generally any function tail-calling a nil-returning function remains on the
;;; stack to allow a debugger to see variables in the signaling frame.
;;; However ARG-COUNT-ERROR is not supposed to be visible.
;;; Techniques that evolved to address its invisibility were brittle:
;;; undocumented DEBUG versus SPEED policy-based decisions in TAIL-ANNOTATE,
;;; or inefficient (performing FIND-CALLER-FRAME before calling ERROR).
;;; Frobbing *STACK-TOP-HINT* seems no better than a declaration saying
;;; "do what I mean," as the latter at least produces a consistent backtrace
;;; between Lisp and ldb or gdb.
;;;
(define-optimization-quality allow-non-returning-tail-call
    0
  ("no" "no" "no" "yes"))
