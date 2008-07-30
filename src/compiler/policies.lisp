;;;; aimed optimization qualities

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

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
          ((and (< safety 2) (< safety speed)) 2)
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

(define-optimization-quality verify-arg-count
    (if (zerop safety) 0 3)
  ("no" "maybe" "yes" "yes"))

(define-optimization-quality merge-tail-calls
    (if (or (> space debug)
            (> speed debug))
        3
        0)
  ("no" "maybe" "yes" "yes")
  "Control whether tail-calls should reuse caller stack frame.
Enabling this option make functions use less stack space, and make
tail-recursive functions execute in constant stack, but debugging
become harder, because backtraces show only part of function call
sequence.

This options has no effect when INSERT-DEBUG-CATCH is set.")

(define-optimization-quality insert-debug-catch
    (if (> debug (max speed space))
        3
        0)
  ("no" "maybe" "yes" "yes")
  "Enable possibility of returning from stack frames with the debugger.

Enabling this option effectively disables MERGE-TAIL-CALLS.")

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
  ("no" "minimal" "yes" "yes"))

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

(define-optimization-quality store-xref-data
    (if (= space 3)
        0
        3)
  ("no" "yes" "yes" "yes"))

(define-optimization-quality store-coverage-data
    0
  ("no" "no" "yes" "yes"))
