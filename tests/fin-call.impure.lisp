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

(defclass foo ()
  ((%function :initarg :function :reader foo-%function))
  (:metaclass sb-mop:funcallable-standard-class))

(defmethod initialize-instance :after ((f foo) &key)
  (sb-mop:set-funcallable-instance-function f (foo-%function f)))

(setf (fdefinition 'zonk) (make-instance 'foo :function (lambda (y) y)))
(with-test (:name :call-nonstandard-funcallable-instance)
  #+immobile-code
  (let* ((fdefn (sb-int:find-fdefn 'zonk))
         (raw-entry-point
          (sb-sys:sap-ref-sap (sb-sys:int-sap (sb-kernel:get-lisp-obj-address fdefn))
                              sb-vm::(+ (ash fdefn-raw-addr-slot word-shift)
                                        (- other-pointer-lowtag))))
         (code-obj (sb-di::code-header-from-pc raw-entry-point)))
    ;; gets a custom trampoline because it has no embedded trampoline
    (assert (search "#<trampoline #<" (write-to-string code-obj))))

  ;; And it doesn't crash when called
  (funcall 'zonk 3))

#|
This file contains a test of immobile_space_preserve_pointer(),
but demonstrates just about nothing, as it stands.

The vulnerability that it fixes is one which can't
be caused given how the compiler currently generates code.
The bug is that in the following call sequence:
  BB:       FF7508           PUSH QWORD PTR [RBP+8]
  BE:       FF60FD           JMP QWORD PTR [RAX-3]
it is possible that the _only_ pointer to a funcallable-instance is
in register RAX, which gets overwritten as soon as the first instruction
of the FIN trampoline is executed. This is a problem only if the
trampoline instructions are inside the FIN. (It isn't if they're not)

A self-contained trampoline can be disassembled from a FIN:
* (sb-disassem:disassemble-memory (+ (sb-kernel:get-lisp-obj-address #'print-object)
                                  (- sb-vm:fun-pointer-lowtag)
                                  (* 4 sb-vm:n-word-bytes))
                                  10)
->
Size: 10 bytes. Origin: #x20393E60
0:       488B05E9FFFFFF   MOV RAX, [RIP-23]                 ; [#x20393E50]
7:       FF60FD           JMP QWORD PTR [RAX-3]

As can be seen, after the MOV instruction, RAX points to the instance's
function, which is usually a closure. We then want to jump to the address
of the closure's function, dereferenced from [RAX-3]. Doing that requires
the instruction decoder to fetch the JMP instruction from the FIN.
But what if the FIN has already been trashed? If you're lucky,
the bytes will still be there depending on whether GC lazily or eagerly
clears memory. This GC does so eagerly in immobile space.
And Intel/AMD say that self-modifying code "works", which is bad in this
case because it means that the CPU fetches the modified code.

To produce such a situation requires a bunch of patches/coercions to:

(1) perform an anonymous call to a function using no additional stack
    slots nor registers. This can only be done by hacking up the call vop
    or writing a new one, because the compiler always wants to preserve the
    object being called by stashing it somewhere, then moving it from
    there into register RAX just prior to call.
    This could change if the compiler were smarter.

(2) ensure that the funcallable-instance's implementation (the FIN-FUN)
    is not a closure that back-references the funcallable instance itself,
    because if it is, then loading the closure (i.e. executing the first
    instruction of the trampoline) keeps the FIN live, as the closure's
    data block points to the FIN. Requiring that you not close over the FIN
    renders the whole concept of mutable functions slightly useless,
    so from a practical perspective, this situation might never arise.

(3) ensure that there is no symbol that names the funcallable-instance.
    In particular, it must not be a global function attached to an fdefn.
    In theory this could happen - anonymous functions are things.

(4) ensure that that no method is associated with the GF specialized on
    any class named by a symbol. (Because various global tables map names
    of specializers to lists of methods specialized to that specializer;
    and standard methods point to their GF)
    This is an aspect of CLOS that everything points to everything.
    But it's conceivable that you call a GF that has no methods.

(5) insert a breakpoint or debugger trap or something into the
    funcallable-instance trampoline so that GC can occur in between
    the first and second intructions in the trampoline.
    This is for testing - otherwise it would be hard to trigger.

If you manage to do all the above, and GC occurs in between the two
instructions of the trampoline, then without this patch, a crash happens
on return from the garbage collector. Here are some diffs that will do that:

diff --git a/src/code/x86-64-vm.lisp b/src/code/x86-64-vm.lisp
index a7a8c5144..0dfa632f3 100644
--- a/src/code/x86-64-vm.lisp
+++ b/src/code/x86-64-vm.lisp
@@ -217,11 +217,16 @@
         (t
          (closurep fun))))

+(defvar *trap-on-fin-entry* nil)
 (defun %set-fin-trampoline (fin)
   (let ((sap (int-sap (- (get-lisp-obj-address fin) fun-pointer-lowtag)))
         (insts-offs (ash (1+ funcallable-instance-info-offset) word-shift)))
-    (setf (sap-ref-word sap insts-offs) #xFFFFFFE9058B48 ; MOV RAX,[RIP-23]
-          (sap-ref-32 sap (+ insts-offs 7)) #x00FD60FF)) ; JMP [RAX-3]
+    (setf (sap-ref-word sap insts-offs) #xFFFFFFE9058B48) ; MOV RAX,[RIP-23]
+    (incf insts-offs 7)
+    (when *trap-on-fin-entry*
+      (setf (sap-ref-8 sap insts-offs) #xCE) ; INTO - illegal instruction
+      (incf insts-offs))
+    (setf (sap-ref-32 sap insts-offs) #x00FD60FF)) ; JMP [RAX-3]
   fin)

 (defun %set-fdefn-fun (fdefn fun)
diff --git a/src/compiler/x86-64/call.lisp b/src/compiler/x86-64/call.lisp
index d66b5c90c..d33c26556 100644
--- a/src/compiler/x86-64/call.lisp
+++ b/src/compiler/x86-64/call.lisp
@@ -668,6 +668,7 @@
 ;;; In tail call with fixed arguments, the passing locations are
 ;;; passed as a more arg, but there is no new-FP, since the arguments
 ;;; have been set up in the current frame.
+(defvar *clobber-rsi* nil)
 (macrolet ((define-full-call (vop-name named return variable)
             (aver (not (and variable (eq return :tail))))
             #+immobile-code (when named (setq named :direct))
@@ -785,6 +786,10 @@
                    '((if (zerop nargs)
                          (zeroize rcx)
                        (inst mov rcx (fixnumize nargs)))))
+
+               (when *clobber-rsi* ; 3rd argument-passing register
+                 (inst mov rsi-tn (fixnumize -1)))
+
                ,@(cond ((eq return :tail)
                         '(;; Python has figured out what frame we should
                           ;; return to so might as well use that clue.
diff --git a/src/runtime/interrupt.c b/src/runtime/interrupt.c
index 400772889..229eea81c 100644
--- a/src/runtime/interrupt.c
+++ b/src/runtime/interrupt.c
@@ -42,6 +42,9 @@

 #include "sbcl.h"

+#define _GNU_SOURCE /* for REG_RAX etc. from sys/ucontext */
+#include <sys/ucontext.h>
+
 #include <stdio.h>
 #include <stdlib.h>
 #include <string.h>
@@ -1942,6 +1945,19 @@ low_level_unblock_me_trampoline(int signal, siginfo_t *info, void *void_context)
 static void
 low_level_handle_now_handler(int signal, siginfo_t *info, void *void_context)
 {
+    unsigned char *pc = (unsigned char*)
+        ((struct ucontext*)void_context)->uc_mcontext.gregs[REG_RIP];
+    if (*pc == 0xCE) { // this is the illegal instruction placed into a FIN
+      printf("bytes around PC (%p):", pc);
+      int i;
+      for(i=-10; i<10; ++i) printf(" %02x", pc[i]);
+      putchar('\n');
+      printf("calling GC\n");
+      collect_garbage(0);
+      printf("back from GC\n");
+      ++((struct ucontext*)void_context)->uc_mcontext.gregs[REG_RIP];
+      return;
+    }
     SAVE_ERRNO(signal,context,void_context);
     (*interrupt_low_level_handlers[signal])(signal, info, context);
     RESTORE_ERRNO;
|#

(progv (let ((s1 (find-symbol "*TRAP-ON-FIN-ENTRY*" "SB-VM"))
             (s2 (find-symbol "*CLOBBER-RSI*" "SB-VM")))
         (if s1 (list s1 s2)))
       '(t t)
  (defgeneric blub (x))
  ;; BAR calls F with only two args, so we can freely clobber RSI
  ;; (the third call TN on x86-64), and not have RSI be an accidental
  ;; copy of F which gets moved into RAX and is therefore not needed.
  (setf (symbol-function 'bar)
        (compile nil '(lambda (f) (funcall (truly-the function f) 1 2)))))

;;; Just set any function that doesn't close over BLUB
(setf (sb-kernel:%funcallable-instance-fun #'blub)
      (compile nil '(lambda (&rest args)
                     (format t "Can't call me ~S" args))))

(defglobal *z* (list #'blub)) ; capture BLUB somewhere that is not an FDEFN
(fmakunbound 'blub)

(defun foo ()
  (let* ((z *z*)
         (f (car (truly-the cons z))))
    (rplaca z nil)
    (bar f)))
;;; FOO, if interpreted, holds on to *Z*'s CAR on the stack,
;;; making this test not demonstrate what it should about stack pins.
(compile'foo)

;;; The "expected" behavior of this test without the patch
;;; to enliven FINs based on unboxed pointers is:
;;; ::: UNEXPECTED-FAILURE :GARBAGE-FUNCALLABLE-INSTANCE-CALL-CRASH
;;;     due to SB-SYS:MEMORY-FAULT-ERROR: "Unhandled memory fault at #x1E31B7B."
;;;
(with-test (:name :garbage-funcallable-instance-call-crash)
  (foo))
