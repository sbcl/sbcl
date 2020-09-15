;;;; GC object pinning

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(defmacro with-pinned-objects ((&rest objects) &body body)
  #.(concatenate 'string
  "Arrange with the garbage collector that the pages occupied by
OBJECTS will not be moved in memory for the duration of BODY.
Useful for e.g. foreign calls where another thread may trigger
garbage collection."
     #-gencgc "  This is currently implemented by disabling GC")
  #-gencgc
  `(progn ,@objects (,(if objects 'without-gcing 'progn) ,@body))
  #+(and gencgc (not (or x86 x86-64)))
  `(let ((*pinned-objects* (list* ,@objects *pinned-objects*)))
     (declare (truly-dynamic-extent *pinned-objects*))
     ,@body)
  #+(and gencgc (or x86 x86-64))
  (if objects
      (let ((pins (make-gensym-list (length objects)))
            (wpo (sb-xc:gensym "WITH-PINNED-OBJECTS-THUNK")))
        ;; BODY is stuffed in a function to preserve the lexical
        ;; environment.
        `(flet ((,wpo () (progn ,@body)))
           ;; The cross-compiler prints either "unknown type: COMPILER-NOTE" at
           ;; each use of W-P-O prior to 'ir1report' being compiled, or else
           ;; "could not stack allocate". Kill it with fire :-(
           (declare (muffle-conditions #+sb-xc compiler-note #-sb-xc t))
           ;; PINS are dx-allocated in case the compiler for some
           ;; unfathomable reason decides to allocate value-cells
           ;; for them -- since we have DX value-cells on x86oid
           ;; platforms this still forces them on the stack.
           (dx-let ,(mapcar #'list pins objects)
             (multiple-value-prog1 (,wpo)
               ;; TOUCH-OBJECT has a VOP with an empty body: compiler
               ;; thinks we're using the argument and doesn't flush
               ;; the variable, but we don't have to pay any extra
               ;; beyond that -- and MULTIPLE-VALUE-PROG1 keeps them
               ;; live till the body has finished. *whew*
               ,@(mapcar (lambda (pin)
                           `(touch-object ,pin))
                         pins)))))
      `(progn ,@body)))

(defmacro with-pinned-object-iterator ((name) &body body)
  #-gencgc
  `(macrolet ((,name (arg) (declare (ignore arg)) nil)) ,@body)
  #+(and gencgc (not (or x86 x86-64)))
  `(dx-let ((.cell. (cons nil *pinned-objects*)))
     (let ((*pinned-objects* .cell.))
       (macrolet ((,name (arg) `(rplaca .cell. ,arg))) ,@body)))
  #+(and gencgc (or x86 x86-64))
  `(dx-let ((.cell. (cons nil nil)))
     (macrolet ((,name (arg) `(rplaca .cell. ,arg))) ,@body)))

;;; Allow GC within the body, but pin (for some definition of "pin") all code.
;;; There are two different behaviors:
;;;
;;; - If SPACE is :DYNAMIC, then no code object in the dynamic-space may move or die,
;;;   but immobile-space code objects may die, in the absence of any (ambiguous or exact)
;;;   reference. This mode of pinning prevents object movement, which only implies
;;;   preventing death in as much as those are inextricably the same concept.
;;;
;;; - If SPACE is :IMMOBILE, then the GC is not allowed to do anything that affects the
;;;   freelists in the mark-and-sweep code space. This prevents death, and of course
;;;   the non-movement is implicit. Dynamic-space code is unnaffected.
;;;   The use-case it to provide mutual exclusion of the allocator and collector.
;;;   ** THIS MODE IS NOT IMPLEMENTED YET ***
;;;
;;; The two may not be specified simultaneously, however, nesting may occur, and
;;; should behave as expected, taking the union of the requests into account.
;;; e.g. one could imagine that during a backtrace - hence with :DYNAMIC space
;;; code pinned - it might be necessary to JIT-compile a method for PRINT-OBJECT
;;; which might pin :IMMOBILE space code.
;;;
(defmacro with-code-pages-pinned ((space) &body body)
  #+cheneygc (declare (ignore space))
  #+gencgc `(let ((*gc-pin-code-pages*
                    (logior *gc-pin-code-pages*
                            ,(ecase space
                               (:dynamic 1)
                               #+immobile-space (:immobile 2)))))
              ,@body)
  #+cheneygc `(without-gcing ,@body))
