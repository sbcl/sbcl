;;; This code is currently essentially the same as code posted by Eric
;;; Marsden to cmucl-imp, to detect stale symbols in a core.
;;;
;;; Known deficiencies:
;;;
;;; * flags CATCH tags as stale;
;;; * flags constants (under certain circumstances) as stale;
;;; * output is not necessarily terribly clear;
;;; * takes a long time (several hours on CSR's 300MHz x86 desktop) to
;;;   run.
;;;
;;; Comment from Eric Marsden:
;;;
;;; This file contains code that attempts to identify symbols in a
;;; CMUCL image that are stale. For example, the package descriptions
;;; in src/code/package.lisp can get out of sync with the source code,
;;; leading to symbols that are exported without being used anywhere.
;;;
;;; The routines work by walking all the objects allocated in a heap
;;; image (using the function VM::MAP-ALLOCATED-OBJECTS). For each
;;; object of type symbol, it scans the entire heap for objects that
;;; reference that symbol. If it finds no references, or if there is
;;; only one reference that looks like it is likely from the internals
;;; of a package-related datastructure, the name of the symbol and its
;;; package is displayed.
;;; The "references to that symbol" are found using the function
;;; VM::LIST-REFERENCING-OBJECTS. Consider for example a function that
;;; uses the value of a symbol. The code-object for that function
;;; contains a reference to the symbol, so that a call to SYMBOL-VALUE
;;; can be made at runtime. The data structures corresponding to a
;;; package must maintain a list of its exported an imported symbols.
;;; They contain a hashtable, which contains a vector, which contains
;;; symbols. So all exported symbols will have at least one referencing
;;; object: a vector related to some package.
;;;
;;; Limitations: these routines will provide a number of false
;;; positives (symbols that are not actually stale). Throw/catch tags
;;; are displayed, but are not stale. It displays the names of
;;; restarts. Worse, it displays the names of CMUCL-internal constants.
;;; These symbols that name constants are not referenced from anywhere
;;; except the package datastructures because the compiler can
;;; substitute their value wherever they're used in the CMUCL source
;;; code, without keeping a reference to the symbol hanging around.
;;; There are also a number of PCL-related symbols that are displayed,
;;; but probably used internally by PCL.
;;;
;;; Moral: the output of these routines must be checked carefully
;;; before going on a code deletion spree.

(defun print-stale-reference (obj stream)
  (cond ((vectorp obj)
         (format stream "vector (probable package internals)"))
        ((sb-c::compiled-debug-fun-p obj)
         (format stream "#<compiled-debug-fun ~a>"
                 (sb-c::compiled-debug-fun-name obj)))
        (t
         (format stream "~w" obj))))

(defun find-stale-objects ()
  (dolist (space '(:static :dynamic :read-only))
    (sb-vm::map-allocated-objects
     (lambda (obj type size)
       (declare (optimize (safety 0))
                (ignore size))
       (when (eql type sb-vm:symbol-header-widetag)
         (ignore-errors
           (let ((read-only-space-refs (sb-vm::list-referencing-objects :read-only obj))
                 (static-space-refs (sb-vm::list-referencing-objects :static obj))
                 (dynamic-space-refs (sb-vm::list-referencing-objects :dynamic obj)))
             (when (>= 1 (+ (length read-only-space-refs)
                            (length static-space-refs)
                            (length dynamic-space-refs)))
               (format t "Symbol ~a::~a~%"
                       (and (symbol-package obj)
			    (package-name (symbol-package obj)))
                       (symbol-name obj))
               (unless (null read-only-space-refs)
                 (princ "   Reference in read-only space: ")
                 (print-stale-reference (car read-only-space-refs) t)
                 (terpri))
               (unless (null static-space-refs)
                 (princ "   Reference in static space: ")
                 (print-stale-reference (car static-space-refs) t)
                 (terpri))
               (unless (null dynamic-space-refs)
                 (princ "   Reference in dynamic space: ")
                 (print-stale-reference (car dynamic-space-refs) t)
                 (terpri)))))))
     space)))
