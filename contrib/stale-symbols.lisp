;;; This code is currently essentially the same as code posted by Eric
;;; Marsden to cmucl-imp, to detect stale symbols in a core.
;;;
;;; Known deficiencies:
;;;
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
;;;
;;; The "references to that symbol" are found using the function
;;; SB-VM::MAP-REFERENCING-OBJECTS. Consider for example a function
;;; that uses the value of a symbol. The code-object for that function
;;; contains a reference to the symbol, so that a call to SYMBOL-VALUE
;;; can be made at runtime. The data structures corresponding to a
;;; package must maintain a list of its exported an imported symbols.
;;; They contain a hashtable, which contains a vector, which contains
;;; symbols. So all exported symbols will have at least one
;;; referencing object: a vector related to some package.
;;;
;;; Limitations: these routines may provide a number of false
;;; positives (symbols that are not actually stale).  There are also a
;;; number of PCL-related symbols that are displayed, but probably
;;; used internally by PCL.  Moral: the output of these routines must
;;; be checked carefully before going on a code deletion spree.

(defun print-stale-reference (obj stream)
  (cond ((vectorp obj)
         (format stream "vector (probable package internals)"))
        ((sb-c::compiled-debug-fun-p obj)
         (format stream "#<compiled-debug-fun ~A>"
                 (sb-c::compiled-debug-fun-name obj)))
        (t
         (format stream "~w" obj))))

(defun external-symbol-p (obj)
  (declare (type symbol obj))
  (let ((package (symbol-package obj)))
    (and package
         (eq (nth-value 1 (find-symbol (symbol-name obj) package))
             :external))))

(defun find-stale-objects ()
  (sb-vm:map-allocated-objects
     (lambda (obj type size)
       (declare (optimize (safety 0))
                (ignore size))
       (block mapper
         (when (eql type sb-vm:symbol-widetag)
           (ignore-errors
             (let ((refs (let ((res nil)
                               (count 0))
                           (dolist (space '(:static :dynamic :read-only
                                            #+immobile-space :immobile))
                             (sb-vm::map-referencing-objects
                              (lambda (o)
                                (when (> (incf count) 1)
                                  (return-from mapper nil))
                                (push (cons space o) res))
                              ;; FIXME: while we could use :ALL here,
                              ;; then we have a different problem:
                              ;; inferring the space for the preceding PUSH.
                              ;; That's most readily done by calling SB-VM::SPACE-BOUNDS
                              ;; for each known space, storing those answers somewhere,
                              ;; and comparing GET-LISP-OBJ-ADDRESS of O to each space.
                              ;; Would that be the tail wagging the dog?
                              space obj))
                           res)))
               (let ((externalp (external-symbol-p obj)))
                 (format t "~:[S~;External s~]ymbol ~:[#~;~:*~A:~]~2:*~:[:~;~]~*~A~%"
                         externalp
                         (and (symbol-package obj)
                              (package-name (symbol-package obj)))
                         (symbol-name obj)))
               (if (null refs)
                   (progn (princ "   No references found") (terpri))
                   (progn
                     (ecase (caar refs)
                       (:read-only
                        (princ "   Reference in read-only space: "))
                       (:static
                        (princ "   Reference in static space: "))
                       (:dynamic
                        (princ "   Reference in dynamic space: ")))
                     (print-stale-reference (cdar refs) t)
                     (terpri))))))))
     :all))
