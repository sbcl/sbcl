;;; Do warm init without compiling files.
(progn
  (defvar *compile-files-p* nil)
  "about to LOAD warm.lisp (with *compile-files-p* = NIL)")
(let ((*print-length* 10)
      (*print-level* 5)
      (*print-circle* t))
  (load "src/cold/warm.lisp")

  ;; Collapse identical FUN-INFOs
  sb-int::
  (let ((ht (make-hash-table :test 'equalp))
        (old-count 0))
    (sb-c::call-with-each-globaldb-name
     (lambda (name)
       (binding* ((info (info :function :info name) :exit-if-null)
                  (shared-info (gethash info ht info)))
         (incf old-count)
         (if (eq info shared-info)
             (setf (gethash info ht) info)
           (setf (info :function :info name) shared-info)))))
    (format t "~&FUN-INFO: Collapsed ~D -> ~D~%"
            old-count (hash-table-count ht)))

  (sb-disassem::!compile-inst-printers)

  ;; Unintern no-longer-needed stuff before the possible PURIFY in
  ;; SAVE-LISP-AND-DIE.
  #-sb-fluid (sb-impl::!unintern-init-only-stuff)

  ;; A symbol whose INFO slot underwent any kind of manipulation
  ;; such that it now has neither properties nor globaldb info,
  ;; can have the slot set back to NIL if it wasn't already.
  (do-all-symbols (symbol)
    (when (and (sb-kernel:symbol-info symbol)
               (null (sb-kernel:symbol-info-vector symbol))
               (null (symbol-plist symbol)))
      (setf (sb-kernel:symbol-info symbol) nil)))

  "done with warm.lisp, about to GC :FULL T")
(sb-ext:gc :full t)

;;; resetting compilation policy to neutral values in preparation for
;;; SAVE-LISP-AND-DIE as final SBCL core (not in warm.lisp because
;;; SB-C::*POLICY* has file scope)
(setq sb-c::*policy* (copy-structure sb-c::**baseline-policy**))

;;; Lock internal packages
#+sb-package-locks
(dolist (p (list-all-packages))
  (unless (member p (mapcar #'find-package '("KEYWORD" "CL-USER")))
    (sb-ext:lock-package p)))

"done with warm.lisp, about to SAVE-LISP-AND-DIE"
;;; Even if /SHOW output was wanted during build, it's probably
;;; not wanted by default after build is complete. (And if it's
;;; wanted, it can easily be turned back on.)
#+sb-show (setf sb-int:*/show* nil)
;;; The system is complete now, all standard functions are
;;; defined.
;;; The call to CTYPE-OF-CACHE-CLEAR is probably redundant.
;;; SAVE-LISP-AND-DIE calls DEINIT which calls DROP-ALL-HASH-CACHES.
(sb-kernel::ctype-of-cache-clear)
(setq sb-c::*flame-on-necessarily-undefined-thing* t)

;;; Clean up stray symbols from the CL-USER package.
(with-package-iterator (iter "CL-USER" :internal :external)
  (loop (multiple-value-bind (winp symbol) (iter)
          (if winp (unintern symbol "CL-USER") (return)))))

#+sb-fasteval (setq sb-ext:*evaluator-mode* :interpret)
(sb-ext:save-lisp-and-die "output/sbcl.core")
