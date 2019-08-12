(in-package sb-impl)

;;; Remove all symbols from all packages, storing them in weak pointers,
;;; then collect garbage, and re-intern all symbols that survived GC.
;;; Any symbol satisfying PREDICATE will be strongly referenced during GC
;;; so that it doesn't disappear, regardless of whether it appeared unused.
(defun shake-packages (predicate &key print verbose query)
  (declare (function predicate))
  (let (list)
    (flet ((weaken (table accessibility)
             (let ((cells (package-hashtable-cells table))
                   (result))
               (dovector (x cells)
                 (when (symbolp x)
                   (if (funcall predicate x accessibility)
                       (push x result) ; keep a strong reference to this symbol
                       (push (cons (string x) (make-weak-pointer x)) result))))
               (fill cells 0)
               (resize-package-hashtable table 0)
               result)))
      (dolist (package (list-all-packages))
        ;; Never discard standard symbols
        (unless (eq package sb-int:*cl-package*)
          (push (list* (weaken (package-internal-symbols package) :internal)
                       (weaken (package-external-symbols package) :external)
                       package)
                list))))
    (gc :gen 7)
    (when query
      #+cheneygc (error "Can't search for GC roots")
      #+gencgc (sb-ext:search-roots query :criterion :static :gc t))
    (let ((n-dropped 0))
      (flet ((reintern (symbols table package access)
               (declare (ignore package))
               (dolist (item symbols)
                 (if (symbolp item)
                     (add-symbol table item)
                     (let ((symbol (weak-pointer-value (cdr item))))
                       (cond (symbol
                              (add-symbol table symbol))
                             (t
                              (when print
                                (format t "  (~a)~A~%" access (car item)))
                              (incf n-dropped))))))))
        (loop for (internals externals . package) in list
              do (when print
                   (format t "~&Package ~A~%" package))
                 (reintern internals (package-internal-symbols package)
                           package #\i)
                 (reintern externals (package-external-symbols package)
                           package #\e))
        (when verbose
          (format t "~&Dropped ~D symbols~%" n-dropped))
        (force-output)))))
