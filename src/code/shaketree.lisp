(in-package sb-impl)

;;; Remove all symbols from all packages, storing them in weak pointers,
;;; then collect garbage, and re-intern all symbols that survived GC.
;;; Any symbol satisfying PREDICATE will be strongly referenced during GC
;;; so that it doesn't disappear, regardless of whether it appeared unused.
(defun shake-packages (predicate &key print verbose query)
  (declare (function predicate))
  (let (list)
    (flet ((weaken (table accessibility)
             (let ((cells (symtbl-cells table))
                   (result))
               (dovector (x cells)
                 (when (symbolp x)
                   (if (funcall predicate x accessibility)
                       (push x result) ; keep a strong reference to this symbol
                       (push (cons (string x) (make-weak-pointer x)) result))))
               (fill cells 0)
               (resize-symbol-table table 0 'intern)
               result)))
      (dolist (package (list-all-packages))
        ;; Never discard standard symbols
        (unless (eq package sb-int:*cl-package*)
          (push (list* (weaken (package-internal-symbols package) :internal)
                       (weaken (package-external-symbols package) :external)
                       package)
                list))))

    (flet ((visit-ctors (xform)
             (maphash (lambda (classoid layout)
                        (declare (ignore classoid))
                        (binding* ((dd (layout-info layout) :exit-if-null))
                          (setf (dd-constructors dd) (mapcan xform (dd-constructors dd)))))
                      (classoid-subclasses (find-classoid t)))))
      ;; Weaken references from a defstruct-descriptions to its constructors.
      ;; No global def needed if every call site was inlined
      (visit-ctors (lambda (x) `((,(make-weak-pointer (car x)) ,(string (car x)) . ,(cdr x)))))
      (gc :gen 7)
      ;; Unweaken dd constructor references ASAP because the compiler is in a fragile state.
      ;; If FUN-NAME-INLINE-EXPANSION were to be called, it might fail an AVER.
      (visit-ctors (lambda (x &aux (wpv (weak-pointer-value (car x))))
                     (if wpv `((,wpv . ,(cddr x)))))))

    (when query
      (sb-ext:search-roots query :criterion :static))
    (let ((n-dropped 0))
      (flet ((reintern (symbols table package access)
               (declare (ignore package))
               (dolist (item symbols)
                 (if (symbolp item)
                     (add-symbol table item 'intern)
                     (let ((symbol (weak-pointer-value (cdr item))))
                       (cond (symbol
                              (add-symbol table symbol 'intern))
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
