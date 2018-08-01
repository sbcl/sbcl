(in-package sb-x86-64-asm)

(defparameter *files*
  '("src/assembly/x86-64/arith"
    "src/assembly/x86-64/array"
    "src/assembly/x86-64/assem-rtns"
    "src/assembly/x86-64/support"
    "src/assembly/x86-64/tramps"
    "src/compiler/x86-64/alloc"
    "src/compiler/x86-64/arith"
    "src/compiler/x86-64/array"
    "src/compiler/x86-64/c-call"
    "src/compiler/x86-64/call"
    "src/compiler/x86-64/cell"
    "src/compiler/x86-64/char"
    "src/compiler/x86-64/debug"
    "src/compiler/x86-64/float"
    "src/compiler/x86-64/macros"
    "src/compiler/x86-64/memory"
    "src/compiler/x86-64/move"
    "src/compiler/x86-64/nlx"
    "src/compiler/x86-64/sap"
    "src/compiler/x86-64/simd-pack"
    "src/compiler/x86-64/system"
    "src/compiler/x86-64/type-vops"
    "src/compiler/x86-64/values"
    ))

(defvar sb-vm::+qword-register-names+
    #("RAX" "RCX" "RDX" "RBX" "RSP" "RBP" "RSI" "RDI"
      "R8"  "R9"  "R10" "R11" "R12" "R13" "R14" "R15"))

(defun forms-equal (x y)
  (sb-int:named-let recurse ((x x) (y y))
    (cond ((eql x y) t)
          ((consp x)
           (and (consp y)
                (recurse (car x) (car y))
                (recurse (cdr x) (cdr y))))
          ((sb-int:comma-p x)
           (and (sb-int:comma-p y)
                (eql (sb-int:comma-kind x) (sb-int:comma-kind y))
                (recurse (sb-int:comma-expr x) (sb-int:comma-expr y))))
          ((stringp x) (and (stringp y) (string= x y)))
          ((pathnamep x) (and (pathnamep y) (pathname= x y)))
          ((bit-vector-p x) (and (bit-vector-p y) (bit-vector-= x y)))
          (t nil))))

(defun tree-find-if (pred tree)
  (sb-int:named-let recurse ((subtree tree) (path nil))
    (let ((i 0))
      (dolist (x subtree)
        (let ((path (cons i path)))
          (when (funcall pred x)
            (return-from tree-find-if (values x (reverse path))))
          (when (consp x)
            (recurse x path))
          (incf i))))))

(defun extract-path (path tree)
  (dolist (i path tree)
    (setq tree (nth i tree))))

(defun equivalentp (old new)
  (unless (and (consp new)
               (symbolp (car new))
               (string= (car new) "EA"))
    (return-from equivalentp nil))
  (let ((size (cadr old)))
    (destructuring-bind (&key (base nil base-p) (index nil index-p)
                              (scale nil scale-p) (disp nil disp-p))
        (cddr old)
      (when (eql disp 0)
        (setq disp nil disp-p nil))
      (when (some
             (lambda (expected)
               (forms-equal (cdr new) expected))
             (cond ((and base-p (not index-p) (not scale-p) (not disp-p))
                    (list `(0 ,base) `(,base))) ; two acceptable spellings
                   ((and base-p index-p (not disp-p))
                    (list `(,base ,index ,@(if scale-p (list scale)))))
                   ((not disp-p) ; omit the leading 'disp'
                    (list (nconc (if (or base-p index-p scale-p) (list base))
                                 (if (or index-p scale-p) (list index))
                                 (if (or scale-p) (list scale)))))
                   (t
                    ;; other forms: base+index+disp, index only, displacement only
                    (list (nconc (list (or disp 0))
                                 (if (or base-p index-p scale-p) (list base))
                                 (if (or index-p scale-p) (list index))
                                 (if (or scale-p) (list scale)))))))
        (return-from equivalentp t))
      ;; the full form with all 5 args is valid, but to specify the last arg,
      ;; SCALE must be supplied, not defaulted.
      (when (and index (not scale-p))
        (setq scale 1))
      (forms-equal (cdr new)
                   `(,(or disp 0) ,base ,index ,scale ,size)))))

(defun compare-trees (old-dir new-dir
                      &aux (total 0) (per-instruction)
                           (*package* *package*))
  (dolist (file *files*)
    (format t "processing ~s~%" file)
    (with-open-file (f1 (format nil "~a~a.lisp" old-dir file))
      (with-open-file (f2 (format nil "~a~a.lisp" new-dir file))
        (loop
         (let ((form1 (read f1 nil f1))
               (form2)
               (printed-tlf))
            (when (eq form1 f1) (return))
           (setq form2 (read f2))
           (cond ((and (consp form1) (eq (car form1) 'in-package))
                  (assert (equal form2 form1))
                  (eval form1))
                 ((and (consp form1) (eq (car form1) 'eval-when)
                       (member :compile-toplevel (second form1)))
                  (assert (forms-equal form2 form1))
                  (handler-bind ((warning #'muffle-warning))
                    (mapc 'eval (cddr form1)))))
           (unless (forms-equal form2 form1)
             (loop
               (multiple-value-bind (subform path)
                   (tree-find-if (lambda (x)
                                   (and (consp x)
                                        (eq (car x) 'make-ea)))
                                 form1)
                 (unless subform (return))
                 ;; tally
                 (let ((containing-form (extract-path (butlast path) form1)))
                   (when (and (symbolp (car containing-form))
                              (string= (car containing-form) "INST")
                              (symbolp (cadr containing-form)))
                     (let* ((sym (cadr containing-form))
                            (found (assoc sym per-instruction)))
                       (if found
                           (incf (cdr found))
                           (push (cons sym 1) per-instruction)))))
                 (incf total)
                 ;;
                 (let ((new-subform (extract-path path form2)))
                   (unless (equivalentp subform new-subform)
                     (let ((*print-pretty* nil)
                           (*print-length* 4)
                           (*print-level* 2)
                           (*package* (find-package :keyword)))
                       (unless printed-tlf
                         (format t "TLF=~s~%" form1)
                         (setq printed-tlf t)))
                     (format t "Possibly non-equivalent:~%~S~%~S~%"
                             subform new-subform))
                   (rplaca subform 'splat))))
             (when printed-tlf
               (format t "--~%"))))))))
  (format t "Total: ~d, breakdown:~% ~s~%"
          total (sort per-instruction #'> :key #'cdr)))

