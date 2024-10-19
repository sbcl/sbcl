;;;; Lisp Linkage Table

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(eval-when (:compile-toplevel) (aver (= symbol-fdefn-slot fdefn-fun-slot)))

(deftype linkage-index () `(unsigned-byte ,n-linkage-index-bits))

(define-symbol-macro *linkage-table* (extern-alien "linkage_space" system-area-pointer))
(define-symbol-macro *next-fname-index* (extern-alien "linkage_table_count" int))
(define-alien-variable elf-linkage-space system-area-pointer)
(define-alien-variable elf-linkage-table-count int)

(define-load-time-global *linkage-space-mutex* (sb-thread:make-mutex))
(declaim (type sb-thread:mutex *linkage-space-mutex*))
(defglobal *elf-linkage-cell-modified* nil)
(declaim (type (or null simple-bit-vector) *elf-linkage-cell-modified*))

;;; A weak mapping from linkage index to name, represented as a simple-vector
;;; of weak vectors so that we don't reallocate the whole thing when growing.
;;; Each inner vector is exactly one GC page:
;;;   GENCGC-PAGE-BYTES = (ash (+ 4094 vector-data-offset) word-shift).
(define-load-time-global *linkage-name-map* #())
(declaim (simple-vector *linkage-name-map*))
(defconstant linkage-smallvec-elts (- gencgc-page-words vector-data-offset))
(defglobal *fname-map-available-elts* nil)
(defglobal *fname-map-observed-gc-epoch* 0)
(declaim (ftype function unbypass-linkage))

(defun fname-linkage-index (fname)
  (etypecase fname
    ((and symbol (not null))
     (ldb (byte n-linkage-index-bits 0)
          (with-pinned-objects (fname)
            (#+big-endian sap-ref-word #+little-endian sap-ref-32
             (int-sap (get-lisp-obj-address fname))
             (- (ash symbol-hash-slot word-shift) other-pointer-lowtag)))))
    (fdefn (ash (get-header-data fname) -24))))

(macrolet ((entry-addr (index f)
             `(values #+ppc64 (truly-the word
                                         (+ (get-lisp-obj-address ,f)
                                            (- (ash simple-fun-insts-offset word-shift)
                                               fun-pointer-lowtag)))
                      #+x86-64 (sap-ref-word (int-sap (get-lisp-obj-address ,f))
                                             (- (ash closure-fun-slot word-shift)
                                                fun-pointer-lowtag))
                      (sap+ *linkage-table* (ash ,index word-shift)))))

;;; Give DESIGNATOR (a symbol, list, or fdefn) a linkage index.
(defun ensure-linkage-index (designator
                             &optional quiet
                             &aux (fname (if (typep designator '(or symbol fdefn))
                                             designator
                                             (find-or-create-fdefn designator))))
  ;; The QUIET arg is for an extension to the FASLoader that discriminates between looking
  ;; up a linkage index where the compiler did/did-not emit an "undefined" warning.
  ;; In certain cases where a linkage-cell is referenced, the compiler will never warn,
  ;; so neither should the loader, for example in (WHEN (FBOUNDP 'F) (FUNCALL 'F)).
  ;; Observing the QUIET argument requires placing an encapsulation on this function.
  (declare (ignore quiet))
  (aver designator) ; can not assign a linkage index to NIL
  ;; Optimistically assume FNAME has an index already but don't return it if the linkage cell
  ;; isn't also set, which avoids a subtle data race. Consider: Thread A sets the index in the name,
  ;; thread B reads the index, sees that it's nonzero (after compiling some code that needs to jump
  ;; via that linkage cell), immediately calls the freshly-compiled code, and crashes because
  ;; thread A hadn't assigned a callable object into the cell.
  (let ((index (fname-linkage-index fname)))
    (when (/= 0 (sap-ref-word *linkage-table* (ash index word-shift)))
      (return-from ensure-linkage-index index)))
  (or (with-system-mutex (*linkage-space-mutex*)
        (let ((index (fname-linkage-index fname)))
          (declare (linkage-index index))
          (if (/= index 0) ; double-check
              index
              (let ((epoch sb-kernel::*gc-epoch*))
                (unless (eq epoch *fname-map-observed-gc-epoch*) ; Rebuild the freelist as needed
                  (setf *fname-map-observed-gc-epoch* epoch
                        *fname-map-available-elts*
                        (let ((n -1))
                          (collect ((result))
                            (dovector (inner *linkage-name-map* (result))
                              (unless (eql inner 0)
                                (dotimes (j (weak-vector-len inner))
                                  (incf n)
                                  (when (null (weak-vector-ref inner j))
                                    (result n)))))))))
                (when (or *fname-map-available-elts* (typep *next-fname-index* 'linkage-index))
                  (if *fname-map-available-elts*
                      (setq index (pop *fname-map-available-elts*))
                      (setq index *next-fname-index* *next-fname-index* (1+ index)))
                  (binding* (((hi lo) (floor index linkage-smallvec-elts))
                             (map *linkage-name-map*)
                             (inner (svref map hi)))
                    (when (eql inner 0)
                      (setf inner (make-weak-vector linkage-smallvec-elts :initial-element 0)
                            (svref map hi) inner))
                    (setf (weak-vector-ref inner lo) fname))
                  (let ((simply-callable (ensure-simplistic (fdefn-fun fname) fname)))
                    (with-pinned-objects (simply-callable)
                      (multiple-value-bind (entrypoint cell) (entry-addr index simply-callable)
                        (%primitive set-fname-linkage-index fname index cell entrypoint))))
                  index)))))
      (bug "No more linkage table cells available. Rebuild SBCL with a larger table size")))

;;; Assign FUNCTION into DESIGNATOR and possibly into the linkage cell (if nonzero index).
;;; This never assigns an index. FMAKUNBOUND must pass 0 (not NIL) for the function.
(defun fset (designator function)
  (declare (type (or fdefn symbol list) designator))
  (declare (type (or function (eql 0)) function))
  (let ((fname (the (not null) (if (listp designator) (find-fdefn designator) designator))))
    (declare (type (or fdefn symbol) fname))
    (with-system-mutex (*linkage-space-mutex*)
      (let ((index (fname-linkage-index fname)))
        (if (= index 0)
            (%primitive set-fname-fun fname function (int-sap 0) 0)
            (let ((bits *elf-linkage-cell-modified*))
              (when (plusp elf-linkage-table-count)
                (when (null bits)
                  (setf bits (make-array elf-linkage-table-count :element-type 'bit
                                                                 :initial-element 0)
                        *elf-linkage-cell-modified* bits))
                (when (and (< index (length bits)) (= (sbit bits index) 0))
                  ;; Undo direct linkage in any code object that calls the entrypoint
                  ;; currently in this linkage cell. This also tells GC to treat the
                  ;; ELF copy of the cell as a static root.
                  (let ((fun (sap-ref-word *linkage-table* (ash index word-shift))))
                    ;; Don't scan all code if FUN couldn't have been direct-linked
                    (when (and (>= fun text-space-start)
                               (< fun (sap-int *text-space-free-pointer*)))
                      (unbypass-linkage (if (fdefn-p fname) (fdefn-name fname) fname)
                                        fun index)))
                  (setf (sbit bits index) 1)))
              (let ((simply-callable (ensure-simplistic function fname)))
                (with-pinned-objects (simply-callable)
                  (multiple-value-bind (entrypoint cell) (entry-addr index simply-callable)
                    (when (< index elf-linkage-table-count)
                      (setf (sap-ref-word elf-linkage-space (ash index word-shift))
                            entrypoint))
                    (%primitive set-fname-fun fname function cell entrypoint)))))))))
  function)
) ; end MACROLET

(defun linkage-addr->name (value mode)
  (declare (type (member :index :abs :rel) mode))
  (flet ((in-range-p (low count &aux (high (sap+ low (ash count word-shift))))
           (when (and (>= value (sap-int low)) (< value (sap-int high)))
             (ash (sap- (int-sap value) low) (- word-shift)))))
    (let ((index
           (cond ((eq mode :index) value)
                 ((eq mode :rel) (ash value (- word-shift)))
                 ((in-range-p *linkage-table* *next-fname-index*))
                 ((in-range-p elf-linkage-space elf-linkage-table-count))
                 (t (return-from linkage-addr->name nil)))))
      (declare (linkage-index index))
      (multiple-value-bind (hi lo) (floor index linkage-smallvec-elts)
        (let ((inner (svref *linkage-name-map* hi)))
          (unless (eql inner 0)
            (weak-vector-ref inner lo)))))))

(defvar *!initial-linkage-table*)
(defun !initialize-lisp-linkage-table ()
  (setq sb-vm::*code-alloc-count* 0)
  (let* ((map (make-array (ceiling (ash 1 n-linkage-index-bits) linkage-smallvec-elts)))
         (i 0))
    (setf *linkage-name-map* map
          *linkage-space-mutex* (sb-thread:make-mutex :name "linkage"))
    (dovector (fname (the simple-vector *!initial-linkage-table*))
      (multiple-value-bind (hi lo) (floor (incf i) linkage-smallvec-elts)
        (let ((inner (svref map hi)))
          (when (eql inner 0)
            (setf inner (make-weak-vector linkage-smallvec-elts :initial-element 0)
                  (svref map hi) inner))
          (setf (weak-vector-ref inner lo) fname)))
      (unless (fdefn-fun fname) ; accepts symbol or fdefn
        (fset fname 0))))
  #+ppc64
  (let ((from (fname-linkage-index '%coerce-callable-to-fun))
        (into (fname-linkage-index '%coerce-callable-for-call)))
    (setf (sap-ref-word *linkage-table* (ash into word-shift))
          (sap-ref-word *linkage-table* (ash from word-shift)))))
