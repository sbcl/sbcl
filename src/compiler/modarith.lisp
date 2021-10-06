;;;; This file contains macros, transforms and optimizers needed for
;;;; performing modular arithmetic.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;; For documentation, see CUT-TO-WIDTH.

(defstruct (modular-class (:copier nil))
  ;; hash: name -> { :GOOD | optimizer | ({modular-fun-info}*)}
  (funs (make-hash-table)) ; keys are symbols
  ;; hash: modular-variant -> (prototype width)
  ;;
  ;; FIXME: Reimplement with generic function names of kind
  ;; (MODULAR-VERSION prototype width)
  (versions (make-hash-table))
  ;; list of increasing widths + signedps
  (widths nil))
(define-load-time-global *untagged-unsigned-modular-class* (make-modular-class))
(define-load-time-global *untagged-signed-modular-class* (make-modular-class))
(define-load-time-global *tagged-modular-class* (make-modular-class))
(defun find-modular-class (kind signedp)
  (ecase kind
    (:untagged
     (ecase signedp
       ((nil) *untagged-unsigned-modular-class*)
       ((t) *untagged-signed-modular-class*)))
    (:tagged
     (aver signedp)
     *tagged-modular-class*)))

(defstruct (modular-fun-info (:copier nil))
  (name (missing-arg) :type symbol)
  (width (missing-arg) :type (integer 0))
  (signedp (missing-arg) :type boolean)
  (lambda-list (missing-arg) :type list)
  (prototype (missing-arg) :type symbol))

(defun find-modular-version (fun-name kind signedp width)
  (let ((infos (gethash fun-name (modular-class-funs (find-modular-class kind signedp)))))
    (if (listp infos)
        (find-if (lambda (mfi)
                   (aver (eq (modular-fun-info-signedp mfi) signedp))
                   (>= (modular-fun-info-width mfi) width))
                 infos)
        infos)))

;;; Return (VALUES prototype-name width)
(defun modular-version-info (name kind signedp)
  (values-list (gethash name (modular-class-versions (find-modular-class kind signedp)))))

(defun %define-modular-fun (name lambda-list prototype kind signedp width)
  (let* ((class (find-modular-class kind signedp))
         (funs (modular-class-funs class))
         (versions (modular-class-versions class))
         (infos (the list (gethash prototype funs)))
         (info (find-if (lambda (mfi)
                          (and (eq (modular-fun-info-signedp mfi) signedp)
                               (= (modular-fun-info-width mfi) width)))
                        infos)))
    (if info
        (unless (and (eq name (modular-fun-info-name info))
                     (= (length lambda-list)
                        (length (modular-fun-info-lambda-list info))))
          (setf (modular-fun-info-name info) name)
          (style-warn "Redefining modular version ~S of ~S for ~
                       ~:[un~;~]signed width ~S."
                      name prototype signedp width))
        (setf (gethash prototype funs)
              (merge 'list
                     (list (make-modular-fun-info :name name
                                                  :width width
                                                  :signedp signedp
                                                  :lambda-list lambda-list
                                                  :prototype prototype))
                     infos
                     #'< :key #'modular-fun-info-width)
              (gethash name versions)
              (list prototype width)))
    (setf (modular-class-widths class)
          (merge 'list (list (cons width signedp)) (modular-class-widths class)
                 #'< :key #'car))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %check-modular-fun-macro-arguments
      (name kind &optional (lambda-list nil lambda-list-p))
    (check-type name symbol)
    (check-type kind (member :untagged :tagged))
    (when lambda-list-p
      (dolist (arg lambda-list)
        (when (member arg lambda-list-keywords)
          (error "Lambda list keyword ~S is not supported for modular ~
                function lambda lists." arg))))))

(defun make-modular-fun-type-deriver (prototype width signedp)
  (let ((info (fun-info-or-lose prototype))
        (mask-type (specifier-type
                    (if signedp
                        `(signed-byte ,width)
                        `(unsigned-byte ,width)))))
    (lambda (call)
      (let ((res (funcall (fun-info-derive-type info) call)))
        (when res
          (if (csubtypep res mask-type)
              res
              mask-type))))))

(defmacro define-modular-fun (name lambda-list prototype kind signedp width)
  (%check-modular-fun-macro-arguments name kind lambda-list)
  (check-type prototype symbol)
  (check-type width unsigned-byte)
  `(progn
     (%define-modular-fun ',name ',lambda-list ',prototype ',kind ',signedp ,width)
     (defknown ,name ,(mapcar (constantly 'integer) lambda-list)
         (,(ecase signedp
             ((nil) 'unsigned-byte)
             ((t) 'signed-byte))
          ,width)
         (foldable flushable movable)
       :derive-type (make-modular-fun-type-deriver ',prototype ,width ',signedp))))

(defun %define-good-modular-fun (name kind signedp)
  (setf (gethash name (modular-class-funs (find-modular-class kind signedp))) :good)
  name)

(defmacro define-good-modular-fun (name kind signedp)
  (%check-modular-fun-macro-arguments name kind)
  `(%define-good-modular-fun ',name ',kind ',signedp))

(defmacro define-modular-fun-optimizer
    (name ((&rest lambda-list) kind signedp &key (width (gensym "WIDTH")))
     &body body)
  (%check-modular-fun-macro-arguments name kind lambda-list)
  (with-unique-names (call args)
    `(setf (gethash ',name (modular-class-funs (find-modular-class ',kind ',signedp)))
           (lambda (,call ,width)
             (declare (type basic-combination ,call)
                      (type (integer 0) ,width))
             (let ((,args (basic-combination-args ,call)))
               (when (= (length ,args) ,(length lambda-list))
                 (destructuring-bind ,lambda-list ,args
                   (declare (type lvar ,@lambda-list))
                   ,@body)))))))

;;; (ldb (byte s 0) (foo                 x  y ...)) =
;;; (ldb (byte s 0) (foo (ldb (byte s 0) x) y ...))
;;;
;;; and similar for other arguments.

;;; Try to recursively cut all uses of LVAR to WIDTH bits.
;;;
;;; For good functions, we just recursively cut arguments; their
;;; "goodness" means that the result will not increase (in the
;;; (unsigned-byte +infinity) sense). An ordinary modular function is
;;; replaced with the version, cutting its result to WIDTH or more
;;; bits. For most functions (e.g. for +) we cut all arguments; for
;;; others (e.g. for ASH) we have "optimizers", cutting only necessary
;;; arguments (maybe to a different width) and returning the name of a
;;; modular version, if it exists, or NIL. If we have changed
;;; anything, we need to flush old derived types, because they have
;;; nothing in common with the new code.
(defun cut-to-width (lvar kind width signedp)
  (declare (type lvar lvar) (type (integer 0) width))
  (let ((type (specifier-type (if (zerop width)
                                  '(eql 0)
                                  `(,(ecase signedp
                                       ((nil) 'unsigned-byte)
                                       ((t) 'signed-byte))
                                     ,width)))))
    (labels ((reoptimize-node (node name)
               (setf (node-derived-type node)
                     (fun-type-returns
                      (global-ftype name)))
               (setf (lvar-%derived-type (node-lvar node)) nil)
               (setf (node-reoptimize node) t)
               (setf (block-reoptimize (node-block node)) t)
               (reoptimize-component (node-component node) :maybe))
             (insert-lvar-cut (lvar)
               "Insert a LOGAND/MASK-SIGNED-FIELD to cut the value of LVAR
                to the required bit width. Returns T if any change was made.

                When the destination of LVAR will definitely cut LVAR's value
                to width (i.e. it's a logand or mask-signed-field with constant
                other argument), do nothing. Otherwise, splice LOGAND/M-S-F in."
               (binding* ((dest (lvar-dest lvar) :exit-if-null)
                          (nil  (combination-p dest) :exit-if-null)
                          (name (lvar-fun-name (combination-fun dest) t))
                          (args (combination-args dest)))
                 (case name
                   (logand
                    (when (= 2 (length args))
                      (let ((other (if (eql (first args) lvar)
                                       (second args)
                                       (first args))))
                        (when (and (constant-lvar-p other)
                                   (ctypep (lvar-value other) type)
                                   (not signedp))
                          (return-from insert-lvar-cut)))))
                   (mask-signed-field
                    (when (and signedp
                               (eql lvar (second args))
                               (constant-lvar-p (first args))
                               (<= (lvar-value (first args)) width))
                      (return-from insert-lvar-cut)))))
               (filter-lvar lvar
                            (if signedp
                                (lambda (dummy)
                                  `(mask-signed-field ,width ,dummy))
                                (lambda (dummy)
                                  `(logand ,dummy ,(ldb (byte width 0) -1)))))
               (do-uses (node lvar)
                 (setf (block-reoptimize (node-block node)) t)
                 (reoptimize-component (node-component node) :maybe))
               t)
             (cut-node (node)
               "Try to cut a node to width. The primary return value is
                whether we managed to cut (cleverly), and the second whether
                anything was changed.  The third return value tells whether
                the cut value might be wider than expected."
               (when (block-delete-p (node-block node))
                 (return-from cut-node (values t nil)))
               (typecase node
                 (ref
                  (typecase (ref-leaf node)
                    (constant
                     (let* ((constant-value (constant-value (ref-leaf node)))
                            (new-value
                              (cond ((not (integerp constant-value))
                                     (return-from cut-node (values t nil)))
                                    (signedp
                                     (mask-signed-field width constant-value))
                                    (t
                                     (ldb (byte width 0) constant-value)))))
                       (cond ((= constant-value new-value)
                              (values t nil)) ; we knew what to do and did nothing
                             (t
                              (change-ref-leaf node (make-constant new-value)
                                               :recklessly t)
                              (let ((lvar (node-lvar node)))
                                (setf (lvar-%derived-type lvar)
                                      (and (lvar-has-single-use-p lvar)
                                           (make-values-type :required (list (ctype-of new-value))))))
                              (setf (block-reoptimize (node-block node)) t)
                              (reoptimize-component (node-component node) :maybe)
                              (values t t)))))))
                 (combination
                  (when (eq (basic-combination-kind node) :known)
                    (let* ((fun-ref (lvar-use (combination-fun node)))
                           (fun-name (lvar-fun-name (combination-fun node)))
                           (modular-fun (find-modular-version fun-name kind
                                                              signedp width)))
                      (cond ((not modular-fun)
                             ;; don't know what to do here
                             (values nil nil))
                            ((let ((dtype (single-value-type
                                           (node-derived-type node))))
                               (and
                                (case fun-name
                                  (logand
                                   (csubtypep dtype
                                              (specifier-type 'unsigned-byte)))
                                  (logior
                                   (csubtypep dtype
                                              (specifier-type '(integer * 0))))
                                  (mask-signed-field
                                   t)
                                  (t nil))
                                (csubtypep dtype type)))
                             ;; nothing to do
                             (values t nil))
                            (t
                             (binding* ((name (etypecase modular-fun
                                                ((eql :good) fun-name)
                                                (modular-fun-info
                                                 (modular-fun-info-name modular-fun))
                                                (function
                                                 (funcall modular-fun node width)))
                                              :exit-if-null)
                                        (did-something nil)
                                        (over-wide nil))
                               (unless (eql modular-fun :good)
                                 (setq did-something t
                                       over-wide t)
                                 (change-ref-leaf
                                  fun-ref
                                  (find-free-fun name "in a strange place"))
                                 (setf (combination-kind node) :full))
                               (unless (functionp modular-fun)
                                 (dolist (arg (basic-combination-args node))
                                   (multiple-value-bind (change wide)
                                       (cut-lvar arg)
                                     (setf did-something (or did-something change)
                                           over-wide (or over-wide wide)))))
                               (when did-something
                                 (reoptimize-node node name))
                               (values t did-something over-wide)))))))))
             (cut-lvar (lvar &key head
                        &aux did-something must-insert over-wide)
               "Cut all the LVAR's use nodes. If any of them wasn't handled
                and its type is too wide for the operation we wish to perform
                insert an explicit bit-width narrowing operation (LOGAND or
                MASK-SIGNED-FIELD) between the LVAR (*) and its destination.
                The narrowing operation might not be inserted if the LVAR's
                destination is already such an operation, to avoid endless
                recursion.

                If we're at the head, forcibly insert a cut operation if the
                result might be too wide.

                (*) We can't easily do that for each node, and doing so might
                result in code bloat, anyway. (I'm also not sure it would be
                correct for complicated C/D FG)"
               (do-uses (node lvar)
                 (multiple-value-bind (handled any-change wide)
                     (cut-node node)
                   (setf did-something (or did-something any-change)
                         must-insert (or must-insert
                                         (not (or handled
                                                  (csubtypep (single-value-type
                                                              (node-derived-type node))
                                                             type))))
                         over-wide (or over-wide wide))))
               (when (or must-insert
                         (and head over-wide))
                 (setf did-something (or (insert-lvar-cut lvar) did-something)
                       ;; we're just the right width after an explicit cut.
                       over-wide nil))
               (values did-something over-wide)))
      (cut-lvar lvar :head t))))

(defun best-modular-version (width signedp)
  ;; 1. exact width-matched :untagged
  ;; 2. >/>= width-matched :tagged
  ;; 3. >/>= width-matched :untagged
  (let* ((uuwidths (modular-class-widths *untagged-unsigned-modular-class*))
         (uswidths (modular-class-widths *untagged-signed-modular-class*))
         (uwidths (if (and uuwidths uswidths)
                      (merge 'list (copy-list uuwidths) (copy-list uswidths)
                             #'< :key #'car)
                      (or uuwidths uswidths)))
         (twidths (modular-class-widths *tagged-modular-class*)))
    (let ((exact (find (cons width signedp) uwidths :test #'equal)))
      (when exact
        (return-from best-modular-version (values width :untagged signedp))))
    (flet ((inexact-match (w)
             (cond
               ((eq signedp (cdr w)) (<= width (car w)))
               ((eq signedp nil) (< width (car w))))))
      (let ((tgt (find-if #'inexact-match twidths)))
        (when tgt
          (return-from best-modular-version
            (values (car tgt) :tagged (cdr tgt)))))
      (let ((ugt (find-if #'inexact-match uwidths)))
        (when ugt
          (return-from best-modular-version
            (values (car ugt) :untagged (cdr ugt))))))))

(defoptimizer (logand optimizer) ((x y) node)
  (let ((result-type (single-value-type (node-derived-type node))))
    (multiple-value-bind (low high)
        (integer-type-numeric-bounds result-type)
      (when (and (numberp low)
                 (numberp high)
                 (>= low 0))
        (let ((width (integer-length high)))
          (multiple-value-bind (w kind signedp)
              (best-modular-version width nil)
            (when w
              ;; FIXME: This should be (CUT-TO-WIDTH NODE KIND WIDTH SIGNEDP).
              ;;
              ;; FIXME: I think the FIXME (which is from APD) above
              ;; implies that CUT-TO-WIDTH should do /everything/
              ;; that's required, including reoptimizing things
              ;; itself that it knows are necessary.  At the moment,
              ;; CUT-TO-WIDTH sets up some new calls with
              ;; combination-type :FULL, which later get noticed as
              ;; known functions and properly converted.
              ;;
              ;; We cut to W not WIDTH if SIGNEDP is true, because
              ;; signed constant replacement needs to know which bit
              ;; in the field is the signed bit.
              (let ((xact (cut-to-width x kind (if signedp w width) signedp))
                    (yact (cut-to-width y kind (if signedp w width) signedp)))
                (declare (ignore xact yact))
                nil) ; After fixing above, replace with T, meaning
                                        ; "don't reoptimize this (LOGAND) node any more".
              )))))))

(defoptimizer (mask-signed-field optimizer) ((width x) node)
  (declare (ignore width))
  (let ((result-type (single-value-type (node-derived-type node))))
    (multiple-value-bind (low high)
        (integer-type-numeric-bounds result-type)
      (when (and (numberp low) (numberp high))
        (let ((width (max (integer-length high) (integer-length low))))
          (multiple-value-bind (w kind)
              (best-modular-version (1+ width) t)
            (when w
              ;; FIXME: This should be (CUT-TO-WIDTH NODE KIND W T).
              ;; [ see comment above in LOGAND optimizer ]
              (cut-to-width x kind w t)
              nil                ; After fixing above, replace with T.
              )))))))

(defoptimizer (logior optimizer) ((x y) node)
  (let ((result-type (single-value-type (node-derived-type node))))
    (multiple-value-bind (low high)
        (integer-type-numeric-bounds result-type)
      (when (and (numberp low)
                 (numberp high)
                 (<= high 0))
        (let ((width (integer-length low)))
          (multiple-value-bind (w kind)
              (best-modular-version (1+ width) t)
            (when w
              ;; FIXME: see comment in LOGAND optimizer
              (let ((xact (cut-to-width x kind w t))
                    (yact (cut-to-width y kind w t)))
                (declare (ignore xact yact))
                nil) ; After fixing above, replace with T
              )))))))

(deftransform ash ((value amount))
  (let ((value-node (lvar-uses value)))
    (unless (combination-p value-node)
      (give-up-ir1-transform))
    (let ((inside-fun-name (lvar-fun-name (combination-fun value-node))))
      (multiple-value-bind (prototype width)
          (modular-version-info inside-fun-name :untagged nil)
        (unless (eq (or prototype inside-fun-name) 'ash)
          (give-up-ir1-transform))
        (when (and width (not (constant-lvar-p amount)))
          (give-up-ir1-transform))
        (let ((inside-args (combination-args value-node)))
          (unless (= (length inside-args) 2)
            (give-up-ir1-transform))
          (let ((inside-amount (second inside-args)))
            (unless (and (constant-lvar-p inside-amount)
                         (not (minusp (lvar-value inside-amount))))
              (give-up-ir1-transform)))
          (splice-fun-args value inside-fun-name 2)
          (if width
              `(lambda (value amount1 amount2)
                 (logand (ash value (+ amount1 amount2))
                         ,(1- (ash 1 (+ width (lvar-value amount))))))
              `(lambda (value amount1 amount2)
                 (ash value (+ amount1 amount2)))))))))
(macrolet
    ((def (left-name name kind width signedp)
       (declare (ignorable name))
       (let ((type (ecase signedp
                     ((nil) 'unsigned-byte)
                     ((t) 'signed-byte))))
         `(progn
            (defknown ,left-name (integer (integer 0)) (,type ,width)
                (foldable flushable movable)
              :derive-type (make-modular-fun-type-deriver 'ash ',width ',signedp))
            (define-modular-fun-optimizer ash ((integer count) ,kind ,signedp :width width)
              (let ((integer-type (lvar-type integer))
                    (count-type (lvar-type count)))
                (declare (ignorable integer-type))
                (when (<= width ,width)
                  (cond ((or (and (constant-lvar-p count)
                                  (plusp (lvar-value count)))
                             (csubtypep count-type
                                        (specifier-type '(and unsigned-byte fixnum))))
                         (cut-to-width integer ,kind width ,signedp)
                         ',left-name)
                        #+(or arm64 x86-64)
                        ((and (not (constant-lvar-p count))
                              (csubtypep count-type (specifier-type 'fixnum))
                              ;; Unknown sign
                              (not (csubtypep count-type (specifier-type '(integer * 0))))
                              (not (csubtypep count-type (specifier-type '(integer 0 *))))
                              (or (csubtypep integer-type (specifier-type `(unsigned-byte ,sb-vm:n-word-bits)))
                                  (csubtypep integer-type (specifier-type `(signed-byte ,sb-vm:n-word-bits)))))
                         ',name)))))
            (setf (gethash ',left-name (modular-class-versions (find-modular-class ',kind ',signedp)))
                  `(ash ,',width))
            (deftransform ,left-name ((integer count) (t (constant-arg (eql 0))))
              'integer)
            #+(or arm64 x86-64)
            (progn
              (defknown ,name (integer integer) (,type ,width)
                  (foldable flushable movable)
                :derive-type (make-modular-fun-type-deriver 'ash ',width ',signedp))
              (setf (gethash ',name (modular-class-versions (find-modular-class ',kind ',signedp)))
                    `(ash ,',width))
              ;; Go back to ASH if the sign becomes known
              (flet ((cut (x)
                       (if ,signedp
                           `(mask-signed-field ,',width ,x)
                           `(logand ,x (ldb (byte ,',width 0) -1)))))
                (deftransform ,name ((integer count) (t (integer * 0)) * :important nil)
                  `,(cut '(ash integer count)))
                (deftransform ,name ((integer count) (t (integer 0 *)) * :important nil)
                  `(,',left-name ,(cut 'integer) count))))))))
  #+(or x86 x86-64 arm arm64)
  (def sb-vm::ash-left-modfx sb-vm::ash-modfx :tagged #.sb-vm:n-fixnum-bits t)
  (def #.(intern (format nil "ASH-LEFT-MOD~D" sb-vm:n-machine-word-bits) "SB-VM")
    #.(intern (format nil "ASH-MOD~D" sb-vm:n-machine-word-bits) "SB-VM")
    :untagged #.sb-vm:n-machine-word-bits nil))

