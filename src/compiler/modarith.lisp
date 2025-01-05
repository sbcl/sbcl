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
         (foldable flushable movable always-translatable)
       :derive-type (make-modular-fun-type-deriver ',prototype ,width ',signedp))))

(defun %define-good-modular-fun (name kind signedp)
  (setf (gethash name (modular-class-funs (find-modular-class kind signedp))) :good)
  name)

(defmacro define-good-modular-fun (name kind signedp)
  (%check-modular-fun-macro-arguments name kind)
  `(%define-good-modular-fun ',name ',kind ',signedp))

(defmacro define-modular-fun-optimizer
    (name ((&rest lambda-list) kind signedp &key (width (gensym "WIDTH"))
                                                 result-width)
     &body body)
  (%check-modular-fun-macro-arguments name kind lambda-list)
  (with-unique-names (call args result-width-name)
    `(setf (gethash ',name (modular-class-funs (find-modular-class ',kind ',signedp)))
           (lambda (,call ,width ,(or result-width
                                   result-width-name))
             (declare (type basic-combination ,call)
                      (type (integer 0) ,width)
                      ,@(unless result-width
                          `((ignore ,result-width-name))))
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
(defun cut-to-width (lvar kind width signedp &optional (result-width width))
  (declare (type lvar lvar) (type (integer 0) width))
  (let ((type (specifier-type (if (zerop width)
                                  '(eql 0)
                                  `(,(ecase signedp
                                       ((nil) 'unsigned-byte)
                                       ((t) 'signed-byte))
                                     ,width)))))
    (labels ((insert-lvar-cut (lvar)
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
                                  `(truly-the (signed-byte ,width) (mask-signed-field ,width ,dummy)))
                                (lambda (dummy)
                                  `(truly-the (unsigned-byte ,width) (logand ,dummy ,(ldb (byte width 0) -1))))))
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
                              (change-ref-leaf node (find-constant new-value)
                                               :recklessly t)
                              (let ((lvar (node-lvar node)))
                                (setf (lvar-%derived-type lvar)
                                      (and (lvar-has-single-use-p lvar)
                                           (make-values-type (list (ctype-of new-value))))))
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
                                                 (funcall modular-fun node width result-width)))
                                              :exit-if-null)
                                        (did-something nil)
                                        (over-wide nil))
                               (unless (eql modular-fun :good)
                                 (setq did-something t
                                       over-wide t)
                                 (unless (eq name t)
                                   (change-ref-leaf
                                    fun-ref
                                    (find-free-fun name "CUT-TO-WIDTH"))
                                   (setf (combination-kind node) :full)))
                               (unless (functionp modular-fun)
                                 (dolist (arg (basic-combination-args node))
                                   (multiple-value-bind (change wide)
                                       (cut-lvar arg)
                                     (setf did-something (or did-something change)
                                           over-wide (or over-wide wide)))))
                               (when did-something
                                 ;; Can't rely on REOPTIMIZE-NODE, as it may neve get reoptimized.
                                 ;; But the outer functions don't want the type to get
                                 ;; widened and their VOPs may never be applied.
                                 (setf (node-derived-type node)
                                       (fun-type-returns (global-ftype (if (eq name t)
                                                                           fun-name
                                                                           name))))
                                 (setf (lvar-%derived-type (node-lvar node)) nil)
                                 (ir1-optimize-combination node))
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
      (declare (dynamic-extent #'inexact-match))
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
              (let ((xact (cut-to-width x kind (if signedp w width) signedp width))
                    (yact (cut-to-width y kind (if signedp w width) signedp width)))
                (declare (ignore xact yact))
                nil) ; After fixing above, replace with T, meaning
                                        ; "don't reoptimize this (LOGAND) node any more".
              )))))))

(setf (fun-info-optimizer (fun-info-or-lose 'logandc2)) #'logand-optimizer-optimizer)

(defoptimizer (mask-signed-field optimizer) ((width x) node)
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

;;; Combine (ash (ash x 1) 1) into (ash x 2)
(deftransform ash ((value amount))
  (let ((value-node (lvar-uses value)))
    (unless (combination-p value-node)
      (give-up-ir1-transform))
    (let ((inside-fun-name (lvar-fun-name (combination-fun value-node))))
      (if (eq inside-fun-name 'ash)
          (let* ((inside-args (combination-args value-node))
                 (inside-amount (second inside-args))
                 ;; Can't do anything if it shifts right erasing bits.
                 (in-range (or (type-approximate-interval (lvar-type inside-amount))
                               (give-up-ir1-transform)))
                 (in-range (if (eq (interval-range-info in-range) '+)
                               in-range
                               (give-up-ir1-transform)))
                 (out-range (type-approximate-interval (lvar-type amount)))
                 (new-range (when out-range
                              (interval-add in-range out-range))))

            (when (and (or ;; Don't do it if the new amount won't shift in one direction
                        (and new-range
                             (not (interval-range-info new-range))
                             (interval-range-info out-range))
                        ;; Do not disturb the conversion to a right shift
                        (combination-is (lvar-uses amount) '(%negate -)))
                       ;; but not if it won't be inlined anyway
                       (or (csubtypep (lvar-type value) (specifier-type 'word))
                           (csubtypep (lvar-type value) (specifier-type 'sb-vm:signed-word))))
              (give-up-ir1-transform))
            (splice-fun-args value inside-fun-name 2)
            `(lambda (value amount1 amount2)
               (ash value (+ amount1 amount2))))
          (multiple-value-bind (prototype width)
              (modular-version-info inside-fun-name :untagged nil)
            (unless (eq prototype 'ash)
              (give-up-ir1-transform))
            (when (not (constant-lvar-p amount))
              (give-up-ir1-transform))
            (let ((inside-args (combination-args value-node)))
              (unless (= (length inside-args) 2)
                (give-up-ir1-transform))
              (let ((inside-amount (second inside-args)))
                (unless (and (constant-lvar-p inside-amount)
                             (not (minusp (lvar-value inside-amount))))
                  (give-up-ir1-transform)))
              (splice-fun-args value inside-fun-name 2)
              `(lambda (value amount1 amount2)
                 (logand (ash value (+ amount1 amount2))
                         ,(1- (ash 1 (+ width (lvar-value amount))))))))))))
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
            (define-modular-fun-optimizer ash ((integer count) ,kind ,signedp :width width
                                               :result-width result-width)
              (let ((integer-type (lvar-type integer))
                    (count-type (lvar-type count)))
                (declare (ignorable integer-type))
                (when (<= width ,width)
                  (cond ((or (and (constant-lvar-p count)
                                  (plusp (lvar-value count)))
                             (csubtypep count-type (specifier-type 'word)))
                         (cut-to-width integer ,kind width ,signedp)
                         ',left-name)
                        #+(or arm64 x86-64)
                        ((and (not (constant-lvar-p count))
                              (csubtypep count-type (specifier-type 'sb-vm:signed-word))
                              ;; Unknown sign
                              (not (csubtypep count-type (specifier-type '(integer * 0))))
                              (not (csubtypep count-type (specifier-type '(integer 0 *))))
                              (or (csubtypep integer-type (specifier-type `(unsigned-byte ,sb-vm:n-word-bits)))
                                  (csubtypep integer-type (specifier-type `(signed-byte ,sb-vm:n-word-bits)))))
                         ',name)
                        ((and (not (csubtypep integer-type (specifier-type 'word)))
                              (not (csubtypep integer-type (specifier-type 'sb-vm:signed-word)))
                              (csubtypep count-type (specifier-type `(integer ,(- result-width width) ,most-positive-fixnum))))
                         (cut-to-width integer ,kind width ,signedp)
                         t)))))
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

;;; Take a perfect hash expression using {+,-,^,&,<<,>>} and convert it to a simple
;;; register-based intermediate language ("IL") that is suspiciously similar to x86-64 asm.
;;; The IL can be lowered to real asm by a vop which prefixes all its instructions
;;; with :DWORD.  See CALC-PHASH in src/compiler/x86-64/arith.
;;; TABLES should be an alist specify the values of TAB and SCRAMBLE
;;; as output by MAKE-PERFECT-HASH-LAMBDA.
#+x86-64
(progn
(export '(phash-convert-to-2-operand-code phash-renumber-temps))
(defglobal *enable-32-bit-codegen* t)
(defun phash-convert-to-2-operand-code (expr tables &aux (temp-counter 0) temps statements)
  (labels ((scan-for-shr (x)
             (typecase x
               ((eql val) (values 1 0)) ; no right-shift is like a right-shift of 0
               (atom (values 0 nil))
               ((cons (eql >>) (cons (eql val)))
                (let ((n (caddr x)))
                  (aver (fixnump n)) ; shift amount is constant
                  (values 1 n)))
               (t (let ((tot 0) (min 32))
                    (dolist (cell x (values tot min))
                      (multiple-value-bind (count local-min) (scan-for-shr cell)
                        (incf tot count)
                        (setf min (min (or local-min 32) min)))))))))
    ;; If all uses of the argument are right-shifted, compute the smallest right-shift
    ;; and do it once, decreasing all other shift amounts.
    (multiple-value-bind (n-shifts preshift) (scan-for-shr expr)
      (cond ((and (plusp preshift) (> n-shifts 1))
             (setq expr
                   (named-let decrease-shift ((x expr))
                     (typecase x
                       (atom x)
                       ((cons (eql >>) (cons (eql val)))
                        (let ((n (- (caddr x) preshift)))
                          (if (= n 0) 'val `(>> val ,n))))
                       (t (mapcar #'decrease-shift x))))))
            (t
             (setq preshift 0)))
      ;; Untag the fixnum and then maybe right-shift some more.
      (push `(>>= val ,(+ preshift sb-vm:n-fixnum-tag-bits)) expr)))
  (labels ((emit (statement)
             (push statement statements))
           (move (dest source)
             (unless (eq dest source)
               (emit `(move ,dest ,source))))
           (new-temp ()
             (let ((temp (intern (format nil "v~D" (incf temp-counter)))))
               (push temp temps)
               temp))
           (select-instruction (op)
             (ecase op
               (& 'and)
               ((+ u32+ +=) 'add)
               ((- u32-) 'sub)
               (<< 'shl)
               ((>> >>=) 'shr)
               ((^ ^=) 'xor)
               (aref 'aref)))
           (commutativep (inst) (member inst '(add and xor)))
           (convert-list (list) ; like PROGN
             (let ((car (convert (car list))))
               (acond ((cdr list) (convert-list it))
                      (t car))))
           (convert (expr)
             (case (car expr)
               ((let) ; there will be exactly one binding
                (destructuring-bind (((name value)) . body) (cdr expr)
                  (aver (member name '(val newval a b)))
                  (convert-operand value name)
                  (convert-list body)))
               (t
                (convert-arith expr))))
           (convert-operand (x &optional name)
             (cond ((cadr (assoc x tables :test 'eq)))
                   ((typep x '(or symbol (unsigned-byte 32) array)) x)
                   (t (convert-arith x name))))
           (convert-arith (expr &optional name &aux (operator (car expr)))
             (case operator
               ((^= += >>=)
                (destructuring-bind (varname operand) (cdr expr)
                  (let ((operand (convert-operand operand)))
                    (emit `(,(select-instruction operator) ,varname ,operand)))))
               (t
                (when (and (member operator '(+ ^)) (= (length (cdr expr)) 3))
                  ;; left-associate
                  (destructuring-bind (first second third) (cdr expr)
                    (return-from convert-arith
                      (convert-arith `(,operator (,operator ,first ,second) ,third) name))))
                (multiple-value-bind (first second)
                    (if (eq operator 'u32-) ; always unary
                        (destructuring-bind (operand) (cdr expr) operand)
                        (destructuring-bind (first second) (cdr expr) ; binary
                          (values first second)))
                  (let* ((first (convert-operand first name))
                         (second (if second (convert-operand second)))
                         (inst (select-instruction operator))
                         (result (cond (name)
                                       ((memq first temps) first)
                                       ((and (memq second temps) (commutativep inst))
                                        (rotatef first second)
                                        first)
                                       (t (new-temp)))))
                    (move result first)
                    (emit (cond ((eq inst 'aref)
                                 (let ((scale
                                        (etypecase first
                                          ((simple-array (unsigned-byte 8) (*)) 1)
                                          ((simple-array (unsigned-byte 16) (*)) 2)
                                          ((simple-array (unsigned-byte 32) (*)) 4))))
                                   `(,inst ,result ,second ,scale)))
                                ((not second) `(neg ,result))
                                (t `(,inst ,result ,second))))
                    result))))))
    (convert `(let ((val arg)) ,@expr)))
  (values (reverse (coerce statements 'vector))
          temp-counter))

(defun phash-renumber-temps (statements)
  (let ((var-map (make-array 4 :initial-element 0))
        (temps #(t0 t1 t2 t3))
        (temps-used 0))
    (flet ((assign-temp (statement operand-index r/w)
             (let* ((var (nth (1+ operand-index) statement))
                    (temp (position var var-map :test #'eq)))
               (unless temp
                 (aver r/w)
                 (let ((claimed (position 0 var-map)))
                   (aver claimed) ; mustn't run out of temps
                   (setf (aref var-map claimed) var
                         temps-used (max temps-used (1+ claimed))
                         temp claimed)))
               (setf (nth (1+ operand-index) statement) (svref temps temp))
               temp))
           (is-unused-after (symbol start)
             (loop for i from start below (length statements)
                   never (find symbol (aref statements i)))))
      (loop for i below (length statements)
            do (let* ((statement (svref statements i))
                      (source-operand (third statement)))
                 ;; Process the source register before the dest because doing that
                 ;; might find that the source is unused after, and so the dest
                 ;; can be the same, eliminating a move
                 (when (typep source-operand '(and symbol (not null)))
                   (let ((temp (assign-temp statement 1 nil)))
                     (when (is-unused-after source-operand (1+ i))
                       (setf (aref var-map temp) 0)))) ; kill
                 (assign-temp statement 0 t)
                 (when (and (eq (car statement) 'move)
                            (eq (cadr statement) (caddr statement)))
                   (setf (svref statements i) 'nop)))))
    (values (remove 'nop statements) temps-used)))

;;; Predict whether the compiler will generate better or worse code on its own
;;; when compared to SB-VM::CALC-PHASH. Do this by counting the arithmetic
;;; operations that require an extra instruction when performed on tagged words.
;;; These are as follows:
;;;   {<< U32+ U32- +=} - ANDed with (FIXNUMIZE UINT-MAX)
;;;   {>> >>=}          - ANDed with (LOGNOT FIXNUM-TAG-MASK)
(defun phash-count-32-bit-modular-ops (expr &aux (count 0))
  (named-let recurse ((expr expr))
    (cond ((listp expr)
           (mapc #'recurse expr))
          ((find expr '(<< u32+ u32- += >> >>=))
           (incf count))))
  count)

(defun optimize-for-calc-phash (form env)
  (aver (eq (cadr form) 'val))
  (let ((scramble (lexenv-find 'scramble vars :lexenv env))
        (tab (lexenv-find 'tab vars :lexenv env))
        (calculation (cddr form)))
    (unless (and *enable-32-bit-codegen*
                 (or scramble tab
                     (>= (phash-count-32-bit-modular-ops calculation) 2)))
      (return-from optimize-for-calc-phash form)) ; decline
    (let (tables)
      (when scramble
        (aver (typep scramble '(cons (eql macro))))
        (push `(scramble ,(cdr scramble)) tables))
      (when tab
        (aver (typep tab '(cons (eql macro))))
        (push `(tab ,(cdr tab)) tables))
      (multiple-value-bind (steps n-temps)
          (phash-renumber-temps
           (phash-convert-to-2-operand-code calculation tables))
        (if (<= n-temps 4) ; always, I think?
            `(sb-vm::calc-phash val ,n-temps ,steps)
            form)))))
)
