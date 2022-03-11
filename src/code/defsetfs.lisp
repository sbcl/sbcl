;;;; various DEFSETFs, pulled into one file for convenience in doing
;;;; them as early in the build process as possible so as to avoid
;;;; hassles with invoking SETF FOO before DEFSETF FOO and thus
;;;; compiling a call to some nonexistent function #'(SETF FOO)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;; from alieneval.lisp
(in-package "SB-ALIEN")
(defsetf slot %set-slot)
(defsetf deref (alien &rest indices) (value)
  `(%set-deref ,alien ,value ,@indices))
(defsetf %heap-alien %set-heap-alien)

;;; from arch-vm.lisp
(in-package "SB-VM")
(defsetf context-register %set-context-register)
(defsetf boxed-context-register %set-boxed-context-register)
(defsetf context-float-register %set-context-float-register)

#-x86-64
(progn
  (declaim (inline assign-vector-flags logior-header-bits reset-header-bits))
  (defun assign-vector-flags (vector flags)
    (set-header-data vector (dpb flags (byte 8 #.array-flags-data-position) (get-header-data vector)))
    (values))
  (defun logior-header-bits (object bits)
    (set-header-data object (logior (get-header-data object) bits))
    object)
  (defun reset-header-bits (object bits)
    (set-header-data object (logand (get-header-data object) (lognot bits)))
    (values)))

(defmacro logior-array-flags (array flags)
  `(logior-header-bits ,array (ash ,flags #.array-flags-data-position)))
(defmacro reset-array-flags (array flags)
  `(reset-header-bits ,array (ash ,flags #.array-flags-data-position)))

(in-package "SB-IMPL")

(declaim (inline (setf %funcallable-instance-info)))
;;; Funcallable instances are just like closures, but there's another slot or two
;;; depending on whether the layout pointer is in a slot or in the header word.
(defun (setf %funcallable-instance-info) (newval fin index)
  (%closure-index-set fin (+ index (- sb-vm:funcallable-instance-info-offset
                                      sb-vm:closure-info-offset))
                      newval)
  newval)
;;; This is just to keep the DEFSTRUCT logic consistent with %INSTANCE-SET,
;;; but the canonical setter is the function named (setf %funcallable-instance-info)
(declaim (inline %set-funcallable-instance-info))
(defun %set-funcallable-instance-info (fin index newval)
  (funcall #'(setf %funcallable-instance-info) newval fin index)
  (values))

;;; from early-setf.lisp

;;; (setf aref/bit/sbit) are implemented using setf-functions,
;;; because they have to work with (setf (apply #'aref array subscripts))
;;; All other setfs can be done using setf-functions too, but I
;;; haven't found technical advantages or disadvantages for either
;;; scheme.
(defsetf car   %rplaca)
(defsetf first %rplaca)
(defsetf cdr   %rplacd)
(defsetf rest  %rplacd)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun %cxr-setf-expander (sub-accessor setter)
  (flet ((expand (place-reader original-form)
           (let ((temp (make-symbol "LIST"))
                 (newval (make-symbol "NEW")))
             (values (list temp)
                     `((,@place-reader ,@(cdr original-form)))
                     (list newval)
                     `(,setter ,temp ,newval)
                     `(,(if (eq setter '%rplacd) 'cdr 'car) ,temp)))))
    (if (eq sub-accessor 'nthcdr) ; random N
        (lambda (access-form env)
          (declare (ignore env))
          (declare (sb-c::lambda-list (n list)))
          (destructuring-bind (n list) (cdr access-form) ; for effect
            (declare (ignore n list)))
          (expand '(nthcdr) access-form))
        ;; NTHCDR of fixed N, or CxxxxR composition
        (lambda (access-form env)
          (declare (ignore env))
          (declare (sb-c::lambda-list (list)))
          (destructuring-bind (list) (cdr access-form) ; for effect
            (declare (ignore list)))
          (expand sub-accessor access-form))))))

(macrolet ((def (name &optional alias &aux (string (string name)))
             `(eval-when (:compile-toplevel :load-toplevel :execute)
                (let ((closure
                       (%cxr-setf-expander
                        '(,(symbolicate "C" (subseq string 2)))
                        ',(symbolicate "%RPLAC" (subseq string 1 2)))))
                  (%defsetf ',name closure)
                  ,@(when alias `((%defsetf ',alias closure)))))))
  ;; Rather than expand into a DEFINE-SETF-EXPANDER, install a single closure
  ;; as the expander and capture just enough to distinguish the variations.
  (def caar)
  (def cadr second)
  (def cdar)
  (def cddr)
  (def caaar)
  (def cadar)
  (def cdaar)
  (def cddar)
  (def caadr)
  (def caddr third)
  (def cdadr)
  (def cdddr)
  (def caaaar)
  (def cadaar)
  (def cdaaar)
  (def cddaar)
  (def caadar)
  (def caddar)
  (def cdadar)
  (def cdddar)
  (def caaadr)
  (def cadadr)
  (def cdaadr)
  (def cddadr)
  (def caaddr)
  (def cadddr fourth)
  (def cdaddr)
  (def cddddr))

;; FIFTH through TENTH
(macrolet ((def (name subform)
             `(eval-when (:compile-toplevel :load-toplevel :execute)
                (%defsetf ',name (%cxr-setf-expander ',subform '%rplaca)))))
  (def fifth   (nthcdr 4)) ; or CDDDDR
  (def sixth   (nthcdr 5))
  (def seventh (nthcdr 6))
  (def eighth  (nthcdr 7))
  (def ninth   (nthcdr 8))
  (def tenth   (nthcdr 9)))

;; CLHS says under the entry for NTH:
;; "nth may be used to specify a place to setf. Specifically,
;;  (setf (nth n list) new-object) == (setf (car (nthcdr n list)) new-object)"
;; which means that it's wrong to use %SETNTH because in the second form,
;; (NTHCDR ...) is a subform of the CAR expression, and so must be
;; bound to a temporary variable.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (%defsetf 'nth (%cxr-setf-expander 'nthcdr '%rplaca)))

(defsetf elt %setelt)
(defsetf row-major-aref %set-row-major-aref)
(defsetf svref %svset)
(defsetf char %charset)
(defsetf schar %scharset)
(declaim (inline (setf %vector-raw-bits)))
(defun (setf %vector-raw-bits) (bits vector index)
  (%set-vector-raw-bits vector index bits)
  bits)
(defsetf symbol-value set)
(defsetf symbol-global-value set-symbol-global-value)
(defsetf fill-pointer %set-fill-pointer)
(defsetf subseq (sequence start &optional end) (v)
  `(progn (replace ,sequence ,v :start1 ,start :end1 ,end) ,v))

;;; from fdefinition.lisp
(defsetf fdefinition %set-fdefinition)

;;; from kernel.lisp
#-darwin-jit
(progn
(declaim (inline (setf code-header-ref)))
(defun (setf code-header-ref) (value code index)
  (code-header-set code index value)
  value))

;;; from pcl
(defsetf slot-value sb-pcl::set-slot-value)

;;; from sxhash.lisp
(define-modify-macro mixf (y) mix)

;;;; Long form DEFSETF macros:

;; CLHS Notes on DEFSETF say that: "A setf of a call on access-fn also evaluates
;;  all of access-fn's arguments; it cannot treat any of them specially."
;; An implication is that even though the DEFAULT argument to GET,GETHASH serves
;; no purpose except when used in a R/M/W context such as PUSH, you can't elide
;; it. In particular, this must fail: (SETF (GET 'SYM 'IND (ERROR "Foo")) 3).

(defsetf get (symbol indicator &optional default &environment e) (newval)
  (let ((constp (constantp default e)))
    ;; always reference default's temp var to "use" it
    `(%put ,symbol ,indicator ,(if constp newval `(progn ,default ,newval)))))

;; A possible optimization for read/modify/write of GETHASH
;; would be to predetermine the vector element where the key/value pair goes.
(defsetf gethash (key hashtable &optional default &environment e) (newval)
  (let ((constp (constantp default e)))
    ;; always reference default's temp var to "use" it
    `(%puthash ,key ,hashtable ,(if constp newval `(progn ,default ,newval)))))

;;;; DEFINE-SETF-MACROs

(define-setf-expander the (&whole form type place &environment env)
  (binding* ((op (car form))
             ((temps subforms store-vars setter getter)
              (get-setf-expansion place env)))
    (values temps subforms store-vars
            `(multiple-value-bind ,store-vars (,op ,type (values ,@store-vars))
               ,setter)
            `(,op ,type ,getter))))

(define-setf-expander getf (place prop &optional default &environment env)
  (binding* (((place-tempvars place-tempvals stores set get)
              (get-setf-expansion place env))
             ((call-tempvars call-tempvals call-args bitmask)
              (collect-setf-temps (list prop default) env '(indicator default)))
             (newval (gensym "NEW")))
      (values `(,@place-tempvars ,@call-tempvars)
              `(,@place-tempvals ,@call-tempvals)
              `(,newval)
              `(let ((,(car stores) (%putf ,get ,(first call-args) ,newval))
                     ,@(cdr stores))
                 ;; prevent "unused variable" style-warning
                 ,@(when (logbitp 1 bitmask) (last call-tempvars))
                 ,set
                 ,newval)
              `(getf ,get ,@call-args))))

(define-setf-expander values (&rest places &environment env)
  ;; KLUDGE: don't use COLLECT - it gets defined later.
  ;; It could be potentially be defined earlier if it were important,
  ;; but sidestepping it this one time wasn't so difficult.
  (let (all-dummies all-vals newvals setters getters)
    (dolist (place places)
      (multiple-value-bind (dummies vals newval setter getter)
          (get-setf-expansion place env)
        ;; ANSI 5.1.2.3 explains this logic quite precisely.  --
        ;; CSR, 2004-06-29
        (setq all-dummies (append all-dummies dummies (cdr newval))
              all-vals (append all-vals vals
                               (mapcar (constantly nil) (cdr newval)))
              newvals (append newvals (and newval (list (car newval)))))
        (push setter setters)
        (push getter getters)))
    (values all-dummies all-vals newvals
            `(values ,@(nreverse setters)) `(values ,@(nreverse getters)))))

;;; CMU CL had a comment here that:
;;;   Evil hack invented by the gnomes of Vassar Street (though not as evil as
;;;   it used to be.)  The function arg must be constant, and is converted to
;;;   an APPLY of the SETF function, which ought to exist.
;;;
;;; Historical note: The hack was considered evil becase prior to the
;;; standardization of #'(SETF F) as a namespace for functions, all that existed
;;; were SETF expanders. To "invert" (APPLY #'F A B .. LAST), you assumed that
;;; the SETF expander was ok to use on (F A B .. LAST), yielding something
;;; like (set-F A B .. LAST). If the LAST arg didn't move (based on comparing
;;; gensyms between the "getter" and "setter" forms), you'd stick APPLY
;;; in front and hope for the best. Plus AREF still had to be special-cased.
;;;
;;; It may not be clear (wasn't to me..) that this is a standard thing, but See
;;; "5.1.2.5 APPLY Forms as Places" in the ANSI spec. I haven't actually
;;; verified that this code has any correspondence to that code, but at least
;;; ANSI has some place for SETF APPLY. -- WHN 19990604
(define-setf-expander apply (functionoid &rest args &environment env)
  ;; Technically (per CLHS) this only must allow AREF,BIT,SBIT
  ;; but there's not much danger in allowing other stuff.
  (unless (typep functionoid '(cons (eql function) (cons symbol null)))
    (error "SETF of APPLY is only defined for function args like #'SYMBOL."))
  (multiple-value-bind (vars vals args) (collect-setf-temps args env nil)
    (let ((new-var (copy-symbol 'new)))
      (values vars vals (list new-var)
              `(apply #'(setf ,(cadr functionoid)) ,new-var ,@args)
              `(apply ,functionoid ,@args)))))

;;; Perform expansion of SETF on LDB, MASK-FIELD, or LOGBITP.
;;; It is preferable to destructure the BYTE form and bind temp vars to its
;;; parts rather than bind a temp for its result. (See the source transforms
;;; for LDB/DPB). But for constant arguments to BYTE, we don't need any temp.
(define-setf-expander ldb (&whole form spec place &environment env)
  "The first argument is a byte specifier. The second is any place form
acceptable to SETF. Replace the specified byte of the number in this
place with bits from the low-order end of the new value."
  (binding* (((bytespec-form store-fun load-fun)
              (ecase (car form)
                (ldb (values spec 'dpb 'ldb))
                (mask-field (values spec 'deposit-field 'mask-field))
                (logbitp (values `(byte 1 ,spec) 'dpb 'logbitp))))
             (spec (%macroexpand bytespec-form env))
             ((byte-tempvars byte-tempvals byte-args)
              (if (typep spec '(cons (eql byte)
                                     (and (not (cons integer (cons integer)))
                                          (cons t (cons t null)))))
                  (collect-setf-temps (cdr spec) env '(size pos))
                  (collect-setf-temps (list spec) env '(bytespec))))
             (byte (if (cdr byte-args) (cons 'byte byte-args) (car byte-args)))
             ((place-tempvars place-tempvals stores setter getter)
              (get-setf-expansion place env))
             (newval (sb-xc:gensym "NEW"))
             (new-int `(,store-fun
                        ,(if (eq load-fun 'logbitp) `(if ,newval 1 0) newval)
                        ,byte ,getter)))
    (values `(,@byte-tempvars ,@place-tempvars)
            `(,@byte-tempvals ,@place-tempvals)
            (list newval)
            ;; FIXME: expand-rmw-macro has code for determining whether
            ;; a binding of a "newval" can be elided.
            (if (and (typep setter '(cons (eql setq)
                                          (cons symbol (cons t null))))
                     (not (cdr stores))
                     (eq (third setter) (first stores)))
                `(progn (setq ,(second setter) ,new-int) ,newval)
                `(let ((,(car stores) ,new-int) ,@(cdr stores))
                   ,setter
                   ,newval))
            (if (eq load-fun 'logbitp)
                ;; If there was a temp for the POS, then use it.
                ;; Otherwise use the constant POS from the original spec.
                `(logbitp ,(or (car byte-tempvars) (third spec)) ,getter)
                `(,load-fun ,byte ,getter)))))

(locally (declare (notinline info)) ; can't inline
(eval-when (:compile-toplevel :load-toplevel :execute)
  (%defsetf 'truly-the (info :setf :expander 'the))
  (%defsetf 'the* (info :setf :expander 'the))
  (%defsetf 'mask-field
              (lambda (&rest args)
              "The first argument is a byte specifier. The second is any place form
acceptable to SETF. Replaces the specified byte of the number in this place
with bits from the corresponding position in the new value."
              (apply (info :setf :expander 'ldb) args)))

;;; SETF of LOGBITP is not mandated by CLHS but is nice to have.
;;; FIXME: the code is suboptimal. Better code would "pre-shift" the 1 bit,
;;; so that result = (in & ~mask) | (flag ? mask : 0)
;;; Additionally (setf (logbitp N x) t) is extremely stupid- it first clears
;;; and then sets the bit, though it does manage to pre-shift the constants.
  (%defsetf 'logbitp (info :setf :expander 'ldb))))

;;; Rather than have a bunch of SB-PCL::FAST-METHOD function names all point
;;; to one that is randomly chosen - and therefore looks confusing -
;;; use these trivial do-nothing functions which are compiled asap.
;;; The selection is deterministic since ties are broken by serial number.
(export '(0-arg-nil 1-arg-nil 2-arg-nil 3-arg-nil n-arg-nil
          1-arg-t n-arg-t)
        "SB-IMPL") ; export to prevent death by tree-shaker
(defun n-arg-nil () (declare (optimize (sb-c:verify-arg-count 0))) nil)
(defun n-arg-t   () (declare (optimize (sb-c:verify-arg-count 0))) t)
(defun 0-arg-nil () nil)
(defun 1-arg-nil (a) (declare (ignore a)) nil)
(defun 1-arg-t   (a) (declare (ignore a)) t)
(defun 2-arg-nil (a b) (declare (ignore a b)) nil)
(defun 3-arg-nil (a b c) (declare (ignore a b c)) nil)
