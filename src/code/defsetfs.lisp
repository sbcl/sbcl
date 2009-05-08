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

(sb!int:/show0 "entering defsetfs.lisp")

;;; from alieneval.lisp
(in-package "SB!ALIEN")
(defsetf slot %set-slot)
(defsetf deref (alien &rest indices) (value)
  `(%set-deref ,alien ,value ,@indices))
(defsetf %heap-alien %set-heap-alien)

;;; from bignum.lisp
(in-package "SB!BIGNUM")
(defsetf %bignum-ref %bignum-set)

;;; from bit-bash.lisp
(in-package "SB!VM")
(defsetf word-sap-ref %set-word-sap-ref)

;;; from debug-int.lisp
(in-package "SB!DI")
(defsetf stack-ref %set-stack-ref)
(defsetf debug-var-value %set-debug-var-value)
(defsetf debug-var-value %set-debug-var-value)
(defsetf breakpoint-info %set-breakpoint-info)

;;; from defstruct.lisp
(in-package "SB!KERNEL")
(defsetf %instance-ref %instance-set)

(defsetf %raw-instance-ref/word %raw-instance-set/word)
(defsetf %raw-instance-ref/single %raw-instance-set/single)
(defsetf %raw-instance-ref/double %raw-instance-set/double)
(defsetf %raw-instance-ref/complex-single %raw-instance-set/complex-single)
(defsetf %raw-instance-ref/complex-double %raw-instance-set/complex-double)

(defsetf %instance-layout %set-instance-layout)
(defsetf %funcallable-instance-info %set-funcallable-instance-info)
(defsetf %funcallable-instance-layout %set-funcallable-instance-layout)

;;; from early-setf.lisp
(in-package "SB!IMPL")

;;; KLUDGE: Various of these (e.g. AREF and BIT) have DEFUN (SETF FOO) versions
;;; too. Do we really need both? -- WHN 19990921
#-sb-xc-host (defsetf car %rplaca)
#-sb-xc-host (defsetf cdr %rplacd)
#-sb-xc-host (defsetf caar (x) (v) `(%rplaca (car ,x) ,v))
#-sb-xc-host (defsetf cadr (x) (v) `(%rplaca (cdr ,x) ,v))
#-sb-xc-host (defsetf cdar (x) (v) `(%rplacd (car ,x) ,v))
#-sb-xc-host (defsetf cddr (x) (v) `(%rplacd (cdr ,x) ,v))
#-sb-xc-host (defsetf caaar (x) (v) `(%rplaca (caar ,x) ,v))
#-sb-xc-host (defsetf cadar (x) (v) `(%rplaca (cdar ,x) ,v))
#-sb-xc-host (defsetf cdaar (x) (v) `(%rplacd (caar ,x) ,v))
#-sb-xc-host (defsetf cddar (x) (v) `(%rplacd (cdar ,x) ,v))
#-sb-xc-host (defsetf caadr (x) (v) `(%rplaca (cadr ,x) ,v))
#-sb-xc-host (defsetf caddr (x) (v) `(%rplaca (cddr ,x) ,v))
#-sb-xc-host (defsetf cdadr (x) (v) `(%rplacd (cadr ,x) ,v))
#-sb-xc-host (defsetf cdddr (x) (v) `(%rplacd (cddr ,x) ,v))
#-sb-xc-host (defsetf caaaar (x) (v) `(%rplaca (caaar ,x) ,v))
#-sb-xc-host (defsetf cadaar (x) (v) `(%rplaca (cdaar ,x) ,v))
#-sb-xc-host (defsetf cdaaar (x) (v) `(%rplacd (caaar ,x) ,v))
#-sb-xc-host (defsetf cddaar (x) (v) `(%rplacd (cdaar ,x) ,v))
#-sb-xc-host (defsetf caadar (x) (v) `(%rplaca (cadar ,x) ,v))
#-sb-xc-host (defsetf caddar (x) (v) `(%rplaca (cddar ,x) ,v))
#-sb-xc-host (defsetf cdadar (x) (v) `(%rplacd (cadar ,x) ,v))
#-sb-xc-host (defsetf cdddar (x) (v) `(%rplacd (cddar ,x) ,v))
#-sb-xc-host (defsetf caaadr (x) (v) `(%rplaca (caadr ,x) ,v))
#-sb-xc-host (defsetf cadadr (x) (v) `(%rplaca (cdadr ,x) ,v))
#-sb-xc-host (defsetf cdaadr (x) (v) `(%rplacd (caadr ,x) ,v))
#-sb-xc-host (defsetf cddadr (x) (v) `(%rplacd (cdadr ,x) ,v))
#-sb-xc-host (defsetf caaddr (x) (v) `(%rplaca (caddr ,x) ,v))
#-sb-xc-host (defsetf cadddr (x) (v) `(%rplaca (cdddr ,x) ,v))
#-sb-xc-host (defsetf cdaddr (x) (v) `(%rplacd (caddr ,x) ,v))
#-sb-xc-host (defsetf cddddr (x) (v) `(%rplacd (cdddr ,x) ,v))
#-sb-xc-host (defsetf first %rplaca)
#-sb-xc-host (defsetf second (x) (v) `(%rplaca (cdr ,x) ,v))
#-sb-xc-host (defsetf third (x) (v) `(%rplaca (cddr ,x) ,v))
#-sb-xc-host (defsetf fourth (x) (v) `(%rplaca (cdddr ,x) ,v))
#-sb-xc-host (defsetf fifth (x) (v) `(%rplaca (cddddr ,x) ,v))
#-sb-xc-host (defsetf sixth (x) (v) `(%rplaca (cdr (cddddr ,x)) ,v))
#-sb-xc-host (defsetf seventh (x) (v) `(%rplaca (cddr (cddddr ,x)) ,v))
#-sb-xc-host (defsetf eighth (x) (v) `(%rplaca (cdddr (cddddr ,x)) ,v))
#-sb-xc-host (defsetf ninth (x) (v) `(%rplaca (cddddr (cddddr ,x)) ,v))
#-sb-xc-host (defsetf tenth (x) (v) `(%rplaca (cdr (cddddr (cddddr ,x))) ,v))
#-sb-xc-host (defsetf rest %rplacd)
#-sb-xc-host (defsetf elt %setelt)
#-sb-xc-host (defsetf aref %aset)
#-sb-xc-host (defsetf row-major-aref %set-row-major-aref)
#-sb-xc-host (defsetf svref %svset)
#-sb-xc-host (defsetf char %charset)
#-sb-xc-host (defsetf bit %bitset)
#-sb-xc-host (defsetf schar %scharset)
#-sb-xc-host (defsetf sbit %sbitset)
(defsetf %array-dimension %set-array-dimension)
(defsetf sb!kernel:%vector-raw-bits sb!kernel:%set-vector-raw-bits)
#-sb-xc-host (defsetf symbol-value set)
#-sb-xc-host (defsetf symbol-global-value set-symbol-global-value)
#-sb-xc-host (defsetf symbol-plist %set-symbol-plist)
#-sb-xc-host (defsetf nth %setnth)
#-sb-xc-host (defsetf fill-pointer %set-fill-pointer)
(defsetf sap-ref-8 %set-sap-ref-8)
(defsetf signed-sap-ref-8 %set-signed-sap-ref-8)
(defsetf sap-ref-16 %set-sap-ref-16)
(defsetf signed-sap-ref-16 %set-signed-sap-ref-16)
(defsetf sap-ref-32 %set-sap-ref-32)
(defsetf signed-sap-ref-32 %set-signed-sap-ref-32)
(defsetf sap-ref-64 %set-sap-ref-64)
(defsetf signed-sap-ref-64 %set-signed-sap-ref-64)
(defsetf sap-ref-word %set-sap-ref-word)
(defsetf signed-sap-ref-word %set-signed-sap-ref-word)
(defsetf sap-ref-sap %set-sap-ref-sap)
(defsetf sap-ref-single %set-sap-ref-single)
(defsetf sap-ref-double %set-sap-ref-double)
#!+long-float (defsetf sap-ref-long %set-sap-ref-long)
#-sb-xc-host (defsetf subseq (sequence start &optional (end nil)) (v)
            `(progn (replace ,sequence ,v :start1 ,start :end1 ,end)
                    ,v))

;;; from fdefinition.lisp
(in-package "SB!IMPL")
#-sb-xc-host (defsetf fdefinition %set-fdefinition)

;;; from kernel.lisp
(in-package "SB!KERNEL")
(defsetf code-header-ref code-header-set)

;;; from serve-event.lisp
(in-package "SB!IMPL")
(defsetf object-set-operation %set-object-set-operation
  #!+sb-doc
  "Set the handler function for an object set operation.")

;;; from x86-vm.lisp
(in-package "SB!VM")
(defsetf context-register %set-context-register)
(defsetf context-float-register %set-context-float-register)

(sb!int:/show0 "leaving defsetfs.lisp")
