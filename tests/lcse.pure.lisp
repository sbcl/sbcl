;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

;;;; Tests of local common subexpression elimination

(invoke-restart 'run-tests::skip-file)
(import '(ctu:inspect-ir
          sb-c::combination-fun-debug-name
          sb-c::basic-combination-p))

(defun ir-calls (form)
  (let (calls)
    (inspect-ir
     form
     (lambda (component)
       (ctu:do-blocks (block component)
         (ctu:do-nodes (node nil block)
           (when (basic-combination-p node)
             (push node calls))))))
    calls))

(defmacro assert-calls (fun-name expected-count (&rest lambda-args) &body body)
  (sb-int:binding*
      (((forms decls) (sb-int:parse-body body nil))
       (lexpr `(lambda (,@lambda-args)
                 ,@decls
                  ;; Without a containing LET form, the current implementation of CSE
                  ;; is unwilling to bind a temporary lambda var for reuse.
                  (let ((active-let-var 0))
                    (print active-let-var)
                    (incf active-let-var (random 5))
                    (print active-let-var)
                    ,@forms))))
    `(assert (= (count ',fun-name (ir-calls ',lexpr) :key #'combination-fun-debug-name)
                ,expected-count))))

;;; Basic tests: a sampling of elidable load type with two identical loads

(with-test (:name (:cse :car))
  (assert-calls car 1 (cons)
    (declare (type cons cons))
    (if (car cons)
        (princ (car cons))
        (princ (cdr cons)))))

(with-test (:name (:cse :different-names-for-same-object))
  ;; Y and X refer to the same thing, and it doesn't matter
  ;; whose CDR we take first. Only one CDR operation is performed
  (assert-calls cdr 1 (x)
    (declare (type cons x))
    (let ((y x))
      (list (cdr y) (cdr x))))
  (assert-calls cdr 1 (x)
    (declare (type cons x))
    (let ((y x))
      (list (cdr x) (cdr y)))))

(with-test (:name (:cse :composition-of-cxr))
  ;; The CAR extraction which is part of the CDAR function can be
  ;; reused based on the fact that we evaluated (LISTP (CAR X))
  (assert-calls car 1 (x)
    (if (listp (car x)) (cdar x)))
  ;; negative test - CDAR on a different Y is another CAR operation
  (assert-calls car 2 (x y)
    (if (listp (car x)) (cdar y))))

(defstruct cse-test-foo slot (wslot 0 :type sb-vm:word))
(with-test (:name (:cse :instance-ref))
  (assert-calls sb-kernel:%instance-ref 1 (x)
    ;; Depending on what TRANSFORM-INSTANCE-TYPEP does, it might access instance-layout
    ;; using %INSTANCE-REF which would have to be discounted in the call counting.
    ;; Just brute-force the type to avoid that situation.
    (let ((x (truly-the cse-test-foo x)))
      (if (cse-test-foo-slot x)
          (princ (cse-test-foo-slot x))
          nil))))

(with-test (:name (:cse :raw-instance-ref/word))
  (assert-calls sb-kernel:%raw-instance-ref/word 1 (x)
    (declare (type cse-test-foo x))
    (if (logtest (cse-test-foo-wslot x) #xff000)
        (princ (cse-test-foo-wslot x))
        nil)))

(with-test (:name (:cse :sap-ref-32))
  (assert-calls sb-sys:sap-ref-32 1 (sap offset)
    (declare (type sb-sys:system-area-pointer sap)
             (type sb-vm:word offset))
    (if (plusp (sb-sys:sap-ref-32 sap offset))
        (princ (sb-sys:sap-ref-32 sap offset))
        nil))
  (assert-calls sb-sys:signed-sap-ref-32 1 (sap offset)
    (declare (type sb-sys:system-area-pointer sap)
             (type sb-vm:word offset))
    (if (plusp (sb-sys:signed-sap-ref-32 sap offset))
        (princ (sb-sys:signed-sap-ref-32 sap offset))
        nil)))

(with-test (:name (:cse :sap-ref-64) :skipped-on (:not :64-bit))
  (assert-calls sb-sys:sap-ref-64 1 (sap offset)
    (declare (type sb-sys:system-area-pointer sap)
             (type sb-vm:word offset))
    (if (plusp (sb-sys:sap-ref-64 sap offset))
        (princ (sb-sys:sap-ref-64 sap offset))
        nil))
  (assert-calls sb-sys:signed-sap-ref-64 1 (sap offset)
    (declare (type sb-sys:system-area-pointer sap)
             (type sb-vm:word offset))
    (if (plusp (sb-sys:signed-sap-ref-64 sap offset))
        (princ (sb-sys:signed-sap-ref-64 sap offset))
        nil)))

(with-test (:name (:cse :loop-over-deref))
  (let ((lexpr
         '(lambda (f)
            (declare (optimize (sb-c::alien-funcall-saves-fp-and-pc 0)
                               (sb-c::type-check 0)))
            ;; F returns a pointer to a null-terminated array of unsigned-int.
            ;; The deref for the loop termination test, and again in the body
            ;; should use a single memory load.
            (let ((a (alien-funcall (the (alien (function (* unsigned))) f))))
              (do ((index 0 (1+ index)))
                  ((zerop (deref a index)))
                (princ (the fixnum (deref a index))))))))
    (assert (= (count #+(or arm64 x86-64) 'sb-sys:%sap-ref-64-indexed
                      #-(or arm64 x86-64)
                      (progn #+64-bit 'sb-sys:sap-ref-64 #-64-bit 'sb-sys:sap-ref-32)
                      (ir-calls lexpr) :key #'combination-fun-debug-name)
               1))))

;;; SETQ of a variable used as an argument to an elidable load prevents CSE.
;;; When a variable that participates in the load expression (e.g., the cons
;;; being CAR'd) is assigned between two identical loads, the second load
;;; must not be eliminated because the variable may reference a different object.
(with-test (:name (:cse :setq-of-load-arg-prevents-car))
  ;; CONS is the argument to CAR. Assigning CONS between two (CAR CONS)
  ;; prevents CSE.
  (assert-calls car 2 (cons other)
    (declare (type cons cons other))
    (let ((a (car cons)))
      (setq cons other)
      (let ((b (car cons)))
        (list a b)))))

(with-test (:name (:cse :setq-of-load-arg-prevents-cdr))
  (assert-calls cdr 2 (cons other)
    (declare (type cons cons other))
    (let ((a (cdr cons)))
      (setq cons other)
      (let ((b (cdr cons)))
        (list a b)))))

(with-test (:name (:cse :setq-of-load-arg-prevents-instance-ref))
  ;; Assigning the struct variable between two reads of the same slot prevents CSE.
  (assert-calls sb-kernel:%instance-ref 2 (x y)
    (let ((x (truly-the cse-test-foo x))
          (y (truly-the cse-test-foo y)))
      (let ((a (cse-test-foo-slot x)))
        (setq x y)
        (let ((b (cse-test-foo-slot x)))
          (list a b))))))

(with-test (:name (:cse :setq-of-load-arg-prevents-sap-ref))
  ;; Assigning the SAP variable between two reads at the same offset
  ;; must prevent CSE since the second read may use a different SAP.
  (assert-calls sb-sys:sap-ref-32 2 (sap1 sap2 offset)
    (declare (type sb-sys:system-area-pointer sap1 sap2)
             (type sb-vm:word offset))
    (let ((a (sb-sys:sap-ref-32 sap1 offset)))
      (setq sap1 sap2)
      (let ((b (sb-sys:sap-ref-32 sap1 offset)))
        (list a b)))))

;;; SETQ of an *unrelated* variable should NOT prevent CSE.
;;; Only mutations of variables that participate in the load matter.
(with-test (:name (:cse :setq-of-unrelated-var-allows-cse))
  ;; Z is not an argument to CAR, so setting Z does not inhibit CSE.
  (assert-calls car 1 (cons z)
    (declare (type cons cons))
    (let ((a (car cons)))
      (setq z a)
      (let ((b (car cons)))
        (list z b)))))

;;; SB-THREAD:BARRIER of any kind prevents elision of the second load
;;; since the memory barriers is not flushable. This is the conservative stance.
;;; The actual (looser) requirements are more subtle than I care to deal with.
(defmacro barrier-test (kind)
  `(with-test (:name (:cse :barrier-prevents-car ,kind))
     (assert-calls car 2 (cons)
      (declare (type cons cons))
      (let ((a (car cons)))
        (sb-thread:barrier (,kind))
        (let ((b (car cons)))
          (list a b))))))
(barrier-test :read)
(barrier-test :write)
(barrier-test :memory)
(barrier-test :compiler)
(barrier-test :data-dependency)

;;; Any non-flushable call between two identical loads prevents CSE
;;; because the call may have side effects that modify the loaded memory.

(with-test (:name (:cse :non-flushable-call-prevents-cse))
  ;; PRINC is a non-flushable
  (assert-calls car 2 (cons)
    (declare (type cons cons))
    (let ((a (car cons)))
      (princ 42)
      (let ((b (car cons)))
        (list a b)))))

;;; Different functions on the same object are NOT considered common subexpressions.
;;; (CAR x) and (CDR x) are different loads even if x is the same.
(with-test (:name (:cse :different-accessors-not-cse))
  ;; Both CAR and CDR should appear, each exactly once.
  (assert-calls car 1 (cons)
    (declare (type cons cons))
    (if (car cons)
        (princ (cdr cons))
        nil))
  (assert-calls cdr 1 (cons)
    (declare (type cons cons))
    (if (car cons)
        (princ (cdr cons))
        nil)))

(defvar *a*)
(defvar *b*)

(macrolet ((guts-of-g ()
             '(let ((x 0))
                (opaque-identity x)
                (opaque-identity (incf x (random 2)))
                (let ((a *a*))
                  (list (car a) *b* (car a))))))
(defun g-regular () (guts-of-g))
(defun g-safe ()
  (declare (optimize safety))
  ;; Don't treat (CAR A) as a common subexpression if the ref of *B*
  ;; could perform memory stores.
  (guts-of-g)))
(compile 'g-regular)
(compile 'g-safe)

(defun try-trapping-ref (safep)
  (setf *a* (cons 5 'foo))
  (handler-bind ((cell-error
                  (lambda (c)
                    (declare (ignore c))
                    (setf (car *a*) -1)
                    (use-value 32))))
    (if safep (g-safe) (g-regular))))

(with-test (:name :test-trapping-ref)
  (assert (equal (try-trapping-ref nil) '(5 32 5))))
(with-test (:name :test-trapping-ref-safe)
  (assert (equal (try-trapping-ref t) '(5 32 -1))))
