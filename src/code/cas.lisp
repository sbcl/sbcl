(in-package "SB-IMPL")

(declaim (inline (cas car) (cas cdr) (cas first) (cas rest)))
(defun (cas car) (old new cons) (%compare-and-swap-car cons old new))
(defun (cas cdr) (old new cons) (%compare-and-swap-cdr cons old new))
(defun (cas first) (old new cons) (%compare-and-swap-car cons old new))
(defun (cas rest) (old new cons) (%compare-and-swap-cdr cons old new))

;;; Out-of-line definitions for various primitive cas functions.
(macrolet ((def (name lambda-list ref &optional set)
             #+compare-and-swap-vops
             (declare (ignore ref set))
             `(defun ,name (,@lambda-list old new)
                #+compare-and-swap-vops
                (,name ,@lambda-list old new)
                #-compare-and-swap-vops
                (progn
                  #+sb-thread
                  ,(error "No COMPARE-AND-SWAP-VOPS on a threaded build?")
                  #-sb-thread
                  (let ((current (,ref ,@lambda-list)))
                    ;; Shouldn't this be inside a WITHOUT-INTERRUPTS ?
                    (when (eq current old)
                      ,(if set
                           `(,set ,@lambda-list new)
                           `(setf (,ref ,@lambda-list) new)))
                    current)))))
  (def %compare-and-swap-car (cons) car)
  (def %compare-and-swap-cdr (cons) cdr)
  ;; %instance-set is OK here even though it doesn't return a value
  ;; because it is used for effect. And if compare-and-swap vops exist,
  ;; then the setter isn't used at all.
  (def %instance-cas (instance index) %instance-ref %instance-set)
  #+(or x86-64 x86 riscv)
  (def %raw-instance-cas/word (instance index)
       %raw-instance-ref/word
       %raw-instance-set/word)
  #+riscv
  (def %raw-instance-cas/signed-word (instance index)
       %raw-instance-ref/signed-word
       %raw-instance-set/signed-word)
  (def %compare-and-swap-symbol-value (symbol) symbol-value)
  (def %compare-and-swap-svref (vector index) svref))

;; Atomic increment/decrement ops on tagged storage cells (as contrasted with
;; specialized arrays and raw structure slots) are defined in terms of CAS.

;; This code would be more concise if workable versions
;; of +-MODFX, --MODFX were defined generically.
(macrolet ((modular (fun a b)
             #+(or x86 x86-64)
             `(,(package-symbolicate "SB-VM" fun "-MODFX") ,a ,b)
             #-(or x86 x86-64)
             ;; algorithm of https://graphics.stanford.edu/~seander/bithacks
             `(let ((res (logand (,fun ,a ,b)
                                 (ash sb-ext:most-positive-word
                                      (- sb-vm:n-fixnum-tag-bits))))
                    (m (ash 1 (1- sb-vm:n-fixnum-bits))))
                (- (logxor res m) m))))

  ;; Atomically frob the CAR or CDR of a cons, or a symbol-value.
  ;; The latter will be a global value because the ATOMIC-INCF/DECF
  ;; macros work on a symbol only if it is known global.
  (macrolet ((def-frob (name op type slot)
               `(defun ,name (place delta)
                  (declare (type ,type place) (type fixnum delta))
                  (loop (let ((old (the fixnum (,slot place))))
                          (when (eq (cas (,slot place) old
                                         (modular ,op old delta)) old)
                            (return old)))))))
    (def-frob %atomic-inc-symbol-global-value + symbol symbol-value)
    (def-frob %atomic-dec-symbol-global-value - symbol symbol-value)
    (def-frob %atomic-inc-car + cons car)
    (def-frob %atomic-dec-car - cons car)
    (def-frob %atomic-inc-cdr + cons cdr)
    (def-frob %atomic-dec-cdr - cons cdr)))
