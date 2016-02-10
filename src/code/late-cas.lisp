(in-package "SB!IMPL")

(defcas car (cons) %compare-and-swap-car)
(defcas cdr (cons) %compare-and-swap-cdr)
(defcas first (cons) %compare-and-swap-car)
(defcas rest (cons) %compare-and-swap-cdr)
(defcas symbol-plist (symbol) %compare-and-swap-symbol-plist)

;;; Out-of-line definitions for various primitive cas functions.
(macrolet ((def (name lambda-list ref &optional set)
             #!+compare-and-swap-vops
             (declare (ignore ref set))
             `(defun ,name (,@lambda-list old new)
                #!+compare-and-swap-vops
                (,name ,@lambda-list old new)
                #!-compare-and-swap-vops
                (progn
                  #!+sb-thread
                  ,(error "No COMPARE-AND-SWAP-VOPS on a threaded build?")
                  #!-sb-thread
                  (let ((current (,ref ,@lambda-list)))
                    ;; Shouldn't this be inside a WITHOUT-INTERRUPTS ?
                    (when (eq current old)
                      ,(if set
                           `(,set ,@lambda-list new)
                           `(setf (,ref ,@lambda-list) new)))
                    current)))))
  (def %compare-and-swap-car (cons) car)
  (def %compare-and-swap-cdr (cons) cdr)
  (def %instance-cas (instance index) %instance-ref %instance-set)
  #!+(or x86-64 x86)
  (def %raw-instance-cas/word (instance index)
       %raw-instance-ref/word
       %raw-instance-set/word)
  (def %compare-and-swap-symbol-info (symbol) symbol-info)
  (def %compare-and-swap-symbol-value (symbol) symbol-value)
  (def %compare-and-swap-svref (vector index) svref))

;; Atomic increment/decrement ops on tagged storage cells (as contrasted with
;; specialized arrays and raw structure slots) are defined in terms of CAS.

;; This code would be more concise if workable versions
;; of +-MODFX, --MODFX were defined generically.
(macrolet ((modular (fun a b)
             #!+(or x86 x86-64)
             `(,(let ((*package* (find-package "SB!VM")))
                  (symbolicate fun "-MODFX"))
                ,a ,b)
             #!-(or x86 x86-64)
             ;; algorithm of https://graphics.stanford.edu/~seander/bithacks
             `(let ((res (logand (,fun ,a ,b)
                                 (ash sb!ext:most-positive-word
                                      (- sb!vm:n-fixnum-tag-bits))))
                    (m (ash 1 (1- sb!vm:n-fixnum-bits))))
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

;;; ATOMIC-MUMBLE functions are not used when self-building.

(defmacro atomic-update (place update-fn &rest arguments &environment env)
  #!+sb-doc
  "Updates PLACE atomically to the value returned by calling function
designated by UPDATE-FN with ARGUMENTS and the previous value of PLACE.

PLACE may be read and UPDATE-FN evaluated and called multiple times before the
update succeeds: atomicity in this context means that the value of PLACE did
not change between the time it was read, and the time it was replaced with the
computed value.

PLACE can be any place supported by SB-EXT:COMPARE-AND-SWAP.

Examples:

  ;;; Conses T to the head of FOO-LIST.
  (defstruct foo list)
  (defvar *foo* (make-foo))
  (atomic-update (foo-list *foo*) #'cons t)

  (let ((x (cons :count 0)))
     (mapc #'sb-thread:join-thread
           (loop repeat 1000
                 collect (sb-thread:make-thread
                          (lambda ()
                            (loop repeat 1000
                                  do (atomic-update (cdr x) #'1+)
                                     (sleep 0.00001))))))
     ;; Guaranteed to be (:COUNT . 1000000) -- if you replace
     ;; atomic update with (INCF (CDR X)) above, the result becomes
     ;; unpredictable.
     x)
"
  (multiple-value-bind (vars vals old new cas-form read-form)
      (get-cas-expansion place env)
    `(let* (,@(mapcar 'list vars vals)
            (,old ,read-form))
       (loop for ,new = (funcall ,update-fn ,@arguments ,old)
             until (eq ,old (setf ,old ,cas-form))
             finally (return ,new)))))

(defmacro atomic-push (obj place &environment env)
  #!+sb-doc
  "Like PUSH, but atomic. PLACE may be read multiple times before
the operation completes -- the write does not occur until such time
that no other thread modified PLACE between the read and the write.

Works on all CASable places."
  (multiple-value-bind (vars vals old new cas-form read-form)
      (get-cas-expansion place env)
    `(let* (,@(mapcar 'list vars vals)
            (,old ,read-form)
            (,new (cons ,obj ,old)))
       (loop until (eq ,old (setf ,old ,cas-form))
             do (setf (cdr ,new) ,old)
             finally (return ,new)))))

(defmacro atomic-pop (place &environment env)
  #!+sb-doc
  "Like POP, but atomic. PLACE may be read multiple times before
the operation completes -- the write does not occur until such time
that no other thread modified PLACE between the read and the write.

Works on all CASable places."
  (multiple-value-bind (vars vals old new cas-form read-form)
      (get-cas-expansion place env)
    `(let* (,@(mapcar 'list vars vals))
       (loop for ,old = ,read-form
             for ,new = (cdr ,old)
             until (eq ,old (setf ,old ,cas-form))
             finally (return (car ,old))))))
