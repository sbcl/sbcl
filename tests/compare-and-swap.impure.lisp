;;; Basics

(defstruct xxx yyy)

(macrolet ((test (init op)
             `(with-test (:name (:cas :basics ,(intern (symbol-name op) "KEYWORD")))
                (let ((x ,init)
                      (y (list 'foo))
                      (z (list 'bar)))
                  (assert (eql nil (compare-and-swap (,op x) nil y)))
                  (assert (eql y (compare-and-swap (,op x) nil z)))
                  (assert (eql y (,op x)))
                  (let ((x (eval "foo"))) ; hide the compile-time type error
                    (multiple-value-bind (res err)
                        (ignore-errors (compare-and-swap (,op x) nil nil))
                      (unless (not res)
                        (error "Wanted NIL and type-error, got: ~S" res))
                      (assert (typep err 'type-error))))))))
  (test (cons nil :no) car)
  (test (cons nil :no) first)
  (test (cons :no nil) cdr)
  (test (cons :no nil) rest)
  (test '.foo. symbol-plist)
  (test (progn (set '.bar. nil) '.bar.) symbol-value)
  (test (make-xxx) xxx-yyy))

(defvar *foo*)

;;; thread-local bindings

(with-test (:name (:cas :tls))
  (let ((*foo* 42))
    (let ((*foo* nil))
      (assert (eql nil (compare-and-swap (symbol-value '*foo*) nil t)))
      (assert (eql t (compare-and-swap (symbol-value '*foo*) nil :foo)))
      (assert (eql t *foo*)))
    (assert (eql 42 *foo*))))

;;; unbound symbols + symbol-value

(assert (not (boundp '*foo*)))

(with-test (:name (:cas :unbound))
  (multiple-value-bind (res err)
      (ignore-errors (compare-and-swap (symbol-value '*foo*) nil t))
    (assert (not res))
    (assert (typep err 'unbound-variable))))

(defvar *bar* t)

(with-test (:name (:cas :unbound 2))
  (let ((*bar* nil))
    (makunbound '*bar*)
    (multiple-value-bind (res err)
        (ignore-errors (compare-and-swap (symbol-value '*bar*) nil t))
      (assert (not res))
      (assert (typep err 'unbound-variable)))))

;;; SVREF

(defvar *v* (vector 1))

;; basics
(with-test (:name (:cas :svref))
  (assert (eql 1 (compare-and-swap (svref *v* 0) 1 2)))
  (assert (eql 2 (compare-and-swap (svref *v* 0) 1 3)))
  (assert (eql 2 (svref *v* 0))))

;; bounds
(with-test (:name (:cas :svref :bounds))
  (multiple-value-bind (res err)
      (ignore-errors (compare-and-swap (svref *v* -1) 1 2))
    (assert (not res))
    (assert (typep err 'type-error)))
  (multiple-value-bind (res err)
      (ignore-errors (compare-and-swap (svref *v* 1) 1 2))
    (assert (not res))
    (assert (typep err 'type-error))))

;; type of the first argument
(with-test (:name (:cas :svref :type))
  (multiple-value-bind (res err)
      (ignore-errors (compare-and-swap (svref (eval "foo") 1) 1 2))
    (assert (not res))
    (assert (typep err 'type-error))))

;; Check that we don't modify constants
(defconstant +a-constant+ 42)
(with-test (:name (:cas :symbol-value :constant-modification))
  (assert
   (eq :error
       (handler-case
           (sb-ext:compare-and-swap (symbol-value '+a-constant+) 42 13)
         (error () :error))))
  (let ((name '+a-constant+))
    (assert
     (eq :error
         (handler-case
             (sb-ext:compare-and-swap (symbol-value name) 42 13)
           (error () :error))))))

;; Check that we don't mess declaimed types
(declaim (boolean *a-boolean*))
(defparameter *a-boolean* t)
(with-test (:name (:cas :symbol-value :type-checking))
  (assert
   (eq :error
       (handler-case
           (sb-ext:compare-and-swap (symbol-value '*a-boolean*) t (eval 42))
         (error () :error))))
  (let ((name '*a-boolean*))
    (assert
     (eq :error
         (handler-case
             (sb-ext:compare-and-swap (symbol-value name) t (eval 42))
           (error () :error))))))

;;;; ATOMIC-INCF and ATOMIC-DECF (we should probably rename this file atomic-ops...)

(defstruct box
  (word 0 :type sb-vm:word))

;; Have the following tests check that CAS access to the superclass
;; works in the presence of a subclass sharing the conc-name.
(defstruct (subbox (:include box) (:conc-name "BOX-")))

(defun inc-box (box n)
  (declare (fixnum n) (box box))
  (loop repeat n
        do (sb-ext:atomic-incf (box-word box))))

(defun dec-box (box n)
  (declare (fixnum n) (box box))
  (loop repeat n
        do (sb-ext:atomic-decf (box-word box))))

(with-test (:name :atomic-incf/decf)
  (let ((box (make-box)))
    (inc-box box 10000)
    (assert (= 10000 (box-word box)))
    (dec-box box 10000)
    (assert (= 0 (box-word box)))))

(with-test (:name :atomic-incf-wraparound)
  (let ((box (make-box :word sb-ext:most-positive-word)))
    (sb-ext:atomic-incf (box-word box) 2)
    (assert (= 1 (box-word box)))))

(with-test (:name :atomic-decf-wraparound)
  (let ((box (make-box :word 0)))
    (sb-ext:atomic-decf (box-word box) 2)
    (assert (= (- (ash 1 sb-vm:n-word-bits) 2) (box-word box)))))

(with-test (:name :cas-raw-instance-ref-word
            :skipped-on (not (or :x86 :x86-64)))
  (let ((foo (make-box :word 42)))
    ;; basic smoke test - not checking for atomicity or anything
    (assert (eql (cas (box-word foo) 42 43) 42))
    (assert (eql (cas (box-word foo) 43 44) 43))))

(with-test (:name :atomic-incf-full-call-lp1381867
            :skipped-on (not (or :x86 :x86-64 :ppc)))
  ;; contortions to avoid reader errors
  (let* ((%riai/w (intern "%RAW-INSTANCE-ATOMIC-INCF/WORD" "SB-KERNEL"))
         (form
          `(locally
            ;; Rebind %RAW-INSTANCE-ATOMIC-INCF/WORD as a local
            ;; function.  Declaring it locally notinline fails because
            ;; it is marked with the ALWAYS-TRANSLATABLE attribute, so
            ;; it's a bug to call it even though we've asked to call
            ;; it. (Maybe that's a bug?)  And I don't want to call it
            ;; explictly - I want the macro to do it so that I don't
            ;; have to inject any of the sign masking noise and such.
            (declare (disable-package-locks ,%riai/w))
            (let ((b (make-box :word 0))
                  (delta (- (ash 1 (1- sb-vm:n-word-bits))))
                  (f (fdefinition ',%riai/w)))
              (flet ((,%riai/w (a b c) (funcall f a b c)))
                (assert (= (atomic-incf (box-word b) delta) 0))
                (assert (= (atomic-incf (box-word b) delta)
                           (ash 1 (1- sb-vm:n-word-bits))))
                (assert (= (box-word b) 0))
                (atomic-decf (box-word b))
                (assert (= (box-word b) sb-ext:most-positive-word)))))))
    (eval form)))

#+sb-thread
(with-test (:name (:atomic-incf/decf :threads))
  (let* ((box (make-box))
         (threads (loop repeat 64
                        collect (sb-thread:make-thread (lambda ()
                                                         (inc-box box 1000)
                                                         (dec-box box 10000)
                                                         (inc-box box 10000)
                                                         (dec-box box 1000))
                                                       :name "inc/dec thread"))))
    (mapc #'sb-thread:join-thread threads)
    (assert (= 0 (box-word box)))))

(defglobal **my-atomic-counter* 0)
(declaim (fixnum **my-atomic-counter*))
;; Assert that safe (atomic-incf car) type-checks the car.
(with-test (:name :atomic-incf-car-safe)
  (let ((x (cons (1+ most-positive-fixnum) 0))) ; a bignum
    (assert (eq (handler-case (atomic-incf (car x))
                  (type-error () 'win)) 'win))))

;; Basic correctness tests, not testing atomicity
(macrolet
    ((test-place (place)
       `(progn
          ;; using a constant for the delta
          (assert (eq (atomic-incf ,place 2) 0))
          (assert (eq ,place 2))
          (assert (eq (atomic-incf ,place -1) 2))
          (assert (eq ,place 1))
          (assert (eq (atomic-decf ,place 1) 1))
          (assert (eq ,place 0))
          ;; using a non-constant for the delta
          (assert (eq (atomic-incf ,place (eval 2)) 0))
          (assert (eq ,place 2))
          (assert (eq (atomic-incf ,place (eval -1)) 2))
          (assert (eq ,place 1))
          (assert (eq (atomic-decf ,place (eval 1)) 1))
          (assert (eq ,place 0))
          (setf ,place most-positive-fixnum)
          (atomic-incf ,place)
          (assert (eq ,place most-negative-fixnum))
          (atomic-decf ,place)
          (assert (eq ,place most-positive-fixnum)))))
  (with-test (:name (:atomic-incf :global-var))
    (test-place **my-atomic-counter*))
  (with-test (:name (:atomic-incf :car))
    (let ((x (cons 0 'foo)))
      (test-place (car x))))
  (with-test (:name (:atomic-incf :cdr))
    (let ((x (cons 'foo 0)))
      (test-place (cdr x))))
  ;; Fast code for (atomic-{incf|decf} {car|cdr}) is decidedly unsafe
  ;; on x86-64. Ensure basic correctness when used correctly.
  (with-test (:name (:atomic-incf-fast :car))
    (let ((x (cons 0 'foo)))
      (declare (optimize (safety 0)))
      (test-place (car x))))
  (with-test (:name (:atomic-incf-fast :cdr))
    (let ((x (cons 'foo 0)))
      (declare (optimize (safety 0)))
      (test-place (cdr x)))))

;;; STANDARD-INSTANCE-ACCESS, FUNCALLABLE-STANDARD-INSTANCE-ACCESS

(defclass sia-cas-test ()
  ((a :initarg :a)
   (b :initarg :b)))

(with-test (:name (:cas :standard-instance-access))
  (flet ((slot-loc (slot class)
           (sb-mop:slot-definition-location
            (find slot (sb-mop:class-slots class) :key #'sb-mop:slot-definition-name))))
    (let* ((class (find-class 'sia-cas-test))
           (instance (make-instance class :a 'a :b 'b))
           (a-loc (slot-loc 'a class))
           (b-loc (slot-loc 'b class)))
      (assert (eq 'a (slot-value instance 'a)))
      (assert (eq 'a (compare-and-swap (sb-mop:standard-instance-access instance a-loc)
                                       'x 'oops)))
      (assert (eq 'a (sb-mop:standard-instance-access instance a-loc)))
      (assert (eq 'a (compare-and-swap (sb-mop:standard-instance-access instance a-loc)
                                       'a 'a2)))
      (assert (eq 'a2 (sb-mop:standard-instance-access instance a-loc)))
      (assert (eq 'a2 (slot-value instance 'a)))
      (assert (eq 'b (slot-value instance 'b)))
      (assert (eq 'b (sb-mop:standard-instance-access instance b-loc))))))

(defclass fia-cas-test (sb-mop:funcallable-standard-object)
  ((a :initarg :a)
   (b :initarg :b))
  (:metaclass sb-mop:funcallable-standard-class))

(with-test (:name (:cas :standard-instance-access))
  (flet ((slot-loc (slot class)
           (sb-mop:slot-definition-location
            (find slot (sb-mop:class-slots class) :key #'sb-mop:slot-definition-name))))
    (let* ((class (find-class 'fia-cas-test))
           (instance (make-instance class :a 'a :b 'b))
           (a-loc (slot-loc 'a class))
           (b-loc (slot-loc 'b class)))
      (sb-mop:set-funcallable-instance-function instance (lambda () :ok))
      (eq :ok (funcall instance))
      (assert (eq 'a (slot-value instance 'a)))
      (assert (eq 'a (compare-and-swap
                      (sb-mop:funcallable-standard-instance-access instance a-loc)
                      'x 'oops)))
      (assert (eq 'a (sb-mop:funcallable-standard-instance-access instance a-loc)))
      (assert (eq 'a (compare-and-swap
                      (sb-mop:funcallable-standard-instance-access instance a-loc)
                                       'a 'a2)))
      (assert (eq 'a2 (sb-mop:funcallable-standard-instance-access instance a-loc)))
      (assert (eq 'a2 (slot-value instance 'a)))
      (assert (eq 'b (slot-value instance 'b)))
      (assert (eq 'b (sb-mop:funcallable-standard-instance-access instance b-loc))))))

;;; SLOT-VALUE

(defclass standard-thing ()
  ((x :initform 42)
   (y)))

(defmethod slot-unbound ((class standard-class) (obj standard-thing) slot)
  (list :unbound slot))

(defmethod slot-missing ((class standard-class) (obj standard-thing) slot op &optional val)
  (list :missing slot op val))

(with-test (:name (:cas :slot-value :standard-object))
  (let ((x (make-instance 'standard-thing)))
    (assert (eql 42 (slot-value x 'x)))
    (assert (eql 42 (compare-and-swap (slot-value x 'x) 0 :foo)))
    (assert (eql 42 (slot-value x 'x)))
    (assert (eql 42 (compare-and-swap (slot-value x 'x) 42 :foo)))
    (assert (eql :foo (slot-value x 'x)))))

(with-test (:name (:cas :slot-value :slot-unbound))
  (let ((x (make-instance 'standard-thing)))
    (assert (equal '(:unbound y) (slot-value x 'y)))
    (assert (equal '(:unbound y) (compare-and-swap (slot-value x 'y) 0 :foo)))
    (assert (equal '(:unbound y) (slot-value x 'y)))
    (assert (eq sb-pcl:+slot-unbound+
                (compare-and-swap (slot-value x 'y) sb-pcl:+slot-unbound+ :foo)))
    (assert (eq :foo (slot-value x 'y)))))

(with-test (:name (:cas :slot-value :slot-missing))
  (let ((x (make-instance 'standard-thing)))
    (assert (equal '(:missing z slot-value nil) (slot-value x 'z)))
    (assert (equal '(:missing z sb-ext:cas (0 :foo)) (compare-and-swap (slot-value x 'z) 0 :foo)))
    (assert (equal '(:missing z slot-value nil) (slot-value x 'z)))))

(defclass non-standard-class (standard-class)
  ())

(defmethod sb-mop:validate-superclass ((class non-standard-class) (superclass standard-class))
  t)

(defclass non-standard-thing-0 ()
  ((x :initform 13))
  (:metaclass non-standard-class))

(defclass non-standard-thing-1 ()
  ((x :initform 13))
  (:metaclass non-standard-class))

(defclass non-standard-thing-2 ()
  ((x :initform 13))
  (:metaclass non-standard-class))

(defclass non-standard-thing-3 ()
  ((x :initform 13))
  (:metaclass non-standard-class))

(defvar *access-list* nil)

(defmethod sb-mop:slot-value-using-class
    ((class non-standard-class) (obj non-standard-thing-1) slotd)
  (let ((v (call-next-method)))
    (push :read *access-list*)
    v))

(defmethod (setf sb-mop:slot-value-using-class)
    (value (class non-standard-class) (obj non-standard-thing-2) slotd)
  (let ((v (call-next-method)))
    (push :write *access-list*)
    v))

(defmethod sb-mop:slot-boundp-using-class
    ((class non-standard-class) (obj non-standard-thing-3) slotd)
  (let ((v (call-next-method)))
    (push :boundp *access-list*)
    v))

(with-test (:name (:cas :slot-value :non-standard-object :standard-access))
  (let ((x (make-instance 'non-standard-thing-0)))
    (assert (eql 13 (slot-value x 'x)))
    (assert (eql 13 (compare-and-swap (slot-value x 'x) 0 :bar)))
    (assert (eql 13 (slot-value x 'x)))
    (assert (eql 13 (compare-and-swap (slot-value x 'x) 13 :bar)))
    (assert (eql :bar (slot-value x 'x)))))

(with-test (:name (:cas :slot-value :non-standard-object :slot-value-using-class))
  (setf *access-list* nil)
  (let ((x (make-instance 'non-standard-thing-1)))
    (declare (notinline slot-value))
    (assert (null *access-list*))
    (assert (eql 13 (slot-value x 'x)))
    (assert (equal '(:read) *access-list*))
    (assert (eq :error
                (handler-case
                    (compare-and-swap (slot-value x 'x) 0 :bar)
                  (error () :error))))
    (assert (eql 13 (slot-value x 'x)))
    (assert (equal '(:read :read) *access-list*))))

(with-test (:name (:cas :slot-value :non-standard-object :setf-slot-value-using-class))
  (setf *access-list* nil)
  (let ((x (make-instance 'non-standard-thing-2)))
    (assert (equal '(:write) *access-list*))
    (assert (eql 13 (slot-value x 'x)))
    (assert (equal '(:write) *access-list*))
    (assert (eq :error
                (handler-case
                    (compare-and-swap (slot-value x 'x) 0 :bar)
                  (error () :error))))
    (assert (eql 13 (slot-value x 'x)))
    (assert (equal '(:write) *access-list*))))

(with-test (:name (:cas :slot-value :non-standard-object :slot-boundp-using-class))
  (setf *access-list* nil)
  (let ((x (make-instance 'non-standard-thing-3)))
    (assert (equal '(:boundp) *access-list*))
    (assert (eql 13 (slot-value x 'x)))
    (assert (eq :error
                (handler-case
                    (compare-and-swap (slot-value x 'x) 0 :bar)
                  (error () :error))))
    (assert (eql 13 (slot-value x 'x)))))

(defvar *foo* nil)

(defun foo ()
  *foo*)

(defun (cas foo) (old new)
  (cas (symbol-value '*foo*) old new))

(with-test (:name (:cas :defun))
  (assert (null (foo)))
  (assert (null (cas (foo) nil t)))
  (assert (eq t (foo)))
  (assert (eq t (cas (foo) nil :oops)))
  (assert (eq t (foo))))

(with-test (:name (:cas :flet))
  (let (x)
    (flet (((cas x) (old new)
             (let ((tmp x))
               (when (eq tmp old)
                 (setf x new))
               tmp))
           (x ()
             x))
      (assert (null (x)))
      (assert (null (cas (x) nil t)))
      (assert (eq t (x)))
      (assert (eq t (cas (x) nil :oops)))
      (assert (eq t (x))))))

(defgeneric (cas thing) (old new thing))

(defmethod (cas thing) (old new (thing cons))
  (cas (car thing) old new))

(defmethod (cas thing) (old new (thing symbol))
  (cas (symbol-value thing) old new))

(defgeneric thing (thing)
  (:method ((x cons))
    (car x))
  (:method ((x symbol))
    (symbol-value x)))

(with-test (:name (:cas :defgeneric))
  (let ((a (list nil))
        (b (gensym "X")))
    (set b nil)
    (assert (null (thing a)))
    (assert (null (thing b)))
    (assert (null (cas (thing a) nil t)))
    (assert (null (cas (thing b) nil t)))
    (assert (eq t (thing a)))
    (assert (eq t (thing b)))
    (assert (eq t (cas (thing a) nil :oops)))
    (assert (eq t (cas (thing b) nil :oops)))
    (assert (eq t (thing a)))
    (assert (eq t (thing b)))))

;;; SYMBOL-VALUE with a constant argument used to return a bogus read-form
(with-test (:name :symbol-value-cas-expansion)
  (multiple-value-bind (vars vals old new cas-form read-form)
      (get-cas-expansion `(symbol-value t))
    (declare (ignore old new cas-form))
    (assert (not vars))
    (assert (not vals))
    (assert (eq t (eval read-form))))
  (multiple-value-bind (vars vals old new cas-form read-form)
      (get-cas-expansion `(symbol-value *))
    (declare (ignore old new cas-form))
    (let ((* :foo))
      (assert (eq :foo
                  (eval `(let (,@(mapcar 'list vars vals))
                      ,read-form)))))
    (let ((* :bar))
      (assert (eq :bar
                  (eval `(let (,@(mapcar 'list vars vals))
                      ,read-form)))))))

(with-test (:name :atomic-push
            :skipped-on (not :sb-thread))
  (let ((store (cons nil nil))
        (n 100000))
    (symbol-macrolet ((x (car store))
                      (y (cdr store)))
      (dotimes (i n)
        (push i y))
      (mapc #'sb-thread:join-thread
            (loop repeat (ecase sb-vm:n-word-bits (32 100) (64 1000))
                  collect (sb-thread:make-thread
                           (lambda ()
                             (loop for z = (atomic-pop y)
                                   while z
                                   do (atomic-push z x)
                                      (sleep 0.00001))))))
      (assert (not y))
      (assert (eql n (length x))))))

(with-test (:name :local-special-symbol-value)
  (assert
   (= (funcall (compile nil
                        `(lambda ()
                           (let ((x 10))
                             (declare (special x))
                             (cas (symbol-value 'x) 10 12)
                             x))))
      12))
  (assert
   (= (funcall
       (compile nil
                `(lambda ()
                   (let ((x (list 1)))
                     (declare (special x))
                     (atomic-pop (symbol-value 'x))))))
      1)))

(defclass cas-fsc (generic-function)
  ((a :initform 2))
  (:metaclass sb-mop:funcallable-standard-class))

(with-test (:name :cas-funcallable-instance)
  (let ((x (make-instance 'cas-fsc)))
    (assert
     (= (funcall (compile nil
                          `(lambda (x)
                             (cas (slot-value x 'a) 0 1)))
                 x)
        2))
    (assert (eql (slot-value x 'a) 2))
    (assert
     (= (funcall (compile nil
                          `(lambda (x)
                             (cas (slot-value x 'a) 2 3)))
                 x)
        2))
    (assert (eql (slot-value x 'a) 3))))

(in-package "SB-VM")

;;; This file defines a structure, so is an 'impure' test
(defstruct my-struct
  ;; The slots under test have to be naturally aligned for a double-Lispword,
  ;; at least on x86-64, so add a random slot if there is no layout slot.
  #+compact-instance-header fluff
  one two three four)

(locally
  (declare (muffle-conditions style-warning)) ; functions don't exist for non-x86
  (defun test-a-cons (acons oldcar oldcdr newcar newcdr)
    (%cons-cas-pair acons oldcar oldcdr newcar newcdr))
  (defun test-a-vect (avect ind old1 old2 new1 new2)
    (%vector-cas-pair avect ind old1 old2 new1 new2))
  (defun test-a-struct (inst ind old1 old2 new1 new2)
    (%instance-cas-pair inst ind old1 old2 new1 new2))

  (defun test-wide-cmpxchg ()
    (let ((x (cons 'a 'b)))
      (multiple-value-bind (old1 old2) (test-a-cons x 'a 'b 'foo 'bar)
        (assert (and (eq old1 'a) (eq old2 'b) (equal x '(foo . bar)))))
      (multiple-value-bind (old1 old2) (test-a-cons x 0 0 1 2)
        (assert (and (eq old1 'foo) (eq old2 'bar) (equal x '(foo . bar))))))

    ;; This is just testing that the offsets are correct.
    ;; Correct working of the instruction is tested by the CONS example.
    (let ((x (make-array 6 :initial-element nil)))
      (multiple-value-bind (old1 old2) (test-a-vect x 2 nil nil 'foo 'bar)
        (assert (and (null old1) (null old2) (equalp x #(nil nil foo bar nil nil))))))

    ;; Same remark applies - just check that the offset to the slot is right.
    (let ((s (make-my-struct :three 'the :four 'floor)))
      ;; in slots 3 and 4 put your bootee (a baby shoe, i.e.) on the floor
      (multiple-value-bind (old1 old2) (test-a-struct s 3 'the 'floor 'your 'bootee)
        (assert (and (eq old1 'the) (eq old2 'floor)
                     (eq (my-struct-three s) 'your)
                     (eq (my-struct-four s) 'bootee)))))
    t))

(test-util:with-test (:name :wide-compare-and-exchange
                      :skipped-on (not (or :x86 :x86-64)))
  (multiple-value-bind (a b c d) (%cpu-identification 0 0)
    (declare (ignore b c d))
    ;; paranoidly check for whether we can execute function ID 1
    (or (and (>= a 1) ; the highest function ID
             (multiple-value-bind (a b c d) (%cpu-identification 1 0)
               (declare (ignore a b) (ignorable c d))
               ;; paranoidly check for CMPXCHGxB presence
               ;; constants from Table 3-20 and 3-21 of Intel manual
               (and #+x86(logbitp 8 d) #+x86-64(logbitp 13 c)
                    (test-wide-cmpxchg))))
        (format t "Double-width compare-and-swap NOT TESTED~%"))))

(test-util:with-test (:name :cas-sap-ref-smoke-test
                            :fails-on :riscv ; unsigned-32-bit gets the wrong answer
                            :skipped-on (not :sb-thread))
  (let ((data (make-array 1 :element-type 'sb-vm:word)))
    (sb-sys:with-pinned-objects (data)
      (let ((sap (sb-sys:vector-sap data)))
        ;; It's important to exercise an initial value that has lots of bits on,
        ;; because I made at least two mistakes in the x86-64 lispword-sized vop:
        ;;  1. it was using a :dword move where it should have used a :qword
        ;;  2. it was emitting constants as bignums instead of inline raw constants
        (macrolet ((test (signedp nbits newval
                          &aux (ref (symbolicate (if signedp "SIGNED-" "")
                                                 "SAP-REF-" nbits))
                               (init (if signedp -1
                                         (ldb (byte nbits 0) most-positive-word))))
                     `(progn
                        ;; (format t "Testing ~a with initial bits ~x~%" ',ref ,init)
                        (setf (,ref sap 0) ,init)
                        (let ((old (cas (,ref sap 0) 0 5)))
                          (assert (eql old ,init)) ; actual old
                          (assert (eql (,ref sap 0) ,init))) ; memory should not have changed
                        (let ((old (cas (,ref sap 0) ,init ,newval)))
                          (assert (eql old ,init)) ; actual old
                          (assert (eql (,ref sap 0) ,newval)))))) ; should have changed
          #+64-bit (test nil 64 #xdeadc0fefe00)
          #+64-bit (test t   64 most-negative-fixnum)
          (test nil 32 #xbabab00e)
          ;; on riscv I did not implement these sizes, and
          ;; ppc64 might get "illegal instruction" depending on the particular CPU
          #-(or riscv ppc64) (test nil 16 #xfafa)
          #-(or riscv ppc64) (test nil 8 #xbb)
          )
        ;; SAP-REF-SAP
        (setf (aref data 0) 0)
        (let ((old (cas (sap-ref-sap sap 0) (int-sap 1) (int-sap #xffff))))
          (assert (sb-sys:sap= old (int-sap 0))) ; actual old
          (assert (sb-sys:sap= (sap-ref-sap sap 0) (int-sap 0)))) ; memory should not have changed
        (let ((old (cas (sap-ref-sap sap 0) (int-sap 0) (int-sap sb-ext:most-positive-word))))
          (assert (sb-sys:sap= old (int-sap 0)))
          (assert (sb-sys:sap= (sap-ref-sap sap 0) (int-sap sb-ext:most-positive-word))))
        ;; SAP-REF-LISPOBJ
        (setf (aref data 0) sb-vm:nil-value)
        (let ((old (cas (sap-ref-lispobj sap 0) t '*print-base*)))
          (assert (eq old nil)) ; actual old
          (assert (eq (sap-ref-lispobj sap 0) nil))) ; memory should not have changed
        (let ((old (cas (sap-ref-lispobj sap 0) nil t)))
          (assert (eq old nil))
          (assert (eq (sap-ref-lispobj sap 0) t)))))))

(test-util:with-test (:name :cas-sb16
                      :skipped-on (or :interpreter
                                      (not (or :arm64 :x86-64))))
 (let ((a (make-array 1 :element-type '(signed-byte 16))))
   (flet ((cas-sb16 (sap old new)
           (cas (sb-sys:signed-sap-ref-16 sap 0) old new)))
     (setf (aref a 0) -1000)
     (loop for old from -1000 to 1000 do
        (sb-sys:with-pinned-objects (a)
          (let ((actual (cas-sb16 (sb-sys:vector-sap a) old (1+ old))))
            (assert (= actual old))
            (assert (= (aref a 0) (1+ old)))))))))

(test-util:with-test (:name :cas-sap-ref-stress-test
                            :skipped-on (not :sb-thread))
  (let ((data (make-array 1 :element-type 'sb-vm:word
                             :initial-element 0)))
    (sb-sys:with-pinned-objects (data)
      (let ((sap (sb-sys:vector-sap data))
            (n-threads 3)
            (n-increments 100000)
            (threads))
        (flet ((increment ()
                 (let ((fails 0))
                   (dotimes (i n-increments fails)
                     (let ((old (sap-ref-32 sap 0)))
                       (loop
                          (let ((actual (cas (sap-ref-32 sap 0) old (1+ old))))
                            (if (eq actual old) (return))
                            (incf fails)
                            (setq old actual))))))))
          (dotimes (i n-threads)
            (push (sb-thread:make-thread #'increment) threads))
          (mapc 'sb-thread:join-thread threads)
          (assert (= (sap-ref-32 sap 0) (* n-threads n-increments))))))))

(define-alien-variable "small_generation_limit" (signed 8))
;; PPC64 shouldn't fail, but depending on the CPU revision it might not
;; have the needed instruction, and I don't know how to test for it.
;; And surely it doesn't really depend on endian-ness, but the machine
;; that I'm testing on which is little-endian passes the test.
#+(and sb-thread (or x86-64 (and ppc64 little-endian)))
(progn
(defun cas-an-alien-byte (x y) (cas small-generation-limit x y))
(compile 'cas-an-alien-byte)
(test-util:with-test (:name :cas-alien)
  (assert (= small-generation-limit 1))
  (assert (= (cas-an-alien-byte 0 5) 1))
  (assert (= (cas-an-alien-byte 1 6) 1))
  (assert (= small-generation-limit 6))
  (setf small-generation-limit 1)))

(test-util:with-test (:name :cas-aref
                      :skipped-on (not (or :arm64 :x86-64)))
  (dolist (bits '(8 16 32 #+64-bit 64))
    (let ((unsigned (make-array '(3 3) :element-type `(unsigned-byte ,bits)
                                :initial-element 0))
          (signed (make-array '(3 3) :element-type `(signed-byte ,bits)
                              :initial-element 0))
          (uint 0)
          (sint -5))
      (dotimes (i 3)
        (dotimes (j 3)
          (cas (aref unsigned i j) 0 (incf uint))
          (cas (aref signed i j) 0 (incf sint))))
      (assert (equalp unsigned #2A((1 2 3) (4 5 6) (7 8 9))))
      (assert (equalp signed #2A((-4 -3 -2) (-1 0 1) (2 3 4)))))))

(test-util:with-test (:name :cas-aref-float :skipped-on (not :x86-64))
  (dolist (et '(single-float double-float))
    (let ((a (make-array 4 :element-type et)))
      (dotimes (i 4)
        (cas (aref a i) (coerce 0 et) (coerce (1+ i) et)))
      (assert (equalp a #(1d0 2d0 3d0 4d0))))))
