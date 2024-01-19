
;;; The perfect hash generator can process any set of UB32 values.
;;; For example:
#|
(sb-c:make-perfect-hash-lambda
  (map '(array (unsigned-byte 32) 1) (lambda (x) (ldb (byte 32 0) (sxhash x)))
       '(a b c d e f g h i j k l m n o p)))
 =>
(LAMBDA (ARG &AUX (VAL (TRULY-THE (UNSIGNED-BYTE 32) ARG)))
  (DECLARE (OPTIMIZE (SAFETY 0) (SB-C:STORE-SOURCE-FORM 0)))
  (MACROLET ((& (A B)
               `(LOGAND ,A ,B))
             (^ (A B)
               `(LOGXOR ,A ,B))
             (<< (N C)
               `(LOGAND (ASH ,N ,C) 4294967295))
             (>> (N C)
               `(ASH ,N (- ,C))))
    (LET ((TAB #(0 12 11 13 0 0 12 1)))
      (LET ((B (& (>> VAL 13) 7)))
        (LET ((A (>> (<< VAL 5) 29)))
(^ A (AREF TAB B)))))))
|#

(defun test-perfect-hashfun (fun keys &optional print)
  (when (listp keys)
    (setq keys (coerce keys 'vector)))
  (let* ((n (length keys))
         (seen (make-array n :initial-element nil)))
    (sb-int:dovector (key keys)
      (let ((hash
             (etypecase key
               ((unsigned-byte 32) (funcall fun key))
               (symbol
                (funcall fun (ldb (byte 32 0) (sb-kernel:symbol-hash key)))))))
        (when print (format t "~s -> ~d~%" key hash))
        ;; Can't exceed N-1, and can't repeat
        (when (or (>= hash n) (aref seen hash))
          (error "Hash was not perfect"))
        (setf (aref seen hash) t)))))

(with-test (:name :typo-example-1) ; of which there may be more
  (let* ((keys (make-array 5 :element-type '(unsigned-byte 32)
                           :initial-contents #(3739790036 1578584344 2172243460 496160493 696125627)))
         (f (compile nil (sb-c:make-perfect-hash-lambda keys))))
    (test-perfect-hashfun f keys)))

;;; I have no plans to actually use perfect hashing of symbols in packages,
;;; since packages can have symools added and removed at runtime.
;;; However, they serve as a nice source of test data.
(defun get-all-syms (pkg table-selector)
  (let ((pkg (find-package pkg)))
    (labels ((filter (reader)
               (coerce
                (remove-if-not 'symbolp
                               (sb-impl::symtbl-cells (funcall reader pkg)))
                'list))
             (int () (filter 'sb-impl::package-internal-symbols))
             (ext () (filter 'sb-impl::package-external-symbols)))
      (case table-selector
        (:internal (int))
        (:external (ext))
        (:both (nconc (int) (ext)))))))

(defun generate-perfect-hashfun (symbols)
  (let ((hashes
         (map '(simple-array (unsigned-byte 32) (*))
              (lambda (x) (ldb (byte 32 0) (sb-kernel:symbol-hash x)))
              symbols)))
    ;; if symbols hash the same, we have a real problem
    (assert (= (length (remove-duplicates hashes)) (length hashes)))
    (let ((expr (sb-c:make-perfect-hash-lambda hashes)))
      #+nil
      (let ((*package* (find-package "SB-IMPL"))
            (*print-readably* t)
            (*print-length* 20) ; don't show huge arrays
            (*print-right-margin* 200))
        (pprint expr)
        (terpri))
      (values (compile nil expr) expr))))

(with-test (:name :exercise-perfect-hash-generator)
  (format t "~&Symbol count:")
  (dolist (p (list-all-packages))
    ;; Get a bunch of symbols, generate a perfect hash function,
    ;; and then assert that it is one.
    (dolist (symbol-set '(:internal :external :both))
      (let ((symbols (get-all-syms p symbol-set)))
        (when (> (length symbols) 4)
          (format t " ~D" (length symbols))
          (force-output)
          (let ((f (generate-perfect-hashfun symbols)))
            (test-perfect-hashfun f symbols))))))
  (terpri))

(defun find-tables (expression)
  (let (tab scramble)
    (sb-int:named-let recurse ((x expression))
      (when (consp x)
        (cond ((eq (car x) 'let)
               (let* ((bindings (second x))
                      (binding (first bindings))
                      (var (first binding)))
                 (cond ((string= var "SCRAMBLE")
                        (setq scramble (second binding)))
                       ((string= var "TAB")
                        (setq tab (second binding)))))
               (recurse (cddr x)))
              (t
               (recurse (car x))
               (recurse (cdr x))))))
    (values scramble tab)))

(defun random-subset (random-state keys n)
  (let ((result (make-array n :element-type '(unsigned-byte 32)))
        (picked (make-hash-table)))
    (dotimes (i n result)
      (loop
        (let* ((pick (random (length keys) random-state))
               (key (aref keys pick)))
          (unless (gethash key picked)
            (setf (gethash key picked) t)
            (setf (aref result i) key)
            (return)))))))

;;; To exercise all the cases from 'h8a'..'h8i' and 'hna'..'hnr'
;;; we have to use many different set cardinalities.
;;; Even then there's no guarantee to hit them all.
;;; But I did manage to expose some syntax errors in my translation
;;; from C to Lisp. It would be nice if the output constructors
;;; could be more stylish than just printf.
;;; Unfortunately the algorithm can get really slow on certain
;;; sets, and then go fast again when adding _more_ symbols.
;;; This usually doesn't take more than about 5 seconds on a good machine,
;;; but it's pretty random how long it takes.
(with-test (:name :try-various-sizes :skipped-on (:not :slow))
  (let ((hashes (make-hash-table))
        (random-state (make-random-state t))) ; randomly-seeded random-state
    (dolist (str (sb-vm:list-allocated-objects :all :test #'stringp))
      (let ((hash (ldb (byte 32 0) (sxhash str))))
        (unless (gethash hash hashes)
          (setf (gethash hash hashes) t))))
    (let ((a (make-array (hash-table-count hashes))))
      (format t "~&~D unique hashes~%" (length a))
      (let ((i -1))
        (sb-int:dohash ((k v) hashes)
          (declare (ignore v))
          (setf (aref a (incf i)) k)))
      (let (trials)
        ;; Generate random sizes to try.
        (dotimes (i 100)
          (let ((n (floor (expt 10 (random 4.5 random-state)))))
            (when (and (>= n 5) (<= n (/ (length a) 2)))
              (push n trials))))
        ;; Try those sizes
        (dolist (n (sort trials #'<))
          (let* ((subset (random-subset random-state a n))
                 (lambda (sb-c:make-perfect-hash-lambda subset)))
            (multiple-value-bind (scramble tab) (find-tables lambda)
              (let ((fun (compile nil lambda)))
                (test-perfect-hashfun fun subset)
                (let ((*print-pretty* nil))
                  (format t "~&~5D keys:~@[ tab=~S~]~@[ scramble=~S~]~%"
                          n
                          (if tab (type-of tab))
                          (if scramble (type-of scramble))))))))))))
