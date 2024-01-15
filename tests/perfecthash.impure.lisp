
;;; The perfect hash generator can process any set of UB32 values.
;;; For example:
#|
(sb-impl:make-perfect-hash-lambda
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
         (f (compile nil (sb-impl:make-perfect-hash-lambda keys))))
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
    (let ((expr (sb-impl:make-perfect-hash-lambda hashes)))
      #+nil
      (let ((*package* (find-package "SB-IMPL"))
            (*print-length* 20) ; don't show huge arrays
            (*print-right-margin* 200))
        (pprint expr)
        (terpri))
      (values (compile nil expr) expr))))

(with-test (:name :execrcise-perfect-hash-generator)
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

;;; To exercise all the cases from 'h8a'..'h8i' and 'hna'..'hnr'
;;; we have to use many different set cardinalities.
;;; Even then there's no guarantee to hit them all.
;;; But I did manage to expose some syntax errors in my translation
;;; from C to Lisp. It would be nice if the output constructors
;;; could be more stylish than just printf.
;;; Unfortunately the algorithm can get really slow on certain
;;; sets, and then go fast again when adding _more_ symbols.
#+nil
(with-test (:name :try-various-sizes)
  (let ((hashes (make-hash-table))
        (symbols))
    (do-all-symbols (sym)
      (let ((hash (ldb (byte 32 0) (sb-kernel:symbol-hash sym))))
        (unless (gethash hash hashes)
          (setf (gethash hash hashes) t)
          (push sym symbols)
          (when (> (hash-table-count hashes) 4)
            (when (zerop (random 100))
              (format t "~&~D symbols~%" (hash-table-count hashes))
              (let ((fun (generate-perfect-hashfun symbols)))
                (test-perfect-hashfun fun symbols)))))))))
