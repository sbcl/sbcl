(defstruct foo)

(with-test (:name :xset-hash-equal)
  (let ((list `(sym ,(make-foo) 1 -12d0 4/3 #\z ,(ash 1 70)))
        (a (sb-int:alloc-xset)))
    (dolist (elt list)
      (sb-int:add-to-xset elt a))
    (sb-kernel::xset-generate-stable-hashes a)
    (dotimes (i 10)
      (let ((b (sb-int:alloc-xset)))
        (dolist (elt (shuffle list))
          (sb-int:add-to-xset elt b))
        (sb-kernel::xset-generate-stable-hashes b)
        (assert (sb-int:xset= a b))
        (assert (= (sb-int:xset-elts-hash a)
                   (sb-int:xset-elts-hash b)))))))

(with-test (:name :xset-hash-numerics-better)
  ;; It was very easy to make xsets which collided on their hash if any
  ;; elements got assigned a stable hash. In such xsets, the non-pointer elements
  ;; could hash to themselves. This meant that any set of fixnums which summed
  ;; to the same as another set of fixnums might hash identically
  (let ((str "hi")
        (xset1 (sb-int:alloc-xset))
        (xset2 (sb-int:alloc-xset)))
    (dolist (e `(1 ,str 10)) (sb-int:add-to-xset e xset1))
    (dolist (e `(8 3 ,str)) (sb-int:add-to-xset e xset2))
    (sb-kernel::xset-generate-stable-hashes xset1)
    (sb-kernel::xset-generate-stable-hashes xset2)
    (assert (/= (sb-int:xset-elts-hash xset1) (sb-int:xset-elts-hash xset2)))))

(with-test (:name :xset-fast-union)
  (let ((s1 (sb-int:alloc-xset))
        (s2 (sb-int:alloc-xset)))
    (sb-int:add-to-xset #\a s1)
    (sb-int:add-to-xset #\b s1)
    (sb-int:add-to-xset #\c s1)
    (assert (eq (sb-int:xset-union s1 s2) s1))
    (assert (eq (sb-int:xset-union s2 s1) s1))
    (sb-int:add-to-xset #\b s2)
    (assert (eq (sb-int:xset-union s1 s2) s1))
    (assert (eq (sb-int:xset-union s2 s1) s1)))
  (let ((s1 (sb-int:alloc-xset))
        (s2 (sb-int:alloc-xset)))
    (loop for i from (char-code #\a) to (char-code #\z)
          do (sb-int:add-to-xset (code-char i) s1))
    (sb-int:add-to-xset #\x s2)
    (loop for i from 1 to 10
          do (sb-int:add-to-xset (code-char i) s1))
    (assert (listp (sb-kernel::xset-data s2)))
    (let ((union1 (sb-int:xset-union s1 s2)))
      (assert (= (sb-int:xset-count union1) (+ 26 10)))
      (let ((union2 (sb-int:xset-union s2 s1))) ; had better commute
        (assert (sb-int:xset= union1 union2))))))

(with-test (:name :xset-dedup-list-to-hash)
 (compile nil
  '(lambda (p1)
    (declare (type
            (or
             (or
              (and
               (or symbol t)
               (or
                (and
                 (or
                  (or symbol (member p standard-char-p f -))
                  (vector *))
                 (or
                  (and (or (member structure-class - #:g15490820) list)
                       (or list (member #:g15490821 - atan)))
                  (or
                   (member #:g15490817 #("a") char-name #:g15490818
                           #:g15490819)
                   (or array
                       (member interactive-stream-p #:g15490816
                               multiple-value-bind nil u)))))
                (and (or (cons (array * (9)) t) list)
                     (or
                      (eql
                       (53391669 -44590101
                        (#\b "081uw~" -52809.280758190354d0)))
                      (array * 1)))))
              (or (member d y :b byte-position i echo-stream-output-stream)
                  (member z w #:g15490815 nil g q t)))
             (or character (member #\1 -50073916617313)))
            p1))
    p1)))
