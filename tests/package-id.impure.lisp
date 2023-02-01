(import 'sb-impl::*all-packages*)

;;; Assert that a global name of a package that hashes to the same bucket as a different
;;; global name of the package does not cause disappearance of the package from
;;; *ALL-PACKAGES* when only one name is removed. i.e. don't naively blast the item
;;; out of the bucket corresponding to the hash of the removed name.
;;; Start by making 4 packages and deleting them so that the table won't resize during
;;; the addition of nicknames. (It assumes that every name increases the table load)
(mapc 'delete-package (mapcar 'make-package '("AA" "BB" "CC" "DD")))
(defvar *tp* (make-package "SOMETESTPACKAGE"))
(defun compute-name-bucket (str)
  (mod (sxhash str) (1- (length *all-packages*))))
(defvar *tp-bucket* (compute-name-bucket "SOMETESTPACKAGE"))

;;; Generate a random nicknames for SOMETESTPACKAGE that hashes to
;;; the same bucket.
(defvar *random-names* nil)
(loop for i from 1
      do (let ((name (format nil "PKGNAME~36R" i)))
           (when (eql (compute-name-bucket name) *tp-bucket*)
             (push name *random-names*)
             (when (eql (length *random-names*) 3) (loop-finish)))))
(rename-package *tp* (first *random-names*)
                (cons "SOMETESTPACKAGE" (rest *random-names*)))
(defun get-bucket (i) (sb-int:ensure-list (aref *all-packages* i)))
(with-test (:name :package-name-hash-collision)
  ;; SOMETESTPACKGE should be in its bucket exactly once for all 4 names
  (assert (= (count *tp* (get-bucket *tp-bucket*)) 1))
  ;; Remove 2 names, it should still be there exactly once
  (rename-package *tp* "SOMETESTPACKAGE" (last *random-names*))
  (assert (= (count *tp* (get-bucket *tp-bucket*)) 1)))

;;; this test is slow

;;; It's extremely unlikely that a user would make >2^16 packages, but test that it works.
(defun grow-id->package-vector ()
  (let ((table (make-array 65535 :initial-element nil))) ; grow once only. Sorry for cheating
    (replace table sb-impl:*id->package*)
    (setf sb-impl:*id->package* table)))
(compile 'grow-id->package-vector)

(with-test (:name :ridiculous-amount-of-packages)
  (make-package "WATPACKAGE")
  (grow-id->package-vector) ; grow once only. Sorry for cheating
  (loop
    ;; This loop unfortunately takes 2 seconds, which kind of speaks to
    ;; the slowness of package creation. I don't think we need to improve that,
    ;; but we _do_ need to test this, so ... it's a minor point of pain.
    ;; Unsurprisingly, 50% of the time is spent in PACKAGE-REGISTRY-UPDATE
    (let* ((package (make-package "STRANGE"))
           (id (sb-impl::package-id package))
           (new-name (format nil "TEST-PKGID-~D" id)))
      (unless id (return))
      (rename-package package new-name)))
  (let ((p (find-package "STRANGE")))
    (assert (not (sb-impl::package-id p)))
    (let ((symbol (intern "WAT123" p)))
      (assert (eq (symbol-package symbol) p))
      (delete-package p)
      (assert (not (symbol-package symbol)))
      (import symbol "WATPACKAGE")
      (assert (eq (symbol-package symbol) (find-package "WATPACKAGE")))
      ;; assert that the symbol got a small ID
      (assert (not (sb-int:info :symbol :package symbol)))))
  (delete-package "WATPACKAGE")
  (let ((p (make-package "ANOTHERPACKAGE")))
    (assert (sb-impl::package-id p)))
  (let ((p (make-package "YETANOTHERPACKAGE")))
    (assert (not (sb-impl::package-id p))))
  ;; Now for every package named TEST-PKGIDnm, check that a symbol interned
  ;; in that package can read the bits back correctly (because vops are confusing)
  (let ((n 0))
    (dolist (package (list-all-packages))
      (when (search "TEST-PKGID-" (package-name package))
        (incf n)
        (let ((the-symbol (intern "FROBOLA" package)))
          (assert (eq (symbol-package the-symbol) package)))))
    (assert (> n 65450)))) ; assert that we exercised lots of bit patterns
