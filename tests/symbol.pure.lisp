;;;; miscellaneous tests of SYMBOL-related stuff

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

;;; Reported by Paul F. Dietz
(with-test (:name (:symbol :non-simple-string-name))
  (let ((sym (make-symbol (make-array '(1) :element-type 'character
                                      :adjustable t :initial-contents "X"))))
    (assert (simple-string-p (symbol-name sym)))
    (print sym (make-broadcast-stream))))

(with-test (:name (gentemp :pprinter))
  (let* ((*print-pprint-dispatch* (copy-pprint-dispatch)))
    (set-pprint-dispatch 'string
                         (lambda (stream obj)
                           (declare (ignore obj))
                           (write-string "BAR-" stream)))
    (assert (string= "FOO-" (gentemp "FOO-") :end2 4))))

(with-test (:name (gensym :fixnum-restriction))
  (gensym (1+ most-positive-fixnum)))

;; lp#1439921
;; CLHS states that SYMBOL-FUNCTION of a symbol naming a special operator
;; or macro must return something implementation-defined that might not
;; be a function. In this implementation it is a function, but it is illegal
;; to assign that function into another symbol via (SETF FDEFINITION).
(with-test (:name :setf-fdefinition-no-magic-functions)
  (assert-error (setf (fdefinition 'mysym) (fdefinition 'and)))
  (assert-error (setf (fdefinition 'mysym) (fdefinition 'if)))
  (assert-error (setf (symbol-function 'mysym) (symbol-function 'and)))
  (assert-error (setf (symbol-function 'mysym) (symbol-function 'if))))

(with-test (:name :macro-guard-function-name)
  (do-all-symbols (s)
    (when (macro-function s)
      (let* ((f (symbol-function s))
             (name (sb-kernel:%fun-name f)))
        (if (special-operator-p s)
            (assert (typep name '(cons (eql :special)
                                       (cons symbol null))))
            (assert (typep name '(cons (eql :macro)
                                       (cons symbol null)))))))))

(with-test (:name :fdefinition-no-consing
            :skipped-on :interpreter)
  (ctu:assert-no-consing (fdefinition 'list)))

(with-test (:name :progv-no-body)
  (checked-compile-and-assert
   ()
   '(lambda (vars vals)
     (progv vars vals))
   ((nil nil) nil)))

(defun summarize-colliding-hashes (print)
  ;; Collisions on SYMBOL-HASH aren't errors, merely unfortunate.
  ;; The printed output is nifty because it shows e.g.
  ;; -  of the 25 distinct symbols named "ARGS", only 2 collide on hash
  ;; -  20 symbols named "RESULT", 2 collide, etc.
  (let ((ht (make-hash-table :test 'equal))
        (result)
        (n-homograph-sets 0)
        (n-well-hashed-sets 0))
    ;; map string -> set of symbols whose print name is that string
    (do-all-symbols (s)
      (pushnew s (gethash (string s) ht)))
    (sb-int:dohash ((string symbols) ht)
      (when (cdr symbols)
        (incf n-homograph-sets)
        (let* ((hashes (mapcar #'sb-kernel:symbol-hash symbols))
               (dedup (remove-duplicates hashes))
               (alist))
          (when (< (length dedup) (length symbols))
            (push symbols result))
          (cond ((= (length dedup) (length symbols))
                 (incf n-well-hashed-sets))
                (print
                 ;; Start by binning symbols by their hash.
                 (dolist (s symbols)
                   (let* ((h (sb-kernel:symbol-hash s))
                          (cell (assoc h alist)))
                     (if cell
                         (push s (cdr cell))
                         (push (list h s) alist))))
                 (format t "Collisions on ~S (~D same-named symbols):~%"
                         string (length symbols))
                 ;; Only print bins having > 1 symbol
                 (dolist (cell alist)
                   (when (cddr cell)
                     (format t "  ~x ~s~%" (car cell) (cdr cell)))))))))
    (when print
      (format t "~D sets of symbols spelled the same, ~D well-hashed~%"
              n-homograph-sets n-well-hashed-sets))
    ;; The primary result is a "score", the nearer to 1 the better.
    ;; The secondary result is a list of all sets with collisions
    (values (/ n-well-hashed-sets n-homograph-sets)
            result)))

(with-test (:name :hashing-improvements :skipped-on (not :salted-symbol-hash))
  ;; Roughly: For each set of symbols colliding on SXHASH at all, what fraction
  ;; of those sets do NOT have any collisions on SYMBOL-HASH.
  (let ((expectation
         #+64-bit .95
         #-64-bit .70))
    (assert (> (summarize-colliding-hashes nil) expectation))))

(with-test (:name :fast-slot-name-mapper-small)
  ;; The XSET type has only2 slots, does not get a compiled function
  ;; as its slot mapper. Test that STRUCTURE-SLOT-VALUE is ok with that.
  (assert (vectorp (sb-kernel::layout-slot-mapper (sb-kernel:find-layout 'sb-int:xset))))
  (let ((x (sb-int:alloc-xset)))
    (sb-int:add-to-xset #\A x)
    (assert (equal (sb-pcl::structure-slot-value x 'sb-kernel::data) '(#\A)))))

;;; This test needs to be rewritten.
;;; It no longer works to have a slot-mapper that is dissociated from a LAYOUT.
#+nil
(with-test (:name :fast-slot-name-mapper-big)
  (let ((collision-sets
         #+salted-symbol-hash
          (nth-value 1 (summarize-colliding-hashes nil))
         ;; Require at least 4 different sets of colliding symbols,
         ;; Needless to say, this hashes horribly, with some sets
         ;; having 20 element per bucket.
         #-salted-symbol-hash (list (find-all-symbols "X")
                                    (find-all-symbols "Y")
                                    (find-all-symbols "RESULT") ; lots of these
                                    (find-all-symbols "ARGS")))
        (alist)
        (arb-value 0))
    (dolist (set collision-sets)
      (dolist (symbol set)
        (push (cons symbol (incf arb-value)) alist)))
    ;; the mapper shouldn't be a simple-vector
    (let ((function
           (the function (sb-kernel::make-hash-based-slot-mapper
                          alist "foo"))))
      ;; now try it
      (dolist (pair alist)
        (let* ((key (car pair))
               (computed (funcall function key)))
          (assert (eql computed (cdr pair))))))))
