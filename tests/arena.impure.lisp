#-system-tlabs (invoke-restart 'run-tests::skip-file)
#+parallel-test-runner (invoke-restart 'run-tests::skip-file) ; crashes and hangs
#+interpreter (invoke-restart 'run-tests::skip-file)
(in-package sb-vm)

(defvar *many-arenas*
  (coerce (loop for i below 10 collect (new-arena 1048576)) 'vector))

(defvar *arena* (aref *many-arenas* 0))
;;; This REWIND is strictly unnecessary. It simply should not crash
(rewind-arena *arena*)

(defun f (x y z)
  (with-arena (*arena*) (list x y z)))

(test-util:with-test (:name :arena-alloc-waste-reduction)
  (let* ((list1 (f 'foo 'bar'baz))
         (list1-addr (get-lisp-obj-address list1))
         (prev list1-addr))
    (dotimes (i 40)
      (let* ((list2 (f 'baz 'quux 'glerp))
             (this (get-lisp-obj-address list2)))
        ;; Thread should have picked up where it  left off in the arena
        ;; on the previous allocation.
        ;; The list is 3 conses.
        (assert (= (- this prev) (* 3 cons-size n-word-bytes)))
        (setq prev this)))
    (rewind-arena *arena*)
    (let* ((list3 (f 'zot nil 'bork))
           (this (get-lisp-obj-address list3)))
      (assert (= this list1-addr)))))

;;;

(test-util:with-test (:name :points-to-arena)
  (let (tests)
    (dotimes (i 20)
      (let ((randomly-arena-thing (if (evenp i)
                                      (with-arena (*arena*) (cons 1 2))
                                      (cons 3 4))))
        (push (make-array 1 :initial-element randomly-arena-thing) tests)))
    (setq tests (nreverse tests))
    (dolist (x tests)
      (let* ((arena-ref-p (points-to-arena x))
             (item (aref x 0)))
        (if (find-containing-arena (get-lisp-obj-address item))
            (assert arena-ref-p)
            (assert (not arena-ref-p)))))))

(defun test-with-open-file ()
  (with-open-file (stream (format nil "/proc/~A/stat" (sb-unix:unix-getpid))
                          :if-does-not-exist nil)
    (if stream
        (let ((pn (pathname stream)))
          (values pn (namestring pn) (read-line stream nil)))
        (values nil nil nil))))

(defvar *answerstring*)
(test-util:with-test (:name :with-open-stream :skipped-on (:not :linux))
  (multiple-value-bind (pathname namestring answer)
      (sb-vm:with-arena (*arena*) (test-with-open-file))
    (when pathname
      (assert (heap-allocated-p pathname))
      (assert (heap-allocated-p namestring))
      (assert (not (sb-vm:points-to-arena pathname)))
      (assert (not (heap-allocated-p answer)))
      ;; 1. check that a global symbol value can be found
      (unwind-protect
           (progn
             (setq *answerstring* answer)
             ;; user's string went to the arena, and detector finds the source object
             (let ((finder-result (sb-vm:c-find-heap->arena)))
               (assert (equal finder-result '(*answerstring*)))))
        (makunbound '*answerstring*))
      ;; 2. check that a thread-local binding can be found
      (let ((*answerstring* answer))
        (let ((finder-result (sb-vm:c-find-heap->arena)))
          (assert (equal (first finder-result)
                         `(,sb-thread:*current-thread* :tls *answerstring*))))
        ;; 3. check that a shadowed binding can be found
        (let ((*answerstring* "hi"))
          (let ((finder-result (sb-vm:c-find-heap->arena)))
            (assert (equal (first finder-result)
                           `(,sb-thread:*current-thread* :binding *answerstring*)))))))))

;;;

(defun test-vpe-heap-vector (vector count &aux grown)
  (with-arena (*arena*)
    (assert (not (heap-allocated-p (cons 1 2)))) ; assert arena is in use
    (dotimes (i count)
      (let ((old-data (%array-data vector)))
        (vector-push-extend i vector)
        (let ((new-data (%array-data vector)))
          (unless (eq new-data old-data)
            (assert (heap-allocated-p new-data))
            (setq grown t))))))
  (assert grown)) ; make sure the test proved something

(defun test-vpe-arena-vector (count &aux grown)
  (with-arena (*arena*)
    (let ((v (make-array 4 :fill-pointer 0 :adjustable t)))
      (assert (not (heap-allocated-p v)))
      (assert (not (heap-allocated-p (%array-data v))))
      (dotimes (i count)
        (let ((old-data (%array-data v)))
          (vector-push-extend i v)
          (let ((new-data (%array-data v)))
            (unless (eq new-data old-data)
              (assert (not (heap-allocated-p new-data)))
              (setq grown t)))))))
  (assert grown))

(defun test-puthash-heap-table (table count &aux grown)
  (assert (sb-impl::hash-table-hash-vector table)) ; require a hash vector
  (with-arena (*arena*)
    (assert (not (heap-allocated-p (cons 1 2))))
    (dotimes (i count)
      (let ((old-data (sb-impl::hash-table-pairs table)))
        (setf (gethash i table) i)
        (let ((new-data (sb-impl::hash-table-pairs table)))
          (unless (eq new-data old-data)
            (assert (heap-allocated-p new-data))
            (assert (heap-allocated-p (sb-impl::hash-table-hash-vector table)))
            (assert (heap-allocated-p (sb-impl::hash-table-index-vector table)))
            (assert (heap-allocated-p (sb-impl::hash-table-next-vector table)))
            (setq grown t))))))
  (assert grown))

(defun test-puthash-arena-table (count &aux grown)
  (with-arena (*arena*)
    (let ((table (make-hash-table :test 'equal)))
      (assert (sb-impl::hash-table-hash-vector table)) ; require a hash vector
      (assert (not (heap-allocated-p table)))
      (dotimes (i count)
        (let ((old-data (sb-impl::hash-table-pairs table)))
          (setf (gethash i table) i)
          (let ((new-data (sb-impl::hash-table-pairs table)))
            (unless (eq new-data old-data)
              (assert (not (heap-allocated-p new-data)))
              (assert (not (heap-allocated-p (sb-impl::hash-table-hash-vector table))))
              (assert (not (heap-allocated-p (sb-impl::hash-table-index-vector table))))
              (assert (not (heap-allocated-p (sb-impl::hash-table-next-vector table))))
              (setq grown t)))))))
  (assert grown))

;;; There is a case that this doesn't assert anything about, which is that
;;; an arena-allocated table or vector which grows while *not* in the scope of
;;; a WITH-ARENA (or inside a nested WITHOUT-ARENA) will go to the dynamic space.
;;; I think that is the right behavior: you can't force an object to be arena-allocated
;;; within a dynamic controls that asks for no arena allocation.
;;; I can't see how such a situation would legitimately arise,
;;; and it's probably only through application programmer error.

(test-util:with-test (:name :vector-push-extend-heap-vector)
  (test-vpe-heap-vector (make-array 4 :fill-pointer 0 :adjustable t) 100))

(test-util:with-test (:name :vector-push-extend-arena-vector)
  (test-vpe-arena-vector 100))

(test-util:with-test (:name :puthash-heap-table)
  (test-puthash-heap-table (make-hash-table :test 'equal) 100))

(test-util:with-test (:name :puthash-arena-table)
  (test-puthash-arena-table 100))

(defvar arena1 (new-arena 65536))
(defvar arena2 (new-arena 65536))

(defun f (a) (with-arena (a) (make-array 1000)))
(defun g (a) (with-arena (a) (list 'x 'y 'z)))

(defvar ptr1 (cons (f arena1) 'foo))
(defvar ptr2 (g arena2))

(test-util:with-test (:name :find-ptrs-all-arenas :skipped-on :win32)
  (let ((result (c-find-heap->arena)))
    ;; There should be a cons pointing to ARENA1,
    ;; the cons which happens to be in PTR1
    (assert (member ptr1 result))
    ;; The symbol PTR2 points directly to ARENA2.
    (assert (member 'ptr2 result))
    ;; There should not be anything else
    (assert (= (length result) 2))))

(test-util:with-test (:name :find-ptrs-specific-arena :skipped-on :win32)
  (let ((result (c-find-heap->arena arena1)))
    (assert (equal result (list ptr1))))
  (let ((result (c-find-heap->arena arena2)))
    (assert (equal result '(ptr2)))))

(defun use-up-some-space (n &aux (arenas *many-arenas*)
                                 (bytes-used (make-array (length arenas)
                                                         :initial-element 0)))
  (dotimes (k n)
    (let* ((i (mod k (length arenas)))
           (arena (aref arenas i)))
      (sb-vm:with-arena (arena)
        (let ((object (make-array (+ 100 (random 100)))))
          (incf (aref bytes-used i) (primitive-object-size object)))))
    #+nil
    (when (zerop (random 1000))
      (let ((i (random (length arenas))))
        (let ((arena (aref arenas i)))
          (format t "~&REWINDING ~D~%" (arena-index arena))
          (sb-vm:rewind-arena arena)
          (sb-vm:with-arena (arena)
            (test-util:opaque-identity (make-array 5)))))))
  bytes-used)
(test-util:with-test (:name :allocator-resumption)
  (map nil 'rewind-arena *many-arenas*)
  (let ((bytes-used-per-arena (use-up-some-space 10000)))
    (dotimes (i (length *many-arenas*))
      (let* ((est (aref bytes-used-per-arena i))
             (act (arena-bytes-used (aref *many-arenas* i)))
             (delta (- act est))
             (frac (* 100 (/ delta act))))
      (format t "Used: estimate=~D actual=~D diff=~,2f%~%"
              est act frac)
      (assert (< frac 1))))))

(test-util:with-test (:name :thread-arena-inheritance)
  (sb-vm:with-arena (*arena*)
    (let ((thread
           (sb-thread:make-thread
            (lambda ()
              (assert (arena-p (thread-current-arena)))
              ;; Starting a new thread doesn't ensure that the arena savearea
              ;; has enough room to save the state, so we now ensure space in the
              ;; savearea only when switching away from the arena.
              (unuse-arena)))))
      (sb-thread:join-thread thread))))

(defvar *newpkg* (make-package "PACKAGE-GROWTH-TEST"))
(defun addalottasymbols ()
  (sb-vm:with-arena (*arena*)
    (dotimes (i 200)
      (let ((str (concatenate 'string "S" (write-to-string i))))
        (assert (not (heap-allocated-p str)))
        (let ((sym (intern str *newpkg*)))
          (assert (heap-allocated-p sym))
          (assert (heap-allocated-p (symbol-name sym))))))))
(test-util:with-test (:name :intern-a-bunch)
  (let ((old-n-cells
         (length (sb-impl::symtbl-cells
                  (sb-impl::package-internal-symbols *newpkg*)))))
    (addalottasymbols)
    (let* ((cells (sb-impl::symtbl-cells
                   (sb-impl::package-internal-symbols *newpkg*))))
      (assert (> (length cells) old-n-cells)))))

(defun all-arenas ()
  (let ((head (sb-kernel:%make-lisp-obj (extern-alien "arena_chain" unsigned))))
    (cond ((eql head 0) nil)
          (t
           (assert (typep (arena-link head) '(or null arena)))
           (collect ((output))
             (do ((a head (arena-link a)))
                 ((null a)
                  ;; (format t "CHAIN: ~X~%" (output))
                  (output))
               (output (get-lisp-obj-address a))))))))

(test-util:with-test (:name destroy-arena)
  (macrolet ((exit-if-no-arenas ()
               '(progn (incf n-deleted)
                       (when (zerop (extern-alien "arena_chain" unsigned)) (return)))))
    (let ((n-arenas (length (all-arenas)))
          (n-deleted 0))
      (loop ; until all deleted
        ;; 1.delete the first item
        (let* ((chain (all-arenas))
               (item (car chain))
               (arena (%make-lisp-obj item)))
          (assert (typep arena 'arena))
          (destroy-arena arena)
          (assert (equal (all-arenas) (cdr chain))))
        (exit-if-no-arenas)
        ;; 2. delete something from the middle
        (let* ((chain (all-arenas))
               (item (nth (floor (length chain) 2) chain))
               (arena (%make-lisp-obj item)))
          (assert (typep arena 'arena))
          (destroy-arena arena)
          (assert (equal (all-arenas) (delete item chain))))
        (exit-if-no-arenas)
        ;; 3. delete the last item
        (let* ((chain (all-arenas))
               (item (car (last chain)))
               (arena (%make-lisp-obj item)))
          (assert (typep arena 'arena))
          (destroy-arena arena)
          (assert (equal (all-arenas) (butlast chain))))
        (exit-if-no-arenas))
      (assert (= n-deleted n-arenas)))))

(test-util:with-test (:name :disassemble-pcl-stuff)
  (let ((stream (make-string-output-stream)))
    (with-package-iterator (iter "SB-PCL" :internal :external)
      (loop
        (multiple-value-bind (got symbol) (iter)
          (unless got (return))
          (when (and (fboundp symbol)
                     (not (closurep (symbol-function symbol)))
                     (not (sb-pcl::generic-function-p
                           (symbol-function symbol))))
            (disassemble (sb-kernel:fun-code-header
                          (or (macro-function symbol)
                              (symbol-function symbol)))
                         :stream stream)
            (let ((lines (test-util:split-string
                          (get-output-stream-string stream)
                          #\newline)))
              (dolist (line lines)
                (cond ((search "LIST-ALLOC-TRAMP" line)
                       (assert (search "SYS-LIST-ALLOC-TRAMP" line)))
                      ((search "ALLOC-TRAMP" line)
                       (assert (search "SYS-ALLOC-TRAMP" line)))
                      ((search "LISTIFY-&REST" line)
                       (assert (search "SYS-LISTIFY-&REST" line))))))))))))
