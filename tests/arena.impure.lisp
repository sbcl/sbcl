#+(or (not system-tlabs) interpreter) (invoke-restart 'run-tests::skip-file)

(in-package sb-vm)

(defvar *many-arenas*
  (coerce (loop for i below 10 collect (new-arena 1048576)) 'vector))

;;; KEEP THIS AS THE FIRST TEST IN THE FILE. (Or make a new file of tests)
(test-util:with-test (:name :run-finder-with-no-arenas)
  (assert (null (c-find-heap->arena))))

(defvar *arena* (aref *many-arenas* 0))
;;; This REWIND is strictly unnecessary. It simply should not crash
(rewind-arena *arena*)

(defun f (x y z)
  (with-arena (*arena*) (list x y z)))

(test-util:with-test (:name :arena-huge-object)
  ;; This arena can grow to 10 MiB.
  (let ((a (new-arena 1048576 1048576 9)))
    ;; 4 arrays of about 2MiB each should fit in the allowed space
    (dotimes (i 4)
      (test-util:opaque-identity
       (with-arena (a) (make-array 2097152 :element-type '(unsigned-byte 8)))))
    (destroy-arena a)))

(test-util:with-test (:name :disassembler)
  (let ((a (new-arena 1048576)))
    (with-arena (a) (sb-disassem:get-inst-space))
    (assert (null (c-find-heap->arena)))
    (destroy-arena a)))

(test-util:with-test (:name :no-arena-symbol-name)
  (let* ((a (new-arena 1048576))
         (symbol
          (sb-vm:with-arena (a)
            (let ((string (format nil "test~D" (random 10))))
              (make-symbol string)))))
    (assert (heap-allocated-p symbol))
    (assert (heap-allocated-p (symbol-name symbol)))
    (destroy-arena a)))

(test-util:with-test (:name :no-arena-symbol-property)
  (let* ((a (new-arena 1048576))
         (copy-of-foo
          (with-arena (a)
            (setf (get 'testsym 'fooprop) 9)
            (copy-symbol 'testsym t))))
    (test-util:opaque-identity copy-of-foo)
    (assert (not (c-find-heap->arena)))
    (destroy-arena a)))

(test-util:with-test (:name :interrupt-thread-on-arena)
  (let* ((a (new-arena 1048576))
         (sem (sb-thread:make-semaphore))
         (junk))
    (sb-vm:with-arena (a)
      (sb-thread:interrupt-thread
       sb-thread:*current-thread*
       (lambda ()
         (setf junk (cons 'foo 'bar))
         (sb-thread:signal-semaphore sem))))
    (sb-thread:wait-on-semaphore sem)
    (sb-vm:destroy-arena a)
    (assert (heap-allocated-p junk))))

(defun find-some-pkg () (find-package "NOSUCHPKG"))

(test-util:with-test (:name :find-package-1-element-cache)
  (let* ((cache (let ((code (fun-code-header #'find-some-pkg)))
                  (loop for i from code-constants-offset
                        below (code-header-words code)
                        when (and (typep (code-header-ref code i) '(cons string))
                                  (string= (car (code-header-ref code i))
                                           "NOSUCHPKG"))
                        return (code-header-ref code i))))
         (elements (cdr cache)))
    (assert (or (equalp (cdr cache) #(NIL NIL NIL))
                (search "#<weak array [3]" (write-to-string (cdr cache)))))
    (let* ((a (new-arena 1048576))
           (pkg (with-arena (a) (find-some-pkg))))
      (assert (not pkg)) ; no package was found of course
      (assert (neq (cdr cache) elements)) ; the cache got affected
      (assert (not (c-find-heap->arena)))
      (destroy-arena a))))

#+nil
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

(test-util:with-test (:name :copy-numbers-to-heap)
  (let (list1 list2)
    (with-arena (*arena*)
      (setq list1 (let ((r (ash #xf00 (+ 60 (random 10)))))
                    (list r
                          (coerce r 'double-float)
                          (coerce r '(complex single-float))
                          (coerce r '(complex double-float))
                          (complex 1 (1+ (random 40)))
                          (/ 1 r)))
            ;; still inside the WITH-ARENA or else the test is not useful!
            list2 (mapcar 'copy-number-to-heap list1)))
    (assert (not (heap-allocated-p list1)))
    (assert (notany #'heap-allocated-p list1))
    (assert (every #'heap-allocated-p list2))))

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

(defvar *foo-storage*)
(test-util:with-test (:name :ctype-cache-force-to-heap)
  (drop-all-hash-caches)
  (test-util:opaque-identity
   (with-arena (*arena*)
     ;; finder-result will assert that this goes to the heap
     (setq *foo-storage* (sb-impl::allocate-hashset-storage 128 t))
     ;; for each test, the type specifier itself can not be cached because
     ;; it is a list in the arena. And the internal representation has to
     ;; take care to copy arena-allocated numbers back to dynamic space.
     (list (let* ((n (- (test-util:opaque-identity 0d0)))
                  (spec `(member ,n)))
             (assert (not (heap-allocated-p (second spec)))) ; -0d0 is off-heap
             (typep (random 2) spec))
           (let* ((n (+ 5.0d0 (random 10)))
                  (bound (list n))
                  (spec `(or (double-float ,bound) (integer ,(random 4)))))
             ;; should not cache the type specifier
             (typep 'foo spec)))))
  (dolist (symbol sb-impl::*cache-vector-symbols*)
    (let ((cache (symbol-value symbol)))
      ; (format t "~&Checking cache ~S~%" symbol)
      (when cache
        (assert (heap-allocated-p cache))
        (dovector (line cache)
          (unless (eql line 0)
            (unless (and (heap-allocated-p line) (not (points-to-arena line)))
              (hexdump line 2 nil)
              (error "~S has ~S" symbol line)))))))
  (let ((finder-result (c-find-heap->arena)))
    (assert (null finder-result))))

(defun decode-all-debug-data ()
  (dolist (code (sb-vm:list-allocated-objects :all :type sb-vm:code-header-widetag))
    (let ((info (sb-kernel:%code-debug-info code)))
      (when (typep info 'sb-c::compiled-debug-info)
        (let ((fun-map (sb-di::get-debug-info-fun-map
                        (sb-kernel:%code-debug-info code))))
          (loop for i from 0 below (length fun-map) by 2 do
            (let ((cdf (aref fun-map i)))
              (test-util:opaque-identity
               (sb-di::debug-fun-lambda-list
                (sb-di::make-compiled-debug-fun cdf code))))))))))

(test-util:with-test (:name :debug-data-force-to-heap)
  (let ((a (sb-vm:new-arena (* 1024 1024 1024))))
    (sb-vm:with-arena (a)
      (decode-all-debug-data))
    (assert (zerop (length (sb-vm:c-find-heap->arena a))))
    (sb-vm:destroy-arena a)))

(defun test-with-open-file ()
  ;; Force allocation of a new BUFFER containing a new SAP,
  ;; and thereby a new finalizer (a closure) so that the test can
  ;; ascertain that none of those went to the arena.
  (setq sb-impl::*available-buffers* nil)
  (with-open-file (stream *load-pathname*)
    (if stream
        (let ((pn (pathname stream)))
          (values pn (namestring pn) (read-line stream nil)))
        (values nil nil nil))))

(defvar *answerstring*)
(test-util:with-test (:name :with-open-stream)
  (multiple-value-bind (pathname namestring answer)
      (with-arena (*arena*) (test-with-open-file))
    (when pathname
      (assert (heap-allocated-p pathname))
      (assert (heap-allocated-p namestring))
      (assert (not (points-to-arena pathname)))
      (assert (not (heap-allocated-p answer)))
      ;; 1. check that a global symbol value can be found
      (unwind-protect
           (progn
             (setq *answerstring* answer)
             ;; user's string went to the arena, and detector finds the source object
             (let ((finder-result (c-find-heap->arena)))
               (assert (equal finder-result '(*answerstring*)))))
        (makunbound '*answerstring*))
      ;; 2. check that a thread-local binding can be found
      (let ((*answerstring* answer))
        (let ((finder-result (c-find-heap->arena)))
          (assert (equal (first finder-result)
                         `(,sb-thread:*current-thread* :tls *answerstring*))))
        ;; 3. check that a shadowed binding can be found
        (let ((*answerstring* "hi"))
          (let ((finder-result (c-find-heap->arena)))
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

(test-util:with-test (:name :find-ptrs-all-arenas)
  (let ((result (c-find-heap->arena)))
    ;; There should be a cons pointing to ARENA1,
    ;; the cons which happens to be in PTR1
    (assert (member ptr1 result))
    ;; The symbol PTR2 points directly to ARENA2.
    (assert (member 'ptr2 result))
    ;; There should not be anything else
    (assert (= (length result) 2))))

(test-util:with-test (:name :find-ptrs-specific-arena)
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
      (with-arena (arena)
        (let ((object (make-array (+ 100 (random 100)))))
          (incf (aref bytes-used i) (primitive-object-size object)))))
    #+nil
    (when (zerop (random 1000))
      (let ((i (random (length arenas))))
        (let ((arena (aref arenas i)))
          (format t "~&REWINDING ~D~%" (arena-index arena))
          (rewind-arena arena)
          (with-arena (arena)
            (test-util:opaque-identity (make-array 5)))))))
  bytes-used)
#+nil
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
  (with-arena (*arena*)
    (let ((thread
           (sb-thread:make-thread
            (lambda ()
              (assert (arena-p (thread-current-arena)))
              ;; Starting a new thread doesn't ensure that the arena savearea
              ;; has enough room to save the state, so we now ensure space in the
              ;; savearea only when switching away from the arena.
              (unuse-arena)))))
      (sb-thread:join-thread thread))))

;;;; Type specifier parsing and operations

(defparameter *bunch-of-objects*
  `((foo)
    "astring"
    #*1010
    ,(find-package "CL")
    ,(pathname "/tmp/blub")
    ,#'open
    #2a((1 2) (3 4))
    ,(ash 1 64)
    ))

;; These type-specs are themselves consed so that we can
;; ascertain whether there are arena pointers in internalized types.
(defun get-bunch-of-type-specs ()
  `((integer ,(random 47) *)
    (and bignum (not (eql ,(random 1000))))
    (and bignum (not (eql ,(logior #x8000000000000001
                                   (ash (1+ (random #xF00)) 10)))))
    (member ,(complex (coerce (random 10) 'single-float)
                      (coerce (- (random 10)) 'single-float))
            (goo)
            #+sb-unicode
            #\thumbs_up_sign
            #-sb-unicode
            #\a)
    (or stream (member :hello
                       #+sb-unicode #\thumbs_down_sign
                       #-sb-unicode #\B))
    (array t (,(+ 10 (random 10))))))

(defun show-cache-counts ()
  (dolist (s sb-impl::*cache-vector-symbols*)
    (let ((v (symbol-value s)))
      (when (vectorp v)
        (format t "~5d  ~a~%"
                (count-if (lambda (x) (not (eql x 0))) v)
                s)))))

(defun ctype-operator-tests (arena &aux (result 0))
  (sb-int:drop-all-hash-caches)
  (flet ((try (spec)
           (dolist (x *bunch-of-objects*)
             (when (typep x spec)
               (incf result)))))
    (with-arena (arena)
      (let ((specs (get-bunch-of-type-specs)))
        (dolist (spec1 specs)
          (dolist (spec2 specs)
            (try `(and ,spec1 ,spec2))
            (try `(or ,spec1 ,spec2))
            (try `(and ,spec1 (not ,spec2)))
            (try `(or ,spec1 (not ,spec2))))))))
  (assert (not (c-find-heap->arena arena)))
  result)
(test-util:with-test (:name :ctype-cache)
  (let ((arena (new-arena 1048576)))
    (ctype-operator-tests arena)))

;;;;

(defvar *newpkg* (make-package "PACKAGE-GROWTH-TEST"))
(defun addalottasymbols ()
  (with-arena (*arena*)
    (dotimes (i 200)
      (let ((str (concatenate 'string "S" (write-to-string i))))
        (assert (not (heap-allocated-p str)))
        (let ((sym (intern str *newpkg*)))
          (assert (heap-allocated-p sym))
          (assert (heap-allocated-p (symbol-name sym)))))))
  (assert (not (c-find-heap->arena *arena*))))

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

(define-condition foo (simple-warning)
  ((a :initarg :a)
   (b :initarg :b)))
(defvar *condition* (make-condition 'foo
                                    :format-control "hi there"
                                    :a '(x y) :b #P"foofile"
                                    :format-arguments '("Yes" "no")))
(test-util:with-test (:name :arena-condition-slot-access)
  (assert (null (sb-kernel::condition-assigned-slots *condition*)))
  (let ((val (with-arena (*arena*)
               (slot-value *condition* 'b))))
    (assert (pathnamep val))
    (assert (not (points-to-arena *condition*)))))

(test-util:with-test (:name :gc-epoch-not-in-arena)
  (with-arena (*arena*) (gc))
  (assert (heap-allocated-p sb-kernel::*gc-epoch*)))

(defvar *sem* (sb-thread:make-semaphore))
(defvar *thing-created-by-hook* nil)
(push (lambda ()
        (push (cons 1 2) *thing-created-by-hook*)
        (sb-thread:signal-semaphore *sem*))
      *after-gc-hooks*)
(test-util:with-test (:name :post-gc-hooks-unuse-arena)
  (with-arena (*arena*) (gc))
  (sb-thread:wait-on-semaphore *sem*)
  (setq *after-gc-hooks* nil)
  (assert *thing-created-by-hook*)
  (assert (heap-allocated-p *thing-created-by-hook*))
  (assert (heap-allocated-p (car *thing-created-by-hook*))))

;;; CAUTION: tests of C-FIND-HEAP->ARENA that execute after destroy-arena and a following
;;; NEW-ARENA might spuriously fail depending on how eagerly malloc() reuses addresses.
;;; The failure goes something like this:
;;;
;;; stack -> some-cons C1 ; conservative reference
;;; heap: C1 = (#<instance-in-arena> . mumble)
;;;
;;; now rewind the arena. "instance-in-arena" is not a valid object.
;;; But: allocate more stuff in the arena, and suppose the address where instance-in-arena
;;; formerly was now holds a different primitive object, like a cons cell.
;;; The C-FIND-HEAP->ARENA function won't die, but you *will* die when trying to examine
;;; what it found.  The heap cons is conservatively live, its contents are assumed good,
;;; yet its CAR has instance-pointer-lowtag pointing to something that does not have
;;; INSTANCE-WIDETAG. The Lisp printer suffers a horrible fate and causes recursive errors.
;;; Had malloc() not reused an address, this would not happen, because the destroyed arena
;;; can not be seen, and the cons pointing to nothing will not be returned by the finder.
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

(defvar *another-arena* (new-arena 131072))
(defun g (n) (make-array (the integer n) :initial-element #\z))
(defun f (a n) (with-arena (a) (g n)))

(defvar *vect* (f *another-arena* 10))
(setf (aref *vect* 3) "foo")

;;; "Hiding" an arena asserts that no references will be made to it until
;;; unhidden and potentially rewound. So any use of it is like a use-after-free bug,
;;; except that the memory is still there so we can figure out what went wrong
;;; with user code. This might pass on #+-linux but has not been tested.
(setf (extern-alien "lose_on_corruption_p" int) 0)
(test-util:with-test (:name :arena-use-after-free :skipped-on (:not :linux))
  ;; scary messages scare me
  (format t "::: NOTE: Expect a \"CORRUPTION WARNING\" from this test~%")
  (hide-arena *another-arena*)
  (let (caught)
    (block foo
      (handler-bind
          ((sb-sys:memory-fault-error
             (lambda (c)
               (format t "~&Uh oh spaghetti-o: tried to read @ ~x~%"
                       (sb-sys:system-condition-address c))
               (setq caught t)
               (return-from foo))))
        (aref *vect* 3)))
    (assert caught))
  ;; Assert that it becomes usable again
  (unhide-arena *another-arena*)
  (rewind-arena *another-arena*)
  (dotimes (i 10) (f *another-arena* 1000)))
(setf (extern-alien "lose_on_corruption_p" int) 1)

;; #+sb-devel preserves some symbols that the test doesn't care about
;; as the associated function will never be called.
(defvar *ignore* '("!EARLY-LOAD-METHOD"))

(test-util:with-test (:name :disassemble-pcl-stuff)
  (let ((stream (make-string-output-stream)))
    (with-package-iterator (iter "SB-PCL" :internal :external)
      (loop
        (multiple-value-bind (got symbol) (iter)
          (unless got (return))
          (when (and (fboundp symbol)
                     (not (member (string symbol) *ignore* :test 'string=))
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
              ;; Each alloc-tramp call should be the SYS- variant
              (flet ((line-ok (line)
                       (cond ((search "LIST-ALLOC-TRAMP" line)
                              (search "SYS-LIST-ALLOC-TRAMP" line))
                             ((search "ALLOC-TRAMP" line)
                              (search "SYS-ALLOC-TRAMP" line))
                             ((search "LISTIFY-&REST" line)
                              (search "SYS-LISTIFY-&REST" line))
                             (t t))))
                (unless (every #'line-ok lines)
                  (format *error-output* "Failure:~{~%~A~}~%" lines)
                  (error  "Bad result for ~S" symbol))))))))))

(defun collect-objects-pointing-off-heap ()
  (let (list)
    (flet ((add-to-result (obj referent)
             ;; If this is a code component and it points to fixups
             ;; which are a bignum in a random place, assume that it's an ELF core
             ;; and that the packed fixup locs are in a ".rodata" section
             (cond ((and (typep obj 'sb-kernel:code-component)
                         (typep referent 'bignum)
                         (eq referent (%code-fixups obj)))
                    nil)
                   (t
                    (push (cons (sb-ext:make-weak-pointer obj)
                                (if (sb-ext:stack-allocated-p referent t)
                                    :stack
                                    (sb-sys:int-sap (get-lisp-obj-address referent))))
                          list)))))
      (macrolet ((visit (referent)
                   `(let ((r ,referent))
                      (when (and (is-lisp-pointer (get-lisp-obj-address r))
                                 (not (heap-allocated-p r))
                                 (add-to-result obj r))
                        (return-from done)))))
        (map-allocated-objects
         (lambda (obj type size)
           (declare (ignore type size))
           (block done
             (do-referenced-object (obj visit)
               (t
                :extend
                (case (widetag-of obj)
                  (#.value-cell-widetag
                   (visit (value-cell-ref obj)))
                  (t
                   (warn "Unknown widetag ~x" (widetag-of obj))))))))
         :all)))
    list))

(defun show-objects-pointing-off-heap (list)
  (dolist (x list)
    (let ((obj (weak-pointer-value (car x))))
      (if (typep obj '(or sb-kernel:code-component
                       symbol))
          (format t "~s -> ~s~%" obj (cdr x))
          (format t "~s -> ~s~%" (type-of obj) (cdr x))))))
