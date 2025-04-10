(shadowing-import 'sb-lockless::endp)
(import 'sb-int::(aver fixnump unbound-marker-p))
(import 'sb-sys:with-pinned-objects)
(import 'sb-kernel::(generation-of get-lisp-obj-address))
(import 'sb-lockless::(+hash-nbits+ %node-next
                       get-next node-hash
                       so-head so-bins so-key so-data so-count
                       so-key-node-p
                       so-insert so-delete so-find so-find/string so-maplist
                       make-so-map/string make-so-set/string make-so-map/addr
                       make-marked-ref))

;;; Make sure no promotions occur so that objects will be movable
;;; throughout these tests.
(setf (generation-number-of-gcs-before-promotion 0) 1000000)

(defun dummy-node-p (node) (evenp (node-hash node)))

;;; Show all nodes including dummies.
(defun show-list (solist)
  (let ((node (so-head solist)))
    (loop (format t "~s~%" node)
          (when (endp node) (return))
          (setq node (%node-next node)))))

(defun show-bin (solist i)
  (let ((node (aref (car (so-bins solist)) i))
        (bin-nbits (- +hash-nbits+ (cdr (so-bins solist))))
        (count 0))
    (flet ((bit-string (hash)
             (let ((s (format nil " ~v,'0b" +hash-nbits+ hash)))
               (replace s s :end1 bin-nbits :start2 1)
               (setf (char s bin-nbits) #\.)
               s)))
      (cond
        ((unbound-marker-p node)
         (values 0 0))
        (t
         (let ((node node))
           (loop (let ((next (get-next node)))
                   (when (or (endp next) (evenp (node-hash next)))
                     (return))
                   (incf count)
                   (setq node next))))
         (format t " ~5d [~2d] = ~a" i count (bit-string (node-hash node)))
         (loop (let ((next (get-next node)))
                 (when (or (endp next) (evenp (node-hash next)))
                   (return))
                 (setq node next)
                 (if (= count 1)
                     (format t " ~a=~s"
                             (bit-string (node-hash node)) (so-key node))
                     (format t "~%              ~a=~s"
                             (bit-string (node-hash node)) (so-key node)))))
         (terpri)
         (values 1 count))))))

(defun show-bins (solist)
  (let ((bins (car (so-bins solist)))
        (bin-nbits (- +hash-nbits+ (cdr (so-bins solist))))
        (n-occupied-bins 0)
        (sum-chainlengths 0)
        (max-chainlength 0))
    (aver (= (length bins) (ash 1 bin-nbits)))
    (format t "Bins (~d total, ~d leading bits):~%"
            (length bins) bin-nbits)
    (dotimes (i (length bins))
      (multiple-value-bind (occupied count) (show-bin solist i)
        (incf n-occupied-bins occupied)
        (incf sum-chainlengths count)
        (setq max-chainlength (max count max-chainlength))))
    (let ((avg-chainlength (/ sum-chainlengths n-occupied-bins)))
      (format t "~&Total ~D items, avg ~F items/bin~%"
              (so-count solist) avg-chainlength)
      (values max-chainlength (float avg-chainlength)))))

(defun print-hashes (solist)
  (do ((node (%node-next (so-head solist)) (%node-next node)))
      ((endp node))
    (format t "~16x~@[ ~s~]~%"
            (node-hash node)
            (if (so-key-node-p node) (type-of (so-key node))))))

#|
;;; Our SXHASH has _extremely_ bad behavior for the split-order algorithm,
;;; which consumes high bits before low bits. The high bits tend not to get
;;; randomized at all.
;;; Perhaps we should actually try to hash FIXNUMs better for users. Example:
* (dotimes (i 20)
    (let ((a (+ sb-vm:dynamic-space-start (* i 32768))))
      (format t "~4d ~x ~v,'0b~%" i a (or #+64-bit 64 32) (sxhash a))))
   0 1000000000 0001000010010001101110100101010000110101100010111010111001001010
   1 1000008000 0001000010010001101110100101010000110101100000111110111001001010
   2 1000010000 0001000010010001101110100101010000110101100110110010111001001010
   3 1000018000 0001000010010001101110100101010000110101100100110110111001001010
   4 1000020000 0001000010010001101110100101010000110101101010101010111001001010
   5 1000028000 0001000010010001101110100101010000110101101000101110111001001010
   6 1000030000 0001000010010001101110100101010000110101101110100010111001001010
   7 1000038000 0001000010010001101110100101010000110101101100100110111001001010
   8 1000040000 0001000010010001101110100101010000110101110010011010111001001010
   9 1000048000 0001000010010001101110100101010000110101110000011110111001001010
  10 1000050000 0001000010010001101110100101010000110101110110010010111001001010
  11 1000058000 0001000010010001101110100101010000110101110100010110111001001010
  12 1000060000 0001000010010001101110100101010000110101111010001010111001001010
  13 1000068000 0001000010010001101110100101010000110101111000001110111001001010
  14 1000070000 0001000010010001101110100101010000110101111110000010111001001010
  15 1000078000 0001000010010001101110100101010000110101111100000110111001001010
  ...
|#

(defparameter *strings*
  (let ((h (make-hash-table :test 'equal)))
    (dolist (str (sb-vm:list-allocated-objects :all :test #'simple-string-p))
      (setf (gethash str h) t
            (gethash (string-upcase str) h) t
            (gethash (string-downcase str) h) t)
      (setq str (reverse str))
      (setf (gethash str h) t
            (gethash (string-upcase str) h) t
            (gethash (string-downcase str) h) t))
    (loop for str being each hash-key of h collect str)))
(format t "~&Using ~D strings for test~%" (length *strings*))

(defun fill-table-from (table list)
  (dolist (key list table)
    (if (hash-table-p table)
        (setf (gethash key table) t)
        (so-insert table key t))))

(defun make-threads (nwriters nreaders sem strings-holder writer-fn reader-fn)
  (format t "~&GCing...~%")
  (gc :full t)
  (format t "~&Starting test...~%")
  (append
   (loop for i below nwriters
         collect
         (sb-thread:make-thread
          (lambda (&aux (ct 0))
            (sb-thread:wait-on-semaphore sem)
            (loop
              (let ((string (pop (car strings-holder))))
                (unless string (return ct))
                (incf ct)
                (funcall writer-fn string))))
          :name (format nil "writer ~D" i)))
   (loop for i below nreaders
         collect
         (sb-thread:make-thread
          (lambda (&aux (found 0))
            (sb-thread:wait-on-semaphore sem)
            (loop
              ;; just read every string
              (dolist (str *strings*)
                (when (funcall reader-fn str) (incf found)))
              (when (null (car strings-holder)) (return found))))
          :name (format nil "reader ~d" i)))))

(defun test-insert-to-synchronized-table (ntrials nwriters nreaders)
  (let* ((h (make-hash-table :test 'equal :synchronized t))
         (sem (sb-thread:make-semaphore))
         (strings-holder (list *strings*))
         (threads
          (make-threads nwriters nreaders sem strings-holder
                        (lambda (str)  (setf (gethash str h) t)) ; writer action
                        (lambda (str) (gethash str h)))) ; reader action
         (results))
    (sb-thread:signal-semaphore sem (+ nwriters nreaders))
    (time (dolist (thread threads)
            (push (sb-thread:join-thread thread) results)))
    (assert (= (hash-table-count h) (length *strings*)))
    (if (> ntrials 1)
        (test-insert-to-synchronized-table (1- ntrials) nwriters nreaders)
        (values h results))))

(defun test-insert-to-lockfree-table (ntrials nwriters nreaders)
  (let* ((h (make-so-map/string))
         (sem (sb-thread:make-semaphore))
         (strings-holder (list *strings*))
         (threads
          (make-threads nwriters nreaders sem strings-holder
                        (lambda (str) (so-insert h str t))
                        (lambda (str) (so-find/string h str))))
         (results))
    (sb-thread:signal-semaphore sem (+ nwriters nreaders))
    (time (dolist (thread threads)
            (push (sb-thread:join-thread thread) results)))
    (assert (= (so-count h) (length *strings*)))
    (dolist (str *strings*)
      (assert (so-find/string h str)))
    (if (> ntrials 1)
        (test-insert-to-lockfree-table (1- ntrials) nwriters nreaders)
        (values h results))))

(defun test-remove-from-synchronized-table (ntrials nthreads)
  (let* ((h (fill-table-from (make-hash-table :test 'equal :synchronized t)
                             *strings*))
         (sem (sb-thread:make-semaphore))
         (strings-holder (list *strings*))
         (threads
          (loop for i below nthreads
               collect
               (sb-thread:make-thread
                (lambda (&aux (ct 0))
                  (sb-thread:wait-on-semaphore sem)
                  (loop
                        (let ((string (pop (car strings-holder))))
                          (unless string (return ct))
                          (incf ct)
                          (remhash string h)))))))
         (results))
    (sb-thread:signal-semaphore sem nthreads)
    (time (dolist (thread threads)
            (push (sb-thread:join-thread thread) results)))
    (assert (= (hash-table-count h) 0))
    (if (> ntrials 1)
        (test-remove-from-synchronized-table nthreads (1- ntrials))
        results)))

(defun test-remove-from-lockfree-table (ntrials nthreads)
  (let* ((h (fill-table-from (make-so-map/string)
                             *strings*))
         (sem (sb-thread:make-semaphore))
         (strings-holder (list *strings*))
         (threads
          (loop for i below nthreads
               collect
               (sb-thread:make-thread
                (lambda (&aux (ct 0))
                  (sb-thread:wait-on-semaphore sem)
                  (loop
                        (let ((string (pop (car strings-holder))))
                          (unless string (return ct))
                          (incf ct)
                          (so-delete h string)))))))
         (results))
    (sb-thread:signal-semaphore sem nthreads)
    (time (dolist (thread threads)
            (push (sb-thread:join-thread thread) results)))
    (assert (= (so-count h) 0))
    (if (> ntrials 1)
        (test-remove-from-lockfree-table nthreads (1- ntrials))
        results)))

;;; Smoke test:
;;; - Table starts out with some number of keys (all symbol names)
;;; - One mutator thread removes from the table and notifies a semaphore
;;;   each time it has removed one key.
;;; - One mutator thread adds keys and notifies a semaphore.
;;; - The reader asserts that after each semaphore notification,
;;;   the table has/doesn't-have the expected key

;; Build a collection of strings to try inserting
(defparameter *symbol-names*
  (let ((h (make-hash-table :test 'equal)))
    (do-all-symbols (s)
      (let ((name (string s)))
        (when (and (find-if #'upper-case-p name)
                   (not (find-if #'lower-case-p name)))
          (setf (gethash name h) t))))
    (loop for str being each hash-key of h collect str)))

(defun inserter (start-sem msg-sem tbl msgs)
  (sb-thread:wait-on-semaphore start-sem)
  (dolist (string *symbol-names*)
    (let ((key (string-downcase string)))
      (so-insert tbl key)
      (atomic-push `(:inserted ,key) (svref msgs 0))
      (sb-thread:signal-semaphore msg-sem)))
  (atomic-push `(:done) (svref msgs 0))
  (sb-thread:signal-semaphore msg-sem))
(defun deleter (start-sem msg-sem tbl msgs)
  (sb-thread:wait-on-semaphore start-sem)
  (dolist (string *symbol-names*)
    (so-delete tbl string)
    (atomic-push `(:deleted ,string) (svref msgs 0))
    (sb-thread:signal-semaphore msg-sem))
  (atomic-push `(:done) (svref msgs 0))
  (sb-thread:signal-semaphore msg-sem))
(defun reader (start-sem msg-sem tbl msgs &aux (done-count 0))
  (sb-thread:wait-on-semaphore start-sem)
  (loop
    (sb-thread:wait-on-semaphore msg-sem)
    (let ((action (atomic-pop (svref msgs 0))))
      (ecase (car action)
        (:inserted
         (let ((key (second action)))
           (assert (so-find/string tbl key))))
        (:deleted
         (let ((key (second action)))
           (assert (not (so-find/string tbl key)))))
        (:done
         (when (= (incf done-count) 2) (return)))))))

(defun smoke-test ()
  (let* ((tbl (make-so-set/string))
         (start-sem (sb-thread:make-semaphore))
         (msg-sem (sb-thread:make-semaphore))
         (msgs (make-array 1 :initial-element nil))
         (args (list start-sem msg-sem tbl msgs))
         (threads
          (list (sb-thread:make-thread #'inserter :arguments args)
                (sb-thread:make-thread #'deleter :arguments args)
                (sb-thread:make-thread #'reader :arguments args))))
    (dolist (string *symbol-names*)
      (so-insert tbl string))
    (let ((initial-count (so-count tbl)))
      (sb-thread:signal-semaphore start-sem 3)
      (mapc 'sb-thread:join-thread threads)
      (assert (= (so-count tbl) initial-count)))
    tbl))

(test-util:with-test (:name :basic-functionality :skipped-on (not :sb-thread))
  (smoke-test))

;;; All threads try to insert each key. At most one thread wins,
;;; and the others increment a count associated with the key.
;;; The final count per key should be the number of threads,
;;; and total number of actual insertions performed across threads
;;; should equal the total number of keys.
(defun test-insert-same-keys-concurrently (tbl keys
                                           &key (nthreads (max 2 (floor test-util:*n-cpus* 2)))
                                                ((:delete keys-to-delete)))
  (flet ((worker (sem &aux inserted)
           (sb-thread:wait-on-semaphore sem)
           (dolist (key keys)
             (when keys-to-delete
               (let ((delete (atomic-pop (cdr keys-to-delete))))
                 (when delete
                   (so-delete tbl delete))))
             (multiple-value-bind (node foundp) (so-insert tbl key (list 1))
               (if foundp
                   (atomic-incf (car (so-data node)))
                   (push key inserted))))
           ;; Return the list of keys that this worker inserted
           inserted))
    (let* ((start-sem (sb-thread:make-semaphore))
           (args (list start-sem))
           (threads
            (loop repeat nthreads
                  collect (sb-thread:make-thread #'worker :arguments args))))
      (sb-thread:signal-semaphore start-sem nthreads)
      (let* ((results (mapcar 'sb-thread:join-thread threads))
             (counts (mapcar 'length results)))
        (format t "~&Insertion counts: ~S~%" counts)
        (assert (= (reduce #'+ counts) (length keys)))
        (dolist (key keys)
          (let ((node (so-find tbl key)))
            (assert (= (car (so-data node)) nthreads))))))))

(defun assert-not-found (tbl keys)
  ;; no key in keys-to-delete should be in the table
  (dolist (k keys)
    (assert (not (so-find tbl k)))))

(test-util:with-test (:name :concurrently-insert-same-keys/string
                      :skipped-on (not :sb-thread))
  (let* ((objects *strings*)
         (tbl (make-so-map/string))
         (keys-to-delete
          (loop for i from 1 to (length objects)
                collect (let ((key (concatenate 'string
                                               ;; avoid colliding with anything
                                               ;; in *STRINGS*
                                                (string #+sb-unicode #\blue_heart
                                                        #-sb-unicode (code-char 255))
                                                (write-to-string i))))
                          (so-insert tbl key i)
                          key))))
    (test-insert-same-keys-concurrently tbl objects :delete (cons nil keys-to-delete))
    (assert-not-found tbl keys-to-delete)
    tbl))

;;; sb-eval gets
;;;    UNEXPECTED-FAILURE :CONCURRENTLY-INSERT-SAME-KEYS/OBJECT
;;;      due to UNBOUND-VARIABLE: "The variable X is unbound."
;;; It's talking about the X in the REMOVE-IF lambda.
;;; I tried renaming it to BLAHBLAH and sure enough got
;;;      due to UNBOUND-VARIABLE: "The variable BLAHBLAH is unbound."
;;; Based on that I know what the problem was: sb-eval uses a certain magic uninterned
;;; symbol in a LET binding frame to indicate that the variable is special.
;;; To read the binding, it examines the value in the storage location, and if it sees
;;; the magic symbol, it calls SYMBOL-VALUE instead. So sb-eval can't actually represent
;;; a lexical var whose _actual_ _value_ is that magic uninterned symbol.
;;; Instead you'll (potentially) get an error that the variable you referenced is
;;; unbound, unless it really is specially bound also.
;;; Naturally MAP-ALLOCATED-OBJECTS produces that symbol, and so this test can't run,
;;; because we "don't know" what the magic symbol is, since it's uninterned and
;;; therefore can't easily weed it out from the list.
;;; sb-fasteval does not use that same technique to represent special bindings,
;;; and has no problem iterating over all symbols.
(test-util:with-test (:name :concurrently-insert-same-keys/object
                      :skipped-on (or (not :sb-thread)
                                      (and :interpreter (not :sb-fasteval))))
  (let* ((objects
          (remove-if (lambda (x)
                       (not (eql (generation-of x) sb-vm:+pseudo-static-generation+)))
                     (sb-vm:list-allocated-objects :all :type sb-vm:symbol-widetag)))
         (tbl (make-so-map/addr))
         (keys-to-delete
          ;; preload the hashset with some keys that will be deleted
          ;; concurrently with all the insertions
          (loop for i from 1 to (length objects)
                do (so-insert tbl i i)
                   collect i)))
    (test-insert-same-keys-concurrently tbl objects :delete (cons nil keys-to-delete))
    (assert-not-found tbl keys-to-delete)))

(defun logical-delete (node &aux (succ (%node-next node)))
  (unless (fixnump succ)
    (with-pinned-objects (succ)
      (cas (%node-next node) succ (make-marked-ref succ))))
  node)

(defparameter *so-map*
  (let ((tbl (make-so-map/string)))
    (loop for i from (char-code #\a) to (char-code #\z)
          do (so-insert tbl (string (code-char i)) (char-upcase (code-char i))))
    tbl))
(defparameter *keys-in-table-order* nil)
(defparameter *deleted-nodes* nil)
(defparameter *node-addresses* nil)

(let ((x 0))
  (sb-int:collect ((allkeys) (addresses) (deleted))
    (so-maplist
     (lambda (node)
       (let ((key (so-key node)))
         (allkeys key)
         (when (evenp (incf x))
           (deleted node)
           (logical-delete node))
         (addresses (get-lisp-obj-address node))))
     *so-map*)
    (setf *keys-in-table-order* (allkeys)
          *deleted-nodes* (deleted)
          *node-addresses* (addresses))))

(gc)
;;; Most of the node addresses should have changed,
;;; but the list should still be fully intact.
(test-util:with-test (:name :solist-integrity)
  (let ((node (so-head *so-map*))
        (addr-change 0)
        (addresses *node-addresses*)
        (keys *keys-in-table-order*))
    (loop
      (cond ((endp node) (return))
            ((dummy-node-p node) ; skip
             (setq node (get-next node)))
            (t
             (assert (eq (so-key node) (pop keys)))
             (unless (= (get-lisp-obj-address node) (pop addresses))
               (incf addr-change))
             (multiple-value-bind (next next-bits) (get-next node)
               (let ((node-deletedp (fixnump next-bits))
                     (found (so-find *so-map* (so-key node))))
                 (assert (eq (not (null (find node *deleted-nodes*)))
                             node-deletedp))
                 (if node-deletedp
                     (assert (not found))
                     (assert (eq found node))))
               (setq node next)))))
    #-mark-region-gc
    (assert (>= addr-change 22)))) ; seems about right

;;; SO-MAPLIST does not include deleted nodes.
;;; Nodes marked for deletion should still be marked after GC
;;; (though it should be possible to modify GC to finish deletion)
(test-util:with-test (:name :solist-mid-deletion)
  (let ((present-keys
         (remove-if
          (lambda (string)
            (member string *deleted-nodes* :key #'so-key :test #'string=))
          *keys-in-table-order*)))
    (so-maplist (lambda (node)
                  (assert (string= (so-key node) (pop present-keys))))
                *so-map*)))

(defvar *example-objects*
  (subseq (append
           (remove-if (lambda (x) (/= (sb-kernel:generation-of x) sb-vm:+pseudo-static-generation+))
                      (sb-vm:list-allocated-objects :dynamic :type sb-vm:simple-base-string-widetag))
           (sb-vm:list-allocated-objects :read-only :type sb-vm:simple-base-string-widetag))
          0 1000))

(test-util:with-test (:name :c-find-in-solist)
  (let ((set (sb-lockless:make-so-set/addr))  )
    (dolist (x *example-objects*)
      (sb-lockless:so-insert set x))
    (assert (not (sb-lockless:c-so-find/addr set 'random)))
    (dolist (x *example-objects*)
      (let ((node (sb-lockless:c-so-find/addr set x)))
        (assert node)
        (assert (eq (sb-lockless:so-key node) x))))))

(test-util:with-test (:name :solist-2-phase-insert)
  (let ((set (sb-lockless:make-so-set/addr))
        (example-objects *example-objects*)
        (n-deleted 0)
        (nodes))
    ;; This example is artificial. The real usage would allocate one object and perform
    ;; both insert phases in the following pattern:
    ;;   begin pseudo-atomic
    ;;     allocate split-order node 'n'
    ;;     allocate off-heap large unboxed object
    ;;     phase1 insert node 'n' pointing to large-object
    ;;   end pseudo-atomic
    ;;   phase2 insert
    ;; If GC occurs just after the pseudo-atomic section, it is possible to test each stack word
    ;; as being a conservative pointer to an off-heap object based on its presence in the table.
    (dolist (object example-objects)
      (let ((node (sb-lockless::%make-so-set-node 0 0)))
        (push node nodes)
        (sb-lockless::%so-eq-set-phase1-insert set node object)))
    ;; It has no bearing on currectness that the table count is understated and that
    ;; the number of bins may be too few prior to running the second step.
    ;; This is evident from the loop below which shows that each example-object can be found.
    (dolist (node nodes)
      (sb-lockless::%so-eq-set-phase2-insert set node)
      ;; delete some keys at random. This could occur only after the node is fully inserted.
      (when (zerop (random 10))
        (sb-lockless:so-delete set (sb-lockless:so-key node))
        (incf n-deleted))
      (let ((table-count
             (loop for object in example-objects
                   count (sb-lockless:so-find set object))))
        (assert (= table-count (- 1000 n-deleted)))))
    ;; Finally the table count should be correct
    (assert (= (sb-lockless::so-count set) (- 1000 n-deleted)))
    set))

#|
;; Speedup: 4x
Small test: 20k keys, 8 writers, 2 readers
==========================================
* (test-insert-to-synchronized-table 1 8 2)
GCing...
Starting test...
Evaluation took:
  0.048 seconds of real time
  0.261340 seconds of total run time (0.091142 user, 0.170198 system)
  543.75% CPU
  123,504,864 processor cycles
  4,029,824 bytes consed
#<HASH-TABLE :TEST EQUAL :COUNT 24141 {10010A2803}>
* (test-insert-to-lockfree-table 1 8 2)
GCing...
Starting test...
Evaluation took:
  0.011 seconds of real time
  0.096867 seconds of total run time (0.096867 user, 0.000000 system)
  881.82% CPU
  33,070,828 processor cycles
  4,924,544 bytes consed
#<SPLIT-ORDERED-LIST 24141 keys, 16384 bins {1003CA0373}>

;; Speedup: 7x
Large test: 96k keys, 8 writers, 4 readers
==========================================
* (test-insert-to-synchronized-table 1 8 4)
GCing...
Starting test...
Evaluation took:
  0.344 seconds of real time
  2.180249 seconds of total run time (0.689017 user, 1.491232 system)
  633.72% CPU
  963,461,936 processor cycles
  15,657,312 bytes consed
#<HASH-TABLE :TEST EQUAL :COUNT 95949 {10010A56B3}>
* (test-insert-to-lockfree-table 1 8 4)
GCing...
Starting test...
Evaluation took:
  0.047 seconds of real time
  0.453098 seconds of total run time (0.453098 user, 0.000000 system)
  963.83% CPU
  136,186,892 processor cycles
  12,143,696 bytes consed
#<SPLIT-ORDERED-LIST 95949 keys, 32768 bins {10010E2C83}>

;; Speedup: 14x
Huge test: 122k keys, 8 writers, 10 readers
============================================
* (test-insert-to-synchronized-table 1 8 10)
GCing...
Starting test...
Evaluation took:
  1.007 seconds of real time
  10.534241 seconds of total run time (2.623577 user, 7.910664 system)
  1046.08% CPU
  2,815,228,188 processor cycles
  29,103,872 bytes consed
* (test-insert-to-lockfree-table 1 8 10)
GCing...
Starting test...
Evaluation took:
  0.068 seconds of real time
  1.069891 seconds of total run time (1.069891 user, 0.000000 system)
  1573.53% CPU
  187,631,304 processor cycles
  22,077,904 bytes consed
|#

#+nil
(let* ((t0 (get-internal-real-time))
       (table (test-insert-to-synchronized-table 1 8 10))
       (t1 (get-internal-real-time))
       (sb-lockless::*desired-elts-per-bin* 9)
       (solist (test-insert-to-lockfree-table 1 8 10))
       (t2 (get-internal-real-time))
       (et1 (- t1 t0))
       (et2 (- t2 t1))
       (deep-size-ht (deep-size table))
       (deep-size-so (deep-size solist)))
  (print table)
  (print solist)
  (format t "~&Speedup-ratio = ~F~%" (/ et1 et2))
  (format t "Size ratio ~F~%" (/ deep-size-so deep-size-ht)))
