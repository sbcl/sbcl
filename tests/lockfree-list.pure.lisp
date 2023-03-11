(shadowing-import 'sb-lockless::endp)
(import 'sb-kernel::(find-layout generation-of get-lisp-obj-address))
(import 'sb-int::(fixnump))
(import 'sb-sys::(sap>= with-pinned-objects))
(import 'sb-lockless::(list-node keyed-node +tail+
                       %node-next get-next
                       list-head lfl-insert lfl-delete make-ordered-list
                       make-marked-ref
                       finish-incomplete-deletions))

;; Missing runtime functions
#+interpreter (invoke-restart 'run-tests::skip-file)

(test-util:with-test (:name :layout-bits)
  (dolist (type '(list-node keyed-node))
    (assert (eq (sb-kernel::dd-%element-type
                 (sb-kernel:find-defstruct-description type))
                '*))
    (assert (not (logtest (sb-kernel:wrapper-flags (find-layout type))
                          sb-kernel::+strictly-boxed-flag+)))))


;;; Make sure no promotions occur so that objects will be movable
;;; throughout these tests.
(setf (generation-number-of-gcs-before-promotion 0) 1000000)

;;; Set up a list of 2 items for later use
(defparameter *lll2* (make-ordered-list :key-type 'fixnum))
(defparameter *5* (lfl-insert *lll2* 5 'five))
(defparameter *10* (lfl-insert *lll2* 10 'ten))
(defparameter *addr-of-10* (get-lisp-obj-address *10*))

(defun check-next-is-10 (node)
  (assert (eq (get-next node) *10*)))

;;;; These functions are for examining GC behavior.

(defun lfl-nth (n list)
  (let ((node (%node-next (list-head list))))
    (dotimes (i n node)
      (setq node (get-next node)))))

;;; For testing (especially the garbage collector), perform the first CAS
;;; operation but not the second CAS of the deletion algorithm.
(defun logical-delete (node &aux (succ (%node-next node)))
  (unless (fixnump succ)
    (with-pinned-objects (succ)
      (cas (%node-next node) succ (make-marked-ref succ))))
  node)
(defun logical-delete-nth (n list)
  (logical-delete (lfl-nth n list))
  list)

(defun count-list-nodes (list &aux (n 0))
  (do ((node (get-next (list-head list)) (get-next node)))
      ((endp node) n)
    (incf n)))

(defun lockfree-list-with-many-deleted-nodes (n)
  (let ((list (make-ordered-list :key-type 'fixnum)))
    ;; insert items in descending order because otherwise this would take N^2 time
    (loop for i from n downto 1 do (lfl-insert list i (- i)))
    ;; logically delete half the nodes
    (do ((i 0 (1+ i))
         (node (list-head list) (get-next node)))
        ((endp node))
      (when (oddp i) (logical-delete node)))
    list))

(test-util:with-test (:name :lockfree-list-node-implicit-pin-untagged-pointer
                            :skipped-on (not :sb-thread))
  (let* ((keepon t)
         (n-nodes 15000)
         (list (lockfree-list-with-many-deleted-nodes n-nodes))
         (thread ; Start a thread to invoke GC
          (sb-thread:make-thread
           (lambda ()
             (loop
               (sb-thread:barrier (:read))
               (when (not keepon) (return))
               (gc)
               (sleep .001))))))
    (loop repeat 1000 for i from 0
          do (let ((counted (count-list-nodes list)))
               (assert (= counted n-nodes))))
    (setq keepon nil)
    (sb-thread:barrier (:write))
    (sb-thread:join-thread thread)))

(logical-delete-nth 0 *lll2*)

(defvar *lfl*)
(defvar *l*)
(flet ((show (node step when)
         (format t "~a: " when)
         (loop (format t "~x" (get-lisp-obj-address node))
               (unless (setq node (funcall step node)) (return))
               (format t " - "))
         (terpri)))
  (defun makelist (n)
    (setq *l* nil)
    (dotimes (i n) (push (cons i (format nil "~r" i)) *l*))
    (show *l* #'cdr "init")
    (setq *l* (nreverse *l*))
    (show *l* #'cdr "rev ")
    (gc)
    (show *l* #'cdr "GCed"))
  (defun makelflist (n &optional show)
    (setq *lfl* (make-ordered-list :key-type 'fixnum))
    (dotimes (i n) (lfl-insert *lfl* (* i n) (format nil "~r" i)))
    (when show (show (list-head *lfl*) #'get-next "init"))
    (gc)
    (when show (show (list-head *lfl*) #'get-next "GCed"))
    (logical-delete-nth 1 *lfl*)
    (logical-delete-nth 4 *lfl*)
    (gc)
    (when show (show (list-head *lfl*) #'get-next "del "))))

;; Enable heap validity tester
#+gencgc (setf (sb-alien:extern-alien "verify_gens" char) 0)

(test-util:with-test (:name :lockfree-list-gc-correctness)
  ;; Create a small list and perform logical deletion of 2 nodes
  (makelflist 10)
  (gc)) ; Verify no post-gc crash

(test-util:with-test (:name :lockfree-list-finalize-deletion)
  ;; Check that save-lisp-and-die can remove deleted nodes
  (let ((list (make-ordered-list :key-type 'fixnum)))
    (lfl-insert list 4 "four")
    (lfl-insert list 5 "five")
    (logical-delete-nth 0 list)
    (logical-delete-nth 1 list)
    (finish-incomplete-deletions list)
    (let* ((node (list-head list))
           (next (get-next node)))
      (assert (endp next)))))

;;; These functions are for comparing the running time of a lock-based
;;; implementation versus lockfree.

(defun new-synchronized-list ()
  (list (sb-thread:make-mutex)))
(defun list-search (list key)
  (let* (left
         (this list)
         (next (cdr this)))
    (loop (setq left this this next)
          (when (null this)
            (return))
          (setq next (cdr this))
          (unless (< (truly-the fixnum (caar this)) key)
            (return)))
    (values this left)))
(defun locked-insert (list key value)
  (sb-thread:with-mutex ((car list))
    (multiple-value-bind (successor predecessor) (list-search list key)
      (let ((new (cons (cons key value) successor)))
        (rplacd predecessor new)))
    list))
(defun locked-delete (list key)
  (sb-thread:with-mutex ((car list))
    (multiple-value-bind (this predecessor) (list-search list key)
      (when (and this (= (caar this) key))
        (rplacd predecessor (cdr this)))))
  list)

;;;

(defglobal *worklist* nil)
(defmacro smoketest-macro (constructor inserter deleter)
  `(let ((list ,constructor)
         (threads))
     (assert (<= n-threads 50))
     (let ((max (* n-threads n-items)))
       (dotimes (i n-threads)
         (push (sb-thread:make-thread
                (lambda (my-items &aux (ct 0))
                  (loop
                   (let ((val (atomic-pop *worklist*)))
                     (unless val (return))
                     (setf (aref my-items ct) val)
                     (,inserter list val (- val))
                     (incf ct)
                     (when (oddp ct)
                       (let ((item-to-delete
                              (aref my-items (floor ct 2))))
                         (,deleter list item-to-delete))))))
                :arguments (make-array max))
               threads)))
     (dolist (thr threads) (sb-thread:join-thread thr))
     list))

(defun smoketest-lockfree (n-threads n-items)
  (smoketest-macro (make-ordered-list :key-type 'fixnum)
                   lfl-insert lfl-delete))
(defun smoketest-locked (n-threads n-items)
  (smoketest-macro (new-synchronized-list)
                   locked-insert locked-delete))

(defun primitive-benchmark (&key (n-trials 8) (n-threads 10) (n-items 600) print)
  (let ((best 0))
    (dotimes (trial n-trials (float best))
      (let* ((max (* n-threads n-items))
             (worklist (test-util:shuffle (test-util:integer-sequence max)))
             (random-state (make-random-state)))
        (let ((rt0 (get-internal-real-time))
              (rt1)
              (rt2))
          (setq *worklist* (copy-list worklist)
                *random-state* (make-random-state random-state))
          (smoketest-locked n-threads n-items)
          (setq rt1 (get-internal-real-time))
          (setq *worklist* (copy-list worklist)
                *random-state* (make-random-state random-state))
          (smoketest-lockfree n-threads n-items)
          (setq rt2 (get-internal-real-time))
          (let* ((et-locked (- rt1 rt0))
                 (et-lockfree (- rt2 rt1))
                 (ratio (/ et-locked et-lockfree)))
          (when print
            (format t "elapsed-times: locked=~d and lockfree=~d ratio=~f~%"
                    et-locked et-lockfree ratio))
          (setq best (max ratio best))))))))

(test-util:with-test (:name :lockfree-list-performance
                      :skipped-on :sbcl)
  (let ((cpus test-util:*n-cpus*))
    (assert (> (primitive-benchmark) (min 2 (log cpus 2))))))

(test-util:with-test (:name :lfl-next-implicit-pin-one-more-time)
  (let* ((anode *5*)
         (next (%node-next anode)))
    (assert (fixnump next))
    ;; maybe this should be a keyword to WITH-PINNED-OBJECTS saying
    ;; to explicitly cons onto the list of pins?
    (let ((sb-vm::*pinned-objects* (cons anode sb-vm::*pinned-objects*)))
      (gc)
      ;; nowhere do we reference node *10* in this test,
      ;; so there's no reason the GC should want not to move that node
      ;; except for the implicit pin on account of node *5*.
      ;; i.e. if we can reconstruct node 10 from its pointer
      ;; as a fixnum, then the special GC strategy flag worked.
      (check-next-is-10 anode)
      (assert (eql (logandc2 (get-lisp-obj-address (get-next anode))
                             sb-vm:lowtag-mask)
                   (ldb (byte sb-vm:n-word-bits 0)
                        (ash next sb-vm:n-fixnum-tag-bits)))))))

;; Show that nodes which are referenced only via a "fixnum" pointer
;; can and do actually move via GC, which adjusts the fixnum accordingly.
(test-util:with-test (:name :lfl-not-pinned)
  (gc)
  (assert (= (sb-kernel:generation-of *5*) 0))
  (assert (= (sb-kernel:generation-of *10*) 0))
  (assert (not (eql (get-lisp-obj-address *10*)
                    *addr-of-10*))))


(defun scan-pointee-gens (page &optional print)
  (unless (zerop (sb-alien:slot (sb-alien:deref sb-vm::page-table page) 'sb-vm::start))
    (warn "Can't properly scan: page start is a lower address"))
  (sb-sys:without-gcing
    (let* ((where (sb-sys:sap+ (sb-sys:int-sap sb-vm:dynamic-space-start)
                               (* page sb-vm:gencgc-page-bytes)))
           (limit (sb-sys:sap+ where sb-vm:gencgc-page-bytes))
           (gens 0))
      (do ((where where (sb-sys:sap+ where sb-vm:n-word-bytes)))
          ((sap>= where limit) gens)
        (let* ((word (sb-sys:sap-ref-word where 0))
               (targ-page)
               (gen (when (and (sb-vm:is-lisp-pointer word)
                               (>= (setq targ-page (sb-vm:find-page-index word)) 0))
                      (sb-alien:slot (sb-alien:deref sb-vm::page-table targ-page)
                                     'sb-vm::gen))))
          (when gen (setq gens (logior gens (ash 1 gen))))
          (when print
            (sb-alien:alien-funcall
             (sb-alien:extern-alien "printf"
                                    (function sb-alien:void system-area-pointer
                                              system-area-pointer
                                              sb-alien:unsigned
                                              sb-alien:unsigned))
               (sb-sys:vector-sap (if gen #.(format nil "%p: %p -> %d~%") #.(format nil "%p: %p~%")))
               where word (or gen 0))))))))

;;; Make sure promotions do occur.
(setf (generation-number-of-gcs-before-promotion 0) 1)

(defstruct foo a b c)
(defparameter *l* nil)
(defun construct (n)
  (setq *l* (make-ordered-list :key-type 'fixnum))
  (loop for key from 10 by 10 repeat n do (lfl-insert *l* key (make-foo :a key))))

(defun scan-lfl-gens (deletep &aux page-indices)
  (do ((node (get-next (list-head *l*)) ; can't delete the dummy node (list head)
             (get-next node)))
      ((eq node +tail+))
    (when (eql (generation-of node) 1)
      (let ((page (sb-vm:find-page-index
                   (sb-kernel:get-lisp-obj-address node))))
        (pushnew page page-indices)))
    (let ((succ (get-next node)))
      (when (and (eql (generation-of node) 1)
                 (eql (generation-of succ) 0)
                 deletep)
        ;; Logically delete NODE, turning its successor pointer untagged.
        (format t "~A (deleting) -> ~A~%" node succ)
        (with-pinned-objects (succ)
          (cas (%node-next node) succ (make-marked-ref succ))))))
  (dolist (page page-indices)
    (format t "Page ~d -> ~b~%" page (scan-pointee-gens page))))

(defun test-fixnum-as-pointer ()
  (construct 250)
  (gc :gen 1)
  (loop for key from 15 by 400 repeat 10  do (lfl-insert *l* key (- key)))
  ;; For informational purposes, print the page number on which any node
  ;; of *L* is present, and the bitmask of generations to which that page points.
  (scan-lfl-gens nil)
  ;; Now logically delete any node on a generation 1 page that points
  ;; to a generation 0 page.  The deletion algorithm first marks the NEXT
  ;; pointer on the node being deleted, where "mark" equates to turning
  ;; the successor pointer to an untagged pointer.
  (scan-lfl-gens t)
  (gc))

(test-util:with-test (:name :lfl-hidden-pointers) (test-fixnum-as-pointer))

;;; Test lists of WORD stored as tagged fixnum (since we don't have
;;; a variant of the struct with a untagged slot for the key)
(test-util:with-test (:name :word-list)
  (let ((l (sb-lockless:make-ordered-list :key-type 'sb-lockless::word))
        ;; these are in sorted order already
        (keys `(#x1000
                ,(ash 1 (1- sb-vm:n-word-bits))
                ,(logandc2 sb-ext:most-positive-word sb-vm:fixnum-tag-mask))))
    (dolist (key keys)
      (sb-lockless:lfl-insert l (sb-kernel:%make-lisp-obj key) key))
    (sb-lockless::do-lockfree-list (node l)
      (assert (eql (sb-lockless::node-data node) (pop keys))))))

;;; Test a custom sort

(test-util:with-test (:name :custom-sort)
  ;; ordinary case-sensitive list
  (let ((l (sb-lockless:make-ordered-list :key-type 'string)))
    (sb-lockless:lfl-insert l "Monkey" 1)
    (sb-lockless:lfl-insert l "aardvark" 1)
    (assert (equal (sb-lockless::lfl-keys l)
                   '("Monkey" "aardvark"))))
  (let ((l (sb-lockless:make-ordered-list :sort 'string-lessp :test 'string-equal)))
    (sb-lockless:lfl-insert l "Monkey" 1)
    (sb-lockless:lfl-insert l "MONKEY" 2) ; won't insert this
    (sb-lockless:lfl-insert l "aardvark" 1)
    (sb-lockless:lfl-insert l "Aardvark" 1) ; won't insert this
    (assert (equal (sb-lockless::lfl-keys l)
                   '("aardvark" "Monkey")))))

;;; Test the finder which takes a node argument

(defparameter *stringlist* '("ANT" "BAT" "CAT" "DOG" "EGG" "FAN"))

(defun test-find-from-starting-node (lflist)
  ;; This computes the pointer to "BAT", not "CAT", because the head node
  ;; is a dummy node, so head->next is "ANT" and head->next->next is "BAT"
  ;; which can be construed as the CDR and not the CDDR of the list.
  (let ((cdr
       (sb-lockless::get-next
        (sb-lockless::get-next
         (sb-lockless::list-head lflist)))))
    (assert (sb-lockless:lfl-find lflist "BAT"))
    ;; starting at CDR you can't find "ANT"
    (assert (not (sb-lockless::lfl-find*/t lflist cdr "ANT")))
    ;; perhaps somewhat surprisingly, when specifying a starting node,
    ;; you can not find the key in exactly that node- you have pass in a node
    ;; that is strictly to the left. It make sense in light of the need to
    ;; assist with repair of pointers to items in mid-deletion state.
    (assert (not (sb-lockless::lfl-find*/t lflist cdr "BAT")))
    (assert (sb-lockless::lfl-find*/t lflist cdr "CAT"))
    ;; You can delete "CAT" starting from "BAT", just like in a list built of conses
    (assert (sb-lockless::lfl-delete*/t lflist cdr "CAT"))
    (assert (sb-lockless::lfl-delete*/t lflist cdr "DOG"))
    ;; Insert them back in, then find "CAT", logically delete it, and then
    ;; try to find "CAT" and "DOG". Each FIND should physically delete CAT.
    (dolist (key-to-find '("CAT" "DOG"))
      (sb-lockless::lfl-insert*/t lflist cdr "DOG" t)
      (sb-lockless::lfl-insert*/t lflist cdr "CAT" t)
      (logical-delete (sb-lockless::lfl-find*/t lflist cdr "CAT"))
      ;; "CAT" is flagged for deletion in the printed representation
      (assert (string= (princ-to-string lflist)
                       "#<LINKED-LIST {ANT BAT *CAT DOG EGG FAN}>"))
      (let ((node (sb-lockless::lfl-find*/t lflist cdr key-to-find)))
        (cond ((string= key-to-find "CAT")
               (assert (not node))) ; didn't find CAT
              (t
               (assert node) ; did find DOG
               (assert (string= (sb-lockless::node-key node) "DOG")))))
      ;; either operation finished the deletion of "CAT"
      (assert (string= (princ-to-string lflist)
                       "#<LINKED-LIST {ANT BAT DOG EGG FAN}>")))))

(defun traced-string< (a b)
  (format t "~&STRING< ~S ~S~%" a b) (force-output)
  (string< a b))
(defun traced-string= (a b)
  (format t "~&STRING= ~S ~S~%" a b) (force-output)
  (string= a b))

(test-util:with-test (:name :guided-find-and-deletion)
  (let ((lf-stringlist (sb-lockless:make-ordered-list :key-type 'string)))
    (dolist (s *stringlist*) (sb-lockless:lfl-insert lf-stringlist s 0))
    (test-find-from-starting-node lf-stringlist))
  #+nil
  (let ((lf-stringlist (sb-lockless:make-ordered-list :sort 'traced-string<
                                                      :test 'traced-string=)))
    (dolist (s *stringlist*) (sb-lockless:lfl-insert lf-stringlist s 0))
    (test-find-from-starting-node lf-stringlist)))

;;; Some tests which should pound on the algorithms a bit more aggressively

(defglobal *timestamp* 0)
(declaim (fixnum *timestamp*))
(defglobal *keys-to-delete* nil)

(defvar *start-sem* (sb-thread:make-semaphore))
(defvar *done-sem* (sb-thread:make-semaphore))

(defun insert-and-delete-some-stuff (my-insertions lflist &aux (ndeletions 0))
  (sb-thread:wait-on-semaphore *start-sem*)
  (loop
    ;; try to delete 1 thing, if there is something to delete
    ;; and either it was not inserted by me (i.e. try to delete
    ;; someone else's key) or else I have no other insertions
    (let ((head (car *keys-to-delete*)))
      (when (and head (or (not (eq (car head) sb-thread:*current-thread*))
                          (not my-insertions)))
        (let ((string-to-delete
               (cdr (atomic-pop *keys-to-delete*))))
          (when string-to-delete
            ;; whatever we got, just delete it
            (incf ndeletions)
            ;; DELETE returns T if it deleted, NIL if it didn't
            (assert (sb-lockless:lfl-delete lflist string-to-delete))))))
   (when my-insertions
     (let ((string-to-insert (pop my-insertions))
           (value (atomic-incf *timestamp*)))
       (sb-lockless:lfl-insert lflist string-to-insert value)
       ;; Schedule for deletion any string containing the letter A
       (when (find #\A string-to-insert)
         (atomic-push (cons sb-thread:*current-thread* string-to-insert)
                      *keys-to-delete*))))
   (when (and (not my-insertions) (not *keys-to-delete*))
     (return)))
  ndeletions)

(defun get-test-strings (package-name)
  (map 'list #'string
   (remove-if-not #'symbolp
                  (sb-impl::symtbl-cells
                   (sb-impl::package-internal-symbols
                    (find-package package-name))))))

(defun tester (lflist worklists &aux (nthreads (length worklists)) threads)
  (dotimes (i nthreads)
    (push (sb-thread:make-thread
           #'insert-and-delete-some-stuff
           :arguments (list (aref worklists i) lflist))
          threads))
  (sb-thread:signal-semaphore *start-sem* nthreads)
  (dolist (thread threads) (sb-thread:join-thread thread))
  ;;(format t " ~s" (sb-thread:join-thread thread)))
  ;;(format t "; ~D more items to delete~%" (length *keys-to-delete*))
  (dolist (k *keys-to-delete*) (sb-lockless:lfl-delete lflist (cdr k)))
  (setq *keys-to-delete* nil)
  (let ((result (sb-lockless::lfl-keys lflist))
        (expected-result
         ;; list should end up in the correct order with no lost
         ;; insertions or deletions
         (coerce (sort (remove-if (lambda (x) (find #\A x))
                                  (apply #'append (coerce worklists 'list)))
                       #'string<)
                 'list)))
    (assert (equal result expected-result)))
  lflist)

(defparameter nthreads 5)

(test-util:with-test (:name :insert-sorted-up :skipped-on (:not :sb-thread))
  (dotimes (i 10)
    (let ((worklists (make-array nthreads :initial-element nil))
          (lflist (sb-lockless:make-ordered-list :key-type 'string)))
      ;; round-robin assign items to the worklists so that a cluster of keys
      ;; that end up near each other will tend to get simultaneously inserted
      ;; by all threads (no guarantee of course).
      ;; This should tend to retry the compare-and-swap more often.
      (let ((i 0))
        (dolist (item (sort (get-test-strings "SB-C") #'string<))
          (push item (elt worklists i))
          (setf i (mod (1+ i) nthreads))))
      (tester lflist worklists))))

(test-util:with-test (:name :insert-sorted-down :skipped-on (:not :sb-thread))
  (dotimes (i 10)
    (let ((worklists (make-array nthreads :initial-element nil))
          (lflist (sb-lockless:make-ordered-list :key-type 'string)))
      (let ((i 0))
        (dolist (item (sort (get-test-strings "SB-PCL") #'string>))
          (push item (elt worklists i))
          (setf i (mod (1+ i) nthreads))))
      (tester lflist worklists))))

(test-util:with-test (:name :insert-shuffled :skipped-on (:not :sb-thread))
  (dotimes (i 100)
    (let* ((strings (test-util:shuffle (get-test-strings "SB-LOOP")))
           (chunk-size (floor (length strings) nthreads))
           (chunk-start 0)
           (lflist (sb-lockless:make-ordered-list :key-type 'string))
           (worklists (make-array nthreads)))
      (dotimes (i nthreads)
        (setf (aref worklists i)
              (subseq strings chunk-start
                      (if (< i (1- nthreads)) (+ chunk-start chunk-size))))
        (incf chunk-start chunk-size))
      (tester lflist worklists))))
