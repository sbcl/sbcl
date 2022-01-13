(in-package "SB-LOCKLESS")

#-gencgc (sb-ext:exit :code 104)

(assert (not (logtest (sb-kernel:wrapper-flags (find-layout 'keyed-node))
                      sb-kernel::+strictly-boxed-flag+)))

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
(defun logical-delete (n list)
  (let* ((node (lfl-nth n list))
         (succ (%node-next node)))
    (unless (fixnump succ)
      (with-pinned-objects (succ)
        (cas (%node-next node) succ (make-marked-ref succ)))))
  list)

(logical-delete 0 *lll2*)

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
    (logical-delete 1 *lfl*)
    (logical-delete 4 *lfl*)
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
    (logical-delete 0 list)
    (logical-delete 1 list)
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
  (let ((cpus
          (max 1
               #-win32 (sb-alien:alien-funcall
                        (sb-alien:extern-alien "sysconf"
                                               (function sb-alien:long sb-alien:int))
                        sb-unix::sc-nprocessors-onln)
               #+win32 (sb-alien:extern-alien "os_number_of_processors" sb-alien:int))))
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
    (let* ((where (sb-sys:sap+ (sb-sys:int-sap (current-dynamic-space-start))
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
  (loop repeat n for key from 10 by 10 do (lfl-insert *l* key (make-foo :a key))))

(defun scan-lfl-gens (deletep &aux page-indices)
  (do ((node (get-next (list-head *l*)) ; can't delete the dummy node (list head)
             (get-next node)))
      ((eq node *tail-atom*))
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
