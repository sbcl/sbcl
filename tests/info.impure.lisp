;;;; tests of the INFO/globaldb system
;;;;
;;;; KLUDGE: Unlike most of the system's tests, these are not in the
;;;; problem domain, but in the implementation domain, so modification
;;;; of the system could cause these tests to fail even if the system
;;;; was still a correct implementation of ANSI Common Lisp + SBCL
;;;; extensions. Perhaps such tests should be separate from tests in
;;;; the problem domain. -- WHN 2001-02-11

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

(in-package :cl-user)

(defun foo (a) (list a))
(let ((x 1)) (foo x))

(assert (eq (sb-int:info :function :where-from 'foo)
            :defined))

(defun foo (a b) (list a b))
(let ((x 1)) (foo x 2))

(flet ((foo (a b c)
         (list a b c)))
  (foo 1 2 3))

;;; FIXME: This one is commented out since it doesn't work when
;;; the DEFUN is just LOADed instead of COMPILE-FILEd, and it's
;;; not immediately obvious what's the best way to set up
;;; the COMPILE-FILE test.
#||
(assert
  (equal
   (format nil "~A" (sb-int:info :function :type 'foo))
   "#<FUN-TYPE (FUNCTION (T T) LIST)>"))
||#

(in-package "SB-C")

(test-util:with-test (:name :bug-458015)
  ;; Make sure layouts have sane source-locations
  (dolist (env *info-environment*)
    (do-info (env :class class :type type :name info-name :value value)
      (when (and (symbolp info-name)
                 (eql class :type)
                 (eql type :kind))
        (let* ((classoid (find-classoid info-name nil))
               (layout (and classoid (classoid-layout classoid)))
               (srcloc (and layout (sb-kernel::layout-source-location layout))))
          (when (and layout)
            (assert (or (definition-source-location-p srcloc)
                        (null srcloc)))))))))

(test-util:with-test (:name :set-info-value-type-check)
  (loop for type-info across *info-types*
        when (and type-info (not (eq (type-info-type type-info) 't)))
        do
        (let ((key1 (class-info-name (type-info-class type-info)))
              (key2 (type-info-name type-info))
              (sillyval (make-string-output-stream))) ; nothing should be this
          ;; check the type-checker function
          (let ((f (compile nil
                            `(lambda (x)
                               (declare (notinline (setf info)))
                               (setf (info ,key1 ,key2 'grrr) x)))))
            (assert (typep (nth-value 1 (ignore-errors (funcall f sillyval)))
                           'type-error)))
          ;; Demonstrate that the SETF disallows the illegal value
          ;; even though this lambda attempts to be non-type-safe.
          (let ((f (compile nil `(lambda (x)
                                   (declare (optimize (safety 0)))
                                   (setf (info ,key1 ,key2 'grrr) x)))))
            (assert (typep (nth-value 1 (ignore-errors (funcall f sillyval)))
                           'type-error)))))
  ;; but if I *really* want, a bad value can be installed
  (set-info-value (gensym)
                  (type-info-number (type-info-or-lose :variable :kind))
                  :this-is-no-good))

(test-util:with-test (:name :unrecognize-recognized-declaration)
  (proclaim '(declaration happiness))
  (let ((saved (copy-list *recognized-declarations*)))
    (assert (member 'happiness *recognized-declarations*))
    (proclaim '(declaration happiness))
    (assert (equal *recognized-declarations* saved)) ; not pushed again
    (setf (info :declaration :recognized 'happiness) nil)
    (assert (not (member 'happiness *recognized-declarations*)))))

(test-util:with-test (:name :recognized-decl-not-also-type)
  (deftype pear (x) `(cons ,x ,x))
  (assert (typep (nth-value 1 (ignore-errors (proclaim '(declaration pear))))
                 'declaration-type-conflict-error))
  (proclaim '(declaration nthing))
  (assert (typep (nth-value 1 (ignore-errors (deftype nthing (x) `(not ,x))))
                 'declaration-type-conflict-error)))

(test-util:with-test (:name :info-env-clear)
  (let ((e (make-info-environment :name "Ben")))
    (setf (info :variable :kind 'beefsupreme) :special)
    (let ((*info-environment* (cons e *info-environment*)))
      ;; ordinarily there will not be two volatile info environments
      ;; in the list of environments, but make sure it works ok.
      (assert (eq (info :variable :kind 'beefsupreme) :special))
      (setf (info :variable :kind 'fruitbaskets) :macro
            (info :variable :macro-expansion 'fruitbaskets) 32))
    (let ((ce (compact-info-environment e))) ; compactify E
      ;; Now stick an empty volatile env in front of the compact env.
      ;; This is realistic in that it mimics an image restarted
      ;; from (save-lisp-and-die) built on top of the base core image.
      (let ((*info-environment* (list* (make-info-environment)
                                       ce (cdr *info-environment*))))
        (assert (eq (info :variable :kind 'fruitbaskets) :macro))
        (assert (eq (info :variable :macro-expansion 'fruitbaskets) 32))
        (setf (info :variable :kind 'fruitbaskets) :constant)
        (clear-info :variable :kind 'fruitbaskets)
        (multiple-value-bind (data foundp)
            (info :variable :kind 'fruitbaskets)
          (assert (and (eq data :unknown) (not foundp))))
        (multiple-value-bind (data foundp)
            (info :variable :macro-expansion 'fruitbaskets)
          (assert (and foundp (eql data 32))))
        (clear-info :variable :macro-expansion 'fruitbaskets)
        (multiple-value-bind (data foundp)
            (info :variable :macro-expansion 'fruitbaskets)
          (assert (and (not foundp) (not data))))
        (assert (every #'null (compact-info-env-entries ce)))))))

;; packed info vector tests

(test-util:with-test (:name :globaldb-info-iterate)
  (show-info '*))

(test-util:with-test (:name :find-fdefinition-agreement)
  ;; Shows that GET-INFO-VALUE agrees with FIND-FDEFINITION on all symbols,
  ;; since they use diffent code. Something would have crashed long before here...
  (flet ((try (x)
           (assert (eq (find-fdefinition x) (info :function :definition x)))))
    (do-all-symbols (s)
      (try s)
      (try `(setf ,s))
      (try `(cas ,s)))))

(defun shuffle (vector) ; destructive
  (loop for lim from (1- (length vector)) downto 0
        for chosen = (random (1+ lim))
        unless (= chosen lim)
        do (rotatef (aref vector chosen) (aref vector lim)))
  vector)

(defun crossprod (a b)
  (mapcan (lambda (x) (mapcar (lambda (y) (cons x y)) b))
          a))

;; The real GET-INFO-VALUE AVERs that TYPE-NUMBER is legal. This one doesn't.
(defun cheating-get-info-value (sym aux-key type-number)
  (let* ((vector (symbol-info-vector sym))
         (index (packed-info-value-index vector aux-key type-number)))
    (if index
        (values (svref vector index) t)
        (values nil nil))))

;; Info vectors may be concurrently updated. If more than one thread writes
;; the same name/type-number, it's random which thread prevails, but for
;; non-colliding updates, none should be lost.
;; This is not a "reasonable" use of packed info vectors.
;; It's just a check of the response of the algorithm to heavy pounding.
#+sb-thread
(test-util:with-test (:name :info-vector-concurrency)
  (let ((s (gensym))
        (a (make-array 1 :element-type 'sb-ext:word)))
    (let* ((aux-keys '(0 a b c d e f g h nil i j k l m n o p setf q r s))
           (info-types (loop for i from 1 below 64 collect i))
           (work (shuffle (coerce (crossprod aux-keys info-types) 'vector)))
           (n (floor (length work) 4))
           (threads))
      (dotimes (i 4)
        (push
         (sb-thread:make-thread
          (lambda (work my-id)
            (loop for x across work
                  do (set-info-value (if (eq (car x) 0) s `(,(car x) ,s))
                                     (cdr x)
                                     (list (atomic-incf (aref a 0)) my-id
                                           (format nil "~A,~A"
                                                       (car x) (cdr x))))))
          :arguments (list (subseq work (* i n) (if (= i 3) nil (* (1+ i) n)))
                           i))
         threads))
      (dolist (thread threads)
        (sb-thread:join-thread thread))
      (let ((foo (make-array (aref a 0))))
        ;; Returning FOO is to give a rough visual indication that
        ;; there were in fact intermingled updates.
        (dolist (k aux-keys foo)
          (dotimes (i 64)
            (let ((answer (cheating-get-info-value s k i)))
              (if answer
                  (setf (aref foo (car answer)) answer))
              (assert (equal (third answer)
                             (if (= i 0) nil
                                 (format nil "~A,~A" k i)))))))))))

;; specialized concurrent hashtable tests

(defun integer-range (min max)
  (let* ((n (1+ (- max min)))
         (a (make-array n)))
    (dotimes (j n a)
      (setf (aref a j) (+ j min)))))

(defun randomize (key random-state)
  (if random-state
      (logior (ash key 10) (random (ash 1 10) random-state))
      key)) ; not randomizing

(defun show-tally (table tally verb)
  (format t "Hashtable has ~D entries~%" (info-env-count table))
  (let ((tot 0))
    (dotimes (thread-id (length tally) tot)
      (let ((n (aref tally thread-id)))
        (format t "Thread ~2d ~A ~7d time~:P~%" thread-id verb n)
        (incf tot n)))))

(defun test-concurrent-incf (&key (table (make-info-hashtable))
                                  (n-threads 40) (n-inserts 50000))
  (declare (optimize safety))
  (let ((threads)
        (worklists (make-array n-threads))
        (failures)
        (tries (make-array n-threads :initial-element 0)))
    (dotimes (i n-threads)
      ;; Insert the integers [-n .. -2]. Keys 0 and -1 are illegal.
      (setf (aref worklists i)
            (shuffle (integer-range (- (1+ n-inserts)) -2))))
    (dotimes (i n-threads)
      (push (sb-thread:make-thread
             (lambda (worklist me)
               (declare (simple-vector worklist))
               (flet ((doer (val)
                        (incf (aref tries me))
                        (1+ (or val 0))))
                 (declare (dynamic-extent #'doer))
                 ;; for each item in worklist, increment that key
                 (loop for key across worklist do
                   (info-puthash table key #'doer))
                 ;; again backwards just for fun
                 (loop for j downfrom (1- (length worklist)) to 0 do
                   (info-puthash table (svref worklist j) #'doer))))
             :name (format nil "Worker ~D" i)
             :arguments (list (aref worklists i) i))
            threads))
    (format t "Started ~D threads doing INCF~%" n-threads)
    (dolist (thread threads)
      (sb-thread:join-thread thread))
    (assert (= (info-env-count table) n-inserts))
    (show-tally table tries "updated")
    ;; expect val[key] = 2*n-threads for all keys
    (info-maphash (lambda (k v)
                    (unless (= v (* 2 n-threads))
                      (push (cons k v) failures)))
                  table)
    (if failures
        (format t "Fail: ~S~%" failures))
    (assert (not failures))
    table))

(defun test-concurrent-consing (&key (table (make-info-hashtable))
                                (n-threads 40) (n-inserts 100000)
                                (randomize t))
  (declare (optimize safety))
  (assert (evenp n-threads))
  (let ((threads)
        (rs (make-random-state)))
    ;; Under each key, the value stored will be a list of the threads
    ;; which pushed their ID. For any pair of even/odd numbered threads,
    ;; exactly one should win the race to push its ID on behalf of the pair.
    (dotimes (i n-threads)
      (push (sb-thread:make-thread
             (lambda (me rs)
               ;; Randomizing makes keys be used up in a quasi-random
               ;; order without having to pre-compute a shuffle.
               (dotimes (i n-inserts)
                 (info-puthash
                  table (randomize (1+ i) rs)
                  (lambda (list)
                    (let ((peer (logxor me 1)))
                      (if (member peer list) list (cons me list)))))))
             :name (format nil "Worker ~D" i)
             :arguments (list i (if randomize (make-random-state rs))))
            threads))
    (format t "Started ~D threads doing CONS~%" n-threads)
    (dolist (thread threads)
      (sb-thread:join-thread thread))
    (assert (= (info-env-count table) n-inserts))
    ;; Collect the distribution of threads which inserted, for display only
    ;; since it not expected to be particularly "fair"
    (let ((tally (make-array n-threads :initial-element 0)))
      (info-maphash
       (lambda (key id-list)
         (let ((scoreboard (make-array (/ n-threads 2) :element-type 'bit)))
           (dolist (thread-id id-list)
             (let ((group-id (floor thread-id 2)))
               ;; assert no duplicate for a peer group
               (if (= (sbit scoreboard group-id) 1)
                   (error "Fail: ~S ~S~%" key id-list))
               (setf (sbit scoreboard group-id) 1)
               (incf (aref tally thread-id))))
           ;; the scoreboard should be full
           (when (find 0 scoreboard)
             (error "Fail: ~S -> ~S (~S)~%" key id-list scoreboard))))
       table)
      ;; There should be half as many puthash operations that succeeded
      ;; as the product of n-threads and n-inserts.
      (assert (= (show-tally table tally "inserted")
                 (* 1/2 n-threads n-inserts)))))
  table)

#+sb-thread
(progn
  (test-util:with-test (:name :lockfree-hash-concurrent-twiddling)
    (test-concurrent-incf))
  (test-util:with-test (:name :lockfree-hash-concurrent-consing)
    (test-concurrent-consing)))

;;; success
