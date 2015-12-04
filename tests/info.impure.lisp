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

(test-util:with-test (:name :no-meta-info)
 (assert-signal (compile nil '(lambda (x) (sb-int:info :type :nokind x)))
                style-warning))

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
   (format nil "~A" (sb-int:proclaimed-ftype 'foo))
   "#<FUN-TYPE (FUNCTION (T T) LIST)>"))
||#

(with-test (:name :fboundp-type-error)
  (assert-error (funcall (compile nil `(lambda (x) (fboundp x))) 0)
                 type-error)
  (assert-error (funcall (compile nil `(lambda (x) (fdefinition x))) 0)
                 type-error))

(in-package "SB-C")

(test-util:with-test (:name :globaldb-sxhashoid-discrimination)
  (assert (not (eql (globaldb-sxhashoid '(a b c d e))
                    (globaldb-sxhashoid '(a b c d mumble))))))

(test-util:with-test (:name :bug-458015)
  ;; Make sure layouts have sane source-locations
  (sb-c::call-with-each-globaldb-name
   (lambda (info-name)
     (when (and (symbolp info-name) (info :type :kind info-name))
        (let* ((classoid (find-classoid info-name nil))
               (layout (and classoid (classoid-layout classoid)))
               (srcloc (and layout (sb-kernel::layout-source-location layout))))
          (when (and layout)
            (assert (or (definition-source-location-p srcloc)
                        (null srcloc)))))))))

(test-util:with-test (:name :find-classoid-signal-error)
  ;; (EVAL ''SILLY) dumbs down the compiler for this test.
  ;; FIND-CLASSOID on a constant symbol becomes
  ;;  `(CLASSOID-CELL-CLASSOID ',(FIND-CLASSOID-CELL name :create t))
  ;; and I want just the primitive operations without muddying the water.
  (let ((name (eval ''silly)))
    (assert (not (find-classoid name nil)))
    (assert (typep (handler-case (find-classoid name) (error (e) e)) 'error))
    (find-classoid-cell name :create t) ; After this, have cell but no classoid
    (assert (typep (handler-case (find-classoid name) (error (e) e)) 'error))))

(test-util:with-test (:name :set-info-value-type-check)
  (loop for type-info across *info-types*
        when (and type-info (not (eq (meta-info-type-spec type-info) 't)))
        do
        (let ((key1 (meta-info-category type-info))
              (key2 (meta-info-kind type-info))
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
                  (meta-info-number (meta-info :variable :kind))
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
  (setf (info :variable :kind 'fruitbaskets) :macro
        (info :variable :macro-expansion 'fruitbaskets) 32)
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
    (assert (and (not foundp) (not data)))))

;; packed info vector tests

(test-util:with-test (:name :globaldb-info-iterate)
  (let ((s (with-output-to-string (*standard-output*) (show-info '*))))
    (dolist (x '((:function :definition) (:function :type)
                 (:function :where-from) (:function :kind)
                 (:function :info) (:function :source-transform)
                 (:type :kind) (:type :builtin)
                 (:source-location :declaration)
                 (:variable :kind)
                 #+sb-doc (:variable :documentation)
                 (:variable :type) (:variable :where-from)
                 (:source-location :variable)
                 (:alien-type :kind) (:alien-type :translator)))
      (assert (search (format nil "~S ~S" (car x) (cadr x)) s)))))

(test-util:with-test (:name :find-fdefn-agreement)
  ;; Shows that GET-INFO-VALUE agrees with FIND-FDEFN on all symbols,
  ;; since they use diffent code. Something would have crashed long before here...
  (flet ((try (x)
           (assert (eq (find-fdefn x) (info :function :definition x)))))
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

(test-util:with-test (:name :quick-packed-info-insert)
  ;; Exercise some bit patterns that touch the sign bit on 32-bit machines.
  (loop repeat 10
        do
    (let ((iv1 +nil-packed-infos+)
          (iv2 +nil-packed-infos+)
          (type-nums
           (cons 1 (subseq '(#b100000 #b110000 #b010000 #b011000
                             #b000100 #b000010 #b000011 #b000001)
                           0 (- +infos-per-word+ 2)))))
      ;; Randomize because maybe there's an ordering constraint more
      ;; complicated than fdefn always getting to be first.
      ;; (there isn't, but could be)
      (dolist (num (coerce (shuffle (coerce type-nums 'vector)) 'list))
        (let ((val (format nil "value for ~D" num)))
          (setq iv1 (quick-packed-info-insert iv1 num val)
                iv2 (%packed-info-insert ; not PACKED-INFO-INSERT
                     iv2 +no-auxilliary-key+ num val))
          (assert (equalp iv1 iv2))))
      ;; the first and only info descriptor should be full
      (assert (not (info-quickly-insertable-p iv1))))))

(defun crossprod (a b)
  (mapcan (lambda (x) (mapcar (lambda (y) (cons x y)) b))
          a))

;; The real GET-INFO-VALUE AVERs that INFO-NUMBER is legal. This one doesn't.
(defun cheating-get-info-value (sym aux-key info-number)
  (let* ((vector (symbol-info-vector sym))
         (index (packed-info-value-index vector aux-key info-number)))
    (if index
        (values (svref vector index) t)
        (values nil nil))))

;; Info vectors may be concurrently updated. If more than one thread writes
;; the same name/info-number, it's random which thread prevails, but for
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

(defun show-tally (table tally verb print)
  (when print
    (format t "Hashtable has ~D entries~%" (info-env-count table)))
  (let ((tot 0))
    (dotimes (thread-id (length tally) tot)
      (let ((n (aref tally thread-id)))
        (when print
          (format t "Thread ~2d ~A ~7d time~:P~%" thread-id verb n))
        (incf tot n)))))

(defun test-concurrent-incf (&key (table (make-info-hashtable))
                                  (n-threads 40) (n-inserts 50000)
                                  (print nil))
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
    (when print (format t "Started ~D threads doing INCF~%" n-threads))
    (dolist (thread threads)
      (sb-thread:join-thread thread))
    (assert (= (info-env-count table) n-inserts))
    (show-tally table tries "updated" print)
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
                                (randomize t) (print nil))
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
    (when print (format t "Started ~D threads doing CONS~%" n-threads))
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
      (assert (= (show-tally table tally "inserted" print)
                 (* 1/2 n-threads n-inserts)))))
  table)

#+sb-thread
(progn
  (test-util:with-test (:name :lockfree-hash-concurrent-twiddling)
    (test-concurrent-incf))
  (test-util:with-test (:name :lockfree-hash-concurrent-consing)
    (test-concurrent-consing)))

;; classoid cells

(in-package "SB-IMPL")

(defglobal *make-classoid-cell-callcount* (make-array 1 :element-type 'sb-ext:word))
(defglobal *really-make-classoid-cell* #'sb-kernel::make-classoid-cell)
(without-package-locks
  (defun sb-kernel::make-classoid-cell (name &optional classoid)
    (sb-ext:atomic-incf (aref *make-classoid-cell-callcount* 0))
    (funcall *really-make-classoid-cell* name classoid)))

;; Return a set of symbols to play around with
(defun classoid-cell-test-get-lotsa-symbols ()
  (remove-if-not
   #'symbolp
   (package-hashtable-cells
    (package-internal-symbols (find-package "SB-C")))))

;; Make every symbol in the test set have a classoid-cell
(defun be-a-classoid-cell-writer ()
  (let* ((symbols (classoid-cell-test-get-lotsa-symbols))
         (result (make-array (length symbols) :initial-element nil)))
    (loop for s across symbols
          for i from 0
          do (setf (aref result i) (find-classoid-cell s :create t)))
    result))

;; Get the classoid-cells
(defun be-a-classoid-cell-reader ()
  (let* ((symbols (classoid-cell-test-get-lotsa-symbols))
         (result (make-array (length symbols) :initial-element nil)))
    (dotimes (iter 3)
      (loop for i below (length symbols)
            do (pushnew (find-classoid-cell (svref symbols i))
                        (svref result i))))
    ;; The thread shall have observed at most two different values
    ;; for FIND-CLASSOID-CELL - nil and/or a CLASSOID-CELL.
    ;; For each symbol, if the thread observed a classoid cell, store that.
    (loop for list across result
          for i from 0
          do (let ((observed-value (remove nil list)))
               (if (cdr observed-value)
                   (error "Should not happen: find-classoid-cell => ~S" list)
                   (setf (svref result i) (car observed-value)))))
    result))

;; Perform some silly updates to plists, because they mess with
;; the symbol-info slot alongside globaldb writers.
(defun be-a-plist-writer ()
  (loop for s across (classoid-cell-test-get-lotsa-symbols)
        do
       (loop (let ((old (symbol-plist s)))
               (when (or (member 'foo old)
                         (eq (cas (symbol-plist s) old (list* 'foo s old)) old))
                 (return))))))

#+sb-thread
(test-util:with-test (:name :info-vector-classoid-cell)
  (let (readers writers more-threads results)
    (dotimes (i 5)
      (push (sb-thread:make-thread #'be-a-classoid-cell-writer) writers))
    (dotimes (i 5)
      (push (sb-thread:make-thread #'be-a-classoid-cell-reader) readers)
      (push (sb-thread:make-thread #'be-a-plist-writer) more-threads))
    (mapc #'sb-thread:join-thread more-threads)
    (dolist (thread (append readers writers))
      (push (sb-thread:join-thread thread) results))
    (let ((result-vect (make-array 10)))
      (loop for i below (length (first results))
            do
           (dotimes (thread-num 10)
             (setf (aref result-vect thread-num)
                   (aref (nth thread-num results) i)))
           ;; some thread should have observed a classoid-cell
           (let ((representative (find-if-not #'null result-vect)))
             ;; For each thread which observed a classoid-cell,
             ;; assert that the cell is EQ to the representative.
             (dotimes (thread-num 10)
               (let ((cell (aref result-vect thread-num)))
                 (if cell
                     (assert (eq cell representative))))))))
    ;; and make sure the property list updates also weren't lost
    (let ((symbols (classoid-cell-test-get-lotsa-symbols)))
      (loop for s across symbols
         do (assert (eq (get s 'foo) s)))
      ;; a statistic of no real merit, but verifies that
      ;; the lockfree logic does discard some created objects.
      (format t "Consed ~D classoid-cells (~D symbols)~%"
              (aref *make-classoid-cell-callcount* 0)
              (length symbols)))))

;;; test %GET-INFO-VALUE-INITIALIZING using generalized function names

(defun be-an-fdefn-reader (names)
  (declare (simple-vector names))
  (let ((result (make-array (length names) :initial-element nil)))
    (dotimes (iter 3)
      (loop for i below (length names)
            do (pushnew (find-fdefn (aref names i)) (aref result i))))
    ;; The thread shall observe either nil or an fdefn, and at most one fdefn.
    (loop for list across result
          for i from 0
          do (let ((observed-value (remove nil list)))
               (if (cdr observed-value)
                   (error "Should not happen: fdefn => ~S" list)
                   (setf (aref result i) (car observed-value)))))
    result))

(defun be-an-fdefn-writer (names)
  (declare (simple-vector names))
  (let ((fdefn-result (make-array (length names) :initial-element nil))
        (random-result (make-array (length names) :initial-element nil))
        (n-created 0)
        (highest-type-num
         (position-if #'identity sb-c::*info-types*
                      :end sb-int:+fdefn-info-num+ :from-end t)))
    (loop for name across names
          for i from 0
          do (setf (aref fdefn-result i)
                   (get-info-value-initializing
                    :function :definition name
                    (progn (incf n-created) (make-fdefn name))))
             (dotimes (i (random 3))
               ;; Set random info for other names to cause CAS failures.
               ;; Pick an info-type number and give it a random value.
               ;; Store the random value so that we can assert on it later.
               ;; Never touch reserved type numbers 0 or 63.
               (let ((random-name-index (random (length names)))
                     (random-type (+ (random (1- highest-type-num)) 2))
                     (random-value (random most-positive-fixnum)))
                 (push (cons random-type random-value)
                       (aref random-result random-name-index))
                 (sb-c::set-info-value (aref names random-name-index)
                                       random-type random-value))))
    (values n-created fdefn-result random-result)))

(test-util:with-test (:name :get-info-value-initializing
                      :skipped-on '(not :sb-thread))
  ;; Precompute random generalized function names for testing, some of which
  ;; are "simple" (per the taxonomy of globaldb) and some hairy.
  (let ((work (coerce (loop repeat 10000
                            nconc (list `(sb-pcl::ctor ,(gensym) ,(gensym))
                                        `(defmacro ,(gensym)) ; simple name
                                         (gensym))) ; very simple name
                      'vector))
        (n-threads 10) readers writers fdefn-results random-results)
    (dotimes (i (ash n-threads -1))
      (push (sb-thread:make-thread
             #'be-an-fdefn-writer :arguments (list work)
                                  :name (write-to-string i)) writers))
    (dotimes (i (ash n-threads -1))
      (push (sb-thread:make-thread #'be-an-fdefn-reader :arguments (list work))
            readers))
    (dolist (thread readers)
      (push (sb-thread:join-thread thread) fdefn-results))
    (let ((tot 0))
      (dolist (thread writers)
        (multiple-value-bind (n-created fdefn-result random-result)
            (sb-thread:join-thread thread)
          (incf tot n-created)
          (format t "~5D fdefns from ~A~%" n-created
                  (sb-thread:thread-name thread))
          (push fdefn-result fdefn-results)
          (push random-result random-results)))
      (format t "~5D total~%" tot))
    (let ((aggregate (make-array n-threads)))
      (dotimes (name-index (length work))
        (dotimes (thread-num n-threads)
          (setf (aref aggregate thread-num)
                (aref (nth thread-num fdefn-results) name-index)))
        ;; some thread should have observed an fdefn
        (let ((representative (find-if-not #'null aggregate)))
          ;; For each thread which observed an fdefn,
          ;; assert that the cell is EQ to the representative.
          (dotimes (thread-num n-threads)
            (awhen (aref aggregate thread-num)
              (assert (eq it representative)))))))
    ;; For each name and each info type number that some thread inserted,
    ;; verify that the info-value is among the set of random values.
    (dotimes (name-index (length work))
      (dotimes (type-num 64)
        ;; some thread says that TYPE-NUM exists for NAME-INDEX
        (when (some (lambda (output)
                      (assoc type-num (aref output name-index)))
                    random-results)
          (let ((actual (sb-c::get-info-value (aref work name-index)
                                              type-num)))
            (unless (some (lambda (output)
                            (some (lambda (cell)
                                    (and (eq (car cell) type-num)
                                         (eql (cdr cell) actual)))
                                  (aref output name-index)))
                          random-results)
              (error "Fail ~S ~S => ~S.~%Choices are ~S"
                     (aref work name-index) type-num actual
                     (mapcar (lambda (output)
                               (aref output name-index))
                             random-results)))))))))

;; As explained in the comments at the top of 'info-vector.lisp',
;; it is a bad idea to use globaldb to store an atomic counter as
;; a piece of info for a name, as it is quite brutal and consy,
;; but for this test, that's precisely the goal.
;; This test conses ~5 Megabytes on 64-bit almost entirely due
;; to allocation of each immutable info storage vector.
(test-util:with-test (:name :get-info-value-updating
                      :skipped-on '(not :sb-thread))
  (flet ((run (names)
           (declare (simple-vector names))
           (let* ((n (length names))
                  (counts (make-array n :element-type 'sb-ext:word))
                  (threads))
             (dotimes (i 15)
               (push (sb-thread:make-thread
                      (lambda ()
                        (dotimes (iter 1000)
                          ;; increment (:variable :macro-expansion)
                          ;; for a randomly chosen name. That particular
                          ;; info-type harmlessly accepts any data type.
                          (let* ((index (random n))
                                 (name (aref names index)))
                            (atomic-incf (aref counts index))
                            ;; should probably be SB-INT:
                            (sb-c::atomic-set-info-value
                             :variable :macro-expansion name
                             (lambda (old old-p)
                               (if old-p (1+ old) 1))))
                          ;; randomly touch an item of info
                          ;; for another (or the same) name.
                          (let* ((index (random n))
                                 (name (aref names index)))
                            ;; source-location also accepts anything :-(
                            (setf (info :type :source-location name) iter)))))
                     threads))
             (mapc #'sb-thread:join-thread threads)
             ;; assert that no updates were lost
             (loop for name across names
                   for count across counts
                   for val = (info :variable :macro-expansion name)
                   do (assert (eql (or val 0) count))))))
    ;; Try it when names are symbols or "simple" 2-list names
    (run (coerce (loop repeat 50
                       for sym = (gensym)
                       nconc (list `(setf ,sym) sym))
                 'vector))
    ;; For hairy names, the tricky piece is in the rehash algorithm,
    ;; but there's no way to stress-test that because *INFO-ENVIRONMENT*
    ;; would have to keep doubling in size. To that end, it would have to begin
    ;; as a tiny table again, but it can't, without destroying the Lisp runtime.
    ;; The :lockfree-hash-concurrent-twiddling test should give high confidence
    ;; that it works, by creating and testing a standalone hash-table.
    (run (coerce (loop repeat 50 collect `(foo ,(gensym) hair)) 'vector))))

;;; success
