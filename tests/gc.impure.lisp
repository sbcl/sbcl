;;;; gc tests

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;
;;;; This software is in the public domain and is provided with
;;;; absoluely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(in-package :cl-user)

(defvar *weak-vect* (make-weak-vector 8))
(with-test (:name :weak-vector)
  (let ((a *weak-vect*)
        (random-symbol (make-symbol "FRED")))
    (setf (aref a 0) (cons 'foo 'bar)
          (aref a 1) (format nil "Time is: ~D~%" (get-internal-real-time))
          (aref a 2) 'interned-symbol
          (aref a 3) random-symbol
          (aref a 4) 18
          (aref a 5) (+ most-positive-fixnum (random 100) (random 100))
          (aref a 6) (make-hash-table))
    (assert (typep (aref a 5) 'bignum))
    (assert (weak-vector-p a))
    (sb-sys:scrub-control-stack)
    (gc)
    (assert (eq (aref a 2) 'interned-symbol))
    (assert (eq (aref a 3) random-symbol))
    (assert (= (aref a 4) 18))
    ;; broken cells are the cons, string, bignum, hash-table, plus one NIL
    ;; cell that was never assigned into
    (assert (= (count nil *weak-vect*) 5))))

;;; Make sure MAP-REFERENCING-OBJECTS doesn't spuriously treat raw bits as
;;; potential pointers. Also make sure it sees the SYMBOL-INFO slot.
(defstruct afoo (slot nil :type sb-ext:word))
(defvar *afoo* (make-afoo :slot (sb-kernel:get-lisp-obj-address '*posix-argv*)))
(with-test (:name :map-referencing-objs)
  (sb-vm::map-referencing-objects (lambda (x) (assert (not (typep x 'afoo))))
                                  :dynamic '*posix-argv*)
  (let ((v (sb-kernel:symbol-info 'satisfies)) referers)
    (sb-vm::map-referencing-objects (lambda (referer) (push referer referers))
                                    #+gencgc :dynamic #-gencgc :static v)
    #+immobile-space
    (sb-vm::map-referencing-objects (lambda (referer) (push referer referers))
                                    :immobile v)
    (assert (member 'satisfies referers))))

;; Assert something about *CURRENT-THREAD* seeing objects that it just consed.
(with-test (:name :m-a-o-threadlocally-precise
                  :skipped-on (:or (:not (:and :gencgc :sb-thread))
                                   :interpreter))
  (let ((before (make-array 4))
        (after  (make-array 4 :initial-element 0)))
    (flet ((countit (obj type size)
             (declare (ignore type size))
             (symbol-macrolet ((n-conses     (aref after 1))
                               (n-bitvectors (aref after 2))
                               (n-symbols    (aref after 3))
                               (n-other      (aref after 0)))
               (typecase obj
                 (list       (incf n-conses))
                 (bit-vector (incf n-bitvectors))
                 (symbol     (incf n-symbols))
                 (t          (incf n-other))))))
      (sb-vm:map-allocated-objects #'countit :all)
      (replace before after)
      (fill after 0)
      ;; expect to see 1 cons, 1 bit-vector, 1 symbol, and nothing else
      (let ((* (cons (make-array 5 :element-type 'bit)
                     (make-symbol "WAT"))))
        (sb-vm:map-allocated-objects #'countit :all)
        (assert (equal (map 'list #'- after before) '(0 1 1 1)))))))

(defun count-dynamic-space-objects ()
  (let ((n 0))
    (sb-vm:map-allocated-objects
     (lambda (obj widetag size)
       (declare (ignore obj widetag size))
       (incf n))
     :dynamic)
    n))
(defun make-one-cons () (cons 'x 'y))

;;; While this does not directly test LIST-ALLOCATED-OBJECTS,
;;; it checks that L-A-O would potentially (probably) include in its
;;; output each new object allocated, barring any intervening GC.
;;; It is all but impossible to actually test L-A-O in an A/B scenario
;;; because it conses as many new cells as there were objects to begin
;;; with, plus a vector. i.e. you can't easily perform "list the objects,
;;; create one cons, list the objects, assert that there is that one
;;; cons plus exactly the previous list of objects"
;;; Counting and getting the right answer should be somewhat reassuring.
;;; This test needs dynamic-extent to work properly.
;;; (I don't know what platforms it passes on, but at least these two it does)
(with-test (:name :repeatably-count-allocated-objects
            :skipped-on (or (not (or :x86 :x86-64))
                            :interpreter)
            :fails-on (not :sb-thread))
  (let ((a (make-array 5)))
    (dotimes (i (length a))
      (setf (aref a i) (count-dynamic-space-objects))
      (make-one-cons))
    (dotimes (i (1- (length a)))
      (assert (= (aref a (1+ i)) (1+ (aref a i)))))))

(with-test (:name :list-allocated-objects)
  ;; Assert that if :COUNT is supplied as a higher number
  ;; than number of objects that exists, the output is
  ;; not COUNT many items long.
  (let ((l (sb-vm:list-allocated-objects :dynamic
                                         :count 1000
                                         :type sb-vm:weak-pointer-widetag)))
    ;; This is a change-detector unfortunately,
    ;; but seems like it'll be OK for a while.
    ;; I see only 4 weak pointers in the baseline image.
    ;; Really we could just assert /= 1000.
    (assert (< (length l) 15))))

(defparameter *x* ())

(defun cons-madly ()
  (loop repeat 10000 do
        (setq *x* (make-string 100000))))

;; check that WITHOUT-INTERRUPTS doesn't block the gc trigger
(with-test (:name :cons-madly-without-interrupts)
  (sb-sys:without-interrupts (cons-madly)))

;; check that WITHOUT-INTERRUPTS doesn't block SIG_STOP_FOR_GC
(with-test (:name :gc-without-interrupts
            :skipped-on (not :sb-thread))
 (sb-sys:without-interrupts
   (let ((thread (sb-thread:make-thread (lambda () (sb-ext:gc)))))
     (loop while (sb-thread:thread-alive-p thread)))))

(with-test (:name :without-gcing)
  (let ((gc-happend nil))
    (push (lambda () (setq gc-happend t)) sb-ext:*after-gc-hooks*)

    ;; check that WITHOUT-GCING defers explicit gc
    (sb-sys:without-gcing
      (gc)
      (assert (not gc-happend)))
    (assert gc-happend)

    ;; check that WITHOUT-GCING defers SIG_STOP_FOR_GC
    #+sb-thread
    (let ((in-without-gcing nil))
      (setq gc-happend nil)
      (sb-thread:make-thread (lambda ()
                               (loop while (not in-without-gcing))
                               (sb-ext:gc)))
      (sb-sys:without-gcing
        (setq in-without-gcing t)
        (sleep 3)
        (assert (not gc-happend)))
      ;; give the hook time to run
      (sleep 1)
      (assert gc-happend))))


#+immobile-space
(with-test (:name :generation-of-fdefn)
  (assert (= (sb-kernel:generation-of (sb-kernel::find-fdefn 'car))
             sb-vm:+pseudo-static-generation+)))

;;; SB-EXT:GENERATION-* accessors returned bogus values for generation > 0
(with-test (:name :bug-529014 :skipped-on (not :gencgc))
  (loop for i from 0 to sb-vm:+pseudo-static-generation+
     do (assert (= (sb-ext:generation-bytes-consed-between-gcs i)
                   (truncate (sb-ext:bytes-consed-between-gcs)
                             sb-vm:+highest-normal-generation+)))
        ;; FIXME: These parameters are a) tunable in the source and b)
        ;; duplicated multiple times there and now here.  It would be good to
        ;; OAOO-ify them (probably to src/compiler/generic/params.lisp).
        (assert (= (sb-ext:generation-minimum-age-before-gc i) 0.75))
        (assert (= (sb-ext:generation-number-of-gcs-before-promotion i) 1))))

(with-test (:name :gc-logfile :skipped-on (not :gencgc))
  (assert (not (gc-logfile)))
  (let ((p (scratch-file-name "log")))
    (assert (not (probe-file p)))
    (assert (equal p (setf (gc-logfile) p)))
    (gc)
    (let ((p2 (gc-logfile)))
      (assert (equal (truename p2) (truename p))))
    (assert (not (setf (gc-logfile) nil)))
    (assert (not (gc-logfile)))
    (delete-file p)))

#+nil ; immobile-code
(with-test (:name (sb-kernel::order-by-in-degree :uninterned-function-names))
  ;; This creates two functions whose names are uninterned symbols and
  ;; that are both referenced once, resulting in a tie
  ;; w.r.t. ORDER-BY-IN-DEGREE. Uninterned symbols used to cause an
  ;; error in the tie-breaker.
  (let* ((sb-c::*compile-to-memory-space* :immobile)
         (f (eval `(defun ,(gensym) ())))
         (g (eval `(defun ,(gensym) ()))))
    (eval `(defun h () (,f) (,g))))
  (sb-kernel::order-by-in-degree))

(defparameter *pin-test-object* nil)
(defparameter *pin-test-object-address* nil)

(with-test (:name (sb-sys:with-pinned-objects :actually-pins-objects)
                  :skipped-on :cheneygc)
  ;; The interpreters (both sb-eval and sb-fasteval) special-case
  ;; WITH-PINNED-OBJECTS as a "special form", because the x86oid
  ;; version of WITH-PINNED-OBJECTS uses black magic that isn't
  ;; supportable outside of the compiler.  The non-x86oid versions of
  ;; WITH-PINNED-OBJECTS don't use black magic, but are overridden
  ;; anyway.  But the special-case logic was, historically broken, and
  ;; this affects all gencgc targets (cheneygc isn't affected because
  ;; cheneygc WITH-PINNED-OBJECTS devolves to WITHOUT-GC>ING).
  ;;
  ;; Our basic approach is to allocate some kind of object and stuff
  ;; it where it doesn't need to be on the control stack.  We then pin
  ;; the object, take its address and store that somewhere as well,
  ;; force a full GC, re-take the address, and see if it moved.
  (locally (declare (notinline make-string)) ;; force full call
    (setf *pin-test-object* (make-string 100)))
  (sb-sys:with-pinned-objects (*pin-test-object*)
    (setf *pin-test-object-address*
          (sb-kernel:get-lisp-obj-address *pin-test-object*))
    (gc :full t)
    (assert (= (sb-kernel:get-lisp-obj-address *pin-test-object*)
               *pin-test-object-address*))))

#+gencgc
(defun ensure-code/data-separation ()
  (let* ((n-bits (+ sb-vm:next-free-page 10))
         (code-bits (make-array n-bits :element-type 'bit))
         (data-bits (make-array n-bits :element-type 'bit))
         (total-code-size 0))
    (sb-vm:map-allocated-objects
     (lambda (obj type size)
       (declare ((and fixnum (integer 1)) size))
       ;; M-A-O disables GC, therefore GET-LISP-OBJ-ADDRESS is safe
       (let ((obj-addr (sb-kernel:get-lisp-obj-address obj))
             (array (cond ((= type sb-vm:code-header-widetag)
                           (incf total-code-size size)
                           code-bits)
                          (t
                           data-bits))))
         ;; This is not the most efficient way to update the bit arrays,
         ;; but the simplest and clearest for sure. (The loop could avoided
         ;; if the current page is the same as the previously seen page)
         (loop for index from (sb-vm::find-page-index obj-addr)
               to (sb-vm::find-page-index (truly-the word
                                                     (+ (logandc2 obj-addr sb-vm:lowtag-mask)
                                                        (1- size))))
               do (setf (sbit array index) 1))))
     :dynamic)
    (assert (not (find 1 (bit-and code-bits data-bits))))
    (let* ((code-bytes-consumed
             (* (count 1 code-bits) sb-vm:gencgc-card-bytes))
           (waste
             (- total-code-size code-bytes-consumed)))
      ;; This should be true for all platforms.
      ;; Some have as little as .5% space wasted.
      (assert (<= waste (* 3/100 code-bytes-consumed))))))



(with-test (:name :code/data-separation
            :skipped-on (not :gencgc))
  (compile 'ensure-code/data-separation)
  (ensure-code/data-separation))

#+immobile-space
(with-test (:name :immobile-space-addr-p)
  ;; Upper bound should be exclusive
  (assert (not (sb-kernel:immobile-space-addr-p
                (+ sb-vm:fixedobj-space-start
                   sb-vm:fixedobj-space-size
                   sb-vm:varyobj-space-size)))))

;;; After each iteration of FOO there are a few pinned conses.
;;; On alternate GC cycles, those get promoted to generation 1.
;;; When the logic for page-spanning-object zeroing incorrectly decreased
;;; the upper bound on bytes used for partially pinned pages, it caused
;;; an accumulation of pages in generation 1 each with 2 objects' worth
;;; of bytes, and the remainder waste. Because the waste was not accounted
;;; for, it did not trigger GC enough to avoid heap exhaustion.
(with-test (:name :smallobj-auto-gc-trigger)
  ;; Ensure that these are compiled functions because the interpreter
  ;; would make lots of objects of various sizes which is insufficient
  ;; to provoke the bug.
  (setf (symbol-function 'foo)
        (compile nil '(lambda () (list 1 2))))
  ;; 500 million iterations of this loop seems to be reliable enough
  ;; to show that GC happens.
  (setf (symbol-function 'callfoo)
        (compile nil '(lambda () (loop repeat 500000000 do (foo)))))
  (funcall 'callfoo))

;;; Pseudo-static large objects should retain the single-object flag
#+gencgc ; PSEUDO-STATIC-GENERATION etc don't exist for cheneygc
(with-test (:name :pseudostatic-large-objects)
  (sb-vm:map-allocated-objects
   (lambda (obj type size)
     (declare (ignore type size))
     (when (>= (sb-vm::primitive-object-size obj) (* 4 sb-vm:gencgc-card-bytes))
       (let* ((addr (sb-kernel:get-lisp-obj-address obj))
              (pte (deref sb-vm:page-table (sb-vm:find-page-index addr))))
         (when (eq (slot pte 'sb-vm::gen) sb-vm:+pseudo-static-generation+)
           (let* ((flags (slot pte 'sb-vm::flags))
                  (type (ldb (byte 5 (+ #+big-endian 3)) flags)))
             (assert (logbitp 4 type)))))))
   :all))

#+64-bit ; code-serialno not defined unless 64-bit
(with-test (:name :unique-code-serialno)
  (let ((a (make-array 100000 :element-type 'bit :initial-element 0)))
    (sb-vm:map-allocated-objects
     (lambda (obj type size)
       (declare (ignore size))
       (when (and (= type sb-vm:code-header-widetag)
                  (plusp (sb-kernel:code-n-entries obj)))
         (let ((serial (sb-kernel:%code-serialno obj)))
           (assert (zerop (aref a serial)))
           (setf (aref a serial) 1))))
     :all)))

(defvar *foo*)
#+gencgc
(with-test (:name (sb-ext:search-roots :simple-fun))
  ;; Tracing a path to a simple fun wasn't working at some point
  ;; because of failure to employ fun_code_header in the right place.
  (setq *foo* (compile nil '(lambda () 42)))
  (let ((wp (sb-ext:make-weak-pointer *foo*)))
    (assert (sb-ext:search-roots wp :criterion :oldest :print nil))))

#+gencgc
(with-test (:name (sb-ext:search-roots :ignore-immediate))
  (sb-ext:search-roots (make-weak-pointer 48) :gc t :print nil))

#+sb-thread
(with-test (:name :concurrently-alloc-code)
  (let ((gc-thread
         (sb-thread:make-thread
          (let ((stop (+ (get-internal-real-time)
                         internal-time-units-per-second)))
            (lambda ()
              (loop while (<= (get-internal-real-time) stop)
                    do (gc) (sleep 0)))))))
    (loop (compile nil `(lambda () (print 20)))
          (unless (sb-thread:thread-alive-p gc-thread)
            (return)))
    (sb-thread:join-thread gc-thread)))

(defun get-shared-library-maps ()
  (let (result)
    #+linux
    (with-open-file (f "/proc/self/maps")
      (loop (let ((line (read-line f nil)))
              (unless line (return))
              (when (and (search "r-xp" line) (search ".so" line))
                (let ((p (position #\- line)))
                  (let ((start (parse-integer line :end p :radix 16))
                        (end (parse-integer line :start (1+ p) :radix 16
                                                 :junk-allowed t)))
                    (push `(,start . ,end) result)))))))
    #+darwin
    (let ((p (run-program "/usr/bin/vmmap" (list (write-to-string (sb-unix:unix-getpid)))
                          :output :stream
                          :wait nil)))
      (with-open-stream (s (process-output p))
        (loop (let ((line (read-line s)))
                (when (search "regions for" line) (return))))
        (assert (search "REGION TYPE" (read-line s)))
        (loop (let ((line (read-line s)))
                (when (zerop (length line)) (return))
                (when (search ".dylib" line)
                  (let ((c (search "00-00" line)))
                    (assert c)
                    (let ((start (parse-integer line :start (+ c 2 (- 16)) :radix 16
                                                     :junk-allowed t))
                          (end (parse-integer line :start (+ c 3) :radix 16
                                                   :junk-allowed t)))
                      (push `(,start . ,end) result)))))))
      (process-wait p))
    result))

;;; Change 7143001bbe7d50c6 contained little to no rationale for why Darwin could
;;; deadlock, and how adding a WITHOUT-GCING to SAP-FOREIGN-SYMBOL fixed anything.
;;; Verify that it works fine while invoking GC in another thread
;;; despite removal of the mysterious WITHOUT-GCING.
#+sb-thread
(with-test (:name :sap-foreign-symbol-no-deadlock)
  (let* ((worker-thread
          (sb-thread:make-thread
           (lambda (ranges)
             (dolist (range ranges)
               (let ((start (car range))
                     (end (cdr range))
                     (prevsym "")
                     (nsyms 0))
                 (loop for addr from start to end by 8
                       do (let ((sym (sb-sys:sap-foreign-symbol (sb-sys:int-sap addr))))
                            (when (and sym (string/= sym prevsym))
                              (incf nsyms)
                              (setq prevsym sym))))
                 #+nil (format t "~x ~x: ~d~%" start end nsyms))))
           :arguments (list (get-shared-library-maps))))
         (working t)
         (gc-thread
          (sb-thread:make-thread
           (lambda ()
             (loop while working do (gc) (sleep .001))))))
    (sb-thread:join-thread worker-thread)
    (setq working nil)
    (sb-thread:join-thread gc-thread)))
