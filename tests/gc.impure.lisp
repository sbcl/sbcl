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
    (flet ((x ()
             (setf (aref a 0) (cons 'foo 'bar)
                   (aref a 1) (format nil "Time is: ~D~%" (get-internal-real-time))
                   (aref a 2) 'interned-symbol
                   (aref a 3) random-symbol
                   (aref a 4) 18
                   (aref a 5) (+ most-positive-fixnum 1 (random 100) (random 100))
                   (aref a 6) (make-hash-table))))
      (declare (notinline x)) ;; Leave all the values below the stack pointer for
      (x))                    ;; scrub-control-stack to work
    (assert (weak-vector-p a))
    (sb-sys:scrub-control-stack)
    (gc)
    (assert (eq (aref a 2) 'interned-symbol))
    (assert (eq (aref a 3) random-symbol))
    (assert (= (aref a 4) 18))
    ;; broken cells are the cons, string, bignum, hash-table, plus one NIL
    ;; cell that was never assigned into
    *weak-vect*))

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
                            :interpreter))
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

;; check that WITHOUT-INTERRUPTS doesn't block SIG_STOP_FOR_GC
(with-test (:name :gc-without-interrupts
            :skipped-on (not :sb-thread))
 (sb-sys:without-interrupts
   (let ((thread (sb-thread:make-thread (lambda () (sb-ext:gc)))))
     (loop while (sb-thread:thread-alive-p thread)))))

(defglobal *some-object-handles* nil)
(defun make-some-objects ()
  (declare (notinline format))
  (let* ((string-one (format nil "~a~a~a" "pot" "ayt" "o"))
         (string-two (concatenate 'string "two " string-one))
         (afunction
          (let (#+immobile-space (sb-c::*compile-to-memory-space* :dynamic))
            (compile nil `(sb-int:named-lambda ,string-two (x) (coerce x 'float))))))
    (setq *some-object-handles*
          (list (sb-kernel:get-lisp-obj-address afunction)
                (sb-kernel:get-lisp-obj-address string-one)
                (sb-kernel:get-lisp-obj-address string-two)))))
#+gencgc
(with-test (:name :pin-all-code-with-gc-enabled
                  :skipped-on :interpreter)
  #+sb-thread (sb-thread:join-thread (sb-thread:make-thread #'make-some-objects))
  #-sb-thread (progn (make-some-objects) (sb-sys:scrub-control-stack))
  (sb-sys:with-code-pages-pinned (:dynamic) (gc))
  ;; this should not fail to find FUN at its old address
  (let ((fun (sb-kernel:make-lisp-obj (first *some-object-handles*))))
    ;; this should fail to find a string at its old address
    (assert (not (nth-value 1 (sb-kernel:make-lisp-obj (second *some-object-handles*) nil))))
    ;; this should similarly fail- STRING-TWO was transitively reachable but movable
    (assert (not (nth-value 1 (sb-kernel:make-lisp-obj (third *some-object-handles*) nil))))
    (assert (string= (sb-kernel:%simple-fun-name fun) "two potayto"))))

#+immobile-space
(with-test (:name :generation-of-fdefn)
  ;; generation-of broke when fdefns stopped storing a generation in word 0
  (assert (= (sb-kernel:generation-of (sb-int:find-fdefn 'car))
             sb-vm:+pseudo-static-generation+)))

(with-test (:name :static-fdefn-space)
  (sb-int:dovector (name sb-vm:+static-fdefns+)
    (assert (eq (sb-ext:heap-allocated-p (sb-int:find-fdefn name))
                (or #+immobile-code :immobile :static)))))

;;; SB-EXT:GENERATION-* accessors returned bogus values for generation > 0
#+gencgc ; sb-ext: symbol was removed for cheneygc
(with-test (:name :bug-529014)
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
  ;; version of WITH-PINNED-OBJECTS uses special functionality that
  ;; isn't supportable outside of the compiler.  The non-x86oid
  ;; versions of WITH-PINNED-OBJECTS don't use this special
  ;; functionality, but are overridden anyway.  But the special-case
  ;; logic was, historically broken, and this affects all gencgc
  ;; targets (cheneygc isn't affected because cheneygc
  ;; WITH-PINNED-OBJECTS devolves to WITHOUT-GCING).
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
         (code-bits (make-array n-bits :element-type 'bit :initial-element 0))
         (data-bits (make-array n-bits :element-type 'bit :initial-element 0))
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
             (* (count 1 code-bits) sb-vm:gencgc-page-bytes))
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

;;; Pseudo-static large objects should retain the single-object flag
#+gencgc ; PSEUDO-STATIC-GENERATION etc don't exist for cheneygc
(with-test (:name :pseudostatic-large-objects)
  (sb-vm:map-allocated-objects
   (lambda (obj type size)
     (declare (ignore type size))
     (when (>= (sb-ext:primitive-object-size obj) (* 4 sb-vm:gencgc-page-bytes))
       (let* ((addr (sb-kernel:get-lisp-obj-address obj))
              (pte (deref sb-vm:page-table (sb-vm:find-page-index addr))))
         (when (eq (slot pte 'sb-vm::gen) sb-vm:+pseudo-static-generation+)
           (let* ((flags (slot pte 'sb-vm::flags))
                  (type (ldb (byte 5 (+ #+big-endian 3)) flags)))
             (assert (logbitp 4 type)))))))
   :all))

(with-test (:name :unique-code-serialno :skipped-on :interpreter)
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

(defun parse-address-range (line)
  ;; I hope nothing preceding the match of "-" could be a false positive.
  ;; If there is, I suspect we should parse the legend that contains
  ;;  "REGION TYPE                      START - END" to determine the column
  ;; with a #\- which appears consistently in the same place on each following line.
  (let ((separator (position #\- line)))
    (assert separator)
    (let* ((start separator))
      (loop (if (digit-char-p (char line (1- start)) 16) (decf start) (return)))
      (values (parse-integer line :start start :end separator :radix 16)
              (multiple-value-bind (value end)
                (parse-integer line :start (1+ separator) :radix 16 :junk-allowed t)
                (assert (and (> end (+ separator 3))
                             (or (= end (length line))
                                 (char= (char line end) #\space))))
                value)))))

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
                ;; Look for lines that look like
                ;; "{mumble} 7fff646c8000-7fff646ca000 {mumble}.dylib"
                (when (search ".dylib" line)
                  (multiple-value-bind (start end) (parse-address-range line)
                    (push `(,start . ,end) result))))))
      (process-wait p))
    result))

;;; Change 7143001bbe7d50c6 contained little to no rationale for why Darwin could
;;; deadlock, and how adding a WITHOUT-GCING to SAP-FOREIGN-SYMBOL fixed anything.
;;; Verify that it works fine while invoking GC in another thread
;;; despite removal of the mysterious WITHOUT-GCING.
#+sb-thread
(with-test (:name :sap-foreign-symbol-no-deadlock
                  :skipped-on :interpreter) ;; needlessly slow when interpreted
  (let* ((worker-thread
          (sb-thread:make-thread
           (lambda (ranges)
             (dolist (range ranges)
               (let ((start (car range))
                     (end (cdr range))
                     (prevsym "")
                     (nsyms 0))
                 (loop for addr from start to end by 8
                       repeat 100
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
             (loop while working do (gc) (sleep .01))))))
    (sb-thread:join-thread worker-thread)
    (setq working nil)
    (sb-thread:join-thread gc-thread)))

(defun use-up-thread-region ()
  ;; cons until the thread-local allocation buffer uses up a page
  (loop
   (let* ((c (cons 1 2))
          (end (+ (sb-kernel:get-lisp-obj-address c)
                  (- sb-vm:list-pointer-lowtag)
                  (* 2 sb-vm:n-word-bytes))))
     (when (zerop (logand end (1- sb-vm:gencgc-page-bytes)))
       (return)))))
(defglobal *go* nil)

#+sb-thread
(with-test (:name :c-call-save-p :skipped-on :interpreter)
  (let* ((fun (compile nil '(lambda (a b c d e f g h i j k l m)
                             (declare (optimize (sb-c::alien-funcall-saves-fp-and-pc 0)))
                             (setq *go* t)
                             #+win32
                             (alien-funcall (extern-alien "Sleep" (function void int))  300)
                             #-win32
                             (alien-funcall (extern-alien "sb_nanosleep" (function void int int)) 0 300000000)
                             (values a b c d e f g h i j k l m))))
         (thr (sb-thread:make-thread (lambda ()
                                       (let ((args #1=(list (LIST 'A) (LIST 'B) (LIST 'C)
                                                            (LIST 'D) (LIST 'E) (LIST 'F) (LIST 'G)
                                                            (LIST 'H) (LIST 'I) (LIST 'J) (LIST 'K)
                                                            (LIST 'L) (LIST 'M))))
                                         (use-up-thread-region)
                                         (apply fun
                                                args))))))
    (loop (sb-thread:barrier (:read))
          (if *go* (return))
          (sleep .1))
    (gc)
    (assert (equal (multiple-value-list (sb-thread:join-thread thr)) #1#))))

#+gencgc
(progn
(defun code-iterator (how)
  (let ((n 0) (tot-bytes 0))
    (sb-int:dx-flet ((visit (obj type size)
                       (declare (ignore obj))
                       (when (= type sb-vm:code-header-widetag)
                         (incf n)
                         (incf tot-bytes size))))
    (ecase how
      (:slow (sb-vm:map-allocated-objects #'visit :dynamic))
      (:fast (sb-vm::walk-dynamic-space #'visit #x7f 3 3)))
    (values n tot-bytes))))
(compile 'code-iterator)

(with-test (:name :code-iteration-fast)
  (sb-int:binding* (((slow-n slow-bytes) (code-iterator :slow))
                    ((fast-n fast-bytes) (code-iterator :fast)))
    ;; Fast should be 20x to 50x faster than slow, but that's kinda sensitive
    ;; to the machine and can't be reliably asserted.
    (assert (= slow-n fast-n))
    (assert (= slow-bytes fast-bytes)))))

(defglobal *wp-for-signal-handler-gc-test* nil)
#+(and gencgc unix sb-thread)
(with-test (:name :signal-handler-gc-test)
  (sb-thread:join-thread
   (sb-thread:make-thread
    (lambda ()
      (let ((foo (make-symbol "hey")))
        (setf *wp-for-signal-handler-gc-test* (make-weak-pointer foo))
        (sb-sys:enable-interrupt
         23
         (lambda (&rest x) (declare (ignore x)) (constantly foo)))))))
  (sb-ext:gc :gen 7)
  ;; If fullcgc fails to see the closure that is installed as a signal handler
  ;; (actually a closure around a closure) then the weak pointer won't survive.
  ;; Was broken in https://sourceforge.net/p/sbcl/sbcl/ci/04296434
  (assert (weak-pointer-value *wp-for-signal-handler-gc-test*)))

;;; We can be certain that the marked status pertains to exactly one
;;; object by ensuring that it can not share pages with other objects.
(defvar *vvv* (make-array
               (/ sb-vm:large-object-size sb-vm:n-word-bytes)))
(gc)
#+gencgc
(with-test (:name :page-protected-p :broken-on :x86)
  (if (= (sb-kernel:generation-of *vvv*) 0) (gc))
  (assert (= (sb-kernel:generation-of *vvv*) 1))
  (assert (sb-kernel:page-protected-p *vvv*))
  (let ((marks (sb-kernel:object-card-marks *vvv*)))
    (assert (not (find 1 marks))))
  (setf (svref *vvv* (/ sb-vm:gencgc-card-bytes sb-vm:n-word-bytes))
        (gensym))
  (let ((marks (sb-kernel:object-card-marks *vvv*)))
    (assert (eql (bit marks 1) 1))) ; should be marked
  (gc)
  ;; Depending whether the gensym was promoted (it's now in gen1)
  ;; the vector is or isn't marked on one of its cards.
  (let ((marks (sb-kernel:object-card-marks *vvv*)))
    (ecase (sb-kernel:generation-of
            (svref *vvv* (/ sb-vm:gencgc-card-bytes sb-vm:n-word-bytes)))
      (0
       (assert (eql (bit marks 1) 1))) ; should be marked
      (1
       (assert (eql (bit marks 1) 0))))) ; should not be marked
  (setf (svref *vvv* (/ sb-vm:gencgc-card-bytes sb-vm:n-word-bytes)) 0)
  (gc)
  (let ((marks (sb-kernel:object-card-marks *vvv*)))
    (assert (not (find 1 marks)))))
