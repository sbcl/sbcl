#+cheneygc (invoke-restart 'run-tests::skip-file)

(shadow "ASSERT-ERROR") ; conflict between SB-KERNEL:ASSERT-ERROR, ASSERTOID:ASSERT-ERROR
(use-package "SB-KERNEL")
(use-package "SB-THREAD")
(import 'sb-sys::(int-sap sap-int sap+ vector-sap without-gcing))
(import 'sb-int::(binding* descriptor-sap))

;;; Test out-of-memory (or something) that goes wrong in pthread_create
#+pauseless-threadstart ; no SB-THREAD::PTHREAD-CREATE symbol if not
(test-util:with-test (:name :failed-thread-creation)
  ;; This test needs to ensure that nothing is in *ALL-THREADS* to begin with.
  (sb-thread::join-pthread-joinables #'identity)
  (let ((encapsulation
          (compile nil
                   '(lambda (realfun thread stack-base)
                     (if (sb-thread:thread-ephemeral-p thread)
                         (funcall realfun thread stack-base)
                         nil))))
        (success))
    (sb-int:encapsulate 'sb-thread::pthread-create 'test encapsulation)
    (unwind-protect
         (handler-case (make-thread #'list :name "thisfails")
           (error (e)
             (setq success (search "Could not create new OS thread" (write-to-string e)))))
      (sb-int:unencapsulate 'sb-thread::pthread-create 'test))
    (assert success))
  (let ((threads sb-thread::*starting-threads*))
    (when (find-if-not #'thread-ephemeral-p threads)
      (error "Should not see new thread in starting list: ~S" threads)))
  (let ((threads (remove sb-thread::*initial-thread*
                         (sb-thread::avltree-list sb-thread::*all-threads*))))
    (when (find-if-not #'thread-ephemeral-p threads)
      (error "Should not see new thread in running list: ~S" threads))))

(defun actually-get-stack-roots (current-sp
                                 &key allwords (print t)
                                 &aux (current-sp (descriptor-sap current-sp))
                                      (roots))
  (declare (type (member nil t :everything) allwords))
  (without-gcing
    (binding* ((stack-low (get-lisp-obj-address sb-vm:*control-stack-start*))
               (stack-high (get-lisp-obj-address sb-vm:*control-stack-end*))
               ((nwords copy-from direction base)
                #+c-stack-is-control-stack ; growth direction is always down
                (values (ash (- stack-high (sap-int current-sp)) (- sb-vm:word-shift))
                        current-sp #\- "sp")
                #-c-stack-is-control-stack ; growth direction is always up
                (values (ash (- (sap-int current-sp) stack-low) (- sb-vm:word-shift))
                        (int-sap stack-low) #\+ "base"))
               (array (make-array nwords :element-type 'sb-ext:word)))
      (when print
        (format t "SP=~a~dw (range = ~x..~x)~%" direction nwords stack-low stack-high))
      (alien-funcall (extern-alien "memcpy" (function void system-area-pointer
                                                      system-area-pointer unsigned))
                     (vector-sap array) copy-from (* nwords sb-vm:n-word-bytes))
      (loop for i downfrom (1- nwords) to 0 by 1 do
        (let ((word (aref array i)))
          (when (or (/= word sb-vm:nil-value) allwords)
            (let ((baseptr (alien-funcall (extern-alien "search_all_gc_spaces" (function unsigned unsigned))
                                          word)))
              (cond ((/= baseptr 0) ; an object reference
                     (let ((obj (sb-vm::reconstitute-object (%make-lisp-obj baseptr))))
                       (when (code-component-p obj)
                         (cond
                          #+(or c-stack-is-control-stack arm64)
                          ((= (logand word sb-vm:lowtag-mask) sb-vm:fun-pointer-lowtag)
                           (dotimes (i (code-n-entries obj))
                             (when (= (get-lisp-obj-address (%code-entry-point obj i)) word)
                               (return (setq obj (%code-entry-point obj i))))))
                          #-(or c-stack-is-control-stack arm64) ; i.e. does this backend have LRAs
                          ((= (logand (sb-sys:sap-ref-word (int-sap (logandc2 word sb-vm:lowtag-mask)) 0)
                                      sb-vm:widetag-mask) sb-vm:return-pc-widetag)
                           (setq obj (%make-lisp-obj word)))))
                       ;; interior pointers to objects that contain instructions are OK,
                       ;; otherwise only correctly tagged pointers.
                       (when (or (typep obj '(or fdefn code-component funcallable-instance))
                                 (= (get-lisp-obj-address obj) word))
                         (push obj roots)
                         (when print
                           (format t "~x = ~a[~5d] = ~16x (~A) "
                                   (sap-int (sap+ copy-from (ash i sb-vm:word-shift)))
                                   base i word
                                   (or (generation-of obj) #\S)) ; S is for static
                           (let ((*print-pretty* nil))
                             (cond ((consp obj) (format t "a cons"))
                                   #+sb-fasteval
                                   ((typep obj 'sb-interpreter::sexpr) (format t "a sexpr"))
                                   ((arrayp obj) (format t "a ~s" (type-of obj)))
                                   #+c-stack-is-control-stack
                                   ((and (code-component-p obj)
                                         (>= word (sap-int (code-instructions obj))))
                                    (format t "PC in ~a" obj))
                                   (t (format t "~a" obj))))
                           (terpri)))))
                    ((and print
                          (or (eq allwords :everything) (and allwords (/= word 0))))
                     (format t "~x = ~a[~5d] = ~16x~%"
                             (sap-int (sap+ copy-from (ash i sb-vm:word-shift)))
                             base i word)))))))))
  (if print
      (format t "~D roots~%" (length roots))
      roots))
(compile 'actually-get-stack-roots)
(defun get-stack-roots (&rest rest)
  (apply #'actually-get-stack-roots (%make-lisp-obj (sap-int (current-sp))) rest))

(defstruct big-structure x)
(defstruct other-big-structure x)
(defun make-a-closure (arg options)
  (lambda (&optional (z 0) y)
    (declare (ignore y))
    (test-util:opaque-identity
     (format nil "Ahoy-hoy! ~d~%" (+ (big-structure-x arg) z)))
    (apply #'get-stack-roots options)))
(defun tryit (&rest options)
  (let ((thread
          (make-thread (make-a-closure (make-big-structure :x 0) options)
                       :arguments (list 1 (make-other-big-structure)))))
    ;; Sometimes the THREAD instance shows up in the list of objects
    ;; on the stack, sometimes it doesn't. This is annoying, but work around it.
    (remove thread (join-thread thread))))

(defun make-a-closure-nontail (arg)
  (lambda (&optional (z 0) y)
    (declare (ignore y))
    (get-stack-roots)
    (test-util:opaque-identity
     (format nil "Ahoy-hoy! ~d~%" (+ (big-structure-x arg) z)))
    1))
(defun tryit-nontail ()
  (join-thread
   (make-thread (make-a-closure-nontail (make-big-structure :x 0))
                :arguments (list 1 (make-other-big-structure)))))

;;; Test that reusing memory from an exited thread does not point to junk.
;;; In fact, assert something stronger: there are no young objects
;;; between the current SP and end of stack.
(test-util:with-test (:name :expected-gc-roots
                      :skipped-on (or :interpreter (not :pauseless-threadstart)))
  (let ((list
         (delete (sb-kernel:fun-code-header #'actually-get-stack-roots)
                 (tryit :print nil))))
    ;; should be not many things pointed to by the stack
    (assert (< (length list) #+x86    38   ; more junk, I don't know why
                             #+x86-64 30   ; less junk, I don't know why
                             #-(or x86 x86-64) 44)) ; even more junk
    ;; Either no objects are in GC generation 0, or all are, depending on
    ;; whether CORE_PAGE_GENERATION has been set to 0 for testing.
    (let ((n-objects-in-g0 (count 0 list :key #'sb-kernel:generation-of)))
      (assert (or (= n-objects-in-g0 0)
                  (= n-objects-in-g0 (length list)))))))

;; lp#1595699
(test-util:with-test (:name :start-thread-in-without-gcing
                      :skipped-on (not :pauseless-threadstart))
  (assert (eq (join-thread
               (without-gcing
                   (make-thread (lambda () 'hi))))
              'hi)))
