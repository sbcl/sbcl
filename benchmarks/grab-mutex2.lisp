(in-package sb-thread)

(sb-ext:defglobal *mutex* (sb-thread:make-mutex :name "experiment"))
(declaim (type mutex *mutex*))
(sb-ext:defglobal *start-semaphore* (sb-thread:make-semaphore))
(sb-ext:defglobal *ready-semaphore* (sb-thread:make-semaphore))

(defmacro normal-with-mutex ((m) &body body)
  ;; Quoted T for wait-p causes WITH-MUTEX not to expand into WITH-ULTRAFUTEX.
  ;; obviously this is a bad hack.
  `(sb-thread:with-mutex (,m :wait-p 't) ,@body))
;; assert that preceding hack works
(let ((expansion (macroexpand '(normal-with-mutex (m) (something)))))
  (assert (eq (car expansion) 'flet)))
;; and that the new expansion happens otherwise
(let ((expansion (macroexpand-1 '(sb-thread:with-mutex (m) (something)))))
  (assert (eq (car expansion) 'sb-thread:with-ultrafutex)))

(defvar *shared-var* 0)
(defvar *count-my-work*)
(defmacro define-inline-tester (name macro-selector)
  `(defun ,name (mutex n-threads total-n-increments)
     (declare (notinline format))
     (let ((a (make-array n-threads)))
       (setq *shared-var* 0)
       (dotimes (i n-threads)
         (setf (aref a i)
               (make-thread
                (lambda (&aux (*count-my-work* 0))
                  (signal-semaphore *ready-semaphore*)
                  (wait-on-semaphore *start-semaphore*)
                  (loop
                    (,macro-selector (mutex)
                      (when (>= *shared-var* total-n-increments)
                        (return))
                      (incf *count-my-work*)
                      (incf *shared-var*)))
                  (let ((slow-grab-count
                         ;; KLUDGE: repurposing the thread slot
                         ;; to count the slow grab fallbacks
                         (sap-int (sb-vm::current-thread-offset-sap
                                  sb-vm::thread-slow-path-allocs-slot))))
                  (list *count-my-work*
                        (float (/ slow-grab-count *count-my-work*)))))
                :name (format nil "worker~d" i))))
       ;; Wait until all threads say that they're ready to run
       (wait-on-semaphore *ready-semaphore* :n n-threads)
       (let ((real-time-start (get-internal-real-time)))
         ;; Open the floodgates
         (signal-semaphore *start-semaphore* n-threads)
         (dotimes (i n-threads)
           (setf (aref a i) (join-thread (aref a i))))
         (let ((real-time-end (get-internal-real-time)))
           (assert (null (mutex-owner mutex)))
           (assert (= *shared-var* total-n-increments))
           (let ((et (- real-time-end real-time-start)))
             (format t "~&Run-time: ~D, ~A~%"
                     et
                     (if (= *shared-var* (reduce #'+ a :key #'car))
                         "OK" " ***FAIL***"))
             (format t "~&Thread work: ~D~%" a)
             et))))))

(define-inline-tester normal-way normal-with-mutex)
(define-inline-tester system-mutex-way with-system-mutex)
(define-inline-tester ultrafast-way with-ultrafutex)

(defun compute-speed-ratio (n-threads count)
  (let ((et-normal (normal-way *mutex* n-threads count))
        (et-system (system-mutex-way *mutex* n-threads count))
        (et-ultrafast (ultrafast-way *mutex* n-threads count)))
    (format t "Ratios=~F and ~F~%"
            (/ et-normal et-ultrafast)
            (/ et-system et-ultrafast))))
;;; This example shows that we can run at least 4x faster than WITH-MUTEX
;;; and 3x faster than WITH-SYSTEM-MUTEX.
;;; The parameters are: n-threads and maximum count the shared variable should attain.
;;; The output is number of times each thread incremented the variable,
;;; and total real time seconds elapseed, as well as ratios of the times.
#|
* (compute-speed-ratio 4 1000000)
Run-time: 244003, OK
Thread work: #(227928 255980 259442 256650)
Run-time: 160000, OK
Thread work: #(245476 244904 260978 248642)
Run-time: 52001, OK
Thread work: #(255598 257553 241181 245668)
Ratios=4.692275 and 3.076864
|#

(defvar *count-throws*)
(declaim (fixnum *shared-var* *count-my-work*))
(defun workfun (should-throw)
  (when should-throw
    (incf *count-throws*)
    (throw 'foo nil))
  (incf *count-my-work*)
  (incf *shared-var*))

(defmacro define-nlx-tester (name macro-selector mutex-var)
  `(defun ,name (n-threads n-seconds)
     (let ((a (make-array n-threads))
           (stopflag nil))
       (setq *shared-var* 0)
       (dotimes (i n-threads)
         (setf (aref a i)
               (make-thread
                (lambda (&aux (*count-my-work* 0) (*count-throws* 0)
                              (my-random-state (make-random-state)))
                  (signal-semaphore *ready-semaphore*)
                  (wait-on-semaphore *start-semaphore*)
                  (loop
                    ;; bail out of WORKFUN with small probability
                    ;; The random values will be the same for
                    ;; all threads since their starting states
                    ;; are the same.
                    (let ((should-nlx (< (random 10000 my-random-state) 1)))
                      (when (catch 'foo
                              (,macro-selector (,mutex-var)
                                (workfun should-nlx)
                                stopflag))
                        (return))))
                  (cons *count-my-work* *count-throws*))
                :name (format nil "worker~d" i))))
       ;; Wait until all threads say that they're ready to run
       (wait-on-semaphore *ready-semaphore* :n n-threads)
       (let ((run-time-start (get-internal-run-time)))
         ;; Open the floodgates
         (signal-semaphore *start-semaphore* n-threads)
         ;; Let them work
         (sleep n-seconds)
         (setq stopflag t)
         (barrier (:write)) ; Ensure they all see the stop flag
         (dotimes (i n-threads)
           (setf (aref a i) (join-thread (aref a i))))
         (let ((run-time-end (get-internal-run-time)))
           (format t "~&Run-time: ~D, final count=~D ~A~%"
                   (- run-time-end run-time-start)
                   *shared-var*
                   (if (= *shared-var* (reduce #'+ a :key #'car))
                       "OK" " ***FAIL***"))
           (format t "~&Thread work: ~D~%" a)
           (values))))))

(define-nlx-tester nlx-default-way   normal-with-mutex *mutex*)
(define-nlx-tester nlx-best-way      with-ultrafutex *mutex*)
;;; This comparison shows that we can run faster than WITH-MUTEX
;;; and that unwinding through a held mutex is no less safe in the ultrafutex
;;; way of doing the acquire/release.
;;; These parameters are: number of threads, seconds to run for.
;;; The output is a count of how many increments were done to a shared global.
;;; Additionally, each thread indicates how may times it incremented the
;;; global and how many times it executed a THROW out of the mutex scope.
#|
* (nlx-default-way 20 3)
Run-time: 56750341, final count=6591621 OK
Thread work: #((330583 . 25) (331136 . 25) (325310 . 24) (331700 . 25) (330879 . 25) (329571 . 25) (335072 . 26) (329791 . 25)
               (324907 . 24) (335155 . 26) (332269 . 25) (330300 . 25) (327156 . 24) (330632 . 25) (331427 . 25) (329232 . 25)
               (325618 . 24) (328507 . 24) (326585 . 24) (325791 . 24))
* (nlx-best-way 20 3)
Run-time: 58370075, final count=22720257 OK
Thpppread work: #((1194075 . 111) (1165514 . 110) (1135883 . 108) (1088051 . 97) (1175451 . 110) (1073905 . 96) (1088743 . 97)
               (1212059 . 111) (1124702 . 106) (1175308 . 110) (1206684 . 111) (1122955 . 106) (1168016 . 110) (1123304 . 106)
               (1143791 . 108) (1119477 . 103) (1132718 . 107) (1129192 . 107) (1071186 . 96) (1069243 . 96))
* (float (/ 22720257  6591621))
3.446839
|#

