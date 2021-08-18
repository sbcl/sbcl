;;; This is a stress test of multi-thread garbage collection without
;;; introducing contrived factors into the system such as extra debugging
;;; or manually invoked GC.

#|

NB: You need to compile with #+sb-devel
so that compiling "src/compiler/srctran" does not fail quickly.

Legend: One line per statistic, measured in microsec. One column per thread.
The statistics:
 1. worst observed GC wait time per thread
 2. average GC wait time per thread
 3. total CPU time per thread

./run-sbcl.sh --dynamic-space-size 4GB
* (load "src/cold/chill")
* (load (compile-file "benchmarks/threads-compile"))

* (benchmark 4 4) ; 4 threads, 4 iterations each
;; worst-case stop-the-world pause = .528 sec
 [  161289   528922   528972   528953]
 [   67780   100901    93856    99925]
 [ 4342694  3875017  4088501  3992092]

* (benchmark 20 6) ; 20 threads, 6 iterations each
;; worst-case stop-the-world pause = .905 sec
;; but I've seen this parameter pair produce as much as 1.38 sec worst-case pause
 [  853493   905262   904932   904812   904660   904480   905302   904205   904040   904363   905253   905290   905231   903957   903644   903827   903432   903272   903607   905291]
 [   95906    98105    96955    96992   100130    98969    99631    96186    98073    96748    97830    97861    94608    94574    97282    95638    97308    96941    97169    95195]
 [ 8638099  7620908  8408132  7783041  7411906  7616439  7550742  7881625  8042876  7627665  7090403  7322993  8996690  8231693  7415837  8477329  7745566  8130082  7640412  7891094]

* (benchmark 30 4) ; 30 threads, 4 iterations each
;; worst-case stop-the-world pause = 1.59 sec
 [ 1589254  1589235  1589236  1589246  1589200  1589244  1589293  1589249  1589258  1589260  1589195  1589517  1589541  1589267  1589454  1589577  1589311  1589311  1589420  1589658  1589638  1589322  1589302  1589262  1426929  1589448  1589644  1589307  1589492  1589577]
 [  131124   133234   134216   132862   134032   133074   131811   133394   134221   133830   133337   135129   133034   131109   133957   130416   128010   133089   128650   131075   134138   133200   130342   132036   126419   133778   132877   135274   132027   132272]
 [ 6463084  5699894  6391162  5323400  5510025  5425688  6288613  4886611  5456971  5394043  5564274  5639621  5054329  5722550  5208487  5986264  6858847  5267559  7030543  5811645  5656792  5012832  6000738  5682139  7220169  6433044  5468151  5295718  5333045  5908446]
|#

(defparameter *gcmetrics-condvar*
  (sb-sys:find-dynamic-foreign-symbol-address "gcmetrics_condvar"))
(defparameter *gcmetrics-mutex*
  (sb-sys:find-dynamic-foreign-symbol-address "gcmetrics_mutex"))

(define-alien-routine pthread-mutex-lock int (m unsigned))
(define-alien-routine pthread-mutex-unlock int (m unsigned))
(define-alien-routine pthread-cond-wait int (cv unsigned) (m unsigned))
(define-alien-routine pthread-cond-broadcast int (cv unsigned))

(defun thread-gcmetrics (thread)
  (sb-thread:with-deathlok (thread c-thread)
    (if (= c-thread 0)
        (values nil nil nil)
        (let ((sap
               (sb-sys:sap+ (sb-sys:int-sap c-thread)
                           (+ (sb-alien:extern-alien "dynamic_values_bytes" (sb-alien:unsigned 32))
                              (* 8 8))))) ; interrupt context pointers
          (values (sb-sys:sap-ref-64 sap 8) ; avg
                  (sb-sys:sap-ref-64 sap 16) ; worst
                  (sb-sys:sap-ref-64 sap 0)))))) ; runtime

;;; Exercise COMPILE-FILE in many threads, which is representative of a
;;; lispy workload.  Any suitable workload should do.
;;; It would be better to have each thread doing a different kind of work,
;;; but I took the easy route.
(defun gc-benchmark (n-threads n-iter)
  (let (threads
        (running (make-array n-threads :initial-element t))
        (avg-gc-wait (make-array n-threads))
        (worst-gc-wait (make-array n-threads))
        (runtime (make-array n-threads)))
    (flet ((work (arg)
             (with-open-file (*standard-output*
                              (format nil "/tmp/foo~d.stdout" arg)
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
               (let ((*error-output* *standard-output*))
                 (dotimes (i n-iter)
                   (let ((file (format nil "/tmp/foo~d.fasl" arg)))
                     (compile-file "src/compiler/srctran"
                                   :print nil
                                   :output-file file)))))
             (setf (aref running arg) nil)
             (pthread-cond-broadcast *gcmetrics-condvar*)))
    (dotimes (i n-threads)
      (push (sb-thread:make-thread
             #'work
             :name (format nil "worker~d" i)
             :arguments i)
            threads)))
    (let ((start (get-internal-real-time)))
      (assert (= 0 (pthread-mutex-lock *gcmetrics-mutex*)))
      (loop
        (let ((count (count t running)))
          (when (zerop count) (return))
          (assert (= 0 (pthread-cond-wait *gcmetrics-condvar* *gcmetrics-mutex*)))
          (let ((i 0))
            (dolist (thread threads)
              (multiple-value-bind (avg worst run) (thread-gcmetrics thread)
                (when avg
                  (setf (aref avg-gc-wait i) avg
                        (aref worst-gc-wait i) worst
                        (aref runtime i) run)))
              (incf i)))
          (format t "~D threads:~% [~{~8d~^ ~}]~% [~{~8d~^ ~}]~% [~{~8d~^ ~}]~%"
                  count
                  (coerce worst-gc-wait 'list)
                  (coerce avg-gc-wait 'list)
                  (coerce runtime 'list))))
      (pthread-mutex-unlock *gcmetrics-mutex*)
      (let ((end (get-internal-real-time)))
        (format t "~&all done: ~fs~%"
                (/ (- end start) internal-time-units-per-second))))))

;;; run this with a 16GB dynamic space
(defun allocator-benchmark (n-threads n-iter)
  (let (threads (sem (make-semaphore)))
    (flet ((work (arg)
             (let ((out (format nil "/tmp/out~d.fasl" arg)))
               (dotimes (i n-iter)
                 (compile-file "src/compiler/node"
                               :print nil :block-compile t :verbose nil
                               :output-file out)
                 (signal-semaphore sem))
               (delete-file out))
             (values (sb-vm::current-thread-offset-sap
                      sb-vm::thread-et-allocator-mutex-acq-slot)
                     (sb-vm::current-thread-offset-sap
                      sb-vm::thread-et-find-freeish-page-slot)
                     (sb-vm::current-thread-offset-sap
                      sb-vm::thread-et-bzeroing-slot))))
      (dotimes (i n-threads)
        (push (make-thread #'work :name (format nil "worker~d" i) :arguments i)
              threads)
        (sleep .25))
      (setq threads (nreverse threads))
      (macrolet ((intmetric (slot)
                   `(sap-ref-word sap (ash ,slot sb-vm:word-shift)))
                 (floatmetric (slot)
                   `(float (sap-ref-word sap (ash ,slot sb-vm:word-shift)))))
        (let ((n-to-go (* n-threads n-iter)))
          (loop
            ;; Wait for any thread to be done with one COMPILE-FILE
            (wait-on-semaphore sem)
            (dolist (thread threads)
              (with-deathlok (thread c-thread)
                (unless (= c-thread 0)
                  (let* ((sap (int-sap c-thread))
                         (divisor (intmetric sb-vm::thread-slow-path-allocs-slot))
                         (times
                          (list (/ (floatmetric sb-vm::thread-et-allocator-mutex-acq-slot)
                                   divisor)
                                (/ (floatmetric sb-vm::thread-et-find-freeish-page-slot)
                                   divisor)
                                (/ (floatmetric sb-vm::thread-et-bzeroing-slot)
                                   divisor))))
                    (format t "~a: ~a~%" (thread-name thread) times)))))
            (terpri)
            (when (zerop (decf n-to-go)) (return))))))))

#|
typical results:
(ALLOCATOR-BENCHMARK 1 5)
  worker0: (330.69366 387.3285 6498.661)

(ALLOCATOR-BENCHMARK 2 5)
  worker0: (330.5141 267.6703 7333.836)
  worker1: (228.58601 165.74622 6578.589)

(ALLOCATOR-BENCHMARK 5 5)
  worker0: (690.2581 425.22952 5876.69)
  worker1: (710.41406 348.25806 6209.075)
  worker2: (839.3615 454.86133 7612.185)
  worker3: (885.43054 602.65674 10080.599)
  worker4: (610.4866 262.36072 8558.833)

(ALLOCATOR-BENCHMARK 10 5)
  worker0: (1223.6002 430.6594 7850.7573)
  worker1: (1330.8501 370.85773 6489.9937)
  worker2: (1253.6841 505.19583 5270.5938)
  worker3: (1490.959 715.54004 6404.7485)
  worker4: (1285.563 418.3966 4903.252)
  worker5: (1166.429 367.69632 4751.1025)
  worker6: (1516.6385 703.275 5229.6743)
  worker7: (1445.5946 435.18625 8682.394)
  worker8: (1445.0297 392.44226 6706.816)
  worker9: (1356.9069 461.00558 5664.2266)

(ALLOCATOR-BENCHMARK 20 3)
  worker0: (1556.1759 320.41278 7864.225)
  worker1: (2484.3042 380.25073 6287.422)
  worker2: (2330.8076 518.1103 6229.52)
  worker3: (1892.3644 413.4363 6322.3574)
  worker4: (2391.721 581.5211 5309.2114)
  worker5: (3180.5654 1101.414 5779.844)
  worker6: (2621.355 634.3344 4852.1455)
  worker7: (2378.809 440.01437 4085.8718)
  worker8: (2730.9878 432.23807 3691.8616)
  worker9: (2128.9807 376.76605 6020.571)
  worker10: (2715.6238 483.9466 7864.9487)
  worker11: (2880.8203 445.12094 5770.294)
  worker12: (3576.9197 767.5074 6190.4316)
  worker13: (3010.8503 437.47897 6542.27)
  worker14: (2961.2139 453.69385 6901.6504)
  worker15: (3242.9263 513.6723 6050.3047)
  worker16: (3760.2107 1017.8271 6511.578)
  worker17: (3949.1416 794.4195 5975.102)
  worker18: (3443.0042 444.75006 4557.97)
  worker19: (3430.517 806.4593 3539.3176)

(ALLOCATOR-BENCHMARK 30 2)
  worker0: (3228.2756 465.13016 8892.34)
  worker1: (3792.6448 770.495 7546.6333)
  worker2: (3741.0088 856.29407 9156.665)
  worker3: (3276.4631 410.84845 8926.436)
  worker4: (3618.4817 409.49045 6198.2173)
  worker5: (3677.3682 533.64966 6499.718)
  worker6: (3326.2502 426.92972 6894.4204)
  worker7: (4277.313 497.48938 8042.677)
  worker8: (4424.929 515.2159 8480.562)
  worker9: (4579.331 646.7453 7944.594)
  worker10: (5665.9673 585.96246 9082.217)
  worker11: (4093.323 536.14 8263.94)
  worker12: (5716.6953 636.16815 6921.578)
  worker13: (5787.886 771.44214 4725.5513)
  worker14: (7163.328 1685.777 5396.888)
  worker15: (5750.1753 584.4418 4869.9063)
  worker16: (5826.1787 653.7092 3785.243)
  worker17: (6162.1816 760.8072 3882.8232)
  worker18: (5333.0513 477.8418 4006.6885)
  worker19: (8481.007 597.1158 3250.377)
  worker20: (9162.3125 2120.3945 5063.6616)
  worker21: (5398.499 643.1221 11578.032)
  worker22: (7045.36 1039.0885 5842.894)
  worker23: (9666.884 543.9834 4494.3945)
  worker24: (9476.041 770.6879 4494.854)
  worker25: (4477.0054 348.83954 5587.9424)
  worker26: (5616.502 469.45154 5180.7173)
  worker27: (10800.295 481.92975 6047.9507)
  worker28: (11228.471 606.4268 4192.347)
  worker29: (19881.9 996.91534 157.6319)
|#
