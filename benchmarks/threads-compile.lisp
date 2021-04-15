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

#|
diff to apply:
--- a/src/compiler/main.lisp
+++ b/src/compiler/main.lisp
@@ -1649,7 +1649,7 @@ necessary, since type inference may take arbitrarily long to converge.")
         (handler-bind (((satisfies handle-condition-p) #'handle-condition-handler))
           (with-compilation-values
             (with-compilation-unit ()
-              (with-world-lock ()
+              (progn ; with-world-lock ()
                 (setf (sb-fasl::fasl-output-source-info *compile-object*)
                       (debug-source-for-info info))
                 (with-ir1-namespace
|#

;;; Exercise COMPILE-FILE in many threads, which is representative of a
;;; lispy workload.  Any suitable workload should do.
;;; This test presumes that the world-lock has been removed from around
;;; COMPILE-FILE, in order to allow concurrent progress in each thread.
;;; It would be better to have each thread doing a different kind of work,
;;; but I took the easy route.
(defun benchmark (n-threads n-iter)
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
