(in-package sb-thread)

(sb-ext:defglobal *mutex* (sb-thread:make-mutex))
(declaim (type mutex *mutex*))
(sb-ext:defglobal *start-semaphore* nil)
(sb-ext:defglobal *ready-semaphore* nil)

(defmacro timing (&body body)
  `(binding* ((start-real (get-internal-real-time))
              ((thread-start-sec thread-start-nsec)
               (sb-unix::clock-gettime sb-unix:clock-thread-cputime-id)))
     ,@body
     (binding* ((stop-real (get-internal-real-time))
                ((thread-stop-sec thread-stop-nsec)
                 (sb-unix::clock-gettime sb-unix:clock-thread-cputime-id)))
       (values (- stop-real start-real) ; microseconds
               (+ (* 1000000 (- thread-stop-sec thread-start-sec))
                  (round (- thread-stop-nsec thread-start-nsec) 1000))))))

(defmacro timing-test (&body body)
  `(let ((m *mutex*) (waste (make-array 100 :element-type 'sb-vm:word)))
     (declare (truly-dynamic-extent waste))
     (setq sb-thread::*grab-mutex-calls-performed* 0)
     ;; Get all threads to agree on the moment they should start
     (sb-thread:signal-semaphore *ready-semaphore*)
     (sb-thread:wait-on-semaphore *start-semaphore*)
     (macrolet ((do-some-work ()
                  ;; Perform a slightly nontrivial amount of "stuff"
                  ;; i.e. more than just assigning a variable.
                  '(dotimes (i (length waste))
                      (setf (aref waste i) (logxor (aref waste i) (ash 1 (mod i 64)))))))
       (timing ,@body))))

;;; Default implementation of WITH-MUTEX
(defun grab-and-release-baseline-nlx-protected (n-iter)
  (timing-test
     (dotimes (i (the fixnum n-iter))
       (sb-thread:with-mutex (m)
         (do-some-work)))))

;;; Default implementation of grab/release but no interrupt-safety
(defun grab-and-release-baseline-unprotected (n-iter)
  (timing-test
    (dotimes (i (the fixnum n-iter))
      (sb-thread:grab-mutex m)
      (do-some-work)
      (sb-thread:release-mutex m))))

(defvar *mutexes-held* nil)
(setf (sb-int:info :variable :wired-tls '*mutexes-held*) :always-thread-local
      (sb-int:info :variable :always-bound '*mutexes-held*) :always-bound)
(defmacro binding-mutexes-held ((mutex) &body body)
  `(dx-let ((*mutexes-held* (cons ,mutex *mutexes-held*))) ,@body))

;;; Faster implementation of grab and release
(defun grab-and-release-algo-2 (n-iter)
  (timing-test
    (dotimes (i (the fixnum n-iter))
      (sb-thread:wait-for-mutex-algorithm-2 m)
      (binding-mutexes-held (m)
        (do-some-work)
        (sb-thread:fast-release-mutex m)))))
;;; Faster implementation, and GRAB- can often avoid a function call
(defun grab-and-release-algo-2+ (n-iter)
  (timing-test
    (dotimes (i (the fixnum n-iter))
      (sb-thread:wait-for-mutex-2-partial-inline m)
      (binding-mutexes-held (m)
        (do-some-work)
        (sb-thread:fast-release-mutex m)))))
(defun grab-and-release-algo-3 (n-iter)
  (timing-test
    (dotimes (i (the fixnum n-iter))
      (sb-thread:wait-for-mutex-algorithm-3 m)
      (binding-mutexes-held (m)
        (do-some-work)
        (sb-thread:fast-release-mutex m)))))
(defun grab-and-release-algo-3+ (n-iter)
  (timing-test
    (dotimes (i (the fixnum n-iter))
      (sb-thread:wait-for-mutex-3-partial-inline m)
      (binding-mutexes-held (m)
        (do-some-work)
        (sb-thread:fast-release-mutex m)))))

(defun try (n-threads n-iter label testfun)
  (let (threads)
    (setq *ready-semaphore* (sb-thread:make-semaphore))
    (setq *start-semaphore* (sb-thread:make-semaphore))
    (dotimes (i n-threads)
      (push (sb-thread:make-thread testfun :arguments n-iter)
            threads))
    ;; Wait until all threads say that they're ready
    (sb-thread:wait-on-semaphore *ready-semaphore* :n n-threads)
    ;; "Unleash the hounds"
    (sb-thread:signal-semaphore *start-semaphore* n-threads)
    (let ((sum-real 0)
          (sum-cpu 0))
      (dolist (thread threads)
        (multiple-value-bind (realtime cputime)
            (sb-thread:join-thread thread)
          ;;(format t "real: ~5d cpu: ~5d~%" realtime cputime)
          (incf sum-real realtime)
          (incf sum-cpu cputime)))
      (format t "~20a: cpu=(sum=~8d avg=~8d) real=~8d~@[  [~d calls]~]~%"
              label
              sum-cpu (floor sum-cpu n-threads)
              (floor sum-real n-threads)
              (let ((c sb-thread::*grab-mutex-calls-performed*))
                (unless (zerop c) c))))))

(defun test-n-threads (n)
  (try n 100000 "WITH-MUTEX"   'grab-and-release-baseline-nlx-protected)
  (try n 100000 "GRAB+RELEASE" 'grab-and-release-baseline-unprotected)
  (try n 100000 "ALGO2"        'grab-and-release-algo-2)
  (try n 100000 "ALGO2+"       'grab-and-release-algo-2+)
  (try n 100000 "ALGO3"        'grab-and-release-algo-3)
  (try n 100000 "ALGO3+"       'grab-and-release-algo-3+))

#|
* (sb-thread::test-n-threads 5)
WITH-MUTEX          : cpu=(sum= 1129410 avg=  225882) real=  251198
GRAB+RELEASE        : cpu=(sum= 1013281 avg=  202656) real=  221599
ALGO2               : cpu=(sum=  983301 avg=  196660) real=  217599  [480215 calls]
ALGO2+              : cpu=(sum=  906144 avg=  181228) real=  199999  [240760 calls]
ALGO3               : cpu=(sum=  953727 avg=  190745) real=  212000  [482759 calls]
ALGO3+              : cpu=(sum=  893479 avg=  178695) real=  199998  [246538 calls]

* (sb-thread::test-n-threads 10)
WITH-MUTEX          : cpu=(sum= 4744457 avg=  474445) real=  535597
GRAB+RELEASE        : cpu=(sum= 4854507 avg=  485450) real=  544798
ALGO2               : cpu=(sum= 4168526 avg=  416852) real=  453997  [968121 calls]
ALGO2+              : cpu=(sum= 3720948 avg=  372094) real=  409599  [462061 calls]
ALGO3               : cpu=(sum= 4091659 avg=  409165) real=  444797  [970730 calls]
ALGO3+              : cpu=(sum= 3824551 avg=  382455) real=  418398  [404065 calls]

* (sb-thread::test-n-threads 15)
WITH-MUTEX          : cpu=(sum=12460694 avg=  830712) real=  909595
GRAB+RELEASE        : cpu=(sum=11579420 avg=  771961) real=  835462
ALGO2               : cpu=(sum= 9343377 avg=  622891) real=  654131  [1463285 calls]
ALGO2+              : cpu=(sum= 8908803 avg=  593920) real=  631996  [571894 calls]
ALGO3               : cpu=(sum= 9262899 avg=  617526) real=  654929  [1458653 calls]
ALGO3+              : cpu=(sum= 8771110 avg=  584740) real=  620264  [539979 calls]

* (sb-thread::test-n-threads 20)
WITH-MUTEX          : cpu=(sum=18373401 avg=  918670) real= 1157394
GRAB+RELEASE        : cpu=(sum=19171438 avg=  958571) real= 1110394
ALGO2               : cpu=(sum=15958082 avg=  797904) real=  883196  [1947759 calls]
ALGO2+              : cpu=(sum=14531894 avg=  726594) real=  839596  [776184 calls]
ALGO3               : cpu=(sum=16258896 avg=  812944) real=  869995  [1950461 calls]
ALGO3+              : cpu=(sum=15892267 avg=  794613) real=  840197  [719736 calls]

* (sb-thread::test-n-threads 25)
WITH-MUTEX          : cpu=(sum=24908201 avg=  996328) real= 1425273
GRAB+RELEASE        : cpu=(sum=24098405 avg=  963936) real= 1358552
ALGO2               : cpu=(sum=18334699 avg=  733387) real= 1135194  [2402256 calls]
ALGO2+              : cpu=(sum=18920912 avg=  756836) real= 1095675  [932165 calls]
ALGO3               : cpu=(sum=20350344 avg=  814013) real= 1100794  [2427181 calls]
ALGO3+              : cpu=(sum=19708020 avg=  788320) real= 1051835  [891826 calls]

* (sb-thread::test-n-threads 30)
WITH-MUTEX          : cpu=(sum=29270655 avg=  975688) real= 1725458
GRAB+RELEASE        : cpu=(sum=29194555 avg=  973151) real= 1616391
ALGO2               : cpu=(sum=25331922 avg=  844397) real= 1331460  [2911750 calls]
ALGO2+              : cpu=(sum=25128049 avg=  837601) real= 1291458  [1111621 calls]
ALGO3               : cpu=(sum=25039192 avg=  834639) real= 1310660  [2912492 calls]
ALGO3+              : cpu=(sum=24520302 avg=  817343) real= 1261327  [1069574 calls]

* (sb-thread::test-n-threads 35)
WITH-MUTEX          : cpu=(sum=35886189 avg= 1025319) real= 1958961
GRAB+RELEASE        : cpu=(sum=32335690 avg=  923876) real= 1894504
ALGO2               : cpu=(sum=29280055 avg=  836573) real= 1557933  [3395447 calls]
ALGO2+              : cpu=(sum=28854572 avg=  824416) real= 1490965  [1276195 calls]
ALGO3               : cpu=(sum=28764304 avg=  821837) real= 1539534  [3389637 calls]
ALGO3+              : cpu=(sum=27708686 avg=  791676) real= 1467193  [1239603 calls]

* (sb-thread::test-n-threads 40)
WITH-MUTEX          : cpu=(sum=40892193 avg= 1022304) real= 2209987
GRAB+RELEASE        : cpu=(sum=37467476 avg=  936686) real= 2130089
ALGO2               : cpu=(sum=34310107 avg=  857752) real= 1765091  [3882825 calls]
ALGO2+              : cpu=(sum=33543491 avg=  838587) real= 1711993  [1455625 calls]
ALGO3               : cpu=(sum=32834489 avg=  820862) real= 1724991  [3878370 calls]
ALGO3+              : cpu=(sum=29716033 avg=  742900) real= 1669290  [1378376 calls]

* (sb-thread::test-n-threads 45)
WITH-MUTEX          : cpu=(sum=47760154 avg= 1061336) real= 2463454
GRAB+RELEASE        : cpu=(sum=45402624 avg= 1008947) real= 2385232
ALGO2               : cpu=(sum=36951436 avg=  821143) real= 1971723  [4363504 calls]
ALGO2+              : cpu=(sum=36382508 avg=  808500) real= 1918567  [1626474 calls]
ALGO3               : cpu=(sum=37765508 avg=  839233) real= 1934301  [4365986 calls]
ALGO3+              : cpu=(sum=37104282 avg=  824539) real= 1842213  [1582110 calls]

* (sb-thread::test-n-threads 50)
WITH-MUTEX          : cpu=(sum=51003010 avg= 1020060) real= 2775826
GRAB+RELEASE        : cpu=(sum=50266070 avg= 1005321) real= 2643187
ALGO2               : cpu=(sum=42814331 avg=  856286) real= 2179588  [4851758 calls]
ALGO2+              : cpu=(sum=40166972 avg=  803339) real= 2121509  [1795256 calls]
ALGO3               : cpu=(sum=40122437 avg=  802448) real= 2167029  [4843515 calls]
ALGO3+              : cpu=(sum=40284932 avg=  805698) real= 2058550  [1743178 calls]

* (sb-thread::test-n-threads 55)
WITH-MUTEX          : cpu=(sum=55076760 avg= 1001395) real= 3008711
GRAB+RELEASE        : cpu=(sum=56878995 avg= 1034163) real= 2877731
ALGO2               : cpu=(sum=46343366 avg=  842606) real= 2402168  [5331893 calls]
ALGO2+              : cpu=(sum=43091002 avg=  783472) real= 2330753  [1938588 calls]
ALGO3               : cpu=(sum=45509126 avg=  827438) real= 2342314  [5345695 calls]
ALGO3+              : cpu=(sum=45191331 avg=  821660) real= 2260789  [1909666 calls]
|#
