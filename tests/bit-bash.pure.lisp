
;;; Test {1,2,4,8}-byte-per-element FILL
(defun test-bash-fillers-octet-or-larger ()
  (dolist (bits-per-element '(#+64-bit 64 32 16 8))
    (let* ((function (intern (format nil "UB~D-FILL" bits-per-element) "SB-KERNEL"))
           (element (case bits-per-element
                      (8 #xbe)
                      (16 #xfafa)
                      (32 #xdeadbeef)
                      (64 #xaababababac0fefe)))
           (word (case bits-per-element
                   (8 (*  #x0101010101010101 element))
                   (16 (* #x0001000100010001 element))
                   (32 (* #x0000000100000001 element))
                   (64 element))))
      (format t "~&Testing bits-per-element = ~d~%" bits-per-element)
      ;; Smoke-test a large array
      (let ((array (make-array 20000 :element-type `(unsigned-byte ,bits-per-element))))
        (funcall function array 0 (length array) word)
        (loop for x across array
              do (assert (= x element))))
      (let* ((len 300)
             (*print-pretty* nil))
        ;; Make sure we see all possible offsets into the intial word
        (loop for start from 0 to len
              do
          ;; Iterate over all possible values of number of elements to fill
          (dotimes (n-elts (1+ (- len start)))
            (let ((array (make-array len :element-type `(unsigned-byte ,bits-per-element))))
              (funcall function array start n-elts word)
              ;; Verify the result
              (dotimes (i len)
                (unless (= (aref array i)
                           (if (<= start i (+ start n-elts -1))
                               element
                               0))
                  (let ((*print-array* t))
                    (format t "~&Start=~d N=~d Result=~s~%"
                            start n-elts array)
                    (error "Failure")))))))))))
(with-test (:name :bit-bash-fill)
  (test-bash-fillers-octet-or-larger))

;;; explicitly test UB{8,16,32,64}-bash-copy
;;; for all interesting combinations of src-offset, dst-offset,
;;; i.e. where the number of initial/final elements within a word
;;; varies over all possible values.

(defun test-bash-copiers-octet-or-larger ()
  (dolist (bits-per-element '(8 16 32 #+64-bit 64))
    (format t "~&Testing bits-per-element = ~d~%" bits-per-element)
    (let* ((len 70)
           (dest-control
            (make-array len :element-type `(signed-byte ,bits-per-element)))
           (dest
            (make-array len :element-type `(signed-byte ,bits-per-element)))
           (source
            (make-array len :element-type `(signed-byte ,bits-per-element)))
           (function
            (intern (format nil "UB~D-BASH-COPY" bits-per-element) "SB-KERNEL"))
           (*print-pretty* nil))
      (loop for i from 0 below len
            for val downfrom -1 do (setf (aref source i) val))
      (loop for i from 0 below len
            for val from 1 do (setf (aref dest-control i) val))
      ;; (format t "Src=(~{~3d~^ ~})~%" (coerce source 'list))
      ;; (format t "Dst=(~{~3d~^ ~})~%" (coerce dest-control 'list))
      (macrolet ((reset-arrays ()
                   ;; Don't trust REPLACE while testing the implementation of REPLACE
                   `(progn
                      #+nil
                      (format t "Test parameters: src-index=~d dst-index=~d count=~d~%"
                              src-index dest-index n-elts)
                      ;; Check that we're not overrunning the array
                      (assert (< src-index len))
                      (assert (<= (+ src-index n-elts) len))
                      (assert (< dest-index len))
                      (assert (<= (+ dest-index n-elts) len))
                      (dotimes (i len)
                        (setf (aref dest i) (aref dest-control i)))))
                 (verify (bit-bashing-source)
                   ;; Verify that the byte-blt copier did the same thing
                   ;; as would expected from a simplistic copier.
                   `(let ((replaced-range-end (+ dest-index n-elts)))
                      (dotimes (i len)
                        ;; If outside of the replaced range, the value should be
                        ;; from the control array; otherwise the source of the copy.
                        (let ((expect (if (or (< i dest-index) (>= i replaced-range-end))
                                          (aref dest-control i)
                                          (aref ,bit-bashing-source
                                                (+ src-index (- i dest-index))))))
                       (unless (= (aref dest i) expect)
                         (format t "Test parameters: src-index=~d dst-index=~d count=~d~%"
                                 src-index dest-index n-elts)
                         (format t "Result=~s~%" dest)
                         (error "Error at element ~d, should have been ~d but is ~d"
                                i expect (aref dest i))))))))
        (format t "~&Different arrays ...")
        ;; Make sure we see all possible offsets into the intial word
        (dotimes (src-index (1- len))
          (dotimes (dest-index (1- len))
            (let* ((source-available-elements (- len src-index))
                   (dest-available-elements (- len dest-index))
                   (available-elements
                    (min source-available-elements dest-available-elements)))
              ;; Iterate over all possible values of number of elements to move
              (dotimes (n-elts available-elements)
                (reset-arrays)
                ;; (format t "Count=~d src-off=~d dst-off=~d~%" n-elts src-index dest-index)
                (funcall function source src-index dest dest-index n-elts)
                ;; (format t "Res=(~{~3d~^ ~})~%" (coerce dest 'list))
                (verify source)))))
        (format t "~&Same array ...")
        ;; Make sure we see all possible offsets into the intial word
        (dotimes (src-index (1- len))
          (dotimes (dest-index (1- len))
            (let* ((source-available-elements (- len src-index))
                   (dest-available-elements (- len dest-index))
                   (available-elements
                    (min source-available-elements dest-available-elements)))
              (dotimes (n-elts available-elements)
                (reset-arrays)
                (funcall function dest src-index dest dest-index n-elts)
                (verify dest-control)))))
        (terpri)))))
(compile 'test-bash-copiers-octet-or-larger)
(with-test (:name :bit-bash-copiers)
  (test-bash-copiers-octet-or-larger))

(defvar *arrays*)
(defvar *array-size*)

(defun test-aligned (fun len n-iter) ;; copy from A into B
  (declare (function fun))
  (declare (sb-int:index n-iter))
  (dotimes (i n-iter)
    (loop for (a b) on *arrays* when b do (funcall fun a 0 b 0 len))))
(defun test-misaligned (fun len n-iter) ;; copy from A+1 into B+5
  (declare (function fun))
  (declare (sb-int:index n-iter))
  (dotimes (i n-iter)
    (loop for (a b) on *arrays*
          when b do (funcall fun a 1 b 5 (- len 5)))))
(defun test-same-array-1 (fun len n-iter)
  (declare (function fun))
  (declare (sb-int:index n-iter))
  (dotimes (i n-iter)
    (loop for a in *arrays*
          do (funcall fun a 1 a 5 (- len 5)))))
(defun test-same-array-2 (fun len n-iter)
  (declare (function fun))
  (declare (sb-int:index n-iter))
  (dotimes (i n-iter)
    (loop for a in *arrays*
          do (funcall fun a 5 a 1 (- len 5)))))

(defun compare-move (test arg1 arg2 bits-per-element)
  (let* ((a (symbol-function
             (intern (format nil "UB~D-BASH-COPY" bits-per-element) "SB-KERNEL")))
         (b (symbol-function
             (intern (format nil "NEW-UB~D-BASH-COPY" bits-per-element) "SB-KERNEL")))
         (t0 (get-internal-real-time))
         (dummy1 (funcall test a arg1 arg2))
         (t1 (get-internal-real-time))
         (dummy2 (funcall test b arg1 arg2))
         (t2 (get-internal-real-time)))
    dummy1 dummy2
    (let* ((et1 (- t1 t0))
           (et2 (- t2 t1))
           (delta (- et2 et1))
           (delta-pct (float (* 100 (/ delta et1)))))
      (format t "  ~25a: ~7d vs ~7d (delta = ~,1f%)~%"
              test et1 et2 delta-pct))))

(defun speed-comparison (&optional bits-per-element (scale-iterations 1))
  (dolist (bits-per-element (if bits-per-element
                                (list bits-per-element)
                                '(8 16 32 64)))
    (format t "Testing bits-per-element = ~d~%" bits-per-element)
    (loop for (*array-size* n-iter)
          ;; Try to spend roughly the same time in each test
          in '((  10  25000)
               (  20  20000)
               (  32  20000)
               (  50  20000)
               (  64  15000)
               (  128 10000)
               (  200 10000)
               (  256 10000)
               (  500  5000)
               (  512  5000)
               ( 1000  1000)
               ( 1024  1000)
               ( 2000  1000)
               ( 2048  1000)
               ( 4000  1000))
          do (format t "~&Testing array len ~d, ~d iterations:~%" *array-size* n-iter)
             (setq *arrays*
                   (loop repeat 1000
                         collect (make-array *array-size*
                                             :element-type `(unsigned-byte ,bits-per-element))))
             (gc :full t)
             (let ((n-iter (floor (* n-iter scale-iterations))))
               (compare-move 'test-aligned *array-size* n-iter bits-per-element)
               (compare-move 'test-misaligned *array-size* n-iter bits-per-element)
               (compare-move 'test-same-array-1 *array-size* n-iter bits-per-element)
               (compare-move 'test-same-array-2 *array-size* n-iter bits-per-element)))
    (terpri)))

(defun iterate-fill-test (fun value n-iter)
  (declare (function fun))
  (declare (sb-int:index n-iter))
  (dotimes (i n-iter)
    (loop for a in *arrays* do (funcall fun a 1 (1- (length a)) value))))

(defun readclock ()
  (multiple-value-bind (sec nsec)
      (sb-unix::clock-gettime sb-unix::clock-realtime)
    (cons sec nsec)))

(defun microsec-diff (to from)
  (let* ((sec-diff  (- (car to) (car from)))
         (nsec-diff (+ (- (cdr to) (cdr from))
                       (* sec-diff 1000 1000 1000))))
    (round nsec-diff 1000)))

(defun a/b-compare-fill (bits-per-element n-iter)
  (let* ((old (symbol-function
               (intern (format nil "CONSTANT-UB~D-BASH" bits-per-element) "SB-KERNEL")))
         (new (symbol-function
               (intern (format nil "UB~D-FILL" bits-per-element) "SB-KERNEL")))
         (total-elements
          (loop for a in *arrays* sum (1- (length a))))
         (total-bytes (* total-elements n-iter (/ bits-per-element 8)))
         (t0 (readclock))
         (dummy1 (iterate-fill-test old #xaa n-iter))
         (t1 (readclock))
         (dummy2 (iterate-fill-test new #xaa n-iter))
         (t2 (readclock)))
    dummy1 dummy2
    (let* ((et1 (microsec-diff t1 t0))
           (et2 (microsec-diff t2 t1))
           (delta (- et2 et1))
           (delta-pct (float (* 100 (/ delta et1)))))
      (format t " ~7d vs ~7d   ~7,1f vs ~7,1f  (delta = ~,1f%)~%"
              et1 et2
              (/ total-bytes et1)
              (/ total-bytes et2)
              delta-pct)
      (values et1 et2))))

(defun run-fill-comparison (&optional bits-per-element (scale-iterations 1))
  (dolist (bits-per-element (if bits-per-element
                                (list bits-per-element)
                                '(8 16 32 64)))
    (format t "Bits-per-element = ~d~%" bits-per-element)
    (format t " Trials  Length   Nbytes           µsec              bytes/µsec~%")
    (let ((total-time-old 0)
          (total-time-new 0))
      (loop for (*array-size* n-iter)
            ;; Try to spend roughly the same time in each test
            in '((   4  25000)
                 (  10  25000)
                 (  20  20000)
                 (  32  20000)
                 (  50  20000)
                 (  64  15000)
                 (  128 10000)
                 (  200 10000)
                 (  256 10000)
                 (  500  5000)
                 (  512  5000)
                 ( 1000  3000)
                 ( 1024  2500)
                 ( 2000  1000)
                 ( 2047  1000)
                 ( 2048  1000)
                 ( 4000  1000)
                 (10000   500)
                 (12288   250)
                 (16384   200)
                 (40000   100)
                 (80000    50))
            do (setq n-iter (floor n-iter (/ bits-per-element 8)))
               (format t "~& ~6d ~7d ~8d : " n-iter *array-size*
                       (/ (* *array-size* bits-per-element) 8))
               (setq *arrays*
                     (loop repeat 1000
                           collect (make-array *array-size*
                                               :element-type `(unsigned-byte ,bits-per-element))))
               (gc :full t)
               (let ((n-iter (floor (* n-iter scale-iterations))))
                 (sb-sys:without-gcing
                   (multiple-value-bind (old new)
                       (a/b-compare-fill bits-per-element n-iter)
                     (incf total-time-old old)
                     (incf total-time-new new)))))
      (format t "~26t ~8d   ~8d~%" total-time-old total-time-new))))

;;; Not really a regression test, since it assumes presence of both
;;; the old and new definitions.
;;; But I find it convenient to be able to assert that:
;;; (a) nothing got broken by the changes
;;; (b) the changes yield a performance improvemenet
(with-test (:name :fill-speed-comparison)
  (run-fill-comparison))

;;;; MACHINE-DEPENDENT RESULT

#|
AMD EPYC 7B12
-------------
Bits-per-element = 8
 Trials  Length   Nbytes           µsec              bytes/µsec
  25000       4        4 :   341552 vs  280405     219.6 vs   267.5  (delta = -17.9%)
  25000      10       10 :   399850 vs  231999     562.7 vs   969.8  (delta = -42.0%)
  20000      20       20 :   247647 vs  166991    1534.4 vs  2275.6  (delta = -32.6%)
  20000      32       32 :   309805 vs  235518    2001.3 vs  2632.5  (delta = -24.0%)
  20000      50       50 :   336341 vs  180157    2913.7 vs  5439.7  (delta = -46.4%)
  15000      64       64 :   228946 vs  149129    4127.6 vs  6336.8  (delta = -34.9%)
  10000     128      128 :   214798 vs  138791    5912.5 vs  9150.4  (delta = -35.4%)
  10000     200      200 :   228011 vs  163430    8727.6 vs 12176.5  (delta = -28.3%)
  10000     256      256 :   258894 vs  204279    9849.6 vs 12482.9  (delta = -21.1%)
   5000     500      500 :   174364 vs  108236   14309.1 vs 23051.5  (delta = -37.9%)
   5000     512      512 :   199790 vs  114229   12788.4 vs 22367.3  (delta = -42.8%)
   3000    1000     1000 :   245162 vs  141996   12224.6 vs 21106.2  (delta = -42.1%)
   2500    1024     1024 :   208318 vs   91912   12276.9 vs 27825.5  (delta = -55.9%)
   1000    2000     2000 :   106657 vs   61292   18742.3 vs 32614.4  (delta = -42.5%)
   1000    2047     2047 :   110975 vs   73332   18436.6 vs 27900.5  (delta = -33.9%)
   1000    2048     2048 :   135633 vs   64357   15092.2 vs 31807.0  (delta = -52.6%)
   1000    4000     4000 :   311743 vs  140724   12827.9 vs 28417.3  (delta = -54.9%)
    500   10000    10000 :   433211 vs  235128   11540.6 vs 21262.9  (delta = -45.7%)
    250   12288    12288 :   376491 vs  289956    8158.9 vs 10593.8  (delta = -23.0%)
    200   16384    16384 :   463911 vs  463969    7063.0 vs  7062.1  (delta = 0.0%)
    100   40000    40000 :   421088 vs  526038    9499.0 vs  7603.8  (delta = 24.9%)
     50   80000    80000 :   513660 vs  399886    7787.2 vs 10002.7  (delta = -22.1%)
                            6266847    4461754
Bits-per-element = 16
 Trials  Length   Nbytes           µsec              bytes/µsec
  12500       4        8 :   139319 vs  112301     538.3 vs   667.8  (delta = -19.4%)
  12500      10       20 :   150900 vs  126314    1491.1 vs  1781.3  (delta = -16.3%)
  10000      20       40 :   124834 vs   99902    3044.0 vs  3803.7  (delta = -20.0%)
  10000      32       64 :   135923 vs  145458    4561.4 vs  4262.4  (delta = 7.0%)
  10000      50      100 :   227421 vs  188859    4309.2 vs  5189.1  (delta = -17.0%)
   7500      64      128 :   175130 vs  153831    5396.0 vs  6143.1  (delta = -12.2%)
   5000     128      256 :   151993 vs  144051    8355.6 vs  8816.3  (delta = -5.2%)
   5000     200      400 :   264077 vs  314552    7535.7 vs  6326.5  (delta = 19.1%)
   5000     256      512 :   210889 vs  235466   12091.7 vs 10829.6  (delta = 11.7%)
   2500     500     1000 :   158694 vs  211581   15722.1 vs 11792.2  (delta = 33.3%)
   2500     512     1024 :   166545 vs  179679   15341.2 vs 14219.8  (delta = 7.9%)
   1500    1000     2000 :   168170 vs   97682   17821.3 vs 30681.2  (delta = -41.9%)
   1250    1024     2048 :   137258 vs   81608   18632.8 vs 31338.8  (delta = -40.5%)
    500    2000     4000 :   203020 vs   78799    9846.3 vs 25368.3  (delta = -61.2%)
    500    2047     4094 :   312818 vs   71897    6540.5 vs 28457.4  (delta = -77.0%)
    500    2048     4096 :   175050 vs   87427   11693.8 vs 23413.8  (delta = -50.1%)
    500    4000     8000 :   332506 vs   95210   12026.9 vs 42001.9  (delta = -71.4%)
    250   10000    20000 :   520219 vs  723857    9610.4 vs  6906.8  (delta = 39.1%)
    125   12288    24576 :   597843 vs  457871    5138.1 vs  6708.8  (delta = -23.4%)
    100   16384    32768 :   452273 vs  409799    7244.7 vs  7995.6  (delta = -9.4%)
     50   40000    80000 :   390620 vs  335908   10239.9 vs 11907.7  (delta = -14.0%)
     25   80000   160000 :   502507 vs  383015    7960.0 vs 10443.3  (delta = -23.8%)
                            5698009    4735067
Bits-per-element = 32
 Trials  Length   Nbytes           µsec              bytes/µsec
   6250       4       16 :    64080 vs   51711    1170.4 vs  1450.4  (delta = -19.3%)
   6250      10       40 :    97514 vs   60730    2307.4 vs  3704.9  (delta = -37.7%)
   5000      20       80 :    90553 vs   98885    4196.4 vs  3842.8  (delta = 9.2%)
   5000      32      128 :   150633 vs   71285    4116.0 vs  8697.5  (delta = -52.7%)
   5000      50      200 :    99321 vs   92367    9867.0 vs 10609.8  (delta = -7.0%)
   3750      64      256 :   117165 vs   76358    8065.5 vs 12375.9  (delta = -34.8%)
   2500     128      512 :   105305 vs  102411   12060.2 vs 12401.0  (delta = -2.7%)
   2500     200      800 :   132380 vs  136123   15032.5 vs 14619.1  (delta = 2.8%)
   2500     256     1024 :   208690 vs  168998   12219.1 vs 15088.9  (delta = -19.0%)
   1250     500     2000 :   131438 vs   76971   18982.3 vs 32414.8  (delta = -41.4%)
   1250     512     2048 :   139378 vs   81262   18331.4 vs 31441.5  (delta = -41.7%)
    750    1000     4000 :   145441 vs   71447   20606.3 vs 41947.2  (delta = -50.9%)
    625    1024     4096 :   125167 vs   64600   20432.7 vs 39589.8  (delta = -48.4%)
    250    2000     8000 :    96194 vs   58517   20780.9 vs 34161.0  (delta = -39.2%)
    250    2047     8188 :   148655 vs   69247   13763.4 vs 29546.4  (delta = -53.4%)
    250    2048     8192 :   124125 vs   63342   16491.4 vs 32316.6  (delta = -49.0%)
    250    4000    16000 :   503388 vs  452250    7944.2 vs  8842.5  (delta = -10.2%)
    125   10000    40000 :   629788 vs  557512    7938.4 vs  8967.5  (delta = -11.5%)
     62   12288    49152 :   352325 vs  270733    8648.8 vs 11255.3  (delta = -23.2%)
     50   16384    65536 :   423121 vs  346584    7743.9 vs  9454.0  (delta = -18.1%)
     25   40000   160000 :   452171 vs  479968    8846.0 vs  8333.7  (delta = 6.1%)
     12   80000   320000 :   521224 vs  330157    7367.2 vs 11630.7  (delta = -36.7%)
                            4858056    3781458
Bits-per-element = 64
 Trials  Length   Nbytes           µsec              bytes/µsec
   3125       4       32 :    31031 vs   23563    2416.9 vs  3183.0  (delta = -24.1%)
   3125      10       80 :    64771 vs   29177    3473.8 vs  7711.6  (delta = -55.0%)
   2500      20      160 :    39264 vs   26916    9678.1 vs 14118.0  (delta = -31.4%)
   2500      32      256 :    61615 vs   45848   10062.5 vs 13522.9  (delta = -25.6%)
   2500      50      400 :   101512 vs   98525    9654.0 vs  9946.7  (delta = -2.9%)
   1875      64      512 :    88172 vs   76090   10717.7 vs 12419.5  (delta = -13.7%)
   1250     128     1024 :   112203 vs   75562   11318.8 vs 16807.4  (delta = -32.7%)
   1250     200     1600 :   137411 vs   76432   14482.1 vs 26036.2  (delta = -44.4%)
   1250     256     2048 :   142940 vs   80298   17839.7 vs 31756.7  (delta = -43.8%)
    625     500     4000 :   153480 vs   55754   16256.2 vs 44750.2  (delta = -63.7%)
    625     512     4096 :   122250 vs   58706   20899.8 vs 43522.0  (delta = -52.0%)
    375    1000     8000 :   160302 vs   77494   18696.0 vs 38674.0  (delta = -51.7%)
    312    1024     8192 :   246139 vs  186457   10373.8 vs 13694.4  (delta = -24.2%)
    125    2000    16000 :   246167 vs  154516    8120.5 vs 12937.2  (delta = -37.2%)
    125    2047    16376 :   255975 vs  197239    7993.0 vs 10373.2  (delta = -22.9%)
    125    2048    16384 :   241841 vs  194475    8464.2 vs 10525.8  (delta = -19.6%)
    125    4000    32000 :   433466 vs  466047    9225.6 vs  8580.7  (delta = 7.5%)
     62   10000    80000 :   494196 vs  392047   10035.5 vs 12650.3  (delta = -20.7%)
     31   12288    98304 :   322735 vs  252643    9441.7 vs 12061.2  (delta = -21.7%)
     25   16384   131072 :   376290 vs  352954    8707.6 vs  9283.4  (delta = -6.2%)
     12   40000   320000 :   682984 vs  328251    5622.2 vs 11698.1  (delta = -51.9%)
      6   80000   640000 :   392156 vs  364603    9791.9 vs 10531.9  (delta = -7.0%)
                            4906900    3613597

Intel(R) Xeon(R) CPU E5-2680 v2 @ 2.80GHz
-----------------------------------------
Bits-per-element = 8
 Trials  Length   Nbytes           µsec              bytes/µsec
  25000       4        4 :   350589 vs  296332     213.9 vs   253.1  (delta = -15.5%)
  25000      10       10 :   414277 vs  305662     543.1 vs   736.1  (delta = -26.2%)
  20000      20       20 :   337544 vs  210458    1125.8 vs  1805.6  (delta = -37.7%)
  20000      32       32 :   330239 vs  210401    1877.4 vs  2946.8  (delta = -36.3%)
  20000      50       50 :   395215 vs  238162    2479.7 vs  4114.8  (delta = -39.7%)
  15000      64       64 :   290746 vs  178632    3250.3 vs  5290.2  (delta = -38.6%)
  10000     128      128 :   251330 vs  158338    5053.1 vs  8020.8  (delta = -37.0%)
  10000     200      200 :   316193 vs  236075    6293.6 vs  8429.5  (delta = -25.3%)
  10000     256      256 :   366812 vs  306246    6951.8 vs  8326.6  (delta = -16.5%)
   5000     500      500 :   277140 vs  246626    9002.7 vs 10116.5  (delta = -11.0%)
   5000     512      512 :   279635 vs  247317    9136.9 vs 10330.9  (delta = -11.6%)
   3000    1000     1000 :   240613 vs  239571   12455.7 vs 12509.9  (delta = -0.4%)
   2500    1024     1024 :   200211 vs  197098   12774.0 vs 12975.8  (delta = -1.6%)
   1000    2000     2000 :   138263 vs  136820   14458.0 vs 14610.4  (delta = -1.0%)
   1000    2047     2047 :   143330 vs  144250   14274.8 vs 14183.7  (delta = 0.6%)
   1000    2048     2048 :   142258 vs  147275   14389.3 vs 13899.2  (delta = 3.5%)
   1000    4000     4000 :   257353 vs  199637   15539.0 vs 20031.4  (delta = -22.4%)
    500   10000    10000 :   305155 vs  217864   16383.5 vs 22947.8  (delta = -28.6%)
    250   12288    12288 :   188576 vs  136036   16289.2 vs 22580.4  (delta = -27.9%)
    200   16384    16384 :   289248 vs  324498   11328.0 vs 10097.4  (delta = 12.2%)
    100   40000    40000 :   455906 vs  679435    8773.5 vs  5887.1  (delta = 49.0%)
     50   80000    80000 :   482370 vs  729885    8292.3 vs  5480.2  (delta = 51.3%)
                            6453003    5786618
Bits-per-element = 16
 Trials  Length   Nbytes           µsec              bytes/µsec
  12500       4        8 :   188466 vs  157157     397.9 vs   477.2  (delta = -16.6%)
  12500      10       20 :   210858 vs  197414    1067.1 vs  1139.7  (delta = -6.4%)
  10000      20       40 :   179478 vs  147188    2117.3 vs  2581.7  (delta = -18.0%)
  10000      32       64 :   200994 vs  180875    3084.7 vs  3427.8  (delta = -10.0%)
  10000      50      100 :   240261 vs  205667    4078.9 vs  4765.0  (delta = -14.4%)
   7500      64      128 :   194142 vs  157383    4867.6 vs  6004.5  (delta = -18.9%)
   5000     128      256 :   186972 vs  144972    6792.5 vs  8760.3  (delta = -22.5%)
   5000     200      400 :   252060 vs  211582    7894.9 vs  9405.3  (delta = -16.1%)
   5000     256      512 :   277919 vs  247101    9175.3 vs 10319.7  (delta = -11.1%)
   2500     500     1000 :   200142 vs  182408   12466.1 vs 13678.1  (delta = -8.9%)
   2500     512     1024 :   199862 vs  188715   12783.8 vs 13538.9  (delta = -5.6%)
   1500    1000     2000 :   208101 vs  181708   14401.7 vs 16493.5  (delta = -12.7%)
   1250    1024     2048 :   178020 vs  154346   14366.4 vs 16569.9  (delta = -13.3%)
    500    2000     4000 :   128795 vs  103051   15520.8 vs 19398.2  (delta = -20.0%)
    500    2047     4094 :   136146 vs  101589   15028.0 vs 20140.0  (delta = -25.4%)
    500    2048     4096 :   132086 vs  100875   15497.5 vs 20292.4  (delta = -23.6%)
    500    4000     8000 :   244720 vs  181683   16341.1 vs 22010.9  (delta = -25.8%)
    250   10000    20000 :   415488 vs  409652   12032.8 vs 12204.3  (delta = -1.4%)
    125   12288    24576 :   285116 vs  285427   10773.7 vs 10761.9  (delta = 0.1%)
    100   16384    32768 :   360333 vs  359494    9093.3 vs  9114.5  (delta = -0.2%)
     50   40000    80000 :   483432 vs  494141    8274.0 vs  8094.7  (delta = 2.2%)
     25   80000   160000 :   478848 vs  484554    8353.3 vs  8254.9  (delta = 1.2%)
                            5382239    4876982
Bits-per-element = 32
 Trials  Length   Nbytes           µsec              bytes/µsec
   6250       4       16 :    87510 vs   78519     857.0 vs   955.2  (delta = -10.3%)
   6250      10       40 :    96543 vs   83029    2330.6 vs  2709.9  (delta = -14.0%)
   5000      20       80 :    95159 vs   85402    3993.3 vs  4449.5  (delta = -10.3%)
   5000      32      128 :   116695 vs   95313    5313.0 vs  6504.9  (delta = -18.3%)
   5000      50      200 :   149232 vs  112926    6567.0 vs  8678.2  (delta = -24.3%)
   3750      64      256 :   130864 vs  103255    7221.2 vs  9152.1  (delta = -21.1%)
   2500     128      512 :   136130 vs  119495    9329.3 vs 10628.1  (delta = -12.2%)
   2500     200      800 :   170353 vs  166190   11681.6 vs 11974.2  (delta = -2.4%)
   2500     256     1024 :   197501 vs  185809   12911.3 vs 13723.8  (delta = -5.9%)
   1250     500     2000 :   171833 vs  150050   14519.9 vs 16627.8  (delta = -12.7%)
   1250     512     2048 :   177133 vs  152510   14424.2 vs 16753.0  (delta = -13.9%)
    750    1000     4000 :   192298 vs  153840   15585.2 vs 19481.3  (delta = -20.0%)
    625    1024     4096 :   164689 vs  125915   15529.3 vs 20311.3  (delta = -23.5%)
    250    2000     8000 :   122326 vs   90169   16341.6 vs 22169.5  (delta = -26.3%)
    250    2047     8188 :   126611 vs   91282   16159.7 vs 22414.1  (delta = -27.9%)
    250    2048     8192 :   125556 vs   91128   16303.5 vs 22462.9  (delta = -27.4%)
    250    4000    16000 :   261581 vs  186813   15287.8 vs 21406.4  (delta = -28.6%)
    125   10000    40000 :   591045 vs  597900    8458.7 vs  8361.8  (delta = 1.2%)
     62   12288    49152 :   354409 vs  358780    8597.9 vs  8493.2  (delta = 1.2%)
     50   16384    65536 :   395194 vs  402330    8291.1 vs  8144.1  (delta = 1.8%)
     25   40000   160000 :   492693 vs  487650    8118.4 vs  8202.4  (delta = -1.0%)
     12   80000   320000 :   458683 vs  465661    8371.7 vs  8246.2  (delta = 1.5%)
                            4814038    4383966
Bits-per-element = 64
 Trials  Length   Nbytes           µsec              bytes/µsec
   3125       4       32 :    44894 vs   29158    1670.6 vs  2572.2  (delta = -35.1%)
   3125      10       80 :    58334 vs   36660    3857.1 vs  6137.5  (delta = -37.2%)
   2500      20      160 :    64587 vs   40082    5883.5 vs  9480.6  (delta = -37.9%)
   2500      32      256 :    86324 vs   59180    7182.2 vs 10476.5  (delta = -31.4%)
   2500      50      400 :   125126 vs   91795    7832.1 vs 10676.0  (delta = -26.6%)
   1875      64      512 :   103385 vs   80754    9140.6 vs 11702.2  (delta = -21.9%)
   1250     128     1024 :    99136 vs   88014   12810.7 vs 14429.5  (delta = -11.2%)
   1250     200     1600 :   140342 vs  122598   14179.6 vs 16231.9  (delta = -12.6%)
   1250     256     2048 :   175534 vs  146547   14527.1 vs 17400.6  (delta = -16.5%)
    625     500     4000 :   159574 vs  124617   15635.4 vs 20021.3  (delta = -21.9%)
    625     512     4096 :   164799 vs  123089   15503.7 vs 20757.3  (delta = -25.3%)
    375    1000     8000 :   183164 vs  134339   16362.4 vs 22309.2  (delta = -26.7%)
    312    1024     8192 :   156249 vs  112459   16341.9 vs 22705.2  (delta = -28.0%)
    125    2000    16000 :   131522 vs   92910   15199.0 vs 21515.4  (delta = -29.4%)
    125    2047    16376 :   150736 vs  112639   13573.4 vs 18164.2  (delta = -25.3%)
    125    2048    16384 :   148993 vs  112833   13738.9 vs 18141.9  (delta = -24.3%)
    125    4000    32000 :   447050 vs  449169    8945.3 vs  8903.1  (delta = 0.5%)
     62   10000    80000 :   604220 vs  610829    8208.1 vs  8119.3  (delta = 1.1%)
     31   12288    98304 :   370689 vs  372903    8220.3 vs  8171.5  (delta = 0.6%)
     25   16384   131072 :   396778 vs  402838    8258.0 vs  8133.8  (delta = 1.5%)
     12   40000   320000 :   460090 vs  467245    8346.0 vs  8218.2  (delta = 1.6%)
      6   80000   640000 :   458154 vs  464815    8381.4 vs  8261.2  (delta = 1.5%)
                            4729680    4275473

MacBook Pro 2.8 GHz
-------------------
Bits-per-element = 8
 Trials  Length   Nbytes           µsec              bytes/µsec
  25000       4        4 :   241124 vs  235024     311.0 vs   319.1  (delta = -2.5%)
  25000      10       10 :   284991 vs  222553     789.5 vs  1011.0  (delta = -21.9%)
  20000      20       20 :   240778 vs  204466    1578.2 vs  1858.5  (delta = -15.1%)
  20000      32       32 :   214384 vs  239579    2892.0 vs  2587.9  (delta = 11.8%)
  20000      50       50 :   290911 vs  213628    3368.7 vs  4587.4  (delta = -26.6%)
  15000      64       64 :   181844 vs  189332    5196.8 vs  4991.2  (delta = 4.1%)
  10000     128      128 :   139333 vs   94375    9114.9 vs 13457.0  (delta = -32.3%)
  10000     200      200 :   170438 vs  112016   11675.8 vs 17765.3  (delta = -34.3%)
  10000     256      256 :   185942 vs  116194   13714.0 vs 21946.1  (delta = -37.5%)
   5000     500      500 :   150483 vs   97458   16579.9 vs 25600.8  (delta = -35.2%)
   5000     512      512 :   153279 vs   95656   16668.9 vs 26710.3  (delta = -37.6%)
   3000    1000     1000 :   175584 vs   98628   17068.8 vs 30386.9  (delta = -43.8%)
   2500    1024     1024 :   145547 vs   84248   17571.6 vs 30356.8  (delta = -42.1%)
   1000    2000     2000 :   104843 vs   61019   19066.6 vs 32760.3  (delta = -41.8%)
   1000    2047     2047 :   105760 vs   62367   19345.7 vs 32805.8  (delta = -41.0%)
   1000    2048     2048 :   106194 vs   59407   19276.0 vs 34457.2  (delta = -44.1%)
   1000    4000     4000 :   224683 vs  145214   17798.4 vs 27538.7  (delta = -35.4%)
    500   10000    10000 :   413132 vs  387939   12101.5 vs 12887.3  (delta = -6.1%)
    250   12288    12288 :   278558 vs  248856   11027.3 vs 12343.5  (delta = -10.7%)
    200   16384    16384 :   277809 vs  284423   11794.4 vs 11520.2  (delta = 2.4%)
    100   40000    40000 :   362933 vs  277270   11021.0 vs 14426.0  (delta = -23.6%)
     50   80000    80000 :   491677 vs  156314    8135.3 vs 25589.2  (delta = -68.2%)
                            4940227    3685966
Bits-per-element = 16
 Trials  Length   Nbytes           µsec              bytes/µsec
  12500       4        8 :   125226 vs   98318     598.9 vs   762.8  (delta = -21.5%)
  12500      10       20 :   151405 vs  123576    1486.1 vs  1820.7  (delta = -18.4%)
  10000      20       40 :   122822 vs   98649    3093.9 vs  3852.0  (delta = -19.7%)
  10000      32       64 :   132505 vs  136608    4679.1 vs  4538.5  (delta = 3.1%)
  10000      50      100 :   156368 vs  131965    6267.3 vs  7426.2  (delta = -15.6%)
   7500      64      128 :   115273 vs  109809    8197.9 vs  8605.9  (delta = -4.7%)
   5000     128      256 :   101346 vs   97659   12531.3 vs 13004.4  (delta = -3.6%)
   5000     200      400 :   125164 vs  151169   15899.1 vs 13164.1  (delta = 20.8%)
   5000     256      512 :   149171 vs  172185   17094.5 vs 14809.7  (delta = 15.4%)
   2500     500     1000 :   150690 vs  143385   16557.2 vs 17400.7  (delta = -4.8%)
   2500     512     1024 :   140534 vs  179533   18180.7 vs 14231.4  (delta = 27.8%)
   1500    1000     2000 :   142285 vs  152148   21063.4 vs 19697.9  (delta = 6.9%)
   1250    1024     2048 :   132701 vs   97136   19272.7 vs 26329.1  (delta = -26.8%)
    500    2000     4000 :   121027 vs  132547   16517.0 vs 15081.4  (delta = 9.5%)
    500    2047     4094 :   122150 vs   65675   16749.9 vs 31153.4  (delta = -46.2%)
    500    2048     4096 :   121346 vs   72147   16869.1 vs 28372.6  (delta = -40.5%)
    500    4000     8000 :   256563 vs  210134   15586.8 vs 19030.7  (delta = -18.1%)
    250   10000    20000 :   362402 vs  361780   13795.5 vs 13819.2  (delta = -0.2%)
    125   12288    24576 :   238884 vs  222861   12858.8 vs 13783.3  (delta = -6.7%)
    100   16384    32768 :   251845 vs  259412   13010.4 vs 12630.9  (delta = 3.0%)
     50   40000    80000 :   346147 vs  351812   11555.5 vs 11369.4  (delta = 1.6%)
     25   80000   160000 :   359399 vs  353127   11129.6 vs 11327.2  (delta = -1.7%)
                            3925253    3721635
Bits-per-element = 32
 Trials  Length   Nbytes           µsec              bytes/µsec
   6250       4       16 :    58000 vs   43650    1293.1 vs  1718.2  (delta = -24.7%)
   6250      10       40 :    63418 vs   48231    3547.9 vs  4665.0  (delta = -23.9%)
   5000      20       80 :    60020 vs   67657    6331.2 vs  5616.6  (delta = 12.7%)
   5000      32      128 :    83557 vs   73595    7420.1 vs  8424.5  (delta = -11.9%)
   5000      50      200 :    84117 vs   87721   11650.4 vs 11171.8  (delta = 4.3%)
   3750      64      256 :    68265 vs   79282   13843.1 vs 11919.5  (delta = 16.1%)
   2500     128      512 :    79873 vs  105890   15900.2 vs 11993.6  (delta = 32.6%)
   2500     200      800 :   125618 vs  159192   15841.7 vs 12500.6  (delta = 26.7%)
   2500     256     1024 :   151868 vs  169703   16790.9 vs 15026.3  (delta = 11.7%)
   1250     500     2000 :   137886 vs  112946   18094.7 vs 22090.2  (delta = -18.1%)
   1250     512     2048 :   127360 vs  118789   20061.2 vs 21508.7  (delta = -6.7%)
    750    1000     4000 :   159666 vs  168467   18770.4 vs 17789.8  (delta = 5.5%)
    625    1024     4096 :   146984 vs   86450   17399.9 vs 29583.6  (delta = -41.2%)
    250    2000     8000 :   121293 vs  104969   16480.8 vs 19043.7  (delta = -13.5%)
    250    2047     8188 :   142908 vs  129628   14316.9 vs 15783.6  (delta = -9.3%)
    250    2048     8192 :   135491 vs  147282   15108.0 vs 13898.5  (delta = 8.7%)
    250    4000    16000 :   340307 vs  217186   11751.2 vs 18412.8  (delta = -36.2%)
    125   10000    40000 :   530268 vs  450367    9428.3 vs 11100.9  (delta = -15.1%)
     62   12288    49152 :   276636 vs  358682   11015.1 vs  8495.5  (delta = 29.7%)
     50   16384    65536 :   440909 vs  436411    7431.5 vs  7508.1  (delta = -1.0%)
     25   40000   160000 :   551277 vs  529972    7255.7 vs  7547.4  (delta = -3.9%)
     12   80000   320000 :   396323 vs  344542    9688.9 vs 11145.1  (delta = -13.1%)
                            4282044    4040612
Bits-per-element = 64
 Trials  Length   Nbytes           µsec              bytes/µsec
   3125       4       32 :    32417 vs   29509    2313.6 vs  2541.6  (delta = -9.0%)
   3125      10       80 :    36416 vs   28177    6178.6 vs  7985.2  (delta = -22.6%)
   2500      20      160 :    39175 vs   30829    9700.1 vs 12326.1  (delta = -21.3%)
   2500      32      256 :    51528 vs   44465   12032.3 vs 13943.6  (delta = -13.7%)
   2500      50      400 :    66053 vs   76237   14836.6 vs 12854.7  (delta = 15.4%)
   1875      64      512 :    57077 vs   56099   16556.6 vs 16845.2  (delta = -1.7%)
   1250     128     1024 :    68693 vs  103993   18488.1 vs 12212.4  (delta = 51.4%)
   1250     200     1600 :   106097 vs  131459   18756.4 vs 15137.8  (delta = 23.9%)
   1250     256     2048 :   128902 vs   84460   19782.5 vs 30191.8  (delta = -34.5%)
    625     500     4000 :   122616 vs   79560   20348.1 vs 31360.0  (delta = -35.1%)
    625     512     4096 :   175035 vs  125099   14597.1 vs 20423.8  (delta = -28.5%)
    375    1000     8000 :   193780 vs  167046   15466.0 vs 17941.2  (delta = -13.8%)
    312    1024     8192 :   191722 vs  131477   13318.3 vs 19420.9  (delta = -31.4%)
    125    2000    16000 :   163235 vs  103620   12246.1 vs 19291.6  (delta = -36.5%)
    125    2047    16376 :   150104 vs   91885   13630.5 vs 22267.0  (delta = -38.8%)
    125    2048    16384 :   182256 vs   93088   11231.5 vs 21989.9  (delta = -48.9%)
    125    4000    32000 :   329766 vs  342134   12126.8 vs 11688.4  (delta = 3.8%)
     62   10000    80000 :   447914 vs  413111   11072.4 vs 12005.3  (delta = -7.8%)
     31   12288    98304 :   255453 vs  250743   11928.5 vs 12152.6  (delta = -1.8%)
     25   16384   131072 :   281138 vs  277646   11654.8 vs 11801.4  (delta = -1.2%)
     12   40000   320000 :   350741 vs  319268   10948.0 vs 12027.2  (delta = -9.0%)
      6   80000   640000 :   332966 vs  313419   11532.6 vs 12251.8  (delta = -5.9%)
                            3763084    3293324

|#

#|
This is a comparison of the new UB<n>-BASH-COPY versus old,
for n = {8,16,32,64}. I produced this by temporarily naming the new
function to NEW-UB<n>-BASH-COPY so that both could coexist.

Testing bits-per-element = 8
Testing array len 10, 20000 iterations:
  TEST-ALIGNED             :  343998 vs  287999 (delta = -16.3%)
  TEST-MISALIGNED          :  515998 vs  271999 (delta = -47.3%)
  TEST-SAME-ARRAY-1        :  507998 vs  255998 (delta = -49.6%)
  TEST-SAME-ARRAY-2        :  351999 vs  259999 (delta = -26.1%)
Testing array len 20, 20000 iterations:
  TEST-ALIGNED             :  359997 vs  356000 (delta = -1.1%)
  TEST-MISALIGNED          :  563997 vs  279998 (delta = -50.4%)
  TEST-SAME-ARRAY-1        :  547999 vs  259997 (delta = -52.6%)
  TEST-SAME-ARRAY-2        :  471999 vs  255999 (delta = -45.8%)
Testing array len 32, 15000 iterations:
  TEST-ALIGNED             :  279998 vs  240000 (delta = -14.3%)
  TEST-MISALIGNED          :  459998 vs  199999 (delta = -56.5%)
  TEST-SAME-ARRAY-1        :  451998 vs  191999 (delta = -57.5%)
  TEST-SAME-ARRAY-2        :  419997 vs  187999 (delta = -55.2%)
Testing array len 50, 10000 iterations:
  TEST-ALIGNED             :  212001 vs  191999 (delta = -9.4%)
  TEST-MISALIGNED          :  383998 vs  151998 (delta = -60.4%)
  TEST-SAME-ARRAY-1        :  376000 vs  143999 (delta = -61.7%)
  TEST-SAME-ARRAY-2        :  343998 vs  139999 (delta = -59.3%)
Testing array len 64, 10000 iterations:
  TEST-ALIGNED             :  232000 vs  203999 (delta = -12.1%)
  TEST-MISALIGNED          :  403997 vs  148000 (delta = -63.4%)
  TEST-SAME-ARRAY-1        :  399999 vs  143999 (delta = -64.0%)
  TEST-SAME-ARRAY-2        :  379997 vs  140000 (delta = -63.2%)
Testing array len 128, 8000 iterations:
  TEST-ALIGNED             :  252000 vs  115999 (delta = -54.0%)
  TEST-MISALIGNED          :  483997 vs  135999 (delta = -71.9%)
  TEST-SAME-ARRAY-1        :  483999 vs  120000 (delta = -75.2%)
  TEST-SAME-ARRAY-2        :  467998 vs  123999 (delta = -73.5%)
Testing array len 200, 8000 iterations:
  TEST-ALIGNED             :  327999 vs  147999 (delta = -54.9%)
  TEST-MISALIGNED          :  659997 vs  167999 (delta = -74.5%)
  TEST-SAME-ARRAY-1        :  663997 vs  164000 (delta = -75.3%)
  TEST-SAME-ARRAY-2        :  647996 vs  164000 (delta = -74.7%)
Testing array len 256, 8000 iterations:
  TEST-ALIGNED             :  443998 vs  171999 (delta = -61.3%)
  TEST-MISALIGNED          :  843997 vs  195998 (delta = -76.8%)
  TEST-SAME-ARRAY-1        :  875997 vs  195998 (delta = -77.6%)
  TEST-SAME-ARRAY-2        :  847997 vs  191999 (delta = -77.4%)
Testing array len 500, 1000 iterations:
  TEST-ALIGNED             :   87999 vs   40001 (delta = -54.5%)
  TEST-MISALIGNED          :  183999 vs   44000 (delta = -76.1%)
  TEST-SAME-ARRAY-1        :  187999 vs   40000 (delta = -78.7%)
  TEST-SAME-ARRAY-2        :  183999 vs   44000 (delta = -76.1%)
Testing array len 512, 1000 iterations:
  TEST-ALIGNED             :   87999 vs   39999 (delta = -54.5%)
  TEST-MISALIGNED          :  188001 vs   39999 (delta = -78.7%)
  TEST-SAME-ARRAY-1        :  192000 vs   39999 (delta = -79.2%)
  TEST-SAME-ARRAY-2        :  188000 vs   39999 (delta = -78.7%)
Testing array len 1000, 500 iterations:
  TEST-ALIGNED             :   76000 vs   36000 (delta = -52.6%)
  TEST-MISALIGNED          :  171998 vs   36000 (delta = -79.1%)
  TEST-SAME-ARRAY-1        :  176000 vs   36000 (delta = -79.5%)
  TEST-SAME-ARRAY-2        :  171999 vs   36000 (delta = -79.1%)
Testing array len 1024, 500 iterations:
  TEST-ALIGNED             :   80000 vs   35999 (delta = -55.0%)
  TEST-MISALIGNED          :  176000 vs   36000 (delta = -79.5%)
  TEST-SAME-ARRAY-1        :  175998 vs   40000 (delta = -77.3%)
  TEST-SAME-ARRAY-2        :  176000 vs   36000 (delta = -79.5%)
Testing array len 2000, 200 iterations:
  TEST-ALIGNED             :   59999 vs   24000 (delta = -60.0%)
  TEST-MISALIGNED          :  132000 vs   24000 (delta = -81.8%)
  TEST-SAME-ARRAY-1        :  135999 vs   28000 (delta = -79.4%)
  TEST-SAME-ARRAY-2        :  131999 vs   24000 (delta = -81.8%)
Testing array len 2048, 200 iterations:
  TEST-ALIGNED             :   60000 vs   24000 (delta = -60.0%)
  TEST-MISALIGNED          :  135998 vs   28000 (delta = -79.4%)
  TEST-SAME-ARRAY-1        :  136000 vs   28000 (delta = -79.4%)
  TEST-SAME-ARRAY-2        :  136000 vs   28000 (delta = -79.4%)
Testing array len 4000, 100 iterations:
  TEST-ALIGNED             :   56000 vs   20000 (delta = -64.3%)
  TEST-MISALIGNED          :  128000 vs   23999 (delta = -81.3%)
  TEST-SAME-ARRAY-1        :  128000 vs   28000 (delta = -78.1%)
  TEST-SAME-ARRAY-2        :  127998 vs   32000 (delta = -75.0%)

Testing bits-per-element = 16
Testing array len 10, 20000 iterations:
  TEST-ALIGNED             :  351999 vs  299999 (delta = -14.8%)
  TEST-MISALIGNED          :  403997 vs  328000 (delta = -18.8%)
  TEST-SAME-ARRAY-1        :  387998 vs  367998 (delta = -5.2%)
  TEST-SAME-ARRAY-2        :  379999 vs  323999 (delta = -14.7%)
Testing array len 20, 20000 iterations:
  TEST-ALIGNED             :  367999 vs  351998 (delta = -4.3%)
  TEST-MISALIGNED          :  435997 vs  396000 (delta = -9.2%)
  TEST-SAME-ARRAY-1        :  423998 vs  407998 (delta = -3.8%)
  TEST-SAME-ARRAY-2        :  395998 vs  379998 (delta = -4.0%)
Testing array len 32, 15000 iterations:
  TEST-ALIGNED             :  319999 vs  311999 (delta = -2.5%)
  TEST-MISALIGNED          :  375997 vs  344000 (delta = -8.5%)
  TEST-SAME-ARRAY-1        :  367998 vs  355997 (delta = -3.3%)
  TEST-SAME-ARRAY-2        :  340000 vs  335998 (delta = -1.2%)
Testing array len 50, 10000 iterations:
  TEST-ALIGNED             :  279998 vs  256000 (delta = -8.6%)
  TEST-MISALIGNED          :  307999 vs  279998 (delta = -9.1%)
  TEST-SAME-ARRAY-1        :  303999 vs  303999 (delta = 0.0%)
  TEST-SAME-ARRAY-2        :  291999 vs  271997 (delta = -6.9%)
Testing array len 64, 10000 iterations:
  TEST-ALIGNED             :  304000 vs  139999 (delta = -53.9%)
  TEST-MISALIGNED          :  343999 vs  311998 (delta = -9.3%)
  TEST-SAME-ARRAY-1        :  339999 vs  319999 (delta = -5.9%)
  TEST-SAME-ARRAY-2        :  315997 vs  308000 (delta = -2.5%)
Testing array len 128, 8000 iterations:
  TEST-ALIGNED             :  431998 vs  175998 (delta = -59.3%)
  TEST-MISALIGNED          :  423999 vs  211999 (delta = -50.0%)
  TEST-SAME-ARRAY-1        :  423997 vs  204001 (delta = -51.9%)
  TEST-SAME-ARRAY-2        :  391998 vs  195998 (delta = -50.0%)
Testing array len 200, 8000 iterations:
  TEST-ALIGNED             :  583998 vs  255999 (delta = -56.2%)
  TEST-MISALIGNED          :  671997 vs  283999 (delta = -57.7%)
  TEST-SAME-ARRAY-1        :  663997 vs  283999 (delta = -57.2%)
  TEST-SAME-ARRAY-2        :  595997 vs  271999 (delta = -54.4%)
Testing array len 256, 8000 iterations:
  TEST-ALIGNED             :  707997 vs  295999 (delta = -58.2%)
  TEST-MISALIGNED          :  799996 vs  331999 (delta = -58.5%)
  TEST-SAME-ARRAY-1        :  799996 vs  339999 (delta = -57.5%)
  TEST-SAME-ARRAY-2        :  719996 vs  331999 (delta = -53.9%)
Testing array len 500, 1000 iterations:
  TEST-ALIGNED             :  155999 vs   72000 (delta = -53.8%)
  TEST-MISALIGNED          :  168000 vs   76000 (delta = -54.8%)
  TEST-SAME-ARRAY-1        :  175999 vs   76000 (delta = -56.8%)
  TEST-SAME-ARRAY-2        :  155999 vs   76000 (delta = -51.3%)
Testing array len 512, 1000 iterations:
  TEST-ALIGNED             :  159998 vs   68000 (delta = -57.5%)
  TEST-MISALIGNED          :  176000 vs   72000 (delta = -59.1%)
  TEST-SAME-ARRAY-1        :  179999 vs   75999 (delta = -57.8%)
  TEST-SAME-ARRAY-2        :  156000 vs   75999 (delta = -51.3%)
Testing array len 1000, 500 iterations:
  TEST-ALIGNED             :  147999 vs   59999 (delta = -59.5%)
  TEST-MISALIGNED          :  160001 vs   63999 (delta = -60.0%)
  TEST-SAME-ARRAY-1        :  164000 vs   71999 (delta = -56.1%)
  TEST-SAME-ARRAY-2        :  144000 vs   67999 (delta = -52.8%)
Testing array len 1024, 500 iterations:
  TEST-ALIGNED             :  148000 vs   63998 (delta = -56.8%)
  TEST-MISALIGNED          :  160000 vs   68000 (delta = -57.5%)
  TEST-SAME-ARRAY-1        :  168000 vs   71999 (delta = -57.1%)
  TEST-SAME-ARRAY-2        :  148000 vs   71999 (delta = -51.4%)
Testing array len 2000, 200 iterations:
  TEST-ALIGNED             :  112000 vs   40000 (delta = -64.3%)
  TEST-MISALIGNED          :  123998 vs   44000 (delta = -64.5%)
  TEST-SAME-ARRAY-1        :  123999 vs   52001 (delta = -58.1%)
  TEST-SAME-ARRAY-2        :  116000 vs   59999 (delta = -48.3%)
Testing array len 2048, 200 iterations:
  TEST-ALIGNED             :  116000 vs   40000 (delta = -65.5%)
  TEST-MISALIGNED          :  123999 vs   52000 (delta = -58.1%)
  TEST-SAME-ARRAY-1        :  127999 vs   56000 (delta = -56.2%)
  TEST-SAME-ARRAY-2        :  111999 vs   63999 (delta = -42.9%)
Testing array len 4000, 100 iterations:
  TEST-ALIGNED             :  108001 vs   36000 (delta = -66.7%)
  TEST-MISALIGNED          :  115999 vs   40000 (delta = -65.5%)
  TEST-SAME-ARRAY-1        :  123999 vs   52000 (delta = -58.1%)
  TEST-SAME-ARRAY-2        :  112000 vs   55999 (delta = -50.0%)

Testing bits-per-element = 32
Testing array len 10, 20000 iterations:
  TEST-ALIGNED             :  379998 vs  351999 (delta = -7.4%)
  TEST-MISALIGNED          :  403998 vs  335999 (delta = -16.8%)
  TEST-SAME-ARRAY-1        :  387998 vs  359998 (delta = -7.2%)
  TEST-SAME-ARRAY-2        :  351999 vs  343998 (delta = -2.3%)
Testing array len 20, 20000 iterations:
  TEST-ALIGNED             :  487997 vs  455999 (delta = -6.6%)
  TEST-MISALIGNED          :  539997 vs  447999 (delta = -17.0%)
  TEST-SAME-ARRAY-1        :  523997 vs  451999 (delta = -13.7%)
  TEST-SAME-ARRAY-2        :  451997 vs  451999 (delta = 0.0%)
Testing array len 32, 15000 iterations:
  TEST-ALIGNED             :  463997 vs  212000 (delta = -54.3%)
  TEST-MISALIGNED          :  531997 vs  435997 (delta = -18.0%)
  TEST-SAME-ARRAY-1        :  519999 vs  435997 (delta = -16.2%)
  TEST-SAME-ARRAY-2        :  435999 vs  439997 (delta = 0.9%)
Testing array len 50, 10000 iterations:
  TEST-ALIGNED             :  404000 vs  179999 (delta = -55.4%)
  TEST-MISALIGNED          :  483997 vs  208000 (delta = -57.0%)
  TEST-SAME-ARRAY-1        :  479998 vs  199998 (delta = -58.3%)
  TEST-SAME-ARRAY-2        :  387999 vs  199999 (delta = -48.5%)
Testing array len 64, 10000 iterations:
  TEST-ALIGNED             :  543997 vs  224000 (delta = -58.8%)
  TEST-MISALIGNED          :  583997 vs  259998 (delta = -55.5%)
  TEST-SAME-ARRAY-1        :  575998 vs  243998 (delta = -57.6%)
  TEST-SAME-ARRAY-2        :  463999 vs  235999 (delta = -49.1%)
Testing array len 128, 8000 iterations:
  TEST-ALIGNED             :  715997 vs  295999 (delta = -58.7%)
  TEST-MISALIGNED          :  819996 vs  319999 (delta = -61.0%)
  TEST-SAME-ARRAY-1        :  815996 vs  331998 (delta = -59.3%)
  TEST-SAME-ARRAY-2        :  703998 vs  319997 (delta = -54.5%)
Testing array len 200, 8000 iterations:
  TEST-ALIGNED             : 1023996 vs  463999 (delta = -54.7%)
  TEST-MISALIGNED          : 1139995 vs  483997 (delta = -57.5%)
  TEST-SAME-ARRAY-1        : 1147996 vs  487997 (delta = -57.5%)
  TEST-SAME-ARRAY-2        : 1019996 vs  471998 (delta = -53.7%)
Testing array len 256, 8000 iterations:
  TEST-ALIGNED             : 1259994 vs  559997 (delta = -55.6%)
  TEST-MISALIGNED          : 1387995 vs  575996 (delta = -58.5%)
  TEST-SAME-ARRAY-1        : 1403995 vs  595996 (delta = -57.5%)
  TEST-SAME-ARRAY-2        : 1255996 vs  571997 (delta = -54.5%)
Testing array len 500, 1000 iterations:
  TEST-ALIGNED             :  288000 vs  127999 (delta = -55.6%)
  TEST-MISALIGNED          :  307999 vs  124000 (delta = -59.7%)
  TEST-SAME-ARRAY-1        :  311997 vs  132001 (delta = -57.7%)
  TEST-SAME-ARRAY-2        :  291998 vs  128000 (delta = -56.2%)
Testing array len 512, 1000 iterations:
  TEST-ALIGNED             :  295997 vs  128000 (delta = -56.8%)
  TEST-MISALIGNED          :  315999 vs  128000 (delta = -59.5%)
  TEST-SAME-ARRAY-1        :  319998 vs  139999 (delta = -56.3%)
  TEST-SAME-ARRAY-2        :  296000 vs  135999 (delta = -54.1%)
Testing array len 1000, 500 iterations:
  TEST-ALIGNED             :  279999 vs   99999 (delta = -64.3%)
  TEST-MISALIGNED          :  291998 vs  108000 (delta = -63.0%)
  TEST-SAME-ARRAY-1        :  299999 vs  124000 (delta = -58.7%)
  TEST-SAME-ARRAY-2        :  283998 vs  151999 (delta = -46.5%)
Testing array len 1024, 500 iterations:
  TEST-ALIGNED             :  291999 vs  104000 (delta = -64.4%)
  TEST-MISALIGNED          :  295999 vs  123999 (delta = -58.1%)
  TEST-SAME-ARRAY-1        :  303999 vs  131999 (delta = -56.6%)
  TEST-SAME-ARRAY-2        :  283999 vs  155999 (delta = -45.1%)
Testing array len 2000, 200 iterations:
  TEST-ALIGNED             :  219998 vs   72000 (delta = -67.3%)
  TEST-MISALIGNED          :  224000 vs   79999 (delta = -64.3%)
  TEST-SAME-ARRAY-1        :  231999 vs  100000 (delta = -56.9%)
  TEST-SAME-ARRAY-2        :  219998 vs  112000 (delta = -49.1%)
Testing array len 2048, 200 iterations:
  TEST-ALIGNED             :  224000 vs   76000 (delta = -66.1%)
  TEST-MISALIGNED          :  235999 vs   88000 (delta = -62.7%)
  TEST-SAME-ARRAY-1        :  235998 vs  103999 (delta = -55.9%)
  TEST-SAME-ARRAY-2        :  224000 vs  116000 (delta = -48.2%)
Testing array len 4000, 100 iterations:
  TEST-ALIGNED             :  215999 vs   76000 (delta = -64.8%)
  TEST-MISALIGNED          :  227998 vs   88000 (delta = -61.4%)
  TEST-SAME-ARRAY-1        :  232000 vs  103999 (delta = -55.2%)
  TEST-SAME-ARRAY-2        :  223999 vs  112000 (delta = -50.0%)

Testing bits-per-element = 64
Testing array len 10, 20000 iterations:
  TEST-ALIGNED             :  423997 vs  399999 (delta = -5.7%)
  TEST-MISALIGNED          :  347999 vs  327998 (delta = -5.7%)
  TEST-SAME-ARRAY-1        :  335999 vs  339998 (delta = 1.2%)
  TEST-SAME-ARRAY-2        :  339999 vs  343998 (delta = 1.2%)
Testing array len 20, 20000 iterations:
  TEST-ALIGNED             :  639997 vs  351999 (delta = -45.0%)
  TEST-MISALIGNED          :  571997 vs  547998 (delta = -4.2%)
  TEST-SAME-ARRAY-1        :  567997 vs  551998 (delta = -2.8%)
  TEST-SAME-ARRAY-2        :  555997 vs  559997 (delta = 0.7%)
Testing array len 32, 15000 iterations:
  TEST-ALIGNED             :  783998 vs  343997 (delta = -56.1%)
  TEST-MISALIGNED          :  647998 vs  383997 (delta = -40.7%)
  TEST-SAME-ARRAY-1        :  651999 vs  367997 (delta = -43.6%)
  TEST-SAME-ARRAY-2        :  611998 vs  351998 (delta = -42.5%)
Testing array len 50, 10000 iterations:
  TEST-ALIGNED             :  719998 vs  323998 (delta = -55.0%)
  TEST-MISALIGNED          :  735997 vs  351998 (delta = -52.2%)
  TEST-SAME-ARRAY-1        :  731996 vs  348000 (delta = -52.5%)
  TEST-SAME-ARRAY-2        :  675996 vs  331999 (delta = -50.9%)
Testing array len 64, 10000 iterations:
  TEST-ALIGNED             :  871996 vs  367999 (delta = -57.8%)
  TEST-MISALIGNED          :  907996 vs  399998 (delta = -55.9%)
  TEST-SAME-ARRAY-1        :  911996 vs  423998 (delta = -53.5%)
  TEST-SAME-ARRAY-2        :  827995 vs  396000 (delta = -52.2%)
Testing array len 128, 8000 iterations:
  TEST-ALIGNED             : 1247994 vs  559998 (delta = -55.1%)
  TEST-MISALIGNED          : 1343994 vs  579997 (delta = -56.8%)
  TEST-SAME-ARRAY-1        : 1391994 vs  595998 (delta = -57.2%)
  TEST-SAME-ARRAY-2        : 1215994 vs  563997 (delta = -53.6%)
Testing array len 200, 8000 iterations:
  TEST-ALIGNED             : 1867992 vs  811997 (delta = -56.5%)
  TEST-MISALIGNED          : 2047991 vs  823996 (delta = -59.8%)
  TEST-SAME-ARRAY-1        : 2127991 vs  875996 (delta = -58.8%)
  TEST-SAME-ARRAY-2        : 1847992 vs  839996 (delta = -54.5%)
Testing array len 256, 8000 iterations:
  TEST-ALIGNED             : 2351990 vs 1015995 (delta = -56.8%)
  TEST-MISALIGNED          : 2587989 vs 1027996 (delta = -60.3%)
  TEST-SAME-ARRAY-1        : 2683987 vs 1107995 (delta = -58.7%)
  TEST-SAME-ARRAY-2        : 2331991 vs 1059995 (delta = -54.5%)
Testing array len 500, 1000 iterations:
  TEST-ALIGNED             :  555996 vs  196001 (delta = -64.7%)
  TEST-MISALIGNED          :  619997 vs  219998 (delta = -64.5%)
  TEST-SAME-ARRAY-1        :  639998 vs  251998 (delta = -60.6%)
  TEST-SAME-ARRAY-2        :  559998 vs  295999 (delta = -47.1%)
Testing array len 512, 1000 iterations:
  TEST-ALIGNED             :  575998 vs  207999 (delta = -63.9%)
  TEST-MISALIGNED          :  635997 vs  239999 (delta = -62.3%)
  TEST-SAME-ARRAY-1        :  647996 vs  264000 (delta = -59.3%)
  TEST-SAME-ARRAY-2        :  571996 vs  300000 (delta = -47.6%)
Testing array len 1000, 500 iterations:
  TEST-ALIGNED             :  547997 vs  171999 (delta = -68.6%)
  TEST-MISALIGNED          :  611998 vs  195999 (delta = -68.0%)
  TEST-SAME-ARRAY-1        :  627997 vs  243999 (delta = -61.1%)
  TEST-SAME-ARRAY-2        :  555998 vs  275999 (delta = -50.4%)
Testing array len 1024, 500 iterations:
  TEST-ALIGNED             :  567996 vs  188000 (delta = -66.9%)
  TEST-MISALIGNED          :  627996 vs  216000 (delta = -65.6%)
  TEST-SAME-ARRAY-1        :  639998 vs  251998 (delta = -60.6%)
  TEST-SAME-ARRAY-2        :  563998 vs  283999 (delta = -49.6%)
Testing array len 2000, 200 iterations:
  TEST-ALIGNED             :  435999 vs  147999 (delta = -66.1%)
  TEST-MISALIGNED          :  495997 vs  172000 (delta = -65.3%)
  TEST-SAME-ARRAY-1        :  507998 vs  203999 (delta = -59.8%)
  TEST-SAME-ARRAY-2        :  443998 vs  219999 (delta = -50.5%)
Testing array len 2048, 200 iterations:
  TEST-ALIGNED             :  459997 vs  208000 (delta = -54.8%)
  TEST-MISALIGNED          :  511998 vs  239998 (delta = -53.1%)
  TEST-SAME-ARRAY-1        :  543998 vs  243999 (delta = -55.1%)
  TEST-SAME-ARRAY-2        :  487998 vs  259999 (delta = -46.7%)
Testing array len 4000, 100 iterations:
  TEST-ALIGNED             :  471997 vs  527999 (delta = 11.9%)
  TEST-MISALIGNED          :  519998 vs  491998 (delta = -5.4%)
  TEST-SAME-ARRAY-1        :  639997 vs  347998 (delta = -45.6%)
  TEST-SAME-ARRAY-2        :  511998 vs  355998 (delta = -30.5%)

|#
