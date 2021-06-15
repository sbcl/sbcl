
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

(defun compare (test arg1 arg2 bits-per-element)
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
               (compare 'test-aligned *array-size* n-iter bits-per-element)
               (compare 'test-misaligned *array-size* n-iter bits-per-element)
               (compare 'test-same-array-1 *array-size* n-iter bits-per-element)
               (compare 'test-same-array-2 *array-size* n-iter bits-per-element)))
    (terpri)))

;;;; MACHINE-DEPENDENT RESULT

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
