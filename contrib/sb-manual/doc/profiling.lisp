(in-package :sb-manual)

(defsection @profiling (:title "Profiling")
  "SBCL includes both a deterministic profiler, that can collect
  statistics on individual functions, and a more \"modern\",
  statistical profiler.

  Inlined functions do not appear in the results reported by either."
  (@deterministic-profiler section)
  (@statistical-profiler section))

(defsection @deterministic-profiler (:title "Deterministic Profiler")
  "The package `SB-PROFILE` provides a classic, per-function-call
  profiler.

  > __Warning__: When profiling code executed by multiple threads in
  > parallel, the consing attributed to each function is inaccurate."
  (sb-profile:profile macro)
  (sb-profile:unprofile macro)
  (sb-profile:report function)
  (sb-profile:reset function))

(defsection @statistical-profiler (:title "Statistical Profiler")
  "The `SB-SPROF` module, loadable by

      (require :sb-sprof)

  provides an alternate profiler which works by taking samples of the
  program execution at regular intervals, instead of instrumenting
  functions as SB-PROFILE:PROFILE does. You might find `SB-SPROF` more
  useful than the deterministic profiler when profiling functions in the
  `COMMON-LISP` package, SBCL internals, or code where the instrumenting
  overhead is excessive.

  Additionally `SB-SPROF` includes a limited deterministic profiler
  which can be used for reporting the amounts of calls to some functions
  during

  __Example usage:__

      (in-package :cl-user)

      (require :sb-sprof)

      (declaim (optimize speed))

      (defun cpu-test-inner (a i)
        (logxor a
                (* i 5)
                (+ a i)))

      (defun cpu-test (n)
        (let ((a 0))
          (dotimes (i (expt 2 n) a)
            (setf a (cpu-test-inner a i)))))

      ;;;; CPU profiling

      ;;; Take up to 1000 samples of running (CPU-TEST 26), and give a flat
      ;;; table report at the end. Profiling will end one the body has been
      ;;; evaluated once, whether or not 1000 samples have been taken.
      (sb-sprof:with-profiling (:max-samples 1000
                                :report :flat
                                :loop nil)
        (cpu-test 26))

      ;;; Record call counts for functions defined on symbols in the CL-USER
      ;;; package.
      (sb-sprof:profile-call-counts \"CL-USER\")

      ;;; Take 1000 samples of running (CPU-TEST 24), and give a flat
      ;;; table report at the end. The body will be re-evaluated in a loop
      ;;; until 1000 samples have been taken. A sample count will be printed
      ;;; after each iteration.
      (sb-sprof:with-profiling (:max-samples 1000
                                :report :flat
                                :loop t
                                :show-progress t)
        (cpu-test 24))

      ;;;; Allocation profiling

      (defun foo (&rest args)
        (mapcar (lambda (x) (float x 1d0)) args))

      (defun bar (n)
        (declare (fixnum n))
        (apply #'foo (loop repeat n collect n)))

      (sb-sprof:with-profiling (:max-samples 10000
                                :mode :alloc
                                :report :flat)
        (bar 1000))

  __Output:__

  The flat report format will show a table of all functions that the
  profiler encountered on the call stack during sampling, ordered by
  the number of samples taken while executing that function.

                 Self        Total        Cumul
        Nr  Count     %  Count     %  Count     %    Calls  Function
      ------------------------------------------------------------------------
         1     69  24.4     97  34.3     69  24.4 67108864  CPU-TEST-INNER
         2     64  22.6     64  22.6    133  47.0        -  SB-VM::GENERIC-+
         3     39  13.8    256  90.5    172  60.8        1  CPU-TEST
         4     31  11.0     31  11.0    203  71.7        -  SB-KERNEL:TWO-ARG-XOR

  For each function, the table will show three absolute and relative
  sample counts. The `Self` column shows samples taken while directly
  executing that function. The `Total` column shows samples taken
  while executing that function or functions called from it (sampled
  to a platform-specific depth). The `Cumul` column shows the sum of
  all `Self` columns up to and including that line in the table.

  Additionally the `Calls` column will record the amount of calls that
  were made to the function during the profiling run. This value will
  only be reported for functions that have been explicitly marked for
  call counting with SB-SPROF:PROFILE-CALL-COUNTS.

  The profiler also hooks into the disassembler such that instructions
  which have been sampled are annotated with their relative frequency
  of sampling. This information is not stored across different
  sampling runs.

      ;      6CF:       702E             JO L4              ; 6/242 samples
      ;      6D1:       D1E3             SHL EBX, 1
      ;      6D3:       702A             JO L4
      ;      6D5: L2:   F6C303           TEST BL, 3         ; 2/242 samples
      ;      6D8:       756D             JNE L8
      ;      6DA:       8BC3             MOV EAX, EBX       ; 5/242 samples
      ;      6DC: L3:   83F900           CMP ECX, 0         ; 4/242 samples

  __Platform support__

  Allocation profiling is only supported on SBCL builds that use the
  generational garbage collector. Tracking of call stacks at a depth
  of more than two levels is only supported on x86 and x86-64.

  __Macros__"
  (sb-sprof:with-profiling macro)
  (sb-sprof:with-sampling macro)
  "__Functions__"
  (sb-sprof:map-traces function)
  (sb-sprof:sample-pc function)
  (sb-sprof:report function)
  (sb-sprof:reset function)
  (sb-sprof:start-profiling function)
  (sb-sprof:stop-profiling function)
  (sb-sprof:profile-call-counts function)
  (sb-sprof:unprofile-call-counts function)
  "__Variables__"
  (sb-sprof:*max-samples* variable)
  (sb-sprof:*sample-interval* variable)
  "__Credits__

  `SB-SPROF` is an SBCL port, with enhancements, of Gerd Moellmann's
  statistical profiler for CMUCL.")
