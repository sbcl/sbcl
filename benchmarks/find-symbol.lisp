(in-package sb-impl)

(defun find-sym-timing (niter &aux (res 0))
  (let* ((pkg (find-package "KEYWORD"))
         (externals (package-external-symbols pkg))
         (vect (remove-if-not #'symbolp (symtbl-cells externals))))
    (dotimes (i niter)
      (dovector (sym vect)
        (let ((str (symbol-name (truly-the symbol sym))))
          (when (find-symbol str pkg)
            (incf res))))))
  res)

(find-sym-timing 5000)

#|
;; This benchmarks shows just how large the effect is of removing
;; one IDIV instruction from the hash -> bin calculation in SYMBOL-HASHSET.
;; As expected, we take a bunch more branches and run more instructions,
;; but the overall instructions/cycle is higher, and CPU time is lower.
;; These results are completely repeatable.

% perf stat path/to/sbcl --noinform --noprint --no-sysinit --no-userinit < benchmarks/find-symbol.lisp

Without fast remainder vop
==========================
          1,963.00 msec task-clock:u              #    0.999 CPUs utilized
                 0      context-switches:u        #    0.000 /sec
                 0      cpu-migrations:u          #    0.000 /sec
             1,094      page-faults:u             #  557.311 /sec
     5,433,686,072      cycles:u                  #    2.768 GHz
     2,067,083,081      stalled-cycles-frontend:u #   38.04% frontend cycles idle
     8,180,495,043      instructions:u            #    1.51  insn per cycle
                                                  #    0.25  stalled cycles per insn
     1,648,928,710      branches:u                #  840.006 M/sec
        36,906,902      branch-misses:u           #    2.24% of all branches

       1.965377442 seconds time elapsed

       1.956049000 seconds user
       0.008000000 seconds sys

With fast remainder vop
=======================
          1,689.64 msec task-clock:u              #    0.999 CPUs utilized
                 0      context-switches:u        #    0.000 /sec
                 0      cpu-migrations:u          #    0.000 /sec
             1,018      page-faults:u             #  602.494 /sec
     4,670,096,246      cycles:u                  #    2.764 GHz
     1,540,757,518      stalled-cycles-frontend:u #   32.99% frontend cycles idle
     8,636,783,323      instructions:u            #    1.85  insn per cycle
                                                  #    0.18  stalled cycles per insn
     1,707,772,211      branches:u                #    1.011 G/sec
        38,296,405      branch-misses:u           #    2.24% of all branches

       1.691727650 seconds time elapsed

       1.678228000 seconds user
       0.011958000 seconds sys
|#
