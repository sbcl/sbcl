#!/bin/sh
./run-sbcl.sh --noprint --disable-debugger $* <<\EOF
(defvar *configs-to-build* (cdr *posix-argv*))

;;; TODO: allow this to be specified on the command line
(defparameter *jobs*
  (max 1
       #+unix
       (floor (sb-alien:alien-funcall
               (sb-alien:extern-alien "sysconf"
                                      (function sb-alien:long sb-alien:int))
               sb-unix::sc-nprocessors-onln)
              2)))
(format t "~&Using up to ~D job~:P~%" *jobs*)

;;; I should probably have a way to add a fixed set of features in
;;; for an architecture, such as :LITTLE-ENDIAN for x86{-64}.
;;; This list is: arch-name and ((configuation-name feature ...) ...)
(defparameter *all-configurations*
  '(("arm"    ("arm" :little-endian :largefile))
    ("arm64"  ("arm64" :little-endian :sb-thread)
              ("arm64-darwin" :little-endian :sb-thread :darwin-jit))
    ("mips"   ("mips" :largefile :little-endian))
    ("ppc"    ("ppc" :big-endian)
              ("ppc-thread" :big-endian :sb-thread))
    ("ppc64"  ("ppc64" :ppc64 :big-endian)) ; sb-thread is the default and required
    ("riscv"  ("riscv" :64-bit :little-endian :sb-thread))
    ("sparc"  ("sparc" :big-endian))
    ("x86"    ("x86" :little-endian :largefile)
              ("x86-thread" :little-endian :largefile :sb-thread))
    ("x86-64" ("x86-64" :little-endian :avx2 :gencgc :sb-simd-pack :sb-simd-pack-256)
              ("x86-64-imm" :little-endian :avx2 :gencgc :sb-simd-pack :sb-simd-pack-256
                            :immobile-space)
              ("x86-64-permgen" :little-endian :avx2 :gencgc :sb-simd-pack :sb-simd-pack-256
                                :permgen))))

(setq sb-ext:*evaluator-mode* :compile)
(defun interpolate (string substitutions)
  (sb-int:collect ((fragments))
    (let ((start 0))
      (loop
       (let ((p (position #\{ string :start start)))
         (unless p
           (fragments (subseq string start))
           (return (apply #'concatenate 'string (fragments))))
         (fragments (subseq string start p))
         (let* ((end (the (not null) (position #\} string :start (1+ p))))
                (var (subseq string (1+ p) end))
                (pair (assoc var substitutions :test #'string=)))
           (fragments (the string (cdr pair)))
           (setq start (1+ end))))))))

;;; TODOs:
;;; 1) Consider not rewriting Makefile if nothing changed. Write to a temp file first
;;;    and switch it out when different. On the other hand, isn't this the same problem
;;;    as canonicalize-whitespace.lisp altering timestamps on unchanged files,
;;;    or did we fix that?
;;; 2) Put in correct dependencies for each target based on build-order.lisp-expr
(with-open-file (makefile "crossbuild-runner/Makefile"
                          :direction :output
                          :if-exists :supersede)
  (format makefile
          "SBCL=src/runtime/sbcl
ARGS=--core output/sbcl.core --noinform --disable-debugger --noprint --no-userinit --no-sysinit
SCRIPT1=crossbuild-runner/pass-1.lisp
SCRIPT2=crossbuild-runner/pass-2.lisp
DEPS1=crossbuild-runner/pass-1.lisp src/cold/build-order.lisp-expr~%")
  (dolist (arch+configs *all-configurations*)
    (dolist (config (cdr arch+configs))
      (destructuring-bind (config-name . features) config
        (format makefile
                (interpolate
                 "~%obj/xbuild/{cfg}/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) {cfg} {arch} \"{feat}\" < $(SCRIPT1)
obj/xbuild/{cfg}.core: obj/xbuild/{cfg}/xc.core
	$(SBCL) $(ARGS) {cfg} < $(SCRIPT2)~%"
                 `(("cfg" . ,config-name)
                   ("arch" . ,(car arch+configs))
                   ("feat" . ,(write-to-string features))))
                )))))

(defun corefiles ()
  (mapcar (lambda (x) (format nil "obj/xbuild/~A.core" x))
          (or *configs-to-build*
              (mapcan (lambda (x) (mapcar 'car (cdr x)))
                      *all-configurations*))))

(ensure-directories-exist "obj/xbuild/")
(run-program "make"
             `(,(format nil "-j~D" *jobs*)
               "-k"
               "-fcrossbuild-runner/Makefile"
               ,@(corefiles))
             :output t :error t
             :search t)
EOF
