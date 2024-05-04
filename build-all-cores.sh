#!/bin/sh
./run-sbcl.sh --noprint --disable-debugger $* <<\EOF
(defvar *configs-to-build* (cdr *posix-argv*))

(defparameter *jobs*
  (max 1
       #+unix
       (floor (sb-alien:alien-funcall
               (sb-alien:extern-alien "sysconf"
                                      (function sb-alien:long sb-alien:int))
               sb-unix::sc-nprocessors-onln)
              2)))

(let ((first (car *configs-to-build*)))
  (if (and (stringp first)
           (>= (length first) 2)
           (string= first "-j" :end1 2))
      ;; allow "-jN" as two args
      (cond ((= (length first) 2)
             (pop *configs-to-build*)
             (setq *jobs* (parse-integer (pop *configs-to-build*))))
            ((digit-char-p (char first 2)) ; or "-jN" as one arg
             (setq *jobs* (parse-integer first :start 2))
             (pop *configs-to-build*))
            (t
             (error "-j requires a number")))))

(when (> *jobs* 1) (format t "~&Using up to ~D job~:P~%" *jobs*))

;;; I should probably have a way to add a fixed set of features in
;;; for an architecture, such as :LITTLE-ENDIAN for x86{-64}.
;;; This list is: arch-name and ((configuation-name feature ...) ...)
(defparameter *all-configurations*
  '(("arm"    ("arm" :little-endian :largefile))
    ("arm64"  ("arm64" :little-endian :sb-thread)
              ("arm64-darwin" :darwin :bsd :unix :mach-o :little-endian :sb-thread :darwin-jit)
              ("arm64-reloc"  :little-endian :sb-thread :relocatable-static-space))
    ("mips"   ("mips" :largefile :little-endian))
    ("ppc"    ("ppc" :big-endian)
              ("ppc-thread" :big-endian :sb-thread))
    ("ppc64"  ("ppc64" :ppc64 :big-endian)) ; sb-thread is the default and required
    ("riscv"  ("riscv" :64-bit :little-endian :sb-thread))
    ("sparc"  ("sparc" :big-endian)
              ("sparc-sunos" :big-endian :unix :sunos :elf))
    ("x86"    ("x86" :little-endian :largefile (not :sb-eval) :sb-fasteval)
              ("x86-ascii" :little-endian :largefile (not :sb-unicode))
              ("x86-thread" :little-endian :largefile :sb-thread)
              ("x86-linux" :little-endian :largefile :sb-thread :linux :unix :elf :sb-thread))
    ("x86-64" ("x86-64" :little-endian :avx2 :gencgc :sb-simd-pack :sb-simd-pack-256)
              ("x86-64-linux" :linux :unix :elf :little-endian :avx2 :gencgc :sb-simd-pack :sb-simd-pack-256
                              (not :sb-eval) :sb-fasteval)
              ("x86-64-darwin" :darwin :bsd :unix :mach-o :little-endian :avx2 :gencgc
                               :sb-simd-pack :sb-simd-pack-256)
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

(defun add-os-features (arch features)
  (if (intersection '(:unix :win32) features)
      features
      (let* ((arch-symbol (sb-int:keywordicate (string-upcase arch)))
             (os-features (case arch-symbol
                            ((:x86 :x86-64) '(:win32 :sb-thread :sb-safepoint))
                            (t '(:unix :linux :elf)))))
        (append os-features features))))

;;; TODO: dependencies for each target based on build-order.lisp-expr
(let
  ((new
    (with-output-to-string (makefile)
      (format makefile
          "SBCL=src/runtime/sbcl
ARGS=--core output/sbcl.core --noinform --disable-debugger --noprint --no-userinit --no-sysinit
SCRIPT1=crossbuild-runner/pass-1.lisp
SCRIPT2=crossbuild-runner/pass-2.lisp
DEPS1=crossbuild-runner/pass-1.lisp src/cold/build-order.lisp-expr~%")
  (dolist (arch+configs *all-configurations*)
    (dolist (config (cdr arch+configs))
      (destructuring-bind (config-name . features) config
        (setq features (add-os-features (car arch+configs) features))
        (format makefile
                (interpolate
                 "~%obj/xbuild/{cfg}/xc.core: $(DEPS1)
	$(SBCL) $(ARGS) {cfg} {arch} \"{feat}\" < $(SCRIPT1)
obj/xbuild/{cfg}.core: obj/xbuild/{cfg}/xc.core
	$(SBCL) $(ARGS) {cfg} < $(SCRIPT2)~%"
                 `(("cfg" . ,config-name)
                   ("arch" . ,(car arch+configs))
                   ;; features string can't contain a #\newline
                   ("feat" . ,(write-to-string features :pretty nil))))))))))
   (existing (with-open-file (stream "crossbuild-runner/Makefile")
               (let ((a (make-array (file-length stream) :element-type 'character)))
                 (read-sequence a stream)
                 a))))
 (unless (string= new existing)
    (with-open-file (stream "crossbuild-runner/Makefile" :direction :output :if-exists :supersede)
      (write-string new stream))
    (format t "~&Rewrote Makefile~%")))

(defun corefiles ()
  (mapcar (lambda (x) (format nil "obj/xbuild/~A.core" x))
          (or *configs-to-build*
              (mapcan (lambda (x) (mapcar 'car (cdr x)))
                      *all-configurations*))))

(ensure-directories-exist "obj/xbuild/")
;; if it's just one target this could exec() instead
(defvar *process*
  (run-program "make"
               `(,(format nil "-j~D" *jobs*)
                 "-k"
                 "-fcrossbuild-runner/Makefile"
                 ,@(corefiles))
               :output t :error t
               :search t))
(when (= (process-exit-code *process*) 0)
  (load "src/cold/shared" :verbose t)
  (load "validate-float.lisp")
  (dolist (pathname (directory "obj/xbuild/**/xfloat-math.lisp-expr"))
    (check-float-file pathname))
  (dolist (nbits '(30 61 63))
    (let* ((filename (format nil "xperfecthash~D.lisp-expr" nbits))
           (sources (directory (format nil "obj/xbuild/*/from-xc/~A" filename))))
      (when sources
        (funcall (intern "UPDATE-PERFECT-HASHFUNS" "SB-COLD")
                 sources filename))))
  (format t "~&Success~%"))
(sb-ext:exit :code (sb-ext:process-exit-code *process*))
EOF
