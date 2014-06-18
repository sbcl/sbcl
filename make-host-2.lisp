;;; Set up the cross-compiler.
(setf *print-level* 5 *print-length* 5)
(load "src/cold/shared.lisp")
(in-package "SB-COLD")
;;; FIXME: these prefixes look like non-pathnamy ways of defining a
;;; relative pathname.  Investigate whether they can be made relative
;;; pathnames.
(setf *host-obj-prefix* "obj/from-host/"
      *target-obj-prefix* "obj/from-xc/")
(load "src/cold/set-up-cold-packages.lisp")
(load "src/cold/defun-load-or-cload-xcompiler.lisp")
(load-or-cload-xcompiler #'host-load-stem)

(defun proclaim-target-optimization ()
  (let ((debug (if (position :sb-show *shebang-features*) 2 1)))
    (sb-xc:proclaim
     `(optimize
       (compilation-speed 1) (debug ,debug)
       ;; CLISP's pretty-printer is fragile and tends to cause stack
       ;; corruption or fail internal assertions, as of 2003-04-20; we
       ;; therefore turn off as many notes as possible.
       (sb!ext:inhibit-warnings #-clisp 2 #+clisp 3)
       ;; SAFETY = SPEED (and < 3) should provide reasonable safety,
       ;; but might skip some unreasonably expensive stuff
       ;; (e.g. %DETECT-STACK-EXHAUSTION in sbcl-0.7.2).
       (safety 2) (space 1) (speed 2)
       ;; sbcl-internal optimization declarations:
       ;;
       ;; never insert stepper conditions
       (sb!c:insert-step-conditions 0)
       ;; save FP and PC for alien calls -- or not
       (sb!c:alien-funcall-saves-fp-and-pc #!+x86 3 #!-x86 0)))))
(compile 'proclaim-target-optimization)

(defun in-target-cross-compilation-mode (fun)
  "Call FUN with everything set up appropriately for cross-compiling
   a target file."
  (let (;; In order to increase microefficiency of the target Lisp,
        ;; enable old CMU CL defined-function-types-never-change
        ;; optimizations. (ANSI says users aren't supposed to
        ;; redefine our functions anyway; and developers can
        ;; fend for themselves.)
        #!-sb-fluid
        (sb!ext:*derive-function-types* t)
        ;; Let the target know that we're the cross-compiler.
        (*features* (cons :sb-xc *features*))
        ;; We need to tweak the readtable..
        (*readtable* (copy-readtable)))
    ;; ..in order to make backquotes expand into target code
    ;; instead of host code.
    ;; FIXME: Isn't this now taken care of automatically by
    ;; toplevel forms in the xcompiler backq.lisp file?
    (set-macro-character #\` #'sb!impl::backquote-macro)
    (set-macro-character #\, #'sb!impl::comma-macro)

    (set-dispatch-macro-character #\# #\+ #'she-reader)
    (set-dispatch-macro-character #\# #\- #'she-reader)
    ;; Control optimization policy.
    (proclaim-target-optimization)
    ;; Specify where target machinery lives.
    (with-additional-nickname ("SB-XC" "SB!XC")
      (funcall fun))))
(compile 'in-target-cross-compilation-mode)


;; Supress function/macro redefinition warnings under clisp.
#+clisp (setf custom:*suppress-check-redefinition* t)

(setf *target-compile-file* #'sb-xc:compile-file)
(setf *target-assemble-file* #'sb!c:assemble-file)
(setf *in-target-compilation-mode-fn* #'in-target-cross-compilation-mode)

;;; Run the cross-compiler to produce cold fasl files.
;; This suppresses ~6000 lines of "undefined function" warnings from the
;; cross-compiler stemming from the calls to INSTANCE-TYPEP that occur before
;; src/code/class gets compiled. It magically converts efficiently,
;; but IR1 is a little bit naive about how it happens.
(dolist (f '(sb!kernel:layout-depthoid
             sb!kernel:layout-inherits))
  (setf (sb!int:info :function :kind f) :function
        (sb!int:info :function :where-from f) :declared))
(load "src/cold/compile-cold-sbcl.lisp")

;; After cross-compiling, show me a list of types that checkgen
;; would have liked to use primitive traps for but couldn't.
#+nil
(let ((l (sb-impl::%hash-table-alist sb!c::*checkgen-used-types*)))
  (format t "~&Types needed by checkgen: ('+' = has internal error number)~%")
  (setq l (sort l #'> :key #'cadr))
  (loop for (type-spec . (count . interr-p)) in l
        do (format t "~:[ ~;+~] ~5D ~S~%" interr-p count type-spec))
  (format t "~&Error numbers not used by checkgen:~%")
  (loop for (spec . symbol) across sb!c::*backend-internal-errors*
        when (and (not (stringp spec))
                  (not (gethash spec sb!c::*checkgen-used-types*)))
        do (format t "       ~S~%" spec)))

;; Print some information about how well the function caches performed
(when sb!impl::*profile-hash-cache*
  (sb!impl::show-hash-cache-statistics))
#|
Sample output
-------------
     Seek       Hit      (%)    Evict      (%) Size    full
 23698219  18382256 ( 77.6%)  5313915 ( 22.4%) 2048  100.0% TYPE=-CACHE
 23528751  23416735 ( 99.5%)    46242 (  0.2%) 1024   20.1% VALUES-SPECIFIER-TYPE-CACHE
 16755212  13072420 ( 78.0%)  3681768 ( 22.0%) 1024  100.0% CSUBTYPEP-CACHE
  9913114   8374965 ( 84.5%)  1537893 ( 15.5%)  256  100.0% MAKE-VALUES-TYPE-CACHED-CACHE
  7718160   4702069 ( 60.9%)  3675019 ( 47.6%)  512  100.0% TYPE-INTERSECTION2-CACHE
  5184706   1626512 ( 31.4%)  3557973 ( 68.6%)  256   86.3% %TYPE-INTERSECTION-CACHE
  5156044   3986450 ( 77.3%)  1169338 ( 22.7%)  256  100.0% VALUES-SUBTYPEP-CACHE
  4550163   2969409 ( 65.3%)  1580498 ( 34.7%)  256  100.0% VALUES-TYPE-INTERSECTION-CACHE
  3544211   2607658 ( 73.6%)   936300 ( 26.4%)  256   98.8% %TYPE-UNION-CACHE
  2545070   2110741 ( 82.9%)   433817 ( 17.0%)  512  100.0% PRIMITIVE-TYPE-AUX-CACHE
  2164841   1112785 ( 51.4%)  1706097 ( 78.8%)  256  100.0% TYPE-UNION2-CACHE
  1568022   1467575 ( 93.6%)   100191 (  6.4%)  256  100.0% TYPE-SINGLETON-P-CACHE
   779941    703208 ( 90.2%)    76477 (  9.8%)  256  100.0% %COERCE-TO-VALUES-CACHE
   618605    448427 ( 72.5%)   169922 ( 27.5%)  256  100.0% VALUES-TYPE-UNION-CACHE
   145805     29403 ( 20.2%)   116206 ( 79.7%)  256   76.6% %%MAKE-UNION-TYPE-CACHED-CACHE
   118634     76203 ( 64.2%)    42188 ( 35.6%)  256   94.9% %%MAKE-ARRAY-TYPE-CACHED-CACHE
    12319     12167 ( 98.8%)       47 (  0.4%)  128   82.0% WEAKEN-TYPE-CACHE
    10416      9492 ( 91.1%)      668 (  6.4%)  256  100.0% TYPE-NEGATION-CACHE
|#

;;; miscellaneous tidying up and saving results
(let ((filename "output/object-filenames-for-genesis.lisp-expr"))
  (ensure-directories-exist filename :verbose t)
  (with-open-file (s filename :direction :output :if-exists :supersede)
    (write *target-object-file-names* :stream s :readably t)))

;;; Let's check that the type system was reasonably sane. (It's easy
;;; to spend a long time wandering around confused trying to debug
;;; cold init if it wasn't.)
(when (position :sb-test *shebang-features*)
  (load "tests/type.after-xc.lisp"))

;;; If you're experimenting with the system under a cross-compilation
;;; host which supports CMU-CL-style SAVE-LISP, this can be a good
;;; time to run it. The resulting core isn't used in the normal build,
;;; but can be handy for experimenting with the system. (See slam.sh
;;; for an example.)
(when (position :sb-after-xc-core *shebang-features*)
  #+cmu (ext:save-lisp "output/after-xc.core" :load-init-file nil)
  #+sbcl (sb-ext:save-lisp-and-die "output/after-xc.core")
  #+openmcl (ccl::save-application "output/after-xc.core")
  #+clisp (ext:saveinitmem "output/after-xc.core"))
#+cmu (ext:quit)
#+clisp (ext:quit)
#+abcl (ext:quit)
