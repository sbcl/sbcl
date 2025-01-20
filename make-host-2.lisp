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
;;; Set up the perfect hash generator for the target's value of N-FIXNUM-BITS.
(preload-perfect-hash-generator (perfect-hash-generator-journal :input))

;; Supress function/macro redefinition warnings under clisp.
#+clisp (setf custom:*suppress-check-redefinition* t)

;; Avoid natively compiling new code under ecl
#+ecl (ext:install-bytecodes-compiler)

(defun copy-file-from-file (new old)
  (with-open-file (output new :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
    (with-open-file (input old)
      (loop (let ((line (read-line input nil)))
              (if line (write-line line output) (return new)))))))

;;; Run the cross-compiler to produce cold fasl files.
(setq sb-c::*track-full-called-fnames* :minimal) ; Change this as desired
(setq sb-c::*static-vop-usage-counts* (make-hash-table))
(defvar *emitted-full-calls*)
(let (fail
      variables
      functions
      types
      warnp
      style-warnp)
  ;; Even the host may get STYLE-WARNINGS from e.g. cross-compiling
  ;; macro definitions. FIXME: This is duplicate code from make-host-1
  (handler-bind ((style-warning
                   (lambda (c)
                     (signal c)
                     (setq style-warnp 'style-warning)))
                 (simple-warning
                   (lambda (c)
                     (declare (ignore c))
                     (setq warnp 'warning))))
    (sb-xc:with-compilation-unit ()
      (load "src/cold/compile-cold-sbcl.lisp")
      (setf *emitted-full-calls*
            (sb-c::cu-emitted-full-calls sb-c::*compilation-unit*))
      (let ((cache (math-journal-pathname :output)))
        (when (probe-file cache)
          (copy-file-from-file "output/xfloat-math.lisp-expr" cache)
          (format t "~&Math journal: replaced from ~S~%" cache)))
      ;; Enforce absence of unexpected forward-references to warm loaded code.
      ;; Looking into a hidden detail of this compiler seems fair game.
      (when sb-c::*undefined-warnings*
        (setf fail t)
        (dolist (warning sb-c::*undefined-warnings*)
          (case (sb-c::undefined-warning-kind warning)
            (:variable (setf variables t))
            (:type (setf types t))
            (:function (setf functions t)))))))
  ;; Exit the compilation unit so that the summary is printed. Then complain.
  (when fail
    (cerror "Proceed anyway"
            "Undefined ~:[~;variables~] ~:[~;types~]~
             ~:[~;functions (incomplete SB-COLD::*UNDEFINED-FUN-ALLOWLIST*?)~]"
            variables types functions))
  (when (and (or warnp style-warnp) *fail-on-warnings*)
    (cerror "Proceed anyway"
            "make-host-2 stopped due to unexpected ~A raised from the host." (or warnp style-warnp))))

#-clisp ; DO-ALL-SYMBOLS seems to kill CLISP at random
(do-all-symbols (s)
  (when (and (sb-int:info :function :inlinep s)
             (eq (sb-int:info :function :where-from s) :assumed))
      (error "INLINE declaration for an undefined function: ~S?" s)))

(with-open-file (output "output/cold-vop-usage.txt"
                        :direction :output :if-exists :supersede)
  (sb-int:dohash ((name vop) sb-c::*backend-parsed-vops*)
    (declare (ignore vop))
    (format output "~7d ~s~%"
            (gethash name sb-c::*static-vop-usage-counts* 0)
            ;; change SB-XC symbols back to their normal counterpart
            (if (string= (cl:package-name (cl:symbol-package name)) "SB-XC")
                (find-symbol (string name) "COMMON-LISP")
                name))))

(when sb-c::*track-full-called-fnames*
  (let (possibly-suspicious likely-suspicious)
    (sb-int:dohash ((name cell) *emitted-full-calls*)
       (let* ((inlinep (eq (sb-int:info :function :inlinep name) 'inline))
              (source-xform (sb-int:info :function :source-transform name))
              (info (sb-int:info :function :info name)))
         (when (and cell
                    (or inlinep
                        source-xform
                        (and info (sb-c::fun-info-templates info))
                        (sb-int:info :function :compiler-macro-function name)))
             (cond (inlinep
                    ;; A full call to an inline function almost always indicates
                    ;; an out-of-order definition. If not an inline function,
                    ;; the call could be due to an inapplicable transformation.
                    (push (list name cell) likely-suspicious))
                   ;; structure constructors aren't inlined by default,
                   ;; though we have a source-xform.
                   ((and (listp source-xform) (eq :constructor (cdr source-xform))))
                   (t
                    (push (list name cell) possibly-suspicious))))))
    (flet ((show (label list)
             (when list
               (format t "~%~A suspicious calls:~:{~%~4d ~S~@{~%     ~S~}~}~%"
                       label
                       (mapcar (lambda (x) (list* (ash (cadr x) -2) (car x) (cddr x)))
                               (sort list #'> :key #'cadr))))))
      ;; Called inlines not in the presence of a declaration to the contrary
      ;; indicate that perhaps the function definition appeared too late.
      (show "Likely" likely-suspicious) ; "quite likely" an error
      ;; Failed transforms are considered not quite as suspicious
      ;; because it could either be too late, or that the transform failed.
      (show "Possibly" possibly-suspicious)) ; _potentially_ an error
    ;; As each platform's build becomes warning-free,
    ;; it should be added to the list here to prevent regresssions.
    ;; But oops! apparently this check started failing a long time ago
    ;; but because it was done in the wrong place, the check failed to fail.
    #+nil
    (when (and likely-suspicious
               (target-featurep '(:and (:or :x86 :x86-64) (:or :linux :darwin))))
      (warn "Expected zero inlinining failures"))))

;; After cross-compiling, show me a list of types that checkgen
;; would have liked to use primitive traps for but couldn't.
#+nil
(let ((l (sb-impl::%hash-table-alist sb-c::*checkgen-used-types*)))
  (format t "~&Types needed by checkgen: ('+' = has internal error number)~%")
  (setq l (sort l #'> :key #'cadr))
  (loop for (type-spec . (count . interr-p)) in l
        do (format t "~:[ ~;+~] ~5D ~S~%" interr-p count type-spec))
  (format t "~&Error numbers not used by checkgen:~%")
  (loop for (spec symbol) across sb-c:+backend-internal-errors+
        when (and (not (stringp spec))
                  (not (gethash spec sb-c::*checkgen-used-types*)))
        do (format t "       ~S~%" spec)))

;; Print some information about how well the type operator caches performed
(when sb-impl::*profile-hash-cache*
  (sb-impl::show-hash-cache-statistics))
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

;;; Let's check that the type system was reasonably sane. (It's easy
;;; to spend a long time wandering around confused trying to debug
;;; cold init if it wasn't.)
(load "tests/type.after-xc.lisp")

;;; If you're experimenting with the system under a cross-compilation
;;; host which supports CMU-CL-style SAVE-LISP, this can be a good
;;; time to run it. The resulting core isn't used in the normal build,
;;; but can be handy for experimenting with the system. (See slam.sh
;;; for an example.)
;;; FIXME: can we just always do this for supported hosts, and remove the choice?
(cond #+sbcl (t (host-sb-ext:save-lisp-and-die "output/after-xc.core"))
      ((member :sb-after-xc-core sb-xc:*features*)
       #+cmu (ext:save-lisp "output/after-xc.core" :load-init-file nil)
       #+openmcl (ccl::save-application "output/after-xc.core")
       #+clisp (ext:saveinitmem "output/after-xc.core")))
