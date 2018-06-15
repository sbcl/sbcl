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

;; Supress function/macro redefinition warnings under clisp.
#+clisp (setf custom:*suppress-check-redefinition* t)

;;; Run the cross-compiler to produce cold fasl files.
(setq sb!c::*track-full-called-fnames* :minimal) ; Change this as desired
(setq sb!c::*static-vop-usage-counts* (make-hash-table))
(let (fail
      variables
      functions
      types)
  (sb!xc:with-compilation-unit ()
    (load "src/cold/compile-cold-sbcl.lisp")
    ;; Enforce absence of unexpected forward-references to warm loaded code.
    ;; Looking into a hidden detail of this compiler seems fair game.
    #!+(or x86 x86-64 arm64) ; until all the rest are clean
    (when sb!c::*undefined-warnings*
      (setf fail t)
      (dolist (warning sb!c::*undefined-warnings*)
        (case (sb!c::undefined-warning-kind warning)
          (:variable (setf variables t))
          (:type (setf types t))
          (:function (setf functions t))))))
  ;; Exit the compilation unit so that the summary is printed. Then complain.
  (when fail
    #!-win32 ; build is known to be unclean
    (cerror "Proceed anyway"
            "Undefined ~:[~;variables~] ~:[~;types~]~
             ~:[~;functions (incomplete SB-COLD::*UNDEFINED-FUN-WHITELIST*?)~]"
            variables types functions)))

(do-all-symbols (s)
  (when (and (sb!int:info :function :inlinep s)
             (eq (sb!int:info :function :where-from s) :assumed))
      (warn "Did you forget to define ~S?" s)))

;; enable this too see which vops were or weren't used
#+nil
(when (hash-table-p sb!c::*static-vop-usage-counts*)
  (format t "Vops used:~%")
  (dolist (cell (sort (sb!int:%hash-table-alist sb!c::*static-vop-usage-counts*)
                      #'> :key #'cdr))
    (format t "~6d ~s~%" (cdr cell) (car cell))))

(when sb!c::*track-full-called-fnames*
  (let (possibly-suspicious likely-suspicious)
    (sb!int:call-with-each-globaldb-name
     (lambda (name)
       (let* ((cell (sb!int:info :function :emitted-full-calls name))
              (inlinep (eq (sb!int:info :function :inlinep name) :inline))
              (info (sb!int:info :function :info name)))
         (if (and cell
                  (or inlinep
                      (and info (sb!c::fun-info-templates info))
                      (sb!int:info :function :compiler-macro-function name)
                      (sb!int:info :function :source-transform name)))
             (if inlinep
                 ;; A full call to an inline function almost always indicates
                 ;; an out-of-order definition. If not an inline function,
                 ;; the call could be due to an inapplicable transformation.
                 (push (cons name cell) likely-suspicious)
                 (push (cons name cell) possibly-suspicious))))))
    (flet ((show (label list)
             (when list
               (format t "~%~A suspicious calls:~:{~%~4d ~S~@{~%     ~S~}~}~%"
                       label
                       (mapcar (lambda (x) (list* (ash (cadr x) -2) (car x) (cddr x)))
                               (sort list #'> :key #'cadr))))))
      ;; Called inlines not in the presence of a declaration to the contrary
      ;; indicate that perhaps the function definition appeared too late.
      (show "Likely" likely-suspicious)
      ;; Failed transforms are considered not quite as suspicious
      ;; because it could either be too late, or that the transform failed.
      (show "Possibly" possibly-suspicious))
    ;; As each platform's build becomes warning-free,
    ;; it should be added to the list here to prevent regresssions.
    #!+(and (or x86 x86-64) (or linux darwin))
    (when likely-suspicious
      (warn "Expected zero inlinining failures"))))

;; After cross-compiling, show me a list of types that checkgen
;; would have liked to use primitive traps for but couldn't.
#+nil
(let ((l (sb-impl::%hash-table-alist sb!c::*checkgen-used-types*)))
  (format t "~&Types needed by checkgen: ('+' = has internal error number)~%")
  (setq l (sort l #'> :key #'cadr))
  (loop for (type-spec . (count . interr-p)) in l
        do (format t "~:[ ~;+~] ~5D ~S~%" interr-p count type-spec))
  (format t "~&Error numbers not used by checkgen:~%")
  (loop for (spec symbol) across sb!c:+backend-internal-errors+
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
(load "tests/type.after-xc.lisp")

;;; If you're experimenting with the system under a cross-compilation
;;; host which supports CMU-CL-style SAVE-LISP, this can be a good
;;; time to run it. The resulting core isn't used in the normal build,
;;; but can be handy for experimenting with the system. (See slam.sh
;;; for an example.)
#+sb-after-xc-core
(progn
  #+cmu (ext:save-lisp "output/after-xc.core" :load-init-file nil)
  #+sbcl (sb-ext:save-lisp-and-die "output/after-xc.core")
  #+openmcl (ccl::save-application "output/after-xc.core")
  #+clisp (ext:saveinitmem "output/after-xc.core"))
