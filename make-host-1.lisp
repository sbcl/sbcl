(progn
  (load "src/cold/shared.lisp")
  (let ((*print-pretty* nil)
        (*print-length* nil))
    (dolist (thing '(("SB-XC" "*FEATURES*")
                     ("SB-COLD" "*SHEBANG-BACKEND-SUBFEATURES*")))
      (let* ((sym (intern (cadr thing) (car thing)))
             (val (symbol-value sym)))
        (when val
          (format t "~&target ~S = ~S~%" sym  val))))))
(in-package "SB-COLD")
(progn
  ;; Generating ldso-stubs should be driven by GNUmakefile, but that won't work
  ;; because we generate it using Lisp code, and we don't presume the target
  ;; to have a lisp implementation. So we generate it on the host by knowing
  ;; something about which Config files depend on ldso-stubs.
  (when (or (member :alpha sb-xc:*features*)
            (member :hppa sb-xc:*features*))
    (let ((*readtable* *xc-readtable*)) (load "tools-for-build/ldso-stubs.lisp")))

  (setf *host-obj-prefix* "obj/from-host/")
  (load "src/cold/set-up-cold-packages.lisp")
  (load "src/cold/defun-load-or-cload-xcompiler.lisp")

  ;; Supress function/macro redefinition warnings under clisp.
  #+clisp (setf custom:*suppress-check-redefinition* t)

  (defmacro maybe-with-compilation-unit (&body forms)
    ;; A compilation-unit seems to kill the compile. I'm not sure if it's
    ;; running out of memory or what. I don't care to find out,
    ;; but it's most definitely the cause of the breakage.
    #+clisp `(progn ,@forms)

    #+sbcl
    ;; Watch for deferred warnings under SBCL.
    ;; UNDEFINED-VARIABLE does not cause COMPILE-FILE to return warnings-p
    ;; unless outside a compilation unit. You find out about it only upon
    ;; exit of SUMMARIZE-COMPILATION-UNIT. So we set up a handler for that.
    `(let (in-summary fail)
       (handler-bind (((and simple-warning (not style-warning))
                       (lambda (c &aux (fc (simple-condition-format-control c)))
                         ;; hack for PPC. See 'build-order.lisp-expr'
                         ;; Ignore the warning, and the warning about the warning.
                         (unless (and (stringp fc)
                                      (or (search "not allowed by the operand type" fc)
                                          (search "ignoring FAILURE-P return" fc)))
                           (setq fail 'warning))))
                      ;; Prevent regressions on a few platforms
                      ;; that are known to build cleanly.
                      (sb-int:simple-style-warning
                       (lambda (c &aux (fc (simple-condition-format-control c)))
                         (when (and (feature-in-list-p '(:or :x86 :x86-64 :arm64)
                                                       :target)
                                    in-summary
                                    (stringp fc)
                                    (search "undefined" fc))
                           (unless (eq fail 'warning)
                             (setq fail 'style-warning))))))
         (with-compilation-unit ()
           (multiple-value-prog1 (progn ,@forms) (setq in-summary t))))
       (when fail
         (cerror "Proceed anyway"
                 "make-host-1 stopped due to unexpected ~A." fail)))

    #-(or clisp sbcl) `(with-compilation-unit () ,@forms)))

;;; Return T if we can skip rebuild of unicode data when re-running make-host-1.
(defun outputs-up-to-date (inputs outputs)
  (let ((min-output-stamp))
    (dolist (name outputs)
      (unless (probe-file name)
        (return-from outputs-up-to-date nil))
      (let ((time (file-write-date name)))
        (when (or (null min-output-stamp) (< time min-output-stamp))
          (setq min-output-stamp time))))
    (> min-output-stamp
       (reduce #'max inputs :key #'file-write-date))))

(defvar *ucd-inputs*)
(defvar *ucd-outputs*)

;;; Build the unicode database now. It depends on nothing in the cross-compiler
;;; (and let's keep it that way). This code is slow to run, so compile it.
(let ((inputs '("tools-for-build/ucd.lisp"
                "tools-for-build/UnicodeData.txt"
                "tools-for-build/NormalizationCorrections.txt"
                "tools-for-build/CompositionExclusions.txt"
                "tools-for-build/SpecialCasing.txt"
                "tools-for-build/EastAsianWidth.txt"
                "tools-for-build/Scripts.txt"
                "tools-for-build/LineBreak.txt"
                "tools-for-build/DerivedAge.txt"
                "tools-for-build/allkeys.txt"
                "tools-for-build/ConfusablesEdited.txt"
                "tools-for-build/BidiMirroring.txt"
                "tools-for-build/Blocks.txt"
                "tools-for-build/Jamo.txt"
                "tools-for-build/CaseFolding.txt"
                "tools-for-build/PropList.txt"
                "tools-for-build/DerivedNormalizationProps.txt"
                "tools-for-build/more-ucd-consts.lisp-expr"))
      (outputs '("output/bidi-mirrors.lisp-expr"
                 "output/block-ranges.lisp-expr"
                 "output/block-names.lisp-expr"
                 "output/case.dat"
                 "output/CaseFolding.txt"
                 "output/casepages.dat"
                 "output/casepages.lisp-expr"
                 "output/collation.dat"
                 "output/comp.dat"
                 "output/confusables.lisp-expr"
                 "output/decomp.dat"
                 "output/foldcases.lisp-expr"
                 "output/misc-properties.lisp-expr"
                 "output/n-collation-entries.lisp-expr"
                 "output/numerics.lisp-expr"
                 "output/other-collation-info.lisp-expr"
                 "output/titlecases.lisp-expr"
                 "output/ucd1-names.lisp-expr"
                 "output/ucdhigh.dat"
                 "output/ucdlow.dat"
                 "output/ucdmisc.dat"
                 "output/ucd-names.lisp-expr")))
  (unless (outputs-up-to-date inputs outputs)
    (format t "~&; Building Unicode data~%")
    (let ((*ucd-inputs* (make-hash-table :test 'equal))
          (*ucd-outputs* (make-hash-table :test 'equal)))
      (dolist (input inputs)
        (setf (gethash input *ucd-inputs*) 'unused))
      (dolist (output outputs)
        (setf (gethash output *ucd-outputs*) 'unmade))
      (let ((object (compile-file "tools-for-build/ucd.lisp")))
        (setf (gethash "tools-for-build/ucd.lisp" *ucd-inputs*) 'used)
        (load object :verbose t)
        (delete-file object))
      (dolist (s '(sb-cold::slurp-ucd sb-cold::slurp-proplist sb-cold::output))
        (funcall s))
      (let (unused-inputs extra-inputs unused-outputs extra-outputs)
        (maphash (lambda (k v) (when (eql v 'unused) (push k unused-inputs))) *ucd-inputs*)
        (maphash (lambda (k v) (when (and (eql v 'used) (not (member k inputs :test 'equal)))
                                 (push k extra-inputs)))
                 *ucd-inputs*)
        (maphash (lambda (k v) (when (eql v 'unmade) (push k unused-outputs))) *ucd-outputs*)
        (maphash (lambda (k v) (when (and (eql v 'made) (not (member k outputs :test 'equal)))
                                 (push k extra-outputs)))
                 *ucd-outputs*)
        (unless (and (null unused-inputs) (null extra-inputs)
                     (null unused-outputs) (null extra-outputs))
          (error "~&~@[Unused ucd inputs: ~A~%~]~
                  ~@[Extra ucd inputs: ~A~%~]~
                  ~@[Uncreated ucd outputs: ~A~%~]~
                  ~@[Extra ucd outputs: ~A~%~]"
                 unused-inputs extra-inputs
                 unused-outputs extra-outputs))))))

;;; I don't know the best combination of OPTIMIZE qualities to produce a correct
;;; and reasonably fast cross-compiler in ECL. At over half an hour to complete
;;; make-host-{1,2}, I don't really want to waste any more time finding out.
;;; These settings work, while the defaults do not.
#+ecl (proclaim '(optimize (safety 2) (debug 2)))

(maybe-with-compilation-unit
 (let ((*feature-evaluation-results* nil))
  ;; If make-host-1 is parallelized, it will produce host fasls without loading
  ;; them. The host will have interpreted definitions of most everything,
  ;; which is OK because writing out the C headers is not compute-intensive.
  (load-or-cload-xcompiler #'host-cload-stem)
  (write-feature-eval-results))
 ;; propagate structure offset and other information to the C runtime
 ;; support code.
 (load "tools-for-build/corefile.lisp" :verbose nil)
 (host-cload-stem "src/compiler/generic/genesis" nil)
) ; END with-compilation-unit

(sb-cold:genesis :c-header-dir-name "src/runtime/genesis")
