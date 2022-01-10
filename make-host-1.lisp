(progn
  (load "src/cold/shared.lisp")
  (let ((*print-pretty* nil)
        (*print-length* nil))
    (dolist (thing '(("SB-XC" "*FEATURES*")
                     ("SB-COLD" "BACKEND-SUBFEATURES")))
      (let* ((sym (intern (cadr thing) (car thing)))
             (val (symbol-value sym)))
        (when val
          (format t "~&target ~S = ~S~%" sym  val))))))
(in-package "SB-COLD")
#+sbcl
(declaim (sb-ext:muffle-conditions
          sb-ext:compiler-note
          (satisfies optional+key-style-warning-p)))
(progn
  (setf *host-obj-prefix* (if (boundp 'cl-user::*sbcl-host-obj-prefix*)
                              (symbol-value 'cl-user::*sbcl-host-obj-prefix*)
                              "obj/from-host/"))
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
    `(let (warnp style-warnp)
       (handler-bind ((style-warning
                       ;; Any unmuffled STYLE-WARNING should fail
                       ;; These would typically be from undefined functions,
                       ;; or optional-and-key when that was visible.
                       (lambda (c)
                         (signal c) ; won't do SETQ if MUFFLE-WARNING is invoked
                         (setq style-warnp 'style-warning)))
                      (simple-warning
                        (lambda (c)
                          (declare (ignore c))
                          (setq warnp 'warning))))
         (with-compilation-unit () ,@forms))
       (when (and (or warnp style-warnp) *fail-on-warnings*)
         (cerror "Proceed anyway"
                 "make-host-1 stopped due to unexpected ~A." (or warnp style-warnp))))

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
      (let ((object (apply #'compile-file "tools-for-build/ucd.lisp"
                           ;; ECL creates its compiled files beside
                           ;; the truename of a source; that's bad
                           ;; when we're in a build tree of symlinks.
                           #+ecl
                           (list
                            :output-file
                            (compile-file-pathname "tools-for-build/ucd.lisp"))
                           #-ecl
                           ())))
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
  ;; If make-host-1 is parallelized, it will produce host fasls without loading
  ;; them. The host will have interpreted definitions of most everything,
  ;; which is OK because writing out the C headers is not compute-intensive.
  (load-or-cload-xcompiler #'host-cload-stem)
 ;; propagate structure offset and other information to the C runtime
 ;; support code.
 (load "tools-for-build/corefile.lisp" :verbose nil)
 (host-cload-stem "src/compiler/generic/genesis" nil)
) ; END with-compilation-unit

(unless (member :crossbuild-test sb-xc:*features*)
  (sb-cold:genesis :c-header-dir-name "src/runtime/genesis"))
