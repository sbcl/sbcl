;;;; "warm initialization": initialization which comes after cold init

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "COMMON-LISP-USER")

;;;; general warm init compilation policy

;;; First things first, bootstrap the WARNING handler.
sb-kernel::
(setq **initial-handler-clusters**
      `(((,(find-classoid-cell 'warning) .
           ,(named-lambda "MAYBE-MUFFLE" (warning)
              (when (muffle-warning-p warning)
                (muffle-warning warning))))
         (,(find-classoid-cell 'step-condition) . sb-impl::invoke-stepper))))
;;;; And now a trick: splice those into the oldest *HANDLER-CLUSTERS*
;;;; which had a placeholder NIL reserved for this purpose.
sb-kernel::(rplaca (last *handler-clusters*) (car **initial-handler-clusters**))

;;;; Use the same settings as PROCLAIM-TARGET-OPTIMIZATION
;;;; I could not think of a trivial way to ensure that this stays functionally
;;;; identical to the corresponding code in 'compile-cold-sbcl'.
;;;; (One possibility would be to read this form from a lisp-expr file)
;;;; The intent is that we should generate identical code if a file is moved
;;;; from the cross-compiled sources to warm-compiled or vice-versa.
(proclaim '(optimize
            #+sb-show (debug 2)
            (safety 2) (speed 2)
            ;; never insert stepper conditions
            (sb-c:insert-step-conditions 0)
            (sb-c:alien-funcall-saves-fp-and-pc #+x86 3 #-x86 0)))

(locally
    (declare (notinline find-symbol)) ; don't ask
  (let ((s (find-symbol "*/SHOW*" "SB-INT")))
  ;; If you made it this far, chances are that you no longer wish to see
  ;; whatever it is that show would have shown. Comment this out if you need.
    (when s (set s nil))))

(let ((byte (deref (extern-alien "widetag_lowtag" (array char 256))
                   sb-vm:character-widetag)))
  (assert (not (logbitp 7 byte))) ; not a headered object
  (assert (= (logand byte sb-vm:lowtag-mask) sb-vm:list-pointer-lowtag)))
(gc :full t)

;;; Verify that all defstructs except for one were compiled in a null lexical
;;; environment. Compiling any call to a structure constructor would like to
;;; know whether some slots get their default value especially if the default
;;; is incompatible with the slot type (consider MISSING-ARG, e.g).
;;; If some initform was compiled in a non-null environment, it might not refer
;;; to a global function. We'd rather ignore it than incorrectly style-warn.
(let (result)
  (do-all-symbols (s)
    (let ((dd (sb-kernel:find-defstruct-description s nil)))
      (when (and dd (not (sb-kernel::dd-null-lexenv-p dd)))
        (push (sb-kernel:dd-name dd) result))))
  (assert (equal result '(sb-c::conset))))

;;; Assert that genesis preserved shadowing symbols.
(let ((p sb-assem::*backend-instruction-set-package*))
  (unless (eq p (find-package "SB-VM"))
    (dolist (expect '("SEGMENT" "MAKE-SEGMENT"))
      (assert (find expect (package-shadowing-symbols p) :test 'string=)))))

;;; Verify that compile-time floating-point math matches load-time.
(defvar *compile-files-p*)
(when (or (not (boundp '*compile-files-p*)) *compile-files-p*)
  (with-open-file (stream "float-math.lisp-expr" :if-does-not-exist nil)
    (when stream
      (format t "; Checking ~S~%" (pathname stream))
      ;; Ensure that we're reading the correct variant of the file
      ;; in case there is more than one set of floating-point formats.
      (assert (eq (read stream) :default))
      (sb-kernel::with-float-traps-masked (:overflow :divide-by-zero)
        (let ((*package* (find-package "SB-KERNEL")))
          (dolist (expr (read stream))
            (destructuring-bind (fun args . result) expr
              (let ((result (if (eq (first result) 'sb-kernel::&values)
                                (rest result)
                                result))
                    (actual (multiple-value-list (apply fun (sb-int:ensure-list args)))))
                (unless (equalp actual result)
                  (#+sb-devel cerror #+sb-devel ""
                   #-sb-devel format #-sb-devel t
                   "FLOAT CACHE LINE ~S vs COMPUTED ~S~%"
                   expr actual))))))))))

(when (if (boundp '*compile-files-p*) *compile-files-p* t)
  (with-open-file (output "output/cold-vop-usage.txt" :if-does-not-exist nil)
    (when output
      (setq sb-c::*static-vop-usage-counts* (make-hash-table))
      (loop (let ((line (read-line output nil)))
              (unless line (return))
              (let ((count (read-from-string line))
                    (name (read-from-string line t nil :start 8)))
                (setf (gethash name sb-c::*static-vop-usage-counts*) count)))))))

;;;; compiling and loading more of the system

;;; FIXME: CMU CL's pclcom.lisp had extra optional stuff wrapped around
;;; COMPILE-PCL, at least some of which we should probably have too:
;;;
;;; (with-compilation-unit
;;;     (:optimize '(optimize (debug #+(and (not high-security) small) .5
;;;                               #-(or high-security small) 2
;;;                               #+high-security 3)
;;;                        (speed 2) (safety #+(and (not high-security) small) 0
;;;                                          #-(or high-security small) 2
;;;                                          #+high-security 3)
;;;                        (inhibit-warnings 2))
;;;      :optimize-interface '(optimize-interface #+(and (not high-security) small)
;;; (safety 1)
;;;                                            #+high-security (safety 3))
;;;      :context-declarations
;;;      '((:external (declare (optimize-interface (safety #-high-security 2 #+high-
;;; security 3)
;;;                                             (debug #-high-security 1 #+high-s
;;; ecurity 3))))
;;;     ((:or :macro (:match "$EARLY-") (:match "$BOOT-"))
;;;     (declare (optimize (speed 0))))))
;;;
(defvar *sbclroot* "")
(let ((sources (with-open-file (f (merge-pathnames "build-order.lisp-expr" *load-pathname*))
                 (read f) ; skip over the make-host-{1,2} input files
                 (read f)))
      (sb-c::*handled-conditions* sb-c::*handled-conditions*))
 (proclaim '(sb-ext:muffle-conditions compiler-note))
 (flet ((do-srcs (list)
         (dolist (stem list)
          ;; Do like SB-COLD::LPNIFY-STEM for consistency, though parse/xlate/unparse
          ;; would probably also work. I don't think that's better.
          (let ((fullname (sb-int:logically-readonlyize
                           (format nil "SYS:~:@(~A~).LISP" (substitute #\; #\/ stem))
                           ;; indicate shareable string even if not dumped as
                           ;; a literal (when compiling in the LOAD step)
                           t))
                (output
                  (compile-file-pathname
                   (concatenate 'string *sbclroot* stem)
                   :output-file
                   (merge-pathnames
                    (concatenate
                     'string sb-fasl::*!target-obj-prefix*
                     (subseq stem 0 (1+ (position #\/ stem :from-end t))))))))
           (flet ((report-recompile-restart (stream)
                    (format stream "Recompile file ~S" stem))
                  (report-continue-restart (stream)
                    (format stream "Continue, using possibly bogus file ~S" output)))
             (tagbody
              retry-compile-file
                (multiple-value-bind (output-truename warnings-p failure-p)
                    (ecase (if (boundp '*compile-files-p*) *compile-files-p* t)
                     ((t)
                      (let ((sb-c::*source-namestring* fullname)
                            (sb-ext:*derive-function-types*
                              (unless (search "/pcl/" stem)
                                t)))
                        (ensure-directories-exist output)
                        ;; Like PROCLAIM-TARGET-OPTIMIZATION in 'compile-cold-sbcl'
                        ;; We should probably stash a copy of the POLICY instance from
                        ;; make-host-2 in a global var and apply it here.
                        (proclaim '(optimize
                                    (safety 2) (speed 2)
                                    (sb-c:insert-step-conditions 0)
                                    (sb-c:alien-funcall-saves-fp-and-pc #+x86 3 #-x86 0)))
                        (compile-file (concatenate 'string *sbclroot* stem)
                                      :output-file output)))
                     ((nil) output))
                  (cond ((not output-truename)
                         (error "COMPILE-FILE of ~S failed." stem))
                        (failure-p
                         (unwind-protect
                              (restart-case
                                  (error "FAILURE-P was set when creating ~S."
                                         output-truename)
                                (recompile ()
                                  :report report-recompile-restart
                                  (go retry-compile-file))
                                (continue ()
                                  :report report-continue-restart
                                  (setf failure-p nil)))
                           ;; Don't leave failed object files lying around.
                           (when (and failure-p (probe-file output-truename))
                                 (delete-file output-truename)
                                 (format t "~&deleted ~S~%" output-truename))))
                        (warnings-p
                         ;; Maybe we should escalate more warnings to errors
                         ;; (see HANDLER-BIND for SIMPLE-WARNING below)
                         ;; rather than asking what to do here?
                         #+(or x86 x86-64) ;; these should complete without warnings
                         (cerror "Ignore warnings" "Compile completed with warnings")))
                  (unless (handler-bind
                              ((sb-kernel:redefinition-with-defgeneric
                                #'muffle-warning))
                            (let ((sb-c::*source-namestring* fullname))
                              (load output-truename)))
                    (error "LOAD of ~S failed." output-truename))
                  (sb-int:/show "done loading" output-truename))))))))

  (let ((cl:*compile-print* nil))
    (dolist (group sources)
      ;; For the love of god, what are we trying to do here???
      ;; It's gone through so many machinations that I can't figure it out.
      ;; The goal should be to build warning-free, not layer one
      ;; kludge upon another so that it can be allowed not to.
      (handler-bind (((and #+x86-64 warning #-x86-64 simple-warning
                           (not sb-kernel:redefinition-warning))
                      (lambda (c)
                        ;; escalate "undefined variable" warnings to errors.
                        ;; There's no reason to allow them in our code.
                        (when (and #-x86-64 ; Don't allow any warnings on x86-64.
                                   (search "undefined variable"
                                           (write-to-string c :escape nil)))
                          (cerror "Finish warm compile ignoring the problem" c)))))
        (with-compilation-unit ()
          (do-srcs group)))))))

(sb-c::dump/restore-interesting-types 'write)
(when (hash-table-p sb-c::*static-vop-usage-counts*)
  (with-open-file (output "output/warm-vop-usage.txt"
                          :direction :output :if-exists :supersede)
    (let (list)
      (sb-int:dohash ((name vop) sb-c::*backend-parsed-vops*)
        (declare (ignore vop))
        (push (cons (gethash name sb-c::*static-vop-usage-counts* 0) name) list))
      (dolist (cell (sort list #'> :key #'car))
        (format output "~7d ~s~%" (car cell) (cdr cell))))))

(when (sb-sys:find-dynamic-foreign-symbol-address "tot_gc_nsec")
  (let* ((run-sec (/ (get-internal-real-time) internal-time-units-per-second))
         (gc-nsec (extern-alien "tot_gc_nsec" unsigned))
         (gc-msec (/ (float gc-nsec) 1000000)))
    (format t "~&Done with warm.lisp. INTERNAL-REAL-TIME=~Fs~@[, GC=~Fms (~,1,2f%)~]~%"
            run-sec
            (if (plusp gc-msec) gc-msec) ; timing wasn't enabled if this is 0
            (/ gc-msec (* 1000 run-sec)))))
