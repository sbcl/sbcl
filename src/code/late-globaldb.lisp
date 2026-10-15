;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMUCL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;; This is in its own file to avoid creating an early dependency on
;; target-package iterators.
(macrolet
    ((def (&rest situations)
       `(eval-when ,situations
          ;; Return all function names that are stored in SYMBOL's packe-info.
          ;; As an example, (INFO-NAME-LIST 'SB-PCL::DIRECT-SUPERCLASSES) =>
          ;; ((SB-PCL::SLOT-ACCESSOR :GLOBAL SB-PCL::DIRECT-SUPERCLASSES SB-PCL::READER)
          ;;  (SB-PCL::SLOT-ACCESSOR :GLOBAL SB-PCL::DIRECT-SUPERCLASSES BOUNDP)
          ;;  (SB-PCL::SLOT-ACCESSOR :GLOBAL SB-PCL::DIRECT-SUPERCLASSES SB-PCL::WRITER))
          (defun info-name-list (symbol)
            (let ((packed-info (symbol-dbinfo symbol))
                  (list))
              (cond (packed-info
                     (do-packed-info-aux-key (packed-info key-index)
                                             (push (construct-globaldb-name (%info-ref packed-info key-index) symbol)
                                                   list))
                     (nconc (and (or (plusp (packed-info-field packed-info 0 0))
                                     ;; fdefns are stored directly in the symbol.
                                     (fboundp symbol))
                                 (list symbol))
                            (nreverse list)))
                    ((fboundp symbol)
                     (list symbol)))))
          ;; Call FUNCTION once for each Name in globaldb that has information associated
          ;; with it, passing the function the Name as its only argument.
          (defun call-with-each-globaldb-name (fun-designator)
            (let ((function (cl:coerce fun-designator 'function)))
              (with-package-iterator (iter (list-all-packages) :internal :external)
                (loop (multiple-value-bind (winp symbol access package) (iter)
                        (declare (ignore access))
                        (if (not winp) (return))
                        ;; Try to process each symbol at most once by associating it with
                        ;; a single package. If a symbol is apparently uninterned,
                        ;; always keep it since we can't know if it has been seen once.
                        (when (or (not (sb-xc:symbol-package symbol))
                                  (eq package (sb-xc:symbol-package symbol)))
                          (dolist (name (info-name-list symbol))
                            (funcall function name))))))
              ,@(unless (equal situations '(:compile-toplevel))
                  `((dovector (obj (car *fdefns*))
                      (when (fdefn-p obj)
                        (funcall function (fdefn-name obj))))
                    (info-maphash (lambda (name data)
                                    (declare (ignore data))
                                    (funcall function name))
                                  *info-environment*))))))))
  ;; In host mode we can not use INFO-MAPHASH because the lockfree hashtable
  ;; is target-only code.  We don't have a deep dependence on this for the host,
  ;; but make-host-2 does iterate over globaldb post-build
  ;; to detect possible inlining failures
  (def :compile-toplevel)
  (def :load-toplevel :execute))

(in-package "SB-THREAD")

;;; Initialize thread-local special vars other than the GC control specials.
;;; globaldb should indicate that the variable is :ALWAYS-THREAD-LOCAL
;;; (which says that the TLS index is nonzero), and often but not necessarily
;; :ALWAYS-BOUND (which says that the value in TLS is not UNBOUND-MARKER).
(defun init-thread-local-storage (thread)
  ;; In addition to wanting the expressly unsafe variant of SYMBOL-VALUE, any error
  ;; signaled such as invalid-arg-count would just go totally wrong at this point.
  (declare (optimize (safety 0)))
  #-sb-thread
  (macrolet ((expand () `(setf ,@(apply #'append (cdr *thread-local-specials*)))))
    (setf *current-thread* thread)
    (expand))
  ;; These assignments require a trick with #+sb-thread as all of the symbols' TLS
  ;; cells contain NO-TLS-VALUE which ordinarily causes SET to affect SYMBOL-GLOBAL-VALUE.
  ;; So we have to store directly into offsets off the primitive thread.
  ;; See %SET-SYMBOL-VALUE-IN-THREAD for comparison.
  ;; (On x86-64, the SET vops understands that for a symbol which is always-thread-local
  ;; it should store into the TLS bypassing the test for NO-TLS-VALUE. However, that is
  ;; currently not a mandatory behavior, but rather an optimization, and not all the
  ;; backends behave as desired)
  #+sb-thread
  (macrolet ((expand ()
               `(setf (sap-ref-lispobj sap ,(info :variable :wired-tls '*current-thread*))
                      thread
                      ,@(loop for (var form) in (cdr *thread-local-specials*)
                              for index = (info :variable :wired-tls var)
                              append
                              (if (fixnump form)
                                  `((sap-ref-word sap ,index)
                                    ,(ash form sb-vm:n-fixnum-tag-bits))
                                  `((sap-ref-lispobj sap ,index)
                                       ,(if (equal form '(sb-kernel:make-unbound-marker))
                                            'ubm form)))))))
    (let ((sap (current-thread-sap)) (ubm (make-unbound-marker))) (expand)))
  ;; Some applications can't tolerate unexpected SIGSEGV. Even ones that ordinarily could
  ;; may exhibit sporadic crashes under the LLVM interceptors which change the sa_flags in
  ;; struct sigaction before passing along the syscall argument. (How is that reasonable?)
  #+tls-load-indirect
  (when (or (= (extern-alien "enable_tls_indirection_preinit" char) 1)
            ;; Try to avoid TLS indirection traps. This is no guarantee, just best effort.
            (thread-ephemeral-p thread)
            (main-thread-p))
    (do ((symbolmap (int-sap (ash sb-vm::*tls-symbol-map* sb-vm:n-fixnum-tag-bits)))
         (i (- (symbol-tls-index '*package*) 8) (+ i 16)) ; step by 2 words
         (end (- (ash sb-vm::*free-tls-index* sb-vm:n-fixnum-tag-bits) 8)))
        ((>= i end))
      (let ((indirection-word (sap-ref-word (current-thread-sap) i)))
        (when (= indirection-word sb-vm:no-tls-value-marker) ; now must point it to the symbol
          ;; (could AVER that no-tls-value is stored as the value, but that would just crash)
          (let ((symbol (sap-ref-lispobj symbolmap (ash i -1))))
            ;; If no symbol, the index is unused (or else there's an unavoidable race)
            (unless (= (get-lisp-obj-address symbol) sb-vm:no-tls-value-marker)
              (setf (sap-ref-lispobj (sb-thread:current-thread-sap) i) symbol)))))))
  thread)

(eval-when (:compile-toplevel)
  ;; Inform genesis of the index <-> symbol mapping made by DEFINE-THREAD-LOCAL
  (with-open-file (output (sb-cold:find-bootstrap-file "output/tls-init.lisp-expr" t)
                          :direction :output :if-exists :supersede)
    (let ((list (mapcar (lambda (x &aux (symbol (car x)))
                          (cons (info :variable :wired-tls symbol) symbol))
                        (cdr *thread-local-specials*)))
          (*package* *keyword-package*))
      (write-char #\( output)
      (dolist (pair list)
        (terpri output)
        (write pair :stream output :readably t :pretty nil))
      (format output "~%)~%")))
  ;; Prevent further use of DEFINE-THREAD-LOCAL after compiling this file
  ;; because the definition of INIT-THREAD-LOCAL-STORAGE is now frozen.
  (setf *thread-local-specials* (cons :final (cdr *thread-local-specials*))))
