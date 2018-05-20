;;;; the implementation-independent parts of the code generator. We use
;;;; functions and information provided by the VM definition to convert
;;;; IR2 into assembly code. After emitting code, we finish the
;;;; assembly and then do the post-assembly phase.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;;; utilities used during code generation

;;; the number of bytes used by the code object header
(defun component-header-length (&optional
                                (component *component-being-compiled*))
  (let* ((2comp (component-info component))
         (constants (ir2-component-constants 2comp)))
    (ash (align-up (length constants) code-boxed-words-align) sb!vm:word-shift)))

;;; the size of the NAME'd SB in the currently compiled component.
;;; This is useful mainly for finding the size for allocating stack
;;; frames.
(defun sb-allocated-size (name)
  (finite-sb-current-size (sb-or-lose name)))

;;; the TN that is used to hold the number stack frame-pointer in
;;; VOP's function, or NIL if no number stack frame was allocated
(defun current-nfp-tn (vop)
  (unless (zerop (sb-allocated-size 'non-descriptor-stack))
    (let ((block (ir2-block-block (vop-block vop))))
    (when (ir2-physenv-number-stack-p
           (physenv-info
            (block-physenv block)))
      (ir2-component-nfp (component-info (block-component block)))))))

;;; the TN that is used to hold the number stack frame-pointer in the
;;; function designated by 2ENV, or NIL if no number stack frame was
;;; allocated
(defun callee-nfp-tn (2env)
  (unless (zerop (sb-allocated-size 'non-descriptor-stack))
    (when (ir2-physenv-number-stack-p 2env)
      (ir2-component-nfp (component-info *component-being-compiled*)))))

;;; the TN used for passing the return PC in a local call to the function
;;; designated by 2ENV
(defun callee-return-pc-tn (2env)
  (ir2-physenv-return-pc-pass 2env))

;;;; noise to emit an instruction trace

(defun trace-instruction (section vop inst args state
                          &aux (*standard-output* *compiler-trace-output*))
  (macrolet ((prev-section () `(car state))
             (prev-vop () `(cdr state)))
    (unless (eq (prev-section) section)
      (format t "in the ~A section:~%" section)
      (setf (prev-section) section))
    (unless (eq (prev-vop) vop)
      (when vop
        (format t "~%VOP ")
        (if (vop-p vop)
            (print-vop vop)
            (format *compiler-trace-output* "~S~%" vop)))
      (terpri)
      (setf (prev-vop) vop))
    (case inst
      (:label
       (format t "~A:~%" args))
      (:align
       (format t "~0,8T.align~0,8T~A~%" args))
      (t
       (format t "~0,8T~A~@[~0,8T~{~A~^, ~}~]~%" inst args))))
  (values))

;;;; GENERATE-CODE and support routines

;;; standard defaults for slots of SEGMENT objects
(defun default-segment-run-scheduler ()
  (policy (lambda-bind
           (block-home-lambda
            (block-next (component-head *component-being-compiled*))))
          (or (> speed compilation-speed) (> space compilation-speed))))
(defun default-segment-inst-hook ()
  (and *compiler-trace-output*
       #'trace-instruction))

;;; Return T if and only if there were any constants emitted.
(defun emit-inline-constants ()
  #!+inline-constants
  (let* ((asmstream *asmstream*)
         (sorted (sb!vm:sort-inline-constants
                  (asmstream-constant-vector asmstream)))
         (section (asmstream-data-section asmstream)))
    (dovector (constant sorted (plusp (length sorted)))
      (sb!vm:emit-inline-constant section (car constant) (cdr constant)))))

;;; If a constant is already loaded into a register use that register.
(defun optimize-constant-loads (component)
  (let* ((register-sb (sb-or-lose 'sb!vm::registers))
         (loaded-constants
           (make-array (sb-size register-sb)
                       :initial-element nil)))
    (do-ir2-blocks (block component)
      (fill loaded-constants nil)
      (do ((vop (ir2-block-start-vop block) (vop-next vop)))
          ((null vop))
        (labels ((register-p (tn)
                   (and (tn-p tn)
                        (not (eq (tn-kind tn) :unused))
                        (eq (sc-sb (tn-sc tn)) register-sb)))
                 (constant-eql-p (a b)
                   (or (eq a b)
                       (and (eq (sc-name (tn-sc a)) 'constant)
                            (eq (tn-sc a) (tn-sc b))
                            (eql (tn-offset a) (tn-offset b)))))
                 (remove-constant (tn)
                   (when (register-p tn)
                     (setf (svref loaded-constants (tn-offset tn)) nil)))
                 (remove-written-tns ()
                   (cond ((memq (vop-info-save-p (vop-info vop))
                                '(t :force-to-stack))
                          (fill loaded-constants nil))
                         (t
                          (do ((ref (vop-results vop) (tn-ref-across ref)))
                              ((null ref))
                            (remove-constant (tn-ref-tn ref))
                            (remove-constant (tn-ref-load-tn ref)))
                          (do ((ref (vop-temps vop) (tn-ref-across ref)))
                              ((null ref))
                            (remove-constant (tn-ref-tn ref)))
                          (do ((ref (vop-args vop) (tn-ref-across ref)))
                              ((null ref))
                            (remove-constant (tn-ref-load-tn ref))))))
                 (compatible-scs-p (a b)
                   (or (eql a b)
                       (and (eq (sc-name a) 'sb!vm::control-stack)
                            (eq (sc-name b) 'sb!vm::descriptor-reg))
                       (and (eq (sc-name b) 'sb!vm::control-stack)
                            (eq (sc-name a) 'sb!vm::descriptor-reg))))
                 (find-constant-tn (constant sc)
                   (loop for (saved-constant . tn) across loaded-constants
                         when (and saved-constant
                                   (constant-eql-p saved-constant constant)
                                   (compatible-scs-p (tn-sc tn) sc))
                         return tn)))
          (case (vop-name vop)
            ((move sb!vm::move-arg)
             (let* ((args (vop-args vop))
                    (x (tn-ref-tn args))
                    (y (tn-ref-tn (vop-results vop)))
                    constant)
               (cond ((or (eq (sc-name (tn-sc x)) 'null)
                          (not (eq (tn-kind x) :constant)))
                      (remove-written-tns))
                     ((setf constant (find-constant-tn x (tn-sc y)))
                      (when (register-p y)
                        (setf (svref loaded-constants (tn-offset y))
                              (cons x y)))
                      ;; XOR is more compact on x86oids and many
                      ;; RISCs have a zero register
                      (unless (and (constant-p (tn-leaf x))
                                   (eql (tn-value x) 0)
                                   (register-p y))
                        (setf (tn-ref-tn args) constant)
                        (setf (tn-ref-load-tn args) nil)))
                     ((register-p y)
                      (setf (svref loaded-constants (tn-offset y))
                            (cons x y)))
                     (t
                      (remove-written-tns)))))
            (t
             (remove-written-tns))))))))

(defun generate-code (component)
  (when *compiler-trace-output*
    (format *compiler-trace-output*
            "~|~%assembly code for ~S~2%"
            component))
  (let* ((prev-env nil)
         (n-entries (length (ir2-component-entries (component-info component))))
         (asmstream (make-asmstream))
         (*asmstream* asmstream))

    (emit (asmstream-elsewhere-section asmstream)
          (asmstream-elsewhere-label asmstream))

    (do-ir2-blocks (block component)
      (let ((1block (ir2-block-block block)))
        (when (and (eq (block-info 1block) block)
                   (block-start 1block))
          (assemble (:code 'nil) ; bind **CURRENT-VOP** to nil
            ;; Align first emitted block of each loop: x86 and x86-64 both
            ;; like 16 byte alignment, however, since x86 aligns code objects
            ;; on 8 byte boundaries we cannot guarantee proper loop alignment
            ;; there (yet.)  Only x86-64 does something with ALIGNP, but
            ;; it may be useful in the future.
            (let ((alignp (let ((cloop (block-loop 1block)))
                            (when (and cloop
                                       (loop-tail cloop)
                                       (not (loop-info cloop)))
                              ;; Mark the loop as aligned by saving the IR1 block aligned.
                              (setf (loop-info cloop) 1block)
                              t))))
              (emit-block-header (block-label 1block)
                                 (ir2-block-%trampoline-label block)
                                 (ir2-block-dropped-thru-to block)
                                 alignp)))
          (let ((env (block-physenv 1block)))
            (unless (eq env prev-env)
              (let ((lab (gen-label)))
                (setf (ir2-physenv-elsewhere-start (physenv-info env))
                      lab)
                (emit (asmstream-elsewhere-section asmstream) lab))
              (setq prev-env env)))))
      (do ((vop (ir2-block-start-vop block) (vop-next vop)))
          ((null vop))
        (let ((gen (vop-info-generator-function (vop-info vop))))
          (assemble (:code vop)
            (cond ((not gen)
                   (format t
                           "missing generator for ~S~%"
                           (template-name (vop-info vop))))
                  #!+arm64
                  ((and (vop-next vop)
                        (eq (vop-name vop)
                            (vop-name (vop-next vop)))
                        (memq (vop-name vop) '(move move-operand sb!vm::move-arg))
                        (sb!vm::load-store-two-words vop (vop-next vop)))
                   (setf vop (vop-next vop)))
                  (t
                   (funcall gen vop)))))))
    (emit-inline-constants)
    (let ((trailer (sb!assem::make-section)))
      ;; Build the simple-fun-offset table.
      ;; The assembler is not capable of emitting the difference between labels,
      ;; so we'll just leave space and fill them in later
      (emit trailer `(.align 2)) ; align for uint32_t
      (dotimes (i n-entries) (emit trailer `(.byte 0 0 0 0)))
      #!+little-endian
      (emit trailer `(.byte ,(ldb (byte 8 0) n-entries) ,(ldb (byte 8 8) n-entries)))
      #!+big-endian
      (emit trailer `(.byte ,(ldb (byte 8 8) n-entries) ,(ldb (byte 8 0) n-entries)))
      (let* ((segment
              (assemble-sections
               (make-segment :header-skew
                             (if (and (= code-boxed-words-align 1)
                                      (oddp (length (ir2-component-constants
                                                     (component-info component)))))
                                 sb!vm:n-word-bytes
                                 0)
                             :run-scheduler (default-segment-run-scheduler)
                             :inst-hook (default-segment-inst-hook))
               (asmstream-data-section asmstream)
               (asmstream-code-section asmstream)
               (asmstream-elsewhere-section asmstream)
               trailer))
             (size
              (finalize-segment segment))
             (buffer
              (sb!assem::segment-buffer segment)))
        (flet ((store-ub32 (index val)
                 (multiple-value-bind (b0 b1 b2 b3)
                     #!+little-endian
                     (values (ldb (byte 8  0) val) (ldb (byte 8  8) val)
                             (ldb (byte 8 16) val) (ldb (byte 8 24) val))
                     #!+big-endian
                     (values (ldb (byte 8 24) val) (ldb (byte 8 16) val)
                             (ldb (byte 8  8) val) (ldb (byte 8  0) val))
                   (setf (aref buffer (+ index 0)) b0
                         (aref buffer (+ index 1)) b1
                         (aref buffer (+ index 2)) b2
                         (aref buffer (+ index 3)) b3))))
          (let ((index size))
            (decf index 2)
            ;; Assert that we are aligned for storing uint32_t
            (aver (not (logtest index #b11)))
            (dolist (entry (reverse (sb!c::ir2-component-entries
                                     (component-info component))))
              (store-ub32 (decf index 4)
                          (label-position (entry-info-offset entry))))))
        (values segment
                size
                (asmstream-elsewhere-label asmstream)
                (sb!assem::segment-fixup-notes segment))))))

(defun label-elsewhere-p (label-or-posn kind)
  (let ((elsewhere (label-position *elsewhere-label*))
        (label (etypecase label-or-posn
                 (label
                  (label-position label-or-posn))
                 (index
                  label-or-posn))))
    (if (memq kind '(:single-value-return
                     :unknown-return
                     :known-return))
        ;; We're interested in what precedes the return, not after
        (< elsewhere label)
        (<= elsewhere label))))

#!+inline-constants
(defun register-inline-constant (&rest constant-descriptor)
  (declare (dynamic-extent constant-descriptor))
  (let ((asmstream *asmstream*)
        (constant (sb!vm:canonicalize-inline-constant constant-descriptor)))
    (ensure-gethash
     constant
     (asmstream-constant-table asmstream)
     (multiple-value-bind (label value) (sb!vm:inline-constant-value constant)
       (vector-push-extend (cons constant label)
                           (asmstream-constant-vector asmstream))
       value))))
