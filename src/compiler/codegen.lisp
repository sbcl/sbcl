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
         (constants (ir2-component-constants 2comp))
         (num-consts (length constants)))
    (ash (logandc2 (1+ num-consts) 1) sb!vm:word-shift)))

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

;;;; specials used during code generation

(defvar *code-segment* nil)
(defvar *elsewhere* nil)
(defvar *elsewhere-label* nil)

;;;; noise to emit an instruction trace

(defvar *prev-segment*)
(defvar *prev-vop*)

(defun trace-instruction (segment vop inst &rest args)
  (declare (dynamic-extent args))
  (let ((*standard-output* *compiler-trace-output*))
    (unless (eq *prev-segment* segment)
      (format t "in the ~A segment:~%" (sb!assem:segment-type segment))
      (setf *prev-segment* segment))
    (unless (eq *prev-vop* vop)
      (when vop
        (format t "~%VOP ")
        (if (vop-p vop)
            (print-vop vop)
            (format *compiler-trace-output* "~S~%" vop)))
      (terpri)
      (setf *prev-vop* vop))
    (case inst
      (:label
       (format t "~A:~%" (car args)))
      (:align
       (format t "~0,8T.align~0,8T~A~%" (car args)))
      (t
       (format t "~0,8T~A~@[~0,8T~{~A~^, ~}~]~%" inst args))))
  (values))

;;;; GENERATE-CODE and support routines

;;; standard defaults for slots of SEGMENT objects
(defun default-segment-run-scheduler ()
  (and *assembly-optimize*
        (policy (lambda-bind
                 (block-home-lambda
                  (block-next (component-head *component-being-compiled*))))
                (or (> speed compilation-speed) (> space compilation-speed)))))
(defun default-segment-inst-hook ()
  (and *compiler-trace-output*
       #'trace-instruction))

(defun init-assembler ()
  (setf *code-segment*
        (sb!assem:make-segment :type :regular
                               :run-scheduler (default-segment-run-scheduler)
                               :inst-hook (default-segment-inst-hook)))
  #!+sb-dyncount
  (setf (sb!assem:segment-collect-dynamic-statistics *code-segment*)
        *collect-dynamic-statistics*)
  (setf *elsewhere*
        (sb!assem:make-segment :type :elsewhere
                               :run-scheduler (default-segment-run-scheduler)
                               :inst-hook (default-segment-inst-hook)
                               :alignment 0))
  #!+inline-constants
  (setf *unboxed-constants* (make-unboxed-constants))
  (values))

#!+inline-constants
(defun emit-inline-constants (&aux (constant-holder *unboxed-constants*)
                                   (vector (constant-vector constant-holder)))
  (setf *unboxed-constants* nil)
  (unless (zerop (length vector))
    (let ((constants (sb!vm:sort-inline-constants vector)))
      (assemble ((constant-segment constant-holder))
        (map nil (lambda (constant)
                   (sb!vm:emit-inline-constant (car constant) (cdr constant)))
             constants)))
    (setf *code-segment* (let ((seg (constant-segment constant-holder)))
                           (sb!assem:append-segment seg *code-segment*)
                           seg))))

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
               (cond ((and (eq (vop-name vop) 'move)
                           (location= x y))
                      ;; Helps subsequent optimization of adjacent VOPs
                      (delete-vop vop))
                     ((or (eq (sc-name (tn-sc x)) 'null)
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
  (let ((prev-env nil)
        (*prev-segment* nil)
        (*prev-vop* nil)
        (*fixup-notes* nil))
    (let ((label (sb!assem:gen-label)))
      (setf *elsewhere-label* label)
      (sb!assem:assemble (*elsewhere*)
        (sb!assem:emit-label label)))
    (do-ir2-blocks (block component)
      (let ((1block (ir2-block-block block)))
        (when (and (eq (block-info 1block) block)
                   (block-start 1block))
          (sb!assem:assemble (*code-segment*)
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
                (emit-label-elsewhere lab))
              (setq prev-env env)))))
      (do ((vop (ir2-block-start-vop block) (vop-next vop)))
          ((null vop))
        (let ((gen (vop-info-generator-function (vop-info vop))))
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
                 (funcall gen vop))))))
    (sb!assem:append-segment *code-segment* *elsewhere*)
    (setf *elsewhere* nil)
    #!+inline-constants
    (emit-inline-constants)
    (values (sb!assem:finalize-segment *code-segment*)
            *fixup-notes*)))

(defun emit-label-elsewhere (label)
  (sb!assem:assemble (*elsewhere*)
    (sb!assem:emit-label label)))

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
  (let ((constants *unboxed-constants*)
        (constant (sb!vm:canonicalize-inline-constant constant-descriptor)))
    (or (gethash constant (constant-table constants))
        (multiple-value-bind (label value) (sb!vm:inline-constant-value constant)
          (vector-push-extend (cons constant label) (constant-vector constants))
          (setf (gethash constant (constant-table constants)) value)))))
