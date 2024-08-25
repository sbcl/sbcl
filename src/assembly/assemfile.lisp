;;;; the extra code necessary to feed an entire file of assembly code
;;;; to the assembler

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;; a list of (NAME LABEL OFFSET) for every entry point
(defvar *entry-points* nil)

;;; Note: You might think from the name that this would act like
;;; COMPILE-FILE, but in fact it's arguably more like LOAD, even down
;;; to the return convention. It LOADs a file, then writes out any
;;; assembly code created by the process.
(defun assemble-file (name &key (output-file (error "Missing keyword"))
                                print)
  (declare (ignore print))
  (when sb-cold::*compile-for-effect-only*
    (return-from assemble-file t))
  (let* ((name (pathname name))
         ;; the fasl file currently being output to
         (lap-fasl-output (open-fasl-output (pathname output-file) name))
         (*entry-points* nil)
         (won nil)
         (asmstream (make-asmstream))
         (*asmstream* asmstream)
         (sb-vm::*adjustable-vectors* nil))
    (declare (special sb-vm::*adjustable-vectors*))
    (unwind-protect
        (let ((sb-xc:*features* (cons :sb-assembling sb-xc:*features*))
              (*readtable* sb-cold:*xc-readtable*))
          (load (merge-pathnames name (make-pathname :type "lisp")))
          (resolve-ep-labels (asmstream-code-section asmstream))
          ;; Reserve space for the jump table. The first word of the table
          ;; indicates the total length in words, counting the length word itself
          ;; as a word, and the words comprising jump tables that were registered
          ;; as unboxed constants. The logic in compiler/codegen splits unboxed
          ;; constants into two groups: jump-tables and everything else, but that's
          ;; because a code coverage map, if present, goes in between them.
          ;; Assembly routines have no coverage map, so there is no need to split.
          (let ((n-extra-words ; Space for the indirect call table if needed
                 (+ #+(or x86 x86-64) (length *entry-points*)))
                (n-data-words  ; Space for internal jump tables if needed
                 (loop for ((category . data) . label)
                       across (asmstream-constant-vector asmstream)
                       do (progn #+host-quirks-ccl label) ; shut up the host
                       when (eq category :jump-table) sum (length data))))
            (emit (asmstream-data-section asmstream)
                  `(.lispword ,(+ n-extra-words n-data-words 1))
                  `(.skip ,(* n-extra-words sb-vm:n-word-bytes))))
          (emit-inline-constants)
          ;; Ensure alignment to double-Lispword in case a raw constant
          ;; causes misalignment, as actually happens on ARM64.
          ;; You might think one Lispword is aligned enough, but it isn't,
          ;; because to created a tagged pointer to an asm routine,
          ;; the base address must have all 0s in the tag bits.
          ;; Note that for code components that contain simple-funs,
          ;; alignment is ensured by SIMPLE-FUN-HEADER-WORD.
          (emit (asmstream-data-section asmstream)
                `(.align ,sb-vm:n-lowtag-bits))
          (let ((segment (assemble-sections
                          asmstream nil
                          (make-segment nil))))
            (unless (= (length (remove-duplicates (mapcar 'car *entry-points*)))
                       (length *entry-points*))
              (error "Duplicate asm routine: ~S"
                     (loop for (this . rest) on *entry-points*
                           when (assoc (car this) rest)
                           collect (car this))))
            (dump-assembler-routines segment
                                     (segment-buffer segment)
                                     (sb-assem::segment-fixup-notes segment)
                                     (sb-assem::get-allocation-points asmstream)
                                     *entry-points*
                                     lap-fasl-output))
          (setq won t))
      (close-fasl-output lap-fasl-output (not won)))
    won))

(defstruct (reg-spec (:copier nil))
  (kind :temp :type (member :arg :temp :res))
  (name nil :type symbol)
  (temp nil :type symbol)
  (scs nil :type (or list symbol))
  (offset nil))
(defmethod print-object ((spec reg-spec) stream)
  (print-unreadable-object (spec stream :type t)
    (format stream
            ":KIND ~S :NAME ~S :SCS ~S :OFFSET ~S"
            (reg-spec-kind spec)
            (reg-spec-name spec)
            (reg-spec-scs spec)
            (reg-spec-offset spec))))

(defun reg-spec-sc (spec)
  (if (atom (reg-spec-scs spec))
      (reg-spec-scs spec)
      (car (reg-spec-scs spec))))

(defun parse-reg-spec (kind name sc offset)
  (let ((reg (make-reg-spec :kind kind :name name :scs sc :offset offset)))
    (ecase kind
      (:temp)
      ((:arg :res)
       (setf (reg-spec-temp reg) (symbolicate (symbol-name name) '-arg-temp))))
    reg))

(defun expand-one-export-spec (export)
  (if (symbolp export)
      `(list ',export ,export 0)
      (destructuring-bind
          (name (operator base-label offset))
          export
        ;; KLUDGE: Presume that all compound export specs are of the
        ;; form (NAME (OPERATOR BASE-LABEL OFFSET)), where OPERATOR is
        ;; + or -, BASE-LABEL is a LABEL in the present scope, and
        ;; OFFSET evaluates to an integer.  Ideally, we should be
        ;; smarter about this.
        `(list ',name ,base-label (,operator ,offset)))))

(defun expand-export-option (exports)
  (loop
    for export in exports
    collect `(push ,(expand-one-export-spec export) *entry-points*)))

(defun resolve-ep-labels (section)
  (do ((statement (stmt-next (section-start section)) (stmt-next statement)))
      ((null statement))
    ;; We also output some raw words using fixups; I'm not sure if they should be
    ;; changed to labels. For now, restrict to control transfers.
    (binding* ((patch
                (when (member (stmt-mnemonic statement)
                              '("B" "BEQ" "JMP" "CALL") ; KLUDGE
                              :test 'string=)
                  (member-if (lambda (x)
                               (and (typep x 'fixup)
                                    (eq (fixup-flavor x) :assembly-routine)
                                    (eql (fixup-offset x) 0)))
                             (stmt-operands statement)))
                :exit-if-null)
               (ep (assoc (fixup-name (car patch)) *entry-points*)))
      ;; oy. what is (third ep) ? An offset?
      (aver (and ep (= (third ep) 0)))
      (rplaca patch (second ep)))))

(defun expand-align-option (align)
  (when align
    `((emit-alignment ,align))))

(defun emit-assemble (name options regs code)
  (collect ((decls))
    (loop
      (if (and (consp code) (consp (car code)) (eq (caar code) 'declare))
          (decls (pop code))
          (return)))
    `(let ,(mapcar (lambda (reg)
                     `(,(reg-spec-name reg)
                       (make-random-tn
                        :kind :normal
                        :sc (sc-or-lose ',(reg-spec-sc reg))
                        :offset ,(reg-spec-offset reg))))
                   regs)
       ,@(decls)
       (assemble (:code 'nil)
         ,@(expand-align-option (cadr (assoc :align options)))
         ,name
         (push (list ',name ,name 0) *entry-points*)
         ,@(expand-export-option (cdr (assoc :export options)))
         ,@code
         ,@(generate-return-sequence
            (or (cadr (assoc :return-style options)) :raw))
         ;; Place elsewhere section following the regular code, except on 32-bit
         ;; ARM. See the comment in arm/assem-rtns that says it expects THROW to
         ;; drop through into UNWIND. That wouldn't work if the code emitted
         ;; by GENERATE-ERROR-CODE were interposed between those.
         #-arm
         (let ((asmstream *asmstream*))
           (append-sections (asmstream-code-section asmstream)
                            (asmstream-elsewhere-section asmstream)))
         (emit-alignment sb-vm:n-lowtag-bits
                         ;; EMIT-LONG-NOP does not exist for (not x86-64)
                         #+x86-64 :long-nop))
       (when cl:*compile-print*
         (format *error-output* "~S assembled~%" ',name)))))

(defun arg-or-res-spec (reg)
  `(,(reg-spec-name reg)
    :scs ,(if (atom (reg-spec-scs reg))
              (list (reg-spec-scs reg))
              (reg-spec-scs reg))
    ,@(unless (eq (reg-spec-kind reg) :res)
        `(:target ,(reg-spec-temp reg)))))

(defun emit-assemble-vop (name options vars)
  (let* ((args (remove :arg vars :key #'reg-spec-kind :test #'neq))
         (temps (remove :temp vars :key #'reg-spec-kind :test #'neq))
         (results (remove :res vars :key #'reg-spec-kind :test #'neq))
         (return-style (or (cadr (assoc :return-style options)) :raw))
         (cost (or (cadr (assoc :cost options)) 247))
         (vop (or (cadr (assoc :vop-var options))
                  (make-symbol "VOP"))))
    ;; :full-call-no-return entails the call site behavior for :full-call
    ;; without automatic insertion of the return sequence. This avoids some confusion
    ;; on the part of the human reading the code, especially if the asm routine
    ;; tail calls something else.
    (unless (member return-style '(:raw :full-call :none :full-call-no-return))
      (error "unknown return-style for ~S: ~S" name return-style))
    (multiple-value-bind (call-sequence call-temps)
        (generate-call-sequence name return-style vop options)
      `(define-vop ,(if (atom name) (list name) name)
         (:args ,@(mapcar #'arg-or-res-spec args))
         ,@(let ((index -1))
             (mapcar (lambda (arg)
                       `(:temporary (:sc ,(reg-spec-sc arg)
                                     :offset ,(reg-spec-offset arg)
                                     :from (:argument ,(incf index))
                                     :to (:eval 2))
                                    ,(reg-spec-temp arg)))
                     args))
         ,@(mapcar (lambda (temp)
                     `(:temporary (:sc ,(reg-spec-sc temp)
                                   :offset ,(reg-spec-offset temp)
                                   :from (:eval 1)
                                   :to (:eval 3))
                                  ,(reg-spec-name temp)))
                   temps)
         (:vop-var ,vop)
         ,@(let ((index -1))
             (mapcar (lambda (res)
                       `(:temporary (:sc ,(reg-spec-sc res)
                                     :offset ,(reg-spec-offset res)
                                     :from (:eval 2)
                                     :to (:result ,(incf index))
                                     :target ,(reg-spec-name res))
                                    ,(reg-spec-temp res)))
                     results))
         (:results ,@(mapcar #'arg-or-res-spec results))
         ;; This formerly unioned in the contents of an :ignore clause from
         ;; the value of the 'call-temps' variable, for no good reason afaict.
         (:ignore ,@(sort (set-difference (mapcar #'reg-spec-name temps)
                                          (cdr (assoc :call-temps options)))
                          #'string<))
         ,@call-temps
         ;; This too is a tad sleazy - because of how VOP parsing works,
         ;; any :SAVE-P specified in options supersedes one from call-temps.
         ;; It would be wiser to signal an error about duplicate options.
         ,@(remove-if (lambda (x)
                        (member x '(:return-style :cost :call-temps
                                    :vop-var :vop-prefix)))
                      options
                      :key #'car)
         (:generator ,cost
           ,@(cdr (assoc :vop-prefix options))
           ,@(mapcar (lambda (arg)
                       `(move ,(reg-spec-temp arg) ,(reg-spec-name arg)))
                     args)
           ,@call-sequence
           ,@(mapcar (lambda (res)
                       `(move ,(reg-spec-name res) ,(reg-spec-temp res)))
                     results))))))
