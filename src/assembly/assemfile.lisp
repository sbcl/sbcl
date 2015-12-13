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

(in-package "SB!C")

;;; If non-NIL, emit assembly code. If NIL, emit VOP templates.
(defvar *emit-assembly-code-not-vops-p* nil)

;;; a list of (NAME LABEL OFFSET) for every entry point
(defvar *entry-points* nil)

;;; Note: You might think from the name that this would act like
;;; COMPILE-FILE, but in fact it's arguably more like LOAD, even down
;;; to the return convention. It LOADs a file, then writes out any
;;; assembly code created by the process.
#+sb-xc-host
(defun assemble-file (name
                      &key
                      (output-file (make-pathname :defaults name
                                                  :type "assem")))
  (when sb-cold::*compile-for-effect-only*
    (return-from assemble-file t))
  ;; FIXME: Consider nuking the filename defaulting logic here.
  (let* ((*emit-assembly-code-not-vops-p* t)
         (name (pathname name))
         ;; the fasl file currently being output to
         (lap-fasl-output (open-fasl-output (pathname output-file) name))
         (*entry-points* nil)
         (won nil)
         (*code-segment* nil)
         (*elsewhere* nil)
         (*assembly-optimize* nil)
         (*fixup-notes* nil)
         #!+inline-constants
         *constant-segment*
         #!+inline-constants
         *constant-table*
         #!+inline-constants
         *constant-vector*)
    (unwind-protect
        (let ((*features* (cons :sb-assembling *features*)))
          (init-assembler)
          (load (merge-pathnames name (make-pathname :type "lisp")))
          (sb!assem:append-segment *code-segment* *elsewhere*)
          (setf *elsewhere* nil)
          #!+inline-constants
          (emit-inline-constants)
          (let ((length (sb!assem:finalize-segment *code-segment*)))
            (dump-assembler-routines *code-segment*
                                     length
                                     *fixup-notes*
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
(def!method print-object ((spec reg-spec) stream)
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
       (setf (reg-spec-temp reg) (make-symbol (symbol-name name)))))
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
       (sb!assem:assemble (*code-segment* ',name)
         ,@(expand-align-option (cadr (assoc :align options)))
         ,name
         (push (list ',name ,name 0) *entry-points*)
         ,@(expand-export-option (cdr (assoc :export options)))
         ,@code
         ,@(generate-return-sequence
            (or (cadr (assoc :return-style options)) :raw))
         (emit-alignment sb!vm:n-lowtag-bits))
       (when sb!xc:*compile-print*
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
         (vop (make-symbol "VOP")))
    (unless (member return-style '(:raw :full-call :none))
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
                        (member x '(:return-style :cost :call-temps)))
                      options
                      :key #'car)
         (:generator ,cost
           ,@(mapcar (lambda (arg)
                       #!+(or hppa alpha) `(move ,(reg-spec-name arg)
                                                 ,(reg-spec-temp arg))
                       #!-(or hppa alpha) `(move ,(reg-spec-temp arg)
                                                 ,(reg-spec-name arg)))
                     args)
           ,@call-sequence
           ,@(mapcar (lambda (res)
                       #!+(or hppa alpha) `(move ,(reg-spec-temp res)
                                                 ,(reg-spec-name res))
                       #!-(or hppa alpha) `(move ,(reg-spec-name res)
                                                 ,(reg-spec-temp res)))
                     results))))))

(def!macro define-assembly-routine (name&options vars &body code)
  (multiple-value-bind (name options)
      (if (atom name&options)
          (values name&options nil)
          (values (car name&options)
                  (cdr name&options)))
    (let ((regs (mapcar (lambda (var) (apply #'parse-reg-spec var)) vars)))
      (if *emit-assembly-code-not-vops-p*
          (emit-assemble name options regs code)
          (emit-assemble-vop name options regs)))))
