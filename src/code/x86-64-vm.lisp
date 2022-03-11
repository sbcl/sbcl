;;;; X86-64-specific runtime stuff

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")
(defun machine-type ()
  "Return a string describing the type of the local machine."
  "X86-64")

#+(or darwin linux openbsd win32 sunos (and freebsd x86-64))
(define-alien-routine ("os_context_float_register_addr" context-float-register-addr)
  (* unsigned) (context (* os-context-t)) (index int))

;;; This is like CONTEXT-REGISTER, but returns the value of a float
;;; register. FORMAT is the type of float to return.

(defun context-float-register (context index format)
  (declare (ignorable context index))
  #-(or darwin linux openbsd win32 sunos (and freebsd x86-64))
  (progn
    (warn "stub CONTEXT-FLOAT-REGISTER")
    (coerce 0 format))
  #+(or darwin linux openbsd win32 sunos (and freebsd x86-64))
  (let ((sap (alien-sap (context-float-register-addr context index))))
    (ecase format
      (single-float
       (sap-ref-single sap 0))
      (double-float
       (sap-ref-double sap 0))
      (complex-single-float
       (complex (sap-ref-single sap 0)
                (sap-ref-single sap 4)))
      (complex-double-float
       (complex (sap-ref-double sap 0)
                (sap-ref-double sap 8)))
      #+sb-simd-pack
      (simd-pack-int
       (%make-simd-pack-ub64
        (sap-ref-64 sap 0)
        (sap-ref-64 sap 8)))
      #+sb-simd-pack
      (simd-pack-single
       (%make-simd-pack-single
        (sap-ref-single sap 0)
        (sap-ref-single sap 4)
        (sap-ref-single sap 8)
        (sap-ref-single sap 12)))
      #+sb-simd-pack
      (simd-pack-double
       (%make-simd-pack-double
        (sap-ref-double sap 0)
        (sap-ref-double sap 8)))
      #+sb-simd-pack-256
      (simd-pack-256-int
       (%make-simd-pack-256-ub64
        (sap-ref-64 sap 0)
        (sap-ref-64 sap 8)
        (sap-ref-64 sap 16)
        (sap-ref-64 sap 24)))
      #+sb-simd-pack-256
      (simd-pack-256-single
       (%make-simd-pack-256-single
        (sap-ref-single sap 0)
        (sap-ref-single sap 4)
        (sap-ref-single sap 8)
        (sap-ref-single sap 12)
        (sap-ref-single sap 16)
        (sap-ref-single sap 20)
        (sap-ref-single sap 24)
        (sap-ref-single sap 28)))
      #+sb-simd-pack-256
      (simd-pack-256-double
       (%make-simd-pack-256-double
        (sap-ref-double sap 0)
        (sap-ref-double sap 8)
        (sap-ref-double sap 16)
        (sap-ref-double sap 24))))))

(defun %set-context-float-register (context index format value)
  (declare (ignorable context index format))
  #-(or linux win32)
  (progn
    (warn "stub %SET-CONTEXT-FLOAT-REGISTER")
    value)
  #+(or linux win32)
  (let ((sap (alien-sap (context-float-register-addr context index))))
    (ecase format
      (single-float
       (setf (sap-ref-single sap 0) value))
      (double-float
       (setf (sap-ref-double sap 0) value))
      (complex-single-float
       (locally
           (declare (type (complex single-float) value))
         (setf (sap-ref-single sap 0) (realpart value)
               (sap-ref-single sap 4) (imagpart value))))
      (complex-double-float
       (locally
           (declare (type (complex double-float) value))
         (setf (sap-ref-double sap 0) (realpart value)
               (sap-ref-double sap 8) (imagpart value))))
      #+sb-simd-pack
      (simd-pack-int
       (multiple-value-bind (a b) (%simd-pack-ub64s value)
         (setf (sap-ref-64 sap 0) a
               (sap-ref-64 sap 8) b)))
      #+sb-simd-pack
      (simd-pack-single
       (multiple-value-bind (a b c d) (%simd-pack-singles value)
         (setf (sap-ref-single sap 0) a
               (sap-ref-single sap 4) b
               (sap-ref-single sap 8) c
               (sap-ref-single sap 12) d)))
      #+sb-simd-pack
      (simd-pack-double
       (multiple-value-bind (a b) (%simd-pack-doubles value)
         (setf (sap-ref-double sap 0) a
               (sap-ref-double sap 8) b)))
      #+sb-simd-pack-256
      (simd-pack-256-int
       (multiple-value-bind (a b c d) (%simd-pack-256-ub64s value)
         (setf (sap-ref-64 sap 0) a
               (sap-ref-64 sap 8) b
               (sap-ref-64 sap 16) c
               (sap-ref-64 sap 24) d)))
      #+sb-simd-pack-256
      (simd-pack-256-single
       (multiple-value-bind (a b c d e f g h) (%simd-pack-256-singles value)
         (setf (sap-ref-single sap 0) a
               (sap-ref-single sap 4) b
               (sap-ref-single sap 8) c
               (sap-ref-single sap 12) d
               (sap-ref-single sap 16) e
               (sap-ref-single sap 20) f
               (sap-ref-single sap 24) g
               (sap-ref-single sap 28) h)))
      #+sb-simd-pack-256
      (simd-pack-256-double
       (multiple-value-bind (a b c d) (%simd-pack-256-doubles value)
         (setf (sap-ref-double sap 0) a
               (sap-ref-double sap 8) b
               (sap-ref-double sap 16) c
               (sap-ref-double sap 24) d))))))

;;; Given a signal context, return the floating point modes word in
;;; the same format as returned by FLOATING-POINT-MODES.
#-linux
(defun context-floating-point-modes (context)
  (declare (ignore context)) ; stub!
  (warn "stub CONTEXT-FLOATING-POINT-MODES")
  0)
#+linux
(define-alien-routine ("os_context_fp_control" context-floating-point-modes)
    (unsigned 32)
  (context (* os-context-t)))

(define-alien-routine
    ("arch_get_fp_modes" floating-point-modes) (unsigned 32))

(define-alien-routine
    ("arch_set_fp_modes" %floating-point-modes-setter) void (fp (unsigned 32)))

(defun (setf floating-point-modes) (val) (%floating-point-modes-setter val))


;;;; INTERNAL-ERROR-ARGS

;;; Given a (POSIX) signal context, extract the internal error
;;; arguments from the instruction stream.
(defun internal-error-args (context)
  (declare (type (alien (* os-context-t)) context))
  (let* ((pc (context-pc context))
         (trap-number (sap-ref-8 pc 0)))
    (declare (type system-area-pointer pc))
    (cond ((= trap-number invalid-arg-count-trap)
           (values #.(error-number-or-lose 'invalid-arg-count-error)
                   '(#.arg-count-sc)))
          #+linux
          ((= trap-number uninitialized-load-trap)
           (values #.(error-number-or-lose 'uninitialized-memory-error)
                   (locally
                       (declare (optimize (safety 0)))
                     (let* ((data (sap-ref-8 pc 1)) ; encodes dst register and size
                            (value (sb-vm:context-register context (ash data -2)))
                            (nbytes (ash 1 (logand data #b11)))
                            ;; EMIT-SAP-REF wires the EA to a predetermined register,
                            ;; which now points to the shadow space, not the user memory.
                            (ea (logxor (sb-vm:context-register context msan-temp-reg-number)
                                        msan-mem-to-shadow-xor-const)))
                       `(:raw ,ea ,nbytes ,value)))))
          (t
           (sb-kernel::decode-internal-error-args (sap+ pc 1) trap-number)))))


#+immobile-space
(defun alloc-immobile-fdefn ()
  (alloc-immobile-fixedobj fdefn-size
                           (logior (ash undefined-fdefn-header 16)
                                   fdefn-widetag))) ; word 0

#+immobile-code
(progn
(defconstant trampoline-entry-offset n-word-bytes)
(defun make-simplifying-trampoline (fun)
  (let ((code (truly-the (values code-component &optional)
                         (allocate-code-object :dynamic 0 3 24)))) ; KLUDGE
    (setf (%code-debug-info code) fun)
    (let ((sap (sap+ (code-instructions code) trampoline-entry-offset))
          (ea (+ (logandc2 (get-lisp-obj-address code) lowtag-mask)
                 (ash code-debug-info-slot word-shift))))
      ;; For a funcallable-instance, the instruction sequence is:
      ;;    MOV RAX, [RIP-n] ; load the function
      ;;    MOV RAX, [RAX+5] ; load the funcallable-instance-fun
      ;;    JMP [RAX-3]
      ;; Otherwise just instructions 1 and 3 will do.
      ;; We could use the #xA1 opcode to save a byte, but that would
      ;; be another headache do deal with when relocating this code.
      ;; There's precedent for this style of hand-assembly,
      ;; in arch_write_linkage_table_entry() and arch_do_displaced_inst().
      (setf (sap-ref-32 sap 0) #x058B48 ; REX MOV [RIP-n]
            (signed-sap-ref-32 sap 3) (- ea (+ (sap-int sap) 7))) ; disp
      (let ((i (if (/= (%fun-pointer-widetag fun) funcallable-instance-widetag)
                   7
                   (let ((disp8 (- (ash funcallable-instance-function-slot
                                        word-shift)
                                   fun-pointer-lowtag))) ; = 5
                     (setf (sap-ref-32 sap 7) (logior (ash disp8 24) #x408B48))
                     11))))
        (setf (sap-ref-32 sap i) #xFD60FF))) ; JMP [RAX-3]
    ;; Verify that the jump table size reads as  0.
    (aver (zerop (code-jump-table-words code)))
    ;; It is critical that there be a trailing 'uint16' of 0 in this object
    ;; so that CODE-N-ENTRIES reports 0.  By luck, there is exactly enough
    ;; room in the object to hold two 0 bytes. It would be easy enough to enlarge
    ;; by 2 words if it became necessary. The assertions makes sure we stay ok.
    (aver (zerop (code-n-entries code)))
    code))

;;; Return T if FUN can't be called without loading RAX with its descriptor.
;;; This is true of any funcallable instance which is not a GF, and closures.
(defun fun-requires-simplifying-trampoline-p (fun)
  (case (%fun-pointer-widetag fun)
    (#.sb-vm:closure-widetag t)
    (#.sb-vm:funcallable-instance-widetag
     ;; if the FIN has no raw words then it has no internal trampoline
     (sb-kernel::bitmap-all-taggedp (%fun-layout fun)))))

;; TODO: put a trampoline in all fins and allocate them anywhere.
;; Revision e7cd2bd40f5b9988 caused some FINs to go in dynamic space
;; which is fine, but those fins need to have a default bitmap of -1 instead
;; of a special bitmap because we examine the bitmap when deciding whether
;; the FIN can be installed into an FDEFN without needing an external trampoline.
;; The easiest way to achieve this intent is to default all bitmaps to -1,
;; then change it in the layout when writing raw words. A better fix would
;; try to allocate all FINs in immobile space until it is exhausted, then fallback
;; to dynamic space. The address of the fin is no longer an issue, since fdefns
;; can point to the entire address space, but the fixed-size immobile object
;; allocator doesn't returns 0 - it calls the monitor if it fails.

;; So ideally, all funcallable instances would resemble simple-funs for a
;; small added cost of 2 words per object. It will be necessary to have the GC
;; treat ambiguous interior pointers to the unboxed words in the same way as
;; any code pointer. Placing FINs on pages marked as containing code will allow
;; the conservative root check to be skipped for obviously non-code objects.

;; Also we will need to write the embedded trampoline either in a word index
;; that differs based on length of the FIN, or place the boxed slots after
;; the trampoline. As of now, this can only deal with standard GFs.
;; The primitive object has 2 descriptor slots (fin-fun and CLOS slot vector)
;; and 2 non-descriptor slots containing machine instructions, after the
;; self-pointer (trampoline) slot. Scavenging the self-pointer is unnecessary
;; though harmless. This intricate and/or obfuscated calculation of #b110
;; is insensitive to the index of the trampoline slot, probably.
(defun make-immobile-funinstance (layout slot-vector)
  (let ((gf (truly-the funcallable-instance
             (alloc-immobile-fixedobj 6 ; KLUDGE
                                      (logior (ash 5 n-widetag-bits)
                                              funcallable-instance-widetag)))))
    ;; Assert that raw bytes will not cause GC invariant lossage
    (aver (not (sb-kernel::bitmap-all-taggedp layout)))
    ;; Set layout prior to writing raw slots
    (setf (%fun-wrapper gf) layout)
    ;; just being pedantic - liveness is preserved by the stack reference.
    (with-pinned-objects (gf)
      (let* ((addr (logandc2 (get-lisp-obj-address gf) lowtag-mask))
             (sap (int-sap addr))
             (insts-offs (ash (1+ funcallable-instance-info-offset) word-shift)))
        (setf (sap-ref-word sap (ash funcallable-instance-trampoline-slot word-shift))
              (truly-the word (+ addr insts-offs))
              (sap-ref-word sap insts-offs) #xFFFFFFE9058B48  ; MOV RAX,[RIP-23]
              (sap-ref-32 sap (+ insts-offs 7)) #x00FD60FF))) ; JMP [RAX-3]
    (setf (%funcallable-instance-info gf 0) slot-vector)
    gf))

(defun fdefn-has-static-callers (fdefn)
  (declare (type fdefn fdefn))
  (with-pinned-objects (fdefn)
    (logbitp 7 (sap-ref-8 (int-sap (get-lisp-obj-address fdefn))
                          (- 1 other-pointer-lowtag)))))

(defun set-fdefn-has-static-callers (fdefn newval)
  (declare (type fdefn fdefn) (type bit newval))
  (if (= newval 0)
      (%primitive unset-fdefn-has-static-callers fdefn)
      (%primitive set-fdefn-has-static-callers fdefn))
  fdefn)

(defun %set-fdefn-fun (fdefn fun)
  (declare (type fdefn fdefn) (type function fun)
           (values function))
  (when (fdefn-has-static-callers fdefn)
    (remove-static-links fdefn))
  (let ((trampoline (when (fun-requires-simplifying-trampoline-p fun)
                      (make-simplifying-trampoline fun)))) ; a newly made CODE object
    (with-pinned-objects (fdefn trampoline fun)
      (let* ((jmp-target
              (if trampoline
                  ;; Jump right to code-instructions + N. There's no simple-fun.
                  (sap-int (sap+ (code-instructions trampoline)
                                 trampoline-entry-offset))
                  ;; CLOSURE-CALLEE accesses the self pointer of a funcallable
                  ;; instance w/ builtin trampoline, or a simple-fun.
                  ;; But the result is shifted by N-FIXNUM-TAG-BITS because
                  ;; CELL-REF yields a descriptor-reg, not an unsigned-reg.
                  (get-lisp-obj-address (%closure-callee fun)))))
        (%primitive set-fdefn-fun fdefn fun jmp-target))))
  fun)

) ; end PROGN

;;; Find an immobile FDEFN or FUNCTION given an interior pointer to it.
#+immobile-space
(defun find-called-object (address)
  (let ((obj (alien-funcall (extern-alien "search_all_gc_spaces"
                                          (function unsigned unsigned))
                            address)))
    (unless (eql obj 0)
      (case (sap-ref-8 (int-sap obj) 0)
        (#.code-header-widetag
         (%simple-fun-from-entrypoint
          (make-lisp-obj (logior obj other-pointer-lowtag))
          address))
        (#.fdefn-widetag
         (make-lisp-obj (logior obj other-pointer-lowtag)))
        (#.funcallable-instance-widetag
         (make-lisp-obj (logior obj fun-pointer-lowtag)))))))

;;; Compute the PC that FDEFN will jump to when called.
#+immobile-code
(defun fdefn-raw-addr (fdefn)
  (sap-ref-word (int-sap (get-lisp-obj-address fdefn))
                (- (ash fdefn-raw-addr-slot word-shift) other-pointer-lowtag)))

;;; Undo the effects of XEP-ALLOCATE-FRAME
;;; and point PC to FUNCTION
(defun context-call-function (context function &optional arg-count)
  (with-pinned-objects (function)
    (let ((rsp (decf (context-register context rsp-offset) n-word-bytes))
          (rbp (context-register context rbp-offset))
          (fun-addr (get-lisp-obj-address function)))
      (setf (sap-ref-word (int-sap rsp) 0)
            (sap-ref-word (int-sap rbp) 8))
      (when arg-count
        (setf (context-register context rcx-offset)
              (get-lisp-obj-address arg-count)))
      (setf (context-register context rax-offset) fun-addr)
      (set-context-pc context (sap-ref-word (int-sap fun-addr)
                                            (- (ash simple-fun-self-slot word-shift)
                                               fun-pointer-lowtag))))))

(defun singly-occurs-p (thing things &aux (len (length things)))
  ;; Return T if THING occurs exactly once in vector THINGS,
  ;; assuming that it occurs at all.
  (declare (simple-vector things))
  (dotimes (i len)
    (when (eq (svref things i) thing)
      ;; re-using I as the index is OK because we leave the outer loop
      ;; after this.
      (return (loop (cond ((>= (incf i) len) (return t))
                          ((eq thing (svref things i)) (return nil))))))))

(define-load-time-global *static-linker-lock* (sb-thread:make-mutex :name "static linker"))

(define-load-time-global *never-statically-link* '(find-package))
;;; Remove calls via fdefns from CODE. This is called after compiling
;;; to memory, or when saving a core.
;;; Do not replace globally notinline functions, because notinline has
;;; an extra connotation of ensuring that replacement of the function
;;; under that name always works. It usually works to replace a statically
;;; linked function, but with a caveat: un-statically-linking requires calling
;;; MAP-OBJECTS-IN-RANGE, which is unreliable in the presence of
;;; multiple threads. Unfortunately, some users dangerously redefine
;;; builtin functions, and moreover, while there are multiple threads.
(defun statically-link-code-obj (code fixups)
  (declare (ignorable code fixups))
  (unless (immobile-space-obj-p code)
    (return-from statically-link-code-obj code))
  #+immobile-code
  (let* ((fdefns-start (+ code-constants-offset
                          (* code-slots-per-simple-fun (code-n-entries code))))
         (fdefns-count (the index (code-n-named-calls code)))
         (replacements (make-array fdefns-count :initial-element nil))
         (ambiguous (make-array fdefns-count :initial-element 0 :element-type 'bit))
         (any-replacements)
         (any-ambiguous))
    ;; For each fdefn, decide two things:
    ;; * whether the fdefn can be replaced by its function - possible only when
    ;;   that function is in immobile space and needs no trampoline.
    ;; * whether the replacement creates ambiguitity - if #'F and #'G are the same
    ;;   function, then substituting that function in for the fdefn of F and G
    ;;   requires storing locations at which replacement was done
    (dotimes (i fdefns-count)
      (let* ((fdefn (code-header-ref code (+ fdefns-start i)))
             (fun (when (fdefn-p fdefn) (fdefn-fun fdefn))))
        (when (and (immobile-space-obj-p fun)
                   (not (fun-requires-simplifying-trampoline-p fun))
                   (not (member (fdefn-name fdefn) *never-statically-link* :test 'equal))
                   (neq (info :function :inlinep (fdefn-name fdefn)) 'notinline))
          (setf any-replacements t (aref replacements i) fun))))
    (dotimes (i fdefns-count)
      (when (and (aref replacements i)
                 (not (singly-occurs-p (aref replacements i) replacements)))
        (setf any-ambiguous t (bit ambiguous i) 1)))
    (unless any-replacements
      (return-from statically-link-code-obj))
    ;; Map each fixup to an index in REPLACEMENTS (which currently holds functions,
    ;; not fdefns, so we have to scan the code header).
    ;; This can be done outside the lock
    (flet ((index-of (fdefn)
             (dotimes (i fdefns-count)
               (when (eq fdefn (code-header-ref code (+ fdefns-start i)))
                 (return i)))))
      (setq fixups (mapcar (lambda (fixup) ; = (offset . #<fdefn>)
                             (cons (index-of (cdr fixup)) (car fixup)))
                           fixups)))
    (let ((insts (code-instructions code)))
      ;; One final check: if any of the fixed-up instructions is "MOV EAX, #xNNNN"
      ;; instead of a CALL or JMP, we can't fixup that particular fdefn for any
      ;; of its call sites. (They should all use MOV if any one does).
      ;; This happens when *COMPILE-TO-MEMORY-SPACE* is set to :AUTOMATIC.
      ;; In that case we don't know that the code will be within an imm32 of
      ;; the target address, because the code might have gone into dynamic space.
      (dolist (fixup fixups)
        (binding* ((fdefn-index (car fixup) :exit-if-null)
                   (offset (cdr fixup)))
          (when (and (aref replacements fdefn-index)
                     (not (eql (logior (sap-ref-8 insts (1- offset)) 1) #xE9)))
            (setf (aref replacements fdefn-index) nil))))
      (let ((stored-locs (if any-ambiguous
                             (make-array fdefns-count :initial-element nil))))
        (with-system-mutex (*static-linker-lock*)
          (dolist (fixup fixups)
            (binding* ((fdefn-index (car fixup) :exit-if-null)
                       (offset (cdr fixup))
                       (fdefn (code-header-ref code (+ fdefns-start fdefn-index)))
                       (fun (aref replacements fdefn-index)))
              (when fun
                ;; Set the statically-linked flag
                (sb-vm::set-fdefn-has-static-callers fdefn 1)
                (when (= (bit ambiguous fdefn-index) 1)
                  (push offset (aref stored-locs fdefn-index)))
                ;; Change the machine instruction
                ;; %CLOSURE-CALLEE reads the entry addresss word of any
                ;; kind of function, but as if it were a tagged fixnum.
                (let ((entry (descriptor-sap (%closure-callee fun))))
                  (setf (signed-sap-ref-32 insts offset)
                        (sap- entry (sap+ insts (+ offset 4))))))))
          ;; Replace ambiguous elements of the code header while still holding the lock
          (dotimes (i fdefns-count)
            (when (= (bit ambiguous i) 1)
              (let ((wordindex (+ fdefns-start i))
                    (locs (aref stored-locs i)))
                (setf (code-header-ref code wordindex)
                      (cons (code-header-ref code wordindex) locs)))))))))
  code)
