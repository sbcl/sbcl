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

;;;; :CODE-OBJECT fixups

;;; This gets called by LOAD to resolve newly positioned objects
;;; with things (like code instructions) that have to refer to them.
;;; Return KIND  if the fixup needs to be recorded in %CODE-FIXUPS.
;;; The code object we're fixing up is pinned whenever this is called.
(defun fixup-code-object (code offset fixup kind flavor)
  (declare (type index offset) (ignorable flavor))
  (let* ((sap (code-instructions code))
         (fixup (+ (if (eq kind :absolute64)
                       (signed-sap-ref-64 sap offset)
                       (signed-sap-ref-32 sap offset))
                   fixup)))
    (ecase kind
      (:absolute64
         ;; Word at sap + offset contains a value to be replaced by
         ;; adding that value to fixup.
         (setf (sap-ref-64 sap offset) fixup))
      (:absolute
         ;; Word at sap + offset contains a value to be replaced by
         ;; adding that value to fixup.
         (setf (sap-ref-32 sap offset) fixup))
      (:relative
         ;; Fixup is the actual address wanted.
         ;; Replace word with value to add to that loc to get there.
         ;; In the #-immobile-code case, there's nothing to assert.
         ;; Relative fixups don't exist with movable code.
         #+immobile-code
         (unless (immobile-space-obj-p code)
           (error "Can't compute fixup relative to movable object ~S" code))
         (setf (signed-sap-ref-32 sap offset)
               ;; JMP/CALL are relative to the next instruction,
               ;; so add 4 bytes for the size of the displacement itself.
               (- fixup (sap-int (sap+ sap (+ offset 4))))))))
  ;; An absolute fixup is stored in the code header's %FIXUPS slot if it
  ;; references an immobile-space (but not static-space) object.
  ;; Note that:
  ;;  (1) Call fixups occur in both :RELATIVE and :ABSOLUTE kinds.
  ;;      We can ignore the :RELATIVE kind, except for foreign call,
  ;;      as those point to the linkage table which has an absolute address
  ;;      and therefore might change in displacement from the call site
  ;;      if the immobile code space is relocated on startup.
  ;;  (2) :STATIC-CALL fixups point to immobile space, not static space.
  #+immobile-space
  (return-from fixup-code-object
    (case flavor
      ((:named-call :layout :immobile-symbol :symbol-value ; -> fixedobj subspace
        :assembly-routine :assembly-routine* :static-call) ; -> varyobj subspace
       (if (eq kind :absolute) :absolute))
      (:foreign
       ;; linkage-table calls using the "CALL rel32" format need to be saved,
       ;; because the linkage table resides at a fixed address.
       ;; Space defragmentation can handle the fixup automatically,
       ;; but core relocation can't - it can't find all the call sites.
       (if (eq kind :relative) :relative))))
  nil) ; non-immobile-space builds never record code fixups

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
                (sap-ref-double sap 8))))))

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
               (sap-ref-double sap 8) (imagpart value)))))))

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
                            ;; EMIT-SAP-REF always loads the EA into TEMP-REG-TN
                            ;; which points to the shadow, not the user memory now.
                            (ea (logxor (sb-vm:context-register
                                         context (tn-offset sb-vm::temp-reg-tn))
                                        msan-mem-to-shadow-xor-const)))
                       `(:raw ,ea ,nbytes ,value)))))
          (t
           (sb-kernel::decode-internal-error-args (sap+ pc 1) trap-number)))))


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
      (let ((i (if (/= (fun-subtype fun) funcallable-instance-widetag)
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
  (let ((kind (fun-subtype fun)))
    (or (and (eql kind sb-vm:funcallable-instance-widetag)
             ;; if the FIN has no raw words then it has no internal trampoline
             (eql (layout-bitmap (%funcallable-instance-layout fun))
                  sb-kernel:+layout-all-tagged+))
        (eql kind sb-vm:closure-widetag))))

(defconstant +fsc-layout-bitmap+
  (logxor (1- (ash 1 funcallable-instance-info-offset))
          (ash 1 (1- funcallable-instance-trampoline-slot))))

;;; This allocator is in its own function because the immobile allocator
;;; VOPs are impolite (i.e. bad) and trash all registers.
;;; Since there are no callee-saved registers, this makes it legit'
;;; to put in a separate function.
#+immobile-code
(defun alloc-immobile-funinstance ()
  (values (%primitive alloc-immobile-fixedobj fun-pointer-lowtag 6 ; kludge
                      (logior (ash 5 n-widetag-bits) funcallable-instance-widetag))))

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
#+immobile-code
(defun make-immobile-funinstance (layout slot-vector)
  (let ((gf (truly-the funcallable-instance (alloc-immobile-funinstance))))
    ;; Ensure that the layout has a bitmap indicating the indices of
    ;; non-descriptor slots. Doing this just-in-time is easiest, it turns out.
    (unless (= (layout-bitmap layout) +fsc-layout-bitmap+)
      (setf (layout-bitmap layout) +fsc-layout-bitmap+))
    ;; Set layout prior to writing raw slots
    (setf (%funcallable-instance-layout gf) layout)
    ;; just being pedantic - liveness is preserved by the stack reference.
    (with-pinned-objects (gf)
      (let* ((addr (logandc2 (get-lisp-obj-address gf) lowtag-mask))
             (sap (int-sap addr))
             (insts-offs (ash (1+ funcallable-instance-info-offset) word-shift)))
        (setf (sap-ref-word sap (ash funcallable-instance-trampoline-slot word-shift))
              (truly-the word (+ addr insts-offs))
              (sap-ref-word sap insts-offs) #xFFFFFFE9058B48  ; MOV RAX,[RIP-23]
              (sap-ref-32 sap (+ insts-offs 7)) #x00FD60FF))) ; JMP [RAX-3]
    (%set-funcallable-instance-info gf 0 slot-vector)
    gf))

#+immobile-space
(defun alloc-immobile-fdefn ()
  (or #+nil ; Avoid creating new objects in the text segment for now
      (and (= (alien-funcall (extern-alien "lisp_code_in_elf" (function int))) 1)
           (allocate-immobile-obj (* fdefn-size n-word-bytes)
                                  (logior (ash undefined-fdefn-header 16)
                                          fdefn-widetag) ; word 0
                                  0 other-pointer-lowtag nil)) ; word 1, lowtag, errorp
      (values (%primitive alloc-immobile-fixedobj other-pointer-lowtag
                          fdefn-size
                          (logior (ash undefined-fdefn-header 16)
                                  fdefn-widetag))))) ; word 0

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

;;; CALL-DIRECT-P when true indicates that at the instruction level
;;; it is fine to change the JMP or CALL instruction - i.e. the offset is
;;; a (SIGNED-BYTE 32), and the function can be called without loading
;;; its address into RAX, *and* there is a 1:1 relation between the fdefns
;;; and fdefn-funs for all callees of the code that contains FUN.
;;; The 1:1 constraint is due to a design limit - when removing static links,
;;; it is impossible to distinguish fdefns that point to the same called function.
;;; FIXME: It would be a nice to remove the uniqueness constraint, either
;;; by recording ay ambiguous fdefns, or just recording all replacements.
(defun call-direct-p (fun code-header-funs)
  #-immobile-code (declare (ignore fun code-header-funs))
  #+immobile-code
  (flet ((singly-occurs-p (thing things &aux (len (length things)))
           ;; Return T if THING occurs exactly once in vector THINGS.
           (declare (simple-vector things))
           (dotimes (i len)
             (when (eq (svref things i) thing)
               ;; re-using I as the index is OK because we leave the outer loop
               ;; after this.
               (return (loop (cond ((>= (incf i) len) (return t))
                                   ((eq thing (svref things i)) (return nil)))))))))
    (and (immobile-space-obj-p fun)
         (not (fun-requires-simplifying-trampoline-p fun))
         (singly-occurs-p fun code-header-funs))))

;;; Allocate a code object.
(defun alloc-dynamic-space-code (total-words)
  (values (%primitive alloc-dynamic-space-code (the fixnum total-words))))

;;; Remove calls via fdefns from CODE when compiling into memory.
(defun statically-link-code-obj (code fixups)
  (declare (ignorable code fixups))
  #+immobile-code
  (let ((insts (code-instructions code))
        (fdefns)) ; group by fdefn
    (loop for (offset . name) in fixups
          do (binding* ((fdefn (find-fdefn name) :exit-if-null)
                        (cell (assq fdefn fdefns)))
               (if cell
                   (push offset (cdr cell))
                   (push (list fdefn offset) fdefns))))
    (let ((funs (make-array (length fdefns))))
      (sb-thread::with-system-mutex (sb-c::*static-linker-lock*)
        (loop for i from 0 for (fdefn) in fdefns
              do (setf (aref funs i) (fdefn-fun fdefn)))
        (dolist (fdefn-use fdefns)
          (let* ((fdefn (car fdefn-use))
                 (callee (fdefn-fun fdefn)))
            ;; Because we're holding the static linker lock, the elements of
            ;; FUNS can not change while this test is performed.
            (when (call-direct-p callee funs)
              (let ((entry (sb-vm::fdefn-raw-addr fdefn)))
                (dolist (offset (cdr fdefn-use))
                  ;; Only a CALL or JMP will get statically linked.
                  ;; A MOV will always load the address of the fdefn.
                  (when (eql (logior (sap-ref-8 insts (1- offset)) 1) #xE9)
                    ;; Set the statically-linked flag
                    (sb-vm::set-fdefn-has-static-callers fdefn 1)
                    ;; Change the machine instruction
                    (setf (signed-sap-ref-32 insts offset)
                          (- entry (+ (sap-int (sap+ insts offset)) 4)))))))))))))
