;;; Allocation profiler

;;; Quick start: To enable this code, you must DECLAIM or DECLARE
;;;  (OPTIMIZE SB-C::INSTRUMENT-CONSING)
;;; to affect functions in which instrumentation should be inserted.
;;; After that, the simplest way to run the function under test is to pass
;;; it to APROF-RUN. Alternatively, collection can be toggled with APROF-START
;;; and APROF-STOP, but some setup is needed (as done by APROF-RUN).
;;;
;;; Note: This is x86-64 only at the moment.

;;; Profiling works via compile-time instrumentation.
;;; Each point in code emitted by ALLOCATION adds a few instructions
;;; prior to the actual inline allocation sequence. The extra instructions
;;; are normally disabled (there is a jump around them).  When the profiler
;;; is switched on, the instructions are modified to atomically increment
;;; a counter (or two). If the allocation is a compile-time fixed size,
;;; only one counter is needed. If the size is variable, then two counters
;;; are used: hits and total bytes.
;;; The array of counters is prepared by the profiler when first started.
;;; The index into the array for any particular counter is determined
;;; the first time that counter is hit.  Subsequent hits are cheap.

;;; On x86-64, instrumented code initially looks like this:

;;;  9E98:       4D8B5D18         MOV R11, [R13+24]      ; thread.profile-data
;;;  9E9C:       4D85DB           TEST R11, R11
;;;  9E9F:       EB17             JMP L21
;;;  9EA1:       0F1F8000000000   NOP
;;;  9EA8:       E8D366FDFF       CALL #x21B00580        ; ENABLE-SIZED-ALLOC-COUNTER
;;;  9EAD:       0F1F00           NOP
;;;  9EB0:       4885C9           TEST RCX, RCX
;;;  9EB3:       0F1F440000       NOP
;;;  9EB8: L21:  4D8B5D20         MOV R11, [R13+32]      ; thread.alloc-region
;;;
;;; The first TEST instruction's result is ignored.
;;; The second TEST conveys the register holding the size to allocate in bytes.

;;; After enabling profiling, the first JMP becomes a JEQ so that
;;; if the data buffer exists, the CALL is performed.
;;; If this code is actually reached, and the call performed,
;;; then the call instruction is modified to (something like):
;;;
;;;  9E98:       4D8B5D18         MOV R11, [R13+24]      ; thread.profile-data
;;;  9E9C:       4D85DB           TEST R11, R11
;;;  9E9F:       7417             JEQ L21
;;;  9EA1:       0F1F8000000000   NOP
;;;  9EA8:       F049FF83C8020000 LOCK INC QWORD PTR [R11+712]
;;;  9EB0:       F049018BD0020000 LOCK ADD [R11+720], RCX
;;;  9EB8: L21:  4D8B5D20         MOV R11, [R13+32]      ; thread.alloc-region

;;; All of the modifications to running code can be made safely
;;; even if multiple threads are in that code at the same time, because:
;;;
;;; * changing a JMP to a JNE requires writing only 1 byte.
;;;   Instruction fetch will either see the new byte or the old,
;;;   and it doesn't matter which.
;;;
;;; * changing CALL to LOCK INC writes one naturally aligned octword.
;;;   Moreover, RIP gets decremented so that the next fetch won't read
;;;   the NOP or only part of the bytes of the LOCK INC.

;;; See the example run at the bottom of this file.

(defpackage #:sb-aprof
  (:use #:cl #:sb-ext #:sb-unix #:sb-alien #:sb-sys #:sb-int
        #:sb-kernel)
  (:export #:aprof-run #:aprof-show)
  (:import-from #:sb-di #:valid-lisp-pointer-p)
  (:import-from #:sb-disassem #:dstate-inst-properties)
  (:import-from #:sb-vm #:rbp-offset)
  (:import-from #:sb-x86-64-asm
                #:lock #:x66 #:rex #:+rex-b+
                #:inst-operand-size
                #:register-p #:reg-num
                #:machine-ea-p
                #:machine-ea-base
                #:machine-ea-index
                #:machine-ea-disp
                #:regrm-inst-reg #:ext-regrm-inst-reg
                #:regrm-inst-r/m #:ext-regrm-inst-r/m
                #:reg-imm-data
                #:reg/mem-imm-data
                #:add #:xadd #:inc #:mov #:lea #:cmp #:xor #:jmp
                #:|push| #:|pop| #:|or| #:|call| #:|break|))

(in-package #:sb-aprof)
(setf (system-package-p *package*) t)

(defstruct (alloc (:constructor make-alloc (bytes count type pc)))
  bytes count type pc)

(defvar *allocation-profile-metadata* nil)

(defvar *allocation-fixups-installed*
  (make-hash-table :test 'eq :weakness :key :synchronized t))

(define-alien-variable alloc-profile-buffer system-area-pointer)
(defun aprof-reset ()
  (alien-funcall (extern-alien "memset" (function void system-area-pointer int size-t))
                 alloc-profile-buffer
                 0
                 (* (/ (length *allocation-profile-metadata*) 2)
                    sb-vm:n-word-bytes)))

(defun patch-fixups ()
  (let ((n-fixups 0)
        (n-patched 0)
        (from-ht sb-c::*allocation-point-fixups*)
        (to-ht *allocation-fixups-installed*))
    (when (plusp (hash-table-count from-ht))
      (dohash ((code fixups) from-ht)
        (do-packed-varints (loc fixups)
          (incf n-fixups)
          (let ((byte (sap-ref-8 (code-instructions code) loc)))
            (when (eql byte #xEB)
              (setf (sap-ref-8 (code-instructions code) loc) #x74) ; JEQ
              (incf n-patched))))
        (setf (gethash code to-ht) fixups)
        (remhash code from-ht)))
    (values n-fixups n-patched)))

(defun aprof-start ()
  (let ((v *allocation-profile-metadata*))
    (unless v
      (setq v (make-array 100000) *allocation-profile-metadata* v)
      (with-pinned-objects (v)
        (setf (extern-alien "alloc_profile_data" unsigned)
              (sb-kernel:get-lisp-obj-address v)))))
  (alien-funcall (extern-alien "allocation_profiler_start" (function void))))

(defun aprof-stop ()
  (alien-funcall (extern-alien "allocation_profiler_stop" (function void))))

(defconstant +state-initial+              1)
(defconstant +state-profiler+             2)
(defconstant +state-begin-pa+             3)
(defconstant +state-loaded-free-ptr+      4)
(defconstant +state-bumped-free-ptr+      5)
(defconstant +state-tested-free-ptr+      6)
(defconstant +state-jumped+               7)
(defconstant +state-stored-free-ptr+      8)
(defconstant +state-lowtag-only+          9)
(defconstant +state-low-then-widetag+    10)
(defconstant +state-widetag-only+        11)
(defconstant +state-wide-then-lowtag+    12)
(defconstant +state-trampoline-arg+      13)
(defconstant +state-called+              14)
(defconstant +state-result-popped+       15)
(defconstant +state-end-pa+              16)
(defconstant +state-test-interrupted+    17)
(defconstant +state-pa-trap+             18)

(defglobal *tag-to-type*
  (map 'vector
       (lambda (x)
        (cond ((sb-vm::specialized-array-element-type-properties-p x)
               (let ((et (sb-vm:saetp-specifier x)))
                 (sb-kernel:type-specifier
                  (sb-kernel:specifier-type `(simple-array ,et 1)))))
              (x
               (sb-vm::room-info-name x))))
       sb-vm::*room-info*))

(defun layout-name (ptr)
  (if (eql (valid-lisp-pointer-p (int-sap ptr)) 0)
      'structure
      (layout-classoid-name (make-lisp-obj ptr))))

;;; map-segment-instructions is really deficient in providing an intelligent
;;; decoding of the bits, as they're wired into the instruction printer.
;;; So figure out what kind of MOV instruction we have, unless it's
;;; immediate-to-register (I should probably rememdy that).
(defun infer-mov-direction (dchunk)
  (case (logand dchunk #xFF)
    ((#xC6 #xC7) :store)  ; immediate-to-memory (low bit = size)
    (#x89 :store)  ; register-to-memory
    (#x8B :load))) ; memory-to-register

;;; The compiler emits no additional metadata per code component
;;; other than the addresses of the enabling JMPs.
;;; So we disassemble just enough instructions at PC to deduce
;;; what type of object is being allocated.
(defun infer-type (pc component)
  (declare (ignorable pc component))
  #+(and x86-64 sb-thread)
  (let* ((seg (sb-disassem::%make-segment
               :sap-maker (lambda () (error "Bad sap maker"))
               :virtual-location 0))
         (dstate (sb-disassem:make-dstate nil))
         (allocator-state +state-initial+)
         (profiler-index)    ; the index number of this allocation point
         (orig-free-ptr-reg) ; free-pointer before bump up
         (free-ptr-reg)      ; free-pointer aftr bump up
         (target-reg)        ; register holding the new allocation
         (lowtag)
         (widetag)
         (size)
         (thread-base-reg 13))
    (declare (type (or null (unsigned-byte 4)) target-reg))
    (setf (sb-disassem::seg-object seg) component
          (sb-disassem:seg-virtual-location seg) pc
          (sb-disassem:seg-length seg) 100 ; arb
          (sb-disassem:seg-sap-maker seg)
          (let ((sap (int-sap pc))) (lambda () sap)))
    (macrolet ((fail ()
                `(return-from fail))
               (advance (newstate)
                 `(setq allocator-state ,newstate))
               (advance-if (condition newstate)
                 `(if ,condition (advance ,newstate) (fail))))
      (block fail
        (flet ((pseudoatomic-flag-p ()
                 (let ((rm (regrm-inst-r/m 0 dstate)))
                   (eq (machine-ea-base rm) thread-base-reg)
                   (eq (machine-ea-disp rm)
                       (ash sb-vm::thread-pseudo-atomic-bits-slot
                            sb-vm:word-shift))))
               (infer-layout (opcode ea)
                 (cond ((and (eql lowtag sb-vm:instance-pointer-lowtag)
                             (eq opcode 'mov)
                             (eq (inst-operand-size dstate) :dword)
                             (eql (machine-ea-base ea) target-reg)
                             (not (machine-ea-index ea))
                             (eql (machine-ea-disp ea) (- 4 lowtag)))
                        (return-from infer-type
                          (values (layout-name (reg/mem-imm-data 0 dstate))
                                  size)))
                       (t
                        (fail)))))
          (sb-disassem::map-segment-instructions
           (lambda (dchunk inst)
             (let* ((opcode (sb-disassem::inst-name inst))
                    (opcode-byte (logand dchunk #xFF))
                    (ea (case opcode
                          (xadd (ext-regrm-inst-r/m 0 dstate))
                          ;; 64-bit mode 'inc' has reg/mem format,
                          ;; never the single-byte format
                          ((add inc mov lea cmp) (regrm-inst-r/m 0 dstate))))
                    (free-ptr-p
                     (and (machine-ea-p ea)
                          (eql (machine-ea-base ea) thread-base-reg)
                          (eql (machine-ea-disp ea)
                               (ash sb-vm::thread-alloc-region-slot
                                    sb-vm:word-shift))
                          (not (machine-ea-index ea))))
                    (header-word-p
                     (and target-reg lowtag
                          (machine-ea-p ea)
                          (eql (machine-ea-base ea) target-reg)
                          (eql (machine-ea-disp ea) (- lowtag))
                          (not (machine-ea-index ea)))))
               (unless (member opcode '(rex lock x66))
                 ;; FIXME: these ignore the direction of load/store/compare
                 (ecase allocator-state
                   (#.+state-initial+
                    (advance-if (and (eq opcode 'inc)
                                     (eql (machine-ea-base ea) 11)
                                     (null (machine-ea-index ea)))
                                +state-profiler+)
                    (setq profiler-index (machine-ea-disp ea)))
                   (#.+state-profiler+
                    (cond
                      ((and (eq opcode 'add) ; possibly stay in +state-profiler+
                            (machine-ea-p ea)
                            (eql (machine-ea-base ea) 11)
                            (null (machine-ea-index ea))
                            (eql (machine-ea-disp ea)
                                 (+ profiler-index sb-vm:n-word-bytes))))
                      (t
                       (advance-if (and (eq opcode 'mov) (pseudoatomic-flag-p))
                                   +state-begin-pa+))))
                   (#.+state-begin-pa+
                    (cond ((eq opcode '|push|)
                           ;; Known huge allocation goes straight to C call
                           (setq size (ldb (byte 32 8) dchunk))
                           (advance +state-trampoline-arg+))
                          (t
                           (advance-if (and (eq opcode 'mov) free-ptr-p)
                                       +state-loaded-free-ptr+)
                           (setq orig-free-ptr-reg (reg-num (regrm-inst-reg dchunk dstate))))))
                   (#.+state-loaded-free-ptr+
                    (case opcode
                      ;; Variable-size alloc can use LEA, ADD, or XADD to compute the
                      ;; new free ptr depending on whether the alloc-tn and the size
                      ;; are in the same register (ADD,XADD) or different (LEA).
                      (lea
                       (advance-if (or (and (not (machine-ea-index ea))
                                            (plusp (machine-ea-disp ea))
                                            (eql (machine-ea-base ea) orig-free-ptr-reg))
                                       (and (not (machine-ea-disp ea))
                                            (or (eql (machine-ea-index ea) orig-free-ptr-reg)
                                                (eql (machine-ea-base ea) orig-free-ptr-reg))))
                                   +state-bumped-free-ptr+)
                       (setq free-ptr-reg (regrm-inst-reg dchunk dstate)
                             size (machine-ea-disp ea)))
                      (add
                       (advance-if (register-p ea) +state-bumped-free-ptr+)
                       (setq free-ptr-reg ea))
                      (xadd
                       (advance-if (and (register-p ea) (eql (reg-num ea) orig-free-ptr-reg))
                                   +state-bumped-free-ptr+)
                       (setq target-reg (reg-num (ext-regrm-inst-reg dchunk dstate))
                             free-ptr-reg ea))
                      (t (fail))))
                   (#.+state-bumped-free-ptr+
                    (advance-if (and (eq opcode 'cmp)
                                     (eql (machine-ea-base ea) thread-base-reg)
                                     (eql (machine-ea-disp ea)
                                          (ash (1+ sb-vm::thread-alloc-region-slot)
                                               sb-vm:word-shift))
                                     (not (machine-ea-index ea))
                                     (eql (regrm-inst-reg dchunk dstate) free-ptr-reg))
                                +state-tested-free-ptr+))
                   (#.+state-tested-free-ptr+
                    (advance-if (eq opcode 'jmp) +state-jumped+))
                   (#.+state-jumped+
                    (advance-if (and (eq opcode 'mov) free-ptr-p
                                     (eql (regrm-inst-reg dchunk dstate) free-ptr-reg))
                                +state-stored-free-ptr+))
                   (#.+state-stored-free-ptr+
                    ;; lowtag/widetag can be assigned in either order
                    (case opcode
                      (lea ; convert to descriptor before writing widetag
                       (cond ((and (eql (machine-ea-base ea) orig-free-ptr-reg)
                                   (not (machine-ea-index ea))
                                   (<= 0 (machine-ea-disp ea) sb-vm:lowtag-mask))
                              (advance +state-lowtag-only+)
                              (setq target-reg (reg-num (regrm-inst-reg dchunk dstate))
                                    lowtag (machine-ea-disp ea))
                              (when (= lowtag sb-vm:list-pointer-lowtag)
                                (return-from infer-type (values 'list size))))
                             ((and (eql (machine-ea-base ea) orig-free-ptr-reg)
                                   (> (machine-ea-disp ea) (* 2 sb-vm:n-word-bytes))
                                   (not (machine-ea-index ea)))
                              ;; do nothing, this is probably a cons, we're computing
                              ;; the next cell address, and the car was 0 so wasn't stored
                              )
                             (t (fail))))
                      (mov
                       ;; Some moves ares ignorable, those which either load from
                       ;; the constant pool or store to the newly allocated memory.
                       ;; LIST and LIST* sometimes do those. Writing an immediate value
                       ;; to the car of a cons resembles a widetag store,
                       ;; so we transition into widetagged state, rightly or not.
                       (let ((dir (infer-mov-direction dchunk)))
                         (ecase dir
                          (:load
                           (cond ((or (eql (machine-ea-base ea) rbp-offset) ; load from frame
                                      (eq (machine-ea-base ea) :rip)) ; load from constant pool
                                  ) ; do nothing
                                 (t (fail))))
                          (:store
                           ;; widetag stored before ORing in lowtag
                           (unless target-reg
                             (setq target-reg orig-free-ptr-reg))
                           (cond ((and (eql (machine-ea-base ea) target-reg)
                                       (not (machine-ea-disp ea))
                                       (not (machine-ea-index ea)))
                                  (advance +state-widetag-only+)
                                  (setq widetag (if (eq (inst-operand-size dstate) :qword)
                                                    :variable
                                                    (logand (reg/mem-imm-data 0 dstate) #xFF)))
                                  ;; Done, unless we need to scan more in order to infer the layout
                                  (when (and (integerp widetag)
                                             (not (member widetag `(,sb-vm:funcallable-instance-widetag
                                                                    ,sb-vm:instance-widetag))))
                                    (return-from infer-type
                                      (values (aref *tag-to-type* widetag) size))))
                                 ((and (eql (machine-ea-base ea) target-reg)
                                       (not (machine-ea-index ea))
                                       (machine-ea-disp ea))) ; ignore
                                 (t (fail)))))))
                      (|or|
                       (cond ((and (not lowtag)
                                   (eq (reg/mem-imm-data 0 dstate) sb-vm:list-pointer-lowtag))
                              (return-from infer-type (values 'list size)))
                             (t (fail))))
                      (t (fail))))
                   (#.+state-lowtag-only+
                    (unless (eq opcode 'mov)
                      (fail))
                    (case lowtag
                      (#.sb-vm:other-pointer-lowtag
                       (unless header-word-p (fail))
                       (return-from infer-type
                         ;; KLUDGE: non-immediate is qword, immediate is anything but that.
                         (if (member (inst-operand-size dstate) '(:byte :word :dword))
                             (values (aref *tag-to-type* (logand (reg/mem-imm-data 0 dstate) #xFF))
                                     size)
                             (values '#:|unknown| nil))))
                      (#.sb-vm:instance-pointer-lowtag
                       (unless header-word-p (fail))
                       ;; META: WTF does this KLUDGE comment mean? We're not computing a length.
                       ;; KLUDGE: computed length is a :qword (though it should be a :dword),
                       ;; and immediate is :word or :dword.
                       (when (eq (inst-operand-size dstate) :qword)
                         (return-from infer-type (values 'instance nil)))
                       (advance-if (and (member (inst-operand-size dstate) '(:byte :word :dword))
                                        (eql (logand (reg/mem-imm-data 0 dstate) #xFF)
                                             sb-vm:instance-widetag))
                                   +state-low-then-widetag+))
                      (#.sb-vm:fun-pointer-lowtag
                       ;; CLOSURE allocation performs a MOV immediate-to-register,
                       ;; then ORs in the function layout, then stores.
                       ;; Also note that FIN must use REG/MEM-IMM-DATA to read the
                       ;; immediate operand where closure uses REG-IMM-DATA.
                       (when (and (register-p ea)
                                  (eql (logand (reg-imm-data 0 dstate) #xFF)
                                       sb-vm:closure-widetag))
                         (return-from infer-type (values 'closure size)))
                       (unless header-word-p (fail))
                       (if (and (member (inst-operand-size dstate) '(:word :dword))
                                (eql (logand (reg/mem-imm-data 0 dstate) #xFF)
                                     sb-vm:funcallable-instance-widetag))
                           (return-from infer-type (values 'funcallable-instance size))
                           ;; Not sure the subtype, but it must be a function
                           (return-from infer-type (values 'function size))))
                      (t
                       (fail))))
                   (#.+state-low-then-widetag+
                    (infer-layout opcode ea))
                   (#.+state-widetag-only+
                    (cond
                     ((eq opcode 'mov)
                      (let ((dir (infer-mov-direction dchunk)))
                        ;; load from the constants is ignorable.
                        ;; storing through the untagged pointer is ignorable.
                        (cond ((and (eq dir :load) (eq (machine-ea-base ea) :rip)))
                              ((and (eql (machine-ea-base ea) target-reg)
                                    (null (machine-ea-index ea))))
                              (t
                               (fail)))))
                     ((eq opcode 'lea)
                      (cond ((and (eql (machine-ea-base ea) orig-free-ptr-reg)
                                  (null (machine-ea-index ea))
                                  (eql (machine-ea-disp ea) sb-vm:list-pointer-lowtag))
                             (return-from infer-type (values 'list size)))
                            ;; computing the CDR of the first cons in a list of 2, presumably
                            ((and (eql (machine-ea-base ea) target-reg)
                                  (null (machine-ea-index ea))))
                            (t
                             (fail))))
                     (t
                      (advance-if (and (eq opcode '|or|)
                                       ;; TODO: AVER correct register as well
                                       (not lowtag))
                                  +state-wide-then-lowtag+)
                      (setq lowtag (reg/mem-imm-data 0 dstate))
                      (ecase lowtag
                       (#.sb-vm:other-pointer-lowtag
                        (return-from infer-type
                         (values (if (eq widetag :variable)
                                     'unknown
                                   (aref *tag-to-type* widetag))
                                 size)))
                       (#.sb-vm:instance-pointer-lowtag)
                       (#.sb-vm:list-pointer-lowtag
                        (return-from infer-type (values 'list size)))
                       (#.sb-vm:fun-pointer-lowtag
                        (return-from infer-type (values 'function size)))))))
                   (#.+state-wide-then-lowtag+
                    (advance-if (and (eq opcode 'xor) (pseudoatomic-flag-p))
                                +state-end-pa+))
                   (#.+state-end-pa+
                    (advance-if (eq opcode 'jmp) +state-test-interrupted+))
                   (#.+state-test-interrupted+
                    (advance-if (eq opcode '|break|) +state-pa-trap+))
                   (#.+state-pa-trap+
                    (infer-layout opcode ea))
                   (#.+state-trampoline-arg+
                    (advance-if (eq opcode '|call|) +state-called+))
                   (#.+state-called+
                    (cond ((and (eq opcode '|pop|) (<= #x58 opcode-byte #x5F))
                           (setq target-reg
                                 (+ (if (logtest +rex-b+ (dstate-inst-properties dstate))
                                        8 0)
                                    (- opcode-byte #x58)))
                           (advance +state-result-popped+))
                          (t
                           (fail))))
                   (#.+state-result-popped+
                    (case opcode
                      (mov
                       (advance-if (and (not (machine-ea-index ea))
                                        (eq (machine-ea-base ea) target-reg)
                                        (null (machine-ea-disp ea)))
                                   +state-widetag-only+)
                       (setq widetag (logand (reg/mem-imm-data 0 dstate) #xFF)))
                      (|or|
                           (if (eql opcode-byte #x0C)
                               ;; OR AL, $byte (2 byte encoding)
                               (setq lowtag (ldb (byte 8 8) dchunk))
                               ;; OR other-reg, $byte ; (3 byte encoding)
                               (setq lowtag (ldb (byte 8 16) dchunk)))
                           (advance +state-lowtag-only+))
                      (t
                       (fail))))))))
           seg dstate))))
    (cerror "" "fail @ ~x in ~x state ~d from=~x"
            (sb-disassem:dstate-cur-addr dstate)
            component allocator-state pc)
    (values nil nil)))

;;; Return a name for PC-OFFS in CODE. PC-OFFSET is relative
;;; to CODE-INSTRUCTIONS.
(defun pc-offs-to-fun-name (pc-offs code &aux (di (%code-debug-info code)))
  (if (consp di) ; assembler routines
      (block nil
       (maphash (lambda (k v) ; FIXME: OAOO violation, at least twice over
                  (when (<= (car v) pc-offs (cadr v))
                    (return k)))
                (car di)))
      (let* ((funmap (sb-c::compiled-debug-info-fun-map di)))
        (unless (sb-c::compiled-debug-fun-next funmap)
          (aver (typep funmap 'sb-c::compiled-debug-fun-toplevel))
          (return-from pc-offs-to-fun-name :toplevel))
        (loop for fun = funmap then next
              for next = (sb-c::compiled-debug-fun-next fun)
              when (or (not next)
                       (< (sb-c::compiled-debug-fun-offset next) pc-offs))
              return (sb-c::compiled-debug-fun-name fun)))))

(defun aprof-collect (stream)
  (let* ((metadata *allocation-profile-metadata*)
         (n-hit (extern-alien "alloc_profile_n_counters" int))
         (metadata-len (/ (length metadata) 2))
         (n-counters (min metadata-len n-hit))
         (sap (extern-alien "alloc_profile_buffer" system-area-pointer))
         (index 3)
         (collection (make-hash-table :test 'equal)))
    (when stream
      (format stream "~&~d (of ~d max) profile entries consumed~2%"
              n-hit metadata-len))
    (loop
     (when (>= index n-counters)
       (return collection))
     (let ((count (sap-ref-word sap (* index 8))))
       (multiple-value-bind (code pc-offset total-bytes)
           (if (null (aref metadata (ash index 1))) ; sized alloc
               (values (aref metadata (+ (ash index 1) 2))
                       (aref metadata (+ (ash index 1) 3))
                       (sap-ref-word sap (* (1+ index) 8)))
               (values (aref metadata (+ (ash index 1) 0))
                       (aref metadata (+ (ash index 1) 1))))
         (incf index (if total-bytes 2 1)) ; <count,bytes> or just a count
         (unless (eql count 0)
           (with-pinned-objects (code)
            (let ((pc (+ (sb-kernel:get-lisp-obj-address code)
                          (- sb-vm:other-pointer-lowtag)
                          pc-offset))
                  (name (pc-offs-to-fun-name
                         ;; Relativize to CODE-INSTRUCTIONS, not the base address
                         (- pc-offset (ash (code-header-words code) sb-vm:word-shift))
                         code)))
              (multiple-value-bind (type size) (infer-type pc code)
                (cond (size ; fixed-size allocation
                       (aver (not total-bytes))
                       (push (make-alloc (* count size) count type pc)
                             (gethash name collection)))
                      (t ; variable-size
                       (aver total-bytes)
                       ;; The only allocator that determines TYPE at run-time
                       ;; is ALLOCATE-VECTOR-ON-HEAP. VAR-ALLOC uses a codegen
                       ;; arg, not a TN, for the widetag, but it looks like
                       ;; the type is dynamic because it kind of is: the
                       ;; header words is computed into a register and written
                       ;; in a single store.
                       (push (make-alloc total-bytes count type pc)
                             (gethash name collection)))))))))))))

(defun collapse-by-type (data &aux new)
  (dolist (datum data new)
    (let ((found (find (alloc-type datum) new :key #'alloc-type
                       :test #'equal)))
      (cond (found
             (incf (alloc-bytes found) (alloc-bytes datum))
             (incf (alloc-count found) (alloc-count datum)))
            (t
             (push datum new))))))

;; DETAIL NIL shows just function name and percent space consumption
;; DETAIL T shows function, bytes, percent,
;;    and unless there is only one detail line, the detail lines
;;
(defun aprof-show (&key (top-n 20) (detail t) (collapse t) (stream *standard-output*))
  (unless top-n
    (setq top-n 1000))
  (let* ((collection (%hash-table-alist (aprof-collect stream)))
         (summary
          (mapcar (lambda (x)
                    (list* (car x)
                           (reduce #'+ (cdr x) :key #'alloc-bytes)
                           (cdr x)))
                  collection))
         (sorted (sort (copy-list summary) #'> :key #'second))
         (total-bytes (reduce #'+ sorted :key #'second))
         (*print-pretty* nil)
         (i 0)
         (sum-pct 0)
         (sum-bytes 0))
    (when (eq stream nil)
      (setq stream (make-broadcast-stream))) ; lazy's person's approach
    (cond ((not detail)
           (format stream "~&       %    Sum %        Bytes    Allocations   Function~%")
           (format stream "~& -------  -------  -----------    -----------   --------~%"))
          (t
           (format stream "~&       %        Bytes        Count    ~:[~;    PC        ~]Function~%"
                   (not collapse))
           (format stream "~& -------  -----------    ---------    ~:[~;----------    ~]--------~%"
                   (not collapse))))

    ;; In detailed view, each function takes up either one line or
    ;; more than one line. In the interest of saving space, newlines are
    ;; omitted between consecutive functions each of whose detail consists
    ;; of a single line. But to avoid ambiguity, esure that there is always
    ;; a blank line before and after each function having multiline detail.
    (let ((emitted-newline t))
      (dolist (x sorted)
        (destructuring-bind (name bytes . data) x
          (when detail
            (when collapse
              (setq data (collapse-by-type data)))
            (setq data (sort data #'> :key #'alloc-bytes)))
          (assert (eq bytes (reduce #'+ data :key #'alloc-bytes)))
          (when (and detail (cdr data) (not emitted-newline))
            (terpri stream))
          (incf sum-pct (float (/ bytes total-bytes)))
          ;; Show summary for the function
          (cond ((not detail)
                 (format stream " ~5,1,2f      ~5,1,2f ~12d~15d   ~s~%"
                         (/ bytes total-bytes)
                         sum-pct
                         bytes
                         (reduce #'+ data :key #'alloc-count)
                         name))
                (t
                 (format stream " ~5,1,2f   ~12d   ~:[~10@t~;~:*~10d~]~@[~14@a~]    ~s~@[ - ~s~]~%"
                         (/ bytes total-bytes)
                         bytes
                         (if (cdr data) nil (alloc-count (car data)))
                         (cond (collapse nil)
                               ((cdr data) "")
                               (t (write-to-string (alloc-pc (car data)) :base 16)))
                         name
                         (if (cdr data) nil (alloc-type (car data)))
                         )))
          (when (and detail (cdr data))
            (dolist (point data)
              (format stream "     ~5,1,2f ~12d ~10d~@[~14x~]~@[        ~s~]~%"
                        (/ (alloc-bytes point) bytes) ; fraction within function
                        (alloc-bytes point)
                        (alloc-count point)
                        (if collapse nil (alloc-pc point))
                        (alloc-type point))))
          (incf sum-bytes bytes)
          (when (and detail
                     (setq emitted-newline (not (null (cdr data)))))
            (terpri stream)))
        (incf i)
        (if (and (neq top-n :all) (>= i top-n)) (return))))
;    (assert (= sum-bytes total-bytes))
    (cond ((not detail)
           (format stream "~19@t===========~%~19@t~11d~%" sum-bytes))
          (t
           (format stream " =======  ===========~%~6,1,2f   ~12d~%"
                   sum-pct sum-bytes)))
    sum-bytes))

;;; Call FUN and return the exact number of bytes it (an all descendant
;;; calls) allocated, provided that they were instrumented for precise
;;; cons profiling.
;;; STREAM is where to report, defaulting to *standard-output*.
;;; The convention is that of map-segment-instructions, meaning NIL is a sink.
(defun aprof-run (fun &key (stream *standard-output*))
  (aprof-reset)
  (patch-fixups)
  (let (nbytes)
    (unwind-protect
         (progn (aprof-start) (funcall fun))
      (aprof-stop)
      (setq nbytes (aprof-show :stream stream))
      (when stream (terpri stream)))
    nbytes))

;;;;

;;; Example:
;;;   The default output is a report showing the top 20 allocators
;;;   (by function name) with a line of detail for each distinct
;;;   type of object allocated within the function.
;;;
;;; * (aprof-run (lambda () (compile-file "~/aprof")))
;;;        %        Bytes        Count    Function
;;;  -------  -----------    ---------    --------
;;;    6.6        1980160                 INIT-SB-VECTORS
;;;       79.1      1565824      48880        SIMPLE-BIT-VECTOR
;;;       20.9       414336       1872        SIMPLE-VECTOR
;;;
;;;    4.6        1368960                 MAKE-TN
;;;       83.3      1140800       7130        TN
;;;       16.7       228160       7130        SIMPLE-BIT-VECTOR

;;; The report can be made more detailed by not combining lines
;;; for the same object type within a function:
;;;
;;; * (aprof-show :detail  t :collapse nil)
;;;
;;;        %        Bytes        Count        PC        Function
;;;  -------  -----------    ---------    ----------    --------
;;;    6.6        1980160                               INIT-SB-VECTORS
;;;       76.0      1504256      47008      21D07E88        SIMPLE-BIT-VECTOR
;;;       20.9       414336       1872      21D07D28        SIMPLE-VECTOR
;;;        3.1        61568       1872      21D07F30        SIMPLE-BIT-VECTOR
;;;
;;;    4.6        1368960                               MAKE-TN
;;;       83.3      1140800       7130      21C98638        TN
;;;       16.7       228160       7130      21C985D8        SIMPLE-BIT-VECTOR
;;;
;;;    4.0        1204160                               MAKE-HASH-TABLE
;;;       39.3       473472       1644      21B2A008        SIMPLE-VECTOR
;;;       37.1       447168       1644      21B29F50        (SIMPLE-ARRAY (UNSIGNED-BYTE 64) (*))
;;;       21.8       263040       1644      21B29FA8        (SIMPLE-ARRAY (UNSIGNED-BYTE 64) (*))
;;;        1.7        20480        128      21B2A098        (SIMPLE-ARRAY (UNSIGNED-BYTE 64) (*))

;;; Or less detailed by combining all lines within a function:
;;;
;;; * (sb-aprof::aprof-show :detail nil :top-n 1000) ; or anything
;;;        %    Sum %        Bytes    Allocations   Function
;;;  -------  -------  -----------    -----------   --------
;;;    6.6        6.6      1980160          50752   INIT-SB-VECTORS
;;;    4.6       11.1      1368960          14260   MAKE-TN
;;;    4.0       15.1      1204160           5060   MAKE-HASH-TABLE
;;;    3.8       19.0      1154384          28727   CONSTRAIN-REF-TYPE
;;;    3.8       22.8      1152160          13084   COPY-CONSET
;;;    3.6       26.5      1094240          13678   MAKE-TN-REF
;;; ... many more lines ...
;;;   00.0      100.0           16              1   %ENTER-NEW-NICKNAMES
;;;   00.0      100.0           16              1   UNIX-LSTAT
;;;                    ===========
;;;                       30054816
