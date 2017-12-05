;;;; Utilities for separating an SBCL core file into two pieces:
;;;; 1. An assembly language file containing the immobile code space
;;;; 2. A '.o' file wrapping a core file containing everything else
;;;; We operate as a "tool" that processes external files rather than
;;;; operating on the in-process data, but it is also possible to dump
;;;; the current image by creating a straight-through translation
;;;; of internal/external code addresses.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix)) ; for mmap

(defpackage "SB-EDITCORE"
  (:use "CL" "SB-VM" "SB-INT" "SB-EXT" "SB-KERNEL" "SB-SYS"
        "SB-ALIEN")
  (:import-from "SB-ALIEN-INTERNALS"
                #:alien-type-bits #:parse-alien-type
                #:alien-value-sap #:alien-value-type)
  (:import-from "SB-C" #:+backend-page-bytes+)
  (:import-from "SB-VM" #:map-objects-in-range #:reconstitute-object
                #:%closure-callee))

(in-package "SB-EDITCORE")

(declaim (muffle-conditions compiler-note))

(eval-when (:execute)
  (setq sb-ext:*evaluator-mode* :compile))

(defconstant core-magic
  (logior (ash (char-code #\S) 24)
          (ash (char-code #\B) 16)
          (ash (char-code #\C) 8)
          (char-code #\L)))

(defconstant build-id-core-entry-type-code 3860)
(defconstant new-directory-core-entry-type-code 3861)
(defconstant initial-fun-core-entry-type-code 3863)
(defconstant page-table-core-entry-type-code 3880)
(defconstant end-core-entry-type-code 3840)

(defconstant static-core-space-id 2)
(defconstant immobile-fixedobj-core-space-id 4)
(defconstant immobile-varyobj-core-space-id 5)

(defglobal +noexec-stack-note+ ".section .note.GNU-stack, \"\", @progbits")

;;; Given ADDR which is an address in the target core, return the address at which
;;; ADDR is currently mapped while performing the split.
(defun translate-ptr (addr spaces &aux (space (assoc addr (cdr spaces) :test #'>=)))
  (+ (sap-int (car spaces))
     (* (cadr space) +backend-page-bytes+)
     (- addr (car space))))

;;; Use extreme care: It works to use host accessors on the target core,
;;; but we must avoid type checks on instances because LAYOUTs need translation.
;;; Printing boxed objects from the target core will almost always crash.
(defun translate (obj spaces)
  (%make-lisp-obj (translate-ptr (get-lisp-obj-address obj) spaces)))

(defstruct (core-sym (:copier nil) (:predicate nil)
                     (:constructor make-core-sym (package name external)))
  (package nil)
  (name nil :read-only t)
  (external nil :read-only t))

(defun c-name (lispname pp-state)
  (when (and (symbolp lispname)
             (eq (symbol-package lispname) *cl-package*))
    (return-from c-name (concatenate 'string "cl:" (string-downcase lispname))))
  ;; Get rid of junk from LAMBDAs
  (setq lispname
        (named-let recurse ((x lispname))
                   (cond ((typep x '(cons (eql lambda)))
                          (let ((args (second x)))
                            `(lambda ,(if args sb-c::*debug-name-sharp* "()")
                               ,@(recurse (cddr x)))))
                         ((eq x :in) "in")
                         ((consp x)
                          (recons x (recurse (car x)) (recurse (cdr x))))
                         (t x))))

  ;; Shorten obnoxiously long printed representations of methods
  ;; by changing FAST-METHOD to METHOD (because who cares?)
  ;; and shorten
  ;;   (method my-long-package-name:my-method-name (my-long-package-name:type-name))
  ;; to
  ;;   (method my-method-name (type-name))
  ;; I suspect that can use DWARF info to provide even more description,
  ;; though I also suspect it's relatively unambiguous anyway
  ;; especially given that file information is available separately.
  (flet ((unpackageize (thing)
           (when (typep thing 'core-sym)
             (setf (core-sym-package thing) nil))
           thing))
    (when (typep lispname '(cons (eql sb-pcl::fast-method)))
      (setq lispname `(method ,@(cdr lispname)))
      (setf (second lispname) (unpackageize (second lispname)))
        (dolist (qual (car (last lispname)))
          (unpackageize qual))))

  ;; Perform backslash escaping on the exploded string
  ;; Strings were stringified without surrounding quotes,
  ;; but there might be quotes embedded anywhere, so escape them,
  ;; and also remove newlines and non-ASCII.
  (let ((characters
         (mapcan (lambda (char)
                   (cond ((not (base-char-p char)) (list #\?))
                         ((member char '(#\\ #\")) (list #\\ char))
                         ((eql char #\newline) (list #\_))
                         (t (list char))))
                 (coerce (if (and (stringp lispname)
                                     ;; L denotes a symbol which can not be global on macOS.
                                     (char= (char lispname 0) #\L))
                             (concatenate 'string "_" lispname)
                             (write-to-string lispname
                              :pretty t :pprint-dispatch (cdr pp-state)
                              ;; FIXME: should be :level 1, however see
                              ;; https://bugs.launchpad.net/sbcl/+bug/1733222
                              :escape t :level 2 :length 5
                              :case :downcase :gensym nil
                              :right-margin 10000))
                         'list))))
    (let* ((string (coerce characters 'string))
           (occurs (incf (gethash string (car pp-state) 0))))
      (if (> occurs 1)
          (concatenate 'string  string "_" (write-to-string occurs))
          string))))

(defmethod print-object ((sym core-sym) stream)
  (format stream "~(~:[~*~;~:*~A~:[:~;~]:~]~A~)"
          (core-sym-package sym)
          (core-sym-external sym)
          (core-sym-name sym)))

(defun fun-name-from-core (fun spaces core-nil packages
                               &aux (name (%simple-fun-name fun)))
  (named-let recurse ((depth 0) (x name))
    (unless (= (logand (get-lisp-obj-address x) 3) 3)
      (return-from recurse x)) ; immediate object
    (when (eq x core-nil)
      (return-from recurse nil))
    (setq x (translate x spaces))
    (ecase (lowtag-of x)
      (#.list-pointer-lowtag
       (cons (recurse (1+ depth) (car x))
             (recurse (1+ depth) (cdr x))))
      ((#.instance-pointer-lowtag #.fun-pointer-lowtag) "?")
      (#.other-pointer-lowtag
       (cond
        ((symbolp x)
         (let ((name (translate (symbol-name x) spaces)))
           (if (eq (symbol-package x) core-nil) ; uninterned
               (string-downcase name)
               (let* ((package (truly-the package
                                          (translate (symbol-package x) spaces)))
                      (package-name (translate (sb-impl::package-%name package)
                                               spaces))
                      (compute-externals
                       (not (or (string= package-name "KEYWORD")
                                (string= package-name "COMMON-LISP"))))
                      (externals (if compute-externals
                                     (gethash package-name packages)
                                     t)))
                 (unless externals
                   (dovector (x (translate
                                 (sb-impl::package-hashtable-cells
                                  (truly-the sb-impl::package-hashtable
                                   (translate (package-external-symbols package)
                                              spaces)))
                                 spaces))
                     (unless (fixnump x)
                       (push (if (eq x core-nil) ; random packages can export NIL. wow.
                                 "NIL"
                                 (translate (symbol-name (translate x spaces)) spaces))
                             externals)))
                   (setf externals (coerce externals 'vector)
                         (gethash package-name packages) externals))
                 ;; The name-cleaning code wants to compare against symbols
                 ;; in CL, PCL, and KEYWORD, so use real symbols for those.
                 ;; Other than that, we avoid finding host symbols
                 ;; because the externalness could be wrong and misleading.
                 ;; It's a very subtle point, but best to get it right.
                 (cond ((string= package-name "COMMON-LISP")
                        (find-symbol name *cl-package*))
                       ((and (string= package-name "KEYWORD")
                             (find-symbol name package-name))) ; if existing keyword, use it
                       ((string= package-name "SB-PCL")
                        (or (find-symbol name "SB-PCL")
                            (error "FIND-SYMBOL failed? ~S ~S" name package-name)))
                       (t
                        (make-core-sym (if (string= package-name "KEYWORD") nil package-name)
                                       name
                                       (if compute-externals
                                           (find name externals :test 'string=)
                                           t))))))))
        ((stringp x) x)
        (t "?"))))))

(defstruct (core-state
            (:conc-name "CS-")
            (:predicate nil)
            (:copier nil)
            (:constructor make-core-state
                (fixedobj-space-start fixedobj-space-end
                 &aux (inst-space (sb-disassem::get-inst-space))
                      (call-inst (sb-disassem::find-inst #b11101000 inst-space))
                      (jmp-inst (sb-disassem::find-inst #b11101001 inst-space))
                      (pop-inst (sb-disassem::find-inst #x5d inst-space)))))
  (fixedobj-space-start 0 :type fixnum :read-only t)
  (fixedobj-space-end 0 :type fixnum :read-only t)
  (dstate (sb-disassem::make-dstate nil) :read-only t)
  (seg (sb-disassem::%make-segment
        :sap-maker (lambda () (error "Bad sap maker"))
        :virtual-location 0) :read-only t)
  (call-inst nil :read-only t)
  (jmp-inst nil :read-only t)
  (pop-inst nil :read-only t))

;;; Emit .byte or .quad directives dumping memory from SAP for COUNT bytes
;;; to STREAM.  SIZE specifies which direcive to emit.
;;; EXCEPTIONS specify offsets at which a specific string should be
;;; written to the file in lieu of memory contents, useful for emitting
;;; expressions involving the assembler '.' symbol (the current PC).
(defun emit-asm-directives (size sap count stream &optional exceptions)
  (declare (optimize speed))
  (declare (stream stream))
  (let ((*print-base* 16)
        (string-buffer (make-array 18 :element-type 'base-char))
        (fmt #.(coerce "0x%lx" 'base-string))
        (per-line 0))
    (declare ((integer 0 32) per-line)
             (fixnum count))
    string-buffer fmt
    (ecase size
      (:qword
       (format stream " .quad")
       (dotimes (i count)
         (declare ((unsigned-byte 20) i))
         (declare (simple-vector exceptions))
         (write-char (if (> per-line 0) #\, #\space) stream)
         (acond ((and (< i (length exceptions)) (aref exceptions i))
                 (write-string it stream))
                (t
                 #+nil
                 (let ((len
                        ;; output-reasonable-integer-in-base is so slow comparated
                        ;; to printf() that the second-most amount of time spent
                        ;; writing the asm file occurs in that function.
                        ;; Unbelievable that we can't do better than that.
                        (with-pinned-objects (string-buffer fmt)
                          (alien-funcall
                           (extern-alien "snprintf"
                            (function int system-area-pointer unsigned system-area-pointer unsigned))
                           (vector-sap string-buffer)
                           (length string-buffer)
                           (vector-sap fmt)
                           (sap-ref-word sap (* i n-word-bytes))))))
                   (write-string string-buffer stream :end len))
                 (write-string "0x" stream)
                 (write (sap-ref-word sap (* i n-word-bytes)) :stream stream)))
         (when (and (= (incf per-line) 16) (< (1+ i) count))
           (format stream "~% .quad")
           (setq per-line 0))))
      (:byte
       (aver (not exceptions))
       (format stream " .byte")
       (dotimes (i count)
         (write-char (if (> per-line 0) #\, #\space) stream)
         (write-string "0x" stream)
         (write (sap-ref-8 sap i) :stream stream)
         (when (and (= (incf per-line) 32) (< (1+ i) count))
           (format stream "~% .byte")
           (setq per-line 0))))))
  (terpri stream))

(defun emit-lisp-asm-routines (spaces code-component output emit-sizes vector count)
  (emit-asm-directives :qword
                       (sap+ (code-instructions code-component)
                             (- (* sb-vm:code-constants-offset sb-vm:n-word-bytes)))
                       sb-vm:code-constants-offset
                       output #())
  (let ((list (loop for i from 2 by 2 repeat count
                    collect
                    (let* ((location (translate (svref vector (1+ i)) spaces))
                           (offset (car location))
                           (nbytes (- (1+ (cdr location)) offset))
                           (name (translate
                                  (symbol-name (translate (svref vector i) spaces))
                                  spaces)))
                      (list* offset name nbytes)))))
    (loop for (offset name . nbytes) in (sort list #'< :key #'car)
          do (format output " .set ~a, .~%~@[ .size ~:*~a, ~d~%~]"
                     (format nil "~(\"~a\"~)" name) (if emit-sizes nbytes))
             (emit-asm-directives
              :qword
              (sap+ (code-instructions code-component) offset)
              (ceiling nbytes sb-vm:n-word-bytes)
              output #()))))

;;; Disassemble the function pointed to by SAP for LENGTH bytes, returning
;;; all instructions that should be emitted using assembly language
;;; instead of assembler pseudo-ops. This includes two sets of instructions:
;;; - function prologue instructions that setup the call frame
;;; - jmp/call instructions that transfer control to the fixedoj space
;;;    delimited by bounds in STATE.
;;; At execution time the function will have virtual address LOAD-ADDR.
(defun list-annotated-instructions (sap length state load-addr emit-cfi)
  (let ((dstate (cs-dstate state))
        (seg (cs-seg state))
        (call-inst (cs-call-inst state))
        (jmp-inst (cs-jmp-inst state))
        (pop-inst (cs-pop-inst state))
        (list))
    (setf (sb-disassem::seg-virtual-location seg) load-addr
          (sb-disassem::seg-length seg) length
          (sb-disassem::seg-sap-maker seg) (lambda () sap))
    ;; KLUDGE: "8f 45 08" is the standard prologue
    (when (and emit-cfi (= (logand (sap-ref-32 sap 0) #xFFFFFF) #x08458f))
      (push (list* 0 3 "pop" "8(%rbp)") list))
    (sb-disassem::map-segment-instructions
     (lambda (dchunk inst)
       (cond
         ((or (eq inst jmp-inst) (eq inst call-inst))
          (let ((target-addr (+ (sb-x86-64-asm::near-jump-displacement dchunk dstate)
                                (sb-disassem::dstate-next-addr dstate))))
            (when (<= (cs-fixedobj-space-start state)
                      target-addr
                      (cs-fixedobj-space-end state))
              (push (list* (sb-disassem::dstate-cur-offs dstate)
                           5 ; length
                           (if (eq inst call-inst) "call" "jmp")
                           target-addr)
                    list))))
         ((and (eq inst pop-inst) (eq (logand dchunk #xFF) #x5D))
          (push (list* (sb-disassem::dstate-cur-offs dstate) 1 "pop" "%rbp") list))))
     seg
     dstate
     nil)
    (nreverse list)))

;;; Using assembler directives and/or real mnemonics, dump COUNT bytes
;;; of memory at PADDR (physical addr) to STREAM.
;;; The function's address as per the core file is VADDR.
;;; (Its eventual address is indeterminate)
;;; If EMIT-CFI is true, then also emit cfi directives.
;;;
;;; Notice that we can use one fewer cfi directive than usual because
;;; Lisp always carries a frame pointer as set up by the caller.
;;;
;;; C convention
;;; ============
;;; pushq %rbp
;;; .cfi_def_cfa_offset 16   # CFA offset from default register (rsp) is +16
;;; .cfi_offset 6, -16       # old rbp was saved in -16(CFA)
;;; movq %rsp, %rbp
;;; .cfi_def_cfa_register 6  # use rbp as CFA register
;;;
;;; Lisp convention
;;; ===============
;;; popq 8(%rbp) # place saved %rip in its ABI-compatible stack slot
;;;              # making RSP = RBP after the pop, and RBP = CFA - 16
;;; .cfi_def_cfa 6, 16
;;; .cfi_offset 6, -16
;;;
;;; Of course there is a flip-side to this: unwinders think that the new frame
;;; is already begun in the caller. Interruption between these two instructions:
;;;   MOV RBP, RSP / CALL #xzzzzz
;;; will show the backtrace as if two invocations of the caller are on stack.
;;; This is tricky to fix because while we can relativize the CFA to the
;;; known frame size, we can't do that based only on a disassembly.

(defun emit-lisp-function (paddr vaddr count stream emit-cfi core-state)
  (when emit-cfi
    (format stream " .cfi_startproc~%"))
  ;; Any byte offset that appears as a key in the INSTRUCTIONS causes the indicated
  ;; bytes to be written as an assembly language instruction rather than opaquely,
  ;; thereby affecting the ELF data (cfi or relocs) produced.
  (let ((instructions
         (list-annotated-instructions (int-sap paddr) count core-state vaddr emit-cfi))
        (ptr paddr))
    (symbol-macrolet ((cur-offset (- ptr paddr)))
      (loop
        (let ((until (if instructions (caar instructions) count)))
          ;; if we're not aligned, then write some number of bytes
          ;; to cause alignment. But do not write past the next offset
          ;; that needs to be written as an instruction.
          (when (logtest ptr #x7) ; unaligned
            (let ((n (min (- (nth-value 1 (ceiling ptr 8)))
                          (- until cur-offset))))
              (aver (<= 0 n 7))
              (emit-asm-directives :byte (int-sap ptr) n stream)
              (incf ptr n)))
          ;; Now we're either aligned to a multiple of 8, or the current
          ;; offset needs to be written as a textual instruction.
          (let ((n (- until cur-offset)))
            (aver (>= n 0))
            (multiple-value-bind (qwords remainder) (floor n 8)
              (when (plusp qwords)
                (emit-asm-directives :qword (int-sap ptr) qwords stream #())
                (incf ptr (* qwords 8)))
              (when (plusp remainder)
                (emit-asm-directives :byte (int-sap ptr) remainder stream)
                (incf ptr remainder))))
          ;; If the current offset is COUNT, we're done.
          (when (= cur-offset count) (return))
          (aver (= cur-offset until))
          (destructuring-bind (length opcode . operand) (cdr (pop instructions))
            (when (cond ((integerp operand) ; jmp or call
                         (format stream " ~A 0x~X~%" opcode operand))
                        ((string= opcode "pop")
                         (format stream " ~A ~A~%" opcode operand)
                         (cond ((string= operand "8(%rbp)")
                                (format stream " .cfi_def_cfa 6, 16~% .cfi_offset 6, -16~%"))
                               ((string= operand "%rbp")
                                ;(format stream " .cfi_def_cfa 7, 8~%")
                                nil)
                               (t)))
                        (t))
              (bug "Random annotated opcode ~S" opcode))
            (incf ptr length))
          (when (= cur-offset count) (return))))))
  (when emit-cfi
    (format stream " .cfi_endproc~%")))

;;; Convert immobile CODE-SPACE to an assembly file in OUTPUT.
;;; TODO: relocate fdefns and instances of standard-generic-function
;;; into the space that is dumped into an ELF section.
(defun write-assembler-text
    (code-space static-space fixedobj-space spaces output
     &optional emit-sizes (emit-cfi t)
     &aux (code-addr (car code-space)) ; target logical address, not in-memory address now
          (scan-limit (cdr code-space))
          (core-state (make-core-state (car fixedobj-space)
                                       (cdr fixedobj-space)))
          (total-code-size 0)
          (pp-state (cons (make-hash-table :test 'equal)
                          ;; copy no entries for macros/special-operators (flet, etc)
                          (sb-pretty::make-pprint-dispatch-table)))
          (packages (make-hash-table :test 'equal))
          (core-nil (sb-kernel:%make-lisp-obj (logior (car static-space) #x17)))
          (prev-namestring "")
          (n-linker-relocs 0)
          end-loc)
  (set-pprint-dispatch 'string
                       ;; Write strings without string quotes
                       (lambda (stream string) (write-string string stream))
                       0
                       (cdr pp-state))
  (labels ((ldsym-quote (name)
             (concatenate 'string '(#\") name '(#\")))
           (dumpwords (addr count stream &optional (exceptions #()) logical-addr)
             (let ((sap (int-sap addr)))
               (aver (sap>= sap (car spaces)))
               ;; Make intra-code-space pointers computed at link time
               (dotimes (i (if logical-addr count 0))
                 (unless (and (< i (length exceptions)) (svref exceptions i))
                   (let ((word (sap-ref-word sap (* i n-word-bytes))))
                     (when (and (= (logand word 3) 3) ; is a pointer
                                (<= (car code-space) word (1- (cdr code-space)))) ; to code space
                       #+nil
                       (format t "~&~(~x: ~x~)~%" (+ logical-addr  (* i n-word-bytes))
                               word)
                       (incf n-linker-relocs)
                       (setf exceptions (adjust-array exceptions (max (length exceptions) (1+ i))
                                                      :initial-element nil)
                             (svref exceptions i)
                             (format nil "__lisp_code_start+0x~x"
                                     (- word (car code-space))))))))
               (emit-asm-directives :qword sap count stream exceptions)))
           (make-code-obj (addr)
             (let ((translation (translate-ptr addr spaces)))
               (aver (= (%widetag-of (sap-ref-word (int-sap translation) 0))
                        code-header-widetag))
               (%make-lisp-obj (logior translation other-pointer-lowtag))))
           (calc-obj-size (code)
             ;; No need to pin - it's not managed by GC
             (nth-value 2
              (reconstitute-object
               (ash (logandc2 (get-lisp-obj-address code) lowtag-mask)
                    (- n-fixnum-tag-bits)))))
           (%widetag-of (word)
             (logand word widetag-mask)))
    (format output " .text~% .file \"sbcl.core\"
 .globl __lisp_code_start, __lisp_code_end~% .balign 4096~%__lisp_code_start:~%")

    ;; Scan the assembly routines.
    (if (typep sb-fasl:*assembler-routines* 'sb-kernel:code-component)
        (let* ((code-component (make-code-obj code-addr))
               (size (calc-obj-size code-component))
               (hashtable
                (truly-the hash-table
                 (translate (car (translate (sb-kernel:%code-debug-info code-component)
                                            spaces))
                            spaces)))
               (cells (translate (sb-impl::hash-table-table hashtable) spaces))
               (count (sb-impl::hash-table-number-entries hashtable)))
          (incf code-addr size)
          (setf total-code-size size)
          (emit-lisp-asm-routines spaces code-component output emit-sizes
                                  cells count))
          #+nil
          (collect ((code-components))
            (loop (when (>= code-addr scan-limit) (return))
                  (let ((code (make-code-obj code-addr)))
                    (when (plusp (code-n-entries code)) (return))
                    (let ((size (calc-obj-size code)))
                      (code-components (cons code-addr size))
                      (incf code-addr size))))
            (setf total-code-size (- code-addr (car code-space)))
            (emit-lisp-asm-routines spaces (code-components) output emit-sizes)))

    (loop
      (when (>= code-addr scan-limit) (return))
      (let* ((code (make-code-obj code-addr))
             (objsize (calc-obj-size code)))
        (setq end-loc (+ code-addr objsize))
        (incf total-code-size objsize)
        (cond
          ((< (code-header-words code) 4) ; filler object
           ;; Shouldn't occur unless defrag was not performed
           (format output "#x~x:~% .quad 0x~X, 0x~X~% .fill ~D~%"
                   code-addr
                   simple-array-unsigned-byte-8-widetag
                   (ash (- objsize (* 2 n-word-bytes))
                        n-fixnum-tag-bits)
                   (- objsize (* 2 n-word-bytes))))
          ((%instancep (%code-debug-info code)) ; assume it's a COMPILED-DEBUG-INFO
           (aver (plusp (code-n-entries code)))
           (let* ((source
                   (sb-c::compiled-debug-info-source
                    (truly-the sb-c::compiled-debug-info
                               (translate (%code-debug-info code) spaces))))
                  (namestring
                   (sb-c::debug-source-namestring
                    (truly-the sb-c::debug-source (translate source spaces)))))
             (setq namestring (if (eq namestring core-nil)
                                  "sbcl.core"
                                  (translate namestring spaces)))
             (unless (string= namestring prev-namestring)
               (format output " .file \"~a\"~%" namestring)
               (setq prev-namestring namestring)))
           (let* ((code-physaddr (logandc2 (get-lisp-obj-address code) lowtag-mask))
                  (boxed-end (+ code-physaddr
                                (ash (code-header-words code) word-shift)))
                  (first-fun (logandc2 (get-lisp-obj-address (%code-entry-point code 0))
                                       lowtag-mask)))
             (format output "#x~x:~%" code-addr)
             (dumpwords code-physaddr (code-header-words code) output #() code-addr)
             ;; Any words after 'boxed' preceding 'first-fun' are unboxed
             (when (> first-fun boxed-end)
               (dumpwords boxed-end (floor (- first-fun boxed-end) n-word-bytes)
                          output)))
           ;; Loop over all embedded functions.
           ;; Because simple-fun offsets are relative to the code start
           ;; (and not in a linked list as they were in the past),
           ;; iteratation in a "foreign" code object works just fine,
           ;; subject to the caution about reading boxed words.
           (dotimes (j (code-n-entries code))
             (let* ((fun (%code-entry-point code j))
                    (fun-addr (logandc2 (get-lisp-obj-address fun) lowtag-mask))
                    (end (if (< (1+ j) (code-n-entries code))
                             (logandc2 (get-lisp-obj-address (%code-entry-point code (1+ j)))
                                       lowtag-mask)
                             (+ (translate-ptr code-addr spaces) objsize)))
                    (entrypoint
                     (+ fun-addr (* simple-fun-code-offset n-word-bytes)))
                    (size (- end entrypoint))
                    (lispname (fun-name-from-core fun spaces core-nil packages))
                    (quotname (ldsym-quote (c-name lispname pp-state))))
               ;; Globalize the C symbol only if the name is a legal function designator
               ;; per the standard definition.
               ;; This is a technique to try to avoid appending a uniquifying suffix
               ;; on all the junky internal things like "(lambda # in srcfile.lisp)"
               (format output "~:[~*~; .globl ~a~%~]~@[ .type ~:*~a, @function~%~]"
                       (typep lispname '(or symbol core-sym (cons (eql setf))))
                       quotname emit-sizes)
               (dumpwords fun-addr
                          simple-fun-code-offset output
                          (load-time-value
                           `#(nil ,(format nil ".+~D"
                                           (* (1- simple-fun-code-offset)
                                              n-word-bytes)))
                           t)
                          nil)
               (format output " .set ~a, .~%~@[ .size ~:*~a, ~d~%~]"
                       quotname (if emit-sizes size))
               ;; entrypoint is the current physical address.
               ;; Also pass in the virtual address in the core
               ;; (which will differ from the actual load-time address)
               (emit-lisp-function entrypoint
                                   (+ code-addr (- entrypoint
                                                   (logandc2 (get-lisp-obj-address code)
                                                             lowtag-mask)))
                                   size output emit-cfi core-state)))
           (terpri output))
          (t
           (error "Strange code component: ~S" code)))
        (incf code-addr objsize))))

  ;; coreparse uses unpadded __lisp_code_end to set varyobj_free_pointer
  (format output "~:[~; .size __lisp_code_start, 0x~x~%~]__lisp_code_end:~%"
          emit-sizes total-code-size)

  ;; Pad so that non-lisp code can't be colocated on a GC page.
  ;; (Lack of Lisp object headers in C code is the issuee)
  (let ((aligned-end (logandc2 (+ end-loc 4095) 4095)))
    (when (> aligned-end end-loc)
      (multiple-value-bind (nwords remainder)
          (floor (- aligned-end end-loc) n-word-bytes)
        (aver (>= nwords 2))
        (aver (zerop remainder))
        (decf nwords 2)
        (format output " .quad ~d, ~d # (simple-array fixnum (~d))~%"
                simple-array-fixnum-widetag
                (ash nwords n-fixnum-tag-bits)
                nwords)
        (when (plusp nwords)
          (format output " .fill ~d~%" (* nwords n-word-bytes))))))
  ; (format t "~&linker-relocs=~D~%" n-linker-relocs)
  (values total-code-size n-linker-relocs))

(defun extract-required-c-symbols (fixedobj-space spaces asm-file &optional (verbose nil))
  (flet ((remote-find-symbol (package-name symbol-name)
           (let ((physaddr (translate-ptr (car fixedobj-space) spaces))
                 ;; Make sure we remain in-bounds for the fixedobj space when translating.
                 (limit (1+ (translate-ptr (1- (cdr fixedobj-space)) spaces))))
             (loop
               (when (>= physaddr limit) (bug "Can't find symbol"))
               (multiple-value-bind (obj tag size)
                   (reconstitute-object (ash physaddr (- n-fixnum-tag-bits)))
                 (when (and (= tag symbol-widetag)
                            (string= symbol-name (translate (symbol-name obj) spaces))
                            (%instancep (symbol-package obj))
                            (string= package-name
                                     (translate
                                      (sb-impl::package-%name
                                       (truly-the package (translate (symbol-package obj) spaces)))
                                      spaces)))
                   (return (%make-lisp-obj (logior physaddr other-pointer-lowtag))))
                 (incf physaddr size)))))
         (symbol-fdefn-fun (symbol)
           (let ((vector (translate (symbol-info-vector symbol) spaces)))
             ;; TODO: allow for (plist . info-vector) in the info slot
             (aver (simple-vector-p vector))
             (translate (fdefn-fun (translate (info-vector-fdefn vector) spaces))
                        spaces))))
    (let ((linkage-info
           (translate (symbol-global-value (remote-find-symbol "SB-SYS" "*LINKAGE-INFO*"))
                      spaces))
          (dyn-syminfo
           (symbol-fdefn-fun
            (remote-find-symbol "SB-SYS" "ENSURE-DYNAMIC-FOREIGN-SYMBOL-ADDRESS"))))
      (aver (= (get-closure-length dyn-syminfo) 3))
      (let* ((ht1 (translate (%closure-index-ref dyn-syminfo 1) spaces))
             (ht2 (translate (%closure-index-ref dyn-syminfo 0) spaces))
             (table0
              (translate (sb-impl::hash-table-table (truly-the hash-table linkage-info))
                         spaces))
             (table1
              (translate (sb-impl::hash-table-table (truly-the hash-table ht1)) spaces))
             (table2
              (translate (sb-impl::hash-table-table (truly-the hash-table ht2)) spaces))
             (linkage)
             (foreign))
        (declare (simple-vector table0 table1 table2))
        (flet ((show (x)
                 (push x foreign)
                 (when verbose
                   (format t "~A~%" x)))
               (scan-table (table name fun &aux (n 0) (end (length table)))
                 (when verbose
                   (format t "~&~A:~%~A~%"
                           name (make-string (1+ (length name)) :initial-element #\-)))
                 (do ((i 2 (+ i 2)))
                     ((= i end))
                   (let ((val (svref table i)))
                     (unless (unbound-marker-p val)
                       (funcall fun (translate val spaces))
                       (incf n))))
                 (when verbose
                   (format t "TOTAL: ~D entries~2%" n))))
          (scan-table table0 "linkage info"
                      (lambda (x &aux (type #\T))
                        (when (consp x)
                          (setq x (translate (car x) spaces) type #\D))
                        (format asm-file " .long ~A~%" x)
                        (when verbose
                          (format t "~A ~A~%" type x))
                        (push x linkage)))
          (scan-table table1 "defined" #'show)
          (scan-table table2 "undefined" #'show)
          (let ((diff1 ; linkage not in foreign
                 (remove-if (lambda (x) (member x foreign :test #'string=)) linkage))
                (diff2 ; foreign not in linkage
                 (remove-if (lambda (x) (member x linkage :test #'string=)) foreign)))
            (when verbose
              (format t "~&Linkage not in foreign:~%~S~%" diff1)
              (format t "~&Foreign not in linkage:~%~S~%" diff2))
            )))))
  (terpri asm-file))

;;;; ELF file I/O

(defconstant +sht-null+     0)
(defconstant +sht-progbits+ 1)
(defconstant +sht-symtab+   2)
(defconstant +sht-strtab+   3)
(defconstant +sht-rela+     4)
(defconstant +sht-rel+      9)

(define-alien-type elf64-ehdr
  (struct elf64-edhr
    (ident     (array unsigned-char 16)) ; 7F 45 4C 46 2 1 1 0 0 0 0 0 0 0 0 0
    (type      (unsigned 16))   ; 1 0
    (machine   (unsigned 16))   ; 3E 0
    (version   (unsigned 32))   ; 1 0 0 0
    (entry     unsigned)        ; 0 0 0 0 0 0 0 0
    (phoff     unsigned)        ; 0 0 0 0 0 0 0 0
    (shoff     unsigned)        ;
    (flags     (unsigned 32))   ; 0 0 0 0
    (ehsize    (unsigned 16))   ; 40 0
    (phentsize (unsigned 16))   ;  0 0
    (phnum     (unsigned 16))   ;  0 0
    (shentsize (unsigned 16))   ; 40 0
    (shnum     (unsigned 16))   ;  n 0
    (shstrndx  (unsigned 16)))) ;  n 0
(define-alien-type elf64-shdr
  (struct elf64-shdr
    (name      (unsigned 32))
    (type      (unsigned 32))
    (flags     (unsigned 64))
    (addr      (unsigned 64))
    (off       (unsigned 64))
    (size      (unsigned 64))
    (link      (unsigned 32))
    (info      (unsigned 32))
    (addralign (unsigned 64))
    (entsize   (unsigned 64))))
(define-alien-type elf64-sym
  (struct elf64-sym
    (name  (unsigned 32))
    (info  (unsigned 8))
    (other (unsigned 8))
    (shndx (unsigned 16))
    (value unsigned)
    (size  unsigned)))
(define-alien-type elf64-rela
  (struct elf64-rela
    (offset  (unsigned 64))
    (info    (unsigned 64))
    (addendr (signed 8))))

(defun make-elf64-sym (name info)
  (let ((a (make-array 24 :element-type '(unsigned-byte 8))))
    (with-pinned-objects (a)
      (setf (sap-ref-32 (vector-sap a) 0) name
            (sap-ref-8 (vector-sap a) 4) info))
    a))

;;; Return two values: an octet vector comprising a string table
;;; and an alist which maps string to offset in the table.
(defun string-table (strings)
  (let* ((length (+ (1+ (length strings)) ; one more null than there are strings
                    (reduce #'+ strings :key #'length))) ; data length
         (bytes (make-array length :element-type '(unsigned-byte 8)
                            :initial-element 0))
         (index 1)
         (alist))
    (dolist (string strings)
      (push (cons string index) alist)
      (replace bytes (map 'vector #'char-code string) :start1 index)
      (incf index (1+ (length string))))
    (values bytes (nreverse alist))))

(defun write-alien (alien size stream)
  (dotimes (i size)
    (write-byte (sap-ref-8 (alien-value-sap alien) i) stream)))

;;; Make a relocatable ELF file from an SBCL core file
(defun objcopy (input-path output-path relocs)
  (with-open-file (input input-path :element-type '(unsigned-byte 8))
    (binding* ((sym-entry-size   24)
               (reloc-entry-size 24)
               (sections
                `#((:core "lisp.core"       ,+sht-progbits+ 0 0 0 4096  0)
                   (:sym  ".symtab"         ,+sht-symtab+   0 3 1    8 ,sym-entry-size)
                                ; section with the strings -- ^ ^ -- 1+ highest local symbol
                   (:str  ".strtab"         ,+sht-strtab+   0 0 0    1  0)
                   (:rel  ".relalisp.core"  ,+sht-rela+     0 2 1    8 ,reloc-entry-size)
                                            ; symbol table -- ^ ^ -- for which section
                   (:note ".note.GNU-stack" ,+sht-null+     0 0 0    1  0)))
               ((string-table map)
                (string-table (append '("__lisp_code_start")
                                      (map 'list #'second sections))))
               (padded-strings-size (logandc2 (+ (length string-table) 7) 7))
               (ehdr-size #.(ceiling (alien-type-bits (parse-alien-type 'elf64-ehdr nil)) 8))
               (shdr-size #.(ceiling (alien-type-bits (parse-alien-type 'elf64-shdr nil)) 8))
               (symbols-size (* 2 sym-entry-size))
               (shdrs-start (+ ehdr-size symbols-size padded-strings-size))
               (shdrs-end (+ shdrs-start (* (1+ (length sections)) shdr-size)))
               (core-size (file-length input))
               (core-align 4096)
               (core-start (logandc2 (+ shdrs-end (1- core-align)) (1- core-align)))
               (ident #.(coerce '(#x7F #x45 #x4C #x46 2 1 1 0 0 0 0 0 0 0 0 0)
                                '(array (unsigned-byte 8) 1))))
      reloc-entry-size
      (with-open-file (output output-path :direction :output :if-exists :supersede
                              :element-type '(unsigned-byte 8))
        (with-alien ((ehdr elf64-ehdr))
          (dotimes (i (ceiling ehdr-size n-word-bytes))
            (setf (sap-ref-word (alien-value-sap ehdr) (* i n-word-bytes)) 0))
          (with-pinned-objects (ident)
            (%byte-blt (vector-sap ident) 0 (alien-value-sap ehdr) 0 16))
          (setf (slot ehdr 'type)      1
                (slot ehdr 'machine)   #x3E
                (slot ehdr 'version)   1
                (slot ehdr 'shoff)     shdrs-start
                (slot ehdr 'ehsize)    ehdr-size
                (slot ehdr 'shentsize) shdr-size
                (slot ehdr 'shnum)     (1+ (length sections)) ; section 0 is implied
                (slot ehdr 'shstrndx)  (1+ (position :str sections :key #'car)))
          (write-alien ehdr ehdr-size output))

        ;; Write symbol table
        (aver (eql (file-position output) ehdr-size))
        (write-sequence (make-elf64-sym 0 0) output)
        ;; The symbol name index is always 1 by construction. The type is #x10
        ;; given: #define STB_GLOBAL 1
        ;;   and: #define ELF32_ST_BIND(val) ((unsigned char) (val)) >> 4)
        ;; which places the binding in the high 4 bits of the low byte.
        (write-sequence (make-elf64-sym 1 #x10) output)

        ;; Write string table
        (aver (eql (file-position output) (+ ehdr-size symbols-size)))
        (write-sequence string-table output)
        (dotimes (i (- padded-strings-size (length string-table)))
          (write-byte 0 output))

        ;; Write section headers
        (aver (eql (file-position output) shdrs-start))
        (with-alien ((shdr elf64-shdr))
          (dotimes (i (ceiling shdr-size n-word-bytes)) ; Zero-fill
            (setf (sap-ref-word (alien-value-sap shdr) (* i n-word-bytes)) 0))
          (dotimes (i (1+ (length sections)))
            (when (plusp i) ; Write the zero-filled header as section 0
              (destructuring-bind (key name type flags link info alignment entsize)
                  (aref sections (1- i))
                (multiple-value-bind (offset size)
                    (ecase key
                      (:sym  (values ehdr-size symbols-size))
                      (:str  (values (+ ehdr-size symbols-size) (length string-table)))
                      (:core (values core-start core-size))
                      (:rel  (values (+ core-start core-size)
                                     (* (length relocs) reloc-entry-size)))
                      (:note (values 0 0)))
                  (setf (slot shdr 'name)  (cdr (assoc name map :test #'string=))
                        (slot shdr 'type)  type
                        (slot shdr 'flags) flags
                        (slot shdr 'off)   offset
                        (slot shdr 'size)  size
                        (slot shdr 'link)  link
                        (slot shdr 'info)  info
                        (slot shdr 'addralign) alignment
                        (slot shdr 'entsize) entsize))))
            (write-alien shdr shdr-size output)))
        ;; Write padding
        (dotimes (i (- core-start (file-position output)))
          (write-byte 0 output))
        (aver (eq (file-position output) core-start))
        ;; Copy the core file
        (let ((buffer (make-array 65536 :element-type '(unsigned-byte 8))))
          (loop (let ((n (read-sequence buffer input)))
                  (when (zerop n) (return))
                  (write-sequence buffer output :end n))))))))

;;; Return a list of fixups (FIXUP-WHERE KIND ADDEND) to peform in a foreign core
;;; whose code space is subject to link-time relocation.
;;;   #define R_X86_64_64         1  /* Direct 64 bit  */
;;;   #define R_X86_64_PC32       2  /* PC relative 32 bit signed */
;;;
(defun collect-relocations (spaces)
  (format t "mapped @ ~s~%~x~%" (car spaces) (cdr spaces))
  (let* ((code-space (find immobile-varyobj-core-space-id (cdr spaces) :key #'fourth))
         (code-start (car code-space))
         (code-end (+ code-start (ash (third code-space) word-shift) -1))
         (fixups)
         (bias 0))
    (declare (ignore bias))
    (format t "~&code space is ~X:~X~%" code-start code-end)
    (labels
        ((abs-fixup (where referent)
           (format t "0x~(~x~): (a)~%" (core-to-logical where) referent)
           (push `(,where ,(- referent code-start) . :abs) fixups))
         (rel-fixup (where referent)
           (format t "0x~(~x~): (r)~%" (core-to-logical where) referent)
           (push `(,where ,(- referent code-start) . :rel) fixups))
         ;; Given a address which is an offset into the data pages of the target core,
         ;; compute the logical address which that offset would be mapped to.
         ;; For example core address 0 is the virtual address of static space.
         (core-to-logical (core-offs &aux (page (floor core-offs +backend-page-bytes+)))
;          (format t "core-offs ~x, page=~d~%" addr page)
           (loop for (vaddr page0 nwords id) in (cdr spaces)
                 for npages = (ceiling nwords (/ +backend-page-bytes+ n-word-bytes))
                 when (and (<= page0 page (+ page0 (1- npages)))
                           (/= id immobile-varyobj-core-space-id))
                 do (return (+ vaddr
                               (* (- page page0) +backend-page-bytes+)
                               (logand core-offs (1- +backend-page-bytes+))))
                 finally (bug "Can't translate core offset ~x using ~x"
                              core-offs spaces)))
         (scanptrs (obj wordindex-min wordindex-max)
           (do* ((base-addr (logandc2 (get-lisp-obj-address obj) lowtag-mask))
                 (sap (int-sap base-addr))
                 ;; core-offs is the offset in the lisp.core ELF section.
                 (core-offs (- base-addr (sap-int (car spaces))))
                 (i wordindex-min (1+ i)))
                ((> i wordindex-max))
             (let ((ptr (sap-ref-word sap (ash i word-shift))))
               (when (and (= (logand ptr 3) 3) (<= code-start ptr code-end))
                 (abs-fixup (+ core-offs (ash i word-shift)) ptr)))))
         (scanptr (obj wordindex)
           (scanptrs obj wordindex wordindex)) ; trivial wrapper
         (scan-obj (obj widetag vaddr size
                    &aux (core-offs (- (logandc2 (get-lisp-obj-address obj) lowtag-mask)
                                       (sap-int (car spaces))))
                         (nwords (ceiling size n-word-bytes)))
;           (format t "~&obj @ physical ~x core addr ~x widetag ~x~%" (get-lisp-obj-address obj) core-offs widetag)
           (when (listp obj)
             (scanptrs obj 0 1)
             (return-from scan-obj))
           (case widetag
             (#.instance-widetag
              (let ((layout (truly-the layout
                             (translate (%instance-layout obj) spaces))))
                ;; FIXME: even though the layout is supplied, it's not good enough,
                ;; because the macro references the layout-bitmap which might
                ;; be a bignum which is a pointer into the logical core address.
                (unless (fixnump (layout-bitmap layout))
                  (error "Can't process bignum bitmap"))
                (do-instance-tagged-slot (i obj :layout layout)
                  (scanptr obj (1+ i))))
              (return-from scan-obj))
             (#.simple-vector-widetag
              (let ((len (length (the simple-vector obj))))
                (when (eql (logand (get-header-data obj) #xFF) vector-valid-hashing-subtype)
                  (do ((i 2 (+ i 2)) (needs-rehash))
                      ((= i len)
                       (when needs-rehash
                         (setf (svref obj 1) 1)))
                    (when (scanptr obj (+ vector-data-offset i))
                      (format t "~&SET REHASH: vector=~X~%" (get-lisp-obj-address obj))
                      (setq needs-rehash t))
                    (scanptr obj (+ vector-data-offset i 1)))
                  (return-from scan-obj))
                (setq nwords (+ len 2))))
             (#.fdefn-widetag
              (scanptrs obj 1 2)
              (let* ((fdefn-pc-sap ; where to read to access the rel32 operand
                      (int-sap (+ (- (get-lisp-obj-address obj) other-pointer-lowtag)
                                  (ash fdefn-raw-addr-slot word-shift))))
                     ;; what the fdefn's logical PC will be
                     (fdefn-logical-pc (+ vaddr (ash fdefn-raw-addr-slot word-shift)))
                     (rel32off (signed-sap-ref-32 fdefn-pc-sap 1))
                     (target (+ fdefn-logical-pc 5 rel32off)))
                ;; 5 = length of jmp/call inst
                (when (<= code-start target code-end)
                  ;; This addend needs to account for the fact that the location
                  ;; where fixup occurs is not where the fdefn will actually exist.
                  (rel-fixup (+ core-offs (ash 3 word-shift) 1) target)))
              (return-from scan-obj))
             ((#.closure-widetag #.funcallable-instance-widetag)
              (let ((word (sap-ref-word (int-sap (get-lisp-obj-address obj))
                                        (- n-word-bytes fun-pointer-lowtag))))
                (when (<= code-start word code-end)
                  (abs-fixup (+ core-offs (ash 1 word-shift)) word)))
              (when (eq widetag funcallable-instance-widetag)
                (let ((layout (truly-the layout
                               (translate (%funcallable-instance-layout obj) spaces))))
                  (unless (fixnump (layout-bitmap layout))
                    (error "Can't process bignum bitmap"))
                  (let ((bitmap (layout-bitmap layout)))
                    (unless (eql bitmap -1)
                      ;; tagged slots precede untagged slots,
                      ;; so integer-length is the count of tagged slots.
                      (setq nwords (1+ (integer-length bitmap))))))))
             ;; mixed boxed/unboxed objects
             (#.code-header-widetag
              (dotimes (i (code-n-entries obj))
                (scanptrs (%code-entry-point obj i) 2 5))
              (setq nwords (code-header-words obj)))
             ;; boxed objects that can reference code/simple-funs
             ((#.value-cell-widetag #.symbol-widetag #.weak-pointer-widetag))
             (t
              (return-from scan-obj)))
           (scanptrs obj 1 (1- nwords))))
      (dolist (space (reverse (cdr spaces)))
        (destructuring-bind (logical-addr data-page nwords id) space
          (unless (= id immobile-varyobj-core-space-id)
            (let* ((size (ash nwords word-shift))
                   (physical-start (sap+ (car spaces) (* data-page +backend-page-bytes+)))
                   (physical-end (sap+ physical-start size)))
              (format t "~&scan range vaddr=~12x:~12x paddr=~12x:~12x~%"
                      logical-addr (+ logical-addr size)
                      (sap-int physical-start) (sap-int physical-end))
              (dx-flet ((visit (obj widetag size)
                          ;; Compute the object's intended virtual address
                          (let ((vaddr (+ (- (logandc2 (get-lisp-obj-address obj) lowtag-mask)
                                             (sap-int physical-start))
                                          logical-addr)))
                            (scan-obj obj widetag vaddr size))))
                       (map-objects-in-range
                        #'visit
                        (ash (sap-int physical-start) (- n-fixnum-tag-bits))
                        (ash (sap-int physical-end) (- n-fixnum-tag-bits)))))))))
    (format t "~D fixups~%" (length fixups))
    (bug "not done yet")))
;;;;

(macrolet ((do-core-header-entry ((id-var len-var ptr-var) &body body)
             `(let ((,ptr-var 1))
                (loop
                  (let ((,id-var (%vector-raw-bits buffer ,ptr-var))
                        (,len-var (%vector-raw-bits buffer (1+ ,ptr-var))))
                    (incf ,ptr-var 2)
                    (decf ,len-var 2)
                    (when (= ,id-var end-core-entry-type-code) (return))
                    ,@body
                    (incf ,ptr-var ,len-var)))))
           (do-directory-entry ((index-var start-index input-nbytes) &body body)
             `(let ((words-per-dirent 5))
                (multiple-value-bind (n-entries remainder)
                    (floor ,input-nbytes words-per-dirent)
                  (aver (zerop remainder))
                  (symbol-macrolet ((id        (%vector-raw-bits buffer index))
                                    (nwords    (%vector-raw-bits buffer (+ index 1)))
                                    (data-page (%vector-raw-bits buffer (+ index 2)))
                                    (addr      (%vector-raw-bits buffer (+ index 3)))
                                    (npages    (%vector-raw-bits buffer (+ index 4))))
                    (do ((,index-var ,start-index (+ ,index-var words-per-dirent)))
                        ((= ,index-var (+ ,start-index (* n-entries words-per-dirent))))
                      ,@body)))))
           (with-mapped-core ((sap-var start npages stream) &body body)
             `(let (,sap-var)
                (unwind-protect
                     (progn
                       (setq ,sap-var
                             (sb-posix:mmap nil
                                            (* ,npages +backend-page-bytes+)
                                            (logior sb-posix:prot-read sb-posix:prot-write)
                                            sb-posix:map-private
                                            (sb-sys:fd-stream-fd ,stream)
                                            ;; Skip the core header
                                            (+ ,start +backend-page-bytes+)))
                       ,@body)
                  (when ,sap-var
                    (sb-posix:munmap ,sap-var (* ,npages +backend-page-bytes+)))))))

(defun split-core
    (input-pathname asm-pathname
     &key emit-sizes (verbose t)
     &aux (split-core-pathname
           (merge-pathnames
            (make-pathname :name (concatenate 'string (pathname-name asm-pathname) "-core")
                           :type "tmp")
            asm-pathname))
          (elf-core-pathname (merge-pathnames (make-pathname :type "o") split-core-pathname))
          (buffer (make-array +backend-page-bytes+ :element-type '(unsigned-byte 8)))
          (original-total-npages 0)
          (core-offset 0)
          (static-space)
          (code-space)
          (fixedobj-space)
          (copy-actions)
          (page-adjust 0)
          (spaces))

  ;; Remove old files
  (ignore-errors (delete-file asm-pathname))
  (ignore-errors (delete-file elf-core-pathname))
  ;; Ensure that all files can be opened
  (with-open-file (input input-pathname :element-type '(unsigned-byte 8))
    (with-open-file (asm-file asm-pathname :direction :output :if-exists :supersede)
      (with-open-file (temp-core split-core-pathname :direction :output
                                 :element-type '(unsigned-byte 8) :if-exists :supersede)
        (read-sequence buffer input)
        (cond ((= (%vector-raw-bits buffer 0) core-magic))
              (t ; possible embedded core
               (file-position input (- (file-length input)
                                       (* 2 n-word-bytes)))
               (aver (eql (read-sequence buffer input) (* 2 n-word-bytes)))
               (aver (= (%vector-raw-bits buffer 1) core-magic))
               (setq core-offset (%vector-raw-bits buffer 0))
               (when verbose
                 (format t "~&embedded core starts at #x~x into input~%" core-offset))
               (file-position input core-offset)
               (read-sequence buffer input)
               (aver (= (%vector-raw-bits buffer 0) core-magic))))
        (do-core-header-entry (id len ptr)
          (case id
            (#.build-id-core-entry-type-code
             (let ((string (make-string (%vector-raw-bits buffer ptr)
                                        :element-type 'base-char))
                   (string-base (* (1+ ptr) n-word-bytes)))
               (loop for i below (length string)
                     do (setf (char string i) (code-char (aref buffer (+ string-base i)))))
               (when verbose
                 (format t "Build ID [~a]~%" string))))
            (#.new-directory-core-entry-type-code
             (do-directory-entry (index ptr len)
               (incf original-total-npages npages)
               (push (list addr data-page nwords id) spaces)
               (when verbose
                 (format t "id=~d page=~5x + ~5x addr=~10x words=~8x~:[~; (drop)~]~%"
                         id data-page npages addr nwords
                         (= id immobile-varyobj-core-space-id)))
               (cond ((= id immobile-varyobj-core-space-id)
                      ;; subsequent entries adjust their start page downward
                      ;; (should be just the dynamic space entry)
                      (setq code-space (cons addr (+ addr (ash nwords word-shift)))
                            page-adjust npages)
                      ;; Keep the entry but delete the page count. We need to know
                      ;; where the space was supposed to be mapped and at what size.
                      (setf data-page 0 npages 0))
                     (t
                      ;; Keep track of where the static and fixedobj spaces
                      ;; were supposed to be mapped.
                      (case id
                        (#.static-core-space-id
                         (setq static-space
                               (cons addr (+ addr (ash nwords word-shift)))))
                        (#.immobile-fixedobj-core-space-id
                         (setq fixedobj-space
                               (cons addr (+ addr (ash nwords word-shift))))))
                      (when (plusp npages) ; enqueue
                        (push (cons data-page npages) copy-actions))
                      ;; adjust this entry's start page in the new core
                      (decf data-page page-adjust)))))
            (#.page-table-core-entry-type-code
             (aver (= len 3))
             (let ((bytes (%vector-raw-bits buffer (1+ ptr)))
                   (data-page (%vector-raw-bits buffer (+ ptr 2))))
               (aver (= data-page original-total-npages))
               (when verbose
                 (format t "PTE: page=~5x~40tbytes=~8x~%" data-page bytes))
               (multiple-value-bind (npages remainder)
                   (floor bytes +backend-page-bytes+)
                 (aver (zerop remainder))
                 (push (cons data-page npages) copy-actions)
                 (decf (%vector-raw-bits buffer (1+ ptr)) page-adjust))))))
        (write-sequence buffer temp-core)
        (dolist (action (nreverse copy-actions))
          ;; page index convention assumes absence of core header.
          ;; i.e. data page 0 is the file page immediately following the core header
          (let ((offset (* (1+ (car action)) +backend-page-bytes+))
                (npages (cdr action)))
            (when verbose
              (format t "File offset ~10x: ~4x page~:P~%" offset npages))
            (file-position input (+ core-offset offset))
            (dotimes (page npages)
              (read-sequence buffer input)
              (write-sequence buffer temp-core))))
        ;; Trailer (runtime options and magic number)
        (let ((nbytes (read-sequence buffer input)))
          ;; expect trailing magic number
          (let ((ptr (floor (- nbytes n-word-bytes) n-word-bytes)))
            (aver (= (%vector-raw-bits buffer ptr) core-magic)))
          ;; File position of the core header needs to be set to 0
          ;; regardless of what it was
          (setf (%vector-raw-bits buffer 4) 0)
          (when verbose
            (format t "Trailer words:(~{~X~^ ~})~%"
                    (loop for i below (floor nbytes n-word-bytes)
                          collect (%vector-raw-bits buffer i))))
          (write-sequence buffer temp-core :end nbytes)
          (finish-output temp-core))
        ;; Sanity test
        (aver (= (+ core-offset
                    (* page-adjust +backend-page-bytes+)
                    (file-length temp-core))
                 (file-length input))))
      (setq buffer nil) ; done with buffer now

      ;; Map the original core file to memory
      (with-mapped-core (sap core-offset original-total-npages input)
        (let* ((map (cons sap (sort spaces #'> :key 'car)))
               (relocs #+nil(collect-relocations map)))
          ;; Convert the partial core into a '.o' file
          (objcopy split-core-pathname elf-core-pathname relocs)
          (delete-file split-core-pathname)

          ;; There's no relation between emit-sizes and which section to put
          ;; C symbol references in, however it's a safe bet that if sizes
          ;; are supported then so is the .rodata directive.
          (format asm-file (if emit-sizes " .rodata~%" " .data~%"))
          (extract-required-c-symbols fixedobj-space map asm-file)
          (write-assembler-text code-space static-space fixedobj-space
                                map asm-file emit-sizes)))

      (format asm-file "~% ~A~%" +noexec-stack-note+))))

) ; end MACROLET

;;;;

(defun cl-user::elfinate (&optional (args (cdr sb-ext:*posix-argv*)))
  (cond ((string= (car args) "split")
         (pop args)
         (let ((sizes (string= (car args) "--sizes")))
           (when sizes
             (pop args))
           (destructuring-bind (input asm) args
             (split-core input asm :emit-sizes sizes))))
        #+nil
        ((string= (car args) "relocate")
         (destructuring-bind (input output binary start-sym) (cdr args)
           (relocate-core
            input output binary (parse-integer start-sym :radix 16))))
        (t
         (error "Unknown command: ~S" args))))

;; If loaded as a script, do this
(eval-when (:execute)
  (let ((args (cdr sb-ext:*posix-argv*)))
    (when args
      (let ((*print-pretty* nil))
        (format t "Args: ~S~%" args)
        (cl-user::elfinate args)))))
