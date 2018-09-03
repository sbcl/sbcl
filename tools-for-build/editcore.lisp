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

(load (merge-pathnames "corefile.lisp" *load-pathname*))

(defpackage "SB-EDITCORE"
  (:use "CL" "SB-ALIEN" "SB-COREFILE" "SB-INT" "SB-EXT"
        "SB-KERNEL" "SB-SYS" "SB-VM")
  (:import-from "SB-ALIEN-INTERNALS"
                #:alien-type-bits #:parse-alien-type
                #:alien-value-sap #:alien-value-type)
  (:import-from "SB-C" #:+backend-page-bytes+)
  (:import-from "SB-VM" #:map-objects-in-range #:reconstitute-object
                #:%closure-callee #:code-component-size)
  (:import-from "SB-DISASSEM" #:get-inst-space #:find-inst
                #:make-dstate #:%make-segment
                #:seg-virtual-location #:seg-length #:seg-sap-maker
                #:map-segment-instructions
                #:dstate-next-addr #:dstate-cur-offs)
  (:import-from "SB-X86-64-ASM" #:near-jump-displacement)
  (:import-from "SB-IMPL" #:package-hashtable #:package-%name
                #:package-hashtable-cells
                #:hash-table-table #:hash-table-number-entries))

(in-package "SB-EDITCORE")

(declaim (muffle-conditions compiler-note))

(eval-when (:execute)
  (setq *evaluator-mode* :compile))

(defconstant core-magic
  (logior (ash (char-code #\S) 24)
          (ash (char-code #\B) 16)
          (ash (char-code #\C) 8)
          (char-code #\L)))

(defglobal +noexec-stack-note+ ".section .note.GNU-stack, \"\", @progbits")

(defstruct (core-space ; "space" is a CL symbol
            (:conc-name space-)
            (:constructor make-space (id addr data-page page-adjust nwords)))
  id addr data-page page-adjust nwords)
(defun space-size (space) (* (space-nwords space) n-word-bytes))
(defun space-end (space) (+  (space-addr space) (space-size space)))
(defun space-nbytes-aligned (space)
  (align-up (space-size space) +backend-page-bytes+))
(defun space-physaddr (space spaces)
  (sap+ (car spaces) (* (space-data-page space) +backend-page-bytes+)))

;;; Given ADDR which is an address in the target core, return the address at which
;;; ADDR is currently mapped while performing the split.
;;; SPACES is a cons of a SAP and an alist whose elements are (ADDR . CORE-SPACE)
(defun translate-ptr (addr spaces)
  (let ((space (find addr (cdr spaces) :key #'space-addr :test #'>=)))
    ;; FIXME: duplicates SPACE-PHYSADDR to avoid consing a SAP.
    ;; macroize or something.
    (+ (sap-int (car spaces)) (* (space-data-page space) +backend-page-bytes+)
       (- addr (space-addr space)))))

;;;
(defun get-space (id spaces)
  (find id (cdr spaces) :key #'space-id))
(defun compute-nil-object (spaces)
  (let ((space (get-space static-core-space-id spaces)))
    (%make-lisp-obj (logior (space-addr space) #x17))))

;;; Given OBJ which is tagged pointer into the target core, translate it into
;;; the range at which the core is now mapped during execution of this tool,
;;; so that host accessors can dereference its slots.
;;; Use extreme care: while it works to use host accessors on the target core,
;;; we must avoid type checks on instances because LAYOUTs need translation.
;;; Printing boxed objects from the target core will almost always crash.
(defun translate (obj spaces)
  (%make-lisp-obj (translate-ptr (get-lisp-obj-address obj) spaces)))

(defstruct (core-sym (:copier nil) (:predicate nil)
                     (:constructor make-core-sym (package name external)))
  (package nil)
  (name nil :read-only t)
  (external nil :read-only t))

(defstruct (bounds (:constructor make-bounds (low high)))
  (low 0 :type word) (high 0 :type word))

(defstruct (core (:predicate nil)
                 (:copier nil)
                 (:constructor %make-core))
  (spaces)
  (nil-object)
  ;; mapping from string naming a package to list of symbol names (strings)
  ;; that are external in the package.
  (packages (make-hash-table :test 'equal))
  ;; hashset of symbol names (as strings) that should be package-qualified.
  ;; (Prefer not to package-qualify unambiguous names)
  (nonunique-symbol-names)
  (code-bounds nil :type bounds :read-only t)
  (fixedobj-bounds nil :type bounds :read-only t)
  (linkage-bounds nil :type bounds :read-only t)
  (linkage-symbols nil)
  (linkage-symbol-usedp nil)
  (linkage-entry-size nil)
  (dstate (make-dstate nil) :read-only t)
  (seg (%make-segment :sap-maker (lambda () (error "Bad sap maker"))
                      :virtual-location 0) :read-only t)
  (fixup-addrs nil)
  (call-inst nil :read-only t)
  (jmp-inst nil :read-only t)
  (pop-inst nil :read-only t))

(defun c-name (lispname core pp-state)
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

  (when (typep lispname '(string 0))
    (setq lispname "anonymous"))

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
                 (coerce (cond
                           #+darwin
                           ((and (stringp lispname)
                                 ;; L denotes a symbol which can not be global on macOS.
                                 (char= (char lispname 0) #\L))
                            (concatenate 'string "_" lispname))
                           (t
                            (write-to-string lispname
                              :pretty t :pprint-dispatch (cdr pp-state)
                              ;; FIXME: should be :level 1, however see
                              ;; https://bugs.launchpad.net/sbcl/+bug/1733222
                              :escape t :level 2 :length 5
                              :case :downcase :gensym nil
                              :right-margin 10000)))
                         'list))))
    (let ((string (coerce characters 'string)))
      ;; If the string appears in the linker symbols, then string-upcase it
      ;; so that it looks like a conventional Lisp symbol.
      (cond ((find-if (lambda (x) (string= string (if (consp x) (car x) x)))
                      (core-linkage-symbols core))
             (setq string (string-upcase string)))
            ((string= string ".") ; can't use the program counter symbol either
             (setq string "|.|")))
      ;; If the symbol is still nonunique, add a random suffix.
      ;; The secondary value is whether the symbol should be a linker global.
      ;; For now, make nothing global, thereby avoiding potential conflicts.
      (let ((occurs (incf (gethash string (car pp-state) 0))))
        (if (> occurs 1)
            (values (concatenate 'string  string "_" (write-to-string occurs))
                    nil)
            (values string
                    nil))))))

(defmethod print-object ((sym core-sym) stream)
  (format stream "~(~:[~*~;~:*~A~:[:~;~]:~]~A~)"
          (core-sym-package sym)
          (core-sym-external sym)
          (core-sym-name sym)))

(defun space-bounds (id spaces)
  (let ((space (get-space id spaces)))
    (make-bounds (space-addr space) (space-end space))))
(defun in-bounds-p (addr bounds)
  (and (>= addr (bounds-low bounds)) (< addr (bounds-high bounds))))

(defun make-hashset (contents count)
  (if (<= count 20)
      (coerce contents 'vector)
      (let ((ht (make-hash-table :test 'equal)))
        (dolist (x contents ht)
          (setf (gethash x ht) t)))))

(defun hashset-find (item hashset)
  (if (vectorp hashset)
      (find item hashset :test 'string=)
      (gethash item hashset)))

(defun scan-package-hashtable (function table core)
  (let ((spaces (core-spaces core))
        (nil-object (core-nil-object core)))
    (dovector (x (translate
                  (package-hashtable-cells
                   (truly-the package-hashtable (translate table spaces)))
                  spaces))
      (unless (fixnump x)
        (funcall function
                 (if (eq x nil-object) ; any random package can export NIL. wow.
                     "NIL"
                     (translate (symbol-name (translate x spaces)) spaces))
                 x)))))

(defun fun-name-from-core (name core &aux (spaces (core-spaces core))
                                          (packages (core-packages core))
                                          (core-nil (core-nil-object core)))
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
        ((stringp x)
         (let ((p (position #\/ x :from-end t)))
           (if p (subseq x (1+ p)) x)))
        ((symbolp x)
         (let ((name (translate (symbol-name x) spaces)))
           (when (eq (symbol-package x) core-nil) ; uninterned
             (return-from recurse (string-downcase name)))
           (let* ((package (truly-the package
                                      (translate (symbol-package x) spaces)))
                  (package-name (translate (package-%name package) spaces)))
             ;; The name-cleaning code wants to compare against symbols
             ;; in CL, PCL, and KEYWORD, so use real symbols for those.
             ;; Other than that, we avoid finding host symbols
             ;; because the externalness could be wrong and misleading.
             ;; It's a very subtle point, but best to get it right.
             (when (member package-name '("COMMON-LISP" "KEYWORD" "SB-PCL")
                           :test #'string=)
               ;; NIL can't occur. It was picked off above.
               (awhen (find-symbol name package-name) ; if existing symbol, use it
                 (return-from recurse it)))
             (unless (gethash name (core-nonunique-symbol-names core))
               ;; Don't care about package
               (return-from recurse (make-core-sym nil name nil)))
             (when (string= package-name "KEYWORD") ; make an external core-symbol
               (return-from recurse (make-core-sym nil name t)))
             (let ((externals (gethash package-name packages))
                   (n 0))
               (unless externals
                 (scan-package-hashtable
                  (lambda (string symbol)
                    (declare (ignore symbol))
                    (incf n)
                    (push string externals))
                  (package-external-symbols package)
                  core)
                 (setf externals (make-hashset externals n)
                       (gethash package-name packages) externals))
               (make-core-sym package-name
                              name
                              (hashset-find name externals))))))
        (t "?"))))))

(defstruct (descriptor (:constructor make-descriptor (bits)))
  (bits 0 :type sb-ext:word))
(defmethod print-object ((self descriptor) stream)
  (format stream "#<ptr ~x>" (descriptor-bits self)))
(defun descriptorize (obj)
  (if (is-lisp-pointer (get-lisp-obj-address obj))
      (make-descriptor (get-lisp-obj-address obj))
      obj))
(defun undescriptorize (target-descriptor)
  (%make-lisp-obj (descriptor-bits target-descriptor)))

(defun target-hash-table-alist (table spaces)
  (let ((table (truly-the hash-table (translate table spaces))))
    (let ((cells (the simple-vector (translate (hash-table-table table) spaces))))
      (collect ((pairs))
        (do ((count (hash-table-number-entries table) (1- count))
             (i 2 (+ i 2)))
            ((zerop count)
             (pairs))
          (pairs (cons (descriptorize (svref cells i))
                       (descriptorize (svref cells (1+ i))))))))))

;;; Return either the physical or logical address of the specified symbol.
(defun find-target-symbol (package-name symbol-name spaces
                           &optional (address-mode :physical))
  (dolist (id `(,immobile-fixedobj-core-space-id ,static-core-space-id))
    (let* ((space (get-space id spaces))
           (start (translate-ptr (space-addr space) spaces))
           (end (+ start (space-size space)))
           (physaddr start))
     (loop
      (when (>= physaddr end) (return))
      (multiple-value-bind (obj tag size)
          (reconstitute-object (ash physaddr (- n-fixnum-tag-bits)))
        (when (and (= tag symbol-widetag)
                   (string= symbol-name (translate (symbol-name obj) spaces))
                   (%instancep (symbol-package obj))
                   (string= package-name
                            (translate
                             (package-%name
                              (truly-the package (translate (symbol-package obj) spaces)))
                             spaces)))
          (return-from find-target-symbol
            (%make-lisp-obj
                   (logior (ecase address-mode
                             (:physical physaddr)
                             (:logical (+ (space-addr space) (- physaddr start))))
                           other-pointer-lowtag))))
        (incf physaddr size)))))
  (bug "Can't find symbol ~A::~A" package-name symbol-name))

(defun compute-linkage-symbols (spaces entry-size)
  (let* ((hashtable (symbol-global-value
                     (find-target-symbol "SB-SYS" "*LINKAGE-INFO*"
                                         spaces :physical)))
         (pairs (target-hash-table-alist hashtable spaces))
         (min (reduce #'min pairs :key #'cdr))
         (max (reduce #'max pairs :key #'cdr))
         (n (1+ (/ (- max min) entry-size)))
         (vector (make-array n)))
    (dolist (entry pairs vector)
      (let* ((key (undescriptorize (car entry)))
             (entry-index (/ (- (cdr entry) min) entry-size))
             (string (translate (if (consp key) (car (translate key spaces)) key)
                                spaces)))
        (setf (aref vector entry-index)
              (if (consp key) (list string) string))))))

(defun make-core (spaces code-bounds fixedobj-bounds)
  (let* ((linkage-bounds
          (make-bounds
           (symbol-global-value
            (find-target-symbol "SB-VM" "LINKAGE-TABLE-SPACE-START" spaces :physical))
           (symbol-global-value
            (find-target-symbol "SB-VM" "LINKAGE-TABLE-SPACE-END" spaces :physical))))
         (linkage-entry-size
          (symbol-global-value
           (find-target-symbol "SB-VM" "LINKAGE-TABLE-ENTRY-SIZE"
                               spaces :physical)))
         (linkage-symbols (compute-linkage-symbols spaces linkage-entry-size))
         (inst-space (get-inst-space))
         (nil-object (compute-nil-object spaces))
         (ambiguous-symbols (make-hash-table :test 'equal))
         (core
          (%make-core
           :spaces spaces
           :nil-object nil-object
           :nonunique-symbol-names ambiguous-symbols
           :code-bounds code-bounds
           :fixedobj-bounds fixedobj-bounds
           :linkage-bounds linkage-bounds
           :linkage-entry-size linkage-entry-size
           :linkage-symbols linkage-symbols
           :linkage-symbol-usedp (make-array (length linkage-symbols) :element-type 'bit)
           :call-inst (find-inst #b11101000 inst-space)
           :jmp-inst (find-inst #b11101001 inst-space)
           :pop-inst (find-inst #x5d inst-space))))
    (let ((package-table
           (symbol-global-value
            (find-target-symbol "SB-KERNEL" "*PACKAGE-NAMES*" spaces :physical)))
          (symbols (make-hash-table :test 'equal)))
      (dovector (x (translate (%instance-ref (translate package-table spaces) 0) spaces))
        (when (%instancep x) ; package
          (flet ((scan (table)
                   (scan-package-hashtable
                    (lambda (str sym)
                      (pushnew (get-lisp-obj-address sym) (gethash str symbols)))
                    table core)))
            (let ((package (truly-the package (translate x spaces))))
              (scan (package-external-symbols package))
              (scan (package-internal-symbols package))))))
      (dohash ((string symbols) symbols)
        (when (cdr symbols)
          (setf (gethash string ambiguous-symbols) t))))
    core))

;;; Emit .byte or .quad directives dumping memory from SAP for COUNT units
;;; (bytes or qwords) to STREAM.  SIZE specifies which direcive to emit.
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

(defun code-fixup-locs (code spaces)
  (let ((locs (sb-vm::%code-fixups code)))
    ;; Return only the absolute fixups
    ;; Ensure that a bignum LOCS is translated before using it.
    (values (sb-c::unpack-code-fixup-locs
             (if (fixnump locs) locs (translate locs spaces))))))

;;; Disassemble the function pointed to by SAP for LENGTH bytes, returning
;;; all instructions that should be emitted using assembly language
;;; instead of assembler pseudo-ops. This includes two sets of instructions:
;;; - function prologue instructions that setup the call frame
;;; - jmp/call instructions that transfer control to the fixedoj space
;;;    delimited by bounds in STATE.
;;; At execution time the function will have virtual address LOAD-ADDR.
(defun list-non-opaque-instructions (sap length core load-addr emit-cfi)
  (let ((dstate (core-dstate core))
        (seg (core-seg core))
        (call-inst (core-call-inst core))
        (jmp-inst (core-jmp-inst core))
        (pop-inst (core-pop-inst core))
        (next-fixup-addr
         (or (car (core-fixup-addrs core)) most-positive-word))
        (list))
    (setf (seg-virtual-location seg) load-addr
          (seg-length seg) length
          (seg-sap-maker seg) (lambda () sap))
    ;; KLUDGE: "8f 45 08" is the standard prologue
    (when (and emit-cfi (= (logand (sap-ref-32 sap 0) #xFFFFFF) #x08458f))
      (push (list* 0 3 "pop" "8(%rbp)") list))
    (map-segment-instructions
     (lambda (dchunk inst)
       (cond
         ((< next-fixup-addr (dstate-next-addr dstate))
          (let ((operand (sap-ref-32 sap (- next-fixup-addr load-addr))))
            (when (in-bounds-p operand (core-code-bounds core))
              (aver (eql (sap-ref-8 sap (- next-fixup-addr load-addr 1)) #xB8)) ; mov rax, imm32
              (push (list* (dstate-cur-offs dstate) 5 "mov" operand) list)))
          (pop (core-fixup-addrs core))
          (setq next-fixup-addr (or (car (core-fixup-addrs core)) most-positive-word)))
         ((or (eq inst jmp-inst) (eq inst call-inst))
          (let ((target-addr (+ (near-jump-displacement dchunk dstate)
                                (dstate-next-addr dstate))))
            (when (or (in-bounds-p target-addr (core-fixedobj-bounds core))
                      (in-bounds-p target-addr (core-linkage-bounds core)))
              (push (list* (dstate-cur-offs dstate)
                           5 ; length
                           (if (eq inst call-inst) "call" "jmp")
                           target-addr)
                    list))))
         ((and (eq inst pop-inst) (eq (logand dchunk #xFF) #x5D))
          (push (list* (dstate-cur-offs dstate) 1 "pop" "%rbp") list))))
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

(defun emit-lisp-function (paddr vaddr count labels stream emit-cfi core)
  (when emit-cfi
    (format stream " .cfi_startproc~%"))
  ;; Any byte offset that appears as a key in the INSTRUCTIONS causes the indicated
  ;; bytes to be written as an assembly language instruction rather than opaquely,
  ;; thereby affecting the ELF data (cfi or relocs) produced.
  (let ((instructions
         (merge 'list labels
                (list-non-opaque-instructions (int-sap paddr) count core vaddr emit-cfi)
                #'< :key #'car))
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
          ;; A label and a textual instruction could co-occur.
          ;; If so, the label must be emitted first.
          (when (eq (cadar instructions) :label)
            (destructuring-bind (globalp c-symbol) (cddr (pop instructions))
              ;; The C symbol is global only if the Lisp name is a legal function
              ;; designator and not random noise.
              ;; This is a technique to try to avoid appending a uniquifying suffix
              ;; on all the junky internal things like "(lambda # in srcfile.lisp)"
              (when emit-cfi
                (format stream "~:[~; .globl ~a~:*~%~] .type ~a, @function~%"
                        globalp c-symbol))
              (format stream "~a:~%" c-symbol)))
          ;; If both a label and textual instruction occur here, handle the latter.
          ;; [This could could be simpler if all labels were emitted as
          ;; '.set "thing", .+const' together in a single place, but it's more readable
          ;; to see them where they belong in the instruction stream]
          (when (and instructions (= (caar instructions) cur-offset))
            (destructuring-bind (length opcode . operand) (cdr (pop instructions))
              (when (cond ((member opcode '("jmp" "call") :test #'string=)
                           (when (in-bounds-p operand (core-linkage-bounds core))
                             (let ((entry-index
                                    (/ (- operand (bounds-low (core-linkage-bounds core)))
                                       (core-linkage-entry-size core))))
                               (setf (bit (core-linkage-symbol-usedp core) entry-index) 1
                                     operand (aref (core-linkage-symbols core) entry-index))))
                           (format stream " ~A ~:[0x~X~;~a~]~%"
                                   opcode (stringp operand) operand))
                          ((string= opcode "pop")
                           (format stream " ~A ~A~%" opcode operand)
                           (cond ((string= operand "8(%rbp)")
                                  (format stream " .cfi_def_cfa 6, 16~% .cfi_offset 6, -16~%"))
                                 ((string= operand "%rbp")
                                        ;(format stream " .cfi_def_cfa 7, 8~%")
                                  nil)
                                 (t)))
                          ((string= opcode "mov")
                           (format stream " mov $(__lisp_code_start+0x~x),%eax~%"
                                   (- operand (bounds-low (core-code-bounds core)))))
                          (t))
                (bug "Random annotated opcode ~S" opcode))
              (incf ptr length)))
          (when (= cur-offset count) (return))))))
  (when emit-cfi
    (format stream " .cfi_endproc~%")))

;;; Examine CODE, returning a list of lists describing how to emit
;;; the contents into the assembly file.
;;;   ({:data | :padding} . N) | ((start-pc . end-pc) (pc-offset . name) ...)
(defun get-text-ranges (code spaces)
  (flet ((fun-pc-offs (index)
           (- (get-lisp-obj-address (%code-entry-point code index))
              fun-pointer-lowtag
              (sap-int (code-instructions code)))))
    (let ((map (translate (sb-c::compiled-debug-info-fun-map
                           (truly-the sb-c::compiled-debug-info
                                      (translate (%code-debug-info code) spaces)))
                          spaces))
          (next-simple-fun-pc-offs (fun-pc-offs 0))
          (start-pc (code-n-unboxed-data-bytes code))
          (simple-fun-index -1)
          (simple-fun)
          (blobs))
      (when (plusp start-pc)
        (aver (zerop (rem start-pc n-word-bytes)))
        (push `(:data . ,(ash start-pc (- word-shift))) blobs))
      (do ((i 0 (+ i 2)))
          ((>= i (length map)) (nreverse blobs))
        (let* ((cdf (translate (aref map i) spaces))
               (end-pc (if (< (1+ i) (length map))
                           (svref map (1+ i))
                           (%code-text-size code)))
               (name (get-lisp-obj-address
                      (sb-c::compiled-debug-fun-name
                       (truly-the sb-c::compiled-debug-fun cdf)))))
          ;; (format t "range=~D .. ~D ~X~%" start-pc end-pc name)
          (cond
            ((= start-pc end-pc)) ; crazy shiat. do not add to blobs
            ((<= start-pc next-simple-fun-pc-offs (1- end-pc))
             (incf simple-fun-index)
             (setq simple-fun (%code-entry-point code simple-fun-index))
             (let ((padding (- next-simple-fun-pc-offs start-pc)))
               (when (plusp padding)
                 ;; Assert that SIMPLE-FUN always begins at an entry
                 ;; in the fun-map, and not somewhere in the middle:
                 ;;   |<--  fun  -->|<--  fun  -->|
                 ;;   ^- start (GOOD)      ^- alleged start (BAD)
                 (cond ((eq simple-fun (%code-entry-point code 0))
                        (bug "Misaligned fun start"))
                       (t ; sanity-check the length of the filler
                        (aver (< padding (* 2 n-word-bytes)))))
                 (push `(:pad . ,padding) blobs)
                 (incf start-pc padding)))
             (let ((skip (* simple-fun-code-offset n-word-bytes)))
               (push `((,start-pc . ,end-pc) (,(+ start-pc skip) . ,name)) blobs))
             (setq next-simple-fun-pc-offs (fun-pc-offs (1+ simple-fun-index))))
            (t
             (let ((current-blob (car blobs)))
               (setf (cdar current-blob) end-pc) ; extend this blob
               ;; Assuming that the compiler properly coalesced EQUAL names
               ;; when dumping fun-map names, = is valid as a comparator
               ;; to determine that we're in the same sub-function.
               (unless (= name (cdadr current-blob))
                 (push `(,start-pc . ,name) (cdr current-blob))))))
          (setq start-pc end-pc))))))

(defun c-symbol-quote (name)
  (concatenate 'string '(#\") name '(#\")))

(defun emit-funs (code vaddr core pp-state dumpwords output emit-cfi)
  (let* ((spaces (core-spaces core))
         (ranges (get-text-ranges code spaces))
         (text-sap (code-instructions code))
         (text (sap-int text-sap))
         (text-vaddr (+ vaddr (* (code-header-words code) n-word-bytes)))
         (max-end 0))
    (when (eq (caar ranges) :data) ; Unboxed data and/or padding
      (funcall dumpwords text (cdr (pop ranges)) output))
    (loop
      (destructuring-bind ((start . end) . symbol-table) (pop ranges)
        (setq max-end end)
        (funcall dumpwords (+ text start) simple-fun-code-offset output
                 #(nil #.(format nil ".+~D" (* (1- simple-fun-code-offset)
                                             n-word-bytes))))
        (incf start (* simple-fun-code-offset n-word-bytes))
        ;; Compute assembler labels. DWARF info can represent discontiguous PC ranges
        ;; belonging to a single function. e.g. in CL:COMPLEX we have:
        ;;  range   0 .. 715 = COMPLEX
        ;;  range 715 .. 964 = (FLET SB-KERNEL::%%MAKE-COMPLEX :IN COMPLEX)
        ;;  range 964 .. end = COMPLEX
        ;; You have to explicitly write your own DWARF info to represent that-
        ;; the assembler won't do it. To avoid that complication for now,
        ;; we append a uniquifying suffix to the the second and later occurrence.
        (let ((labels
               (mapcar (lambda (x)
                         (binding* ((lispname
                                     (fun-name-from-core (%make-lisp-obj (cdr x)) core))
                                    ((c-name globalp)
                                     (c-name (or lispname "anonymous") core pp-state))
                                    (quoted-name
                                     (c-symbol-quote c-name)))
                           (list (- (car x) start) :label globalp quoted-name)))
                       (nreverse symbol-table))))
          ;; Pass the current physical address at which to disassemble,
          ;; the notional core address (which changes after linker relocation),
          ;; and the list of locations at which to attach linker symbols.
          (emit-lisp-function (+ text start) (+ text-vaddr start) (- end start)
                              labels output emit-cfi core))
        (cond ((not ranges) (return))
              ((eq (caar ranges) :pad)
               (format output " .byte ~{0x~x~^,~}~%"
                       (loop for i from 0 below (cdr (pop ranges))
                             collect (sap-ref-8 text-sap (+ end i))))))))
    ;; All fixups should have been consumed by writing out the text.
    (aver (null (core-fixup-addrs core)))
    ;; Emit bytes from the maximum function end to the object end.
    ;; We can't just round up %CODE-CODE-SIZE to a double-lispword
    ;; because the boxed header could end at an odd word, requiring that
    ;; the unboxed bytes have an odd size in words making the total even.
    (format output " .byte ~{0x~x~^,~}~%"
            (loop for i from max-end
                  below (- (code-component-size code)
                           (* (code-header-words code) n-word-bytes))
                  collect (sap-ref-8 text-sap i)))))

;;; Convert immobile varyobj space to an assembly file in OUTPUT.
;;; TODO: relocate fdefns and instances of standard-generic-function
;;; into the space that is dumped into an ELF section.
(defun write-assembler-text
    (spaces output
     &optional emit-sizes (emit-cfi t)
     &aux (code-bounds (space-bounds immobile-varyobj-core-space-id spaces))
          (fixedobj-bounds (space-bounds immobile-fixedobj-core-space-id spaces))
          (core (make-core spaces code-bounds fixedobj-bounds))
          (code-addr (bounds-low code-bounds))
          (total-code-size 0)
          (pp-state (cons (make-hash-table :test 'equal)
                          ;; copy no entries for macros/special-operators (flet, etc)
                          (sb-pretty::make-pprint-dispatch-table)))
          (prev-namestring "")
          (n-linker-relocs 0)
          end-loc)
  (set-pprint-dispatch 'string
                       ;; Write strings without string quotes
                       (lambda (stream string) (write-string string stream))
                       0
                       (cdr pp-state))
  (labels ((dumpwords (addr count stream &optional (exceptions #()) logical-addr)
             (let ((sap (int-sap addr)))
               (aver (sap>= sap (car spaces)))
               ;; Make intra-code-space pointers computed at link time
               (dotimes (i (if logical-addr count 0))
                 (unless (and (< i (length exceptions)) (svref exceptions i))
                   (let ((word (sap-ref-word sap (* i n-word-bytes))))
                     (when (and (= (logand word 3) 3) ; is a pointer
                                (in-bounds-p word code-bounds)) ; to code space
                       #+nil
                       (format t "~&~(~x: ~x~)~%" (+ logical-addr  (* i n-word-bytes))
                               word)
                       (incf n-linker-relocs)
                       (setf exceptions (adjust-array exceptions (max (length exceptions) (1+ i))
                                                      :initial-element nil)
                             (svref exceptions i)
                             (format nil "__lisp_code_start+0x~x"
                                     (- word (bounds-low code-bounds))))))))
               (emit-asm-directives :qword sap count stream exceptions)))
           (make-code-obj (addr)
             (let ((translation (translate-ptr addr spaces)))
               (aver (= (%widetag-of (sap-ref-word (int-sap translation) 0))
                        code-header-widetag))
               (%make-lisp-obj (logior translation other-pointer-lowtag))))
           (%widetag-of (word)
             (logand word widetag-mask)))
    (format output " .text~% .file \"sbcl.core\"
 .globl __lisp_code_start, lisp_jit_code, __lisp_code_end
 .balign 4096~%__lisp_code_start:~%")

    ;; Scan the assembly routines.
    (let* ((code-component (make-code-obj code-addr))
           (header-len (code-header-words code-component)))
      ;; Write the code component header
      (emit-asm-directives :qword
                           (int-sap (- (get-lisp-obj-address code-component)
                                       sb-vm:other-pointer-lowtag))
                           header-len output #())
      (let ((name->addr
             ;; the CDR of each alist item is a target cons (needing translation)
             (sort
              (mapcar (lambda (entry &aux (name (translate (undescriptorize (car entry)) spaces)) ; symbol
                                          ;; VAL is (start end . index)
                                          (val (translate (undescriptorize (cdr entry)) spaces))
                                          (start (car val))
                                          (end (car (translate (cdr val) spaces))))
                        (list* (translate (symbol-name name) spaces) start end))
                      (target-hash-table-alist
                       (car (translate (%code-debug-info code-component) spaces))
                       spaces))
              #'< :key #'cadr)))
        (let* ((n-entrypoints (length name->addr))
               (min-entry-offs (cadar name->addr))
               (n-words (/ min-entry-offs sb-vm:n-word-bytes)))
          ;; Write a table of N-WORDS in length containing the entrypoints
          ;; Not all words in the jump table will necessarily be used.
          (dotimes (i n-words)
            (format output " .quad ~:[0~;__lisp_code_start+0x~:*~x~]~%"
                    (when (< i n-entrypoints)
                      (+ (ash header-len sb-vm:word-shift)
                         (cadr (nth i name->addr)))))))
        ;; Loop over the embedded routines
        (dolist (entry name->addr)
          (destructuring-bind (name start-offs . end-offs) entry
            (let ((nbytes (- (1+ end-offs) start-offs)))
              (format output " .set ~a, .~%~@[ .size ~:*~a, ~d~%~]"
                      (format nil "~(\"~a\"~)" name) (if emit-sizes nbytes))
              (emit-lisp-function (+ (sap-int (code-instructions code-component))
                                     start-offs)
                                  (+ code-addr
                                     (ash (code-header-words code-component) sb-vm:word-shift)
                                     start-offs)
                                  nbytes nil output nil core)))))
      (format output " .quad 0, 0~%") ; trailer with SIMPLE-FUN count of 0
      (let ((size (code-component-size code-component))) ; No need to pin
        (incf code-addr size)
        (setf total-code-size size)))
    (loop
      (when (>= code-addr (bounds-high code-bounds)) (return))
      (let* ((code (make-code-obj code-addr))
             (objsize (code-component-size code)))
        (setq end-loc (+ code-addr objsize))
        (incf total-code-size objsize)
        (cond
          ((< (code-header-words code) 4) ; filler object
           ;; ** THIS CASE IS UNTESTED **
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
             (setq namestring (if (eq namestring (core-nil-object core))
                                  "sbcl.core"
                                  (translate namestring spaces)))
             (unless (string= namestring prev-namestring)
               (format output " .file \"~a\"~%" namestring)
               (setq prev-namestring namestring)))
           (setf (core-fixup-addrs core)
                 (mapcar (lambda (x)
                           (+ code-addr (ash (code-header-words code) word-shift) x))
                         (code-fixup-locs code spaces)))
           (let ((code-physaddr (logandc2 (get-lisp-obj-address code) lowtag-mask)))
             (format output "#x~x:~%" code-addr)
             (dumpwords code-physaddr (code-header-words code) output #() code-addr)
             (emit-funs code code-addr core pp-state #'dumpwords output emit-cfi)))
          (t
           (error "Strange code component: ~S" code)))
        (incf code-addr objsize))))

  ;; coreparse uses the 'lisp_jit_code' symbol to set varyobj_free_pointer
  ;; The intent is that compilation to memory can use this reserved area
  ;; (if space remains) so that profilers can associate a C symbol with the
  ;; program counter range. It's better than nothing.
  (format output "lisp_jit_code:~%")

  ;; Pad so that non-lisp code can't be colocated on a GC page.
  ;; (Lack of Lisp object headers in C code is the issue)
  (let ((aligned-end (align-up end-loc 4096)))
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
  ;; Extend with .5 MB of filler
  (format output " .fill ~D~%__lisp_code_end:
 .size lisp_jit_code, .-lisp_jit_code~%"
          (* 512 1024))
  ; (format t "~&linker-relocs=~D~%" n-linker-relocs)
  (values core total-code-size n-linker-relocs))

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
(defconstant ehdr-size (ceiling (alien-type-bits (parse-alien-type 'elf64-ehdr nil)) 8))
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
(defconstant shdr-size (ceiling (alien-type-bits (parse-alien-type 'elf64-shdr nil)) 8))
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
    (offset (unsigned 64))
    (info   (unsigned 64))
    (addend (signed 64))))

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
    (cons (nreverse alist) bytes)))

(defun write-alien (alien size stream)
  (dotimes (i size)
    (write-byte (sap-ref-8 (alien-value-sap alien) i) stream)))

(defun copy-bytes (in-stream out-stream nbytes
                             &optional (buffer
                                        (make-array 1024 :element-type '(unsigned-byte 8))))
  (loop (let ((chunksize (min (length buffer) nbytes)))
          (aver (eql (read-sequence buffer in-stream :end chunksize) chunksize))
          (write-sequence buffer out-stream :end chunksize)
          (when (zerop (decf nbytes chunksize)) (return)))))

;;; core header should be an array of words in '.rodata', not a 32K page
(defconstant core-header-size +backend-page-bytes+) ; stupidly large (FIXME)

(defun write-elf-header (shdrs-start sections output)
  (let ((shnum (1+ (length sections))) ; section 0 is implied
        (shstrndx (1+ (position :str sections :key #'car)))
        (ident #.(coerce '(#x7F #x45 #x4C #x46 2 1 1 0 0 0 0 0 0 0 0 0)
                         '(array (unsigned-byte 8) 1))))
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
          (slot ehdr 'shnum)     shnum
          (slot ehdr 'shstrndx)  shstrndx)
    (write-alien ehdr ehdr-size output))))

(defun write-section-headers (placements sections string-table output)
  (with-alien ((shdr elf64-shdr))
    (dotimes (i (ceiling shdr-size n-word-bytes)) ; Zero-fill
      (setf (sap-ref-word (alien-value-sap shdr) (* i n-word-bytes)) 0))
    (dotimes (i (1+ (length sections)))
      (when (plusp i) ; Write the zero-filled header as section 0
        (destructuring-bind (name type flags link info alignment entsize)
            (cdr (aref sections (1- i)))
          (destructuring-bind (offset . size)
              (pop placements)
            (setf (slot shdr 'name)  (cdr (assoc name (car string-table)))
                  (slot shdr 'type)  type
                  (slot shdr 'flags) flags
                  (slot shdr 'off)   offset
                  (slot shdr 'size)  size
                  (slot shdr 'link)  link
                  (slot shdr 'info)  info
                  (slot shdr 'addralign) alignment
                  (slot shdr 'entsize) entsize))))
      (write-alien shdr shdr-size output))))

(defconstant core-align 4096)
(defconstant sym-entry-size 24)

;;; Write everything except for the core file itself into OUTPUT-STREAM
;;; and leave the stream padded to a 4K boundary ready to receive data.
(defun prepare-elf (core-size relocs output)
  (let* ((reloc-entry-size 24)
         (sections
          `#((:core "lisp.core"       ,+sht-progbits+ 0 0 0 ,core-align 0)
             (:sym  ".symtab"         ,+sht-symtab+   0 3 1 8 ,sym-entry-size)
                          ; section with the strings -- ^ ^ -- 1+ highest local symbol
             (:str  ".strtab"         ,+sht-strtab+   0 0 0 1  0)
             (:rel  ".relalisp.core"  ,+sht-rela+     0 2 1 8 ,reloc-entry-size)
                                      ; symbol table -- ^ ^ -- for which section
             (:note ".note.GNU-stack" ,+sht-null+     0 0 0 1  0)))
         (string-table
          (string-table (append '("__lisp_code_start") (map 'list #'second sections))))
         (strings (cdr string-table))
         (padded-strings-size (align-up (length strings) 8))
         (symbols-size (* 2 sym-entry-size))
         (shdrs-start (+ ehdr-size symbols-size padded-strings-size))
         (shdrs-end (+ shdrs-start (* (1+ (length sections)) shdr-size)))
         (relocs-size (* (length relocs) reloc-entry-size))
         (relocs-end (+ shdrs-end relocs-size))
         (core-start (align-up relocs-end core-align)))

    (write-elf-header shdrs-start sections output)

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
    (write-sequence strings output) ; an octet vector at this point
    (dotimes (i (- padded-strings-size (length strings)))
      (write-byte 0 output))

    ;; Write section headers
    (aver (eql (file-position output) shdrs-start))
    (write-section-headers
     (map 'list
          (lambda (x)
            (ecase (car x)
              (:note '(0 . 0))
              (:sym  (cons ehdr-size symbols-size))
              (:str  (cons (+ ehdr-size symbols-size) (length strings)))
              (:rel  (cons shdrs-end relocs-size))
              (:core (cons core-start core-size))))
          sections)
     sections string-table output)

    ;; Write relocations
    (aver (eql (file-position output) shdrs-end))
    (let ((buf (make-array relocs-size :element-type '(unsigned-byte 8)))
          (ptr 0))
      (with-alien ((rela elf64-rela))
        (dovector (reloc relocs)
          (destructuring-bind (place addend . kind) reloc
            (setf (slot rela 'offset) place
                  (slot rela 'info)   (logior (ash 1 32) kind) ; 1 = symbol index
                  (slot rela 'addend) addend))
          (setf (%vector-raw-bits buf (+ ptr 0)) (sap-ref-word (alien-value-sap rela) 0)
                (%vector-raw-bits buf (+ ptr 1)) (sap-ref-word (alien-value-sap rela) 8)
                (%vector-raw-bits buf (+ ptr 2)) (sap-ref-word (alien-value-sap rela) 16))
          (incf ptr 3)))
      (write-sequence buf output))

    ;; Write padding
    (dotimes (i (- core-start (file-position output)))
      (write-byte 0 output))
    (aver (eq (file-position output) core-start))))

(defconstant R_X86_64_64    1) ; /* Direct 64 bit  */
(defconstant R_X86_64_PC32  2) ; /* PC relative 32 bit signed */
(defconstant R_X86_64_32   10) ; /* Direct 32 bit zero extended */
(defconstant R_X86_64_32S  11) ; /* Direct 32 bit sign extended */

;;; Return a list of fixups (FIXUP-WHERE ADDEND . KIND) to peform in a foreign core
;;; whose code space is subject to link-time relocation.
(defun collect-relocations (spaces fixups &aux (print nil))
  (let* ((code-bounds (space-bounds immobile-varyobj-core-space-id spaces))
         (code-start (bounds-low code-bounds))
         (n-abs 0)
         (n-rel 0))
    (labels
        ((abs-fixup (core-offs referent)
           (incf n-abs)
           (when print
              (format t "~x = 0x~(~x~): (a)~%" core-offs (core-to-logical core-offs) #+nil referent))
           (setf (sap-ref-word (car spaces) core-offs) 0)
           (vector-push-extend `(,(+ core-header-size core-offs)
                                 ,(- referent code-start) . ,R_X86_64_64)
                               fixups))
         (abs32-fixup (core-offs referent)
           (incf n-abs)
           (when print
              (format t "~x = 0x~(~x~): (a)~%" core-offs (core-to-logical core-offs) #+nil referent))
           (setf (sap-ref-32 (car spaces) core-offs) 0)
           (vector-push-extend `(,(+ core-header-size core-offs)
                                 ,(- referent code-start) . ,R_X86_64_32)
                               fixups))
         (rel-fixup (core-offs referent addend)
           (incf n-rel)
           (when print
             (format t "~x = 0x~(~x~): (r)~%" core-offs (core-to-logical core-offs) #+nil referent))
           (setf (sap-ref-32 (car spaces) core-offs) 0)
           (vector-push-extend `(,(+ core-header-size core-offs)
                                 ;; Emitted as signed absolute plus addend,
                                 ;; since the originating PC is known.
                                 ,(+ (- referent code-start) addend) . ,R_X86_64_32S)
                               fixups))
         ;; Given a address which is an offset into the data pages of the target core,
         ;; compute the logical address which that offset would be mapped to.
         ;; For example core address 0 is the virtual address of static space.
         (core-to-logical (core-offs &aux (page (floor core-offs +backend-page-bytes+)))
           (dolist (space (cdr spaces)
                          (bug "Can't translate core offset ~x using ~x"
                               core-offs spaces))
             (let* ((page0 (space-data-page space))
                    (nwords (space-nwords space))
                    (id (space-id space))
                    (npages (ceiling nwords (/ +backend-page-bytes+ n-word-bytes))))
               (when (and (<= page0 page (+ page0 (1- npages)))
                          (/= id immobile-varyobj-core-space-id))
                 (return (+ (space-addr space)
                            (* (- page page0) +backend-page-bytes+)
                            (logand core-offs (1- +backend-page-bytes+))))))))
         (scanptrs (obj wordindex-min wordindex-max &aux (n-fixups 0))
           (do* ((base-addr (logandc2 (get-lisp-obj-address obj) lowtag-mask))
                 (sap (int-sap base-addr))
                 ;; core-offs is the offset in the lisp.core ELF section.
                 (core-offs (- base-addr (sap-int (car spaces))))
                 (i wordindex-min (1+ i)))
                ((> i wordindex-max) n-fixups)
             (let ((ptr (sap-ref-word sap (ash i word-shift))))
               (when (and (= (logand ptr 3) 3) (in-bounds-p ptr code-bounds))
                 (abs-fixup (+ core-offs (ash i word-shift)) ptr)
                 (incf n-fixups)))))
         (scanptr (obj wordindex)
           (plusp (scanptrs obj wordindex wordindex))) ; trivial wrapper
         (scan-obj (obj widetag size vaddr
                    &aux (core-offs (- (logandc2 (get-lisp-obj-address obj) lowtag-mask)
                                       (sap-int (car spaces))))
                         (nwords (ceiling size n-word-bytes)))
           (when (listp obj)
             (scanptrs obj 0 1)
             (return-from scan-obj))
           (case widetag
             (#.instance-widetag
              (let* ((layout (truly-the layout
                              (translate (%instance-layout obj) spaces)))
                     (bitmap (layout-bitmap layout))
                     (translated
                      (if (fixnump bitmap) bitmap (translate bitmap spaces))))
                (do-instance-tagged-slot (i obj :bitmap translated)
                  (scanptr obj (1+ i))))
              (return-from scan-obj))
             (#.simple-vector-widetag
              (let ((len (length (the simple-vector obj))))
                (when (eql (logand (get-header-data obj) #xFF) vector-valid-hashing-subtype)
                  (do ((i 2 (+ i 2)) (needs-rehash))
                      ((= i len)
                       (when needs-rehash
                         (setf (svref obj 1) 1)))
                    ;; A weak or EQ-based hash table any of whose keys is a function
                    ;; or code-component might need the 'rehash' flag set.
                    ;; In practice, it is likely already set, because any object that
                    ;; could move in the final GC probably did move.
                    (when (scanptr obj (+ vector-data-offset i))
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
                     (next-pc (+ fdefn-logical-pc 5))
                     (target (+ next-pc rel32off)))
                (when (in-bounds-p target code-bounds)
                  ;; This addend needs to account for the fact that the location
                  ;; where fixup occurs is not where the fdefn will actually exist.
                  (rel-fixup (+ core-offs (ash 3 word-shift) 1)
                             target (- next-pc))))
              (return-from scan-obj))
             ((#.closure-widetag #.funcallable-instance-widetag)
              (let ((word (sap-ref-word (int-sap (get-lisp-obj-address obj))
                                        (- n-word-bytes fun-pointer-lowtag))))
                (when (in-bounds-p word code-bounds)
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
              (dolist (loc (code-fixup-locs obj spaces))
                (let ((val (sap-ref-32 (code-instructions obj) loc)))
                  (when (in-bounds-p val code-bounds)
                    (abs32-fixup (sap- (sap+ (code-instructions obj) loc) (car spaces))
                                 val))))
              (dotimes (i (code-n-entries obj))
                (scanptrs (%code-entry-point obj i) 2 5))
              (setq nwords (code-header-words obj)))
             ;; boxed objects that can reference code/simple-funs
             ((#.value-cell-widetag #.symbol-widetag #.weak-pointer-widetag))
             (t
              (return-from scan-obj)))
           (scanptrs obj 1 (1- nwords))))
      (dolist (space (reverse (cdr spaces)))
        (let* ((logical-addr (space-addr space))
               (size (space-size space))
               (physical-addr (space-physaddr space spaces))
               (physical-end (sap+ physical-addr size))
               (vaddr-translation (+ (- (sap-int physical-addr)) logical-addr)))
          (unless (= (space-id space) immobile-varyobj-core-space-id)
            (dx-flet ((visit (obj widetag size)
                        ;; Compute the object's intended virtual address
                        (let ((vaddr (+ (logandc2 (get-lisp-obj-address obj) lowtag-mask)
                                        vaddr-translation)))
                          (scan-obj obj widetag size vaddr))))
              (map-objects-in-range
               #'visit
               (ash (sap-int physical-addr) (- n-fixnum-tag-bits))
               (ash (sap-int physical-end) (- n-fixnum-tag-bits))))
            (when (and (plusp (logior n-abs n-rel)) print)
              (format t "space @ ~x: ~d absolute + ~d relative fixups~%"
                      logical-addr n-abs n-rel))
            (setq n-abs 0 n-rel 0))))))
  (when print
    (format t "total of ~D linker fixups~%" (length fixups)))
  fixups)

;;;;

(defun read-core-header (input core-header verbose &aux (core-offset 0))
  (read-sequence core-header input)
  (cond ((= (%vector-raw-bits core-header 0) core-magic))
        (t ; possible embedded core
         (file-position input (- (file-length input)
                                 (* 2 n-word-bytes)))
         (aver (eql (read-sequence core-header input) (* 2 n-word-bytes)))
         (aver (= (%vector-raw-bits core-header 1) core-magic))
         (setq core-offset (%vector-raw-bits core-header 0))
         (when verbose
           (format t "~&embedded core starts at #x~x into input~%" core-offset))
         (file-position input core-offset)
         (read-sequence core-header input)
         (aver (= (%vector-raw-bits core-header 0) core-magic))))
  core-offset)

(macrolet ((do-core-header-entry (((id-var len-var ptr-var) buffer) &body body)
             `(let ((,ptr-var 1))
                (loop
                  (let ((,id-var (%vector-raw-bits ,buffer ,ptr-var))
                        (,len-var (%vector-raw-bits ,buffer (1+ ,ptr-var))))
                    (incf ,ptr-var 2)
                    (decf ,len-var 2)
                    (when (= ,id-var end-core-entry-type-code) (return))
                    ,@body
                    (incf ,ptr-var ,len-var)))))
           (do-directory-entry (((index-var start-index input-nbytes) buffer) &body body)
             `(let ((words-per-dirent 5))
                (multiple-value-bind (n-entries remainder)
                    (floor ,input-nbytes words-per-dirent)
                  (aver (zerop remainder))
                  (symbol-macrolet ((id        (%vector-raw-bits ,buffer index))
                                    (nwords    (%vector-raw-bits ,buffer (+ index 1)))
                                    (data-page (%vector-raw-bits ,buffer (+ index 2)))
                                    (addr      (%vector-raw-bits ,buffer (+ index 3)))
                                    (npages    (%vector-raw-bits ,buffer (+ index 4))))
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

;;; Given a native SBCL '.core' file, or one attached to the end of an executable,
;;; separate it into pieces.
;;; ASM-PATHNAME is the name of the assembler file that will hold all the Lisp code.
;;; The other two output pathnames are implicit: "x.s" -> "x.core" and "x-core.o"
;;; The ".core" file is a native core file used for starting a binary that
;;; contains the asm code using the "--core" argument.  The "-core.o" file
;;; is for linking in to a binary that needs no "--core" argument.
(defun split-core
    (input-pathname asm-pathname
     &key emit-sizes (verbose nil)
     &aux (split-core-pathname
           (merge-pathnames (make-pathname :type "core") asm-pathname))
          (elf-core-pathname
           (merge-pathnames
            (make-pathname :name (concatenate 'string (pathname-name asm-pathname) "-core")
                           :type "o")
            asm-pathname))
          (core-header (make-array +backend-page-bytes+ :element-type '(unsigned-byte 8)))
          (original-total-npages 0)
          (core-offset 0)
          (page-adjust 0)
          (code-start-fixup-ofs 0) ; where to fixup the core header
          (spaces)
          (copy-actions)
          (fixedobj-range) ; = (START . SIZE-IN-BYTES)
          (relocs (make-array 100000 :adjustable t :fill-pointer 0)))

  ;; Remove old files
  (ignore-errors (delete-file asm-pathname))
  (ignore-errors (delete-file split-core-pathname))
  (ignore-errors (delete-file elf-core-pathname))
  ;; Ensure that all files can be opened
  (with-open-file (input input-pathname :element-type '(unsigned-byte 8))
    (with-open-file (asm-file asm-pathname :direction :output :if-exists :supersede)
      (with-open-file (split-core split-core-pathname :direction :output
                                  :element-type '(unsigned-byte 8) :if-exists :supersede)
        (setq core-offset (read-core-header input core-header verbose))
        (do-core-header-entry ((id len ptr) core-header)
          (case id
            (#.build-id-core-entry-type-code
             (when verbose
               (let ((string (make-string (%vector-raw-bits core-header ptr)
                                          :element-type 'base-char)))
                 (%byte-blt core-header (* (1+ ptr) n-word-bytes) string 0 (length string))
                 (format t "Build ID [~a]~%" string))))
            (#.directory-core-entry-type-code
             (do-directory-entry ((index ptr len) core-header)
               (incf original-total-npages npages)
               (push (make-space id addr data-page page-adjust nwords) spaces)
               (when verbose
                 (format t "id=~d page=~5x + ~5x addr=~10x words=~8x~:[~; (drop)~]~%"
                         id data-page npages addr nwords
                         (= id immobile-varyobj-core-space-id)))
               (cond ((= id immobile-varyobj-core-space-id)
                      (setq code-start-fixup-ofs (+ index 3))
                      ;; Keep this entry but delete the page count. We need to know
                      ;; where the space was supposed to be mapped and at what size.
                      ;; Subsequent core entries will need to adjust their start page
                      ;; downward (just the PTEs's start page now).
                      (setq page-adjust npages data-page 0 npages 0))
                     (t
                      ;; Keep track of where the fixedobj space wants to be.
                      (when (= id immobile-fixedobj-core-space-id)
                        (setq fixedobj-range (cons addr (ash nwords word-shift))))
                      (when (plusp npages) ; enqueue
                        (push (cons data-page (* npages +backend-page-bytes+))
                              copy-actions))
                      ;; adjust this entry's start page in the new core
                      (decf data-page page-adjust)))))
            (#.page-table-core-entry-type-code
             (aver (= len 3))
             (symbol-macrolet ((nbytes (%vector-raw-bits core-header (1+ ptr)))
                               (data-page (%vector-raw-bits core-header (+ ptr 2))))
               (aver (= data-page original-total-npages))
               (aver (= (ceiling (space-nwords
                                  (find dynamic-core-space-id spaces :key #'space-id))
                                 (/ +backend-page-bytes+ n-word-bytes))
                        (%vector-raw-bits core-header ptr))) ; number of PTEs
               (when verbose
                 (format t "PTE: page=~5x~40tbytes=~8x~%" data-page nbytes))
               (push (cons data-page nbytes) copy-actions)
               (decf data-page page-adjust)))))
        (let ((buffer (make-array +backend-page-bytes+
                                  :element-type '(unsigned-byte 8)))
              (filepos))
          ;; Write the new core file
          (write-sequence core-header split-core)
          (dolist (action (reverse copy-actions)) ; nondestructive
            ;; page index convention assumes absence of core header.
            ;; i.e. data page 0 is the file page immediately following the core header
            (let ((offset (* (1+ (car action)) +backend-page-bytes+))
                  (nbytes (cdr action)))
              (when verbose
                (format t "File offset ~10x: ~10x bytes~%" offset nbytes))
              (setq filepos (+ core-offset offset))
              (file-position input filepos)
              (copy-bytes input split-core nbytes buffer)))
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
            (write-sequence buffer split-core :end nbytes)
            (finish-output split-core))
          ;; Sanity test
          (aver (= (+ core-offset
                      (* page-adjust +backend-page-bytes+)
                      (file-length split-core))
                   (file-length input)))
          ;; Seek back to the PTE pages so they can be copied to the '.o' file
          (file-position input filepos)))

      ;; Map the original core file to memory
      (with-mapped-core (sap core-offset original-total-npages input)
        (let* ((data-spaces
                (delete immobile-varyobj-core-space-id (reverse spaces)
                        :key #'space-id))
               (map (cons sap (sort (copy-list spaces) #'> :key #'space-addr)))
               (pte-nbytes (cdar copy-actions)))
          (collect-relocations map relocs)
          (with-open-file (output elf-core-pathname
                                  :direction :output :if-exists :supersede
                                  :element-type '(unsigned-byte 8))
            (vector-push-extend
             `(,(ash code-start-fixup-ofs word-shift) 0 . ,R_X86_64_64) relocs)
            (prepare-elf (+ (apply #'+ (mapcar #'space-nbytes-aligned data-spaces))
                            +backend-page-bytes+ ; core header
                            pte-nbytes)
                         relocs output)
            (setf (%vector-raw-bits core-header code-start-fixup-ofs) 0)
            (write-sequence core-header output) ; Copy prepared header
            (force-output output)
            ;; Change SB-C::*COMPILE-FILE-TO-MEMORY-SPACE* to :DYNAMIC
            ;; and SB-C::*COMPILE-TO-MEMORY-SPACE* to :AUTO
            ;; in case the resulting executable needs to compile anything.
            ;; (Call frame info will be missing, but at least it's something.)
            (dolist (item '(("*COMPILE-FILE-TO-MEMORY-SPACE*" . "DYNAMIC")
                            ("*COMPILE-TO-MEMORY-SPACE*" . "AUTO")))
              (destructuring-bind (symbol . value) item
                (%set-symbol-global-value
                 (find-target-symbol "SB-C" symbol map)
                 (find-target-symbol "KEYWORD" value map :logical))))
            ;;
            (dolist (space data-spaces) ; Copy pages from memory
              (let ((start (space-physaddr space map))
                    (size (space-nbytes-aligned space)))
                (aver (eql (sb-unix:unix-write (sb-sys:fd-stream-fd output)
                                               start 0 size)
                           size))))
            (when verbose
              (format t "Copying ~d bytes (#x~x) from ptes = ~d PTEs~%"
                      pte-nbytes pte-nbytes (floor pte-nbytes 10)))
            (copy-bytes input output pte-nbytes)) ; Copy PTEs from input
          (let ((core (write-assembler-text map asm-file emit-sizes))
                (emit-all-c-symbols t))
            ;; There's no relation between emit-sizes and which section to put
            ;; C symbol references in, however it's a safe bet that if sizes
            ;; are supported then so is the .rodata directive.
            (format asm-file (if emit-sizes "~% .rodata~%" "~% .data~%"))
            (format asm-file " .globl ~A~%~:*~A:
 .quad ~d # ct~%"
                    "__lisp_linkage_values"
                    (length (core-linkage-symbols core)))
            ;; -1 (not a plausible function address) signifies that word
            ;; following it is a data, not text, reference.
            (loop for s across (core-linkage-symbols core)
                  for bit across (core-linkage-symbol-usedp core)
                  when (or emit-all-c-symbols (eql bit 0))
                  do (format asm-file " .quad ~:[~;-1, ~]~a~%"
                             (consp s)
                             (if (consp s) (car s) s))))))

      (format asm-file "~% ~A~%" +noexec-stack-note+))))

;;; Copy the input core into an ELF section without splitting into code & data.
;;; Also force a linker reference to each C symbol that the Lisp core mentions.
(defun copy-to-elf-obj (input-pathname output-pathname)
  ;; Remove old files
  (ignore-errors (delete-file output-pathname))
  ;; Ensure that all files can be opened
  (with-open-file (input input-pathname :element-type '(unsigned-byte 8))
    (with-open-file (output output-pathname :direction :output
                            :element-type '(unsigned-byte 8) :if-exists :supersede)
      (let* ((core-header (make-array +backend-page-bytes+
                                      :element-type '(unsigned-byte 8)))
             (core-offset (read-core-header input core-header nil))
             (spaces)
             (total-npages 0) ; excluding core header page
             (core-size 0))
        (do-core-header-entry ((id len ptr) core-header)
          (case id
            (#.directory-core-entry-type-code
             (do-directory-entry ((index ptr len) core-header)
               (incf total-npages npages)
               (when (plusp nwords)
                 (push (make-space id addr data-page 0 nwords) spaces))))
            (#.page-table-core-entry-type-code
             (aver (= len 3))
             (symbol-macrolet ((nbytes (%vector-raw-bits core-header (1+ ptr)))
                               (data-page (%vector-raw-bits core-header (+ ptr 2))))
               (aver (= data-page total-npages))
               (setq core-size (+ (* total-npages +backend-page-bytes+) nbytes))))))
        (incf core-size +backend-page-bytes+) ; add in core header page
        ;; Map the core file to memory
        (with-mapped-core (sap core-offset total-npages input)
          (let* ((spaces (cons sap (sort (copy-list spaces) #'> :key #'space-addr)))
                 (core (make-core spaces
                                  (space-bounds immobile-varyobj-core-space-id spaces)
                                  (space-bounds immobile-fixedobj-core-space-id spaces)))
                 (c-symbols (map 'list (lambda (x) (if (consp x) (car x) x))
                                 (core-linkage-symbols core)))
                 (sections `#((:str  ".strtab"         ,+sht-strtab+   0 0 0 1  0)
                              (:sym  ".symtab"         ,+sht-symtab+   0 1 1 8 ,sym-entry-size)
                              ;;             section with the strings -- ^ ^ -- 1+ highest local symbol
                              (:core "lisp.core"       ,+sht-progbits+ 0 0 0 ,core-align 0)
                              (:note ".note.GNU-stack" ,+sht-null+     0 0 0 1  0)))
                 (string-table (string-table (append (map 'list #'second sections)
                                                     c-symbols)))
                 (packed-strings (cdr string-table))
                 (strings-start (+ ehdr-size (* (1+ (length sections)) shdr-size)))
                 (strings-end (+ strings-start (length packed-strings)))
                 (symbols-start (align-up strings-end 8))
                 (symbols-size (* (1+ (length c-symbols)) sym-entry-size))
                 (symbols-end (+ symbols-start symbols-size))
                 (core-start (align-up symbols-end 4096)))
            (write-elf-header ehdr-size sections output)
            (write-section-headers `((,strings-start . ,(length packed-strings))
                                     (,symbols-start . ,symbols-size)
                                     (,core-start    . ,core-size)
                                     (0 . 0))
                                   sections string-table output)
            (write-sequence packed-strings output)
            ;; Write symbol table
            (file-position output symbols-start)
            (write-sequence (make-elf64-sym 0 0) output)
            (dolist (sym c-symbols)
              (let ((name-ptr (cdr (assoc sym (car string-table)))))
                (write-sequence (make-elf64-sym name-ptr #x10) output)))
            ;; Copy core
            (file-position output core-start)
            (file-position input core-offset)
            (let ((remaining core-size))
              (loop (let ((n (read-sequence core-header input
                                            :end (min +backend-page-bytes+ remaining))))
                      (write-sequence core-header output :end n)
                      (unless (plusp (decf remaining n)) (return))))
              (aver (zerop remaining)))))))))

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
        ((string= (car args) "copy")
         (apply #'copy-to-elf-obj (cdr args)))
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
