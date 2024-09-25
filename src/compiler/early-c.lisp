;;;; This file contains compiler code and compiler-related stuff which
;;;; can be built early on. Some of the stuff may be here because it's
;;;; needed early on, some other stuff (e.g. constants) just because
;;;; it might as well be done early so we don't have to think about
;;;; whether it's done early enough.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;; ANSI limits on compilation
;;; On AMD64 we prefer to use the ECX (not RCX) register,
;;; which means that there can only be 32 bits of precision,
;;; so accounting for the fixnum tag and 1 bit for the sign,
;;; this leaves 30 bits. Of course this number is ridiculous
;;; as a call with that many args would consume 8 GB of stack,
;;; but it's surely not as ridiculous as ARRAY-DIMENSION-LIMIT.
(defconstant call-arguments-limit
  #+x86-64 (ash 1 30)
  #-x86-64 array-dimension-limit
  "The exclusive upper bound on the number of arguments which may be passed
  to a function, including &REST args.")
(defconstant lambda-parameters-limit call-arguments-limit
  "The exclusive upper bound on the number of parameters which may be specified
  in a given lambda list. This is actually the limit on required and &OPTIONAL
  parameters. With &KEY and &AUX you can get more.")
(defconstant multiple-values-limit call-arguments-limit
  "The exclusive upper bound on the number of multiple VALUES that you can
  return.")


;;;; miscellaneous types used both in the cross-compiler and on the target

(defstruct (dxable-args (:constructor make-dxable-args (list))
                        (:predicate nil)
                        (:copier nil))
  (list nil :read-only t))
(defstruct (inlining-data (:include dxable-args)
                          (:constructor make-inlining-data (expansion list))
                          (:predicate nil)
                          (:copier nil))
  (expansion nil :read-only t))
(declaim (freeze-type dxable-args))

(defstruct (ir1-namespace (:conc-name "") (:copier nil) (:predicate nil))
  ;; FREE-VARS translates from the names of variables referenced
  ;; globally to the LEAF structures for them. FREE-FUNS is like
  ;; FREE-VARS, only it deals with function names.
  ;;
  ;; We must preserve the property that a proclamation for a global
  ;; thing only affects the code after it. This takes some work, since
  ;; a proclamation may appear in the middle of a block being
  ;; compiled. If there are references before the proclaim, then we
  ;; copy the current entry before modifying it. Code converted before
  ;; the proclaim sees the old Leaf, while code after it sees the new
  ;; LEAF.
  (free-vars (make-hash-table :test 'eq) :read-only t :type hash-table)
  (free-funs (make-hash-table :test #'equal) :read-only t :type hash-table)
  ;; We use the same CONSTANT structure to represent all EQL anonymous
  ;; constants. This hashtable translates from constants to the LEAFs
  ;; that represent them.
  (eql-constants (make-hash-table :test 'eql) :read-only t :type hash-table)
  ;; During file compilation we are allowed to coalesce similar
  ;; constants. This coalescing is distinct from the coalescing done
  ;; in the dumper, since the effect here is to reduce the number of
  ;; boxed constants appearing in a code component.
  (similar-constants (make-similarity-table) :read-only t :type hash-table))
(declaim (freeze-type ir1-namespace))

(sb-impl::define-thread-local *ir1-namespace*)
(declaim (type ir1-namespace *ir1-namespace*))

;;; *ALLOW-INSTRUMENTING* controls whether we should allow the
;;; insertion of instrumenting code (like a (CATCH ...)) around code
;;; to allow the debugger RETURN and STEP commands to function (we
;;; disallow it for internal stuff).
(defvar *allow-instrumenting*)

;;; miscellaneous forward declarations
(defvar *component-being-compiled*)
(defvar *compiler-error-context*)
;;; Bind this to a stream to capture various internal debugging output.
(defvar *compiler-trace-output* nil)
;;; These are the default, but the list can also include
;;; :pre-ir2-optimize, :constraints.
(defvar *compile-trace-targets* '(:ir1 :ir2 :vop :symbolic-asm :disassemble))
(defvar *current-path*)
(defvar *current-component*)
(defvar *elsewhere-label*)
(defvar *source-info*)
(defvar *source-plist*)
(defvar *source-namestring*)

(defvar *handled-conditions* nil)
(defvar *disabled-package-locks* nil)


;;;; miscellaneous utilities

;;; COMPILE-FILE usually puts all nontoplevel code in immobile space, but COMPILE
;;; offers a choice. Because the immobile space GC does not run often enough (yet),
;;; COMPILE usually places code in the dynamic space managed by our copying GC.
;;; Change this variable if your application always demands immobile code.
;;; In particular, ELF cores shrink the immobile code space down to just enough
;;; to contain all code, plus about 1/2 MiB of spare, which means that you can't
;;; subsequently compile a whole lot into immobile space.
;;; The value is changed to :AUTO in make-target-2-load.lisp which suppresses
;;; codegen optimizations for immobile space, but nonetheless prefers to allocate
;;; the code there, falling back to dynamic space if there is no room left.
;;; These controls exist whether or not the immobile-space feature is present.
(declaim (type (member :immobile :dynamic :auto) *compile-to-memory-space*)
         (type (member :immobile :dynamic) *compile-file-to-memory-space*))
(defvar *compile-to-memory-space* :immobile) ; BUILD-TIME default
(export '*compile-file-to-memory-space*) ; silly user code looks at, even if no immobile-space
(defvar *compile-file-to-memory-space* :immobile) ; BUILD-TIME default

(defun compile-perfect-hash (lambda test-inputs)
  ;; Don't blindly trust the hash generator: assert that computed values are
  ;; in range and not repeated.
  (let ((seen (make-array (power-of-two-ceiling (length test-inputs))
                          :element-type 'bit :initial-element 0))
        (f #-sb-xc-host ; use fasteval if possible
           (cond #+sb-fasteval
                 ((< (length test-inputs) 100) ; interpreting is faster
                  (let ((*evaluator-mode* :interpret))
                    (eval lambda)))
                 (t (let ((*compile-to-memory-space* :dynamic))
                      (compile nil lambda))))
           #+sb-xc-host
           (destructuring-bind (head lambda-list . body) lambda
             (aver (eq head 'lambda))
             (multiple-value-bind (forms decls) (parse-body body nil)
               (declare (ignore decls))
               ;; Give the host a definition for SB-C::UINT32-MODULARLY and remove _all_
               ;; OPTIMIZE decls hidden within. We don't need to pedantically correctly
               ;; code-walk here, because hash expressions are largely boilerplate that
               ;; will not confusingly match forms such as (LET ((OPTIMIZE ...))).
               (let ((new-body
                      `(macrolet ((uint32-modularly (&whole form &rest exprs)
                                    (declare (ignore exprs))
                                    (funcall (sb-xc:macro-function 'sb-c::uint32-modularly)
                                             form nil)))
                         ,@(subst-if '(optimize)
                                     (lambda (x) (typep x '(cons (eql optimize) (not null))))
                                     forms))))
                 (compile nil `(lambda ,lambda-list ,new-body)))))))
    (loop for input across test-inputs
          do (let ((h (funcall f input)))
               (unless (zerop (bit seen h))
                 (bug "Perfect hash generator failed on ~X" test-inputs))
               (setf (bit seen h) 1)))
    f))

(declaim (ftype (sfunction () list) name-context))
(defun debug-name (type thing &optional context)
  (let ((name (list* type thing (when context (name-context)))))
    (when (legal-fun-name-p name)
      (bug "~S is a legal function name, and cannot be used as a ~
            debug name." name))
    name))

;;; Bound during eval-when :compile-time evaluation.
(defvar *compile-time-eval* nil)
(declaim (always-bound *compile-time-eval*))

#-immobile-code (defmacro code-immobile-p (thing) `(progn ,thing nil))
#-sb-xc-host ; not needed for make-hlst-1
(defmacro maybe-with-system-tlab ((source-object) allocator)
  (declare (ignorable source-object))
  #+system-tlabs `(if (sb-vm::force-to-heap-p ,source-object)
                      (locally (declare (sb-c::tlab :system)) ,allocator)
                      ,allocator)
  #-system-tlabs allocator)
;;; TLAB selection is an aspect of a POLICY but this sets the global choice
(defvar *force-system-tlab* nil)

;;; The allocation quantum for boxed code header words.
;;; 2 implies an even length boxed header; 1 implies no restriction.
(defconstant code-boxed-words-align (+ 2 #+(or x86 x86-64) -1))

;;; Unique number assigned into high 4 bytes of 64-bit code size slot
;;; so that we can sort the contents of text space in a more-or-less
;;; predictable manner based on the order in which code was loaded.
;;; This wraps around at 32 bits, but it's still deterministic.
(define-load-time-global *code-serialno* 0)
(declaim (fixnum *code-serialno*))

(deftype id-array ()
  '(and (array t (*))
        ;; Might as well be as specific as we can.
        ;; Really it should be (satisfies array-has-fill-pointer-p)
        ;; but that predicate is not total (errors on NIL).
        ;; And who knows what the host considers "simple".
        #-sb-xc-host (not simple-array)))

(defun make-fun-name-hashset ()
  (make-hashset 32
                (lambda (a b) (or (eq a b) (and (consp a) (consp b) (equal a b))))
                ;; We don't emulate sb-xc:sxhash thoroughly enough to hash compound names
                ;; (lists are rejected) but it doesn't actually matter what the hash is
                ;; for duplicate name detection.
                #+sb-xc-host #'cl:sxhash
                #-sb-xc-host #'sxhash))

(defstruct (compilation (:constructor make-compilation
                                      (&key coverage-metadata msan-unpoison
                                       block-compile entry-points compile-toplevel-object))
                        (:copier nil)
                        (:predicate nil)
                        (:conc-name ""))
  (fun-names-in-this-file (make-fun-name-hashset))
  ;; for constant coalescing across code components, and/or for situations
  ;; where SIMILARP does not do what you want.
  (constant-cache)
  ;; When compiling within the extent of *macro-policy* we have to store up
  ;; any DECLAIMs for later replay. The logic is explained in EVAL-COMPILE-TLF.
  ;; This slot is set to NIL before use and reset when done.
  (saved-optimize-decls :none)
  (coverage-metadata nil :type (or (cons hash-table hash-table) null) :read-only t)
  (msan-unpoison nil :read-only t)
  (sset-counter 1 :type fixnum)
  ;; if emitting a cfasl, the fasl stream to that
  (compile-toplevel-object nil :read-only t)
  ;; The current block compilation state.  These are initialized to
  ;; the :Block-Compile and :Entry-Points arguments that COMPILE-FILE
  ;; was called with.  Subsequent START-BLOCK or END-BLOCK
  ;; declarations alter the values.
  (block-compile nil :type (member nil t :specified))
  (entry-points nil :type list)
  ;; When block compiling, used by PROCESS-FORM to accumulate top
  ;; level lambdas resulting from compiling subforms. (In reverse
  ;; order.)
  (toplevel-lambdas nil :type list)
  ;; We build a list of top-level lambdas, and then periodically smash them
  ;; together into a single component and compile it.
  (pending-toplevel-lambdas nil :type list)
  ;; We record whether the package environment has changed during the
  ;; compilation of some sequence top level forms. This allows the
  ;; compiler to dump symbols in such a way that the loader can
  ;; reconstruct them in the correct package.
  (package-environment-changed nil :type boolean)
  ;; Bidirectional map between IR1/IR2/assembler abstractions and a corresponding
  ;; small integer or string identifier. One direction could be done by adding
  ;; the ID as slot to each object, but we want both directions.
  ;; These could just as well be scoped by WITH-IR1-NAMESPACE, but
  ;; since it's primarily a debugging tool, it's nicer to have
  ;; a wider unique scope by ID.
  (objmap-obj-to-id      (make-hash-table :test 'eq) :read-only t)
  (objmap-id-to-node     nil :type (or null id-array)) ; number -> NODE
  (objmap-id-to-comp     nil :type (or null id-array)) ; number -> COMPONENT
  (objmap-id-to-leaf     nil :type (or null id-array)) ; number -> LEAF
  (objmap-id-to-cont     nil :type (or null id-array)) ; number -> CTRAN or LVAR
  (objmap-id-to-ir2block nil :type (or null id-array)) ; number -> IR2-BLOCK
  (objmap-id-to-tn       nil :type (or null id-array)) ; number -> TN
  (objmap-id-to-label    nil :type (or null id-array)) ; number -> LABEL
  deleted-source-paths)
(declaim (freeze-type compilation))

(sb-impl::define-thread-local *compilation*)
(declaim (type compilation *compilation*))

;; from 'llvm/projects/compiler-rt/lib/msan/msan.h':
;;  "#define MEM_TO_SHADOW(mem) (((uptr)(mem)) ^ 0x500000000000ULL)"
#+linux ; shadow space differs by OS
(defconstant sb-vm::msan-mem-to-shadow-xor-const #x500000000000)

(defstruct (compilation-unit (:conc-name cu-) (:predicate nil) (:copier nil)
                             (:constructor make-compilation-unit ()))
  ;; Count of the number of compilation units dynamically enclosed by
  ;; the current active WITH-COMPILATION-UNIT that were unwound out of.
  (aborted-count 0 :type fixnum)
  ;; Keep track of how many times each kind of condition happens.
  (error-count 0 :type fixnum)
  (warning-count 0 :type fixnum)
  (style-warning-count 0 :type fixnum)
  (note-count 0 :type fixnum)
  ;; Map of function name -> something about how many calls were converted
  ;; as ordinary calls not in the scope of a local or global notinline declaration.
  ;; Useful for finding functions that were supposed to have been converted
  ;; through some kind of transformation but were not.
  (emitted-full-calls (make-hash-table :test 'equal))
  ;; hash-table of hash-tables:
  ;;  outer: GF-Name -> hash-table
  ;;  inner: (qualifiers . specializers) -> lambda-list
  (methods nil :type (or null hash-table)))
;;; This is a COMPILATION-UNIT if we are within a WITH-COMPILATION-UNIT form (which
;;; normally causes nested uses to be no-ops).
(defvar *compilation-unit* nil)

(defmacro get-emitted-full-calls (name)
  `(awhen *compilation-unit* (gethash ,name (cu-emitted-full-calls it))))

;; Return the number of calls to NAME that IR2 emitted as full calls,
;; not counting calls via #'F that went untracked.
;; Return 0 if the answer is nonzero but a warning was already signaled
;; about any full calls were emitted. This return convention satisfies the
;; intended use of this statistic - to decide whether to generate a warning
;; about failure to inline NAME, which is shown at most once per name
;; to avoid unleashing a flood of identical warnings.
(defun emitted-full-call-count (name)
  (let ((status (get-emitted-full-calls name)))
    (and (integerp status)
         ;; Bit 0 tells whether any call was NOT in the presence of
         ;; a 'notinline' declaration, thus eligible to be inline.
         ;; Bit 1 tells whether any warning was emitted yet.
         (= (logand status 3) #b01)
         (ash status -2)))) ; the call count as tracked by IR2

;;; FIXME: the math here is very suspicious-looking. If the flag bits are #b11
;;; then they can rollover into the counts. And we're ORing counts. Wtf is this doing???
;;; Well, it doesn't matter really. This is used only in FOP-NOTE-FULL-CALLS which is called
;;; only if DUMP-EMITTED-FULL-CALLS emits that fop. But that function is never invoked.
(defun accumulate-full-calls (data)
  (loop for (name status) in data
        do
        (let* ((table (cu-emitted-full-calls *compilation-unit*))
               (existing (gethash name table 0)))
          (setf (gethash name table)
                (logior (+ (logand existing #b11) ; old flag bits
                           (logand status #b11))  ; new flag bits
                        (logand existing -4)      ; old count
                        (logand status -4))))))   ; new count

(declaim (type (simple-array (unsigned-byte 16) 1) *asm-routine-offsets*))
(define-load-time-global *asm-routine-offsets*
  (make-array 0 :element-type '(unsigned-byte 16)))
