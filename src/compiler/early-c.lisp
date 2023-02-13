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
;;; but it's surely not as ridiculous as MOST-POSITIVE-FIXNUM.
(defconstant call-arguments-limit
  #+x86-64 (ash 1 30)
  #-x86-64 most-positive-fixnum
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
  (similar-constants (sb-fasl::make-similarity-table) :read-only t :type hash-table))
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
;;; :pre-ir2-optimize, :symbolic-asm.
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

(defstruct (debug-name-marker (:print-function print-debug-name-marker)
                              (:copier nil)))
(declaim (freeze-type debug-name-marker))

(defvar *debug-name-level* 4)
(defvar *debug-name-length* 12)
(defvar *debug-name-punt*)
(define-load-time-global *debug-name-sharp* (make-debug-name-marker))
(define-load-time-global *debug-name-ellipsis* (make-debug-name-marker))

(defun print-debug-name-marker (marker stream level)
  (declare (ignore level))
  (cond ((eq marker *debug-name-sharp*)
         (write-char #\# stream))
        ((eq marker *debug-name-ellipsis*)
         (write-string "..." stream))
        (t
         (write-string "???" stream))))

(declaim (ftype (sfunction () list) name-context))
(defun debug-name (type thing &optional context)
  (let ((*debug-name-punt* nil))
    (labels ((walk (x)
               (typecase x
                 (cons
                  (if (plusp *debug-name-level*)
                      (let ((*debug-name-level* (1- *debug-name-level*)))
                        (do ((tail (cdr x) (cdr tail))
                             (name (cons (walk (car x)) nil)
                                   (cons (walk (car tail)) name))
                             (n (1- *debug-name-length*) (1- n)))
                            ((or (not (consp tail))
                                 (not (plusp n))
                                 *debug-name-punt*)
                             (cond (*debug-name-punt*
                                    (setf *debug-name-punt* nil)
                                    (nreverse name))
                                   ((atom tail)
                                    (nconc (nreverse name) (walk tail)))
                                   (t
                                    (setf *debug-name-punt* t)
                                    (nconc (nreverse name) (list *debug-name-ellipsis*)))))))
                      *debug-name-sharp*))
                 ((or symbol number string)
                  x)
                 (t
                  ;; wtf?? This looks like a source of sensitivity to the cross-compiler host
                  ;; in addition to which it seems generally a stupid idea.
                  (type-of x)))))
      (let ((name (list* type (walk thing) (when context (name-context)))))
        (when (legal-fun-name-p name)
          (bug "~S is a legal function name, and cannot be used as a ~
                debug name." name))
        name))))

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

;;; Used as the CDR of the code coverage instrumentation records
;;; (instead of NIL) to ensure that any well-behaving user code will
;;; not have constants EQUAL to that record. This avoids problems with
;;; the records getting coalesced with non-record conses, which then
;;; get mutated when the instrumentation runs. Note that it's
;;; important for multiple records for the same location to be
;;; coalesced. -- JES, 2008-01-02
(defconstant +code-coverage-unmarked+ '%code-coverage-unmarked%)

;;; Stores the code coverage instrumentation results.
;;; The CAR is a hashtable. The CDR is a list of weak pointers to code objects
;;; having coverage marks embedded in the unboxed constants.
;;; Keys in the hashtable are namestrings, the
;;; value is a list of (CONS PATH STATE), where STATE is +CODE-COVERAGE-UNMARKED+
;;; for a path that has not been visited, and T for one that has.
#-sb-xc-host
(progn
  (define-load-time-global *code-coverage-info*
    (list (make-hash-table :test 'equal :synchronized t)))
  (declaim (type (cons hash-table) *code-coverage-info*)))

(deftype id-array ()
  '(and (array t (*))
        ;; Might as well be as specific as we can.
        ;; Really it should be (satisfies array-has-fill-pointer-p)
        ;; but that predicate is not total (errors on NIL).
        ;; And who knows what the host considers "simple".
        #-sb-xc-host (not simple-array)))

(defstruct (compilation (:copier nil)
                        (:predicate nil)
                        (:conc-name ""))
  (fun-names-in-this-file)
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
  ;; Bidrectional map between IR1/IR2/assembler abstractions and a corresponding
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
