;;; Do warm init without compiling files.

;;; Get back to a reasonable state where all of the compiler works,
;;; as does SB-VM:HEXDUMP and MAP-ALLOCATED-OBJECTS, etc.
;;; before trying to define any more functions.
(defvar *compile-files-p* nil)
(load (merge-pathnames "src/cold/warm.lisp" *load-pathname*))

;; sb-xref-for-internals is actively harmful to tree-shaking.
;; Remove some symbols to make the hide-packages test pass.
#+(and sb-xref-for-internals (not sb-devel))
(progn
  (fmakunbound 'sb-kernel::type-class-fun-slot)
  (fmakunbound 'sb-kernel::new-ctype))

(sb-impl::!recompile-globaldb-checkfuns)

;;; Users don't want to know if there are multiple TLABs per se, but they do want
;;; to know if NEW-ARENA returns an arena, so give them a sensible feature name.
#+system-tlabs (push :arena-allocator *features*)

;;; Remove symbols from CL:*FEATURES* that should not be exposed to users.
(export 'sb-impl::+internal-features+ 'sb-impl)
(let* (#-sb-devel
       (non-target-features
        ;;
        ;; FIXME: I suspect that this list should be changed to its inverse-
        ;; features that _SHOULD_ go into SB-IMPL:+INTERNAL-FEATURES+ and
        ;; comments about the reasoning behind each, rather than features to
        ;; discard and reasons they're not needed. The default assumption should be
        ;; to drop any build-time feature that lacks a rationale to preserve it.
        ;;
        ;; READ-FROM-STRING prevents making references to
        ;; all these keywords from the source form itself.
        (read-from-string "
         (;; :SB-AFTER-XC-CORE is essentially an option flag to make-host-2
          :SB-AFTER-XC-CORE
          ;; CONS-PROFILING sets the initial compiler policy which persists
          ;; into the default baseline policy. It has no relevance post-build
          ;; in as much as policy can be changed later arbitrarily.
          :CONS-PROFILING
          ;; Used by nothing after compiling 'target-thread.lisp' or 'thread.c'
          :SB-FUTEX :OS-THREAD-STACK
          ;; These affect the BREAK instruction emitter, but the C code is able
          ;; to handle anything, and post-build we don't care which it is.
          :UD2-BREAKPOINTS :INT4-BREAKPOINTS
          ;; Uses of OS-PROVIDES-DLOPEN and -DLADDR are confined to src/code/foreign.lisp
          :OS-PROVIDES-DLOPEN :OS-PROVIDES-DLADDR
          ;; more-or-less confined to serve-event, except for a test which now
          ;; detects whether COMPUTE-POLLFDS is defined and therefore testable.
          :OS-PROVIDES-POLL
          ;; Silly feature we add in because additive features are more sensible
          ;; than subtractive ones. So you opt out, not in, to having *LOAD-TRUENAME*
          ;; eagerly bound.
          :ANSI-COMPLIANT-LOAD-TRUENAME
          ;; The final batch of symbols is strictly for C. The LISP_FEATURE_
          ;; prefix on the corresponding #define is unfortunate.
          :GCC-TLS :USE-SYS-MMAP
          ;;; Enforce using of posix semaphores on Darwin instead of dispatch.
          :USE-DARWIN-POSIX-SEMAPHORES
          ;; only for 'src/runtime/wrap.h'
          :OS-PROVIDES-BLKSIZE-T
          ;; only for src/runtime/run-program.c
          :OS-PROVIDES-CLOSE-RANGE-WRAPPER)"))
       (public-features
        (cons
         sb-impl::!sbcl-architecture
         (read-from-string "
          (:COMMON-LISP :SBCL :ANSI-CL :IEEE-FLOATING-POINT
           :64-BIT ; choice of word size. 32-bit if absent
           :BIG-ENDIAN :LITTLE-ENDIAN ; endianness: pick one and only one
           :BSD :UNIX :LINUX :WIN32 :DARWIN :SUNOS :ANDROID ; OS: pick one or more
           :FREEBSD :GNU-KFREEBSD :OPENBSD :NETBSD :DRAGONFLY :HAIKU
           :MACH-O :ELF ; obj file format: pick zero or one
           ;; I would argue that this should not be exposed,
           ;; but I would also anticipate blowback from removing it.
           :GENCGC :MARK-REGION-GC ; GC: pick one and only one
           :ARENA-ALLOCATOR :ALLOCATION-SIZE-HISTOGRAM
           ;; Can't use s-l-a-d :compression safely without it
           :SB-CORE-COMPRESSION
           ;; Features that are also in *FEATURES-POTENTIALLY-AFFECTING-FASL-FORMAT*
           ;; and would probably mess up something if made non-public,
           ;; though I don't think they should all be public.
           :MSAN :UBSAN
           :SB-SAFEPOINT
           :SB-THREAD :SB-UNICODE
           ;; Things which (I think) at least one person has requested be kept around
           :SB-LDB
           ;; We keep the :SB-PACKAGE-LOCKS feature despite it no longer
           ;; affecting the build. (It's not a choice any more)
           :SB-PACKAGE-LOCKS
           ;; unsure, I think this is for end-user consumption,
           ;; though every release of SBCL since eons ago has had local nicknames.
           :PACKAGE-LOCAL-NICKNAMES
           ;; Developer mode features. A release build will never have them,
           ;; hence it makes no difference whether they're public or not.
           :SB-DEVEL :SB-DEVEL-LOCK-PACKAGES)")))
       #-sb-devel
       (removable-features
        (append non-target-features public-features)))
  (defconstant sb-impl:+internal-features+
            (remove-if (lambda (x) (member x #+sb-devel public-features
                                             #-sb-devel removable-features))
                       *features*))
  (setq *features* (remove-if-not (lambda (x) (member x public-features))
                                  *features*)))

;;; There's a fair amount of machinery which is needed only at cold
;;; init time, and should be discarded before freezing the final
;;; system. We discard it by uninterning the associated symbols.
;;; Rather than using a special table of symbols to be uninterned,
;;; which might be tedious to maintain, instead we use a hack:
;;; anything whose name matches a magic character pattern is
;;; uninterned.
;;; Additionally, you can specify an arbitrary way to destroy
;;; random bootstrap stuff on per-package basis.
(defun !unintern-init-only-stuff (&aux result)
  (dolist (package (list-all-packages))
    (sb-int:awhen (find-symbol "!REMOVE-BOOTSTRAP-SYMBOLS" package)
      (funcall sb-int:it)))
  (dolist (list sb-int:*!removable-symbols*)
    (let ((package (find-package (car list))))
      (dolist (symbol (cdr list))
        (fmakunbound symbol)
        (unintern symbol package))))
  sb-kernel::
  (flet ((uninternable-p (symbol)
           (let ((name (symbol-name symbol)))
             (or (and (>= (length name) 1) (char= (char name 0) #\!))
                 (and (>= (length name) 2) (string= name "*!" :end1 2))
                 (memq symbol
                       '(sb-c::sb-pcl sb-c::sb-impl sb-c::sb-kernel
                         sb-c::sb-c sb-c::sb-int))))))
    ;; Delete bootstrap-only vops
    (flet ((drop-keys (table)
             (loop for symbol being each hash-key of table
                   when (uninternable-p symbol) do (remhash symbol table))))
      (drop-keys sb-c::*backend-parsed-vops*)
      (drop-keys sb-c::*backend-template-names*))
    ;; A structure constructor name, in particular !MAKE-SAETP,
    ;; can't be uninterned if referenced by a defstruct-description.
    ;; So loop over all structure classoids and clobber any
    ;; symbol that should be uninternable.
    (maphash (lambda (classoid layout)
               (when (structure-classoid-p classoid)
                 (let ((dd (layout-%info layout)))
                   (setf (dd-constructors dd)
                         (delete-if (lambda (x)
                                      (and (consp x) (uninternable-p (car x))))
                                    (dd-constructors dd))))))
             (classoid-subclasses (find-classoid t)))
    ;; PATHNAME is not a structure-classoid
    (setf (sb-kernel:dd-constructors (sb-kernel:find-defstruct-description 'pathname))
          nil)
    ;; Todo: perform one pass, then a full GC, then a final pass to confirm
    ;; it worked. It should be an error if any uninternable symbols remain,
    ;; but at present there are about 7 symbols with referrers.
    (with-package-iterator (iter (list-all-packages) :internal :external)
      (loop (multiple-value-bind (winp symbol accessibility package) (iter)
              (declare (ignore accessibility))
              (unless winp
                (return))
              (when (uninternable-p symbol)
                ;; Uninternable symbols which are referenced by other stuff
                ;; can't disappear from the image, but we don't need to preserve
                ;; their functions, so FMAKUNBOUND them. This doesn't have
                ;; the intended effect if the function shares a code-component
                ;; with non-cold-init lambdas. Though the cold-init function is
                ;; never called post-build, it is not discarded. Also, I suspect
                ;; that the following loop should print nothing, but it does:
                #|
                (sb-vm:map-allocated-objects ;
                (lambda (obj type size) ;
                (declare (ignore size)) ;
                (when (= type sb-vm:code-header-widetag) ;
                (let ((name (sb-c::debug-info-name ;
                (sb-kernel:%code-debug-info obj)))) ;
                (when (and (stringp name) (search "COLD-INIT-FORMS" name)) ;
                (print obj)))))         ;
                :dynamic)               ;
                |#
                (fmakunbound symbol)
                (unintern symbol package))))))
  (sb-int:dohash ((k v) sb-c::*backend-parsed-vops*)
    (declare (ignore k))
    (setf (sb-c::vop-parse-body v) nil))
  ;; Used for inheriting from other VOPs, not needed in the target.
  (setf sb-c::*backend-parsed-vops* (make-hash-table))
  result)


;;; Check for potentially bad format-control strings
(defun scan-format-control-strings ()
  (labels ((possibly-ungood-package-reference (string)
             ;; We want to see nothing SB-package-like at all
             (or (search "sb-" string :test #'char-equal)
                 ;; catch mistakes due to imitating the way things used to be
                 (search "sb!" string :test #'char-equal)))
           (possibly-format-control (string)
             (when (find #\~ string)
               ;; very likely to be a format control if it parses OK.
               ;; Possibly not, but false positives are acceptable.
               (some (lambda (x)
                       (and (typep x 'sb-format::format-directive)
                            (eql (sb-format::directive-character x) #\/)
                            (possibly-ungood-package-reference
                             (subseq string
                                     (sb-format::directive-start x)
                                     (sb-format::directive-end x)))))
                     (ignore-errors
                      (sb-format::%tokenize-control-string
                       string 0 (length string) nil))))))
    (let (wps)
      (sb-vm:map-allocated-objects
       (lambda (obj type size)
         (declare(ignore type size))
         (when (and (stringp obj) (possibly-format-control obj))
           (push (make-weak-pointer obj) wps)))
       :all)
      (when wps
        (dolist (wp wps)
          (sb-int:binding* ((v (weak-pointer-value wp) :exit-if-null))
            (format t "Found string ~S~%" v)))
        (warn "Potential problem with format-control strings.
Please check that all strings which were not recognizable to the compiler
(as the first argument to WARN, etc.) are wrapped in SB-FORMAT:TOKENS"))
      wps)))

;;; If the SB-DOC internal feature is not present, remove any and all
;;; docstrings that snuck in (as can happen with any file compiled in
;;; warm load).
(unless (member :sb-doc sb-impl:+internal-features+)
  (let ((count 0))
    (macrolet ((clear-it (place)
                 `(when ,place
                    ,(if (typep place '(cons (eql sb-int:info)))
                         `(sb-int:clear-info ,@(cdr place))
                         `(setf ,place nil))
                    (incf count))))
      ;; 1. Functions, macros, special operators
      (sb-vm:map-allocated-objects
       (lambda (obj type size)
         (declare (ignore size))
         (case type
          (#.sb-vm:code-header-widetag
           (dotimes (i (sb-kernel:code-n-entries obj))
             (let ((f (sb-kernel:%code-entry-point obj i)))
               (clear-it (sb-kernel:%simple-fun-doc f)))))
          (#.sb-vm:instance-widetag
           (when (typep obj 'class)
             (when (slot-boundp obj 'sb-pcl::%documentation)
               (clear-it (slot-value obj 'sb-pcl::%documentation)))))
          (#.sb-vm:funcallable-instance-widetag
           (when (typep obj 'standard-generic-function)
             (when (slot-boundp obj 'sb-pcl::%documentation)
               (clear-it (slot-value obj 'sb-pcl::%documentation)))))))
       :all)
      ;; 2. Variables, types, and anything else
      (do-all-symbols (s)
        (let ((expander (sb-int:info :setf :expander s)))
          (when (typep expander '(cons t (cons string)))
            (setf (second expander) nil)))
        (dolist (category '(:variable :type :typed-structure))
          (clear-it (sb-int:info category :documentation s)))
        (clear-it (sb-int:info :random-documentation :stuff s))))
    (when (plusp count)
      (format t "~&Removed ~D doc string~:P" count)))
)

#+sb-core-compression
(defun compress-debug-info (code)
  (let ((info (sb-c::%code-debug-info code)))
    (when (typep info 'sb-c::compiled-debug-info)
      (let ((map (sb-c::compiled-debug-info-fun-map info)))
        (when (typep map '(simple-array (unsigned-byte 8) (*)))
          (sb-alien:with-alien ((compress-vector (function int unsigned size-t) :extern))
            (sb-sys:with-pinned-objects (map)
              (sb-alien:alien-funcall compress-vector
                                      (sb-kernel:get-lisp-obj-address map)
                                      (length map)))))))))
(progn
  ;; Remove source forms of compiled-to-memory lambda expressions.
  ;; The disassembler is the major culprit for retention of these,
  ;; but there are others and I don't feel like figuring out where from.
  ;; Globally declaiming EVAL-STORE-SOURCE-FORM 0 would work too,
  ;; but isn't it nice to know that the logic for storing the forms
  ;; actually works? (Yes)
  (sb-vm:map-allocated-objects
   (lambda (obj type size)
     (declare (ignore size))
     (case type
      (#.sb-vm:instance-widetag
       (when (typep obj 'sb-c::core-debug-source)
         (setf (sb-c::core-debug-source-form obj) nil)))
      (#.sb-vm:code-header-widetag
       #+sb-core-compression
       (compress-debug-info obj)
       (dotimes (i (sb-kernel:code-n-entries obj))
         (let ((fun (sb-kernel:%code-entry-point obj i)))
           (when (sb-kernel:%simple-fun-lexpr fun)
             (setf (sb-impl::%simple-fun-source fun)
                   (sb-impl::%simple-fun-doc fun))))))))
   :all)

  ;; Disable the format-control optimizer for ERROR and WARN
  ;; while preserving the argument-checking logic. Technically the optimizer is
  ;; probably ok to leave in, but the spec is ambiguous as to whether
  ;; implicit compile-time transformations on format strings is permitted.
  ;; http://www.lispworks.com/documentation/HyperSpec/Issues/iss170_w.htm
  ;; seems to imply that it is, but I would imagine that users don't expect it.
  (setq sb-c::*optimize-format-strings* nil)

  ;; Fix unknown types in globaldb
  (let ((l nil))
    (do-all-symbols (s)
      (flet ((fixup (kind)
               (multiple-value-bind (type present)
                   (sb-int:info kind :type s)
                 (when (and present
                            (sb-kernel:ctype-p type)
                            (sb-kernel:contains-unknown-type-p type))
                   (setf (sb-int:info kind :type s)
                         (sb-kernel:specifier-type (sb-kernel:type-specifier type)))
                   (push s l)))))
        (fixup :function)
        (fixup :variable)))
    (unless (sb-impl::!c-runtime-noinform-p)
      (let ((*print-pretty* nil)
            (*print-length* nil))
        (format t "~&; Fixed types: ~S~%" (sort l #'string<)))))

  ;; Unintern no-longer-needed stuff before the possible PURIFY in
  ;; SAVE-LISP-AND-DIE.
  #-sb-devel (!unintern-init-only-stuff)


  (do-all-symbols (symbol)
    ;; Don't futz with the header of static symbols.
    ;; Technically LOGIOR-HEADER-BITS can only be used on an OTHER-POINTER-LOWTAG
    ;; objects, so modifying NIL should not ever work, but it's especially wrong
    ;; on ppc64 where OTHER- and LIST- pointer lowtags are 10 bytes apart instead
    ;; of 8, so this was making a random alteration to the header.
    (unless (eq (heap-allocated-p symbol) :static)
      (sb-kernel:logior-header-bits symbol sb-vm::+symbol-initial-core+))

    ;; A symbol whose INFO slot underwent any kind of manipulation
    ;; such that it now has neither properties nor globaldb info,
    ;; can have the slot set back to NIL if it wasn't already.
    (when (and (sb-kernel:symbol-%info symbol) ; "raw" value is something
               ;; but both "cooked" values are empty
               (null (sb-kernel:symbol-dbinfo symbol))
               (null (symbol-plist symbol)))
      (sb-sys:%primitive sb-c:set-slot symbol nil
                         'make-symbol sb-vm:symbol-info-slot sb-vm:other-pointer-lowtag)))
)

(sb-ext:gc :full t)

;;; resetting compilation policy to neutral values in preparation for
;;; SAVE-LISP-AND-DIE as final SBCL core (not in warm.lisp because
;;; SB-C::*POLICY* has file scope)
(setq sb-c::*policy* (copy-structure sb-c::**baseline-policy**))

;;; Adjust READTABLE-BASE-CHAR-PREFERENCE back to the advertised default.
(dolist (rt (list sb-impl::*standard-readtable* *debug-readtable*))
  (setf (readtable-base-char-preference rt) :symbols))
;;; Change the internal constructor's default too.
(let ((dsd sb-kernel::(find 'sb-impl::%readtable-string-preference
                            (dd-slots (find-defstruct-description 'readtable))
                            :key #'dsd-name)))
  (funcall #'(setf slot-value) 'character dsd 'sb-kernel::default))

;;; The system is complete now, all standard functions are
;;; defined.

;;; In case there is xref data for internals, repack it here to
;;; achieve a more compact encoding.
;;;
;;; However, repacking changes
;;; SB-C::**MOST-COMMON-XREF-NAMES-BY-{INDEX,NAME}** thereby changing
;;; the interpretation of xref data written into and loaded from
;;; fasls. Since fasls should be compatible between images originating
;;; from the same SBCL build, REPACK-XREF is of no use after the
;;; target image has been built.
(when (member :sb-xref-for-internals sb-impl:+internal-features+)
  (sb-c::repack-xref :verbose 1))
(fmakunbound 'sb-c::repack-xref)

(load (merge-pathnames "src/code/shaketree" *load-pathname*))
(defun asm-inst-p (symbol)
    ;; Assembler instruction names can't be made external because to do so would
    ;; conflict with common-lisp symbols. Notable examples are PUSH and POP.
    ;; So other criteria must pertain to detecting the important symbols.
    ;; And as we don't need to preserve Lisp macros but do need to retain
    ;; assembler macro instructions, those merit special consideration.
    ;; Additionally, a DEFUN may co-exist with an identically named macro
    ;; instruction. (I'm not happy about it, but that's historical baggage).
    ;; A macro instruction is recognizable to INST by a naming convention
    ;; that is unused for anything else by way of being inconvenient to use -
    ;; a symbol whose print name start with "M:" is a macro instruction.
    (or (get symbol 'sb-disassem::instructions)
        (let ((name (string symbol)))
          (and (> (length name) 2)
               (string= name "M:" :end1 2)))))
(let ((counts
       (mapcar (lambda (x)
                 (list x
                       (sb-impl::package-external-symbol-count x)
                       (sb-impl::package-internal-symbol-count x)))
               (sort (list-all-packages) #'string< :key 'package-name))))
  #-sb-devel
  ;; Remove inline expansions
  (do-symbols (symbol #.(find-package "SB-C"))
    (when (equal (symbol-package symbol) #.(find-package "SB-C"))
      (sb-int:clear-info :function :inlining-data symbol)
      (sb-int:clear-info :function :inlinep symbol)))
  (sb-impl::shake-packages
   ;; Development mode: retain all symbols with any system-related properties
   #+sb-devel
   (lambda (symbol accessibility)
     (declare (ignore accessibility))
     (or (sb-kernel:symbol-%info symbol)
         (sb-kernel:%symbol-function symbol)
         (and (boundp symbol) (not (keywordp symbol)))))
   ;; Release mode: retain all symbols satisfying this intricate test
   #-sb-devel
   (lambda (symbol accessibility)
     (case (symbol-package symbol)
      (#.(find-package "SB-VM")
       (or (eq accessibility :external)
           ;; overapproximate what we need for contribs and tests
           (member symbol `(sb-vm::map-referencing-objects
                            sb-vm::map-stack-references
                            sb-vm::reconstitute-object
                            sb-vm::points-to-arena
                            ;; need this for defining a vop which
                            ;; tests the x86-64 allocation profiler
                            sb-vm::pseudo-atomic
                            ,@(or #+(or x86 x86-64) '(sb-vm::%vector-cas-pair
                                                      sb-vm::%instance-cas-pair
                                                      sb-vm::%cons-cas-pair))
                            ;; Naughty outside-world code uses these.
                            #+x86-64 sb-vm::reg-in-size))
           (let ((s (string symbol))) (and (search "THREAD-" s) (search "-SLOT" s)))
           (search "-OFFSET" (string symbol))
           (search "-TN" (string symbol))))
      (#.(find-package "SB-ALIEN")
       (or (eq accessibility :external) (eq symbol 'sb-alien::alien-callback-p)))
      (#.(mapcar 'find-package
                 '("SB-ASSEM" "SB-BROTHERTREE" "SB-DISASSEM" "SB-FORMAT"
                   "SB-IMPL" "SB-KERNEL" "SB-MOP" "SB-PCL" "SB-PRETTY" "SB-PROFILE"
                   "SB-REGALLOC" "SB-SYS" "SB-UNICODE" "SB-UNIX" "SB-WALKER"))
       ;; Assume all and only external symbols must be retained
         (eq accessibility :external))
      (#.(find-package "SB-C")
       (or (eq accessibility :external)
           (member symbol '(sb-c::tab sb-c::scramble))))
      (#.(find-package "SB-LOOP")
       (or (eq accessibility :external)
           ;; Retain some internals to keep CLSQL working.
           (member symbol '(sb-loop::*loop-epilogue*
                            sb-loop::add-loop-path))))
      (#.(find-package "SB-LOCKLESS")
       (or (eq accessibility :external)
           (member symbol '(sb-lockless::+hash-nbits+)))) ; for a test
      (#.(find-package "SB-THREAD")
       (or (eq accessibility :external)
           ;; for some reason a recent change caused the tree-shaker to drop MAKE-SPINLOCK
           ;; which makes sense. I'm not sure what was rooting the symbol.
           ;; However the :spinlock-api test in threads.impure asserts that spinlock symbols
           ;; exist despite being internal symbols.
           (sb-int:info :function :deprecated symbol)))
      (#.(find-package "SB-FASL")
       ;; Retain +BACKEND-FASL-FILE-IMPLEMENTATION+ and +FASL-FILE-VERSION+
       ;; (and anything else otherwise reachable)
       (and (eq accessibility :external)
            (constantp symbol)))
      (#.(find-package "SB-BIGNUM")
       ;; There are 2 important external symbols for sb-gmp, and 2
       ;; important external symbols for sb-rotate-byte.
       ;; Other externals can disappear.
       (member symbol '(sb-bignum:%allocate-bignum
                        sb-bignum:maximum-bignum-length
                        sb-bignum:bit-index
                        sb-bignum:make-small-bignum)))
      (t
       (if (eq (symbol-package symbol)
               sb-assem::*backend-instruction-set-package*)
           (or (eq accessibility :external) (asm-inst-p symbol))
           ;; By default, retain any symbol with any attachments
           (or (sb-kernel:symbol-%info symbol)
               (sb-kernel:%symbol-function symbol)
               (and (boundp symbol) (not (keywordp symbol))))))))
   :verbose nil :print nil)
  (unintern 'sb-impl::shake-packages 'sb-impl)
  (let ((sum-delta-ext 0)
        (sum-delta-int 0))
    (format t "~&~26TExternal   |    Internal~%")
    (dolist (entry counts)
      (let* ((ext (sb-impl::package-external-symbol-count (car entry)))
             (int (sb-impl::package-internal-symbol-count (car entry)))
             (delta-ext (- ext (cadr entry)))
             (delta-int (- int (caddr entry))))
        (incf sum-delta-ext delta-ext)
        (incf sum-delta-int delta-int)
        (assert (<= delta-ext 0))
        (assert (<= delta-int 0))
        (format t "~20a | ~5d (~5@d) | ~5d (~5@d)~%"
                (package-name (car entry))
                ext delta-ext int delta-int)))
    (format t "~28t (~5@d) |       (~5@d) = (~d)~%"
            sum-delta-ext sum-delta-int
            (+ sum-delta-ext sum-delta-int))))

(scan-format-control-strings)

(macrolet ((def-backward-compatible-sb-c-specials (pairs) ; for UIOP + ASDF
             `(progn
                ,@(mapcar (lambda (pair)
                            `(define-symbol-macro ,(car pair)
                                 (,(sb-int:package-symbolicate "SB-C" "CU-" (cdr pair) "-COUNT")
                                   sb-c::*compilation-unit*)))
                          pairs))))
  ;; coece to a strict boolean
  (define-symbol-macro sb-c::*in-compilation-unit* (not (null sb-c::*compilation-unit*)))
  ;; the "specials" are all SETFable when and only when *IN-COMPILATION-UNIT* is T
  (def-backward-compatible-sb-c-specials
      sb-c::((*aborted-compilation-unit-count* . "ABORTED")
             (*compiler-error-count* . "ERROR")
             (*compiler-warning-count*  . "WARNING")
             (*compiler-style-warning-count* . "STYLE-WARNING")
             (*compiler-note-count* . "NOTE"))))

#+sb-devel
(rename-package "COMMON-LISP" "COMMON-LISP" '("SB-XC" "CL"))

;;; Lock internal packages
#-(and sb-devel
       (not sb-devel-lock-packages))
(dolist (p (list-all-packages))
  (unless (member p (mapcar #'find-package '("KEYWORD" "CL-USER")))
    (sb-ext:lock-package p)))

;;; Clean up stray symbols from the CL-USER package.
(with-package-iterator (iter "CL-USER" :internal :external)
  (loop (multiple-value-bind (winp symbol) (iter)
          (if winp (unintern symbol "CL-USER") (return)))))

(setq sb-c:*compile-to-memory-space* :auto)
(when (find-package "SB-INTERPRETER") (setq sb-ext:*evaluator-mode* :interpret))
#+x86-64 (sb-ext:fold-identical-code :aggressive t :preserve-docstrings t)

;; See comments in 'readtable.lisp'
(setf (readtable-base-char-preference *readtable*) :symbols)
