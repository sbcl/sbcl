;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

#-x86-64 (sb-ext:exit :code 104)

;;; This trivial function failed to compile due to rev 88d078fe
(defun foo (&key k)
  (make-list (reduce #'max (mapcar #'length k))))
(compile 'foo)

(with-test (:name :lowtag-test-elision)
  ;; This tests a certain behavior that while "undefined" should at least not
  ;; be fatal. This is important for things like hash-table :TEST where we might
  ;; call (EQUAL x y) with X being an unbound marker indicating an empty cell.
  ;; After we started using IR1 type derivation to elide lowtag as a guard condition
  ;; in some type tests, it became more likely to dereference an unbound marker
  ;; which does not fit anywhere in the lisp type space.
  (let ((f (compile nil
                    '(lambda (x)
                      (typecase x
                        ((or character number list sb-kernel:instance function) 1)
                        ;; After eliminating the preceding cases, the compiler knows
                        ;; that the only remaining pointer type is OTHER-POINTER,
                        ;; so it just tries to read the widetag.
                        ;; If X is the unbound marker, this will read a byte preceding
                        ;; the start of static space, but it holds a zero.
                        (simple-vector 2))))))
    (assert (not (funcall f (sb-kernel:make-unbound-marker)))))
  (assert (not (equalp (sb-kernel:make-unbound-marker) "")))
  (let ((a (- (sb-kernel:get-lisp-obj-address (sb-kernel:make-unbound-marker))
              sb-vm:other-pointer-lowtag)))
    (assert (> a sb-vm:static-space-start))))

(load "compiler-test-util.lisp")
(defun disassembly-lines (fun)
  ;; FIXME: I don't remember what this override of the hook is for.
  (sb-int:encapsulate 'sb-disassem::add-debugging-hooks 'test
                      (lambda (f &rest args) (declare (ignore f args))))
  (prog1
      (mapcar (lambda (x) (string-left-trim " ;" x))
              (cddr
               (split-string
                (with-output-to-string (s)
                  (let ((sb-disassem:*disassem-location-column-width* 0)
                        (*print-pretty* nil))
                    (disassemble fun :stream s)))
                #\newline)))
    (sb-int:unencapsulate 'sb-disassem::add-debugging-hooks 'test)))

(sb-vm::define-vop (tryme)
    (:generator 1 (sb-assem:inst mov :byte (sb-vm::ea :gs sb-vm::rax-tn) 0)))
(with-test (:name :try-gs-segment)
  (assert (loop for line in (disassembly-lines
                             (compile nil
                                      '(lambda () (sb-sys:%primitive tryme))))
                thereis (search "MOV BYTE PTR GS:[RAX]" line))))

(defun disasm (safety expr &optional (remove-epilogue t))
  ;; This lambda has a name because if it doesn't, then the name
  ;; is something stupid like (lambda () in ...) which pretty-prints
  ;; on a random number of lines.
  (let ((fun (compile nil
                      `(sb-int:named-lambda test ()
                         (declare (optimize (debug 0) (safety ,safety)
                                            (sb-c:verify-arg-count 0)))
                         ,expr))))
    (let ((lines (disassembly-lines fun)))
      ;; For human-readability, kill the whitespace
      (setq lines (mapcar (lambda (x) (string-left-trim " ;" x)) lines))
      (when (string= (car (last lines)) "")
        (setq lines (nbutlast lines)))
      ;; Remove safepoint traps
      (setq lines (remove-if (lambda (x) (search "; safepoint" x)) lines))
      ;; If the last 4 lines are of the expected form
      ;;   MOV RSP, RBP / CLC / POP RBP / RET
      ;; then strip them out
      (if (and remove-epilogue
               (every #'search
                      '("MOV RSP, RBP" "CLC" "POP RBP" "RET")
                      (subseq lines (- (length lines) 4))))
          (butlast lines 4)
          lines))))

(with-test (:name :symeval-known-thread-local
            :skipped-on (not :sb-thread))
  ;; It should take 1 instruction to read a known thread-local var
  (assert (= (length (disasm 1 'sb-thread:*current-thread*)) 1))
  (assert (= (length (disasm 1 'sb-sys:*interrupt-pending*)) 1))
  (assert (= (length (disasm 1 'sb-kernel:*gc-inhibit*)) 1))
  (assert (= (length (disasm 1 'sb-kernel:*restart-clusters*)) 1))
  (assert (= (length (disasm 1 'sb-kernel:*handler-clusters*)) 1)))

;; Lack of earmuffs on this symbol allocates it in dynamic space
(defvar foo)
#-immobile-symbols (assert (not (sb-kernel:immobile-space-obj-p 'foo)))
;; This compilation causes a side-effect of assigning FOO a TLS index
;; DO NOT REMOVE!
(compile nil '(lambda (foo) (eval 'frob)))

(with-test (:name :symeval-known-tls-index
            :skipped-on (or (not :immobile-space) :immobile-symbols
                            (not :sb-thread)))
  ;; When symbol SC is IMMEDIATE:
  ;;    498B9578210000     MOV RDX, [R13+disp]       ; tls: *PRINT-BASE*
  ;;    83FA61             CMP EDX, 97
  ;;    480F44142538F94B20 CMOVEQ RDX, [#x204BF938]  ; *PRINT-BASE*
  ;; (TODO: could use "CMOVEQ RDX, [RIP-n]" in immobile code)
  (let ((text (disasm 0 '*print-base*)))
    (assert (= (length text) 3)) ; number of lines
    ;; two lines should be annotated with *PRINT-BASE*
    (assert (= (loop for line in text count (search "*PRINT-BASE*" line)) 2)))

  ;; When symbol SC is CONSTANT:
  ;;    498B9578290000     MOV RDX, [R13+disp]       ; tls: FOO
  ;;    488B059EFFFFFF     MOV RAX, [RIP-98]         ; 'FOO
  ;;    83FA61             CMP EDX, 97
  ;;    480F4450F9         CMOVEQ RDX, [RAX-7]
  (let ((text (disasm 0 'foo)))
    (assert (= (length text) 4))
    ;; two lines should be annotated with FOO
    (assert (= (loop for line in text count (search "FOO" line)) 2))))

(defvar *blub*) ; immobile space
(defvar blub)   ; dynamic space
#+immobile-space (assert (sb-kernel:immobile-space-obj-p '*blub*))
#-immobile-symbols (assert (not (sb-kernel:immobile-space-obj-p 'blub)))

(with-test (:name :symeval-unknown-tls-index
            :skipped-on (or (not :immobile-space) :immobile-symbols
                            (not :sb-thread)))
  ;; When symbol SC is immediate:
  ;;    8B142514A24C20     MOV EDX, [#x204CA214]    ; tls_index: *BLUB*
  ;;    4A8B142A           MOV RDX, [RDX+R13]
  ;;    83FA61             CMP EDX, 97
  ;;    480F44142518A24C20 CMOVEQ RDX, [#x204CA218] ; *BLUB*
  ;; (TODO: could use "CMOVEQ RDX, [RIP-n]" in immobile code)
  (let ((text (disasm 0 '*blub*)))
    (assert (= (length text) 4))
    ;; two lines should be annotated with *BLUB*
    (assert (= (loop for line in text count (search "*BLUB*" line)) 2)))

  ;; When symbol SC is constant:
  ;;    488B05B3FFFFFF     MOV RAX, [RIP-77]          ; 'BLUB"
  ;;    8B50F5             MOV EDX, [RAX-11]
  ;;    4A8B142A           MOV RDX, [RDX+R13]
  ;;    83FA61             CMP EDX, 97
  ;;    480F4450F9         CMOVEQ RDX, [RAX-7]
  (assert (= (length (disasm 0 'blub)) 5)))

(with-test (:name :object-not-type-error-encoding)
  ;; There should not be a "MOV Rnn, #xSYMBOL" instruction
  ;; before the OBJECT-NOT-TYPE-ERROR.
  (let* ((lines
          (split-string
           (with-output-to-string (s)
            (let ((sb-disassem:*disassem-location-column-width* 0))
              (disassemble '(lambda (x) (the sb-assem:label x))
                           :stream s)))
           #\newline))
         (index
          (position "OBJECT-NOT-TYPE-ERROR" lines :test 'search)))
    (let ((line (nth (+ index 2) lines)))
      (assert (search "; #<SB-KERNEL:WRAPPER " line))
      (assert (search " SB-ASSEM:LABEL" line)))))

#+immobile-code
(with-test (:name :reference-assembly-tramp)
  (dolist (testcase '(("FUNCALLABLE-INSTANCE-TRAMP"
                       sb-kernel:%make-funcallable-instance)))
    (let ((lines
           (split-string
            (with-output-to-string (stream)
              (let ((sb-disassem:*disassem-location-column-width* 0))
                (disassemble (cadr testcase) :stream stream)))
            #\newline)))
      (assert (loop for line in lines
                    thereis (and (search "LEA" line)
                                 (search "RIP" line) ; require RIP-relative mode
                                 ;; and verify disassembly
                                 (search (car testcase) line)))))))

#+immobile-code ; uses SB-C::*COMPILE-TO-MEMORY-SPACE*
(with-test (:name :static-link-compile-to-memory)
  (let* ((string
          (with-output-to-string (stream)
            (disassemble
             (let ((sb-c::*compile-to-memory-space* :immobile))
               (compile nil '(lambda () (print (gensym)))))
             :stream stream)))
         (lines (split-string string #\newline)))
    (flet ((find-line (mnemonic operand)
             (find-if (lambda (line)
                        (and (search mnemonic line)
                             (search operand line)))
                      lines)))
    (assert (find-line "CALL" "FUNCTION GENSYM"))
    (assert (find-line "JMP" "FUNCTION PRINT")))))

#+immobile-code
(with-test (:name :static-unlinker)
  (let ((sb-c::*compile-to-memory-space* :immobile))
    (declare (muffle-conditions style-warning))
    (flet ((disassembly-lines (name)
             (split-string
              (with-output-to-string (s)
                (let ((sb-disassem:*disassem-location-column-width* 0))
                  (disassemble name :stream s)))
              #\newline))
           (expect (match lines)
             (assert (loop for line in lines
                           thereis (search match line)))))
      (compile 'h '(lambda (x) (1+ x)))
      (setf (symbol-function 'g) #'h (symbol-function 'f) #'h)
      (compile 'c '(lambda (x) (g x)))
      (compile 'd '(lambda (x) (f (g x))))
      ;; The FDEFN-FUN of F is same as that of G.
      ;; Statically linking D should not patch the fdefn calls into static calls
      ;; because it can't unambiguously be undone without storing additional data
      ;; about where patches were performed to begin with.
      (sb-vm::statically-link-core :callers '(c d))
      (let ((lines (disassembly-lines 'c)))
        (expect "#<FUNCTION H>" lines))
      (let ((lines (disassembly-lines 'd)))
        (expect "#<FUNCTION H>" lines))
      (setf (symbol-function 'g) #'+)
      (let ((lines (disassembly-lines 'c)))
        (expect "#<FDEFN G>" lines))
      (let ((lines (disassembly-lines 'd)))
        (expect "#<FDEFN G>" lines)
        (expect "#<FUNCTION H>" lines)))))

(with-test (:name :c-call :skipped-on (and :win32 :x86-64))
  (let* ((lines (split-string
                 (with-output-to-string (s)
                   (let ((sb-disassem:*disassem-location-column-width* 0))
                     (disassemble 'sb-sys:deallocate-system-memory :stream s)))
                 #\newline))
         (c-call (find "os_deallocate" lines :test #'search)))
    ;; Depending on #+immobile-code it's either direct or memory indirect.
    #+immobile-code (assert (search "CALL #x" c-call))
    #-immobile-code (assert (search "CALL QWORD PTR [#x" c-call))))

(with-test (:name :set-symbol-value-imm)
  (let (success)
    (dolist (line (split-string
                   (with-output-to-string (s)
                     (let ((sb-disassem:*disassem-location-column-width* 0))
                       (disassemble '(lambda () (setq *print-base* 8)) :stream s)))
                   #\newline))
      (when (and #+sb-thread (search "MOV QWORD PTR [R" line)
                 #-sb-thread (search "MOV QWORD PTR [" line)
                 (search (format nil ", ~D" (ash 8 sb-vm:n-fixnum-tag-bits)) line))
        (setq success t)))
    (assert success)))

(defglobal *avar* nil)
(with-test (:name :set-symbol-value-imm-2
                  :skipped-on (not :immobile-space))
  (let (success)
    (dolist (line (split-string
                   (with-output-to-string (s)
                     (let ((sb-disassem:*disassem-location-column-width* 0))
                       (disassemble '(lambda () (setq *avar* :downcase)) :stream s)))
                   #\newline))
      ;; Should have an absolute mem ref and an immediate operand:
      ;;   48C7042568904B207F723A20 MOV QWORD PTR [#x204B9068], #x203A727F
      (when (and (search "MOV QWORD PTR [#x" line)
                 (search "], #x" line))
        (setq success t)))
    (assert success)))

(defun test-arith-op-codegen (fun imm)
  (split-string
   (with-output-to-string (s)
    (let ((sb-disassem:*disassem-location-column-width* 0))
      (disassemble `(lambda (a b)
                      (declare (fixnum b))
                      (print 1) ; force spilling args to stack
                      ;; Use an expression that doesn't select CMOV
                      ;; as the implementation.
                      ;; CMOV thinks it needs all args loaded,
                      ;; defeating the purpose of this test.
                      (values a (if (,fun b ,imm) 'baz (print 2))))
                   :stream s)))
   #\newline))

(with-test (:name :test-high-byte-reg)
  ;; Assert two things:
  ;; - that LOGBITP can use a high byte register (sometimes)
  ;; - that the fixnum #x80 (representation #x100) is a single byte test
  (let (success)
    (dolist (line
             (split-string
              (with-output-to-string (s)
               (let ((sb-disassem:*disassem-location-column-width* 0))
                 (disassemble '(lambda (x) (logtest (the fixnum x) #x80))
                              :stream s)))
              #\newline))
      (when (search (format nil "TEST DH, ~D"
                            (ash (ash #x80 sb-vm:n-fixnum-tag-bits) -8))
                    line)
        (setq success t)))
    (assert success)))

(with-test (:name :test-byte-stack-imm)
  ;; Assert that LOGBITP can accept memory + immediate as the operands
  (let (success)
    (dolist (line (test-arith-op-codegen 'logtest #x80))
      (when (and (search "TEST BYTE PTR [RBP-" line)
                 (search (format nil
                          ", ~d"
                          (ash (ash #x80 sb-vm:n-fixnum-tag-bits) -8))
                         line))
        (setq success t)))
    (assert success)))

(with-test (:name :fixnum-cmp-stack-imm)
  ;; Assert that < can accept memory + immediate as the operands
  (let (success)
    (dolist (line (test-arith-op-codegen '< -5))
      (when (and (search "CMP QWORD PTR [RBP-" line)
                 (search (format nil
                          ", ~d" (ash -5 sb-vm:n-fixnum-tag-bits))
                         line))
        (setq success t)))
    (assert success)))

(with-test (:name :list-vop-immediate-to-mem
                  :skipped-on (not :immobile-space))
  (let ((lines
         (split-string
          (with-output-to-string (s)
            (let ((sb-disassem:*disassem-location-column-width* 0))
              (disassemble '(lambda () (list :key :test)) :stream s)))
          #\newline)))
    (assert (loop for line in lines
                  thereis (and (search "MOV QWORD PTR [" line)
                               (search ":KEY" line))))))

(defstruct thing x)
(with-test (:name :instance-ref-eq
                  :skipped-on (not :immobile-space))
  (let ((lines
         (split-string
          (with-output-to-string (s)
            (disassemble '(lambda (obj)
                           (if (eq (thing-x (truly-the thing obj)) :yup) 1 2))
                         :stream s))
          #\newline)))
    ;; assert that the comparison instruction performed the memory load
    (assert (loop for line in lines
                  thereis
                  (and (search "CMP QWORD PTR [" line)
                       (search ":YUP" line))))))

(defun thing-ref-thing-ref (arg1 arg2)
  (declare (optimize (safety 0)))
  (let ((answer (typep (thing-x (thing-x arg1)) 'fixnum)))
    (frobify arg1 arg2)
    answer))
(defun frobify (a b) (values a b))
(compile 'thing-ref-thing-ref)

(with-test (:name :fixnump-thing-ref)
  (flet ((try (access-form true false)
           (let* ((f (compile nil `(lambda (obj) (typep ,access-form 'fixnum))))
                  (lines
                   (split-string
                    (with-output-to-string (s) (disassemble f :stream s))
                    #\newline)))
             (assert (funcall f true))
             (assert (not (funcall f false)))
             ;; assert that the TEST instruction dereferenced OBJ and performed the test
             (assert (loop for line in lines
                           thereis
                           (and (search "TEST BYTE PTR [" line)
                                (search (format nil ", ~D" sb-vm:fixnum-tag-mask)
                                        line)))))))
    (try '(thing-x (truly-the thing obj))
         (make-thing :x 1) (make-thing :x "hi"))
    (try '(car obj) '(1) '("hi"))
    (try '(cdr obj) '("hi" . 1) '("hi")))
  ;; fixnump of memref of memref was eliding one memref by accident
  (assert (thing-ref-thing-ref (make-thing :x (make-thing :x 3)) 'foo)))

(with-test (:name :huge-code :skipped-on (not :immobile-code))
  (sb-vm::allocate-code-object :immobile 0 4 (* 2 1024 1024)))

(defun bbb (x y z)
  ;; I don't want the number of expected comparisons to depend on whether
  ;; the code gets disassembled with versus without the initial segment.
  (declare (optimize (sb-c:verify-arg-count 0)))
  (if x
      (ecase y
        (-2 'a) (2 'b) (3 'c) (4 (error "no")) (5 'd) (6 'e) (7 'wat) (8 '*) (9 :hi))
      (case z
        (#\a :a) (#\b :b) (#\e :c) (#\f :d) (#\g :e) (#\h :f) (t nil))))

(defun try-case-known-fixnum (x)
  (declare (optimize (sb-c:verify-arg-count 0)))
  (case (the fixnum x)
    (0 :a) (1 :b) (2 :c) (5 :d) (6 :c) (-1 :blah)))
(defun try-case-maybe-fixnum (x)
  (when (typep x 'fixnum)
    (case x
      (0 :a) (1 :b) (2 :c) (5 :d) (6 :c) (-1 :blah))))

(defun try-case-known-char (x)
  (declare (optimize (sb-c:verify-arg-count 0)))
  (case (the character x)
    (#\a :a) (#\b :b)(#\c :c) (#\d :d) (#\e :e) (#\f :b)))
(defun try-case-maybe-char (x)
  (declare (optimize (sb-c:verify-arg-count 0)))
  (when (characterp x)
    (case x (#\a :a) (#\b :b)(#\c :c) (#\d :d) (#\e :e) (#\f :a))))

(defun expect-n-comparisons (fun-name howmany)
  (let ((lines
          (split-string
           (with-output-to-string (s) (disassemble fun-name :stream s))
           #\newline)))
    (assert (= (loop for line in lines count (search "CMP" line))
               howmany))))

(with-test (:name :multiway-branch-generic-eq)
  ;; there's 1 test of NIL, 1 test of character-widetag, and 2 limit checks
  (expect-n-comparisons 'bbb 4)
  (loop for ((x y z) . expect) in '(((t 3 nil) . c)
                                    ((t 9 nil) . :hi)
                                    ((nil nil #\b) . :b)
                                    ((nil nil #\x) . nil))
        do (assert (eq (bbb x y z) expect))))

(with-test (:name :multiway-branch-fixnum-eq)
  (expect-n-comparisons 'try-case-known-fixnum 1) ; just the upper bound
  (expect-n-comparisons 'try-case-maybe-fixnum 1))

(with-test (:name :multiway-branch-char-eq)
  (expect-n-comparisons 'try-case-known-char 2) ; widetag test and upper bound
  (expect-n-comparisons 'try-case-maybe-char 2))

(with-test (:name :multiway-branch-min-branch-factor)
  ;; Test that multiway vop shows up in IR2
  (let ((s (with-output-to-string (sb-c::*compiler-trace-output*)
             (checked-compile '(lambda (b)
                                (case b
                                  ((0) :a) ((0) :b) ((0) :c) ((1) :d)
                                  ((2) :e) ((3) :f)))
                              :allow-style-warnings t))))
    (assert (search "MULTIWAY-BRANCH" s)))
  ;; There are too few cases after duplicate removal to be considered multiway
  (let ((s (with-output-to-string (sb-c::*compiler-trace-output*)
             (checked-compile '(lambda (b) (case b ((0) :a) ((0) :b) ((0) :c) ((1) :d)))
                              :allow-style-warnings t))))
    (assert (not (search "MULTIWAY-BRANCH" s)))))

;;; Don't crash on large constants
;;; https://bugs.launchpad.net/sbcl/+bug/1850701
;;; (modified example to avoid style warning)
(with-test (:name :multiway-branch-large-constants)
  (let ((f (checked-compile
            '(lambda (p)
               (case p
                (1881481965704634 0)
                (1881481965704630 1)
                (1881481965704635 2)
                (1881481965704629 3)
                (1881481965704633 4)
                (t nil))))))
    (assert (eql (funcall f 1881481965704629) 3))))

;; https://bugs.launchpad.net/sbcl/+bug/1850705
(with-test (:name :multiway-branch-dead-code-elim)
  (let ((f (checked-compile
            '(lambda (b)
              (case b
                ((4 1 2 3) 0)
                ((2 4) (mod b (min -49 0)))
                (t 0)))
            :allow-style-warnings t)))
    (dotimes (i 5) (assert (eql (funcall f i) 0)))
    (assert (eql (funcall f 'anything) 0)))
  (let ((f (checked-compile
            '(lambda (s1 s2 c)
              (case s2
                ((2) s2)
                ((2) (mod s1 (max 9 0)))
                ((1 4) 0)
                ((-1) c)
                (t 0)))
            :allow-style-warnings t)))
    (assert (eql (funcall f nil 2 nil) 2))
    (assert (eql (funcall f 0 -1 :hi) :hi))))

;; https://bugs.launchpad.net/sbcl/+bug/1850785
(with-test (:name :multiway-branch-to-branch)
  ;; The behavior changes because of single-use var elimination
  ;; which eliminates a move which eliminates a vop as a no-op,
  ;; or something, which left an empty block. (Hand-waving explanation)
  (let* ((form '(case c
                 ((6) c)
                 ((9 13 12) (case a ((-2589) a) ((970) 0) (t b)))
                 (t a)))
         (l1 `(lambda (a b c) (declare (optimize (debug 0))) ,form))
         (l2 `(lambda (a b c) ,form))
         (f1 (checked-compile l1))
         (f2 (checked-compile l2))
         (vals '(:good :bad 0)))
    (assert (eq (apply f1 vals) :good))
    (assert (eq (apply f2 vals) :good)))
  ;;
  (let* ((form '(case d
                 ((7 18) (case b
                           ((114361) :bad)
                           ((77773) a)
                           ((118772) c)
                           (t :bad)))
                 ((6) 2)
                 ((3) b)
                 ((15 21 9 19 2) 3)
                 ((20) 1)
                 (t a)))
         (vals '(:good 5 :xyz 0))
         (l1 `(lambda (a b c d)
                (declare (type fixnum b))
                ,form))
         (l2 `(lambda (a b c d)
                (declare (optimize (speed 3)))
                ,form))
         (f1 (checked-compile l1))
         (f2 (checked-compile l2)))
    (assert (eq (apply f1 vals) :good))
    (assert (eq (apply f2 vals) :good))))

(defun count-assembly-labels (lines)
  (count-if (lambda (line &aux (colon (search ": " line)))
              (and colon
                   (char= (char line (+ colon 2)) #\L)))
            lines))

(with-test (:name :disassembler-label-jump-table-targets)
  (let* ((f (checked-compile
             '(lambda (x)
               (declare (optimize (sb-c:verify-arg-count 0)))
               (case (truly-the fixnum x)
                 (0 (a)) (1 (b)) (2 (c)) (3 (d))))
             :allow-style-warnings t))
         (lines (split-string
                 (with-output-to-string (s) (disassemble f :stream s))
                 #\newline)))
    (assert (>= (count-assembly-labels lines) 4))))

(with-test (:name :ecase-symbol->integer-branch-free)
  (let* ((f (checked-compile
             '(lambda (x)
               ;; The safety 0 decl skips the arg count check
               ;; and trusts that X satisfies symbolp.
               (declare (symbol x) (optimize (safety 0)))
               (ecase x
                 ((nil) 0) (a 1) (b 2) (c 3) (d 4)
                 (e 5) (f 6) (g 7) (h 8) (i 9)))))
         (lines (split-string
                 (with-output-to-string (s) (disassemble f :stream s))
                 #\newline)))
    ;; Aside from ECASE failure, there are no other JMPs
    (assert (= (count-assembly-labels lines) 1))))

(with-test (:name :ecase-failure-trap)
  (assert (null (ctu:find-named-callees
                 (checked-compile `(lambda (x)
                                     (ecase x (:a 1) (:b 2) (:c 3)))))))
  (assert (null (ctu:find-named-callees
                 (checked-compile `(lambda (x)
                                     (etypecase x
                                       ((integer 1 20) 'hi)
                                       ((cons (eql :thing)) 'wat)
                                       (bit-vector 'hi-again))))))))

(with-test (:name :symbol-case-optimization-levels)
  (let ((cases
         '((a 1) (b 1) (c 3/2)
           (d 2) (e 2) (f "hello")
           (g 3) (h 3) (i -3))))
    (dolist (constraint '(t symbol (not null) (and symbol (not null))))
      (dotimes (safety 4)
        (let ((f (checked-compile
                  `(lambda (x)
                    (declare (optimize (safety ,safety)))
                    (case (the ,constraint x) ,@cases (t :feep))))))
          (dolist (input '(a b c d e f g h i j k nil 5 7))
            (when (typep input constraint)
              (assert (eq (funcall f input)
                          (let ((cell (assoc input cases)))
                            (if cell (cadr cell) :feep)))))))))))

(defun count-assembly-lines (f)
  (length (split-string (with-output-to-string (string)
                          (disassemble f :stream string))
                        #\newline)))

(with-test (:name :peephole-optimizations-1)
  ;; The test does not check that both the load and the shift
  ;; have been sized as :dword instead of :qword, but it should.
  (let ((f '(lambda (x)
             ;; eliminate arg count check, type check
             (declare (optimize speed (safety 0)))
             (ldb (byte 3 0) (sb-kernel:symbol-hash x)))))
    (let ((unoptimized (let ((sb-c::*do-instcombine-pass* nil))
                         (checked-compile f)))
          (instcombined (checked-compile f)))
      (assert (= (count-assembly-lines instcombined)
                 (- (count-assembly-lines unoptimized) 1)))))

  (let ((f '(lambda (x)
             ;; eliminate arg count check, type check
             (declare (optimize speed (safety 0)))
             (ldb (byte 5 2) (sb-kernel:symbol-hash x)))))
    (let ((unoptimized (let ((sb-c::*do-instcombine-pass* nil))
                         (checked-compile f)))
          (instcombined (checked-compile f)))
      (assert (= (count-assembly-lines instcombined)
                 (- (count-assembly-lines unoptimized) 2))))))

(with-test (:name :array-subtype-dispatch-table)
  (assert (eql (sb-kernel:code-jump-table-words
                (sb-kernel:fun-code-header #'sb-kernel:vector-subseq*))
               ;; n-widetags divided by 4, plus jump table count word.
               65)))

sb-vm::(define-vop (cl-user::test)
  (:generator 0
   ;; pointless to resize to :qword, but the rule won't try to apply itself
   ;; to :dword because zeroing the upper 32 bits is a visible effect.
   (inst mov (reg-in-size rax-tn :qword) (reg-in-size rcx-tn :qword))
   (inst mov rax-tn rcx-tn)))
(with-test (:name :mov-mov-elim-ignore-resized-reg
                  :fails-on :sbcl) ; just don't crash
  (checked-compile '(lambda () (sb-sys:%primitive test) 0)))

(defstruct a)
(defstruct (achild (:include a)))
(defstruct (agrandchild (:include achild)))
(defstruct (achild2 (:include a)))
(defstruct b)
(defstruct c)
(defstruct d)
(defstruct e)
(defstruct (echild (:include e)))
(defstruct f)

(declaim (freeze-type a b c d e f))
(defun typecase-jump-table (x)
  (typecase x
    (a 'is-a)
    (b 'is-b)
    (c 'is-c)
    ((or d e) 'is-d-or-e)
    (f 'is-f)))
(compile 'typecase-jump-table)

(with-test (:name :typecase-jump-table)
  (assert (eql (sb-kernel:code-jump-table-words
                (sb-kernel:fun-code-header #'typecase-jump-table))
               ;; 6 cases including NIL return, plus the size
               7)))

(defun assert-thereis-line (lambda expect)
  (let ((f (checked-compile lambda)))
    (assert
     (loop for line in (split-string (with-output-to-string (string)
                                       (disassemble f :stream string))
                                     #\newline)
           ;; very brittle, as it looks for a specific register
           thereis (search expect line)))))

(with-test (:name :char-code-is-single-shr)
  (assert-thereis-line '(lambda (x) (char-code (truly-the character x)))
                       "SHR EDX, 7"))

(import '(sb-x86-64-asm::get-gpr sb-x86-64-asm::machine-ea))
;; to make this pass on different configurations we'd have to add
;; some abstraction on the PC offsets on each line.
#+nil
(with-test (:name :simple-fun-instruction-model)
  (let ((rax (get-gpr :qword sb-vm::rax-offset))
        (eax (get-gpr :dword sb-vm::rax-offset))
        (al  (get-gpr :byte  sb-vm::rax-offset))
        (rcx (get-gpr :qword sb-vm::rcx-offset))
        (rdx (get-gpr :qword sb-vm::rdx-offset))
        (rsp (get-gpr :qword sb-vm::rsp-offset))
        (rbp (get-gpr :qword sb-vm::rbp-offset)))
    (flet ((compare (a b)
             (and (eql (car a) (car b)) ; PC offs
                  (string= (cadr a) (cadr b)) ; inst name
                  (equalp (cddr a) (cddr b)))))
      (mapc (lambda (a b)
              (unless (compare a b)
                (error "Didn't match: ~S ~S" a b)))
            (get-simple-fun-instruction-model #'car)
            `(( 0 pop (#s(machine-ea :base 5 :disp 8) . :qword))
              ( 3 cmp ,rcx 2)
              ( 7 jmp :ne 29)
              ( 9 mov ,rsp ,rbp)
              (12 lea ,eax (#s(machine-ea :base 2 :disp -7) . :dword))
              (15 test ,al 15)
              (17 jmp :ne +5) ; = PC offs 24
              (19 mov ,rax ,rdx)
              (22 jmp +4) ; = PC offs 28
              (24 break 71)
              (28 mov ,rdx (#s(machine-ea :base 0 :disp -7) . :qword))
              (32 mov ,rsp ,rbp)
              (35 clc)
              (36 pop ,rbp)
              (37 ret)
              (38 break 16))))))

(with-test (:name :typep-compiled-with-jump-table)
  ;; Expect to reference the CTYPE layout because %%TYPEP declares its argument
  ;; to be that.  Expect to reference the UNKNOWN-TYPE layout because of an
  ;; explicit call to UNKNOWN-TYPE-P; same for FUN-DESIGNATOR-TYPE-P.

  ;; The other types are referenced from other functions in the code
  ;; component.
  (let ((names
          (mapcar (lambda (x)
                    (sb-kernel:classoid-name (sb-kernel:wrapper-classoid x)))
                  (ctu:find-code-constants #'sb-kernel:%%typep :type 'sb-kernel:wrapper))))
    (assert (null (set-difference names
                                  '(sb-kernel:ctype
                                    sb-kernel:unknown-type
                                    sb-kernel:fun-designator-type
                                    sb-c::abstract-lexenv
                                    sb-kernel::classoid-cell
                                    sb-kernel:wrapper
                                    sb-kernel:classoid
                                    sb-kernel:built-in-classoid
                                    #-immobile-space null))))))

;; lp#1857861
(with-test (:name :undecoded-immediate-data)
  (let ((f (compile nil '(lambda (x) (when (floatp x) 0.0))))
        (expect (format nil ", ~D" sb-vm:single-float-widetag)))
     (loop for line in (split-string (with-output-to-string (string)
                                       (disassemble f :stream string))
                                     #\newline)
             thereis (let ((p (search expect line)))
                       ;; expect no end-of-line comment
                       (and p (not (find #\; line :start p))))))
  (let ((f (compile nil '(lambda (x) (sb-ext:process-p x)))))
     (loop for line in (split-string (with-output-to-string (string)
                                       (disassemble f :stream string))
                                     #\newline)
             thereis (and (search "WRAPPER for" line)
                          (search "CMP DWORD PTR" line)))))

(with-test (:name :thread-local-unbound)
  (let ((c (nth-value 1 (ignore-errors sb-c::*compilation*))))
    (assert (eq (cell-error-name c) 'sb-c::*compilation*))))

#+immobile-code
(with-test (:name :debug-fun-from-pc-more-robust)
  ;; This test verifies that debug-fun-from-pc does not croak when the PC points
  ;; within a trampoline allocated to wrap a closure in a simple-funifying wrapper
  ;; for installation into a global symbol.
  (let ((closure (funcall (compile nil '(lambda (x)  (lambda () x))) 0))
        (symbol (gensym)))
    (assert (sb-kernel:closurep closure))
    (setf (fdefinition symbol) closure)
    (let ((trampoline
            (sb-di::code-header-from-pc
             (sb-sys:int-sap (sb-vm::fdefn-raw-addr
                              (sb-int:find-fdefn symbol))))))
      (assert (zerop (sb-kernel:code-n-entries trampoline)))
      (assert (typep (sb-di::debug-fun-from-pc trampoline 8)
                     'sb-di::bogus-debug-fun)))))

(defstruct foo (s 0 :type (or null string)))
(with-test (:name :reduce-stringp-to-not-null)
  (let ((f1 (disassembly-lines
             '(lambda (x) (if (null (foo-s (truly-the foo x))) 'not 'is))))
        (f2 (disassembly-lines
             '(lambda (x) (if (stringp (foo-s (truly-the foo x))) 'is 'not)))))
    ;; the comparison of X to NIL should be a single-byte test
    (assert (loop for line in f1
                  thereis (and (search (format nil "CMP ") line) ; register is arbitrary
                               (search (format nil ", ~D" (logand sb-vm:nil-value #xff)) line))))
    ;; the two variations of the test compile to the identical code
    (dotimes (i 4)
      (assert (string= (nth i f1) (nth i f2))))))

(with-test (:name :make-list-ridiculously-huge)
  (checked-compile '(lambda () (make-list 3826305079707827596))
                   :allow-warnings t))

(with-test (:name :with-foo-macro-elides-arg-count-trap)
  (let ((lines
          (split-string
           (with-output-to-string (s)
             (sb-c:dis '(lambda (x) (with-standard-io-syntax (eval x))) s))
           #\newline)))
    ;; The outer lambda checks its arg count, but the lambda
    ;; passed to call-with-mutex does not.
    (assert (= (count-if (lambda (line)
                           (search "Invalid argument count trap" line))
                         lines)
               1))))

#+compact-instance-header
(with-test (:name :gf-self-contained-trampoline)
  (let ((l (sb-kernel:find-layout 'standard-generic-function)))
    (assert (/= (sb-kernel:wrapper-bitmap l) sb-kernel:+layout-all-tagged+))))

(with-test (:name :known-array-rank)
  (flet ((try (type)
           (let ((lines
                  (disassembly-lines
                   `(lambda (x)
                     #+sb-safepoint (declare (optimize (sb-c::insert-safepoints 0)))
                     (array-rank (truly-the ,type x))))))
             ;; (format t "~{~&~A~}" lines)
             ;; Naturally this is brittle as heck. I wish we had a better way.
             (assert (<= (length lines) 9)))))
    (try 'string)
    (try '(and vector (not simple-array)))
    (try '(or string bit-vector)) ; not an array type, but known rank
    (try '(array * (4 5 *)))))

;;; Match the same set of objects that %OTHER-POINTER-P does.
(deftype other-pointer-object ()
  '(not (or fixnum single-float function list sb-kernel:instance character)))

;;; Helper to assert something about how many comparisons it takes to test
;;; for various sets of widetags.
(defun count-cmp-opcodes (type expect function)
  (let ((lines (disassembly-lines function)))
    (let ((actual
           (loop for line in lines
                 count (or (search "CMP" line)
                           (search "TEST" line)))))
      (unless (eql actual expect)
        (format t "~{~&~a~}~%" lines)
        (error "typep: needed ~d test ops but expected ~d for ~a"
               actual expect type)))))

(defun check-arrayp-cmp-opcodes (expect type)
  ;; Assume the lowtag test passed already.
  (count-cmp-opcodes type expect
          `(lambda (x)
             (declare (optimize (sb-c::verify-arg-count 0)
                                #+sb-safepoint (sb-c::insert-safepoints 0)))
             (typep (truly-the other-pointer-object x) ',type))))

(with-test (:name :arrayp-exactly-one-comparison-etc)
  (check-arrayp-cmp-opcodes 1 'array)

  (check-arrayp-cmp-opcodes 1 'string)
  (check-arrayp-cmp-opcodes 1 'base-string) ; widetags differing in one bit
  #+sb-unicode
  (check-arrayp-cmp-opcodes 1 'sb-kernel::character-string) ; ditto
  (check-arrayp-cmp-opcodes 1 'simple-string) ; 2 adjacent widetags
  (check-arrayp-cmp-opcodes 1 '(and string (not simple-array)))
  (check-arrayp-cmp-opcodes 1 'simple-base-string)
  #+sb-unicode
  (check-arrayp-cmp-opcodes 1 'sb-kernel::simple-character-string)
  ;; FIXME: (AND STRING (NOT SIMPLE-STRING)) executs 4 tests
  ;; but it denotes the same set of objects as (AND STRING (NOT SIMPLE-ARRAY)).
  ;; The problem is with type algebra, not in the backend.

  ;; FIXME: this should be 1 comparison: widetag >= start-of-complex-widetags
  (check-arrayp-cmp-opcodes 2 '(and array (not simple-array)))

  ;; some other interesting pairs
  ;; This was passing just by random coincidence.
  ;; The widetag patterns no longer differ in exactly 1 bit.
  ;; Is there any real relevance to this test?
  #+nil
  (check-arrayp-cmp-opcodes 1 '(or (simple-array (unsigned-byte 8) (*))
                                    (simple-array (unsigned-byte 16) (*))))
  ;; FIXME: what's up with SIMPLE-UNBOXED-ARRAY requiring 1 SUB,
  ;; 3 CMPs, and a MOVZX. Something doesn't feel right.
  ;; In general, a lot of the array types are source-transforming to
  ;; (AND (SB-KERNEL:%OTHER-POINTER-P #:OBJECT0)
  ;;      (EQ (SB-KERNEL:%OTHER-POINTER-WIDETAG #:OBJECT0) something))
  )

(defun check-integerp-cmp-opcodes (expect type)
  (count-cmp-opcodes type expect
          `(lambda (x)
             (declare (optimize (sb-c::verify-arg-count 0)
                                #+sb-safepoint (sb-c::insert-safepoints 0)))
             (typep x ',type))))

(with-test (:name :typep-integer-doubleton)
  ;; This was taking 3 comparisons because it was a FIXNUMP test
  ;; and some range-based testing rather than just 2 EQ tests.
  (check-integerp-cmp-opcodes 2 '(integer 1 2)))

(defun typep-asm-code-length (type)
  (let* ((lines
          (disassembly-lines
           (compile nil
                    `(lambda (x)
                       (declare (optimize (sb-c::verify-arg-count 0) (debug 0)))
                       (typep x ',type)))))
         (callp
          (some (lambda (x) (or (search "#<FUNCTION" x)
                                (search "#<FDEFN" x)))
                lines)))
    (values (length lines) callp)))

;;; Counting instructions of assembly is sort of a very rough guess
;;; as to whether widetags are being tested as efficiently as possible.
;;; This file is for both human and machine consumption.
;;; #\! precedes any line where the type test involves a function call.
;;; It might be just a call. We might consider inlining some of those.
(defun write-golden-typep-data (input-name output-name)
  (with-open-file (input input-name)
    (with-open-file (output output-name :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (let ((*package* (find-package "SB-KERNEL"))
            (*print-pretty* nil))
        (loop (let ((type (read input nil input)))
                (when (eq type input) (return))
                (multiple-value-bind (linecount callp) (typep-asm-code-length type)
                  (format output "~a ~3d ~s~%"
                          (if callp "!" " ")
                          linecount type))))))))

#+nil
(write-golden-typep-data "../interesting-types.lisp-expr"
                         "typep-golden-data.txt")

(defun compare-to-golden-typep-data (pathname)
  (with-open-file (input pathname)
    (let ((*package* (find-package "SB-KERNEL")))
      (loop (let ((line (read-line input nil input)))
              (when (eq line input) (return))
              (with-input-from-string (stream line :start 2)
                (let ((expect-n (read stream))
                      (type (read stream)))
                  (multiple-value-bind (linecount callp) (typep-asm-code-length type)
                    (declare (ignore callp))
                    (when (/= linecount expect-n)
                      (warn "~S was ~d is ~d" type expect-n linecount))))))))))

(with-test (:name :many-interesting-array-types :skipped-on (:not :sb-unicode))
  (compare-to-golden-typep-data "typep-golden-data.txt"))

(with-test (:name :integerp->bignump-strength-reduction)
  (let ((f1 (compile nil '(lambda (x)
                           (typecase x (fixnum 'a) (integer 'b) (t 'c)))))
        (f2 (compile nil '(lambda (x)
                           (typecase x (fixnum 'a) (bignum 'b) (t 'c))))))
    (assert (= (length (disassembly-lines f1))
               (length (disassembly-lines f2))))))

(with-test (:name :boundp+symbol-value
            :skipped-on (not :sb-thread))
  ;; The vop combiner produces exactly one reference to SB-C::*COMPILATION*.
  ;; Previously there would have been one from BOUNDP and one from SYMBOL-VALUE.
  (let ((lines (disassembly-lines
                '(lambda ()
                  (if (boundp 'sb-c::*compilation*) sb-c::*compilation*) '(hi)))))
    (dolist (line lines)
      (assert (not (search "ERROR" line))))
    (assert (= (loop for line in lines
                     count (search "*COMPILATION*" line))
               1)))
  ;; Non-constant symbol works too now
  (let ((lines (disassembly-lines
                '(lambda (x) (if (boundp (truly-the symbol x)) x '(hi))))))
    (dolist (line lines)
      (assert (not (search "ERROR" line))))))

;;; We were missing the fndb info that fill-pointer-error doesn't return
;;; (not exactly "missing", but in the wrong package)
(with-test (:name :fill-pointer-no-return-multiple)
  (let ((lines (disassembly-lines '(lambda (x) (fill-pointer x)))))
    (dolist (line lines)
      (assert (not (search "RETURN-MULTIPLE" line))))))

(with-test (:name :elide-zero-fill)
  (let* ((f (compile nil '(lambda () (make-array 100 :initial-element 0))))
         (lines (disassembly-lines f)))
    (dolist (line lines)
      (assert (not (search "REPE STOSQ" line))))))

;;; Word-sized stores (or larger, like double-float on 32-bit) would cons a new lisp object
(with-test (:name :sap-set-does-not-cons)
  (loop for (type accessor telltale) in
        '((sb-vm:word sb-sys:sap-ref-word "ALLOC-UNSIGNED-BIGNUM")
          (double-float sb-sys:sap-ref-double "ALLOC-TRAMP"))
        do (let* ((positive-test
                   (compile nil `(lambda (sap) (,accessor sap 0))))
                  (negative-test
                   (compile nil `(lambda (sap obj) (setf (,accessor sap 0) obj)))))
             ;; Positive test ensures we know the right telltale for the type
             ;; in case the allocation logic changes
             (assert (loop for line in (disassembly-lines positive-test)
                           thereis (search telltale line)))
             (assert (not (loop for line in (disassembly-lines negative-test)
                                thereis (search telltale line)))))))

(with-test (:name :bash-copiers-byte-or-larger)
  (dolist (f '(sb-kernel::ub8-bash-copy
               sb-kernel::ub16-bash-copy
               sb-kernel::ub32-bash-copy
               sb-kernel::ub64-bash-copy))
    ;; Should not call anything
    (assert (not (ctu:find-code-constants (symbol-function f))))))

(defstruct bitsy
  (fix 0 :type fixnum)
  (sw 0 :type sb-vm:signed-word))

(defun s62 (x) (logtest (ash 1 62) (bitsy-fix x)))
(compile 's62)
(with-test (:name :lp-1939897)
  (assert (not (s62 (make-bitsy :fix (ash 1 61))))))

(defmacro try-logbitp-walking-bit-test
    (slot-name initarg nbits most-negative-value)
  `(let ((functions (make-array ,nbits)))
     (flet ((bit-num-to-value (b)
              (if (= b ,(1- nbits)) ,most-negative-value (ash 1 b))))
       (loop for bit-index from 0 below ,nbits
          do (setf (aref functions bit-index)
                   (compile nil `(lambda (obj)
                                  (values (logtest ,(bit-num-to-value bit-index)
                                           (,',slot-name obj))
                                   (logbitp ,bit-index (,',slot-name obj)))))))
       (loop for set-bit-index from 0 below ,nbits
          do (let ((struct (make-bitsy ,initarg
                                       (bit-num-to-value set-bit-index))))
               (loop for test-bit-index from 0 below ,nbits
                  do (multiple-value-bind (value1 value2)
                         (funcall (aref functions test-bit-index) struct)
                       ;; The expressions should agree at each bit
                       (assert (eq value1 value2))
                       ;; And should give the right answer
                       (if (= test-bit-index set-bit-index)
                           (assert value1)
                           (assert (not value1))))))))))

(with-test (:name :logbitp-vs-logtest-exhaustive-test)
  (try-logbitp-walking-bit-test bitsy-fix :fix 63
                                most-negative-fixnum)
  (try-logbitp-walking-bit-test bitsy-sw  :sw  64
                                (sb-c::mask-signed-field 64 (ash 1 63))))

#+allocator-metrics ; missing symbols if absent feature
(with-test (:name :allocator-histogram-bucketing)
  (let* ((min 0) (max 5000)
         (var-fun (compile nil '(lambda (n) (make-array (the fixnum n))))))
    (loop for n-elements from min to max
       do
         (let* ((fixed-fun (compile nil `(lambda () (make-array ,n-elements))))
               (h1 (progn
                     (sb-thread::reset-allocator-histogram)
                     (funcall fixed-fun)
                     (first (sb-thread::allocator-histogram))))
               (h2 (progn
                     (sb-thread::reset-allocator-histogram)
                     (funcall var-fun n-elements)
                     (first (sb-thread::allocator-histogram)))))
           ;; We have to allow for the possibilty of either or both MAKE-ARRAY calls causing
           ;; a GC to occur immediately thereafter, which allocates a cons for the GC epoch.
           ;; So if there is an extra element in bin 0, just remove it.
           (when (and (> (aref h1 0) 0)
                      (= (loop for value across h1 sum value) 2))
             (decf (aref h1 0)))
           (when (and (> (aref h2 0) 0) (= (loop for value across h2 sum value) 2))
             (decf (aref h2 0)))
           (unless (and (= (loop for value across h1 sum value) 1) ; exactly 1 bucket is nonzero
                        (equalp h1 h2))
             (let ((*print-length* nil))
               (format t "h1=~s~%h2=~s~%" h1 h2)
               (error "Error on n-elements = ~d" n-elements)))))))

(with-test (:name :uniquify-fixups)
  (let* ((f (let ((sb-c::*compile-to-memory-space* :dynamic))
              (compile nil
                       '(lambda (x)
                         `(,(list 1 2) ,(cons 1 2) ,(list nil x) ,(list '(a) #\x))))))
         (fixups
          (sb-c::unpack-code-fixup-locs
           (sb-vm::%code-fixups (sb-kernel:fun-code-header f)))))
    ;; There are 5 call outs to the fallback allocator, but only 2 (or 3)
    ;; fixups to the asm routines, because of uniquification per code component.
    ;; 2 of them are are CONS->RNN and CONS->R11
    ;; the other is ENABLE-ALLOC-COUNTER which may or may not be present
    (assert (<= (length fixups) 3))))

(defstruct submarine x y z)
(defstruct (moreslots (:include submarine)) a b c)
(declaim (ftype (function () double-float) get-dbl))
(with-test (:name :write-combining-instance-set)
  (let* ((f (compile nil '(lambda (s)
                            (setf (submarine-x (truly-the submarine s)) 0
                                  (submarine-y s) 0
                                  (submarine-z s) 0))))
         (lines (disassembly-lines f)))
    (assert (= 1 (loop for line in lines count (search "MOVUPD" line))))
    (assert (= 1 (loop for line in lines count (search "MOVSD" line)))))
  (let* ((f (compile nil '(lambda (s)
                            (setf (moreslots-a (truly-the moreslots s)) 0
                                  (submarine-x s) 0
                                  (moreslots-c s) 0))))
         (lines (disassembly-lines f)))
    (assert (= 3 (loop for line in lines count (search "MOVSD" line)))))
  ;; This was crashing in the MOV emitter (luckily) because it received
  ;; an XMM register due to omission of a MOVE-FROM-DOUBLE to heap-allocate.
  (compile nil
           '(lambda (sub a)
             (declare (submarine sub))
             (let ((fooval (+ (get-dbl) 23d0)))
               (setf (submarine-x sub) a
                     (submarine-y sub) fooval)
               a))))

#+immobile-code
(with-test (:name :no-static-linkage-if-notinline)
  ;; The normal state of the image has no "static" calls to FIND-PACKAGE
  ;; but also has no globally proclaimed NOTINLINE, because that would
  ;; suppress the optimization for CACHED-FIND-PACKAGE on a constant string.
  (assert (not (sb-vm::fdefn-has-static-callers (sb-int:find-fdefn 'find-package))))
  (assert (not (sb-int:info :function :inlinep 'find-package))))

(sb-vm::define-vop (trythis)
  (:generator 1
   (sb-vm::inst and sb-vm::rax-tn (sb-c:make-fixup nil :gc-barrier))))
(defun zook ()
  (sb-sys:%primitive trythis)
  nil)
