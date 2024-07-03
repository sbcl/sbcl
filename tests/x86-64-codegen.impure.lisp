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

#-x86-64 (invoke-restart 'run-tests::skip-file)

(load "compiler-test-util.lisp")
(import 'ctu:disassembly-lines)

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

(sb-vm::define-vop (tryme)
    (:generator 1 (sb-assem:inst mov :byte (sb-vm::ea :gs sb-vm::rax-tn) 0)))
(with-test (:name :try-gs-segment)
  (assert (loop for line in (disassembly-lines
                             (compile nil
                                      '(lambda () (sb-sys:%primitive tryme))))
                thereis (search "MOV BYTE PTR GS:[RAX]" line))))

(defun strip-assem-junk (fun &optional (remove-epilogue t))
    (let ((lines (disassembly-lines (compile nil fun))))
      ;; For human-readability, kill the whitespace
      (setq lines (mapcar (lambda (x) (string-left-trim " ;" x)) lines))
      (when (string= (car (last lines)) "")
        (setq lines (nbutlast lines)))
      ;; Remove safepoint traps
      (setq lines (remove-if (lambda (x) (search "; safepoint" x)) lines))
      ;; If the last 3 lines are of the expected form
      ;;   LEAVE / CLC / RET
      ;; then strip them out
      (if (and remove-epilogue
               (let ((last3 (subseq lines (- (length lines) 3))))
                 (and (search "LEAVE" (first last3))
                      (search "CLC" (second last3))
                      (search "RET" (third last3)))))
          (butlast lines 3)
          lines)))
(defun disasm-load (safety symbol)
  ;; This lambda has a name because if it doesn't, then the name
  ;; is something stupid like (lambda () in ...) which pretty-prints
  ;; on a random number of lines.
  (strip-assem-junk   `(sb-int:named-lambda test ()
                         (declare (optimize (debug 0) (safety ,safety)
                                            (sb-c:verify-arg-count 0)))
                         ,symbol)))
(defun disasm-store (sexpr)
  (strip-assem-junk   `(sb-int:named-lambda test (x)
                         (declare (ignorable x))
                         (declare (optimize (debug 0)
                                            (sb-c:verify-arg-count 0)))
                         ,sexpr)))

(with-test (:name :symeval-known-thread-local
            :skipped-on (not :sb-thread))
  ;; It should take 1 instruction to read a known thread-local var
  (assert (= (length (disasm-load 1 'sb-thread:*current-thread*)) 1))
  (assert (= (length (disasm-load 1 'sb-sys:*interrupt-pending*)) 1))
  (assert (= (length (disasm-load 1 'sb-kernel:*gc-inhibit*)) 1))
  (assert (= (length (disasm-load 1 'sb-kernel:*restart-clusters*)) 1))
  (assert (= (length (disasm-load 1 'sb-kernel:*handler-clusters*)) 1)))

(with-test (:name :set-known-thread-local :skipped-on (or (not :immobile-space)
                                                          (not :sb-thread)))
  ;; It should take 1 instruction to write a known thread-local var
  (assert (= (length (disasm-store '(setq sb-kernel:*gc-inhibit* x))) 1)))

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
  (let ((text (disasm-load 0 '*print-base*)))
    (assert (= (length text) 3)) ; number of lines
    ;; two lines should be annotated with *PRINT-BASE*
    (assert (= (loop for line in text count (search "*PRINT-BASE*" line)) 2)))

  ;; When symbol SC is CONSTANT:
  ;;    498B9578290000     MOV RDX, [R13+disp]       ; tls: FOO
  ;;    488B059EFFFFFF     MOV RAX, [RIP-98]         ; 'FOO
  ;;    83FA61             CMP EDX, 97
  ;;    480F4450F9         CMOVEQ RDX, [RAX-7]
  (let ((text (disasm-load 0 'foo)))
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
  (let ((text (disasm-load 0 '*blub*)))
    (assert (= (length text) 4))
    ;; two lines should be annotated with *BLUB*
    (assert (= (loop for line in text count (search "*BLUB*" line)) 2)))

  ;; When symbol SC is constant:
  ;;    488B05B3FFFFFF     MOV RAX, [RIP-77]          ; 'BLUB"
  ;;    8B50F5             MOV EDX, [RAX-11]
  ;;    4A8B142A           MOV RDX, [RDX+R13]
  ;;    83FA61             CMP EDX, 97
  ;;    480F4450F9         CMOVEQ RDX, [RAX-7]
  (assert (= (length (disasm-load 0 'blub)) 5)))

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
      (assert (search "; #<SB-KERNEL:LAYOUT " line))
      (assert (search " SB-ASSEM:LABEL" line)))))

(with-test (:name :c-call :skipped-on :win32)
  (let* ((lines (split-string
                 (with-output-to-string (s)
                   (let ((sb-disassem:*disassem-location-column-width* 0))
                     (disassemble 'sb-sys:deallocate-system-memory :stream s)))
                 #\newline))
         (c-call (find "os_deallocate" lines :test #'search)))
    ;; Depending on #+immobile-code it's either direct or memory indirect.
    #+immobile-code (assert (search "CALL #x" c-call))
    #-immobile-code (assert (search "CALL [#x" c-call))))

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
  (sb-vm::allocate-code-object :immobile 4 (* 2 1024 1024)))

(defun bbb (x y z)
  ;; I don't want the number of expected comparisons to depend on whether
  ;; the code gets disassembled with versus without the initial segment.
  (declare (optimize (sb-c:verify-arg-count 0)))
  (if x
      (ecase y
        (-2 'a) (2 'b) (3 'c) (4 (error "no")) (5 'd) (6 'e) (7 'wat) (8 '*) (9 :hi))
      (case z
        (#\a :a) (#\b :b) (#\e :c) (#\f (print :d)) (#\g :e) (#\h :f) (t nil))))

(defun try-case-known-fixnum (x)
  (declare (optimize (sb-c:verify-arg-count 0)))
  (case (the fixnum x)
    (0 :a) (1 :b) (2 :c) (5 (print :d)) (6 :c) (-1 :blah)))
(defun try-case-maybe-fixnum (x)
  (when (typep x 'fixnum)
    (case x
      (0 :a) (1 :b) (2 :c) (5 :d) (6 (print :c)) (-1 :blah))))

(defun try-case-known-char (x)
  (declare (optimize (sb-c:verify-arg-count 0)))
  (case (the character x)
    (#\a :a) (#\b :b)(#\c :c) (#\d :d) (#\e (print :e)) (#\f :b)))
(defun try-case-maybe-char (x)
  (declare (optimize (sb-c:verify-arg-count 0)))
  (when (characterp x)
    (case x (#\a :a) (#\b :b)(#\c :c) (#\d :d) (#\e :e) (#\f (print :a)))))

(defun expect-n-comparisons (fun-name howmany)
  (let ((lines
          (split-string
           (with-output-to-string (s) (disassemble fun-name :stream s))
           #\newline)))
    (assert (= (loop for line in lines count (search "CMP" line))
               howmany))))

(with-test (:name :multiway-branch-generic-eq)
  ;; there's 1 test of NIL, 1 test of character-widetag, and 2 limit checks
  ;; and 1 for comparing the key
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
                                  ((2) :e) ((3) (print :f))))
                              :allow-style-warnings t))))
    (assert (search "JUMP-TABLE" s)))
  ;; There are too few cases after duplicate removal to be considered multiway
  (let ((s (with-output-to-string (sb-c::*compiler-trace-output*)
             (checked-compile '(lambda (b) (case b ((0) :a) ((0) :b) ((0) :c) ((1) (print :d))))
                              :allow-style-warnings t))))
    (assert (not (search "JUMP-TABLE" s)))))

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
    ;; There is not a conditional branch per symbol
    (assert (= (count-assembly-labels lines) 1))))

;;; Assert that the ECASE-FAILURE vop emits a trap and that we don't call ERROR
;;; (cutting down on the code size for each ECASE)
(with-test (:name :ecase-failure-trap)
  ;; test ECASE
  (assert (ctu:asm-search "ECASE-FAILURE-ERROR"
                                  `(lambda (x)
                                     (ecase x (:a 1) (:b 2) (:c 3)))))
  ;; test ETYPECASE
  (assert (ctu:asm-search "ETYPECASE-FAILURE-ERROR"
                                  `(lambda (x)
                                     (etypecase x
                                       ((integer 1 20) 'hi)
                                       ((cons (eql :thing)) 'wat)
                                       (bit-vector 'hi-again))))))

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

(with-test (:name :peephole-optimizations-1 :skipped-on :sbcl)
  ;; The test does not check that both the load and the shift
  ;; have been sized as :dword instead of :qword, but it should
  ;; FIXME: this test was supposed to assert that
  ;; "AND r, -2 ; AND r, n" combines the two ANDs into one,
  ;; but there is no longer an AND in the symbol-hash vop
  ;; so I need to find a different test case.
  (let ((f '(lambda (x)
             ;; eliminate arg count check, type check
             (declare (optimize speed (safety 0)))
             (ldb (byte 3 0) (sb-kernel:symbol-hash x)))))
    (let ((unoptimized (let ((sb-c::*do-instcombine-pass* nil))
                         (checked-compile f)))
          (instcombined (checked-compile f)))
      (assert (= (count-assembly-lines instcombined)
                 (- (count-assembly-lines unoptimized) 1)))))

  ;; Likewise this was "AND RDX, -2 ; SAR RDX, 2 ; AND RDX, -2 ; AND RDX, 62"
  ;; becoming "SHR EDX, 2 ; AND EDX, 62"
  (let ((f '(lambda (x)
             ;; eliminate arg count check, type check
             (declare (optimize speed (safety 0)))
             (ldb (byte 5 2) (sb-kernel:symbol-hash x)))))
    (let ((unoptimized (let ((sb-c::*do-instcombine-pass* nil))
                         (checked-compile f)))
          (instcombined (checked-compile f)))
      (assert (= (count-assembly-lines instcombined)
                 (- (count-assembly-lines unoptimized) 2))))))

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
                    (sb-kernel:classoid-name (sb-kernel:layout-classoid x)))
                  (ctu:find-code-constants #'sb-kernel:%%typep :type 'sb-kernel:layout))))
    (assert (null (set-difference names
                                  '(sb-kernel:ctype
                                    sb-kernel:unknown-type
                                    sb-kernel:fun-designator-type
                                    sb-c::abstract-lexenv
                                    sb-kernel::classoid-cell
                                    sb-kernel:layout
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
             thereis (and (search "LAYOUT for" line)
                          (search "CMP DWORD PTR" line)))))

(with-test (:name :thread-local-unbound)
  (declare (optimize safety))
  (let ((c (nth-value 1 (ignore-errors sb-c::*compilation*))))
    (assert (eq (cell-error-name c) 'sb-c::*compilation*))))

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
  (check-arrayp-cmp-opcodes 1 '(and string (not simple-array)))
  (check-arrayp-cmp-opcodes #+sb-unicode 1
                            #-sb-unicode 2
                            '(and array (not simple-array)))

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

(defun show-pretty-lines (type lines)
  (format t ";;;; Type: ~s~%" type)
  (dolist (line lines)
    (when (plusp (length line))
      (let* ((label
              (if (char= (char line 0) #\L)
                  (subseq line 0 (1+ (position #\: line)))
                  ""))
             (opcode-bytes
              (progn
                (setq line (string-left-trim " " (subseq line (length label))))
                (subseq line 0 (position #\Space line)))))
        (setq line (string-left-trim " " (subseq line (length opcode-bytes))))
        (format t "; ~4a~20a~a~%"
                label opcode-bytes line))))
  (terpri))

(defun typep-asm-code-length (type &optional print)
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
    (when print (show-pretty-lines type lines))
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

;;; FIXME: Some time after the MANY-INTERESTING-ARRAY-TYPES test
;;; got disabled, the code size changed either due to a regression,
;;; or more aggressive inlining of ">=" and "<="
;;; which now causes these warnings. Figure out the new baseline.
;;
;; WARNING: (AND INTEGER (NOT (SIGNED-BYTE 31))) was 32 is 38
;; WARNING: (AND INTEGER (NOT (SIGNED-BYTE 32))) was 32 is 38
;; WARNING: (INTEGER (0) *) was 18 is 22
;; WARNING: (INTEGER * -1) was 18 is 22
;; WARNING: (INTEGER * -1000000) was 18 is 22
;; WARNING: (INTEGER * -1073741825) was 18 is 23
;; WARNING: (INTEGER * -12) was 18 is 22
;; WARNING: (INTEGER * -14) was 18 is 22
;; WARNING: (INTEGER * -17) was 18 is 22
;; WARNING: (INTEGER * -2) was 18 is 22
;; WARNING: (INTEGER * -2147483649) was 18 is 23
;; WARNING: (INTEGER * -4611686018427387901) was 18 is 23
;; WARNING: (INTEGER * -54043195528445951) was 18 is 23
;; WARNING: (INTEGER * -541073411) was 18 is 22
;; WARNING: (INTEGER * -6) was 18 is 22
;; WARNING: (INTEGER * -9) was 18 is 22
;; WARNING: (INTEGER * 0) was 24 is 22
;; WARNING: (INTEGER * 20) was 18 is 22
;; WARNING: (INTEGER * 2047) was 18 is 22
;; WARNING: (INTEGER * 2147483647) was 18 is 23
;; WARNING: (INTEGER * 4611686018427387900) was 18 is 23
;; WARNING: (INTEGER * 576460752303423487) was 18 is 23
;; WARNING: (INTEGER * 65535) was 18 is 22
;; WARNING: (INTEGER -1) was 18 is 22
;; WARNING: (INTEGER 1) was 18 is 22
;; WARNING: (INTEGER 1073741824) was 18 is 23
;; WARNING: (INTEGER 1152921504606846961) was 18 is 23
;; WARNING: (INTEGER 1899) was 18 is 22
;; WARNING: (INTEGER 2) was 18 is 22
;; WARNING: (INTEGER 2147483648) was 18 is 23
;; WARNING: (INTEGER 2305843009214) was 18 is 23
;; WARNING: (INTEGER 4611686018427387900) was 18 is 23
;; WARNING: (INTEGER 4611686018427387901) was 18 is 23
;; WARNING: (INTEGER 4611686018427387902) was 18 is 23
;; WARNING: (INTEGER 4611686018427387903) was 18 is 23
;; WARNING: (INTEGER 63) was 18 is 22
(defun compare-to-golden-typep-data (pathname &optional print)
  (with-open-file (input pathname)
    (let ((*package* (find-package "SB-KERNEL")))
      (loop (let ((line (read-line input nil input)))
              (when (eq line input) (return))
              (with-input-from-string (stream line :start 2)
                (let ((expect-n (read stream))
                      (type (read stream)))
                  (multiple-value-bind (linecount callp)
                      (typep-asm-code-length type print)
                    (declare (ignore callp))
                    (when (/= linecount expect-n)
                      (warn "~S was ~d is ~d" type expect-n linecount))))))))))

#+nil ; gotta figure out how to make this insensitive to standard asm boilerplate
(with-test (:name :many-interesting-array-types
                  :skipped-on (:or (:not :sb-unicode)
                                   (:not :immobile-space)))
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
                  (declare (optimize safety))
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

(sb-vm::define-vop (trythis)
  (:generator 1
   (sb-vm::inst and sb-vm::rax-tn (sb-c:make-fixup nil :card-table-index-mask))))
(defun zook ()
  (sb-sys:%primitive trythis)
  nil)

;;; Previously INITIALIZE-VECTOR would move every element into
;;; a register, causing each to require a store barrier
;;; and then a register-to-memory move like so:
;;;     BB02000000       MOV EBX, 2
;;;     488D4601         LEA RAX, [RSI+1]
;;;     48C1E80A         SHR RAX, 10
;;;     25FFFF0F00       AND EAX, 1048575
;;;     41C6040400       MOV BYTE PTR [R12+RAX], 0
;;;     48895E01         MOV [RSI+1], RBX
;;; The improved code emits 4 consecutive MOV instructions,
;;; one per item. It is still suboptimal in that it can not discern
;;; between initializing and updating, so it always uses a :QWORD move
;;; despite the prezeroed pages.
(with-test (:name :init-vector-mov-to-mem
            :skipped-on :debug-gc-barriers)
  (let* ((lines (disassembly-lines
                 '(lambda () (vector #\x 1 2 3))))
         (magic-value
          (write-to-string
           (logior (ash (char-code #\x) 8) sb-vm:character-widetag)))
         (start
          (position-if
           (lambda (line) (search magic-value line))
           lines)))
    (assert start)
    (assert (search ", 2" (nth (+ start 1) lines)))
    (assert (search ", 4" (nth (+ start 2) lines)))
    (assert (search ", 6" (nth (+ start 3) lines)))))

(defglobal *myglobalvar* 3)
(with-test (:name :disassemble-symbol-global-value)
  (assert (loop for line in (disassembly-lines '(lambda () *myglobalvar*))
                thereis (search "*MYGLOBALVAR*" line))))

(defun compiler-trace-output-lines (lexpr)
  (let ((string-stream (make-string-output-stream)))
    (let ((sb-c::*compiler-trace-output* string-stream)
          (sb-c::*compile-trace-targets* '(:vop))
          (*print-pretty* nil))
      (compile nil lexpr))
    (split-string (get-output-stream-string string-stream)
                  #\newline)))

;;; Return the list of vops (as strings) that emitted a GC barrier.
(defun find-gc-barriers (lambda-expression)
  (let ((lines
         (compiler-trace-output-lines lambda-expression))
        (current-vop)
        (result))
    ;; FIXME: can this miss vops on a label line, or do we always print labels
    ;; on their own line?
    (dolist (line lines (nreverse result))
     (let ((line (pop lines)))
       (cond ((and (>= (length line) 4) (string= line "VOP " :end1 4))
              (let ((string (subseq line 4 (position #\space line :start 5))))
                (setq current-vop string)))
             ((search ":FLAVOR CARD-TABLE-INDEX-MASK" line)
              (push current-vop result)))))))

(with-test (:name :closure-init-gc-barrier)
  (let ((vops-with-barrier
         (find-gc-barriers
           '(lambda (address)
              (let ((sap (sb-sys:int-sap (truly-the sb-vm:word address))))
                ;; The compiler conses the SAP *after* making the closure, so the store
                ;; of the sap into the closure needs a barrier, as it's an old->young store.
                (lambda () sap))))))
    (assert (equal vops-with-barrier '("CLOSURE-INIT")))))

(defstruct (point (:constructor make-point (x))) x (y 0) (z 0))
(with-test (:name :structure-init-gc-barrier)
  (let ((vops-with-barrier
          (find-gc-barriers
           '(lambda (val)
             (declare (double-float val) (inline make-point))
             ;; Similarly to the closure-init test, DOUBLE-FLOAT reg is cast to
             ;; DESCRIPTOR-REG only after making the object. This seems like a
             ;; low-hanging-fruit optimization to flip the order.
             (let ((neg (- val))) (make-point neg))))))
    (assert (equal vops-with-barrier '("SET-SLOT")))))

(with-test (:name :system-tlabs :skipped-on (not :system-tlabs))
  (assert (loop for line in (disassembly-lines 'sb-impl:test-make-packed-info)
                thereis (search "SYS-ALLOC-TRAMP" line)))
  (assert (loop for line in (disassembly-lines 'sb-impl:test-copy-packed-info)
                thereis (search "SYS-ALLOC-TRAMP" line)))
  (let ((f (compile nil '(lambda (x)
                          (declare (sb-c::tlab :system))
                          (sb-pcl::%copy-cache x)))))
    (assert (loop for line in (disassembly-lines f)
                  thereis (search "SYS-ALLOC-TRAMP" line)))))

(defun find-in-disassembly (string lambda-expression)
  (let ((disassembly
         (with-output-to-string (s)
           (disassemble (compile nil lambda-expression) :stream s))))
    (loop for line in (split-string disassembly #\Newline)
          thereis (search string line))))

#+immobile-space
(with-test (:name :disassemble-alien-linkage-table-ref
                  :fails-on (or (not :sb-thread) :win32))
  (dolist (memspace '(:dynamic :immobile))
    (let ((sb-c::*compile-to-memory-space* memspace))
      (assert (find-in-disassembly
               (if (eq sb-c::*compile-to-memory-space* :immobile) "lose" "&lose")
               '(lambda ()
                 (declare (optimize (sb-c::alien-funcall-saves-fp-and-pc 0)))
                 (alien-funcall (extern-alien "lose" (function void))))))
      (assert (find-in-disassembly
               "&verify_gens"
               '(lambda ()
                 (extern-alien "verify_gens" char)))))))

;;; This tests that the x86-64 disasembler does not crash
;;; on LEA with a rip-relative operand and no label.
(with-test (:name (disassemble :no-labels))
  (let* ((lines
          (split-string
           (with-output-to-string (stream)
             ;; A smallish function whose code happens to contain
             ;; the thing under test.
             (disassemble 'sb-impl::inspector :stream stream))
          #\Newline))
         (line (find "; = L0" lines :test 'search)))
    (assert (search "LEA " line)) ; verify our test precondition
    ;; Now just disassemble without labels and see that we don't crash
    (disassemble 'sb-impl::inspector
                 :use-labels nil
                 :stream (make-broadcast-stream))))

#+immobile-space
(with-test (:name (disassemble :static-call))
  (dolist (sym '(sb-kernel:two-arg-* sb-kernel:two-arg-/
                 sb-kernel:two-arg-+ sb-kernel:two-arg--))
    (let ((f (let ((sb-c:*compile-to-memory-space* :dynamic))
               (compile nil `(lambda (x y) (,sym x y))))))
      (assert (loop for line in (disassembly-lines f)
                    thereis (search (princ-to-string sym) line))))))

(with-test (:name :closure-gc-barrier)
  (let ((vops-with-barrier
         (find-gc-barriers
           '(lambda ()
             (let ((c (cons 1 2)))
               (lambda (x)
                 (setf (car c) x)))))))
    (assert (equal vops-with-barrier '("SET-SLOT")))))

(defstruct (wordpair (:predicate nil) (:copier nil)) a b)
(defun func (x) (wordpair-b x))
(with-test (:name :non-stack-instance-set)
  ;; This is a trivial test of the control case for :stack-instance-set
  (let ((vops-with-barrier
         (find-gc-barriers
          '(lambda (x)
            (declare (inline make-wordpair))
            (let ((pair (make-wordpair :a 'foo)))
              (setf (wordpair-b pair) (car x))
              (values (func pair)))))))
    (assert (equal vops-with-barrier '("INSTANCE-INDEX-SET")))))

(with-test (:name :stack-instance-set)
  (let ((vops-with-barrier
         (find-gc-barriers
          '(lambda ()
            (declare (inline make-wordpair))
            (let ((pair (make-wordpair :a 'foo)))
              (declare (dynamic-extent pair))
              (setf (wordpair-b pair) "hi")
              (values (func pair)))))))
    (assert (not vops-with-barrier))))

(declaim (freeze-type wordpair))
(with-test (:name :stack-instance-copy)
  (let ((vops-with-barrier
         (find-gc-barriers
          '(lambda (x)
            (let ((copy (copy-structure (truly-the wordpair x))))
              (declare (dynamic-extent copy))
              (values (func copy)))))))
    (assert (not vops-with-barrier))))

;;; word-sized add, subtract, multiply vops which yield either a fixnum
;;; or bignum where the bignum can have 1, 2, or 3 bigdigits.
(defparameter unsigned-word-test-inputs
  (loop for i from 0 by (ash 1 56) repeat 256 collect i))

(defparameter signed-word-test-inputs
  (funcall
   (compile nil ;; no interpreter stubs
            `(lambda ()
               (loop for word in unsigned-word-test-inputs
                     collect (let ((b (sb-bignum:%allocate-bignum 1)))
                               (setf (sb-bignum:%bignum-ref b 0) word)
                               (sb-bignum::%normalize-bignum b 1)))))))

(defun check-result (fun x y actual)
  (let ((expect (funcall fun x y)))
    (unless (eql actual expect)
      (when (typep expect 'bignum)
        (sb-vm:hexdump expect)
        (terpri)
        (sb-vm:hexdump actual))
      (error "Failure @ ~X ~A ~X, expect ~D (~A) got ~D~%"
             x fun y expect
             (typecase expect
               (fixnum 'fixnum)
               (bignum (format nil "~d-word bignum"
                               (sb-bignum:%bignum-length expect))))
             actual))))

(macrolet ((test-op (op)
             `(progn
                (format t "~&Testing ~A~%" ',op)
                (dolist (x signed-word-test-inputs)
                  (dolist (y signed-word-test-inputs)
                    (check-result
                     ',op x y
                     (,op (the sb-vm:signed-word x) (the sb-vm:signed-word y))))))))
  (defun test-signed ()
    (test-op +)
    (test-op -)
    (test-op *)))

(macrolet ((test-op (op)
             `(progn
                (format t "~&Testing ~A~%" ',op)
                (dolist (x unsigned-word-test-inputs)
                  (dolist (y unsigned-word-test-inputs)
                    (check-result
                     ',op x y
                     (,op (the sb-vm:word x) (the sb-vm:word y))))))))
  (defun test-unsigned ()
    (test-op +)
    (test-op -)
    (test-op *)))

(with-test (:name :signed-vops) (test-signed))
(with-test (:name :unsigned-vops) (test-unsigned))

(with-test (:name :old-slot-set-no-barrier)
  (let ((vops-with-barrier
          (find-gc-barriers
           '(lambda (y)
             (let ((x (cons 0 0)))
               (setf (car x) y)
               x)))))
    (assert (not vops-with-barrier))))

(with-test (:name :smaller-than-qword-cons-slot-init
                  :skipped-on (:not :mark-region-gc))
  (let ((lines (disassembly-lines
                (compile nil '(lambda (a) (list 1 a #\a))))))
    (assert (loop for line in lines
                  thereis (search "MOV BYTE PTR" line))) ; constant 1
    (assert (loop for line in lines
                  thereis (search "MOV WORD PTR" line))) ; constant #\a
    (assert (loop for line in lines
                  thereis (search "MOV DWORD PTR" line))))) ; constant NIL

(defun count-labeled-instructions (function &aux (answer 0))
  (let ((lines (disassembly-lines function)))
    (dolist (line lines answer)
      (when (and (>= (length line) 3)
                 (char= (char line 0) #\L)
                 (let ((p (position #\: line)))
                   (every #'digit-char-p (subseq line 1 p))))
        (incf answer)))))

;;; Jump-to-jump elimination helps with conditional vops (those returning the result in EFLAGS)
;;; which also contain an internal jump, and naturally are followed by a conditional jump.
;;; It's usually possible to redirect the internal jump, if it has either exactly the same
;;; sense of or is exactly the negation of the jump that follows the vop.
(with-test (:name :jump-to-jump-elimination)
  ;; NON-NULL SYMBOL-P tests lowtag and widetag, and thus has 2 jumps
  ;; but there's only 1 target of a jump.
  (assert (= (count-labeled-instructions
              (checked-compile
               `(lambda (x)
                  (declare (optimize (sb-c::verify-arg-count 0)))
                  (if (sb-kernel:non-null-symbol-p x) 'zook (foo)))))
             1)))

(with-test (:name :disassemble-instance-type-test
            :skipped-on (not :immobile-space))
  (let ((lines
          (disassembly-lines
           (compile nil '(lambda (m) (the sb-thread:mutex m))))))
    (assert
     (loop for line in lines
           thereis (and (search "CMP DWORD PTR" line)
                        (search "#<LAYOUT" line)
                        (search "for SB-THREAD:MUTEX" line))))))
