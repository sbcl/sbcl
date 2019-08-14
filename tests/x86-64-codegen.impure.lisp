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

(defun disasm (safety expr &optional (remove-epilogue t))
  ;; This lambda has a name because if it doesn't, then the name
  ;; is something stupid like (lambda () in ...) which pretty-prints
  ;; on a random number of lines.
  (let ((fun (compile nil
                      `(sb-int:named-lambda test ()
                         (declare (optimize (debug 0) (safety ,safety)
                                            (sb-c::verify-arg-count 0)))
                         ,expr))))
    (sb-int:encapsulate 'sb-disassem::add-debugging-hooks 'test
                        (lambda (f &rest args) (declare (ignore f args))))
    (let ((lines
            (split-string
             (with-output-to-string (s)
               (let ((sb-disassem:*disassem-location-column-width* 0))
                 (disassemble fun :stream s)))
             #\newline)))
      (sb-int:unencapsulate 'sb-disassem::add-debugging-hooks 'test)
      (setq lines (cddr lines))         ; remove "Disassembly for"
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
  (assert (= (length (disasm 0 '*print-base*)) 3))

  ;; When symbol SC is CONSTANT:
  ;;    498B9578290000     MOV RDX, [R13+disp]       ; tls: FOO
  ;;    488B059EFFFFFF     MOV RAX, [RIP-98]         ; 'FOO
  ;;    83FA61             CMP EDX, 97
  ;;    480F4450F9         CMOVEQ RDX, [RAX-7]
  (assert (= (length (disasm 0 'foo)) 4)))

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
  (assert (= (length (disasm 0 '*blub*)) 4))

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
    (assert (search "; #<SB-KERNEL:LAYOUT for SB-ASSEM:LABEL" (nth (+ index 2) lines)))))

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
        (expect "#<FDEFN F>" lines)
        (expect "#<FDEFN G>" lines))
      (handler-bind ((warning #'muffle-warning))
        (defun g (x) (- x)))
      (let ((lines (disassembly-lines 'c)))
        (expect "#<FDEFN G>" lines)))))

(with-test (:name :c-call
            :skipped-on (not :sb-dynamic-core))
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
    (try '(cdr obj) '("hi" . 1) '("hi"))))

(with-test (:name :huge-code :skipped-on (not :immobile-code))
  (sb-vm::allocate-code-object :immobile 4 (* 2 1024 1024)))
