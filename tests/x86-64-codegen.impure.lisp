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

#-(and x86-64 immobile-space sb-thread) (sb-ext:exit :code 104) ; can't run these tests

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
      (setq lines (cddr lines)) ; remove "Disassembly for"
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

(with-test (:name :symeval-known-thread-local)
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

(with-test (:name :symeval-known-tls-index :skipped-on :immobile-symbols)
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
(assert (sb-kernel:immobile-space-obj-p '*blub*))
#-immobile-symbols (assert (not (sb-kernel:immobile-space-obj-p 'blub)))

(with-test (:name :symeval-unknown-tls-index :skipped-on :immobile-symbols)
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
                       sb-kernel:%make-funcallable-instance)
                      ("UNDEFINED-FDEFN"
                       sb-kernel:make-fdefn)))
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
            :broken-on (not :sb-dynamic-core))
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
(with-test (:name :set-symbol-value-imm-2)
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

(with-test (:name :list-vop-immediate-to-mem)
  (let ((lines
         (split-string
          (with-output-to-string (s)
            (let ((sb-disassem:*disassem-location-column-width* 0))
              (disassemble '(lambda () (list :key :test)) :stream s)))
          #\newline)))
    (assert (loop for line in lines
                  thereis (and (search "MOV QWORD PTR [" line)
                               (search ":KEY" line))))))

(with-test (:name :aprof-smoketest-struct)
  (let ((nbytes
         (let ((*standard-output* (make-broadcast-stream)))
           (sb-aprof:aprof-run
            (checked-compile
             '(sb-int:named-lambda "test" ()
                (declare (inline sb-thread:make-mutex)
                         (optimize sb-c::instrument-consing))
                (loop repeat 50 collect (sb-thread:make-mutex))))))))
    (assert (= nbytes
                (+ (* 51 2 sb-vm:n-word-bytes) ; list (extra for dummy head)
                   (* 50 (sb-vm::primitive-object-size
                          (sb-thread:make-mutex))))))))

(with-test (:name :aprof-smoketest-non-constant-size-vector)
  (let ((nbytes
         (let ((*standard-output* (make-broadcast-stream)))
           (sb-aprof:aprof-run
            (checked-compile
             '(sb-int:named-lambda "test" (&optional (n 10))
                (declare (optimize sb-c::instrument-consing))
                (make-array (the (mod 64) n))))))))
    (assert (= nbytes (* 12 sb-vm:n-word-bytes)))))

;;; The profiler's disassembler expected to see a store at alloc-ptr
;;; or that + n-word-bytes, when in fact the code might write to 1 byte
;;; positioned anywhere in the word after the object header.
(with-test (:name :aprof-smoketest-bit-vector)
  (let ((nbytes
         (let ((*standard-output* (make-broadcast-stream)))
           (sb-aprof:aprof-run
            (checked-compile
             '(sb-int:named-lambda "test" ()
                (declare (optimize sb-c::instrument-consing))
                (make-array (* 128 16) :element-type 'bit)))))))
    (assert (= nbytes (sb-vm::primitive-object-size
                       (make-array (* 128 16) :element-type 'bit))))))

(with-test (:name :aprof-smoketest-large-vector)
  (let ((nbytes
         (let ((*standard-output* (make-broadcast-stream)))
           (sb-aprof:aprof-run
            (checked-compile
             '(sb-int:named-lambda "test" ()
               (declare (optimize sb-c::instrument-consing))
               (make-array 45000)))))))
    (assert (= nbytes (* (+ 45000 sb-vm:vector-data-offset)
                         8)))))

sb-vm::
(define-vop (cl-user::alloc-to-r8)
  (:temporary (:sc any-reg :offset r8-offset :from :eval) result)
  (:node-var node)
  (:generator 1
    (let* ((bytes large-object-size) ; payload + header total
           (words (- (/ bytes n-word-bytes) vector-data-offset)))
      (instrument-alloc bytes node)
      (pseudo-atomic ()
       (allocation result bytes node nil other-pointer-lowtag)
       (storew* simple-array-unsigned-byte-64-widetag result 0
                other-pointer-lowtag t)
       (storew* (fixnumize words) result vector-length-slot
                other-pointer-lowtag t)))))

(with-test (:name :aprof-smoketest-large-vector-to-upper-register)
  (let ((nbytes
         (let ((*standard-output* (make-broadcast-stream)))
           (sb-aprof:aprof-run
            (checked-compile
             '(sb-int:named-lambda "test" ()
               (declare (optimize sb-c::instrument-consing))
               (sb-sys:%primitive cl-user::alloc-to-r8)
               nil))))))
    (assert (= nbytes sb-vm:large-object-size))))

