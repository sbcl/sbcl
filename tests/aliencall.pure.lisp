;;; Note that defining an alien type has no observable after-effect from the test
;;; because the randomly-named test package gets deleted, leaving only a weak
;;; ref in SB-ALIEN::*STRUCT-TYPE-CACHE which will get GCed.
(define-alien-type nil (struct myalien))

(with-test (:name :no-alien-sap-call)
  (let ((lines
         (ctu:disassembly-lines
          (compile nil
           '(lambda (x)
             (alien-funcall (extern-alien "printf"
                             (function void system-area-pointer (* (struct myalien))))
              ;; This lambda is never called, so lack of pinning here is fine.
              ;; (Didn't want to work around seeing STRING-TO-C-STRING in the disassembly
              ;; depending on #+/- unicode and all)
              (sb-sys:vector-sap "Ptr=%p\\n")
              x))))))
    #+x86-64
    (let (typep callout other)
      (dolist (line lines)
        (when (search "CALL" line)
          (cond ((search "ALIEN-VALUE-TYPEP" line) (setq typep t))
                ((search "printf" line) (setq callout t))
                (t (setq other t)))))
      (assert (and typep callout (not other))))

    ;; x86 disassembly is not as well-annotated as I'd like, so just check for two calls
    #+x86 (assert (= (loop for line in lines count (search "CALL" line)) 2))

    ;; arm64 shows comments, but correlating them to the BLR instruction requires thought
    ;;    F6FAFF58         LDR LEXENV, #x1001A10020 ; #<SB-KERNEL:FDEFN SB-ALIEN-INTERNALS:ALIEN-VALUE-TYPEP>
    ;;    970080D2         MOVZ NARGS, #4
    ;;    B80B00F9         STR NFP, [CFP, #16]
    ;;    DE9240F8         LDR LR, [LEXENV, #9]
    ;;    FD031BAA         MOV CFP, CSP
    ;; 1) C0033FD6         BLR LR
    ;;    ...
    ;;    53F9FF58         LDR R9, #x1001A10048     ; printf
    ;; 2) 60023FD6         BLR R9
    #+arm64 (assert (= (loop for line in lines count (search "BLR" line)) 2))))
