;;;; Reorganization of immobile code space.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-X86-64-ASM")

(defmacro do-immobile-functions ((code-var fun-var addr-var &key (if t)) &body body)
  ;; Loop over all code objects
  `(let* ((call (find-inst #xE8 (get-inst-space)))
          (jmp  (find-inst #xE9 (get-inst-space)))
          (dstate (make-dstate nil))
          (sap (int-sap 0))
          (seg (sb-disassem::%make-segment :sap-maker (lambda () sap))))
     (sb-vm::map-objects-in-range
      (lambda (,code-var obj-type obj-size)
        (declare (ignore obj-size))
        (when (and (= obj-type code-header-widetag) ,if)
          ;; Loop over all embedded functions
          (dotimes (fun-index (code-n-entries ,code-var))
            (let* ((,fun-var (%code-entry-point ,code-var fun-index))
                   (,addr-var (+ (get-lisp-obj-address ,fun-var)
                                 (- fun-pointer-lowtag)
                                 (ash simple-fun-code-offset word-shift))))
              (with-pinned-objects (sap) ; Mutate SAP to point to fun
                (setf (sap-ref-word (int-sap (get-lisp-obj-address sap))
                                    (- n-word-bytes other-pointer-lowtag))
                      ,addr-var))
              (setf (seg-virtual-location seg) ,addr-var
                    (seg-length seg) (%simple-fun-text-len ,fun-var fun-index))
              ,@body))))
      ;; Slowness here is bothersome, especially for SB-VM::REMOVE-STATIC-LINKS,
      ;; so skip right over all fixedobj pages.
      (ash varyobj-space-start (- n-fixnum-tag-bits))
      (%make-lisp-obj (sap-int *varyobj-space-free-pointer*)))))

(defun sb-vm::collect-immobile-code-relocs ()
  (let ((code-components
         (make-array 20000 :element-type '(unsigned-byte 32)
                           :fill-pointer 0 :adjustable t))
        (relocs
         (make-array 100000 :element-type '(unsigned-byte 32)
                            :fill-pointer 0 :adjustable t))
        ;; Look for these three instruction formats.
        (lea-inst (find-inst #x8D (get-inst-space)))
        (jmp-inst (find-inst #b11101001 (get-inst-space)))
        (cond-jmp-inst (find-inst #x800f (get-inst-space)))
        (call-inst (find-inst #b11101000 (get-inst-space)))
        (seg (sb-disassem::%make-segment
              :sap-maker (lambda () (error "Bad sap maker")) :virtual-location 0))
        (dstate (make-dstate nil)))
    (flet ((scan-function (fun-entry-addr fun-end-addr predicate)
             (setf (seg-virtual-location seg) fun-entry-addr
                   (seg-length seg) (- fun-end-addr fun-entry-addr)
                   (seg-sap-maker seg)
                   (let ((sap (int-sap fun-entry-addr))) (lambda () sap)))
             (map-segment-instructions
              (lambda (dchunk inst)
                (cond ((or (and (or (eq inst jmp-inst) (eq inst call-inst))
                                (funcall predicate
                                         (+ (near-jump-displacement dchunk dstate)
                                            (dstate-next-addr dstate))))
                           (and (eq inst cond-jmp-inst)
                                (funcall predicate
                                         (+ (near-cond-jump-displacement dchunk dstate)
                                            (dstate-next-addr dstate)))))
                       (vector-push-extend (dstate-cur-addr dstate) relocs))
                      ((eq inst lea-inst)
                       (let ((sap (funcall (seg-sap-maker seg))))
                         (aver (eql (sap-ref-8 sap (dstate-cur-offs dstate)) #x8D))
                         (let ((modrm (sap-ref-8 sap (1+ (dstate-cur-offs dstate)))))
                           (when (and (= (logand modrm #b11000111) #b00000101) ; RIP-relative mode
                                      (funcall predicate
                                               (+ (signed-sap-ref-32
                                                   sap (+ (dstate-cur-offs dstate) 2))
                                                  (dstate-next-addr dstate))))
                             (aver (eql (logand (sap-ref-8 sap (1- (dstate-cur-offs dstate))) #xF0)
                                        #x40)) ; expect a REX prefix
                             (vector-push-extend (dstate-cur-addr dstate) relocs)))))))
              seg dstate nil))
           (finish-component (code start-relocs-index)
             (when (> (fill-pointer relocs) start-relocs-index)
               (vector-push-extend (get-lisp-obj-address code) code-components)
               (vector-push-extend start-relocs-index code-components))))

      ;; Assembler routines contain jumps to immobile code.
      (let* ((code sb-fasl:*assembler-routines*)
             (origin (sap-int (code-instructions code)))
             (end (+ origin (%code-text-size code)))
             (relocs-index (fill-pointer relocs)))
        (dolist (range (sort (loop for range being each hash-value
                                   of (car (%code-debug-info code)) collect range)
                             #'< :key #'car))
          ;; byte range is inclusive bound on both ends
          (scan-function (+ origin (car range))
                         (+ origin (cadr range) 1)
                         (lambda (addr)
                           (and (not (<= origin addr end))
                                (immobile-space-addr-p addr)))))
        (finish-component code relocs-index))

      ;; Immobile space - code components can jump to immobile space,
      ;; read-only space, and C runtime routines.
      (sb-vm::map-allocated-objects
       (lambda (code type size)
         (declare (ignore size))
         (when (= type code-header-widetag)
           (let* ((text-origin (sap-int (code-instructions code)))
                  (text-end (+ text-origin (%code-text-size code)))
                  (relocs-index (fill-pointer relocs)))
             (dotimes (i (code-n-entries code) (finish-component code relocs-index))
               (let* ((fun (%code-entry-point code i))
                      (fun-text (+ (get-lisp-obj-address fun)
                                   (- fun-pointer-lowtag)
                                   (ash simple-fun-code-offset word-shift))))
                 (scan-function
                  fun-text (+ fun-text (%simple-fun-text-len fun i))
                  ;; Exclude transfers within this code component
                  (lambda (jmp-targ-addr)
                    (not (<= text-origin jmp-targ-addr text-end)))))))))
       :immobile))

    ;; Write a delimiter into the array passed to C
    (vector-push-extend 0 code-components)
    (vector-push-extend (fill-pointer relocs) code-components)
    (values code-components relocs)))

(defun sb-vm::statically-link-core (&key callers exclude-callers
                                         callees exclude-callees
                                         verbose)
  (flet ((match-p (name include exclude)
           (and (not (member name exclude :test 'equal))
                (or (not include) (member name include :test 'equal))))
         (ambiguous-name-p (fun funs)
           ;; Return T if FUN occurs more than once in FUNS
           (declare (simple-vector funs))
           (dotimes (i (length funs))
             (when (eq (svref funs i) fun)
               (loop (cond ((>= (incf i) (length funs))
                            (return-from ambiguous-name-p nil))
                           ((eq (svref funs i) fun)
                            (return-from ambiguous-name-p t))))))))
    (do-immobile-functions (code fun addr)
      (when (match-p (%simple-fun-name fun) callers exclude-callers)
        (dx-let ((printed-fun-name nil)
                 (code-header-funs (make-array (code-header-words code))))
          ;; Collect the called functions
          (do ((i code-constants-offset (1+ i)))
              ((= i (length code-header-funs)))
            (let ((obj (code-header-ref code i)))
              (when (fdefn-p obj)
                (setf (aref code-header-funs i) (fdefn-fun obj)))))
          ;; Loop over function's assembly code
          (map-segment-instructions
           (lambda (chunk inst)
             (when (or (eq inst jmp) (eq inst call))
               (let ((fdefn (sb-vm::find-called-object
                             (+ (near-jump-displacement chunk dstate)
                                (dstate-next-addr dstate)))))
                 (when (and (fdefn-p fdefn)
                            (let ((callee (fdefn-fun fdefn)))
                              (and (immobile-space-obj-p callee)
                                   (not (sb-vm::fun-requires-simplifying-trampoline-p callee))
                                   (match-p (%fun-name callee)
                                            callees exclude-callees)
                                   (not (ambiguous-name-p callee code-header-funs)))))
                   (let ((entry (sb-vm::fdefn-call-target fdefn)))
                     (when verbose
                       (let ((*print-pretty* nil))
                         (unless printed-fun-name
                           (format t "#x~X ~S~%" (get-lisp-obj-address fun) fun)
                           (setq printed-fun-name t))
                         (format t "  @~x -> ~s [~x]~%"
                                 (dstate-cur-addr dstate) (fdefn-name fdefn) entry)))
                     ;; Set the statically-linked flag
                     (setf (sb-vm::fdefn-has-static-callers fdefn) 1)
                     ;; Change the machine instruction
                     (setf (signed-sap-ref-32 (int-sap (dstate-cur-addr dstate)) 1)
                           (- entry (dstate-next-addr dstate))))))))
           seg dstate))))))

;;; While concurrent use of un-statically-link is unlikely, misuse could easily
;;; cause heap corruption. It's preventable by ensuring that this is atomic
;;; with respect to GC and other code attempting to change the same fdefn.
;;; The issue is that if the fdefn loses the pointer to the underlying code
;;; via (setf fdefn-fun) before we were done removing the static links,
;;; then there could be no remaining pointers visible to GC.
;;; The only way to detect the current set of references is to find uses of the
;;; current jump address, which means we need to fix them *all* before anyone
;;; else gets an opportunity to change the fdefn-fun of this same fdefn again.
(define-load-time-global *static-linker-lock*
    (sb-thread:make-mutex :name "static linker"))
(defun sb-vm::remove-static-links (fdefn)
  ; (warn "undoing static linkage of ~S" (fdefn-name fdefn))
  (sb-thread::with-system-mutex (*static-linker-lock* :without-gcing t)
    ;; If the jump is to FUN-ENTRY, change it back to FDEFN-ENTRY
    (let ((fun-entry (sb-vm::fdefn-call-target fdefn))
          (fdefn-entry (+ (get-lisp-obj-address fdefn)
                          (ash fdefn-raw-addr-slot word-shift)
                          (- other-pointer-lowtag))))
      ;; Examine only those code components which potentially use FDEFN.
      (do-immobile-functions
         (code fun addr :if (loop for i downfrom (1- (sb-kernel:code-header-words code))
                                  to sb-vm:code-constants-offset
                                  thereis (eq (sb-kernel:code-header-ref code i)
                                              fdefn)))
        (map-segment-instructions
         (lambda (chunk inst)
           (when (or (eq inst jmp) (eq inst call))
             ;; TRULY-THE because near-jump-displacement isn't a known fun.
             (let ((disp (truly-the (signed-byte 32)
                                    (near-jump-displacement chunk dstate))))
               (when (= fun-entry (+ disp (dstate-next-addr dstate)))
                 (let ((new-disp
                        (the (signed-byte 32)
                             (- fdefn-entry (dstate-next-addr dstate)))))
                   ;; CMPXCHG is atomic even when misaligned, and x86-64 promises
                   ;; that self-modifying code works correctly, so the fetcher
                   ;; should never see a torn write.
                   (%primitive sb-vm::signed-sap-cas-32
                               (int-sap (dstate-cur-addr dstate))
                               1 disp new-disp))))))
         seg dstate)))
    (setf (sb-vm::fdefn-has-static-callers fdefn) 0))) ; Clear static link flag

(in-package "SB-KERNEL")

(defun order-by-in-degree ()
  (let ((compiler-stuff (make-hash-table :test 'eq))
        (other-stuff (make-hash-table :test 'eq)))
    (flet ((pick-table (fun-name)
             (if (symbolp fun-name)
                 (let ((package (symbol-package fun-name)))
                   (if (member package
                               (load-time-value
                                (cons sb-assem::*backend-instruction-set-package*
                                      (mapcar 'find-package
                                              '("SB-C" "SB-VM" "SB-FASL"
                                                "SB-ASSEM" "SB-DISASSEM"
                                                "SB-REGALLOC")))
                                t))
                       compiler-stuff
                       other-stuff))
                 other-stuff))
           (hashtable-keys-sorted (table)
             (mapcar #'car
              (sort (%hash-table-alist table)
                    (lambda (a b)
                      (cond ((> (cdr a) (cdr b)) t) ; higher in-degree
                            ((< (cdr a) (cdr b)) nil) ; lower in-degree
                            ;; break ties by name, and failing that,
                            ;; by address (which = random)
                            (t
                             (let ((name1
                                    (%simple-fun-name (%code-entry-point (car a) 0)))
                                   (name2
                                    (%simple-fun-name (%code-entry-point (car b) 0))))
                               (if (and (symbolp name1) (symbol-package name1)
                                        (symbolp name2) (symbol-package name2))
                                   (let ((p1 (package-name (symbol-package name1)))
                                         (p2 (package-name (symbol-package name2))))
                                     (cond ((string< p1 p2) t)
                                           ((string> p1 p2) nil)
                                           ((string< name1 name2))))
                                   (< (get-lisp-obj-address (car a))
                                      (get-lisp-obj-address (car b))))))))))))
      (sb-vm::map-allocated-objects
       (lambda (obj type size)
         size
         (when (= type sb-vm:code-header-widetag)
           (loop for i from sb-vm:code-constants-offset
                 below (code-header-words obj)
                 do (let ((ref (code-header-ref obj i))
                          (fun))
                      (when (and (fdefn-p ref)
                                 (simple-fun-p (setq fun (fdefn-fun ref)))
                                 (immobile-space-obj-p fun))
                        (let* ((code (fun-code-header fun))
                               (ht (pick-table (%simple-fun-name
                                                (%code-entry-point code 0)))))
                          (incf (gethash code ht 0))))))))
       :immobile)
      (append (hashtable-keys-sorted other-stuff)
              (hashtable-keys-sorted compiler-stuff)))))

;;; Passing your own toplevel functions as the root set
;;; will encourage the defrag procedure to place them early
;;; in the space, which should be better than leaving the
;;; organization to random chance.
;;; Note that these aren't roots in the GC sense, just a locality sense.
(defun choose-code-component-order (&optional roots)
  (let ((ordering (make-array 10000 :adjustable t :fill-pointer 0))
        (hashset (make-hash-table :test 'eq)))

    ;; Place assembler routines first.
    (let ((code sb-fasl:*assembler-routines*))
      (setf (gethash code hashset) t)
      (vector-push-extend code ordering))

    (labels ((visit (thing)
               (typecase thing
                 (code-component (visit-code thing))
                 (simple-fun (visit-code (fun-code-header thing)))
                 (closure (visit (%closure-fun thing)))
                 (symbol (when (and (fboundp thing)
                                    (not (special-operator-p thing))
                                    (not (macro-function thing)))
                           (visit (symbol-function thing))))))
             (visit-code (code-component)
               (when (or (not (immobile-space-obj-p code-component))
                         (gethash code-component hashset))
                 (return-from visit-code))
               (setf (gethash code-component hashset) t)
               (vector-push-extend code-component ordering)
               (loop for i from sb-vm:code-constants-offset
                     below (code-header-words code-component)
                     do (let ((obj (code-header-ref code-component i)))
                          (typecase obj
                            (fdefn  (awhen (fdefn-fun obj) (visit it)))
                            (symbol (visit obj))
                            (vector (map nil #'visit obj)))))))
      (mapc #'visit
            (mapcan (lambda (x)
                      (let ((f (coerce x 'function)))
                        (when (simple-fun-p f)
                          (list (fun-code-header f)))))
                    (or roots '(read eval print compile)))))

    (dolist (code (order-by-in-degree))
      (unless (gethash code hashset)
        (setf (gethash code hashset) t)
        (vector-push-extend code ordering)))

    (sb-vm::map-allocated-objects
     (lambda (obj type size)
       (declare (ignore size))
       (when (and (= type sb-vm:code-header-widetag)
                  (not (typep (%code-debug-info obj) 'function))
                  (not (gethash obj hashset)))
         (setf (gethash obj hashset) t)
         (vector-push-extend obj ordering)))
     :immobile)

    (let* ((n (length ordering))
           (array (make-alien int (1+ (* n 2)))))
      (loop for i below n
            do (setf (deref array (* i 2))
                     (get-lisp-obj-address (aref ordering i))))
      (setf (deref array (* n 2)) 0) ; null-terminate the array
      (setf (extern-alien "code_component_order" unsigned)
            (sap-int (alien-value-sap array)))))

  (multiple-value-bind (index relocs) (sb-vm::collect-immobile-code-relocs)
    (let* ((n (length index))
           (array (make-alien int n)))
      (dotimes (i n) (setf (deref array i) (aref index i)))
      (setf (extern-alien "immobile_space_reloc_index" unsigned)
            (sap-int (alien-value-sap array))))
    (let* ((n (length relocs))
           (array (make-alien int n)))
      (dotimes (i n) (setf (deref array i) (aref relocs i)))
      (setf (extern-alien "immobile_space_relocs" unsigned)
            (sap-int (alien-value-sap array))))))
