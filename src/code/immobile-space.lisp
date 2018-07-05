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
      (sb-vm:map-allocated-objects
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

(in-package "SB-VM")

;;; Return the caller -> callee graph as an array grouped by caller.
;;; i.e. each element is (CALLING-CODE-COMPONENT . CODE-COMPONENT*)).
;;; A call is assumed only if we see a function or fdefn in the calling
;;; component. This underestimates the call graph of course,
;;; because it's impossible to predict whether calls occur through symbols,
;;; arrays of functions, or anything else. But it's a good approximation.
(defun compute-direct-call-graph (&optional verbose)
  (let ((graph (make-array 10000 :adjustable t :fill-pointer 0))
        (gf-code-cache (make-hash-table :test 'eq))
        (n-code-objs 0))
    (labels ((get-gf-code (gf)
               (ensure-gethash
                gf gf-code-cache
                (let (result)
                  (dolist (method (sb-mop:generic-function-methods gf) result)
                    (let ((fun (sb-mop:method-function method)))
                      (if (typep fun 'sb-pcl::%method-function)
                          (setq result
                                (list* (code-from-fun (sb-pcl::%method-function-fast-function fun))
                                       (code-from-fun (%funcallable-instance-function fun))
                                       result))
                          (pushnew (code-from-fun fun) result)))))))
             (code-from-fun (fun)
               (ecase (fun-subtype fun)
                 (#.simple-fun-widetag
                  (fun-code-header fun))
                 (#.funcallable-instance-widetag
                  (code-from-fun (%funcallable-instance-function fun)))
                 (#.closure-widetag
                  (fun-code-header (%closure-fun fun))))))
      (map-allocated-objects
       (lambda (obj type size)
         obj size
         (when (and (= type code-header-widetag)
                    (plusp (code-n-entries obj)))
           (incf n-code-objs)
           (let (list)
             (loop for j from code-constants-offset
                   below (code-header-words obj)
                   do (let* ((const (code-header-ref obj j))
                             (fun (typecase const
                                    (fdefn (fdefn-fun const))
                                    (function const))))
                        (when fun
                          (if (typep fun 'generic-function)
                              ;; Don't claim thousands of callees
                              (unless (and (typep const 'fdefn)
                                           (eq (fdefn-name const) 'print-object))
                                (setf list (union (copy-list (get-gf-code fun))
                                                  list)))
                              (pushnew (code-from-fun fun) list :test 'eq)))))
             (when list
               (vector-push-extend (cons obj list) graph)))))
       :immobile))
    (when verbose
      (format t "~&Call graph: ~D nodes, ~D with out-edges, max-edges=~D~%"
              n-code-objs
              (length graph)
              (reduce (lambda (x y) (max x (length (cdr y))))
                      graph :initial-value 0)))
    graph))

;;; Return list of code components ordered in a quasi-predictable way,
;;; provided that LOAD happened in a most 1 thread.
;;; In general: user code sorts before system code, never-called code sorts
;;; to the end, and ties are impossible due to uniqueness of serial#.
(defun deterministically-sort-immobile-code ()
  (let ((forward-graph (compute-direct-call-graph))
        (reverse-graph (make-hash-table :test 'eq))
        (ranking))
    ;; Compute the inverted call graph as a hash-table
    ;; for O(1) lookup of callers of any component.
    (dovector (item forward-graph)
      (let ((caller (car item))
            (callees (cdr item)))
        (dolist (callee callees)
          (push caller (gethash callee reverse-graph)))))
    ;; Compute popularity of each code component in varyobj space
    (map-allocated-objects
     (lambda (obj type size)
       (declare (ignore size))
       (when (and (= type code-header-widetag)
                  (plusp (code-n-entries obj))
                  (immobile-space-addr-p (get-lisp-obj-address obj)))
         (push (cons (length (gethash obj reverse-graph)) obj) ranking)))
     :immobile)
    ;; Sort by a 4-part key:
    ;; -  1 bit  : 0 = ever called, 1 = apparently un-called
    ;; -  1 bit  : system/non-system source file (system has lower precedence)
    ;; -  8 bits : popularity (as computed above)
    ;; - 32 bits : code component serial# (as stored on creation)
    (flet ((calc-key (item &aux (code (cdr item)))
             (let ((systemp
                    (or (let ((di (%code-debug-info code)))
                          (and (typep di 'sb-c::compiled-debug-info)
                               (let ((src (sb-c::compiled-debug-info-source di)))
                                 (and (typep src 'sb-c::debug-source)
                                      (let ((str (sb-c::debug-source-namestring src)))
                                        (if (= (mismatch str "SYS:") 4) 1))))))
                        0))
                   ;; cap the popularity index to 255 and negate so that higher
                   ;; sorts earlier
                   (popularity (- 255 (min (car item) 255)))
                   (serialno (sb-impl::%code-serialno code)))
               (logior (ash (if (= (car item) 0) 1 0) 41)
                       (ash systemp 40)
                       (ash popularity 32)
                       serialno))))
      (mapcar #'cdr (sort ranking #'< :key #'calc-key)))))

#+nil
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
      (sb-vm:map-allocated-objects
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
  (declare (ignore roots))
  (let ((ordering (make-array 10000 :adjustable t :fill-pointer 0))
        (hashset (make-hash-table :test 'eq)))

    (labels ((emplace (code)
               (unless (gethash code hashset)
                 (setf (gethash code hashset) t)
                 (vector-push-extend code ordering)))
             (visit (thing)
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

      ;; Place assembler routines first.
      (emplace sb-fasl:*assembler-routines*)
      ;; Place functions called by assembler routines next.
      (dovector (f +static-fdefns+)
        (emplace (fun-code-header (symbol-function f))))
      #+nil
      (mapc #'visit
            (mapcan (lambda (x)
                      (let ((f (coerce x 'function)))
                        (when (simple-fun-p f)
                          (list (fun-code-header f)))))
                    (or roots '(read eval print compile))))

      (mapc #'emplace (deterministically-sort-immobile-code))

      (map-allocated-objects
       (lambda (obj type size)
         (declare (ignore size))
         (when (and (= type code-header-widetag)
                    (not (typep (%code-debug-info obj) 'function)))
           (emplace obj)))
       :immobile))

    (let* ((n (length ordering))
           (array (make-alien int (1+ (* n 2)))))
      (loop for i below n
            do (setf (deref array (* i 2))
                     (get-lisp-obj-address (aref ordering i))))
      (setf (deref array (* n 2)) 0) ; null-terminate the array
      (setf (extern-alien "code_component_order" unsigned)
            (sap-int (alien-value-sap array)))))

  (multiple-value-bind (index relocs) (collect-immobile-code-relocs)
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
