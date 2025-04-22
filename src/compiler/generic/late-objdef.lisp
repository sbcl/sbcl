;;;; late machine-independent aspects of the object representation

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

#-c-headers-only
(macrolet ((frob ()
             `(progn ,@*!late-primitive-object-forms*)))
  (frob))

#+sb-thread
(dovector (slot (primitive-object-slots (primitive-object 'thread)))
  (when (slot-special slot)
    (setf (info :variable :wired-tls (slot-special slot))
          (ash (slot-offset slot) word-shift))))

(progn
;;; don't change allocation granularity
(assert (= gencgc-alloc-granularity 0))
;;; cards are not larger than pages
(assert (<= gencgc-page-bytes +backend-page-bytes+))
;;; largeness does not depend on the hardware page size
(defconstant large-object-size #-mark-region-gc (* 4 gencgc-page-bytes)
                               #+mark-region-gc (* 3/4 gencgc-page-bytes))
(assert (integerp large-object-size)))

;;; Keep this (mostly) lined up with 'early-objdef' for sanity's sake!
;;; The "transport" function is used only if the object is an OTHER-POINTER
;;; (which goes through the dispatch table).
#+sb-xc-host
(defparameter *scav/trans/size*
 (mapcar
  (lambda (entry)
    (cons (symbol-value (symbolicate (car entry) "-WIDETAG"))
          (cdr entry)))
  `((bignum "bignum")
    (ratio "boxed" "ratio_or_complex" "boxed")
    (single-float ,(or #+64-bit "immediate" "unboxed"))
    (double-float "unboxed")
    (complex-rational "boxed" "ratio_or_complex" "boxed")
    (complex-single-float "unboxed")
    (complex-double-float "unboxed")

    (code-header "code_blob")
    ;; For simple-fun, all three methods are "lose": "scav" is because you can't
    ;; encounter a simple-fun in heap scanning; "trans" is because it's not an OTHER pointer,
    ;; and "size" is because you can't take the size of a simple-fun by itself.
    (simple-fun "lose")
    ;; The closure scavenge function needs to know if the "self" slot
    ;; has pointer nature though it be fixnum tagged, as on x86.
    ;; The sizer is short_boxed.
    (closure ,(or #+(or arm64 ppc64 x86 x86-64) "closure" "short_boxed") "lose" "short_boxed")
    ;; Like closure, but these can also have a layout pointer in the high header bytes.
    (funcallable-instance "funinstance" "lose" "short_boxed")
    ;; These have a scav and trans function, but no size function.
    #-(or x86 x86-64 arm64 riscv)
    (return-pc "return_pc_header" "return_pc_header" "lose")

    (value-cell "boxed")
    (symbol "symbol")
    ;; Can't transport characters as "other" pointer objects.
    ;; It should be a cons cell half which would go through trans_list()
    (character "immediate")
    (sap "unboxed")
    (unbound-marker "immediate")
    (weak-pointer "weakptr")
    (instance "instance" "lose" "instance")
    (fdefn "fdefn")

    #+sb-simd-pack (simd-pack "unboxed")
    #+sb-simd-pack-256 (simd-pack-256 "unboxed")
    (filler "filler" "lose" "filler")

    (simple-array "array")
    (simple-array-unsigned-byte-2 "vector_unsigned_byte_2")
    (simple-array-unsigned-byte-4 "vector_unsigned_byte_4")
    (simple-array-unsigned-byte-7 "vector_unsigned_byte_8")
    (simple-array-unsigned-byte-8 "vector_unsigned_byte_8")
    (simple-array-unsigned-byte-15 "vector_unsigned_byte_16")
    (simple-array-unsigned-byte-16 "vector_unsigned_byte_16")
    (simple-array-unsigned-fixnum #-64-bit "vector_unsigned_byte_32"
                                  #+64-bit "vector_unsigned_byte_64")
    (simple-array-unsigned-byte-31 "vector_unsigned_byte_32")
    (simple-array-unsigned-byte-32 "vector_unsigned_byte_32")
    #+64-bit (simple-array-unsigned-byte-63 "vector_unsigned_byte_64")
    #+64-bit (simple-array-unsigned-byte-64 "vector_unsigned_byte_64")

    (simple-array-signed-byte-8 "vector_unsigned_byte_8")
    (simple-array-signed-byte-16 "vector_unsigned_byte_16")
    (simple-array-signed-byte-32 "vector_unsigned_byte_32")
    (simple-array-fixnum #-64-bit "vector_unsigned_byte_32"
                         #+64-bit "vector_unsigned_byte_64")
    #+64-bit (simple-array-signed-byte-64 "vector_unsigned_byte_64")

    (simple-array-single-float "vector_unsigned_byte_32")
    (simple-array-double-float "vector_unsigned_byte_64")
    (simple-array-complex-single-float "vector_unsigned_byte_64")
    (simple-array-complex-double-float "vector_unsigned_byte_128")

    (simple-bit-vector "vector_bit")
    (simple-vector "vector_t")

    (simple-array-nil "vector_nil")
    (simple-base-string "base_string")
    ;; UB32 works fine for character string, unless we decide to reimplement
    ;; using 3 octets per code point.
    #+sb-unicode (simple-character-string "vector_unsigned_byte_32")
    #+sb-unicode (complex-character-string "array")
    (complex-base-string "array")

    (complex-bit-vector "array")
    (complex-vector "array")
    (complex-array "array"))))

#+sb-xc-host
(defun write-gc-tables (stream)
  (format stream "#include ~S~%" (sb-fasl::lispobj-dot-h))
  ;; Compute a bitmask of all specialized vector types,
  ;; not including array headers, for maybe_adjust_large_object().
  (let ((min #xff) (bits 0))
    (dovector (saetp *specialized-array-element-type-properties*)
      (unless (eq (saetp-primitive-type-name saetp) 'simple-vector)
        (let ((widetag (saetp-typecode saetp)))
          (setf min (min widetag min)
                bits (logior bits (ash 1 (ash widetag -2)))))))
    (format stream "static inline int specialized_vector_widetag_p(unsigned char widetag) {
  return widetag>=0x~X && (0x~8,'0XU >> ((widetag-0x80)>>2)) & 1;~%}~%"
            min (ldb (byte 32 32) bits))
    ;; Union in the bits for other unboxed object types.
    (dolist (entry *scav/trans/size*)
      (when (member (second entry) '("bignum" "unboxed" "filler") :test 'string=)
        (setf bits (logior bits (ash 1 (ash (car entry) -2))))))
    (format stream "static inline int leaf_obj_widetag_p(unsigned char widetag) {~%")
    #+64-bit (format stream "  return (0x~XLU >> (widetag>>2)) & 1;" bits)
    #-64-bit (format stream "  int bit = widetag>>2;
  return (bit<32 ? 0x~XU >> bit : 0x~XU >> (bit-32)) & 1;"
                      (ldb (byte 32 0) bits) (ldb (byte 32 32) bits))
    (format stream "~%}~%"))

  (format stream "~%#ifdef WANT_SCAV_TRANS_SIZE_TABLES~%")
  (let ((lowtag-tbl (make-array 256 :initial-element 0)))
    ;; Build a table translating from the from low byte of first word of any
    ;; heap object to that object's lowtag when pointed to by a tagged pointer.
    ;; If the first word is {immediate | pointer} then the object is a cons,
    ;; otherwise the object is a headered object.
    (dotimes (byte 256)
      (when (or (eql 0 (logand byte fixnum-tag-mask))
                (member (logand byte lowtag-mask)
                        `(,instance-pointer-lowtag
                          ,list-pointer-lowtag
                          ,fun-pointer-lowtag
                          ,other-pointer-lowtag))
                (member byte `(#+64-bit ,single-float-widetag
                               ,character-widetag
                               ,unbound-marker-widetag)))
        ;; gotta be a CONS
        (setf (svref lowtag-tbl byte) list-pointer-lowtag)))
    (dolist (entry *scav/trans/size*)
      (destructuring-bind (widetag scav &rest ignore) entry
        (declare (ignore ignore))
        (unless (string= scav "immediate")
          (setf (svref lowtag-tbl widetag)
                (+ #x80 (case widetag
                          (#.instance-widetag instance-pointer-lowtag)
                          (#.+function-widetags+ fun-pointer-lowtag)
                          (#.filler-widetag 0)
                          (t other-pointer-lowtag)))))))
    (format stream "unsigned char widetag_lowtag[256] = {")
    (dotimes (line 16)
      (format stream "~%~:{ ~:[0x~2,'0x~;~4d~],~}"
              (mapcar (lambda (x) (list (member x `(0 ,sb-vm:list-pointer-lowtag)) x))
                      (coerce (subseq lowtag-tbl (* line 16) (* (1+ line) 16)) 'list))))
    (format stream "~%};~%"))
  (let ((scavtab  (make-array 256 :initial-element nil))
        (ptrtab   (make-list #+ppc64 16 #-ppc64 4))
        (transtab (make-array 64  :initial-element nil))
        (sizetab  (make-array 256 :initial-element nil)))
    (dotimes (i 256)
      (cond ((eql 0 (logand i fixnum-tag-mask))
             (setf (svref scavtab i) "immediate" (svref sizetab i) "immediate"))
            (t
             (let ((pointer-kind (case (logand i lowtag-mask)
                                   (#.instance-pointer-lowtag "instance")
                                   (#.list-pointer-lowtag     "list")
                                   (#.fun-pointer-lowtag      "fun")
                                   (#.other-pointer-lowtag    "other"))))
               (when pointer-kind
                 #-ppc64
                 (let ((n (ldb (byte 2 (- sb-vm:n-lowtag-bits 2)) i)))
                   (unless (nth n ptrtab)
                     (setf (nth n ptrtab) (format nil "scav_~A_pointer" pointer-kind))))
                 (setf (svref scavtab i) (format nil "~A_pointer" pointer-kind)
                       (svref sizetab i) "pointer"))))))
    #+ppc64
    (progn
      (fill ptrtab "scav_lose")
      (setf (aref scavtab #xff) "consfiller"
            (aref sizetab #xff) "consfiller")
      (setf (nth instance-pointer-lowtag ptrtab) "scav_instance_pointer"
            (nth list-pointer-lowtag ptrtab)     "scav_list_pointer"
            (nth fun-pointer-lowtag ptrtab)      "scav_fun_pointer"
            (nth other-pointer-lowtag ptrtab)    "scav_other_pointer"))
    (dolist (entry *scav/trans/size*)
      (destructuring-bind (widetag scav &optional (trans scav) (size trans)) entry
        ;; immediates use trans_lose which is what trans_immediate did anyway.
        ;; Substitution here makes the *scav/trans/size* table definition
        ;; more clear, because single-float is either immediate or unboxed,
        ;; and it's not nice to repeat the reader conditional expressing that.
        (when (string= trans "immediate") (setq trans "lose"))
        (setf (svref scavtab widetag) scav
              (svref transtab (ash widetag -2)) trans
              (svref sizetab widetag) size)))
    (flet ((write-table (decl prefix contents)
             (format stream "~A = {" decl)
             (loop for i from 0 for x across contents
                   when (zerop (mod i 4))
                   do (format stream "~%  ")
                   do (format stream "~V@<~A~A~:[~;,~]~>"
                              (if (= (mod i 4) 3) 0 31)
                              prefix (or x "lose") (< i (length contents))))
             (format stream "~%};~%")))
      (write-table "sword_t (*const scavtab[256])(lispobj *where, lispobj object)"
                   "scav_" scavtab)
      (format stream "static void (*scav_ptr[~d])(lispobj *where, lispobj object)~
 = {~{~%  (void(*)(lispobj*,lispobj))~A~^,~}~%};~%" (length ptrtab) ptrtab)
      (write-table "static lispobj (*transother[64])(lispobj object)"
                   "trans_" transtab)
      (format stream "#define size_pointer (sizerfn)0~%")
      (format stream "#define size_immediate (sizerfn)0~%")
      (write-table "sword_t (*sizetab[256])(lispobj *where)"
                   "size_" sizetab)
      (format stream "#undef size_immediate~%")
      (format stream "#undef size_pointer~%")
      (format stream "#undef size_unboxed~%")))
  (format stream "#endif~%"))

(sb-xc:defstruct (arena (:constructor nil))
  ;; Address of the 'struct arena_memblk' we're currently allocating to.
  (current-block 0 :type word)
  ;; Address of the one mandatory 'struct arena_memblk' for this arena
  (first-block 0 :type word)
  ;; Huge objects are those whose size exceeds a tiny fraction of the growth amount.
  (huge-objects 0 :type word)
  ;; Arena allocation parameters
  (original-size 0 :type word)
  (growth-amount 0 :type word) ; additive
  ;; Maximum we'll allow the arena to grow to, accounting for extension blocks
  ;; and huge object blocks.
  (size-limit 0 :type word)
  ;; Sum of sizes of currently allocated blocks
  (length 0 :type word)
  ;; Sum of unusable bytes resulting from discarding the tail of the
  ;; most recently claimed chunk when switching from the arena to the heap.
  (bytes-wasted 0 :type word)
  ;; Small integer identifier starting from 0
  (index 0 :type fixnum)
  ;; T if all memory has been protected with PROT_NONE (for debugging)
  hidden
  ;; a counter that increments on each rewind, and which can be used by a threads
  ;; in a pool to detect that their cached TLAB pointers are invalid
  (token 0 :type word)
  userdata
  ;; Link for global chain of all arenas, needed for GC when 'scavenge_arenas' is 1,
  ;; so that GC can find all in-use arenas.
  ;; This is a tagged pointer to the next arena in the chain, terminated by NIL.
  ;; It is 0 until added to the global chain so we can tell the difference between
  ;; an arena that was made but never used, and one that was used at some point.
  (link 0))
