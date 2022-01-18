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
  (frob)
  (defknown symbol-extra (t) t (flushable))
  (def-reffer 'symbol-extra symbol-size other-pointer-lowtag)
  (defknown (setf symbol-extra) (t t) t ())
  (def-setter '(setf symbol-extra) symbol-size other-pointer-lowtag))

(defconstant augmented-symbol-size (1+ symbol-size))

#+sb-thread
(dovector (slot (primitive-object-slots (primitive-object 'thread)))
  (when (slot-special slot)
    (setf (info :variable :wired-tls (slot-special slot))
          (ash (slot-offset slot) word-shift))))

#+gencgc
(progn
;;; don't change allocation granularity
(assert (= gencgc-alloc-granularity 0))
;;; cards are not larger than pages
(assert (<= gencgc-page-bytes +backend-page-bytes+))
;;; largeness does not depend on the hardware page size
(defconstant large-object-size (* 4 gencgc-page-bytes)))

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
    (complex "boxed" "ratio_or_complex" "boxed")
    (complex-single-float "unboxed")
    (complex-double-float "unboxed")

    (code-header "code_header")
    ;; For simple-fun, all three methods are "lose": "scav" is because you can't
    ;; encounter a simple-fun in heap scanning; "trans" is because it's not an OTHER pointer,
    ;; and "size" is because you can't take the size of a simple-fun by itself.
    (simple-fun "lose")
    ;; The closure scavenge function needs to know if the "self" slot
    ;; has pointer nature though it be fixnum tagged, as on x86.
    ;; The sizer is short_boxed.
    (closure ,(or #+(or x86 x86-64 arm64) "closure" "short_boxed") "lose" "short_boxed")
    ;; Like closure, but these can also have a layout pointer in the high header bytes.
    (funcallable-instance "funinstance" "lose" "short_boxed")
    ;; These have a scav and trans function, but no size function.
    #-(or x86 x86-64 arm64) (return-pc "return_pc_header" "return_pc_header" "lose")

    (value-cell "boxed")
    (symbol "symbol"
            "tiny_mixed" "tiny_boxed") ; trans and size respectively
    ;; Can't transport characters as "other" pointer objects.
    ;; It should be a cons cell half which would go through trans_list()
    (character "immediate")
    (sap "unboxed")
    (unbound-marker "immediate")
    (weak-pointer "weak_pointer" "tiny_mixed" "boxed")
    (instance "instance" "lose" "instance")
    (fdefn "fdefn")

    (no-tls-value-marker "immediate")

    #+sb-simd-pack (simd-pack "unboxed")
    #+sb-simd-pack-256 (simd-pack-256 "unboxed")
    (filler "unboxed")

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
  (format stream "#include \"lispobj.h\"~%")
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
      (when (member (second entry) '("bignum" "unboxed") :test 'string=)
        (setf bits (logior bits (ash 1 (ash (car entry) -2))))))
    (format stream "static inline int leaf_obj_widetag_p(unsigned char widetag) {~%")
    #+64-bit (format stream "  return (0x~XLU >> (widetag>>2)) & 1;" bits)
    #-64-bit (format stream "  int bit = widetag>>2;
  return (bit<32 ? 0x~XU >> bit : 0x~XU >> (bit-32)) & 1;"
                      (ldb (byte 32 0) bits) (ldb (byte 32 32) bits))
    (format stream "~%}~%"))

  (format stream "extern unsigned char widetag_lowtag[256];
static inline lispobj compute_lispobj(lispobj* base_addr) {
  return make_lispobj(base_addr, LOWTAG_FOR_WIDETAG(*base_addr & WIDETAG_MASK));~%}~%")

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
      (format stream "#define size_pointer size_immediate~%")
      (format stream "#define size_unboxed size_boxed~%")
      (write-table "sword_t (*sizetab[256])(lispobj *where)"
                   "size_" sizetab)
      (format stream "#undef size_pointer~%")
      (format stream "#undef size_unboxed~%")))
  (format stream "#endif~%"))

;;; AVLNODE is primitive-object-like because it is needed by C code that looks up
;;; entries in the tree of lisp threads.  But objdef doesn't have SB-XC:DEFSTRUCT
;;; working, and I'm reluctant to create yet another 'something-thread' file to
;;; put this in, not to mention that SB-THREAD is the wrong package anyway.
(in-package "SB-THREAD")
(sb-xc:defstruct (avlnode (:constructor avlnode (key data left right)))
  (left  nil :read-only t)
  (right nil :read-only t)
  (key   0   :read-only t :type sb-vm:word)
  data)
