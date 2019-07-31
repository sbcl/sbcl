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

(defconstant extended-symbol-size (1+ symbol-size))

#+sb-thread
(dolist (slot (primitive-object-slots
               (find 'thread *primitive-objects* :key #'primitive-object-name)))
  (when (slot-special slot)
    (setf (info :variable :wired-tls (slot-special slot))
          (ash (slot-offset slot) word-shift))))

#+gencgc
(defconstant large-object-size
  (* 4 (max +backend-page-bytes+ gencgc-card-bytes
            gencgc-alloc-granularity)))


;;; Keep this (mostly) lined up with 'early-objdef' for sanity's sake!
#+sb-xc-host
(defparameter *scav/trans/size*
 (mapcar
  (lambda (entry)
    (cons (symbol-value (symbolicate (car entry) "-WIDETAG"))
          (cdr entry)))
  `((bignum "unboxed" "bignum" "bignum")
    (ratio "boxed" "ratio_or_complex" "boxed")
    (single-float ,(or #+64-bit "immediate" "unboxed"))
    (double-float "unboxed")
    (complex "boxed" "ratio_or_complex" "boxed")
    (complex-single-float "unboxed")
    (complex-double-float "unboxed")

    (code-header "code_header")
    ;; For all 3 function subtypes, the transporter is "lose"
    ;; because functions are not OTHER pointer objects.
    ;; The scavenge function for fun-header is basically "lose",
    ;; but it's only defined on non-x86 platforms for some reason.
    ;; The sizer is "lose" because it's an error if a function is encountered
    ;; in a heap scan.
    (simple-fun ,(or #+(or x86 x86-64) "lose" "fun_header") "lose" "lose")
    ;; The closure scavenge function needs to know if the "self" slot
    ;; has pointer nature though it be fixnum tagged, as on x86.
    ;; The sizer is short_boxed.
    (closure ,(or #+(or x86 x86-64) "closure" "short_boxed") "lose" "short_boxed")
    ;; Like closure, but these can also have a layout pointer in the high header bytes.
    (funcallable-instance ,(or #+compact-instance-header "funinstance" "short_boxed")
                          "lose" "short_boxed")
    ;; These have a scav and trans function, but no size function.
    #-(or x86 x86-64) (return-pc "return_pc_header" "return_pc_header" "lose")

    (value-cell "boxed")
    (symbol "tiny_boxed")
    ;; Can't transport characters as "other" pointer objects.
    ;; It should be a cons cell half which would go through trans_list()
    (character "immediate")
    (sap "unboxed")
    (unbound-marker "immediate")
    (weak-pointer "lose" "weak_pointer" "boxed")
    (instance "instance" "lose" "short_boxed")
    (fdefn "fdefn")

    (no-tls-value-marker "immediate")

    #+sb-simd-pack (simd-pack "unboxed")
    #+sb-simd-pack-256 (simd-pack-256 "unboxed")
    (filler "unboxed")

    (simple-array "boxed")
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
    (simple-vector "vector")

    (simple-array-nil "vector_nil")
    (simple-base-string "base_string")
    #+sb-unicode (simple-character-string "character_string")
    #+sb-unicode (complex-character-string "boxed")
    (complex-base-string "boxed")
    (complex-vector-nil "boxed")

    (complex-bit-vector "boxed")
    (complex-vector "boxed")
    (complex-array "boxed"))))

#+sb-xc-host
(defun write-gc-tables (stream)
  ;; Compute a bitmask of all specialized vector types,
  ;; not including array headers, for maybe_adjust_large_object().
  (let ((min #xff) (bits 0))
    (dovector (saetp *specialized-array-element-type-properties*)
      (unless (eq (saetp-primitive-type-name saetp) 'simple-vector)
        (let ((widetag (saetp-typecode saetp)))
          (setf min (min widetag min)
                bits (logior bits (ash 1 (ash widetag -2)))))))
    (format stream "static inline boolean specialized_vector_widetag_p(unsigned char widetag) {
  return widetag>=0x~X && (0x~8,'0XU >> ((widetag-0x80)>>2)) & 1;~%}~%"
            min (ldb (byte 32 32) bits))
    ;; Union in the bits for other unboxed object types.
    (dolist (entry *scav/trans/size*)
      (when (string= (second entry) "unboxed")
        (setf bits (logior bits (ash 1 (ash (car entry) -2))))))
    (format stream "static inline boolean leaf_obj_widetag_p(unsigned char widetag) {~%")
    #+64-bit (format stream "  return (0x~XLU >> (widetag>>2)) & 1;" bits)
    #-64-bit (format stream "  int bit = widetag>>2;
  return (bit<32 ? 0x~XU >> bit : 0x~XU >> (bit-32)) & 1;"
                      (ldb (byte 32 0) bits) (ldb (byte 32 32) bits))
    (format stream "~%}~%"))

  (format stream "extern unsigned char lowtag_for_widetag[64];
static inline lispobj compute_lispobj(lispobj* base_addr) {
  lispobj header = *base_addr;
  return make_lispobj(base_addr,
                      is_cons_half(header) ? LIST_POINTER_LOWTAG :
                        lowtag_for_widetag[header_widetag(header)>>2]);~%}~%")

  (format stream "~%#ifdef WANT_SCAV_TRANS_SIZE_TABLES~%")
  (let ((a (make-array 64 :initial-element 0)))
    (dolist (entry *scav/trans/size*)
      (destructuring-bind (widetag scav &rest ignore) entry
        (declare (ignore ignore))
        (unless (string= scav "immediate")
          (setf (aref a (ash widetag -2))
                (case widetag
                  (#.instance-widetag instance-pointer-lowtag)
                  (#.+function-widetags+ fun-pointer-lowtag)
                  (t other-pointer-lowtag))))))
    (let ((contents (format nil "~{0x~x,~} " (coerce a 'list))))
      (format stream
              "unsigned char lowtag_for_widetag[64] = {~{~%  ~A~}~%};~%"
              ;; write 4 characters per widetag ("0xN,"), 16 per line
              (loop for i from 0 by 64 repeat 4
                    ;; trailing comma on the last item is OK in C
                    collect (subseq contents i (+ i 64))))))
  (let ((scavtab  (make-array 256 :initial-element nil))
        (ptrtab   (make-array 4   :initial-element nil))
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
                 (setf (svref ptrtab (ldb (byte 2 (- sb-vm:n-lowtag-bits 2)) i))
                       pointer-kind)
                 (setf (svref scavtab i) (format nil "~A_pointer" pointer-kind)
                       (svref sizetab i) "pointer"))))))
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
      (write-table "sword_t (*scavtab[256])(lispobj *where, lispobj object)"
                   "scav_" scavtab)
      (format stream "static void (*scav_ptr[4])(lispobj *where, lispobj object)~
 = {~{~%  (void(*)(lispobj*,lispobj))scav_~A_pointer~^,~}~%};~%"
              (coerce ptrtab 'list))
      (write-table "static lispobj (*transother[64])(lispobj object)"
                   "trans_" transtab)
      (format stream "#define size_pointer size_immediate~%")
      (format stream "#define size_unboxed size_boxed~%")
      (write-table "sword_t (*sizetab[256])(lispobj *where)"
                   "size_" sizetab)
      (format stream "#undef size_pointer~%")
      (format stream "#undef size_unboxed~%")))
  (format stream "#endif~%"))
