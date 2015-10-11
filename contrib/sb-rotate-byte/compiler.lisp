(in-package "SB-ROTATE-BYTE")

(defknown rotate-byte (integer byte-specifier integer) integer
  (foldable flushable)
  :overwrite-fndb-silently t)
(defknown %rotate-byte (integer bit-index bit-index integer) integer
  (foldable flushable)
  :overwrite-fndb-silently t)
(defknown %unsigned-32-rotate-byte ((integer -31 31) (unsigned-byte 32))
    (unsigned-byte 32)
  (foldable flushable)
  :overwrite-fndb-silently t)
#+64-bit
(defknown %unsigned-64-rotate-byte ((integer -63 63) (unsigned-byte 64))
    (unsigned-byte 64)
  (foldable flushable)
  :overwrite-fndb-silently t)

(macrolet (;; see src/compiler/srctran.lisp
           (with-byte-specifier ((size-var pos-var spec) &body body)
             (once-only ((spec `(macroexpand ,spec))
                         (temp '(gensym)))
                        `(if (and (consp ,spec)
                                  (eq (car ,spec) 'byte)
                                  (= (length ,spec) 3))
                        (let ((,size-var (second ,spec))
                              (,pos-var (third ,spec)))
                          ,@body)
                        (let ((,size-var `(byte-size ,,temp))
                              (,pos-var `(byte-position ,,temp)))
                          `(let ((,,temp ,,spec))
                             ,,@body))))))
  (define-source-transform rotate-byte (count spec num)
    (with-byte-specifier (size pos spec)
      `(%rotate-byte ,count ,size ,pos ,num))))

(defoptimizer (%rotate-byte derive-type) ((count size posn num))
  ;; FIXME: this looks fairly unwieldy.  I'm sure it can be made
  ;; simpler, and also be made to deal with negative integers too.
  (declare (ignore count posn))
  (let ((size (sb-c::lvar-type size)))
    (if (numeric-type-p size)
        (let ((size-high (numeric-type-high size))
              (num-type (sb-c::lvar-type num)))
          (if (and size-high
                   num-type
                   (<= size-high sb-vm:n-word-bits)
                   (csubtypep num-type
                              (specifier-type `(unsigned-byte ,size-high))))
              (specifier-type `(unsigned-byte ,size-high))
              *universal-type*))
        *universal-type*)))

(deftransform %rotate-byte ((count size pos integer)
                            ((integer -31 31)
                             (constant-arg (member 32))
                             (constant-arg (member 0))
                             (unsigned-byte 32)) *)
  "inline 32-bit rotation"
  '(%unsigned-32-rotate-byte count integer))

;; Generic implementation for platforms that don't supply VOPs for 32-bit
;; rotate.
#-(or x86 x86-64 ppc arm arm64)
(deftransform %unsigned-32-rotate-byte ((.count. .integer.)
                                        ((integer -31 31)
                                         (unsigned-byte 32)) *)
  '(if (< .count. 0)
       (logior (ldb (byte 32 0) (ash .integer. (+ .count. 32)))
               (ash .integer. .count.))
       (logior (ldb (byte 32 0) (ash .integer. .count.))
               (ash .integer. (- .count. 32)))))

#+64-bit
(deftransform %rotate-byte ((count size pos integer)
                            ((integer -63 63)
                             (constant-arg (member 64))
                             (constant-arg (member 0))
                             (unsigned-byte 64)) *)
  "inline 64-bit rotation"
  '(%unsigned-64-rotate-byte count integer))

;;; This transform needs to come after the others to ensure it gets
;;; first crack at a zero COUNT, since transforms are currently run
;;; latest-defined first.
(deftransform %rotate-byte ((count size pos integer)
                            ((constant-arg (member 0)) * * *) *)
  "fold identity operation"
  'integer)
