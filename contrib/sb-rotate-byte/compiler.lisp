(in-package "SB-ROTATE-BYTE")

(defknown rotate-byte (integer byte-specifier integer) integer
  (foldable flushable))
(defknown %rotate-byte (integer bit-index bit-index integer) integer
  (foldable flushable))
(defknown %unsigned-32-rotate-byte ((integer -31 31) (unsigned-byte 32))
    (unsigned-byte 32)
  (foldable flushable))

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
                            ((constant-arg (member 0)) * * *) *)
  "fold identity operation"
  'integer)

(deftransform %rotate-byte ((count size pos integer)
                            ((integer -31 31)
                             (constant-arg (member 32))
                             (constant-arg (member 0))
                             (unsigned-byte 32)) *)
  "inline 32-bit rotation"
  ;; FIXME: What happens when, as here, the two type specifiers for
  ;; COUNT overlap?  Which gets to run first?
  '(%unsigned-32-rotate-byte count integer))
