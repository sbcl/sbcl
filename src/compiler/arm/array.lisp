;;;; array operations for the ARM VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")


;;;; Accessors/Setters

;;; Variants built on top of word-index-ref, etc.  I.e. those vectors whos
;;; elements are represented in integer registers and are built out of
;;; 8, 16, or 32 bit elements.
(macrolet ((def-full-data-vector-frobs (type element-type &rest scs)
  `(progn
     (define-full-reffer ,(symbolicate "DATA-VECTOR-REF/" type) ,type
       vector-data-offset other-pointer-lowtag
       ,(remove-if #'(lambda (x) (member x '(null))) scs)
       ,element-type
       data-vector-ref)
     (define-full-setter ,(symbolicate "DATA-VECTOR-SET/" type) ,type
       vector-data-offset other-pointer-lowtag ,scs ,element-type
       data-vector-set)))

           #!+(or)
           (def-partial-data-vector-frobs (type element-type size signed &rest scs)
  `(progn
     (define-partial-reffer ,(symbolicate "DATA-VECTOR-REF/" type) ,type
       ,size ,signed vector-data-offset other-pointer-lowtag ,scs
       ,element-type data-vector-ref)
     (define-partial-setter ,(symbolicate "DATA-VECTOR-SET/" type) ,type
       ,size vector-data-offset other-pointer-lowtag ,scs
       ,element-type data-vector-set))))

  (def-full-data-vector-frobs simple-vector *
    descriptor-reg any-reg null)

  #!+(or)
  (def-partial-data-vector-frobs simple-base-string character
    :byte nil character-reg)
  #!+sb-unicode
  (def-full-data-vector-frobs simple-character-string character character-reg)

  #!+(or)
  (def-partial-data-vector-frobs simple-array-unsigned-byte-7 positive-fixnum
    :byte nil unsigned-reg signed-reg)
  #!+(or)
  (def-partial-data-vector-frobs simple-array-unsigned-byte-8 positive-fixnum
    :byte nil unsigned-reg signed-reg)

  #!+(or)
  (def-partial-data-vector-frobs simple-array-unsigned-byte-15 positive-fixnum
    :short nil unsigned-reg signed-reg)
  #!+(or)
  (def-partial-data-vector-frobs simple-array-unsigned-byte-16 positive-fixnum
    :short nil unsigned-reg signed-reg)

  (def-full-data-vector-frobs simple-array-unsigned-byte-31 unsigned-num
    unsigned-reg)
  (def-full-data-vector-frobs simple-array-unsigned-byte-32 unsigned-num
    unsigned-reg)

  #!+(or)
  (def-partial-data-vector-frobs simple-array-signed-byte-8 tagged-num
    :byte t signed-reg)

  #!+(or)
  (def-partial-data-vector-frobs simple-array-signed-byte-16 tagged-num
    :short t signed-reg)

  (def-full-data-vector-frobs simple-array-unsigned-fixnum positive-fixnum
    any-reg)
  (def-full-data-vector-frobs simple-array-fixnum tagged-num
    any-reg)

  (def-full-data-vector-frobs simple-array-signed-byte-32 signed-num
    signed-reg))
