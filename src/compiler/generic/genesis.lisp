;;;; "cold" core image builder: This is how we create a target Lisp
;;;; system from scratch, by converting from fasl files to an image
;;;; file in the cross-compilation host, without the help of the
;;;; target Lisp system.
;;;;
;;;; As explained by Rob MacLachlan on the CMU CL mailing list Wed, 06
;;;; Jan 1999 11:05:02 -0500, this cold load generator more or less
;;;; fakes up static function linking. I.e. it makes sure that all the
;;;; DEFUN-defined functions in the fasl files it reads are bound to the
;;;; corresponding symbols before execution starts. It doesn't do
;;;; anything to initialize variable values; instead it just arranges
;;;; for !COLD-INIT to be called at cold load time. !COLD-INIT is
;;;; responsible for explicitly initializing anything which has to be
;;;; initialized early before it transfers control to the ordinary
;;;; top level forms.
;;;;
;;;; (In CMU CL, and in SBCL as of 0.6.9 anyway, functions not defined
;;;; by DEFUN aren't set up specially by GENESIS.)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!FASL")

;;; a magic number used to identify our core files
(defconstant core-magic
  (logior (ash (sb!xc:char-code #\S) 24)
          (ash (sb!xc:char-code #\B) 16)
          (ash (sb!xc:char-code #\C) 8)
          (sb!xc:char-code #\L)))

(defun round-up (number size)
  "Round NUMBER up to be an integral multiple of SIZE."
  (* size (ceiling number size)))

;;;; implementing the concept of "vector" in (almost) portable
;;;; Common Lisp
;;;;
;;;; "If you only need to do such simple things, it doesn't really
;;;; matter which language you use." -- _ANSI Common Lisp_, p. 1, Paul
;;;; Graham (evidently not considering the abstraction "vector" to be
;;;; such a simple thing:-)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +smallvec-length+
    (expt 2 16)))

;;; an element of a BIGVEC -- a vector small enough that we have
;;; a good chance of it being portable to other Common Lisps
(deftype smallvec ()
  `(simple-array (unsigned-byte 8) (,+smallvec-length+)))

(defun make-smallvec ()
  (make-array +smallvec-length+ :element-type '(unsigned-byte 8)
              :initial-element 0))

;;; a big vector, implemented as a vector of SMALLVECs
;;;
;;; KLUDGE: This implementation seems portable enough for our
;;; purposes, since realistically every modern implementation is
;;; likely to support vectors of at least 2^16 elements. But if you're
;;; masochistic enough to read this far into the contortions imposed
;;; on us by ANSI and the Lisp community, for daring to use the
;;; abstraction of a large linearly addressable memory space, which is
;;; after all only directly supported by the underlying hardware of at
;;; least 99% of the general-purpose computers in use today, then you
;;; may be titillated to hear that in fact this code isn't really
;;; portable, because as of sbcl-0.7.4 we need somewhat more than
;;; 16Mbytes to represent a core, and ANSI only guarantees that
;;; ARRAY-DIMENSION-LIMIT is not less than 1024. -- WHN 2002-06-13
(defstruct bigvec
  (outer-vector (vector (make-smallvec)) :type (vector smallvec)))

;;; analogous to SVREF, but into a BIGVEC
(defun bvref (bigvec index)
  (multiple-value-bind (outer-index inner-index)
      (floor index +smallvec-length+)
    (aref (the smallvec
            (svref (bigvec-outer-vector bigvec) outer-index))
          inner-index)))
(defun (setf bvref) (new-value bigvec index)
  (multiple-value-bind (outer-index inner-index)
      (floor index +smallvec-length+)
    (setf (aref (the smallvec
                  (svref (bigvec-outer-vector bigvec) outer-index))
                inner-index)
          new-value)))

;;; analogous to LENGTH, but for a BIGVEC
;;;
;;; the length of BIGVEC, measured in the number of BVREFable bytes it
;;; can hold
(defun bvlength (bigvec)
  (* (length (bigvec-outer-vector bigvec))
     +smallvec-length+))

;;; analogous to WRITE-SEQUENCE, but for a BIGVEC
(defun write-bigvec-as-sequence (bigvec stream &key (start 0) end pad-with-zeros)
  (let* ((bvlength (bvlength bigvec))
         (data-length (min (or end bvlength) bvlength)))
    (loop for i of-type index from start below data-length do
      (write-byte (bvref bigvec i)
                  stream))
    (when (and pad-with-zeros (< bvlength data-length))
      (loop repeat (- data-length bvlength) do (write-byte 0 stream)))))

;;; analogous to READ-SEQUENCE-OR-DIE, but for a BIGVEC
(defun read-bigvec-as-sequence-or-die (bigvec stream &key (start 0) end)
  (loop for i of-type index from start below (or end (bvlength bigvec)) do
        (setf (bvref bigvec i)
              (read-byte stream))))

;;; Grow BIGVEC (exponentially, so that large increases in size have
;;; asymptotic logarithmic cost per byte).
(defun expand-bigvec (bigvec)
  (let* ((old-outer-vector (bigvec-outer-vector bigvec))
         (length-old-outer-vector (length old-outer-vector))
         (new-outer-vector (make-array (* 2 length-old-outer-vector))))
    (dotimes (i length-old-outer-vector)
      (setf (svref new-outer-vector i)
            (svref old-outer-vector i)))
    (loop for i from length-old-outer-vector below (length new-outer-vector) do
          (setf (svref new-outer-vector i)
                (make-smallvec)))
    (setf (bigvec-outer-vector bigvec)
          new-outer-vector))
  bigvec)

;;;; looking up bytes and multi-byte values in a BIGVEC (considering
;;;; it as an image of machine memory on the cross-compilation target)

;;; BVREF-32 and friends. These are like SAP-REF-n, except that
;;; instead of a SAP we use a BIGVEC.
(macrolet ((make-bvref-n
            (n)
            (let* ((name (intern (format nil "BVREF-~A" n)))
                   (number-octets (/ n 8))
                   (ash-list-le
                    (loop for i from 0 to (1- number-octets)
                          collect `(ash (bvref bigvec (+ byte-index ,i))
                                        ,(* i 8))))
                   (ash-list-be
                    (loop for i from 0 to (1- number-octets)
                          collect `(ash (bvref bigvec
                                               (+ byte-index
                                                  ,(- number-octets 1 i)))
                                        ,(* i 8))))
                   (setf-list-le
                    (loop for i from 0 to (1- number-octets)
                          append
                          `((bvref bigvec (+ byte-index ,i))
                            (ldb (byte 8 ,(* i 8)) new-value))))
                   (setf-list-be
                    (loop for i from 0 to (1- number-octets)
                          append
                          `((bvref bigvec (+ byte-index ,i))
                            (ldb (byte 8 ,(- n 8 (* i 8))) new-value)))))
              `(progn
                 (defun ,name (bigvec byte-index)
                   (logior ,@(ecase sb!c:*backend-byte-order*
                               (:little-endian ash-list-le)
                               (:big-endian ash-list-be))))
                 (defun (setf ,name) (new-value bigvec byte-index)
                   (setf ,@(ecase sb!c:*backend-byte-order*
                             (:little-endian setf-list-le)
                             (:big-endian setf-list-be))))))))
  (make-bvref-n 8)
  (make-bvref-n 16)
  (make-bvref-n 32)
  (make-bvref-n 64))

;; lispobj-sized word, whatever that may be
;; hopefully nobody ever wants a 128-bit SBCL...
#!+64-bit
(progn
  (defun bvref-word (bytes index)
    (bvref-64 bytes index))
  (defun (setf bvref-word) (new-val bytes index)
    (setf (bvref-64 bytes index) new-val)))

#!-64-bit
(progn
  (defun bvref-word (bytes index)
    (bvref-32 bytes index))
  (defun (setf bvref-word) (new-val bytes index)
    (setf (bvref-32 bytes index) new-val)))


;;;; representation of spaces in the core

;;; If there is more than one dynamic space in memory (i.e., if a
;;; copying GC is in use), then only the active dynamic space gets
;;; dumped to core.
(defvar *dynamic*)
(defconstant dynamic-core-space-id 1)

(defvar *static*)
(defconstant static-core-space-id 2)

(defvar *read-only*)
(defconstant read-only-core-space-id 3)

(defconstant max-core-space-id 3)
(defconstant deflated-core-space-id-flag 4)

;; This is somewhat arbitrary as there is no concept of the the
;; number of bits in the "low" part of a descriptor any more.
(defconstant target-space-alignment (ash 1 16)
  "the alignment requirement for spaces in the target.")

;;; a GENESIS-time representation of a memory space (e.g. read-only
;;; space, dynamic space, or static space)
(defstruct (gspace (:constructor %make-gspace)
                   (:copier nil))
  ;; name and identifier for this GSPACE
  (name (missing-arg) :type symbol :read-only t)
  (identifier (missing-arg) :type fixnum :read-only t)
  ;; the word address where the data will be loaded
  (word-address (missing-arg) :type unsigned-byte :read-only t)
  ;; the data themselves. (Note that in CMU CL this was a pair of
  ;; fields SAP and WORDS-ALLOCATED, but that wasn't very portable.)
  ;; (And then in SBCL this was a VECTOR, but turned out to be
  ;; unportable too, since ANSI doesn't think that arrays longer than
  ;; 1024 (!) should needed by portable CL code...)
  (bytes (make-bigvec) :read-only t)
  ;; the index of the next unwritten word (i.e. chunk of
  ;; SB!VM:N-WORD-BYTES bytes) in BYTES, or equivalently the number of
  ;; words actually written in BYTES. In order to convert to an actual
  ;; index into BYTES, thus must be multiplied by SB!VM:N-WORD-BYTES.
  (free-word-index 0))

(defun gspace-byte-address (gspace)
  (ash (gspace-word-address gspace) sb!vm:word-shift))

(def!method print-object ((gspace gspace) stream)
  (print-unreadable-object (gspace stream :type t)
    (format stream "~S" (gspace-name gspace))))

(defun make-gspace (name identifier byte-address)
  (unless (zerop (rem byte-address target-space-alignment))
    (error "The byte address #X~X is not aligned on a #X~X-byte boundary."
           byte-address
           target-space-alignment))
  (%make-gspace :name name
                :identifier identifier
                :word-address (ash byte-address (- sb!vm:word-shift))))

;;;; representation of descriptors

(defun is-fixnum-lowtag (lowtag)
  (zerop (logand lowtag sb!vm:fixnum-tag-mask)))

(defun is-other-immediate-lowtag (lowtag)
  ;; The other-immediate lowtags are similar to the fixnum lowtags, in
  ;; that they have an "effective length" that is shorter than is used
  ;; for the pointer lowtags.  Unlike the fixnum lowtags, however, the
  ;; other-immediate lowtags are always effectively two bits wide.
  (= (logand lowtag 3) sb!vm:other-immediate-0-lowtag))

(defstruct (descriptor
            (:constructor make-descriptor (bits &optional gspace word-offset))
            (:copier nil))
  ;; the GSPACE that this descriptor is allocated in, or NIL if not set yet.
  (gspace nil :type (or gspace (eql :load-time-value) null))
  ;; the offset in words from the start of GSPACE, or NIL if not set yet
  (word-offset nil :type (or sb!vm:word null))
  (bits 0 :read-only t :type (unsigned-byte #.sb!vm:n-machine-word-bits)))

(declaim (inline descriptor=))
(defun descriptor= (a b) (eql (descriptor-bits a) (descriptor-bits b)))

;; FIXME: most uses of MAKE-RANDOM-DESCRIPTOR are abuses for writing a raw word
;; into target memory as if it were a descriptor, because there is no variant
;; of WRITE-WORDINDEXED taking a non-descriptor value.
;; As an intermediary step, perhaps this should be renamed to MAKE-RAW-BITS.
(defun make-random-descriptor (bits)
  (make-descriptor (logand bits sb!ext:most-positive-word)))

(def!method print-object ((des descriptor) stream)
  (let ((lowtag (descriptor-lowtag des)))
    (print-unreadable-object (des stream :type t)
      (cond ((is-fixnum-lowtag lowtag)
             (format stream "for fixnum: ~W" (descriptor-fixnum des)))
            ((is-other-immediate-lowtag lowtag)
             (format stream
                     "for other immediate: #X~X, type #b~8,'0B"
                     (ash (descriptor-bits des) (- sb!vm:n-widetag-bits))
                     (logand (descriptor-bits des) sb!vm:widetag-mask)))
            (t
             (format stream
                     "for pointer: #X~X, lowtag #b~v,'0B, ~A"
                     (logandc2 (descriptor-bits des) sb!vm:lowtag-mask)
                     sb!vm:n-lowtag-bits lowtag
                     (let ((gspace (descriptor-gspace des)))
                       (if gspace
                           (gspace-name gspace)
                           "unknown"))))))))

;;; Return a descriptor for a block of LENGTH bytes out of GSPACE. The
;;; free word index is boosted as necessary, and if additional memory
;;; is needed, we grow the GSPACE. The descriptor returned is a
;;; pointer of type LOWTAG.
(defun allocate-cold-descriptor (gspace length lowtag)
  (let* ((bytes (round-up length (ash 1 sb!vm:n-lowtag-bits)))
         (old-free-word-index (gspace-free-word-index gspace))
         (new-free-word-index (+ old-free-word-index
                                 (ash bytes (- sb!vm:word-shift)))))
    ;; Grow GSPACE as necessary until it's big enough to handle
    ;; NEW-FREE-WORD-INDEX.
    (do ()
        ((>= (bvlength (gspace-bytes gspace))
             (* new-free-word-index sb!vm:n-word-bytes)))
      (expand-bigvec (gspace-bytes gspace)))
    ;; Now that GSPACE is big enough, we can meaningfully grab a chunk of it.
    (setf (gspace-free-word-index gspace) new-free-word-index)
    (let ((ptr (+ (gspace-word-address gspace) old-free-word-index)))
      (make-descriptor (logior (ash ptr sb!vm:word-shift) lowtag)
                       gspace
                       old-free-word-index))))

(defun descriptor-lowtag (des)
  "the lowtag bits for DES"
  (logand (descriptor-bits des) sb!vm:lowtag-mask))

(defun descriptor-fixnum (des)
  (let ((bits (descriptor-bits des)))
    (if (logbitp (1- sb!vm:n-word-bits) bits)
        ;; KLUDGE: The (- SB!VM:N-WORD-BITS 2) term here looks right to
        ;; me, and it works, but in CMU CL it was (1- SB!VM:N-WORD-BITS),
        ;; and although that doesn't make sense for me, or work for me,
        ;; it's hard to see how it could have been wrong, since CMU CL
        ;; genesis worked. It would be nice to understand how this came
        ;; to be.. -- WHN 19990901
        (logior (ash bits (- sb!vm:n-fixnum-tag-bits))
                (ash -1 (1+ sb!vm:n-positive-fixnum-bits)))
        (ash bits (- sb!vm:n-fixnum-tag-bits)))))

(defun descriptor-word-sized-integer (des)
  ;; Extract an (unsigned-byte 32), from either its fixnum or bignum
  ;; representation.
  (let ((lowtag (descriptor-lowtag des)))
    (if (is-fixnum-lowtag lowtag)
        (make-random-descriptor (descriptor-fixnum des))
        (read-wordindexed des 1))))

;;; common idioms
(defun descriptor-bytes (des)
  (gspace-bytes (descriptor-intuit-gspace des)))
(defun descriptor-byte-offset (des)
  (ash (descriptor-word-offset des) sb!vm:word-shift))

;;; If DESCRIPTOR-GSPACE is already set, just return that. Otherwise,
;;; figure out a GSPACE which corresponds to DES, set it into
;;; (DESCRIPTOR-GSPACE DES), set a consistent value into
;;; (DESCRIPTOR-WORD-OFFSET DES), and return the GSPACE.
(declaim (ftype (function (descriptor) gspace) descriptor-intuit-gspace))
(defun descriptor-intuit-gspace (des)
  (or (descriptor-gspace des)

      ;; gspace wasn't set, now we have to search for it.
      (let* ((lowtag (descriptor-lowtag des))
             (abs-word-addr (ash (- (descriptor-bits des) lowtag)
                                 (- sb!vm:word-shift))))

        ;; Non-pointer objects don't have a gspace.
        (unless (or (eql lowtag sb!vm:fun-pointer-lowtag)
                    (eql lowtag sb!vm:instance-pointer-lowtag)
                    (eql lowtag sb!vm:list-pointer-lowtag)
                    (eql lowtag sb!vm:other-pointer-lowtag))
          (error "don't even know how to look for a GSPACE for ~S" des))

        (dolist (gspace (list *dynamic* *static* *read-only*)
                 (error "couldn't find a GSPACE for ~S" des))
          ;; Bounds-check the descriptor against the allocated area
          ;; within each gspace.
          (when (and (<= (gspace-word-address gspace)
                         abs-word-addr
                         (+ (gspace-word-address gspace)
                            (gspace-free-word-index gspace))))
            ;; Update the descriptor with the correct gspace and the
            ;; offset within the gspace and return the gspace.
            (setf (descriptor-word-offset des)
                  (- abs-word-addr (gspace-word-address gspace)))
            (return (setf (descriptor-gspace des) gspace)))))))

(defun %fixnum-descriptor-if-possible (num)
  (and (typep num '(signed-byte #.sb!vm:n-fixnum-bits))
       (make-random-descriptor (ash num sb!vm:n-fixnum-tag-bits))))

(defun make-fixnum-descriptor (num)
  (or (%fixnum-descriptor-if-possible num)
      (error "~W is too big for a fixnum." num)))

(defun make-other-immediate-descriptor (data type)
  (make-descriptor (logior (ash data sb!vm:n-widetag-bits) type)))

(defun make-character-descriptor (data)
  (make-other-immediate-descriptor data sb!vm:character-widetag))

(defun descriptor-beyond (des offset lowtag)
  ;; OFFSET is in bytes and relative to the descriptor as a native pointer,
  ;; not the tagged address. Then we slap on a new lowtag.
  (make-descriptor
   (logior (+ offset (logandc2 (descriptor-bits des) sb!vm:lowtag-mask))
           lowtag)))

;;;; miscellaneous variables and other noise

;;; a numeric value to be returned for undefined foreign symbols, or NIL if
;;; undefined foreign symbols are to be treated as an error.
;;; (In the first pass of GENESIS, needed to create a header file before
;;; the C runtime can be built, various foreign symbols will necessarily
;;; be undefined, but we don't need actual values for them anyway, and
;;; we can just use 0 or some other placeholder. In the second pass of
;;; GENESIS, all foreign symbols should be defined, so any undefined
;;; foreign symbol is a problem.)
;;;
;;; KLUDGE: It would probably be cleaner to rewrite GENESIS so that it
;;; never tries to look up foreign symbols in the first place unless
;;; it's actually creating a core file (as in the second pass) instead
;;; of using this hack to allow it to go through the motions without
;;; causing an error. -- WHN 20000825
(defvar *foreign-symbol-placeholder-value*)

;;; a handle on the trap object
(defvar *unbound-marker*)
;; was:  (make-other-immediate-descriptor 0 sb!vm:unbound-marker-widetag)

;;; a handle on the NIL object
(defvar *nil-descriptor*)

;;; the head of a list of TOPLEVEL-THINGs describing stuff to be done
;;; when the target Lisp starts up
;;;
;;; Each TOPLEVEL-THING can be a function to be executed or a fixup or
;;; loadtime value, represented by (CONS KEYWORD ..).
(defvar *current-reversed-cold-toplevels*) ; except for DEFUNs and SETF macros

;;; the head of a list of DEBUG-SOURCEs which need to be patched when
;;; the cold core starts up
(defvar *current-debug-sources*)

;;; foreign symbol references
(defparameter *cold-foreign-undefined-symbols* nil)

;;;; miscellaneous stuff to read and write the core memory

;;; FIXME: should be DEFINE-MODIFY-MACRO
(defmacro cold-push (thing list)
  "Push THING onto the given cold-load LIST."
  `(setq ,list (cold-cons ,thing ,list)))

;; Like above, but the list is held in the target's image of the host symbol,
;; not the host's value of the symbol.
(defun target-push (cold-thing host-symbol)
  (cold-set host-symbol (cold-cons cold-thing (cold-symbol-value host-symbol))))

(declaim (ftype (function (descriptor sb!vm:word) descriptor) read-wordindexed))
(macrolet ((read-bits ()
             `(bvref-word (descriptor-bytes address)
                          (ash (+ index (descriptor-word-offset address))
                               sb!vm:word-shift))))
  (defun read-bits-wordindexed (address index)
    (read-bits))
  (defun read-wordindexed (address index)
  "Return the value which is displaced by INDEX words from ADDRESS."
    (make-random-descriptor (read-bits))))

(declaim (ftype (function (descriptor) descriptor) read-memory))
(defun read-memory (address)
  "Return the value at ADDRESS."
  (read-wordindexed address 0))

;;; (Note: In CMU CL, this function expected a SAP-typed ADDRESS
;;; value, instead of the object-and-offset we use here.)
(declaim (ftype (function (descriptor sb!vm:word descriptor) (values))
                note-load-time-value-reference))
(defun note-load-time-value-reference (address offset marker)
  (cold-push (cold-list (cold-intern :load-time-value-fixup)
                        address
                        (number-to-core offset)
                        (number-to-core (descriptor-word-offset marker)))
             *current-reversed-cold-toplevels*)
  (values))

(declaim (ftype (function (descriptor sb!vm:word (or symbol descriptor))) write-wordindexed))
(defun write-wordindexed (address index value)
  "Write VALUE displaced INDEX words from ADDRESS."
  ;; If we're passed a symbol as a value then it needs to be interned.
  (let ((value (cond ((symbolp value) (cold-intern value))
                     (t value))))
    (if (eql (descriptor-gspace value) :load-time-value)
        (note-load-time-value-reference address
                                        (- (ash index sb!vm:word-shift)
                                           (logand (descriptor-bits address)
                                                   sb!vm:lowtag-mask))
                                        value)
        (let* ((bytes (descriptor-bytes address))
               (byte-index (ash (+ index (descriptor-word-offset address))
                                sb!vm:word-shift)))
          (setf (bvref-word bytes byte-index) (descriptor-bits value))))))

(declaim (ftype (function (descriptor (or symbol descriptor))) write-memory))
(defun write-memory (address value)
  "Write VALUE (a DESCRIPTOR) at ADDRESS (also a DESCRIPTOR)."
  (write-wordindexed address 0 value))

;;;; allocating images of primitive objects in the cold core

(defun write-header-word (des header-data widetag)
  (write-memory des (make-other-immediate-descriptor header-data widetag)))

;;; There are three kinds of blocks of memory in the type system:
;;; * Boxed objects (cons cells, structures, etc): These objects have no
;;;   header as all slots are descriptors.
;;; * Unboxed objects (bignums): There is a single header word that contains
;;;   the length.
;;; * Vector objects: There is a header word with the type, then a word for
;;;   the length, then the data.
(defun allocate-object (gspace length lowtag)
  "Allocate LENGTH words in GSPACE and return a new descriptor of type LOWTAG
  pointing to them."
  (allocate-cold-descriptor gspace (ash length sb!vm:word-shift) lowtag))
(defun allocate-header+object (gspace length widetag)
  "Allocate LENGTH words plus a header word in GSPACE and
  return an ``other-pointer'' descriptor to them. Initialize the header word
  with the resultant length and WIDETAG."
  (let ((des (allocate-cold-descriptor gspace
                                       (ash (1+ length) sb!vm:word-shift)
                                       sb!vm:other-pointer-lowtag)))
    (write-header-word des length widetag)
    des))
(defun allocate-vector-object (gspace element-bits length widetag)
  "Allocate LENGTH units of ELEMENT-BITS size plus a header plus a length slot in
  GSPACE and return an ``other-pointer'' descriptor to them. Initialize the
  header word with WIDETAG and the length slot with LENGTH."
  ;; ALLOCATE-COLD-DESCRIPTOR will take any rational number of bytes
  ;; and round up to a double-word. This doesn't need to use CEILING.
  (let* ((bytes (/ (* element-bits length) sb!vm:n-byte-bits))
         (des (allocate-cold-descriptor gspace
                                        (+ bytes (* 2 sb!vm:n-word-bytes))
                                        sb!vm:other-pointer-lowtag)))
    (write-header-word des 0 widetag)
    (write-wordindexed des
                       sb!vm:vector-length-slot
                       (make-fixnum-descriptor length))
    des))

;;; the hosts's representation of LAYOUT-of-LAYOUT
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *host-layout-of-layout* (find-layout 'layout)))

(defun cold-layout-length (layout)
  (descriptor-fixnum (read-slot layout *host-layout-of-layout* :length)))

;; Make a structure and set the header word and layout.
;; LAYOUT-LENGTH is as returned by the like-named function.
(defun allocate-struct
    (gspace layout &optional (layout-length (cold-layout-length layout)))
  ;; The math in here is best illustrated by two examples:
  ;; even: size 4 => request to allocate 5 => rounds up to 6, logior => 5
  ;; odd : size 5 => request to allocate 6 => no rounding up, logior => 5
  ;; In each case, the length of the memory block is even.
  ;; ALLOCATE-OBJECT performs the rounding. It must be supplied
  ;; the number of words minimally needed, counting the header itself.
  ;; The number written into the header (%INSTANCE-LENGTH) is always odd.
  (let ((des (allocate-object gspace (1+ layout-length)
                              sb!vm:instance-pointer-lowtag)))
    (write-header-word des (logior layout-length 1)
                       sb!vm:instance-header-widetag)
    (write-wordindexed des sb!vm:instance-slots-offset layout)
    des))

;;;; copying simple objects into the cold core

(defun base-string-to-core (string &optional (gspace *dynamic*))
  "Copy STRING (which must only contain STANDARD-CHARs) into the cold
core and return a descriptor to it."
  ;; (Remember that the system convention for storage of strings leaves an
  ;; extra null byte at the end to aid in call-out to C.)
  (let* ((length (length string))
         (des (allocate-vector-object gspace
                                      sb!vm:n-byte-bits
                                      (1+ length)
                                      sb!vm:simple-base-string-widetag))
         (bytes (gspace-bytes gspace))
         (offset (+ (* sb!vm:vector-data-offset sb!vm:n-word-bytes)
                    (descriptor-byte-offset des))))
    (write-wordindexed des
                       sb!vm:vector-length-slot
                       (make-fixnum-descriptor length))
    (dotimes (i length)
      (setf (bvref bytes (+ offset i))
            (sb!xc:char-code (aref string i))))
    (setf (bvref bytes (+ offset length))
          0) ; null string-termination character for C
    des))

(defun base-string-from-core (descriptor)
  (let* ((len (descriptor-fixnum
               (read-wordindexed descriptor sb!vm:vector-length-slot)))
         (str (make-string len))
         (bytes (descriptor-bytes descriptor)))
    (dotimes (i len str)
      (setf (aref str i)
            (code-char (bvref bytes
                              (+ (descriptor-byte-offset descriptor)
                                 (* sb!vm:vector-data-offset sb!vm:n-word-bytes)
                                 i)))))))

(defun bignum-to-core (n)
  "Copy a bignum to the cold core."
  (let* ((words (ceiling (1+ (integer-length n)) sb!vm:n-word-bits))
         (handle
          (allocate-header+object *dynamic* words sb!vm:bignum-widetag)))
    (declare (fixnum words))
    (do ((index 1 (1+ index))
         (remainder n (ash remainder (- sb!vm:n-word-bits))))
        ((> index words)
         (unless (zerop (integer-length remainder))
           ;; FIXME: Shouldn't this be a fatal error?
           (warn "~W words of ~W were written, but ~W bits were left over."
                 words n remainder)))
        ;; FIXME: this is disgusting. there should be WRITE-BITS-WORDINDEXED.
      (write-wordindexed handle index
                         (make-random-descriptor
                          (ldb (byte sb!vm:n-word-bits 0) remainder))))
    handle))

(defun bignum-from-core (descriptor)
  (let ((n-words (ash (descriptor-bits (read-memory descriptor))
                      (- sb!vm:n-widetag-bits)))
        (val 0))
    (dotimes (i n-words val)
      (let ((bits (read-bits-wordindexed descriptor
                                         (+ i sb!vm:bignum-digits-offset))))
        ;; sign-extend the highest word
        (when (and (= i (1- n-words)) (logbitp (1- sb!vm:n-word-bits) bits))
          (setq bits (dpb bits (byte sb!vm:n-word-bits 0) -1)))
        (setq val (logior (ash bits (* i sb!vm:n-word-bits)) val))))))

(defun number-pair-to-core (first second type)
  "Makes a number pair of TYPE (ratio or complex) and fills it in."
  (let ((des (allocate-header+object *dynamic* 2 type)))
    (write-wordindexed des 1 first)
    (write-wordindexed des 2 second)
    des))

(defun write-double-float-bits (address index x)
  (let ((hi (double-float-high-bits x))
        (lo (double-float-low-bits x)))
    (ecase sb!vm::n-word-bits
      (32
       ;; As noted in BIGNUM-TO-CORE, this idiom is unclear - the things
       ;; aren't descriptors. Same for COMPLEX-foo-TO-CORE
       (let ((high-bits (make-random-descriptor hi))
             (low-bits (make-random-descriptor lo)))
         (ecase sb!c:*backend-byte-order*
           (:little-endian
            (write-wordindexed address index low-bits)
            (write-wordindexed address (1+ index) high-bits))
           (:big-endian
            (write-wordindexed address index high-bits)
            (write-wordindexed address (1+ index) low-bits)))))
      (64
       (let ((bits (make-random-descriptor
                    (ecase sb!c:*backend-byte-order*
                      (:little-endian (logior lo (ash hi 32)))
                      ;; Just guessing.
                      #+nil (:big-endian (logior (logand hi #xffffffff)
                                                 (ash lo 32)))))))
         (write-wordindexed address index bits))))
    address))

(defun float-to-core (x)
  (etypecase x
    (single-float
     ;; 64-bit platforms have immediate single-floats.
     #!+64-bit
     (make-random-descriptor (logior (ash (single-float-bits x) 32)
                                     sb!vm::single-float-widetag))
     #!-64-bit
     (let ((des (allocate-header+object *dynamic*
                                         (1- sb!vm:single-float-size)
                                         sb!vm:single-float-widetag)))
       (write-wordindexed des
                          sb!vm:single-float-value-slot
                          (make-random-descriptor (single-float-bits x)))
       des))
    (double-float
     (let ((des (allocate-header+object *dynamic*
                                         (1- sb!vm:double-float-size)
                                         sb!vm:double-float-widetag)))
       (write-double-float-bits des sb!vm:double-float-value-slot x)))))

(defun complex-single-float-to-core (num)
  (declare (type (complex single-float) num))
  (let ((des (allocate-header+object *dynamic*
                                      (1- sb!vm:complex-single-float-size)
                                      sb!vm:complex-single-float-widetag)))
    #!-64-bit
    (progn
      (write-wordindexed des sb!vm:complex-single-float-real-slot
                         (make-random-descriptor (single-float-bits (realpart num))))
      (write-wordindexed des sb!vm:complex-single-float-imag-slot
                         (make-random-descriptor (single-float-bits (imagpart num)))))
    #!+64-bit
    (write-wordindexed des sb!vm:complex-single-float-data-slot
                       (make-random-descriptor
                        (logior (ldb (byte 32 0) (single-float-bits (realpart num)))
                                (ash (single-float-bits (imagpart num)) 32))))
    des))

(defun complex-double-float-to-core (num)
  (declare (type (complex double-float) num))
  (let ((des (allocate-header+object *dynamic*
                                      (1- sb!vm:complex-double-float-size)
                                      sb!vm:complex-double-float-widetag)))
    (write-double-float-bits des sb!vm:complex-double-float-real-slot
                             (realpart num))
    (write-double-float-bits des sb!vm:complex-double-float-imag-slot
                             (imagpart num))))

;;; Copy the given number to the core.
(defun number-to-core (number)
  (typecase number
    (integer (or (%fixnum-descriptor-if-possible number)
                 (bignum-to-core number)))
    (ratio (number-pair-to-core (number-to-core (numerator number))
                                (number-to-core (denominator number))
                                sb!vm:ratio-widetag))
    ((complex single-float) (complex-single-float-to-core number))
    ((complex double-float) (complex-double-float-to-core number))
    #!+long-float
    ((complex long-float)
     (error "~S isn't a cold-loadable number at all!" number))
    (complex (number-pair-to-core (number-to-core (realpart number))
                                  (number-to-core (imagpart number))
                                  sb!vm:complex-widetag))
    (float (float-to-core number))
    (t (error "~S isn't a cold-loadable number at all!" number))))

(declaim (ftype (function (sb!vm:word) descriptor) sap-int-to-core))
(defun sap-int-to-core (sap-int)
  (let ((des (allocate-header+object *dynamic* (1- sb!vm:sap-size)
                                      sb!vm:sap-widetag)))
    (write-wordindexed des
                       sb!vm:sap-pointer-slot
                       (make-random-descriptor sap-int))
    des))

;;; Allocate a cons cell in GSPACE and fill it in with CAR and CDR.
(defun cold-cons (car cdr &optional (gspace *dynamic*))
  (let ((dest (allocate-object gspace 2 sb!vm:list-pointer-lowtag)))
    (write-wordindexed dest sb!vm:cons-car-slot car)
    (write-wordindexed dest sb!vm:cons-cdr-slot cdr)
    dest))
(defun cold-list (&rest args)
  (let ((head *nil-descriptor*)
        (tail nil))
    ;; A recursive algorithm would have the first cons at the highest
    ;; address. This way looks nicer when viewed in ldb.
    (loop
     (unless args (return head))
     (let ((cons (cold-cons (pop args) *nil-descriptor*)))
       (if tail (cold-rplacd tail cons) (setq head cons))
       (setq tail cons)))))
(defun cold-list-length (list) ; but no circularity detection
  ;; a recursive implementation uses too much stack for some Lisps
  (let ((n 0))
    (loop (if (cold-null list) (return n))
          (incf n)
          (setq list (cold-cdr list)))))

;;; Make a simple-vector on the target that holds the specified
;;; OBJECTS, and return its descriptor.
;;; This is really "vectorify-list-into-core" but that's too wordy,
;;; so historically it was "vector-in-core" which is a fine name.
(defun vector-in-core (objects &optional (gspace *dynamic*))
  (let* ((size (length objects))
         (result (allocate-vector-object gspace sb!vm:n-word-bits size
                                         sb!vm:simple-vector-widetag)))
    (dotimes (index size)
      (write-wordindexed result (+ index sb!vm:vector-data-offset)
                         (pop objects)))
    result))
(defun cold-svset (vector index value)
  (write-wordindexed vector
                     (+ (descriptor-fixnum index) sb!vm:vector-data-offset)
                     value))

(declaim (inline cold-vector-len cold-svref))
(defun cold-vector-len (vector)
  (descriptor-fixnum (read-wordindexed vector sb!vm:vector-length-slot)))
(defun cold-svref (vector i)
  (read-wordindexed vector (+ (if (integerp i) i (descriptor-fixnum i))
                              sb!vm:vector-data-offset)))
(defun cold-vector-elements-eq (a b)
  (and (eql (cold-vector-len a) (cold-vector-len b))
       (dotimes (k (cold-vector-len a) t)
         (unless (descriptor= (cold-svref a k) (cold-svref b k))
           (return nil)))))
(defun vector-from-core (descriptor &optional (transform #'identity))
  (let* ((len (cold-vector-len descriptor))
         (vector (make-array len)))
    (dotimes (i len vector)
      (setf (aref vector i) (funcall transform (cold-svref descriptor i))))))

;;;; symbol magic

;; Simulate *FREE-TLS-INDEX*. This is a count, not a displacement.
;; In C, sizeof counts 1 word for the variable-length interrupt_contexts[]
;; but primitive-object-size counts 0, so add 1, though in fact the C code
;; implies that it might have overcounted by 1. We could make this agnostic
;; of MAX-INTERRUPTS by moving the thread base register up by TLS-SIZE words,
;; using negative offsets for all dynamically assigned indices.
(defvar *genesis-tls-counter*
  (+ 1 sb!vm::max-interrupts
     (sb!vm:primitive-object-size
      (find 'sb!vm::thread sb!vm:*primitive-objects*
            :key #'sb!vm:primitive-object-name))))

#!+sb-thread
(progn
  ;; Assign SYMBOL the tls-index INDEX. SYMBOL must be a descriptor.
  ;; This is a backend support routine, but the style within this file
  ;; is to conditionalize by the target features.
  (defun cold-assign-tls-index (symbol index)
    #!+64-bit
    (let ((header-word
           (logior (ash index 32)
                   (descriptor-bits (read-memory symbol)))))
      (write-wordindexed symbol 0 (make-random-descriptor header-word)))
    #!-64-bit
    (write-wordindexed symbol sb!vm:symbol-tls-index-slot
                       (make-random-descriptor index)))

  ;; Return SYMBOL's tls-index,
  ;; choosing a new index if it doesn't have one yet.
  (defun ensure-symbol-tls-index (symbol)
    (let* ((cold-sym (cold-intern symbol))
           (tls-index
            #!+64-bit
            (ldb (byte 32 32) (descriptor-bits (read-memory cold-sym)))
            #!-64-bit
            (descriptor-bits
             (read-wordindexed cold-sym sb!vm:symbol-tls-index-slot))))
      (unless (plusp tls-index)
        (let ((next (prog1 *genesis-tls-counter* (incf *genesis-tls-counter*))))
          (setq tls-index (ash next sb!vm:word-shift))
          (cold-assign-tls-index cold-sym tls-index)))
      tls-index)))

;; A table of special variable names which get known TLS indices.
;; Some of them are mapped onto 'struct thread' and have pre-determined offsets.
;; Others are static symbols used with bind_variable() in the C runtime,
;; and might not, in the absence of this table, get an index assigned by genesis
;; depending on whether the cross-compiler used the BIND vop on them.
;; Indices for those static symbols can be chosen arbitrarily, which is to say
;; the value doesn't matter but must update the tls-counter correctly.
;; All symbols other than the ones in this table get the indices assigned
;; by the fasloader on demand.
#!+sb-thread
(defvar *known-tls-symbols*
    ;; FIXME: no mechanism exists to determine which static symbols C code will
    ;; dynamically bind. TLS is a finite resource, and wasting indices for all
    ;; static symbols isn't the best idea. This list was hand-made with 'grep'.
                  '(sb!vm:*alloc-signal*
                    sb!sys:*allow-with-interrupts*
                    sb!vm:*current-catch-block*
                    sb!vm::*current-unwind-protect-block*
                    sb!kernel:*free-interrupt-context-index*
                    sb!kernel:*gc-inhibit*
                    sb!kernel:*gc-pending*
                    sb!impl::*gc-safe*
                    sb!impl::*in-safepoint*
                    sb!sys:*interrupt-pending*
                    sb!sys:*interrupts-enabled*
                    sb!vm::*pinned-objects*
                    sb!kernel:*restart-clusters*
                    sb!kernel:*stop-for-gc-pending*
                    #!+sb-thruption
                    sb!sys:*thruption-pending*))

;;; Symbol print names are coalesced by string=.
;;; This is valid because it is an error to modify a print name.
(defvar *symbol-name-strings* (make-hash-table :test 'equal))

;;; Allocate (and initialize) a symbol.
(defun allocate-symbol (name &key (gspace *dynamic*))
  (declare (simple-string name))
  (let ((symbol (allocate-header+object gspace (1- sb!vm:symbol-size)
                                        sb!vm:symbol-header-widetag)))
    (write-wordindexed symbol sb!vm:symbol-value-slot *unbound-marker*)
    (write-wordindexed symbol sb!vm:symbol-hash-slot (make-fixnum-descriptor 0))
    (write-wordindexed symbol sb!vm:symbol-info-slot *nil-descriptor*)
    (write-wordindexed symbol sb!vm:symbol-name-slot
                       (or (gethash name *symbol-name-strings*)
                           (setf (gethash name *symbol-name-strings*)
                                 (base-string-to-core name *dynamic*))))
    (write-wordindexed symbol sb!vm:symbol-package-slot *nil-descriptor*)
    symbol))

#!+sb-thread
(defun assign-tls-index (symbol cold-symbol)
  (let ((index (info :variable :wired-tls symbol)))
    (cond ((integerp index) ; thread slot
           (cold-assign-tls-index cold-symbol index))
          ((memq symbol *known-tls-symbols*)
           ;; symbols without which the C runtime could not start
           (shiftf index *genesis-tls-counter* (1+ *genesis-tls-counter*))
           (cold-assign-tls-index cold-symbol (ash index sb!vm:word-shift))))))

;;; Set the cold symbol value of SYMBOL-OR-SYMBOL-DES, which can be either a
;;; descriptor of a cold symbol or (in an abbreviation for the
;;; most common usage pattern) an ordinary symbol, which will be
;;; automatically cold-interned.
(declaim (ftype (function ((or symbol descriptor) descriptor)) cold-set))
(defun cold-set (symbol-or-symbol-des value)
  (let ((symbol-des (etypecase symbol-or-symbol-des
                      (descriptor symbol-or-symbol-des)
                      (symbol (cold-intern symbol-or-symbol-des)))))
    (write-wordindexed symbol-des sb!vm:symbol-value-slot value)))
(defun cold-symbol-value (symbol)
  (let ((val (read-wordindexed (cold-intern symbol) sb!vm:symbol-value-slot)))
    (if (= (descriptor-bits val) sb!vm:unbound-marker-widetag)
        (error "Taking Cold-symbol-value of unbound symbol ~S" symbol)
        val)))
(defun cold-fdefn-fun (cold-fdefn)
  (read-wordindexed cold-fdefn sb!vm:fdefn-fun-slot))

;;;; layouts and type system pre-initialization

;;; Since we want to be able to dump structure constants and
;;; predicates with reference layouts, we need to create layouts at
;;; cold-load time. We use the name to intern layouts by, and dump a
;;; list of all cold layouts in *!INITIAL-LAYOUTS* so that type system
;;; initialization can find them. The only thing that's tricky [sic --
;;; WHN 19990816] is initializing layout's layout, which must point to
;;; itself.

;;; a map from name as a host symbol to the descriptor of its target layout
(defvar *cold-layouts* (make-hash-table :test 'equal))

;;; a map from DESCRIPTOR-BITS of cold layouts to the name, for inverting
;;; mapping
(defvar *cold-layout-names* (make-hash-table :test 'eql))

;;; FIXME: *COLD-LAYOUTS* and *COLD-LAYOUT-NAMES* should be
;;; initialized by binding in GENESIS.

;;; the descriptor for layout's layout (needed when making layouts)
(defvar *layout-layout*)
;;; the descriptor for PACKAGE's layout (needed when making packages)
(defvar *package-layout*)

(defvar *known-structure-classoids*)

(defconstant target-layout-length
  ;; LAYOUT-LENGTH counts the number of words in an instance,
  ;; including the layout itself as 1 word
  (layout-length *host-layout-of-layout*))

;;; Return a list of names created from the cold layout INHERITS data
;;; in X.
(defun listify-cold-inherits (x)
  (map 'list (lambda (des)
               (or (gethash (descriptor-bits des) *cold-layout-names*)
                   (error "~S is not the descriptor of a cold-layout" des)))
       (vector-from-core x)))

(flet ((get-slots (host-layout-or-type)
         (etypecase host-layout-or-type
           (layout (dd-slots (layout-info host-layout-or-type)))
           (symbol (dd-slots-from-core host-layout-or-type))))
       (get-slot-index (slots initarg)
         (+ sb!vm:instance-slots-offset
            (if (descriptor-p slots)
                (do ((dsd-layout (find-layout 'defstruct-slot-description))
                     (slots slots (cold-cdr slots)))
                    ((cold-null slots) (error "No slot for ~S" initarg))
                  (let* ((dsd (cold-car slots))
                         (slot-name (read-slot dsd dsd-layout :name)))
                    (when (eq (keywordicate (warm-symbol slot-name)) initarg)
                      ;; Untagged slots are not accessible during cold-load
                      (aver (eql (descriptor-fixnum
                                  (read-slot dsd dsd-layout :%raw-type)) -1))
                      (return (descriptor-fixnum
                               (read-slot dsd dsd-layout :index))))))
                (let ((dsd (find initarg slots
                                 :test (lambda (x y)
                                         (eq x (keywordicate (dsd-name y)))))))
                  (aver (eq (dsd-raw-type dsd) t)) ; Same as above: no can do.
                  (dsd-index dsd))))))
  (defun write-slots (cold-object host-layout-or-type &rest assignments)
    (aver (evenp (length assignments)))
    (let ((slots (get-slots host-layout-or-type)))
      (loop for (initarg value) on assignments by #'cddr
            do (write-wordindexed
                cold-object (get-slot-index slots initarg) value)))
    cold-object)

  ;; For symmetry, the reader takes an initarg, not a slot name.
  (defun read-slot (cold-object host-layout-or-type slot-initarg)
    (let ((slots (get-slots host-layout-or-type)))
      (read-wordindexed cold-object (get-slot-index slots slot-initarg)))))

;; Given a TYPE-NAME of a structure-class, find its defstruct-description
;; as a target descriptor, and return the slot list as a target descriptor.
(defun dd-slots-from-core (type-name)
  (let* ((host-dd-layout (find-layout 'defstruct-description))
         (target-dd
          ;; This is inefficient, but not enough so to worry about.
          (or (car (assoc (cold-intern type-name) *known-structure-classoids*
                          :key (lambda (x) (read-slot x host-dd-layout :name))
                          :test #'descriptor=))
              (error "No known layout for ~S" type-name))))
    (read-slot target-dd host-dd-layout :slots)))

(defvar *simple-vector-0-descriptor*)
(defvar *vacuous-slot-table*)
(declaim (ftype (function (symbol descriptor descriptor descriptor descriptor)
                          descriptor)
                make-cold-layout))
(defun make-cold-layout (name length inherits depthoid metadata)
  (let ((result (allocate-struct *dynamic* *layout-layout*
                                 target-layout-length)))
    ;; Don't set the CLOS hash value: done in cold-init instead.
    ;;
    ;; Set other slot values.
    ;;
    ;; leave CLASSOID uninitialized for now
    (multiple-value-call
     #'write-slots result *host-layout-of-layout*
     :invalid *nil-descriptor*
     :inherits inherits
     :depthoid depthoid
     :length length
     :info *nil-descriptor*
     :pure *nil-descriptor*
     #!-interleaved-raw-slots
     (values :n-untagged-slots metadata)
     #!+interleaved-raw-slots
     (values :untagged-bitmap metadata
      ;; Nothing in cold-init needs to call EQUALP on a structure with raw slots,
      ;; but for type-correctness this slot needs to be a simple-vector.
             :equalp-tests (if (boundp '*simple-vector-0-descriptor*)
                               *simple-vector-0-descriptor*
                               (setq *simple-vector-0-descriptor*
                                     (vector-in-core nil))))
     :source-location *nil-descriptor*
     :%for-std-class-b (make-fixnum-descriptor 0)
     :slot-list *nil-descriptor*
     (if (member name '(null list symbol))
      ;; Assign an empty slot-table.  Why this is done only for three
      ;; classoids is ... too complicated to explain here in a few words,
      ;; but revision 18c239205d9349abc017b07e7894a710835c5205 broke it.
      ;; Keep this in sync with MAKE-SLOT-TABLE in pcl/slots-boot.
         (values :slot-table (if (boundp '*vacuous-slot-table*)
                                 *vacuous-slot-table*
                                 (setq *vacuous-slot-table*
                                       (host-constant-to-core '#(1 nil)))))
         (values)))

    (setf (gethash (descriptor-bits result) *cold-layout-names*) name
          (gethash name *cold-layouts*) result)))

;; This is called to backpatch three small sets of objects:
;;  - layouts which are made before layout-of-layout is made (4 of them)
;;  - packages, which are made before layout-of-package is made (all of them)
;;  - a small number of classoid-cells (probably 3 or 4).
(defun patch-instance-layout (thing layout)
  ;; Layout pointer is in the word following the header
  (write-wordindexed thing sb!vm:instance-slots-offset layout))

(defun cold-layout-of (cold-struct)
  (read-wordindexed cold-struct sb!vm:instance-slots-offset))

(defun initialize-layouts ()
  (clrhash *cold-layouts*)
  ;; This assertion is due to the fact that MAKE-COLD-LAYOUT does not
  ;; know how to set any raw slots.
  (aver (= 0 (layout-raw-slot-metadata *host-layout-of-layout*)))
  (setq *layout-layout* (make-fixnum-descriptor 0))
  (flet ((chill-layout (name &rest inherits)
           ;; Check that the number of specified INHERITS matches
           ;; the length of the layout's inherits in the cross-compiler.
           (let ((warm-layout (classoid-layout (find-classoid name))))
             (assert (eql (length (layout-inherits warm-layout))
                          (length inherits)))
             (make-cold-layout
              name
              (number-to-core (layout-length warm-layout))
              (vector-in-core inherits)
              (number-to-core (layout-depthoid warm-layout))
              (number-to-core (layout-raw-slot-metadata warm-layout))))))
    (let* ((t-layout   (chill-layout 't))
           (s-o-layout (chill-layout 'structure-object t-layout))
           (s!o-layout (chill-layout 'structure!object t-layout s-o-layout)))
      (setf *layout-layout*
            (chill-layout 'layout t-layout s-o-layout s!o-layout))
      (dolist (layout (list t-layout s-o-layout s!o-layout *layout-layout*))
        (patch-instance-layout layout *layout-layout*))
      (setf *package-layout*
            (chill-layout 'package t-layout s-o-layout)))))

;;;; interning symbols in the cold image

;;; a map from package name as a host string to
;;; (cold-package-descriptor . (external-symbols . internal-symbols))
(defvar *cold-package-symbols*)
(declaim (type hash-table *cold-package-symbols*))

(setf (get 'find-package :sb-cold-funcall-handler/for-value)
      (lambda (descriptor &aux (name (base-string-from-core descriptor)))
        (or (car (gethash name *cold-package-symbols*))
            (error "Genesis could not find a target package named ~S" name))))

(defvar *classoid-cells*)
(setf (get 'find-classoid-cell :sb-cold-funcall-handler/for-value)
      (lambda (name &key create)
        (aver (eq create t))
        (or (gethash name *classoid-cells*)
            (let ((layout (gethash 'sb!kernel::classoid-cell *cold-layouts*))
                  (host-layout (find-layout 'sb!kernel::classoid-cell)))
              (setf (gethash name *classoid-cells*)
                    (write-slots (allocate-struct *dynamic* layout
                                                  (layout-length host-layout))
                                 host-layout
                                 :name name
                                 :pcl-class *nil-descriptor*
                                 :classoid *nil-descriptor*))))))

;;; a map from descriptors to symbols, so that we can back up. The key
;;; is the address in the target core.
(defvar *cold-symbols*)
(declaim (type hash-table *cold-symbols*))

(defun initialize-packages (package-data-list)
  (let ((package-layout (find-layout 'package))
        (target-pkg-list nil))
    (labels ((init-cold-package (name &optional docstring)
               (let ((cold-package (car (gethash name *cold-package-symbols*))))
                 ;; patch in the layout
                 (patch-instance-layout cold-package *package-layout*)
                 ;; Initialize string slots
                 (write-slots cold-package package-layout
                              :%name (base-string-to-core
                                      (target-package-name name))
                              :%nicknames (chill-nicknames name)
                              :doc-string (if docstring
                                              (base-string-to-core docstring)
                                              *nil-descriptor*)
                              :%use-list *nil-descriptor*)
                 ;; the cddr of this will accumulate the 'used-by' package list
                 (push (list name cold-package) target-pkg-list)))
             (target-package-name (string)
               (if (eql (mismatch string "SB!") 3)
                   (concatenate 'string "SB-" (subseq string 3))
                   string))
             (chill-nicknames (pkg-name)
               (let ((result *nil-descriptor*))
                 ;; Make the package nickname lists for the standard packages
                 ;; be the minimum specified by ANSI, regardless of what value
                 ;; the cross-compilation host happens to use.
                 ;; For packages other than the standard packages, the nickname
                 ;; list was specified by our package setup code, and we can just
                 ;; propagate the current state into the target.
                 (dolist (nickname
                          (cond ((string= pkg-name "COMMON-LISP") '("CL"))
                                ((string= pkg-name "COMMON-LISP-USER")
                                 '("CL-USER"))
                                ((string= pkg-name "KEYWORD") '())
                                (t
                                 ;; 'package-data-list' contains no nicknames.
                                 ;; (See comment in 'set-up-cold-packages')
                                 (aver (null (package-nicknames
                                              (find-package pkg-name))))
                                 nil))
                          result)
                   (cold-push (base-string-to-core nickname) result))))
             (find-cold-package (name)
               (cadr (find-package-cell name)))
             (find-package-cell (name)
               (or (assoc (if (string= name "CL") "COMMON-LISP" name)
                          target-pkg-list :test #'string=)
                   (error "No cold package named ~S" name))))
      ;; pass 1: make all proto-packages
      (dolist (pd package-data-list)
        (init-cold-package (sb-cold:package-data-name pd)
                           #!+sb-doc(sb-cold::package-data-doc pd)))
      ;; pass 2: set the 'use' lists and collect the 'used-by' lists
      (dolist (pd package-data-list)
        (let ((this (find-cold-package (sb-cold:package-data-name pd)))
              (use nil))
          (dolist (that (sb-cold:package-data-use pd))
            (let ((cell (find-package-cell that)))
              (push (cadr cell) use)
              (push this (cddr cell))))
          (write-slots this package-layout
                       :%use-list (apply 'cold-list (nreverse use)))))
      ;; pass 3: set the 'used-by' lists
      (dolist (cell target-pkg-list)
        (write-slots (cadr cell) package-layout
                     :%used-by-list (apply 'cold-list (cddr cell)))))))

;;; sanity check for a symbol we're about to create on the target
;;;
;;; Make sure that the symbol has an appropriate package. In
;;; particular, catch the so-easy-to-make error of typing something
;;; like SB-KERNEL:%BYTE-BLT in cold sources when what you really
;;; need is SB!KERNEL:%BYTE-BLT.
(defun package-ok-for-target-symbol-p (package)
  (let ((package-name (package-name package)))
    (or
     ;; Cold interning things in these standard packages is OK. (Cold
     ;; interning things in the other standard package, CL-USER, isn't
     ;; OK. We just use CL-USER to expose symbols whose homes are in
     ;; other packages. Thus, trying to cold intern a symbol whose
     ;; home package is CL-USER probably means that a coding error has
     ;; been made somewhere.)
     (find package-name '("COMMON-LISP" "KEYWORD") :test #'string=)
     ;; Cold interning something in one of our target-code packages,
     ;; which are ever-so-rigorously-and-elegantly distinguished by
     ;; this prefix on their names, is OK too.
     (string= package-name "SB!" :end1 3 :end2 3)
     ;; This one is OK too, since it ends up being COMMON-LISP on the
     ;; target.
     (string= package-name "SB-XC")
     ;; Anything else looks bad. (maybe COMMON-LISP-USER? maybe an extension
     ;; package in the xc host? something we can't think of
     ;; a valid reason to cold intern, anyway...)
     )))

;;; like SYMBOL-PACKAGE, but safe for symbols which end up on the target
;;;
;;; Most host symbols we dump onto the target are created by SBCL
;;; itself, so that as long as we avoid gratuitously
;;; cross-compilation-unfriendly hacks, it just happens that their
;;; SYMBOL-PACKAGE in the host system corresponds to their
;;; SYMBOL-PACKAGE in the target system. However, that's not the case
;;; in the COMMON-LISP package, where we don't get to create the
;;; symbols but instead have to use the ones that the xc host created.
;;; In particular, while ANSI specifies which symbols are exported
;;; from COMMON-LISP, it doesn't specify that their home packages are
;;; COMMON-LISP, so the xc host can keep them in random packages which
;;; don't exist on the target (e.g. CLISP keeping some CL-exported
;;; symbols in the CLOS package).
(defun symbol-package-for-target-symbol (symbol)
  ;; We want to catch weird symbols like CLISP's
  ;; CL:FIND-METHOD=CLOS::FIND-METHOD, but we don't want to get
  ;; sidetracked by ordinary symbols like :CHARACTER which happen to
  ;; have the same SYMBOL-NAME as exports from COMMON-LISP.
  (multiple-value-bind (cl-symbol cl-status)
      (find-symbol (symbol-name symbol) *cl-package*)
    (if (and (eq symbol cl-symbol)
             (eq cl-status :external))
        ;; special case, to work around possible xc host weirdness
        ;; in COMMON-LISP package
        *cl-package*
        ;; ordinary case
        (let ((result (symbol-package symbol)))
          (unless (package-ok-for-target-symbol-p result)
            (bug "~A in bad package for target: ~A" symbol result))
          result))))

(defvar *uninterned-symbol-table* (make-hash-table :test #'equal))
;; This coalesces references to uninterned symbols, which is allowed because
;; "similar-as-constant" is defined by string comparison, and since we only have
;; base-strings during Genesis, there is no concern about upgraded array type.
;; There is a subtlety of whether coalescing may occur across files
;; - the target compiler doesn't and couldn't - but here it doesn't matter.
(defun get-uninterned-symbol (name)
  (or (gethash name *uninterned-symbol-table*)
      (let ((cold-symbol (allocate-symbol name)))
        (setf (gethash name *uninterned-symbol-table*) cold-symbol))))

;;; Dump the target representation of HOST-VALUE,
;;; the type of which is in a restrictive set.
(defun host-constant-to-core (host-value &optional (sanity-check t))
  ;; rough check for no shared substructure and/or circularity.
  ;; of course this would be wrong if it were a string containing "#1="
  (when (and sanity-check
             (search "#1=" (write-to-string host-value :circle t :readably t)))
    (warn "Strange constant to core from Genesis: ~S" host-value))
  (labels ((target-representation (value)
             (etypecase value
               (descriptor value)
               (symbol (if (symbol-package value)
                           (cold-intern value)
                           (get-uninterned-symbol (string value))))
               (number (number-to-core value))
               (string (base-string-to-core value))
               (cons (cold-cons (target-representation (car value))
                                (target-representation (cdr value))))
               (simple-vector
                (vector-in-core (map 'list #'target-representation value))))))
    (target-representation host-value)))

;; Look up the target's descriptor for #'FUN where FUN is a host symbol.
(defun target-symbol-function (symbol)
  (let ((f (cold-fdefn-fun (cold-fdefinition-object symbol))))
    ;; It works only if DEFUN F was seen first.
    (aver (not (cold-null f)))
    f))

;;; Create the effect of executing a (MAKE-ARRAY) call on the target.
;;; This is for initializing a restricted set of vector constants
;;; whose contents are typically function pointers.
(defun emulate-target-make-array (form)
  (destructuring-bind (size-expr &key initial-element) (cdr form)
    (let* ((size (eval size-expr))
           (result (allocate-vector-object *dynamic* sb!vm:n-word-bits size
                                           sb!vm:simple-vector-widetag)))
      (aver (integerp size))
      (unless (eql initial-element 0)
        (let ((target-initial-element
               (etypecase initial-element
                 ((cons (eql function) (cons symbol null))
                  (target-symbol-function (second initial-element)))
                 (null *nil-descriptor*)
                 ;; Insert more types here ...
                 )))
          (dotimes (index size)
            (cold-svset result (make-fixnum-descriptor index)
                        target-initial-element))))
      result)))

;; Return a target object produced by emulating evaluation of EXPR
;; with *package* set to ORIGINAL-PACKAGE.
(defun emulate-target-eval (expr original-package)
  (let ((*package* (find-package original-package)))
    ;; For most things, just call EVAL and dump the host object's
    ;; target representation. But with MAKE-ARRAY we allow that the
    ;; initial-element might not be evaluable in the host.
    ;; Embedded MAKE-ARRAY is kept as-is because we don't "look into"
    ;; the EXPR, just hope that it works.
    (if (typep expr '(cons (eql make-array)))
        (emulate-target-make-array expr)
        (host-constant-to-core (eval expr)))))

;;; Return a handle on an interned symbol. If necessary allocate the
;;; symbol and record its home package.
(defun cold-intern (symbol
                    &key (access nil)
                         (gspace *dynamic*)
                    &aux (package (symbol-package-for-target-symbol symbol)))
  (aver (package-ok-for-target-symbol-p package))

  ;; Anything on the cross-compilation host which refers to the target
  ;; machinery through the host SB-XC package should be translated to
  ;; something on the target which refers to the same machinery
  ;; through the target COMMON-LISP package.
  (let ((p (find-package "SB-XC")))
    (when (eq package p)
      (setf package *cl-package*))
    (when (eq (symbol-package symbol) p)
      (setf symbol (intern (symbol-name symbol) *cl-package*))))

  (or (get symbol 'cold-intern-info)
      (let ((pkg-info (gethash (package-name package) *cold-package-symbols*))
            (handle (allocate-symbol (symbol-name symbol) :gspace gspace)))
        ;; maintain reverse map from target descriptor to host symbol
        (setf (gethash (descriptor-bits handle) *cold-symbols*) symbol)
        (unless pkg-info
          (error "No target package descriptor for ~S" package))
        (record-accessibility
         (or access (nth-value 1 (find-symbol (symbol-name symbol) package)))
         handle pkg-info symbol package t)
        #!+sb-thread
        (assign-tls-index symbol handle)
        (acond ((eq package *keyword-package*)
                (setq access :external)
                (cold-set handle handle))
               ((assoc symbol sb-cold:*symbol-values-for-genesis*)
                (cold-set handle (destructuring-bind (expr . package) (cdr it)
                                   (emulate-target-eval expr package)))))
        (setf (get symbol 'cold-intern-info) handle))))

(defun record-accessibility (accessibility symbol-descriptor target-pkg-info
                             host-symbol host-package &optional set-home-p)
  (when set-home-p
    (write-wordindexed symbol-descriptor sb!vm:symbol-package-slot
                       (car target-pkg-info)))
  (let ((access-lists (cdr target-pkg-info)))
    (case accessibility
      (:external (push symbol-descriptor (car access-lists)))
      (:internal (push symbol-descriptor (cdr access-lists)))
      (t (error "~S inaccessible in package ~S" host-symbol host-package)))))

;;; Construct and return a value for use as *NIL-DESCRIPTOR*.
;;; It might be nice to put NIL on a readonly page by itself to prevent unsafe
;;; code from destroying the world with (RPLACx nil 'kablooey)
(defun make-nil-descriptor (target-cl-pkg-info)
  (let* ((des (allocate-header+object *static* sb!vm:symbol-size 0))
         (result (make-descriptor (+ (descriptor-bits des)
                                     (* 2 sb!vm:n-word-bytes)
                                     (- sb!vm:list-pointer-lowtag
                                        sb!vm:other-pointer-lowtag)))))
    (write-wordindexed des
                       1
                       (make-other-immediate-descriptor
                        0
                        sb!vm:symbol-header-widetag))
    (write-wordindexed des
                       (+ 1 sb!vm:symbol-value-slot)
                       result)
    (write-wordindexed des
                       (+ 2 sb!vm:symbol-value-slot) ; = 1 + symbol-hash-slot
                       result)
    (write-wordindexed des
                       (+ 1 sb!vm:symbol-info-slot)
                       (cold-cons result result)) ; NIL's info is (nil . nil)
    (write-wordindexed des
                       (+ 1 sb!vm:symbol-name-slot)
                       ;; NIL's name is in dynamic space because any extra
                       ;; bytes allocated in static space would need to
                       ;; be accounted for by STATIC-SYMBOL-OFFSET.
                       (base-string-to-core "NIL" *dynamic*))
    ;; RECORD-ACCESSIBILITY can't assign to the package slot
    ;; due to NIL's base address and lowtag being nonstandard.
    (write-wordindexed des
                       (+ 1 sb!vm:symbol-package-slot)
                       (car target-cl-pkg-info))
    (record-accessibility :external result target-cl-pkg-info nil *cl-package*)
    (setf (gethash (descriptor-bits result) *cold-symbols*) nil
          (get nil 'cold-intern-info) result)))

;;; Since the initial symbols must be allocated before we can intern
;;; anything else, we intern those here. We also set the value of T.
(defun initialize-non-nil-symbols ()
  "Initialize the cold load symbol-hacking data structures."
  ;; Intern the others.
  (dolist (symbol sb!vm:*static-symbols*)
    (let* ((des (cold-intern symbol :gspace *static*))
           (offset-wanted (sb!vm:static-symbol-offset symbol))
           (offset-found (- (descriptor-bits des)
                            (descriptor-bits *nil-descriptor*))))
      (unless (= offset-wanted offset-found)
        ;; FIXME: should be fatal
        (warn "Offset from ~S to ~S is ~W, not ~W"
              symbol
              nil
              offset-found
              offset-wanted))))
  ;; Establish the value of T.
  (let ((t-symbol (cold-intern t :gspace *static*)))
    (cold-set t-symbol t-symbol))
  ;; Establish the value of *PSEUDO-ATOMIC-BITS* so that the
  ;; allocation sequences that expect it to be zero upon entrance
  ;; actually find it to be so.
  #!+(or x86-64 x86)
  (let ((p-a-a-symbol (cold-intern '*pseudo-atomic-bits*
                                   :gspace *static*)))
    (cold-set p-a-a-symbol (make-fixnum-descriptor 0))))

;;; Sort *COLD-LAYOUTS* to return them in a deterministic order.
(defun sort-cold-layouts ()
  (sort (%hash-table-alist *cold-layouts*) #'<
        :key (lambda (x) (descriptor-bits (cdr x)))))

;;; a helper function for FINISH-SYMBOLS: Return a cold alist suitable
;;; to be stored in *!INITIAL-LAYOUTS*.
(defun cold-list-all-layouts ()
  (let ((result *nil-descriptor*))
    (dolist (layout (sort-cold-layouts) result)
      (cold-push (cold-cons (cold-intern (car layout)) (cdr layout))
                 result))))

;;; Establish initial values for magic symbols.
;;;
(defun finish-symbols ()

  ;; Everything between this preserved-for-posterity comment down to
  ;; the assignment of *CURRENT-CATCH-BLOCK* could be entirely deleted,
  ;; including the list of *C-CALLABLE-STATIC-SYMBOLS* itself,
  ;; if it is GC-safe for the C runtime to have its own implementation
  ;; of the INFO-VECTOR-FDEFN function in a multi-threaded build.
  ;;
  ;;   "I think the point of setting these functions into SYMBOL-VALUEs
  ;;    here, instead of using SYMBOL-FUNCTION, is that in CMU CL
  ;;    SYMBOL-FUNCTION reduces to FDEFINITION, which is a pretty
  ;;    hairy operation (involving globaldb.lisp etc.) which we don't
  ;;    want to invoke early in cold init. -- WHN 2001-12-05"
  ;;
  ;; So... that's no longer true. We _do_ associate symbol -> fdefn in genesis.
  ;; Additionally, the INFO-VECTOR-FDEFN function is extremely simple and could
  ;; easily be implemented in C. However, info-vectors are inevitably
  ;; reallocated when new info is attached to a symbol, so the vectors can't be
  ;; in static space; they'd gradually become permanent garbage if they did.
  ;; That's the real reason for preserving the approach of storing an #<fdefn>
  ;; in a symbol's value cell - that location is static, the symbol-info is not.

  ;; FIXME: So OK, that's a reasonable reason to do something weird like
  ;; this, but this is still a weird thing to do, and we should change
  ;; the names to highlight that something weird is going on. Perhaps
  ;; *MAYBE-GC-FUN*, *INTERNAL-ERROR-FUN*, *HANDLE-BREAKPOINT-FUN*,
  ;; and *HANDLE-FUN-END-BREAKPOINT-FUN*...
  (dolist (symbol sb!vm::*c-callable-static-symbols*)
    (cold-set symbol (cold-fdefinition-object (cold-intern symbol))))

  (cold-set 'sb!vm::*current-catch-block*          (make-fixnum-descriptor 0))
  (cold-set 'sb!vm::*current-unwind-protect-block* (make-fixnum-descriptor 0))

  (cold-set '*free-interrupt-context-index* (make-fixnum-descriptor 0))

  (cold-set '*!initial-layouts* (cold-list-all-layouts))

  #!+sb-thread
  (cold-set 'sb!vm::*free-tls-index*
            (make-descriptor (ash *genesis-tls-counter* sb!vm:word-shift)))

  (dolist (symbol sb!impl::*cache-vector-symbols*)
    (cold-set symbol *nil-descriptor*))

  ;; Symbols for which no call to COLD-INTERN would occur - due to not being
  ;; referenced until warm init - must be artificially cold-interned.
  ;; Inasmuch as the "offending" things are compiled by ordinary target code
  ;; and not cold-init, I think we should use an ordinary DEFPACKAGE for
  ;; the added-on bits. What I've done is somewhat of a fragile kludge.
  (let (syms)
    (with-package-iterator (iter '("SB!PCL" "SB!MOP" "SB!GRAY" "SB!SEQUENCE"
                                   "SB!PROFILE" "SB!EXT" "SB!VM"
                                   "SB!C" "SB!FASL" "SB!DEBUG")
                                 :external)
      (loop
         (multiple-value-bind (foundp sym accessibility package) (iter)
           (declare (ignore accessibility))
           (cond ((not foundp) (return))
                 ((eq (symbol-package sym) package) (push sym syms))))))
    (setf syms (stable-sort syms #'string<))
    (dolist (sym syms)
      (cold-intern sym)))

  (let ((cold-pkg-inits *nil-descriptor*)
        cold-package-symbols-list)
    (maphash (lambda (name info)
               (push (cons name info) cold-package-symbols-list))
             *cold-package-symbols*)
    (setf cold-package-symbols-list
          (sort cold-package-symbols-list #'string< :key #'car))
    (dolist (pkgcons cold-package-symbols-list)
      (destructuring-bind (pkg-name . pkg-info) pkgcons
        (let ((shadow
               ;; Record shadowing symbols (except from SB-XC) in SB! packages.
               (when (eql (mismatch pkg-name "SB!") 3)
                 ;; Be insensitive to the host's ordering.
                 (sort (remove (find-package "SB-XC")
                               (package-shadowing-symbols (find-package pkg-name))
                               :key #'symbol-package) #'string<))))
          (write-slots (car (gethash pkg-name *cold-package-symbols*)) ; package
                       (find-layout 'package)
                       :%shadowing-symbols
                       (apply 'cold-list (mapcar 'cold-intern shadow))))
        (unless (member pkg-name '("COMMON-LISP" "KEYWORD") :test 'string=)
          (let ((host-pkg (find-package pkg-name))
                (sb-xc-pkg (find-package "SB-XC"))
                syms)
            ;; Now for each symbol directly present in this host-pkg,
            ;; i.e. accessible but not :INHERITED, figure out if the symbol
            ;; came from a different package, and if so, make a note of it.
            (with-package-iterator (iter host-pkg :internal :external)
              (loop (multiple-value-bind (foundp sym accessibility) (iter)
                      (unless foundp (return))
                      (unless (or (eq (symbol-package sym) host-pkg)
                                  (eq (symbol-package sym) sb-xc-pkg))
                        (push (cons sym accessibility) syms)))))
            (dolist (symcons (sort syms #'string< :key #'car))
              (destructuring-bind (sym . accessibility) symcons
                (record-accessibility accessibility (cold-intern sym)
                                      pkg-info sym host-pkg)))))
        (cold-push (cold-cons (car pkg-info)
                              (cold-cons (vector-in-core (cadr pkg-info))
                                         (vector-in-core (cddr pkg-info))))
                   cold-pkg-inits)))
    (cold-set 'sb!impl::*!initial-symbols* cold-pkg-inits))

  (dump-symbol-info-vectors
   (attach-fdefinitions-to-symbols
    (attach-classoid-cells-to-symbols (make-hash-table :test #'eq))))

  (cold-set '*!reversed-cold-toplevels* *current-reversed-cold-toplevels*)
  (cold-set '*!initial-debug-sources* *current-debug-sources*)

  #!+(or x86 x86-64)
  (progn
    (cold-set 'sb!vm::*fp-constant-0d0* (number-to-core 0d0))
    (cold-set 'sb!vm::*fp-constant-1d0* (number-to-core 1d0))
    (cold-set 'sb!vm::*fp-constant-0f0* (number-to-core 0f0))
    (cold-set 'sb!vm::*fp-constant-1f0* (number-to-core 1f0))))

;;;; functions and fdefinition objects

;;; a hash table mapping from fdefinition names to descriptors of cold
;;; objects
;;;
;;; Note: Since fdefinition names can be lists like '(SETF FOO), and
;;; we want to have only one entry per name, this must be an 'EQUAL
;;; hash table, not the default 'EQL.
(defvar *cold-fdefn-objects*)

(defvar *cold-fdefn-gspace* nil)

;;; Given a cold representation of a symbol, return a warm
;;; representation.
(defun warm-symbol (des)
  ;; Note that COLD-INTERN is responsible for keeping the
  ;; *COLD-SYMBOLS* table up to date, so if DES happens to refer to an
  ;; uninterned symbol, the code below will fail. But as long as we
  ;; don't need to look up uninterned symbols during bootstrapping,
  ;; that's OK..
  (multiple-value-bind (symbol found-p)
      (gethash (descriptor-bits des) *cold-symbols*)
    (declare (type symbol symbol))
    (unless found-p
      (error "no warm symbol"))
    symbol))

;;; like CL:CAR, CL:CDR, and CL:NULL but for cold values
(defun cold-car (des)
  (aver (= (descriptor-lowtag des) sb!vm:list-pointer-lowtag))
  (read-wordindexed des sb!vm:cons-car-slot))
(defun cold-cdr (des)
  (aver (= (descriptor-lowtag des) sb!vm:list-pointer-lowtag))
  (read-wordindexed des sb!vm:cons-cdr-slot))
(defun cold-rplacd (des newval)
  (aver (= (descriptor-lowtag des) sb!vm:list-pointer-lowtag))
  (write-wordindexed des sb!vm:cons-cdr-slot newval)
  des)
(defun cold-null (des)
  (= (descriptor-bits des)
     (descriptor-bits *nil-descriptor*)))

;;; Given a cold representation of a function name, return a warm
;;; representation.
(declaim (ftype (function ((or symbol descriptor)) (or symbol list)) warm-fun-name))
(defun warm-fun-name (des)
  (let ((result
         (if (symbolp des)
             ;; This parallels the logic at the start of COLD-INTERN
             ;; which re-homes symbols in SB-XC to COMMON-LISP.
             (if (eq (symbol-package des) (find-package "SB-XC"))
                 (intern (symbol-name des) *cl-package*)
                 des)
             (ecase (descriptor-lowtag des)
                    (#.sb!vm:list-pointer-lowtag
                     (aver (not (cold-null des))) ; function named NIL? please no..
                     ;; Do cold (DESTRUCTURING-BIND (COLD-CAR COLD-CADR) DES ..).
                     (let* ((car-des (cold-car des))
                            (cdr-des (cold-cdr des))
                            (cadr-des (cold-car cdr-des))
                            (cddr-des (cold-cdr cdr-des)))
                       (aver (cold-null cddr-des))
                       (list (warm-symbol car-des)
                             (warm-symbol cadr-des))))
                    (#.sb!vm:other-pointer-lowtag
                     (warm-symbol des))))))
    (legal-fun-name-or-type-error result)
    result))

(defun cold-fdefinition-object (cold-name &optional leave-fn-raw)
  (declare (type (or symbol descriptor) cold-name))
  (/noshow0 "/cold-fdefinition-object")
  (let ((warm-name (warm-fun-name cold-name)))
    (or (gethash warm-name *cold-fdefn-objects*)
        (let ((fdefn (allocate-header+object (or *cold-fdefn-gspace* *dynamic*)
                                             (1- sb!vm:fdefn-size)
                                             sb!vm:fdefn-widetag)))
          (setf (gethash warm-name *cold-fdefn-objects*) fdefn)
          (write-wordindexed fdefn sb!vm:fdefn-name-slot cold-name)
          (unless leave-fn-raw
            (write-wordindexed fdefn sb!vm:fdefn-fun-slot *nil-descriptor*)
            (write-wordindexed fdefn
                               sb!vm:fdefn-raw-addr-slot
                               (make-random-descriptor
                                (cold-foreign-symbol-address "undefined_tramp"))))
          fdefn))))

;;; Handle the at-cold-init-time, fset-for-static-linkage operation.
;;; "static" is sort of a misnomer. It's just ordinary fdefinition linkage.
(defun static-fset (cold-name defn)
  (declare (type (or symbol descriptor) cold-name))
  (let ((fdefn (cold-fdefinition-object cold-name t))
        (type (logand (descriptor-bits (read-memory defn)) sb!vm:widetag-mask)))
    (write-wordindexed fdefn sb!vm:fdefn-fun-slot defn)
    (write-wordindexed fdefn
                       sb!vm:fdefn-raw-addr-slot
                       (ecase type
                         (#.sb!vm:simple-fun-header-widetag
                          (/noshow0 "static-fset (simple-fun)")
                          #!+(or sparc arm)
                          defn
                          #!-(or sparc arm)
                          (make-random-descriptor
                           (+ (logandc2 (descriptor-bits defn)
                                        sb!vm:lowtag-mask)
                              (ash sb!vm:simple-fun-code-offset
                                   sb!vm:word-shift))))
                         (#.sb!vm:closure-header-widetag
                          ;; There's no way to create a closure.
                          (bug "FSET got closure-header-widetag")
                          (/show0 "/static-fset (closure)")
                          (make-random-descriptor
                           (cold-foreign-symbol-address "closure_tramp")))))
    fdefn))

;;; the names of things which have had COLD-FSET used on them already
;;; (used to make sure that we don't try to statically link a name to
;;; more than one definition)
(defparameter *cold-fset-warm-names*
  (make-hash-table :test 'equal)) ; names can be conses, e.g. (SETF CAR)

(defun cold-fset (name compiled-lambda source-loc &optional inline-expansion)
  ;; SOURCE-LOC can be ignored, because functions intrinsically store
  ;; their location as part of the code component.
  ;; The argument is supplied here only to provide context for
  ;; a redefinition warning, which can't happen in cold load.
  (declare (ignore source-loc))
  (multiple-value-bind (cold-name warm-name)
      ;; (SETF f) was descriptorized when dumped, symbols were not,
      ;; Figure out what kind of name we're looking at.
      (if (symbolp name)
          (values (cold-intern name) name)
          (values name (warm-fun-name name)))
    (when (gethash warm-name *cold-fset-warm-names*)
      (error "duplicate COLD-FSET for ~S" warm-name))
    (setf (gethash warm-name *cold-fset-warm-names*) t)
    (target-push (cold-cons cold-name inline-expansion)
                 '*!reversed-cold-defuns*)
    (static-fset cold-name compiled-lambda)))

(defun initialize-static-fns ()
  (let ((*cold-fdefn-gspace* *static*))
    (dolist (sym sb!vm:*static-funs*)
      (let* ((fdefn (cold-fdefinition-object (cold-intern sym)))
             (offset (- (+ (- (descriptor-bits fdefn)
                              sb!vm:other-pointer-lowtag)
                           (* sb!vm:fdefn-raw-addr-slot sb!vm:n-word-bytes))
                        (descriptor-bits *nil-descriptor*)))
             (desired (sb!vm:static-fun-offset sym)))
        (unless (= offset desired)
          ;; FIXME: should be fatal
          (error "Offset from FDEFN ~S to ~S is ~W, not ~W."
                 sym nil offset desired))))))

(defun attach-classoid-cells-to-symbols (hashtable)
  (let ((num (sb!c::meta-info-number (sb!c::meta-info :type :classoid-cell)))
        (layout (gethash 'sb!kernel::classoid-cell *cold-layouts*)))
    (when (plusp (hash-table-count *classoid-cells*))
      (aver layout))
    ;; Iteration order is immaterial. The symbols will get sorted later.
    (maphash (lambda (symbol cold-classoid-cell)
               ;; Some classoid-cells are dumped before the cold layout
               ;; of classoid-cell has been made, so fix those cases now.
               ;; Obviously it would be better if, in general, ALLOCATE-STRUCT
               ;; knew when something later must backpatch a cold layout
               ;; so that it could make a note to itself to do those ASAP
               ;; after the cold layout became known.
               (when (cold-null (cold-layout-of cold-classoid-cell))
                 (patch-instance-layout cold-classoid-cell layout))
               (setf (gethash symbol hashtable)
                     (packed-info-insert
                      (gethash symbol hashtable +nil-packed-infos+)
                      sb!c::+no-auxilliary-key+ num cold-classoid-cell)))
             *classoid-cells*))
  hashtable)

;; Create pointer from SYMBOL and/or (SETF SYMBOL) to respective fdefinition
;;
(defun attach-fdefinitions-to-symbols (hashtable)
    ;; Collect fdefinitions that go with one symbol, e.g. CAR and (SETF CAR),
    ;; using the host's code for manipulating a packed info-vector.
    (maphash (lambda (warm-name cold-fdefn)
               (with-globaldb-name (key1 key2) warm-name
                 :hairy (error "Hairy fdefn name in genesis: ~S" warm-name)
                 :simple
                 (setf (gethash key1 hashtable)
                       (packed-info-insert
                        (gethash key1 hashtable +nil-packed-infos+)
                        key2 +fdefn-info-num+ cold-fdefn))))
              *cold-fdefn-objects*)
    hashtable)

(defun dump-symbol-info-vectors (hashtable)
    ;; Emit in the same order symbols reside in core to avoid
    ;; sensitivity to the iteration order of host's maphash.
    (loop for (warm-sym . info)
          in (sort (%hash-table-alist hashtable) #'<
                   :key (lambda (x) (descriptor-bits (cold-intern (car x)))))
          do (write-wordindexed
              (cold-intern warm-sym) sb!vm:symbol-info-slot
              ;; Each vector will have one fixnum, possibly the symbol SETF,
              ;; and one or two #<fdefn> objects in it, and/or a classoid-cell.
              (vector-in-core
                     (map 'list (lambda (elt)
                                  (etypecase elt
                                    (symbol (cold-intern elt))
                                    (fixnum (make-fixnum-descriptor elt))
                                    (descriptor elt)))
                          info)))))


;;;; fixups and related stuff

;;; an EQUAL hash table
(defvar *cold-foreign-symbol-table*)
(declaim (type hash-table *cold-foreign-symbol-table*))

;; Read the sbcl.nm file to find the addresses for foreign-symbols in
;; the C runtime.
(defun load-cold-foreign-symbol-table (filename)
  (/show "load-cold-foreign-symbol-table" filename)
  (with-open-file (file filename)
    (loop for line = (read-line file nil nil)
          while line do
          ;; UNIX symbol tables might have tabs in them, and tabs are
          ;; not in Common Lisp STANDARD-CHAR, so there seems to be no
          ;; nice portable way to deal with them within Lisp, alas.
          ;; Fortunately, it's easy to use UNIX command line tools like
          ;; sed to remove the problem, so it's not too painful for us
          ;; to push responsibility for converting tabs to spaces out to
          ;; the caller.
          ;;
          ;; Other non-STANDARD-CHARs are problematic for the same reason.
          ;; Make sure that there aren't any..
          (let ((ch (find-if (lambda (char)
                               (not (typep char 'standard-char)))
                             line)))
            (when ch
              (error "non-STANDARD-CHAR ~S found in foreign symbol table:~%~S"
                     ch
                     line)))
          (setf line (string-trim '(#\space) line))
          (let ((p1 (position #\space line :from-end nil))
                (p2 (position #\space line :from-end t)))
            (if (not (and p1 p2 (< p1 p2)))
                ;; KLUDGE: It's too messy to try to understand all
                ;; possible output from nm, so we just punt the lines we
                ;; don't recognize. We realize that there's some chance
                ;; that might get us in trouble someday, so we warn
                ;; about it.
                (warn "ignoring unrecognized line ~S in ~A" line filename)
                (multiple-value-bind (value name)
                    (if (string= "0x" line :end2 2)
                        (values (parse-integer line :start 2 :end p1 :radix 16)
                                (subseq line (1+ p2)))
                        (values (parse-integer line :end p1 :radix 16)
                                (subseq line (1+ p2))))
                  ;; KLUDGE CLH 2010-05-31: on darwin, nm gives us
                  ;; _function but dlsym expects us to look up
                  ;; function, without the leading _ . Therefore, we
                  ;; strip it off here.
                  #!+darwin
                  (when (equal (char name 0) #\_)
                    (setf name (subseq name 1)))
                  (multiple-value-bind (old-value found)
                      (gethash name *cold-foreign-symbol-table*)
                    (when (and found
                               (not (= old-value value)))
                      (warn "redefining ~S from #X~X to #X~X"
                            name old-value value)))
                  (/show "adding to *cold-foreign-symbol-table*:" name value)
                  (setf (gethash name *cold-foreign-symbol-table*) value)
                  #!+win32
                  (let ((at-position (position #\@ name)))
                    (when at-position
                      (let ((name (subseq name 0 at-position)))
                        (multiple-value-bind (old-value found)
                            (gethash name *cold-foreign-symbol-table*)
                          (when (and found
                                     (not (= old-value value)))
                            (warn "redefining ~S from #X~X to #X~X"
                                  name old-value value)))
                        (setf (gethash name *cold-foreign-symbol-table*)
                              value)))))))))
  (values))     ;; PROGN

(defun cold-foreign-symbol-address (name)
  (or (find-foreign-symbol-in-table name *cold-foreign-symbol-table*)
      *foreign-symbol-placeholder-value*
      (progn
        (format *error-output* "~&The foreign symbol table is:~%")
        (maphash (lambda (k v)
                   (format *error-output* "~&~S = #X~8X~%" k v))
                 *cold-foreign-symbol-table*)
        (error "The foreign symbol ~S is undefined." name))))

(defvar *cold-assembler-routines*)

(defvar *cold-assembler-fixups*)

(defun record-cold-assembler-routine (name address)
  (/xhow "in RECORD-COLD-ASSEMBLER-ROUTINE" name address)
  (push (cons name address)
        *cold-assembler-routines*))

(defun record-cold-assembler-fixup (routine
                                    code-object
                                    offset
                                    &optional
                                    (kind :both))
  (push (list routine code-object offset kind)
        *cold-assembler-fixups*))

(defun lookup-assembler-reference (symbol)
  (let ((value (cdr (assoc symbol *cold-assembler-routines*))))
    ;; FIXME: Should this be ERROR instead of WARN?
    (unless value
      (warn "Assembler routine ~S not defined." symbol))
    value))

;;; Unlike in the target, FOP-KNOWN-FUN sometimes has to backpatch.
(defvar *deferred-known-fun-refs*)

;;; The x86 port needs to store code fixups along with code objects if
;;; they are to be moved, so fixups for code objects in the dynamic
;;; heap need to be noted.
#!+x86
(defvar *load-time-code-fixups*)

#!+x86
(defun note-load-time-code-fixup (code-object offset)
  ;; If CODE-OBJECT might be moved
  (when (= (gspace-identifier (descriptor-intuit-gspace code-object))
           dynamic-core-space-id)
    (push offset (gethash (descriptor-bits code-object)
                          *load-time-code-fixups*
                          nil)))
  (values))

#!+x86
(defun output-load-time-code-fixups ()
  (let ((fixup-infos nil))
    (maphash
     (lambda (code-object-address fixup-offsets)
       (push (cons code-object-address fixup-offsets) fixup-infos))
     *load-time-code-fixups*)
    (setq fixup-infos (sort fixup-infos #'< :key #'car))
    (dolist (fixup-info fixup-infos)
      (let ((code-object-address (car fixup-info))
            (fixup-offsets (cdr fixup-info)))
        (let ((fixup-vector
               (allocate-vector-object
                *dynamic* sb!vm:n-word-bits (length fixup-offsets)
                sb!vm:simple-array-unsigned-byte-32-widetag)))
          (do ((index sb!vm:vector-data-offset (1+ index))
               (fixups fixup-offsets (cdr fixups)))
              ((null fixups))
            (write-wordindexed fixup-vector index
                               (make-random-descriptor (car fixups))))
          ;; KLUDGE: The fixup vector is stored as the first constant,
          ;; not as a separately-named slot.
          (write-wordindexed (make-random-descriptor code-object-address)
                             sb!vm:code-constants-offset
                             fixup-vector))))))

;;; Given a pointer to a code object and an offset relative to the
;;; tail of the code object's header, return an offset relative to the
;;; (beginning of the) code object.
;;;
;;; FIXME: It might be clearer to reexpress
;;;    (LET ((X (CALC-OFFSET CODE-OBJECT OFFSET0))) ..)
;;; as
;;;    (LET ((X (+ OFFSET0 (CODE-OBJECT-HEADER-N-BYTES CODE-OBJECT)))) ..).
(declaim (ftype (function (descriptor sb!vm:word)) calc-offset))
(defun calc-offset (code-object offset-from-tail-of-header)
  (let* ((header (read-memory code-object))
         (header-n-words (ash (descriptor-bits header)
                              (- sb!vm:n-widetag-bits)))
         (header-n-bytes (ash header-n-words sb!vm:word-shift))
         (result (+ offset-from-tail-of-header header-n-bytes)))
    result))

(declaim (ftype (function (descriptor sb!vm:word sb!vm:word keyword))
                do-cold-fixup))
(defun do-cold-fixup (code-object after-header value kind)
  (let* ((offset-within-code-object (calc-offset code-object after-header))
         (gspace-bytes (descriptor-bytes code-object))
         (gspace-byte-offset (+ (descriptor-byte-offset code-object)
                                offset-within-code-object))
         (gspace-byte-address (gspace-byte-address
                               (descriptor-gspace code-object))))
    ;; There's just a ton of code here that gets deleted,
    ;; inhibiting the view of the the forest through the trees.
    ;; Use of #+sbcl would say "probable bug in read-time conditional"
    #+#.(cl:if (cl:member :sbcl cl:*features*) '(and) '(or))
    (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
    (ecase +backend-fasl-file-implementation+
      ;; See CMU CL source for other formerly-supported architectures
      ;; (and note that you have to rewrite them to use BVREF-X
      ;; instead of SAP-REF).
      (:alpha
         (ecase kind
         (:jmp-hint
          (assert (zerop (ldb (byte 2 0) value))))
         (:bits-63-48
          (let* ((value (if (logbitp 15 value) (+ value (ash 1 16)) value))
                 (value (if (logbitp 31 value) (+ value (ash 1 32)) value))
                 (value (if (logbitp 47 value) (+ value (ash 1 48)) value)))
            (setf (bvref-8 gspace-bytes gspace-byte-offset)
                  (ldb (byte 8 48) value)
                  (bvref-8 gspace-bytes (1+ gspace-byte-offset))
                  (ldb (byte 8 56) value))))
         (:bits-47-32
          (let* ((value (if (logbitp 15 value) (+ value (ash 1 16)) value))
                 (value (if (logbitp 31 value) (+ value (ash 1 32)) value)))
            (setf (bvref-8 gspace-bytes gspace-byte-offset)
                  (ldb (byte 8 32) value)
                  (bvref-8 gspace-bytes (1+ gspace-byte-offset))
                  (ldb (byte 8 40) value))))
         (:ldah
          (let ((value (if (logbitp 15 value) (+ value (ash 1 16)) value)))
            (setf (bvref-8 gspace-bytes gspace-byte-offset)
                  (ldb (byte 8 16) value)
                  (bvref-8 gspace-bytes (1+ gspace-byte-offset))
                  (ldb (byte 8 24) value))))
         (:lda
          (setf (bvref-8 gspace-bytes gspace-byte-offset)
                (ldb (byte 8 0) value)
                (bvref-8 gspace-bytes (1+ gspace-byte-offset))
                (ldb (byte 8 8) value)))))
      (:arm
       (ecase kind
         (:absolute
          (setf (bvref-32 gspace-bytes gspace-byte-offset) value))))
      (:arm64
       (ecase kind
         (:absolute
          (setf (bvref-64 gspace-bytes gspace-byte-offset) value))
         (:cond-branch
          (setf (ldb (byte 19 5)
                     (bvref-32 gspace-bytes gspace-byte-offset))
                (ash (- value (+ gspace-byte-address gspace-byte-offset))
                     -2)))
         (:uncond-branch
          (setf (ldb (byte 26 0)
                     (bvref-32 gspace-bytes gspace-byte-offset))
                (ash (- value (+ gspace-byte-address gspace-byte-offset))
                     -2)))))
      (:hppa
       (ecase kind
         (:absolute
          (setf (bvref-32 gspace-bytes gspace-byte-offset) value))
         (:load
          (setf (bvref-32 gspace-bytes gspace-byte-offset)
                (logior (mask-field (byte 18 14)
                                    (bvref-32 gspace-bytes gspace-byte-offset))
                        (if (< value 0)
                          (1+ (ash (ldb (byte 13 0) value) 1))
                          (ash (ldb (byte 13 0) value) 1)))))
         (:load11u
          (setf (bvref-32 gspace-bytes gspace-byte-offset)
                (logior (mask-field (byte 18 14)
                                    (bvref-32 gspace-bytes gspace-byte-offset))
                        (if (< value 0)
                          (1+ (ash (ldb (byte 10 0) value) 1))
                          (ash (ldb (byte 11 0) value) 1)))))
         (:load-short
          (let ((low-bits (ldb (byte 11 0) value)))
            (assert (<= 0 low-bits (1- (ash 1 4)))))
          (setf (bvref-32 gspace-bytes gspace-byte-offset)
                (logior (ash (dpb (ldb (byte 4 0) value)
                                  (byte 4 1)
                                  (ldb (byte 1 4) value)) 17)
                        (logand (bvref-32 gspace-bytes gspace-byte-offset)
                                #xffe0ffff))))
         (:hi
          (setf (bvref-32 gspace-bytes gspace-byte-offset)
                (logior (mask-field (byte 11 21)
                                    (bvref-32 gspace-bytes gspace-byte-offset))
                        (ash (ldb (byte 5 13) value) 16)
                        (ash (ldb (byte 2 18) value) 14)
                        (ash (ldb (byte 2 11) value) 12)
                        (ash (ldb (byte 11 20) value) 1)
                        (ldb (byte 1 31) value))))
         (:branch
          (let ((bits (ldb (byte 9 2) value)))
            (assert (zerop (ldb (byte 2 0) value)))
            (setf (bvref-32 gspace-bytes gspace-byte-offset)
                  (logior (ash bits 3)
                          (mask-field (byte 1 1) (bvref-32 gspace-bytes gspace-byte-offset))
                          (mask-field (byte 3 13) (bvref-32 gspace-bytes gspace-byte-offset))
                          (mask-field (byte 11 21) (bvref-32 gspace-bytes gspace-byte-offset))))))))
      (:mips
       (ecase kind
         (:jump
          (assert (zerop (ash value -28)))
          (setf (ldb (byte 26 0)
                     (bvref-32 gspace-bytes gspace-byte-offset))
                (ash value -2)))
         (:lui
          (setf (bvref-32 gspace-bytes gspace-byte-offset)
                (logior (mask-field (byte 16 16)
                                    (bvref-32 gspace-bytes gspace-byte-offset))
                        (ash (1+ (ldb (byte 17 15) value)) -1))))
         (:addi
          (setf (bvref-32 gspace-bytes gspace-byte-offset)
                (logior (mask-field (byte 16 16)
                                    (bvref-32 gspace-bytes gspace-byte-offset))
                        (ldb (byte 16 0) value))))))
       ;; FIXME: PowerPC Fixups are not fully implemented. The bit
       ;; here starts to set things up to work properly, but there
       ;; needs to be corresponding code in ppc-vm.lisp
       (:ppc
        (ecase kind
          (:ba
           (setf (bvref-32 gspace-bytes gspace-byte-offset)
                 (dpb (ash value -2) (byte 24 2)
                      (bvref-32 gspace-bytes gspace-byte-offset))))
          (:ha
           (let* ((un-fixed-up (bvref-16 gspace-bytes
                                         (+ gspace-byte-offset 2)))
                  (fixed-up (+ un-fixed-up value))
                  (h (ldb (byte 16 16) fixed-up))
                  (l (ldb (byte 16 0) fixed-up)))
             (setf (bvref-16 gspace-bytes (+ gspace-byte-offset 2))
                   (if (logbitp 15 l) (ldb (byte 16 0) (1+ h)) h))))
          (:l
           (let* ((un-fixed-up (bvref-16 gspace-bytes
                                         (+ gspace-byte-offset 2)))
                  (fixed-up (+ un-fixed-up value)))
             (setf (bvref-16 gspace-bytes (+ gspace-byte-offset 2))
                   (ldb (byte 16 0) fixed-up))))))
      (:sparc
       (ecase kind
         (:call
          (error "can't deal with call fixups yet"))
         (:sethi
          (setf (bvref-32 gspace-bytes gspace-byte-offset)
                (dpb (ldb (byte 22 10) value)
                     (byte 22 0)
                     (bvref-32 gspace-bytes gspace-byte-offset))))
         (:add
          (setf (bvref-32 gspace-bytes gspace-byte-offset)
                (dpb (ldb (byte 10 0) value)
                     (byte 10 0)
                     (bvref-32 gspace-bytes gspace-byte-offset))))))
      ((:x86 :x86-64)
       ;; XXX: Note that un-fixed-up is read via bvref-word, which is
       ;; 64 bits wide on x86-64, but the fixed-up value is written
       ;; via bvref-32.  This would make more sense if we supported
       ;; :absolute64 fixups, but apparently the cross-compiler
       ;; doesn't dump them.
       (let* ((un-fixed-up (bvref-word gspace-bytes
                                               gspace-byte-offset))
              (code-object-start-addr (logandc2 (descriptor-bits code-object)
                                                sb!vm:lowtag-mask)))
         (assert (= code-object-start-addr
                  (+ gspace-byte-address
                     (descriptor-byte-offset code-object))))
         (ecase kind
           (:absolute
            (let ((fixed-up (+ value un-fixed-up)))
              (setf (bvref-32 gspace-bytes gspace-byte-offset)
                    fixed-up)
              ;; comment from CMU CL sources:
              ;;
              ;; Note absolute fixups that point within the object.
              ;; KLUDGE: There seems to be an implicit assumption in
              ;; the old CMU CL code here, that if it doesn't point
              ;; before the object, it must point within the object
              ;; (not beyond it). It would be good to add an
              ;; explanation of why that's true, or an assertion that
              ;; it's really true, or both.
              ;;
              ;; One possible explanation is that all absolute fixups
              ;; point either within the code object, within the
              ;; runtime, within read-only or static-space, or within
              ;; the linkage-table space.  In all x86 configurations,
              ;; these areas are prior to the start of dynamic space,
              ;; where all the code-objects are loaded.
              #!+x86
              (unless (< fixed-up code-object-start-addr)
                (note-load-time-code-fixup code-object
                                           after-header))))
           (:relative ; (used for arguments to X86 relative CALL instruction)
            (let ((fixed-up (- (+ value un-fixed-up)
                               gspace-byte-address
                               gspace-byte-offset
                               4))) ; "length of CALL argument"
              (setf (bvref-32 gspace-bytes gspace-byte-offset)
                    fixed-up)
              ;; Note relative fixups that point outside the code
              ;; object, which is to say all relative fixups, since
              ;; relative addressing within a code object never needs
              ;; a fixup.
              #!+x86
              (note-load-time-code-fixup code-object
                                         after-header))))))))
  (values))

(defun resolve-assembler-fixups ()
  (dolist (fixup *cold-assembler-fixups*)
    (let* ((routine (car fixup))
           (value (lookup-assembler-reference routine)))
      (when value
        (do-cold-fixup (second fixup) (third fixup) value (fourth fixup))))))

#!+sb-dynamic-core
(progn
  (defparameter *dyncore-address* sb!vm::linkage-table-space-start)
  (defparameter *dyncore-linkage-keys* nil)
  (defparameter *dyncore-table* (make-hash-table :test 'equal))

  (defun dyncore-note-symbol (symbol-name datap)
    "Register a symbol and return its address in proto-linkage-table."
    (let ((key (cons symbol-name datap)))
      (symbol-macrolet ((entry (gethash key *dyncore-table*)))
        (or entry
            (setf entry
                  (prog1 *dyncore-address*
                    (push key *dyncore-linkage-keys*)
                    (incf *dyncore-address* sb!vm::linkage-table-entry-size))))))))

;;; *COLD-FOREIGN-SYMBOL-TABLE* becomes *!INITIAL-FOREIGN-SYMBOLS* in
;;; the core. When the core is loaded, !LOADER-COLD-INIT uses this to
;;; create *STATIC-FOREIGN-SYMBOLS*, which the code in
;;; target-load.lisp refers to.
(defun foreign-symbols-to-core ()
  (let ((result *nil-descriptor*))
    #!-sb-dynamic-core
    (dolist (symbol (sort (%hash-table-alist *cold-foreign-symbol-table*)
                          #'string< :key #'car))
      (cold-push (cold-cons (base-string-to-core (car symbol))
                            (number-to-core (cdr symbol)))
                 result))
    (cold-set '*!initial-foreign-symbols* result)
    #!+sb-dynamic-core
    (let ((runtime-linking-list *nil-descriptor*))
      (dolist (symbol *dyncore-linkage-keys*)
        (cold-push (cold-cons (base-string-to-core (car symbol))
                              (cdr symbol))
                   runtime-linking-list))
      (cold-set 'sb!vm::*required-runtime-c-symbols*
                runtime-linking-list)))
  (let ((result *nil-descriptor*))
    (dolist (rtn (sort (copy-list *cold-assembler-routines*) #'string< :key #'car))
      (cold-push (cold-cons (cold-intern (car rtn))
                            (number-to-core (cdr rtn)))
                 result))
    (cold-set '*!initial-assembler-routines* result)))


;;;; general machinery for cold-loading FASL files

(defun pop-fop-stack (stack)
  (let ((top (svref stack 0)))
    (declare (type index top))
    (when (eql 0 top)
      (error "FOP stack empty"))
    (setf (svref stack 0) (1- top))
    (svref stack top)))

;;; Cause a fop to have a special definition for cold load.
;;;
;;; This is similar to DEFINE-FOP, but unlike DEFINE-FOP, this version
;;; looks up the encoding for this name (created by a previous DEFINE-FOP)
;;; instead of creating a new encoding.
(defmacro define-cold-fop ((name &optional arglist) &rest forms)
  (let* ((code (get name 'opcode))
         (argp (plusp (sbit (car **fop-signatures**) (ash code -2))))
         (fname (symbolicate "COLD-" name)))
    (unless code
      (error "~S is not a defined FOP." name))
    (when (and argp (not (singleton-p arglist)))
      (error "~S must take one argument" name))
    `(progn
       (defun ,fname (.fasl-input. ,@arglist)
         (declare (ignorable .fasl-input.))
         (macrolet ((fasl-input () '(the fasl-input .fasl-input.))
                    (fasl-input-stream () '(%fasl-input-stream (fasl-input)))
                    (pop-stack ()
                      '(pop-fop-stack (%fasl-input-stack (fasl-input)))))
           ,@forms))
       ;; We simply overwrite elements of **FOP-FUNS** since the contents
       ;; of the host are never propagated directly into the target core.
       ,@(loop for i from code to (logior code (if argp 3 0))
               collect `(setf (svref **fop-funs** ,i) #',fname)))))

;;; Cause a fop to be undefined in cold load.
(defmacro not-cold-fop (name)
  `(define-cold-fop (,name)
     (error "The fop ~S is not supported in cold load." ',name)))

;;; COLD-LOAD loads stuff into the core image being built by calling
;;; LOAD-AS-FASL with the fop function table rebound to a table of cold
;;; loading functions.
(defun cold-load (filename)
  "Load the file named by FILENAME into the cold load image being built."
  (with-open-file (s filename :element-type '(unsigned-byte 8))
    (load-as-fasl s nil nil)))

;;;; miscellaneous cold fops

(define-cold-fop (fop-misc-trap) *unbound-marker*)

(define-cold-fop (fop-character (c))
  (make-character-descriptor c))

(define-cold-fop (fop-empty-list) nil)
(define-cold-fop (fop-truth) t)

(define-cold-fop (fop-struct (size)) ; n-words incl. layout, excluding header
  (let* ((layout (pop-stack))
         (result (allocate-struct *dynamic* layout size))
         (metadata
          (descriptor-fixnum
           (read-slot layout *host-layout-of-layout*
                      #!-interleaved-raw-slots :n-untagged-slots
                      #!+interleaved-raw-slots :untagged-bitmap)))
         #!-interleaved-raw-slots (ntagged (- size metadata))
         )
    ;; Raw slots can not possibly work because dump-struct uses
    ;; %RAW-INSTANCE-REF/WORD which does not exist in the cross-compiler.
    ;; Remove this assertion if that problem is somehow circumvented.
    (unless (= metadata 0)
      (error "Raw slots not working in genesis."))

    (do ((index 1 (1+ index)))
        ((eql index size))
      (declare (fixnum index))
      (write-wordindexed result
                         (+ index sb!vm:instance-slots-offset)
                         (if #!-interleaved-raw-slots (>= index ntagged)
                             #!+interleaved-raw-slots (logbitp index metadata)
                             (descriptor-word-sized-integer (pop-stack))
                             (pop-stack))))
    result))

(define-cold-fop (fop-layout)
  (let* ((metadata-des (pop-stack))
         (length-des (pop-stack))
         (depthoid-des (pop-stack))
         (cold-inherits (pop-stack))
         (name (pop-stack))
         (old-layout-descriptor (gethash name *cold-layouts*)))
    (declare (type descriptor length-des depthoid-des cold-inherits))
    (declare (type symbol name))
    ;; If a layout of this name has been defined already
    (if old-layout-descriptor
      ;; Enforce consistency between the previous definition and the
      ;; current definition, then return the previous definition.
      (flet ((get-slot (keyword)
               (read-slot old-layout-descriptor *host-layout-of-layout* keyword)))
        (let ((old-length (descriptor-fixnum (get-slot :length)))
              (old-depthoid (descriptor-fixnum (get-slot :depthoid)))
              (old-metadata
               (descriptor-fixnum
                (get-slot #!-interleaved-raw-slots :n-untagged-slots
                          #!+interleaved-raw-slots :untagged-bitmap)))
              (length (descriptor-fixnum length-des))
              (depthoid (descriptor-fixnum depthoid-des))
              (metadata (descriptor-fixnum metadata-des)))
          (unless (= length old-length)
            (error "cold loading a reference to class ~S when the compile~%~
                    time length was ~S and current length is ~S"
                   name
                   length
                   old-length))
          (unless (cold-vector-elements-eq cold-inherits (get-slot :inherits))
            (error "cold loading a reference to class ~S when the compile~%~
                    time inherits were ~S~%~
                    and current inherits are ~S"
                   name
                   (listify-cold-inherits cold-inherits)
                   (listify-cold-inherits (get-slot :inherits))))
          (unless (= depthoid old-depthoid)
            (error "cold loading a reference to class ~S when the compile~%~
                    time inheritance depthoid was ~S and current inheritance~%~
                    depthoid is ~S"
                   name
                   depthoid
                   old-depthoid))
          (unless (= metadata old-metadata)
            (error "cold loading a reference to class ~S when the compile~%~
                    time raw-slot-metadata was ~S and is currently ~S"
                   name
                   metadata
                   old-metadata)))
        old-layout-descriptor)
      ;; Make a new definition from scratch.
      (make-cold-layout name length-des cold-inherits depthoid-des
                        metadata-des))))

;;;; cold fops for loading symbols

;;; Load a symbol SIZE characters long from FASL-INPUT, and
;;; intern that symbol in PACKAGE.
(defun cold-load-symbol (size package fasl-input)
  (let ((string (make-string size)))
    (read-string-as-bytes (%fasl-input-stream fasl-input) string)
    (push-fop-table (intern string package) fasl-input)))

;; I don't feel like hacking up DEFINE-COLD-FOP any more than necessary,
;; so this code is handcrafted to accept two operands.
(flet ((fop-cold-symbol-in-package-save (fasl-input index pname-len)
         (cold-load-symbol pname-len (ref-fop-table fasl-input index)
                           fasl-input)))
  (dotimes (i 16) ; occupies 16 cells in the dispatch table
    (setf (svref **fop-funs** (+ (get 'fop-symbol-in-package-save 'opcode) i))
          #'fop-cold-symbol-in-package-save)))

(define-cold-fop (fop-lisp-symbol-save (namelen))
  (cold-load-symbol namelen *cl-package* (fasl-input)))

(define-cold-fop (fop-keyword-symbol-save (namelen))
  (cold-load-symbol namelen *keyword-package* (fasl-input)))

(define-cold-fop (fop-uninterned-symbol-save (namelen))
  (let ((name (make-string namelen)))
    (read-string-as-bytes (fasl-input-stream) name)
    (push-fop-table (get-uninterned-symbol name) (fasl-input))))

(define-cold-fop (fop-copy-symbol-save (index))
  (let* ((symbol (ref-fop-table (fasl-input) index))
         (name
          (if (symbolp symbol)
              (symbol-name symbol)
              (base-string-from-core
               (read-wordindexed symbol sb!vm:symbol-name-slot)))))
    ;; Genesis performs additional coalescing of uninterned symbols
    (push-fop-table (get-uninterned-symbol name) (fasl-input))))

;;;; cold fops for loading packages

(define-cold-fop (fop-named-package-save (namelen))
  (let ((name (make-string namelen)))
    (read-string-as-bytes (fasl-input-stream) name)
    (push-fop-table (find-package name) (fasl-input))))

;;;; cold fops for loading lists

;;; Make a list of the top LENGTH things on the fop stack. The last
;;; cdr of the list is set to LAST.
(defmacro cold-stack-list (length last)
  `(do* ((index ,length (1- index))
         (result ,last (cold-cons (pop-stack) result)))
        ((= index 0) result)
     (declare (fixnum index))))

(define-cold-fop (fop-list)
  (cold-stack-list (read-byte-arg (fasl-input-stream)) *nil-descriptor*))
(define-cold-fop (fop-list*)
  (cold-stack-list (read-byte-arg (fasl-input-stream)) (pop-stack)))
(define-cold-fop (fop-list-1)
  (cold-stack-list 1 *nil-descriptor*))
(define-cold-fop (fop-list-2)
  (cold-stack-list 2 *nil-descriptor*))
(define-cold-fop (fop-list-3)
  (cold-stack-list 3 *nil-descriptor*))
(define-cold-fop (fop-list-4)
  (cold-stack-list 4 *nil-descriptor*))
(define-cold-fop (fop-list-5)
  (cold-stack-list 5 *nil-descriptor*))
(define-cold-fop (fop-list-6)
  (cold-stack-list 6 *nil-descriptor*))
(define-cold-fop (fop-list-7)
  (cold-stack-list 7 *nil-descriptor*))
(define-cold-fop (fop-list-8)
  (cold-stack-list 8 *nil-descriptor*))
(define-cold-fop (fop-list*-1)
  (cold-stack-list 1 (pop-stack)))
(define-cold-fop (fop-list*-2)
  (cold-stack-list 2 (pop-stack)))
(define-cold-fop (fop-list*-3)
  (cold-stack-list 3 (pop-stack)))
(define-cold-fop (fop-list*-4)
  (cold-stack-list 4 (pop-stack)))
(define-cold-fop (fop-list*-5)
  (cold-stack-list 5 (pop-stack)))
(define-cold-fop (fop-list*-6)
  (cold-stack-list 6 (pop-stack)))
(define-cold-fop (fop-list*-7)
  (cold-stack-list 7 (pop-stack)))
(define-cold-fop (fop-list*-8)
  (cold-stack-list 8 (pop-stack)))

;;;; cold fops for loading vectors

(define-cold-fop (fop-base-string (len))
  (let ((string (make-string len)))
    (read-string-as-bytes (fasl-input-stream) string)
    (base-string-to-core string)))

#!+sb-unicode
(define-cold-fop (fop-character-string (len))
  (bug "CHARACTER-STRING[~D] dumped by cross-compiler." len))

(define-cold-fop (fop-vector (size))
  (let* ((result (allocate-vector-object *dynamic*
                                         sb!vm:n-word-bits
                                         size
                                         sb!vm:simple-vector-widetag)))
    (do ((index (1- size) (1- index)))
        ((minusp index))
      (declare (fixnum index))
      (write-wordindexed result
                         (+ index sb!vm:vector-data-offset)
                         (pop-stack)))
    result))

(define-cold-fop (fop-spec-vector)
  (let* ((len (read-word-arg (fasl-input-stream)))
         (type (read-byte-arg (fasl-input-stream)))
         (sizebits (aref **saetp-bits-per-length** type))
         (result (progn (aver (< sizebits 255))
                        (allocate-vector-object *dynamic* sizebits len type)))
         (start (+ (descriptor-byte-offset result)
                   (ash sb!vm:vector-data-offset sb!vm:word-shift)))
         (end (+ start
                 (ceiling (* len sizebits)
                          sb!vm:n-byte-bits))))
    (read-bigvec-as-sequence-or-die (descriptor-bytes result)
                                    (fasl-input-stream)
                                    :start start
                                    :end end)
    result))

(not-cold-fop fop-array)
#+nil
;; This code is unexercised. The only use of FOP-ARRAY is from target-dump.
;; It would be a shame to delete it though, as it might come in handy.
(define-cold-fop (fop-array)
  (let* ((rank (read-word-arg (fasl-input-stream)))
         (data-vector (pop-stack))
         (result (allocate-object *dynamic*
                                  (+ sb!vm:array-dimensions-offset rank)
                                  sb!vm:other-pointer-lowtag)))
    (write-header-word result rank sb!vm:simple-array-widetag)
    (write-wordindexed result sb!vm:array-fill-pointer-slot *nil-descriptor*)
    (write-wordindexed result sb!vm:array-data-slot data-vector)
    (write-wordindexed result sb!vm:array-displacement-slot *nil-descriptor*)
    (write-wordindexed result sb!vm:array-displaced-p-slot *nil-descriptor*)
    (write-wordindexed result sb!vm:array-displaced-from-slot *nil-descriptor*)
    (let ((total-elements 1))
      (dotimes (axis rank)
        (let ((dim (pop-stack)))
          (unless (is-fixnum-lowtag (descriptor-lowtag dim))
            (error "non-fixnum dimension? (~S)" dim))
          (setf total-elements (* total-elements (descriptor-fixnum dim)))
          (write-wordindexed result
                             (+ sb!vm:array-dimensions-offset axis)
                             dim)))
      (write-wordindexed result
                         sb!vm:array-elements-slot
                         (make-fixnum-descriptor total-elements)))
    result))


;;;; cold fops for loading numbers

(defmacro define-cold-number-fop (fop &optional arglist)
  ;; Invoke the ordinary warm version of this fop to cons the number.
  `(define-cold-fop (,fop ,arglist)
     (number-to-core (,fop (fasl-input) ,@arglist))))

(define-cold-number-fop fop-single-float)
(define-cold-number-fop fop-double-float)
(define-cold-number-fop fop-word-integer)
(define-cold-number-fop fop-byte-integer)
(define-cold-number-fop fop-complex-single-float)
(define-cold-number-fop fop-complex-double-float)
(define-cold-number-fop fop-integer (n-bytes))

(define-cold-fop (fop-ratio)
  (let ((den (pop-stack)))
    (number-pair-to-core (pop-stack) den sb!vm:ratio-widetag)))

(define-cold-fop (fop-complex)
  (let ((im (pop-stack)))
    (number-pair-to-core (pop-stack) im sb!vm:complex-widetag)))

;;;; cold fops for calling (or not calling)

(not-cold-fop fop-eval)
(not-cold-fop fop-eval-for-effect)

(defvar *load-time-value-counter*)

(flet ((pop-args (fasl-input)
         (let ((args)
               (stack (%fasl-input-stack fasl-input)))
           (dotimes (i (read-byte-arg (%fasl-input-stream fasl-input))
                       (values (pop-fop-stack stack) args))
             (push (pop-fop-stack stack) args))))
       (call (fun-name handler-name args)
         (acond ((get fun-name handler-name) (apply it args))
                (t (error "Can't ~S ~S in cold load" handler-name fun-name)))))

  (define-cold-fop (fop-funcall)
    (multiple-value-bind (fun args) (pop-args (fasl-input))
      (if args
          (case fun
           (fdefinition
            ;; Special form #'F fopcompiles into `(FDEFINITION ,f)
            (aver (and (singleton-p args) (symbolp (car args))))
            (target-symbol-function (car args)))
           (cons (cold-cons (first args) (second args)))
           (symbol-global-value (cold-symbol-value (first args)))
           (t (call fun :sb-cold-funcall-handler/for-value args)))
          (let ((counter *load-time-value-counter*))
            (cold-push (cold-list (cold-intern :load-time-value) fun
                                  (number-to-core counter))
                       *current-reversed-cold-toplevels*)
            (setf *load-time-value-counter* (1+ counter))
            (make-descriptor 0 :load-time-value counter)))))

  (define-cold-fop (fop-funcall-for-effect)
    (multiple-value-bind (fun args) (pop-args (fasl-input))
      (if (not args)
          (cold-push fun *current-reversed-cold-toplevels*)
          (case fun
            (sb!impl::%defun (apply #'cold-fset args))
            (sb!kernel::%defstruct
             (push args *known-structure-classoids*)
             (cold-push (apply #'cold-list (cold-intern 'defstruct) args)
                        *current-reversed-cold-toplevels*))
            (sb!impl::%defsetf
             (target-push (host-constant-to-core args nil)
                          '*!reversed-cold-setf-macros*))
            (set
             (aver (= (length args) 2))
             (cold-set (first args)
                       (let ((val (second args)))
                         (if (symbolp val) (cold-intern val) val))))
            (%svset (apply 'cold-svset args))
            (t (call fun :sb-cold-funcall-handler/for-effect args)))))))

(defun finalize-load-time-value-noise ()
  (cold-set '*!load-time-values*
            (allocate-vector-object *dynamic*
                                    sb!vm:n-word-bits
                                    *load-time-value-counter*
                                    sb!vm:simple-vector-widetag)))


;;;; cold fops for fixing up circularities

(define-cold-fop (fop-rplaca)
  (let ((obj (ref-fop-table (fasl-input) (read-word-arg (fasl-input-stream))))
        (idx (read-word-arg (fasl-input-stream))))
    (write-memory (cold-nthcdr idx obj) (pop-stack))))

(define-cold-fop (fop-rplacd)
  (let ((obj (ref-fop-table (fasl-input) (read-word-arg (fasl-input-stream))))
        (idx (read-word-arg (fasl-input-stream))))
    (write-wordindexed (cold-nthcdr idx obj) 1 (pop-stack))))

(define-cold-fop (fop-svset)
  (let ((obj (ref-fop-table (fasl-input) (read-word-arg (fasl-input-stream))))
        (idx (read-word-arg (fasl-input-stream))))
    (write-wordindexed obj
                   (+ idx
                      (ecase (descriptor-lowtag obj)
                        (#.sb!vm:instance-pointer-lowtag 1)
                        (#.sb!vm:other-pointer-lowtag 2)))
                   (pop-stack))))

(define-cold-fop (fop-structset)
  (let ((obj (ref-fop-table (fasl-input) (read-word-arg (fasl-input-stream))))
        (idx (read-word-arg (fasl-input-stream))))
    (write-wordindexed obj (+ idx sb!vm:instance-slots-offset) (pop-stack))))

(define-cold-fop (fop-nthcdr)
  (cold-nthcdr (read-word-arg (fasl-input-stream)) (pop-stack)))

(defun cold-nthcdr (index obj)
  (dotimes (i index)
    (setq obj (read-wordindexed obj sb!vm:cons-cdr-slot)))
  obj)

;;;; cold fops for loading code objects and functions

(define-cold-fop (fop-note-debug-source)
  (let ((debug-source (pop-stack)))
    (cold-push debug-source *current-debug-sources*)))

(define-cold-fop (fop-fdefn)
  (cold-fdefinition-object (pop-stack)))

(define-cold-fop (fop-known-fun)
  (let* ((name (pop-stack))
         (fun (cold-fdefn-fun (cold-fdefinition-object name))))
    (if (cold-null fun) `(:known-fun . ,name) fun)))

#!-(or x86 x86-64)
(define-cold-fop (fop-sanctify-for-execution)
  (pop-stack))

;;; Setting this variable shows what code looks like before any
;;; fixups (or function headers) are applied.
#!+sb-show (defvar *show-pre-fixup-code-p* nil)

(defun cold-load-code (fasl-input nconst code-size)
  (macrolet ((pop-stack () '(pop-fop-stack (%fasl-input-stack fasl-input))))
     (let* ((raw-header-n-words (+ sb!vm:code-constants-offset nconst))
            (header-n-words
             ;; Note: we round the number of constants up to ensure
             ;; that the code vector will be properly aligned.
             (round-up raw-header-n-words 2))
            (des (allocate-cold-descriptor *dynamic*
                                           (+ (ash header-n-words
                                                   sb!vm:word-shift)
                                              code-size)
                                           sb!vm:other-pointer-lowtag)))
       (write-header-word des header-n-words sb!vm:code-header-widetag)
       (write-wordindexed des
                          sb!vm:code-code-size-slot
                          (make-fixnum-descriptor code-size))
       (write-wordindexed des sb!vm:code-entry-points-slot *nil-descriptor*)
       (write-wordindexed des sb!vm:code-debug-info-slot (pop-stack))
       (when (oddp raw-header-n-words)
         (write-wordindexed des raw-header-n-words (make-descriptor 0)))
       (do ((index (1- raw-header-n-words) (1- index)))
           ((< index sb!vm:code-constants-offset))
         (let ((obj (pop-stack)))
           (if (and (consp obj) (eq (car obj) :known-fun))
               (push (list* (cdr obj) des index) *deferred-known-fun-refs*)
               (write-wordindexed des index obj))))
       (let* ((start (+ (descriptor-byte-offset des)
                        (ash header-n-words sb!vm:word-shift)))
              (end (+ start code-size)))
         (read-bigvec-as-sequence-or-die (descriptor-bytes des)
                                         (%fasl-input-stream fasl-input)
                                         :start start
                                         :end end)
         #!+sb-show
         (when *show-pre-fixup-code-p*
           (format *trace-output*
                   "~&/raw code from code-fop ~W ~W:~%"
                   nconst
                   code-size)
           (do ((i start (+ i sb!vm:n-word-bytes)))
               ((>= i end))
             (format *trace-output*
                     "/#X~8,'0x: #X~8,'0x~%"
                     (+ i (gspace-byte-address (descriptor-gspace des)))
                     (bvref-32 (descriptor-bytes des) i)))))
       des)))

(dotimes (i 16) ; occupies 16 cells in the dispatch table
  (setf (svref **fop-funs** (+ (get 'fop-code 'opcode) i))
        #'cold-load-code))

(defun resolve-deferred-known-funs ()
  (dolist (item *deferred-known-fun-refs*)
    (let ((fun (cold-fdefn-fun (cold-fdefinition-object (car item)))))
      (aver (not (cold-null fun)))
      (let ((place (cdr item)))
        (write-wordindexed (car place) (cdr place) fun)))))

(define-cold-fop (fop-alter-code (slot))
  (let ((value (pop-stack))
        (code (pop-stack)))
    (write-wordindexed code slot value)))

(defvar *simple-fun-metadata* (make-hash-table :test 'equalp))

;; Return an expression that can be used to coalesce type-specifiers
;; and lambda lists attached to simple-funs. It doesn't have to be
;; a "correct" host representation, just something that preserves EQUAL-ness.
(defun make-equal-comparable-thing (descriptor)
  (labels ((recurse (x)
            (cond ((cold-null x) (return-from recurse nil))
                  ((is-fixnum-lowtag (descriptor-lowtag x))
                   (return-from recurse (descriptor-fixnum x)))
                  #!+64-bit
                  ((is-other-immediate-lowtag (descriptor-lowtag x))
                   (let ((bits (descriptor-bits x)))
                     (when (= (logand bits sb!vm:widetag-mask)
                              sb!vm:single-float-widetag)
                       (return-from recurse `(:ffloat-bits ,bits))))))
            (ecase (descriptor-lowtag x)
              (#.sb!vm:list-pointer-lowtag
               (cons (recurse (cold-car x)) (recurse (cold-cdr x))))
              (#.sb!vm:other-pointer-lowtag
               (ecase (logand (descriptor-bits (read-memory x)) sb!vm:widetag-mask)
                   (#.sb!vm:symbol-header-widetag
                    (if (cold-null (read-wordindexed x sb!vm:symbol-package-slot))
                        (get-or-make-uninterned-symbol
                         (base-string-from-core
                          (read-wordindexed x sb!vm:symbol-name-slot)))
                        (warm-symbol x)))
                   #!-64-bit
                   (#.sb!vm:single-float-widetag
                    `(:ffloat-bits
                      ,(read-bits-wordindexed x sb!vm:single-float-value-slot)))
                   (#.sb!vm:double-float-widetag
                    `(:dfloat-bits
                      ,(read-bits-wordindexed x sb!vm:double-float-value-slot)
                      #!-64-bit
                      ,(read-bits-wordindexed
                        x (1+ sb!vm:double-float-value-slot))))
                   (#.sb!vm:bignum-widetag
                    (bignum-from-core x))
                   (#.sb!vm:simple-base-string-widetag
                    (base-string-from-core x))
                   ;; Why do function lambda lists have simple-vectors in them?
                   ;; Because we expose all &OPTIONAL and &KEY default forms.
                   ;; I think this is abstraction leakage, except possibly for
                   ;; advertised constant defaults of NIL and such.
                   ;; How one expresses a value as a sexpr should otherwise
                   ;; be of no concern to a user of the code.
                   (#.sb!vm:simple-vector-widetag
                    (vector-from-core x #'recurse))))))
           ;; Return a warm symbol whose name is similar to NAME, coaelescing
           ;; all occurrences of #:.WHOLE. across all files, e.g.
           (get-or-make-uninterned-symbol (name)
             (let ((key `(:uninterned-symbol ,name)))
               (or (gethash key *simple-fun-metadata*)
                   (let ((symbol (make-symbol name)))
                     (setf (gethash key *simple-fun-metadata*) symbol))))))
    (recurse descriptor)))

(define-cold-fop (fop-fun-entry)
  (let* ((info (pop-stack))
         (type (pop-stack))
         (arglist (pop-stack))
         (name (pop-stack))
         (code-object (pop-stack))
         (offset (calc-offset code-object (read-word-arg (fasl-input-stream))))
         (fn (descriptor-beyond code-object
                                offset
                                sb!vm:fun-pointer-lowtag))
         (next (read-wordindexed code-object sb!vm:code-entry-points-slot)))
    (unless (zerop (logand offset sb!vm:lowtag-mask))
      (error "unaligned function entry: ~S at #X~X" name offset))
    (write-wordindexed code-object sb!vm:code-entry-points-slot fn)
    (write-memory fn
                  (make-other-immediate-descriptor
                   (ash offset (- sb!vm:word-shift))
                   sb!vm:simple-fun-header-widetag))
    (write-wordindexed fn
                       sb!vm:simple-fun-self-slot
                       ;; KLUDGE: Wiring decisions like this in at
                       ;; this level ("if it's an x86") instead of a
                       ;; higher level of abstraction ("if it has such
                       ;; and such relocation peculiarities (which
                       ;; happen to be confined to the x86)") is bad.
                       ;; It would be nice if the code were instead
                       ;; conditional on some more descriptive
                       ;; feature, :STICKY-CODE or
                       ;; :LOAD-GC-INTERACTION or something.
                       ;;
                       ;; FIXME: The X86 definition of the function
                       ;; self slot breaks everything object.tex says
                       ;; about it. (As far as I can tell, the X86
                       ;; definition makes it a pointer to the actual
                       ;; code instead of a pointer back to the object
                       ;; itself.) Ask on the mailing list whether
                       ;; this is documented somewhere, and if not,
                       ;; try to reverse engineer some documentation.
                       #!-(or x86 x86-64)
                       ;; a pointer back to the function object, as
                       ;; described in CMU CL
                       ;; src/docs/internals/object.tex
                       fn
                       #!+(or x86 x86-64)
                       ;; KLUDGE: a pointer to the actual code of the
                       ;; object, as described nowhere that I can find
                       ;; -- WHN 19990907
                       (make-descriptor ; raw bits that look like fixnum
                        (+ (descriptor-bits fn)
                           (- (ash sb!vm:simple-fun-code-offset
                                   sb!vm:word-shift)
                              ;; FIXME: We should mask out the type
                              ;; bits, not assume we know what they
                              ;; are and subtract them out this way.
                              sb!vm:fun-pointer-lowtag))))
    (write-wordindexed fn sb!vm:simple-fun-next-slot next)
    (write-wordindexed fn sb!vm:simple-fun-name-slot name)
    (flet ((coalesce (sexpr) ; a warm symbol or a cold cons tree
             (if (symbolp sexpr) ; will be cold-interned automatically
                 sexpr
                 (let ((representation (make-equal-comparable-thing sexpr)))
                   (or (gethash representation *simple-fun-metadata*)
                       (setf (gethash representation *simple-fun-metadata*)
                             sexpr))))))
      (write-wordindexed fn sb!vm:simple-fun-arglist-slot (coalesce arglist))
      (write-wordindexed fn sb!vm:simple-fun-type-slot (coalesce type)))
    (write-wordindexed fn sb!vm::simple-fun-info-slot info)
    fn))

#!+sb-thread
(define-cold-fop (fop-symbol-tls-fixup)
  (let* ((symbol (pop-stack))
         (kind (pop-stack))
         (code-object (pop-stack)))
    (do-cold-fixup code-object
                   (read-word-arg (fasl-input-stream))
                   (ensure-symbol-tls-index symbol) kind)
    code-object))

(define-cold-fop (fop-foreign-fixup)
  (let* ((kind (pop-stack))
         (code-object (pop-stack))
         (len (read-byte-arg (fasl-input-stream)))
         (sym (make-string len)))
    (read-string-as-bytes (fasl-input-stream) sym)
    #!+sb-dynamic-core
    (let ((offset (read-word-arg (fasl-input-stream)))
          (value (dyncore-note-symbol sym nil)))
      (do-cold-fixup code-object offset value kind))
    #!- (and) (format t "Bad non-plt fixup: ~S~S~%" sym code-object)
    #!-sb-dynamic-core
    (let ((offset (read-word-arg (fasl-input-stream)))
          (value (cold-foreign-symbol-address sym)))
      (do-cold-fixup code-object offset value kind))
   code-object))

#!+linkage-table
(define-cold-fop (fop-foreign-dataref-fixup)
  (let* ((kind (pop-stack))
         (code-object (pop-stack))
         (len (read-byte-arg (fasl-input-stream)))
         (sym (make-string len)))
    #!-sb-dynamic-core (declare (ignore code-object))
    (read-string-as-bytes (fasl-input-stream) sym)
    #!+sb-dynamic-core
    (let ((offset (read-word-arg (fasl-input-stream)))
          (value (dyncore-note-symbol sym t)))
      (do-cold-fixup code-object offset value kind)
      code-object)
    #!-sb-dynamic-core
    (progn
      (maphash (lambda (k v)
                 (format *error-output* "~&~S = #X~8X~%" k v))
               *cold-foreign-symbol-table*)
      (error "shared foreign symbol in cold load: ~S (~S)" sym kind))))

(define-cold-fop (fop-assembler-code)
  (let* ((length (read-word-arg (fasl-input-stream)))
         (header-n-words
          ;; Note: we round the number of constants up to ensure that
          ;; the code vector will be properly aligned.
          (round-up sb!vm:code-constants-offset 2))
         (des (allocate-cold-descriptor *read-only*
                                        (+ (ash header-n-words
                                                sb!vm:word-shift)
                                           length)
                                        sb!vm:other-pointer-lowtag)))
    (write-header-word des header-n-words sb!vm:code-header-widetag)
    (write-wordindexed des
                       sb!vm:code-code-size-slot
                       (make-fixnum-descriptor length))
    (write-wordindexed des sb!vm:code-entry-points-slot *nil-descriptor*)
    (write-wordindexed des sb!vm:code-debug-info-slot *nil-descriptor*)

    (let* ((start (+ (descriptor-byte-offset des)
                     (ash header-n-words sb!vm:word-shift)))
           (end (+ start length)))
      (read-bigvec-as-sequence-or-die (descriptor-bytes des)
                                      (fasl-input-stream)
                                      :start start
                                      :end end))
    des))

(define-cold-fop (fop-assembler-routine)
  (let* ((routine (pop-stack))
         (des (pop-stack))
         (offset (calc-offset des (read-word-arg (fasl-input-stream)))))
    (record-cold-assembler-routine
     routine
     (+ (logandc2 (descriptor-bits des) sb!vm:lowtag-mask) offset))
    des))

(define-cold-fop (fop-assembler-fixup)
  (let* ((routine (pop-stack))
         (kind (pop-stack))
         (code-object (pop-stack))
         (offset (read-word-arg (fasl-input-stream))))
    (record-cold-assembler-fixup routine code-object offset kind)
    code-object))

(define-cold-fop (fop-code-object-fixup)
  (let* ((kind (pop-stack))
         (code-object (pop-stack))
         (offset (read-word-arg (fasl-input-stream)))
         (value (descriptor-bits code-object)))
    (do-cold-fixup code-object offset value kind)
    code-object))

;;;; sanity checking space layouts

(defun check-spaces ()
  ;;; Co-opt type machinery to check for intersections...
  (let (types)
    (flet ((check (start end space)
             (unless (< start end)
               (error "Bogus space: ~A" space))
             (let ((type (specifier-type `(integer ,start ,end))))
               (dolist (other types)
                 (unless (eq *empty-type* (type-intersection (cdr other) type))
                   (error "Space overlap: ~A with ~A" space (car other))))
               (push (cons space type) types))))
      (check sb!vm:read-only-space-start sb!vm:read-only-space-end :read-only)
      (check sb!vm:static-space-start sb!vm:static-space-end :static)
      #!+gencgc
      (check sb!vm:dynamic-space-start sb!vm:dynamic-space-end :dynamic)
      #!-gencgc
      (progn
        (check sb!vm:dynamic-0-space-start sb!vm:dynamic-0-space-end :dynamic-0)
        (check sb!vm:dynamic-1-space-start sb!vm:dynamic-1-space-end :dynamic-1))
      #!+linkage-table
      (check sb!vm:linkage-table-space-start sb!vm:linkage-table-space-end :linkage-table))))

;;;; emitting C header file

(defun tailwise-equal (string tail)
  (and (>= (length string) (length tail))
       (string= string tail :start1 (- (length string) (length tail)))))

(defun write-boilerplate ()
  (format t "/*~%")
  (dolist (line
           '("This is a machine-generated file. Please do not edit it by hand."
             "(As of sbcl-0.8.14, it came from WRITE-CONFIG-H in genesis.lisp.)"
             nil
             "This file contains low-level information about the"
             "internals of a particular version and configuration"
             "of SBCL. It is used by the C compiler to create a runtime"
             "support environment, an executable program in the host"
             "operating system's native format, which can then be used to"
             "load and run 'core' files, which are basically programs"
             "in SBCL's own format."))
    (format t " *~@[ ~A~]~%" line))
  (format t " */~%"))

(defun c-name (string &optional strip)
  (delete #\+
          (substitute-if #\_ (lambda (c) (member c '(#\- #\/ #\%)))
                         (remove-if (lambda (c) (position c strip))
                                    string))))

(defun c-symbol-name (symbol &optional strip)
  (c-name (symbol-name symbol) strip))

(defun write-makefile-features ()
  ;; propagating *SHEBANG-FEATURES* into the Makefiles
  (dolist (shebang-feature-name (sort (mapcar #'c-symbol-name
                                              sb-cold:*shebang-features*)
                                      #'string<))
    (format t "LISP_FEATURE_~A=1~%" shebang-feature-name)))

(defun write-config-h ()
  ;; propagating *SHEBANG-FEATURES* into C-level #define's
  (dolist (shebang-feature-name (sort (mapcar #'c-symbol-name
                                              sb-cold:*shebang-features*)
                                      #'string<))
    (format t "#define LISP_FEATURE_~A~%" shebang-feature-name))
  (terpri)
  ;; and miscellaneous constants
  (format t "#define SBCL_VERSION_STRING ~S~%"
            (sb!xc:lisp-implementation-version))
  (format t "#define CORE_MAGIC 0x~X~%" core-magic)
  (format t "#ifndef LANGUAGE_ASSEMBLY~2%")
  (format t "#define LISPOBJ(x) ((lispobj)x)~2%")
  (format t "#else /* LANGUAGE_ASSEMBLY */~2%")
  (format t "#define LISPOBJ(thing) thing~2%")
  (format t "#endif /* LANGUAGE_ASSEMBLY */~2%")
  (terpri))

(defun write-constants-h ()
  ;; writing entire families of named constants
  (let ((constants nil))
    (dolist (package-name '( ;; Even in CMU CL, constants from VM
                            ;; were automatically propagated
                            ;; into the runtime.
                            "SB!VM"
                            ;; In SBCL, we also propagate various
                            ;; magic numbers related to file format,
                            ;; which live here instead of SB!VM.
                            "SB!FASL"))
      (do-external-symbols (symbol (find-package package-name))
        (when (constantp symbol)
          (let ((name (symbol-name symbol)))
            (labels ( ;; shared machinery
                     (record (string priority suffix)
                       (push (list string
                                   priority
                                   (symbol-value symbol)
                                   suffix
                                   (documentation symbol 'variable))
                             constants))
                     ;; machinery for old-style CMU CL Lisp-to-C
                     ;; arbitrary renaming, being phased out in favor of
                     ;; the newer systematic RECORD-WITH-TRANSLATED-NAME
                     ;; renaming
                     (record-with-munged-name (prefix string priority)
                       (record (concatenate
                                'simple-string
                                prefix
                                (delete #\- (string-capitalize string)))
                               priority
                               ""))
                     (maybe-record-with-munged-name (tail prefix priority)
                       (when (tailwise-equal name tail)
                         (record-with-munged-name prefix
                                                  (subseq name 0
                                                          (- (length name)
                                                             (length tail)))
                                                  priority)))
                     ;; machinery for new-style SBCL Lisp-to-C naming
                     (record-with-translated-name (priority large)
                       (record (c-name name) priority
                               (if large
                                   #!+(and win32 x86-64) "LLU"
                                   #!-(and win32 x86-64) "LU"
                                   "")))
                     (maybe-record-with-translated-name (suffixes priority &key large)
                       (when (some (lambda (suffix)
                                     (tailwise-equal name suffix))
                                   suffixes)
                         (record-with-translated-name priority large))))
              (maybe-record-with-translated-name '("-LOWTAG") 0)
              (maybe-record-with-translated-name '("-WIDETAG" "-SHIFT") 1)
              (maybe-record-with-munged-name "-FLAG" "flag_" 2)
              (maybe-record-with-munged-name "-TRAP" "trap_" 3)
              (maybe-record-with-munged-name "-SUBTYPE" "subtype_" 4)
              (maybe-record-with-munged-name "-SC-NUMBER" "sc_" 5)
              (maybe-record-with-translated-name '("-SIZE" "-INTERRUPTS") 6)
              (maybe-record-with-translated-name '("-START" "-END" "-PAGE-BYTES"
                                                   "-CARD-BYTES" "-GRANULARITY")
                                                 7 :large t)
              (maybe-record-with-translated-name '("-CORE-ENTRY-TYPE-CODE") 8)
              (maybe-record-with-translated-name '("-CORE-SPACE-ID") 9)
              (maybe-record-with-translated-name '("-CORE-SPACE-ID-FLAG") 9)
              (maybe-record-with-translated-name '("-GENERATION+") 10))))))
    ;; KLUDGE: these constants are sort of important, but there's no
    ;; pleasing way to inform the code above about them.  So we fake
    ;; it for now.  nikodemus on #lisp (2004-08-09) suggested simply
    ;; exporting every numeric constant from SB!VM; that would work,
    ;; but the C runtime would have to be altered to use Lisp-like names
    ;; rather than the munged names currently exported.  --njf, 2004-08-09
    (dolist (c '(sb!vm:n-word-bits sb!vm:n-word-bytes
                 sb!vm:n-lowtag-bits sb!vm:lowtag-mask
                 sb!vm:n-widetag-bits sb!vm:widetag-mask
                 sb!vm:n-fixnum-tag-bits sb!vm:fixnum-tag-mask))
      (push (list (c-symbol-name c)
                  -1                    ; invent a new priority
                  (symbol-value c)
                  ""
                  nil)
            constants))
    ;; One more symbol that doesn't fit into the code above.
    (let ((c 'sb!impl::+magic-hash-vector-value+))
      (push (list (c-symbol-name c)
                  9
                  (symbol-value c)
                  #!+(and win32 x86-64) "LLU"
                  #!-(and win32 x86-64) "LU"
                  nil)
            constants))
    (setf constants
          (sort constants
                (lambda (const1 const2)
                  (if (= (second const1) (second const2))
                      (if (= (third const1) (third const2))
                          (string< (first const1) (first const2))
                          (< (third const1) (third const2)))
                      (< (second const1) (second const2))))))
    (let ((prev-priority (second (car constants))))
      (dolist (const constants)
        (destructuring-bind (name priority value suffix doc) const
          (unless (= prev-priority priority)
            (terpri)
            (setf prev-priority priority))
          (when (minusp value)
            (error "stub: negative values unsupported"))
          (format t "#define ~A ~A~A /* 0x~X ~@[ -- ~A ~]*/~%" name value suffix value doc))))
    (terpri))

  ;; writing information about internal errors
  ;; Assembly code needs only the constants for UNDEFINED_[ALIEN_]FUN_ERROR
  ;; but to avoid imparting that knowledge here, we'll expose all error
  ;; number constants except for OBJECT-NOT-<x>-ERROR ones.
  (loop for interr across sb!c:+backend-internal-errors+
        for i from 0
        when (stringp (car interr))
        do (format t "#define ~A ~D~%" (c-symbol-name (cdr interr)) i))
  ;; C code needs strings for describe_internal_error()
  (format t "#define INTERNAL_ERROR_NAMES ~{\\~%~S~^, ~}~2%"
          (map 'list 'sb!kernel::!c-stringify-internal-error
               sb!c:+backend-internal-errors+))

  ;; I'm not really sure why this is in SB!C, since it seems
  ;; conceptually like something that belongs to SB!VM. In any case,
  ;; it's needed C-side.
  (format t "#define BACKEND_PAGE_BYTES ~DLU~%" sb!c:*backend-page-bytes*)

  (terpri)

  ;; FIXME: The SPARC has a PSEUDO-ATOMIC-TRAP that differs between
  ;; platforms. If we export this from the SB!VM package, it gets
  ;; written out as #define trap_PseudoAtomic, which is confusing as
  ;; the runtime treats trap_ as the prefix for illegal instruction
  ;; type things. We therefore don't export it, but instead do
  #!+sparc
  (when (boundp 'sb!vm::pseudo-atomic-trap)
    (format t
            "#define PSEUDO_ATOMIC_TRAP ~D /* 0x~:*~X */~%"
            sb!vm::pseudo-atomic-trap)
    (terpri))
  ;; possibly this is another candidate for a rename (to
  ;; pseudo-atomic-trap-number or pseudo-atomic-magic-constant
  ;; [possibly applicable to other platforms])

  #!+sb-safepoint
  (format t "#define GC_SAFEPOINT_PAGE_ADDR ((void*)0x~XUL) /* ~:*~A */~%"
            sb!vm:gc-safepoint-page-addr)

  (dolist (symbol '(sb!vm::float-traps-byte
                    sb!vm::float-exceptions-byte
                    sb!vm::float-sticky-bits
                    sb!vm::float-rounding-mode))
    (format t "#define ~A_POSITION ~A /* ~:*0x~X */~%"
            (c-symbol-name symbol)
            (sb!xc:byte-position (symbol-value symbol)))
    (format t "#define ~A_MASK 0x~X /* ~:*~A */~%"
            (c-symbol-name symbol)
            (sb!xc:mask-field (symbol-value symbol) -1))))

#!+sb-ldb
(defun write-tagnames-h (&optional (out *standard-output*))
  (labels
      ((pretty-name (symbol strip)
         (let ((name (string-downcase symbol)))
           (substitute #\Space #\-
                       (subseq name 0 (- (length name) (length strip))))))
       (list-sorted-tags (tail)
         (loop for symbol being the external-symbols of "SB!VM"
               when (and (constantp symbol)
                         (tailwise-equal (string symbol) tail))
               collect symbol into tags
               finally (return (sort tags #'< :key #'symbol-value))))
       (write-tags (kind limit ash-count)
         (format out "~%static const char *~(~A~)_names[] = {~%"
                 (subseq kind 1))
         (let ((tags (list-sorted-tags kind)))
           (dotimes (i limit)
             (if (eql i (ash (or (symbol-value (first tags)) -1) ash-count))
                 (format out "    \"~A\"" (pretty-name (pop tags) kind))
                 (format out "    \"unknown [~D]\"" i))
             (unless (eql i (1- limit))
               (write-string "," out))
             (terpri out)))
         (write-line "};" out)))
    (write-tags "-LOWTAG" sb!vm:lowtag-limit 0)
    ;; this -2 shift depends on every OTHER-IMMEDIATE-?-LOWTAG
    ;; ending with the same 2 bits. (#b10)
    (write-tags "-WIDETAG" (ash (1+ sb!vm:widetag-mask) -2) -2))
  ;; Inform print_otherptr() of all array types that it's too dumb to print
  (let ((array-type-bits (make-array 32 :initial-element 0)))
    (flet ((toggle (b)
             (multiple-value-bind (ofs bit) (floor b 8)
               (setf (aref array-type-bits ofs) (ash 1 bit)))))
      (dovector (saetp sb!vm:*specialized-array-element-type-properties*)
        (unless (or (typep (sb!vm:saetp-ctype saetp) 'character-set-type)
                    (eq (sb!vm:saetp-specifier saetp) t))
          (toggle (sb!vm:saetp-typecode saetp))
          (awhen (sb!vm:saetp-complex-typecode saetp) (toggle it)))))
    (format out
            "~%static unsigned char unprintable_array_types[32] =~% {~{~d~^,~}};~%"
            (coerce array-type-bits 'list)))
  (values))

(defun write-primitive-object (obj)
  ;; writing primitive object layouts
  (format t "#ifndef LANGUAGE_ASSEMBLY~2%")
  (format t
          "struct ~A {~%"
          (c-name (string-downcase (string (sb!vm:primitive-object-name obj)))))
  (when (sb!vm:primitive-object-widetag obj)
    (format t "    lispobj header;~%"))
  (dolist (slot (sb!vm:primitive-object-slots obj))
    (format t "    ~A ~A~@[[1]~];~%"
            (getf (sb!vm:slot-options slot) :c-type "lispobj")
            (c-name (string-downcase (string (sb!vm:slot-name slot))))
            (sb!vm:slot-rest-p slot)))
  (format t "};~2%")
  (format t "#else /* LANGUAGE_ASSEMBLY */~2%")
  (format t "/* These offsets are SLOT-OFFSET * N-WORD-BYTES - LOWTAG~%")
  (format t " * so they work directly on tagged addresses. */~2%")
  (let ((name (sb!vm:primitive-object-name obj))
        (lowtag (or (symbol-value (sb!vm:primitive-object-lowtag obj))
                    0)))
    (dolist (slot (sb!vm:primitive-object-slots obj))
      (format t "#define ~A_~A_OFFSET ~D~%"
              (c-symbol-name name)
              (c-symbol-name (sb!vm:slot-name slot))
              (- (* (sb!vm:slot-offset slot) sb!vm:n-word-bytes) lowtag)))
    (terpri))
  (format t "#endif /* LANGUAGE_ASSEMBLY */~2%"))

(defun write-structure-object (dd)
  (flet ((cstring (designator)
           (c-name (string-downcase (string designator)))))
    (format t "#ifndef LANGUAGE_ASSEMBLY~2%")
    (format t "struct ~A {~%" (cstring (dd-name dd)))
    (format t "    lispobj header;~%")
    ;; "self layout" slots are named '_layout' instead of 'layout' so that
    ;; classoid's expressly declared layout isn't renamed as a special-case.
    (format t "    lispobj _layout;~%")
    #!-interleaved-raw-slots
    (progn
      ;; Note: if the structure has no raw slots, but has an even number of
      ;; ordinary slots (incl. layout, sans header), then the last slot gets
      ;; named 'raw_slot_paddingN' (not 'paddingN')
      ;; The choice of name is mildly disturbing, but harmless.
      (dolist (slot (dd-slots dd))
        (when (eq t (dsd-raw-type slot))
          (format t "    lispobj ~A;~%" (cstring (dsd-name slot)))))
      (unless (oddp (+ (dd-length dd) (dd-raw-length dd)))
        (format t "    lispobj raw_slot_padding;~%"))
      (dotimes (n (dd-raw-length dd))
        (format t "    lispobj raw~D;~%" (- (dd-raw-length dd) n 1))))
    #!+interleaved-raw-slots
    (let ((index 1))
      (dolist (slot (dd-slots dd))
        (cond ((eq t (dsd-raw-type slot))
               (loop while (< index (dsd-index slot))
                     do
                     (format t "    lispobj raw_slot_padding~A;~%" index)
                     (incf index))
               (format t "    lispobj ~A;~%" (cstring (dsd-name slot)))
               (incf index))))
      (unless (oddp (dd-length dd))
        (format t "    lispobj end_padding;~%")))
    (format t "};~2%")
    (format t "#endif /* LANGUAGE_ASSEMBLY */~2%")))

(defun write-static-symbols ()
  (dolist (symbol (cons nil sb!vm:*static-symbols*))
    ;; FIXME: It would be nice to use longer names than NIL and
    ;; (particularly) T in #define statements.
    (format t "#define ~A LISPOBJ(0x~X)~%"
            ;; FIXME: It would be nice not to need to strip anything
            ;; that doesn't get stripped always by C-SYMBOL-NAME.
            (c-symbol-name symbol "%*.!")
            (if *static*                ; if we ran GENESIS
              ;; We actually ran GENESIS, use the real value.
              (descriptor-bits (cold-intern symbol))
              ;; We didn't run GENESIS, so guess at the address.
              (+ sb!vm:static-space-start
                 sb!vm:n-word-bytes
                 sb!vm:other-pointer-lowtag
                   (if symbol (sb!vm:static-symbol-offset symbol) 0))))))


;;;; writing map file

;;; Write a map file describing the cold load. Some of this
;;; information is subject to change due to relocating GC, but even so
;;; it can be very handy when attempting to troubleshoot the early
;;; stages of cold load.
(defun write-map ()
  (let ((*print-pretty* nil)
        (*print-case* :upcase))
    (format t "assembler routines defined in core image:~2%")
    (dolist (routine (sort (copy-list *cold-assembler-routines*) #'<
                           :key #'cdr))
      (format t "#X~8,'0X: ~S~%" (cdr routine) (car routine)))
    (let ((funs nil)
          (undefs nil))
      (maphash (lambda (name fdefn)
                 (let ((fun (cold-fdefn-fun fdefn)))
                   (if (cold-null fun)
                       (push name undefs)
                       (let ((addr (read-wordindexed
                                    fdefn sb!vm:fdefn-raw-addr-slot)))
                         (push (cons name (descriptor-bits addr))
                               funs)))))
               *cold-fdefn-objects*)
      (format t "~%~|~%initially defined functions:~2%")
      (setf funs (sort funs #'< :key #'cdr))
      (dolist (info funs)
        (format t "0x~8,'0X: ~S   #X~8,'0X~%" (cdr info) (car info)
                (- (cdr info) #x17)))
      (format t
"~%~|
(a note about initially undefined function references: These functions
are referred to by code which is installed by GENESIS, but they are not
installed by GENESIS. This is not necessarily a problem; functions can
be defined later, by cold init toplevel forms, or in files compiled and
loaded at warm init, or elsewhere. As long as they are defined before
they are called, everything should be OK. Things are also OK if the
cross-compiler knew their inline definition and used that everywhere
that they were called before the out-of-line definition is installed,
as is fairly common for structure accessors.)
initially undefined function references:~2%")

      (setf undefs (sort undefs #'string< :key #'fun-name-block-name))
      (dolist (name undefs)
        (format t "~8,'0X: ~S~%"
                (descriptor-bits (gethash name *cold-fdefn-objects*))
                name)))

    (format t "~%~|~%layout names:~2%")
    (dolist (x (sort-cold-layouts))
      (let* ((des (cdr x))
             (inherits (read-slot des *host-layout-of-layout* :inherits)))
        (format t "~8,'0X: ~S[~D]~%~10T~:S~%" (descriptor-bits des) (car x)
                  (cold-layout-length des) (listify-cold-inherits inherits)))))

  (values))

;;;; writing core file

(defvar *core-file*)
(defvar *data-page*)

;;; magic numbers to identify entries in a core file
;;;
;;; (In case you were wondering: No, AFAIK there's no special magic about
;;; these which requires them to be in the 38xx range. They're just
;;; arbitrary words, tested not for being in a particular range but just
;;; for equality. However, if you ever need to look at a .core file and
;;; figure out what's going on, it's slightly convenient that they're
;;; all in an easily recognizable range, and displacing the range away from
;;; zero seems likely to reduce the chance that random garbage will be
;;; misinterpreted as a .core file.)
(defconstant build-id-core-entry-type-code 3860)
(defconstant new-directory-core-entry-type-code 3861)
(defconstant initial-fun-core-entry-type-code 3863)
(defconstant page-table-core-entry-type-code 3880)
(defconstant end-core-entry-type-code 3840)

(declaim (ftype (function (sb!vm:word) sb!vm:word) write-word))
(defun write-word (num)
  (ecase sb!c:*backend-byte-order*
    (:little-endian
     (dotimes (i sb!vm:n-word-bytes)
       (write-byte (ldb (byte 8 (* i 8)) num) *core-file*)))
    (:big-endian
     (dotimes (i sb!vm:n-word-bytes)
       (write-byte (ldb (byte 8 (* (- (1- sb!vm:n-word-bytes) i) 8)) num)
                   *core-file*))))
  num)

(defun advance-to-page ()
  (force-output *core-file*)
  (file-position *core-file*
                 (round-up (file-position *core-file*)
                           sb!c:*backend-page-bytes*)))

(defun output-gspace (gspace)
  (force-output *core-file*)
  (let* ((posn (file-position *core-file*))
         (bytes (* (gspace-free-word-index gspace) sb!vm:n-word-bytes))
         (pages (ceiling bytes sb!c:*backend-page-bytes*))
         (total-bytes (* pages sb!c:*backend-page-bytes*)))

    (file-position *core-file*
                   (* sb!c:*backend-page-bytes* (1+ *data-page*)))
    (format t
            "writing ~S byte~:P [~S page~:P] from ~S~%"
            total-bytes
            pages
            gspace)
    (force-output)

    ;; Note: It is assumed that the GSPACE allocation routines always
    ;; allocate whole pages (of size *target-page-size*) and that any
    ;; empty gspace between the free pointer and the end of page will
    ;; be zero-filled. This will always be true under Mach on machines
    ;; where the page size is equal. (RT is 4K, PMAX is 4K, Sun 3 is
    ;; 8K).
    (write-bigvec-as-sequence (gspace-bytes gspace)
                              *core-file*
                              :end total-bytes
                              :pad-with-zeros t)
    (force-output *core-file*)
    (file-position *core-file* posn)

    ;; Write part of a (new) directory entry which looks like this:
    ;;   GSPACE IDENTIFIER
    ;;   WORD COUNT
    ;;   DATA PAGE
    ;;   ADDRESS
    ;;   PAGE COUNT
    (write-word (gspace-identifier gspace))
    (write-word (gspace-free-word-index gspace))
    (write-word *data-page*)
    (multiple-value-bind (floor rem)
        (floor (gspace-byte-address gspace) sb!c:*backend-page-bytes*)
      (aver (zerop rem))
      (write-word floor))
    (write-word pages)

    (incf *data-page* pages)))

;;; Create a core file created from the cold loaded image. (This is
;;; the "initial core file" because core files could be created later
;;; by executing SAVE-LISP in a running system, perhaps after we've
;;; added some functionality to the system.)
(declaim (ftype (function (string)) write-initial-core-file))
(defun write-initial-core-file (filename)

  (let ((filenamestring (namestring filename))
        (*data-page* 0))

    (format t
            "[building initial core file in ~S: ~%"
            filenamestring)
    (force-output)

    (with-open-file (*core-file* filenamestring
                                 :direction :output
                                 :element-type '(unsigned-byte 8)
                                 :if-exists :rename-and-delete)

      ;; Write the magic number.
      (write-word core-magic)

      ;; Write the build ID.
      (write-word build-id-core-entry-type-code)
      (let ((build-id (with-open-file (s "output/build-id.tmp")
                        (read s))))
        (declare (type simple-string build-id))
        (/show build-id (length build-id))
        ;; Write length of build ID record: BUILD-ID-CORE-ENTRY-TYPE-CODE
        ;; word, this length word, and one word for each char of BUILD-ID.
        (write-word (+ 2 (length build-id)))
        (dovector (char build-id)
          ;; (We write each character as a word in order to avoid
          ;; having to think about word alignment issues in the
          ;; sbcl-0.7.8 version of coreparse.c.)
          (write-word (sb!xc:char-code char))))

      ;; Write the New Directory entry header.
      (write-word new-directory-core-entry-type-code)
      (write-word 17) ; length = (5 words/space) * 3 spaces + 2 for header.

      (output-gspace *read-only*)
      (output-gspace *static*)
      (output-gspace *dynamic*)

      ;; Write the initial function.
      (write-word initial-fun-core-entry-type-code)
      (write-word 3)
      (let* ((cold-name (cold-intern '!cold-init))
             (initial-fun
              (cold-fdefn-fun (cold-fdefinition-object cold-name))))
        (format t
                "~&/(DESCRIPTOR-BITS INITIAL-FUN)=#X~X~%"
                (descriptor-bits initial-fun))
        (write-word (descriptor-bits initial-fun)))

      ;; Write the End entry.
      (write-word end-core-entry-type-code)
      (write-word 2)))

  (format t "done]~%")
  (force-output)
  (/show "leaving WRITE-INITIAL-CORE-FILE")
  (values))

;;;; the actual GENESIS function

;;; Read the FASL files in OBJECT-FILE-NAMES and produce a Lisp core,
;;; and/or information about a Lisp core, therefrom.
;;;
;;; input file arguments:
;;;   SYMBOL-TABLE-FILE-NAME names a UNIX-style .nm file *with* *any*
;;;     *tab* *characters* *converted* *to* *spaces*. (We push
;;;     responsibility for removing tabs out to the caller it's
;;;     trivial to remove them using UNIX command line tools like
;;;     sed, whereas it's a headache to do it portably in Lisp because
;;;     #\TAB is not a STANDARD-CHAR.) If this file is not supplied,
;;;     a core file cannot be built (but a C header file can be).
;;;
;;; output files arguments (any of which may be NIL to suppress output):
;;;   CORE-FILE-NAME gets a Lisp core.
;;;   C-HEADER-FILE-NAME gets a C header file, traditionally called
;;;     internals.h, which is used by the C compiler when constructing
;;;     the executable which will load the core.
;;;   MAP-FILE-NAME gets (?) a map file. (dunno about this -- WHN 19990815)
;;;
;;; FIXME: GENESIS doesn't belong in SB!VM. Perhaps in %KERNEL for now,
;;; perhaps eventually in SB-LD or SB-BOOT.
(defun sb!vm:genesis (&key
                      object-file-names
                      symbol-table-file-name
                      core-file-name
                      map-file-name
                      c-header-dir-name
                      #+nil (list-objects t))
  #!+sb-dynamic-core
  (declare (ignorable symbol-table-file-name))

  (format t
          "~&beginning GENESIS, ~A~%"
          (if core-file-name
            ;; Note: This output summarizing what we're doing is
            ;; somewhat telegraphic in style, not meant to imply that
            ;; we're not e.g. also creating a header file when we
            ;; create a core.
            (format nil "creating core ~S" core-file-name)
            (format nil "creating headers in ~S" c-header-dir-name)))

  (let ((*cold-foreign-symbol-table* (make-hash-table :test 'equal)))

    #!-sb-dynamic-core
    (when core-file-name
      (if symbol-table-file-name
          (load-cold-foreign-symbol-table symbol-table-file-name)
          (error "can't output a core file without symbol table file input")))

    #!+sb-dynamic-core
    (progn
      (setf (gethash (extern-alien-name "undefined_tramp")
                     *cold-foreign-symbol-table*)
            (dyncore-note-symbol "undefined_tramp" nil))
      (dyncore-note-symbol "undefined_alien_function" nil))

    ;; Now that we've successfully read our only input file (by
    ;; loading the symbol table, if any), it's a good time to ensure
    ;; that there'll be someplace for our output files to go when
    ;; we're done.
    (flet ((frob (filename)
             (when filename
               (ensure-directories-exist filename :verbose t))))
      (frob core-file-name)
      (frob map-file-name))

    ;; (This shouldn't matter in normal use, since GENESIS normally
    ;; only runs once in any given Lisp image, but it could reduce
    ;; confusion if we ever experiment with running, tweaking, and
    ;; rerunning genesis interactively.)
    (do-all-symbols (sym)
      (remprop sym 'cold-intern-info))

    (check-spaces)

    (let* ((*foreign-symbol-placeholder-value* (if core-file-name nil 0))
           (*load-time-value-counter* 0)
           (*cold-fdefn-objects* (make-hash-table :test 'equal))
           (*cold-symbols* (make-hash-table :test 'eql)) ; integer keys
           (*cold-package-symbols* (make-hash-table :test 'equal)) ; string keys
           (pkg-metadata (sb-cold::package-list-for-genesis))
           (*read-only* (make-gspace :read-only
                                     read-only-core-space-id
                                     sb!vm:read-only-space-start))
           (*static*    (make-gspace :static
                                     static-core-space-id
                                     sb!vm:static-space-start))
           (*dynamic*   (make-gspace :dynamic
                                     dynamic-core-space-id
                                     #!+gencgc sb!vm:dynamic-space-start
                                     #!-gencgc sb!vm:dynamic-0-space-start))
           ;; There's a cyclic dependency here: NIL refers to a package;
           ;; a package needs its layout which needs others layouts
           ;; which refer to NIL, which refers to a package ...
           ;; Break the cycle by preallocating packages without a layout.
           ;; This avoids having to track any symbols created prior to
           ;; creation of packages, since packages are primordial.
           (target-cl-pkg-info
            (dolist (name (list* "COMMON-LISP" "COMMON-LISP-USER" "KEYWORD"
                                 (mapcar #'sb-cold:package-data-name
                                         pkg-metadata))
                          (gethash "COMMON-LISP" *cold-package-symbols*))
              (setf (gethash name *cold-package-symbols*)
                    (cons (allocate-struct
                           *dynamic* (make-fixnum-descriptor 0)
                           (layout-length (find-layout 'package)))
                          (cons nil nil))))) ; (externals . internals)
           (*nil-descriptor* (make-nil-descriptor target-cl-pkg-info))
           (*known-structure-classoids* nil)
           (*classoid-cells* (make-hash-table :test 'eq))
           (*current-reversed-cold-toplevels* *nil-descriptor*)
           (*current-debug-sources* *nil-descriptor*)
           (*unbound-marker* (make-other-immediate-descriptor
                              0
                              sb!vm:unbound-marker-widetag))
           *cold-assembler-fixups*
           *cold-assembler-routines*
           (*deferred-known-fun-refs* nil)
           #!+x86 (*load-time-code-fixups* (make-hash-table)))

      ;; Prepare for cold load.
      (initialize-non-nil-symbols)
      (initialize-layouts)
      (initialize-packages
       ;; docstrings are set in src/cold/warm. It would work to do it here,
       ;; but seems preferable not to saddle Genesis with such responsibility.
       (list* (sb-cold:make-package-data :name "COMMON-LISP" :doc nil)
              (sb-cold:make-package-data :name "KEYWORD" :doc nil)
              (sb-cold:make-package-data :name "COMMON-LISP-USER" :doc nil
               :use '("COMMON-LISP"
                      ;; ANSI encourages us to put extension packages
                      ;; in the USE list of COMMON-LISP-USER.
                      "SB!ALIEN" "SB!DEBUG" "SB!EXT" "SB!GRAY" "SB!PROFILE"))
              pkg-metadata))
      (initialize-static-fns)

      ;; Initialize the *COLD-SYMBOLS* system with the information
      ;; from common-lisp-exports.lisp-expr.
      ;; Packages whose names match SB!THING were set up on the host according
      ;; to "package-data-list.lisp-expr" which expresses the desired target
      ;; package configuration, so we can just mirror the host into the target.
      ;; But by waiting to observe calls to COLD-INTERN that occur during the
      ;; loading of the cross-compiler's outputs, it is possible to rid the
      ;; target of accidental leftover symbols, not that it wouldn't also be
      ;; a good idea to clean up package-data-list once in a while.
      (dolist (exported-name
               (sb-cold:read-from-file "common-lisp-exports.lisp-expr"))
        (cold-intern (intern exported-name *cl-package*) :access :external))

      ;; Cold load.
      (dolist (file-name object-file-names)
        (write-line (namestring file-name))
        (cold-load file-name))

      (when *known-structure-classoids*
        (let ((dd-layout (find-layout 'defstruct-description)))
          (dolist (defstruct-args *known-structure-classoids*)
            (let* ((dd (first defstruct-args))
                   (name (warm-symbol (read-slot dd dd-layout :name)))
                   (layout (gethash name *cold-layouts*)))
              (aver layout)
              (write-slots layout *host-layout-of-layout* :info dd))))
        (format t "~&; SB!Loader: ~D DEFSTRUCTs, ~D DEFUNs, ~D SETF macros~%"
                (length *known-structure-classoids*)
                (cold-list-length (cold-symbol-value '*!reversed-cold-defuns*))
                (cold-list-length
                 (cold-symbol-value '*!reversed-cold-setf-macros*))))

      ;; Tidy up loose ends left by cold loading. ("Postpare from cold load?")
      (resolve-deferred-known-funs)
      (resolve-assembler-fixups)
      #!+x86 (output-load-time-code-fixups)
      (foreign-symbols-to-core)
      (finish-symbols)
      (/show "back from FINISH-SYMBOLS")
      (finalize-load-time-value-noise)

      ;; Tell the target Lisp how much stuff we've allocated.
      (cold-set 'sb!vm:*read-only-space-free-pointer*
                (allocate-cold-descriptor *read-only*
                                          0
                                          sb!vm:even-fixnum-lowtag))
      (cold-set 'sb!vm:*static-space-free-pointer*
                (allocate-cold-descriptor *static*
                                          0
                                          sb!vm:even-fixnum-lowtag))
      (/show "done setting free pointers")

      ;; Write results to files.
      ;;
      ;; FIXME: I dislike this approach of redefining
      ;; *STANDARD-OUTPUT* instead of putting the new stream in a
      ;; lexical variable, and it's annoying to have WRITE-MAP (to
      ;; *STANDARD-OUTPUT*) not be parallel to WRITE-INITIAL-CORE-FILE
      ;; (to a stream explicitly passed as an argument).
      (macrolet ((out-to (name &body body)
                   `(let ((fn (format nil "~A/~A.h" c-header-dir-name ,name)))
                     (ensure-directories-exist fn)
                     (with-open-file (*standard-output* fn
                                      :if-exists :supersede :direction :output)
                       (write-boilerplate)
                       (let ((n (c-name (string-upcase ,name))))
                         (format
                          t
                          "#ifndef SBCL_GENESIS_~A~%#define SBCL_GENESIS_~A 1~%"
                          n n))
                       ,@body
                       (format t
                        "#endif /* SBCL_GENESIS_~A */~%"
                        (string-upcase ,name))))))
        (when map-file-name
          (with-open-file (*standard-output* map-file-name
                                             :direction :output
                                             :if-exists :supersede)
            (write-map)))
        (out-to "config" (write-config-h))
        (out-to "constants" (write-constants-h))
        #!+sb-ldb
        (out-to "tagnames" (write-tagnames-h))
        (let ((structs (sort (copy-list sb!vm:*primitive-objects*) #'string<
                             :key (lambda (obj)
                                    (symbol-name
                                     (sb!vm:primitive-object-name obj))))))
          (dolist (obj structs)
            (out-to
             (string-downcase (string (sb!vm:primitive-object-name obj)))
             (write-primitive-object obj)))
          (out-to "primitive-objects"
                  (dolist (obj structs)
                    (format t "~&#include \"~A.h\"~%"
                            (string-downcase
                             (string (sb!vm:primitive-object-name obj)))))))
        (dolist (class '(hash-table
                         classoid
                         layout
                         sb!c::compiled-debug-info
                         sb!c::compiled-debug-fun
                         package))
          (out-to
           (string-downcase (string class))
           (write-structure-object
            (layout-info (find-layout class)))))
        (out-to "static-symbols" (write-static-symbols))

        (let ((fn (format nil "~A/Makefile.features" c-header-dir-name)))
          (ensure-directories-exist fn)
          (with-open-file (*standard-output* fn :if-exists :supersede
                                             :direction :output)
            (write-makefile-features)))

        (when core-file-name
          (write-initial-core-file core-file-name))))))

;; This generalization of WARM-FUN-NAME will do in a pinch
;; to view strings and things in a gspace.
(defun warmelize (descriptor)
  (labels ((recurse (x)
            (when (cold-null x)
              (return-from recurse nil))
            (ecase (descriptor-lowtag x)
              (#.sb!vm:list-pointer-lowtag
               (cons (recurse (cold-car x)) (recurse (cold-cdr x))))
              (#.sb!vm:fun-pointer-lowtag
               (let ((name (read-wordindexed x sb!vm:simple-fun-name-slot)))
                 `(function ,(recurse name))))
              (#.sb!vm:other-pointer-lowtag
               (let ((widetag (logand (descriptor-bits (read-memory x))
                                      sb!vm:widetag-mask)))
                 (ecase widetag
                   (#.sb!vm:symbol-header-widetag
                    ;; this is only approximate, as it disregards package
                    (intern (recurse (read-wordindexed x sb!vm:symbol-name-slot))))
                   (#.sb!vm:simple-base-string-widetag
                    (base-string-from-core x))))))))
    (recurse descriptor)))
