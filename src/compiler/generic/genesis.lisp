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
;;;; by DEFUN aren't set up specially by GENESIS. In particular,
;;;; structure slot accessors are not set up. Slot accessors are
;;;; available at cold init time because they're usually compiled
;;;; inline. They're not available as out-of-line functions until the
;;;; toplevel forms installing them have run.)

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

;;; the current version of SBCL core files
;;;
;;; FIXME: This is left over from CMU CL, and not well thought out.
;;; It's good to make sure that the runtime doesn't try to run core
;;; files from the wrong version, but a single number is not the ideal
;;; way to do this in high level data like this (as opposed to e.g. in
;;; IP packets), and in fact the CMU CL version number never ended up
;;; being incremented past 0. A better approach might be to use a
;;; string which is set from CVS data. (Though now as of sbcl-0.7.8 or
;;; so, we have another problem that the core incompatibility
;;; detection mechanisms are on such a hair trigger -- with even
;;; different builds from the same sources being considered
;;; incompatible -- that any coarser-grained versioning mechanisms
;;; like this are largely irrelevant as long as the hair-triggering
;;; persists.)
;;;
;;; 0: inherited from CMU CL
;;; 1: rearranged static symbols for sbcl-0.6.8
;;; 2: eliminated non-ANSI %DEFCONSTANT/%%DEFCONSTANT support,
;;;    deleted a slot from DEBUG-SOURCE structure
;;; 3: added build ID to cores to discourage sbcl/.core mismatch
;;; 4: added gc page table data
(defconstant sbcl-core-version-integer 4)

(defun round-up (number size)
  #!+sb-doc
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
(defun write-bigvec-as-sequence (bigvec stream &key (start 0) end)
  (loop for i of-type index from start below (or end (bvlength bigvec)) do
        (write-byte (bvref bigvec i)
                    stream)))

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
#!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
(progn
(defun bvref-word (bytes index)
  (bvref-64 bytes index))
(defun (setf bvref-word) (new-val bytes index)
  (setf (bvref-64 bytes index) new-val)))

#!+#.(cl:if (cl:= 32 sb!vm:n-word-bits) '(and) '(or))
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

(defconstant descriptor-low-bits 16
  "the number of bits in the low half of the descriptor")
(defconstant target-space-alignment (ash 1 descriptor-low-bits)
  "the alignment requirement for spaces in the target.
  Must be at least (ASH 1 DESCRIPTOR-LOW-BITS)")

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

(defstruct (descriptor
            (:constructor make-descriptor
                          (high low &optional gspace word-offset))
            (:copier nil))
  ;; the GSPACE that this descriptor is allocated in, or NIL if not set yet.
  (gspace nil :type (or gspace (eql :load-time-value) null))
  ;; the offset in words from the start of GSPACE, or NIL if not set yet
  (word-offset nil :type (or sb!vm:word null))
  ;; the high and low halves of the descriptor
  ;;
  ;; KLUDGE: Judging from the comments in genesis.lisp of the CMU CL
  ;; old-rt compiler, this split dates back from a very early version
  ;; of genesis where 32-bit integers were represented as conses of
  ;; two 16-bit integers. In any system with nice (UNSIGNED-BYTE 32)
  ;; structure slots, like CMU CL >= 17 or any version of SBCL, there
  ;; seems to be no reason to persist in this. -- WHN 19990917
  high
  low)
(def!method print-object ((des descriptor) stream)
  (let ((lowtag (descriptor-lowtag des)))
    (print-unreadable-object (des stream :type t)
      (cond ((or (= lowtag sb!vm:even-fixnum-lowtag)
                 (= lowtag sb!vm:odd-fixnum-lowtag))
             (let ((unsigned (logior (ash (descriptor-high des)
                                          (1+ (- descriptor-low-bits
                                                 sb!vm:n-lowtag-bits)))
                                     (ash (descriptor-low des)
                                          (- 1 sb!vm:n-lowtag-bits)))))
               (format stream
                       "for fixnum: ~W"
                       (if (> unsigned #x1FFFFFFF)
                           (- unsigned #x40000000)
                           unsigned))))
            ((or (= lowtag sb!vm:other-immediate-0-lowtag)
                 (= lowtag sb!vm:other-immediate-1-lowtag)
                 #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
                 (= lowtag sb!vm:other-immediate-2-lowtag)
                 #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
                 (= lowtag sb!vm:other-immediate-3-lowtag))
             (format stream
                     "for other immediate: #X~X, type #b~8,'0B"
                     (ash (descriptor-bits des) (- sb!vm:n-widetag-bits))
                     (logand (descriptor-low des) sb!vm:widetag-mask)))
            (t
             (format stream
                     "for pointer: #X~X, lowtag #b~3,'0B, ~A"
                     (logior (ash (descriptor-high des) descriptor-low-bits)
                             (logandc2 (descriptor-low des) sb!vm:lowtag-mask))
                     lowtag
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
      (make-descriptor (ash ptr (- sb!vm:word-shift descriptor-low-bits))
                       (logior (ash (logand ptr
                                            (1- (ash 1
                                                     (- descriptor-low-bits
                                                        sb!vm:word-shift))))
                                    sb!vm:word-shift)
                               lowtag)
                       gspace
                       old-free-word-index))))

(defun descriptor-lowtag (des)
  #!+sb-doc
  "the lowtag bits for DES"
  (logand (descriptor-low des) sb!vm:lowtag-mask))

(defun descriptor-bits (des)
  (logior (ash (descriptor-high des) descriptor-low-bits)
          (descriptor-low des)))

(defun descriptor-fixnum (des)
  (let ((bits (descriptor-bits des)))
    (if (logbitp (1- sb!vm:n-word-bits) bits)
        ;; KLUDGE: The (- SB!VM:N-WORD-BITS 2) term here looks right to
        ;; me, and it works, but in CMU CL it was (1- SB!VM:N-WORD-BITS),
        ;; and although that doesn't make sense for me, or work for me,
        ;; it's hard to see how it could have been wrong, since CMU CL
        ;; genesis worked. It would be nice to understand how this came
        ;; to be.. -- WHN 19990901
        (logior (ash bits (- 1 sb!vm:n-lowtag-bits))
                (ash -1 (1+ sb!vm:n-positive-fixnum-bits)))
        (ash bits (- 1 sb!vm:n-lowtag-bits)))))

(defun descriptor-word-sized-integer (des)
  ;; Extract an (unsigned-byte 32), from either its fixnum or bignum
  ;; representation.
  (let ((lowtag (descriptor-lowtag des)))
    (if (or (= lowtag sb!vm:even-fixnum-lowtag)
            (= lowtag sb!vm:odd-fixnum-lowtag))
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
      (let ((lowtag (descriptor-lowtag des))
            (high (descriptor-high des))
            (low (descriptor-low des)))

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
          ;;
          ;; Most of the faffing around in here involving ash and
          ;; various computed shift counts is due to the high/low
          ;; split representation of the descriptor bits and an
          ;; apparent disinclination to create intermediate values
          ;; larger than a target fixnum.
          ;;
          ;; This code relies on the fact that GSPACEs are aligned
          ;; such that the descriptor-low-bits low bits are zero.
          (when (and (>= high (ash (gspace-word-address gspace)
                                   (- sb!vm:word-shift descriptor-low-bits)))
                     (<= high (ash (+ (gspace-word-address gspace)
                                      (gspace-free-word-index gspace))
                                   (- sb!vm:word-shift descriptor-low-bits))))
            ;; Update the descriptor with the correct gspace and the
            ;; offset within the gspace and return the gspace.
            (setf (descriptor-gspace des) gspace)
            (setf (descriptor-word-offset des)
                  (+ (ash (- high (ash (gspace-word-address gspace)
                                       (- sb!vm:word-shift
                                          descriptor-low-bits)))
                          (- descriptor-low-bits sb!vm:word-shift))
                     (ash (logandc2 low sb!vm:lowtag-mask)
                          (- sb!vm:word-shift))))
            (return gspace))))))

(defun make-random-descriptor (value)
  (make-descriptor (logand (ash value (- descriptor-low-bits))
                           (1- (ash 1
                                    (- sb!vm:n-word-bits
                                       descriptor-low-bits))))
                   (logand value (1- (ash 1 descriptor-low-bits)))))

(defun make-fixnum-descriptor (num)
  (when (>= (integer-length num)
            (1+ (- sb!vm:n-word-bits sb!vm:n-lowtag-bits)))
    (error "~W is too big for a fixnum." num))
  (make-random-descriptor (ash num (1- sb!vm:n-lowtag-bits))))

(defun make-other-immediate-descriptor (data type)
  (make-descriptor (ash data (- sb!vm:n-widetag-bits descriptor-low-bits))
                   (logior (logand (ash data (- descriptor-low-bits
                                                sb!vm:n-widetag-bits))
                                   (1- (ash 1 descriptor-low-bits)))
                           type)))

(defun make-character-descriptor (data)
  (make-other-immediate-descriptor data sb!vm:character-widetag))

(defun descriptor-beyond (des offset type)
  (let* ((low (logior (+ (logandc2 (descriptor-low des) sb!vm:lowtag-mask)
                         offset)
                      type))
         (high (+ (descriptor-high des)
                  (ash low (- descriptor-low-bits)))))
    (make-descriptor high (logand low (1- (ash 1 descriptor-low-bits))))))

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
;;; loadtime value, represented by (CONS KEYWORD ..). The FILENAME
;;; tells which fasl file each list element came from, for debugging
;;; purposes.
(defvar *current-reversed-cold-toplevels*)

;;; the head of a list of DEBUG-SOURCEs which need to be patched when
;;; the cold core starts up
(defvar *current-debug-sources*)

;;; the name of the object file currently being cold loaded (as a string, not a
;;; pathname), or NIL if we're not currently cold loading any object file
(defvar *cold-load-filename* nil)
(declaim (type (or string null) *cold-load-filename*))

;;;; miscellaneous stuff to read and write the core memory

;;; FIXME: should be DEFINE-MODIFY-MACRO
(defmacro cold-push (thing list)
  #!+sb-doc
  "Push THING onto the given cold-load LIST."
  `(setq ,list (cold-cons ,thing ,list)))

(declaim (ftype (function (descriptor sb!vm:word) descriptor) read-wordindexed))
(defun read-wordindexed (address index)
  #!+sb-doc
  "Return the value which is displaced by INDEX words from ADDRESS."
  (let* ((gspace (descriptor-intuit-gspace address))
         (bytes (gspace-bytes gspace))
         (byte-index (ash (+ index (descriptor-word-offset address))
                          sb!vm:word-shift))
         (value (bvref-word bytes byte-index)))
    (make-random-descriptor value)))

(declaim (ftype (function (descriptor) descriptor) read-memory))
(defun read-memory (address)
  #!+sb-doc
  "Return the value at ADDRESS."
  (read-wordindexed address 0))

;;; (Note: In CMU CL, this function expected a SAP-typed ADDRESS
;;; value, instead of the object-and-offset we use here.)
(declaim (ftype (function (descriptor sb!vm:word descriptor) (values))
                note-load-time-value-reference))
(defun note-load-time-value-reference (address offset marker)
  (cold-push (cold-cons
              (cold-intern :load-time-value-fixup)
              (cold-cons address
                         (cold-cons (number-to-core offset)
                                    (cold-cons
                                     (number-to-core (descriptor-word-offset marker))
                                     *nil-descriptor*))))
             *current-reversed-cold-toplevels*)
  (values))

(declaim (ftype (function (descriptor sb!vm:word (or descriptor symbol))) write-wordindexed))
(defun write-wordindexed (address index value)
  #!+sb-doc
  "Write VALUE displaced INDEX words from ADDRESS."
  ;; If we're passed a symbol as a value then it needs to be interned.
  (when (symbolp value) (setf value (cold-intern value)))
  (if (eql (descriptor-gspace value) :load-time-value)
    (note-load-time-value-reference address
                                    (- (ash index sb!vm:word-shift)
                                       (logand (descriptor-bits address)
                                               sb!vm:lowtag-mask))
                                    value)
    (let* ((bytes (gspace-bytes (descriptor-intuit-gspace address)))
           (byte-index (ash (+ index (descriptor-word-offset address))
                               sb!vm:word-shift)))
      (setf (bvref-word bytes byte-index)
            (descriptor-bits value)))))

(declaim (ftype (function (descriptor (or descriptor symbol))) write-memory))
(defun write-memory (address value)
  #!+sb-doc
  "Write VALUE (a DESCRIPTOR) at ADDRESS (also a DESCRIPTOR)."
  (write-wordindexed address 0 value))

;;;; allocating images of primitive objects in the cold core

;;; There are three kinds of blocks of memory in the type system:
;;; * Boxed objects (cons cells, structures, etc): These objects have no
;;;   header as all slots are descriptors.
;;; * Unboxed objects (bignums): There is a single header word that contains
;;;   the length.
;;; * Vector objects: There is a header word with the type, then a word for
;;;   the length, then the data.
(defun allocate-boxed-object (gspace length lowtag)
  #!+sb-doc
  "Allocate LENGTH words in GSPACE and return a new descriptor of type LOWTAG
  pointing to them."
  (allocate-cold-descriptor gspace (ash length sb!vm:word-shift) lowtag))
(defun allocate-unboxed-object (gspace element-bits length type)
  #!+sb-doc
  "Allocate LENGTH units of ELEMENT-BITS bits plus a header word in GSPACE and
  return an ``other-pointer'' descriptor to them. Initialize the header word
  with the resultant length and TYPE."
  (let* ((bytes (/ (* element-bits length) sb!vm:n-byte-bits))
         (des (allocate-cold-descriptor gspace
                                        (+ bytes sb!vm:n-word-bytes)
                                        sb!vm:other-pointer-lowtag)))
    (write-memory des
                  (make-other-immediate-descriptor (ash bytes
                                                        (- sb!vm:word-shift))
                                                   type))
    des))
(defun allocate-vector-object (gspace element-bits length type)
  #!+sb-doc
  "Allocate LENGTH units of ELEMENT-BITS size plus a header plus a length slot in
  GSPACE and return an ``other-pointer'' descriptor to them. Initialize the
  header word with TYPE and the length slot with LENGTH."
  ;; FIXME: Here and in ALLOCATE-UNBOXED-OBJECT, BYTES is calculated using
  ;; #'/ instead of #'CEILING, which seems wrong.
  (let* ((bytes (/ (* element-bits length) sb!vm:n-byte-bits))
         (des (allocate-cold-descriptor gspace
                                        (+ bytes (* 2 sb!vm:n-word-bytes))
                                        sb!vm:other-pointer-lowtag)))
    (write-memory des (make-other-immediate-descriptor 0 type))
    (write-wordindexed des
                       sb!vm:vector-length-slot
                       (make-fixnum-descriptor length))
    des))

;;;; copying simple objects into the cold core

(defun base-string-to-core (string &optional (gspace *dynamic*))
  #!+sb-doc
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

(defun bignum-to-core (n)
  #!+sb-doc
  "Copy a bignum to the cold core."
  (let* ((words (ceiling (1+ (integer-length n)) sb!vm:n-word-bits))
         (handle (allocate-unboxed-object *dynamic*
                                          sb!vm:n-word-bits
                                          words
                                          sb!vm:bignum-widetag)))
    (declare (fixnum words))
    (do ((index 1 (1+ index))
         (remainder n (ash remainder (- sb!vm:n-word-bits))))
        ((> index words)
         (unless (zerop (integer-length remainder))
           ;; FIXME: Shouldn't this be a fatal error?
           (warn "~W words of ~W were written, but ~W bits were left over."
                 words n remainder)))
      (let ((word (ldb (byte sb!vm:n-word-bits 0) remainder)))
        (write-wordindexed handle index
                           (make-descriptor (ash word (- descriptor-low-bits))
                                            (ldb (byte descriptor-low-bits 0)
                                                 word)))))
    handle))

(defun number-pair-to-core (first second type)
  #!+sb-doc
  "Makes a number pair of TYPE (ratio or complex) and fills it in."
  (let ((des (allocate-unboxed-object *dynamic* sb!vm:n-word-bits 2 type)))
    (write-wordindexed des 1 first)
    (write-wordindexed des 2 second)
    des))

(defun write-double-float-bits (address index x)
  (let ((hi (double-float-high-bits x))
        (lo (double-float-low-bits x)))
    (ecase sb!vm::n-word-bits
      (32
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
     #!+#.(cl:if (cl:= sb!vm:n-word-bits 64) '(and) '(or))
     (make-random-descriptor (logior (ash (single-float-bits x) 32)
                                     sb!vm::single-float-widetag))
     #!-#.(cl:if (cl:= sb!vm:n-word-bits 64) '(and) '(or))
     (let ((des (allocate-unboxed-object *dynamic*
                                         sb!vm:n-word-bits
                                         (1- sb!vm:single-float-size)
                                         sb!vm:single-float-widetag)))
       (write-wordindexed des
                          sb!vm:single-float-value-slot
                          (make-random-descriptor (single-float-bits x)))
       des))
    (double-float
     (let ((des (allocate-unboxed-object *dynamic*
                                         sb!vm:n-word-bits
                                         (1- sb!vm:double-float-size)
                                         sb!vm:double-float-widetag)))
       (write-double-float-bits des sb!vm:double-float-value-slot x)))))

(defun complex-single-float-to-core (num)
  (declare (type (complex single-float) num))
  (let ((des (allocate-unboxed-object *dynamic* sb!vm:n-word-bits
                                      (1- sb!vm:complex-single-float-size)
                                      sb!vm:complex-single-float-widetag)))
    #!-x86-64
    (progn
      (write-wordindexed des sb!vm:complex-single-float-real-slot
                         (make-random-descriptor (single-float-bits (realpart num))))
      (write-wordindexed des sb!vm:complex-single-float-imag-slot
                         (make-random-descriptor (single-float-bits (imagpart num)))))
    #!+x86-64
    (write-wordindexed des sb!vm:complex-single-float-data-slot
                       (make-random-descriptor
                        (logior (ldb (byte 32 0) (single-float-bits (realpart num)))
                                (ash (single-float-bits (imagpart num)) 32))))
    des))

(defun complex-double-float-to-core (num)
  (declare (type (complex double-float) num))
  (let ((des (allocate-unboxed-object *dynamic* sb!vm:n-word-bits
                                      (1- sb!vm:complex-double-float-size)
                                      sb!vm:complex-double-float-widetag)))
    (write-double-float-bits des sb!vm:complex-double-float-real-slot
                             (realpart num))
    (write-double-float-bits des sb!vm:complex-double-float-imag-slot
                             (imagpart num))))

;;; Copy the given number to the core.
(defun number-to-core (number)
  (typecase number
    (integer (if (< (integer-length number)
                    (- (1+ sb!vm:n-word-bits) sb!vm:n-lowtag-bits))
                 (make-fixnum-descriptor number)
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
  (let ((des (allocate-unboxed-object *dynamic*
                                      sb!vm:n-word-bits
                                      (1- sb!vm:sap-size)
                                      sb!vm:sap-widetag)))
    (write-wordindexed des
                       sb!vm:sap-pointer-slot
                       (make-random-descriptor sap-int))
    des))

;;; Allocate a cons cell in GSPACE and fill it in with CAR and CDR.
(defun cold-cons (car cdr &optional (gspace *dynamic*))
  (let ((dest (allocate-boxed-object gspace 2 sb!vm:list-pointer-lowtag)))
    (write-memory dest car)
    (write-wordindexed dest 1 cdr)
    dest))

;;; Make a simple-vector on the target that holds the specified
;;; OBJECTS, and return its descriptor.
(defun vector-in-core (&rest objects)
  (let* ((size (length objects))
         (result (allocate-vector-object *dynamic* sb!vm:n-word-bits size
                                         sb!vm:simple-vector-widetag)))
    (dotimes (index size)
      (write-wordindexed result (+ index sb!vm:vector-data-offset)
                         (pop objects)))
    result))

;;;; symbol magic

;;; Allocate (and initialize) a symbol.
(defun allocate-symbol (name &key (gspace *dynamic*))
  (declare (simple-string name))
  (let ((symbol (allocate-unboxed-object gspace
                                         sb!vm:n-word-bits
                                         (1- sb!vm:symbol-size)
                                         sb!vm:symbol-header-widetag)))
    (write-wordindexed symbol sb!vm:symbol-value-slot *unbound-marker*)
    (write-wordindexed symbol
                       sb!vm:symbol-hash-slot
                       (make-fixnum-descriptor 0))
    (write-wordindexed symbol sb!vm:symbol-plist-slot *nil-descriptor*)
    (write-wordindexed symbol sb!vm:symbol-name-slot
                       (base-string-to-core name *dynamic*))
    (write-wordindexed symbol sb!vm:symbol-package-slot *nil-descriptor*)
    symbol))

;;; Set the cold symbol value of SYMBOL-OR-SYMBOL-DES, which can be either a
;;; descriptor of a cold symbol or (in an abbreviation for the
;;; most common usage pattern) an ordinary symbol, which will be
;;; automatically cold-interned.
(declaim (ftype (function ((or descriptor symbol) descriptor)) cold-set))
(defun cold-set (symbol-or-symbol-des value)
  (let ((symbol-des (etypecase symbol-or-symbol-des
                      (descriptor symbol-or-symbol-des)
                      (symbol (cold-intern symbol-or-symbol-des)))))
    (write-wordindexed symbol-des sb!vm:symbol-value-slot value)))

;;;; layouts and type system pre-initialization

;;; Since we want to be able to dump structure constants and
;;; predicates with reference layouts, we need to create layouts at
;;; cold-load time. We use the name to intern layouts by, and dump a
;;; list of all cold layouts in *!INITIAL-LAYOUTS* so that type system
;;; initialization can find them. The only thing that's tricky [sic --
;;; WHN 19990816] is initializing layout's layout, which must point to
;;; itself.

;;; a map from class names to lists of
;;;    `(,descriptor ,name ,length ,inherits ,depth)
;;; KLUDGE: It would be more understandable and maintainable to use
;;; DEFSTRUCT (:TYPE LIST) here. -- WHN 19990823
(defvar *cold-layouts* (make-hash-table :test 'equal))

;;; a map from DESCRIPTOR-BITS of cold layouts to the name, for inverting
;;; mapping
(defvar *cold-layout-names* (make-hash-table :test 'eql))

;;; FIXME: *COLD-LAYOUTS* and *COLD-LAYOUT-NAMES* should be
;;; initialized by binding in GENESIS.

;;; the descriptor for layout's layout (needed when making layouts)
(defvar *layout-layout*)

(defconstant target-layout-length
  (layout-length (find-layout 'layout)))

(defun target-layout-index (slot-name)
  ;; KLUDGE: this is a little bit sleazy, but the tricky thing is that
  ;; structure slots don't have a terribly firm idea of their names.
  ;; At least here if we change LAYOUT's package of definition, we
  ;; only have to change one thing...
  (let* ((name (find-symbol (symbol-name slot-name) "SB!KERNEL"))
         (layout (find-layout 'layout))
         (dd (layout-info layout))
         (slots (dd-slots dd))
         (dsd (find name slots :key #'dsd-name)))
    (aver dsd)
    (dsd-index dsd)))

(defun cold-set-layout-slot (cold-layout slot-name value)
  (write-wordindexed
   cold-layout
   (+ sb!vm:instance-slots-offset (target-layout-index slot-name))
   value))

;;; Return a list of names created from the cold layout INHERITS data
;;; in X.
(defun listify-cold-inherits (x)
  (let ((len (descriptor-fixnum (read-wordindexed x
                                                  sb!vm:vector-length-slot))))
    (collect ((res))
      (dotimes (index len)
        (let* ((des (read-wordindexed x (+ sb!vm:vector-data-offset index)))
               (found (gethash (descriptor-bits des) *cold-layout-names*)))
          (if found
            (res found)
            (error "unknown descriptor at index ~S (bits = ~8,'0X)"
                   index
                   (descriptor-bits des)))))
      (res))))

(declaim (ftype (function (symbol descriptor descriptor descriptor descriptor)
                          descriptor)
                make-cold-layout))
(defun make-cold-layout (name length inherits depthoid nuntagged)
  (let ((result (allocate-boxed-object *dynamic*
                                       ;; KLUDGE: Why 1+? -- WHN 19990901
                                       ;; header word? -- CSR 20051204
                                       (1+ target-layout-length)
                                       sb!vm:instance-pointer-lowtag)))
    (write-memory result
                  (make-other-immediate-descriptor
                   target-layout-length sb!vm:instance-header-widetag))

    ;; KLUDGE: The offsets into LAYOUT below should probably be pulled out
    ;; of the cross-compiler's tables at genesis time instead of inserted
    ;; by hand as bare numeric constants. -- WHN ca. 19990901

    ;; Set slot 0 = the layout of the layout.
    (write-wordindexed result sb!vm:instance-slots-offset *layout-layout*)

    ;; Don't set the CLOS hash value: done in cold-init instead.
    ;;
    ;; Set other slot values.
    ;;
    ;; leave CLASSOID uninitialized for now
    (cold-set-layout-slot result 'invalid *nil-descriptor*)
    (cold-set-layout-slot result 'inherits inherits)
    (cold-set-layout-slot result 'depthoid depthoid)
    (cold-set-layout-slot result 'length length)
    (cold-set-layout-slot result 'info *nil-descriptor*)
    (cold-set-layout-slot result 'pure *nil-descriptor*)
    (cold-set-layout-slot result 'n-untagged-slots nuntagged)
    (cold-set-layout-slot result 'source-location *nil-descriptor*)
    (cold-set-layout-slot result 'for-std-class-p *nil-descriptor*)

    (setf (gethash name *cold-layouts*)
          (list result
                name
                (descriptor-fixnum length)
                (listify-cold-inherits inherits)
                (descriptor-fixnum depthoid)
                (descriptor-fixnum nuntagged)))
    (setf (gethash (descriptor-bits result) *cold-layout-names*) name)

    result))

(defun initialize-layouts ()

  (clrhash *cold-layouts*)

  ;; We initially create the layout of LAYOUT itself with NIL as the LAYOUT and
  ;; #() as INHERITS,
  (setq *layout-layout* *nil-descriptor*)
  (let ((xlayout-layout (find-layout 'layout)))
    (aver (= 0 (layout-n-untagged-slots xlayout-layout)))
    (setq *layout-layout*
          (make-cold-layout 'layout
                            (number-to-core target-layout-length)
                            (vector-in-core)
                            (number-to-core (layout-depthoid xlayout-layout))
                            (number-to-core 0)))
  (write-wordindexed
   *layout-layout* sb!vm:instance-slots-offset *layout-layout*)

  ;; Then we create the layouts that we'll need to make a correct INHERITS
  ;; vector for the layout of LAYOUT itself..
  ;;
  ;; FIXME: The various LENGTH and DEPTHOID numbers should be taken from
  ;; the compiler's tables, not set by hand.
  (let* ((t-layout
          (make-cold-layout 't
                            (number-to-core 0)
                            (vector-in-core)
                            (number-to-core 0)
                            (number-to-core 0)))
         (so-layout
          (make-cold-layout 'structure-object
                            (number-to-core 1)
                            (vector-in-core t-layout)
                            (number-to-core 1)
                            (number-to-core 0)))
         (bso-layout
          (make-cold-layout 'structure!object
                            (number-to-core 1)
                            (vector-in-core t-layout so-layout)
                            (number-to-core 2)
                            (number-to-core 0)))
         (layout-inherits (vector-in-core t-layout
                                          so-layout
                                          bso-layout)))

    ;; ..and return to backpatch the layout of LAYOUT.
    (setf (fourth (gethash 'layout *cold-layouts*))
          (listify-cold-inherits layout-inherits))
    (cold-set-layout-slot *layout-layout* 'inherits layout-inherits))))

;;;; interning symbols in the cold image

;;; In order to avoid having to know about the package format, we
;;; build a data structure in *COLD-PACKAGE-SYMBOLS* that holds all
;;; interned symbols along with info about their packages. The data
;;; structure is a list of sublists, where the sublists have the
;;; following format:
;;;   (<make-package-arglist>
;;;    <internal-symbols>
;;;    <external-symbols>
;;;    <imported-internal-symbols>
;;;    <imported-external-symbols>
;;;    <shadowing-symbols>
;;;    <package-documentation>)
;;;
;;; KLUDGE: It would be nice to implement the sublists as instances of
;;; a DEFSTRUCT (:TYPE LIST). (They'd still be lists, but at least we'd be
;;; using mnemonically-named operators to access them, instead of trying
;;; to remember what THIRD and FIFTH mean, and hoping that we never
;;; need to change the list layout..) -- WHN 19990825

;;; an alist from packages to lists of that package's symbols to be dumped
(defvar *cold-package-symbols*)
(declaim (type list *cold-package-symbols*))

;;; a map from descriptors to symbols, so that we can back up. The key
;;; is the address in the target core.
(defvar *cold-symbols*)
(declaim (type hash-table *cold-symbols*))

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

;;; Return a handle on an interned symbol. If necessary allocate the
;;; symbol and record which package the symbol was referenced in. When
;;; we allocate the symbol, make sure we record a reference to the
;;; symbol in the home package so that the package gets set.
(defun cold-intern (symbol
                    &key
                    (package (symbol-package-for-target-symbol symbol))
                    (gspace *dynamic*))

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

  (let (;; Information about each cold-interned symbol is stored
        ;; in COLD-INTERN-INFO.
        ;;   (CAR COLD-INTERN-INFO) = descriptor of symbol
        ;;   (CDR COLD-INTERN-INFO) = list of packages, other than symbol's
        ;;                            own package, referring to symbol
        ;; (*COLD-PACKAGE-SYMBOLS* and *COLD-SYMBOLS* store basically the
        ;; same information, but with the mapping running the opposite way.)
        (cold-intern-info (get symbol 'cold-intern-info)))
    (unless cold-intern-info
      (cond ((eq (symbol-package-for-target-symbol symbol) package)
             (let ((handle (allocate-symbol (symbol-name symbol) :gspace gspace)))
               (setf (gethash (descriptor-bits handle) *cold-symbols*) symbol)
               (when (eq package *keyword-package*)
                 (cold-set handle handle))
               (setq cold-intern-info
                     (setf (get symbol 'cold-intern-info) (cons handle nil)))))
            (t
             (cold-intern symbol)
             (setq cold-intern-info (get symbol 'cold-intern-info)))))
    (unless (or (null package)
                (member package (cdr cold-intern-info)))
      (push package (cdr cold-intern-info))
      (let* ((old-cps-entry (assoc package *cold-package-symbols*))
             (cps-entry (or old-cps-entry
                            (car (push (list package)
                                       *cold-package-symbols*)))))
        (unless old-cps-entry
          (/show "created *COLD-PACKAGE-SYMBOLS* entry for" package symbol))
        (push symbol (rest cps-entry))))
    (car cold-intern-info)))

;;; Construct and return a value for use as *NIL-DESCRIPTOR*.
(defun make-nil-descriptor ()
  (let* ((des (allocate-unboxed-object
               *static*
               sb!vm:n-word-bits
               sb!vm:symbol-size
               0))
         (result (make-descriptor (descriptor-high des)
                                  (+ (descriptor-low des)
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
                       (+ 2 sb!vm:symbol-value-slot)
                       result)
    (write-wordindexed des
                       (+ 1 sb!vm:symbol-plist-slot)
                       result)
    (write-wordindexed des
                       (+ 1 sb!vm:symbol-name-slot)
                       ;; This is *DYNAMIC*, and DES is *STATIC*,
                       ;; because that's the way CMU CL did it; I'm
                       ;; not sure whether there's an underlying
                       ;; reason. -- WHN 1990826
                       (base-string-to-core "NIL" *dynamic*))
    (write-wordindexed des
                       (+ 1 sb!vm:symbol-package-slot)
                       result)
    (setf (get nil 'cold-intern-info)
          (cons result nil))
    (cold-intern nil)
    result))

;;; Since the initial symbols must be allocated before we can intern
;;; anything else, we intern those here. We also set the value of T.
(defun initialize-non-nil-symbols ()
  #!+sb-doc
  "Initialize the cold load symbol-hacking data structures."
  ;; Intern the others.
  (dolist (symbol sb!vm:*static-symbols*)
    (let* ((des (cold-intern symbol :gspace *static*))
           (offset-wanted (sb!vm:static-symbol-offset symbol))
           (offset-found (- (descriptor-low des)
                            (descriptor-low *nil-descriptor*))))
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
  (let ((p-a-a-symbol (cold-intern 'sb!kernel:*pseudo-atomic-bits*
                                   :gspace *static*)))
    (cold-set p-a-a-symbol (make-fixnum-descriptor 0))))

;;; a helper function for FINISH-SYMBOLS: Return a cold alist suitable
;;; to be stored in *!INITIAL-LAYOUTS*.
(defun cold-list-all-layouts ()
  (let ((layouts nil)
        (result *nil-descriptor*))
    (maphash (lambda (key stuff)
               (push (cons key (first stuff)) layouts))
             *cold-layouts*)
    (flet ((sorter (x y)
             (let ((xpn (package-name (symbol-package-for-target-symbol x)))
                   (ypn (package-name (symbol-package-for-target-symbol y))))
               (cond
                 ((string= x y) (string< xpn ypn))
                 (t (string< x y))))))
      (setq layouts (sort layouts #'sorter :key #'car)))
    (dolist (layout layouts result)
      (cold-push (cold-cons (cold-intern (car layout)) (cdr layout))
                 result))))

;;; Establish initial values for magic symbols.
;;;
;;; Scan over all the symbols referenced in each package in
;;; *COLD-PACKAGE-SYMBOLS* making that for each one there's an
;;; appropriate entry in the *!INITIAL-SYMBOLS* data structure to
;;; intern it.
(defun finish-symbols ()

  ;; I think the point of setting these functions into SYMBOL-VALUEs
  ;; here, instead of using SYMBOL-FUNCTION, is that in CMU CL
  ;; SYMBOL-FUNCTION reduces to FDEFINITION, which is a pretty
  ;; hairy operation (involving globaldb.lisp etc.) which we don't
  ;; want to invoke early in cold init. -- WHN 2001-12-05
  ;;
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

  (/show "dumping packages" (mapcar #'car *cold-package-symbols*))
  (let ((initial-symbols *nil-descriptor*))
    (dolist (cold-package-symbols-entry *cold-package-symbols*)
      (let* ((cold-package (car cold-package-symbols-entry))
             (symbols (cdr cold-package-symbols-entry))
             (shadows (package-shadowing-symbols cold-package))
             (documentation (base-string-to-core
                             ;; KLUDGE: NIL punned as 0-length string.
                             (unless
                                 ;; don't propagate the arbitrary
                                 ;; docstring from host packages into
                                 ;; the core
                                 (or (eql cold-package *cl-package*)
                                     (eql cold-package *keyword-package*))
                               (documentation cold-package t))))
             (internal-count 0)
             (external-count 0)
             (internal *nil-descriptor*)
             (external *nil-descriptor*)
             (imported-internal *nil-descriptor*)
             (imported-external *nil-descriptor*)
             (shadowing *nil-descriptor*))
        (declare (type package cold-package)) ; i.e. not a target descriptor
        (/show "dumping" cold-package symbols)

        ;; FIXME: Add assertions here to make sure that inappropriate stuff
        ;; isn't being dumped:
        ;;   * the CL-USER package
        ;;   * the SB-COLD package
        ;;   * any internal symbols in the CL package
        ;;   * basically any package other than CL, KEYWORD, or the packages
        ;;     in package-data-list.lisp-expr
        ;; and that the structure of the KEYWORD package (e.g. whether
        ;; any symbols are internal to it) matches what we want in the
        ;; target SBCL.

        ;; FIXME: It seems possible that by looking at the contents of
        ;; packages in the target SBCL we could find which symbols in
        ;; package-data-lisp.lisp-expr are now obsolete. (If I
        ;; understand correctly, only symbols which actually have
        ;; definitions or which are otherwise referred to actually end
        ;; up in the target packages.)

        (dolist (symbol symbols)
          (let ((handle (car (get symbol 'cold-intern-info)))
                (imported-p (not (eq (symbol-package-for-target-symbol symbol)
                                     cold-package))))
            (multiple-value-bind (found where)
                (find-symbol (symbol-name symbol) cold-package)
              (unless (and where (eq found symbol))
                (error "The symbol ~S is not available in ~S."
                       symbol
                       cold-package))
              (when (memq symbol shadows)
                (cold-push handle shadowing))
              (case where
                (:internal (if imported-p
                               (cold-push handle imported-internal)
                               (progn
                                 (cold-push handle internal)
                                 (incf internal-count))))
                (:external (if imported-p
                               (cold-push handle imported-external)
                               (progn
                                 (cold-push handle external)
                                 (incf external-count))))))))
        (let ((r *nil-descriptor*))
          (cold-push documentation r)
          (cold-push shadowing r)
          (cold-push imported-external r)
          (cold-push imported-internal r)
          (cold-push external r)
          (cold-push internal r)
          (cold-push (make-make-package-args cold-package
                                             internal-count
                                             external-count)
                     r)
          ;; FIXME: It would be more space-efficient to use vectors
          ;; instead of lists here, and space-efficiency here would be
          ;; nice, since it would reduce the peak memory usage in
          ;; genesis and cold init.
          (cold-push r initial-symbols))))
    (cold-set '*!initial-symbols* initial-symbols))

  (cold-set '*!initial-fdefn-objects* (list-all-fdefn-objects))

  (cold-set '*!reversed-cold-toplevels* *current-reversed-cold-toplevels*)
  (cold-set '*!initial-debug-sources* *current-debug-sources*)

  #!+(or x86 x86-64)
  (progn
    (cold-set 'sb!vm::*fp-constant-0d0* (number-to-core 0d0))
    (cold-set 'sb!vm::*fp-constant-1d0* (number-to-core 1d0))
    (cold-set 'sb!vm::*fp-constant-0f0* (number-to-core 0f0))
    (cold-set 'sb!vm::*fp-constant-1f0* (number-to-core 1f0))))

;;; Make a cold list that can be used as the arg list to MAKE-PACKAGE in
;;; order to make a package that is similar to PKG.
(defun make-make-package-args (pkg internal-count external-count)
  (let* ((use *nil-descriptor*)
         (cold-nicknames *nil-descriptor*)
         (res *nil-descriptor*))
    (dolist (u (package-use-list pkg))
      (when (assoc u *cold-package-symbols*)
        (cold-push (base-string-to-core (package-name u)) use)))
    (let* ((pkg-name (package-name pkg))
           ;; Make the package nickname lists for the standard packages
           ;; be the minimum specified by ANSI, regardless of what value
           ;; the cross-compilation host happens to use.
           (warm-nicknames (cond ((string= pkg-name "COMMON-LISP")
                                  '("CL"))
                                 ((string= pkg-name "COMMON-LISP-USER")
                                  '("CL-USER"))
                                 ((string= pkg-name "KEYWORD")
                                  '())
                                 ;; For packages other than the
                                 ;; standard packages, the nickname
                                 ;; list was specified by our package
                                 ;; setup code, not by properties of
                                 ;; what cross-compilation host we
                                 ;; happened to use, and we can just
                                 ;; propagate it into the target.
                                 (t
                                  (package-nicknames pkg)))))
      (dolist (warm-nickname warm-nicknames)
        (cold-push (base-string-to-core warm-nickname) cold-nicknames)))

    ;; INTERNAL-COUNT and EXTERNAL-COUNT are the number of symbols that
    ;; the package contains in the core. We arrange for the package
    ;; symbol tables to be created somewhat larger so that they don't
    ;; need to be rehashed so easily when additional symbols are
    ;; interned during the warm build.
    (cold-push (number-to-core (truncate internal-count 0.8)) res)
    (cold-push (cold-intern :internal-symbols) res)
    (cold-push (number-to-core (truncate external-count 0.8)) res)
    (cold-push (cold-intern :external-symbols) res)

    (cold-push cold-nicknames res)
    (cold-push (cold-intern :nicknames) res)

    (cold-push use res)
    (cold-push (cold-intern :use) res)

    (cold-push (base-string-to-core (package-name pkg)) res)
    res))

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
(defun cold-null (des)
  (= (descriptor-bits des)
     (descriptor-bits *nil-descriptor*)))

;;; Given a cold representation of a function name, return a warm
;;; representation.
(declaim (ftype (function ((or descriptor symbol)) (or symbol list)) warm-fun-name))
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
  (declare (type (or descriptor symbol) cold-name))
  (/show0 "/cold-fdefinition-object")
  (let ((warm-name (warm-fun-name cold-name)))
    (or (gethash warm-name *cold-fdefn-objects*)
        (let ((fdefn (allocate-boxed-object (or *cold-fdefn-gspace* *dynamic*)
                                            (1- sb!vm:fdefn-size)
                                            sb!vm:other-pointer-lowtag)))

          (setf (gethash warm-name *cold-fdefn-objects*) fdefn)
          (write-memory fdefn (make-other-immediate-descriptor
                               (1- sb!vm:fdefn-size) sb!vm:fdefn-widetag))
          (write-wordindexed fdefn sb!vm:fdefn-name-slot cold-name)
          (unless leave-fn-raw
            (write-wordindexed fdefn sb!vm:fdefn-fun-slot
                               *nil-descriptor*)
            (write-wordindexed fdefn
                               sb!vm:fdefn-raw-addr-slot
                               (make-random-descriptor
                                (cold-foreign-symbol-address "undefined_tramp"))))
          fdefn))))

;;; Handle the at-cold-init-time, fset-for-static-linkage operation
;;; requested by FOP-FSET.
(defun static-fset (cold-name defn)
  (declare (type (or descriptor symbol) cold-name))
  (let ((fdefn (cold-fdefinition-object cold-name t))
        (type (logand (descriptor-low (read-memory defn)) sb!vm:widetag-mask)))
    (write-wordindexed fdefn sb!vm:fdefn-fun-slot defn)
    (write-wordindexed fdefn
                       sb!vm:fdefn-raw-addr-slot
                       (ecase type
                         (#.sb!vm:simple-fun-header-widetag
                          (/show0 "static-fset (simple-fun)")
                          #!+sparc
                          defn
                          #!-sparc
                          (make-random-descriptor
                           (+ (logandc2 (descriptor-bits defn)
                                        sb!vm:lowtag-mask)
                              (ash sb!vm:simple-fun-code-offset
                                   sb!vm:word-shift))))
                         (#.sb!vm:closure-header-widetag
                          (/show0 "/static-fset (closure)")
                          (make-random-descriptor
                           (cold-foreign-symbol-address "closure_tramp")))))
    fdefn))

(defun initialize-static-fns ()
  (let ((*cold-fdefn-gspace* *static*))
    (dolist (sym sb!vm:*static-funs*)
      (let* ((fdefn (cold-fdefinition-object (cold-intern sym)))
             (offset (- (+ (- (descriptor-low fdefn)
                              sb!vm:other-pointer-lowtag)
                           (* sb!vm:fdefn-raw-addr-slot sb!vm:n-word-bytes))
                        (descriptor-low *nil-descriptor*)))
             (desired (sb!vm:static-fun-offset sym)))
        (unless (= offset desired)
          ;; FIXME: should be fatal
          (error "Offset from FDEFN ~S to ~S is ~W, not ~W."
                 sym nil offset desired))))))

(defun list-all-fdefn-objects ()
  (let ((fdefns nil)
        (result *nil-descriptor*))
    (maphash (lambda (key value)
               (push (cons key value) fdefns))
             *cold-fdefn-objects*)
    (flet ((sorter (x y)
             (let* ((xbn (fun-name-block-name x))
                    (ybn (fun-name-block-name y))
                    (xbnpn (package-name (symbol-package-for-target-symbol xbn)))
                    (ybnpn (package-name (symbol-package-for-target-symbol ybn))))
               (cond
                 ((eql xbn ybn) (consp x))
                 ((string= xbn ybn) (string< xbnpn ybnpn))
                 (t (string< xbn ybn))))))
      (setq fdefns (sort fdefns #'sorter :key #'car)))
    (dolist (fdefn fdefns result)
      (cold-push (cdr fdefn) result))))

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
      (:hppa
       (ecase kind
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

;;; *COLD-FOREIGN-SYMBOL-TABLE* becomes *!INITIAL-FOREIGN-SYMBOLS* in
;;; the core. When the core is loaded, !LOADER-COLD-INIT uses this to
;;; create *STATIC-FOREIGN-SYMBOLS*, which the code in
;;; target-load.lisp refers to.
(defun foreign-symbols-to-core ()
  (let ((symbols nil)
        (result *nil-descriptor*))
    (maphash (lambda (symbol value)
               (push (cons symbol value) symbols))
             *cold-foreign-symbol-table*)
    (setq symbols (sort symbols #'string< :key #'car))
    (dolist (symbol symbols)
      (cold-push (cold-cons (base-string-to-core (car symbol))
                            (number-to-core (cdr symbol)))
                 result))
    (cold-set (cold-intern 'sb!kernel:*!initial-foreign-symbols*) result))
  (let ((result *nil-descriptor*))
    (dolist (rtn (sort (copy-list *cold-assembler-routines*) #'string< :key #'car))
      (cold-push (cold-cons (cold-intern (car rtn))
                            (number-to-core (cdr rtn)))
                 result))
    (cold-set (cold-intern '*!initial-assembler-routines*) result)))


;;;; general machinery for cold-loading FASL files

;;; FOP functions for cold loading
(defvar *cold-fop-funs*
  ;; We start out with a copy of the ordinary *FOP-FUNS*. The ones
  ;; which aren't appropriate for cold load will be destructively
  ;; modified.
  (copy-seq *fop-funs*))

;;; Cause a fop to have a special definition for cold load.
;;;
;;; This is similar to DEFINE-FOP, but unlike DEFINE-FOP, this version
;;;   (1) looks up the code for this name (created by a previous
;;        DEFINE-FOP) instead of creating a code, and
;;;   (2) stores its definition in the *COLD-FOP-FUNS* vector,
;;;       instead of storing in the *FOP-FUNS* vector.
(defmacro define-cold-fop ((name &key (pushp t) (stackp t)) &rest forms)
  (aver (member pushp '(nil t)))
  (aver (member stackp '(nil t)))
  (let ((code (get name 'fop-code))
        (fname (symbolicate "COLD-" name)))
    (unless code
      (error "~S is not a defined FOP." name))
    `(progn
       (defun ,fname ()
         ,@(if stackp
               `((with-fop-stack ,pushp ,@forms))
               forms))
       (setf (svref *cold-fop-funs* ,code) #',fname))))

(defmacro clone-cold-fop ((name &key (pushp t) (stackp t))
                          (small-name)
                          &rest forms)
  (aver (member pushp '(nil t)))
  (aver (member stackp '(nil t)))
  `(progn
    (macrolet ((clone-arg () '(read-word-arg)))
      (define-cold-fop (,name :pushp ,pushp :stackp ,stackp) ,@forms))
    (macrolet ((clone-arg () '(read-byte-arg)))
      (define-cold-fop (,small-name :pushp ,pushp :stackp ,stackp) ,@forms))))

;;; Cause a fop to be undefined in cold load.
(defmacro not-cold-fop (name)
  `(define-cold-fop (,name)
     (error "The fop ~S is not supported in cold load." ',name)))

;;; COLD-LOAD loads stuff into the core image being built by calling
;;; LOAD-AS-FASL with the fop function table rebound to a table of cold
;;; loading functions.
(defun cold-load (filename)
  #!+sb-doc
  "Load the file named by FILENAME into the cold load image being built."
  (let* ((*fop-funs* *cold-fop-funs*)
         (*cold-load-filename* (etypecase filename
                                 (string filename)
                                 (pathname (namestring filename)))))
    (with-open-file (s filename :element-type '(unsigned-byte 8))
      (load-as-fasl s nil nil))))

;;;; miscellaneous cold fops

(define-cold-fop (fop-misc-trap) *unbound-marker*)

(define-cold-fop (fop-short-character)
  (make-character-descriptor (read-byte-arg)))

(define-cold-fop (fop-empty-list) nil)
(define-cold-fop (fop-truth) t)

(clone-cold-fop (fop-struct)
                (fop-small-struct)
  (let* ((size (clone-arg))
         (result (allocate-boxed-object *dynamic*
                                        (1+ size)
                                        sb!vm:instance-pointer-lowtag))
         (layout (pop-stack))
         (nuntagged
          (descriptor-fixnum
           (read-wordindexed
            layout
            (+ sb!vm:instance-slots-offset
               (target-layout-index 'n-untagged-slots)))))
         (ntagged (- size nuntagged)))
    (write-memory result (make-other-immediate-descriptor
                          size sb!vm:instance-header-widetag))
    (write-wordindexed result sb!vm:instance-slots-offset layout)
    (do ((index 1 (1+ index)))
        ((eql index size))
      (declare (fixnum index))
      (write-wordindexed result
                         (+ index sb!vm:instance-slots-offset)
                         (if (>= index ntagged)
                             (descriptor-word-sized-integer (pop-stack))
                             (pop-stack))))
    result))

(define-cold-fop (fop-layout)
  (let* ((nuntagged-des (pop-stack))
         (length-des (pop-stack))
         (depthoid-des (pop-stack))
         (cold-inherits (pop-stack))
         (name (pop-stack))
         (old (gethash name *cold-layouts*)))
    (declare (type descriptor length-des depthoid-des cold-inherits))
    (declare (type symbol name))
    ;; If a layout of this name has been defined already
    (if old
      ;; Enforce consistency between the previous definition and the
      ;; current definition, then return the previous definition.
      (destructuring-bind
          ;; FIXME: This would be more maintainable if we used
          ;; DEFSTRUCT (:TYPE LIST) to define COLD-LAYOUT. -- WHN 19990825
          (old-layout-descriptor
           old-name
           old-length
           old-inherits-list
           old-depthoid
           old-nuntagged)
          old
        (declare (type descriptor old-layout-descriptor))
        (declare (type index old-length old-nuntagged))
        (declare (type fixnum old-depthoid))
        (declare (type list old-inherits-list))
        (aver (eq name old-name))
        (let ((length (descriptor-fixnum length-des))
              (inherits-list (listify-cold-inherits cold-inherits))
              (depthoid (descriptor-fixnum depthoid-des))
              (nuntagged (descriptor-fixnum nuntagged-des)))
          (unless (= length old-length)
            (error "cold loading a reference to class ~S when the compile~%~
                    time length was ~S and current length is ~S"
                   name
                   length
                   old-length))
          (unless (equal inherits-list old-inherits-list)
            (error "cold loading a reference to class ~S when the compile~%~
                    time inherits were ~S~%~
                    and current inherits are ~S"
                   name
                   inherits-list
                   old-inherits-list))
          (unless (= depthoid old-depthoid)
            (error "cold loading a reference to class ~S when the compile~%~
                    time inheritance depthoid was ~S and current inheritance~%~
                    depthoid is ~S"
                   name
                   depthoid
                   old-depthoid))
          (unless (= nuntagged old-nuntagged)
            (error "cold loading a reference to class ~S when the compile~%~
                    time number of untagged slots was ~S and is currently ~S"
                   name
                   nuntagged
                   old-nuntagged)))
        old-layout-descriptor)
      ;; Make a new definition from scratch.
      (make-cold-layout name length-des cold-inherits depthoid-des
                        nuntagged-des))))

;;;; cold fops for loading symbols

;;; Load a symbol SIZE characters long from *FASL-INPUT-STREAM* and
;;; intern that symbol in PACKAGE.
(defun cold-load-symbol (size package)
  (let ((string (make-string size)))
    (read-string-as-bytes *fasl-input-stream* string)
    (intern string package)))

(macrolet ((frob (name pname-len package-len)
             `(define-cold-fop (,name)
                (let ((index (read-arg ,package-len)))
                  (push-fop-table
                   (cold-load-symbol (read-arg ,pname-len)
                                     (svref *current-fop-table* index)))))))
  (frob fop-symbol-in-package-save #.sb!vm:n-word-bytes #.sb!vm:n-word-bytes)
  (frob fop-small-symbol-in-package-save 1 #.sb!vm:n-word-bytes)
  (frob fop-symbol-in-byte-package-save #.sb!vm:n-word-bytes 1)
  (frob fop-small-symbol-in-byte-package-save 1 1))

(clone-cold-fop (fop-lisp-symbol-save)
                (fop-lisp-small-symbol-save)
  (push-fop-table (cold-load-symbol (clone-arg) *cl-package*)))

(clone-cold-fop (fop-keyword-symbol-save)
                (fop-keyword-small-symbol-save)
  (push-fop-table (cold-load-symbol (clone-arg) *keyword-package*)))

(clone-cold-fop (fop-uninterned-symbol-save)
                (fop-uninterned-small-symbol-save)
  (let* ((size (clone-arg))
         (name (make-string size)))
    (read-string-as-bytes *fasl-input-stream* name)
    (let ((symbol-des (allocate-symbol name)))
      (push-fop-table symbol-des))))

;;;; cold fops for loading packages

(clone-cold-fop (fop-named-package-save :stackp nil)
                (fop-small-named-package-save)
  (let* ((size (clone-arg))
         (name (make-string size)))
    (read-string-as-bytes *fasl-input-stream* name)
    (push-fop-table (find-package name))))

;;;; cold fops for loading lists

;;; Make a list of the top LENGTH things on the fop stack. The last
;;; cdr of the list is set to LAST.
(defmacro cold-stack-list (length last)
  `(do* ((index ,length (1- index))
         (result ,last (cold-cons (pop-stack) result)))
        ((= index 0) result)
     (declare (fixnum index))))

(define-cold-fop (fop-list)
  (cold-stack-list (read-byte-arg) *nil-descriptor*))
(define-cold-fop (fop-list*)
  (cold-stack-list (read-byte-arg) (pop-stack)))
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

(clone-cold-fop (fop-base-string)
                (fop-small-base-string)
  (let* ((len (clone-arg))
         (string (make-string len)))
    (read-string-as-bytes *fasl-input-stream* string)
    (base-string-to-core string)))

#!+sb-unicode
(clone-cold-fop (fop-character-string)
                (fop-small-character-string)
  (bug "CHARACTER-STRING dumped by cross-compiler."))

(clone-cold-fop (fop-vector)
                (fop-small-vector)
  (let* ((size (clone-arg))
         (result (allocate-vector-object *dynamic*
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

(define-cold-fop (fop-int-vector)
  (let* ((len (read-word-arg))
         (sizebits (read-byte-arg))
         (type (case sizebits
                 (0 sb!vm:simple-array-nil-widetag)
                 (1 sb!vm:simple-bit-vector-widetag)
                 (2 sb!vm:simple-array-unsigned-byte-2-widetag)
                 (4 sb!vm:simple-array-unsigned-byte-4-widetag)
                 (7 (prog1 sb!vm:simple-array-unsigned-byte-7-widetag
                      (setf sizebits 8)))
                 (8 sb!vm:simple-array-unsigned-byte-8-widetag)
                 (15 (prog1 sb!vm:simple-array-unsigned-byte-15-widetag
                       (setf sizebits 16)))
                 (16 sb!vm:simple-array-unsigned-byte-16-widetag)
                 (31 (prog1 sb!vm:simple-array-unsigned-byte-31-widetag
                       (setf sizebits 32)))
                 (32 sb!vm:simple-array-unsigned-byte-32-widetag)
                 #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
                 (63 (prog1 sb!vm:simple-array-unsigned-byte-63-widetag
                       (setf sizebits 64)))
                 #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
                 (64 sb!vm:simple-array-unsigned-byte-64-widetag)
                 (t (error "losing element size: ~W" sizebits))))
         (result (allocate-vector-object *dynamic* sizebits len type))
         (start (+ (descriptor-byte-offset result)
                   (ash sb!vm:vector-data-offset sb!vm:word-shift)))
         (end (+ start
                 (ceiling (* len sizebits)
                          sb!vm:n-byte-bits))))
    (read-bigvec-as-sequence-or-die (descriptor-bytes result)
                                    *fasl-input-stream*
                                    :start start
                                    :end end)
    result))

(define-cold-fop (fop-single-float-vector)
  (let* ((len (read-word-arg))
         (result (allocate-vector-object
                  *dynamic*
                  sb!vm:n-word-bits
                  len
                  sb!vm:simple-array-single-float-widetag))
         (start (+ (descriptor-byte-offset result)
                   (ash sb!vm:vector-data-offset sb!vm:word-shift)))
         (end (+ start (* len 4))))
    (read-bigvec-as-sequence-or-die (descriptor-bytes result)
                                    *fasl-input-stream*
                                    :start start
                                    :end end)
    result))

(not-cold-fop fop-double-float-vector)
#!+long-float (not-cold-fop fop-long-float-vector)
(not-cold-fop fop-complex-single-float-vector)
(not-cold-fop fop-complex-double-float-vector)
#!+long-float (not-cold-fop fop-complex-long-float-vector)

(define-cold-fop (fop-array)
  (let* ((rank (read-word-arg))
         (data-vector (pop-stack))
         (result (allocate-boxed-object *dynamic*
                                        (+ sb!vm:array-dimensions-offset rank)
                                        sb!vm:other-pointer-lowtag)))
    (write-memory result
                  (make-other-immediate-descriptor rank
                                                   sb!vm:simple-array-widetag))
    (write-wordindexed result sb!vm:array-fill-pointer-slot *nil-descriptor*)
    (write-wordindexed result sb!vm:array-data-slot data-vector)
    (write-wordindexed result sb!vm:array-displacement-slot *nil-descriptor*)
    (write-wordindexed result sb!vm:array-displaced-p-slot *nil-descriptor*)
    (write-wordindexed result sb!vm:array-displaced-from-slot *nil-descriptor*)
    (let ((total-elements 1))
      (dotimes (axis rank)
        (let ((dim (pop-stack)))
          (unless (or (= (descriptor-lowtag dim) sb!vm:even-fixnum-lowtag)
                      (= (descriptor-lowtag dim) sb!vm:odd-fixnum-lowtag))
            (error "non-fixnum dimension? (~S)" dim))
          (setf total-elements
                (* total-elements
                   (logior (ash (descriptor-high dim)
                                (- descriptor-low-bits
                                   (1- sb!vm:n-lowtag-bits)))
                           (ash (descriptor-low dim)
                                (- 1 sb!vm:n-lowtag-bits)))))
          (write-wordindexed result
                             (+ sb!vm:array-dimensions-offset axis)
                             dim)))
      (write-wordindexed result
                         sb!vm:array-elements-slot
                         (make-fixnum-descriptor total-elements)))
    result))


;;;; cold fops for loading numbers

(defmacro define-cold-number-fop (fop)
  `(define-cold-fop (,fop :stackp nil)
     ;; Invoke the ordinary warm version of this fop to push the
     ;; number.
     (,fop)
     ;; Replace the warm fop result with the cold image of the warm
     ;; fop result.
     (with-fop-stack t
       (let ((number (pop-stack)))
         (number-to-core number)))))

(define-cold-number-fop fop-single-float)
(define-cold-number-fop fop-double-float)
(define-cold-number-fop fop-integer)
(define-cold-number-fop fop-small-integer)
(define-cold-number-fop fop-word-integer)
(define-cold-number-fop fop-byte-integer)
(define-cold-number-fop fop-complex-single-float)
(define-cold-number-fop fop-complex-double-float)

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

(define-cold-fop (fop-funcall)
  (unless (= (read-byte-arg) 0)
    (error "You can't FOP-FUNCALL arbitrary stuff in cold load."))
  (let ((counter *load-time-value-counter*))
    (cold-push (cold-cons
                (cold-intern :load-time-value)
                (cold-cons
                 (pop-stack)
                 (cold-cons
                  (number-to-core counter)
                  *nil-descriptor*)))
               *current-reversed-cold-toplevels*)
    (setf *load-time-value-counter* (1+ counter))
    (make-descriptor 0 0 :load-time-value counter)))

(defun finalize-load-time-value-noise ()
  (cold-set (cold-intern '*!load-time-values*)
            (allocate-vector-object *dynamic*
                                    sb!vm:n-word-bits
                                    *load-time-value-counter*
                                    sb!vm:simple-vector-widetag)))

(define-cold-fop (fop-funcall-for-effect :pushp nil)
  (if (= (read-byte-arg) 0)
      (cold-push (pop-stack)
                 *current-reversed-cold-toplevels*)
      (error "You can't FOP-FUNCALL arbitrary stuff in cold load.")))

;;;; cold fops for fixing up circularities

(define-cold-fop (fop-rplaca :pushp nil)
  (let ((obj (svref *current-fop-table* (read-word-arg)))
        (idx (read-word-arg)))
    (write-memory (cold-nthcdr idx obj) (pop-stack))))

(define-cold-fop (fop-rplacd :pushp nil)
  (let ((obj (svref *current-fop-table* (read-word-arg)))
        (idx (read-word-arg)))
    (write-wordindexed (cold-nthcdr idx obj) 1 (pop-stack))))

(define-cold-fop (fop-svset :pushp nil)
  (let ((obj (svref *current-fop-table* (read-word-arg)))
        (idx (read-word-arg)))
    (write-wordindexed obj
                   (+ idx
                      (ecase (descriptor-lowtag obj)
                        (#.sb!vm:instance-pointer-lowtag 1)
                        (#.sb!vm:other-pointer-lowtag 2)))
                   (pop-stack))))

(define-cold-fop (fop-structset :pushp nil)
  (let ((obj (svref *current-fop-table* (read-word-arg)))
        (idx (read-word-arg)))
    (write-wordindexed obj (1+ idx) (pop-stack))))

;;; In the original CMUCL code, this actually explicitly declared PUSHP
;;; to be T, even though that's what it defaults to in DEFINE-COLD-FOP.
(define-cold-fop (fop-nthcdr)
  (cold-nthcdr (read-word-arg) (pop-stack)))

(defun cold-nthcdr (index obj)
  (dotimes (i index)
    (setq obj (read-wordindexed obj 1)))
  obj)

;;;; cold fops for loading code objects and functions

;;; the names of things which have had COLD-FSET used on them already
;;; (used to make sure that we don't try to statically link a name to
;;; more than one definition)
(defparameter *cold-fset-warm-names*
  ;; This can't be an EQL hash table because names can be conses, e.g.
  ;; (SETF CAR).
  (make-hash-table :test 'equal))

(define-cold-fop (fop-fset :pushp nil)
  (let* ((fn (pop-stack))
         (cold-name (pop-stack))
         (warm-name (warm-fun-name cold-name)))
    (if (gethash warm-name *cold-fset-warm-names*)
        (error "duplicate COLD-FSET for ~S" warm-name)
        (setf (gethash warm-name *cold-fset-warm-names*) t))
    (static-fset cold-name fn)))

(define-cold-fop (fop-note-debug-source :pushp nil)
  (let ((debug-source (pop-stack)))
    (cold-push debug-source *current-debug-sources*)))

(define-cold-fop (fop-fdefinition)
  (cold-fdefinition-object (pop-stack)))

(define-cold-fop (fop-sanctify-for-execution)
  (pop-stack))

;;; Setting this variable shows what code looks like before any
;;; fixups (or function headers) are applied.
#!+sb-show (defvar *show-pre-fixup-code-p* nil)

;;; FIXME: The logic here should be converted into a function
;;; COLD-CODE-FOP-GUTS (NCONST CODE-SIZE) called by DEFINE-COLD-FOP
;;; FOP-CODE and DEFINE-COLD-FOP FOP-SMALL-CODE, so that
;;; variable-capture nastiness like (LET ((NCONST ,NCONST) ..) ..)
;;; doesn't keep me awake at night.
(defmacro define-cold-code-fop (name nconst code-size)
  `(define-cold-fop (,name)
     (let* ((nconst ,nconst)
            (code-size ,code-size)
            (raw-header-n-words (+ sb!vm:code-trace-table-offset-slot nconst))
            (header-n-words
             ;; Note: we round the number of constants up to ensure
             ;; that the code vector will be properly aligned.
             (round-up raw-header-n-words 2))
            (des (allocate-cold-descriptor *dynamic*
                                           (+ (ash header-n-words
                                                   sb!vm:word-shift)
                                              code-size)
                                           sb!vm:other-pointer-lowtag)))
       (write-memory des
                     (make-other-immediate-descriptor
                      header-n-words sb!vm:code-header-widetag))
       (write-wordindexed des
                          sb!vm:code-code-size-slot
                          (make-fixnum-descriptor
                           (ash (+ code-size (1- (ash 1 sb!vm:word-shift)))
                                (- sb!vm:word-shift))))
       (write-wordindexed des sb!vm:code-entry-points-slot *nil-descriptor*)
       (write-wordindexed des sb!vm:code-debug-info-slot (pop-stack))
       (when (oddp raw-header-n-words)
         (write-wordindexed des
                            raw-header-n-words
                            (make-random-descriptor 0)))
       (do ((index (1- raw-header-n-words) (1- index)))
           ((< index sb!vm:code-trace-table-offset-slot))
         (write-wordindexed des index (pop-stack)))
       (let* ((start (+ (descriptor-byte-offset des)
                        (ash header-n-words sb!vm:word-shift)))
              (end (+ start code-size)))
         (read-bigvec-as-sequence-or-die (descriptor-bytes des)
                                         *fasl-input-stream*
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

(define-cold-code-fop fop-code (read-word-arg) (read-word-arg))

(define-cold-code-fop fop-small-code (read-byte-arg) (read-halfword-arg))

(clone-cold-fop (fop-alter-code :pushp nil)
                (fop-byte-alter-code)
  (let ((slot (clone-arg))
        (value (pop-stack))
        (code (pop-stack)))
    (write-wordindexed code slot value)))

(define-cold-fop (fop-fun-entry)
  (let* ((info (pop-stack))
         (type (pop-stack))
         (arglist (pop-stack))
         (name (pop-stack))
         (code-object (pop-stack))
         (offset (calc-offset code-object (read-word-arg)))
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
                       (make-random-descriptor
                        (+ (descriptor-bits fn)
                           (- (ash sb!vm:simple-fun-code-offset
                                   sb!vm:word-shift)
                              ;; FIXME: We should mask out the type
                              ;; bits, not assume we know what they
                              ;; are and subtract them out this way.
                              sb!vm:fun-pointer-lowtag))))
    (write-wordindexed fn sb!vm:simple-fun-next-slot next)
    (write-wordindexed fn sb!vm:simple-fun-name-slot name)
    (write-wordindexed fn sb!vm:simple-fun-arglist-slot arglist)
    (write-wordindexed fn sb!vm:simple-fun-type-slot type)
    (write-wordindexed fn sb!vm::simple-fun-info-slot info)
    fn))

(define-cold-fop (fop-foreign-fixup)
  (let* ((kind (pop-stack))
         (code-object (pop-stack))
         (len (read-byte-arg))
         (sym (make-string len)))
    (read-string-as-bytes *fasl-input-stream* sym)
    (let ((offset (read-word-arg))
          (value (cold-foreign-symbol-address sym)))
      (do-cold-fixup code-object offset value kind))
   code-object))

#!+linkage-table
(define-cold-fop (fop-foreign-dataref-fixup)
  (let* ((kind (pop-stack))
         (code-object (pop-stack))
         (len (read-byte-arg))
         (sym (make-string len)))
    (read-string-as-bytes *fasl-input-stream* sym)
    (maphash (lambda (k v)
               (format *error-output* "~&~S = #X~8X~%" k v))
             *cold-foreign-symbol-table*)
    (error "shared foreign symbol in cold load: ~S (~S)" sym kind)))

(define-cold-fop (fop-assembler-code)
  (let* ((length (read-word-arg))
         (header-n-words
          ;; Note: we round the number of constants up to ensure that
          ;; the code vector will be properly aligned.
          (round-up sb!vm:code-constants-offset 2))
         (des (allocate-cold-descriptor *read-only*
                                        (+ (ash header-n-words
                                                sb!vm:word-shift)
                                           length)
                                        sb!vm:other-pointer-lowtag)))
    (write-memory des
                  (make-other-immediate-descriptor
                   header-n-words sb!vm:code-header-widetag))
    (write-wordindexed des
                       sb!vm:code-code-size-slot
                       (make-fixnum-descriptor
                        (ash (+ length (1- (ash 1 sb!vm:word-shift)))
                             (- sb!vm:word-shift))))
    (write-wordindexed des sb!vm:code-entry-points-slot *nil-descriptor*)
    (write-wordindexed des sb!vm:code-debug-info-slot *nil-descriptor*)

    (let* ((start (+ (descriptor-byte-offset des)
                     (ash header-n-words sb!vm:word-shift)))
           (end (+ start length)))
      (read-bigvec-as-sequence-or-die (descriptor-bytes des)
                                      *fasl-input-stream*
                                      :start start
                                      :end end))
    des))

(define-cold-fop (fop-assembler-routine)
  (let* ((routine (pop-stack))
         (des (pop-stack))
         (offset (calc-offset des (read-word-arg))))
    (record-cold-assembler-routine
     routine
     (+ (logandc2 (descriptor-bits des) sb!vm:lowtag-mask) offset))
    des))

(define-cold-fop (fop-assembler-fixup)
  (let* ((routine (pop-stack))
         (kind (pop-stack))
         (code-object (pop-stack))
         (offset (read-word-arg)))
    (record-cold-assembler-fixup routine code-object offset kind)
    code-object))

(define-cold-fop (fop-code-object-fixup)
  (let* ((kind (pop-stack))
         (code-object (pop-stack))
         (offset (read-word-arg))
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
  (format t "#define SBCL_CORE_VERSION_INTEGER ~D~%" sbcl-core-version-integer)
  (format t
          "#define SBCL_VERSION_STRING ~S~%"
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
                       (record (c-name name) priority (if large "LU" "")))
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
              (maybe-record-with-translated-name '("-SIZE") 6)
              (maybe-record-with-translated-name '("-START" "-END" "-PAGE-BYTES") 7 :large t)
              (maybe-record-with-translated-name '("-CORE-ENTRY-TYPE-CODE") 8)
              (maybe-record-with-translated-name '("-CORE-SPACE-ID") 9)
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
                  "LU"
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
  (let ((internal-errors sb!c:*backend-internal-errors*))
    (dotimes (i (length internal-errors))
      (let ((current-error (aref internal-errors i)))
        ;; FIXME: this UNLESS should go away (see also FIXME in
        ;; interr.lisp) -- APD, 2002-03-05
        (unless (eq nil (car current-error))
          (format t "#define ~A ~D~%"
                  (c-symbol-name (car current-error))
                  i))))
    (format t "#define INTERNAL_ERROR_NAMES \\~%~{~S~#[~:;, \\~%~]~}~%"
            (map 'list #'cdr internal-errors)))
  (terpri)

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
        (lowtag (eval (sb!vm:primitive-object-lowtag obj))))
    (when lowtag
      (dolist (slot (sb!vm:primitive-object-slots obj))
        (format t "#define ~A_~A_OFFSET ~D~%"
                (c-symbol-name name)
                (c-symbol-name (sb!vm:slot-name slot))
                (- (* (sb!vm:slot-offset slot) sb!vm:n-word-bytes) lowtag)))
      (terpri)))
  (format t "#endif /* LANGUAGE_ASSEMBLY */~2%"))

(defun write-structure-object (dd)
  (flet ((cstring (designator)
           (c-name (string-downcase (string designator)))))
    (format t "#ifndef LANGUAGE_ASSEMBLY~2%")
    (format t "struct ~A {~%" (cstring (dd-name dd)))
    (format t "    lispobj header;~%")
    (format t "    lispobj layout;~%")
    (dolist (slot (dd-slots dd))
      (when (eq t (dsd-raw-type slot))
        (format t "    lispobj ~A;~%" (cstring (dsd-name slot)))))
    (unless (oddp (+ (dd-length dd) (dd-raw-length dd)))
      (format t "    lispobj raw_slot_padding;~%"))
    (dotimes (n (dd-raw-length dd))
      (format t "    lispobj raw~D;~%" (- (dd-raw-length dd) n 1)))
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
                 (let ((fun (read-wordindexed fdefn
                                              sb!vm:fdefn-fun-slot)))
                   (if (= (descriptor-bits fun)
                          (descriptor-bits *nil-descriptor*))
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
        (format t "~S~%" name)))

    (format t "~%~|~%layout names:~2%")
    (collect ((stuff))
      (maphash (lambda (name gorp)
                 (declare (ignore name))
                 (stuff (cons (descriptor-bits (car gorp))
                              (cdr gorp))))
               *cold-layouts*)
      (dolist (x (sort (stuff) #'< :key #'car))
        (apply #'format t "~8,'0X: ~S[~D]~%~10T~S~%" x))))

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
(defconstant version-core-entry-type-code 3860)
(defconstant build-id-core-entry-type-code 3899)
(defconstant new-directory-core-entry-type-code 3861)
(defconstant initial-fun-core-entry-type-code 3863)
(defconstant page-table-core-entry-type-code 3880)
#!+(and sb-lutex sb-thread)
(defconstant lutex-table-core-entry-type-code 3887)
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
                              :end total-bytes)
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

      ;; Write the Version entry.
      (write-word version-core-entry-type-code)
      (write-word 3)
      (write-word sbcl-core-version-integer)

      ;; Write the build ID.
      (write-word build-id-core-entry-type-code)
      (let ((build-id (with-open-file (s "output/build-id.tmp"
                                         :direction :input)
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
             (cold-fdefn (cold-fdefinition-object cold-name))
             (initial-fun (read-wordindexed cold-fdefn
                                            sb!vm:fdefn-fun-slot)))
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
                      c-header-dir-name)

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

    (when core-file-name
      (if symbol-table-file-name
          (load-cold-foreign-symbol-table symbol-table-file-name)
          (error "can't output a core file without symbol table file input")))

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
           (*cold-symbols* (make-hash-table :test 'equal))
           (*cold-package-symbols* nil)
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
           (*nil-descriptor* (make-nil-descriptor))
           (*current-reversed-cold-toplevels* *nil-descriptor*)
           (*current-debug-sources* *nil-descriptor*)
           (*unbound-marker* (make-other-immediate-descriptor
                              0
                              sb!vm:unbound-marker-widetag))
           *cold-assembler-fixups*
           *cold-assembler-routines*
           #!+x86 (*load-time-code-fixups* (make-hash-table)))

      ;; Prepare for cold load.
      (initialize-non-nil-symbols)
      (initialize-layouts)
      (initialize-static-fns)

      ;; Initialize the *COLD-SYMBOLS* system with the information
      ;; from package-data-list.lisp-expr and
      ;; common-lisp-exports.lisp-expr.
      ;;
      ;; Why do things this way? Historically, the *COLD-SYMBOLS*
      ;; machinery was designed and implemented in CMU CL long before
      ;; I (WHN) ever heard of CMU CL. It dumped symbols and packages
      ;; iff they were used in the cold image. When I added the
      ;; package-data-list.lisp-expr mechanism, the idea was to
      ;; centralize all information about packages and exports. Thus,
      ;; it was the natural place for information even about packages
      ;; (such as SB!PCL and SB!WALKER) which aren't used much until
      ;; after cold load. This didn't quite match the CMU CL approach
      ;; of filling *COLD-SYMBOLS* with symbols which appear in the
      ;; cold image and then dumping only those symbols. By explicitly
      ;; putting all the symbols from package-data-list.lisp-expr and
      ;; from common-lisp-exports.lisp-expr into *COLD-SYMBOLS* here,
      ;; we feed our centralized symbol information into the old CMU
      ;; CL code without having to change the old CMU CL code too
      ;; much. (And the old CMU CL code is still useful for making
      ;; sure that the appropriate keywords and internal symbols end
      ;; up interned in the target Lisp, which is good, e.g. in order
      ;; to make &KEY arguments work right and in order to make
      ;; BACKTRACEs into target Lisp system code be legible.)
      (dolist (exported-name
               (sb-cold:read-from-file "common-lisp-exports.lisp-expr"))
        (cold-intern (intern exported-name *cl-package*)))
      (dolist (pd (sb-cold:read-from-file "package-data-list.lisp-expr"))
        (declare (type sb-cold:package-data pd))
        (let ((package (find-package (sb-cold:package-data-name pd))))
          (labels (;; Call FN on every node of the TREE.
                   (mapc-on-tree (fn tree)
                                 (declare (type function fn))
                                 (typecase tree
                                   (cons (mapc-on-tree fn (car tree))
                                         (mapc-on-tree fn (cdr tree)))
                                   (t (funcall fn tree)
                                      (values))))
                   ;; Make sure that information about the association
                   ;; between PACKAGE and the symbol named NAME gets
                   ;; recorded in the cold-intern system or (as a
                   ;; convenience when dealing with the tree structure
                   ;; allowed in the PACKAGE-DATA-EXPORTS slot) do
                   ;; nothing if NAME is NIL.
                   (chill (name)
                     (when name
                       (cold-intern (intern name package) :package package))))
            (mapc-on-tree #'chill (sb-cold:package-data-export pd))
            (mapc #'chill (sb-cold:package-data-reexport pd))
            (dolist (sublist (sb-cold:package-data-import-from pd))
              (destructuring-bind (package-name &rest symbol-names) sublist
                (declare (ignore package-name))
                (mapc #'chill symbol-names))))))

      ;; Cold load.
      (dolist (file-name object-file-names)
        (write-line (namestring file-name))
        (cold-load file-name))

      ;; Tidy up loose ends left by cold loading. ("Postpare from cold load?")
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
                         layout
                         sb!c::compiled-debug-info
                         sb!c::compiled-debug-fun
                         sb!xc:package))
          (out-to
           (string-downcase (string class))
           (write-structure-object
            (sb!kernel:layout-info (sb!kernel:find-layout class)))))
        (out-to "static-symbols" (write-static-symbols))

        (let ((fn (format nil "~A/Makefile.features" c-header-dir-name)))
          (ensure-directories-exist fn)
          (with-open-file (*standard-output* fn :if-exists :supersede
                                             :direction :output)
            (write-makefile-features)))

        (when core-file-name
          (write-initial-core-file core-file-name))))))
