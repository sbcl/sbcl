;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;; STRUCTURE-OBJECT supports two different strategies to place raw slots
;;; (containing "just bits", not Lisp descriptors) within it in a way
;;; that GC has knowledge of. No backend supports both strategies though.

;;; The older strategy is "non-interleaved".
;;; Consider a structure of 3 tagged slots (A,B,C) and 2 raw slots,
;;; where (for simplicity) each raw slot takes space equal to one Lisp word.
;;; (In general raw slots can take >1 word)
;;; Lisp code arranges so that raw slots are last.
;;; Word offsets are listed on the left
;;;    0 : header = (instance-length << 8) | instance-header-widetag
;;;    1 : dsd-index 0 = ptr to LAYOUT
;;;    2 : dsd-index 1 = tagged slot A
;;;    3 : dsd-index 2 = ... B
;;;    4 : dsd-index 3 = ... C
;;;    5 : filler
;;;    6 : dsd-index 1 = second raw slot
;;;    7 : dsd-index 0 = first raw slot
;;;
;;; Note that numbering of raw slots with respect to their DSD-INDEX
;;; restarts at 0, so there are two "spaces" of dsd-indices, the non-raw
;;; and the raw space. Also note that filler was added in the middle, so
;;; that adding INSTANCE-LENGTH to the object's address always gets you
;;; to exactly the 0th raw slot. The filler can't be squeezed out, because
;;; all Lisp objects must consume an even number of words, and the length
;;; of an instance reflects the number of physical - not logical - words
;;; that follow the instance header.
;;;
;;; This strategy for placement of raw slots is easy for GC because GC's
;;; view of an instance is simply some number of boxed words followed by
;;; some number of ignored words.
;;; However, this strategy presents a difficulty for Lisp in that a raw
;;; slot at a given index is not at a fixed offset relative to the base of
;;; the object - it is fixed relative to the _last_ word of the object.
;;; This has to do with the requirement that structure accessors defined by
;;; a parent type work correctly on a descendant type, while preserving the
;;; simple-for-GC aspect. If another DEFSTRUCT says to :INCLUDE the above,
;;; adding two more tagged slots D and E, the slot named D occupies word 5
;;; ('filler' above), E occupies word 6, and the two raw slots shift down.
;;; To read raw slot at index N requires adding to the object pointer
;;; the number of words represented by instance-length and subtracting the
;;; raw slot index.
;;; Aside from instance-length, the only additional piece of information
;;; that GC needs to know to scavenge a structure is the number of raw slots,
;;; which is obtained from the object's layout in the N-UNTAGGED-SLOTS slot.

;;; Assuming that it is more important to simplify runtime access than
;;; to simplify GC, we can use the newer strategy, "interleaved" raw slots.
;;; Interleaving freely intermingles tagged data with untagged data
;;; following the layout.  This permits descendant structures to add
;;; slots of any kind to the end without changing any physical placement
;;; that was already determined, and eliminates the runtime computation
;;; of the offset to raw slots. It is also generally easier to understand.
;;; The trade-off is that GC (and a few other things - structure dumping,
;;; EQUALP checking, to name a few) have to be able to determine for each
;;; slot whether it is a Lisp descriptor or just bits. This is done
;;; with the LAYOUT-UNTAGGED-BITMAP of an object's layout.
;;; The bitmap stores a '1' for each bit representing a raw word,
;;; and could be a BIGNUM given a spectacularly huge structure.

;;; Also note that in both strategies there are possibly some alignment
;;; concerns which must be accounted for when DEFSTRUCT lays out slots,
;;; by injecting padding words appropriately.
;;; For example COMPLEX-DOUBLE-FLOAT *should* be aligned to twice the
;;; alignment of a DOUBLE-FLOAT. It is not, as things stand,
;;; but this is considered a minor bug.

;; To utilize a word-sized slot in a defstruct without having to resort to
;; writing (myslot :type (unsigned-byte #.sb!vm:n-word-bits)), or even
;; worse (:type #+sb-xc-host <sometype> #-sb-xc-host <othertype>),
;; these abstractions are provided as soon as the raw slots defs are.
;; 'signed-word' is here for companionship - slots of that type are not raw.
(def!type sb!vm:word () `(unsigned-byte ,sb!vm:n-word-bits))
(def!type sb!vm:signed-word () `(signed-byte ,sb!vm:n-word-bits))

;; These definitions pertain to how a LAYOUT stores the raw-slot metadata,
;; and we need them before 'class.lisp' is compiled (why, I'm can't remember).
;; LAYOUT-RAW-SLOT-METADATA is an abstraction over whichever kind of
;; metadata we have - it will be one or the other.
#!-interleaved-raw-slots
(progn (deftype layout-raw-slot-metadata-type () 'index)
       (defmacro layout-raw-slot-metadata (x) `(layout-n-untagged-slots ,x)))
;; It would be possible to represent an unlimited number of trailing untagged
;; slots (maybe) without consing a bignum if we wished to allow signed integers
;; for the raw slot bitmap, but that's probably confusing and pointless, so...
#!+interleaved-raw-slots
(progn (deftype layout-raw-slot-metadata-type () 'unsigned-byte)
       (defmacro layout-raw-slot-metadata (x) `(layout-untagged-bitmap ,x)))

;; information about how a slot of a given DSD-RAW-TYPE is to be accessed
(defstruct (raw-slot-data
            (:copier nil)
            (:predicate nil))
  ;; the type specifier, which must specify a numeric type.
  (raw-type (missing-arg) :type symbol :read-only t)
  ;; What operator is used to access a slot of this type?
  (accessor-name (missing-arg) :type symbol :read-only t)
  (init-vop (missing-arg) :type symbol :read-only t)
  ;; How many words are each value of this type?
  (n-words (missing-arg) :type (and index (integer 1)) :read-only t)
  ;; Necessary alignment in units of words.  Note that instances
  ;; themselves are aligned by exactly two words, so specifying more
  ;; than two words here would not work.
  (alignment 1 :type (integer 1 2) :read-only t)
  (comparer (missing-arg) :type function :read-only t))

#!-sb-fluid (declaim (freeze-type raw-slot-data))

;; Simulate DEFINE-LOAD-TIME-GLOBAL - always bound in the image
;; but not eval'd in the compiler.
(defglobal *raw-slot-data* nil)
;; By making this a cold-init function, it is possible to use raw slots
;; in cold toplevel forms.
(defun !raw-slot-data-init ()
  (macrolet ((make-comparer (accessor-name)
               #+sb-xc-host
               `(lambda (x y)
                  (declare (ignore x y))
                  (error "~S comparator called" ',accessor-name))
               #-sb-xc-host
               ;; Not a symbol, because there aren't any so-named functions.
               `(named-lambda ,(string (symbolicate accessor-name "="))
                    (index x y)
                  (declare (optimize speed (safety 0)))
                  (= (,accessor-name x index)
                     (,accessor-name y index)))))
    (let ((double-float-alignment
            ;; white list of architectures that can load unaligned doubles:
            #!+(or x86 x86-64 ppc arm64) 1
            ;; at least sparc, mips and alpha can't:
            #!-(or x86 x86-64 ppc arm64) 2))
     (setq *raw-slot-data*
      (vector
       (make-raw-slot-data :raw-type 'sb!vm:word
                           :accessor-name '%raw-instance-ref/word
                           :init-vop 'sb!vm::raw-instance-init/word
                           :n-words 1
                           :comparer (make-comparer %raw-instance-ref/word))
       (make-raw-slot-data :raw-type 'single-float
                           :accessor-name '%raw-instance-ref/single
                           :init-vop 'sb!vm::raw-instance-init/single
                           ;; KLUDGE: On 64 bit architectures, we
                           ;; could pack two SINGLE-FLOATs into the
                           ;; same word if raw slots were indexed
                           ;; using bytes instead of words.  However,
                           ;; I don't personally find optimizing
                           ;; SINGLE-FLOAT memory usage worthwile
                           ;; enough.  And the other datatype that
                           ;; would really benefit is (UNSIGNED-BYTE
                           ;; 32), but that is a subtype of FIXNUM, so
                           ;; we store it unraw anyway.  :-( -- DFL
                           :n-words 1
                           :comparer (make-comparer %raw-instance-ref/single))
       (make-raw-slot-data :raw-type 'double-float
                           :accessor-name '%raw-instance-ref/double
                           :init-vop 'sb!vm::raw-instance-init/double
                           :alignment double-float-alignment
                           :n-words (/ 8 sb!vm:n-word-bytes)
                           :comparer (make-comparer %raw-instance-ref/double))
       (make-raw-slot-data :raw-type 'complex-single-float
                           :accessor-name '%raw-instance-ref/complex-single
                           :init-vop 'sb!vm::raw-instance-init/complex-single
                           :n-words (/ 8 sb!vm:n-word-bytes)
                           :comparer (make-comparer %raw-instance-ref/complex-single))
       (make-raw-slot-data :raw-type 'complex-double-float
                           :accessor-name '%raw-instance-ref/complex-double
                           :init-vop 'sb!vm::raw-instance-init/complex-double
                           :alignment double-float-alignment
                           :n-words (/ 16 sb!vm:n-word-bytes)
                           :comparer (make-comparer %raw-instance-ref/complex-double))
       #!+long-float
       (make-raw-slot-data :raw-type long-float
                           :accessor-name '%raw-instance-ref/long
                           :init-vop 'sb!vm::raw-instance-init/long
                           :n-words #!+x86 3 #!+sparc 4
                           :comparer (make-comparer %raw-instance-ref/long))
       #!+long-float
       (make-raw-slot-data :raw-type complex-long-float
                           :accessor-name '%raw-instance-ref/complex-long
                           :init-vop 'sb!vm::raw-instance-init/complex-long
                           :n-words #!+x86 6 #!+sparc 8
                           :comparer (make-comparer %raw-instance-ref/complex-long)))))))

#+sb-xc-host (!raw-slot-data-init)
#+sb-xc
(declaim (type (simple-vector #.(length *raw-slot-data*)) *raw-slot-data*))

;; DO-INSTANCE-TAGGED-SLOT iterates over the manifest slots of THING
;; that contain tagged objects. (The LAYOUT does not count as a manifest slot).
;; INDEX-VAR is bound to successive slot-indices,
;; and is usually used as the second argument to %INSTANCE-REF.
;; EXCLUDE-PADDING, if T, skips a final word that may be present
;; at the end of the structure due to alignment requirements.
;; LAYOUT is optional and somewhat unnecessary, but since some uses of
;; this macro already have a layout in hand, it can be supplied.
;; [If the compiler were smarter about doing fewer memory accesses,
;; there would be no need at all for the LAYOUT - if it had already been
;; accessed, it shouldn't be another memory read]
;; * CAUTION: with a STANDARD-OBJECT you MUST NOT specify :EXCLUDE-PADDING T
;;   because that equates to using LAYOUT-LENGTH rather than %INSTANCE-LENGTH
;;   to compute the upper bound, but LAYOUT-LENGTH of a STANDARD-OBJECT
;;   is not pertinent to the number of storage cells in the primitive object.
;;
(defmacro do-instance-tagged-slot ((index-var thing &key (layout nil layout-p)
                                                         exclude-padding)
                                   &body body)
  (with-unique-names (instance n-layout limit bitmap)
    (declare (ignorable bitmap))
    (let ((end-expr (if exclude-padding
                        `(layout-length ,n-layout)
                        `(%instance-length ,instance))))
      `(let* (,@(if (and layout-p exclude-padding) nil `((,instance ,thing)))
              (,n-layout ,(or layout `(%instance-layout ,instance))))
         #!+interleaved-raw-slots
         (do ((,bitmap (layout-untagged-bitmap ,n-layout))
              (,index-var sb!vm:instance-data-start (1+ ,index-var))
              (,limit ,end-expr))
             ((>= ,index-var ,limit))
           (declare (type index ,index-var))
           (unless (logbitp ,index-var ,bitmap)
             ,@body))
         #!-interleaved-raw-slots
         (do ((,index-var 1 (1+ ,index-var))
              (,limit (- ,end-expr (layout-n-untagged-slots ,n-layout))))
             ((>= ,index-var ,limit))
           (declare (type index ,index-var))
           ,@body)))))
