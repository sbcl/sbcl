;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

;;; STRUCTURE-OBJECT supports placement of raw bits within the object
;;; to allow representation of native word and float-point types directly.

;;; Historically the implementation was optimized for GC by placing all
;;; such slots at the end of the instance, and scavenging only up to last
;;; non-raw slot. This imposed significant overhead for access from Lisp,
;;; because "is-a" inheritance was obliged to rearrange raw slots
;;; to comply with the GC requirement, thus forcing ancestor structure
;;; accessors to compensate for physical structure length in all cases.
;;; Assuming that it is more important to simplify Lisp access than
;;; to simplify GC, we use a more flexible strategy that permits
;;; descendant structures to place new slots anywhere without changing
;;; slot placement established in ancestor structures.
;;; The trade-off is that GC (and a few other things - structure dumping,
;;; EQUALP checking, to name a few) have to be able to determine for each
;;; slot whether it is a Lisp descriptor or just bits. This is done
;;; with the LAYOUT-BITMAP of an object's layout.
;;;
;;; The bitmap stores a 1 in each bit index corresponding to a tagged slot
;;; index. If tagged slots follow raw slots and the the number of slots is
;;; large, the bitmap could be a bignum.  As a special case, -1 represents
;;; that all slots are tagged regardless of instance length.
;;;
;;; Also note that there are possibly some alignment concerns which must
;;; be accounted for when DEFSTRUCT lays out slots,
;;; by injecting padding words appropriately.
;;; For example COMPLEX-DOUBLE-FLOAT *should* be aligned to twice the
;;; alignment of a DOUBLE-FLOAT. It is not, as things stand,
;;; but this is considered a minor bug.

;; To utilize a word-sized slot in a defstruct without having to resort to
;; writing (myslot :type (unsigned-byte #.sb-vm:n-word-bits)), or even
;; worse (:type #+sb-xc-host <sometype> #-sb-xc-host <othertype>),
;; these abstractions are provided as soon as the raw slots defs are.
(def!type sb-vm:word () `(unsigned-byte ,sb-vm:n-word-bits))
(def!type sb-vm:signed-word () `(signed-byte ,sb-vm:n-word-bits))

;;; This constant has a 1 bit meaning "tagged" for every user data slot.
;;; If LAYOUT is not in the header word, then (%INSTANCE-REF instance 0)
;;; indicates as raw so that GC treats layouts consistently, not scanning
;;; them en passant while visiting the payload. Consequently, 0 means either
;;; no slots or all raw, no matter if the layout consumes a slot.
;;; See remarks above CALCULATE-DD-BITMAP for further details.
(defconstant +layout-all-tagged+ (ash -1 sb-vm:instance-data-start))

;; information about how a slot of a given DSD-RAW-TYPE is to be accessed
(defstruct (raw-slot-data
            (:constructor !make-raw-slot-data)
            (:copier nil)
            (:predicate nil))
  ;; What operator is used to access a slot of this type?
  ;; On the host this is a symbol, on the target it is a function
  ;; from which we can extract a name if needed.
  #+sb-xc-host (accessor-name (missing-arg) :type symbol :read-only t)
  #-sb-xc-host (accessor-fun (missing-arg) :type function :read-only t)
  ;; Function to compare slots of this type. Not used on host.
  #-sb-xc-host (comparator (missing-arg) :type function :read-only t)
  ;; the type specifier, which must specify a numeric type.
  (raw-type (missing-arg) :type symbol :read-only t)
  (init-vop (missing-arg) :type symbol :read-only t) ; FIXME: remove
  ;; How many words are each value of this type?
  (n-words (missing-arg) :type (and index (integer 1)) :read-only t)
  ;; Necessary alignment in units of words.  Note that instances
  ;; themselves are aligned by exactly two words, so specifying more
  ;; than two words here would not work.
  (alignment 1 :type (integer 1 2) :read-only t))
(declaim (freeze-type raw-slot-data))

(declaim (inline raw-slot-data-reader-name))
(defun raw-slot-data-reader-name (rsd)
  #+sb-xc-host (raw-slot-data-accessor-name rsd)
  #-sb-xc-host (%simple-fun-name (raw-slot-data-accessor-fun rsd)))

(defun raw-slot-data-writer-name (rsd)
  (ecase (raw-slot-data-reader-name rsd)
    (%raw-instance-ref/word '%raw-instance-set/word)
    (%raw-instance-ref/single '%raw-instance-set/single)
    (%raw-instance-ref/double '%raw-instance-set/double)
    (%raw-instance-ref/signed-word '%raw-instance-set/signed-word)
    (%raw-instance-ref/complex-single '%raw-instance-set/complex-single)
    (%raw-instance-ref/complex-double '%raw-instance-set/complex-double)))

;; Simulate DEFINE-LOAD-TIME-GLOBAL - always bound in the image
;; but not eval'd in the compiler.
(defglobal *raw-slot-data* nil)
;; By making this a cold-init function, it is possible to use raw slots
;; in cold toplevel forms.
(defun !raw-slot-data-init ()
  (macrolet ((make-raw-slot-data (&rest args &key accessor-name &allow-other-keys)
               (declare (ignorable accessor-name))
               #+sb-xc-host
               `(!make-raw-slot-data ,@args)
               #-sb-xc-host
               (let ((access (cadr accessor-name)))
                 `(!make-raw-slot-data
                   :accessor-fun #',access
                   :comparator
                   ;; Not a symbol, because there aren't any so-named functions.
                   (named-lambda ,(string (symbolicate access "="))
                       (index x y)
                     (declare (optimize speed (safety 0)))
                     (= (,access x index) (,access y index)))
                   ;; Ignore the :ACCESSOR-NAME initarg
                   ,@args :allow-other-keys t))))
    (let ((double-float-alignment
           ;; alignment in machine words of double-float slots.
           ;; For 8 byte words, this should be 1 since double-floats are 8 bytes.
           ;; It can be 1 if the word size is 4 bytes and the machine permits
           ;; double-floats to be unnaturally aligned (x86 and ppc).
           (or #+(or x86 x86-64 ppc ppc64 arm64 riscv) 1
               ;; other architectures align double-floats to twice the
               ;; machine word size
               2)))
     (setq *raw-slot-data*
      (vector
       (make-raw-slot-data :raw-type 'sb-vm:word
                           :accessor-name '%raw-instance-ref/word
                           :init-vop 'sb-vm::raw-instance-init/word
                           :n-words 1)
       (make-raw-slot-data :raw-type 'sb-vm:signed-word
                           :accessor-name '%raw-instance-ref/signed-word
                           :init-vop 'sb-vm::raw-instance-init/signed-word
                           :n-words 1)
       (make-raw-slot-data :raw-type 'single-float
                           :accessor-name '%raw-instance-ref/single
                           :init-vop 'sb-vm::raw-instance-init/single
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
                           :n-words 1)
       (make-raw-slot-data :raw-type 'double-float
                           :accessor-name '%raw-instance-ref/double
                           :init-vop 'sb-vm::raw-instance-init/double
                           :alignment double-float-alignment
                           :n-words (/ 8 sb-vm:n-word-bytes))
       (make-raw-slot-data :raw-type 'complex-single-float
                           :accessor-name '%raw-instance-ref/complex-single
                           :init-vop 'sb-vm::raw-instance-init/complex-single
                           :n-words (/ 8 sb-vm:n-word-bytes))
       (make-raw-slot-data :raw-type 'complex-double-float
                           :accessor-name '%raw-instance-ref/complex-double
                           :init-vop 'sb-vm::raw-instance-init/complex-double
                           :alignment double-float-alignment
                           :n-words (/ 16 sb-vm:n-word-bytes))
       #+long-float
       (make-raw-slot-data :raw-type long-float
                           :accessor-name '%raw-instance-ref/long
                           :init-vop 'sb-vm::raw-instance-init/long
                           :n-words #+x86 3 #+sparc 4)
       #+long-float
       (make-raw-slot-data :raw-type complex-long-float
                           :accessor-name '%raw-instance-ref/complex-long
                           :init-vop 'sb-vm::raw-instance-init/complex-long
                           :n-words #+x86 6 #+sparc 8))))))

#+sb-xc-host (!raw-slot-data-init)
#+sb-xc
(declaim (type (simple-vector #.(length *raw-slot-data*)) *raw-slot-data*))

;;; DO-INSTANCE-TAGGED-SLOT iterates over the manifest slots of THING
;;; that contain tagged objects. (The LAYOUT does not count as a manifest slot).
;;; INDEX-VAR is bound to successive slot-indices,
;;; and is usually used as the second argument to %INSTANCE-REF.
#+sb-xc-host
(defmacro do-instance-tagged-slot ((index-var thing) &body body)
  (with-unique-names (instance dsd)
    `(let ((,instance ,thing))
       (dolist (,dsd (dd-slots (find-defstruct-description (type-of ,instance))))
         (let ((,index-var (dsd-index ,dsd)))
           ,@body)))))

;;; PAD, if T (the default), includes a final word that may be present at the
;;; end of the structure due to alignment requirements.
;;; TYPE should be supplied only by 'editcore' for manipulating an on-disk core
;;; mapped an address that differs from the core's desired address.
;;; I have a love/hate relationship with this macro.
;;; It's more efficient than iterating over DSD-SLOTS, and editcore would
;;; have a harder time using DSD-SLOTS. But it's too complicated.
#-sb-xc-host
(defmacro do-instance-tagged-slot ((index-var thing &optional (pad t) layout-expr)
                                   &body body)
  (with-unique-names (instance layout mask bitmap-index bitmap-limit nbits end)
    `(let* ((,instance ,thing)
            (,layout ,(or layout-expr
                          ;; %INSTANCE-LAYOUT is defknown'ed to return a LAYOUT,
                          ;; but heap walking might encounter an instance with no layout,
                          ;; hence the need to access the layout without assuming
                          ;; it to be of that type.
                          `(let ((l #+compact-instance-header
                                    (%primitive %instance-layout ,instance)
                                    #-compact-instance-header
                                    (%instance-ref ,instance 0)))
                             (truly-the sb-vm:layout
                                        (if (eql l 0) #.(find-layout 't) l)))))
            ;; Shift out 1 bit if skipping bit 0 of the 0th mask word
            ;; because it's not user-visible data.
            (,mask (ash (%raw-instance-ref/signed-word ,layout (type-dd-length sb-vm:layout))
                        (- sb-vm:instance-data-start)))
            ;; Start counting from the next bitmap word as we've consumed one already
            (,bitmap-index (1+ (type-dd-length sb-vm:layout)))
            (,bitmap-limit (%instance-length ,layout))
            ;; If this was the last word of the bitmap, then the high bit
            ;; is infinitely sign-extended, and we can keep right-shifting
            ;; the mask word indefinitely. Most bitmaps will have only 1 word,
            ;; so this is almost always MOST-POSITIVE-FIXNUM.
            (,nbits (if (= ,bitmap-index ,bitmap-limit)
                        sb-vm:instance-length-mask
                        (- sb-vm:n-word-bits sb-vm:instance-data-start))))
       (declare (type sb-vm:signed-word ,mask)
                (type fixnum ,nbits))
       (do ((,index-var sb-vm:instance-data-start (1+ ,index-var))
            (,end ,(if pad
                       ;; target instances have an odd number of payload words.
                       `(logior (%instance-length ,instance) 1)
                       `(%instance-length ,instance))))
           ((>= ,index-var ,end))
         (declare (type index ,index-var))
         ;; If mask was fully consumed, fetch the next bitmap word
         (when (zerop ,nbits)
           (setq ,mask (%raw-instance-ref/signed-word ,layout ,bitmap-index)
                 ,nbits (if (= (incf ,bitmap-index) ,bitmap-limit)
                            sb-vm:instance-length-mask
                            sb-vm:n-word-bits)))
         (when (logbitp 0 ,mask) ,@body)
         (setq ,mask (ash ,mask -1)
               ,nbits (truly-the fixnum (1- ,nbits)))))))
