;;;; This file contains structures and functions for the maintenance of
;;;; basic information about defined types. Different object systems
;;;; can be supported simultaneously.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

;;;; DEFSTRUCT-DESCRIPTION

;;; The DEFSTRUCT-DESCRIPTION structure holds compile-time information
;;; about a structure type.
;;; It is defined prior to WRAPPER because WRAPPER-INFO
;;; is declared to hold a DEFSTRUCT-DESCRIPTION.
(def!struct (defstruct-description
             (:conc-name dd-)
             (:copier nil)
             (:pure t)
             (:constructor make-defstruct-description (null-lexenv-p name)))
  ;; name of the structure
  (name (missing-arg) :type symbol :read-only t)
  ;; documentation on the structure
  (doc nil :type (or string null))
  ;; prefix for slot names. If NIL, none.
  (conc-name nil :type (or string null))
  ;; All the :CONSTRUCTOR specs and posssibly an implied constructor,
  ;; keyword constructors first, then BOA constructors. NIL if none.
  (constructors () :type list)
  ;; True if the DEFSTRUCT appeared in a null lexical environment.
  (null-lexenv-p nil :type boolean :read-only t) ; the safe default is NIL
  ;; name of copying function
  (copier-name nil :type symbol)
  ;; name of type predicate
  (predicate-name nil :type symbol)
  ;; the arguments to the :INCLUDE option, or NIL if no included
  ;; structure
  (include nil :type list)
  ;; properties used to define structure-like classes with an
  ;; arbitrary superclass and that may not have STRUCTURE-CLASS as the
  ;; metaclass. Syntax is:
  ;;    (superclass-name metaclass-name metaclass-constructor)
  (alternate-metaclass nil :type list)
  ;; a list of DEFSTRUCT-SLOT-DESCRIPTION objects for all slots
  ;; (including included ones)
  (slots () :type list)
  ;; bit mask containing a 1 for each word that the garbage collector must visit
  ;; (as opposed to a raw slot). Certain slot types (notably fixnum) may have either
  ;; a 0 or a 1 in the mask because it does not matter if it is seen by GC.
  ;; Bit index 0 in the mask is the word just after the header, and so on.
  (bitmap +layout-all-tagged+ :type integer)
  ;; a list of (NAME . INDEX) pairs for accessors of included structures
  (inherited-accessor-alist () :type list)
  ;; number of data words, including the layout itself if the layout
  ;; requires an entire word (when no immobile-space)
  (length 0 :type index)
  ;; General kind of implementation.
  (type 'structure :type (member structure vector list
                                 funcallable-structure))

  ;; The next three slots are for :TYPE'd structures (which aren't
  ;; classes, DD-CLASS-P = NIL)
  ;;
  ;; vector element type
  (element-type t)
  ;; T if :NAMED was explicitly specified, NIL otherwise
  (named nil :type boolean)
  ;; any INITIAL-OFFSET option on this direct type
  (offset nil :type (or index null))

  ;; which :PRINT-mumble option was given, if either was.
  (print-option nil :type (member nil :print-function :print-object))
  ;; the argument to the PRINT-FUNCTION or PRINT-OBJECT option.
  ;; NIL if the option was given with no argument.
  (printer-fname nil :type (or cons symbol))

  ;; the value of the :PURE option, used by cheneygc when purifying.
  ;; This is true if objects of this class are never modified to
  ;; contain dynamic pointers in their slots or constant-like
  ;; substructure (and hence can be copied into read-only space by
  ;; PURIFY).
  ;; This is only meaningful if DD-CLASS-P = T.
  (pure nil :type (member t nil)))
(declaim (freeze-type defstruct-description))
(!set-load-form-method defstruct-description (:host :xc :target))

;;;; basic LAYOUT stuff

;;; Careful here: if you add more bits, then adjust the bit packing for
;;; 64-bit layouts which also store LENGTH + DEPTHOID in the same word.
(defconstant +structure-layout-flag+         #b000000001)
(defconstant +pathname-layout-flag+          #b000000010)
(defconstant +pcl-object-layout-flag+        #b000000100)
(defconstant +condition-layout-flag+         #b000001000)
(defconstant +simple-stream-layout-flag+     #b000010000)
(defconstant +file-stream-layout-flag+       #b000100000)
(defconstant +string-stream-layout-flag+     #b001000000)
(defconstant +stream-layout-flag+            #b010000000)
(defconstant +sequence-layout-flag+          #b100000000)
(defconstant +strictly-boxed-flag+          #b1000000000)
(defconstant layout-flags-mask #xffff) ; "strictly flags" bits from the packed field

;;; the type of LAYOUT-DEPTHOID and LAYOUT-LENGTH values.
;;; Each occupies two bytes of the %BITS slot when possible,
;;; otherwise a slot unto itself.
(def!type layout-depthoid () '(integer -1 #x7FFF))
(def!type layout-length () '(integer 0 #xFFFF))
(def!type layout-bitmap () 'integer)
;;; ID must be an fixnum for either value of n-word-bits.
(def!type layout-id () '(signed-byte 30))

;;; The LAYOUT structure is pointed to by the first cell of instance
;;; (or structure) objects. It represents what we need to know for
;;; type checking and garbage collection. Whenever a class is
;;; incompatibly redefined, a new layout is allocated. If two object's
;;; layouts are EQ, then they are exactly the same type.
;;;
;;; *** IMPORTANT ***
;;;
;;; If you change the slots of LAYOUT, you need to alter genesis as
;;; well, since the initialization of layout slots is hardcoded there.
;;;
;;; FIXME: ...it would be better to automate this, of course...

;;; 64-bit layout FLAGS slot:
;;;
;;; | 4 bytes  | 16 bits | 16 bits |
;;; +----------+---------+---------+
;;;  depthoid    length     flags
;;;
;;; depthoid is stored as a tagged fixnum in its 4 byte field.
;;; I suspect that by further limiting the max depthoid and length
;;; we could shove the random CLOS-HASH into some unused bits while
;;; utilizing the entire 64-bit word as the random bit string for hashing.
;;; Checking for an invalid layout would need to mask out the
;;; length, depthoid, and flags since they have to stay correct at all times.
;;;
;;; 32-bit layout %BITS slot:
;;;
;;; | 2 bytes | 2 bytes |
;;; +---------+---------+
;;;  depthoid    length
;;; (FLAGS will remain as a separate slot)

;;; 32-bit is not done yet. Three slots are still used, instead of two.

#-metaspace
(progn
(sb-xc:defstruct (wrapper (:copier nil)
                         ;; Parsing DEFSTRUCT uses a temporary layout
                         (:constructor make-temporary-wrapper
                             (clos-hash classoid inherits &aux (invalid nil))))

  ;; A packed field containing the DEPTHOID, LENGTH, and FLAGS
  #+64-bit (flags 0 :type (signed-byte #.sb-vm:n-word-bits))

  ;; a union of +something-LAYOUT-FLAG+ bits
  #-64-bit (flags 0 :type word :read-only nil)

  ;; a quasi-random hash value for use by CLOS. Determine by class-name
  ;; for classes named by a symbol, otherwise a pseudo-random value.
  ;; Must be acceptable as an argument to SB-INT:MIX
  (clos-hash (missing-arg) :type (and fixnum unsigned-byte))
  ;; the class that this is a layout for
  (classoid (missing-arg) :type classoid)
  ;; The value of this slot can be:
  ;;   * :UNINITIALIZED if not initialized yet;
  ;;   * NIL if this is the up-to-date layout for a class; or
  ;;   * T if this layout has been invalidated (by being replaced by
  ;;     a new, more-up-to-date LAYOUT).
  ;;   * something else (probably a list) if the class is a PCL wrapper
  ;;     and PCL has made it invalid and made a note to itself about it
  (invalid :uninitialized :type (or cons (member nil t :uninitialized)))
  ;; the layouts for all classes we inherit. If hierarchical, i.e. if
  ;; DEPTHOID >= 0, then these are ordered by ORDER-LAYOUT-INHERITS
  ;; (least to most specific), so that each inherited layout appears
  ;; at its expected depth, i.e. at its LAYOUT-DEPTHOID value.
  ;;
  ;; Remaining elements are filled by the non-hierarchical layouts or,
  ;; if they would otherwise be empty, by copies of succeeding layouts.
  (inherits #() :type simple-vector)
  ;; If inheritance is not hierarchical, this is -1. If inheritance is
  ;; hierarchical, this is the inheritance depth, i.e. (LENGTH INHERITS).
  ;; Note:
  ;;  (1) This turns out to be a handy encoding for arithmetically
  ;;      comparing deepness; it is generally useful to do a bare numeric
  ;;      comparison of these depthoid values, and we hardly ever need to
  ;;      test whether the values are negative or not.
  ;;  (2) This was called INHERITANCE-DEPTH in classic CMU CL. It was
  ;;      renamed because some of us find it confusing to call something
  ;;      a depth when it isn't quite.
  #-64-bit (depthoid -1 :type layout-depthoid)
  ;; the number of top level descriptor cells in each instance
  ;; For [FUNCALLABLE-]STANDARD-OBJECT instances, this is the slot vector
  ;; length, not the primitive object length.
  ;; I tried making a structure of this many slots, and the compiler blew up;
  ;; so it's fair to say this limit is sufficient for practical purposes,
  ;; Let's be consistent here between the two choices of word size.
  #-64-bit (length 0 :type layout-length) ; smaller than SB-INT:INDEX
  ;; If this layout has some kind of compiler meta-info, then this is
  ;; it. If a structure, then we store the DEFSTRUCT-DESCRIPTION here.
  ;; If this layout is for an object of metatype STANDARD-CLASS,
  ;; then these are the EFFECTIVE-SLOT-DEFINITION metaobjects.
  ;; The two are mutually exclusive.
  (%info nil :type (or list defstruct-description))
  ;; EQUALP comparator for two instances with this layout
  ;; Could be the generalized function, or a type-specific one
  ;; if the defstruct was compiled in a policy of SPEED 3.
  (equalp-impl #'equalp-err :type (sfunction (t t) boolean) :read-only t)
  ;; Information about slots in the class to PCL: this provides fast
  ;; access to slot-definitions and locations by name, etc.
  ;; See MAKE-SLOT-TABLE in pcl/slots-boot.lisp for further details.
  (slot-table #(1 nil) :type simple-vector)
  (id-word0 0 :type word)
  (id-word1 0 :type word)
  (id-word2 0 :type word)
  #-64-bit (id-word3 0 :type word)
  #-64-bit (id-word4 0 :type word)
  #-64-bit (id-word5 0 :type word))
(declaim (freeze-type wrapper)))

#+metaspace
(progn
;;; Separating the pointer and nonpointer slots of LAYOUT satisfies the
;;; requirement of a certain garbage collector (WIP), but also opens up the
;;; possibility of absorbing at least one of the vectorish slots into the
;;; WRAPPER since it could be reallocated with variable size.
;;; The SLOT-TABLE slot might be a good candidate for trailing tagged slots.
(sb-xc:defstruct (wrapper (:copier nil) (:constructor %make-wrapper))
  ;; !!! The FRIEND slot in WRAPPER *MUST* BE FIRST !!! (Wired-in assumption in genesis)
  (friend nil :type sb-vm:layout)
  (clos-hash (missing-arg) :type (and fixnum unsigned-byte)) ; redundant
  (classoid (missing-arg) :type classoid)
  (inherits #() :type simple-vector)
  (equalp-impl #'equalp-err :type (sfunction (t t) boolean) :read-only t)
  (slot-table #(1 nil) :type simple-vector)
  (%info nil :type (or list defstruct-description))
  (invalid :uninitialized :type (or cons (member nil t :uninitialized))))
;;; See #-metaspace structure definition for remarks about each slot.
;;; LAYOUT points to WRAPPER and vice-versa.
;;; The most common LAYOUT is 8 words.
;;; Needing >64 words would be quite unusual - the layout would have
;;; an enormous depthoid or bitmap or both.
(sb-xc:defstruct (sb-vm:layout (:copier nil)
                         ;; Parsing DEFSTRUCT uses a temporary layout
                         (:constructor %make-temporary-layout (friend clos-hash)))
  ;; !!! The FRIEND slot in LAYOUT *MUST* BE FIRST !!!
  (friend nil :type wrapper)
  (clos-hash (missing-arg) :type (and fixnum unsigned-byte))
  (flags 0 :type (signed-byte #.sb-vm:n-word-bits))
  (id-word0 0 :type word)
  (id-word1 0 :type word)
  (id-word2 0 :type word)
  ;; There are zero or more raw words if a type needs to store additional layout-ids,
  ;; and there are one or more raw words for the GC bitmap.
  )
(declaim (freeze-type wrapper sb-vm:layout)))

;;; The cross-compiler representation of a LAYOUT omits several things:
;;;   * BITMAP - obtainable via (DD-BITMAP (LAYOUT-INFO layout)).
;;;     GC wants it in the layout to avoid double indirection.
;;;   * EQUALP-TESTS - needed only for the target's implementation of EQUALP.
;;;   * SLOT-TABLE, and SLOT-LIST - used only by the CLOS implementation.
;;;   * ID-WORDn are optimizations for TYPEP.
;;; So none of those really make sense on the host.
;;; Also, we eschew the packed representation of length+depthoid+flags.
;;; FLAGS are computed on demand, and not stored.
#+sb-xc-host
(progn
  (defstruct (wrapper (:include structure!object)
                      (:constructor host-make-wrapper
                         (id clos-hash classoid
                          &key ((:info %info)) depthoid inherits length invalid
                          #+metaspace friend)))
    #+metaspace (friend)
    (id nil :type (or null fixnum))
    ;; Cross-compiler-only translation from slot index to symbol naming
    ;; the accessor to call. (Since access by position is not a thing)
    (index->accessor-map #() :type simple-vector)
    ;; CLOS-HASH is needed to convert some TYPECASE forms to jump tables.
    ;; Theoretically we don't need this in the cross-compiler, because the
    ;; layout has a classoid which has a name which has a known hash.
    ;; But there's no harm in storing it.
    (clos-hash nil :type (and sb-xc:fixnum unsigned-byte))
    (classoid nil :type classoid)
    (invalid :uninitialized :type (or cons (member nil t :uninitialized)))
    (inherits #() :type simple-vector)
    (depthoid -1 :type layout-depthoid)
    (length 0 :type layout-length)
    (%info nil :type (or null defstruct-description)))
  #+metaspace (defstruct (sb-vm:layout (:include structure!object)
                                       (:constructor %make-layout))
                friend)
  (defun make-temporary-wrapper (clos-hash classoid inherits)
    (host-make-wrapper nil clos-hash classoid :inherits inherits :invalid nil))
  (defun wrapper-flags (wrapper)
    (declare (type wrapper wrapper))
    (let ((mapping `((structure-object  ,+structure-layout-flag+)
                     (standard-object   ,+pcl-object-layout-flag+)
                     (pathname          ,+pathname-layout-flag+)
                     (condition         ,+condition-layout-flag+)
                     (file-stream       ,+file-stream-layout-flag+)
                     (string-stream     ,+string-stream-layout-flag+)
                     (stream            ,+stream-layout-flag+)
                     (sequence          ,+sequence-layout-flag+)))
          (flags 0))
      (dolist (x (cons wrapper (coerce (wrapper-inherits wrapper) 'list)))
        (let ((cell (assoc (wrapper-classoid-name x) mapping)))
          (when cell (setq flags (logior flags (second cell))))))
      (let ((dd (wrapper-%info wrapper)))
        (when (or (logtest flags (logior +pathname-layout-flag+ +condition-layout-flag+))
                  (and (logtest flags +structure-layout-flag+)
                       dd
                       (every (lambda (x) (eq (dsd-raw-type x) t))
                              (dd-slots dd))))
          (setf flags (logior flags +strictly-boxed-flag+))))
      ;; KLUDGE: I really don't care to make defstruct-with-alternate-metaclass
      ;; any more complicated than necessary. It is unable to express that
      ;; these funcallable instances can go on pure boxed pages.
      ;; (The trampoline is always an assembler routine, thus ignorable)
      (when (member (wrapper-classoid-name wrapper)
                    '(sb-pcl::ctor sb-pcl::%method-function))
        (setf flags (logior flags +strictly-boxed-flag+)))
      flags))
  (defun wrapper-bitmap (wrapper)
    (acond ((wrapper-%info wrapper) (dd-bitmap it))
           ;; Give T a 0 bitmap. It's arbitrary, but when we need some layout
           ;; that has this bitmap we can use the layout of T.
           ((eq wrapper (find-layout t)) 0)
           (t
            +layout-all-tagged+))))

(defun equalp-err (a b)
  (bug "EQUALP ~S ~S" a b))

(defmacro name->dd (name)
  ;; This wants to be a toplevel macrolet but can't be, because the body of
  ;; the macro (which is run in the host) wouldn't see NAME->DD as a macro
  ;; when expanding for the target. And it can't be a toplevel FLET because
  ;; that would demote the two using macros from toplevel.
  `(find-defstruct-description (cond #-metaspace ((eq ,name 'sb-vm:layout) 'wrapper)
                                     (t ,name))))
(defmacro type-dd-length (type-name) (dd-length (name->dd type-name)))
(defmacro get-dsd-index (type-name slot-name)
  (declare (notinline dsd-index)) ; avoid later inlining failure style-warning
  (dsd-index (find slot-name (dd-slots (name->dd type-name)) :key #'dsd-name)))

;;; Applicable only if bit-packed (for 64-bit architectures)
(defmacro pack-layout-flags (depthoid length flags)
  `(logior (ash ,depthoid (+ 32 sb-vm:n-fixnum-tag-bits)) (ash ,length 16) ,flags))

(defconstant layout-id-vector-fixed-capacity 7)
(defmacro calculate-extra-id-words (depthoid)
  ;; There are 1 or 2 ids per word depending on n-word-bytes.
  ;; We can always store IDs at depthoids 2,3,4,5,6,7,
  ;; so depthoid less than or equal to 7 needs no extra words.
  ;; 0 and 1 for T and STRUCTURE-OBJECT respectively are not stored.
  `(ceiling (max 0 (- ,depthoid ,layout-id-vector-fixed-capacity))
            ,(/ sb-vm:n-word-bytes 4)))

(declaim (inline wrapper-info wrapper-dd))
(defun wrapper-info (wrapper)
  (let ((info (wrapper-%info wrapper))) (unless (listp info) info)))
(defun (setf wrapper-info) (newval wrapper)
  ;; The current value must be nil or a defstruct-description,
  ;; otherwise we'd clobber a non-nil slot list.
  (aver (not (consp (wrapper-%info wrapper))))
  (setf (wrapper-%info wrapper) newval))
;; Use WRAPPER-DD to read WRAPPER-INFO and assert that it is non-nil.
(defun wrapper-dd (wrapper)
  (the defstruct-description (wrapper-%info wrapper)))

;;; See the pictures above DD-BITMAP in src/code/defstruct for the details.
(defconstant standard-gf-primitive-obj-layout-bitmap
  #+compact-instance-header  6
  #-compact-instance-header -4)

#-sb-xc-host
(progn
(declaim (inline bitmap-nwords bitmap-all-taggedp))
(defun bitmap-nwords (layout)
  (declare (sb-vm:layout layout))
  (- (%instance-length layout) (type-dd-length sb-vm:layout)))

(defun bitmap-all-taggedp (layout)
  ;; All bitmaps have at least 1 word; read that first.
  (and (= (%raw-instance-ref/signed-word layout (type-dd-length sb-vm:layout))
          +layout-all-tagged+)
       ;; Then check that there are no additional words.
       (= (%instance-length layout) (1+ (type-dd-length sb-vm:layout)))))

#+metaspace ; If metaspace, then WRAPPER has no flags; they're in the LAYOUT.
(defmacro wrapper-flags (x) `(layout-flags (wrapper-friend ,x)))

;; 32-bit has the depthoid as a slot, 64-bit has it is part of FLAGS which are
;; in the LAYOUT
#+64-bit
(defun wrapper-depthoid (wrapper) (layout-depthoid (wrapper-friend wrapper)))

(defun wrapper-bitmap (wrapper)
  (declare (type wrapper wrapper))
  (acond ((wrapper-info wrapper) (dd-bitmap it))
         ;; Instances lacking DD-INFO are CLOS objects, which can't generally have
         ;; raw slots, except that funcallable-instances may have 2 raw slots -
         ;; the trampoline and the layout. The trampoline can have a tag, depending
         ;; on the platform, and the layout is tagged but a special case.
         ;; In any event, the bitmap is always 1 word, and there are no "extra ID"
         ;; words preceding it.
         (t (the fixnum
                 (%raw-instance-ref/signed-word (wrapper-friend wrapper)
                                                (type-dd-length sb-vm:layout))))))
#+64-bit
(defmacro wrapper-length (wrapper) ; SETFable
  `(ldb (byte 16 16) (layout-flags (wrapper-friend ,wrapper))))

) ; end PROGN

;;; True of STANDARD-OBJECT, which include generic functions.
(declaim (inline layout-for-pcl-obj-p))
(defun layout-for-pcl-obj-p (wrapper)
  (declare (type wrapper wrapper))
  (logtest (wrapper-flags wrapper) +pcl-object-layout-flag+))

;;; The CLASSOID structure is a supertype of all classoid types.  A
;;; CLASSOID is also a CTYPE structure as recognized by the type
;;; system.  (FIXME: It's also a type specifier, though this might go
;;; away as with the merger of SB-PCL:CLASS and CL:CLASS it's no
;;; longer necessary)
(def!struct (classoid
             (:include ctype)
             (:constructor nil)
             (:copier nil)
             (:print-object
              (lambda (class stream)
                (let ((name (classoid-name class)))
                  (print-unreadable-object (class stream
                                                  :type t
                                                  :identity (not name))
                    (format stream
                            ;; FIXME: Make sure that this prints
                            ;; reasonably for anonymous classes.
                            "~:[anonymous~;~:*~S~]~@[ (~(~A~))~]"
                            name
                            (classoid-state class))))))
             #-sb-xc-host (:pure nil))
  ;; the value to be returned by CLASSOID-NAME.
  (name nil :type symbol)
  ;; the current WRAPPER for this class, or NIL if none assigned yet
  (wrapper nil :type (or null wrapper))
  ;; How sure are we that this class won't be redefined?
  ;;   :READ-ONLY = We are committed to not changing the effective
  ;;                slots or superclasses.
  ;;   :SEALED    = We can't even add subclasses.
  ;;   NIL        = Anything could happen.
  (state nil :type (member nil :read-only :sealed))
  ;; direct superclasses of this class. Always NIL for CLOS classes.
  (direct-superclasses () :type list)
  ;; Definition location
  ;; Not used for standard-classoid, because pcl has its own mechanism.
  (source-location nil)
  ;; representation of all of the subclasses (direct or indirect) of
  ;; this class. This is NIL if no subclasses or not initalized yet;
  ;; otherwise, it's an EQ hash-table mapping CLASSOID objects to the
  ;; subclass layout that was in effect at the time the subclass was
  ;; created.
  ;; Initially an alist, and changed to a hash-table at some threshold.
  (subclasses nil :type (or list hash-table))
  (%lock nil) ; install it just-in-time, similar to hash-table-lock
  ;; the PCL class (= CL:CLASS, but with a view to future flexibility
  ;; we don't just call it the CLASS slot) object for this class, or
  ;; NIL if none assigned yet
  (pcl-class nil))

(defun wrapper-classoid-name (x)
  (classoid-name (wrapper-classoid x)))

;;;; object types to represent classes

;;; An UNDEFINED-CLASSOID is a cookie we make up to stick in forward
;;; referenced layouts. Users should never see them.
(def!struct (undefined-classoid
             (:include classoid)
             (:copier nil)
             (:constructor make-undefined-classoid
                 (name &aux (%bits (pack-ctype-bits classoid name))))))

;;; BUILT-IN-CLASS is used to represent the standard classes that
;;; aren't defined with DEFSTRUCT and other specially implemented
;;; primitive types whose only attribute is their name.
;;;
;;; Some BUILT-IN-CLASSes have a TRANSLATION, which means that they
;;; are effectively DEFTYPE'd to some other type (usually a union of
;;; other classes or a "primitive" type such as NUMBER, ARRAY, etc.)
;;; This translation is done when type specifiers are parsed. Type
;;; system operations (union, subtypep, etc.) should never encounter
;;; translated classes, only their translation.
(sb-xc:defstruct (built-in-classoid (:include classoid) (:copier nil)
                                    (:constructor !make-built-in-classoid))
  ;; the type we translate to on parsing. If NIL, then this class
  ;; stands on its own
  (translation nil :type (or null ctype) :read-only t)
  (predicate nil :type (sfunction (t) boolean) :read-only t))
#+sb-xc-host
(defstruct (built-in-classoid (:include classoid) (:copier nil)
                              (:constructor !make-built-in-classoid))
  ;; until bootstrap of all CTYPEs, store a dummy value distinct from NIL
  (translation nil :type (or null ctype (member :initializing))))

(def!struct (condition-classoid (:include classoid)
                                (:copier nil)
                                (:constructor make-condition-classoid
                                    (&key name &aux (%bits (pack-ctype-bits classoid name)))))
  ;; list of CONDITION-SLOT structures for the direct slots of this
  ;; class
  (slots nil :type list)
  ;; list of CONDITION-SLOT structures for all of the effective class
  ;; slots of this class
  (class-slots nil :type list)
  ;; report function or NIL
  (report nil :type (or function null))
  ;; list of specifications of the form
  ;;
  ;;   (INITARG INITFORM THUNK)
  ;;
  ;; where THUNK, when called without arguments, returns the value for
  ;; INITARG.
  (direct-default-initargs () :type list)
  ;; class precedence list as a list of CLASS objects, with all
  ;; non-CONDITION classes removed
  (cpl () :type list)
  ;; a list of all the effective instance allocation slots of this
  ;; class that have a non-constant initform or default-initarg.
  ;; Values for these slots must be computed in the dynamic
  ;; environment of MAKE-CONDITION.
  (hairy-slots nil :type list))

;;; STRUCTURE-CLASSOID represents what we need to know about structure
;;; classes. Non-structure "typed" defstructs are a special case, and
;;; don't have a corresponding class.
(def!struct (structure-classoid
             (:include classoid)
             (:copier nil)
             (:constructor make-structure-classoid
                           (&key name &aux (%bits (pack-ctype-bits classoid name))))))

;;;; classoid namespace

;;; We use an indirection to allow forward referencing of class
;;; definitions with load-time resolution.
(def!struct (classoid-cell
             (:copier nil)
             (:constructor make-classoid-cell (name &optional classoid))
             (:print-object (lambda (s stream)
                              (print-unreadable-object (s stream :type t)
                                (prin1 (classoid-cell-name s) stream)))))
  ;; Name of class we expect to find.
  (name nil :type symbol :read-only t)
  ;; Classoid or NIL if not yet defined.
  (classoid nil :type (or classoid null))
  ;; PCL class, if any
  (pcl-class nil))
(declaim (freeze-type classoid-cell))
(!set-load-form-method classoid-cell (:xc :target)
  (lambda (self env)
    (declare (ignore env))
    `(find-classoid-cell ',(classoid-cell-name self) :create t)))

;;;; PCL stuff

;;; the CLASSOID that we use to represent type information for
;;; STANDARD-CLASS and FUNCALLABLE-STANDARD-CLASS.  The type system
;;; side does not need to distinguish between STANDARD-CLASS and
;;; FUNCALLABLE-STANDARD-CLASS.
(def!struct (standard-classoid (:include classoid)
                               (:copier nil)
                               (:constructor make-standard-classoid
                                   (&key name pcl-class
                                    &aux (%bits (pack-ctype-bits classoid name)))))
  old-layouts)
;;; a metaclass for classes which aren't standardlike but will never
;;; change either.
(def!struct (static-classoid (:include classoid)
                             (:copier nil)
                             (:constructor make-static-classoid
                                 (&key name &aux (%bits (pack-ctype-bits classoid name))))))

(declaim (freeze-type built-in-classoid condition-classoid
                      standard-classoid static-classoid))
