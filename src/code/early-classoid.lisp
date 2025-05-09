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

(defconstant +dd-named+      #b000001) ; :NAMED was specified
(defconstant +dd-printfun+   #b000010) ; :PRINT-FUNCTION was specified
(defconstant +dd-printobj+   #b000100) ; :PRINT-OBJECT was specified
(defconstant +dd-pure+       #b001000) ; :PURE T was specified
(defconstant +dd-varylen+    #b010000)
(defconstant +dd-nullenv+    #b100000)

;;; The DEFSTRUCT-DESCRIPTION structure holds compile-time information
;;; about a structure type.
;;; It is defined prior to LAYOUT because a LAYOUT-INFO slot
;;; is declared to hold a DEFSTRUCT-DESCRIPTION.
(def!struct (defstruct-description
             (:conc-name dd-)
             (:copier nil)
             (:pure t)
             (:constructor make-defstruct-description (name flags)))
  ;; name of the structure
  (name (missing-arg) :type symbol :read-only t)
  (flags 0 :type fixnum) ; see the constants above
  ;; documentation on the structure
  (doc nil :type (or string null))
  ;; prefix for slot names. If NIL, none.
  (conc-name nil :type (or string null))
  ;; All the :CONSTRUCTOR specs and posssibly an implied constructor,
  ;; keyword constructors first, then BOA constructors. NIL if none.
  (constructors () :type list)
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
  ;; Technically this is redundant information: it can be derived from DD-SLOTS
  ;; by taking the index of the final slot and adding its length in words.
  ;; If there are no slots, then it's just INSTANCE-DATA-START.
  (length 0 :type index)
  ;; General kind of implementation.
  (type 'structure :type (member structure vector list
                                 funcallable-structure))

  ;; If this structure is a classoid, then T if all slots are tagged, * if not.
  ;; If a vector, the vector element type.
  ;; If a list, not used.
  (%element-type t)
  ;; any INITIAL-OFFSET option on this direct type
  (offset nil :type (or index null))

  ;; the argument to the PRINT-FUNCTION or PRINT-OBJECT option.
  ;; NIL if the option was given with no argument.
  (printer-fname nil :type (or cons symbol)))
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

(declaim (start-block))

;;; The CLASSOID structure is a supertype of all classoid types.  A
;;; CLASSOID is also a CTYPE structure as recognized by the type
;;; system.  (FIXME: It's also a type specifier, though this might go
;;; away as with the merger of SB-PCL:CLASS and CL:CLASS it's no
;;; longer necessary)
(defstruct (classoid
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
  ;;; KLUDGE: Keep synchronized with hardcoded slot order in 'instance.inc'
  ;; the value to be returned by CLASSOID-NAME.
  (name nil :type symbol)
  ;; the current LAYOUT for this class, or NIL if none assigned yet
  (layout nil :type (or null layout))
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

(sb-xc:defstruct (layout (:copier nil)
                         ;; Parsing DEFSTRUCT uses a temporary layout
                         (:constructor make-temporary-layout
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
  ;; Information for the quicker variant of SLOT-VALUE on STRUCTURE-OBJECT
  ;; INSTANCE uses at most 14 bits in the primitive object header for the payload
  ;; length, so the function can't actually return all of the INDEX type.
  (slot-mapper nil :type (or (sfunction (symbol) (or index null))
                             simple-vector null))
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
(declaim (freeze-type layout))

(declaim (end-block))

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
  (defstruct (layout (:constructor host-make-layout
                         (id clos-hash classoid
                          &key ((:info %info)) depthoid inherits length invalid)))
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
  (defun make-temporary-layout (clos-hash classoid inherits)
    (host-make-layout nil clos-hash classoid :inherits inherits :invalid nil))
  (defun layout-flags (layout)
    (declare (type layout layout))
    (let ((mapping `((structure-object  ,+structure-layout-flag+)
                     (standard-object   ,+pcl-object-layout-flag+)
                     (pathname          ,+pathname-layout-flag+)
                     (condition         ,+condition-layout-flag+)
                     (file-stream       ,+file-stream-layout-flag+)
                     (string-stream     ,+string-stream-layout-flag+)
                     (stream            ,+stream-layout-flag+)
                     (sequence          ,+sequence-layout-flag+)))
          (flags 0))
      (dolist (x (cons layout (coerce (layout-inherits layout) 'list)))
        (let ((cell (assoc (layout-classoid-name x) mapping)))
          (when cell (setq flags (logior flags (second cell))))))
      (let ((dd (layout-%info layout)))
        (when (or (logtest flags (logior +pathname-layout-flag+ +condition-layout-flag+))
                  (and (logtest flags +structure-layout-flag+)
                       dd
                       (eq (dd-%element-type dd) 't)))
          (setf flags (logior flags +strictly-boxed-flag+))))
      ;; KLUDGE: I really don't care to make defstruct-with-alternate-metaclass
      ;; any more complicated than necessary. It is unable to express that
      ;; these funcallable instances can go on pure boxed pages.
      ;; (The trampoline is always an assembler routine, thus ignorable)
      (when (member (layout-classoid-name layout)
                    '(sb-pcl::ctor sb-pcl::%method-function))
        (setf flags (logior flags +strictly-boxed-flag+)))
      flags))
  (defun layout-bitmap (layout)
    (acond ((layout-%info layout) (dd-bitmap it))
           ;; Give T a 0 bitmap. It's arbitrary, but when we need some layout
           ;; that has this bitmap we can use the layout of T.
           ((eq layout (find-layout t)) 0)
           ;; KLUDGE: PROMISE-COMPILE stuffs the layout of FUNCTION into the
           ;; funcallable-instance it produces.  gencgc verifies that funcallable-instances
           ;; have a bitmap with 0 bits where the trampoline word and layout are.
           #-compact-instance-header ((eq layout (find-layout 'function)) -4)
           (t
            +layout-all-tagged+)))
  (defun %layout-bitmap (layout) (layout-bitmap layout))
) ; end PROGN #+sb-xc-host

(defun equalp-err (a b)
  (bug "EQUALP ~S ~S" a b))

(defmacro get-dsd-index (type-name slot-name)
  (declare (notinline dsd-index)) ; avoid later inlining failure style-warning
  (dsd-index (find slot-name
                   (dd-slots (find-defstruct-description type-name))
                   :key #'dsd-name)))

;;; Applicable only if bit-packed (for 64-bit architectures)
(defmacro pack-layout-flags (depthoid length flags)
  `(logior (ash ,depthoid (+ 32 sb-vm:n-fixnum-tag-bits)) (ash ,length 16) ,flags))

(defmacro type-dd-length (type-name)
  (dd-length (find-defstruct-description type-name)))

(defconstant layout-id-vector-fixed-capacity 7)
(defmacro calculate-extra-id-words (depthoid)
  ;; There are 1 or 2 ids per word depending on n-word-bytes.
  ;; We can always store IDs at depthoids 2,3,4,5,6,7,
  ;; so depthoid less than or equal to 7 needs no extra words.
  ;; 0 and 1 for T and STRUCTURE-OBJECT respectively are not stored.
  `(ceiling (max 0 (- ,depthoid ,layout-id-vector-fixed-capacity))
            ,(/ sb-vm:n-word-bytes 4)))

(declaim (inline layout-dd layout-info))
;; Use LAYOUT-DD to read LAYOUT-INFO if you want to assert that it is non-nil.
(defun layout-dd (layout)
  (the defstruct-description (layout-%info layout)))
(defun layout-info (layout)
  (let ((info (layout-%info layout)))
    (unless (listp info) info)))
(defun (setf layout-info) (newval layout)
  ;; The current value must be nil or a defstruct-description,
  ;; otherwise we'd clobber a non-nil slot list.
  (aver (not (consp (layout-%info layout))))
  (setf (layout-%info layout) newval))

#-sb-xc-host
(progn
(declaim (inline bitmap-start bitmap-nwords bitmap-all-taggedp))
(defun bitmap-start (layout)
  (+ (type-dd-length layout)
     (calculate-extra-id-words (layout-depthoid layout))))
(defun bitmap-nwords (layout)
  (declare (layout layout))
  (- (%instance-length layout)
     (calculate-extra-id-words (layout-depthoid layout))
     (type-dd-length layout)))
(defun bitmap-all-taggedp (layout)
  ;; All bitmaps have at least 1 word; read that first.
  (and (= (%raw-instance-ref/signed-word layout (bitmap-start layout))
          +layout-all-tagged+)
       ;; Then check that there are no additional words.
       (= (bitmap-nwords layout) 1)))
#+64-bit
(defmacro layout-length (layout) ; SETFable
  `(ldb (byte 16 16) (layout-flags ,layout)))
) ; end PROGN #-sb-xc-host

;;; True of STANDARD-OBJECT, which include generic functions.
(declaim (inline layout-for-pcl-obj-p))
(defun layout-for-pcl-obj-p (layout)
  (declare (type layout layout))
  (logtest (layout-flags layout) +pcl-object-layout-flag+))

(defun layout-classoid-name (x)
  (classoid-name (layout-classoid x)))

;;;; object types to represent classes

;;; An UNDEFINED-CLASSOID is a cookie we make up to stick in forward
;;; referenced layouts. Users should never see them.
(defstruct (undefined-classoid
            (:include classoid)
            (:constructor !alloc-undefined-classoid (%bits name))
            (:copier nil)))

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
(defstruct (built-in-classoid (:include classoid) (:copier nil)
                                    (:constructor !make-built-in-classoid))
  ;; the type we translate to on parsing. If NIL, then this class
  ;; stands on its own. Only :INITIALIZING for a period during cold
  ;; load.
  (translation nil :type (or null ctype (member :initializing)))
  (predicate (missing-arg) :type (sfunction (t) boolean) :read-only t))

(defstruct (condition-classoid (:include classoid)
                                (:constructor !alloc-condition-classoid (%bits name))
                                (:copier nil))
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
(defstruct (structure-classoid
             (:include classoid)
             (:constructor !alloc-structure-classoid (%bits name))
             (:copier nil)))

;;;; classoid namespace

;;; We use an indirection to allow forward referencing of class
;;; definitions with load-time resolution.
(defstruct (classoid-cell
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
(defstruct (standard-classoid
             (:include classoid)
             (:constructor !alloc-standard-classoid (%bits name pcl-class))
             (:copier nil))
  old-layouts)
;;; a metaclass for classes which aren't standardlike but will never
;;; change either.
(defstruct (static-classoid (:include classoid)
                             (:constructor !alloc-static-classoid (%bits name))
                             (:copier nil)))

(declaim (freeze-type built-in-classoid condition-classoid
                      standard-classoid static-classoid))

;;; Return the name of the global hashset that OBJ (a CTYPE instance)
;;; would be stored in, if it were stored in one.
;;; This is only for bootstrap, and not 100% precise as it does not know
;;; about *EQL-TYPE-CACHE* or *MEMBER/EQ-TYPE-HASHSET*
(defun ctype->hashset-sym (obj)
  (macrolet ((generate  ()
               (collect ((clauses))
                 (dolist (type-class *type-class-list*)
                   (dolist (instance-type (cdr type-class))
                     (clauses
                      (cons instance-type
                            (unless (member instance-type '(classoid named-type))
                              (symbolicate "*" instance-type "-HASHSET*"))))))
                 #+sb-xc-host
                 `(etypecase obj
                    ,@(mapcar (lambda (x) `(,(car x) ',(cdr x))) (clauses)))
                 ;; For cold-init, we need something guaranteed to work no matter the expansion
                 ;; of TYPEP. If this is called too early, then the optimized code for TYPEP
                 ;; (whatever it is) may fail. But Genesis is able to externalize an alist that
                 ;; maps #<layout> to symbol, and it's mostly ok to compare layouts by EQ here,
                 ;; but it fails on CLASSOID's subtypes, so recognize those specially.
                 #-sb-xc-host
                 (let ((alist (mapcar (lambda (x) (cons (find-layout (car x)) (cdr x)))
                                      (clauses))))
                   `(let ((cell (assoc (%instance-layout obj) ',alist)))
                      (cond (cell (cdr cell))
                            ((classoid-p obj) nil)
                            (t (bug "ctype dumping problem"))))))))
    (generate)))

(declaim (freeze-type ctype))

;;; Anything which performs TYPECASE over the type metatypes should occur
;;; after all sructures are frozen, otherwise we'll use the inefficient
;;; expansion of TYPECASE
;;; Copy X to the heap, give it a random hash, and if it is a MEMBER type
;;; then assert that all members are cacheable.
#+sb-xc-host
(defun copy-ctype (x)
  (let ((bits (logior (type-%bits x) (logand (ctype-random) +ctype-hash-mask+))))
    (etypecase x
      (member-type
       (!alloc-member-type bits (member-type-xset x) (member-type-fp-zeroes x))))))
#-sb-xc-host
(macrolet ((safe-member-type-elt-p (obj)
             `(or (not (sb-vm:is-lisp-pointer (get-lisp-obj-address ,obj)))
                  (heap-allocated-p ,obj))))
(defun copy-ctype (x &optional (flags 0))
  (declare (type ctype x))
  (declare (sb-c::tlab :system) (inline !new-xset))
  #.(cl:if (cl:and (cl:member :c-stack-is-control-stack sb-xc:*features*)
                   sb-ext:*stack-allocate-dynamic-extent*)
           '(aver (stack-allocated-p x)))
  (labels ((copy (x)
             ;; Return a heap copy of X if X was arena or stack-allocated.
             ;; I suspect it's quicker to copy always rather than conditionally.
             ;; The use for this is that supposing the user constructs a type specifier
             ;; like (DOUBLE-FLOAT (2.0) 4.0) where those numbers and the inner list
             ;; were constructed on an arena, they need to be copied.
             (etypecase x
               (number (sb-vm:copy-number-to-heap x))
               (cons (cons (copy (car x)) (copy (cdr x))))
               (symbol x)))
           (copy-xset (xset &aux (data (xset-data xset)))
             ;; MEMBER-TYPE is a problem because the members could be arena-allocated.
             ;; It would be easy enough to avoid entering some instances in a hashset, though
             ;; the larger issue is that it may be inserted into any number of other caches.
             ;; CLHS never says whether DX objects are or aren't legal in type specifiers.
             ;; I consider this "user error" as it seems to push the boundary of what should
             ;; be permissible, but we can do better than to cache data that are on the stack.
             ;; If the XSET is represented as a hash-table, we may have another issue
             ;; which is not dealt with here (hash-table in the arena)
             (cond ((listp data)
                    ;; the XSET can be empty if a MEMBER type contains only FP zeros.
                    ;; While we could use (load-time-value) to reference a constant empty xset
                    ;; there's really no point to doing that.
                    (collect ((elts))
                      (dolist (x data (!new-xset (elts) (xset-extra xset)))
                        (elts (cond ((numberp x) (sb-vm:copy-number-to-heap x))
                                    ((safe-member-type-elt-p x) x)
                                    ;; surely things will go haywire if this occurs
                                    (t (error "Off-heap MEMBER type member @ ~X"
                                              (get-lisp-obj-address x))))))))
                   ;; Huge MEMBER types are rare so I'm not going to worry too much,
                   ;; just check whether it's OK or not
                   ((and (loop for k being each hash-key of data
                               always (safe-member-type-elt-p k))
                         (heap-allocated-p data))
                    xset)
                   (t ; This could certainly be improved
                    (error "Off-heap MEMBER type members")))))
    (let ((bits (logior (type-%bits x) (logand (ctype-random) +ctype-hash-mask+) flags)))
      ;; These cases are in descending order of frequency of seek in the hashsets.
      ;; It matters only for backends that don't convert TYPECASE to a jump table.
      (etypecase x
        (values-type
         (!alloc-values-type bits (values-type-required x) (values-type-optional x)
                             (values-type-rest x)))
        (fun-type ; or FUN-DESIGNATOR-TYPE
         (let ((copy (!alloc-fun-type
                      bits (fun-type-required x) (fun-type-optional x) (fun-type-rest x)
                      (fun-type-keyp x) (fun-type-keywords x) (fun-type-allowp x)
                      (fun-type-wild-args x) (fun-type-returns x))))
           (%set-instance-layout copy (%instance-layout x))
           copy))
        (numeric-union-type
         (!alloc-numeric-union-type bits (numeric-union-type-aspects x)
                                    (map 'vector #'copy (numeric-union-type-ranges x))))
        (compound-type ; UNION or INTERSECTION
         (let ((copy (!alloc-union-type bits (compound-type-enumerable x)
                                        (compound-type-types x))))
           (%set-instance-layout copy (%instance-layout x))
           copy))
        (member-type
         (!alloc-member-type bits (copy-xset (member-type-xset x))
          (mapcar 'sb-vm:copy-number-to-heap (member-type-fp-zeroes x))))
        (array-type
         (!alloc-array-type bits (copy (array-type-dimensions x))
                            (array-type-complexp x) (array-type-element-type x)
                            (array-type-specialized-element-type x)))
        (hairy-type ; SATISFIES or UNKNOWN
         (let ((copy (!alloc-hairy-type bits (copy (hairy-type-specifier x)))))
           (%set-instance-layout copy (%instance-layout x))
           copy))
        (negation-type (!alloc-negation-type bits (negation-type-type x)))
        (constant-type (!alloc-constant-type bits (constant-type-type x)))
        (cons-type (!alloc-cons-type bits (cons-type-car-type x) (cons-type-cdr-type x)))
        (character-set-type
         (!alloc-character-set-type bits (copy (character-set-type-pairs x))))
        #+sb-simd-pack
        (simd-pack-type (!alloc-simd-pack-type bits (simd-pack-type-tag-mask x)))
        #+sb-simd-pack-256
        (simd-pack-256-type (!alloc-simd-pack-256-type bits (simd-pack-256-type-tag-mask x)))
        (alien-type-type (!alloc-alien-type-type bits (alien-type-type-alien-type x)))))))
) ; end  MACROLET

#-sb-xc-host
(progn
(define-load-time-global *!initial-ctypes* nil)
(defun preload-ctype-hashsets ()
  (dolist (pair (nreverse *!initial-ctypes*))
    (let ((instance (car pair))
          (container (symbol-value (cdr pair))))
      (cond ((hash-table-p container)
             (aver (member-type-p instance))
             ;; As of this writing there are only two EQL types to preload:
             ;; one is in the IR1-transform of FORMAT with stream (EQL T),
             ;; the other is CHECK-ARG-TYPE looking for (EQL DUMMY) type.
             (let ((key (first (member-type-members instance))))
               (aver (not (gethash key container)))
               (setf (gethash key container) instance)))
            (t
             (aver (not (hashset-find container instance))) ; instances are built bottom-up
             (hashset-insert container instance)))
      (labels ((ensure-interned-list (list hashset)
                 (let ((found (hashset-find hashset list)))
                   (when (and found (neq found list))
                     (bug "genesis failed to uniquify list-of-ctype in ~X"
                          (get-lisp-obj-address instance)))
                   (when (and list (not found))
                     (hashset-insert hashset list)))
                 (mapc #'check list))
               ;; Assert that looking for SUBPART finds nothing or finds itself
               (check (subpart &aux (hashset-symbol (ctype->hashset-sym subpart)))
                 (when hashset-symbol
                   (let* ((hashset (symbol-value hashset-symbol))
                          (found (hashset-find hashset subpart)))
                     (when (and found (neq found subpart))
                       (bug "genesis dumped bad instance within ~X"
                            (get-lisp-obj-address instance)))))))
        (etypecase instance
          ((or numeric-union-type member-type character-set-type ; nothing extra to do
           #+sb-simd-pack simd-pack-type #+sb-simd-pack-256 simd-pack-256-type
           hairy-type))
          (args-type
           (ensure-interned-list (args-type-required instance) *ctype-list-hashset*)
           (ensure-interned-list (args-type-optional instance) *ctype-list-hashset*)
           (awhen (args-type-rest instance) (check it))
           (when (fun-type-p instance)
             (aver (null (fun-type-keywords instance)))
             (check (fun-type-returns instance))))
          (cons-type
           (check (cons-type-car-type instance))
           (check (cons-type-cdr-type instance)))
          (array-type
           (check (array-type-element-type instance))
           (check (array-type-specialized-element-type instance)))
          (compound-type
           (ensure-interned-list (compound-type-types instance) *ctype-set-hashset*))
          (negation-type
           (check (negation-type-type instance)))))))
  #+sb-devel (setq *hashsets-preloaded* t))
(preload-ctype-hashsets))
