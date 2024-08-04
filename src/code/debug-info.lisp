;;;; structures used for recording debugger information

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;;; flags for compiled debug variables

;;;    Compiled debug variables are in a packed binary representation in the
;;; DEBUG-FUN-VARS:
;;;    single byte of boolean flags:
;;;     uninterned name
;;;     packaged name
;;;     environment-live
;;;     has distinct save location
;;;     has ID (name not unique in this fun)
;;;     minimal debug-info argument (name generated as ARG-0, ...)
;;;     deleted: placeholder for unused minimal argument
;;;    [name length in bytes (as var-length integer), if not minimal]
;;;    [...name bytes..., if not minimal]
;;;    [if packaged, var-length integer that is package name length]
;;;     ...package name bytes...]
;;;    [If has ID, ID as var-length integer]
;;;    SC-Offset of primary location (as var-length integer)
;;;    [If has save SC, SC-OFFSET of save location (as var-length integer)]

(defconstant compiled-debug-var-uninterned             #b00000001)
(defconstant compiled-debug-var-packaged               #b00000010)
(defconstant compiled-debug-var-environment-live       #b00000100)
(defconstant compiled-debug-var-save-loc-p             #b00001000)
(defconstant compiled-debug-var-same-name-p            #b00010000)
(defconstant compiled-debug-var-minimal-p              #b00100000)
(defconstant compiled-debug-var-deleted-p              #b01000000)
(defconstant compiled-debug-var-indirect-p             #b10000000)

;;;; compiled debug blocks
;;;;
;;;;    Compiled debug blocks are in a packed binary representation in the
;;;; DEBUG-FUN-BLOCKS:
;;;;    number of locations in this block
;;;;    kind of first location (single byte)
;;;;    delta from previous PC (or from 0 if first location in function.)
;;;;    [offset of first top level form, if no function TLF-NUMBER]
;;;;    form number of first source form
;;;;    first live mask (length in bytes determined by number of VARIABLES)
;;;;    ...more <kind, delta, top level form offset, form-number, live-set>
;;;;       tuples...

(defconstant-eqx +compiled-code-location-kinds+
    #(:unknown-return :known-return :internal-error :non-local-exit
      :block-start :call-site :single-value-return :non-local-entry)
  #'equalp)

(eval-when (:compile-toplevel)
  (assert (<= (integer-length (1- (length +compiled-code-location-kinds+))) 3)))

;;; Location flags, encoded in the low 4 bits of loction kind byte
(defconstant compiled-code-location-stepping         (ash #b00001 3))
(defconstant compiled-code-location-context          (ash #b00010 3))
(defconstant compiled-code-location-live             (ash #b00100 3))
(defconstant compiled-code-location-zero-form-number (ash #b01000 3))

;;; Means the previous live-set is the same. Since -live is implied,
;;; the -live bit is repurposed to mean that the form-number is also
;;; the same.
(defconstant compiled-code-location-equal-live       (ash #b10000 3))


;;;; DEBUG-FUN objects

(defstruct (debug-fun (:constructor nil)
                      (:copier nil)))

(defstruct (compiled-debug-fun (:include debug-fun)
                               (:copier nil)
                               #-sb-xc-host
                               (:pure t))
  ;; KLUDGE: Courtesy of more than a decade of, ah, organic growth in
  ;; CMU CL, there are two distinct -- but coupled -- mechanisms to
  ;; finding the name of a function. The slot here is one mechanism
  ;; (used in CMU CL to look up names in the debugger, e.g. in
  ;; BACKTRACE). The other mechanism is the NAME slot in function
  ;; primitive objects (used in CMU CL to look up names elsewhere,
  ;; e.g. in CL:FUNCTION-LAMBDA-EXPRESSION and in CL:DESCRIBE).
  ;;
  ;; They're coupled by the way that DEBUG-FUN objects are looked up.
  ;; A list of DEBUG-FUN objects is maintained for each COMPONENT. To
  ;; figure out which DEBUG-FUN object corresponds to your FUNCTION
  ;; object, you compare the name values of each. -- WHN 2001-12-20
  (name (missing-arg) :type (or simple-string cons symbol) :read-only t)
  ;; The kind of function (same as FUNCTIONAL-KIND):
  (kind nil :type (member nil :optional :external :toplevel :cleanup :more))
  ;; a vector of the packed binary representation of variable
  ;; locations in this function. These are in alphabetical order by
  ;; name. This ordering is used in lifetime info to refer to
  ;; variables: the first entry is 0, the second entry is 1,
  ;; etc. Variable numbers are *not* the byte index at which the
  ;; representation of the location starts. This slot may be NIL to
  ;; save space.
  (vars nil :type (or (simple-array (unsigned-byte 8) (*)) null))
  ;; a vector of the packed binary representation of the
  ;; COMPILED-DEBUG-BLOCKs in this function, in the order that the
  ;; blocks were emitted. The first block is the start of the
  ;; function. This slot may be NIL to save space.
  (blocks nil :type (or (simple-array (unsigned-byte 8) (*))
                        ;; hack to pack the fun form-number here on
                        ;; low debug.
                        integer
                        null))
  ;; If all code locations in this function are in the same top level
  ;; form, then this is the number of that form, otherwise NIL. If
  ;; NIL, then each code location represented in the BLOCKS specifies
  ;; the TLF number.
  (tlf-number nil :type (or index null))
  ;; a vector describing the variables that the argument values are
  ;; stored in within this function. The locations are represented by
  ;; the ordinal number of the entry in the VARIABLES slot value. The
  ;; locations are in the order that the arguments are actually passed
  ;; in, but special marker symbols can be interspersed to indicate
  ;; the original call syntax:
  ;;
  ;; DELETED
  ;;    There was an argument to the function in this position, but it was
  ;;    deleted due to lack of references. The value cannot be recovered.
  ;;
  ;; SUPPLIED-P
  ;;    The following location is the supplied-p value for the preceding
  ;;    keyword or optional.
  ;;
  ;; OPTIONAL
  ;;    Indicates that following unqualified args are optionals, not required.
  ;;
  ;; REST
  ;;    The following location holds the list of rest args.
  ;;
  ;; MORE
  ;;    The following two locations are the more arg context and count.
  ;;
  ;; <any other symbol>
  ;;    The following location is the value of the &KEY argument with the
  ;;    specified name.
  ;;
  ;; This may be NIL to save space. If no symbols are present, then
  ;; this will be represented with an I-vector with sufficiently large
  ;; element type. If this is :MINIMAL, then this means that the
  ;; VARIABLES are all required arguments, and are in the order they
  ;; appear in the VARIABLES vector. In other words, :MINIMAL stands
  ;; in for a vector where every element holds its index.
  (arguments nil :type (or (simple-array * (*)) (member :minimal nil)))
  ;; There are three alternatives for this slot:
  ;;
  ;; a VECTOR
  ;;    A vector of SC-OFFSETS describing the return locations. The
  ;;    vector element type is chosen to hold the largest element.
  ;;
  ;; :STANDARD
  ;;    The function returns using the standard unknown-values convention.
  ;;
  ;; :FIXED
  ;;    The function returns using the fixed-values convention, but
  ;;    in order to save space, we elected not to store a vector.
  (returns :fixed :type (or (simple-array * (*)) (member :standard :fixed)))
  ;; SC-OFFSETs describing where the return PC and return FP are kept.
  #-fp-and-pc-standard-save
  (return-pc (missing-arg) :type sc+offset :read-only t)
  #-fp-and-pc-standard-save
  (return-pc-pass (missing-arg) :type sc+offset :read-only t)
  #-fp-and-pc-standard-save
  (old-fp (missing-arg) :type sc+offset :read-only t)
  #-fp-and-pc-standard-save
  (lra-saved-pc (missing-arg) :type sc+offset :read-only t)
  #-fp-and-pc-standard-save
  (cfp-saved-pc (missing-arg) :type sc+offset :read-only t)
  (closure-save (missing-arg) :type (or sc+offset null) :read-only t)
  #+unwind-to-frame-and-call-vop
  (bsp-save (missing-arg) :type (or sc+offset null) :read-only t)
  ;; The earliest PC in this function at which the environment is
  ;; properly initialized (arguments moved from passing locations,
  ;; etc.)
  (start-pc (missing-arg) :type index :read-only t)
  ;; The start of elsewhere code for this function (if any.)
  (elsewhere-pc (missing-arg) :type index :read-only t))


;;;; packed debug function

;;; The packed debug info format compactly represents debug-info for
;;; all debug levels.
;;;
;;; In the packed format, the debug functions and function map are
;;; packed into a single byte-vector which is placed in the
;;; COMPILED-DEBUG-INFO-FUN-MAP. The vector is a sequence
;;; of records in this format:
;;;    name representation + kind + return convention (single byte)
;;;    bit flags (single byte)
;;;     setf, nfp, variables
;;;    [package name length (as var-length int), if name is packaged]
;;;    [...package name bytes, if name is packaged]
;;;    [name length (as var-length int), if there is a name]
;;;    [...name bytes, if there is a name]
;;;    [variables length (as var-length int), if variables flag]
;;;    [...bytes holding variable descriptions]
;;;     If variables are dumped (level 1), then the variables are all
;;;     arguments (in order) with the minimal-arg bit set.
;;;    [If returns is specified, then the number of return values]
;;;    [...sequence of var-length ints holding SC+OFFSETs of the return
;;;     value locations, if fixed return values are specified.]
;;;    return-pc location SC+OFFSET (as var-length int)
;;;    old-fp location SC+OFFSET (as var-length int)
;;;    [nfp location SC+OFFSET (as var-length int), if nfp flag]
;;;    code-start-pc (as a var-length int)
;;;     This field implicitly encodes start of this function's code in the
;;;     function map, as a delta from the previous function's code start.
;;;     If the first function in the component, then this is the delta from
;;;     0 (i.e. the absolute offset.)
;;;    start-pc (as a var-length int)
;;;     This encodes the environment start PC as an offset from the
;;;     code-start PC.
;;;    elsewhere-pc
;;;     This encodes the elsewhere code start for this function, as a delta
;;;     from the previous function's elsewhere code start. (i.e. the
;;;     encoding is the same as for code-start-pc.)

;;; ### For functions with XEPs, name could be represented more simply
;;; and compactly as some sort of info about with how to find the
;;; function entry that this is a function for. Actually, you really
;;; hardly need any info. You can just chain through the functions in
;;; the component until you find the right one. Well, I guess you need
;;; to at least know which function is an XEP for the real function
;;; (which would be useful info anyway).

;;; The following are definitions of bit-fields in the first byte of
;;; the packed debug function:
(defconstant-eqx packed-debug-fun-kind-byte (byte 3 2) #'equalp)
(defconstant-eqx packed-debug-fun-kinds
  #(nil :optional :external :toplevel :cleanup :more) #'equalp)
(defconstant packed-debug-fun-returns-standard 0)
(defconstant packed-debug-fun-returns-specified 1)
(defconstant packed-debug-fun-returns-fixed 2)
(defconstant-eqx packed-debug-fun-returns-byte (byte 2 5) #'equalp)

;;; The following are bit-flags in the second byte of the packed debug
;;; function:

;;; If true, variables (hence arguments) have been dumped.
(defconstant packed-debug-fun-variables-bit (ash 1 0))

;;; If true, blocks have been dumped.
(defconstant packed-debug-fun-blocks-bit (ash 1 1))

;;; If true, non-minimal arguments have been dumped.
(defconstant packed-debug-fun-non-minimal-arguments-bit (ash 1 2))

;;; If true, a TLF number has been dumped.
(defconstant packed-debug-fun-tlf-number-bit (ash 1 3))

;;; If true, the closure save location has been dumped.
(defconstant packed-debug-fun-closure-save-loc-bit (ash 1 4))

;;; If true, the bsp save location has been dumped.
#+unwind-to-frame-and-call-vop
(defconstant packed-debug-fun-bsp-save-loc-bit (ash 1 5))


(defconstant packed-debug-fun-previous-name (ash 1 6))

;;; The following are codes for the marker symbols used to indicate
;;; call syntax.
(defconstant packed-debug-fun-arg-deleted 0)
(defconstant packed-debug-fun-arg-supplied-p 1)
(defconstant packed-debug-fun-arg-optional 2)
(defconstant packed-debug-fun-arg-rest 3)
(defconstant packed-debug-fun-arg-more 4)

(defconstant packed-debug-fun-key-arg-keyword 5)
(defconstant packed-debug-fun-key-arg-packaged 6)
(defconstant packed-debug-fun-key-arg-uninterned 7)

;;; The offset that argument indices are dumped relative to. This must
;;; not overlap the other call syntax codes.
(defconstant packed-debug-fun-arg-index-offset 8)


;;;; DEBUG SOURCE

;;; There is one per compiled file and one per function compiled at
;;; toplevel or loaded from source.
(def!struct (debug-source (:pure t)
                          (:copier nil))
  ;; When the DEBUG-SOURCE describes a file, the file's namestring.
  ;; Otherwise, NIL.
  (namestring nil :type (or null string))
  ;; the universal time that the source was written, or NIL if
  ;; unavailable
  (created nil :type (or unsigned-byte null))
  ;; The FILE-POSITIONs of the truly top level forms read from this
  ;; file (if applicable). The vector element type will be chosen to
  ;; hold the largest element.
  (start-positions nil :type (or (simple-array * (*)) null) :read-only t)
  ;; Additional information from (WITH-COMPILATION-UNIT (:SOURCE-PLIST ...))
  (plist *source-plist* :read-only t))

;;;; DEBUG-INFO structures

(def!struct (debug-info
             (:constructor nil)
             (:copier nil))
  ;; Some string describing something about the code in this component.
  (name (missing-arg) :type t :read-only t)
  ;; A DEBUG-SOURCE structure describing where the code for this
  ;; component came from, in the order that forms were read.
  (source nil))

(def!struct (compiled-debug-info
             (:include debug-info)
             (:constructor !make-compiled-debug-info
                           (name package fun-map contexts rest))
             (:copier nil)
             (:pure t))
  ;; The package that DEBUG-FUN-VARS were dumped relative
  ;; to. Locations that aren't packaged are in this package.
  (package (missing-arg) :type package :read-only t)
  ;; A sequence of packed debug functions in a packed binary
  ;; representation.
  ;;
  ;; When unpacked, a SIMPLE-VECTOR that alternates DEBUG-FUN
  ;; structures and fixnum PCs. The function is valid between the PC
  ;; before it (inclusive) and the PC after it (exclusive). The PCs
  ;; are in sorted order, so we can binary-search. We omit the first
  ;; and last PC, since their values are 0 and the length of the code
  ;; vector.
  (fun-map (missing-arg) :type (or (simple-array (unsigned-byte 8) (*))
                                   (simple-array (signed-byte 8) (*))) :read-only t)
  ;; Location contexts
  ;; A (simple-array * (*)) or a context if there's only one context.
  (contexts nil :type t :read-only t)
  (rest))

;;;; file reading
;;;;
;;;; When reading from a file, we have to keep track of some source
;;;; information. We also exploit our ability to back up for printing
;;;; the error context and for recovering from errors.
;;;;
;;;; The interface we provide to this stuff is the stream-oid
;;;; SOURCE-INFO structure. The bookkeeping is done as a side effect
;;;; of getting the next source form.

;;; A FILE-INFO structure holds all the source information for a
;;; given file.
(defstruct (file-info
             (:copier nil)
             (:print-object (lambda (s stream)
                              (print-unreadable-object (s stream :type t)
                                (princ (or (file-info-pathname s) (file-info-%truename s))
                                       stream)))))
  ;; If a file, the truename of the corresponding source file. If from
  ;; a Lisp form, :LISP. In COMPILE-FILE, this gets filled lazily
  ;; after the file gets opened.
  ;; Can also be :DEFER if you want to lazily populate the slot,
  ;; or :FAIL if you want to prevent use of truenames.
  (%truename nil :type (or pathname null (member :lisp :defer :fail)))
  ;; the external format that we'll call OPEN with, if NAME is a file.
  (external-format nil  :read-only t)
  ;; the defaulted, but not necessarily absolute file name (i.e. prior
  ;; to TRUENAME call.) Null if not a file. This is used to set
  ;; *COMPILE-FILE-PATHNAME*, and if absolute (a harmful constraint to be sure),
  ;; is dumped in the debug-info.
  (pathname nil :type (or pathname null) :read-only t)
  ;; the file's write date (if relevant)
  (write-date nil :type (or unsigned-byte null)  :read-only t)
  ;; parallel vectors containing the forms read out of the file and
  ;; the file positions that reading of each form started at (i.e. the
  ;; end of the previous form)
  (forms (make-array 10 :fill-pointer 0 :adjustable t) :type (vector t)
                                                       :read-only t)
  (positions (make-array 10 :fill-pointer 0 :adjustable t) :type (vector t)
                                                           :read-only t)
  ;; A vector of character ranges than span each subform in the TLF,
  ;; reset to empty for each one, updated by form-tracking-stream-observer.
  (subforms nil :type (or null (vector t)) :read-only t)
  ;; A list of objects about which the compile may/would/should have signaled
  ;; a style-warning in the :compile-toplevel situation, so we don't do it
  ;; again in the :load-toplevel situation.
  ;; This is a somewhat useless thing to track, but arguably
  ;; the "&OPTIONAL and &KEY" warning is quite annoying to see repeated.
  ;; And I doubt it changes anyone's mind about coding style anyway.
  ;; Typically this matters for DEFTYPE and DEFMACRO.
  (style-warning-tracker nil :type list))

(defun file-info-truename (x)
  (case (file-info-%truename x)
    (:defer (setf (file-info-%truename x) (truename (file-info-pathname x))))
    (:fail (error "Don't inquire FILE-INFO-TRUENAME"))
    (t (file-info-%truename x))))

;;; The SOURCE-INFO structure provides a handle on all the source
;;; information for an entire compilation.
(defstruct (source-info
             (:print-object (lambda (s stream)
                              (print-unreadable-object
                                  (s stream :type t :identity t))))
             (:copier nil))
  ;; the IRT that compilation started at
  (start-real-time (get-internal-real-time) :type unsigned-byte :read-only t)
  ;; the FILE-INFO structure for this compilation
  (file-info nil :type (or file-info null) :read-only t)
  ;; the stream that we are using to read the FILE-INFO, or NIL if
  ;; no stream has been opened yet
  (stream nil :type (or stream null))
  ;; for coalescing DEFINITION-SOURCE-LOCATION of effectively toplevel forms
  ;; inside one truly toplevel form.
  (last-defn-source-loc)
  ;; if the current compilation is recursive (e.g., due to EVAL-WHEN
  ;; processing at compile-time), the invoking compilation's
  ;; source-info.
  ;; KLUDGE: expressing this as (OR NULL SOURCE-INFO) rather than
  ;; the reverse avoids a warning from PARSE-1-DSD.
  ;; The compiler really needs to be made more aware of
  ;; some issues involving recursive structures.
  (parent nil :type (or null source-info) :read-only t))
