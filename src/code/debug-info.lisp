;;;; structures used for recording debugger information

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;;; flags for compiled debug variables

;;; FIXME: old CMU CL representation follows:
;;;    Compiled debug variables are in a packed binary representation in the
;;; DEBUG-FUN-VARS:
;;;    single byte of boolean flags:
;;;     uninterned name
;;;        packaged name
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

(def!constant compiled-debug-var-more-context-p         #b00000001)
(def!constant compiled-debug-var-more-count-p           #b00000010)
(def!constant compiled-debug-var-environment-live       #b00000100)
(def!constant compiled-debug-var-save-loc-p             #b00001000)
(def!constant compiled-debug-var-id-p                   #b00010000)
(def!constant compiled-debug-var-minimal-p              #b00100000)
(def!constant compiled-debug-var-deleted-p              #b01000000)
(def!constant compiled-debug-var-indirect-p             #b10000000)

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

(defparameter *compiled-code-location-kinds*
  #(:unknown-return :known-return :internal-error :non-local-exit
    :block-start :call-site :single-value-return :non-local-entry
    :step-before-vop))

;;;; DEBUG-FUN objects

(def!struct (debug-fun (:constructor nil)))

(def!struct (compiled-debug-fun (:include debug-fun)
                                #-sb-xc-host (:pure t))
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
  (name (missing-arg) :type (or simple-string cons symbol))
  ;; The kind of function (same as FUNCTIONAL-KIND):
  (kind nil :type (member nil :optional :external :toplevel :cleanup))
  ;; a description of variable locations for this function, in alphabetical
  ;; order by name; or NIL if no information is available
  ;;
  ;; The variable entries are alphabetically ordered. This ordering is
  ;; used in lifetime info to refer to variables: the first entry is
  ;; 0, the second entry is 1, etc. Variable numbers are *not* the
  ;; byte index at which the representation of the location starts.
  ;;
  ;; Each entry is:
  ;;   * a FLAGS value, which is a FIXNUM with various
  ;;     COMPILED-DEBUG-FUN-FOO bits set
  ;;   * the symbol which names this variable, unless debug info
  ;;     is minimal
  ;;   * the variable ID, when it has one
  ;;   * SC-offset of primary location, if it has one
  ;;   * SC-offset of save location, if it has one
  (vars nil :type (or simple-vector null))
  ;; a vector of the packed binary representation of the
  ;; COMPILED-DEBUG-BLOCKs in this function, in the order that the
  ;; blocks were emitted. The first block is the start of the
  ;; function. This slot may be NIL to save space.
  ;;
  ;; FIXME: The "packed binary representation" description in the
  ;; comment above is the same as the description of the old
  ;; representation of VARIABLES which doesn't work properly in SBCL
  ;; (because it doesn't transform correctly under package renaming).
  ;; Check whether this slot's data might have the same problem that
  ;; that slot's data did.
  (blocks nil :type (or (simple-array (unsigned-byte 8) (*)) null))
  ;; If all code locations in this function are in the same top level
  ;; form, then this is the number of that form, otherwise NIL. If
  ;; NIL, then each code location represented in the BLOCKS specifies
  ;; the TLF number.
  (tlf-number nil :type (or index null))
  (form-number nil :type (or index null))
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
  ;; OPTIONAL-ARGS
  ;;    Indicates that following unqualified args are optionals, not required.
  ;;
  ;; REST-ARG
  ;;    The following location holds the list of rest args.
  ;;
  ;; MORE-ARG
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
  #!-fp-and-pc-standard-save
  (return-pc (missing-arg) :type sc-offset)
  #!-fp-and-pc-standard-save
  (old-fp (missing-arg) :type sc-offset)
  ;; The earliest PC in this function at which the environment is properly
  ;; initialized (arguments moved from passing locations, etc.)
  (start-pc (missing-arg) :type index)
  ;; The start of elsewhere code for this function (if any.)
  (elsewhere-pc (missing-arg) :type index)
  (closure-save nil :type (or sc-offset null))
  #!+unwind-to-frame-and-call-vop
  (bsp-save nil :type (or sc-offset null)))

;;;; minimal debug function

;;; The minimal debug info format compactly represents debug-info for some
;;; cases where the other debug info (variables, blocks) is small enough so
;;; that the per-function overhead becomes relatively large. The minimal
;;; debug-info format can represent any function at level 0, and any fixed-arg
;;; function at level 1.
;;;
;;; In the minimal format, the debug functions and function map are
;;; packed into a single byte-vector which is placed in the
;;; COMPILED-DEBUG-INFO-FUN-MAP. Because of this, all functions in a
;;; component must be representable in minimal format for any function
;;; to actually be dumped in minimal format. The vector is a sequence
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
;;;    [...sequence of var-length ints holding sc-offsets of the return
;;;     value locations, if fixed return values are specified.]
;;;    return-pc location sc-offset (as var-length int)
;;;    old-fp location sc-offset (as var-length int)
;;;    [nfp location sc-offset (as var-length int), if nfp flag]
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

;;;; DEBUG SOURCE

;;; There is one per compiled file and one per function compiled at
;;; toplevel or loaded from source.
(def!struct (debug-source #-sb-xc-host (:pure t))
  ;; (This is one of those structures where IWBNI we had multiple
  ;; inheritance.  The first four slots describe compilation of a
  ;; file, the fifth and sixth compilation of a form processed by
  ;; EVAL, and the seventh and eigth all compilation units; and these
  ;; are orthogonal concerns that can combine independently.)

  ;; When the DEBUG-SOURCE describes a file, the file's namestring.
  ;; Otherwise, NIL.
  (namestring nil :type (or null string))
  ;; the universal time that the source was written, or NIL if
  ;; unavailable
  (created nil :type (or unsigned-byte null))
  ;; The FILE-POSITIONs of the truly top level forms read from this
  ;; file (if applicable). The vector element type will be chosen to
  ;; hold the largest element.
  (start-positions nil :type (or (simple-array * (*)) null))

  ;; For functions processed by EVAL (including EVAL-WHEN and LOAD on
  ;; a source file), the source form.
  (form nil :type list)
  ;; This is the function whose source is the form.
  (function nil)

  ;; the universal time that the source was compiled
  (compiled (missing-arg) :type unsigned-byte)
  ;; Additional information from (WITH-COMPILATION-UNIT (:SOURCE-PLIST ...))
  (plist *source-plist*))

;;;; DEBUG-INFO structures

(def!struct debug-info
  ;; Some string describing something about the code in this component.
  (name (missing-arg) :type t)
  ;; A DEBUG-SOURCE structure describing where the code for this
  ;; component came from, in the order that forms were read.
  (source nil))

(def!struct (compiled-debug-info
             (:include debug-info)
             #-sb-xc-host (:pure t))
  ;; a SIMPLE-VECTOR of alternating DEBUG-FUN objects and fixnum
  ;; PCs, used to map PCs to functions, so that we can figure out what
  ;; function we were running in. Each function is valid between the
  ;; PC before it (inclusive) and the PC after it (exclusive). The PCs
  ;; are in sorted order, to allow binary search. We omit the first
  ;; and last PC, since their values are 0 and the length of the code
  ;; vector.
  ;;
  ;; KLUDGE: PC's can't always be represented by FIXNUMs, unless we're
  ;; always careful to put our code in low memory. Is that how it
  ;; works? Would this break if we used a more general memory map? --
  ;; WHN 20000120
  (fun-map (missing-arg) :type simple-vector :read-only t))

(defvar *!initial-debug-sources*)

(defun !debug-info-cold-init ()
  (let ((now (get-universal-time)))
    (dolist (debug-source *!initial-debug-sources*)
      (let* ((namestring (debug-source-namestring debug-source))
             (timestamp (file-write-date namestring)))
        (setf (debug-source-created debug-source) timestamp
              (debug-source-compiled debug-source) now)))))

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
             #-no-ansi-print-object
             (:print-object (lambda (s stream)
                              (print-unreadable-object (s stream :type t)
                                (princ (file-info-name s) stream)))))
  ;; If a file, the truename of the corresponding source file. If from
  ;; a Lisp form, :LISP. If from a stream, :STREAM.
  (name (missing-arg) :type (or pathname (eql :lisp)))
  ;; the external format that we'll call OPEN with, if NAME is a file.
  (external-format nil)
  ;; the defaulted, but not necessarily absolute file name (i.e. prior
  ;; to TRUENAME call.) Null if not a file. This is used to set
  ;; *COMPILE-FILE-PATHNAME*, and if absolute, is dumped in the
  ;; debug-info.
  (untruename nil :type (or pathname null))
  ;; the file's write date (if relevant)
  (write-date nil :type (or unsigned-byte null))
  ;; parallel vectors containing the forms read out of the file and
  ;; the file positions that reading of each form started at (i.e. the
  ;; end of the previous form)
  (forms (make-array 10 :fill-pointer 0 :adjustable t) :type (vector t))
  (positions (make-array 10 :fill-pointer 0 :adjustable t) :type (vector t))
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

;;; The SOURCE-INFO structure provides a handle on all the source
;;; information for an entire compilation.
(defstruct (source-info
             #-no-ansi-print-object
             (:print-object (lambda (s stream)
                              (print-unreadable-object
                                  (s stream :type t :identity t))))
             (:copier nil))
  ;; the UT that compilation started at
  (start-time (get-universal-time) :type unsigned-byte)
  ;; the IRT that compilation started at
  (start-real-time (get-internal-real-time) :type unsigned-byte)
  ;; the FILE-INFO structure for this compilation
  (file-info nil :type (or file-info null))
  ;; the stream that we are using to read the FILE-INFO, or NIL if
  ;; no stream has been opened yet
  (stream nil :type (or stream null))
  ;; for coalescing DEFINITION-SOURCE-LOCATION of effectively toplevel forms
  ;; inside one truly toplevel form.
  (last-defn-source-loc)
  ;; if the current compilation is recursive (e.g., due to EVAL-WHEN
  ;; processing at compile-time), the invoking compilation's
  ;; source-info.
  (parent nil :type (or source-info null)))
