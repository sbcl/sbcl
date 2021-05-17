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

(defconstant compiled-debug-var-more-context-p         #b00000001)
(defconstant compiled-debug-var-more-count-p           #b00000010)
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

(defconstant debug-info-var-deleted -1)
(defconstant debug-info-var-rest -2)
(defconstant debug-info-var-more -3)
(defconstant debug-info-var-optional -4)
(defconstant debug-info-var-supplied-p -5)


;;;; DEBUG-FUN objects

(def!struct (debug-fun (:constructor nil)
                       (:copier nil)))

(def!struct (compiled-debug-fun (:include debug-fun)
                                (:copier nil)
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
  ;; a description of variable locations for this function, in alphabetical
  ;; order by name; or NIL if no information is available
  ;; If only one variable is encoded then it's stored as is without a vector.
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
  ;; Can either be a single value or a vector for multiple values.
  (vars nil)
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
  (blocks nil :type (or (simple-array (unsigned-byte 8) (*))
                        (simple-array (signed-byte 8) (*))
                        null))
  ;; a vector describing the variables that the argument values are
  ;; stored in within this function. The locations are represented by
  ;; the ordinal number of the entry in the VARIABLES slot value. The
  ;; locations are in the order that the arguments are actually passed
  ;; in, but special negative numbers can be interspersed to indicate
  ;; the original call syntax:
  ;;
  ;;  DEBUG-INFO-VAR-DELETED
  ;;    There was an argument to the function in this position, but it was
  ;;    deleted due to lack of references. The value cannot be recovered.
  ;;
  ;; DEBUG-INFO-VAR-SUPPLIED-P
  ;;    The following location is the supplied-p value for the preceding
  ;;    keyword or optional.
  ;;
  ;; DEBUG-INFO-VAR-OPTIONAL
  ;;    Indicates that following unqualified args are optionals, not required.
  ;;
  ;; DEBUG-INFO-VAR-REST
  ;;    The following location holds the list of rest args.
  ;;
  ;; DEBUG-INFO-VAR-MORE
  ;;    The following two locations are the more arg context and count.
  ;;
  ;; <symbol>
  ;;    The following location is the value of the &KEY argument with the
  ;;    specified name.
  ;;
  ;; This may be NIL to save space. If no symbols are present, then
  ;; this will be represented with an I-vector with sufficiently large
  ;; element type. If this is :MINIMAL, then this means that the
  ;; VARIABLES are all required arguments, and are in the order they
  ;; appear in the VARIABLES vector. In other words, :MINIMAL stands
  ;; in for a vector where every element holds its index.
  ;;
  ;; Can either be a single value or a vector for multiple values.
  (arguments nil)
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
  (return-pc (missing-arg) :type sc+offset)
  #-fp-and-pc-standard-save
  (return-pc-pass (missing-arg) :type sc+offset)
  #-fp-and-pc-standard-save
  (old-fp (missing-arg) :type sc+offset)
  ;; An integer which contains between 4 and 6 varint-encoded fields:
  ;; START-PC -
  ;; The earliest PC in this function at which the environment is properly
  ;; initialized (arguments moved from passing locations, etc.)
  ;; ELSEWHERE-PC -
  ;; FORM-NUMBER
  ;; OFFSET
  ;; The start of elsewhere code for this function (if any.)
  ;; CLOSURE-SAVE, and BSP-SAVE.
  (encoded-locs (missing-arg) :type unsigned-byte :read-only t)
  (next))

(defun cdf-encode-locs (start-pc elsewhere-pc
                        form-number offset
                        closure-save
                        #+unwind-to-frame-and-call-vop bsp-save
                        #-fp-and-pc-standard-save lra-saved-pc
                        #-fp-and-pc-standard-save cfp-saved-pc)
  (dx-let ((bytes (make-array (* 8 4) :fill-pointer 0
                                      :element-type '(unsigned-byte 8))))
    ;; OFFSET and ELSEWHERE are encoded first so that the C backtrace logic
    ;; need not skip over all the other packed fields.
    (write-var-integer offset bytes)
    (write-var-integer elsewhere-pc bytes)
    (write-var-integer form-number bytes)
    (write-var-integer (- start-pc offset) bytes)
    #+unwind-to-frame-and-call-vop
    (write-var-integer (if bsp-save (1+ (sc+offset-offset bsp-save)) 0)
                       bytes)
    #-fp-and-pc-standard-save
    (progn
      (write-var-integer lra-saved-pc bytes)
      (write-var-integer cfp-saved-pc bytes))
    ;; More often the BSP-SAVE is non-null than CLOSURE-SAVE is non-null,
    ;; so the encoding is potentially smaller with CLOSURE-SAVE being last.
    (when closure-save
      (write-var-integer (1+ (sc+offset-offset closure-save)) bytes))
    (integer-from-octets bytes)))

(defun cdf-decode-locs (cdf)
  (let ((encoding (compiled-debug-fun-encoded-locs cdf))
        (input-pointer 0))
    (flet ((decode-varint (&aux (accumulator 0) (shift 0))
             (loop
              (let ((byte (ldb (byte 8 input-pointer) encoding)))
                (incf input-pointer 8)
                (setf accumulator (logior accumulator (ash (logand byte #x7f) shift)))
                (incf shift 7)
                (unless (logtest byte #x80) (return accumulator))))))
      (let* ((offset (decode-varint))
             (elsewhere-pc (decode-varint))
             (form-number (decode-varint))
             (start-pc (+ offset (decode-varint)))
             #+unwind-to-frame-and-call-vop
             ;; 0 -> NULL, 1 -> 0, ...
             (bsp-save (let ((i (decode-varint)))
                         (unless (zerop i)
                           (make-sc+offset sb-vm:control-stack-sc-number (1- i)))))
             #-fp-and-pc-standard-save
             (lra-saved-pc (decode-varint))
             #-fp-and-pc-standard-save
             (cfp-saved-pc (decode-varint))
             (closure-save (let ((i (decode-varint)))
                             (unless (zerop i)
                               (make-sc+offset sb-vm:control-stack-sc-number (1- i))))))
        (values start-pc elsewhere-pc
                form-number offset
                closure-save
                #-fp-and-pc-standard-save lra-saved-pc
                #-fp-and-pc-standard-save cfp-saved-pc
                #+unwind-to-frame-and-call-vop bsp-save)))))

(macrolet ((def (&rest names)
             `(progn
                ,@(loop
                     for name in names
                     for index from 0
                     collect
                       `(defun ,name (cdf)
                          (nth-value ,index (cdf-decode-locs cdf)))))))
  (def
    compiled-debug-fun-start-pc
    compiled-debug-fun-elsewhere-pc
    compiled-debug-fun-form-number
    compiled-debug-fun-offset
    ;; Most compiled-debug-funs don't need these
    compiled-debug-fun-closure-save
    #-fp-and-pc-standard-save compiled-debug-fun-lra-saved-pc
    #-fp-and-pc-standard-save compiled-debug-fun-cfp-saved-pc
    #+unwind-to-frame-and-call-vop compiled-debug-fun-bsp-save))

;;; If you add more subtypes here, be sure to amend the set of
;;; predefined layout FOP codes in src/code/fop
(def!struct (compiled-debug-fun-optional (:include compiled-debug-fun)
                                         (:pure t)
                                         (:copier nil)
                                         (:predicate nil)))
(def!struct (compiled-debug-fun-more (:include compiled-debug-fun)
                                     (:pure t)
                                     (:copier nil)
                                     (:predicate nil)))
(def!struct (compiled-debug-fun-external (:include compiled-debug-fun)
                                         (:pure t)
                                         (:copier nil)
                                         (:predicate nil)))
(def!struct (compiled-debug-fun-toplevel (:include compiled-debug-fun)
                                         (:pure t)
                                         (:copier nil)
                                         (:predicate nil)))
(def!struct (compiled-debug-fun-cleanup (:include compiled-debug-fun)
                                        (:pure t)
                                        (:copier nil)
                                        (:predicate nil)))

(defun compiled-debug-fun-ctor (kind)
  (ecase kind
    (:optional #'make-compiled-debug-fun-optional)
    (:more #'make-compiled-debug-fun-more)
    (:external #'make-compiled-debug-fun-external)
    (:toplevel #'make-compiled-debug-fun-toplevel)
    (:cleanup #'make-compiled-debug-fun-cleanup)
    ((nil) #'make-compiled-debug-fun)))

(defun compiled-debug-fun-kind (debug-fun)
  (etypecase debug-fun
    (compiled-debug-fun-optional :optional)
    (compiled-debug-fun-more :more)
    (compiled-debug-fun-external :external)
    (compiled-debug-fun-toplevel :toplevel)
    (compiled-debug-fun-cleanup :cleanup)
    (compiled-debug-fun nil)))


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
  ;; Additional information from (WITH-COMPILATION-UNIT (:SOURCE-PLIST ...))
  (plist *source-plist* :read-only t))

;;;; DEBUG-INFO structures

(def!struct (debug-info
             (:copier nil))
  ;; Some string describing something about the code in this component.
  (name (missing-arg) :type t :read-only t)
  ;; A DEBUG-SOURCE structure describing where the code for this
  ;; component came from, in the order that forms were read.
  (source nil))

(def!struct (compiled-debug-info
             (:include debug-info)
             (:copier nil)
             (:pure t))
  ;; COMPILED-DEBUG-FUNs linked through COMPILED-DEBUG-FUN-NEXT
  (fun-map (missing-arg) :type compiled-debug-fun)
  ;; Location contexts
  ;; A (simple-array * (*)) or a context if there's only one context.
  (contexts nil :type t :read-only t)
  ;; Packed integers. Also can be a cons of that plus an alist which
  ;; maps SB-C::COMPILED-DEBUG-FUN to SB-DI::COMPILED-DEBUG-FUN instances.
  (tlf-num+offset (missing-arg) :type (or integer cons)))

;;; The TLF-NUMBER and CHAR-OFFSET of a compiled-debug-info can each be NIL,
;;; but aren't often. However, to allow that, convert NIL to 0 and non-nil
;;; value N to N+1.
(defun pack-tlf-num+offset (tlf-number char-offset)
  (with-adjustable-vector (v)
    (write-var-integer (if tlf-number (1+ tlf-number) 0) v)
    (write-var-integer (if char-offset (1+ char-offset) 0) v)
    (integer-from-octets v)))

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
                                (princ (file-info-truename s) stream)))))
  ;; If a file, the truename of the corresponding source file. If from
  ;; a Lisp form, :LISP. In COMPILE-FILE, this gets filled lazily
  ;; after the file gets opened.
  (truename nil :type (or pathname null (eql :lisp)))
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
