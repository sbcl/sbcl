;;;; stuff that knows about dumping FASL files

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;;; fasl dumper state

;;; The FASL-OUTPUT structure represents everything we need to
;;; know about dumping to a fasl file. (We need to objectify the
;;; state because the fasdumper must be reentrant.)
(defstruct (fasl-output
            (:print-object (lambda (x s)
                             (print-unreadable-object (x s :type t)
                               (prin1 (namestring (fasl-output-stream x))
                                      s))))
            (:copier nil))
  ;; the stream we dump to
  (stream (missing-arg) :type stream)
  ;; scratch space for computing varint encodings
  ;; FIXME: can't use the theoretical max of 10 bytes
  ;; due to constraint in WRITE-VAR-INTEGER.
  (varint-buf (make-array 10 :element-type '(unsigned-byte 8) :fill-pointer t))
  ;; hashtables we use to keep track of dumped constants so that we
  ;; can get them from the table rather than dumping them again. The
  ;; SIMILAR-TABLE is used for lists and strings, and the EQ-TABLE is
  ;; used for everything else. We use a separate EQ table to avoid
  ;; performance pathologies with objects for which SIMILAR
  ;; degenerates to EQL. Everything entered in the SIMILAR table is
  ;; also entered in the EQ table.
  (similar-table (make-similarity-table) :type hash-table :read-only t)
  (eq-table (make-hash-table :test 'eq) :type hash-table :read-only t)
  ;; the INSTANCE table maps dumpable instances to unique IDs for calculating
  ;; a similarity hash of composite objects that contain instances.
  ;; A user-defined hash function can not use address-based hashing, and it is
  ;; better not to add a new lazy stable hash slot to instances as a
  ;; side-effect of compiling.
  (instance-id-table (make-hash-table :test 'eq) :type hash-table :read-only t)
  ;; Hashtable mapping a string to a list of fop-table indices of
  ;; symbols whose name is that string. For any name as compared
  ;; by STRING= there can be a symbol whose name is a base string
  ;; and/or a symbol whose name is not a base string.
  (string=-table (make-hash-table :test 'equal) :type hash-table)
  ;; the fasloader table's current free pointer: the next offset to be used
  (table-free 0 :type index)
  ;; an alist (PACKAGE . OFFSET) of the table offsets for each package
  ;; we have currently located.
  (packages () :type list)
  ;; a table mapping from the ENTRY-INFO structures for dumped XEPs to
  ;; the table offsets of the corresponding code pointers
  (entry-table (make-hash-table :test 'eq) :type hash-table)
  ;; a table holding back-patching info for forward references to XEPs.
  ;; The key is the ENTRY-INFO structure for the XEP, and the value is
  ;; a list of conses (<code-handle> . <offset>), where <code-handle>
  ;; is the offset in the table of the code object needing to be
  ;; patched, and <offset> is the offset that must be patched.
  (patch-table (make-hash-table :test 'eq) :type hash-table)
  ;; a list of the table handles for all of the DEBUG-INFO structures
  ;; dumped in this file. These structures must be back-patched with
  ;; source location information when the compilation is complete.
  (debug-info () :type list)
  ;; This is used to keep track of objects that we are in the process
  ;; of dumping so that circularities can be preserved. The key is the
  ;; object that we have previously seen, and the value is the object
  ;; that we reference in the table to find this previously seen
  ;; object. (The value is never NIL.)
  ;;
  ;; Except with list objects, the key and the value are always the
  ;; same. In a list, the key will be some tail of the value.
  (circularity-table (make-hash-table :test 'eq) :type hash-table)
  ;; a hash table of structures that are allowed to be dumped. If we
  ;; try to dump a structure that isn't in this hash table, we lose.
  (valid-structures (make-hash-table :test 'eq) :type hash-table)
  ;; a hash table of slots to be saved when dumping an instance.
  (saved-slot-names (make-hash-table :test 'eq) :type hash-table))
(declaim (freeze-type fasl-output))

;;; Similarity hash table logic.
;;; It really seems bogus to me that we do similarity checking in both
;;; the IR1 namespace and the fasl dumper, even going so far as to use
;;; two different ways to decide that an object is "harmless" to look up
;;; (i.e. has no cyclic references).

#-sb-xc-host
(progn
(defun similarp (x y)
  ;; Do almost the same thing that EQUAL does, but:
  ;;  - consider strings to be dissimilar if their element types differ.
  ;;  - scan elements of all specialized numeric vectors (BIT is done by EQUAL)
  (named-let recurse ((x x) (y y))
    (or (%eql x y)
        (typecase x
          (cons
           (and (consp y)
                (recurse (car x) (car y))
                (recurse (cdr x) (cdr y))))
          (string
           ;; Incidentally, if we want to preserve non-simpleness of dumped arrays
           ;; (which is permissible but not required), this case (and below for arrays)
           ;; would be where to do it by returning non-similar.
           (and (stringp y)
                ;; (= (widetag-of ...)) would be too strict, because a simple string
                ;; can be be similar to a non-simple string.
                (eq (array-element-type x)
                    (array-element-type y))
                (string= x y)))
          ((or pathname bit-vector) ; fall back to EQUAL
           ;; This could be slightly wrong, but so it always was, because we use
           ;; (and have used) EQUAL for PATHNAME in FIND-CONSTANT, but:
           ;;   "Two pathnames S and C are similar if all corresponding pathname components are similar."
           ;; and we readily admit that similarity of strings requires equal element types.
           ;; So this is slightly dubious:
           ;; (EQUAL (MAKE-PATHNAME :NAME (COERCE "A" 'SB-KERNEL:SIMPLE-CHARACTER-STRING))
           ;;        (MAKE-PATHNAME :NAME (COERCE "A" 'BASE-STRING))) => T
           ;; On the other hand, nothing says that the pathname constructors such as
           ;; MAKE-PATHNAME and MERGE-PATHNAMES don't convert to a canonical representation
           ;; which renders them EQUAL when all strings are STRING=.
           ;; This area of the language spec seems to have been a clusterfsck.
           (equal x y))
          ;; We would need to enhance COALESCE-TREE-P to detect cycles involving
          ;; SIMPLE-VECTOR before recursing, otherwise this could exhaust stack.
          ((unboxed-array (*))
           (and (sb-xc:typep y '(simple-array * 1))
                (= (length x) (length y))
                (equal (array-element-type x) (array-element-type y))
                (or (typep x '(array nil (*)))
                    (dotimes (i (length x) t)
                      (unless (= (aref x i) (aref y i)) (return nil))))))
          ;; How do SIMPLE-VECTOR and other array types get here?
          ;; Answer: COALESCE-TREE-P is "weaker than" the the local COALESCE-P function in FIND-CONSTANT,
          ;; so it may return T on trees that contain atoms that COALESCE-P would have returned NIL on.
          ;; Therefore DUMP-NON-IMMEDIATE-OBJECT may call SIMILARP on an object for which COALESCE-P
          ;; would have said NIL.
          ;; As mentioned at the top of this file, this seems incredibly bad,
          ;; But users do not tend to have object cycles involving SIMPLE-VECTOR and such, I guess?
          ;; Anyway, the answer has to be "no" for everything else: un-EQL objects are not similar.
          (t nil)))))
;; This hash function is an amalgam of SXHASH and PSHASH with the following properties:
;;  - numbers must have the same type to be similar (same as SXHASH)
;;  - instances must be EQ to be similar (same as SXHASH)
;;  - strings and characters are compared case-sensitively (same as SXHASH)
;;  - arrays must have the same type to be similar
;; Unlike EQUAL-HASH, we never call EQ-HASH, because there is generally no reason
;; to try to look up an object that lacks a content-based hash value.
(defun similar-hash (x)
  (declare (special *compile-object*))
  (named-let recurse ((x x))
    ;; There is no depth cutoff - X must not be circular,
    ;; which was already determined as a precondition to calling this,
    ;; except that as pointed out, we must not descend into simple-vector
    ;; because there was no circularity checking done for arrays.
    (typecase x
      (list (do ((hash 0))
                ((atom x) (mix (if x (recurse x) #xD00F) hash))
              ;; mix the hash of the CARs only, without consuming stack
              ;; proportional to list length.
              (setf hash (mix (recurse (car x)) hash)
                    x (cdr x))))
      (symbol (sxhash x))
      (number (sb-impl::number-sxhash x))
      (pathname (sb-impl::pathname-sxhash x))
      (instance
       (let ((idmap (fasl-output-instance-id-table *compile-object*)))
         (values (ensure-gethash x idmap
                                 (let ((c (1+ (hash-table-count idmap))))
                                   (mix c c))))))
      ;; Arrays disregard simplicity.
      ((array nil (*)) #xdead) ; don't access the data in these bastards
      ((unboxed-array (*))
       (let* ((simple-array (coerce x '(simple-array * (*))))
              (widetag (%other-pointer-widetag simple-array))
              (saetp (find widetag sb-vm:*specialized-array-element-type-properties*
                           :key #'sb-vm:saetp-typecode))
              (n-data-words (ceiling (sb-vm::vector-n-data-octets simple-array saetp)
                                     sb-vm:n-word-bytes))
              (hash (word-mix (length x) widetag)))
         (declare (word hash))
         (dotimes (i n-data-words (logand hash most-positive-fixnum))
           ;; FIXME: the last word of {1,2,4}-bit-per-element vectors
           ;; needs to be masked. At worst, this fails to coalesce
           ;; similar vectors, so it's not fatal.
           (setq hash (word-mix hash (%vector-raw-bits x i))))))
      (character (char-code x))
      (t 0))))
(defun make-similarity-table ()
  (make-hash-table :hash-function #'similar-hash :test #'similarp))
) ; end PROGN

;;; When cross-compiling, it's good enough to approximate similarity as EQUAL.
#+sb-xc-host
(defun make-similarity-table () (make-hash-table :test 'equal))

(defmacro get-similar (key table) `(gethash ,key ,table))

;;; This structure holds information about a circularity.
(defstruct (circularity (:copier nil))
  ;; the kind of modification to make to create circularity
  (type (missing-arg) :type (member :rplaca :rplacd :svset :struct-set :slot-set))
  ;; the object containing circularity
  object
  ;; index in object for circularity
  (index (missing-arg) :type index)
  ;; slot name in object for circularity
  (slot-name nil :type symbol)
  ;; the object to be stored at INDEX in OBJECT. This is that the key
  ;; that we were using when we discovered the circularity.
  value
  ;; the value that was associated with VALUE in the
  ;; CIRCULARITY-TABLE. This is the object that we look up in the
  ;; EQ-TABLE to locate VALUE.
  enclosing-object)

;;; a list of the CIRCULARITY structures for all of the circularities
;;; detected in the current top level call to DUMP-OBJECT. Setting
;;; this lobotomizes circularity detection as well, since circular
;;; dumping uses the table.
(defvar *circularities-detected*)

;;;; utilities

;;; Write the byte B to the specified FASL-OUTPUT stream.
(defun dump-byte (b fasl-output)
  (declare (type (unsigned-byte 8) b) (type fasl-output fasl-output))
  (write-byte b (fasl-output-stream fasl-output)))

;; Dump a word-sized integer.
(defun dump-word (num fasl-output)
  (declare (type sb-vm:word num) (type fasl-output fasl-output))
  (let ((stream (fasl-output-stream fasl-output)))
    (dotimes (i sb-vm:n-word-bytes)
      (write-byte (ldb (byte 8 (* 8 i)) num) stream))))

;; Dump a 32-bit integer.
(defun dump-unsigned-byte-32 (num fasl-output)
  (declare (type (unsigned-byte 32) num) (type fasl-output fasl-output))
  (let ((stream (fasl-output-stream fasl-output)))
    (dotimes (i 4)
      (write-byte (ldb (byte 8 (* 8 i)) num) stream))))

;;; Dump NUM to the fasl stream, represented by N bytes. This works
;;; for either signed or unsigned integers. There's no range checking
;;; -- if you don't specify enough bytes for the number to fit, this
;;; function cheerfully outputs the low bytes.
;;; Multi-byte integers written by this function are always little-endian.
(defun dump-integer-as-n-bytes (num bytes fasl-output)
  (declare (integer num) (type index bytes))
  (declare (type fasl-output fasl-output))
  (do ((n num (ash n -8))
       (i bytes (1- i)))
      ((= i 0))
    (declare (type index i))
    (dump-byte (logand n #xff) fasl-output))
  (values))

(defun dump-varint (n fasl-output)
  (let ((buf (fasl-output-varint-buf fasl-output)))
    (setf (fill-pointer buf) 0)
    (write-var-integer n buf)
    (write-sequence buf (fasl-output-stream fasl-output))))

(defun dump-fop+operands (fasl-output opcode arg1
                                      &optional (arg2 0 arg2p) (arg3 0 arg3p))
  (declare (type (unsigned-byte 8) opcode) (type word arg1 arg2 arg3))
  (dump-byte opcode fasl-output)
  (dump-varint arg1 fasl-output)
  (when arg2p (dump-varint arg2 fasl-output))
  (when arg3p (dump-varint arg3 fasl-output)))

;;; Dump the FOP code for the named FOP to the specified FASL-OUTPUT.
;;; This macro is supposed to look functional in that it evals its
;;; args, but it wants to evaluate the first arg at compile-time. For
;;; this reason it should really not be a quoted symbol, but I think
;;; this used to actually be a function which had to look up the fop's
;;; opcode every time called. The named FOP is also treated as a string
;;; designator which is interned in the package defining the FOPs.
(defmacro dump-fop (fop-symbol file &rest args)
  (let* ((fop-symbol
           ;; EVAL is too much. Just ascertain we have a quoted symbol
           (if (typep fop-symbol '(cons (eql quote) (cons symbol null)))
               (cadr fop-symbol)
               (error "Bad 1st arg to DUMP-FOP: ~S" fop-symbol)))
         (val (or (gethash (intern (symbol-name fop-symbol) #.(find-package "SB-FASL"))
                           *fop-name-to-opcode*)
                  (error "compiler bug: ~S is not a legal fasload operator."
                         fop-symbol)))
         (fop-argc (aref (car **fop-signatures**) val)))
    (cond
      ((not (eql (length args) fop-argc))
       (error "~S takes ~D argument~:P" fop-symbol fop-argc))
      ((eql fop-argc 0)
       `(dump-byte ,val ,file))
      (t
       `(dump-fop+operands ,file ,val ,@args)))))

;;; Push the object at table offset Handle on the fasl stack.
(defun dump-push (handle fasl-output)
  (declare (type index handle) (type fasl-output fasl-output))
  (dump-fop 'fop-push fasl-output handle)
  (values))

;;; Pop the object currently on the fasl stack top into the table, and
;;; return the table index, incrementing the free pointer.
(defun dump-pop (fasl-output)
  (prog1
      (fasl-output-table-free fasl-output)
    (dump-fop 'fop-pop fasl-output)
    (incf (fasl-output-table-free fasl-output))))

(defun dump-to-table (fasl-output)
  (prog1
      (fasl-output-table-free fasl-output)
    (dump-fop 'fop-move-to-table fasl-output)
    (incf (fasl-output-table-free fasl-output))))

(defun cdr-similarity-p (index fasl-output)
  (when (consp index)
    (destructuring-bind (list . nthcdr) index
      (let ((index (gethash list (fasl-output-eq-table fasl-output))))
        (when (fixnump index)
          (dump-push index fasl-output)
          (dump-fop 'fop-nthcdr fasl-output nthcdr)
          t)))))

;;; If X is in File's SIMILAR-TABLE, then push the object and return T,
;;; otherwise NIL.
(defun similar-check-table (x fasl-output)
  (declare (type fasl-output fasl-output))
  (let ((index (get-similar x (fasl-output-similar-table fasl-output))))
    (cond ((fixnump index)
           (dump-push index fasl-output)
           t)
          ((cdr-similarity-p index fasl-output)))))

;;; These functions are called after dumping an object to save the
;;; object in the table. The object (also passed in as X) must already
;;; be on the top of the FOP stack.
(defun eq-save-object (x fasl-output)
  (declare (type fasl-output fasl-output))
  (setf (gethash x (fasl-output-eq-table fasl-output))
        (dump-to-table fasl-output))
  (values))
(defun similar-save-object (x fasl-output)
  (declare (type fasl-output fasl-output))
  (let ((handle (dump-to-table fasl-output)))
    (setf (get-similar x (fasl-output-similar-table fasl-output)) handle)
    (setf (gethash x (fasl-output-eq-table fasl-output)) handle))
  (values))
;;; Record X in File's CIRCULARITY-TABLE. This is called on objects
;;; that we are about to dump might have a circular path through them.
;;;
;;; The object must not currently be in this table, since the dumper
;;; should never be recursively called on a circular reference.
;;; Instead, the dumping function must detect the circularity and
;;; arrange for the dumped object to be patched.
(defun note-potential-circularity (x fasl-output)
  (let ((circ (fasl-output-circularity-table fasl-output)))
    (aver (not (gethash x circ)))
    (setf (gethash x circ) x))
  (values))

;;;; opening and closing fasl files

;;; Open a fasl file, write its header, and return a FASL-OUTPUT
;;; object for dumping to it. Some human-readable information about
;;; the source code is given by the string WHERE.
(defun open-fasl-output (name where)
  (declare (type pathname name))
  (flet ((fasl-write-string (string stream)
           ;; UTF-8 is safe to use, because +FASL-HEADER-STRING-STOP-CHAR-CODE+
           ;; may not appear in UTF-8 encoded bytes
           (write-sequence (string-to-octets string :external-format :utf-8)
                           stream)))
    (let* ((stream (open name
                         :direction :output
                         :if-exists :supersede
                         :element-type 'sb-assem:assembly-unit))
           (res (make-fasl-output :stream stream)))
      ;; Before the actual FASL header, write a shebang line using the current
      ;; runtime path, so our fasls can be executed directly from the shell.
      #-sb-xc-host ; cross-compiled fasls are not directly executable
      (when *runtime-pathname*
        (fasl-write-string
         (format nil "#!~A --script~%"
                 (native-namestring *runtime-pathname* :as-file t))
         stream))
      ;; Begin the header with the constant machine-readable (and
      ;; semi-human-readable) string which is used to identify fasl files.
      (fasl-write-string *fasl-header-string-start-string* stream)
      ;; The constant string which begins the header is followed by
      ;; arbitrary human-readable text, terminated by
      ;; +FASL-HEADER-STRING-STOP-CHAR-CODE+.
      (fasl-write-string
       (with-standard-io-syntax
         (let ((*print-readably* nil)
               (*print-pretty* nil))
           (format nil
                   "~%  ~
                    compiled from ~S~%  ~
                    using ~A version ~A~%"
                   where
                   (lisp-implementation-type)
                   (lisp-implementation-version))))
       stream)
      (dump-byte +fasl-header-string-stop-char-code+ res)
      ;; Finish the header by outputting fasl file implementation,
      ;; version, and key *FEATURES*.
      (flet ((dump-counted-string (string)
               ;; The count is dumped as a 32-bit unsigned-byte even on 64-bit
               ;; platforms. This ensures that a x86-64 SBCL can gracefully
               ;; detect an error when trying to read a x86 fasl, instead
               ;; of choking on a ridiculously long counted string.
               ;;  -- JES, 2005-12-30
               (dump-unsigned-byte-32 (length string) res)
               (dotimes (i (length string))
                 (dump-byte (char-code (aref string i)) res))))
        (dump-counted-string (symbol-name +backend-fasl-file-implementation+))
        (dump-word +fasl-file-version+ res)
        (dump-counted-string (lisp-implementation-version))
        (dump-counted-string (compute-features-affecting-fasl-format)))
      res)))

;;; Close the specified FASL-OUTPUT, aborting the write if ABORT-P.
(defun close-fasl-output (fasl-output abort-p)
  (declare (type fasl-output fasl-output))

  (unless abort-p
    ;; sanity checks
    (aver (zerop (hash-table-count (fasl-output-patch-table fasl-output))))
    ;; End the group.
    (dump-fop 'fop-end-group fasl-output (fasl-output-table-free fasl-output)))

  ;; That's all, folks.
  (close (fasl-output-stream fasl-output) :abort abort-p)
  (values))

;;;; main entries to object dumping

;;; This function deals with dumping objects that are complex enough
;;; so that we want to cache them in the table, rather than repeatedly
;;; dumping them. If the object is in the EQ-TABLE, then we push it,
;;; otherwise, we do a type dispatch to a type specific dumping
;;; function. The type specific branches do any appropriate
;;; SIMILAR-TABLE check and table entry.
;;;
;;; When we go to dump the object, we enter it in the CIRCULARITY-TABLE.
(defun dump-non-immediate-object (x file)
  (let ((index (gethash x (fasl-output-eq-table file))))
    (cond ((fixnump index)
           (dump-push index file))
          ((cdr-similarity-p index file))
          (t
           (typecase x
             (symbol (dump-symbol x file))
             (list
              (cond ((not (coalesce-tree-p x))
                     (dump-list x file)
                     (eq-save-object x file))
                    ((not (similar-check-table x file))
                     (dump-list x file t)
                     (similar-save-object x file))))
             (layout
              (dump-layout x file)
              (eq-save-object x file))
             #+sb-xc-host
             (ctype
              (aver (not (classoid-p x)))
              (dump-object 'values-specifier-type file)
              (dump-object (type-specifier x) file)
              (dump-fop 'fop-funcall file 1))
             (instance
              (multiple-value-bind (slot-names slot-names-p)
                  (gethash x (fasl-output-saved-slot-names file))
                (if slot-names-p
                    (dump-instance-saving-slots x slot-names file)
                    (dump-structure x file)))
              (eq-save-object x file))
             (array
              ;; DUMP-ARRAY (and its callees) are responsible for
              ;; updating the EQ and SIMILAR hash tables.
              (dump-array x file))
             (number
              (unless (similar-check-table x file)
                (etypecase x
                  (ratio (dump-ratio x file))
                  (complex (dump-complex x file))
                  (float (dump-float x file))
                  (integer (dump-integer x file)))
                (similar-save-object x file)))
             #+(and (not sb-xc-host) sb-simd-pack)
             (simd-pack
              (unless (similar-check-table x file)
                (dump-fop 'fop-simd-pack file)
                (dump-integer-as-n-bytes (%simd-pack-tag  x) 8 file)
                (dump-integer-as-n-bytes (%simd-pack-low  x) 8 file)
                (dump-integer-as-n-bytes (%simd-pack-high x) 8 file)
                (similar-save-object x file)))
             #+(and (not sb-xc-host) sb-simd-pack-256)
             (simd-pack-256
              (unless (similar-check-table x file)
                (dump-simd-pack-256 x file)
                (similar-save-object x file)))
             (t
              ;; This probably never happens, since bad things tend to
              ;; be detected during IR1 conversion.
              (error "This object cannot be dumped into a fasl file:~% ~S"
                     x))))))
  (values))

#+(and (not sb-xc-host) sb-simd-pack-256)
(defun dump-simd-pack-256 (x file)
  (dump-fop 'fop-simd-pack file)
  (dump-integer-as-n-bytes (logior (%simd-pack-256-tag x) (ash 1 6)) 8 file)
  (dump-integer-as-n-bytes (%simd-pack-256-0 x) 8 file)
  (dump-integer-as-n-bytes (%simd-pack-256-1 x) 8 file)
  (dump-integer-as-n-bytes (%simd-pack-256-2 x) 8 file)
  (dump-integer-as-n-bytes (%simd-pack-256-3 x) 8 file))

;;; Dump an object of any type by dispatching to the correct
;;; type-specific dumping function. We pick off immediate objects,
;;; symbols and magic lists here. Other objects are handled by
;;; DUMP-NON-IMMEDIATE-OBJECT.
;;;
;;; This is the function used for recursive calls to the fasl dumper.
;;; We don't worry about creating circularities here, since it is
;;; assumed that there is a top level call to DUMP-OBJECT.
(defun sub-dump-object (x file)
  (cond ((listp x)
         (if x
             (dump-non-immediate-object x file)
             (dump-fop 'fop-empty-list file)))
        ((symbolp x)
         (if (eq x t)
             (dump-fop 'fop-truth file)
             (dump-non-immediate-object x file)))
        ((fixnump x) (dump-integer x file))
        ((characterp x)
         (dump-fop 'fop-character file (char-code x)))
        ((packagep x)
         (dump-push (dump-package x file) file))
        #-sb-xc-host
        ((system-area-pointer-p x)
         (dump-fop 'fop-word-pointer file)
         (dump-integer-as-n-bytes (sap-int x) sb-vm:n-word-bytes file))
        (t
         (dump-non-immediate-object x file))))

;;; Dump stuff to backpatch already dumped objects. INFOS is the list
;;; of CIRCULARITY structures describing what to do. The patching FOPs
;;; take the value to store on the stack. We compute this value by
;;; fetching the enclosing object from the table, and then CDR'ing it
;;; if necessary.
(defun dump-circularities (infos file)
  (let ((table (fasl-output-eq-table file)))
    (dolist (info infos)

      (let* ((value (circularity-value info))
             (enclosing (circularity-enclosing-object info)))
        (dump-push (gethash enclosing table) file)
        (unless (eq enclosing value)
          (do ((current enclosing (cdr current))
               (i 0 (1+ i)))
              ((eq current value)
               (dump-fop 'fop-nthcdr file i))
            (declare (type index i)))))

      (macrolet ((fop-op (symbol)
                   (gethash (intern (symbol-name symbol) "SB-FASL")
                            *fop-name-to-opcode*)))
        (dump-byte (ecase (circularity-type info)
                     (:rplaca     (fop-op fop-rplaca))
                     (:rplacd     (fop-op fop-rplacd))
                     (:svset      (fop-op fop-svset))
                     (:struct-set (fop-op fop-structset))
                     (:slot-set
                      (dump-object (circularity-slot-name info) file)
                      (fop-op fop-slotset)))
                   file))
      (dump-varint (gethash (circularity-object info) table) file)
      (dump-varint (circularity-index info) file))))

;;; Set up stuff for circularity detection, then dump an object. All
;;; shared and circular structure will be exactly preserved within a
;;; single call to DUMP-OBJECT. Sharing between objects dumped by
;;; separate calls is only preserved when convenient.
;;;
;;; We peek at the object type so that we only pay the circular
;;; detection overhead on types of objects that might be circular.
(defun dump-object (x file)
  (if (compound-object-p x)
      (let ((*circularities-detected* ())
            (circ (fasl-output-circularity-table file)))
        (clrhash circ)
        (sub-dump-object x file)
        (when *circularities-detected*
          (dump-circularities *circularities-detected* file)
          (clrhash circ)))
      (sub-dump-object x file)))

;;;; LOAD-TIME-VALUE and MAKE-LOAD-FORM support

;;; Emit a funcall of the function and return the handle for the
;;; result.
(defun fasl-dump-load-time-value-lambda (fun file)
  (declare (type clambda fun) (type fasl-output file))
  (let ((handle (gethash (leaf-info fun)
                         (fasl-output-entry-table file))))
    (aver handle)
    (dump-push handle file)
    (dump-fop 'fop-funcall file 0))
  (dump-pop file))

;;; Return T iff CONSTANT has already been dumped. It's been dumped if
;;; it's in the EQ table.
;;;
;;; Note: historically (1) the above comment was "T iff ... has not been dumped",
;;; (2) the test was  was also true if the constant had been validated / was in
;;; the valid objects table. This led to substructures occasionally skipping the
;;; validation, and hence failing the "must have been validated" test.
(defun fasl-constant-already-dumped-p (constant file)
  (and (gethash constant (fasl-output-eq-table file)) t))

;;; Use HANDLE whenever we try to dump CONSTANT. HANDLE should have been
;;; returned earlier by FASL-DUMP-LOAD-TIME-VALUE-LAMBDA.
(defun fasl-note-handle-for-constant (constant handle file)
  (let ((table (fasl-output-eq-table file)))
    (when (gethash constant table)
      (error "~S already dumped?" constant))
    (setf (gethash constant table) handle))
  (values))

;;; Note that the specified structure can just be dumped by
;;; enumerating the slots.
(defun fasl-validate-structure (structure file)
  (setf (gethash structure (fasl-output-valid-structures file)) t)
  (values))

;;; Note that the specified standard object can just be dumped by
;;; saving its slot values.
(defun fasl-note-instance-saves-slots (instance slot-names file)
  (setf (gethash instance (fasl-output-saved-slot-names file)) slot-names)
  (values))

;;;; number dumping

(defun dump-ratio (x file)
  (sub-dump-object (numerator x) file)
  (sub-dump-object (denominator x) file)
  (dump-fop 'fop-ratio file))

(defun dump-integer (n file)
  (typecase n
    ((signed-byte 8)
     (case n
       (0  (dump-fop 'fop-int-const0 file))
       (1  (dump-fop 'fop-int-const1 file))
       (2  (dump-fop 'fop-int-const2 file))
       (-1 (dump-fop 'fop-int-const-neg1 file))
       (t  (dump-fop 'fop-byte-integer file)
           (dump-byte (logand #xFF n) file))))
    ((unsigned-byte #.(1- sb-vm:n-word-bits))
     (dump-fop 'fop-word-integer file)
     (dump-word n file))
    (sb-vm:signed-word
     (dump-fop 'fop-word-integer file)
     (dump-integer-as-n-bytes n sb-vm:n-word-bytes file))
    (t
     (let ((bytes (ceiling (1+ (integer-length n)) 8)))
       (dump-fop 'fop-integer file bytes)
       (dump-integer-as-n-bytes n bytes file)))))

(defun dump-float (x file)
  (etypecase x
    (single-float
     (dump-fop 'fop-single-float file)
     (dump-integer-as-n-bytes (single-float-bits x) 4 file))
    (double-float
     (dump-fop 'fop-double-float file)
     (dump-integer-as-n-bytes (double-float-low-bits x) 4 file)
     (dump-integer-as-n-bytes (double-float-high-bits x) 4 file))
    #+long-float
    (long-float
     (dump-fop 'fop-long-float file)
     (dump-long-float x file))))

(defun dump-complex (x file)
  (let ((re (realpart x))
        (im (imagpart x)))
    (typecase x
     ((complex single-float)
      (dump-fop 'fop-complex-single-float file)
      (dump-integer-as-n-bytes (single-float-bits re) 4 file)
      (dump-integer-as-n-bytes (single-float-bits im) 4 file))
     ((complex double-float)
      (dump-fop 'fop-complex-double-float file)
      (dump-integer-as-n-bytes (double-float-low-bits re) 4 file)
      (dump-integer-as-n-bytes (double-float-high-bits re) 4 file)
      (dump-integer-as-n-bytes (double-float-low-bits im) 4 file)
      (dump-integer-as-n-bytes (double-float-high-bits im) 4 file))
     #+long-float
     ((complex long-float)
      (dump-fop 'fop-complex-long-float file)
      (dump-long-float re file)
      (dump-long-float im file))
     (t
      (sub-dump-object re file)
      (sub-dump-object im file)
      (dump-fop 'fop-complex file)))))

;;;; symbol dumping

;;; Return the table index of PKG, adding the package to the table if
;;; necessary. During cold load, we read the string as a normal string
;;; so that we can do the package lookup at cold load time.
;;;
;;; FIXME: Despite the parallelism in names, the functionality of
;;; this function is not parallel to other functions DUMP-FOO, e.g.
;;; DUMP-SYMBOL and DUMP-LIST. The mapping between names and behavior
;;; should be made more consistent.
(declaim (ftype (function (package fasl-output) index) dump-package))
(defun dump-package (pkg file)
  (declare (inline assoc))
  (cond ((cdr (assoc pkg (fasl-output-packages file) :test #'eq)))
        (t
         (let ((s (sb-xc:package-name pkg)))
           (dump-fop 'fop-named-package-save file (length s))
           ;; Package names are always dumped as varint-encoded character strings
           ;; except on non-unicode builds.
           (dump-chars (coerce s '(simple-array character (*))) file nil))
         (let ((entry (fasl-output-table-free file)))
           (incf (fasl-output-table-free file))
           (push (cons pkg entry) (fasl-output-packages file))
           entry))))

;;; dumper for lists

;;; Dump a list, setting up patching information when there are
;;; circularities. We scan down the list, checking for CDR and CAR
;;; circularities.
;;;
;;; If there is a CDR circularity, we terminate the list with NIL and
;;; make a CIRCULARITY notation for the CDR of the previous cons.
;;;
;;; If there is no CDR circularity, then we mark the current cons and
;;; check for a CAR circularity. When there is a CAR circularity, we
;;; make the CAR NIL initially, arranging for the current cons to be
;;; patched later.
;;;
;;; Otherwise, we recursively call the dumper to dump the current
;;; element.
(defun dump-list (list file &optional coalesce)
  (aver (and list
             (not (gethash list (fasl-output-circularity-table file)))))
  (let ((circ (fasl-output-circularity-table file)))
    (flet ((cdr-circularity (obj n)
             ;; COALESCE means there's no cycles
             (let ((ref (gethash obj circ)))
               (when ref
                 (push (make-circularity :type :rplacd
                                         :object list
                                         :index (1- n)
                                         :value obj
                                         :enclosing-object ref)
                       *circularities-detected*)
                 (terminate-undotted-list n file)
                 t))))
      (do* ((l list (cdr l))
            (n 0 (1+ n)))
           ((atom l)
            (cond ((null l)
                   (terminate-undotted-list n file))
                  (t
                   (cond ((cdr-circularity l n))
                         (t
                          (sub-dump-object l file)
                          (terminate-dotted-list n file))))))
        (declare (type index n))
        (when (cdr-circularity l n)
          (return))

        ;; if this CONS is EQ to some other object we have already
        ;; dumped, dump a reference to that instead.
        (let* ((table (if coalesce
                          (fasl-output-similar-table file)
                          (fasl-output-eq-table file)))
               (index (gethash l table)))
          (cond ((fixnump index)
                 (dump-push index file)
                 (terminate-dotted-list n file)
                 (return))
                ((cdr-similarity-p index file)
                 (when (> n 0)
                   (terminate-dotted-list n file))
                 (return)))

          ;; put an entry for this cons into the fasl output cons table,
          ;; for the benefit of dumping later constants
          (let ((index (cons list n)))
            (setf (gethash l (fasl-output-eq-table file)) index)
            (when coalesce
              (setf (gethash l (fasl-output-similar-table file)) index))))

        (setf (gethash l circ) list)

        (let* ((obj (car l))
               (ref (gethash obj circ)))
          (cond (ref
                 (push (make-circularity :type :rplaca
                                         :object list
                                         :index n
                                         :value obj
                                         :enclosing-object ref)
                       *circularities-detected*)
                 (sub-dump-object nil file))
                ;; Avoid coalescing if COALESCE-TREE-P decided not to
                ((consp obj)
                 ;; This is the same as DUMP-NON-IMMEDIATE-OBJECT but
                 ;; without calling COALESCE-TREE-P again.
                 (let ((index (gethash obj (fasl-output-eq-table file))))
                   (cond ((fixnump index)
                          (dump-push index file))
                         ((cdr-similarity-p index file))
                         ((not coalesce)
                          (dump-list obj file)
                          (eq-save-object obj file))
                         ((not (similar-check-table obj file))
                          (dump-list obj file t)
                          (similar-save-object obj file)))))
                (t
                 (sub-dump-object obj file))))))))

(defconstant fop-list-base-opcode 128)

(defun terminate-dotted-list (n file)
  (declare (type index n) (type fasl-output file))
  (aver (plusp n))
  (cond ((< n 16)
         (dump-byte (logior fop-list-base-opcode #b10000 n) file))
        (t
         (dump-byte (logior fop-list-base-opcode #b10000) file)
         (dump-varint (- n 16) file))))

(defun terminate-undotted-list (n file)
  (declare (type index n) (type fasl-output file))
  (aver (plusp n))
  (cond ((< n 16)
         (dump-byte (logior fop-list-base-opcode n) file))
        (t
         (dump-byte (logior fop-list-base-opcode) file)
         (dump-varint (- n 16) file))))

;;;; array dumping

;;; Dump the array thing.
(defun dump-array (x file)
  (if (vectorp x)
      (dump-vector x file)
      #-sb-xc-host (dump-multi-dim-array x file)
      #+sb-xc-host (bug "Can't dump multi-dim array")))

;;; Dump the vector object. If it's not simple, then actually dump a
;;; simple realization of it. But we enter the original in the EQ or SIMILAR
;;; tables.
(defun dump-vector (x file)
  (let ((simple-version (if (array-header-p x)
                            (coerce x `(simple-array
                                        ,(array-element-type x)
                                        (*)))
                            x)))
    (typecase simple-version
      ;; On the host, take all strings to be simple-base-string.
      ;; In the target, really test for simple-base-string.
      (#+sb-xc-host simple-string #-sb-xc-host simple-base-string
       (unless (similar-check-table x file)
         (dump-fop 'fop-base-string file (length simple-version))
         (dump-chars simple-version file t)
         (similar-save-object x file)))
      #-sb-xc-host
      ((simple-array character (*))
       #-sb-unicode (bug "how did we get here?")
       (unless (similar-check-table x file)
         (dump-fop 'fop-character-string file (length simple-version))
         (dump-chars simple-version file nil)
         (similar-save-object x file)))
      ;; SB-XC:SIMPLE-VECTOR will not match an array whose element type
      ;; the host upgraded to T but whose expressed type was not T.
      (sb-xc:simple-vector
       (dump-simple-vector simple-version file)
       (eq-save-object x file)
       (unless (eq x simple-version)
         ;; In case it has circularities that need to be patched
         ;; later.
         (setf (gethash simple-version (fasl-output-eq-table file))
               (gethash x (fasl-output-eq-table file)))))
      (t
       (unless (similar-check-table x file)
         (dump-specialized-vector simple-version file)
         (similar-save-object x file))))))

;;; Dump a SIMPLE-VECTOR, handling any circularities.
(defun dump-simple-vector (v file)
  (declare (type simple-vector v) (type fasl-output file))
  (note-potential-circularity v file)
  (do ((index 0 (1+ index))
       (length (length v))
       (circ (fasl-output-circularity-table file)))
      ((= index length)
       (dump-fop 'fop-vector file length))
    (let* ((obj (aref v index))
           (ref (gethash obj circ)))
      (cond (ref
             (push (make-circularity :type :svset
                                     :object v
                                     :index index
                                     :value obj
                                     :enclosing-object ref)
                   *circularities-detected*)
             (sub-dump-object nil file))
            (t
             (sub-dump-object obj file))))))

(defun dump-specialized-vector (vector file &key data-only)
  ;; The DATA-ONLY option was for the now-obsolete trace-table,
  ;; but it seems like a good option to keep around.
  #-sb-xc-host (declare (type (simple-unboxed-array (*)) vector))
  (let* ((length (length vector))
         (widetag (%other-pointer-widetag vector))
         (bits-per-elt (sb-vm::simple-array-widetag->bits-per-elt widetag)))
    (unless data-only
      ;; fop-spec-vector doesn't grok trailing #\null convention.
      (aver (and (/= widetag sb-vm:simple-base-string-widetag)
                 (/= widetag sb-vm:simple-vector-widetag)))
      (dump-fop 'fop-spec-vector file length)
      (dump-byte widetag file))

    #+sb-xc-host
    (when (or (= widetag sb-vm:simple-array-fixnum-widetag)
              (= widetag sb-vm:simple-array-unsigned-fixnum-widetag))
      ;; Fixnum vector contents are tagged numbers. Make a copy.
      (setq vector (map 'vector (lambda (x) (ash x sb-vm:n-fixnum-tag-bits))
                        vector)))

    ;; cross-io doesn't know about fasl streams, so use actual stream.
    (sb-impl::buffer-output (fasl-output-stream file)
                            vector
                            0
                            (ceiling (* length bits-per-elt) sb-vm:n-byte-bits)
                            #+sb-xc-host bits-per-elt)))

;;; Dump string-ish things.

;;; Dump a SIMPLE-STRING.
(defun dump-chars (s fasl-output base-string-p)
  (declare (type simple-string s))
  (if (or base-string-p #-sb-unicode t) ; if non-unicode, every char is 1 byte
      (dovector (c s)
        (dump-byte (char-code c) fasl-output))
      (dovector (c s) ; varint (a/k/a LEB128) is better for this than UTF-8.
        (dump-varint (char-code c) fasl-output))))

;;; If we get here, it is assumed that the symbol isn't in the table,
;;; but we are responsible for putting it there when appropriate.
(defun dump-symbol (s file)
  (declare (type fasl-output file))
  (let* ((pname (symbol-name s))
         (pname-length (length pname))
         ;; If no unicode, then all strings are base-string-p.
         ;; On the host, everything is base-string-p.
         (base-string-p (and #-sb-xc-host (typep pname 'base-string)))
         (length+flag (logior (ash pname-length 1) (if base-string-p 1 0)))
         (dumped-as-copy nil)
         (pkg (sb-xc:symbol-package s)))
    (cond ((null pkg)
           (let ((this-base-p base-string-p))
             (dolist (lookalike (gethash pname (fasl-output-string=-table file))
                                (dump-fop 'fop-uninterned-symbol-save
                                          file length+flag))
               ;; Find the right kind of lookalike symbol.
               ;; [what about a symbol whose name is a (simple-array nil (0))?]
               (let ((that-base-p
                      (and #-sb-xc-host (typep (symbol-name lookalike) 'base-string))))
                 (when (or (and this-base-p that-base-p)
                           (and (not this-base-p) (not that-base-p)))
                   (dump-fop 'fop-copy-symbol-save file
                             (gethash lookalike (fasl-output-eq-table file)))
                   (return (setq dumped-as-copy t)))))))
          ((eq pkg *cl-package*)
           (dump-fop 'fop-lisp-symbol-save file length+flag))
          ((eq pkg *keyword-package*)
           (dump-fop 'fop-keyword-symbol-save file length+flag))
          (t
           (let ((pkg-index (dump-package pkg file)))
             (if (eq (find-symbol pname pkg) :inherited)
                 (dump-fop 'fop-symbol-in-package-save file length+flag pkg-index)
                 (dump-fop 'fop-symbol-in-package-internal-save file length+flag pkg-index)))))

    (unless dumped-as-copy
      (dump-chars pname file base-string-p)
      (push s (gethash (symbol-name s) (fasl-output-string=-table file))))

    (setf (gethash s (fasl-output-eq-table file))
          (fasl-output-table-free file))

    (incf (fasl-output-table-free file)))

  (values))

;;;; component (function) dumping

(defun dump-segment (segment code-length fasl-output)
  (declare (type sb-assem:segment segment)
           (type fasl-output fasl-output))
  (let* ((stream (fasl-output-stream fasl-output))
         (n-written (write-segment-contents segment stream)))
    ;; In CMU CL there was no enforced connection between the CODE-LENGTH
    ;; argument and the number of bytes actually written. I added this
    ;; assertion while trying to debug portable genesis. -- WHN 19990902
    (unless (= code-length n-written)
      (bug "code-length=~W, n-written=~W" code-length n-written)))
  (values))

(eval-when (:compile-toplevel)
  (assert (<= (length +fixup-kinds+) 16))) ; fixup-kind fits in 4 bits

(defconstant-eqx +fixup-flavors+
  #(:assembly-routine
    :card-table-index-mask :symbol-tls-index
    :alien-code-linkage-index :alien-data-linkage-index
    :foreign :foreign-dataref
    :code-object
    :layout :immobile-symbol :fdefn-call :static-call
    :symbol-value
    :layout-id)
  #'equalp)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun encoded-fixup-flavor (flavor)
  (or (position flavor +fixup-flavors+)
      (error "Bad fixup flavor ~s" flavor))))

;;; Pack the aspects of a fixup into an integer.
;;; DATA is for asm routine fixups. The routine can be represented in 8 bits,
;;; so the fixup can be reduced to one word instead of an integer and a symbol.
(declaim (inline !pack-fixup-info))
(defun !pack-fixup-info (offset kind flavor data)
  (logior ;; 4 bits
          (the (mod 16) (or (position kind +fixup-kinds+)
                            (error "Bad fixup kind ~s" kind)))
          ;; 4 bits
          (ash (the (mod 16) (encoded-fixup-flavor flavor)) 4)
          ;; 8 bits
          (ash (the (mod 256) data) 8)
          ;; whatever it needs
          (ash offset 16)))

#-(or x86 x86-64) ; these two architectures provide an overriding definition
(defun pack-fixups-for-reapplication (fixup-notes)
  (let (result)
    (dolist (note fixup-notes (pack-code-fixup-locs nil nil result))
      (let ((fixup (fixup-note-fixup note)))
        (when (eq (fixup-flavor fixup) :card-table-index-mask)
          (push (fixup-note-position note) result))))))

;;; Fasl files encode <flavor,kind> in a packed integer. Dispatching on the integer
;;; is simple, but the case keys still want to be symbols.
(defmacro fixup-flavor-case (flavor-id &rest clauses)
  (declare (notinline position))
  `(case ,flavor-id
     ,@(mapcar (lambda (clause)
                 (if (eq (car clause) t)
                     clause
                     (cons (mapcar (lambda (kwd) (encoded-fixup-flavor kwd))
                                   (ensure-list (car clause)))
                           (cdr clause))))
               clauses)))

;;; Dump all the fixups. The two CASE statements below check that each
;;; fixnum has a NAME of the type appropriate to the flavor.
(defun dump-fixups (fixup-notes alloc-points fasl-output &aux (nelements 2))
  (declare (type list fixup-notes) (type fasl-output fasl-output))
  ;; "retained" fixups are those whose offset in the code needs to be
  ;; remembered for subsequent reapplication by the garbage collector,
  ;; or in some cases, on core startup.
  (dump-object (pack-fixups-for-reapplication fixup-notes) fasl-output)
  (dump-object alloc-points fasl-output)
  (dolist (note fixup-notes nelements)
    (let* ((fixup (fixup-note-fixup note))
           (name (fixup-name fixup))
           (flavor (fixup-flavor fixup))
           (flavor-id (encoded-fixup-flavor flavor))
           (numeric-operand
            (fixup-flavor-case flavor-id
              ((:code-object :card-table-index-mask)
               (the null name)
               1) ; avoid dumping a general operand
              #-sb-xc-host ; ASM routine indices aren't known to the cross-compiler
              (:assembly-routine
               (the (integer 1 *) ; must not be nonzero. 0 decodes as no numeric operand
                    (cddr (gethash name (%asm-routine-table *assembler-routines*)))))))
           (info
            (!pack-fixup-info (fixup-note-position note) (fixup-note-kind note)
                              flavor (or numeric-operand 0))))
      (dump-object info fasl-output)
      (if numeric-operand
          (incf nelements) ; used 1 element of the fasl stack
          (let ((operand
                 (fixup-flavor-case flavor-id
                   ((:alien-code-linkage-index :alien-data-linkage-index
                     :foreign :foreign-dataref) (the string name))
                   (:layout
                    (if (symbolp name)
                        name
                        (layout-classoid-name name)))
                   (:layout-id (the layout name))
                   ((:assembly-routine
                     :symbol-tls-index
                     ;; Only #+immobile-space can use the following two flavors.
                     ;; An :IMMOBILE-SYMBOL fixup references the symbol itself,
                     ;; whereas a :SYMBOL-VALUE fixup references the value of the symbol.
                     ;; In the latter case, the symbol's address doesn't matter,
                     ;; but its global value must be an immobile object.
                     :immobile-symbol :symbol-value)
                    (the symbol name))
                   (t name)))) ; function name
            (dump-object operand fasl-output)
            (incf nelements 2))))))

;;; Dump out the constant pool and code-vector for component, push the
;;; result in the table, and return the offset.
;;;
;;; The only tricky thing is handling constant-pool references to
;;; functions. If we have already dumped the function, then we just
;;; push the code pointer. Otherwise, we must create back-patching
;;; information so that the constant will be set when the function is
;;; eventually dumped. This is a bit awkward, since we don't have the
;;; handle for the code object being dumped while we are dumping its
;;; constants.
;;;
;;; We dump trap objects in any unused slots or forward referenced slots.
(defun dump-code-object (component code-segment code-length fixups alloc-points fasl-output)
  (declare (type component component)
           (type index code-length)
           (type fasl-output fasl-output))
  (let* ((2comp (component-info component))
         (constants (ir2-component-constants 2comp))
         (header-length (length constants))
         (n-fdefns 0))
    (collect ((patches)
              (named-constants))
      ;; Dump the constants, noting any :ENTRY constants that have to
      ;; be patched.
      (loop for i from sb-vm:code-constants-offset below header-length do
        (binding* ((entry (aref constants i))
                   ((kind payload)
                    (if (listp entry) (values (car entry) (cadr entry)))))
          (etypecase entry
            (constant
             (cond ((leaf-has-source-name-p entry)
                    (named-constants (cons (leaf-source-name entry) i))
                    (dump-fop 'fop-misc-trap fasl-output))
                   (t
                    (dump-object (constant-value entry) fasl-output))))
            (null
             (dump-fop 'fop-misc-trap fasl-output))
            (list
             (ecase kind
               (:constant ; anything that has not been wrapped in a #<CONSTANT>
                (dump-object payload fasl-output))
               (:entry
                (let* ((info (leaf-info payload))
                       (handle (gethash info
                                        (fasl-output-entry-table fasl-output))))
                  (declare (type entry-info info))
                  (cond (handle (dump-push handle fasl-output))
                        (t
                         (patches (cons info i))
                         (dump-fop 'fop-misc-trap fasl-output)))))
               (:load-time-value
                (dump-push payload fasl-output))
               (:fdefinition
                ;; It's possible for other fdefns to be found in the header, but they can't
                ;; have resulted from IR2 conversion. They would have had to come from
                ;; something like (load-time-value (find-or-create-fdefn ...))
                ;; which is fine, but they don't count for this purpose.
                (incf n-fdefns)
                (dump-object payload fasl-output)
                (dump-fop 'fop-fdefn fasl-output))
               (:known-fun
                (dump-object payload fasl-output)
                (dump-fop 'fop-known-fun fasl-output))
               (:coverage-marks
                ;; Avoid the coalescence done by DUMP-VECTOR
                (dump-specialized-vector (make-array (cdr entry)
                                                     :element-type '(unsigned-byte 8)
                                                     :initial-element #xFF)
                                         fasl-output))
               #+arm64
               (:tls-index
                (dump-object payload fasl-output)
                (dump-fop 'fop-tls-index fasl-output)))))))

      ;; Dump the debug info.
      (let ((info (debug-info-for-component component)))
        (fasl-validate-structure info fasl-output)
        (dump-object info fasl-output)
        (push (dump-to-table fasl-output)
              (fasl-output-debug-info fasl-output)))

      (let ((n-fixup-elts (dump-fixups fixups alloc-points fasl-output)))
        (dump-fop 'fop-load-code fasl-output
                  (logior (ash header-length 1)
                          (if (code-immobile-p component) 1 0))
                  code-length
                  n-fixup-elts))
      ;; Fasl dumper/loader convention allows at most 3 integer args.
      ;; Others have to be written with explicit calls.
      (dump-integer-as-n-bytes (length (ir2-component-entries 2comp))
                               4 ; output 4 bytes
                               fasl-output)
      (dump-integer-as-n-bytes (the (unsigned-byte 22) n-fdefns)
                               4 ; output 4 bytes
                               fasl-output)
      (dump-segment code-segment code-length fasl-output)

      (let ((handle (dump-pop fasl-output)))
        (dolist (patch (patches))
          (push (cons handle (cdr patch))
                (gethash (car patch)
                         (fasl-output-patch-table fasl-output))))
        (dolist (named-constant (named-constants))
          (dump-object (car named-constant) fasl-output)
          (dump-push handle fasl-output)
          (dump-fop 'fop-named-constant-set fasl-output (cdr named-constant)))
        handle))))

;;; This is only called from assemfile, which doesn't exist in the target.
#+sb-xc-host
(defun dump-assembler-routines (code-segment octets fixups alloc-points routines file)
  (let ((n-fixup-elts (dump-fixups fixups alloc-points file)))
    ;; The name -> address table has to be created before applying fixups
    ;; because a fixup may refer to an entry point in the same code component.
    ;; So these go on the stack last, i.e. nearest the top.
    ;; Reversing sorts the entry points in ascending address order
    ;; except possibly when there are multiple entry points to one routine
    (unless (= (length (remove-duplicates (mapcar 'car routines)))
               (length routines))
      (error "Duplicated asm routine name"))
    (dolist (routine (reverse routines))
      (dump-object (car routine) file)
      (dump-integer (+ (label-position (cadr routine)) (caddr routine))
                    file))
    (dump-fop 'fop-assembler-code file)
    (dump-word (length routines) file)
    (dump-word (length octets) file)
    (dump-word n-fixup-elts file)
    (write-segment-contents code-segment (fasl-output-stream file))
    (dump-pop file)))

;;; Alter the code object referenced by CODE-HANDLE at the specified
;;; OFFSET, storing the object referenced by ENTRY-HANDLE.
(defun dump-alter-code-object (code-handle offset entry-handle file)
  (declare (type index code-handle entry-handle offset))
  (declare (type fasl-output file))
  (dump-push code-handle file)
  (dump-push entry-handle file)
  (dump-fop 'fop-alter-code file offset)
  (values))

;;; Dump the code, constants, etc. for component. We pass in the
;;; assembler fixups, code vector and node info.
(defun fasl-dump-component (component code-segment code-length fixups alloc-points file)
  (declare (type component component))
  (declare (type fasl-output file))

  #+sb-dyncount
  (let ((info (ir2-component-dyncount-info (component-info component))))
    (when info
      (fasl-validate-structure info file)))

  (let* ((2comp (component-info component))
         (entries (ir2-component-entries 2comp))
         (nfuns (length entries))
         (code-handle
           ;; fill in the placeholder elements of constants
           ;; with the NAME, ARGLIST, TYPE, INFO slots of each simple-fun.
           (let ((constants (ir2-component-constants 2comp))
                 (wordindex (+ sb-vm:code-constants-offset
                               (* sb-vm:code-slots-per-simple-fun nfuns))))
             (dolist (entry entries)
               ;; Process in reverse order of ENTRIES.
               ;; See also MAKE-CORE-COMPONENT which does the same thing.
               (decf wordindex sb-vm:code-slots-per-simple-fun)
               (setf (aref constants (+ wordindex sb-vm:simple-fun-name-slot))
                     `(:constant ,(entry-info-name entry))
                     (aref constants (+ wordindex sb-vm:simple-fun-arglist-slot))
                     `(:constant ,(entry-info-arguments entry))
                     (aref constants (+ wordindex sb-vm:simple-fun-source-slot))
                     `(:constant ,(entry-info-form/doc entry))
                     (aref constants (+ wordindex sb-vm:simple-fun-info-slot))
                     `(:constant ,(entry-info-type/xref entry))))
             (dump-code-object component code-segment code-length fixups
                               alloc-points file)))
         (fun-index nfuns))

    (dolist (entry entries)
      (dump-push code-handle file)
      (dump-fop 'fop-fun-entry file (decf fun-index))
      (let ((entry-handle (dump-pop file)))
        ;; When cross compiling, if the entry is a DEFUN, then we also
        ;; dump a FOP-FSET so that the cold loader can instantiate the
        ;; definition at cold-load time, allowing forward references
        ;; to functions in top-level forms. If the entry is a
        ;; DEFMETHOD, we dump a FOP-MSET so that the cold loader
        ;; recognizes the method definition.
        #+sb-xc-host
        (let ((name (entry-info-name entry)))
          (cond ((legal-fun-name-p name)
                 (dump-object name file)
                 (dump-push entry-handle file)
                 (dump-fop 'fop-fset file))
                ((and (listp name)
                      (eq (first name) 'sb-pcl::fast-method))
                 (let ((method (second name))
                       (qualifiers (butlast (cddr name)))
                       (specializers (first (last name))))
                   (dump-object method file)
                   (dump-object qualifiers file)
                   (dump-object specializers file)
                   (dump-push entry-handle file)
                   (dump-fop 'fop-mset file)))))
        (setf (gethash entry (fasl-output-entry-table file)) entry-handle)
        (let ((old (gethash entry (fasl-output-patch-table file))))
          (when old
            (dolist (patch old)
              (dump-alter-code-object (car patch)
                                      (cdr patch)
                                      entry-handle
                                      file))
            (remhash entry (fasl-output-patch-table file)))))))
  (values))

(defun dump-push-previously-dumped-fun (fun fasl-output)
  (declare (type clambda fun))
  (let ((handle (gethash (leaf-info fun)
                         (fasl-output-entry-table fasl-output))))
    (aver handle)
    (dump-push handle fasl-output))
  (values))

;;; Dump a FOP-FUNCALL to call an already-dumped top level lambda at
;;; load time.
(defun fasl-dump-toplevel-lambda-call (fun fasl-output)
  (declare (type clambda fun))
  (dump-push-previously-dumped-fun fun fasl-output)
  (dump-fop 'fop-funcall-for-effect fasl-output 0)
  (values))

;;; Dump some information to allow partial reconstruction of the
;;; DEBUG-SOURCE structure.
(defun fasl-dump-partial-source-info (info file)
  (declare (type source-info info) (type fasl-output file))
  (let ((partial (debug-source-for-info info)))
    (dump-object (debug-source-namestring partial) file)
    (dump-object (debug-source-created partial) file)
    (dump-object (debug-source-plist partial) file)
    (dump-fop 'fop-note-partial-source-info file)))

;;; Compute the correct list of DEBUG-SOURCE structures and backpatch
;;; all of the dumped DEBUG-INFO structures. We clear the
;;; FASL-OUTPUT-DEBUG-INFO, so that subsequent components with
;;; different source info may be dumped.
(defun fasl-dump-source-info (info file)
  (declare (type source-info info) (type fasl-output file))
  (let ((res (debug-source-for-info info)))
    (fasl-validate-structure res file)
    (dump-object res file)
    (let ((res-handle (dump-pop file)))
      (dolist (info-handle (fasl-output-debug-info file))
        (dump-push res-handle file)
        (symbol-macrolet
            ((debug-info-source-index
               (let ((dd (find-defstruct-description 'debug-info)))
                 (dsd-index (find 'source (dd-slots dd)
                                  :key #'dsd-name :test 'string=)))))
          (dump-fop 'fop-structset file info-handle debug-info-source-index)))))

  (setf (fasl-output-debug-info file) nil)
  (values))

;;;; dumping structures

(defun dump-structure (struct file)
  (unless (or (gethash struct (fasl-output-valid-structures file))
              ;; Assume that DEBUG-FUNs are always valid to dump.
              (typep struct 'debug-fun))
    (error "attempt to dump invalid structure:~%  ~S~%How did this happen?"
           struct))
  (note-potential-circularity struct file)
  (do* ((length (%instance-length struct))
        (layout (%instance-layout struct))
        (bitmap (layout-bitmap layout))
        (circ (fasl-output-circularity-table file))
        (index sb-vm:instance-data-start (1+ index)))
      ((>= index length)
       (dump-non-immediate-object layout file)
       (dump-fop 'fop-struct file length))
    (let* ((obj (if (logbitp index bitmap)
                    (%instance-ref struct index)
                    (%raw-instance-ref/word struct index)))
           (ref (gethash obj circ)))
      (sub-dump-object (cond (ref
                              (push (make-circularity :type :struct-set
                                                      :object struct
                                                      :index index
                                                      :value obj
                                                      :enclosing-object ref)
                                    *circularities-detected*)
                              nil)
                             (t obj))
                       file))))

(defun dump-layout (obj file)
  (when (layout-invalid obj)
    (compiler-error "attempt to dump reference to obsolete class: ~S"
                    (layout-classoid obj)))
  ;; STANDARD-OBJECT could in theory be dumpable, but nothing else,
  ;; because all its subclasses can evolve to have new layouts.
  (aver (not (logtest (layout-flags obj) +pcl-object-layout-flag+)))
  (let ((name (layout-classoid-name obj)))
    ;; Q: Shouldn't we aver that NAME is the proper name for its classoid?
    (unless name
      (compiler-error "dumping anonymous layout: ~S" obj))
    ;; The target lisp can save some space in fasls (sometimes),
    ;; but the cross-compiler can't because we need to construct the
    ;; cold representation of all layouts, not reference host layouts.
    #-sb-xc-host
    (let ((fop (known-layout-fop name)))
      (when fop
        (return-from dump-layout (dump-byte fop file))))
    (dump-object name file))
  (sub-dump-object (layout-bitmap obj) file)
  (sub-dump-object (layout-inherits obj) file)
  (dump-fop 'fop-layout file
            (1+ (layout-depthoid obj)) ; non-stack args can't be negative
            (logand (layout-flags obj) sb-kernel::layout-flags-mask)
            (layout-length obj)))

;;;; dumping instances which just save their slots

(defun dump-instance-saving-slots (object slot-names file)
  (note-potential-circularity object file)
  (let ((circ (fasl-output-circularity-table file)))
    (dolist (slot-name slot-names)
      (if (slot-boundp object slot-name)
          (let* ((value (slot-value object slot-name))
                 (ref (gethash value circ)))
            (cond (ref
                   (push (make-circularity :type :slot-set
                                           :object object
                                           :index 0
                                           :slot-name slot-name
                                           :value value
                                           :enclosing-object ref)
                         *circularities-detected*)
                   (sub-dump-object nil file))
                  (t
                   (sub-dump-object value file))))
          (dump-fop 'fop-misc-trap file))
      (sub-dump-object slot-name file)))
  (sub-dump-object (class-name (class-of object)) file)
  (dump-fop 'fop-instance file (length slot-names)))

;;;; code coverage

(defun dump-code-coverage-records (namestring cc file)
  (declare (type string namestring)
           (type list cc))
  (dump-object namestring file)
  (dump-object cc file)
  (dump-fop 'fop-record-code-coverage file))

(defun dump-emitted-full-calls (hash-table fasl)
  (let ((list (%hash-table-alist hash-table)))
    #+sb-xc-host ; enforce host-insensitive reproducible ordering
    (labels ((symbol< (a b)
               (cond ((string< a b) t)
                     ((string= a b)
                      ;; this does find a few pairs of lookalikes
                      (string< (cl:package-name (sb-xc:symbol-package a))
                               (cl:package-name (sb-xc:symbol-package b))))))
             (fname< (a b)
               (cond ((and (atom a) (atom b)) (symbol< a b))
                     ((atom a) t) ; symbol < list
                     ((atom b) nil) ; opposite
                     ((symbol< (cadr a) (cadr b))))))
      (setq list (sort list #'fname< :key #'car)))
    (dump-object list fasl)
    (dump-fop 'fop-note-full-calls fasl)))
