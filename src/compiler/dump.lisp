;;;; stuff that knows about dumping FASL files

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!FASL")
;;; KLUDGE: Even though we're IN-PACKAGE SB!FASL, some of the code in
;;; here is awfully chummy with the SB!C package. CMU CL didn't have
;;; any separation between the two packages, and a lot of tight
;;; coupling remains. -- WHN 2001-06-04

;;;; fasl dumper state

;;; The FASL-OUTPUT structure represents everything we need to
;;; know about dumping to a fasl file. (We need to objectify the
;;; state because the fasdumper must be reentrant.)
(defstruct (fasl-output
            #-no-ansi-print-object
            (:print-object (lambda (x s)
                             (print-unreadable-object (x s :type t)
                               (prin1 (namestring (fasl-output-stream x))
                                      s))))
            (:copier nil))
  ;; the stream we dump to
  (stream (missing-arg) :type stream)
  ;; hashtables we use to keep track of dumped constants so that we
  ;; can get them from the table rather than dumping them again. The
  ;; EQUAL-TABLE is used for lists and strings, and the EQ-TABLE is
  ;; used for everything else. We use a separate EQ table to avoid
  ;; performance pathologies with objects for which EQUAL degenerates
  ;; to EQL. Everything entered in the EQUAL table is also entered in
  ;; the EQ table.
  (equal-table (make-hash-table :test 'equal) :type hash-table)
  (eq-table (make-hash-table :test 'eq) :type hash-table)
  ;; the table's current free pointer: the next offset to be used
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
  (valid-structures (make-hash-table :test 'eq) :type hash-table))

;;; This structure holds information about a circularity.
(defstruct (circularity (:copier nil))
  ;; the kind of modification to make to create circularity
  (type (missing-arg) :type (member :rplaca :rplacd :svset :struct-set))
  ;; the object containing circularity
  object
  ;; index in object for circularity
  (index (missing-arg) :type index)
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

;;; used to turn off the structure validation during dumping of source
;;; info
(defvar *dump-only-valid-structures* t)
;;;; utilities

;;; Write the byte B to the specified FASL-OUTPUT stream.
(defun dump-byte (b fasl-output)
  (declare (type (unsigned-byte 8) b) (type fasl-output fasl-output))
  (write-byte b (fasl-output-stream fasl-output)))

;; Dump a word-sized integer.
(defun dump-word (num fasl-output)
  (declare (type sb!vm:word num))
  (declare (type fasl-output fasl-output))
  (let ((stream (fasl-output-stream fasl-output)))
    (dotimes (i sb!vm:n-word-bytes)
      (write-byte (ldb (byte 8 (* 8 i)) num) stream))))

;; Dump a 32-bit integer.
(defun dump-unsigned-byte-32 (num fasl-output)
  (declare (type sb!vm:word num))
  (declare (type fasl-output fasl-output))
  (let ((stream (fasl-output-stream fasl-output)))
    (dotimes (i 4)
      (write-byte (ldb (byte 8 (* 8 i)) num) stream))))

;;; Dump NUM to the fasl stream, represented by N bytes. This works
;;; for either signed or unsigned integers. There's no range checking
;;; -- if you don't specify enough bytes for the number to fit, this
;;; function cheerfully outputs the low bytes.
(defun dump-integer-as-n-bytes  (num bytes fasl-output)
  (declare (integer num) (type index bytes))
  (declare (type fasl-output fasl-output))
  (do ((n num (ash n -8))
       (i bytes (1- i)))
      ((= i 0))
    (declare (type index i))
    (dump-byte (logand n #xff) fasl-output))
  (values))

;;; Setting this variable to an (UNSIGNED-BYTE 32) value causes
;;; DUMP-FOP to use it as a counter and emit a FOP-NOP4 with the
;;; counter value before every ordinary fop. This can make it easier
;;; to follow the progress of LOAD-AS-FASL when
;;; debugging/testing/experimenting.
#!+sb-show (defvar *fop-nop4-count* nil)
#!+sb-show (declaim (type (or (unsigned-byte 32) null) *fop-nop4-count*))

;;; Dump the FOP code for the named FOP to the specified FASL-OUTPUT.
;;;
;;; FIXME: This should be a function, with a compiler macro expansion
;;; for the common constant-FS case. (Among other things, that'll stop
;;; it from EVALing ,FILE multiple times.)
;;;
;;; FIXME: Compiler macros, frozen classes, inlining, and similar
;;; optimizations should be conditional on #!+SB-FROZEN.
(defmacro dump-fop (fs file)
  (let* ((fs (eval fs))
         (val (get fs 'fop-code)))
    (if val
      `(progn
         #!+sb-show
         (when *fop-nop4-count*
           (dump-byte ,(get 'fop-nop4 'fop-code) ,file)
           (dump-integer-as-n-bytes (mod (incf *fop-nop4-count*) (expt 2 32))
                                    4 ,file))
         (dump-byte ',val ,file))
      (error "compiler bug: ~S is not a legal fasload operator." fs))))

;;; Dump a FOP-CODE along with an integer argument, choosing the FOP
;;; based on whether the argument will fit in a single byte.
;;;
;;; FIXME: This, like DUMP-FOP, should be a function with a
;;; compiler-macro expansion.
(defmacro dump-fop* (n byte-fop word-fop file)
  (once-only ((n-n n)
              (n-file file))
    `(cond ((< ,n-n 256)
            (dump-fop ',byte-fop ,n-file)
            (dump-byte ,n-n ,n-file))
           (t
            (dump-fop ',word-fop ,n-file)
            (dump-word ,n-n ,n-file)))))

;;; Push the object at table offset Handle on the fasl stack.
(defun dump-push (handle fasl-output)
  (declare (type index handle) (type fasl-output fasl-output))
  (dump-fop* handle fop-byte-push fop-push fasl-output)
  (values))

;;; Pop the object currently on the fasl stack top into the table, and
;;; return the table index, incrementing the free pointer.
(defun dump-pop (fasl-output)
  (prog1
      (fasl-output-table-free fasl-output)
    (dump-fop 'fop-pop fasl-output)
    (incf (fasl-output-table-free fasl-output))))

;;; If X is in File's EQUAL-TABLE, then push the object and return T,
;;; otherwise NIL.
(defun equal-check-table (x fasl-output)
  (declare (type fasl-output fasl-output))
  (let ((handle (gethash x (fasl-output-equal-table fasl-output))))
    (cond
     (handle (dump-push handle fasl-output) t)
     (t nil))))
(defun string-check-table (x fasl-output)
  (declare (type fasl-output fasl-output)
           (type string x))
  (let ((handle (cdr (assoc
                      #+sb-xc-host 'base-char ; for repeatable xc fasls
                      #-sb-xc-host (array-element-type x)
                      (gethash x (fasl-output-equal-table fasl-output))))))
    (cond
     (handle (dump-push handle fasl-output) t)
     (t nil))))

;;; These functions are called after dumping an object to save the
;;; object in the table. The object (also passed in as X) must already
;;; be on the top of the FOP stack.
(defun eq-save-object (x fasl-output)
  (declare (type fasl-output fasl-output))
  (let ((handle (dump-pop fasl-output)))
    (setf (gethash x (fasl-output-eq-table fasl-output)) handle)
    (dump-push handle fasl-output))
  (values))
(defun equal-save-object (x fasl-output)
  (declare (type fasl-output fasl-output))
  (let ((handle (dump-pop fasl-output)))
    (setf (gethash x (fasl-output-equal-table fasl-output)) handle)
    (setf (gethash x (fasl-output-eq-table fasl-output)) handle)
    (dump-push handle fasl-output))
  (values))
(defun string-save-object (x fasl-output)
  (declare (type fasl-output fasl-output)
           (type string x))
  (let ((handle (dump-pop fasl-output)))
    (push (cons #+sb-xc-host 'base-char ; repeatable xc fasls
                #-sb-xc-host (array-element-type x)
                handle)
          (gethash x (fasl-output-equal-table fasl-output)))
    (setf (gethash x (fasl-output-eq-table fasl-output)) handle)
    (dump-push handle fasl-output))
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
           ;; SB-EXT:STRING-TO-OCTETS is not available while cross-compiling
           #+sb-xc-host
           (loop for char across string
                 do (let ((code (char-code char)))
                      (unless (<= 0 code 127)
                        (setf char #\?))
                      (write-byte code stream)))
           ;; UTF-8 is safe to use, because +FASL-HEADER-STRING-STOP-CHAR-CODE+
           ;; may not appear in UTF-8 encoded bytes
           #-sb-xc-host
           (write-sequence (string-to-octets string :external-format :utf-8)
                           stream)))
    (let* ((stream (open name
                         :direction :output
                         :if-exists :supersede
                         :element-type 'sb!assem:assembly-unit))
           (res (make-fasl-output :stream stream)))
      ;; Before the actual FASL header, write a shebang line using the current
      ;; runtime path, so our fasls can be executed directly from the shell.
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
                   (sb!xc:lisp-implementation-type)
                   (sb!xc:lisp-implementation-version))))
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
        (dump-counted-string (sb!xc:lisp-implementation-version))
        (dump-counted-string *features-affecting-fasl-format*))
      res)))

;;; Close the specified FASL-OUTPUT, aborting the write if ABORT-P.
(defun close-fasl-output (fasl-output abort-p)
  (declare (type fasl-output fasl-output))

  (unless abort-p
    ;; sanity checks
    (aver (zerop (hash-table-count (fasl-output-patch-table fasl-output))))
    ;; End the group.
    (dump-fop 'fop-verify-empty-stack fasl-output)
    (dump-fop 'fop-verify-table-size fasl-output)
    (dump-word (fasl-output-table-free fasl-output)
               fasl-output)
    (dump-fop 'fop-end-group fasl-output))

  ;; That's all, folks.
  (close (fasl-output-stream fasl-output) :abort abort-p)
  (values))

;;;; main entries to object dumping

;;; This function deals with dumping objects that are complex enough
;;; so that we want to cache them in the table, rather than repeatedly
;;; dumping them. If the object is in the EQ-TABLE, then we push it,
;;; otherwise, we do a type dispatch to a type specific dumping
;;; function. The type specific branches do any appropriate
;;; EQUAL-TABLE check and table entry.
;;;
;;; When we go to dump the object, we enter it in the CIRCULARITY-TABLE.
(defun dump-non-immediate-object (x file)
  (let ((index (gethash x (fasl-output-eq-table file))))
    (cond (index
           (dump-push index file))
          (t
           (typecase x
             (symbol (dump-symbol x file))
             (list
              ;; KLUDGE: The code in this case has been hacked
              ;; to match Douglas Crosher's quick fix to CMU CL
              ;; (on cmucl-imp 1999-12-27), applied in sbcl-0.6.8.11
              ;; with help from Martin Atzmueller. This is not an
              ;; ideal solution; to quote DTC,
              ;;   The compiler locks up trying to coalesce the
              ;;   constant lists. The hack below will disable the
              ;;   coalescing of lists while dumping and allows
              ;;   the code to compile. The real fix would be to
              ;;   take a little more care while dumping these.
              ;; So if better list coalescing is needed, start here.
              ;; -- WHN 2000-11-07
              (if (maybe-cyclic-p x)
                  (progn
                    (dump-list x file)
                    (eq-save-object x file))
                  (unless (equal-check-table x file)
                    (dump-list x file)
                    (equal-save-object x file))))
             (layout
              (dump-layout x file)
              (eq-save-object x file))
             (instance
              (dump-structure x file)
              (eq-save-object x file))
             (array
              ;; DUMP-ARRAY (and its callees) are responsible for
              ;; updating the EQ and EQUAL hash tables.
              (dump-array x file))
             (number
              (unless (equal-check-table x file)
                (etypecase x
                  (ratio (dump-ratio x file))
                  (complex (dump-complex x file))
                  (float (dump-float x file))
                  (integer (dump-integer x file)))
                (equal-save-object x file)))
             (t
              ;; This probably never happens, since bad things tend to
              ;; be detected during IR1 conversion.
              (error "This object cannot be dumped into a fasl file:~% ~S"
                     x))))))
  (values))

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
        ((characterp x) (dump-character x file))
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
               (dump-fop 'fop-nthcdr file)
               (dump-word i file))
            (declare (type index i)))))

      (ecase (circularity-type info)
        (:rplaca     (dump-fop 'fop-rplaca    file))
        (:rplacd     (dump-fop 'fop-rplacd    file))
        (:svset      (dump-fop 'fop-svset     file))
        (:struct-set (dump-fop 'fop-structset file)))
      (dump-word (gethash (circularity-object info) table) file)
      (dump-word (circularity-index info) file))))

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
  (declare (type sb!c::clambda fun) (type fasl-output file))
  (let ((handle (gethash (sb!c::leaf-info fun)
                         (fasl-output-entry-table file))))
    (aver handle)
    (dump-push handle file)
    (dump-fop 'fop-funcall file)
    (dump-byte 0 file))
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

;;;; number dumping

(defun dump-ratio (x file)
  (sub-dump-object (numerator x) file)
  (sub-dump-object (denominator x) file)
  (dump-fop 'fop-ratio file))

(defun dump-integer (n file)
  (typecase n
    ((signed-byte 8)
     (dump-fop 'fop-byte-integer file)
     (dump-byte (logand #xFF n) file))
    ((unsigned-byte #.(1- sb!vm:n-word-bits))
     (dump-fop 'fop-word-integer file)
     (dump-word n file))
    ((signed-byte #.sb!vm:n-word-bits)
     (dump-fop 'fop-word-integer file)
     (dump-integer-as-n-bytes n #.sb!vm:n-word-bytes file))
    (t
     (let ((bytes (ceiling (1+ (integer-length n)) 8)))
       (dump-fop* bytes fop-small-integer fop-integer file)
       (dump-integer-as-n-bytes n bytes file)))))

(defun dump-float (x file)
  (etypecase x
    (single-float
     (dump-fop 'fop-single-float file)
     (dump-integer-as-n-bytes (single-float-bits x) 4 file))
    (double-float
     (dump-fop 'fop-double-float file)
     (let ((x x))
       (declare (double-float x))
       (dump-integer-as-n-bytes (double-float-low-bits x) 4 file)
       (dump-integer-as-n-bytes (double-float-high-bits x) 4 file)))
    #!+long-float
    (long-float
     (dump-fop 'fop-long-float file)
     (dump-long-float x file))))

(defun dump-complex-single-float (re im file)
  (declare (single-float re im))
  (dump-fop 'fop-complex-single-float file)
  (dump-integer-as-n-bytes (single-float-bits re) 4 file)
  (dump-integer-as-n-bytes (single-float-bits im) 4 file))

(defun dump-complex-double-float (re im file)
  (declare (double-float re im))
  (dump-fop 'fop-complex-double-float file)
  (dump-integer-as-n-bytes (double-float-low-bits re) 4 file)
  (dump-integer-as-n-bytes (double-float-high-bits re) 4 file)
  (dump-integer-as-n-bytes (double-float-low-bits im) 4 file)
  (dump-integer-as-n-bytes (double-float-high-bits im) 4 file))

(defun dump-complex-rational (re im file)
  (sub-dump-object re file)
  (sub-dump-object im file)
  (dump-fop 'fop-complex file))

#+sb-xc-host
(defun dump-complex (x file)
  (let ((re (realpart x))
        (im (imagpart x)))
    (cond ((and (typep re 'single-float)
                (typep im 'single-float))
           (dump-complex-single-float re im file))
          ((and (typep re 'double-float)
                (typep im 'double-float))
           (dump-complex-double-float re im file))
          ((and (typep re 'rational)
                (typep im 'rational))
           (dump-complex-rational re im file))
          (t
           (bug "Complex number too complex: ~S" x)))))

#-sb-xc-host
(defun dump-complex (x file)
  (typecase x
    ((complex single-float)
     (dump-complex-single-float (realpart x) (imagpart x) file))
    ((complex double-float)
     (dump-complex-double-float (realpart x) (imagpart x) file))
    #!+long-float
    ((complex long-float)
     (dump-fop 'fop-complex-long-float file)
     (dump-long-float (realpart x) file)
     (dump-long-float (imagpart x) file))
    (t
     (dump-complex-rational (realpart x) (imagpart x) file))))

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
         (let ((s (package-name pkg)))
           (dump-fop* (length s) fop-small-named-package-save fop-named-package-save file)
           #+sb-xc-host
           (dump-base-chars-of-string (coerce s 'simple-base-string) file)
           #-sb-xc-host
           (#!+sb-unicode dump-characters-of-string
            #!-sb-unicode dump-base-chars-of-string
            (coerce s '(simple-array character (*))) file))
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
(defun dump-list (list file)
  (aver (and list
             (not (gethash list (fasl-output-circularity-table file)))))
  (do* ((l list (cdr l))
        (n 0 (1+ n))
        (circ (fasl-output-circularity-table file)))
       ((atom l)
        (cond ((null l)
               (terminate-undotted-list n file))
              (t
               (sub-dump-object l file)
               (terminate-dotted-list n file))))
    (declare (type index n))
    (let ((ref (gethash l circ)))
      (when ref
        (push (make-circularity :type :rplacd
                                :object list
                                :index (1- n)
                                :value l
                                :enclosing-object ref)
              *circularities-detected*)
        (terminate-undotted-list n file)
        (return)))

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
            (t
             (sub-dump-object obj file))))))

(defun terminate-dotted-list (n file)
  (declare (type index n) (type fasl-output file))
  (case n
    (1 (dump-fop 'fop-list*-1 file))
    (2 (dump-fop 'fop-list*-2 file))
    (3 (dump-fop 'fop-list*-3 file))
    (4 (dump-fop 'fop-list*-4 file))
    (5 (dump-fop 'fop-list*-5 file))
    (6 (dump-fop 'fop-list*-6 file))
    (7 (dump-fop 'fop-list*-7 file))
    (8 (dump-fop 'fop-list*-8 file))
    (t (do ((nn n (- nn 255)))
           ((< nn 256)
            (dump-fop 'fop-list* file)
            (dump-byte nn file))
         (declare (type index nn))
         (dump-fop 'fop-list* file)
         (dump-byte 255 file)))))

;;; If N > 255, must build list with one LIST operator, then LIST*
;;; operators.

(defun terminate-undotted-list (n file)
  (declare (type index n) (type fasl-output file))
  (case n
    (1 (dump-fop 'fop-list-1 file))
    (2 (dump-fop 'fop-list-2 file))
    (3 (dump-fop 'fop-list-3 file))
    (4 (dump-fop 'fop-list-4 file))
    (5 (dump-fop 'fop-list-5 file))
    (6 (dump-fop 'fop-list-6 file))
    (7 (dump-fop 'fop-list-7 file))
    (8 (dump-fop 'fop-list-8 file))
    (t (cond ((< n 256)
              (dump-fop 'fop-list file)
              (dump-byte n file))
             (t (dump-fop 'fop-list file)
                (dump-byte 255 file)
                (do ((nn (- n 255) (- nn 255)))
                    ((< nn 256)
                     (dump-fop 'fop-list* file)
                     (dump-byte nn file))
                  (declare (type index nn))
                  (dump-fop 'fop-list* file)
                  (dump-byte 255 file)))))))

;;;; array dumping

;;; Dump the array thing.
(defun dump-array (x file)
  (if (vectorp x)
      (dump-vector x file)
      (dump-multi-dim-array x file)))

;;; Dump the vector object. If it's not simple, then actually dump a
;;; simple version of it. But we enter the original in the EQ or EQUAL
;;; tables.
(defun dump-vector (x file)
  (let ((simple-version (if (array-header-p x)
                            (coerce x `(simple-array
                                        ,(array-element-type x)
                                        (*)))
                            x)))
    (typecase simple-version
      #+sb-xc-host
      (simple-string
       (unless (string-check-table x file)
         (dump-simple-base-string simple-version file)
         (string-save-object x file)))
      #-sb-xc-host
      (simple-base-string
       (unless (string-check-table x file)
         (dump-simple-base-string simple-version file)
         (string-save-object x file)))
      #-sb-xc-host
      ((simple-array character (*))
       #!+sb-unicode
       (unless (string-check-table x file)
         (dump-simple-character-string simple-version file)
         (string-save-object x file))
       #!-sb-unicode
       (bug "how did we get here?"))
      (simple-vector
       (dump-simple-vector simple-version file)
       (eq-save-object x file))
      ((simple-array single-float (*))
       (dump-single-float-vector simple-version file)
       (eq-save-object x file))
      ((simple-array double-float (*))
       (dump-double-float-vector simple-version file)
       (eq-save-object x file))
      #!+long-float
      ((simple-array long-float (*))
       (dump-long-float-vector simple-version file)
       (eq-save-object x file))
      ((simple-array (complex single-float) (*))
       (dump-complex-single-float-vector simple-version file)
       (eq-save-object x file))
      ((simple-array (complex double-float) (*))
       (dump-complex-double-float-vector simple-version file)
       (eq-save-object x file))
      #!+long-float
      ((simple-array (complex long-float) (*))
       (dump-complex-long-float-vector simple-version file)
       (eq-save-object x file))
      (t
       (dump-i-vector simple-version file)
       (eq-save-object x file)))))

;;; Dump a SIMPLE-VECTOR, handling any circularities.
(defun dump-simple-vector (v file)
  (declare (type simple-vector v) (type fasl-output file))
  (note-potential-circularity v file)
  (do ((index 0 (1+ index))
       (length (length v))
       (circ (fasl-output-circularity-table file)))
      ((= index length)
       (dump-fop* length fop-small-vector fop-vector file))
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

;;; In the grand scheme of things I don't pretend to understand any
;;; more how this works, or indeed whether.  But to write out specialized
;;; vectors in the same format as fop-int-vector expects to read them
;;; we need to be target-endian.  dump-integer-as-n-bytes always writes
;;; little-endian (which is correct for all other integers) so for a bigendian
;;; target we need to swap octets -- CSR, after DB

(defun octet-swap (word bits)
  "BITS must be a multiple of 8"
  (do ((input word (ash input -8))
       (output 0 (logior (ash output 8) (logand input #xff)))
       (bits bits (- bits 8)))
      ((<= bits 0) output)))

(defun dump-i-vector (vec file &key data-only)
  (declare (type (simple-array * (*)) vec))
  (let ((len (length vec)))
    (labels ((dump-unsigned-vector (size bytes)
               (unless data-only
                 (dump-fop 'fop-int-vector file)
                 (dump-word len file)
                 (dump-byte size file))
               ;; The case which is easy to handle in a portable way is when
               ;; the element size is a multiple of the output byte size, and
               ;; happily that's the only case we need to be portable. (The
               ;; cross-compiler has to output debug information (including
               ;; (SIMPLE-ARRAY (UNSIGNED-BYTE 8) *).) The other cases are only
               ;; needed in the target SBCL, so we let them be handled with
               ;; unportable bit bashing.
               (cond ((>= size 7) ; easy cases
                      (multiple-value-bind (floor rem) (floor size 8)
                        (aver (or (zerop rem) (= rem 7)))
                        (when (= rem 7)
                          (setq size (1+ size))
                          (setq floor (1+ floor)))
                        (dovector (i vec)
                          (dump-integer-as-n-bytes
                           (ecase sb!c:*backend-byte-order*
                             (:little-endian i)
                             (:big-endian (octet-swap i size)))
                           floor file))))
                     (t ; harder cases, not supported in cross-compiler
                      (dump-raw-bytes vec bytes file))))
             (dump-signed-vector (size bytes)
               ;; Note: Dumping specialized signed vectors isn't
               ;; supported in the cross-compiler. (All cases here end
               ;; up trying to call DUMP-RAW-BYTES, which isn't
               ;; provided in the cross-compilation host, only on the
               ;; target machine.)
               (unless data-only
                 (dump-fop 'fop-signed-int-vector file)
                 (dump-word len file)
                 (dump-byte size file))
               (dump-raw-bytes vec bytes file)))
      (etypecase vec
        #-sb-xc-host
        ((simple-array nil (*))
         (dump-unsigned-vector 0 0))
        (simple-bit-vector
         (dump-unsigned-vector 1 (ceiling len 8))) ; bits to bytes
        ;; KLUDGE: This isn't the best way of expressing that the host
        ;; may not have specializations for (unsigned-byte 2) and
        ;; (unsigned-byte 4), which means that these types are
        ;; type-equivalent to (simple-array (unsigned-byte 8) (*));
        ;; the workaround is to remove them from the etypecase, since
        ;; they can't be dumped from the cross-compiler anyway. --
        ;; CSR, 2002-05-07
        #-sb-xc-host
        ((simple-array (unsigned-byte 2) (*))
         (dump-unsigned-vector 2 (ceiling (ash len 1) 8))) ; bits to bytes
        #-sb-xc-host
        ((simple-array (unsigned-byte 4) (*))
         (dump-unsigned-vector 4 (ceiling (ash len 2) 8))) ; bits to bytes
        #-sb-xc-host
        ((simple-array (unsigned-byte 7) (*))
         (dump-unsigned-vector 7 len))
        ((simple-array (unsigned-byte 8) (*))
         (dump-unsigned-vector 8 len))
        #-sb-xc-host
        ((simple-array (unsigned-byte 15) (*))
         (dump-unsigned-vector 15 (* 2 len)))
        ((simple-array (unsigned-byte 16) (*))
         (dump-unsigned-vector 16 (* 2 len)))
        #-sb-xc-host
        ((simple-array (unsigned-byte 31) (*))
         (dump-unsigned-vector 31 (* 4 len)))
        ((simple-array (unsigned-byte 32) (*))
         (dump-unsigned-vector 32 (* 4 len)))
        #-sb-xc-host
        #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
        ((simple-array (unsigned-byte 63) (*))
         (dump-unsigned-vector 63 (* 8 len)))
        #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
        ((simple-array (unsigned-byte 64) (*))
         (dump-unsigned-vector 64 (* 8 len)))
        ((simple-array (signed-byte 8) (*))
         (dump-signed-vector 8 len))
        ((simple-array (signed-byte 16) (*))
         (dump-signed-vector 16 (* 2 len)))
        #!+#.(cl:if (cl:= 32 sb!vm:n-word-bits) '(and) '(or))
        ((simple-array (unsigned-byte 29) (*))
         (dump-signed-vector 29 (* 4 len)))
        #!+#.(cl:if (cl:= 32 sb!vm:n-word-bits) '(and) '(or))
        ((simple-array (signed-byte 30) (*))
         (dump-signed-vector 30 (* 4 len)))
        ((simple-array (signed-byte 32) (*))
         (dump-signed-vector 32 (* 4 len)))
        #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
        ((simple-array (unsigned-byte 60) (*))
         (dump-signed-vector 60 (* 8 len)))
        #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
        ((simple-array (signed-byte 61) (*))
         (dump-signed-vector 61 (* 8 len)))
        #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
        ((simple-array (signed-byte 64) (*))
         (dump-signed-vector 64 (* 8 len)))))))

;;; Dump characters and string-ish things.

(defun dump-character (char file)
  (let ((code (sb!xc:char-code char)))
    (cond
      ((< code 256)
       (dump-fop 'fop-short-character file)
       (dump-byte code file))
      (t
       (dump-fop 'fop-character file)
       (dump-word code file)))))

(defun dump-base-chars-of-string (s fasl-output)
  (declare #+sb-xc-host (type simple-string s)
           #-sb-xc-host (type simple-base-string s)
           (type fasl-output fasl-output))
  (dovector (c s)
    (dump-byte (sb!xc:char-code c) fasl-output))
  (values))


;;; Dump a SIMPLE-BASE-STRING.
(defun dump-simple-base-string (s file)
  #+sb-xc-host (declare (type simple-string s))
  #-sb-xc-host (declare (type simple-base-string s))
  (dump-fop* (length s) fop-small-base-string fop-base-string file)
  (dump-base-chars-of-string s file)
  (values))

;;; If we get here, it is assumed that the symbol isn't in the table,
;;; but we are responsible for putting it there when appropriate.
(defun dump-symbol (s file)
  (declare (type fasl-output file))
  (let* ((pname (symbol-name s))
         (pname-length (length pname))
         (pkg (symbol-package s)))
    ;; see comment in genesis: we need this here for repeatable fasls
    #+sb-xc-host
    (multiple-value-bind (cl-symbol cl-status)
        (find-symbol (symbol-name s) sb!int:*cl-package*)
      (when (and (eq s cl-symbol)
                 (eq cl-status :external))
        ;; special case, to work around possible xc host "design
        ;; choice" weirdness in COMMON-LISP package
        (setq pkg sb!int:*cl-package*)))

    (cond ((null pkg)
           (dump-fop* pname-length
                      fop-uninterned-small-symbol-save
                      fop-uninterned-symbol-save
                      file))
          ;; CMU CL had FOP-SYMBOL-SAVE/FOP-SMALL-SYMBOL-SAVE fops which
          ;; used the current value of *PACKAGE*. Unfortunately that's
          ;; broken w.r.t. ANSI Common Lisp semantics, so those are gone
          ;; from SBCL.
          ;;((eq pkg *package*)
          ;; (dump-fop* pname-length
          ;;        fop-small-symbol-save
          ;;        fop-symbol-save file))
          ((eq pkg sb!int:*cl-package*)
           (dump-fop* pname-length
                      fop-lisp-small-symbol-save
                      fop-lisp-symbol-save
                      file))
          ((eq pkg sb!int:*keyword-package*)
           (dump-fop* pname-length
                      fop-keyword-small-symbol-save
                      fop-keyword-symbol-save
                      file))
          ((< pname-length 256)
           (dump-fop* (dump-package pkg file)
                      fop-small-symbol-in-byte-package-save
                      fop-small-symbol-in-package-save
                      file)
           (dump-byte pname-length file))
          (t
           (dump-fop* (dump-package pkg file)
                      fop-symbol-in-byte-package-save
                      fop-symbol-in-package-save
                      file)
           (dump-word pname-length file)))

    #+sb-xc-host (dump-base-chars-of-string pname file)
    #-sb-xc-host (#!+sb-unicode dump-characters-of-string
                  #!-sb-unicode dump-base-chars-of-string
                  pname file)

    (setf (gethash s (fasl-output-eq-table file))
          (fasl-output-table-free file))

    (incf (fasl-output-table-free file)))

  (values))

;;;; component (function) dumping

(defun dump-segment (segment code-length fasl-output)
  (declare (type sb!assem:segment segment)
           (type fasl-output fasl-output))
  (let* ((stream (fasl-output-stream fasl-output))
         (n-written (write-segment-contents segment stream)))
    ;; In CMU CL there was no enforced connection between the CODE-LENGTH
    ;; argument and the number of bytes actually written. I added this
    ;; assertion while trying to debug portable genesis. -- WHN 19990902
    (unless (= code-length n-written)
      (bug "code-length=~W, n-written=~W" code-length n-written)))
  (values))

;;; Dump all the fixups. Currently there are three flavors of fixup:
;;;  - assembly routines: named by a symbol
;;;  - foreign (C) symbols: named by a string
;;;  - code object references: don't need a name.
(defun dump-fixups (fixups fasl-output)
  (declare (list fixups) (type fasl-output fasl-output))
  (dolist (note fixups)
    (let* ((kind (fixup-note-kind note))
           (fixup (fixup-note-fixup note))
           (position (fixup-note-position note))
           (name (fixup-name fixup))
           (flavor (fixup-flavor fixup)))
      (dump-object kind fasl-output)
      ;; Depending on the flavor, we may have various kinds of
      ;; noise before the position.
      (ecase flavor
        (:assembly-routine
         (aver (symbolp name))
         (dump-object name fasl-output)
         (dump-fop 'fop-assembler-fixup fasl-output))
        ((:foreign :foreign-dataref)
         (aver (stringp name))
         (ecase flavor
           (:foreign
            (dump-fop 'fop-foreign-fixup fasl-output))
           #!+linkage-table
           (:foreign-dataref
            (dump-fop 'fop-foreign-dataref-fixup fasl-output)))
         (let ((len (length name)))
           (aver (< len 256)) ; (limit imposed by fop definition)
           (dump-byte len fasl-output)
           (dotimes (i len)
             (dump-byte (char-code (schar name i)) fasl-output))))
        (:code-object
         (aver (null name))
         (dump-fop 'fop-code-object-fixup fasl-output)))
      ;; No matter what the flavor, we'll always dump the position
      (dump-word position fasl-output)))
  (values))

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
(defun dump-code-object (component
                         code-segment
                         code-length
                         trace-table-as-list
                         fixups
                         fasl-output)

  (declare (type component component)
           (list trace-table-as-list)
           (type index code-length)
           (type fasl-output fasl-output))

  (let* ((2comp (component-info component))
         (constants (sb!c::ir2-component-constants 2comp))
         (header-length (length constants))
         (packed-trace-table (pack-trace-table trace-table-as-list))
         (total-length (+ code-length
                          (* (length packed-trace-table)
                             sb!c::tt-bytes-per-entry))))

    (collect ((patches))

      ;; Dump the offset of the trace table.
      (dump-object code-length fasl-output)
      ;; FIXME: As long as we don't have GENGC, the trace table is
      ;; hardwired to be empty. And SBCL doesn't have GENGC (and as
      ;; far as I know no modern CMU CL does either -- WHN
      ;; 2001-10-05). So might we be able to get rid of trace tables?
      ;;
      ;; Note that gencgc also does something with the trace table.

      ;; Dump the constants, noting any :ENTRY constants that have to
      ;; be patched.
      (loop for i from sb!vm:code-constants-offset below header-length do
        (let ((entry (aref constants i)))
          (etypecase entry
            (constant
             (dump-object (sb!c::constant-value entry) fasl-output))
            (cons
             (ecase (car entry)
               (:entry
                (let* ((info (sb!c::leaf-info (cdr entry)))
                       (handle (gethash info
                                        (fasl-output-entry-table
                                         fasl-output))))
                  (declare (type sb!c::entry-info info))
                  (cond
                   (handle
                    (dump-push handle fasl-output))
                   (t
                    (patches (cons info i))
                    (dump-fop 'fop-misc-trap fasl-output)))))
               (:load-time-value
                (dump-push (cdr entry) fasl-output))
               (:fdefinition
                (dump-object (cdr entry) fasl-output)
                (dump-fop 'fop-fdefinition fasl-output))))
            (null
             (dump-fop 'fop-misc-trap fasl-output)))))

      ;; Dump the debug info.
      (let ((info (sb!c::debug-info-for-component component))
            (*dump-only-valid-structures* nil))
        (dump-object info fasl-output)
        (let ((info-handle (dump-pop fasl-output)))
          (dump-push info-handle fasl-output)
          (push info-handle (fasl-output-debug-info fasl-output))))

      (let ((num-consts (- header-length sb!vm:code-trace-table-offset-slot)))
        (cond ((and (< num-consts #x100) (< total-length #x10000))
               (dump-fop 'fop-small-code fasl-output)
               (dump-byte num-consts fasl-output)
               (dump-integer-as-n-bytes total-length (/ sb!vm:n-word-bytes 2) fasl-output))
              (t
               (dump-fop 'fop-code fasl-output)
               (dump-word num-consts fasl-output)
               (dump-word total-length fasl-output))))

      ;; These two dumps are only ones which contribute to our
      ;; TOTAL-LENGTH value.
      (dump-segment code-segment code-length fasl-output)
      (dump-i-vector packed-trace-table fasl-output :data-only t)

      ;; DUMP-FIXUPS does its own internal DUMP-FOPs: the bytes it
      ;; dumps aren't included in the TOTAL-LENGTH passed to our
      ;; FOP-CODE/FOP-SMALL-CODE fop.
      (dump-fixups fixups fasl-output)

      (dump-fop 'fop-sanctify-for-execution fasl-output)

      (let ((handle (dump-pop fasl-output)))
        (dolist (patch (patches))
          (push (cons handle (cdr patch))
                (gethash (car patch)
                         (fasl-output-patch-table fasl-output))))
        handle))))

(defun dump-assembler-routines (code-segment length fixups routines file)
  (dump-fop 'fop-assembler-code file)
  (dump-word length file)
  (write-segment-contents code-segment (fasl-output-stream file))
  (dolist (routine routines)
    (dump-object (car routine) file)
    (dump-fop 'fop-assembler-routine file)
    (dump-word (label-position (cdr routine)) file))
  (dump-fixups fixups file)
  (dump-fop 'fop-sanctify-for-execution file)
  (dump-pop file))

;;; Dump a function entry data structure corresponding to ENTRY to
;;; FILE. CODE-HANDLE is the table offset of the code object for the
;;; component.
(defun dump-one-entry (entry code-handle file)
  (declare (type sb!c::entry-info entry) (type index code-handle)
           (type fasl-output file))
  (let ((name (sb!c::entry-info-name entry)))
    (dump-push code-handle file)
    (dump-object name file)
    (dump-object (sb!c::entry-info-arguments entry) file)
    (dump-object (sb!c::entry-info-type entry) file)
    (dump-object (sb!c::entry-info-info entry) file)
    (dump-fop 'fop-fun-entry file)
    (dump-word (label-position (sb!c::entry-info-offset entry)) file)
    (dump-pop file)))

;;; Alter the code object referenced by CODE-HANDLE at the specified
;;; OFFSET, storing the object referenced by ENTRY-HANDLE.
(defun dump-alter-code-object (code-handle offset entry-handle file)
  (declare (type index code-handle entry-handle offset))
  (declare (type fasl-output file))
  (dump-push code-handle file)
  (dump-push entry-handle file)
  (dump-fop* offset fop-byte-alter-code fop-alter-code file)
  (values))

;;; Dump the code, constants, etc. for component. We pass in the
;;; assembler fixups, code vector and node info.
(defun fasl-dump-component (component
                            code-segment
                            code-length
                            trace-table
                            fixups
                            file)
  (declare (type component component) (list trace-table))
  (declare (type fasl-output file))

  (dump-fop 'fop-verify-table-size file)
  (dump-word (fasl-output-table-free file) file)

  #!+sb-dyncount
  (let ((info (sb!c::ir2-component-dyncount-info (component-info component))))
    (when info
      (fasl-validate-structure info file)))

  (let ((code-handle (dump-code-object component
                                       code-segment
                                       code-length
                                       trace-table
                                       fixups
                                       file))
        (2comp (component-info component)))

    (dolist (entry (sb!c::ir2-component-entries 2comp))
      (let ((entry-handle (dump-one-entry entry code-handle file)))
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
  (declare (type sb!c::clambda fun))
  (let ((handle (gethash (sb!c::leaf-info fun)
                         (fasl-output-entry-table fasl-output))))
    (aver handle)
    (dump-push handle fasl-output))
  (values))

;;; Dump a FOP-FUNCALL to call an already-dumped top level lambda at
;;; load time.
(defun fasl-dump-toplevel-lambda-call (fun fasl-output)
  (declare (type sb!c::clambda fun))
  (dump-push-previously-dumped-fun fun fasl-output)
  (dump-fop 'fop-funcall-for-effect fasl-output)
  (dump-byte 0 fasl-output)
  (values))

;;; Dump a FOP-FSET to arrange static linkage (at cold init) between
;;; FUN-NAME and the already-dumped function whose dump handle is
;;; FUN-DUMP-HANDLE.
#+sb-xc-host
(defun fasl-dump-cold-fset (fun-name fun-dump-handle fasl-output)
  (declare (type fixnum fun-dump-handle))
  (aver (legal-fun-name-p fun-name))
  (dump-non-immediate-object fun-name fasl-output)
  (dump-push fun-dump-handle fasl-output)
  (dump-fop 'fop-fset fasl-output)
  (values))

;;; Compute the correct list of DEBUG-SOURCE structures and backpatch
;;; all of the dumped DEBUG-INFO structures. We clear the
;;; FASL-OUTPUT-DEBUG-INFO, so that subsequent components with
;;; different source info may be dumped.
(defun fasl-dump-source-info (info fasl-output)
  (declare (type sb!c::source-info info))
  (let ((res (sb!c::debug-source-for-info info))
        (*dump-only-valid-structures* nil))
    #+sb-xc-host (setf (sb!c::debug-source-created res) 0
                       (sb!c::debug-source-compiled res) 0)
    (dump-object res fasl-output)
    (let ((res-handle (dump-pop fasl-output)))
      (dolist (info-handle (fasl-output-debug-info fasl-output))
        (dump-push res-handle fasl-output)
        (dump-fop 'fop-structset fasl-output)
        (dump-word info-handle fasl-output)
        (dump-word sb!c::+debug-info-source-index+ fasl-output))
      #+sb-xc-host
      (progn
        (dump-push res-handle fasl-output)
        (dump-fop 'fop-note-debug-source fasl-output))))
  (setf (fasl-output-debug-info fasl-output) nil)
  (values))

;;;; dumping structures

(defun dump-structure (struct file)
  (when *dump-only-valid-structures*
    (unless (gethash struct (fasl-output-valid-structures file))
      (error "attempt to dump invalid structure:~%  ~S~%How did this happen?"
             struct)))
  (note-potential-circularity struct file)
  (aver (%instance-ref struct 0))
  (do* ((length (%instance-length struct))
        (ntagged (- length (layout-n-untagged-slots (%instance-ref struct 0))))
        (circ (fasl-output-circularity-table file))
        ;; last slot first on the stack, so that the layout is on top:
        (index (1- length) (1- index)))
      ((minusp index)
       (dump-fop* length fop-small-struct fop-struct file))
    (let* ((obj (if (>= index ntagged)
                    (%raw-instance-ref/word struct (- length index 1))
                    (%instance-ref struct index)))
           (ref (gethash obj circ)))
      (cond (ref
             (aver (not (zerop index)))
             (push (make-circularity :type :struct-set
                                     :object struct
                                     :index index
                                     :value obj
                                     :enclosing-object ref)
                   *circularities-detected*)
             (sub-dump-object nil file))
            (t
             (sub-dump-object obj file))))))

(defun dump-layout (obj file)
  (when (layout-invalid obj)
    (compiler-error "attempt to dump reference to obsolete class: ~S"
                    (layout-classoid obj)))
  (let ((name (classoid-name (layout-classoid obj))))
    (unless name
      (compiler-error "dumping anonymous layout: ~S" obj))
    (dump-object name file))
  (sub-dump-object (layout-inherits obj) file)
  (sub-dump-object (layout-depthoid obj) file)
  (sub-dump-object (layout-length obj) file)
  (sub-dump-object (layout-n-untagged-slots obj) file)
  (dump-fop 'fop-layout file))
