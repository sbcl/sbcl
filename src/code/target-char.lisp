;;;; character functions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;; We compile some trivial character operations via inline expansion.
(declaim (inline standard-char-p graphic-char-p alpha-char-p
                 alphanumericp))
(declaim (maybe-inline upper-case-p lower-case-p both-case-p
                       digit-char-p))

(deftype char-code ()
  `(integer 0 (,char-code-limit)))

(declaim (inline pack-3-codepoints))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun pack-3-codepoints (first &optional (second 0) (third 0))
    (declare (type (unsigned-byte 21) first second third))
    (sb-c::mask-signed-field 63 (logior first (ash second 21) (ash third 42)))))

(defun unpack-3-codepoints (codepoints)
  (declare (type (signed-byte 63) codepoints))
  (cond ((< codepoints (ash 1 21))
         (list (code-char codepoints)))
        ((< codepoints (ash 1 (* 21 2)))
         (list (code-char (ldb (byte 21 0) codepoints))
               (code-char (ldb (byte 21 21) codepoints))))
        (t
         (list (code-char (ldb (byte 21 0) codepoints))
               (code-char (ldb (byte 21 21) codepoints))
               (code-char (ldb (byte 21 (* 21 2)) codepoints))))))

(declaim (inline clear-flag))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun clear-flag (bit integer)
    (logandc2 integer (ash 1 bit))))

(eval-when (:compile-toplevel)
  (defconstant +misc-width+ 9)
  (defmacro misc-index-from-char-code (codepoint high-pages low-pages)
    `(let* ((cp ,codepoint)
            (cp-high (ash cp -8))
            (high-index (aref ,high-pages cp-high)))
       (if (logbitp 15 high-index)
           (* ,+misc-width+ (clear-flag 15 high-index))
           (* ,+misc-width+ (aref ,low-pages (* 2 (+ (ldb (byte 8 0) cp) (ash high-index 8))))))))
  (setf (sb-xc:macro-function 'misc-index-from-char-code)
        (lambda (form env)
          (declare (ignore env))
          (funcall (cl:macro-function 'misc-index-from-char-code) form nil)))
)

(macrolet ((frob ()
             (flet ((file (name type)
                      (sb-cold:find-bootstrap-file (format nil "output/~A.~A" name type)))
                    (read-ub8-vector (pathname)
                      (with-open-file (stream pathname
                                              :element-type '(unsigned-byte 8))
                        (let* ((length (file-length stream))
                               (array (sb-xc:make-array
                                       length :element-type '(unsigned-byte 8)
                                       :retain-specialization-for-after-xc-core t)))
                          (read-sequence array stream)
                          array)))
                    (make-ubn-vector (raw-bytes n)
                      (let* ((et (if (= n 3)
                                     '(unsigned-byte 31)
                                     `(unsigned-byte ,(* 8 n))))
                             (array (sb-xc:make-array (/ (length raw-bytes) n)
                                                      :element-type et
                                                      :retain-specialization-for-after-xc-core t)))
                        (loop for i from 0 below (length raw-bytes) by n
                           do (loop with element = 0
                                 for offset from 0 below n
                                 do (incf element (ash (aref raw-bytes (+ i offset))
                                                       (* 8 (- n offset 1))))
                                 finally (setf (aref array (/ i n)) element)))
                        array)))
              (let* ((misc-database (read-ub8-vector (file "ucdmisc" "dat")))
                     (ucd-high-pages (read-ub8-vector (file "ucdhigh" "dat")))
                     (ucd-low-pages (read-ub8-vector (file "ucdlow" "dat")))
                     (decompositions (read-ub8-vector (file "decomp" "dat")))
                     (primary-compositions (read-ub8-vector (file "comp" "dat")))
                     (case-data (read-ub8-vector (file "case" "dat")))
                     (case-pages (read-ub8-vector (file "casepages" "dat")))
                     (collations (read-ub8-vector (file "collation" "dat")))
                     (high-pages (make-ubn-vector ucd-high-pages 2))
                     (low-pages (make-ubn-vector ucd-low-pages 2))
                     (%*character-case-pages*% (make-ubn-vector case-pages 1)))

                 `(progn
                    (defconstant-eqx sb-unicode::+character-misc-database+ ,misc-database #'equalp)
                    (defconstant-eqx sb-unicode::+character-high-pages+ ,high-pages #'equalp)
                    (defconstant-eqx sb-unicode::+character-low-pages+ ,low-pages #'equalp)
                    (defconstant-eqx sb-unicode::+character-decompositions+
                        ,(make-ubn-vector decompositions 3) #'equalp)
                    (defconstant-eqx +character-case-pages+ ,%*character-case-pages*% #'equalp)
                    (declaim (hash-table **character-primary-compositions**))
                    (define-load-time-global **character-primary-compositions**
                        ,(let ((info (make-ubn-vector primary-compositions 3))
                               (alist))
                           (dotimes (i (/ (length info) 3))
                             (let* ((3i (* 3 i))
                                    (key (dpb (aref info 3i) (byte 21 21) (aref info (1+ 3i))))
                                    (value (aref info (+ 3i 2))))
                               (push (cons key value) alist)))
                           `(%stuff-hash-table (make-hash-table #+64-bit :test #+64-bit #'eq
                                                                :size ,(/ (length info) 3))
                                               ,(nreverse (coerce alist 'vector)))))

                    ,@(let* ((unicode-table
                                 (make-array
                                  (* 64 (1+ (aref %*character-case-pages*%
                                                  (1- (length %*character-case-pages*%)))))
                                  :initial-element 0))
                               (table (sb-xc:make-array
                                       (* 2 (length unicode-table))
                                       :retain-specialization-for-after-xc-core t
                                       :element-type '(unsigned-byte 32)))
                               (info case-data)
                               (index 0)
                               (length (length info)))
                          (labels ((read-codepoint ()
                                     (let* ((b1 (aref info index))
                                            (b2 (aref info (incf index)))
                                            (b3 (aref info (incf index))))
                                       (incf index)
                                       (dpb b1 (byte 8 16)
                                            (dpb b2 (byte 8 8) b3))))
                                   (read-length-tagged ()
                                     (let ((len (aref info index)) ret)
                                       (incf index)
                                       (cond ((zerop len)
                                              (read-codepoint))
                                             (t
                                              (dotimes (i len)
                                                (push (read-codepoint) ret))
                                              (nreverse ret))))))
                            (loop until (>= index length)
                                  for key = (read-codepoint)
                                  for upper = (read-length-tagged)
                                  for lower = (read-length-tagged)
                                  for page = (aref %*character-case-pages*% (ash key -6))
                                  for i = (+ (ash page 6) (ldb (byte 6 0) key))
                                  do
                                  (setf (aref unicode-table i)
                                        (if (or (consp upper)
                                                (consp lower))
                                            (cons upper lower)
                                            (dpb upper (byte 21 21) lower)))
                                  when
                                  (flet (#+sb-unicode
                                         (both-case-p-local (code)
                                           (logbitp 7 (aref misc-database
                                                            (+ 5 (misc-index-from-char-code
                                                                  code high-pages low-pages))))))
                                    (and (atom upper)
                                         (atom lower)
                                         ;; Some characters are only equal under unicode rules,
                                         ;; e.g. #\MICRO_SIGN and #\GREEK_CAPITAL_LETTER_MU
                                         #+sb-unicode
                                         (both-case-p-local lower)
                                         #+sb-unicode
                                         (both-case-p-local upper)))
                                  do
                                  (setf (aref table (* i 2)) lower
                                        (aref table (1+ (* i 2))) upper)))
                          `((defconstant-eqx +character-unicode-cases+ ,unicode-table #'equalp)
                            (defconstant-eqx +character-cases+ ,table #'equalp)))

                    (define-load-time-global **character-collations**
                             ,(let* ((n-entries
                                         (with-open-file (s (file "n-collation-entries" "lisp-expr"))
                                           (read s)))
                                     (table)
                                     (index 0)
                                     (info (make-ubn-vector collations 4))
                                     (len (length info)))
                                (loop while (< index len) do
                                      (let* ((entry-head (aref info index))
                                             (cp-length (ldb (byte 4 28) entry-head))
                                             (key-length (ldb (byte 5 23) entry-head))
                                             (key 0)
                                             (codepoints nil))
                                        (aver (and (/= cp-length 0) (/= key-length 0)))
                                        (loop repeat cp-length do
                                              (push (dpb 0 (byte 10 22) (aref info index))
                                                    codepoints)
                                              (incf index))
                                        (setf codepoints (nreverse codepoints))
                                        (dotimes (i key-length)
                                          (setf (ldb (byte 32 (* i 32)) key) (aref info index))
                                          (incf index))
                                        ;; verify the validity of
                                        ;; :test 'eq on 64-bit
                                        #+64-bit (aver (sb-xc:typep (apply #'pack-3-codepoints codepoints)
                                                                    'sb-xc:fixnum))
                                        (push (cons (apply #'pack-3-codepoints codepoints) key) table)))
                                (aver (= (length table) n-entries))
                                `(%stuff-hash-table
                                  (make-hash-table :size ,n-entries #+64-bit :test #+64-bit #'eq)
                                  ,(nreverse (coerce table 'vector)))))

                    ,@(with-open-file
                         (stream (file "ucd-names" "lisp-expr"))
                       (with-open-file (u1-stream (file "ucd1-names" "lisp-expr"))
                         (flet ((convert-to-double-vector (vector &optional reversed)
                                  (let ((result (make-array (* (length vector) 2))))
                                    (loop for (code . name) across vector
                                          for i by 2
                                          do
                                          (when reversed
                                            (rotatef code name))
                                          (setf (aref result i) code
                                                (aref result (1+ i)) name))
                                    result)))
                           (let ((names (make-hash-table))
                                 (u1-names (make-hash-table)))
                             (loop
                               for code-point = (read stream nil nil)
                               for char-name = (string-upcase (read stream nil nil))
                               while code-point
                               do (setf (gethash code-point names) char-name))
                             (loop
                               for code-point = (read u1-stream nil nil)
                               for char-name = (string-upcase (read u1-stream nil nil))
                               while code-point
                               do (setf (gethash code-point u1-names) char-name))
                             (let ((tree
                                     (make-huffman-tree
                                      (let (list)
                                        (maphash (lambda (code name)
                                                   (declare (ignore code))
                                                   (push name list))
                                                 names)
                                        (maphash (lambda (code u1-name)
                                                   (declare (ignore code))
                                                   (push u1-name list))
                                                 u1-names)
                                        list)))
                                   (code->name
                                     (make-array (hash-table-count names)
                                                 :fill-pointer 0))
                                   (name->code nil)
                                   (code->u1-name
                                     (make-array (hash-table-count u1-names)
                                                 :fill-pointer 0))
                                   (u1-name->code nil))
                               (maphash (lambda (code name)
                                          (vector-push
                                           (cons code (huffman-encode name tree))
                                           code->name))
                                        names)
                               (maphash (lambda (code name)
                                          (vector-push
                                           (cons code (huffman-encode name tree))
                                           code->u1-name))
                                        u1-names)
                               (setf name->code
                                     (sort (copy-seq code->name) #'< :key #'cdr)
                                     code->name
                                     (sort (copy-seq name->code) #'< :key #'car)
                                     u1-name->code
                                     (sort (copy-seq code->u1-name) #'< :key #'cdr)
                                     code->u1-name
                                     (sort (copy-seq u1-name->code) #'< :key #'car))
                               `((defconstant-eqx +unicode-char-name-database+
                                     ,(convert-to-double-vector code->name) #'equalp)
                                 (defconstant-eqx +unicode-name-char-database+
                                     ,(convert-to-double-vector name->code t) #'equalp)
                                 (defconstant-eqx sb-unicode::+unicode-1-char-name-database+
                                     ,(convert-to-double-vector code->u1-name) #'equalp)
                                 (defconstant-eqx +unicode-1-name-char-database+
                                     ,(convert-to-double-vector u1-name->code t) #'equalp)
                                 (defconstant-eqx sb-unicode::+unicode-character-name-huffman-tree+
                                     ',tree #'equal))))))))))))

  (frob))

(define-load-time-global *base-char-name-alist*
  ;; Note: The *** markers here indicate character names which are
  ;; required by the ANSI specification of #'CHAR-NAME. For the others,
  ;; we prefer the ASCII standard name.
  '((#x00 "Nul" "Null" "^@")
    (#x01 "Soh" "^a")
    (#x02 "Stx" "^b")
    (#x03 "Etx" "^c")
    (#x04 "Eot" "^d")
    (#x05 "Enq" "^e")
    (#x06 "Ack" "^f")
    ;; Don't alias to Bell, another Unicode character has that name.
    (#x07 "Bel" "^g")
    (#x08 "Backspace" "^h" "Bs") ; *** See Note above
    (#x09 "Tab" "^i" "Ht") ; *** See Note above
    (#x0A "Newline" "Linefeed" "^j" "Lf" "Nl") ; *** See Note above
    (#x0B "Vt" "^k")
    (#x0C "Page" "^l" "Form" "Formfeed" "Ff" "Np") ; *** See Note above
    (#x0D "Return" "^m" "Cr") ; *** See Note above
    (#x0E "So" "^n")
    (#x0F "Si" "^o")
    (#x10 "Dle" "^p")
    (#x11 "Dc1" "^q")
    (#x12 "Dc2" "^r")
    (#x13 "Dc3" "^s")
    (#x14 "Dc4" "^t")
    (#x15 "Nak" "^u")
    (#x16 "Syn" "^v")
    (#x17 "Etb" "^w")
    (#x18 "Can" "^x")
    (#x19 "Em" "^y")
    (#x1A "Sub" "^z")
    (#x1B "Esc" "Escape" "^[" "Altmode" "Alt")
    (#x1C "Fs" "^\\")
    (#x1D "Gs" "^]")
    (#x1E "Rs" "^^")
    (#x1F "Us" "^_")
    (#x20 "Space" "Sp") ; *** See Note above
    (#x7f "Rubout" "Delete" "Del")
    (#x80 "C80")
    (#x81 "C81")
    (#x82 "Break-Permitted")
    (#x83 "No-Break-Permitted")
    (#x84 "C84")
    (#x85 "Next-Line")
    (#x86 "Start-Selected-Area")
    (#x87 "End-Selected-Area")
    (#x88 "Character-Tabulation-Set")
    (#x89 "Character-Tabulation-With-Justification")
    (#x8A "Line-Tabulation-Set")
    (#x8B "Partial-Line-Forward")
    (#x8C "Partial-Line-Backward")
    (#x8D "Reverse-Linefeed")
    (#x8E "Single-Shift-Two")
    (#x8F "Single-Shift-Three")
    (#x90 "Device-Control-String")
    (#x91 "Private-Use-One")
    (#x92 "Private-Use-Two")
    (#x93 "Set-Transmit-State")
    (#x94 "Cancel-Character")
    (#x95 "Message-Waiting")
    (#x96 "Start-Guarded-Area")
    (#x97 "End-Guarded-Area")
    (#x98 "Start-String")
    (#x99 "C99")
    (#x9A "Single-Character-Introducer")
    (#x9B "Control-Sequence-Introducer")
    (#x9C "String-Terminator")
    (#x9D "Operating-System-Command")
    (#x9E "Privacy-Message")
    (#x9F "Application-Program-Command"))) ; *** See Note above

;;;; UCD accessor functions

;;; The character database is made of several arrays.
;;; +CHARACTER-MISC-DATABASE+ is an array of bytes that encode character
;;; attributes. Each entry in the misc database is +misc-width+ (currently 9)
;;; bytes wide. Within each entry, the bytes represent: general category, BIDI
;;; class, canonical combining class, digit value, decomposition info, other
;;; flags, script, line break class, and age, respectively. Several of the
;;; entries have additional information encoded in them at the bit level. The
;;; digit value field is equal to 128 (has only its high bit set) if characters
;;; with that set of attribute are not digits. Bit 6 is set if that entry
;;; encodes decimal digits, that is, characters that are DIGIT-CHAR-P. The rest
;;; of the value is the digit value of characters with that entry. Decomposition
;;; info contains the length of the decomposition of characters with that entry,
;;; and also sets its high bit if the decompositions are compatibility
;;; decompositions. The other flags byte encodes boolean properties. Bit 7 is
;;; set if the entry's characters are BOTH-CASE-P in the Common Lisp sense. Bit
;;; 6 is set if the entry's characters hav a defined case transformation in
;;; Unicode. Bit 5 is set if the characters have the property BIDI_Mirrored=Y.
;;; Bits 3-0 encode the entry's East Asian Width. Bit 4 is unused. Age stores
;;; the minor version in bits 0-2, and the major version in the remaining 5
;;; bits.
;;;
;;; To find which entry in +CHARACTER-MISC-DATABASE+ encodes a character's
;;; attributes, first index +CHARACTER-HIGH-PAGES+ (an array of 16-bit
;;; values) with the high 13 bits of the character's codepoint. If the result
;;; value has its high bit set, the character is in a "compressed page". To
;;; find the misc entry number, simply clear the high bit. If the high bit is
;;; not set, the misc entry number must be looked up in
;;; +CHARACTER-LOW-PAGES+, which is an array of 16-bit values. Each entry in
;;; the array consists of two such values, the misc entry number and the
;;; decomposition index. To find the misc entry number, index into
;;; +CHARACTER-LOW-PAGES+ using the value retreived from
;;; +CHARACTER-HIGH-PAGES+ (shifted left 8 bits) plus the low 8 bits of the
;;; codepoint, all times two to account for the widtth of the entries. The
;;; value in +CHARACTER-LOW-PAGES+ at this point is the misc entry number. To
;;; transform a misc entry number into an index into
;;; +CHARACTER-MISC-DATABASE+, multiply it by +misc-width*. This gives the
;;; index of the start of the charater's misc entry in
;;; +CHARACTER-MISC-DATABASE+.
;;;
;;; To look up a character's decomposition, first retreive its
;;; decomposition-info from the misc database as described above. If the
;;; decomposition info is not 0, the character has a decomposition with a
;;; length given by the decomposition info with the high bit (which indicates
;;; compatibility/canonical status) cleared. To find the decomposition, move
;;; one value past the character's misc entry number in
;;; **CHARACTER-LOW-DATABASE**, which gives an index into
;;; **CHARACTER-DECOMPOSITIONS**. The next LENGTH values in
;;; **CHARACTER-DECOMPOSITIONS** (an array of codepoints), starting at this
;;; index, are the decomposition of the character. This proceduce does not
;;; apply to Hangul syllables, which have their own decomposition algorithm.
;;;
;;; Case information is stored in +CHARACTER-UNICODE-CASES+, an array that
;;; indirectly maps a character's codepoint to (cons uppercase
;;; lowercase). Uppercase and lowercase are either a single codepoint,
;;; which is the upper- or lower-case of the given character, or a
;;; list of codepoints which taken as a whole are the upper- or
;;; lower-case. These case lists are only used in Unicode case
;;; transformations, not in Common Lisp ones.
;;;
;;; +CHARACTER-CASES+ is similar to the above but it stores codes in
;;; a flat array twice as large, and it includes only the standard casing rules,
;;; so there's always just two characters.
;;;
;;; Similarly, composition information is stored in **CHARACTER-COMPOSITIONS**,
;;; which is a hash table of codepoints indexed by (+ (ash codepoint1 21)
;;; codepoint2).

(declaim (ftype (sfunction (t) (unsigned-byte 16)) misc-index))
(defun misc-index (char)
  (misc-index-from-char-code (char-code char)
                             sb-unicode::+character-high-pages+
                             sb-unicode::+character-low-pages+))

(declaim (ftype (sfunction (t) (unsigned-byte 8)) ucd-general-category)
         (inline ucd-general-category))
(defun ucd-general-category (char)
  (aref sb-unicode::+character-misc-database+ (misc-index char)))

(defun ucd-decimal-digit (char)
  (let ((digit (aref sb-unicode::+character-misc-database+
                     (+ 3 (misc-index char)))))
    (when (logbitp 6 digit) ; decimalp flag
      (ldb (byte 4 0) digit))))

(defun char-code (char)
  "Return the integer code of CHAR."
  (char-code char))

(defun char-int (char)
  "Return the integer code of CHAR. (In SBCL this is the same as CHAR-CODE, as
there are no character bits or fonts.)"
  (char-code char))

(defun code-char (code)
  "Return the character with the code CODE."
  (code-char code))

(defun character (object)
  "Coerce OBJECT into a CHARACTER if possible. Legal inputs are characters,
strings and symbols of length 1."
  (flet ((do-error (control args)
           (declare (optimize allow-non-returning-tail-call))
           (error 'simple-type-error
                  :datum object
                  ;;?? how to express "symbol with name of length 1"?
                  :expected-type '(or character (string 1))
                  :format-control control
                  :format-arguments args)))
    (typecase object
      (character object)
      (string (if (= 1 (length (the string object)))
                  (char object 0)
                  (do-error
                   "String is not of length one: ~S" (list object))))
      (symbol (if (= 1 (length (symbol-name object)))
                  (schar (symbol-name object) 0)
                  (do-error
                   "Symbol name is not of length one: ~S" (list object))))
      (t (do-error "~S cannot be coerced to a character." (list object))))))

(defun char-name (char)
  "Return the name (a STRING) for a CHARACTER object."
  (let ((char-code (char-code char)))
    (or (second (assoc char-code *base-char-name-alist*))
        (let ((h-code (double-vector-binary-search char-code
                                                   +unicode-char-name-database+)))
          (cond
            (h-code
             (huffman-decode h-code sb-unicode::+unicode-character-name-huffman-tree+))
            (t
             (format nil "U~X" char-code)))))))

(defun name-char (name)
  "Given an argument acceptable to STRING, NAME-CHAR returns a character whose
name is that string, if one exists. Otherwise, NIL is returned."
  (let ((char-code (car (rassoc-if (lambda (names)
                                     (member name names :test #'string-equal))
                                   *base-char-name-alist*))))
    (cond (char-code
           (code-char char-code))
          ((let ((start (cond ((eql (string-not-equal "U+" name) 2)
                               2)
                              ((eql (string-not-equal "U" name) 1)
                               1))))
             (and start
                  (loop for i from start
                        below (length name)
                        always (digit-char-p (char name i) 16))
                  (code-char (parse-integer name :start start :radix 16)))))
          (t
           (let ((encoding (huffman-encode (string-upcase name)
                                           sb-unicode::+unicode-character-name-huffman-tree+)))
             (when encoding
               (let ((char-code
                       (or
                        (double-vector-binary-search encoding
                                                     +unicode-name-char-database+)
                        (double-vector-binary-search encoding
                                                     +unicode-1-name-char-database+))))
                 (and char-code
                      (code-char char-code)))))))))

;;;; predicates

(defun standard-char-p (char)
  "The argument must be a character object. STANDARD-CHAR-P returns T if the
argument is a standard character -- one of the 95 ASCII printing characters or
<return>."
  (and (typep char 'base-char)
       (let ((n (char-code (the base-char char))))
         (or (< 31 n 127)
             (= n 10)))))

(defun %standard-char-p (thing)
  "Return T if and only if THING is a standard-char. Differs from
STANDARD-CHAR-P in that THING doesn't have to be a character."
  (and (characterp thing) (standard-char-p thing)))

(defun graphic-char-p (char)
  "The argument must be a character object. GRAPHIC-CHAR-P returns T if the
argument is a printing character (space through ~ in ASCII), otherwise returns
NIL."
  (let ((n (char-code char)))
    (or (< 31 n 127)
        (< 159 n))))

(defun alpha-char-p (char)
  "The argument must be a character object. ALPHA-CHAR-P returns T if the
argument is an alphabetic character, A-Z or a-z; otherwise NIL."
  (< (ucd-general-category char) 5))

(defmacro with-case-info ((char index-var cases-var
                           &key miss-value
                                (cases '+character-cases+))
                          &body body)
  (let ((code-var (gensym "CODE"))
        (shifted-var (gensym "SHIFTED"))
        (page-var (gensym "PAGE")))
    `(block nil
       (locally
           (declare (optimize (sb-c:insert-array-bounds-checks 0)))
         (let ((,code-var (char-code ,char)))
           (let* ((,shifted-var (ash ,code-var -6))
                  (,page-var (if (>= ,shifted-var (length +character-case-pages+))
                                 (return ,miss-value)
                                 (aref +character-case-pages+ ,shifted-var))))
             (if (= ,page-var 255)
                 ,miss-value
                 (let ((,index-var (* (+ (ash ,page-var 6)
                                         (ldb (byte 6 0) ,code-var))
                                      2))
                       (,cases-var ,cases))
                   ,@body))))))))

(defun both-case-p (char)
  "The argument must be a character object. BOTH-CASE-P returns T if the
argument is an alphabetic character and if the character exists in both upper
and lower case. For ASCII, this is the same as ALPHA-CHAR-P."
  (with-case-info (char index cases)
    (plusp (aref cases index))))

(defun upper-case-p (char)
  "The argument must be a character object; UPPER-CASE-P returns T if the
argument is an upper-case character, NIL otherwise."
  (with-case-info (char index cases)
    (= (aref cases (1+ index))
       (char-code char))))

(defun lower-case-p (char)
  "The argument must be a character object; LOWER-CASE-P returns T if the
argument is a lower-case character, NIL otherwise."
  (with-case-info (char index cases)
    (= (aref cases index)
       (char-code char))))

(defun char-upcase (char)
  "Return CHAR converted to upper-case if that is possible. Don't convert
lowercase eszet (U+DF)."
  (with-case-info (char index cases
                   :miss-value char)
    (let ((code (aref cases (1+ index))))
      (if (zerop code)
          char
          (code-char (truly-the char-code code))))))

(defun char-downcase (char)
  "Return CHAR converted to lower-case if that is possible."
  (with-case-info (char index cases
                   :miss-value char)
    (let ((code (aref cases index)))
      (if (zerop code)
          char
          (code-char (truly-the char-code code))))))

(defun alphanumericp (char)
  "Given a character-object argument, ALPHANUMERICP returns T if the argument
is either numeric or alphabetic."
  (let ((gc (ucd-general-category char)))
    (or (< gc 5)
        (= gc 13))))

;;; EQUAL-CHAR-CODE is used by the following functions as a version of CHAR-INT
;;;  which loses font, bits, and case info.

;;; Return a cons with (upper-case . lower-case), where it either can
;;; be a character code or a list of character codes if the character
;;; donwcases or upcases into multiple characters.
(declaim (inline char-case-info))
(defun char-case-info (character)
  (let* ((code (char-code character))
         (page (aref +character-case-pages+ (ash code -6))))
    ;; Pages with 255 means the character is not both-case.
    ;; +CHARACTER-CASES+ has 0 for those characters.
    (aref +character-unicode-cases+
          (+ (ash page 6)
             (ldb (byte 6 0) code)))))

;;; Returns the downcased code or the character code
(declaim (inline equal-char-code))
(defun equal-char-code (char)
  (let* ((code (char-code char))
         (shifted (ash code -6))
         (page (if (>= shifted (length +character-case-pages+))
                   (return-from equal-char-code code)
                   (aref +character-case-pages+ shifted))))
    (if (= page 255)
        code
        (let ((down-code
                (aref +character-cases+
                      (* (+ (ash page 6)
                            (ldb (byte 6 0) code))
                         2))))
          (if (zerop down-code)
              code
              down-code)))))

(declaim (inline two-arg-char-equal-inline))
(defun two-arg-char-equal-inline (c1 c2)
  (flet ((base-char-equal-p ()
           (let* ((code1 (char-code c1))
                  (code2 (char-code c2))
                  (sum (logxor code1 code2)))
             (when (eql sum #x20)
               (let ((sum (+ code1 code2)))
                 (or (and (< 161 sum 213))
                     (and (< 415 sum 461))
                     (and (< 463 sum 477))))))))
    (declare (inline base-char-equal-p))
    (cond ((eq c1 c2))
          #-sb-unicode
          (t
           (base-char-equal-p))
          #+sb-unicode
          ((base-char-p c1)
           (and (base-char-p c2)
                (base-char-equal-p)))
          #+sb-unicode
          ((base-char-p c2)
           nil)
          #+sb-unicode
          (t
           (with-case-info (c1 index cases)
             (or (= (aref cases index) (char-code c2)) ;; lower case
                 (= (aref cases (1+ index)) (char-code c2))))))))

;;; There are transforms on two-arg-char-equal, don't make it inlinable itself.
(defun two-arg-char-equal (c1 c2)
  (two-arg-char-equal-inline c1 c2))

(defun two-arg-char-not-equal (c1 c2)
  (not (two-arg-char-equal-inline c1 c2)))

(macrolet ((def (name test doc)
             `(defun ,name (character &rest more-characters)
                ,doc
                (if more-characters
                    (do ((c character (nth i more-characters))
                         (i 0 (1+ i)))
                        ((>= i (length more-characters)) t)
                      (do-rest-arg ((c2) more-characters i)
                        (when ,test
                          (return-from ,name nil))))
                    ;; CHAR-NOT-EQUAL has explicit check attribute
                    (progn (the character character) t)))))
  (def char/= (eq c (the character c2))
       "Return T if no two of the arguments are the same character.")
  (def char-not-equal (two-arg-char-equal c c2)
       "Return T if no two of the arguments are the same character.
Case is ignored."))

(defun two-arg-char-lessp (c1 c2)
  (< (equal-char-code c1) (equal-char-code c2)))

(defun two-arg-char-greaterp (c1 c2)
  (> (equal-char-code c1) (equal-char-code c2)))

(defun two-arg-char-not-greaterp (c1 c2)
  (<= (equal-char-code c1) (equal-char-code c2)))

(defun two-arg-char-not-lessp (c1 c2)
  (>= (equal-char-code c1) (equal-char-code c2)))

(macrolet ((def (op test doc &optional explicit-check)
             `(defun ,op (character &rest more-characters)
                ,doc
                ,@(when explicit-check `((declare (explicit-check))))
                (let ((c1 character))
                  (declare (character c1))
                  (do-rest-arg ((c2 i) more-characters 0 t)
                     (if ,test
                         (setq c1 c2)
                         (return (do-rest-arg ((c) more-characters (1+ i))
                                   (the character c))))))))) ; for effect
  ;; case-sensitive
  (def char= (eq c1 (the character c2))
    "Return T if all of the arguments are the same character.")
  (def char< (< (char-int c1) (char-int c2))
    "Return T if the arguments are in strictly increasing alphabetic order.")
  (def char> (> (char-int c1) (char-int c2))
    "Return T if the arguments are in strictly decreasing alphabetic order.")
  (def char<= (<= (char-int c1) (char-int c2))
    "Return T if the arguments are in strictly non-decreasing alphabetic order.")
  (def char>= (>= (char-int c1) (char-int c2))
    "Return T if the arguments are in strictly non-increasing alphabetic order.")

  ;; case-insensitive
  (def char-equal (two-arg-char-equal c1 c2)
    "Return T if all of the arguments are the same character.
Case is ignored." t)
  (def char-lessp (two-arg-char-lessp c1 c2)
    "Return T if the arguments are in strictly increasing alphabetic order.
Case is ignored." t)
  (def char-greaterp (two-arg-char-greaterp c1 c2)
    "Return T if the arguments are in strictly decreasing alphabetic order.
Case is ignored." t)
  (def char-not-greaterp (two-arg-char-not-greaterp c1 c2)
    "Return T if the arguments are in strictly non-decreasing alphabetic order.
Case is ignored." t)
  (def char-not-lessp (two-arg-char-not-lessp c1 c2)
    "Return T if the arguments are in strictly non-increasing alphabetic order.
Case is ignored." t))


(defun digit-char-p (char &optional (radix 10.))
  "If char is a digit in the specified radix, returns the fixnum for which
that digit stands, else returns NIL."
  (if (<= (char-code char) 127)
      (let ((weight (- (char-code char) 48)))
        (cond ((minusp weight) nil)
              ((<= radix 10.)
               ;; Special-case ASCII digits in decimal and smaller radices.
               (if (< weight radix) weight nil))
              ;; Digits 0 - 9 are used as is, since radix is larger.
              ((< weight 10) weight)
              ;; Check for upper case A - Z.
              ((and (>= (decf weight 7) 10) (< weight radix)) weight)
              ;; Also check lower case a - z.
              ((and (>= (decf weight 32) 10) (< weight radix)) weight)))
      (let ((number (ucd-decimal-digit char)))
        (when (and number (< (truly-the fixnum number) radix))
          number))))

(defun digit-char (weight &optional (radix 10))
  "All arguments must be integers. Returns a character object that represents
a digit of the given weight in the specified radix. Returns NIL if no such
character exists."
  (and (typep weight 'fixnum)
       (>= weight 0) (< weight radix) (< weight 36)
       (code-char (if (< weight 10) (+ 48 weight) (+ 55 weight)))))
