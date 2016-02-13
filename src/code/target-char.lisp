;;;; character functions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;; We compile some trivial character operations via inline expansion.
#!-sb-fluid
(declaim (inline standard-char-p graphic-char-p alpha-char-p
                 upper-case-p lower-case-p both-case-p alphanumericp))
(declaim (maybe-inline digit-char-p))

(deftype char-code ()
  `(integer 0 (,sb!xc:char-code-limit)))

(defglobal **unicode-character-name-huffman-tree** ())

(declaim (inline pack-3-codepoints))
(defun pack-3-codepoints (first &optional (second 0) (third 0))
  (declare (type (unsigned-byte 21) first second third))
  (sb!c::mask-signed-field 63 (logior first (ash second 21) (ash third 42))))

(define-load-time-global **character-misc-database** nil)
(declaim (type (simple-array (unsigned-byte 8) (*)) **character-misc-database**))
(declaim (inline both-case-index-p))
(defun both-case-index-p (misc-index)
  (declare (type (unsigned-byte 16) misc-index))
  (logbitp 7 (aref **character-misc-database** (+ 5 misc-index))))

(macrolet ((frob ()
             (flet ((coerce-it (array)
                      (!coerce-to-specialized array '(unsigned-byte 8)))
                    (file (name type)
                      (merge-pathnames (make-pathname
                                        :directory
                                        '(:relative :up :up "output")
                                        :name name :type type)
                                       sb!xc:*compile-file-truename*))
                    (read-ub8-vector (pathname)
                      (with-open-file (stream pathname
                                              :element-type '(unsigned-byte 8))
                        (let* ((length (file-length stream))
                               (array (make-array
                                       length :element-type '(unsigned-byte 8))))
                          (read-sequence array stream)
                          array)))
                    (init-global (name type)
                      `(progn
                         (defglobal ,name
                             ,(if (eql type 'hash-table)
                                  `(make-hash-table)
                                  `(make-array 0 :element-type ',type)))
                         (declaim (type ,(if (eql type 'hash-table)
                                             'hash-table
                                             `(simple-array ,type (*))) ,name)))))
               (let ((misc-database (coerce-it (read-ub8-vector (file "ucdmisc" "dat"))))
                     (ucd-high-pages (coerce-it (read-ub8-vector (file "ucdhigh" "dat"))))
                     (ucd-low-pages (coerce-it (read-ub8-vector (file "ucdlow" "dat"))))
                     (decompositions (coerce-it (read-ub8-vector (file "decomp" "dat"))))
                     (primary-compositions (coerce-it (read-ub8-vector (file "comp" "dat"))))
                     (case-data (coerce-it (read-ub8-vector (file "case" "dat"))))
                     (case-pages (coerce-it (read-ub8-vector (file "casepages" "dat"))))
                     (collations (coerce-it (read-ub8-vector (file "collation" "dat")))))
                 `(progn
                    ;; KLUDGE: All temporary values, fixed up in cold-load
                    ,(init-global '**character-misc-database** '(unsigned-byte 8))
                    ,(init-global '**character-high-pages** '(unsigned-byte 16))
                    ,(init-global '**character-low-pages** '(unsigned-byte 16))
                    ,(init-global '**character-decompositions** '(unsigned-byte 21))
                    ,(init-global '**character-case-pages** '(unsigned-byte 8))
                    ,(init-global '**character-primary-compositions** 'hash-table)
                    ,(init-global '**character-cases** '(unsigned-byte 32))
                    ,(init-global '**character-unicode-cases** t)
                    ,(init-global '**character-collations** 'hash-table)
                    ,(init-global '**unicode-char-name-database** t)
                    ,(init-global '**unicode-name-char-database** t)
                    ,(init-global '**unicode-1-char-name-database** t)
                    ,(init-global '**unicode-1-name-char-database** t)

                    (defun !character-database-cold-init ()
                      (flet ((make-ubn-vector (raw-bytes n)
                               (let ((new-array
                                       (make-array
                                        (/ (length raw-bytes) n)
                                        :element-type (list 'unsigned-byte (* 8 n)))))
                                 (loop for i from 0 below (length raw-bytes) by n
                                       for element = 0 do
                                       (loop for offset from 0 below n do
                                             (incf element
                                                   (ash (aref raw-bytes (+ i offset))
                                                        (* 8 (- n offset 1)))))
                                       (setf (aref new-array (/ i n)) element))
                                 new-array)))
                        (setf **character-misc-database**
                              ,misc-database
                              **character-high-pages**
                              (make-ubn-vector ,ucd-high-pages 2)
                              **character-low-pages**
                              (make-ubn-vector ,ucd-low-pages 2)
                              **character-case-pages**
                              ,(coerce-it case-pages)
                              **character-decompositions**
                              (make-ubn-vector ,decompositions 3))

                        (setf **character-primary-compositions**
                              (let ((table (make-hash-table))
                                    (info (make-ubn-vector ,primary-compositions 3)))
                                (dotimes (i (/ (length info) 3))
                                  (setf (gethash (dpb (aref info (* 3 i)) (byte 21 21)
                                                      (aref info (1+ (* 3 i))))
                                                 table)
                                        (aref info (+ (* 3 i) 2))))
                                table))

                        (let* ((unicode-table
                                 (make-array
                                  (* 64 (1+ (aref **character-case-pages**
                                                  (1- (length **character-case-pages**)))))))
                               (table (make-array
                                       (* 2 (length unicode-table))
                                       :element-type '(unsigned-byte 32)))
                               (info ,case-data)
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
                                  for page = (aref **character-case-pages** (ash key -6))
                                  for i = (+ (ash page 6) (ldb (byte 6 0) key))
                                  do
                                  (setf (aref unicode-table i) (cons upper lower))
                                  when (and (atom upper)
                                            (atom lower)
                                            ;; Some characters are only equal under unicode rules,
                                            ;; e.g. #\MICRO_SIGN and #\GREEK_CAPITAL_LETTER_MU
                                            #!+sb-unicode
                                            (both-case-index-p (misc-index (code-char lower)))
                                            #!+sb-unicode
                                            (both-case-index-p (misc-index (code-char upper))))
                                  do
                                  (setf (aref table (* i 2)) lower
                                        (aref table (1+ (* i 2))) upper)))
                          (setf **character-unicode-cases** unicode-table)
                          (setf **character-cases** table))

                        (setf **character-collations**
                              (let* ((table (make-hash-table))
                                     (index 0)
                                     (info (make-ubn-vector ,collations 4))
                                     (len (length info)))
                                (loop while (< index len) do
                                      (let* ((entry-head (aref info index))
                                             (cp-length (ldb (byte 4 28) entry-head))
                                             (key-length (ldb (byte 5 23) entry-head))
                                             (key (make-array
                                                   key-length
                                                   :element-type '(unsigned-byte 32)))
                                             (codepoints nil))
                                        (assert (and (/= cp-length 0) (/= key-length 0)))
                                        (loop repeat cp-length do
                                              (push (dpb 0 (byte 10 22) (aref info index))
                                                    codepoints)
                                              (incf index))
                                        (setf codepoints (nreverse codepoints))
                                        (dotimes (i key-length)
                                          (setf (aref key i) (aref info index))
                                          (incf index))
                                        (setf (gethash
                                               (apply #'pack-3-codepoints codepoints)
                                               table) key)))
                                table))))

                    ,(with-open-file
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
                                     (sort (copy-seq code->name) #'< :key #'cdr))
                               (setf code->name
                                     (sort (copy-seq name->code) #'< :key #'car))
                               (setf u1-name->code
                                     (sort (copy-seq code->u1-name) #'< :key #'cdr))
                               (setf code->u1-name
                                     (sort (copy-seq u1-name->code) #'< :key #'car))
                               (setf names nil u1-names nil)
                               `(defun !character-name-database-cold-init ()
                                  (setf **unicode-character-name-huffman-tree** ',tree
                                        **unicode-char-name-database**
                                        ',(convert-to-double-vector code->name)
                                        **unicode-name-char-database**
                                        ',(convert-to-double-vector name->code t)
                                        **unicode-1-char-name-database**
                                        ',(convert-to-double-vector code->u1-name)
                                        **unicode-1-name-char-database**
                                        ',(convert-to-double-vector u1-name->code t)))))))))))))

  (frob))
#+sb-xc-host (!character-name-database-cold-init)

(defparameter *base-char-name-alist*
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
;;; **CHARACTER-MISC-DATABASE** is an array of bytes that encode character
;;; attributes. Each entry in the misc database is +misc-width+ (currently 8)
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
;;; To find which entry in **CHARACTER-MISC-DATABASE** encodes a character's
;;; attributes, first index **CHARACTER-HIGH-PAGES** (an array of 16-bit
;;; values) with the high 13 bits of the character's codepoint. If the result
;;; value has its high bit set, the character is in a "compressed page". To
;;; find the misc entry number, simply clear the high bit. If the high bit is
;;; not set, the misc entry number must be looked up in
;;; **CHARACTER-LOW-PAGES**, which is an array of 16-bit values. Each entry in
;;; the array consists of two such values, the misc entry number and the
;;; decomposition index. To find the misc entry number, index into
;;; **CHARACTER-LOW-PAGES** using the value retreived from
;;; **CHARACTER-HIGH-PAGES** (shifted left 8 bits) plus the low 8 bits of the
;;; codepoint, all times two to account for the widtth of the entries. The
;;; value in **CHARACTER-LOW-PAGES** at this point is the misc entry number. To
;;; transform a misc entry number into an index into
;;; **CHARACTER-MISC-DATABASE**, multiply it by +misc-width*. This gives the
;;; index of the start of the charater's misc entry in
;;; **CHARACTER-MISC-DATABASE**.
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
;;; Case information is stored in **CHARACTER-UNICODE-CASES**, an array that
;;; indirectly maps a character's codepoint to (cons uppercase
;;; lowercase). Uppercase and lowercase are either a single codepoint,
;;; which is the upper- or lower-case of the given character, or a
;;; list of codepoints which taken as a whole are the upper- or
;;; lower-case. These case lists are only used in Unicode case
;;; transformations, not in Common Lisp ones.
;;;
;;; **CHARACTER-CASES** is similar to the above but it stores codes in
;;; a flat array twice as large, and it includes only the standard casing rules,
;;; so there's always just two characters.
;;;
;;; Similarly, composition information is stored in **CHARACTER-COMPOSITIONS**,
;;; which is a hash table of codepoints indexed by (+ (ash codepoint1 21)
;;; codepoint2).

(declaim (inline clear-flag))
(defun clear-flag (bit integer)
  (logandc2 integer (ash 1 bit)))

(defconstant +misc-width+ 9)

(declaim (ftype (sfunction (t) (unsigned-byte 16)) misc-index))
(defun misc-index (char)
  (let* ((cp (char-code char))
         (cp-high (ash cp -8))
         (high-index (aref **character-high-pages** cp-high)))
    (if (logbitp 15 high-index)
        (* +misc-width+ (clear-flag 15 high-index))
        (* +misc-width+
           (aref **character-low-pages**
                 (* 2 (+ (ldb (byte 8 0) cp) (ash high-index 8))))))))

(declaim (ftype (sfunction (t) (unsigned-byte 8)) ucd-general-category)
         (inline ucd-general-category))
(defun ucd-general-category (char)
  (aref **character-misc-database** (misc-index char)))

(defun ucd-decimal-digit (char)
  (let ((digit (aref **character-misc-database**
                     (+ 3 (misc-index char)))))
    (when (logbitp 6 digit) ; decimalp flag
      (ldb (byte 4 0) digit))))

(defun char-code (char)
  #!+sb-doc
  "Return the integer code of CHAR."
  (char-code char))

(defun char-int (char)
  #!+sb-doc
  "Return the integer code of CHAR. (In SBCL this is the same as CHAR-CODE, as
there are no character bits or fonts.)"
  (char-code char))

(defun code-char (code)
  #!+sb-doc
  "Return the character with the code CODE."
  (code-char code))

(defun character (object)
  #!+sb-doc
  "Coerce OBJECT into a CHARACTER if possible. Legal inputs are characters,
strings and symbols of length 1."
  (flet ((do-error (control args)
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
  #!+sb-doc
  "Return the name (a STRING) for a CHARACTER object."
  (let ((char-code (char-code char)))
    (or (second (assoc char-code *base-char-name-alist*))
        (let ((h-code (double-vector-binary-search char-code
                                                   **unicode-char-name-database**)))
          (cond
            (h-code
             (huffman-decode h-code **unicode-character-name-huffman-tree**))
            (t
             (format nil "U~X" char-code)))))))

(defun name-char (name)
  #!+sb-doc
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
                                           **unicode-character-name-huffman-tree**)))
             (when encoding
               (let ((char-code
                       (or
                        (double-vector-binary-search encoding
                                                     **unicode-name-char-database**)
                        (double-vector-binary-search encoding
                                                     **unicode-1-name-char-database**))))
                 (and char-code
                      (code-char char-code)))))))))

;;;; predicates

(defun standard-char-p (char)
  #!+sb-doc
  "The argument must be a character object. STANDARD-CHAR-P returns T if the
argument is a standard character -- one of the 95 ASCII printing characters or
<return>."
  (and (typep char 'base-char)
       (let ((n (char-code (the base-char char))))
         (or (< 31 n 127)
             (= n 10)))))

(defun %standard-char-p (thing)
  #!+sb-doc
  "Return T if and only if THING is a standard-char. Differs from
STANDARD-CHAR-P in that THING doesn't have to be a character."
  (and (characterp thing) (standard-char-p thing)))

(defun graphic-char-p (char)
  #!+sb-doc
  "The argument must be a character object. GRAPHIC-CHAR-P returns T if the
argument is a printing character (space through ~ in ASCII), otherwise returns
NIL."
  (let ((n (char-code char)))
    (or (< 31 n 127)
        (< 159 n))))

(defun alpha-char-p (char)
  #!+sb-doc
  "The argument must be a character object. ALPHA-CHAR-P returns T if the
argument is an alphabetic character, A-Z or a-z; otherwise NIL."
  (< (ucd-general-category char) 5))

(defun both-case-p (char)
  #!+sb-doc
  "The argument must be a character object. BOTH-CASE-P returns T if the
argument is an alphabetic character and if the character exists in both upper
and lower case. For ASCII, this is the same as ALPHA-CHAR-P."
  (both-case-index-p (misc-index char)))

(defun upper-case-p (char)
  #!+sb-doc
  "The argument must be a character object; UPPER-CASE-P returns T if the
argument is an upper-case character, NIL otherwise."
  (let ((index (misc-index char)))
    (and
     (both-case-index-p index)
     (= (aref **character-misc-database** index) 0))))

(defun lower-case-p (char)
  #!+sb-doc
  "The argument must be a character object; LOWER-CASE-P returns T if the
argument is a lower-case character, NIL otherwise."
  (let ((index (misc-index char)))
    (and
     (both-case-index-p index)
     (= (aref **character-misc-database** index) 1))))

(defun digit-char-p (char &optional (radix 10.))
  #!+sb-doc
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

(defun alphanumericp (char)
  #!+sb-doc
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
         (page (aref **character-case-pages** (ash code -6))))
    ;; Pages with 255 means the character is not both-case.
    ;; **character-cases** has 0 for those characters.
    (aref **character-unicode-cases**
          (+ (ash page 6)
             (ldb (byte 6 0) code)))))

;;; Returns the downcased code or the character code
(declaim (inline equal-char-code))
(defun equal-char-code (char)
  (let* ((code (char-code char))
         (shifted (ash code -6))
         (page (if (>= shifted (length **character-case-pages**))
                   (return-from equal-char-code code)
                   (aref **character-case-pages** shifted))))
    (if (= page 255)
        code
        (let ((down-code
                (aref **character-cases**
                      (* (+ (ash page 6)
                            (ldb (byte 6 0) code))
                         2))))
          (if (zerop down-code)
              code
              down-code)))))

(defun two-arg-char-equal (c1 c2)
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
    (or (eq c1 c2)
        #!-sb-unicode
        (base-char-equal-p)
        #!+sb-unicode
        (typecase c1
          (base-char
           (and (base-char-p c2)
                (base-char-equal-p)))
          (t
           (= (equal-char-code c1) (equal-char-code c2)))))))

(defun char-equal-constant (x char reverse-case-char)
  (declare (type character x) (explicit-check))
  (or (eq char x)
      (eq reverse-case-char x)))

(defun two-arg-char-not-equal (c1 c2)
  (/= (equal-char-code c1) (equal-char-code c2)))

(macrolet ((def (name test doc)
             (declare (ignorable doc))
             `(defun ,name (character &rest more-characters)
                #!+sb-doc ,doc
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
             (declare (ignorable doc))
             `(defun ,op (character &rest more-characters)
                #!+sb-doc ,doc
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


;;;; miscellaneous functions

(defun char-upcase (char)
  #!+sb-doc
  "Return CHAR converted to upper-case if that is possible. Don't convert
lowercase eszet (U+DF)."
  (let* ((code (char-code char))
         (shifted (ash code -6))
         (page (if (>= shifted (length **character-case-pages**))
                   (return-from char-upcase char)
                   (aref **character-case-pages** shifted))))
    (if (= page 255)
        char
        (let ((code
                (aref **character-cases**
                      (1+ (* (+ (ash page 6)
                                (ldb (byte 6 0) code))
                             2)))))
          (if (zerop code)
              char
              (code-char code))))))

(defun char-downcase (char)
  #!+sb-doc
  "Return CHAR converted to lower-case if that is possible."
  (let* ((code (char-code char))
         (shifted (ash code -6))
         (page (if (< shifted (length **character-case-pages**))
                   (aref **character-case-pages** shifted)
                   (return-from char-downcase char))))
    (if (= page 255)
        char
        (let ((code
                (aref **character-cases**
                      (* 2 (+ (ash page 6)
                              (ldb (byte 6 0) code))))))
          (if (zerop code)
              char
              (code-char code))))))

(defun digit-char (weight &optional (radix 10))
  #!+sb-doc
  "All arguments must be integers. Returns a character object that represents
a digit of the given weight in the specified radix. Returns NIL if no such
character exists."
  (and (typep weight 'fixnum)
       (>= weight 0) (< weight radix) (< weight 36)
       (code-char (if (< weight 10) (+ 48 weight) (+ 55 weight)))))

;;; Moved from 'string' because ALPHANUMERICP wants to be inlined,
;;; and moving ALPHANUMERICP earlier causes a snowball effect of
;;; other inlining failures.
(flet ((%capitalize (string start end)
         (declare (string string) (index start) (type sequence-end end))
         (let ((saved-header string))
           (with-one-string (string start end)
             (do ((index start (1+ index))
                  (new-word? t)
                  (char nil))
                 ((= index (the fixnum end)))
               (declare (fixnum index))
               (setq char (schar string index))
               (cond ((not (alphanumericp char))
                      (setq new-word? t))
                     (new-word?
                      ;; CHAR is the first case-modifiable character after
                      ;; a sequence of non-case-modifiable characters.
                      (setf (schar string index) (char-upcase char))
                      (setq new-word? nil))
                     (t
                      (setf (schar string index) (char-downcase char))))))
           saved-header)))
(defun string-capitalize (string &key (start 0) end)
  (%capitalize (copy-seq (string string)) start end))
(defun nstring-capitalize (string &key (start 0) end)
  (%capitalize string start end))
) ; FLET
