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

(declaim (inline clear-flag))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun clear-flag (bit integer)
    (logandc2 integer (ash 1 bit))))

(eval-when (:compile-toplevel)
  (defconstant +misc-width+ 9)
  (defmacro misc-index-from-char-code (codepoint high-pages low-pages)
    (let ((high-pages-value (and (boundp high-pages)
                                 (symbol-value high-pages)))
          (low-pages-value (and (boundp low-pages)
                                (symbol-value low-pages))))
      `(let* ((cp ,codepoint)
              (cp-high (ash cp -8))
              (high-index (aref ,high-pages cp-high)))
         (* ,+misc-width+
            (if (logbitp 15 high-index)
                (truly-the
                 (integer 0
                          ,(if high-pages-value
                               (loop for x across high-pages-value
                                     when (logbitp 15 x)
                                     maximize (clear-flag 15 x))
                               '*))
                 (clear-flag 15 high-index))
                (truly-the
                 (integer 0
                          ,(if low-pages-value
                               (loop for i below (length low-pages-value) by 2
                                     maximize
                                     (aref low-pages-value i))
                               '*))
                 (aref ,low-pages
                       (* 2 (+ (ldb (byte 8 0) cp)
                               (ash (truly-the
                                     (integer 0
                                              ,(if high-pages-value
                                                   (loop for x across high-pages-value
                                                         unless (logbitp 15 x)
                                                         maximize x)
                                                   '*))
                                     high-index)
                                    8))))))))))
  (setf (sb-xc:macro-function 'misc-index-from-char-code)
        (lambda (form env)
          (declare (ignore env))
          (funcall (cl:macro-function 'misc-index-from-char-code) form nil))))

(macrolet ((frob ()
             (flet ((file (name type)
                      (sb-cold:find-bootstrap-file (format nil "output/ucd/~A.~A" name type)))
                    (make-ubn-vector (raw-bytes n)
                      (aver (member n '(1 2)))
                      (ubN-array-from-octets raw-bytes `(unsigned-byte ,(* 8 n)) n)))
              (let* ((misc-database (read-ub8-vector (file "ucdmisc" "dat")))
                     (ucd-high-pages (read-ub8-vector (file "ucdhigh" "dat")))
                     (ucd-low-pages (read-ub8-vector (file "ucdlow" "dat")))
                     (case-data (read-ub8-vector (file "case" "dat")))
                     (case-pages (read-ub8-vector (file "casepages" "dat")))
                     (high-pages (make-ubn-vector ucd-high-pages 2))
                     (low-pages (make-ubn-vector ucd-low-pages 2))
                     (%*character-case-pages*% (make-ubn-vector case-pages 1)))

                 `(progn
                    (defconstant-eqx sb-unicode::+character-misc-database+ ,misc-database #'equalp)
                    (defconstant-eqx sb-unicode::+character-high-pages+ ,high-pages #'equalp)
                    (defconstant-eqx sb-unicode::+character-low-pages+ ,low-pages #'equalp)
                    (defconstant-eqx +character-case-pages+ ,%*character-case-pages*% #'equalp)
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
                                              (nreverse ret)))))
                                   #+sb-unicode
                                   (both-case-p-local (code)
                                     (logbitp 7 (aref misc-database
                                                      (+ 5 (misc-index-from-char-code
                                                            code high-pages low-pages))))))
                            (loop
                              until (>= index length)
                              do (let* ((key (read-codepoint))
                                        (upper (read-length-tagged))
                                        (lower (read-length-tagged))
                                        (page (aref %*character-case-pages*% (ash key -6)))
                                        (i (+ (ash page 6) (ldb (byte 6 0) key))))
                                   (setf (aref unicode-table i)
                                         (if (or (consp upper)
                                                 (consp lower))
                                             (cons upper lower)
                                             (dpb upper (byte 21 21) lower)))
                                   (when (and (atom upper)
                                              (atom lower)
                                              ;; Some characters are only equal under unicode rules,
                                              ;; e.g. #\MICRO_SIGN and #\GREEK_CAPITAL_LETTER_MU
                                              #+sb-unicode
                                              (both-case-p-local lower)
                                              #+sb-unicode
                                              (both-case-p-local upper))
                                     (setf (aref table (* i 2)) lower
                                           (aref table (1+ (* i 2))) upper)))))
                          `((defconstant-eqx +character-unicode-cases+ ,unicode-table #'equalp)
                            (defconstant-eqx +character-cases+ ,table #'equalp))))))))
  (frob))

;;;; UCD accessor functions

;;; The character database is made of several arrays.
;;; +CHARACTER-MISC-DATABASE+ is an array of bytes that encode character
;;; attributes. Each entry in the misc database is +MISC-WIDTH+ (currently 9)
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
;;; 6 is set if the entry's characters have a defined case transformation in
;;; Unicode. Bit 5 is set if the characters have the property BIDI_Mirrored=Y.
;;; Bits 3-0 encode the entry's East Asian Width. Bit 4 is unused. Age stores
;;; the minor version in bits 0-2, and the major version in the remaining 5
;;; bits.
;;;
;;; To find which entry in +CHARACTER-MISC-DATABASE+ encodes a
;;; character's attributes, first index +CHARACTER-HIGH-PAGES+ (an
;;; array of 16-bit values) with the high 13 bits of the character's
;;; codepoint. If the result value has its high bit set, the character
;;; is in a "compressed page", where all characters on that
;;; 256-character page have the same misc entry. To find the misc
;;; entry number, simply clear the high bit. If the high bit is not
;;; set, the misc entry number must be looked up in
;;; +CHARACTER-LOW-PAGES+, which is an array of 16-bit values. Each
;;; entry in the array consists of two such values, the misc entry
;;; number and the decomposition index. To find the misc entry number,
;;; index into +CHARACTER-LOW-PAGES+ using the value retreived from
;;; +CHARACTER-HIGH-PAGES+ (shifted left 8 bits) plus the low 8 bits
;;; of the codepoint, all times two to account for the widtth of the
;;; entries. The value in +CHARACTER-LOW-PAGES+ at this point is the
;;; misc entry number. To transform a misc entry number into an index
;;; into +CHARACTER-MISC-DATABASE+, multiply it by +MISC-WIDTH+. This
;;; gives the index of the start of the charater's misc entry in
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
;;; Primary composition information is stored in a hash table local to
;;; PRIMARY-COMPOSITION, with (+ (ash codepoint1 21) codepoint2) as
;;; keys and the composition as the value

(defun misc-index (char)
  (misc-index-from-char-code (char-code char)
                             sb-unicode::+character-high-pages+
                             sb-unicode::+character-low-pages+))

(aver (csubtypep (global-ftype 'misc-index)
                 (specifier-type '(sfunction (t) (unsigned-byte 16)))))
(proclaim `(ftype ,(type-specifier (global-ftype 'misc-index)) misc-index))

(declaim (ftype (sfunction (t) (unsigned-byte 8)) ucd-general-category)
         (inline ucd-general-category))
(defun ucd-general-category (char)
  (aref sb-unicode::+character-misc-database+ (misc-index char)))

(defun ucd-decimal-digit (char)
  (let ((digit (aref sb-unicode::+character-misc-database+
                     (+ 3 (misc-index char)))))
    (when (logbitp 6 digit) ; decimalp flag
      (ldb (byte 4 0) digit))))
(proclaim `(ftype ,(type-specifier (global-ftype 'ucd-decimal-digit)) ucd-decimal-digit))

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

;;;; predicates

(defun standard-char-p (char)
  "The argument must be a character object. STANDARD-CHAR-P returns T if the
argument is a standard character -- one of the 95 ASCII printing characters or
<return>."
  (let ((n (char-code char)))
    (or (< 31 n 127)
        (= n 10))))

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
                                (cases +character-cases+))
                          &body body)
  (let ((code-var (gensym "CODE"))
        (shifted-var (gensym "SHIFTED"))
        (page-var (gensym "PAGE")))
    `(block nil
       (locally
           (declare (optimize (sb-c:insert-array-bounds-checks 0)))
         (let ((,code-var (char-code ,char)))
           (let* ((,shifted-var (ash ,code-var -6))
                  (,page-var (if (>= ,shifted-var ,(length +character-case-pages+))
                                 (return ,miss-value)
                                 (aref ,+character-case-pages+ ,shifted-var))))
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
          (code-char code)))))

(defun char-downcase (char)
  "Return CHAR converted to lower-case if that is possible."
  (with-case-info (char index cases
                   :miss-value char)
    (let ((code (aref cases index)))
      (if (zerop code)
          char
          (code-char code)))))

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
                   (aref #.+character-case-pages+ shifted))))
    (if (= page 255)
        code
        (let* ((page (truly-the (integer 0
                                         #.(loop for x across +character-case-pages+
                                                 unless (= x 255)
                                                 maximize x))
                                page))
               (down-code
                 (aref #.+character-cases+
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
  (let ((code (char-code char)))
    (if (<= code 1632) ;; (loop for code from 127 when (digit-char-p (code-char code)) return code)
        (let ((weight (- code 48)))
          (cond ((minusp weight) nil)
                ((<= radix 10.)
                 ;; Special-case ASCII digits in decimal and smaller radices.
                 (if (< weight radix) weight nil))
                ;; Digits 0 - 9 are used as is, since radix is larger.
                ((< weight 10) weight)
                (t
                 (let ((weight (logior #x20 code))) ;; downcase ASCII characters.
                   (when (and (>= (decf weight (- (char-code #\a) 10)) 10)
                              (< weight radix))
                     weight) ))))
        (let ((number (ucd-decimal-digit char)))
          (when (and number (< number radix))
            number)))))

(defun digit-char (weight &optional (radix 10))
  "All arguments must be integers. Returns a character object that represents
a digit of the given weight in the specified radix. Returns NIL if no such
character exists."
  (declare (explicit-check weight))
  (cond ((typep weight '(and unsigned-byte fixnum))
         (and (< weight radix)
              (code-char (if (< weight 10) (+ 48 weight) (+ 55 weight)))))
        (t
         (the unsigned-byte weight)
         nil)))
