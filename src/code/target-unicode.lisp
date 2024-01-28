;;;; Unicode functions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-UNICODE")

(eval-when (:compile-toplevel)
  (defun plist-to-alist (list)
    (loop for (key value) on list by #'cddr collect (cons key value))))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *proplist-properties*
    (mapcar (lambda (x) (cons (car x) (sb-xc:coerce (cdr x) '(vector (unsigned-byte 32)))))
            '#.(plist-to-alist (sb-cold:read-from-file "output/ucd/misc-properties.lisp-expr")))))

;;; Unicode property access
(defun ordered-ranges-member (item vector)
  (declare (type (simple-array (unsigned-byte 32) 1) vector)
           (%char-code item)
           (optimize speed))
  (labels ((recurse (start end)
             (declare (type index start end)
                      (optimize (safety 0)))
             (when (< start end)
               (let* ((i (+ start (truncate (the index (- end start)) 2)))
                      (index (* 2 i))
                      (elt1 (aref vector index))
                      (elt2 (aref vector (1+ index))))
                 (declare (type index i)
                          (fixnum elt1 elt2))
                 (cond ((< item elt1)
                        (recurse start i))
                       ((> item elt2)
                        (recurse (+ 1 i) end))
                       (t
                        item))))))
    (recurse 0 (truncate (length vector) 2))))

;; Returns which range `item` was found in or NIL
;; First range = 0, second range = 1 ...
(defun ordered-ranges-position (item vector)
  (declare (type (simple-array (unsigned-byte 32) (*)) vector)
           (type fixnum item))
  (labels ((recurse (start end)
             (declare (type index start end))
             (when (< start end)
               (let* ((i (+ start (truncate (- end start) 2)))
                      (index (* 2 i))
                      (elt1 (aref vector index))
                      (elt2 (aref vector (1+ index))))
                 (declare (type index i))
                 (cond ((< item elt1)
                        (recurse start i))
                       ((> item elt2)
                        (recurse (+ 1 i) end))
                       (t
                        i))))))
    (recurse 0 (truncate (length vector) 2))))

(defun proplist-p (character property)
  "Returns T if CHARACTER has the specified PROPERTY.
PROPERTY is a keyword representing one of the properties from PropList.txt,
with underscores replaced by dashes."
  (macrolet ((get-ub32-vector ()
               ;; This ECASE gets optimized into parallel arrays,
               ;; and does not depend on jump tables
               `(ecase property ,@(mapcar (lambda (x) (list (car x) (cdr x)))
                                          *proplist-properties*))))
    (ordered-ranges-member (char-code character) (get-ub32-vector))))

(define-compiler-macro proplist-p (&whole form character property)
  (if (keywordp property)
      `(ordered-ranges-member (char-code ,character)
                              ,(or (cdr (assoc property *proplist-properties*))
                                   (error "No such property ~S" property)))
      form))

(eval-when (:compile-toplevel)
  (defvar *slurped-random-constants*
    (sb-cold:read-from-file "tools-for-build/more-ucd-consts.lisp-expr"))
  (defun read-ucd-constant (symbol)
    (map 'vector
         (lambda (x) (keywordicate (substitute #\- #\_ (string-upcase x))))
         (or (cadr (assoc symbol *slurped-random-constants*))
             (error "Missing entry in more-ucd-consts for ~S" symbol)))))

(declaim (inline svref-or-null))
(defun svref-or-null (vector index)
  (and (< index (length vector))
       (svref vector index)))

(defun general-category (character)
  "Returns the general category of CHARACTER as it appears in UnicodeData.txt"
  (svref-or-null #.(read-ucd-constant '*general-categories*)
                 (sb-impl::ucd-general-category character)))

(defun bidi-class (character)
  "Returns the bidirectional class of CHARACTER"
  (if (and (eql (general-category character) :Cn)
           (default-ignorable-p character))
      :Bn
      (svref-or-null
       #.(read-ucd-constant '*bidi-classes*)
       (aref +character-misc-database+ (1+ (misc-index character))))))

(declaim (inline combining-class))
(defun combining-class (character)
  "Returns the canonical combining class (CCC) of CHARACTER"
  (aref +character-misc-database+ (+ 2 (misc-index character))))

(defun decimal-value (character)
  "Returns the decimal digit value associated with CHARACTER or NIL if
there is no such value.

The only characters in Unicode with a decimal digit value are those
that are part of a range of characters that encode the digits 0-9.
Because of this, `(decimal-digit c) <=> (digit-char-p c 10)` in
#+sb-unicode builds"
  (sb-impl::ucd-decimal-digit character))

(defun digit-value (character)
  "Returns the Unicode digit value of CHARACTER or NIL if it doesn't exist.

Digit values are guaranteed to be integers between 0 and 9 inclusive.
All characters with decimal digit values have the same digit value,
but there are characters (such as digits of number systems without a 0 value)
that have a digit value but no decimal digit value"
  (let ((%digit (clear-flag 6
                            (aref +character-misc-database+
                                  (+ 3 (misc-index character))))))
    (if (< %digit 10) %digit nil)))

(defun mirrored-p (character)
  "Returns T if CHARACTER needs to be mirrored in bidirectional text.
Otherwise, returns NIL."
  (logbitp 5 (aref +character-misc-database+
                    (+ 5 (misc-index character)))))

(defun east-asian-width (character)
  "Returns the East Asian Width property of CHARACTER as
one of the keywords :N (Narrow), :A (Ambiguous), :H (Halfwidth),
:W (Wide), :F (Fullwidth), or :NA (Not applicable)"
  (svref-or-null #.(read-ucd-constant '*east-asian-widths*)
                 (ldb (byte 3 0)
                      (aref +character-misc-database+
                            (+ 5 (misc-index character))))))

(defun script (character)
  "Returns the Script property of CHARACTER as a keyword.
If CHARACTER does not have a known script, returns :UNKNOWN"
  (svref-or-null #.(read-ucd-constant '*scripts*)
                 (aref +character-misc-database+ (+ 6 (misc-index character)))))

(defun char-block (character)
  "Returns the Unicode block in which CHARACTER resides as a keyword.
If CHARACTER does not have a known block, returns :NO-BLOCK"
  (let* ((code (char-code character))
         (block-index (ordered-ranges-position
                       code
                       #.(coerce (sb-cold:read-from-file "output/ucd/block-ranges.lisp-expr")
                                       '(vector (unsigned-byte 32))))))
    (if block-index
        (aref #.(sb-cold:read-from-file "output/ucd/block-names.lisp-expr") block-index)
        :no-block)))

(defun unicode-1-name (character)
  "Returns the name assigned to CHARACTER in Unicode 1.0 if it is distinct
from the name currently assigned to CHARACTER. Otherwise, returns NIL.
This property has been officially obsoleted by the Unicode standard, and
is only included for backwards compatibility."
  (let* ((char-code (char-code character))
         (h-code (double-vector-binary-search char-code
                                              +unicode-1-char-name-database+)))
    (when h-code
      (huffman-decode h-code +unicode-character-name-huffman-tree+))))

(defun age (character)
  "Returns the version of Unicode in which CHARACTER was assigned as a pair
of values, both integers, representing the major and minor version respectively.
If CHARACTER is not assigned in Unicode, returns NIL for both values."
  (let* ((value (aref +character-misc-database+ (+ 8 (misc-index character))))
         (major (ash value -3))
         (minor (ldb (byte 3 0) value)))
    (if (zerop value) (values nil nil) (values major minor))))

(defun hangul-syllable-type (character)
  "Returns the Hangul syllable type of CHARACTER.
The syllable type can be one of :L, :V, :T, :LV, or :LVT.
If the character is not a Hangul syllable or Jamo, returns NIL"
  (let ((cp (char-code character)))
    (cond
      ((or
        (and (<= #x1100 cp) (<= cp #x115f))
        (and (<= #xa960 cp) (<= cp #xa97c))) :L)
      ((or
        (and (<= #x1160 cp) (<= cp #x11a7))
        (and (<= #xd7B0 cp) (<= cp #xd7C6))) :V)
      ((or
        (and (<= #x11a8 cp) (<= cp #x11ff))
        (and (<= #xd7c8 cp) (<= cp #xd7fb))) :T)
      ((and (<= #xac00 cp) (<= cp #xd7a3))
       (if (= 0 (rem (- cp #xac00) 28)) :LV :LVT)))))

(defun line-break-class (character &key resolve)
  "Returns the line breaking class of CHARACTER, as specified in UAX #14.
If :RESOLVE is NIL, returns the character class found in the property file.
If :RESOLVE is non-NIL, certain line-breaking classes will be mapped to other
classes as specified in the applicable standards. Additionally, if :RESOLVE
is :EAST-ASIAN, Ambigious (class :AI) characters will be mapped to the
Ideographic (:ID) class instead of Alphabetic (:AL)."
  (when (and resolve (listp character)) (setf character (car character)))
  (when (and resolve (not character)) (return-from line-break-class :nil))
  (let ((raw-class
         (svref-or-null #.(read-ucd-constant '*line-break-classes*)
                        (aref +character-misc-database+ (+ 7 (misc-index character)))))
        (syllable-type (hangul-syllable-type character)))
    (when syllable-type
      (setf raw-class
            (cdr (assoc syllable-type
                        '((:l . :JL) (:v . :JV) (:t . :JT)
                          (:lv . :H2) (:lvt . :H3))))))
    (when resolve
      (setf raw-class
            (case raw-class
              (:ai (if (eql resolve :east-asian) :ID :AL))
              ; If we see :CM when resolving, we have a CM that isn't subject
              ; to LB9, so we do LB10 (:ZWJ handled in LINE-BREAK-ANNOTATE)
              ((:xx :cm) :al)
              (:sa (if (member (general-category character) '(:Mn :Mc))
                       :CM :AL))
              (:cj :ns)
              (:sg (error "The character ~S is a surrogate, which should not
appear in an SBCL string. The line-breaking behavior of surrogates is undefined."
                          character))
              (t raw-class))))
    raw-class))

(defun uppercase-p (character)
  "Returns T if CHARACTER has the Unicode property Uppercase and NIL otherwise"
  (or (eql (general-category character) :Lu) (proplist-p character :other-uppercase)))

(defun lowercase-p (character)
  "Returns T if CHARACTER has the Unicode property Lowercase and NIL otherwise"
  (or (eql (general-category character) :Ll) (proplist-p character :other-lowercase)))

(defun cased-p (character)
  "Returns T if CHARACTER has a (Unicode) case, and NIL otherwise"
  (or (uppercase-p character) (lowercase-p character)
      (eql (general-category character) :Lt)))

(defun case-ignorable-p (character)
  "Returns T if CHARACTER is Case Ignorable as defined in Unicode 6.3, Chapter
3"
  (or (member (general-category character)
              '(:Mn :Me :Cf :Lm :Sk))
      (member (word-break-class character)
              '(:midletter :midnumlet :single-quote))))

(defun alphabetic-p (character)
  "Returns T if CHARACTER is Alphabetic according to the Unicode standard
and NIL otherwise"
  (or (member (general-category character) '(:Lu :Ll :Lt :Lm :Lo :Nl))
      (proplist-p character :other-alphabetic)))

(defun ideographic-p (character)
  "Returns T if CHARACTER has the Unicode property Ideographic,
which loosely corresponds to the set of \"Chinese characters\""
  (proplist-p character :ideographic))

(defun math-p (character)
  "Returns T if CHARACTER is a mathematical symbol according to Unicode and
NIL otherwise"
  (or (eql (general-category character) :sm) (proplist-p character :other-math)))

(defun whitespace-p (character)
  "Returns T if CHARACTER is whitespace according to Unicode
and NIL otherwise"
  (proplist-p character :white-space))

(defun hex-digit-p (character &key ascii)
  "Returns T if CHARACTER is a hexadecimal digit and NIL otherwise.
If :ASCII is non-NIL, fullwidth equivalents of the Latin letters A through F
are excluded."
  (proplist-p character (if ascii :ascii-hex-digit :hex-digit)))

(defun soft-dotted-p (character)
  "Returns T if CHARACTER has a soft dot (such as the dots on i and j) which
disappears when accents are placed on top of it. and NIL otherwise"
  (proplist-p character :soft-dotted))

(eval-when (:compile-toplevel)
  (sb-xc:defmacro coerce-to-ordered-ranges (array)
    (coerce array '(vector (unsigned-byte 32)))))

(defun default-ignorable-p (character)
  "Returns T if CHARACTER is a Default_Ignorable_Code_Point"
  (and
   (or (proplist-p character :other-default-ignorable-code-point)
       (eql (general-category character) :cf)
       (proplist-p character :variation-selector))
   (not
    (or (whitespace-p character)
        (ordered-ranges-member
         (char-code character)
         (coerce-to-ordered-ranges
          #(#x0600 #x0604 #x06DD #x06DD #x070F #x070F #xFFF9 #xFFFB
            #x110BD #x110BD)))))))


;;; Implements UAX#15: Normalization Forms
(declaim (inline char-decomposition-info))
(defun char-decomposition-info (char)
  (let ((value (aref +character-misc-database+
                     (+ 4 (misc-index char)))))
    (values (clear-flag 7 value) (logbitp 7 value))))

(defun char-decomposition (char length callback)
  (declare (function callback))
  ;; Caller should have gotten length from char-decomposition-info
  (let* ((cp (char-code char))
         (cp-high (ash cp -8))
         (decompositions +character-decompositions+)
         (high-page (aref +character-high-pages+ cp-high))
         (index (unless (logbitp 15 high-page) ;; Hangul syllable
                  (aref +character-low-pages+
                        (+ 1 (* 2 (+ (ldb (byte 8 0) cp) (ash high-page 8))))))))
    (cond ((= length 1)
           (funcall callback (code-char (aref decompositions index))))
          ((<= #xac00 cp #xd7a3)
           ;; see Unicode 6.2, section 3-12
           (let* ((sbase #xac00)
                  (lbase #x1100)
                  (vbase #x1161)
                  (tbase #x11a7)
                  (vcount 21)
                  (tcount 28)
                  (ncount (* vcount tcount))
                  (sindex (- cp sbase))
                  (lindex (floor sindex ncount))
                  (vindex (floor (mod sindex ncount) tcount))
                  (tindex (mod sindex tcount)))
             (funcall callback (code-char (+ lbase lindex)))
             (funcall callback (code-char (+ vbase vindex)))
             (when (> tindex 0)
               (funcall callback  (code-char (+ tbase tindex))))))

          (t
           (loop for i below length
                 do
                 (funcall callback (code-char (aref decompositions (+ index i)))))))))

(defun decompose-char (char compatibility callback)
  (declare (function callback))
  (multiple-value-bind (info compat) (char-decomposition-info char)
    (if (and (plusp info)
             (or compatibility
                 (not compat)))
        (if compatibility
            (dx-flet ((callback (char)
                        (decompose-char char t callback)))
              (char-decomposition char info #'callback))
            (char-decomposition char info callback))
        (funcall callback char))))

(defun decompose-string (string compatibility filter)
  (let (chars
        (length 0)
        (previous-combining-class 0))
    (declare (type index length))
    (dx-flet ((callback (char)
                        (let ((combining-class (combining-class char)))
                          (incf length)
                          (cond ((< 0 combining-class previous-combining-class)
                                 ;; Ensure it's sorted
                                 (loop for cons on chars
                                       for next-char = (cadr cons)
                                       when (or (not next-char)
                                                (<= 0 (combining-class next-char) combining-class))
                                       do (setf (cdr cons)
                                                (cons char (cdr cons)))
                                          (return)))
                                (t
                                 (push char chars)
                                 (setf previous-combining-class combining-class))))))
      (sb-kernel:with-array-data ((string string) (start) (end)
                                  :check-fill-pointer t)
        (let ((calback (if filter
                           (let ((filter (sb-kernel:%coerce-callable-to-fun filter)))
                             (lambda (char)
                               (when (funcall filter char)
                                 (callback char))))
                           #'callback)))
          (loop for i from start below end
                for char = (schar string i)
                do
                (decompose-char char compatibility calback))))
      (nreverse chars))))

(defun primary-composition (char1 char2)
  #-sb-unicode
  (declare (ignore char1 char2))
  #-sb-unicode
  #.(let* ((data (sb-cold:read-from-file "output/ucd/comp.lisp-expr"))
           (entries (loop for pair across data
                          for key = (car pair)
                          for c1 = (ldb (byte 21 21) key)
                          for c2 = (ldb (byte 21 0) key)
                          when (and (< c1 sb-xc:char-code-limit)
                                    (< c2 sb-xc:char-code-limit))
                          collect pair)))
      (aver (null entries))
      nil)
  #+sb-unicode
  (flet ((composition-hangul-syllable-type (cp)
           (cond
             ((and (<= #x1100 cp) (<= cp #x1112)) :L)
             ((and (<= #x1161 cp) (<= cp #x1175)) :V)
             ((and (<= #x11a8 cp) (<= cp #x11c2)) :T)
             ((and (<= #xac00 cp) (<= cp #.(+ #xac00 11171)))
              (if (= 0 (rem (- cp #xac00) 28)) :LV :LVT)))))
    (declare (inline composition-hangul-syllable-type))
    (let ((c1 (char-code char1))
          (c2 (char-code char2)))
       (cond
         ((gethash (dpb c1 (byte 21 21) c2)
                   (load-time-value
                    (let ((data '#.(sb-cold:read-from-file "output/ucd/comp.lisp-expr")))
                      (sb-impl::%stuff-hash-table
                       (make-hash-table :size (length data) #+64-bit :test #+64-bit #'eq)
                       (loop for pair across data
                             collect (cons (car pair) (code-char (cdr pair))))
                       t))
                    t)))
         ((and (eql (composition-hangul-syllable-type c1) :L)
               (eql (composition-hangul-syllable-type c2) :V))
          (let ((lindex (- c1 #x1100))
                (vindex (- c2 #x1161)))
            (code-char (+ #xac00 (* lindex 588) (* vindex 28)))))
         ((and (eql (composition-hangul-syllable-type c1) :LV)
               (eql (composition-hangul-syllable-type c2) :T))
          (code-char (+ c1 (- c2 #x11a7))))))))

(defun canonically-compose (list)
  (let* ((result list)
         (combine-with (member 0 result :key #'combining-class))
         (previous combine-with)
         (current (cdr combine-with)))
    (when (null current)
      (return-from canonically-compose list))
    (tagbody
     again
       (when (and (neq previous combine-with)
                  ;; test for Blocked (Unicode 3.11 para. D115)
                  ;;
                  ;; (assumes here that string has sorted combiners,
                  ;; so can look back just one step)
                  (>= (combining-class (car previous))
                      (combining-class (car current))))
         (when (= (combining-class (car current)) 0)
           (setf combine-with current))
         (setf previous current)
         (pop current)
         (go next))

       (let ((comp (primary-composition (car combine-with) (car current))))
         (cond
           (comp
            (setf (car combine-with) comp
                  (cdr previous) (setf current (cdr current))))
           (t
            (when (= (combining-class (car current)) 0)
              (setf combine-with current))
            (setf previous current)
            (pop current))))
     next
       (when current
         (go again)))
    result))

(defun normalize-string (string &optional (form :nfd)
                                          filter)
  "Normalize STRING to the Unicode normalization form FORM.
Acceptable values for form are :NFD, :NFC, :NFKD, and :NFKC.
If FILTER is a function it is called on each decomposed character and
only characters for which it returns T are collected."
  (declare (type (member :nfd :nfkd :nfc :nfkc) form))
  #-sb-unicode
  (declare (ignore filter))
  #-sb-unicode
  (etypecase string
    ((array nil (*)) string)
    (string
     (ecase form
       ((:nfc :nfkc) string)
       ((:nfd :nfkd) (error "Cannot normalize to ~A form in #-SB-UNICODE builds" form)))))
  #+sb-unicode
  (etypecase string
    (base-string string)
    ((array character (*))
     (coerce
      (ecase form
        ((:nfc)
         (canonically-compose (decompose-string string nil filter)))
        ((:nfd)
         (decompose-string string nil filter))
        ((:nfkc)
         (canonically-compose (decompose-string string t filter)))
        ((:nfkd)
         (decompose-string string t filter)))
      'string))
    ((array nil (*)) string)))

(defun normalized-p (string &optional (form :nfd))
  "Tests if STRING is normalized to FORM"
  (etypecase string
    (base-string t)
    ((array character (*))
     (flet ((=-to-list (list)
              (sb-kernel:with-array-data ((string string) (start) (end)
                                          :check-fill-pointer t)
                (loop for i from start below end
                      for char = (schar string i)
                      always (eql char (pop list))))))
       (ecase form
         ((:nfc)
          (=-to-list (canonically-compose (decompose-string string nil nil))))
         ((:nfd)
          (=-to-list (decompose-string string nil nil)))
         ((:nfkc)
          (=-to-list (canonically-compose (decompose-string string t nil))))
         ((:nfkd)
          (=-to-list (decompose-string string t nil))))))
    ((array nil (*)) t)))


;;; Unicode case algorithms
;; FIXME: Make these parts less redundant (macro?)
(sb-ext:define-load-time-global **special-titlecases**
  '#.(sb-cold:read-from-file "output/ucd/titlecases.lisp-expr"))

(sb-ext:define-load-time-global **special-casefolds**
  '#.(sb-cold:read-from-file "output/ucd/foldcases.lisp-expr"))

(defun has-case-p (char)
  ;; Bit 6 is the Unicode case flag, as opposed to the Common Lisp one
  (logbitp 6 (aref +character-misc-database+ (+ 5 (misc-index char)))))

(defun char-uppercase (char)
  (if (has-case-p char)
      (let ((cp (char-case-info char)))
        (if (consp cp)
            (let ((cp (car cp)))
              (if (atom cp)
                  (list (code-char cp))
                  (mapcar #'code-char cp)))
            (list (code-char (ldb (byte 21 21) cp)))))
      (list char)))

(defun char-lowercase (char)
  (if (has-case-p char)
      (let ((cp (char-case-info char)))
        (if (consp cp)
            (let ((cp (cdr cp)))
              (if (atom cp)
                  (list (code-char cp))
                  (mapcar #'code-char cp)))
            (list (code-char (ldb (byte 21 0) cp)))))
      (list char)))

(defun char-titlecase (char)
  (unless (has-case-p char) (return-from char-titlecase (list char)))
  (let* ((cp (char-code char))
         (value (assoc cp **special-titlecases**)))
    (if value
        (if (atom (cdr value))
            (list (code-char (cdr value)))
            (mapcar #'code-char (cdr value)))
        (char-uppercase char))))

(defun char-foldcase (char)
  (unless (has-case-p char) (return-from char-foldcase (list char)))
  (let* ((cp (char-code char))
         (value (assoc cp **special-casefolds**)))
    (if value
        (if (atom (cdr value))
            (list (code-char (cdr value)))
            (mapcar #'code-char (cdr value)))
        (char-lowercase char))))

(defun string-somethingcase (fn string special-fn)
  (let (result (len (length string)))
    (loop for index from 0 below len
       for char = (char string index)
       for cased = (or (funcall special-fn char index len)
                       (funcall fn char))
       do (loop for c in (remove :none cased) do (push c result)))
    (setf result (nreverse result))
    (coerce result 'string)))

(declaim (type function sb-unix::posix-getenv))
(defun get-user-locale ()
  (let ((raw-locale
         #+(or win32 unix) (or (sb-unix::posix-getenv "LC_ALL")
                                (sb-unix::posix-getenv "LANG"))
         #-(or win32 unix) nil))
    (when raw-locale
      (let ((lang-code (string-upcase
                        (subseq raw-locale 0 (position #\_ raw-locale)))))
        (when lang-code
          (intern lang-code "KEYWORD"))))))


(defun uppercase (string &key locale)
  "Returns the full uppercase of STRING according to the Unicode standard.
The result is not guaranteed to have the same length as the input. If :LOCALE
is NIL, no language-specific case transformations are applied. If :LOCALE is a
keyword representing a two-letter ISO country code, the case transforms of that
locale are used. If :LOCALE is T, the user's current locale is used (Unix and
Win32 only)."
  (when (eq locale t) (setf locale (get-user-locale)))
  (string-somethingcase
   #'char-uppercase string
   #-sb-unicode (constantly nil)
   #+sb-unicode ;; code-char with a constant > 255 breaks the build
   #'(lambda (char index len)
       (declare (ignore len))
       (cond
         ((and (eql locale :lt) (char= char (code-char #x0307))
                  (loop for i from (1- index) downto 0
                     for c = (char string i)
                     do (case (combining-class c)
                          (0 (return (soft-dotted-p c)))
                          (230 (return nil))
                          (t t))
                     finally (return nil)))
          '(:none))
         ((and (or (eql locale :tr) (eql locale :az))
               (char= char #\i))
          (list (code-char #x0130)))
         (t nil)))))

(defun lowercase (string &key locale)
  "Returns the full lowercase of STRING according to the Unicode standard.
The result is not guaranteed to have the same length as the input.
:LOCALE has the same semantics as the :LOCALE argument to UPPERCASE."
  (when (eq locale t) (setf locale (get-user-locale)))
  (string-somethingcase
   #'char-lowercase string
   #-sb-unicode (constantly nil)
   #+sb-unicode
   #'(lambda (char index len)
       (cond
         ((and (char= char (code-char #x03A3))
               (loop for i from (1- index) downto 0
                  for c = (char string i)
                  do (cond ((cased-p c) (return t))
                           ((case-ignorable-p c))
                           (t (return nil)))
                  finally (return nil))
               (loop for i from (1+ index) below len
                  for c = (char string i)
                  do (cond ((cased-p c) (return nil))
                           ((case-ignorable-p c))
                           (t (return t)))
                  finally (return t)))
          (list (code-char #x03C2)))
       ((eql locale :lt)
        (mapcar
         #'code-char
         (cdr (or
               (assoc (char-code char)
                      '((#x00CC . (#x0069 #x0307 #x0300))
                        (#x00CD . (#x0069 #x0307 #x0301))
                        (#x0128 . (#x0069 #x0307 #x0303))))
               (and (loop for i from (1+ index) below len
                       for c = (char string i)
                       do (case (combining-class c)
                            (230 (return t))
                            (0 (return nil))
                            (t t))
                       finally (return nil))
                    (assoc (char-code char)
                           '((#x0049 . (#x0069 #x0307))
                             (#x004A . (#x006A #x0307))
                             (#x012E . (#x012F #x0307)))))))))
       ((or (eql locale :tr) (eql locale :az))
        (cond
          ((char= char (code-char #x0130)) (list #\i))
          ((and (char= char (code-char #x0307))
                (loop for i from (1- index) downto 0
                   for c = (char string i)
                   do (case (combining-class c)
                        (0 (return (char= c #\I)))
                        (230 (return nil))
                        (t t))
                   finally (return nil)))
           '(:none))
          ((and (char= char #\I)
                (loop for i from (1+ index) below len
                   for c = (char string i)
                   do (case (combining-class c)
                        (0 (return t))
                        (230 (return (char/= c (code-char #x0307))))
                        (t t))
                   finally (return t)))
           (list (code-char #x0131)))
          (t nil)))
       (t nil)))))

(defun titlecase (string &key locale)
  "Returns the titlecase of STRING. The resulting string can
be longer than the input.
:LOCALE has the same semantics as the :LOCALE argument to UPPERCASE."
  (when (eq locale t) (setf locale (get-user-locale)))
  (let ((words (words string))
        (cased nil))
   (loop for word in words
      for first-cased = (or (position-if #'cased-p word) 0)
      for pre = (subseq word 0 first-cased)
      for initial = (char word first-cased)
      for rest = (subseq word (1+ first-cased))
      do (let ((up (char-titlecase initial)) (down (lowercase rest)))
           #+sb-unicode
           (when (and (or (eql locale :tr) (eql locale :az))
                      (eql initial #\i))
             (setf up (list (code-char #x0130))))
           #+sb-unicode
           (when (and (eql locale :lt)
                      (soft-dotted-p initial)
                      (eql (char down
                                 (position-if
                                  #'(lambda (c)
                                      (or (eql (combining-class c) 0)
                                          (eql (combining-class c) 230))) down))
                           (code-char #x0307)))
             (setf down (delete (code-char #x0307) down :count 1)))
           (push (concatenate 'string pre up down) cased)))
   (apply #'concatenate 'string (nreverse cased))))

(defun casefold (string)
  "Returns the full casefolding of STRING according to the Unicode standard.
Casefolding removes case information in a way that allows the results to be used
for case-insensitive comparisons.
The result is not guaranteed to have the same length as the input."
  (string-somethingcase #'char-foldcase string (constantly nil)))


;;; Unicode break algorithms
;;; In all the breaking methods:
;;; (brk) establishes a break between `first` and `second`
;;; (nobrk) prevents a break between `first` and `second`
;;; Setting flag=T/state=:nobrk-next prevents a break between `second` and `third`

(defun grapheme-break-class (char)
  "Returns the grapheme breaking class of CHARACTER, as specified in UAX #29."
  (let ((cp (when char (char-code char)))
        (gc (when char (general-category char)))
        (not-spacing-mark
         #(#x102B #x102C #x1038 #x1062 #x1063 #x1064 #x1067 #x1068 #x1069
           #x106A #x106B #x106C #x106D #x1083 #x1087 #x1088 #x1089 #x108A
           #x108B #x108C #x108F #x109A #x109B #x109C #x19B0 #x19B1 #x19B2
           #x19B3 #x19B4 #x19B8 #x19B9 #x19BB #x19BC #x19BD #x19BE #x19BF
           #x19C0 #x19C8 #x19C9 #x1A61 #x1A63 #x1A64 #xAA7B #xAA7D)))
    (cond
      ((not char) nil)
      ((= cp 10) :LF)
      ((= cp 13) :CR)
      ((or (and (member gc '(:Zl :Zp :Cc :Cs :Cf))
                (not (proplist-p char :prepended-concatenation-mark))
                (not (<= #x200C cp #x200D))
                ;; not documented but in the normative file
                (not (<= #xE0020 cp #xE007F)))
           ;; From Cn and Default_Ignorable_Code_Point
           (eql cp #x2065) (eql cp #xE0000)
           (<= #xFFF0 cp #xFFF8)
           (<= #xE0002 cp #xE001F)
           (<= #xE0080 cp #xE00FF)
           (<= #xE01F0 cp #xE0FFF)) :control)
      ((or (member gc '(:Mn :Me))
           (proplist-p char :other-grapheme-extend)
           (proplist-p char :emoji-modifier))
       :extend)
      ((= cp #x200D) :zwj)
      ((<= #x1F1E6 cp #x1F1FF) :regional-indicator)
      ((or
        ;; Consonant_Preceding_Repha
        (= cp #x0D4E) (= cp #x11941) (= cp #x11D46) (= cp #x11F02)
        ;; Consonant_Prefixed
        (<= #x111C2 cp #x111C3) (= cp #x1193F) (= cp #x11A3A) (<= #x11A84 cp #x11A89)
        (proplist-p char :prepended-concatenation-mark))
       :prepend)
      ((and (or (eql gc :Mc)
                (eql cp #x0E33) (eql cp #x0EB3))
            (not (binary-search cp not-spacing-mark))) :spacing-mark)
      ((hangul-syllable-type char)))))

(macrolet ((def (name extendedp)
             `(defun ,name (function string)
                (do* ((length (length string))
                      (start 0)
                      (end 1 (1+ end))
                      (char1 nil)
                      (c1 nil)
                      (char2 (char string 0))
                      (c2 (and (> (length string) 0) (grapheme-break-class char2)))
                      (extended-pictographic-state nil)
                      (nri (if (eql c2 :regional-indicator) 1 0)))
                     ((>= end length)
                      (if (= end length) (progn (funcall function string start end) nil)))
                  (flet ((brk () (funcall function string start end) (setf start end)))
                    (declare (dynamic-extent #'brk))
                    (shiftf char1 char2 (char string end))
                    (shiftf c1 c2 (grapheme-break-class char2))
                    (if (eql c2 :regional-indicator) (incf nri) (setf nri 0))
                    (setf extended-pictographic-state
                          (cond
                            ((proplist-p char1 :extended-pictographic) :start)
                            ((and (member extended-pictographic-state '(:start :extend))
                                  (eql c1 :extend))
                             :extend)
                            ((and (member extended-pictographic-state '(:start :extend))
                                  (eql c1 :zwj))
                             :zwj)
                            (t nil)))
                    (cond
                      ((and (eql c1 :cr) (eql c2 :lf)))
                      ((or (member c1 '(:control :cr :lf))
                           (member c2 '(:control :cr :lf)))
                       (brk))
                      ((or (and (eql c1 :l) (member c2 '(:l :v :lv :lvt)))
                           (and (or (eql c1 :v) (eql c1 :lv))
                                (or (eql c2 :v) (eql c2 :t)))
                           (and (eql c2 :t) (or (eql c1 :lvt) (eql c1 :t)))))
                      ((member c2 '(:extend :zwj)))
                      ,@(when extendedp
                          `(((or (eql c2 :spacing-mark) (eql c1 :prepend)))))
                      ((and (eql extended-pictographic-state :zwj)
                            (proplist-p char2 :extended-pictographic)))
                      ((and (eql c1 :regional-indicator) (eql c2 :regional-indicator) (evenp nri)))
                      (t (brk))))))))
  (def map-legacy-grapheme-boundaries nil)
  (def map-grapheme-boundaries t))

(macrolet ((def (name mapper)
             `(defun ,name (function string)
                (let ((array (make-array 0 :element-type (array-element-type string) :adjustable t :displaced-to string)))
                  (flet ((fun (string start end)
                           (declare (type string string))
                           (funcall function (adjust-array array (- end start) :displaced-to string :displaced-index-offset start))))
                    (declare (dynamic-extent #'fun))
                    (,mapper #'fun string))))))
  (def map-legacy-graphemes map-legacy-grapheme-boundaries)
  (def map-graphemes map-grapheme-boundaries))

(defun graphemes (string)
  "Breaks STRING into graphemes according to the default
grapheme breaking rules specified in UAX #29, returning a list of strings."
  (let (result)
    (map-graphemes (lambda (a) (push (subseq a 0) result)) string)
    (nreverse result)))

(defun word-break-class (char)
  "Returns the word breaking class of CHARACTER, as specified in UAX #29."
  ;; Words use graphemes as characters to deal with the ignore rule
  (when (listp char) (setf char (car char)))
  (let ((cp (when char (char-code char)))
        (gc (when char (general-category char)))
        (newlines
         (coerce-to-ordered-ranges #(#xB #xC #x0085 #x0085 #x2028 #x2029)))
        (also-katakana
         (coerce-to-ordered-ranges
          #(#x3031 #x3035 #x309B #x309C
            #x30A0 #x30A0 #x30FC #x30FC
            #xFF70 #xFF70)))
        (also-aletter
         #(#x02C2 #x02C3 #x02C4 #x02C5 #x02D2 #x02D3 #x02D4 #x02D5 #x02D6 #x02D7
           #x02DE #x02DF
           #x02E5 #x02E6 #x02E7 #x02E8 #x02E9 #x02EA #x02EB
           #x02ED #x02EF #x02F0 #x02F1 #x02F2 #x02F3 #x02F4 #x02F5
           #x02F6 #x02F7 #x02F8 #x02F9 #x02FA #x02FB #x02FC #x02FD #x02FE #x02FF
           #x055A #x055B #x055C #x055E #x058A
           #x05F3 #xA708 #xA709 #xA70A #xA70B #xA70C #xA70D #xA70E #xA70F
           #xA710 #xA711 #xA712 #xA713 #xA714 #xA715 #xA716
           #xA720 #xA721 #xA789 #xA78A #xAB5B))
        (midnumlet #(#x002E #x2018 #x2019 #x2024 #xFE52 #xFF07 #xFF0E))
        (midletter
         #(#x003A #x00B7 #x0387 #x055F #x05F4 #x2027 #xFE13 #xFE55 #xFF1A))
        (midnum
         ;; Grepping of Line_Break = IS adjusted per UAX #29
         #(#x002C #x003B #x037E #x0589 #x060C #x060D #x066C #x07F8 #x2044
           #xFE10 #xFE14 #xFE50 #xFE54 #xFF0C #xFF1B))
        (zs-and-glue
         ;; Grepping of Line_Break = ";GL.*Zs"
         #(#x00A0 #x2007 #x202F)))
    (cond
      ((not char) nil)
      ((= cp 10) :LF)
      ((= cp 13) :CR)
      ((ordered-ranges-member cp newlines) :newline)
      ((or (eql (grapheme-break-class char) :extend)
           (and (eql gc :mc) (not (= cp #x200D)))) :extend)
      ((= cp #x200D) :zwj)
      ((<= #x1F1E6 cp #x1F1FF) :regional-indicator)
      ((and (eql gc :Cf) (not (<= #x200B cp #x200D))) :format)
      ((or (eql (script char) :katakana)
           (ordered-ranges-member cp also-katakana)) :katakana)
      ((and (eql (script char) :Hebrew) (eql gc :lo)) :hebrew-letter)
      ((and (or (alphabetic-p char) (binary-search cp also-aletter))
            (not (or (ideographic-p char)
                     (eql (line-break-class char) :sa)
                     (eql (script char) :hiragana)))) :aletter)
      ((= cp #x27) :single-quote)
      ((= cp #x22) :double-quote)
      ((binary-search cp midnumlet) :midnumlet)
      ((binary-search cp midletter) :midletter)
      ((binary-search cp midnum) :midnum)
      ((or (eql gc :Nd) (eql cp #x066B)) :numeric)
      ((or (eql gc :Pc) (= cp #x202F)) :extendnumlet)
      ((and (eql gc :Zs) (not (binary-search cp zs-and-glue))) :wsegspace)
      (t nil))))

(defmacro flatpush (thing list)
  (let ((%thing (gensym)) (%i (gensym)))
    `(let ((,%thing ,thing))
       (if (listp ,%thing)
           (dolist (,%i ,%thing)
             (push ,%i ,list))
           (push ,%thing ,list)))))

(defun words (string)
  "Breaks STRING into words according to the default
word breaking rules specified in UAX #29. Returns a list of strings"
  (let ((chars (coerce string 'list))
        words word flag)
    (flatpush (car chars) word)
    (do ((first (car chars) second)
         (tail (cdr chars) (cdr tail))
         (second (cadr chars) (cadr tail))
         (nri 0))
        ((not first) (nreverse (mapcar #'(lambda (l) (coerce l 'string)) words)))
      (flet ((extended-word-break-class (list)
               (loop for tail on list
                     for thing = (car tail)
                     for class = (word-break-class thing)
                     while thing
                     unless (or (eql class :format) (eql class :extend) (eql class :zwj))
                       return class))
             (brk () (push (nreverse word) words) (setf word nil) (flatpush second word))
             (nobrk () (flatpush second word)))
        (let ((c1 (word-break-class first))
              (c2 (word-break-class second))
              (c3 (extended-word-break-class (cdr tail))))
          (when (eql c1 :regional-indicator)
            (incf nri (if (atom first) 1 (count :regional-indicator first :key #'word-break-class))))
          (cond
            ;; handle multiple no-breaks
            (flag
             (nobrk)
             (unless (or (eql c2 :format) (eql c2 :extend) (eql c2 :zwj))
               (setf flag nil)))
            ((and (eql c1 :cr) (eql c2 :lf)) (nobrk))
            ;; CR+LF is handled in the line above
            ((or (eql c1 :newline) (eql c1 :cr) (eql c1 :lf)
                 (eql c2 :newline) (eql c2 :cr) (eql c2 :lf)) (brk))
            ((and (eql c1 :zwj) (and second (proplist-p second :extended-pictographic)))
             (nobrk))
            ((and (eql c1 :wsegspace) (eql c2 :wsegspace)) (nobrk))
            ((or (eql c2 :format) (eql c2 :extend) (eql c2 :zwj))
             ;; handle Any x (Format|Extend|ZWJ)
             (nobrk))
            (t
             (when (or (eql c1 :format) (eql c1 :extend) (eql c1 :zwj))
               ;; find the class of the non-ZWJ_FE character preceding in this word, if any
               (setf c1 (loop for char in word
                              for class = (word-break-class char)
                              unless (or (eql class :format) (eql class :extend) (eql class :zwj))
                              do (return class))))
             (cond
               ((and (or (eql c1 :aletter) (eql c1 :hebrew-letter))
                     (or (eql c2 :aletter) (eql c2 :hebrew-letter))) (nobrk))
               ((and (or (eql c1 :aletter) (eql c1 :hebrew-letter))
                     (member c2 '(:midletter :midnumlet :single-quote))
                     (or (eql c3 :aletter) (eql c3 :hebrew-letter)))
                (nobrk) (setf flag t)) ; Handle the multiple breaks from this rule
               ((and (eql c1 :hebrew-letter) (eql c2 :double-quote)
                     (eql c3 :hebrew-letter))
                (nobrk) (setf flag t))
               ((and (eql c1 :hebrew-letter) (eql c2 :single-quote)) (nobrk))
               ((or (and (eql c1 :numeric) (member c2 '(:numeric :aletter :hebrew-letter)))
                    (and (eql c2 :numeric) (member c1 '(:numeric :aletter :hebrew-letter))))
                (nobrk))
               ((and (eql c1 :numeric)
                     (member c2 '(:midnum :midnumlet :single-quote))
                     (eql c3 :numeric))
                (nobrk) (setf flag t))
               ((and (eql c1 :katakana) (eql c2 :katakana)) (nobrk))
               ((or (and (member c1
                                 '(:aletter :hebrew-letter :katakana
                                   :numeric :extendnumlet)) (eql c2 :extendnumlet))
                    (and (member c2
                                 '(:aletter :hebrew-letter :katakana
                                   :numeric :extendnumlet)) (eql c1 :extendnumlet)))
                (nobrk))
               ((and (eql c1 :regional-indicator) (eql c2 :regional-indicator) (oddp nri))
                (nobrk))
               (t (brk))))))))))

(defun sentence-break-class (char)
  "Returns the sentence breaking class of CHARACTER, as specified in UAX #29."
  (when (listp char) (setf char (car char)))
  (let ((cp (when char (char-code char)))
        (gc (when char (general-category char)))
        (aterms #(#x002E #x2024 #xFE52 #xFF0E))
        (scontinues
         #(#x002C #x002D #x003A #x055D #x060C #x060D #x07F8 #x1802 #x1808
           #x2013 #x2014 #x3001 #xFE10 #xFE11 #xFE13 #xFE31 #xFE32 #xFE50
           #xFE51 #xFE55 #xFE58 #xFE63 #xFF0C #xFF0D #xFF1A #xFF64)))
    (cond
      ((not char) nil)
      ((= cp 10) :LF)
      ((= cp 13) :CR)
      ((or (eql (grapheme-break-class char) :extend)
           (= cp #x200D)
           (eql gc :mc))
       :extend)
      ((or (eql cp #x0085) (<= #x2028 cp #x2029)) :sep)
      ((and (eql gc :Cf) (not (<= #x200C cp #x200D))) :format)
      ((whitespace-p char) :sp)
      ((and (lowercase-p char)
            (not (<= #x10D0 cp #x10FA))
            (not (<= #x10FD cp #x10FF)))
       :lower)
      ((or (eql gc :Lt)
           (and (uppercase-p char)
                (not (<= #x1C90 cp #x1CBA))
                (not (<= #x1CBD cp #x1CBF))))
       :upper)
      ((or (alphabetic-p char) (eql cp #x00A0) (eql cp #x05F3)) :oletter)
      ((or (eql gc :Nd) (<= #x066B cp #x066C)) :numeric)
      ((binary-search cp aterms) :aterm)
      ((binary-search cp scontinues) :scontinue)
      ((proplist-p char :sentence-terminal) :sterm)
      ((and (or (member gc '(:Po :Ps :Pe :Pf :Pi))
                (eql (line-break-class char) :qu)))
       :close)
      (t nil))))

(defun sentence-prebreak (string)
  "Pre-combines some sequences of characters to make the sentence-break
algorithm simpler..
Specifically,
- Combines any character with the following extend of format characters
- Combines CR + LF into '(CR LF)
- Combines any run of :cp*:close* into one character"
  (let ((chars (coerce string 'list))
        cluster clusters last-seen sp-run)
    (labels ((flush () (if (cdr cluster) (push (nreverse cluster) clusters)
                           (if cluster (push (car cluster) clusters)))
                    (setf cluster nil))
             (brk (x)
               (flush) (push x clusters))
             (nobrk (x) (push x cluster)))
    (loop for ch in chars
       for type = (sentence-break-class ch)
       do (cond
            ((and (eql last-seen :cr) (eql type :lf)) (nobrk ch) (flush) (setf last-seen nil))
            ((eql last-seen :cr) (brk ch) (setf last-seen nil))
            ((eql type :cr) (nobrk ch) (setf last-seen :cr))
            ((eql type :lf) (brk ch) (setf last-seen nil))
            ((eql type :sep) (brk ch) (setf last-seen nil))
            ((and last-seen (or (eql type :extend) (eql type :format)))
             (nobrk ch))
            ((eql type :close)
             (unless (eql last-seen :close) (flush))
             (nobrk ch) (setf last-seen :close sp-run nil))
            ((eql type :sp)
             (unless (or (and (not sp-run) (eql last-seen :close)) (eql last-seen :sp))
               (flush) (setf sp-run t))
             (nobrk ch) (setf last-seen :sp))
            (t (flush) (nobrk ch) (setf last-seen type sp-run nil))))
    (flush) (nreverse clusters))))

(defun sentences (string)
  "Breaks STRING into sentences according to the default
sentence breaking rules specified in UAX #29"
  (let ((special-handling '(:close :sp :sep :cr :lf :scontinue :sterm :aterm))
        (chars (sentence-prebreak string))
        sentence sentences state)
    (flatpush (car chars) sentence)
    (do ((first (car chars) second)
         (tail (cdr chars) (cdr tail))
         (second (cadr chars) (cadr tail))
         (third (caddr chars) (caddr tail)))
        ((not first)
         (progn
           ; Shake off last sentence
           (when sentence (push (nreverse sentence) sentences))
           (nreverse (mapcar #'(lambda (l) (coerce l 'string)) sentences))))
      (flet ((brk () (push (nreverse sentence) sentences)
                  (setf sentence nil) (flatpush second sentence))
             (nobrk () (flatpush second sentence)))
      (let ((c1 (sentence-break-class first))
            (c2 (sentence-break-class second))
            (c3 (sentence-break-class third)))
        (cond
          ((eql state :brk-next) (brk) (setf state nil))
          ((eql state :nobrk-next) (nobrk) (setf state nil))
          ((member c1 '(:sep :cr :lf)) (brk))
          ((and (eql c1 :aterm) (eql c2 :numeric)) (nobrk))
          ((and (member c1 '(:lower :upper)) (eql c2 :aterm)
                (eql c3 :upper)) (nobrk) (setf state :nobrk-next))
          ((or (and (member c1 '(:sterm :aterm)) (member c2 '(:close :sp))
                    (member c3 '(:scontinue :sterm :aterm)))
               (and (member c1 '(:sterm :aterm))
                    (member c2 '(:scontinue :sterm :aterm))))
           (nobrk) (when (member c2 '(:close :sp)) (setf state :nobrk-next)))
          ((and (member c1 '(:sterm :aterm)) (member c2 '(:close :sp))
                (member c3 '(:sep :cr :lf)))
           (nobrk) (setf state :nobrk-next)) ;; Let the linebreak call (brk)
          ((and (member c1 '(:sterm :aterm)) (member c2 '(:sep :cr :lf)))
           (nobrk)) ; Doesn't trigger rule 8
          ((eql c1 :sterm) ; Not ambiguous anymore, rule 8a already handled
           (if (member c2 '(:close :sp))
               (progn (nobrk) (setf state :brk-next))
               (brk)))
          ((and (eql c2 :sterm) third (not (member c3 special-handling)))
           (nobrk) (setf state :brk-next)) ; STerm followed by nothing important
          ((or (eql c1 :aterm)
               (and (eql c2 :aterm) third
                    (not (member c3 special-handling)) (not (eql c3 :numeric))))
           ; Finally handle rule 8
           (if (loop for c in
                    (if (and third (not (or (member c3 special-handling)
                                            (eql c3 :numeric))))
                        (cdr tail) tail)
                  for type = (sentence-break-class c) do
                    (when (member type '(:oletter :upper :sep :cr :lf
                                         :sterm :aterm))
                      (return nil))
                    (when (eql type :lower) (return t)) finally (return nil))
               ; Ambiguous case
               (progn (nobrk) (setf state :nobrk-next))
               ; Otherwise
               (if (member c2 '(:close :sp :aterm))
                   (progn (nobrk) (setf state :brk-next))
                   (brk))))
          (t (nobrk))))))))

(defun line-prebreak (string)
  (let ((chars (coerce string 'list))
        cluster clusters last-seen)
    (loop for char in chars
       for type = (line-break-class char)
       do
         (when
             (and cluster
                  (or
                   (not (or (eql type :cm) (eql type :zwj)))
                   (and (or (eql type :cm) (eql type :zwj))
                        (member last-seen '(nil :BK :CR :LF :NL :SP :ZW)))))
           (if (cdr cluster)
               (push (nreverse cluster) clusters)
               (push (car cluster) clusters))
           (setf cluster nil))
         (unless (or (eql type :cm) (eql type :zwj))
           (setf last-seen type))
         (push char cluster))
    (if (cdr cluster)
        (push (nreverse cluster) clusters)
        (push (car cluster) clusters))
    (nreverse clusters)))

(defun line-break-annotate (string)
  (let ((chars (line-prebreak string))
        first second t1 t2 tail (ret (list :cant))
        state after-spaces nri)
    (macrolet ((cmpush (thing)
                 (let ((gthing (gensym)))
                   `(let ((,gthing ,thing))
                      (if (listp ,gthing)
                          (loop for (c next) on ,gthing do
                               (push c ret)
                               (when next (push :cant ret)))
                          (push ,thing ret)))))
               (between (a b action)
                 (let ((atest (if (eql a :any) t
                                  (if (listp a)
                                      (if (eql (car a) 'not)
                                          `(not (member t1 ,(cadr a)))
                                          `(member t1 ,a))
                                      `(eql t1 ,a))))
                       (btest (if (eql b :any) t
                                  (if (listp b)
                                      `(member t2 ,b)
                                      `(eql t2 ,b)))))
                 `(when (and ,atest ,btest)
                    (cmpush ,action)
                    (cmpush second)
                    (go tail))))
               (after-spaces (a b action)
                 (let ((atest (if (eql a :any) t
                                  (if (listp a)
                                      `(member t1 ,a)
                                      `(eql t1 ,a))))
                       (btest (if (eql b :any) t
                                  (if (listp b)
                                      `(member type ,b)
                                      `(eql type ,b)))))
                   `(when
                        (and ,atest
                             (loop for c in tail
                                for type = (line-break-class c :resolve t)
                                do
                                  (when (not (eql type :sp))
                                    (return ,btest))))
                      (if (eql t2 :sp)
                         (progn (cmpush :cant)
                                (cmpush second)
                                (setf state :eat-spaces)
                                (setf after-spaces ,action)
                                (go tail))
                         (progn (cmpush ,action)
                                (cmpush second)
                                (go tail)))))))

      (cmpush (car chars))
      (setf first (car chars))
      (setf tail (cdr chars))
      (setf second (car tail))
      (setf nri 0)
      (tagbody
       top
         (when (not first) (go end))
         (setf t1 (line-break-class first :resolve t))
         (setf t2 (line-break-class second :resolve t))
         (if (and (eql t1 :ri) (eql t2 :ri)) (incf nri) (setf nri 0))
         (between :any :nil :must)      ; LB3
         (when (and (eql state :eat-spaces) (eql t2 :sp))
           (cmpush :cant) (cmpush second) (go tail))
         (between :bk :any :must)                    ; LB4
         (between :cr :lf :cant)                     ; LB5
         (between '(:cr :lf :nl) :any :must)         ; LB5
         (between :any '(:zw :bk :cr :lf :nl) :cant) ; LB6, LB7
         (when after-spaces (cmpush after-spaces) (cmpush second)
               (setf state nil after-spaces nil) (go tail))
         (after-spaces :zw :any :can)                ; LB8
         (between :zwj :any :cant)                   ; LB8a
         ;; LB9 and LB10 (for CM) handled in LINE-BREAK-CLASS / LINE-PREBREAK
         (when (eql t1 :zwj) (setf t1 :al))          ; LB10 (for ZWJ)
         (when (eql t2 :zwj) (setf t2 :al))          ; LB10 (for ZWJ)
         (between :any :wj :cant)                    ; LB11
         (between :wj :any :cant)                    ; LB11
         (between :gl :any :cant)                    ; LB12
         (between (not '(:sp :ba :hy)) :gl :cant)    ; LB12a
         (between :any '(:cl :cp :ex :is :sy) :cant) ; LB13
         (after-spaces :op :any :cant)               ; LB14
         (after-spaces :qu :op :cant)                ; LB15
         (after-spaces '(:cl :cp) :ns :cant)         ; LB16
         (after-spaces :b2 :b2 :cant)                ; LB17
         (between :any :sp :cant) ; LB7, here after all AFTER-SPACES calls
         (between :sp :any :can)  ; LB18
         (between :any :qu :cant) ; LB19
         (between :qu :any :cant) ; LB19
         (between :any :cb :can)  ; LB20
         (between :cb :any :can)  ; LB20
         (when (and (eql t1 :hl) (member t2 '(:hy :ba)))
           (cmpush :cant) (cmpush second)
           (setf first second tail (cdr tail))
           (setf second (car tail))
           (cmpush (if second :cant :must)) (cmpush second)
           (setf after-spaces :can) (go tail)) ; LB21a
         (between :any '(:ba :hy :ns) :cant)   ; LB21
         (between :bb :any :cant)              ; LB21
         (between :sy :hl :cant)               ; LB21b
         (between :any :in :cant) ; LB22
         (between '(:al :hl) :nu :cant)                         ; LB23
         (between :nu '(:al :hl) :cant)                         ; LB23
         (between :pr '(:id :eb :em) :cant)        ; LB23a
         (between '(:id :eb :em) :po :cant)        ; LB23a
         (between '(:pr :po) '(:al :hl) :cant)     ; LB24
         (between '(:al :hl) '(:pr :po) :cant)     ; LB24
         (between '(:cl :cp :nu) '(:po :pr) :cant) ; LB25, first six cases
         (between '(:po :pr) :op :cant) ; LB25, two ? x OP cases
         (between '(:po :pr :hy :is :sy :nu) :nu :cant) ; LB25, six ? x NU cases
         (between :jl '(:jl :jv :h2 :h3) :cant)         ; LB26
         (between '(:jv :h2) '(:jv :jt) :cant)          ; LB26
         (between '(:jt :h3) :jt :cant)                 ; LB26
         (between '(:jl :jv :jt :h2 :h3) :po :cant)        ; LB27
         (between :pr '(:jl :jv :jt :h2 :h3) :cant)        ; LB27
         (between '(:al :hl :is) '(:al :hl) :cant) ; LB28, LB29
         (unless (member (east-asian-width (if (consp second) (car second) second)) '(:h :w :f))
           (between '(:al :hl :nu) :op :cant))        ; LB30
         (unless (member (east-asian-width (if (consp first) (car first) first)) '(:h :w :f))
           (between :cp '(:al :hl :nu) :cant))        ; LB30
         (between :ri :ri (if (oddp nri) :cant :can)) ; LB30a
         (between :eb :em :cant)                      ; LB30b
         (let ((firstchar (if (consp first) (car first) first)))
           (when (and (eql (general-category firstchar) :Cn)
                      (proplist-p firstchar :extended-pictographic))
             (between :any :em :cant)))               ; LB30b
         (between :any :any :can)       ; LB31
       tail
         (setf first second)
         (setf tail (cdr tail))
         (setf second (car tail))
         (go top)
       end)
      ;; LB3 satisfied by (:any :nil) -> :must
      (setf ret (nreverse ret))
      ret)))

(defun break-list-at (list n)
  (let ((tail list) (pre-tail nil))
    (loop repeat n do (setf pre-tail tail) (setf tail (cdr tail)))
    (setf (cdr pre-tail) nil)
    (values list tail)))

(defun lines (string &key (margin *print-right-margin*))
  "Breaks STRING into lines that are no wider than :MARGIN according to the
line breaking rules outlined in UAX #14. Combining marks will always be kept
together with their base characters, and spaces (but not other types of
whitespace) will be removed from the end of lines. If :MARGIN is unspecified,
it defaults to 80 characters"
  (when (string= string "") (return-from lines (list "")))
  (unless margin (setf margin 80))
  (do* ((chars (line-break-annotate string))
        line lines (filled 0) last-break-distance
        (break-type (car chars) (car tail))
        (char (cadr chars) (cadr tail))
        (tail (cddr chars) (cddr tail)))
       ((not break-type)
        (mapcar #'(lambda (s) (coerce s 'string)) (nreverse lines)))
    (ecase break-type
      (:cant
       (push char line)
       (unless (eql (line-break-class char) :CM)
         (incf filled))
       (when last-break-distance (incf last-break-distance)))
      (:can
       (push char line)
       (setf last-break-distance 1)
       (incf filled))
      (:must
       (push char line)
       (setf last-break-distance 1)
       (incf filled)
       (go break)))
    (if (> filled margin)
        (go break)
        (go next))
   break
    (when (not last-break-distance)
      ;; If we don't have any line breaks, remove the last thing we added that
      ;; takes up space, and all its combining marks
      (setf last-break-distance
            (1+ (loop for c in line while (eql (line-break-class c) :cm) summing 1))))
    (multiple-value-bind (next-line this-line) (break-list-at line last-break-distance)
      (loop while (eql (line-break-class (car this-line)) :sp)
         do (setf this-line (cdr this-line)))
      (push (nreverse this-line) lines)
      (setf line next-line)
      (setf filled (length line))
      (setf last-break-distance nil))
   next))


;;; Collation
(defconstant +maximum-variable-primary-element+
  #.(sb-cold:read-from-file "output/ucd/other-collation-info.lisp-expr"))

(defun unpack-collation-key (key)
  (flet ((unpack (value)
           (list (ldb (byte 16 16) value)
                 (ldb (byte 11 5) value)
                 (ldb (byte 5 0) value))))
    (declare (inline unpack))
    (loop for i by 32 below (max (integer-length key) 1)
          collect (unpack (ldb (byte 32 i) key)))))

(declaim (inline variable-p))
(defun variable-p (x)
  (<= 1 x +maximum-variable-primary-element+))

(macrolet ((collations-hash-table ()
             (let ((data (let ((*read-base* 16))
                           (sb-cold:read-from-file "output/ucd/collation.lisp-expr"))))
               #+64-bit (dovector (item data) (aver (fixnump (car item))))
               `(load-time-value
                 (sb-impl::%stuff-hash-table
                  (make-hash-table :size ,(length data) #+64-bit :test #+64-bit 'eq)
                  ',data t)
                 t))))
(defun collation-key (string start end)
  (let (char1
        (char2 (code-char 0))
        (char3 (code-char 0)))
    (case (- end start)
      (1 (setf char1 (char string start)))
      (2 (setf char1 (char string start)
               char2 (char string (+ start 1))))
      (3 (setf char1 (char string start)
               char2 (char string (+ start 1))
               char3 (char string (+ start 2))))
      (t
       ;; There are never more than three characters in a contraction, right?
       (return-from collation-key nil)))
    (let* ((code1 (char-code char1))
           (packed-key (gethash (pack-3-codepoints code1 (char-code char2) (char-code char3))
                                (collations-hash-table))))
      (if packed-key
          (unpack-collation-key packed-key)
          (when (char= (code-char 0) char2 char3)
            (let* ((unified-ideograph-p (proplist-p char1 :unified-ideograph))
                   (tangut-p (or (<= #x17000 code1 #x18AFF)
                                 (<= #x18D00 code1 #x18D8F)))
                   (nushu-p (<= #x1B170 code1 #x1B2FF))
                   (khitan-small-p (<= #x18B00 code1 #x18CFF))
                   (boffset 0)
                   (base
                     (cond ((and unified-ideograph-p
                                 (or (<= #x4E00 code1 #x9FFF) (<= #xF900 code1 #xFAFF)))
                            #xFB40)
                           (unified-ideograph-p #xFB80)
                           (tangut-p (setq boffset #x17000) #xFB00)
                           (nushu-p (setq boffset #x1B170) #xFB01)
                           (khitan-small-p (setq boffset #x18B00) #xFB02)
                           (t #xFBC0)))
                   (a (+ base (if (or tangut-p nushu-p) 0 (ash code1 -15))))
                   (b (logior #x8000 (if (or tangut-p nushu-p) (- code1 boffset) (logand code1 #x7FFF)))))
              (list (list a #x20 #x2) (list b 0 0)))))))))

(defun sort-key (string)
  (let* ((str (normalize-string string :nfd))
         (i 0) (len (length str)) max-match new-i
         sort-key
         after-variable)
    (loop while (< i len)
          do
          (loop for offset from 1 to 3
                for index = (+ i offset)
                while (<= index len)
                do
                (let ((key (collation-key str i index)))
                  (when key
                    (setf max-match key
                          new-i index))))
          (loop for index from new-i below len
                for char = (char str index)
                for combining-class = (combining-class char)
                until (eql combining-class 0)
                unless (and (>= (- index new-i) 1)
                            ;; Combiners are sorted, we only have to look back
                            ;; one step (see canonically-compose)
                            (>= (combining-class (char str (1- index)))
                                combining-class))
                do
                (rotatef (char str new-i) (char str index))
                (let ((key (collation-key str i (1+ new-i))))
                  (if key
                      (setf max-match key
                            new-i (1+ new-i))
                      (rotatef (char str new-i) (char str index)))))
          (loop for key in max-match do (push key sort-key))
          (setf i new-i))
    (macrolet ((push-non-zero (obj place)
                 `(when (/= ,obj 0)
                    (push ,obj ,place))))
      (let (primary secondary tertiary quatenary)
        (loop for (k1 k2 k3) in (nreverse sort-key)
              do
              (cond
                ((= k1 k2 k3 0))
                ((variable-p k1)
                 (setf after-variable t)
                 (push k1 quatenary))
                ((/= k1 0)
                 (setf after-variable nil)
                 (push k1 primary)
                 (push-non-zero k2 secondary)
                 (push-non-zero k3 tertiary)
                 (push #xFFFF quatenary))
                ((/= k3 0)
                 (unless after-variable
                   (push-non-zero k2 secondary)
                   (push k3 tertiary)
                   (push #xFFFF quatenary)))))
        (concatenate 'vector
                     (nreverse primary) #(0) (nreverse secondary) #(0)
                     (nreverse tertiary) #(0) (nreverse quatenary))))))

(defun vector< (vector1 vector2)
  (loop for i across vector1
        for j across vector2
        do
        (cond ((< i j) (return-from vector< t))
              ((> i j) (return-from vector< nil))))
  ;; If there's no differences, shortest vector wins
  (< (length vector1) (length vector2)))

(defun unicode= (string1 string2 &key (start1 0) end1 (start2 0) end2 (strict t))
  "Determines whether STRING1 and STRING2 are canonically equivalent according
to Unicode. The START and END arguments behave like the arguments to STRING=.
If :STRICT is NIL, UNICODE= tests compatibility equavalence instead."
  (let ((str1 (normalize-string (subseq string1 start1 end1) (if strict :nfd :nfkd)))
        (str2 (normalize-string (subseq string2 start2 end2) (if strict :nfd :nfkd))))
    (string= str1 str2)))

(defun unicode-equal (string1 string2 &key (start1 0) end1 (start2 0) end2 (strict t))
  "Determines whether STRING1 and STRING2 are canonically equivalent after
casefolding (that is, ignoring case differences) according to Unicode. The
START and END arguments behave like the arguments to STRING=. If :STRICT is
NIL, UNICODE= tests compatibility equavalence instead."
  (let ((str1 (normalize-string (subseq string1 start1 end1) (if strict :nfd :nfkd)))
        (str2 (normalize-string (subseq string2 start2 end2) (if strict :nfd :nfkd))))
    (string=
     (normalize-string (casefold str1) (if strict :nfd :nfkd))
     (normalize-string (casefold str2) (if strict :nfd :nfkd)))))

(defun unicode< (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Determines whether STRING1 sorts before STRING2 using the Unicode Collation
Algorithm. The function uses an untailored Default Unicode Collation Element Table
to produce the sort keys. The function uses the Shifted method for dealing
with variable-weight characters, as described in UTS #10"
  (let* ((s1 (subseq string1 start1 end1))
         (s2 (subseq string2 start2 end2))
         (k1 (sort-key s1)) (k2 (sort-key s2)))
    (if (equalp k1 k2)
        (string< (normalize-string s1 :nfd) (normalize-string s2 :nfd))
        (vector< k1 k2))))

(defun unicode<= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Tests if STRING1 and STRING2 are either UNICODE< or UNICODE="
  (or
   (unicode= string1 string2 :start1 start1 :end1 end1
             :start2 start2 :end2 end2)
   (unicode< string1 string2 :start1 start1 :end1 end1
             :start2 start2 :end2 end2)))

(defun unicode> (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Tests if STRING2 is UNICODE< STRING1."
   (unicode< string2 string1 :start1 start2 :end1 end2
             :start2 start1 :end2 end1))

(defun unicode>= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Tests if STRING1 and STRING2 are either UNICODE= or UNICODE>"
  (or
   (unicode= string1 string2 :start1 start1 :end1 end1
             :start2 start2 :end2 end2)
   (unicode> string1 string2 :start1 start1 :end1 end1
             :start2 start2 :end2 end2)))

(clear-info :function :compiler-macro-function 'proplist-p)
