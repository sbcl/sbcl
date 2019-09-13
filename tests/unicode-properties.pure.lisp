;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(use-package :sb-unicode)

;; Taken straight out of the common lisp cookbook
(defun replace-all (part replacement string &key (test #'char=))
"Returns a new string in which all the occurences of the part
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))

(defun test-line (line)
  (destructuring-bind (%cp %name %gc ccc %bidi decomp-map
                           %decimal-digit %digit %numeric
                           %bidi-mirrored %old-name old-comment
                           simple-up simple-down simple-title)
      (split-string line #\;)
    (declare (ignore decomp-map old-comment simple-up
                     simple-down simple-title))
    (let* ((cp (parse-integer %cp :radix 16))
           (char #+sb-unicode (code-char cp)
                 #-sb-unicode
                 (if (< cp 256)
                     (code-char cp)
                     (return-from test-line t)))
           (gc (intern (string-upcase %gc) "KEYWORD"))
           (bidi (intern (string-upcase %bidi) "KEYWORD"))
           ;; See `normalize-character-name` in ucd.lisp for a discussion
           ;; of U+1F5CF (PAGE) and the attendant standards-compliance issues2
           (name (unless (or (position #\< %name) (= cp #x1F5CF))
                   (substitute #\_ #\Space %name)))
           (old-name (unless (string= %old-name "")
                       (substitute #\_ #\Space %old-name)))
           (char-from-name (name-char name))
           (char-from-old-name
            ;; Exclude all the renaming conflicts, including the mass-rename of Georgian
            (when (and old-name
                       (not
                        (member old-name
                                '("BELL"
                                  "LATIN_CAPITAL_LETTER_YOGH"
                                  "LATIN_SMALL_LETTER_YOGH"
                                  "CYRILLIC_CAPITAL_LETTER_E"
                                  "CYRILLIC_CAPITAL_LETTER_I"
                                  "CYRILLIC_SMALL_LETTER_E"
                                  "CYRILLIC_SMALL_LETTER_I"
                                  "DOUBLE_VERTICAL_BAR"
                                  "HANGUL_LETTER_CIEUC"
                                  "HANGUL_LETTER_KIYEOK"
                                  "HANGUL_LETTER_PIEUP"
                                  "PARENTHESIZED_HANGUL_CIEUC"
                                  "PARENTHESIZED_HANGUL_KIYEOK"
                                  "PARENTHESIZED_HANGUL_PIEUP"
                                  "CIRCLED_HANGUL_CIEUC"
                                  "CIRCLED_HANGUL_KIYEOK"
                                  "CIRCLED_HANGUL_PIEUP"
                                  "HALFWIDTH_HANGUL_LETTER_CIEUC"
                                  "HALFWIDTH_HANGUL_LETTER_KIYEOK"
                                  "HALFWIDTH_HANGUL_LETTER_PIEUP"
                                  "SQUARED_MV")
                                :test #'string=))
                       (or (< (length old-name) 14)
                           (string/= old-name "GEORGIAN_SMALL" :end1 14)))
              (name-char old-name)))
           (decimal-digit (parse-integer %decimal-digit :junk-allowed t))
           (digit (parse-integer %digit :junk-allowed t))
           (numeric (if (string= %numeric "") nil (read-from-string %numeric)))
           (bidi-mirrored (string= %bidi-mirrored "Y")))
      (when char-from-name
        (assert (char= char char-from-name)))
      (when char-from-old-name
        (assert (char= char char-from-old-name)))
      (assert (eql gc (general-category char)))
      (assert (= (parse-integer ccc) (combining-class char)))
      (assert (eql bidi (bidi-class char)))
      (assert (eql decimal-digit (decimal-value char)))
      (assert (eql digit (digit-value char)))
      (assert (eql numeric (numeric-value char)))
      (assert (eql bidi-mirrored (mirrored-p char)))
      (assert (string= old-name (unicode-1-name char))))))

(defun test-property-reads ()
  (declare (optimize (debug 2)))
  (with-open-file (s (merge-pathnames
                      (make-pathname
                       :directory '(:relative :up "tools-for-build")
                       :name "UnicodeData" :type "txt")
                      (or *load-pathname* *compile-file-pathname*)))
    (with-test (:name (:unicode-properties))
      (loop for line = (read-line s nil nil)
            while line
            do (test-line line)))))

(test-property-reads)

(defun codepoint-or-range (string)
  (remove-if-not
   #'(lambda (i)
       #+sb-unicode i
       #-sb-unicode (< i 256))
   (flet ((parse (str) (parse-integer str :radix 16 :junk-allowed t)))
     (let ((parts (remove "" (split-string string #\.) :test #'string=)))
       (if (cdr parts)
           (loop for i from (parse (car parts)) to (parse (cadr parts)) collect i)
           (mapcar #'parse parts))))))

(defun test-property-line (fn line)
  (destructuring-bind (%codepoints value) (split-string line #\;)
    (let* ((codepoints (codepoint-or-range %codepoints))
           (property (remove #\Space value))
           (expected (intern
                      (string-upcase
                       (subseq property 0 (position #\# property)))
                      "KEYWORD")))

      (loop for i in codepoints do
           (unless (eql expected (funcall fn (code-char i)))
             (error "Character ~S has the wrong value for the tested property.
Wanted ~S, got ~S."
                    (code-char i) expected (funcall fn (code-char i))))))))

(defun test-bidi-class ()
  (declare (optimize (debug 2)))
  (with-open-file (s "data/DerivedBidiClass.txt" :external-format :ascii)
    (with-test (:name (:bidi-class))
      (loop for line = (read-line s nil nil)
            while line
            unless (or (string= "" line) (eql 0 (position #\# line)))
            do (test-property-line #'bidi-class line)))))

(test-bidi-class)

(defun test-hangul-syllable-type ()
  (declare (optimize (debug 2)))
  (with-open-file (s "data/HangulSyllableType.txt" :external-format :ascii)
    (with-test (:name (:hangul-syllable-type))
      (loop for line = (read-line s nil nil)
            while line
            unless (or (string= "" line) (eql 0 (position #\# line)))
            do (test-property-line #'hangul-syllable-type line)))))

(test-hangul-syllable-type)

(defun test-east-asian-width ()
  (declare (optimize (debug 2)))
  (with-open-file (s "../tools-for-build/EastAsianWidth.txt"
                     :external-format :ascii)
    (with-test (:name (:east-asian-width))
      (loop for line = (read-line s nil nil)
            while line
            unless (or (string= "" line) (eql 0 (position #\# line)))
            do (test-property-line #'east-asian-width line)))))

(test-east-asian-width)

(defun test-script ()
  (declare (optimize (debug 2)))
  (with-open-file (s "../tools-for-build/Scripts.txt"
                     :external-format :ascii)
    (with-test (:name (:script))
      (loop for line = (read-line s nil nil)
            while line
            unless (or (string= "" line) (eql 0 (position #\# line)))
            do (test-property-line #'script (substitute #\- #\_ line))))))

(test-script)

(defun test-block ()
  (declare (optimize (debug 2)))
  (with-open-file (s "../tools-for-build/Blocks.txt"
                     :external-format :ascii)
    (with-test (:name (:block))
      (loop for line = (read-line s nil nil)
            while line
            unless (or (string= "" line) (eql 0 (position #\# line)))
            do (test-property-line
                #'char-block
                (replace
                 (substitute #\- #\Space line)
                 "; "
                 :start1 (position #\; line)))))))

(test-block)

(defun test-proplist ()
  (declare (optimize (debug 2)))
  (with-open-file (s "../tools-for-build/PropList.txt"
                     :external-format :ascii)
    (with-test (:name (:proplist))
      (loop for line = (read-line s nil nil)
         while line
         unless (or (string= "" line) (eql 0 (position #\# line)))
         do
           (destructuring-bind (%codepoints value) (split-string line #\;)
                 (let* ((codepoints (codepoint-or-range %codepoints))
                        (property
                         (intern (string-upcase
                                  (substitute
                                   #\- #\_
                                   (subseq (remove #\Space value) 0
                                           (position #\# (remove #\Space value)))))
                          "KEYWORD")))
                   (loop for i in codepoints do
                        (unless (proplist-p (code-char i) property)
                          (error "Character ~S should be ~S, but isn't."
                                 (code-char i) property)))))))))

(test-proplist)

(defun test-bidi-mirroring-glyph ()
  (declare (optimize (debug 2)))
  (with-open-file (s "../tools-for-build/BidiMirroring.txt"
                     :external-format :ascii)
    (with-test (:name (:bidi-mirroring-glyph)
                :skipped-on (not :sb-unicode))
      (loop for line = (read-line s nil nil)
         while line
         unless (or (string= "" line) (eql 0 (position #\# line)))
         do
           (let* ((codepoints
                   (split-string (subseq line 0 (position #\# line)) #\;))
                  (chars
                   (mapcar
                    #'(lambda (s) (code-char (parse-integer s :radix 16)))
                    codepoints)))
             (unless (char= (bidi-mirroring-glyph (first chars)) (second chars))
               (error "The mirroring glyph of ~S is not ~S, but ~S"
                      (first chars) (second chars)
                      (bidi-mirroring-glyph (first chars)))))))))

(test-bidi-mirroring-glyph)

(defun test-age ()
  (declare (optimize (debug 2)))
  (with-open-file (s "../tools-for-build/DerivedAge.txt"
                     :external-format :ascii)
    (with-test (:name (:age))
      (loop for line = (read-line s nil nil)
         while line
         unless (or (string= "" line) (eql 0 (position #\# line)))
         do
           (destructuring-bind (%codepoints %age)
               (split-string (subseq line 0 (position #\# line)) #\;)
             (let* ((range (codepoint-or-range %codepoints))
                    (expected (mapcar #'parse-integer (split-string %age #\.))))
               (loop for i in range
                  for char = (code-char i)
                  do
                    (unless (equalp
                             expected
                             (multiple-value-list (age char)))
                      (error "Character ~S should have age ~S, but has ~S instead."
                             char expected (multiple-value-list (age char)))))))))))

(test-age)

(defun test-grapheme-break-class ()
  (declare (optimize (debug 2)))
  (with-open-file (s "data/GraphemeBreakProperty.txt"
                     :external-format :ascii)
    (with-test (:name (:grapheme-break-class))
      (loop for line = (read-line s nil nil)
            while line
            unless (or (string= "" line) (eql 0 (position #\# line)))
            do (test-property-line #'grapheme-break-class
                                   (replace-all "SpacingMark" "SPACING-MARK"
                                                (substitute #\- #\_ line)))))))

(test-grapheme-break-class)

(defun test-word-break-class ()
  (declare (optimize (debug 2)))
  (with-open-file (s "data/WordBreakProperty.txt"
                     :external-format :ascii)
    (with-test (:name (:word-break-class))
      (loop for line = (read-line s nil nil)
            while line
            unless (or (string= "" line) (eql 0 (position #\# line)))
            do (test-property-line #'word-break-class
                                   (substitute #\- #\_ line))))))

(test-word-break-class)

(defun test-sentence-break-class ()
  (declare (optimize (debug 2)))
  (with-open-file (s "data/SentenceBreakProperty.txt"
                     :external-format :ascii)
    (with-test (:name (:sentence-break-class))
      (loop for line = (read-line s nil nil)
            while line
            unless (or (string= "" line) (eql 0 (position #\# line)))
            do (test-property-line #'sentence-break-class line)))))

(test-sentence-break-class)

(defun test-line-break-class ()
  (declare (optimize (debug 2)))
  (with-open-file (s "../tools-for-build/LineBreak.txt"
                     :external-format :ascii)
    (with-test (:name (:line-break-class))
      (loop for line = (read-line s nil nil)
            while line
            unless (or (string= "" line) (eql 0 (position #\# line)))
            do (test-property-line #'line-break-class line)))))

(test-line-break-class)
