;;;; string hacking functions that are stubs for things that might
;;;; be microcoded someday

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(file-comment
  "$Header$")

;(defun %sp-byte-blt (src-string src-start dst-string dst-start dst-end)
;  "Moves bytes from Src-String into Dst-String between Dst-Start (inclusive)
;and Dst-End (exclusive) (Dst-Start - Dst-End bytes are moved). Overlap of the
;strings does not affect the result. This would be done on the Vax
;with MOVC3. The arguments do not need to be strings: 8-bit U-Vectors
;are also acceptable."
;  (%primitive byte-blt src-string src-start dst-string dst-start dst-end))

(defun %sp-string-compare (string1 start1 end1 string2 start2 end2)
  (declare (simple-string string1 string2))
  (declare (fixnum start1 end1 start2 end2))
  #!+sb-doc
  "Compares the substrings specified by String1 and String2 and returns
NIL if the strings are String=, or the lowest index of String1 in
which the two differ. If one string is longer than the other and the
shorter is a prefix of the longer, the length of the shorter + start1 is
returned. This would be done on the Vax with CMPC3. The arguments must
be simple strings."
  (let ((len1 (- end1 start1))
	(len2 (- end2 start2)))
    (declare (fixnum len1 len2))
    (cond
     ((= len1 len2)
      (do ((index1 start1 (1+ index1))
	   (index2 start2 (1+ index2)))
	  ((= index1 end1) nil)
	(declare (fixnum index1 index2))
	(if (char/= (schar string1 index1) (schar string2 index2))
	    (return index1))))
     ((> len1 len2)
      (do ((index1 start1 (1+ index1))
	   (index2 start2 (1+ index2)))
	  ((= index2 end2) index1)
	(declare (fixnum index1 index2))
	(if (char/= (schar string1 index1) (schar string2 index2))
	    (return index1))))
     (t
      (do ((index1 start1 (1+ index1))
	   (index2 start2 (1+ index2)))
	  ((= index1 end1) index1)
	(declare (fixnum index1 index2))
	(if (char/= (schar string1 index1) (schar string2 index2))
	    (return index1)))))))

(defun %sp-reverse-string-compare (string1 start1 end1 string2 start2 end2)
  (declare (simple-string string1 string2))
  (declare (fixnum start1 end1 start2 end2))
  #!+sb-doc
  "like %SP-STRING-COMPARE, only backwards"
  (let ((len1 (- end1 start1))
	(len2 (- end2 start2)))
    (declare (fixnum len1 len2))
    (cond
     ((= len1 len2)
      (do ((index1 (1- end1) (1- index1))
	   (index2 (1- end2) (1- index2)))
	  ((< index1 start1) nil)
	(declare (fixnum index1 index2))
	(if (char/= (schar string1 index1) (schar string2 index2))
	    (return index1))))
     ((> len1 len2)
      (do ((index1 (1- end1) (1- index1))
	   (index2 (1- end2) (1- index2)))
	  ((< index2 start2) index1)
	(declare (fixnum index1 index2))
	(if (char/= (schar string1 index1) (schar string2 index2))
	    (return index1))))
     (t
      (do ((index1 (1- end1) (1- index1))
	   (index2 (1- end2) (1- index2)))
	  ((< index1 start1) index1)
	(declare (fixnum index1 index2))
	(if (char/= (schar string1 index1) (schar string2 index2))
	    (return index1)))))))

(defmacro maybe-sap-maybe-string ((var) &body body)
  `(etypecase ,var
     (system-area-pointer
      (macrolet ((byte-ref (index)
		   `(sap-ref-8 ,',var ,index))
		 (char-ref (index)
		   `(code-char (byte-ref ,index))))
	,@body))
     (simple-string
      (macrolet ((char-ref (index)
		   `(schar ,',var ,index))
		 (byte-ref (index)
		   `(char-code (char-ref ,index))))
	,@body))))

(defun %sp-find-character-with-attribute (string start end table mask)
  (declare (type (simple-array (unsigned-byte 8) (256)) table)
	   (type (or simple-string system-area-pointer) string)
	   (fixnum start end mask))
  #!+sb-doc
  "%SP-Find-Character-With-Attribute  String, Start, End, Table, Mask
  The codes of the characters of String from Start to End are used as indices
  into the Table, which is a U-Vector of 8-bit bytes. When the number picked
  up from the table bitwise ANDed with Mask is non-zero, the current
  index into the String is returned. The corresponds to SCANC on the Vax."
  (maybe-sap-maybe-string (string)
    (do ((index start (1+ index)))
	((>= index end) nil)
      (declare (fixnum index))
      (unless (zerop (logand (aref table (byte-ref index)) mask))
	(return index)))))

(defun %sp-reverse-find-character-with-attribute (string start end table mask)
  #!+sb-doc
  "Like %SP-Find-Character-With-Attribute, only sdrawkcaB."
  (declare (type (or simple-string system-area-pointer) string)
	   (fixnum start end mask)
	   (type (array (unsigned-byte 8) (256)) table))
  (maybe-sap-maybe-string (string)
    (do ((index (1- end) (1- index)))
	((< index start) nil)
      (declare (fixnum index))
      (unless (zerop (logand (aref table (byte-ref index)) mask))
	(return index)))))

(defun %sp-find-character (string start end character)
  #!+sb-doc
  "%SP-Find-Character  String, Start, End, Character
  Searches String for the Character from Start to End. If the character is
  found, the corresponding index into String is returned, otherwise NIL is
  returned."
  (declare (fixnum start end)
	   (type (or simple-string system-area-pointer) string)
	   (base-char character))
  (maybe-sap-maybe-string (string)
    (do ((index start (1+ index)))
	((>= index end) nil)
      (declare (fixnum index))
      (when (char= (char-ref index) character)
	(return index)))))

(defun %sp-reverse-find-character (string start end character)
  (declare (type (or simple-string system-area-pointer) string)
	   (fixnum start end)
	   (base-char character))
  #!+sb-doc
  "%SP-Reverse-Find-Character  String, Start, End, Character
  Searches String for Character from End to Start. If the character is
  found, the corresponding index into String is returned, otherwise NIL is
  returned."
  (maybe-sap-maybe-string (string)
    (do ((index (1- end) (1- index))
	 (terminus (1- start)))
	((= index terminus) nil)
      (declare (fixnum terminus index))
      (if (char= (char-ref index) character)
	  (return index)))))

(defun %sp-skip-character (string start end character)
  (declare (type (or simple-string system-area-pointer) string)
	   (fixnum start end)
	   (base-char character))
  #!+sb-doc
  "%SP-Skip-Character  String, Start, End, Character
  Returns the index of the first character between Start and End which
  is not Char=  to Character, or NIL if there is no such character."
  (maybe-sap-maybe-string (string)
    (do ((index start (1+ index)))
	((= index end) nil)
      (declare (fixnum index))
      (if (char/= (char-ref index) character)
	  (return index)))))

(defun %sp-reverse-skip-character (string start end character)
  (declare (type (or simple-string system-area-pointer) string)
	   (fixnum start end)
	   (base-char character))
  #!+sb-doc
  "%SP-Skip-Character  String, Start, End, Character
  Returns the index of the last character between Start and End which
  is not Char=  to Character, or NIL if there is no such character."
  (maybe-sap-maybe-string (string)
    (do ((index (1- end) (1- index))
	 (terminus (1- start)))
	((= index terminus) nil)
      (declare (fixnum terminus index))
      (if (char/= (char-ref index) character)
	  (return index)))))

(defun %sp-string-search (string1 start1 end1 string2 start2 end2)
  #!+sb-doc
  "%SP-String-Search  String1, Start1, End1, String2, Start2, End2
   Searches for the substring of String1 specified in String2.
   Returns an index into String2 or NIL if the substring wasn't
   found."
  (declare (simple-string string1 string2))
  (do ((index2 start2 (1+ index2)))
      ((= index2 end2) nil)
    (declare (fixnum index2))
    (when (do ((index1 start1 (1+ index1))
	       (index2 index2 (1+ index2)))
	      ((= index1 end1) t)
	    (declare (fixnum index1 index2))
	    (when (= index2 end2)
	      (return-from %sp-string-search nil))
	    (when (char/= (char string1 index1) (char string2 index2))
	      (return nil)))
      (return index2))))
