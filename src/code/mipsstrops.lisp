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

;;; Compare the substrings specified by STRING1 and STRING2 and return
;;; NIL if the strings are STRING=, or the lowest index of STRING1 in
;;; which the two differ. If one string is longer than the other and
;;; the shorter is a prefix of the longer, the length of the shorter +
;;; START1 is returned. The arguments must be simple strings.
;;;
;;; This would be done on the Vax with CMPC3. 
(defun %sp-string-compare (string1 start1 end1 string2 start2 end2)
  (declare (simple-string string1 string2))
  (declare (fixnum start1 end1 start2 end2))
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

;;; like %SP-STRING-COMPARE, only backwards
(defun %sp-reverse-string-compare (string1 start1 end1 string2 start2 end2)
  (declare (simple-string string1 string2))
  (declare (fixnum start1 end1 start2 end2))
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

;;; Search STRING for the CHARACTER from START to END. If the
;;; character is found, the corresponding index into STRING is
;;; returned, otherwise NIL is returned.
(defun %sp-find-character (string start end character)
  (declare (fixnum start end)
	   (type (or simple-string system-area-pointer) string)
	   (base-char character))
  (maybe-sap-maybe-string (string)
    (do ((index start (1+ index)))
	((>= index end) nil)
      (declare (fixnum index))
      (when (char= (char-ref index) character)
	(return index)))))

;;; Search STRING for CHARACTER from END to START. If the character is
;;; found, the corresponding index into STRING is returned, otherwise
;;; NIL is returned.
(defun %sp-reverse-find-character (string start end character)
  (declare (type (or simple-string system-area-pointer) string)
	   (fixnum start end)
	   (base-char character))
  (maybe-sap-maybe-string (string)
    (do ((index (1- end) (1- index))
	 (terminus (1- start)))
	((= index terminus) nil)
      (declare (fixnum terminus index))
      (if (char= (char-ref index) character)
	  (return index)))))

;;; Return the index of the first character between START and END
;;; which is not CHAR= to CHARACTER, or NIL if there is no such
;;; character.
(defun %sp-skip-character (string start end character)
  (declare (type (or simple-string system-area-pointer) string)
	   (fixnum start end)
	   (base-char character))
  (maybe-sap-maybe-string (string)
    (do ((index start (1+ index)))
	((= index end) nil)
      (declare (fixnum index))
      (if (char/= (char-ref index) character)
	  (return index)))))

;;; Return the index of the last character between START and END which
;;; is not CHAR= to CHARACTER, or NIL if there is no such character.
(defun %sp-reverse-skip-character (string start end character)
  (declare (type (or simple-string system-area-pointer) string)
	   (fixnum start end)
	   (base-char character))
  (maybe-sap-maybe-string (string)
    (do ((index (1- end) (1- index))
	 (terminus (1- start)))
	((= index terminus) nil)
      (declare (fixnum terminus index))
      (if (char/= (char-ref index) character)
	  (return index)))))

;;; Search for the substring of STRING1 specified in STRING2. Return
;;; an index into STRING2, or NIL if the substring wasn't found.
(defun %sp-string-search (string1 start1 end1 string2 start2 end2)
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
