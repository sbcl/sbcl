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
