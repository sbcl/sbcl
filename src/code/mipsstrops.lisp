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

(in-package "SB-IMPL")

;;; Compare the substrings specified by STRING1 and STRING2 and return
;;; two values, the last index and the difference between the last characters
(defun %sp-string-compare (string1 start1 end1 string2 start2 end2)
  (let* ((end1 (or end1
                   (length string1)))
         (end2 (or end2
                   (length string2)))
         (len1 (- end1 start1))
         (len2 (- end2 start2)))
    (declare (fixnum len1 len2
                     end1 end2))
    (cond
      ((= len1 len2)
       (do ((index1 start1 (1+ index1))
            (index2 start2 (1+ index2)))
           ((= index1 end1) (values index1 0))
         (declare (fixnum index1 index2))
         (let ((char1 (schar string1 index1))
               (char2 (schar string2 index2)))
           (if (char/= char1 char2)
               (return (values index1
                               (- (char-code char1)
                                  (char-code char2))))))))
      ((> len1 len2)
       (do ((index1 start1 (1+ index1))
            (index2 start2 (1+ index2)))
           ((= index2 end2) (values index1 1))
         (declare (fixnum index1 index2))
         (let ((char1 (schar string1 index1))
               (char2 (schar string2 index2)))
           (if (char/= char1 char2)
               (return (values index1
                               (- (char-code char1)
                                  (char-code char2))))))))
      (t
       (do ((index1 start1 (1+ index1))
            (index2 start2 (1+ index2)))
           ((= index1 end1) (values index1 -1))
         (declare (fixnum index1 index2))
         (let ((char1 (schar string1 index1))
               (char2 (schar string2 index2)))
           (if (char/= char1 char2)
               (return (values index1
                               (- (char-code char1)
                                  (char-code char2)))))))))))
