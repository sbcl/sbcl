;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(eval-when (:compile-toplevel)
  (sb!xc:defmacro %string (x) `(if (stringp ,x) ,x (string ,x))))

(defun string (x)
  #!+sb-doc
  "Coerces X into a string. If X is a string, X is returned. If X is a
   symbol, X's pname is returned. If X is a character then a one element
   string containing that character is returned. If X cannot be coerced
   into a string, an error occurs."
  (cond ((stringp x) x)
        ((symbolp x) (symbol-name x))
        ((characterp x)
         (let ((res (make-string 1)))
           (setf (schar res 0) x) res))
        (t
         (error 'simple-type-error
                :datum x
                :expected-type 'string-designator
                :format-control "~S cannot be coerced to a string."
                :format-arguments (list x)))))

;;; %CHECK-VECTOR-SEQUENCE-BOUNDS is used to verify that the START and
;;; END arguments are valid bounding indices.
(defun %check-vector-sequence-bounds (vector start end)
  (%check-vector-sequence-bounds vector start end))

(eval-when (:compile-toplevel)
;;; WITH-ONE-STRING is used to set up some string hacking things. The
;;; keywords are parsed, and the string is hacked into a
;;; simple-string.
(sb!xc:defmacro with-one-string ((string start end) &body forms)
  `(let ((,string (%string ,string)))
     (with-array-data ((,string ,string)
                       (,start ,start)
                       (,end ,end)
                       :check-fill-pointer t)
       ,@forms)))
;;; WITH-TWO-STRINGS is used to set up string comparison operations. The
;;; keywords are parsed, and the strings are hacked into SIMPLE-STRINGs.
(sb!xc:defmacro with-two-strings (string1 string2 start1 end1 cum-offset-1
                                            start2 end2 &rest forms)
  `(let ((,string1 (%string ,string1))
         (,string2 (%string ,string2)))
     (with-array-data ((,string1 ,string1 :offset-var ,cum-offset-1)
                       (,start1 ,start1)
                       (,end1 ,end1)
                       :check-fill-pointer t)
       (with-array-data ((,string2 ,string2)
                         (,start2 ,start2)
                         (,end2 ,end2)
                         :check-fill-pointer t)
         ,@forms))))
) ; EVAL-WHEN

(defun char (string index)
  #!+sb-doc
  "Given a string and a non-negative integer index less than the length of
  the string, returns the character object representing the character at
  that position in the string."
  (declare (optimize (safety 1)))
  (char string index))

(defun %charset (string index new-el)
  (declare (optimize (safety 1)))
  (setf (char string index) new-el))

(defun schar (string index)
  #!+sb-doc
  "SCHAR returns the character object at an indexed position in a string
   just as CHAR does, except the string must be a simple-string."
  (declare (optimize (safety 1)))
  (schar string index))

(defun %scharset (string index new-el)
  (declare (optimize (safety 1)))
  (setf (schar string index) new-el))

(defun string=* (string1 string2 start1 end1 start2 end2)
  (with-two-strings string1 string2 start1 end1 nil start2 end2
    (not (%sp-string-compare string1 start1 end1 string2 start2 end2))))

(defun string/=* (string1 string2 start1 end1 start2 end2)
  (with-two-strings string1 string2 start1 end1 offset1 start2 end2
    (let ((comparison (%sp-string-compare string1 start1 end1
                                          string2 start2 end2)))
      (if comparison (- (the fixnum comparison) offset1)))))

(eval-when (:compile-toplevel :execute)

;;; LESSP is true if the desired expansion is for STRING<* or STRING<=*.
;;; EQUALP is true if the desired expansion is for STRING<=* or STRING>=*.
(sb!xc:defmacro string<>=*-body (lessp equalp)
  (let ((offset1 (gensym)))
    `(with-two-strings string1 string2 start1 end1 ,offset1 start2 end2
       (let ((index (%sp-string-compare string1 start1 end1
                                        string2 start2 end2)))
         (if index
             (cond ((= (the fixnum index) (the fixnum end1))
                    ,(if lessp
                         `(- (the fixnum index) ,offset1)
                       `nil))
                   ((= (+ (the fixnum index) (- start2 start1))
                       (the fixnum end2))
                    ,(if lessp
                         `nil
                       `(- (the fixnum index) ,offset1)))
                   ((,(if lessp 'char< 'char>)
                     (schar string1 index)
                     (schar string2 (+ (the fixnum index) (- start2 start1))))
                    (- (the fixnum index) ,offset1))
                   (t nil))
             ,(if equalp `(- (the fixnum end1) ,offset1) nil))))))
) ; EVAL-WHEN

(defun string<* (string1 string2 start1 end1 start2 end2)
  (declare (fixnum start1 start2))
  (string<>=*-body t nil))

(defun string>* (string1 string2 start1 end1 start2 end2)
  (declare (fixnum start1 start2))
  (string<>=*-body nil nil))

(defun string<=* (string1 string2 start1 end1 start2 end2)
  (declare (fixnum start1 start2))
  (string<>=*-body t t))

(defun string>=* (string1 string2 start1 end1 start2 end2)
  (declare (fixnum start1 start2))
  (string<>=*-body nil t))

(defun string< (string1 string2 &key (start1 0) end1 (start2 0) end2)
  #!+sb-doc
  "Given two strings, if the first string is lexicographically less than
  the second string, returns the longest common prefix (using char=)
  of the two strings. Otherwise, returns ()."
  (string<* string1 string2 start1 end1 start2 end2))

(defun string> (string1 string2 &key (start1 0) end1 (start2 0) end2)
  #!+sb-doc
  "Given two strings, if the first string is lexicographically greater than
  the second string, returns the longest common prefix (using char=)
  of the two strings. Otherwise, returns ()."
  (string>* string1 string2 start1 end1 start2 end2))

(defun string<= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  #!+sb-doc
  "Given two strings, if the first string is lexicographically less than
  or equal to the second string, returns the longest common prefix
  (using char=) of the two strings. Otherwise, returns ()."
  (string<=* string1 string2 start1 end1 start2 end2))

(defun string>= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Given two strings, if the first string is lexicographically greater
  than or equal to the second string, returns the longest common prefix
  (using char=) of the two strings. Otherwise, returns ()."
  (string>=* string1 string2 start1 end1 start2 end2))

;;; Note: (STRING= "PREFIX" "SHORT" :END2 (LENGTH "PREFIX")) gives
;;; an error instead of returning NIL as I would have expected.
;;; The ANSI spec for STRING= itself doesn't seem to clarify this
;;; much, but the SUBSEQ-OUT-OF-BOUNDS writeup seems to say that
;;; this is conforming (and required) behavior, because any index
;;; out of range is an error. (So there seems to be no concise and
;;; efficient way to test for strings which begin with a particular
;;; pattern. Alas..) -- WHN 19991206
(defun string= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  #!+sb-doc
  "Given two strings (string1 and string2), and optional integers start1,
  start2, end1 and end2, compares characters in string1 to characters in
  string2 (using char=)."
  (string=* string1 string2 start1 end1 start2 end2))

(defun string/= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  #!+sb-doc
  "Given two strings, if the first string is not lexicographically equal
  to the second string, returns the longest common prefix (using char=)
  of the two strings. Otherwise, returns ()."
  (string/=* string1 string2 start1 end1 start2 end2))

(eval-when (:compile-toplevel :execute)

;;; STRING-NOT-EQUAL-LOOP is used to generate character comparison loops for
;;; STRING-EQUAL and STRING-NOT-EQUAL.
(sb!xc:defmacro string-not-equal-loop (end
                                         end-value
                                         &optional (abort-value nil abortp))
  (declare (fixnum end))
  (let ((end-test (if (= end 1)
                      `(= index1 (the fixnum end1))
                      `(= index2 (the fixnum end2)))))
    `(do ((index1 start1 (1+ index1))
          (index2 start2 (1+ index2)))
         (,(if abortp
               end-test
               `(or ,end-test
                    (not (char-equal (schar string1 index1)
                                     (schar string2 index2)))))
          ,end-value)
       (declare (fixnum index1 index2))
       ,@(if abortp
             `((if (not (char-equal (schar string1 index1)
                                    (schar string2 index2)))
                   (return ,abort-value)))))))

) ; EVAL-WHEN

(defun string-equal (string1 string2 &key (start1 0) end1 (start2 0) end2)
  #!+sb-doc
  "Given two strings (string1 and string2), and optional integers start1,
  start2, end1 and end2, compares characters in string1 to characters in
  string2 (using char-equal)."
  (declare (fixnum start1 start2))
  (with-two-strings string1 string2 start1 end1 nil start2 end2
    (let ((slen1 (- (the fixnum end1) start1))
          (slen2 (- (the fixnum end2) start2)))
      (declare (fixnum slen1 slen2))
      (if (= slen1 slen2)
          ;;return () immediately if lengths aren't equal.
          (string-not-equal-loop 1 t nil)))))

(defun string-not-equal (string1 string2 &key (start1 0) end1 (start2 0) end2)
  #!+sb-doc
  "Given two strings, if the first string is not lexicographically equal
  to the second string, returns the longest common prefix (using char-equal)
  of the two strings. Otherwise, returns ()."
  (with-two-strings string1 string2 start1 end1 offset1 start2 end2
    (let ((slen1 (- end1 start1))
          (slen2 (- end2 start2)))
      (declare (fixnum slen1 slen2))
      (cond ((= slen1 slen2)
             (string-not-equal-loop 1 nil (- index1 offset1)))
            ((< slen1 slen2)
             (string-not-equal-loop 1 (- index1 offset1)))
            (t
             (string-not-equal-loop 2 (- index1 offset1)))))))

(eval-when (:compile-toplevel :execute)

;;; STRING-LESS-GREATER-EQUAL-TESTS returns a test on the lengths of string1
;;; and string2 and a test on the current characters from string1 and string2
;;; for the following macro.
(defun string-less-greater-equal-tests (lessp equalp)
  (if lessp
      (if equalp
          ;; STRING-NOT-GREATERP
          (values '<= `(not (char-greaterp char1 char2)))
          ;; STRING-LESSP
          (values '< `(char-lessp char1 char2)))
      (if equalp
          ;; STRING-NOT-LESSP
          (values '>= `(not (char-lessp char1 char2)))
          ;; STRING-GREATERP
          (values '> `(char-greaterp char1 char2)))))

(sb!xc:defmacro string-less-greater-equal (lessp equalp)
  (multiple-value-bind (length-test character-test)
      (string-less-greater-equal-tests lessp equalp)
    `(with-two-strings string1 string2 start1 end1 offset1 start2 end2
       (let ((slen1 (- (the fixnum end1) start1))
             (slen2 (- (the fixnum end2) start2)))
         (declare (fixnum slen1 slen2))
         (do ((index1 start1 (1+ index1))
              (index2 start2 (1+ index2))
              (char1)
              (char2))
             ((or (= index1 (the fixnum end1)) (= index2 (the fixnum end2)))
              (if (,length-test slen1 slen2) (- index1 offset1)))
           (declare (fixnum index1 index2))
           (setq char1 (schar string1 index1))
           (setq char2 (schar string2 index2))
           (if (not (char-equal char1 char2))
               (if ,character-test
                   (return (- index1 offset1))
                   (return ()))))))))

) ; EVAL-WHEN

(defun string-lessp* (string1 string2 start1 end1 start2 end2)
  (declare (fixnum start1 start2))
  (string-less-greater-equal t nil))

(defun string-greaterp* (string1 string2 start1 end1 start2 end2)
  (declare (fixnum start1 start2))
  (string-less-greater-equal nil nil))

(defun string-not-lessp* (string1 string2 start1 end1 start2 end2)
  (declare (fixnum start1 start2))
  (string-less-greater-equal nil t))

(defun string-not-greaterp* (string1 string2 start1 end1 start2 end2)
  (declare (fixnum start1 start2))
  (string-less-greater-equal t t))

(defun string-lessp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  #!+sb-doc
  "Given two strings, if the first string is lexicographically less than
  the second string, returns the longest common prefix (using char-equal)
  of the two strings. Otherwise, returns ()."
  (string-lessp* string1 string2 start1 end1 start2 end2))

(defun string-greaterp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  #!+sb-doc
  "Given two strings, if the first string is lexicographically greater than
  the second string, returns the longest common prefix (using char-equal)
  of the two strings. Otherwise, returns ()."
  (string-greaterp* string1 string2 start1 end1 start2 end2))

(defun string-not-lessp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  #!+sb-doc
  "Given two strings, if the first string is lexicographically greater
  than or equal to the second string, returns the longest common prefix
  (using char-equal) of the two strings. Otherwise, returns ()."
  (string-not-lessp* string1 string2 start1 end1 start2 end2))

(defun string-not-greaterp (string1 string2 &key (start1 0) end1 (start2 0)
                                    end2)
  #!+sb-doc
  "Given two strings, if the first string is lexicographically less than
  or equal to the second string, returns the longest common prefix
  (using char-equal) of the two strings. Otherwise, returns ()."
  (string-not-greaterp* string1 string2 start1 end1 start2 end2))

(defun make-string (count &key
                    (element-type 'character)
                    ((:initial-element fill-char)))
  #!+sb-doc
  "Given a character count and an optional fill character, makes and returns a
new string COUNT long filled with the fill character."
  (declare (fixnum count))
  (if fill-char
      (make-string count :element-type element-type :initial-element fill-char)
      (make-string count :element-type element-type)))

(flet ((%upcase (string start end)
         (declare (string string) (index start) (type sequence-end end))
         (let ((saved-header string))
           (with-one-string (string start end)
             (do ((index start (1+ index)))
                 ((= index (the fixnum end)))
               (declare (fixnum index))
               (setf (schar string index) (char-upcase (schar string index)))))
           saved-header)))
(defun string-upcase (string &key (start 0) end)
  (%upcase (copy-seq (string string)) start end))
(defun nstring-upcase (string &key (start 0) end)
  (%upcase string start end))
) ; FLET

(flet ((%downcase (string start end)
         (declare (string string) (index start) (type sequence-end end))
         (let ((saved-header string))
           (with-one-string (string start end)
             (do ((index start (1+ index)))
                 ((= index (the fixnum end)))
               (declare (fixnum index))
               (setf (schar string index)
                     (char-downcase (schar string index)))))
           saved-header)))
(defun string-downcase (string &key (start 0) end)
  (%downcase (copy-seq (string string)) start end))
(defun nstring-downcase (string &key (start 0) end)
  (%downcase string start end))
) ; FLET

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

(defun generic-string-trim (char-bag string left-p right-p)
  (let ((header (%string string)))
    (with-array-data ((string header)
                      (start)
                      (end)
                      :check-fill-pointer t)
      (let* ((left-end (if left-p
                           (do ((index start (1+ index)))
                               ((or (= index (the fixnum end))
                                    (not (find (schar string index)
                                               char-bag
                                               :test #'char=)))
                                index)
                             (declare (fixnum index)))
                           start))
             (right-end (if right-p
                            (do ((index (1- (the fixnum end)) (1- index)))
                                ((or (< index left-end)
                                     (not (find (schar string index)
                                                char-bag
                                                :test #'char=)))
                                 (1+ index))
                              (declare (fixnum index)))
                            end)))
        (if (and (eql left-end start)
                 (eql right-end end))
            header
            (subseq (the simple-string string) left-end right-end))))))

(defun string-left-trim (char-bag string)
  (generic-string-trim char-bag string t nil))

(defun string-right-trim (char-bag string)
  (generic-string-trim char-bag string nil t))

(defun string-trim (char-bag string)
  (generic-string-trim char-bag string t t))
