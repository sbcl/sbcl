;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

(defmacro %string (x) `(if (stringp ,x) ,x (string ,x)))

(defun string (x)
  "Coerces X into a string. If X is a string, X is returned. If X is a
   symbol, its name is returned. If X is a character then a one element
   string containing that character is returned. If X cannot be coerced
   into a string, an error occurs."
  (declare (explicit-check))
  (cond ((stringp x) x)
        ((symbolp x) (symbol-name x))
        ((characterp x)
         (let ((res (make-string 1)))
           (setf (schar res 0) x) res))
        (t
         (error 'simple-type-error
                :datum x
                :expected-type 'string-designator
                :format-control "~S is not a string designator."
                :format-arguments (list x)))))

;;; %CHECK-VECTOR-SEQUENCE-BOUNDS is used to verify that the START and
;;; END arguments are valid bounding indices.
(defun %check-vector-sequence-bounds (vector start end)
  (%check-vector-sequence-bounds vector start end))

;;; WITH-ONE-STRING is used to set up some string hacking things. The
;;; keywords are parsed, and the string is hacked into a
;;; simple-string.
(defmacro with-one-string ((string start end) &body forms)
  `(let ((,string (%string ,string)))
     (with-array-data ((,string ,string)
                       (,start ,start)
                       (,end ,end)
                       :check-fill-pointer t)
       ,@forms)))
;;; WITH-TWO-STRINGS is used to set up string comparison operations. The
;;; keywords are parsed, and the strings are hacked into SIMPLE-STRINGs.
(defmacro with-two-strings (string1 string2 start1 end1 cum-offset-1
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

(defmacro with-two-arg-strings (string1 string2 start1 end1 cum-offset-1
                                      start2 end2 &rest forms)
  `(let ((,string1 (%string ,string1))
         (,string2 (%string ,string2)))
     (with-array-data ((,string1 ,string1 :offset-var ,cum-offset-1)
                       (,start1)
                       (,end1)
                       :check-fill-pointer t)
       (with-array-data ((,string2 ,string2)
                         (,start2)
                         (,end2)
                         :check-fill-pointer t)
         ,@forms))))

(defun char (string index)
  "Given a string and a non-negative integer index less than the length of
  the string, returns the character object representing the character at
  that position in the string."
  (declare (optimize (safety 1)))
  (char string index))

(defun %charset (string index new-el)
  (declare (optimize (safety 1)))
  (setf (char string index) new-el))

(defun schar (string index)
  "SCHAR returns the character object at an indexed position in a string
   just as CHAR does, except the string must be a simple-string."
  (declare (optimize (safety 1)))
  (schar string index))

(defun %scharset (string index new-el)
  (declare (optimize (safety 1)))
  (setf (schar string index) new-el))

(macrolet ((memcmp (shift)
             ;; Efficiently compute byte indices. Derive-type on ASH isn't
             ;; good enough. For 32-bit, it should be ok because
             ;; (TYPEP (ASH ARRAY-TOTAL-SIZE-LIMIT 2) 'SB-VM:SIGNED-WORD) => T
             ;; For 63-bit fixnums, that's false in theory, but true in practice.
             ;; ARRAY-TOTAL-SIZE-LIMIT is too large for a 48-bit address space.
             `(macrolet ((sap (string start)
                           `(sap+ (vector-sap (truly-the string ,string))
                                  (scale ,start)))
                         (scale (index)
                           `(truly-the sb-vm:signed-word
                                       (ash (truly-the index ,index) ,',shift))))
                (declare (optimize (sb-c:alien-funcall-saves-fp-and-pc 0)))
                (with-pinned-objects (string1 string2)
                  (zerop (alien-funcall
                          (extern-alien "memcmp"
                                        (function int (* char) (* char) long))
                          (sap string1 start1) (sap string2 start2)
                          (scale len))))))
           (char-loop (type1 type2)
             `(let ((string1 (truly-the (simple-array ,type1 1) string1))
                    (string2 (truly-the (simple-array ,type2 1) string2)))
                (declare (optimize (sb-c:insert-array-bounds-checks 0)))
                (do ((index1 start1 (1+ index1))
                     (index2 start2 (1+ index2)))
                    ((>= index1 end1) t)
                  (declare (index index1 index2))
                  (unless (char= (schar string1 index1)
                                 (schar string2 index2))
                    (return nil))))))

  (defun string=* (string1 string2 start1 end1 start2 end2)
    (declare (explicit-check))
    #+(or arm64 (and x86-64 sb-unicode))
    (when (and (eql start1 0)
               (eql start2 0)
               (not end1)
               (not end2))
      ;; With the range spanning the whole string it can be compared
      ;; 128 bits at a time without the need to process any leftover
      ;; bits.
      (prog ((string1 string1)
             (string2 string2))
         (cond ((simple-base-string-p string1)
                (cond #+sb-unicode
                      ((simple-character-string-p string2)
                       (go 8-32))
                      #+arm64
                      ((simple-base-string-p string2)
                       (go 8-8))
                      (t
                       (go normal))))
               #+sb-unicode
               ((simple-character-string-p string1)
                (cond ((simple-base-string-p string2)
                       (rotatef string1 string2)
                       (go 8-32))
                      #+arm64
                      ((simple-character-string-p string2)
                       (go 32-32))
                      (t
                       (go normal))))
               (t
                (go normal)))
       8-32
         (return-from string=*
           (let ((length1 (length string1)))
             (and (= length1
                     (length string2))
                  (sb-vm::simd-cmp-8-32 string1 string2 length1))))
       8-8
         #+arm64
         (return-from string=*
           (let ((length1 (length string1)))
             (and (= length1
                     (length string2))
                  (sb-vm::simd-cmp-8-8 string1 string2 length1))))

       32-32
         #+arm64
         (return-from string=*
           (let ((length1 (length string1)))
             (and (= length1
                     (length string2))
                  (sb-vm::simd-cmp-32-32 string1 string2 length1))))
       normal))
    (with-two-strings string1 string2 start1 end1 nil start2 end2
      (let ((len (- end1 start1)))
        (unless (= len (- end2 start2)) ; trivial
          (return-from string=* nil))
        ;; Optimizing the non-unicode builds is not terribly important
        ;; because no per-character test for base/UCS4 is needed.
        #+sb-unicode
        (let* ((widetag1 (%other-pointer-widetag string1))
               (widetag2 (%other-pointer-widetag string2)))
          (macrolet ((char-loop (type1 type2)
                       `(return-from string=*
                          (let ((string1 (truly-the (simple-array ,type1 1) string1))
                                (string2 (truly-the (simple-array ,type2 1) string2)))
                            (declare (optimize (sb-c:insert-array-bounds-checks 0)))
                            (do ((index1 start1 (1+ index1))
                                 (index2 start2 (1+ index2)))
                                ((>= index1 end1) t)
                              (declare (index index1 index2))
                              (unless (char= (schar string1 index1)
                                             (schar string2 index2))
                                (return nil)))))))
            (cond #+(or x86 x86-64)
                  ;; The cost of WITH-PINNED-OBJECTS is near nothing on x86,
                  ;; and memcmp() is much faster except below a cutoff point.
                  ;; The threshold is higher on x86-32 because the overhead
                  ;; of a foreign call is higher due to FPU stack save/restore.
                  ((and (= widetag1 widetag2)
                        (>= len #+x86 16
                                #+x86-64 8))
                   (let ((shift (case widetag1
                                  (#.sb-vm:simple-base-string-widetag 0)
                                  (#.sb-vm:simple-character-string-widetag 2))))
                     (memcmp shift)))
                  ((= widetag1 widetag2)
                   (case widetag1
                     (#.sb-vm:simple-base-string-widetag
                      (char-loop base-char base-char))
                     (#.sb-vm:simple-character-string-widetag
                      (char-loop character character))))
                  ((or (and (= widetag1 sb-vm:simple-character-string-widetag)
                            (= widetag2 sb-vm:simple-base-string-widetag))
                       (and (= widetag2 sb-vm:simple-character-string-widetag)
                            (= widetag1 sb-vm:simple-base-string-widetag)
                            (progn (rotatef start1 start2)
                                   (rotatef end1 end2)
                                   (rotatef string1 string2)
                                   t)))
                   (char-loop character base-char))))))
      #-sb-unicode
      (%sp-string= string1 string2 start1 end1 start2 end2)))

  (defun simple-base-string= (string1 string2 start1 end1 start2 end2)
    #+arm64
    (when (and (zerop start1)
               (zerop start2)
               (not end1)
               (not end2))
      (return-from simple-base-string=
        (let ((length1 (length string1)))
          (and (= length1
                  (length string2))
               (sb-vm::simd-cmp-8-8 string1 string2 length1)))))
    (with-two-strings string1 string2 start1 end1 nil start2 end2
      (let ((len (- end1 start1)))
        (cond ((/= len (- end2 start2))
               nil)
              #+(or x86 x86-64)
              ((>= len #+x86 16
                       #+x86-64 8)
               (memcmp 0))
              (t
               (char-loop base-char base-char))))))

  #+sb-unicode
  (defun simple-character-string= (string1 string2 start1 end1 start2 end2)
    #+arm64
    (when (and (eql start1 0)
               (eql start2 0)
               (not end1)
               (not end2))
      (return-from simple-character-string=
        (let ((length1 (length string1)))
          (and (= length1
                  (length string2))
               (sb-vm::simd-cmp-32-32 string1 string2 length1)))))
    (with-two-strings string1 string2 start1 end1 nil start2 end2
      (let ((len (- end1 start1)))
        (cond ((/= len (- end2 start2))
               nil)
              #+(or x86 x86-64)
              ((>= len #+x86 16
                       #+x86-64 8)
               (memcmp 2))
              (t
               (char-loop character character)))))))

(defun string/=* (string1 string2 start1 end1 start2 end2)
  (with-two-strings string1 string2 start1 end1 offset1 start2 end2
    (multiple-value-bind (index diff)
        (%sp-string-compare string1 string2 start1 end1 start2 end2)
      (if (zerop diff)
          nil
          (- index offset1)))))

(defmacro string<>=*-body (test index)
  `(with-two-strings string1 string2 start1 end1 offset1 start2 end2
     (multiple-value-bind (index diff)
         (%sp-string-compare string1 string2
                             start1 end1 start2 end2)
       (if (,test diff 0)
           ,(if index '(- index offset1) nil)
           ,(if index nil '(- index offset1))))))

(defun string<* (string1 string2 start1 end1 start2 end2)
  (string<>=*-body < t))

(defun string>* (string1 string2 start1 end1 start2 end2)
  (string<>=*-body > t))

(defun string<=* (string1 string2 start1 end1 start2 end2)
  (string<>=*-body > nil))

(defun string>=* (string1 string2 start1 end1 start2 end2)
  (declare (fixnum start1 start2))
  (string<>=*-body < nil))

(defun string< (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Given two strings, if the first string is lexicographically less than
  the second string, returns the longest common prefix (using char=)
  of the two strings. Otherwise, returns ()."
  (string<* string1 string2 start1 end1 start2 end2))

(defun two-arg-string< (string1 string2)
  (string<* string1 string2 0 nil 0 nil))

(defun string> (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Given two strings, if the first string is lexicographically greater than
  the second string, returns the longest common prefix (using char=)
  of the two strings. Otherwise, returns ()."
  (string>* string1 string2 start1 end1 start2 end2))

(defun two-arg-string> (string1 string2)
  (string>* string1 string2 0 nil 0 nil))

(defun string<= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Given two strings, if the first string is lexicographically less than
  or equal to the second string, returns the longest common prefix
  (using char=) of the two strings. Otherwise, returns ()."
  (string<=* string1 string2 start1 end1 start2 end2))

(defun two-arg-string<= (string1 string2)
  (string<=* string1 string2 0 nil 0 nil))

(defun string>= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Given two strings, if the first string is lexicographically greater
  than or equal to the second string, returns the longest common prefix
  (using char=) of the two strings. Otherwise, returns ()."
  (string>=* string1 string2 start1 end1 start2 end2))

(defun two-arg-string>= (string1 string2)
  (string>=* string1 string2 0 nil 0 nil))

;;; Note: (STRING= "PREFIX" "SHORT" :END2 (LENGTH "PREFIX")) gives
;;; an error instead of returning NIL as I would have expected.
;;; The ANSI spec for STRING= itself doesn't seem to clarify this
;;; much, but the SUBSEQ-OUT-OF-BOUNDS writeup seems to say that
;;; this is conforming (and required) behavior, because any index
;;; out of range is an error. (So there seems to be no concise and
;;; efficient way to test for strings which begin with a particular
;;; pattern. Alas..) -- WHN 19991206
(defun string= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Given two strings (string1 and string2), and optional integers start1,
  start2, end1 and end2, compares characters in string1 to characters in
  string2 (using char=)."
  (string=* string1 string2 start1 end1 start2 end2))

(defun two-arg-string= (string1 string2)
  (string=* string1 string2 0 nil 0 nil))

(defun string/= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Given two strings, if the first string is not lexicographically equal
  to the second string, returns the longest common prefix (using char=)
  of the two strings. Otherwise, returns ()."
  (string/=* string1 string2 start1 end1 start2 end2))

(defun two-arg-string/= (string1 string2)
  (string/=* string1 string2 0 nil 0 nil))

;;; STRING-NOT-EQUAL-LOOP is used to generate character comparison loops for
;;; STRING-EQUAL and STRING-NOT-EQUAL.
(defmacro string-not-equal-loop (end end-value
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
                   (not (two-arg-char-equal-inline
                         (schar string1 index1)
                         (schar string2 index2)))))
         ,end-value)
      (declare (fixnum index1 index2))
      ,@(if abortp
            `((if (not (two-arg-char-equal-inline
                        (schar string1 index1)
                        (schar string2 index2)))
                  (return ,abort-value)))))))

(defun string-equal (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Given two strings (string1 and string2), and optional integers start1,
  start2, end1 and end2, compares characters in string1 to characters in
  string2 (using char-equal)."
  (declare (fixnum start1 start2))
  (with-two-strings string1 string2 start1 end1 nil start2 end2
    (let ((slen1 (- (the fixnum end1) start1))
          (slen2 (- (the fixnum end2) start2)))
      (declare (fixnum slen1 slen2))
      (when (= slen1 slen2)
        ;;return NIL immediately if lengths aren't equal.
        (string-not-equal-loop 1 t nil)))))

(defun two-arg-string-equal (string1 string2)
  #+arm64
  (prog ((string1 string1)
         (string2 string2))
     (cond ((simple-base-string-p string1)
            (cond #+sb-unicode
                  ((simple-character-string-p string2)
                   (go 8-32))
                  ((simple-base-string-p string2)
                   (go 8-8))
                  (t
                   (go normal))))
           #+sb-unicode
           ((simple-character-string-p string1)
            (cond ((simple-base-string-p string2)
                   (rotatef string1 string2)
                   (go 8-32))
                  #+(or)
                  ((simple-character-string-p string2)
                   (go 32-32))
                  (t
                   (go normal))))
           (t
            (go normal)))
   8-32
     #+sb-unicode
     (return-from two-arg-string-equal
       (let ((length1 (length string1)))
         (and (= length1
                 (length string2))
              (sb-vm::simd-base-character-string-equal string1 string2 length1))))
   8-8
     (return-from two-arg-string-equal
       (let ((length1 (length string1)))
         (and (= length1
                 (length string2))
              (sb-vm::simd-base-string-equal string1 string2 length1))))
   32-32
   normal)
  (with-two-arg-strings string1 string2 start1 end1 nil start2 end2
    (let ((slen1 (- (the fixnum end1) start1))
          (slen2 (- (the fixnum end2) start2)))
      (declare (fixnum slen1 slen2))
      (when (= slen1 slen2)
        (string-not-equal-loop 1 t nil)))))

(defun string-not-equal (string1 string2 &key (start1 0) end1 (start2 0) end2)
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

(defun two-arg-string-not-equal (string1 string2)
  (with-two-arg-strings string1 string2 start1 end1 offset1 start2 end2
    (let ((slen1 (- end1 start1))
          (slen2 (- end2 start2)))
      (declare (fixnum slen1 slen2))
      (cond ((= slen1 slen2)
             (string-not-equal-loop 1 nil (- index1 offset1)))
            ((< slen1 slen2)
             (string-not-equal-loop 1 (- index1 offset1)))
            (t
             (string-not-equal-loop 2 (- index1 offset1)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)

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
) ; EVAL-WHEN

(defmacro string-less-greater-equal (lessp equalp)
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
           (if (not (two-arg-char-equal-inline char1 char2))
               (if ,character-test
                   (return (- index1 offset1))
                   (return ()))))))))

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
  "Given two strings, if the first string is lexicographically less than
  the second string, returns the longest common prefix (using char-equal)
  of the two strings. Otherwise, returns ()."
  (string-lessp* string1 string2 start1 end1 start2 end2))

(defun two-arg-string-lessp (string1 string2)
  (string-lessp* string1 string2 0 nil 0 nil))

(defun string-greaterp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Given two strings, if the first string is lexicographically greater than
  the second string, returns the longest common prefix (using char-equal)
  of the two strings. Otherwise, returns ()."
  (string-greaterp* string1 string2 start1 end1 start2 end2))

(defun two-arg-string-greaterp (string1 string2)
  (string-greaterp* string1 string2 0 nil 0 nil))

(defun string-not-lessp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Given two strings, if the first string is lexicographically greater
  than or equal to the second string, returns the longest common prefix
  (using char-equal) of the two strings. Otherwise, returns ()."
  (string-not-lessp* string1 string2 start1 end1 start2 end2))

(defun two-arg-string-not-lessp (string1 string2)
  (string-not-lessp* string1 string2 0 nil 0 nil))

(defun string-not-greaterp (string1 string2 &key (start1 0) end1 (start2 0)
                                    end2)
  "Given two strings, if the first string is lexicographically less than
  or equal to the second string, returns the longest common prefix
  (using char-equal) of the two strings. Otherwise, returns ()."
  (string-not-greaterp* string1 string2 start1 end1 start2 end2))


(defun two-arg-string-not-greaterp (string1 string2)
  (string-not-greaterp* string1 string2 0 nil 0 nil))

(defun make-string (count &key (element-type 'character)
                               (initial-element nil iep))
  "Given a character count and an optional fill character, makes and returns a
new string COUNT long filled with the fill character."
  (declare (index count))
  (declare (explicit-check))
  (cond ((eq element-type 'character)
         (let ((c (if iep (the character initial-element)))
               (s (make-string count :element-type 'character)))
           (when c (sb-vm::unpoison s) (fill s c))
           s))
         ((or (eq element-type 'base-char)
              (eq element-type 'standard-char)
              ;; What's the "most specialized" thing possible that's still a string?
              ;; Well clearly it's a string whose elements have the smallest domain.
              ;; So that would be 8 bits per character, not 32 bits per character.
              (eq element-type nil))
          (let ((c (if iep (the base-char initial-element)))
                (s (make-string count :element-type 'base-char)))
            (when c (sb-vm::unpoison s) (fill s c))
            s))
         (t
          (multiple-value-bind (widetag n-bits-shift)
              (sb-vm::%vector-widetag-and-n-bits-shift element-type)
            (unless (or #+sb-unicode (= widetag sb-vm:simple-character-string-widetag)
                        (= widetag sb-vm:simple-base-string-widetag))
              (error "~S is not a valid :ELEMENT-TYPE for MAKE-STRING" element-type))
            (let ((string
                   (sb-vm::allocate-vector-with-widetag
                    #+ubsan nil widetag count n-bits-shift)))
              (when iep
                (fill string initial-element))
              string)))))

(defmacro nstring-case (case-index a z)
  (declare (ignorable a z))
  `(cond ((and (zerop start)
               (not end)
               (typecase string
                 #+sb-unicode
                 (simple-base-string
                  (let* ((repeat (ldb (byte sb-vm:n-word-bits 0) #x0101010101010101))
                         (a-mask (* (- 128 (char-code ,a)) repeat))
                         (z-mask (* (- 127 (char-code ,z)) repeat))
                         (7-mask (* 128 repeat)))
                    (declare (optimize sb-c::preserve-single-use-debug-variables
                                       sb-c::preserve-constants))
                    (flet ((%case (bits)
                             (logxor bits (ash (logand (logxor (+ bits a-mask)
                                                               (+ bits z-mask))
                                                       7-mask)
                                               -2))))
                      (declare (inline %case))
                      (loop for i below (ceiling (length string) sb-vm:n-word-bytes)
                            do
                            (setf (%vector-raw-bits string i)
                                  (%case (%vector-raw-bits string i))))))
                  string)
                 #+(or x86-64 arm64)
                 ((simple-array character)
                  (let ((length (length string)))
                    (sb-vm::simd-string-case ,a string string
                          i
                          (do ((index i (1+ index))
                               (cases +character-cases+))
                              ((>= index length))
                            (let ((char (schar string index)))
                              (with-case-info (char case-index cases
                                               :cases cases)
                                (let ((code (aref cases ,case-index)))
                                  (unless (zerop code)
                                    (setf (schar string index)
                                          (code-char (truly-the %char-code code))))))))))
                  string))))
         (t
          (with-one-string (string start end)
            (declare (optimize (sb-c:insert-array-bounds-checks 0)))
            (cond #+sb-unicode
                  ((simple-base-string-p string)
                   (do ((index start (1+ index)))
                       ((>= index end))
                     (let ((char (char-code (schar string index))))
                       (when (<= (char-code ,a) char (char-code ,z))
                         (setf (schar string index)
                               (code-char (truly-the %char-code (logxor char #x20))))))))
                  (t
                   (do ((index start (1+ index))
                        (cases +character-cases+))
                       ((>= index end))
                     (let ((char (schar string index)))
                       (with-case-info (char case-index cases
                                        :cases cases)
                         (let ((code (aref cases ,case-index)))
                           (unless (zerop code)
                             (setf (schar string index)
                                   (code-char (truly-the %char-code code))))))))))))))

(defun nstring-upcase (string &key (start 0) end)
  (nstring-case (1+ case-index) #\a #\z)
  string)

(defun nstring-downcase (string &key (start 0) end)
  (nstring-case case-index #\A #\Z)
  string)

(defmacro string-case (case-index a z)
  (declare (ignorable a z))
  `(let* ((string (%string string))
          (length (length string)))
     (or
      (and (eql start 0)
           (not end)
           (typecase string
             #+sb-unicode
             (simple-base-string
              (let* ((repeat (ldb (byte sb-vm:n-word-bits 0) #x0101010101010101))
                     (a-mask (* (- 128 (char-code ,a)) repeat))
                     (z-mask (* (- 127 (char-code ,z)) repeat))
                     (7-mask (* 128 repeat)))
                (declare (optimize sb-c::preserve-single-use-debug-variables
                                   sb-c::preserve-constants))
                (flet ((%case (bits)
                         (logxor bits (ash (logand (logxor (+ bits a-mask)
                                                           (+ bits z-mask))
                                                   7-mask)
                                           -2))))
                  (declare (inline %case))
                  (let ((new (make-string length :element-type 'base-char)))
                    (loop for i below (ceiling (length string) sb-vm:n-word-bytes)
                          do
                          (setf (%vector-raw-bits new i)
                                (%case (%vector-raw-bits string i))))
                    new))))
             #+(or x86-64 arm64)
             ((simple-array character)
              (let ((new (make-string length)))
                (sb-vm::simd-string-case ,a string new
                     i
                     (do ((index i (1+ index))
                          (cases +character-cases+))
                         ((>= index length))
                       (let* ((char (schar string index))
                              (cased (with-case-info (char case-index cases
                                                      :cases cases
                                                      :miss-value char)
                                       (let ((code (aref cases ,case-index)))
                                         (if (zerop code)
                                             char
                                             (code-char (truly-the %char-code code)))))))
                         (setf (schar new index) cased))))
                new))))
      (with-array-data ((string-data string :offset-var offset)
                        (s-start start)
                        (s-end end)
                        :check-fill-pointer t)
        (declare (optimize (sb-c:insert-array-bounds-checks 0)))
        (cond #+sb-unicode
              ((simple-base-string-p string-data)
               (let* ((new (make-string length :element-type 'base-char)))
                 (when (> start 0)
                   (loop for d-i below start
                         for s-i from offset
                         do
                         (locally (declare (optimize (safety 0)))
                           (setf (schar new d-i)
                                 (schar string-data s-i)))))
                 (when (and end
                            (< end length))
                   (loop for d-i from end below length
                         for s-i from s-end
                         do (locally (declare (optimize (safety 0)))
                              (setf (schar new d-i)
                                    (schar string-data s-i)))))
                 (do ((s-i s-start (truly-the index (1+ s-i)))
                      (d-i start (truly-the index (1+ d-i))))
                     ((>= s-i s-end))
                   (declare (index d-i))
                   (let ((char (char-code (schar string-data s-i))))
                     (setf (schar new d-i)
                           (code-char
                            (if (<= (char-code ,a) char (char-code ,z))
                                (truly-the %char-code (logxor char #x20))
                                char)))))
                 new))
              (t
               (let ((new (make-string length)))
                 (when (> start 0)
                   (loop for d-i below start
                         for s-i from offset
                         do
                         (locally (declare (optimize (safety 0)))
                           (setf (schar new d-i)
                                 (schar string-data s-i)))))
                 (when (and end
                            (< end length))
                   (loop for d-i from end below length
                         for s-i from s-end
                         do (locally (declare (optimize (safety 0)))
                              (setf (schar new d-i)
                                    (schar string-data s-i)))))
                 (do ((s-i s-start (truly-the index (1+ s-i)))
                      (d-i start (truly-the index (1+ d-i)))
                      (cases +character-cases+))
                     ((>= s-i s-end))
                   (declare (index d-i))
                   (let* ((char (schar string-data s-i))
                          (cased (with-case-info (char case-index cases
                                                  :cases cases
                                                  :miss-value char)
                                   (let ((code (aref cases ,case-index)))
                                     (if (zerop code)
                                         char
                                         (code-char (truly-the %char-code code)))))))
                     (setf (schar new d-i) cased)))
                 new)))))))

(defun string-upcase (string &key (start 0) end)
  (declare (explicit-check))
  (string-case (1+ case-index) #\a #\z))

(defun string-downcase (string &key (start 0) end)
  (declare (explicit-check))
  (string-case case-index #\A #\Z))

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
  )                                     ; FLET


(defun generic-string-trim (char-bag string left-p right-p)
  (let ((header (%string string)))
    (with-array-data ((string header)
                      (start)
                      (end)
                      :check-fill-pointer t)
      (flet ((trim-char-p (char)
               (typecase char-bag
                 (list
                  (loop for bag-char in char-bag
                        thereis (char= bag-char char)))
                 (simple-string
                  (loop for bag-char across char-bag
                        thereis (char= bag-char char)))
                 (simple-vector
                  (loop for bag-char across char-bag
                        thereis (char= bag-char char)))
                 (vector
                  (loop for bag-char across char-bag
                        thereis (char= bag-char char)))
                 (t
                  (find char char-bag :test #'char=)))))
        (let* ((left-end (if left-p
                             (do ((index start (1+ index)))
                                 ((or (= index (the fixnum end))
                                      (not (trim-char-p (schar string index))))
                                  index)
                               (declare (fixnum index)))
                             start))
               (right-end (if right-p
                              (do ((index (1- (the fixnum end)) (1- index)))
                                  ((or (< index left-end)
                                       (not (trim-char-p (schar string index))))
                                   (1+ index))
                                (declare (fixnum index)))
                              end)))
          (if (and (eql left-end start)
                   (eql right-end end))
              header
              (subseq (the simple-string string) left-end right-end)))))))

(defun string-left-trim (char-bag string)
  (generic-string-trim char-bag string t nil))

(defun string-right-trim (char-bag string)
  (generic-string-trim char-bag string nil t))

(defun string-trim (char-bag string)
  (generic-string-trim char-bag string t t))

(defun logically-readonlyize (vector &optional (always-shareable t))
  ;; "Always" means that regardless of whether the user wanted
  ;; coalescing of strings used as literals in code compiled to memory,
  ;; the string is shareable.
  (when (dynamic-space-obj-p vector)
    ;; FIXME: did I get the condition backwards? I'm trying to remember what it meant.
    ;; "Always" should mean that the language specifies the behavior, e.g. strings used
    ;; as print names can not be modified. (edge case: if it ceases to be a print name,
    ;; can it then be modified?) "Not always" means that we've done an
    ;; implementation-specific thing to make more strings shareable than specified.
    ;; "Always" isn't really the best terminology for the semantics, I guess.
    ;; And are there any tests around this???
    (logior-array-flags (the (simple-array * 1) vector)
                        (if always-shareable
                            sb-vm:+vector-shareable+
                            sb-vm:+vector-shareable-nonstd+)))
  vector)
