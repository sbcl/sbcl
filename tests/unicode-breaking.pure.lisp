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

(defconstant +mul+ #+sb-unicode (code-char 215) #-sb-unicode #\*)
(defconstant +div+ #+sb-unicode (code-char 247) #-sb-unicode #\/)

(defun line-to-clusters (line)
  (let ((codepoints
         (remove "" (split-string (substitute #\Space +div+ line) #\Space)
                 :test #'string=))
         clusters cluster (nobreak t))
    (loop for i in codepoints do
         (if (string= i (string +mul+)) (setf nobreak t)
             (progn
               (unless nobreak
                 (push (nreverse cluster) clusters)
                 (setf cluster nil))
               (push (code-char (parse-integer i :radix 16)) cluster)
               (setf nobreak nil))))
    (when cluster (push (nreverse cluster) clusters))
    (setf clusters (nreverse (mapcar #'(lambda (x) (coerce x 'string)) clusters)))
    clusters))

(defun parse-codepoints (string &key (singleton-list t))
  (let ((list (mapcar
              (lambda (s) (parse-integer s :radix 16))
              (remove "" (split-string string #\Space) :test #'string=))))
    (if (not (or (cdr list) singleton-list)) (car list) list)))

(defun test-line (fn line n)
  (let ((relevant-portion (subseq line 0 (position #\# line))))
    (when (string/= relevant-portion "")
      (let* ((string
               (coerce (mapcar
                        #'code-char
                        (parse-codepoints
                         (remove +mul+ (remove +div+ relevant-portion))))
                       'string))
             (actual (funcall fn string))
             (expected (line-to-clusters relevant-portion)))
        (assert (equalp actual expected)
                ()
                "~@<line ~D: ~S - expected: ~S, actual: ~S~@:>"
                n line expected actual)))))

(defun test-graphemes ()
  (declare (optimize (debug 2)))
  (with-test (:name (:grapheme-breaking)
                    :skipped-on (not :sb-unicode))
    (with-open-file (s "data/GraphemeBreakTest.txt" :external-format :utf8)
      (loop for line = (read-line s nil nil)
            for n from 0
            while line
            do (test-line #'graphemes (remove #\Tab line) n)))))

(test-graphemes)

(defun test-words ()
  (declare (optimize (debug 2)))
  (with-test (:name (:word-breaking)
                    :skipped-on (not :sb-unicode))
    (with-open-file (s "data/WordBreakTest.txt" :external-format :utf8)
      (loop for line = (read-line s nil nil)
            for n from 0
            while line
            do (test-line #'words (remove #\Tab line) n)))))

(test-words)

(defun test-sentences ()
  (declare (optimize (debug 2)))
  (with-test (:name (:sentence-breaking)
                    :skipped-on (not :sb-unicode))
    (with-open-file (s "data/SentenceBreakTest.txt" :external-format :utf8)
      (loop for line = (read-line s nil nil)
            for n from 0
            while line
            do (test-line #'sentences (remove #\Tab line) n)))))

(test-sentences)

(with-test (:name (:sentence-breaking :test1) :skipped-on (not :sb-unicode))
  ;; CRLF should be its own entity, not joined up with Aterm (.)
  (assert (equal (sb-unicode:sentences (format nil "1.~C~C " #\Return #\Linefeed))
                 (list (format nil "1.~C~C" #\Return #\Linefeed) " "))))

(with-test (:name (:sentence-breaking :test2) :skipped-on (not :sb-unicode))
  ;; the second Close ()) should not extend the Aterm Close* Sp* sequence
  (assert (equal (sb-unicode:sentences "1.) (B")
                 (list "1.) " "(B"))))

(with-test (:name (:sentence-breaking :test3) :skipped-on (not :sb-unicode))
  ;; Other Punctuation (*) should not be treated as Close
  (assert (equal (sb-unicode:sentences "1.* X")
                 (list "1." "* X"))))

(with-test (:name (:sentence-breaking :test4) :skipped-on (not :sb-unicode))
  ;; the U+200D (ZWJ) should extend the Aterm (.)
  (assert (equal (sb-unicode:sentences (format nil "1~C.~C X" #\Return (code-char 8205)))
                 (list (format nil "1~C" #\Return)
                       (format nil ".~C " (code-char 8205))
                       "X"))))

(defun process-line-break-line (line)
  (let ((elements (split-string line #\Space)))
    (mapcar #'(lambda (e)
                (cond
                  ((eql (char e 0) +mul+) :cant)
                  ((eql (char e 0) +div+) :can)
                  (t (code-char (parse-integer e :radix 16)))))
            elements)))

(defun string-from-line-break-line (string)
  (coerce
   (mapcar
    #'(lambda (s) (code-char (parse-integer s :radix 16)))
    (remove
     ""
     (split-string (remove +mul+ (remove +div+ string)) #\Space)
     :test #'string=)) 'string))

(with-test (:name (:line-breaking) :skipped-on (not :sb-unicode))
  (with-open-file (s "data/LineBreakTest.txt" :external-format :utf8)
    (loop for line = (read-line s nil nil)
          for n from 0
          while line
          do (let ((string (subseq line 0 (max 0 (1- (or (position #\# line) 1))))))
               (unless (string= string "")
                 (let* ((expected (process-line-break-line string))
                        (annotated (sb-unicode::line-break-annotate
                                    (string-from-line-break-line string)))
                        (actual (substitute :can :must annotated)))
                   (assert (equal expected actual)
                           ()
                           "~@<line ~D: ~S - expected: ~S, actual; ~S~:@>"
                           n string expected actual)))))))

(with-test (:name (:line-breaking :test1) :skipped-on (not :sb-unicode))
  ;; line-break class SA[gc=Mn|Mc] should be resolved to CM and
  ;; clustered with its preceding character.
  (let* ((string (map 'string 'code-char '(#x41 #x102b #x20)))
         (annotated (sb-unicode::line-break-annotate string))
         (expected `(:cant ,(code-char #x41) :cant ,(code-char #x102b) :cant ,(code-char #x20) :must)))
    (assert (equal annotated expected))))
