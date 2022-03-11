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

(defun test-line (fn line)
  (let ((relevant-portion (subseq line 0 (position #\# line))))
    (when (string/= relevant-portion "")
      (let ((string
             (coerce (mapcar
                      #'code-char
                      (parse-codepoints
                       (remove +mul+ (remove +div+ relevant-portion))))
                     'string)))
        (assert (equalp (funcall fn string)
                        (line-to-clusters relevant-portion)))))))

(defun test-graphemes ()
  (declare (optimize (debug 2)))
  (with-test (:name (:grapheme-breaking)
                    :skipped-on (not :sb-unicode))
    (with-open-file (s "data/GraphemeBreakTest.txt" :external-format :utf8)
      (loop for line = (read-line s nil nil)
            while line
            do (test-line #'graphemes (remove #\Tab line))))))

(test-graphemes)

(defun test-words ()
  (declare (optimize (debug 2)))
  (with-test (:name (:word-breaking)
                    :skipped-on (not :sb-unicode))
    (with-open-file (s "data/WordBreakTest.txt" :external-format :utf8)
      (loop for line = (read-line s nil nil)
            while line
            do (test-line #'words (remove #\Tab line))))))

(test-words)

(defun test-sentences ()
  (declare (optimize (debug 2)))
  (with-test (:name (:sentence-breaking)
                    :skipped-on (not :sb-unicode))
    (with-open-file (s "data/SentenceBreakTest.txt" :external-format :utf8)
      (loop for line = (read-line s nil nil)
            while line
            do (test-line #'sentences (remove #\Tab line))))))

(test-sentences)

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

(with-test (:name (:line-breaking)
            :skipped-on (not :sb-unicode))
  (let* ((line-break-exceptions (make-hash-table :test 'equal))
         (strings
           (with-open-file (s "data/line-break-exceptions.lisp-expr" :external-format :utf-8)
             (read s))))
    (dolist (s strings)
      (setf (gethash s line-break-exceptions) t))
    (with-open-file (s "data/LineBreakTest.txt" :external-format :utf8)
      (loop for line = (read-line s nil nil)
            while line
            do (let ((string (subseq line 0 (max 0 (1- (or (position #\# line) 1))))))
                 (unless (string= string "")
                   (let* ((expected (process-line-break-line string))
                          (annotated (sb-unicode::line-break-annotate
                                      (string-from-line-break-line string)))
                          (actual (substitute :can :must annotated)))
                     (if (gethash string line-break-exceptions)
                         (assert (not (equal expected actual)))
                         (assert (equal expected actual))))))))))
