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

(defun parse-one-line (line)
  (do* ((i 0 (1+ i))
        (start 0 (1+ end))
        (end (position #\; line :start start) (position #\; line :start start))
        result)
       ((= i 5) (nreverse result))
    (with-input-from-string (s (subseq line start (1+ end)))
      (let ((*read-base* 16.))
        (push (map 'string 'code-char (read-delimited-list #\; s)) result)))))

(defmacro assert-all-string= (base &body others)
  `(progn
     ,@(loop for test in others
          collect `(assert (string= ,base ,test)))))

(defun test-line (c1 c2 c3 c4 c5)
  ;; NFC
  (assert-all-string= c2
    (normalize-string c1 :nfc)
    (normalize-string c2 :nfc)
    (normalize-string c3 :nfc))
  (assert-all-string= c4
    (normalize-string c4 :nfc)
    (normalize-string c5 :nfc))

  ;; NFD
  (assert-all-string= c3
    (normalize-string c1 :nfd)
    (normalize-string c2 :nfd)
    (normalize-string c3 :nfd))
  (assert-all-string= c5
    (normalize-string c4 :nfd)
    (normalize-string c5 :nfd))

  ;; NFKC
  (assert-all-string= c4
    (normalize-string c1 :nfkc)
    (normalize-string c2 :nfkc)
    (normalize-string c3 :nfkc)
    (normalize-string c4 :nfkc)
    (normalize-string c5 :nfkc))

  ;; NFKD
  (assert-all-string= c5
    (normalize-string c1 :nfkd)
    (normalize-string c2 :nfkd)
    (normalize-string c3 :nfkd)
    (normalize-string c4 :nfkd)
    (normalize-string c5 :nfkd)))

(defun test-no-normalization (string)
  (assert-all-string= string
    (normalize-string string :nfc)
    (normalize-string string :nfd)
    (normalize-string string :nfkc)
    (normalize-string string :nfkd)))

(defun test-normalization ()
  (declare (optimize (debug 2)))
  (with-open-file (s "data/NormalizationTest.txt" :external-format :latin1)
    (do ((line (read-line s) (read-line s)))
        ((char/= #\# (char line 0))
         (assert (string= "@Part0" line :end2 6))
         (assert (char= #\# (char (read-line s) 0)))))
    ;; Part0: specific cases
    (with-test (:name (:unicode-normalization :part0)
                      :skipped-on (not :sb-unicode))
      (do ((line (read-line s) (read-line s)))
          ((char= #\# (char line 0))
           (assert (string= "@Part1" (read-line s) :end2 6))
           (assert (char= #\# (char (read-line s) 0)))
           (assert (char= #\# (char (read-line s) 0))))
        (destructuring-bind (c1 c2 c3 c4 c5)
            (parse-one-line line)
          (test-line c1 c2 c3 c4 c5))))
    ;; Part1: single characters.  (Extra work to check for conformance
    ;; on unlisted entries)
    (with-test (:name (:unicode-normalization :part1)
                      :skipped-on (not :sb-unicode))
      (do ((line (read-line s) (read-line s))
           (code 0))
          ((char= #\# (char line 0))
           (do ((code code (1+ code)))
               ((= code #x110000))
             (test-no-normalization (string (code-char code))))
           (assert (string= "@Part2" (read-line s) :end2 6))
           (assert (char= #\# (char (read-line s) 0))))
        (destructuring-bind (c1 c2 c3 c4 c5)
            (parse-one-line line)
          (do ((c code (1+ c)))
              ((= c (char-code (char c1 0)))
               (test-line c1 c2 c3 c4 c5)
               (setf code (1+ c)))
            (test-no-normalization (string (code-char code)))))))
    ;; Part2: Canonical Order Test
    (with-test (:name (:unicode-normalization :part2)
                      :skipped-on (not :sb-unicode))
      (do ((line (read-line s) (read-line s)))
          ((char= #\# (char line 0))
           (assert (string= "@Part3" (read-line s) :end2 6))
           (assert (char= #\# (char (read-line s) 0))))
        (destructuring-bind (c1 c2 c3 c4 c5)
            (parse-one-line line)
          (test-line c1 c2 c3 c4 c5))))
    ;; Part3: PRI #29 Test
    (with-test (:name (:unicode-normalization :part3)
                      :skipped-on (not :sb-unicode))
      (do ((line (read-line s) (read-line s)))
          ((char= #\# (char line 0))
           (assert (char= #\# (char (read-line s) 0)))
           (assert (null (read-line s nil nil))))
        (destructuring-bind (c1 c2 c3 c4 c5)
            (parse-one-line line)
          (test-line c1 c2 c3 c4 c5))))))

(test-normalization)
