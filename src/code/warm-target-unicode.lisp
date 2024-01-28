;;;; More Unicode functions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-UNICODE")

;;; Functions not defined in make-host-2 did not get exported
(export '(bidi-mirroring-glyph
          confusable-p
          numeric-value))

(eval-when (:compile-toplevel :execute)
  (defun read-from-file (namestring &key (enforce-single-expr t) build-dependent)
    (declare (notinline concatenate) (ignore build-dependent))
    (with-open-file
        (s (concatenate 'string (if (boundp 'cl-user::*generated-sources-root*)
                                    (symbol-value 'cl-user::*generated-sources-root*)
                                    "")
                        namestring))
      (let* ((result (read s))
             (eof-result (cons nil nil)))
        (unless enforce-single-expr
          (return-from read-from-file result))
        (unless (eq (read s nil eof-result) eof-result)
          (error "more than one expression in file ~S" namestring))
        result))))

;;; This macro produces a lookup table that uses half the storage of a hash-table
;;; and achieves around double the performance on this test:
;;;
;;; * (time (loop for i below char-code-limit count (numeric-value (code-char i))))
;;;
;;; x86-64-based mac:
;;; Old
;;; ===
;;;    0.029 seconds of real time
;;;    0.029656 seconds of total run time (0.029644 user, 0.000012 system)
;;;    103.45% CPU
;;;    71,169,208 processor cycles
;;; New
;;; ===
;;;    0.012 seconds of real time
;;;    0.012075 seconds of total run time (0.012061 user, 0.000014 system)
;;;    100.00% CPU
;;;    29,034,292 processor cycles
;;;
;;; arm-based mac:
;;; Old
;;; ===
;;;    0.051 seconds of real time
;;;    0.050890 seconds of total run time (0.050709 user, 0.000181 system)
;;;    100.00% CPU
;;; New
;;; ===
;;;    0.033 seconds of real time
;;;    0.033195 seconds of total run time (0.033104 user, 0.000091 system)
;;;    100.00% CPU

(defmacro find-in-perfect-hashmap (x filename value-type value-getter)
  (let ((pairs
         (remove-if (lambda (x) (>= (car x) char-code-limit))
                    (read-from-file filename))))
    (when (< (length pairs) 5)
      (aver (eq value-getter 'cdr))
      (return-from find-in-perfect-hashmap
        `(cdr (assoc (char-code ,x) ',pairs))))
    (unless (symbolp value-getter)
      (setq value-getter (compile nil value-getter)))
    (let* ((mapped-chars (coerce (mapcar 'car pairs) '(array (unsigned-byte 32) (*))))
           (lexpr (sb-c:make-perfect-hash-lambda mapped-chars))
           ;; We need the lexpr at compile-time to build the key/value arrays
           ;; and run-time of course, where the expression is stuffed in as
           ;; a form headed by LAMBDA.
           (hasher (compile nil lexpr))
           (n (length mapped-chars))
           ;; This array is pasted in as though written literally in source,
           ;; therefore it gets relocated to read-only space in the core.
           (key-array (make-array n :element-type '(unsigned-byte 32)))
           (value-array (make-array n :element-type value-type)))
      (dolist (pair pairs)
        (let ((index (funcall hasher (car pair))))
          (aver (/= (car pair) 0)) ; a key can't be zero
          (aver (= (aref key-array index) 0)) ; confirm perfect hashing
          (setf (aref key-array index) (car pair)
                (aref value-array index) (funcall value-getter pair))))
      `(let* ((code (char-code ,x)) (hash (,lexpr code)))
         ;; Remember: even though the mapping is dense (range is 0..N-1)
         ;; a key which was not in the mapping as specified to the hash function
         ;; generator may cause it return any value outside the expected range.
         ;; So bounds check it and then confirm a hit.
         (when (and (< hash ,n) (= (aref ,key-array hash) code))
           (aref ,value-array hash))))))

(defun numeric-value (character)
  "Returns the numeric value of CHARACTER or NIL if there is no such value.
Numeric value is the most general of the Unicode numeric properties.
The only constraint on the numeric value is that it be a rational number."
  (or (find-in-perfect-hashmap character "output/ucd/numerics.lisp-expr" t cdr)
      (digit-value character)))

;;; FIXME: why does #-sb-unicode want or need this?
;;; (Indeed the regression test for it is *disabled* so I reiterate - WHY?)
(defun bidi-mirroring-glyph (character)
  "Returns the mirror image of CHARACTER if it exists.
Otherwise, returns NIL."
  ;; This used to call MIRRORED-P before table lookup, but it's not faster to do so
  #+sb-unicode
  (find-in-perfect-hashmap character "output/ucd/bidi-mirrors.lisp-expr"
                           character (lambda (x) (code-char (second x))))
  #-sb-unicode
  (macrolet ((direct-map (&aux (a (make-array char-code-limit :element-type 'character)))
               (dolist (pair (read-from-file "output/ucd/bidi-mirrors.lisp-expr") a)
                 (let ((key (car pair)))
                   (when (< key char-code-limit)
                     (setf (char a key) (code-char (second pair))))))))
    (let ((answer (char (direct-map) (char-code character))))
      (unless (char= answer (code-char 0)) answer))))

;;; Confusable detection

;;; hash-table:
;;;   * (time (loop for i below char-code-limit count (gethash (code-char i) **confusables**)))
;;;     0.020 seconds of real time
;;; perfect hash:
;;;   * (time (loop for i below char-code-limit count (lookup (code-char i))))
;;;     0.008 seconds of real time
(defun canonically-deconfuse (string)
  (declare (string string))
  ;; BUG: filtering by (CAR pair) is inadequate for #-sb-unicode when reading confusables,
  ;; and it always was. An error could be observed by calling canonically-deconfuse
  ;; on any key of this alist:
  ;; ((175 713) (240 8706 821) (162 99 824) (231 99 806) (199 67 806) (208 68 821)
  ;;   (248 111 824) (216 79 824) (37 186 47 8320) (165 89 821) (181 956) (246 1577))
  ;; which would call CODE-CHAR on illegal inputs. The list came from:
  ;; (remove-if (lambda (x) (or (> (car x) 255) (< (reduce #'max (cdr x)) 256)))
  ;;            (with-open-file (f "output/ucd/confusables.lisp-expr") (read f)))
  ;; Maybe we should filter CDR pair here, though it would leave extra keys in the map
  ;; which seems to cause no immediate harm.
  (flet ((lookup (character)
           (find-in-perfect-hashmap
            character "output/ucd/confusables.lisp-expr" t
            (lambda (pair &aux (x (cdr pair)))
              (case (length x)
                (1 (elt x 0))
                (2 (pack-3-codepoints (elt x 0) (elt x 1)))
                (3 (pack-3-codepoints (elt x 0) (elt x 1) (elt x 2)))
                (t (logically-readonlyize
                    (possibly-base-stringize (map 'string #'code-char x)))))))))
    (let (result)
      (loop for char across string
            for deconfused = (lookup char)
            do (cond ((not deconfused)
                      (push (string char) result))
                     ((integerp deconfused)
                      (push (sb-impl::unpack-3-codepoints deconfused)
                            result))
                     (t
                      (push deconfused result))))
      (apply #'concatenate 'string (nreverse result)))))

;;; This function is weird! It reports that every string is confusable with itself
;;; even if it contains no confusable characters at all.
;;; e.g. (lookup-confusable #\W) => NIL but (confusable-p "W" "W") => T
(defun confusable-p (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Determines whether STRING1 and STRING2 could be visually confusable
according to the IDNA confusableSummary.txt table"
    (let* ((form #+sb-unicode :nfd #-sb-unicode :nfc)
           (str1 (normalize-string (subseq string1 start1 end1) form))
           (str2 (normalize-string (subseq string2 start2 end2) form))
           (skeleton1 (normalize-string (canonically-deconfuse str1) form))
           (skeleton2 (normalize-string (canonically-deconfuse str2) form)))
      (string= skeleton1 skeleton2)))
