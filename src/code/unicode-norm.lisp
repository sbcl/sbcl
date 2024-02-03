;;;; Unicode functions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-UNICODE")

(declaim (ftype function canonically-compose decompose-string))

(declaim (inline combining-class))
(defun combining-class (character)
  "Returns the canonical combining class (CCC) of CHARACTER"
  (aref +character-misc-database+ (+ 2 (misc-index character))))

;;; Implements UAX#15: Normalization Forms
(declaim (inline char-decomposition-info))
(defun char-decomposition-info (char)
  (let ((value (aref +character-misc-database+
                     (+ 4 (misc-index char)))))
    (values (clear-flag 7 value) (logbitp 7 value))))

(defun char-decomposition (char length callback)
  (declare (function callback))
  ;; Caller should have gotten length from char-decomposition-info
  (let* ((cp (char-code char))
         (cp-high (ash cp -8))
         (decompositions +character-decompositions+)
         (high-page (aref +character-high-pages+ cp-high))
         (index (unless (logbitp 15 high-page) ;; Hangul syllable
                  (aref +character-low-pages+
                        (+ 1 (* 2 (+ (ldb (byte 8 0) cp) (ash high-page 8))))))))
    (cond ((= length 1)
           (funcall callback (code-char (aref decompositions index))))
          ((<= #xac00 cp #xd7a3)
           ;; see Unicode 6.2, section 3-12
           (let* ((sbase #xac00)
                  (lbase #x1100)
                  (vbase #x1161)
                  (tbase #x11a7)
                  (vcount 21)
                  (tcount 28)
                  (ncount (* vcount tcount))
                  (sindex (- cp sbase))
                  (lindex (floor sindex ncount))
                  (vindex (floor (mod sindex ncount) tcount))
                  (tindex (mod sindex tcount)))
             (funcall callback (code-char (+ lbase lindex)))
             (funcall callback (code-char (+ vbase vindex)))
             (when (> tindex 0)
               (funcall callback  (code-char (+ tbase tindex))))))

          (t
           (loop for i below length
                 do
                 (funcall callback (code-char (aref decompositions (+ index i)))))))))

(defun decompose-char (char compatibility callback)
  (declare (function callback))
  (multiple-value-bind (info compat) (char-decomposition-info char)
    (if (and (plusp info)
             (or compatibility
                 (not compat)))
        (if compatibility
            (dx-flet ((callback (char)
                        (decompose-char char t callback)))
              (char-decomposition char info #'callback))
            (char-decomposition char info callback))
        (funcall callback char))))

(defun decompose-string (string compatibility filter)
  (let (chars
        (length 0)
        (previous-combining-class 0))
    (declare (type index length))
    (dx-flet ((callback (char)
                        (let ((combining-class (combining-class char)))
                          (incf length)
                          (cond ((< 0 combining-class previous-combining-class)
                                 ;; Ensure it's sorted
                                 (loop for cons on chars
                                       for next-char = (cadr cons)
                                       when (or (not next-char)
                                                (<= 0 (combining-class next-char) combining-class))
                                       do (setf (cdr cons)
                                                (cons char (cdr cons)))
                                          (return)))
                                (t
                                 (push char chars)
                                 (setf previous-combining-class combining-class))))))
      (sb-kernel:with-array-data ((string string) (start) (end)
                                  :check-fill-pointer t)
        (let ((calback (if filter
                           (let ((filter (sb-kernel:%coerce-callable-to-fun filter)))
                             (lambda (char)
                               (when (funcall filter char)
                                 (callback char))))
                           #'callback)))
          (loop for i from start below end
                for char = (schar string i)
                do
                (decompose-char char compatibility calback))))
      (nreverse chars))))

(defun primary-composition (char1 char2)
  #-sb-unicode
  (declare (ignore char1 char2))
  #-sb-unicode
  #.(let* ((data (sb-cold:read-from-file "output/ucd/comp.lisp-expr"))
           (entries (loop for pair across data
                          for key = (car pair)
                          for c1 = (ldb (byte 21 21) key)
                          for c2 = (ldb (byte 21 0) key)
                          when (and (< c1 sb-xc:char-code-limit)
                                    (< c2 sb-xc:char-code-limit))
                          collect pair)))
      (aver (null entries))
      nil)
  #+sb-unicode
  (flet ((composition-hangul-syllable-type (cp)
           (cond
             ((and (<= #x1100 cp) (<= cp #x1112)) :L)
             ((and (<= #x1161 cp) (<= cp #x1175)) :V)
             ((and (<= #x11a8 cp) (<= cp #x11c2)) :T)
             ((and (<= #xac00 cp) (<= cp #.(+ #xac00 11171)))
              (if (= 0 (rem (- cp #xac00) 28)) :LV :LVT)))))
    (declare (inline composition-hangul-syllable-type))
    (let ((c1 (char-code char1))
          (c2 (char-code char2)))
       (cond
         ((gethash (dpb c1 (byte 21 21) c2)
                   (load-time-value
                    (let ((data '#.(sb-cold:read-from-file "output/ucd/comp.lisp-expr")))
                      (sb-impl::%stuff-hash-table
                       (make-hash-table :size (length data) #+64-bit :test #+64-bit #'eq)
                       (loop for pair across data
                             collect (cons (car pair) (code-char (cdr pair))))
                       t))
                    t)))
         ((and (eql (composition-hangul-syllable-type c1) :L)
               (eql (composition-hangul-syllable-type c2) :V))
          (let ((lindex (- c1 #x1100))
                (vindex (- c2 #x1161)))
            (code-char (+ #xac00 (* lindex 588) (* vindex 28)))))
         ((and (eql (composition-hangul-syllable-type c1) :LV)
               (eql (composition-hangul-syllable-type c2) :T))
          (code-char (+ c1 (- c2 #x11a7))))))))

(defun canonically-compose (list)
  (let* ((result list)
         (combine-with (member 0 result :key #'combining-class))
         (previous combine-with)
         (current (cdr combine-with)))
    (when (null current)
      (return-from canonically-compose list))
    (tagbody
     again
       (when (and (neq previous combine-with)
                  ;; test for Blocked (Unicode 3.11 para. D115)
                  ;;
                  ;; (assumes here that string has sorted combiners,
                  ;; so can look back just one step)
                  (>= (combining-class (car previous))
                      (combining-class (car current))))
         (when (= (combining-class (car current)) 0)
           (setf combine-with current))
         (setf previous current)
         (pop current)
         (go next))

       (let ((comp (primary-composition (car combine-with) (car current))))
         (cond
           (comp
            (setf (car combine-with) comp
                  (cdr previous) (setf current (cdr current))))
           (t
            (when (= (combining-class (car current)) 0)
              (setf combine-with current))
            (setf previous current)
            (pop current))))
     next
       (when current
         (go again)))
    result))

(defun normalize-string (string &optional (form :nfd)
                                          filter)
  "Normalize STRING to the Unicode normalization form FORM.
Acceptable values for form are :NFD, :NFC, :NFKD, and :NFKC.
If FILTER is a function it is called on each decomposed character and
only characters for which it returns T are collected."
  (declare (type (member :nfd :nfkd :nfc :nfkc) form))
  #-sb-unicode
  (declare (ignore filter))
  #-sb-unicode
  (etypecase string
    ((array nil (*)) string)
    (string
     (ecase form
       ((:nfc :nfkc) string)
       ((:nfd :nfkd) (error "Cannot normalize to ~A form in #-SB-UNICODE builds" form)))))
  #+sb-unicode
  (etypecase string
    (base-string string)
    ((array character (*))
     (coerce
      (ecase form
        ((:nfc)
         (canonically-compose (decompose-string string nil filter)))
        ((:nfd)
         (decompose-string string nil filter))
        ((:nfkc)
         (canonically-compose (decompose-string string t filter)))
        ((:nfkd)
         (decompose-string string t filter)))
      'string))
    ((array nil (*)) string)))

(defun normalized-p (string &optional (form :nfd))
  "Tests if STRING is normalized to FORM"
  (etypecase string
    (base-string t)
    ((array character (*))
     (flet ((=-to-list (list)
              (sb-kernel:with-array-data ((string string) (start) (end)
                                          :check-fill-pointer t)
                (loop for i from start below end
                      for char = (schar string i)
                      always (eql char (pop list))))))
       (ecase form
         ((:nfc)
          (=-to-list (canonically-compose (decompose-string string nil nil))))
         ((:nfd)
          (=-to-list (decompose-string string nil nil)))
         ((:nfkc)
          (=-to-list (canonically-compose (decompose-string string t nil))))
         ((:nfkd)
          (=-to-list (decompose-string string t nil))))))
    ((array nil (*)) t)))
