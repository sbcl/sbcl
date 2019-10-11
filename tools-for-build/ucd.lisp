(in-package "SB-COLD")

(declaim (optimize debug))

;;; Common functions

(defvar *output-directory*
  (merge-pathnames
   (make-pathname :directory '(:relative :up "output"))
   (make-pathname :directory (pathname-directory *load-truename*))))

(defparameter *unicode-character-database*
  (make-pathname :directory (pathname-directory *load-pathname*)))

(defmacro with-input-txt-file ((s name) &body body)
  `(with-open-file (,s (make-pathname :name ,name :type "txt"
                                      :defaults *unicode-character-database*))
     (setf (gethash (format nil "tools-for-build/~A.txt" ,name) *ucd-inputs*) 'used)
     ,@body))

(defmacro with-output-dat-file ((s name) &body body)
  `(with-open-file (,s (make-pathname :name ,name :type "dat"
                                      :defaults *output-directory*)
                       :direction :output :element-type '(unsigned-byte 8)
                       :if-exists :supersede :if-does-not-exist :create)
     (setf (gethash (format nil "output/~A.dat" ,name) *ucd-outputs*) 'made)
     ,@body))

(defmacro with-ucd-output-syntax (&body body)
  `(with-standard-io-syntax
     (let ((*readtable* (copy-readtable))
           (*print-readably* nil)
           (*print-pretty* t))
       ,@body)))

(defmacro with-output-lisp-expr-file ((s name) &body body)
  `(with-open-file (,s (make-pathname :name ,name :type "lisp-expr"
                                      :defaults *output-directory*)
                       :direction :output :element-type 'character
                       :if-exists :supersede :if-does-not-exist :create)
     (setf (gethash (format nil "output/~A.lisp-expr" ,name) *ucd-outputs*) 'made)
     (with-ucd-output-syntax
         ,@body)))

(defun split-string (line character)
  (loop for prev-position = 0 then (1+ position)
     for position = (position character line :start prev-position)
     collect (subseq line prev-position position)
     do (unless position
          (loop-finish))))

(defun parse-codepoints (string &key (singleton-list t))
  "Gets a list of codepoints out of 'aaaa bbbb cccc', stripping surrounding space"
  (let ((list (mapcar
              (lambda (s) (parse-integer s :radix 16))
              (remove "" (split-string string #\Space) :test #'string=))))
    (if (not (or (cdr list) singleton-list)) (car list) list)))


(defun parse-codepoint-range (string)
  "Parse the Unicode syntax DDDD|DDDD..DDDD into an inclusive range (start end)"
  (destructuring-bind (start &optional empty end) (split-string string #\.)
    (declare (ignore empty))
    (let* ((head (parse-integer start :radix 16))
           (tail (if end
                     (parse-integer end :radix 16 :end (position #\Space end))
             head)))
      (list head tail))))

(defvar *slurped-random-constants*
  (with-open-file (f (make-pathname :name "more-ucd-consts" :type "lisp-expr"
                                    :defaults *unicode-character-database*))
    (setf (gethash "tools-for-build/more-ucd-consts.lisp-expr" *ucd-inputs*) 'used)
    (read f)))

(defun init-indices (symbol &aux (strings
                                  (or (cadr (assoc symbol *slurped-random-constants*))
                                      (error "Missing entry in more-ucd-consts for ~S"
                                             symbol))))
  (let ((hash (make-hash-table :test #'equal)))
    (loop for string in strings
       for index from 0
       do (setf (gethash string hash) index))
    hash))

(defun clear-flag (bit integer)
  (logandc2 integer (ash 1 bit)))


;;; Output storage globals
(defstruct ucd misc decomp)

(defparameter *unicode-names* (make-hash-table))
(defparameter *unicode-1-names* (make-hash-table))

(defparameter *decompositions*
  (make-array 10000 :element-type '(unsigned-byte 24) :fill-pointer 0
              :adjustable t)) ; 10000 is not a significant number

(defparameter *decomposition-corrections*
  (with-input-txt-file (s "NormalizationCorrections")
    (loop with result = nil
       for line = (read-line s nil nil) while line
       do (when (position #\; line)
            (destructuring-bind (cp old-decomp correction version)
                (split-string line #\;)
              (declare (ignore old-decomp version))
              (push (cons (parse-integer cp :radix 16)
                          (parse-integer correction :radix 16))
                    result)))
       finally (return result)))
  "List of decompsotions that were amended in Unicode corrigenda")

(defparameter *compositions* (make-hash-table :test #'equal))
(defparameter *composition-exclusions*
  (with-input-txt-file (s "CompositionExclusions")
    (loop with result = nil
       for line = (read-line s nil nil) while line
       when (and (> (length line) 0) (char/= (char line 0) #\#))
       do (push (parse-integer line :end (position #\Space line) :radix 16)
                result) finally (return result)))
  "Characters that are excluded from composition according to UAX#15")

(defparameter *different-titlecases* nil)
(defparameter *different-casefolds* nil)

(defparameter *case-mapping*
  (with-input-txt-file (s "SpecialCasing")
    (loop with hash = (make-hash-table)
       for line = (read-line s nil nil) while line
       unless (or (not (position #\# line)) (= 0 (position #\# line)))
       do (destructuring-bind (%cp %lower %title %upper &optional context comment)
              (split-string line #\;)
            (unless (and context comment)
              (let ((cp (parse-integer %cp :radix 16))
                    (lower (parse-codepoints %lower :singleton-list nil))
                    (title (parse-codepoints %title :singleton-list nil))
                    (upper (parse-codepoints %upper :singleton-list nil)))
                (setf (gethash cp hash) (cons upper lower))
                (unless (equal title upper) (push (cons cp title) *different-titlecases*)))))
         finally (return hash)))
  "Maps cp -> (cons uppercase|(uppercase ...) lowercase|(lowercase ...))")

(defparameter *misc-table* (make-array 3000 :fill-pointer 0)
"Holds the entries in the Unicode database's miscellanious array, stored as lists.
These lists have the form (gc-index bidi-index ccc digit decomposition-info
flags script line-break age). Flags is a bit-bashed integer containing
cl-both-case-p, has-case-p, and bidi-mirrored-p, and an east asian width.
Length should be adjusted when the standard changes.")
(defparameter *misc-hash* (make-hash-table :test #'equal)
"Maps a misc list to its position in the misc table.")

(defparameter *different-numerics* nil)

(defparameter *ucd-entries* (make-hash-table))

(defparameter *general-categories* (init-indices '*general-categories*))
(defparameter *bidi-classes* (init-indices '*bidi-classes*))
(defparameter *east-asian-widths* (init-indices '*east-asian-widths*))
(defparameter *scripts* (init-indices '*scripts*))
(defparameter *line-break-classes* (init-indices '*line-break-classes*))

(defparameter *east-asian-width-table*
  (with-input-txt-file (s "EastAsianWidth")
    (loop with hash = (make-hash-table)
       for line = (read-line s nil nil) while line
       unless (or (not (position #\# line)) (= 0 (position #\# line)))
       do (destructuring-bind (codepoints value)
              (split-string
               (string-right-trim " " (subseq line 0 (position #\# line))) #\;)
            (let ((range (parse-codepoint-range codepoints))
                  (index (gethash value *east-asian-widths*)))
              (loop for i from (car range) to (cadr range)
                 do (setf (gethash i hash) index))))
       finally (return hash)))
  "Table of East Asian Widths. Used in the creation of misc entries.")

(defparameter *script-table*
  (with-input-txt-file (s "Scripts")
    (loop with hash = (make-hash-table)
       for line = (read-line s nil nil) while line
       unless (or (not (position #\# line)) (= 0 (position #\# line)))
       do (destructuring-bind (codepoints value)
              (split-string
               (string-right-trim " " (subseq line 0 (position #\# line))) #\;)
            (let ((range (parse-codepoint-range codepoints))
                  (index (gethash (subseq value 1) *scripts*)))
              (loop for i from (car range) to (cadr range)
                 do (setf (gethash i hash) index))))
       finally (return hash)))
"Table of scripts. Used in the creation of misc entries.")

(defparameter *line-break-class-table*
  (with-input-txt-file (s "LineBreak")
    (loop with hash = (make-hash-table)
       for line = (read-line s nil nil) while line
       unless (or (not (position #\# line)) (= 0 (position #\# line)))
       do (destructuring-bind (codepoints value)
              (split-string
               (string-right-trim " " (subseq line 0 (position #\# line))) #\;)
            (let ((range (parse-codepoint-range codepoints))
                  ;; Hangul syllables temporarily go to "Unkwown"
                  (index (gethash value *line-break-classes* 0)))
              (loop for i from (car range) to (cadr range)
                 do (setf (gethash i hash) index))))
       finally (return hash)))
"Table of line break classes. Used in the creation of misc entries.")

(defparameter *age-table*
  (with-input-txt-file (s "DerivedAge")
    (loop with hash = (make-hash-table)
       for line = (read-line s nil nil) while line
       unless (or (not (position #\# line)) (= 0 (position #\# line)))
       do (destructuring-bind (codepoints value)
              (split-string
               (string-right-trim " " (subseq line 0 (position #\# line))) #\;)
            (let* ((range (parse-codepoint-range codepoints))
                   (age-parts (mapcar #'parse-integer (split-string value #\.)))
                   (age (logior (ash (car age-parts) 3) (cadr age-parts))))
              (loop for i from (car range) to (cadr range)
                 do (setf (gethash i hash) age))))
       finally (return hash)))
"Table of character ages. Used in the creation of misc entries.")

(defvar *block-first* nil)


;;; Unicode data file parsing
(defun hash-misc (gc-index bidi-index ccc digit decomposition-info flags
                  script line-break age)
  (let* ((list (list gc-index bidi-index ccc digit decomposition-info flags
                     script line-break age))
         (index (gethash list *misc-hash*)))
    (or index
        (progn
          (setf (gethash list *misc-hash*)
                (fill-pointer *misc-table*))
          (when (eql nil (vector-push list *misc-table*))
            (error "Misc table too small."))
          (gethash list *misc-hash*)))))

(defun ordered-ranges-member (item vector)
  (labels ((recurse (start end)
             (when (< start end)
               (let* ((i (+ start (truncate (- end start) 2)))
                      (index (* 2 i))
                      (elt1 (svref vector index))
                      (elt2 (svref vector (1+ index))))
                 (cond ((< item elt1)
                        (recurse start i))
                       ((> item elt2)
                        (recurse (+ 1 i) end))
                       (t
                        item))))))
    (recurse 0 (/ (length vector) 2))))

(defun unallocated-bidi-class (code-point)
  ;; See tests/data/DerivedBidiClass.txt for more information
  (flet ((in (vector class)
           (when (ordered-ranges-member code-point vector)
             (gethash class *bidi-classes*))))
    (cond
      ((in
         #(#x0600 #x07BF #x08A0 #x08FF #xFB50 #xFDCF #xFDF0 #xFDFF #xFE70 #xFEFF
           #x1EE00 #x1EEFF) "AL"))
      ((in
         #(#x0590 #x05FF #x07C0 #x089F #xFB1D #xFB4F #x10800 #x10FFF #x1E800 #x1EDFF
           #x1EF00 #x1EFFF) "R"))
      ((in #(#x20A0 #x20CF) "ET"))
      ;; BN is non-characters and default-ignorable.
      ;; Default-ignorable will be dealt with elsewhere
      ((in #(#xFDD0 #xFDEF #xFFFE #xFFFF #x1FFFE #x1FFFF #x2FFFE #x2FFFF
             #x3FFFE #x3FFFF #x4FFFE #x4FFFF #x5FFFE #x5FFFF #x6FFFE #x6FFFF
             #x7FFFE #x7FFFF #x8FFFE #x8FFFF #x9FFFE #x9FFFF #xAFFFE #xAFFFF
             #xBFFFE #xBFFFF #xCFFFE #xCFFFF #xDFFFE #xDFFFF #xEFFFE #xEFFFF
             #xFFFFE #xFFFFF #x10FFFE #x10FFFF)
            "BN"))
      ((in #(#x0 #x10FFFF) "L"))
      (t (error "Somehow we've gone too far in unallocated bidi determination")))))

(defun complete-misc-table ()
  (loop for code-point from 0 to #x10FFFF do ; Flood-fil unallocated codepoints
       (unless (second (multiple-value-list (gethash code-point *ucd-entries*)))
         (let* ((unallocated-misc
                 ;; unallocated characters have a GC of "Cn", aren't digits
                 ;; (digit = 128), have a bidi that depends on their block, and
                 ;; don't decompose, combine, or have case. They have an East
                 ;; Asian Width (eaw) of "N" (0), and a script, line breaking
                 ;; class, and age of 0 ("Unknown"), unless some of those
                 ;; properties are otherwise assigned.
                 `(,(gethash "Cn" *general-categories*)
                    ,(unallocated-bidi-class code-point) 0 128 0
                    ,(gethash code-point *east-asian-width-table* 0)
                    0 ,(gethash code-point *line-break-class-table* 0)
                    ,(gethash code-point *age-table* 0)))
                (unallocated-index (apply #'hash-misc unallocated-misc))
                (unallocated-ucd (make-ucd :misc unallocated-index)))
           (setf (gethash code-point *ucd-entries*) unallocated-ucd)))))

(defun expand-decomposition (decomposition)
  (loop for cp in decomposition
        for ucd = (gethash cp *ucd-entries*)
        for length = (elt (aref *misc-table* (ucd-misc ucd)) 4)
        if (and (not (logbitp 7 length))
                (plusp length))
        append (expand-decomposition (ucd-decomp ucd))
        else
        collect cp))

;;; Recursively expand canonical decompositions
(defun fixup-decompositions ()
  (loop for did-something = nil
        do
        (loop for ucd being each hash-value of *ucd-entries*
              when (and (ucd-decomp ucd)
                        (not (logbitp 7 (elt (aref *misc-table* (ucd-misc ucd)) 4))))
              do
              (let ((expanded (expand-decomposition (ucd-decomp ucd))))
                (unless (equal expanded (ucd-decomp ucd))
                  (setf (ucd-decomp ucd) expanded
                        did-something t))))
        while did-something)
  (loop for i below (hash-table-count *ucd-entries*)
        for ucd = (gethash i *ucd-entries*)
        for decomp = (ucd-decomp ucd)
        do
        (setf (ucd-decomp ucd)
              (cond ((not (consp decomp)) 0)
                    ((logbitp 7 (elt (aref *misc-table* (ucd-misc ucd)) 4))
                     (prog1 (length *decompositions*)
                       (loop for cp in decomp
                             do (vector-push-extend cp *decompositions*))))
                    (t
                     (let ((misc-entry (copy-list (aref *misc-table* (ucd-misc ucd)))))
                       (setf (elt misc-entry 4) (length decomp)
                             (ucd-misc ucd) (apply #'hash-misc misc-entry))
                       (prog1 (length *decompositions*)
                         (loop for cp in decomp
                               do (vector-push-extend cp *decompositions*)))))))))

(defun fixup-compositions ()
  (flet ((fixup (k v)
           (declare (ignore v))
           (let* ((cp (car k))
                  (ucd (gethash cp *ucd-entries*))
                  (misc (aref *misc-table* (ucd-misc ucd)))
                  (ccc (third misc)))
             ;; we can do everything in the first pass except for
             ;; accounting for decompositions where the first
             ;; character of the decomposition is not a starter.
             (when (/= ccc 0)
               (remhash k *compositions*)))))
    (maphash #'fixup *compositions*)))

(defun add-jamo-information (line table)
  (let* ((split (split-string line #\;))
         (code (parse-integer (first split) :radix 16))
         (syllable (string-trim
                    " "
                    (subseq (second split) 0 (position #\# (second split))))))
    (setf (gethash code table) syllable)))

(defun fixup-hangul-syllables ()
  ;; "Hangul Syllable Composition, Unicode 5.1 section 3-12"
  (let* ((sbase #xac00)
         (lbase #x1100)
         (vbase #x1161)
         (tbase #x11a7)
         (scount 11172)
         (lcount 19)
         (vcount 21)
         (tcount 28)
         (ncount (* vcount tcount))
         (table (make-hash-table)))
    (declare (ignore lcount))
    (with-input-txt-file (*standard-input* "Jamo")
      (loop for line = (read-line nil nil)
            while line
            if (position #\; line)
            do (add-jamo-information line table)))
    (dotimes (sindex scount)
      (let* ((l (+ lbase (floor sindex ncount)))
             (v (+ vbase (floor (mod sindex ncount) tcount)))
             (tee (+ tbase (mod sindex tcount)))
             (code-point (+ sbase sindex))
             (name (format nil "HANGUL_SYLLABLE_~A~A~:[~A~;~]"
                           (gethash l table) (gethash v table)
                           (= tee tbase) (gethash tee table))))
        (setf (gethash code-point *unicode-names*) name)))))

(defun normalize-character-name (name)
  (when (find #\_ name)
    (error "Bad name for a character: ~A" name))
  ;; U+1F5CF (PAGE)'s name conflicts with the ANSI CL-assigned
  ;; name for form feed (^L, U+000C). To avoid a case where
  ;; more than one character has a particular name while remaining
  ;; standards-compliant, we remove U+1F5CF's name here.
  (when (string= name "PAGE")
    (return-from normalize-character-name "UNICODE_PAGE"))
  (unless (or (zerop (length name)) (find #\< name) (find #\> name))
    (substitute #\_ #\Space name)))

;;;   3400  --  4DB5  : cjk ideograph extension a ;Lo;0;L;;;;;N;;;;;
;;;   AC00  --  D7A3  : hangul syllables ;Lo;0;L;;;;;N;;;;;
;;;   D800  --  F8FF  : surrogates and private use
;;;  20000  --  2A6D6 : cjk ideograph extension b ;Lo;0;L;;;;;N;;;;;
;;;  F0000  --  FFFFD : private use
;;; 100000  --  10FFFD: private use
(defun encode-ucd-line (line code-point)
  (destructuring-bind (name general-category canonical-combining-class
                            bidi-class decomposition-type-and-mapping
                            decimal-digit digit numeric bidi-mirrored
                            unicode-1-name iso-10646-comment simple-uppercase
                            simple-lowercase simple-titlecase)
      line
    (declare (ignore iso-10646-comment))
    (if (and (> (length name) 8)
             (string= ", First>" name :start2 (- (length name) 8)))
        (progn
          (setf *block-first* code-point)
          nil)
        (let* ((gc-index (or (gethash general-category *general-categories*)
                             (error "unknown general category ~A"
                                    general-category)))
               (bidi-index (or (gethash bidi-class *bidi-classes*)
                               (error "unknown bidirectional class ~A"
                                      bidi-class)))
               (ccc (parse-integer canonical-combining-class))
               (digit-index (if (string= "" digit) 128 ; non-digits have high bit
                                (let ((%digit (parse-integer digit)))
                                  (if (string= digit decimal-digit)
                                      ;; decimal-digit-p is in bit 6
                                      (logior (ash 1 6) %digit) %digit))))
               (upper-index (unless (string= "" simple-uppercase)
                              (parse-integer simple-uppercase :radix 16)))
               (lower-index (unless (string= "" simple-lowercase)
                              (parse-integer simple-lowercase :radix 16)))
               (title-index (unless (string= "" simple-titlecase)
                              (parse-integer simple-titlecase :radix 16)))
               (cl-both-case-p (or (and (= gc-index 0) lower-index)
                                   (and (= gc-index 1) upper-index)))
               (bidi-mirrored-p (string= bidi-mirrored "Y"))
               (decomposition-info 0)
               (eaw-index (gethash code-point *east-asian-width-table*))
               (script-index (gethash code-point *script-table* 0))
               (line-break-index (gethash code-point *line-break-class-table* 0))
               (age-index (gethash code-point *age-table* 0))
               decomposition)
          #+nil
          (when (and (not cl-both-case-p)
                     (< gc-index 2))
            (format t "~A~%" name))

          (when (string/= "" decomposition-type-and-mapping)
            (let* ((compatibility-p (position #\> decomposition-type-and-mapping)))
              (setf decomposition
                    (parse-codepoints
                     (subseq decomposition-type-and-mapping
                             (if compatibility-p (1+ compatibility-p) 0))))
              (when (assoc code-point *decomposition-corrections*)
                (setf decomposition
                      (list (cdr (assoc code-point *decomposition-corrections*)))))
              (setf decomposition-info
                    (logior (length decomposition) (if compatibility-p 128 0)))
              (unless compatibility-p
                ;; Primary composition excludes:
                ;; * singleton decompositions;
                ;; * decompositions of non-starters;
                ;; * script-specific decompositions;
                ;; * later-version decompositions;
                ;; * decompositions whose first character is a
                ;;   non-starter.
                ;; All but the last case can be handled here;
                ;; for the fixup, see FIXUP-COMPOSITIONS
                (when (and (> decomposition-info 1)
                           (= ccc 0)
                           (not (member code-point *composition-exclusions*)))
                  (unless (= decomposition-info 2)
                    (error "canonical decomposition unexpectedly long"))
                  (setf (gethash (cons (first decomposition)
                                       (second decomposition))
                                 *compositions*)
                        code-point)))))
          ;; Hangul decomposition; see Unicode 6.2 section 3-12
          (when (= code-point #xd7a3)
            ;; KLUDGE: The decomposition-length for Hangul syllables in the
            ;; misc database will be a bit of a lie. It doesn't really matter
            ;; since the only purpose of the length is to index into the
            ;; decompositions array (which Hangul decomposition doesn't use).
            ;; The decomposition index is 0 because we won't be going into the
            ;; array
            (setf decomposition-info 3))

          (unless (gethash code-point *case-mapping*) ; Exclude codepoints from SpecialCasing
            (when (string/= simple-uppercase simple-titlecase)
              (push (cons code-point title-index) *different-titlecases*))
            (and (or upper-index lower-index)
                 (setf (gethash code-point *case-mapping*)
                       (cons
                        (or upper-index code-point)
                        (or lower-index code-point)))))

          (when (string/= digit numeric)
            (push (cons code-point numeric) *different-numerics*))

          (when (> ccc 255)
            (error "canonical combining class too large ~A" ccc))
          (let* ((flags (logior
                         (if cl-both-case-p (ash 1 7) 0)
                         (if (gethash code-point *case-mapping*) (ash 1 6) 0)
                         (if bidi-mirrored-p (ash 1 5) 0)
                         eaw-index))
                 (misc-index (hash-misc gc-index bidi-index ccc digit-index
                                        decomposition-info flags script-index
                                        line-break-index age-index))
                 (result (make-ucd :misc misc-index
                                   :decomp decomposition)))
            (when (and (> (length name) 7)
                       (string= ", Last>" name :start2 (- (length name) 7)))
              ;; We can still do this despite East Asian Width being in the
              ;; databasce since each of the UCD <First><Last> blocks
              ;; has a consistent East Asian Width
              (loop for point from *block-first* to code-point do
                   (setf (gethash point *ucd-entries*) result)))
            (values result (normalize-character-name name)
                    (normalize-character-name unicode-1-name)))))))

(defun slurp-ucd-line (line)
  (let* ((split-line (split-string line #\;))
         (code-point (parse-integer (first split-line) :radix 16)))
    (multiple-value-bind (encoding name unicode-1-name)
        (encode-ucd-line (cdr split-line) code-point)
      (setf (gethash code-point *ucd-entries*) encoding
            (gethash code-point *unicode-names*) name)
      (when unicode-1-name
        (setf (gethash code-point *unicode-1-names*) unicode-1-name)))))

;;; this fixes up the case conversion discrepancy between CL and
;;; Unicode: CL operators depend on char-downcase / char-upcase being
;;; inverses, which is not true in general in Unicode even for
;;; characters which change case to single characters.
;;; Also, fix misassigned age values, which are not constant across blocks
(defun second-pass ()
  (let ((case-mapping
         (sort (loop for code-point being the hash-keys in *case-mapping*
                    using (hash-value value)
                    collect (cons code-point value))
               #'< :key #'car)))
    (loop for (code-point upper . lower) in case-mapping
       for misc-index = (ucd-misc (gethash code-point *ucd-entries*))
       for (gc bidi ccc digit decomp flags script lb age) = (aref *misc-table* misc-index)
       when (logbitp 7 flags) do
         (when (or (not (atom upper)) (not (atom lower))
                   (and (= gc 0)
                        (not (equal (car (gethash lower *case-mapping*)) code-point)))
                   (and (= gc 1)
                        (not (equal (cdr (gethash upper *case-mapping*)) code-point))))
           (let* ((new-flags (clear-flag 7 flags))
                  (new-misc (hash-misc gc bidi ccc digit decomp new-flags script lb age)))
             (setf (ucd-misc (gethash code-point *ucd-entries*)) new-misc))))))

(defun fixup-casefolding ()
  ;; KLUDGE: CaseFolding.txt as distributed by Unicode contains a
  ;; non-ASCII character, an eszet, within a comment to act as an
  ;; example.  We can't in general assume that our host lisp will let
  ;; us read that, and we can't portably write that we don't care
  ;; about the text content of anything on a line after a hash because
  ;; text decoding happens at a lower level.  So here we rewrite the
  ;; CaseFolding.txt file to exclude the UTF-8 sequence corresponding
  ;; to the eszet character.
  (with-open-file (in (make-pathname :name "CaseFolding" :type "txt"
                                     :defaults *unicode-character-database*)
                      :element-type '(unsigned-byte 8))
    (setf (gethash "tools-for-build/CaseFolding.txt" *ucd-inputs*) 'used)
    (with-open-file (out (make-pathname :name "CaseFolding" :type "txt"
                                        :defaults *output-directory*)
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create
                         :element-type '(unsigned-byte 8))
      (setf (gethash "output/CaseFolding.txt" *ucd-outputs*) 'made)
      ;; KLUDGE: it's inefficient, though simple, to do the I/O
      ;; byte-by-bite.
      (do ((inbyte (read-byte in nil nil) (read-byte in nil nil))
           (eszet (map '(vector (unsigned-byte 8)) 'char-code "<eszet>"))
           (eszet-count 0)
           (filename "CaseFolding.txt"))
          ((null inbyte)
           (unless (= eszet-count 1)
             (error "Unexpected number of eszets in ~A: ~D"
                    filename eszet-count)))
        (cond
          ((= inbyte #xc3)
           (let ((second (read-byte in nil nil)))
              (cond
                ((null second)
                 (error "No continuation after #xc3 in ~A" filename))
                ((= second #x9f) (incf eszet-count) (write-sequence eszet out))
                (t (error "Unexpected continuation after #xc3 in ~A: #x~X"
                          filename second)))))
          ((>= inbyte #x7f)
           (error "Unexpected octet in ~A: #x~X" filename inbyte))
          (t (write-byte inbyte out))))))
  (with-open-file (s (make-pathname :name "CaseFolding" :type "txt"
                                    :defaults *output-directory*))
    (setf (gethash "tools-for-build/CaseFolding.txt" *ucd-inputs*) 'used)
    (loop for line = (read-line s nil nil)
       while line
       unless (or (not (position #\; line)) (equal (position #\# line) 0))
       do (destructuring-bind (original type mapping comment)
              (split-string line #\;)
            (declare (ignore comment))
            (let ((cp (parse-integer original :radix 16))
                  (fold (parse-codepoints mapping :singleton-list nil)))
              (unless (or (string= type " S") (string= type " T"))
                (when (not (equal (cdr (gethash cp *case-mapping*)) fold))
                  (push (cons cp fold) *different-casefolds*))))))))

(defun fixup-ages ()
  (let ((age (sort
              (loop for code-point being the hash-keys in *age-table*
                 using (hash-value true-age)
                      collect (cons code-point true-age))
              #'< :key #'car)))
    (loop for (code-point . true-age) in age
       for misc-index = (ucd-misc (gethash code-point *ucd-entries*))
       for (gc bidi ccc digit decomp flags script lb age) = (aref *misc-table* misc-index)
       unless (= age true-age) do
         (let* ((new-misc (hash-misc gc bidi ccc digit decomp flags script lb true-age))
                (new-ucd (make-ucd
                          :misc new-misc
                          :decomp (ucd-decomp (gethash code-point *ucd-entries*)))))
           (setf (gethash code-point *ucd-entries*) new-ucd)))))

(defun slurp-ucd ()
  (with-input-txt-file (*standard-input* "UnicodeData")
    (when *load-verbose*
      (format t "~%//slurp-ucd~%"))
    (loop for line = (read-line nil nil)
          while line
          do (slurp-ucd-line line)))
  (second-pass)
  (fixup-compositions)
  (fixup-hangul-syllables)
  (complete-misc-table)
  (fixup-casefolding)
  (fixup-ages)
  (fixup-decompositions)
  nil)


;;; PropList.txt
(defparameter **proplist-properties** nil
  "A list of properties extracted from PropList.txt")

(defun parse-property (stream &optional name)
  (let ((result (make-array 1 :fill-pointer 0 :adjustable t)))
    (loop for line = (read-line stream nil nil)
       for entry = (subseq line 0 (position #\# line))
       ;; Deal with Blah=Blah in DerivedNormalizationProps.txt
       while (and line (not (position #\= (substitute #\Space #\= line :count 1))))
       when (and entry (string/= entry ""))
       do
         (destructuring-bind (start end)
             (parse-codepoint-range (car (split-string entry #\;)))
           (vector-push-extend start result)
           (vector-push-extend end result)))
    (when name
      (push name **proplist-properties**)
      (push result **proplist-properties**))))

(defun slurp-proplist ()
  (with-input-txt-file (s "PropList")
    (parse-property s) ;; Initial comments
    (parse-property s :white-space)
    (parse-property s :bidi-control)
    (parse-property s :join-control)
    (parse-property s :dash)
    (parse-property s :hyphen)
    (parse-property s :quotation-mark)
    (parse-property s :terminal-punctuation)
    (parse-property s :other-math)
    (parse-property s :hex-digit)
    (parse-property s :ascii-hex-digit)
    (parse-property s :other-alphabetic)
    (parse-property s :ideographic)
    (parse-property s :diacritic)
    (parse-property s :extender)
    (parse-property s :other-lowercase)
    (parse-property s :other-uppercase)
    (parse-property s :noncharacter-code-point)
    (parse-property s :other-grapheme-extend)
    (parse-property s :ids-binary-operator)
    (parse-property s :ids-trinary-operator)
    (parse-property s :radical)
    (parse-property s :unified-ideograph)
    (parse-property s :other-default-ignorable-code-point)
    (parse-property s :deprecated)
    (parse-property s :soft-dotted)
    (parse-property s :logical-order-exception)
    (parse-property s :other-id-start)
    (parse-property s :other-id-continue)
    (parse-property s :sterm)
    (parse-property s :variation-selector)
    (parse-property s :pattern-white-space)
    (parse-property s :pattern-syntax))

  (with-input-txt-file (s "DerivedNormalizationProps")
    (parse-property s) ;; Initial comments
    (parse-property s) ;; FC_NFKC_Closure
    (parse-property s) ;; FC_NFKC_Closure
    (parse-property s) ;; Full_Composition_Exclusion
    (parse-property s) ;; NFD_QC Comments
    (parse-property s :nfd-qc)
    (parse-property s) ;; NFC_QC Comments
    (parse-property s :nfc-qc)
    (parse-property s :nfc-qc-maybe)
    (parse-property s) ;; NFKD_QC Comments
    (parse-property s :nfkd-qc)
    (parse-property s) ;; NFKC_QC Comments
    (parse-property s :nfkc-qc)
    (parse-property s :nfkc-qc-maybe))
  (setf **proplist-properties** (nreverse **proplist-properties**))
  (values))


;;; Collation keys
(defvar *maximum-variable-key* 1)

(defun bitpack-collation-key (primary secondary tertiary)
  ;; 0 <= primary <= #xFFFD (default table)
  ;; 0 <= secondary <= #x10C [9 bits]
  ;; 0 <= tertiary <= #x1E (#x1F allowed) [5 bits]
  ;; Because of this, the bit packs don't overlap
  (logior (ash primary 16) (ash secondary 5) tertiary))

(defun parse-collation-line (line)
  (destructuring-bind (%code-points %keys) (split-string line #\;)
    (let* ((code-points (parse-codepoints %code-points))
           (keys
            (remove
             ""
             (split-string (remove #\[ (remove #\Space %keys)) #\]) :test #'string=))
           (ret
            (loop for key in keys
               for variable-p = (position #\* key)
               for parsed =
                 ;; Don't need first value, it's always just ""
                 (cdr (mapcar (lambda (x) (parse-integer x :radix 16 :junk-allowed t))
                              (split-string (substitute #\. #\* key) #\.)))
               collect
                 (destructuring-bind (primary secondary tertiary) parsed
                   (when variable-p (setf *maximum-variable-key*
                                          (max primary *maximum-variable-key*)))
                   (bitpack-collation-key primary secondary tertiary)))))
    (values code-points ret))))

(defparameter *collation-table*
  (with-input-txt-file (stream "allkeys")
    (loop with hash = (make-hash-table :test #'equal)
       for line = (read-line stream nil nil) while line
       unless (eql 0 (position #\# line))
       do (multiple-value-bind (codepoints keys) (parse-collation-line line)
            (setf (gethash codepoints hash) keys))
       finally (return hash))))


;;; Other properties
(defparameter *confusables*
  (with-input-txt-file (stream "ConfusablesEdited")
    (read-line stream)
    (loop for line = (read-line stream nil nil)
          while line
          when (and (not (equal line ""))
                    (char/= (char line 0) #\#))
          collect
          (flet ((parse (x)
                   (mapcar (lambda (x)
                             (parse-integer x :radix 16))
                           (split-string x #\Space))))
            (let* ((semicolon (position #\; line))
                   (semicolon2 (position #\; line :start (1+ semicolon)))
                   (from (parse (subseq line 0 (1- semicolon))))
                   (to (parse (subseq line (+ semicolon 2) (1- semicolon2)))))
              (assert (= (length from) 1))
              (list* (car from) to)))))
  "List of confusable codepoint sets")

(defparameter *bidi-mirroring-glyphs*
  (with-input-txt-file (s "BidiMirroring")
    (loop for line = (read-line s nil nil) while line
          when (and (plusp (length line))
                    (char/= (char line 0) #\#))
          collect
          (mapcar
           #'(lambda (c) (parse-codepoints c :singleton-list nil))
           (split-string (subseq line 0 (position #\# line)) #\;))))
  "List of BIDI mirroring glyph pairs")

(defparameter *blocks*
  (let (ranges names)
    (with-input-txt-file (stream "Blocks")
      (loop
       (let ((line (read-line stream nil nil)))
         (cond ((not line) (return))
               ((or (string= line "") (position #\# line))) ; ignore
               (t (let* ((split (split-string line #\;))
                         (range (parse-codepoint-range (car split))))
                    (setq ranges (list* (cadr range) (car range) ranges))
                    (push (nsubstitute #\- #\Space
                                       (string-left-trim " " (cadr split)))
                          names)))))))
    (cons (nreverse (coerce ranges 'vector)) (nreverse names)))
  "Vector of block starts and ends in a form acceptable to `ordered-ranges-position`.
Used to look up block data.")

;;; Output code
(defun write-codepoint (code-point stream)
  (declare (type (unsigned-byte 32) code-point))
  (write-byte (ldb (byte 8 16) code-point) stream)
  (write-byte (ldb (byte 8 8) code-point) stream)
  (write-byte (ldb (byte 8 0) code-point) stream))

(defun write-4-byte (value stream)
  (declare (type (unsigned-byte 32) value))
  (write-byte (ldb (byte 8 24) value) stream)
  (write-byte (ldb (byte 8 16) value) stream)
  (write-byte (ldb (byte 8 8) value) stream)
  (write-byte (ldb (byte 8 0) value) stream))

(defun output-misc-data ()
  (with-output-dat-file (stream "ucdmisc")
    (loop for (gc-index bidi-index ccc digit decomposition-info flags
                        script line-break age)
       across *misc-table*
       ;; three bits spare here
       do (write-byte gc-index stream)
       ;; three bits spare here
         (write-byte bidi-index stream)
         (write-byte ccc stream)
       ;; bits 0-3 encode [0,9], bit 7 is for non-digit status,
       ;; bit 6 is the decimal-digit flag. Two bits spare
         (write-byte digit stream)
         (write-byte decomposition-info stream)
         (write-byte flags stream) ; includes EAW in bits 0-3, bit 4 is free
         (write-byte script stream)
         (write-byte line-break stream)
         (write-byte age stream))))

(defun output-ucd-data ()
  (with-output-dat-file (high-pages "ucdhigh")
    (with-output-dat-file (low-pages "ucdlow")
      ;; Output either the index into the misc array (if all the points in the
      ;; high-page have the same misc value) or an index into the law-pages
      ;; array / 256. For indexes into the misc array, set bit 15 (high bit).
      ;; We should never have that many misc entries, so that's not a problem.

      ;; If Unicode ever allocates an all-decomposing <First>/<Last> block (the
      ;; only way to get a high page that outputs as the same and has a
      ;; non-zero decomposition-index, which there's nowhere to store now),
      ;; find me, slap me with a fish, and have fun fixing this mess.
      (loop with low-pages-index = 0
         for high-page from 0 to (ash #x10FFFF -8)
         for uniq-ucd-entries = nil do
           (loop for low-page from 0 to #xFF do
                (pushnew
                 (gethash (logior low-page (ash high-page 8)) *ucd-entries*)
                 uniq-ucd-entries :test #'equalp))
           (flet ((write-2-byte (int stream)
                    (declare (type (unsigned-byte 16) int))
                    (write-byte (ldb (byte 8 8) int) stream)
                    (write-byte (ldb (byte 8 0) int) stream)))
             (case (length uniq-ucd-entries)
               (0 (error "Somehow, a high page has no codepoints in it."))
               (1 (write-2-byte (logior
                                 (ash 1 15)
                                 (ucd-misc (car uniq-ucd-entries)))
                                high-pages))
               (t (loop for low-page from 0 to #xFF
                     for cp = (logior low-page (ash high-page 8))
                     for entry = (gethash cp *ucd-entries*) do
                       (write-2-byte (ucd-misc entry) low-pages)
                       (write-2-byte (ucd-decomp entry) low-pages)
                     finally (write-2-byte low-pages-index high-pages)
                       (incf low-pages-index)))))
         finally (assert (< low-pages-index (ash 1 15)))
                 (when *load-print*
                   (print low-pages-index))))))

(defun output-decomposition-data ()
  (with-output-dat-file (stream "decomp")
    (loop for cp across *decompositions* do
         (write-codepoint cp stream)))
  (when *load-print*
    (print (length *decompositions*))))

(defun output-composition-data ()
  (with-output-dat-file (stream "comp")
    (let (comp)
      (maphash (lambda (k v) (push (cons k v) comp)) *compositions*)
      (setq comp (sort comp #'< :key #'cdr))
      (loop for (k . v) in comp
         do (write-codepoint (car k) stream)
           (write-codepoint (cdr k) stream)
           (write-codepoint v stream)))))

(defun output-case-data ()
  (let (casing-pages points-with-case)
    (with-output-dat-file (stream "case")
      (loop for cp being the hash-keys in *case-mapping*
           do (push cp points-with-case))
      (setf points-with-case (sort points-with-case #'<))
      (loop for cp in points-with-case
         for (upper . lower) = (gethash cp *case-mapping*) do
           (pushnew (ash cp -6) casing-pages)
           (write-codepoint cp stream)
           (write-byte (if (atom upper) 0 (length upper)) stream)
           (if (atom upper) (write-codepoint upper stream)
               (map 'nil (lambda (c) (write-codepoint c stream)) upper))
           (write-byte (if (atom lower) 0 (length lower)) stream)
           (if (atom lower) (write-codepoint lower stream)
               (map 'nil (lambda (c) (write-codepoint c stream)) lower))))
    (setf casing-pages (sort casing-pages #'<))
    (assert (< (length casing-pages) 256))
    (let* ((size (1+ (reduce #'max casing-pages)))
           (array (make-array size :initial-element 255))
           (page -1))
      (dolist (entry casing-pages)
        (setf (aref array entry) (incf page)))
      (with-output-dat-file (stream "casepages")
        (dotimes (i size)
          (write-byte (aref array i) stream))))
    (with-output-lisp-expr-file (stream "casepages")
      (print casing-pages stream))))

(defun output-collation-data ()
  (with-output-dat-file (stream "collation")
    (flet ((length-tag (list1 list2)
             ;; takes two lists of UB32 (with the caveat that list1[0]
             ;; needs its high 8 bits free (codepoints always have
             ;; that) and do
             (let* ((l1 (length list1)) (l2 (length list2))
                    (tag (dpb l1 (byte 4 28) (dpb l2 (byte 5 23) (car list1)))))
               (assert (<= l1 3))
               (write-4-byte tag stream)
               (map nil #'(lambda (l) (write-4-byte l stream)) (append (cdr list1) list2)))))
      (let (coll)
        (maphash (lambda (k v) (push (cons k v) coll)) *collation-table*)
        (labels ((sorter (o1 o2)
                   (cond
                     ((null o1) t)
                     ((null o2) nil)
                     (t (or (< (car o1) (car o2))
                            (and (= (car o1) (car o2))
                                 (sorter (cdr o1) (cdr o2))))))))
          (setq coll (sort coll #'sorter :key #'car)))
        (loop for (k . v) in coll
           do (length-tag k v)))))
  (with-output-lisp-expr-file (*standard-output* "other-collation-info")
    (write-string ";;; The highest primary variable collation index")
    (terpri)
    (prin1 *maximum-variable-key*) (terpri))
  (with-output-lisp-expr-file (*standard-output* "n-collation-entries")
    (write-string ";;; The number of entries in the collation table")
    (terpri)
    (prin1 (hash-table-count *collation-table*))
    (terpri)))

(defun output (&optional (*output-directory* *output-directory*))
  (output-misc-data)
  (output-ucd-data)
  (output-decomposition-data)
  (output-composition-data)
  (output-case-data)
  (output-collation-data)
  (with-output-lisp-expr-file (*standard-output* "misc-properties")
    (prin1 **proplist-properties**))

  (with-output-lisp-expr-file (f "ucd-names")
    (write-string ";;; Do not edit by hand: generated by ucd.lisp" f)
    (maphash (lambda (code name)
               (when name
                 (print code f)
                 (prin1 name f)))
             *unicode-names*)
    (setf *unicode-names* nil))
  (with-output-lisp-expr-file (f "ucd1-names")
    (write-string ";;; Do not edit by hand: generated by ucd.lisp" f)
    (maphash (lambda (code name)
               (when name
                 (print code f)
                 (prin1 name f)))
             *unicode-1-names*)
    (setf *unicode-1-names* nil))

  (with-output-lisp-expr-file (*standard-output* "numerics")
    (let ((result (make-array (* (length *different-numerics*) 2))))
      (loop for (code . value) in (sort *different-numerics* #'< :key #'car)
         for i by 2
         do (setf (aref result i) code
                  (aref result (1+ i)) (read-from-string value)))
      (prin1 result)))
  (with-output-lisp-expr-file (*standard-output* "titlecases")
    (prin1 *different-titlecases*))
  (with-output-lisp-expr-file (*standard-output* "foldcases")
    (prin1 *different-casefolds*))
  (with-output-lisp-expr-file (*standard-output* "confusables")
    (prin1 *confusables*))
  (with-output-lisp-expr-file (*standard-output* "bidi-mirrors")
    (prin1 *bidi-mirroring-glyphs*))
  (with-output-lisp-expr-file (*standard-output* "block-ranges")
    (prin1 (car *blocks*)))
  (with-output-lisp-expr-file (*standard-output* "block-names")
    (format t "#(~{:~A~^~%  ~})" (cdr *blocks*)))
  (values))
