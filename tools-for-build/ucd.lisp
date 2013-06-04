(in-package "SB-COLD")

;;; Common

(defparameter *output-directory*
  (merge-pathnames
   (make-pathname :directory '(:relative :up "output"))
   (make-pathname :directory (pathname-directory *load-truename*))))

(defparameter *page-size-exponent* 8)

(defun cp-high (cp)
  (ash cp (- *page-size-exponent*)))

(defun cp-low (cp)
  (ldb (byte *page-size-exponent* 0) cp))

;;; Generator

(defstruct ucd misc transform)

(defparameter *unicode-character-database*
  (make-pathname :directory (pathname-directory *load-truename*)))

(defparameter *ucd-base* nil)
(defparameter *unicode-names* (make-hash-table))

(defparameter *last-uppercase* nil)
(defparameter *uppercase-transition-count* 0)
(defparameter *different-titlecases* nil)
(defparameter *different-numerics* nil)
(defparameter *name-size* 0)
(defparameter *misc-hash* (make-hash-table :test #'equal))
(defparameter *misc-index* -1)
(defparameter *misc-table* nil)
(defparameter *misc-mapping* nil)
(defparameter *both-cases* nil)
(defparameter *long-decompositions* nil)
(defparameter *decomposition-types* nil)
(defparameter *decomposition-base* nil)

(defun hash-misc (gc-index bidi-index ccc-index decimal-digit digit
                  bidi-mirrored cl-both-case-p decomposition-info)
  (let* ((list (list gc-index bidi-index ccc-index decimal-digit digit
                     bidi-mirrored cl-both-case-p decomposition-info))
         (index (gethash list *misc-hash*)))
    (or index
        (progn
          (vector-push list *misc-table*)
          (setf (gethash list *misc-hash*)
                (incf *misc-index*))))))

(defun gc-index-sort-key (gc-index)
  (or (cdr (assoc gc-index '((1 . 2) (2 . 1)))) gc-index))

(defun compare-misc-entry (left right)
  (destructuring-bind (left-gc-index left-bidi-index left-ccc-index
                       left-decimal-digit left-digit left-bidi-mirrored
                       left-cl-both-case-p left-decomposition-info)
      left
    (destructuring-bind (right-gc-index right-bidi-index right-ccc-index
                         right-decimal-digit right-digit right-bidi-mirrored
                         right-cl-both-case-p right-decomposition-info)
        right
      (or (and left-cl-both-case-p (not right-cl-both-case-p))
          (and (or left-cl-both-case-p (not right-cl-both-case-p))
               (or (< (gc-index-sort-key left-gc-index)
                      (gc-index-sort-key right-gc-index))
                   (and (= left-gc-index right-gc-index)
                        (or (< left-decomposition-info right-decomposition-info)
                            (and (= left-decomposition-info right-decomposition-info)
                                 (or (< left-bidi-index right-bidi-index)
                                     (and (= left-bidi-index right-bidi-index)
                                          (or (< left-ccc-index right-ccc-index)
                                              (and (= left-ccc-index right-ccc-index)
                                                   (or (string< left-decimal-digit
                                                                right-decimal-digit)
                                                       (and (string= left-decimal-digit
                                                                     right-decimal-digit)
                                                            (or (string< left-digit right-digit)
                                                                (and (string= left-digit
                                                                              right-digit)
                                                                     (string< left-bidi-mirrored
                                                                              right-bidi-mirrored))))))))))))))))))

(defun build-misc-table ()
  (let ((table (sort *misc-table* #'compare-misc-entry)))
    ;; after sorting, insert at the end a special entry to handle
    ;; unallocated characters.
    (setf *misc-table* (make-array (1+ (length table))))
    (replace *misc-table* table)
    (setf (aref *misc-table* (length table))
          ;; unallocated characters have a GC index of 31 (not
          ;; colliding with any other GC), are not digits or decimal
          ;; digits, aren't BOTH-CASE-P, don't decompose, and aren't
          ;; interestingly bidi or combining.
          '(31 0 0 "" "" "" nil 0)))
  (setq *misc-mapping* (make-array (1+ *misc-index*)))
  (loop for i from 0 to *misc-index*
     do (setf (aref *misc-mapping*
                    (gethash (aref *misc-table* i) *misc-hash*))
              i)))

(defvar *comp-table*)

(defvar *exclusions*
  (with-open-file (s (make-pathname :name "CompositionExclusions" :type "txt"
                                    :defaults *unicode-character-database*))
    (do ((line (read-line s nil nil) (read-line s nil nil))
         result)
        ((null line) result)
      (when (and (> (length line) 0)
                 (char/= (char line 0) #\#))
        (push (parse-integer line :end (position #\Space line) :radix 16)
              result)))))

(defun slurp-ucd ()
  (setf *comp-table* (make-hash-table :test 'equal))
  (setq *last-uppercase* nil)
  (setq *uppercase-transition-count* 0)
  (setq *different-titlecases* nil)
  (setq *different-numerics* nil)
  (setq *name-size* 0)
  (setq *misc-hash* (make-hash-table :test #'equal))
  (setq *misc-index* -1)
  (setq *misc-table* (make-array 2048 :fill-pointer 0))
  (setq *both-cases* nil)
  (setq *long-decompositions*
        (make-array 2048 :fill-pointer 0 :adjustable t))
  (setq *decomposition-types*
        (let ((array (make-array 256 :initial-element nil :fill-pointer 1)))
          (vector-push "" array)
          (vector-push "<compat>" array)
          array))
  (setq *decomposition-base* (make-array (ash #x110000
                                              (- *page-size-exponent*))
                                         :initial-element nil))
  (setq *ucd-base* (make-array (ash #x110000 (- *page-size-exponent*))
                               :initial-element nil))
  (with-open-file (*standard-input*
                   (make-pathname :name "UnicodeData"
                                  :type "txt"
                                  :defaults *unicode-character-database*)
                   :direction :input)
    (loop for line = (read-line nil nil)
          while line
          do (slurp-ucd-line line)))
  (second-pass)
  (fixup-compositions)
  (fixup-hangul-syllables)
  (build-misc-table)
  (length *long-decompositions*))

(defun fixup-compositions ()
  (flet ((fixup (k v)
           (let* ((cp (car k))
                  (ucd (aref (aref *ucd-base* (cp-high cp)) (cp-low cp)))
                  (misc (aref *misc-table* (ucd-misc ucd)))
                  (ccc-index (third misc)))
             ;; we can do everything in the first pass except for
             ;; accounting for decompositions where the first
             ;; character of the decomposition is not a starter.
             (when (/= ccc-index 0)
               (remhash k *comp-table*)))))
    (maphash #'fixup *comp-table*)))

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
    (with-open-file (*standard-input*
                     (make-pathname :name "Jamo" :type "txt"
                                    :defaults *unicode-character-database*))
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
        (setf (gethash code-point *unicode-names*) name)
        (unless (aref *decomposition-base* (cp-high code-point))
          (setf (aref *decomposition-base* (cp-high code-point))
                (make-array (ash 1 *page-size-exponent*)
                            :initial-element nil)))
        (setf (aref (aref *decomposition-base* (cp-high code-point))
                    (cp-low code-point))
              (cons (if (= tee tbase) 2 3) 0))))))

(defun add-jamo-information (line table)
  (let* ((split (split-string line #\;))
         (code (parse-integer (first split) :radix 16))
         (syllable (string-trim '(#\Space)
                                (subseq (second split) 0 (position #\# (second split))))))
    (setf (gethash code table) syllable)))

(defun split-string (line character)
  (loop for prev-position = 0 then (1+ position)
     for position = (position character line :start prev-position)
     collect (subseq line prev-position position)
     do (unless position
          (loop-finish))))

(defun init-indices (strings)
  (let ((hash (make-hash-table :test #'equal)))
    (loop for string in strings
       for index from 0
       do (setf (gethash string hash) index))
    hash))

(defparameter *general-categories*
  (init-indices '("Lu" "Ll" "Lt" "Lm" "Lo" "Cc" "Cf" "Co" "Cs" "Mc"
                  "Me" "Mn" "Nd" "Nl" "No" "Pc" "Pd" "Pe" "Pf" "Pi"
                  "Po" "Ps" "Sc" "Sk" "Sm" "So" "Zl" "Zp" "Zs")))
(defparameter *bidi-classes*
  (init-indices '("AL" "AN" "B" "BN" "CS" "EN" "ES" "ET" "L" "LRE" "LRO"
                  "NSM" "ON" "PDF" "R" "RLE" "RLO" "S" "WS")))


(defparameter *block-first* nil)

(defun normalize-character-name (name)
  (when (find #\_ name)
    (error "Bad name for a character: ~A" name))
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
    (declare (ignore unicode-1-name iso-10646-comment))
    (if (and (> (length name) 8)
             (string= ", First>" name :start2 (- (length name) 8)))
        (progn
          (setq *block-first* code-point)
          nil)
        (let* ((gc-index (or (gethash general-category *general-categories*)
                             (error "unknown general category ~A"
                                    general-category)))
               (bidi-index (or (gethash bidi-class *bidi-classes*)
                               (error "unknown bidirectional class ~A"
                                      bidi-class)))
               (ccc-index (parse-integer canonical-combining-class))
               (digit-index (unless (string= "" decimal-digit)
                              (parse-integer decimal-digit)))
               (upper-index (unless (string= "" simple-uppercase)
                              (parse-integer simple-uppercase :radix 16)))
               (lower-index (unless (string= "" simple-lowercase)
                              (parse-integer simple-lowercase :radix 16)))
               (title-index (unless (string= "" simple-titlecase)
                              (parse-integer simple-titlecase :radix 16)))
               (cl-both-case-p
                (not (null (or (and (= gc-index 0) lower-index)
                               (and (= gc-index 1) upper-index)
                               ;; deal with prosgegrammeni / titlecase
                               (and (= gc-index 2)
                                    (typep code-point '(integer #x1000 #x1fff))
                                    lower-index)))))
               (decomposition-info 0))
          (declare (ignore digit-index))
          (when (and (not cl-both-case-p)
                     (< gc-index 2))
            (format t "~A~%" name))
          (incf *name-size* (length name))
          (when (string/= "" decomposition-type-and-mapping)
            (let ((split (split-string decomposition-type-and-mapping #\Space)))
              (cond
                ((char= #\< (aref (first split) 0))
                 (unless (position (first split) *decomposition-types*
                                   :test #'equal)
                   (vector-push (first split) *decomposition-types*))
                 (setf decomposition-info (position (pop split) *decomposition-types* :test #'equal)))
                (t (setf decomposition-info 1)))
              (unless (aref *decomposition-base* (cp-high code-point))
                (setf (aref *decomposition-base* (cp-high code-point))
                      (make-array (ash 1 *page-size-exponent*)
                                  :initial-element nil)))
              (setf (aref (aref *decomposition-base* (cp-high code-point))
                          (cp-low code-point))
                    (let ((decomposition
                           (mapcar #'(lambda (string)
                                       (parse-integer string :radix 16))
                                   split)))
                      (when (= decomposition-info 1)
                        ;; Primary composition excludes:
                        ;; * singleton decompositions;
                        ;; * decompositions of non-starters;
                        ;; * script-specific decompositions;
                        ;; * later-version decompositions;
                        ;; * decompositions whose first character is a
                        ;;   non-starter.
                        ;; All but the last case can be handled here;
                        ;; for the fixup, see FIXUP-COMPOSITIONS
                        (when (and (> (length decomposition) 1)
                                   (= ccc-index 0)
                                   (not (member code-point *exclusions*)))
                          (unless (= (length decomposition) 2)
                            (error "canonical decomposition unexpectedly long"))
                          (setf (gethash (cons (first decomposition)
                                               (second decomposition))
                                         *comp-table*)
                                code-point)))
                      (if (= (length decomposition) 1)
                          (cons 1 (car decomposition))
                          (cons (length decomposition)
                                (prog1 (fill-pointer *long-decompositions*)
                                  (dolist (code decomposition)
                                    (vector-push-extend code *long-decompositions*)))))))))
          ;; Hangul decomposition; see Unicode 6.2 section 3-12
          (when (= code-point #xd7a3)
            ;; KLUDGE: it's a bit ugly to do this here when we've got
            ;; a reasonable function to do this in
            ;; (FIXUP-HANGUL-SYLLABLES).  The problem is that the
            ;; fixup would be somewhat tedious to do, what with all
            ;; the careful hashing of misc data going on.
            (setf decomposition-info 1)
            ;; the construction of *decomposition-base* entries is,
            ;; however, easy to handle within FIXUP-HANGUL-SYLLABLES.
            )
          (when (and (string/= "" simple-uppercase)
                     (string/= "" simple-lowercase))
            (push (list code-point upper-index lower-index) *both-cases*))
          (when (string/= simple-uppercase simple-titlecase)
            (push (cons code-point title-index) *different-titlecases*))
          (when (string/= digit numeric)
            (push (cons code-point numeric) *different-numerics*))
          (cond
            ((= gc-index 8)
             (unless *last-uppercase*
               (incf *uppercase-transition-count*))
             (setq *last-uppercase* t))
            (t
             (when *last-uppercase*
               (incf *uppercase-transition-count*))
             (setq *last-uppercase* nil)))
          (when (> ccc-index 255)
            (error "canonical combining class too large ~A" ccc-index))
          (let* ((misc-index (hash-misc gc-index bidi-index ccc-index
                                        decimal-digit digit bidi-mirrored
                                        cl-both-case-p decomposition-info))
                 (result (make-ucd :misc misc-index
                                   :transform (or upper-index lower-index 0))))
            (when (and (> (length name) 7)
                       (string= ", Last>" name :start2 (- (length name) 7)))
              (let ((page-start (ash (+ *block-first*
                                        (ash 1 *page-size-exponent*)
                                        -1)
                                     (- *page-size-exponent*)))
                    (page-end (ash code-point (- *page-size-exponent*))))
                (loop for point from *block-first*
                   below (ash page-start *page-size-exponent*)
                   do (setf (aref (aref *ucd-base* (cp-high point))
                                  (cp-low point))
                            result))
                (loop for page from page-start below page-end
                   do (setf (aref *ucd-base* page)
                            (make-array (ash 1 *page-size-exponent*)
                                        :initial-element result)))
                (loop for point from (ash page-end *page-size-exponent*)
                   below code-point
                   do (setf (aref (aref *ucd-base* (cp-high point))
                                  (cp-low point))
                            result))))
            (values result (normalize-character-name name)))))))

(defun slurp-ucd-line (line)
  (let* ((split-line (split-string line #\;))
         (code-point (parse-integer (first split-line) :radix 16))
         (code-high (ash code-point (- *page-size-exponent*)))
         (code-low (ldb (byte *page-size-exponent* 0) code-point)))
    (unless (aref *ucd-base* code-high)
      (setf (aref *ucd-base* code-high)
            (make-array (ash 1 *page-size-exponent*)
                        :initial-element nil)))
    (multiple-value-bind (encoding name)
        (encode-ucd-line (cdr split-line) code-point)
      (setf (aref (aref *ucd-base* code-high) code-low) encoding
            (gethash code-point *unicode-names*) name))))

;;; this fixes up the case conversion discrepancy between CL and
;;; Unicode: CL operators depend on char-downcase / char-upcase being
;;; inverses, which is not true in general in Unicode even for
;;; characters which change case to single characters.
(defun second-pass ()
  (dotimes (i (length *ucd-base*))
    (let ((base (aref *ucd-base* i)))
      (dotimes (j (length base)) ; base is NIL or an array
        (let ((result (aref base j)))
          (when result
            ;; fixup case mappings for CL/Unicode mismatch
            (let* ((transform-point (ucd-transform result))
                   (transform-high (ash transform-point
                                        (- *page-size-exponent*)))
                   (transform-low (ldb (byte *page-size-exponent* 0)
                                       transform-point)))
              (when (and (plusp transform-point)
                         (/= (ucd-transform
                              (aref (aref *ucd-base* transform-high)
                                    transform-low))
                             (+ (ash i *page-size-exponent*) j)))
                (destructuring-bind (gc-index bidi-index ccc-index
                                     decimal-digit digit bidi-mirrored
                                     cl-both-case-p decomposition-info)
                        (aref *misc-table* (ucd-misc result))
                      (declare (ignore cl-both-case-p))
                      (format t "~A~%" (+ (ash i *page-size-exponent*) j))
                      (setf (ucd-misc result)
                            (hash-misc gc-index bidi-index ccc-index
                                       decimal-digit digit bidi-mirrored
                                       nil decomposition-info)))))))))))

(defun write-4-byte (quadruplet stream)
  (write-byte (ldb (byte 8 24) quadruplet) stream)
  (write-byte (ldb (byte 8 16) quadruplet) stream)
  (write-byte (ldb (byte 8 8) quadruplet) stream)
  (write-byte (ldb (byte 8 0) quadruplet) stream))

(defun digit-to-byte (digit)
  (if (string= "" digit)
      255
      (parse-integer digit)))

(defun output-ucd-data ()
  (let ((hash (make-hash-table :test #'equalp))
        (index 0))
    (loop for page across *ucd-base*
          do (when page
               (unless (gethash page hash)
                 (setf (gethash page hash)
                       (incf index)))))
    (let ((array (make-array (1+ index))))
      (maphash #'(lambda (key value)
                   (setf (aref array value) key))
               hash)
      (setf (aref array 0)
            (make-array (ash 1 *page-size-exponent*) :initial-element nil))
      (with-open-file (stream (make-pathname :name "ucd"
                                             :type "dat"
                                             :defaults *output-directory*)
                              :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (loop for (gc-index bidi-index ccc-index decimal-digit digit
                            bidi-mirrored nil decomposition-info)
              across *misc-table*
              ;; three bits spare here
              do (write-byte gc-index stream)
              ;; three bits spare here
              do (write-byte bidi-index stream)
              do (write-byte ccc-index stream)
              ;; we could save some space here: decimal-digit and
              ;; digit are constrained (CHECKME) to be between 0 and
              ;; 9, so we could encode the pair in a single byte.
              ;; (Also, decimal-digit is equal to digit or undefined,
              ;; so we could encode decimal-digit as a single bit,
              ;; meaning that we could save 11 bits here.
              do (write-byte (digit-to-byte decimal-digit) stream)
              do (write-byte (digit-to-byte digit) stream)
              ;; there's an easy 7 bits to spare here
              do (write-byte (if (string= "N" bidi-mirrored) 0 1) stream)
              ;; at the moment we store information about which type
              ;; of compatibility decomposition is used, costing c.3
              ;; bits.  We could elide that.
              do (write-byte decomposition-info stream)
              do (write-byte 0 stream))
        (loop for page across *ucd-base*
           do (write-byte (if page (gethash page hash) 0) stream))
        (loop for page across array
           do (loop for entry across page
                 do (write-4-byte
                     (dpb (if entry
                              (aref *misc-mapping* (ucd-misc entry))
                              ;; the last entry in *MISC-TABLE* (see
                              ;; BUILD-MISC-TABLE) is special,
                              ;; reserved for the information for
                              ;; characters unallocated by Unicode.
                              (1- (length *misc-table*)))
                          (byte 11 21)
                          (if entry (ucd-transform entry) 0))
                     stream)))))))

;;; KLUDGE: this code, to write out decomposition information, is a
;;; little bit very similar to the ucd entries above.  Try factoring
;;; out the common stuff?
(defun output-decomposition-data ()
  (let ((hash (make-hash-table :test #'equalp))
        (index 0))
    (loop for page across *decomposition-base*
       do (when page
            (unless (gethash page hash)
              (setf (gethash page hash)
                    (prog1 index (incf index))))))
    (let ((array (make-array index)))
      (maphash #'(lambda (key value)
                   (setf (aref array value) key))
               hash)
      (with-open-file (stream (make-pathname :name "decomp" :type "dat"
                                             :defaults *output-directory*)
                              :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (loop for page across *decomposition-base*
           do (write-byte (if page (gethash page hash) 0) stream))
        (loop for page across array
           do (loop for entry across page
                 do (write-4-byte
                     (dpb (if entry (car entry) 0)
                          (byte 11 21)
                          (if entry (cdr entry) 0))
                     stream))))
      (with-open-file (stream (make-pathname :name "ldecomp" :type "dat"
                                             :defaults *output-directory*)
                              :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (loop for code across (copy-seq *long-decompositions*)
           do (write-4-byte code stream))))))

(defun output-composition-data ()
  #+nil ; later
  (let (firsts seconds)
    (flet ((frob (k v)
             (declare (ignore v))
             (pushnew (car k) firsts)
             (pushnew (cdr k) seconds)))
      (maphash #'frob *comp-table*)))
  (with-open-file (stream (make-pathname :name "comp" :type "dat"
                                         :defaults *output-directory*)
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists :supersede :if-does-not-exist :create)
    (maphash (lambda (k v)
               (write-4-byte (car k) stream)
               (write-4-byte (cdr k) stream)
               (write-4-byte v stream))
             *comp-table*)))

(defun output ()
  (output-ucd-data)
  (output-decomposition-data)
  (output-composition-data)
  (with-open-file (f (make-pathname :name "ucd-names" :type "lisp-expr"
                                    :defaults *output-directory*)
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (with-standard-io-syntax
      (write-string ";;; Do not edit by hand: generated by ucd.lisp" f)
      (maphash (lambda (code name)
                 (when name
                   (print code f)
                   (prin1 name f)))
               *unicode-names*))
    (setf *unicode-names* nil))
  (with-open-file (*standard-output*
                   (make-pathname :name "numerics"
                                  :type "lisp-expr"
                                  :defaults *output-directory*)
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
    (with-standard-io-syntax
      (let ((*print-pretty* t))
        (prin1 (mapcar #'(lambda (x) (cons (car x) (read-from-string (cdr x))))
                       *different-numerics*)))))
  (with-open-file (*standard-output*
                   (make-pathname :name "titlecases"
                                  :type "lisp-expr"
                                  :defaults *output-directory*)
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
    (with-standard-io-syntax
      (let ((*print-pretty* t))
        (prin1 *different-titlecases*))))
  (with-open-file (*standard-output*
                   (make-pathname :name "misc"
                                  :type "lisp-expr"
                                  :defaults *output-directory*)
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
    (with-standard-io-syntax
      (let ((*print-pretty* t))
        (prin1 `(:length ,(length *misc-table*)
                         :uppercase ,(loop for (gc-index) across *misc-table*
                                        for i from 0
                                        when (= gc-index 0)
                                        collect i)
                         :lowercase ,(loop for (gc-index) across *misc-table*
                                        for i from 0
                                        when (= gc-index 1)
                                        collect i)
                         :titlecase ,(loop for (gc-index) across *misc-table*
                                        for i from 0
                                        when (= gc-index 2)
                                        collect i))))))
  (values))

;;; Use of the generated files

(defparameter *compiled-ucd* nil)

(defun read-compiled-ucd ()
  (with-open-file (stream (make-pathname :name "ucd"
                                         :type "dat"
                                         :defaults *output-directory*)
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (let ((length (file-length stream)))
      (setq *compiled-ucd*
            (make-array length :element-type '(unsigned-byte 8)))
      (read-sequence *compiled-ucd* stream)))
  (values))

;;; The stuff below is dependent on misc.lisp-expr being
;;;
;;; (:LENGTH 395 :UPPERCASE (0 1 2 3 8 9 10 11) :LOWERCASE (4 5 6 7 12 13 14 15) :TITLECASE (16 17))
;;;
;;; There are two groups of entries for UPPERCASE and LOWERCASE
;;; because some characters have case (by Unicode standards) but are
;;; not transformable character-by-character in a locale-independent
;;; way (as CL requires for its standard operators).
;;;
;;; for more details on these debugging functions, see the description
;;; of the character database format in src/code/target-char.lisp

(defparameter *length* 395)

(defun cp-index (cp)
  (let* ((cp-high (cp-high cp))
         (page (aref *compiled-ucd* (+ (* 8 *length*) cp-high))))
    (+ (* 8 *length*)
       (ash #x110000 (- *page-size-exponent*))
       (* (ash 4 *page-size-exponent*) page)
       (* 4 (cp-low cp)))))

(defun cp-value-0 (cp)
  (let ((index (cp-index cp)))
    (dpb (aref *compiled-ucd* index)
         (byte 8 3)
         (ldb (byte 3 5) (aref *compiled-ucd* (1+ index))))))

(defun cp-value-1 (cp)
  (let ((index (cp-index cp)))
    (dpb (aref *compiled-ucd* (1+ index)) (byte 5 16)
         (dpb (aref *compiled-ucd* (+ index 2)) (byte 8 8)
              (aref *compiled-ucd* (+ index 3))))))

(defun cp-general-category (cp)
  (aref *compiled-ucd* (* 8 (cp-value-0 cp))))

(defun cp-decimal-digit (cp)
  (let ((decimal-digit (aref *compiled-ucd* (+ 3 (* 8 (cp-value-0 cp))))))
    (and (< decimal-digit 10)
         decimal-digit)))

(defun cp-alpha-char-p (cp)
  (< (cp-general-category cp) 5))

(defun cp-alphanumericp (cp)
  (let ((gc (cp-general-category cp)))
    (or (< gc 5)
        (= gc 12))))

(defun cp-digit-char-p (cp &optional (radix 10))
  (let ((number (or (cp-decimal-digit cp)
                    (and (<= 65 cp 90)
                         (- cp 55))
                    (and (<= 97 cp 122)
                         (- cp 87)))))
    (when (and number (< number radix))
      number)))

(defun cp-graphic-char-p (cp)
  (or (<= 32 cp 127)
      (<= 160 cp)))

(defun cp-char-upcase (cp)
  (if (< 3 (cp-value-0 cp) 8)
      (cp-value-1 cp)
      cp))

(defun cp-char-downcase (cp)
  (if (< (cp-value-0 cp) 4)
      (cp-value-1 cp)
      cp))

(defun cp-upper-case-p (cp)
  (< (cp-value-0 cp) 4))

(defun cp-lower-case-p (cp)
  (< 3 (cp-value-0 cp) 8))

(defun cp-both-case-p (cp)
  (< (cp-value-0 cp) 8))
