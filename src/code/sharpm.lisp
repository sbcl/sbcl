;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(declaim (special *read-suppress*))

;;; FIXME: Is it standard to ignore numeric args instead of raising errors?
(defun ignore-numarg (sub-char numarg)
  (when numarg
    (warn "A numeric argument was ignored in #~W~A." numarg sub-char)))

;;;; reading arrays and vectors: the #(, #*, and #A readmacros

(defun sharp-left-paren (stream ignore length)
  (declare (ignore ignore))
  (let* ((list (read-list stream nil))
         (list-length
          (handler-case (length list)
            (type-error ()
              (simple-reader-error stream "Improper list in #(): ~S." list)))))
    (declare (list list)
             (fixnum list-length))
    (cond (*read-suppress* nil)
          ((and length (> list-length length))
           (simple-reader-error
            stream
            "Vector longer than the specified length: #~S~S."
            length list))
          (length
           (when (and (plusp length) (null list))
             (simple-reader-error
              stream "Vector of length ~D can't be initialized from ()" length))
           ;; the syntax `#n(foo ,@bar) is not well-defined. [See lp#1096043.]
           ;; We take it to mean that the vector as read should be padded to
           ;; length 'n'. It could be argued that 'n' is the length after
           ;; expansion, but that's not easy, not to mention unportable.
           (fill (replace (make-array length) list)
                 (car (last list)) :start list-length))
          (t
           (coerce list 'vector)))))

(defun sharp-star (stream ignore numarg)
  (declare (ignore ignore))
  (declare (type (or null fixnum) numarg))
  (binding* (((buffer escape-appearedp) (read-extended-token stream))
             (input-len (token-buf-fill-ptr buffer))
             (bstring (token-buf-string buffer)))
    (cond (*read-suppress* nil)
          (escape-appearedp
           (simple-reader-error stream
                                "An escape character appeared after #*."))
          ((and numarg (zerop input-len) (not (zerop numarg)))
           (simple-reader-error
            stream
            "You have to give a little bit for non-zero #* bit-vectors."))
          ((or (null numarg) (>= numarg input-len))
           (do ((bvec
                 (make-array (or numarg input-len)
                             :element-type 'bit
                             :initial-element
                             (if (and (plusp input-len)
                                      (char= (char bstring (1- input-len)) #\1))
                                 1 0)))
                (i 0 (1+ i)))
               ((= i input-len) bvec)
             (declare (index i) (optimize (sb!c::insert-array-bounds-checks 0)))
             (let ((char (char bstring i)))
               (setf (elt bvec i)
                     (case char
                       (#\0 0)
                       (#\1 1)
                       (t (simple-reader-error
                           stream "illegal element given for bit-vector: ~S"
                           char)))))))
          (t
           (simple-reader-error
            stream
            "Bit vector is longer than specified length #~A*~A"
            numarg
            (copy-token-buf-string buffer))))))

(defun sharp-A (stream ignore dimensions)
  (declare (ignore ignore))
  (when *read-suppress*
    (read stream t nil t)
    (return-from sharp-A nil))
  (unless dimensions
    (simple-reader-error stream "No dimensions argument to #A."))
  (collect ((dims))
    (let* ((*bq-error*
            (if (zerop *backquote-depth*)
                *bq-error*
                "Comma inside a backquoted array (not a list or general vector.)"))
           (*backquote-depth* 0)
           (contents (read stream t nil t))
           (seq contents))
      (dotimes (axis dimensions
                     (make-array (dims) :initial-contents contents))
        (unless (typep seq 'sequence)
          (simple-reader-error stream
                               "#~WA axis ~W is not a sequence:~%  ~S"
                               dimensions axis seq))
        (let ((len (length seq)))
          (dims len)
          (unless (or (= axis (1- dimensions))
                      ;; ANSI: "If some dimension of the array whose
                      ;; representation is being parsed is found to be
                      ;; 0, all dimensions to the right (i.e., the
                      ;; higher numbered dimensions) are also
                      ;; considered to be 0."
                      (= len 0))
            (setq seq (elt seq 0))))))))

;;;; reading structure instances: the #S readmacro

(defun sharp-S (stream sub-char numarg)
  (ignore-numarg sub-char numarg)
  (when *read-suppress*
    (read stream t nil t)
    (return-from sharp-S nil))
  (let* ((*bq-error*
          (if (zerop *backquote-depth*)
              *bq-error*
              "Comma inside backquoted structure (not a list or general vector.)"))
         (body (if (char= (read-char stream t) #\( )
                  (let ((*backquote-depth* 0))
                    (read-list stream nil))
                  (simple-reader-error stream "non-list following #S"))))
    (unless (listp body)
      (simple-reader-error stream "non-list following #S: ~S" body))
    (unless (symbolp (car body))
      (simple-reader-error stream
                           "Structure type is not a symbol: ~S"
                           (car body)))
    (let ((classoid (find-classoid (car body) nil)))
      (unless (typep classoid 'structure-classoid)
        (simple-reader-error stream
                             "~S is not a defined structure type."
                             (car body)))
      (let ((default-constructor (dd-default-constructor
                                  (layout-info (classoid-layout classoid)))))
        (unless default-constructor
          (simple-reader-error
           stream
           "The ~S structure does not have a default constructor."
           (car body)))
        (when (and (atom (rest body))
                   (not (null (rest body))))
          (simple-reader-error stream "improper list for #S: ~S." body))
        (apply (fdefinition default-constructor)
               (loop for tail on (rest body) by #'cddr
                     with slot-name = (and (consp tail) (car tail))
                     do (progn
                          (when (null (cdr tail))
                            (simple-reader-error
                             stream
                             "the arglist for the ~S constructor in #S ~
                              has an odd length: ~S."
                             (car body) (rest body)))
                          (when (or (atom (cdr tail))
                                    (and (atom (cddr tail))
                                         (not (null (cddr tail)))))
                            (simple-reader-error
                             stream
                             "the arglist for the ~S constructor in #S ~
                              is improper: ~S."
                             (car body) (rest body)))
                          (when (not (typep (car tail) 'string-designator))
                            (simple-reader-error
                             stream
                             "a slot name in #S is not a string ~
                              designator: ~S."
                             slot-name))
                          (when (not (keywordp slot-name))
                            (warn 'structure-initarg-not-keyword
                                  :format-control
                                  "in #S ~S, the use of non-keywords ~
                                   as slot specifiers is deprecated: ~S."
                                  :format-arguments
                                  (list (car body) slot-name))))
                     collect (intern (string (car tail)) *keyword-package*)
                     collect (cadr tail)))))))

;;;; reading numbers: the #B, #C, #O, #R, and #X readmacros

(defun sharp-B (stream sub-char numarg)
  (ignore-numarg sub-char numarg)
  (sharp-R stream sub-char 2))

(defun sharp-C (stream sub-char numarg)
  (ignore-numarg sub-char numarg)
  ;; The next thing had better be a list of two numbers.
  (let ((cnum (read stream t nil t)))
    (when *read-suppress* (return-from sharp-C nil))
    (if (and (listp cnum) (= (length cnum) 2))
        (complex (car cnum) (cadr cnum))
        (simple-reader-error stream
                             "illegal complex number format: #C~S"
                             cnum))))

(defun sharp-O (stream sub-char numarg)
  (ignore-numarg sub-char numarg)
  (sharp-R stream sub-char 8))

(defun sharp-R (stream sub-char radix)
  (cond (*read-suppress*
         (read-extended-token stream)
         nil)
        ((not radix)
         (simple-reader-error stream "radix missing in #R"))
        ((not (<= 2 radix 36))
         (simple-reader-error stream "illegal radix for #R: ~D." radix))
        (t
         ;; FIXME: (read-from-string "#o#x1f") should not work!
         ;; The token should be comprised strictly of digits in the radix,
         ;; though the docs say this is undefined behavior, so it's ok,
         ;; other than it being something we should complain about
         ;; for portability reasons.
         (let ((res (let ((*read-base* radix))
                      (read stream t nil t))))
           (unless (typep res 'rational)
             (simple-reader-error stream
                                  "#~A (base ~D.) value is not a rational: ~S."
                                  sub-char
                                  radix
                                  res))
           res))))

(defun sharp-X (stream sub-char numarg)
  (ignore-numarg sub-char numarg)
  (sharp-R stream sub-char 16))

;;;; reading circular data: the #= and ## readmacros

;;; objects already seen by CIRCLE-SUBST
(defvar *sharp-equal-circle-table*)
(declaim (type hash-table *sharp-equal-circle-table*))

;; This function is kind of like NSUBLIS, but checks for circularities and
;; substitutes in arrays and structures as well as lists. The first arg is an
;; alist of the things to be replaced assoc'd with the things to replace them.
(defun circle-subst (old-new-alist tree)
  (cond ((not (typep tree '(or cons (array t) instance funcallable-instance)))
         (let ((entry (find tree old-new-alist :key #'second)))
           (if entry (third entry) tree)))
        ((null (gethash tree *sharp-equal-circle-table*))
         (setf (gethash tree *sharp-equal-circle-table*) t)
         (cond ((consp tree)
                (let ((a (circle-subst old-new-alist (car tree)))
                      (d (circle-subst old-new-alist (cdr tree))))
                  (unless (eq a (car tree))
                    (rplaca tree a))
                  (unless (eq d (cdr tree))
                    (rplacd tree d))))
               ((arrayp tree)
                (with-array-data ((data tree) (start) (end))
                  (declare (fixnum start end))
                  (do ((i start (1+ i)))
                      ((>= i end))
                    (let* ((old (aref data i))
                           (new (circle-subst old-new-alist old)))
                      (unless (eq old new)
                        (setf (aref data i) new))))))
               ((typep tree 'instance)
                ;; We don't grovel the layout.
                (do-instance-tagged-slot (i tree)
                  (let* ((old (%instance-ref tree i))
                         (new (circle-subst old-new-alist old)))
                    (unless (eq old new)
                      (setf (%instance-ref tree i) new)))))
               ((typep tree 'funcallable-instance)
                (do ((i 1 (1+ i))
                     (end (- (1+ (get-closure-length tree)) sb!vm:funcallable-instance-info-offset)))
                    ((= i end))
                  (let* ((old (%funcallable-instance-info tree i))
                         (new (circle-subst old-new-alist old)))
                    (unless (eq old new)
                      (setf (%funcallable-instance-info tree i) new))))))
         tree)
        (t tree)))

;;; Sharp-equal works as follows. When a label is assigned (i.e. when
;;; #= is called) we GENSYM a symbol is which is used as an
;;; unforgeable tag. *SHARP-SHARP-ALIST* maps the integer tag to this
;;; gensym.
;;;
;;; When SHARP-SHARP encounters a reference to a label, it returns the
;;; symbol assoc'd with the label. Resolution of the reference is
;;; deferred until the read done by #= finishes. Any already resolved
;;; tags (in *SHARP-EQUAL-ALIST*) are simply returned.
;;;
;;; After reading of the #= form is completed, we add an entry to
;;; *SHARP-EQUAL-ALIST* that maps the gensym tag to the resolved
;;; object. Then for each entry in the *SHARP-SHARP-ALIST, the current
;;; object is searched and any uses of the gensysm token are replaced
;;; with the actual value.
(defvar *sharp-sharp-alist* ())

(defun sharp-equal (stream ignore label)
  (declare (ignore ignore))
  (when *read-suppress* (return-from sharp-equal (values)))
  (unless label
    (simple-reader-error stream "missing label for #=" label))
  (when (or (assoc label *sharp-sharp-alist*)
            (assoc label *sharp-equal-alist*))
    (simple-reader-error stream "multiply defined label: #~D=" label))
  (let* ((tag (gensym))
         (*sharp-sharp-alist* (acons label tag *sharp-sharp-alist*))
         (obj (read stream t nil t)))
    (when (eq obj tag)
      (simple-reader-error stream
                     "must tag something more than just #~D#"
                     label))
    (push (list label tag obj) *sharp-equal-alist*)
    (let ((*sharp-equal-circle-table* (make-hash-table :test 'eq :size 20)))
      (circle-subst *sharp-equal-alist* obj))))

(defun sharp-sharp (stream ignore label)
  (declare (ignore ignore))
  (when *read-suppress* (return-from sharp-sharp nil))
  (unless label
    (simple-reader-error stream "missing label for ##" label))

  (let ((entry (assoc label *sharp-equal-alist*)))
    (if entry
        (third entry)
        (let (;; Has this label been defined previously? (Don't read
              ;; ANSI "2.4.8.15 Sharpsign Equal-Sign" and worry that
              ;; it requires you to implement forward references,
              ;; because forward references are disallowed in
              ;; "2.4.8.16 Sharpsign Sharpsign".)
              (pair (assoc label *sharp-sharp-alist*)))
          (unless pair
            (simple-reader-error stream
                                 "reference to undefined label #~D#"
                                 label))
          (cdr pair)))))

;;;; conditional compilation: the #+ and #- readmacros

(defun sharp-plus-minus (stream sub-char numarg)
    (ignore-numarg sub-char numarg)
    (if (char= (if (featurep (let ((*package* *keyword-package*)
                                   (*reader-package* nil)
                                   (*read-suppress* nil))
                               (read stream t nil t)))
                   #\+ #\-) sub-char)
        (read stream t nil t)
        (let ((*read-suppress* t))
          (read stream t nil t)
          (values))))

;;;; reading miscellaneous objects: the #P, #\, and #| readmacros

(defun sharp-P (stream sub-char numarg)
  (ignore-numarg sub-char numarg)
  (let ((namestring (read stream t nil t)))
    (unless *read-suppress*
      (parse-namestring namestring))))

(defun sharp-backslash (stream backslash numarg)
  (ignore-numarg backslash numarg)
  (let ((buf (read-extended-token-escaped stream)))
    (cond (*read-suppress* nil)
          ((= (token-buf-fill-ptr buf) 1)
           (char (token-buf-string buf) 0))
          ;; NAME-CHAR is specified as case-insensitive
          ((name-char (sized-token-buf-string buf)))
          (t
           (simple-reader-error stream
                                "unrecognized character name: ~S"
                                (copy-token-buf-string buf))))))

(defun sharp-vertical-bar (stream sub-char numarg)
  (ignore-numarg sub-char numarg)
  (handler-bind
      ((character-decoding-error
        #'(lambda (decoding-error)
            (declare (ignorable decoding-error))
            (style-warn
             'sb!kernel::character-decoding-error-in-dispatch-macro-char-comment
             :sub-char sub-char :position (file-position stream) :stream stream)
            (invoke-restart 'attempt-resync))))
    (let ((stream (in-synonym-of stream)))
      (macrolet ((munch (get-char &optional finish)
                   `(do ((level 1)
                         (prev ,get-char char)
                         (char ,get-char ,get-char))
                        (())
                      (cond ((and (char= prev #\|) (char= char #\#))
                             (setq level (1- level))
                             (when (zerop level)
                               ,finish
                               (return (values)))
                             (setq char ,get-char))
                            ((and (char= prev #\#) (char= char #\|))
                             (setq char ,get-char)
                             (setq level (1+ level)))))))
        (if (ansi-stream-p stream)
            (prepare-for-fast-read-char stream
              (munch (fast-read-char) (done-with-fast-read-char)))
            ;; fundamental-stream
            (munch (read-char stream t)))))))

;;;; a grab bag of other sharp readmacros: #', #:, and #.

(defun sharp-quote (stream sub-char numarg)
  (ignore-numarg sub-char numarg)
  ;; The fourth arg tells READ that this is a recursive call.
  `(function ,(read stream t nil t)))

;;; Read an uninterned symbol.
;;; Unescaped whitespace terminates the token, however a token comprised
;;; of zero characters is an edge-case that is not extremely portable other
;;; than for a few well-known uses, such as the incontrovertible fact that
;;; "#* foo" is two tokens: an empty bit-vector followed by a symbol.
;;; But different behaviors can be observed for #: in other implementations:
;;;  (read-from-string "#:  foo") => #:FOO
;;;  (read-from-string "#:  foo") => ERROR "token expected"
(defun sharp-colon (stream sub-char numarg)
  (ignore-numarg sub-char numarg)
  (multiple-value-bind (buffer escapep colon) (read-extended-token stream)
    (unless *read-suppress*
      (casify-read-buffer buffer)
      (let ((token (copy-token-buf-string buffer)))
        (cond (colon
               (simple-reader-error
                stream "The symbol following #: contains a package marker: ~S"
                token))
              ;; We'd like to signal errors on tokens that look like numbers,
              ;; but doing that is actually nontrivial. None of the possible
              ;; ways to test for numeric syntax are great:
              ;; - using SYMBOL-QUOTEP to see if it says that the symbol would
              ;;   print using escapes could produce false positives
              ;;   because it's seldom wrong to use vertical bars.
              ;; - calling READ-FROM-STRING to see if it returns a number
              ;;   would demand a new string stream.
              ;; - a potential number with _ and/or ^ should not be allowed.
              ;; An acceptable rough cut is to use PARSE-INTEGER even though it
              ;; won't help to reject ratios or floating point syntax.
              ((and (not escapep)
                    (multiple-value-bind (num end)
                        (parse-integer token :radix *read-base* :junk-allowed t)
                      (and num (= end (length token)))))
               (simple-reader-error
                stream "The symbol following #: has numeric syntax: ~S"
                token))
              (t
               (make-symbol token)))))))

(defvar *read-eval* t
  #!+sb-doc
  "If false, then the #. read macro is disabled.")

(defun sharp-dot (stream sub-char numarg)
  (ignore-numarg sub-char numarg)
  (let ((*backquote-depth* 0))
    (let ((expr (read stream t nil t)))
      (unless *read-suppress*
        (unless *read-eval*
          (simple-reader-error stream "can't read #. while *READ-EVAL* is NIL"))
        (eval expr)))))

(defun sharp-illegal (stream sub-char ignore)
  (declare (ignore ignore))
  (simple-reader-error stream "illegal sharp macro character: ~S" sub-char))

;;; for cold init: Install SHARPM stuff in the current *READTABLE*.
(defun !sharpm-cold-init ()
  (make-dispatch-macro-character #\# t)
  (set-dispatch-macro-character #\# #\\ #'sharp-backslash)
  (set-dispatch-macro-character #\# #\' #'sharp-quote)
  (set-dispatch-macro-character #\# #\( #'sharp-left-paren)
  (set-dispatch-macro-character #\# #\* #'sharp-star)
  (set-dispatch-macro-character #\# #\: #'sharp-colon)
  (set-dispatch-macro-character #\# #\. #'sharp-dot)
  ;; This used to set the dispatch-function for pairs of alphabetics, but
  ;; {SET,GET}-DISPATCH-MACRO-CHARACTER and READ-DISPATCH-CHAR
  ;; all use CHAR-UPCASE on the sub-char, so it makes no difference.
  (set-dispatch-macro-character #\# #\r #'sharp-R)
  (set-dispatch-macro-character #\# #\b #'sharp-B)
  (set-dispatch-macro-character #\# #\o #'sharp-O)
  (set-dispatch-macro-character #\# #\x #'sharp-X)
  (set-dispatch-macro-character #\# #\a #'sharp-A)
  (set-dispatch-macro-character #\# #\s #'sharp-S)
  (set-dispatch-macro-character #\# #\= #'sharp-equal)
  (set-dispatch-macro-character #\# #\# #'sharp-sharp)
  (set-dispatch-macro-character #\# #\+ #'sharp-plus-minus)
  (set-dispatch-macro-character #\# #\- #'sharp-plus-minus)
  (set-dispatch-macro-character #\# #\c #'sharp-C)
  (set-dispatch-macro-character #\# #\| #'sharp-vertical-bar)
  (set-dispatch-macro-character #\# #\p #'sharp-P)
  (set-dispatch-macro-character #\# #\) #'sharp-illegal)
  (set-dispatch-macro-character #\# #\< #'sharp-illegal)
  (set-dispatch-macro-character #\# #\Space #'sharp-illegal)
  (dolist (cc '#.(list tab-char-code form-feed-char-code return-char-code
                       line-feed-char-code backspace-char-code))
    (set-dispatch-macro-character #\# (code-char cc) #'sharp-illegal)))
