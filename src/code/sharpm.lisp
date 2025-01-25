;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

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
  (declare (type (or null integer) numarg))
  (binding* (((buffer escaped) (read-extended-token stream))
             (input-len (token-buf-fill-ptr buffer))
             (bstring (token-buf-string buffer)))
    (when *read-suppress* (return-from sharp-star nil))
    (when escaped (simple-reader-error stream "An escape character appeared after #*."))
    (when numarg
      (cond ((and (not (eql numarg 0)) (zerop input-len))
             (simple-reader-error
              stream "#~D* requires at least 1 bit of input." numarg))
            ((> input-len numarg)
             (simple-reader-error
              stream "Too many bits in ~S: expected ~D or fewer"
              (copy-token-buf-string buffer) numarg)))
      (when (eql numarg 0) (setq numarg nil)))
    (let* ((fill (if numarg (logand (char-code (char bstring (1- input-len))) 1) 0))
           (bvec (make-array (or numarg input-len)
                             :element-type 'bit :initial-element fill)))
      (do ((i 0 (1+ i)))
          ((= i input-len) bvec)
        (declare (index i) (optimize (sb-c:insert-array-bounds-checks 0)))
        (let ((char (char bstring i)))
          (setf (elt bvec i)
                (case char
                  (#\0 0)
                  (#\1 1)
                  (t (simple-reader-error
                      stream "illegal element given for bit-vector: ~S"  char)))))))))

(defun sharp-A (stream ignore rank)
  (declare (ignore ignore))
  (when *read-suppress*
    (read stream t nil t)
    (return-from sharp-A nil))
  (let* ((*bq-error*
           (if (zerop *backquote-depth*)
               *bq-error*
               "Comma inside a backquoted array (not a list or general vector.)"))
         (*backquote-depth* 0)
         (contents (read stream t nil t)))
    (cond
      (rank
       (collect ((dims))
         (let ((seq contents))
           (dotimes (axis rank
                          (make-array (dims) :initial-contents contents))
             (unless (typep seq 'sequence)
               (simple-reader-error stream
                                    "#~WA axis ~W is not a sequence:~%  ~S"
                                    rank axis seq))
             (let ((len (length seq)))
               (dims len)
               (unless (or (= axis (1- rank))
                           ;; ANSI: "If some dimension of the array whose
                           ;; representation is being parsed is found to be
                           ;; 0, all dimensions to the right (i.e., the
                           ;; higher numbered dimensions) are also
                           ;; considered to be 0."
                           (= len 0))
                 (setq seq (elt seq 0))))))))
      ;; It's not legal to have #A without the rank, use that for
      ;; #A(dimensions element-type contents) to avoid using #. when
      ;; printing specialized arrays readably.
      ((list-of-length-at-least-p contents 2)
       (destructuring-bind (dimensions type &rest contents) contents
         (make-array dimensions :initial-contents contents
                                :element-type type)))
      (t
       (simple-reader-error stream
                            "~@<Array literal is neither of the ~
                             standard form #<rank>A<contents> nor the ~
                             SBCL-specific form #A(dimensions ~
                             element-type . contents).~@:>")))))

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
      (let ((default-constructor
             (dd-default-constructor
              (sb-kernel::layout-%info (classoid-layout classoid)))))
        (unless default-constructor
          (simple-reader-error
           stream
           "The ~S structure does not have a default constructor."
           (car body)))
        (when (and (atom (rest body))
                   (not (null (rest body))))
          (simple-reader-error stream "improper list for #S: ~S." body))
        (let ((constructor-args
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
                     collect (cadr tail))))
          (handler-bind ((type-error
                          (lambda (c)
                            (let ((context (sb-kernel::type-error-context c))
                                  (datum (type-error-datum c)))
                              (when (and (typep context '(cons (eql sb-kernel::struct-context)))
                                         sb-kernel::*type-error-no-check-restart*
                                         (contains-marker datum))
                                (destructuring-bind (head dd-name . dsd-name) context
                                  (declare (ignore head))
                                  (let* ((dd (find-defstruct-description dd-name nil))
                                         (dsd (and dd (find dsd-name (dd-slots dd) :key #'dsd-name)))
                                         (boxedp (and dsd (eq (dsd-raw-type dsd) t))))
                                    (when boxedp
                                      (funcall sb-kernel::*type-error-no-check-restart* datum)))))))))
            (apply (fdefinition default-constructor) constructor-args)))))))

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
         ;; Some other things that shouldn't work:
         ;; * (read-from-string "#x a") => 10
         ;; * (read-from-string "#x #+foo a b") => 11
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

(defconstant +sharp-equal-marker+ '+sharp-equal-marker+)

(defstruct (sharp-equal-wrapper
            (:constructor make-sharp-equal-wrapper (label))
            (:copier nil))
  (label nil :read-only t)
  (value +sharp-equal-marker+))
(declaim (freeze-type sharp-equal-wrapper))

(defun sharp-equal-visit (tree processor visitor)
  (declare (inline alloc-xset))
  (dx-let ((circle-table (alloc-xset)))
    (named-let recurse ((tree tree))
      (when (sharp-equal-wrapper-p tree)
        (return-from recurse (funcall visitor tree)))
      ;; pick off types that never need to be sought in or added to the xset.
      ;; (there are others, but these are common and quick to check)
      (when (or (unbound-marker-p tree) (typep tree '(or number symbol)))
        (return-from recurse (funcall visitor tree)))
      (unless (xset-member-p tree circle-table)
        (add-to-xset tree circle-table)
        (dx-flet ((process (child setter &optional typecheck)
                    (funcall processor tree child #'recurse setter typecheck)))
          (typecase tree
            (cons
             (process (car tree) (lambda (nv d) (setf (car d) nv)))
             (process (cdr tree) (lambda (nv d) (setf (cdr d) nv))))
            ((array t)
             (with-array-data ((data tree) (start) (end))
               (declare (fixnum start end))
               (do ((i start (1+ i)))
                   ((>= i end))
                 (process (aref data i) (lambda (nv d)
                                          (declare (ignore d))
                                          (setf (aref data i) nv))))))
            (instance
             (let* ((layout (%instance-layout tree))
                    (dd (layout-info layout)))
               (cond
                 ((typep dd 'defstruct-description)
                  (dolist (dsd (dd-slots dd))
                    (when (eq (dsd-raw-type dsd) t)
                      (let ((i (dsd-index dsd))
                            (type (dsd-type dsd)))
                        ;; KLUDGE: checking for NOT CONTAINS-MARKER
                        ;; here implies traversing the slot's value
                        ;; twice if it does contain a circularity
                        ;; marker.
                        (if (or (eq type t) (not (contains-marker (%instance-ref tree i))))
                            (process (%instance-ref tree i) (lambda (nv d) (%instance-set d i nv)))
                            (process (%instance-ref tree i) (lambda (nv d) (%instance-set d i nv))
                                     (lambda ()
                                       (loop
                                        (let ((v (%instance-ref tree i)))
                                          (when (typep v type)
                                            (return))
                                          (restart-case
                                              (error 'simple-type-error
                                                     :format-control "while setting slot ~S of structure of class ~S, ~S is not of type ~S"
                                                     :format-arguments (list (dsd-name dsd) (dd-name dd) v type)
                                                     :datum v
                                                     :expected-type type)
                                            (use-value (value)
                                              :report (lambda (stream)
                                                        (format stream "Use specified value."))
                                              :interactive read-evaluated-form
                                              (%instance-set tree i value))))))))))))
                 (t
                  (aver (sb-kernel::bitmap-all-taggedp layout))
                  (do ((len (%instance-length tree))
                       (i sb-vm:instance-data-start (1+ i)))
                      ((>= i len))
                    (process (%instance-ref tree i) (lambda (nv d) (%instance-set d i nv))))))))
            (funcallable-instance
             ;; ASSUMPTION: all funcallable instances have at least 1 slot
             ;; accessible via FUNCALLABLE-INSTANCE-INFO.
             ;; The only such objects with reader syntax are CLOS objects,
             ;; and those have exactly 1 slot in the primitive object.
             (process (%funcallable-instance-info tree 0)
                      (lambda (nv d) (setf (%funcallable-instance-info d 0) nv)))))))
      tree)))

;; This function is kind of like NSUBLIS, but checks for circularities and
;; substitutes in arrays and structures as well as lists.
(defun circle-subst (tree)
  (dx-let ((typecheckers nil))
    (dx-flet ((process (parent child recursor setter typecheck)
                (let ((new (funcall recursor child)))
                  (unless (eq child new)
                    (funcall setter new parent))
                  (when typecheck
                    (push typecheck typecheckers))))
              (visit (value)
                (if (and (sharp-equal-wrapper-p value)
                         (neq (sharp-equal-wrapper-value value) +sharp-equal-marker+))
                    (sharp-equal-wrapper-value value)
                    value)))
      (prog1
          (sharp-equal-visit tree #'process #'visit)
        (mapcar #'funcall typecheckers)))))

(defun contains-marker (tree)
  (dx-flet ((process (parent child recursor setter typechecker)
              (declare (ignore parent setter typechecker))
              (funcall recursor child))
            (visit (value)
              (when (sharp-equal-wrapper-p value)
                (return-from contains-marker t))))
    (sharp-equal-visit tree #'process #'visit)
    nil))

;; This function is kind of like NSUBLIS, but checks for circularities and
;; substitutes in arrays and structures as well as lists.
#+nil
(defun circle-subst (tree)
  (declare (inline alloc-xset))
  (dx-let ((circle-table (alloc-xset)))
    (named-let recurse ((tree tree))
      (when (and (sharp-equal-wrapper-p tree)
                 (neq (sharp-equal-wrapper-value tree) +sharp-equal-marker+))
        (return-from recurse (sharp-equal-wrapper-value tree)))
      ;; pick off types that never need to be sought in or added to the xset.
      ;; (there are others, but these are common and quick to check)
      (when (or (unbound-marker-p tree) (typep tree '(or number symbol)))
        (return-from recurse tree))
      (unless (xset-member-p tree circle-table)
        (add-to-xset tree circle-table)
        (macrolet ((process (place)
                     `(let* ((old ,place)
                             (new (recurse old)))
                        (unless (eq old new)
                          ,(if (eq (car place) '%instance-ref)
                               `(%instance-set ,@(cdr place) new)
                               `(setf ,place new))))))
          (typecase tree
            (cons
             (process (car tree))
             (process (cdr tree)))
            ((array t)
             (with-array-data ((data tree) (start) (end))
               (declare (fixnum start end))
               (do ((i start (1+ i)))
                   ((>= i end))
                 (process (aref data i)))))
            (instance
             ;; Don't refer to the DD-SLOTS unless there is reason to,
             ;; that is, unless some slot might be raw.
             (if (sb-kernel::bitmap-all-taggedp (%instance-layout tree))
                 (do ((len (%instance-length tree))
                      (i sb-vm:instance-data-start (1+ i)))
                     ((>= i len))
                   (process (%instance-ref tree i)))
                 (let ((dd (layout-dd (%instance-layout tree))))
                   (dolist (dsd (dd-slots dd))
                     (when (eq (dsd-raw-type dsd) t)
                       (process (%instance-ref tree (dsd-index dsd))))))))
            ;; ASSUMPTION: all funcallable instances have at least 1 slot
            ;; accessible via FUNCALLABLE-INSTANCE-INFO.
            ;; The only such objects with reader syntax are CLOS objects,
            ;; and those have exactly 1 slot in the primitive object.
            (funcallable-instance
             (process (%funcallable-instance-info tree 0))))))
      tree)))

;;; Sharp-equal works as follows.
;;; When creating a new label a SHARP-EQUAL-WRAPPER is pushed onto
;;; *SHARP-EQUAL* with the value slot being +SHARP-EQUAL-MARKER+.
;;; When #x# looks up the label and SHARP-EQUAL-WRAPPER-VALUE is
;;; +SHARP-EQUAL-MARKER+ it leaves the wrapper, otherwise it uses the
;;; value.
;;; After reading the object the sharp-equal-wrapper-value is set to
;;; the object and CIRCLE-SUBST replaces all the sharp-equal-wrappers
;;; with the already read values.
(defun sharp-equal (stream ignore label)
  (declare (ignore ignore))
  (when *read-suppress* (return-from sharp-equal (values)))
  (unless label
    (simple-reader-error stream "Missing label for #="))
  (when (find label *sharp-equal* :key #'sharp-equal-wrapper-label)
    (simple-reader-error stream "Multiply defined label: #~D=" label))
  (let* ((wrapper (make-sharp-equal-wrapper label))
         (obj (progn
                (push wrapper *sharp-equal*)
                (read stream t nil t))))
    (when (eq obj wrapper)
      (simple-reader-error stream
                           "Must label something more than just #~D#"
                           label))
    (setf (sharp-equal-wrapper-value wrapper) obj)
    (circle-subst obj)))

(defun sharp-sharp (stream ignore label)
  (declare (ignore ignore))
  (when *read-suppress* (return-from sharp-sharp nil))
  (unless label
    (simple-reader-error stream "Missing label for ##"))
  ;; Has this label been defined previously? (Don't read
  ;; ANSI "2.4.8.15 Sharpsign Equal-Sign" and worry that
  ;; it requires you to implement forward references,
  ;; because forward references are disallowed in
  ;; "2.4.8.16 Sharpsign Sharpsign".)
  (let ((entry (find label *sharp-equal* :key #'sharp-equal-wrapper-label)))
    (cond ((not entry)
           (simple-reader-error stream
                                "Reference to undefined label #~D#"
                                label))
          ((eq (sharp-equal-wrapper-value entry) +sharp-equal-marker+)
           entry)
          (t
           (sharp-equal-wrapper-value entry)))))

;;;; conditional compilation: the #+ and #- readmacros

;;; If X is a symbol, see whether it is present in *FEATURES*. Also
;;; handle arbitrary combinations of atoms using NOT, AND, OR.
(defun featurep (x)
  (typecase x
    (cons
     (case (car x)
       ((:not not)
        (cond
          ((cddr x)
           (error "too many subexpressions in feature expression: ~S" x))
          ((null (cdr x))
           (error "too few subexpressions in feature expression: ~S" x))
          (t (not (featurep (cadr x))))))
       ((:and and) (every #'featurep (cdr x)))
       ((:or or) (some #'featurep (cdr x)))
       (t
        (error "unknown operator in feature expression: ~S." x))))
    (symbol
     (let ((present (memq x *features*)))
       (cond (present
              t)
             ((and (boundp '+internal-features+)
                   (memq x (symbol-value '+internal-features+)))
              (warn "~s is no longer present in ~s" x '*features*)))))
    (t
     (error "invalid feature expression: ~S" x))))

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
             'sb-kernel::character-decoding-error-in-dispatch-macro-char-comment
             :sub-char sub-char :position (file-position stream) :stream stream)
            (invoke-restart 'attempt-resync))))
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
            (munch (read-char stream t))))))

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
