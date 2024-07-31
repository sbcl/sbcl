;;;; machine/filesystem-independent pathname functions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

(defstruct (logical-host
             (:copier nil)
             (:print-object
              (lambda (logical-host stream)
                (print-unreadable-object (logical-host stream :type t)
                  (prin1 (logical-host-name logical-host) stream))))
             (:include host
                       (parse #'parse-logical-namestring)
                       (parse-native
                        (lambda (&rest x)
                          (error "called PARSE-NATIVE-NAMESTRING using a ~
                                  logical host: ~S" (first x))))
                       (unparse #'unparse-logical-namestring)
                       (unparse-native
                        (lambda (&rest x)
                          (error "called NATIVE-NAMESTRING using a ~
                                  logical host: ~S" (first x))))
                       (unparse-host
                        (lambda (x)
                          (logical-host-name (%pathname-host x))))
                       (unparse-directory #'unparse-logical-directory)
                       (unparse-file #'unparse-logical-file)
                       (unparse-enough #'unparse-enough-namestring)
                       (unparse-directory-separator ";")
                       (simplify-namestring #'identity)
                       (customary-case :upper)))
  (name-hash 0 :type fixnum)
  (name "" :type simple-string :read-only t)
  (translations nil :type list)
  (canon-transls nil :type list))

;;; Logical pathnames have the following format:
;;;
;;; logical-namestring ::=
;;;      [host ":"] [";"] {directory ";"}* [name] ["." type ["." version]]
;;;
;;; host ::= word
;;; directory ::= word | wildcard-word | **
;;; name ::= word | wildcard-word
;;; type ::= word | wildcard-word
;;; version ::= pos-int | newest | NEWEST | *
;;; word ::= {uppercase-letter | digit | -}+
;;; wildcard-word ::= [word] '* {word '*}* [word]
;;; pos-int ::= integer > 0
;;;
;;; Physical pathnames include all these slots and a device slot.

;;; We can't freeze HOST because later on we define either UNIX-HOST or WIN32-HOST.
(declaim (freeze-type logical-host))

;;; Utility functions

(deftype absent-pathname-component ()
  '(member nil :unspecific))

(defun make-pattern (pieces)
  ;; Ensure that the hash will meet the SXASH persistence requirement:
  ;; "2. For any two objects, x and y, both of which are ... pathnames ... and which are similar,
  ;;     (sxhash x) and (sxhash y) yield the same mathematical value even if x and y exist in
  ;;     different Lisp images of the same implementation."
  ;; Specifically, hashes that depend on object identity (address) are impermissible.
  (dolist (piece pieces)
    (aver (typep piece '(or string symbol (cons (eql :character-set) string)))))
  (%make-pattern (pathname-sxhash pieces) pieces))

(declaim (inline %pathname-directory))
(defun %pathname-directory (pathname) (car (%pathname-dir+hash pathname)))

(declaim (inline pathname-component-present-p))
(defun pathname-component-present-p (component)
  (not (typep component 'absent-pathname-component)))

;;; The following functions are used both for Unix and Windows: while
;;; we accept both \ and / as directory separators on Windows, we
;;; print our own always with /, which is much less confusing what
;;; with being \ needing to be escaped.
(defun unparse-physical-directory (pathname escape-char)
  (declare (pathname pathname))
  (unparse-physical-directory-list (%pathname-directory pathname) escape-char))

(defun unparse-physical-directory-list (directory escape-char)
  (declare (list directory))
  (collect ((pieces))
    (when directory
      (ecase (pop directory)
       (:absolute
        (let ((next (pop directory)))
          (cond ((eq :home next)
                 (pieces "~"))
                ((and (consp next) (eq :home (car next)))
                 (pieces "~")
                 (pieces (second next)))
                ((and (stringp next)
                      (plusp (length next))
                      (char= #\~ (char next 0)))
                 ;; The only place we need to escape the tilde.
                 (pieces "\\")
                 (pieces next))
                (next
                 (push next directory)))
          (pieces "/")))
        (:relative))
      (dolist (dir directory)
        (typecase dir
         ((member :up)
          (pieces "../"))
         ((member :back)
          (error ":BACK cannot be represented in namestrings."))
         ((member :wild-inferiors)
          (pieces "**/"))
         ((or simple-string pattern (member :wild))
          (pieces (unparse-physical-piece dir escape-char))
          (pieces "/"))
         (t
          (error "invalid directory component: ~S" dir)))))
    (apply #'concatenate 'simple-string (pieces))))

(defun unparse-physical-file (pathname escape-char)
  (declare (type pathname pathname))
  (let ((name (%pathname-name pathname))
        (type (%pathname-type pathname)))
    (collect ((fragments))
      ;; Note: by ANSI 19.3.1.1.5, we ignore the version slot when
      ;; translating logical pathnames to a filesystem without
      ;; versions (like Unix and Win32).
      (when name
        (when (and (typep name 'string)
                   (string= name ""))
          (no-namestring-error
           pathname "the ~S component ~S is of length 0" :name name))
        (fragments (unparse-physical-piece
                    name escape-char
                    :escape-dot (when (null type) :unless-at-start))))
      (when (pathname-component-present-p type)
        (unless name
          (no-namestring-error
           pathname
           "there is a ~S component but no ~S component" :type :name))
        (fragments ".")
        (fragments (unparse-physical-piece
                    type escape-char :escape-dot t)))
      (apply #'concatenate 'simple-string (fragments)))))

(defun unparse-native-physical-file (pathname)
  (let ((name (pathname-name pathname))
        (type (pathname-type pathname)))
    (collect ((fragments))
      (cond
        ((pathname-component-present-p name)
         (unless (stringp name)         ; some kind of wild field
           (no-native-namestring-error
            pathname "of the ~S component ~S." :name name))
         (fragments name)
         (when (pathname-component-present-p type)
           (unless (stringp type)       ; some kind of wild field
             (no-native-namestring-error
              pathname "of the ~S component ~S" :type type))
           (fragments ".")
           (fragments type)))
        ((pathname-component-present-p type) ; type without a name
         (no-native-namestring-error
          pathname
          "there is a ~S component but no ~S component" :type :name)))
      (apply #'concatenate 'simple-string (fragments)))))

(defun unparse-physical-enough (pathname defaults escape-char)
  (declare (type pathname pathname defaults))
  (flet ((lose ()
           (error "~S cannot be represented relative to ~S."
                  pathname defaults)))
    (collect ((strings))
      (let* ((pathname-directory (%pathname-directory pathname))
             (defaults-directory (%pathname-directory defaults))
             (prefix-len (length defaults-directory))
             (result-directory
               (cond ((null pathname-directory) '(:relative))
                     ((eq (car pathname-directory) :relative)
                      pathname-directory)
                     ((and (> prefix-len 0)
                           (>= (length pathname-directory) prefix-len)
                           (compare-component (subseq pathname-directory
                                                      0 prefix-len)
                                              defaults-directory))
                      ;; Pathname starts with a prefix of default. So
                      ;; just use a relative directory from then on out.
                      (cons :relative (nthcdr prefix-len pathname-directory)))
                     ((eq (car pathname-directory) :absolute)
                      ;; We are an absolute pathname, so we can just use it.
                      pathname-directory)
                     (t
                      (bug "Bad fallthrough in ~S" 'unparse-physical-enough)))))
        (strings (unparse-physical-directory-list result-directory escape-char)))
      (let* ((pathname-type (%pathname-type pathname))
             (type-needed (pathname-component-present-p pathname-type))
             (pathname-name (%pathname-name pathname))
             (name-needed (or type-needed
                              (and pathname-name
                                   (not (compare-component pathname-name
                                                           (%pathname-name
                                                            defaults)))))))
        (when name-needed
          (unless pathname-name (lose))
          (strings (unparse-physical-piece
                    pathname-name escape-char
                    :escape-dot (when (not pathname-type) :unless-at-start))))
        (when type-needed
          (unless (pathname-component-present-p pathname-type)
            (lose))
          (strings ".")
          (strings (unparse-physical-piece pathname-type
                                           escape-char :escape-dot t))))
      (apply #'concatenate 'simple-string (strings)))))


;;; To be initialized in unix/win32-pathname.lisp
(define-load-time-global *physical-host* nil)

;;; Return a value suitable, e.g., for preinitializing
;;; *DEFAULT-PATHNAME-DEFAULTS* before *DEFAULT-PATHNAME-DEFAULTS* is
;;; initialized (at which time we can't safely call e.g. #'PATHNAME).
(defun make-trivial-default-pathname ()
  (intern-pathname *physical-host* nil nil nil nil :newest))

;;; pathname methods

(defmethod print-object ((pathname pathname) stream)
  (let ((namestring (handler-case (namestring pathname)
                      (error nil))))
    (if namestring
        (format stream
                (if (or *print-readably* *print-escape*)
                    "#P~S"
                    "~A")
                (coerce namestring '(simple-array character (*))))
        (print-unreadable-object (pathname stream :type t)
          (format stream
                  "~@<(with no namestring) ~_:HOST ~S ~_:DEVICE ~S ~_:DIRECTORY ~S ~
                  ~_:NAME ~S ~_:TYPE ~S ~_:VERSION ~S~:>"
                  (%pathname-host pathname)
                  (%pathname-device pathname)
                  (%pathname-directory pathname)
                  (%pathname-name pathname)
                  (%pathname-type pathname)
                  (%pathname-version pathname))))))

;;; Pathnames are stored in an open-addressing weak hash-set.
;;; Ideally there would be only one internal representation of any pathname,
;;; so that EQUAL on pathnames could reduce to EQ.
;;; I'm not sure that's possible. For the time being, we use a comparator
;;; that is stricter (I think) than EQUAL.
;;;
;;; The spec is actually extremely underspecified in regard to the meaning of
;;;  "pathnames that are equal should be functionally equivalent."
;;; The simple test:
;;;  (equal (make-pathname :name "a" :version nil) (make-pathname :name "a" :version :newest))
;;; shows that EQUAL is inconsistent in terms of what "functionally equivalent" means:
;;;  SBCL, ABCL, and CCL => T
;;;  CLISP and ECL => NIL
;;; Also, on case-sensitive-case-preserving filesystems it's not possible
;;; to know which pathnames are equivalent without asking the filesystem.
;;;
;;: TODO: consider similarly interning the DEVICE and TYPE parts
(define-load-time-global *pn-dir-table* nil)
(define-load-time-global *pn-table* nil)
(declaim (type robinhood-hashset *pn-dir-table* *pn-table*))

(defmacro compare-pathname-host/dev/dir/name/type (a b)
  `(and (eq (%pathname-host ,a) (%pathname-host ,b)) ; Interned
        ;; dir+hash are EQ-comparable thanks to INTERN-PATHNAME
        (eq (%pathname-dir+hash ,a) (%pathname-dir+hash ,b))
        ;; the pathname pieces which are strings aren't interned
        (compare-component (%pathname-device ,a) (%pathname-device ,b))
        (compare-component (%pathname-name ,a) (%pathname-name ,b))
        (compare-component (%pathname-type ,a) (%pathname-type ,b))))

(defun pn-table-dir= (entry key)
  (or (eq (car entry) (car key)) ; quick win if lists are EQ
      (and (eq (cdr entry) (cdr key)) ; hashes match
           (compare-component (car entry) (car key)))))
(defun pn-table-hash (pathname)
  ;; The pathname table makes distinctions between pathnames that EQUAL does not.
  (mix (sxhash (%pathname-version pathname))
       (pathname-sxhash pathname)))
(defun pn-table-pn= (entry key)
  (and (compare-pathname-host/dev/dir/name/type entry key)
       (eql (%pathname-version entry) (%pathname-version key))))

(defun !pathname-cold-init ()
  (setq *pn-dir-table* (make-hashset 32 #'pn-table-dir= #'cdr
                                     :synchronized t :weakness t)
        *pn-table* (make-hashset 32 #'pn-table-pn= #'pn-table-hash
                                 :synchronized t :weakness t)))

;;; A pathname is logical if the host component is a logical host.
;;; This constructor is used to make an instance of the correct type
;;; from parsed arguments.
(defun intern-pathname (host device directory name type version)
  ;; We canonicalize logical pathname components to uppercase. ANSI
  ;; doesn't strictly require this, leaving it up to the implementor;
  ;; but the arguments given in the X3J13 cleanup issue
  ;; PATHNAME-LOGICAL:ADD seem compelling: we should canonicalize the
  ;; case, and uppercase is the ordinary way to do that.
  (declare (sb-c::tlab :system))
  (flet ((upcase-maybe (x) (typecase x (string (logical-word-or-lose x)) (t x))))
    (when (typep host 'logical-host)
        (setq device :unspecific
              directory (mapcar #'upcase-maybe directory)
              name (upcase-maybe name)
              type (upcase-maybe type))))
  (dx-let ((dir-key (cons directory (pathname-sxhash directory))))
    (declare (inline !allocate-pathname)) ; for DX-allocation
    (flet ((ensure-heap-string (part) ; return any non-string as-is
             ;; FIXME: what about pattern pieces and (:HOME "user") ?
             (cond ((or (not (stringp part)) (read-only-space-obj-p part)) part)
                   ;; Altering a string that is any piece of any arg to INTERN-PATHNAME
                   ;; is a surefire way to corrupt the hashset, so *always* copy the input.
                   ;; Users might not have reason to believe that once a string is passed
                   ;; to any pathname function, it is immutable. We can only hope that
                   ;; they don't mutate strings returned by pathname accessors.
                   (t (let ((l (length part)))
                        (logically-readonlyize
                         (replace (typecase part
                                    (base-string (make-string l :element-type 'base-char))
                                    (t (make-string l)))
                                  part)))))))
      (let* ((dir+hash
              (if directory ; find the interned dir-key
                  (hashset-insert-if-absent
                   *pn-dir-table* dir-key
                   (lambda (dir)
                     (cons (mapcar #'ensure-heap-string (car dir)) (cdr dir))))))
             (pn-key (!allocate-pathname host device dir+hash name type version)))
        (declare (dynamic-extent pn-key))
        (hashset-insert-if-absent
         *pn-table* pn-key
         (lambda (tmp &aux (host (%pathname-host tmp)))
           (let ((new (!allocate-pathname
                       host (%pathname-device tmp)
                       (%pathname-dir+hash tmp)
                       (ensure-heap-string (%pathname-name tmp))
                       (ensure-heap-string (%pathname-type tmp))
                       (%pathname-version tmp))))
             (when (typep host 'logical-host)
               (setf (%instance-layout new) #.(find-layout 'logical-pathname)))
             new)))))))

;;; Weak vectors don't work at all once rendered pseudo-static.
;;; so in order to weaken the pathname cache, the vectors are copied on restart.
;;; It may not achieve anything for saved pathnames, since the vector elements
;;; are themselves pseudo-static, but at least newly made ones aren't immortal.
(defun rebuild-pathname-cache ()
  (hashset-rehash *pn-dir-table* nil)
  (hashset-rehash *pn-table* nil))

(defun show-pn-cache (&aux (*print-pretty* nil) (*package* (find-package "CL-USER")))
  (dolist (symbol '(*pn-dir-table* *pn-table*))
    (let* ((hashset (symbol-value symbol))
           (v (hss-cells (hashset-storage hashset)))
           (n (hs-cells-capacity v)))
      (multiple-value-bind (live tombstones unused) (hs-cells-occupancy v n)
        (declare (ignore live))
        (format t "~&~S: size=~D tombstones=~D unused=~D~%" symbol n tombstones unused))
      (dotimes (i n)
        (let ((entry (hs-cell-ref v i)))
          (unless (member entry '(nil 0))
            (format t "~4d ~3d ~x " i (generation-of entry) (get-lisp-obj-address entry))
            (if (eq symbol '*pn-dir-table*)
                (format t "~16x ~s~%" (cdr entry) (car entry))
                (flet ((index-of (pn-dir)
                         (let ((dirs (hss-cells (hashset-storage *pn-dir-table*))))
                           (dotimes (i (weak-vector-len dirs))
                             (when (eq pn-dir (weak-vector-ref dirs i))
                               (return i))))))
                  (format t
                   "~16x [~A ~S ~A ~S ~S ~S]~%"
                   (pathname-sxhash entry)
                   (let ((host (%pathname-host entry)))
                     (cond ((logical-host-p host)
                            ;; display with string quotes around name
                            (prin1-to-string (logical-host-name host)))
                           ((eq host *physical-host*) "phys")
                           (t host)))
                   (%pathname-device entry)
                   (acond ((%pathname-dir+hash entry) (format nil "@~D" (index-of it)))
                          (t "-"))
                   (%pathname-name entry)
                   (%pathname-type entry)
                   (%pathname-version entry))))))))))

;;; Vector of logical host objects, each of which contains its translations.
;;; The vector is never mutated- always a new vector is created when adding
;;; translations for a new host. So nothing needs locking.
;;; And the fact that hosts are never deleted keeps things really simple.
(define-load-time-global *logical-hosts* #())
(declaim (simple-vector *logical-hosts*))

;;;; patterns

(defmethod print-object ((pattern pattern) stream)
  (print-unreadable-object (pattern stream :type t)
    (if *print-pretty*
        (let ((*print-escape* t))
          (pprint-fill stream (pattern-pieces pattern) nil))
        (prin1 (pattern-pieces pattern) stream))))

(defun pattern= (pattern1 pattern2)
  (declare (type pattern pattern1 pattern2))
  (let ((pieces1 (pattern-pieces pattern1))
        (pieces2 (pattern-pieces pattern2)))
    (and (= (length pieces1) (length pieces2))
         (every (lambda (piece1 piece2)
                  (typecase piece1
                    (simple-string
                     (and (simple-string-p piece2)
                          (string= piece1 piece2)))
                    (cons
                     (and (consp piece2)
                          (eq (car piece1) (car piece2))
                          (string= (cdr piece1) (cdr piece2))))
                    (t
                     (eq piece1 piece2))))
                pieces1
                pieces2))))

;;; If the string matches the pattern returns the multiple values T
;;; and a list of the matched strings.
(defun pattern-matches (pattern string)
  (declare (type pattern pattern)
           (type simple-string string))
  (let ((len (length string)))
    (labels ((maybe-prepend (subs cur-sub chars)
               (if cur-sub
                   (let* ((len (length chars))
                          (new (make-string len))
                          (index len))
                     (dolist (char chars)
                       (setf (schar new (decf index)) char))
                     (cons new subs))
                   subs))
             (matches (pieces start subs cur-sub chars)
               (if (null pieces)
                   (if (= start len)
                       (values t (maybe-prepend subs cur-sub chars))
                       (values nil nil))
                   (let ((piece (car pieces)))
                     (etypecase piece
                       (simple-string
                        (let ((end (+ start (length piece))))
                          (and (<= end len)
                               (string= piece string
                                        :start2 start :end2 end)
                               (matches (cdr pieces) end
                                        (maybe-prepend subs cur-sub chars)
                                        nil nil))))
                       (list
                        (ecase (car piece)
                          (:character-set
                           (and (< start len)
                                (let ((char (schar string start)))
                                  (if (find char (cdr piece) :test #'char=)
                                      (matches (cdr pieces) (1+ start) subs t
                                               (cons char chars))))))))
                       ((member :single-char-wild)
                        (and (< start len)
                             (matches (cdr pieces) (1+ start) subs t
                                      (cons (schar string start) chars))))
                       ((member :multi-char-wild)
                        (multiple-value-bind (won new-subs)
                            (matches (cdr pieces) start subs t chars)
                          (if won
                              (values t new-subs)
                              (and (< start len)
                                   (matches pieces (1+ start) subs t
                                            (cons (schar string start)
                                                  chars)))))))))))
      (multiple-value-bind (won subs)
          (matches (pattern-pieces pattern) 0 nil nil nil)
        (values won (reverse subs))))))

;;; PATHNAME-MATCH-P for directory components
(defun directory-components-match (thing wild)
  (or (eq thing wild)
      (eq wild :wild)
      ;; If THING has a null directory, assume that it matches
      ;; (:ABSOLUTE :WILD-INFERIORS) or (:RELATIVE :WILD-INFERIORS).
      (and (consp wild)
           (null thing)
           (member (first wild) '(:absolute :relative))
           (eq (second wild) :wild-inferiors))
      (and (consp wild)
           (let ((wild1 (first wild)))
             (if (eq wild1 :wild-inferiors)
                 (let ((wild-subdirs (rest wild)))
                   (or (null wild-subdirs)
                       (loop
                         (when (directory-components-match thing wild-subdirs)
                           (return t))
                         (pop thing)
                         (unless thing (return nil)))))
                 (and (consp thing)
                      (components-match (first thing) wild1)
                      (directory-components-match (rest thing)
                                                  (rest wild))))))))

;;; Return true if pathname component THING is matched by WILD. (not
;;; commutative)
(defun components-match (thing wild)
  (declare (type (or pattern symbol simple-string integer) thing wild))
  (or (eq thing wild)
      (eq wild :wild)
      (typecase thing
        (simple-string
         ;; String is matched by itself, a matching pattern or :WILD.
         (typecase wild
           (pattern
            (values (pattern-matches wild thing)))
           (simple-string
            (string= thing wild))))
        (pattern
         ;; A pattern is only matched by an identical pattern.
         (and (pattern-p wild) (pattern= thing wild)))
        (bignum
         ;; An integer (version number) is matched by :WILD or the
         ;; same integer. This branch will actually always be NIL as
         ;; long as the version is a fixnum.
         (eql thing wild)))))

;;; a predicate for comparing two pathname slot component sub-entries
(defun compare-component (this that)
  (or (eq this that)
      (typecase this
        (simple-string
         (and (simple-string-p that)
              (string= this that)))
        (pattern
         ;; PATTERN instances should probably become interned objects
         ;; so that we can use EQ on them.
         (and (pattern-p that)
              (pattern= this that)))
        (cons
         ;; Even though directory parts are now reliably interned -
         ;; and so you might be inclined to think that the "full" comparison
         ;; could be confined to just the interning operation, that's not so,
         ;; because we also use COMPARE-COMPONENT in ENOUGH-NAMESTRING.
         (and (consp that)
              (compare-component (car this) (car that))
              (compare-component (cdr this) (cdr that))))
        (bignum
         (eql this that)))))

;;;; pathname functions

(defun pathname= (a b)
  (declare (type pathname a b))
  (or (eq a b)
      (and (compare-pathname-host/dev/dir/name/type a b)
           (or (eq (%pathname-host a) *physical-host*)
               (compare-component (pathname-version a)
                                  (pathname-version b))))))

(sb-kernel::assign-equalp-impl 'pathname #'pathname=)
(sb-kernel::assign-equalp-impl 'logical-pathname #'pathname=)

;;; Hash a PATHNAME or a PATHNAME-DIRECTORY or pieces of a PATTERN.
;;; This is called by both SXHASH and by the interning of pathnames, which uses a
;;; multi-step approaching to coalescing shared subparts.
;;; If an EQUAL directory was used before, we share that.
;;; Since a directory is stored with its hash precomputed, hashing a PATHNAME as a
;;; whole entails at most 4 more MIX operations. So using pathnames as keys in
;;; a hash-table pays a small up-front price for later speed improvement.
(defun pathname-sxhash (x)
  (labels
      ((hash-piece (piece)
           (etypecase piece
             (string
              (let ((res (length piece)))
                (if (<= res 6) ; hash it more thoroughly than (SXHASH string)
                    (dovector (ch piece res)
                      (setf res (mix (murmur-hash-word/+fixnum (char-code ch)) res)))
                    (sxhash piece))))
             (symbol (symbol-name-hash piece))
             (pattern (pattern-hash piece))
             ;; next case is only for MAKE-PATTERN
             ((cons (eql :character-set)) (hash-piece (the string (cdr piece))))
             ((cons (eql :home) (cons string null))
              ;; :HOME has two representations- one is just '(:absolute :home ...)
              ;; and the other '(:absolute (:home "user") ...)
              (sxhash (second piece))))))
    (etypecase x
      (pathname
       (let* ((host (%pathname-host x))
              ;; NAME-HASH is based on SXHASH of a string
              (hash (if (typep host 'logical-host) (logical-host-name-hash host) 0)))
         (mixf hash (hash-piece (%pathname-device x))) ; surely stringlike, right?
         (awhen (%pathname-dir+hash x) (mixf hash (cdr it)))
         (mixf hash (hash-piece (%pathname-name x)))
         (mixf hash (hash-piece (%pathname-type x)))
         ;; The requirement NOT to mix the version into the resulting hash is mandated
         ;; by bullet point 1 in the SXHASH specification:
         ;;  (equal x y) implies (= (sxhash x) (sxhash y))
         ;; and the observation that in this implementation of Lisp:
         ;;  (equal (make-pathname :version 1) (make-pathname :version 15)) => T
         hash))
      (list ;; a directory, or the PIECES argument to MAKE-PATTERN
       (let ((hash 0))
         (dolist (piece x hash)
           (mixf hash (hash-piece piece))))))))

;;; Convert PATHNAME-DESIGNATOR (a pathname, or string, or
;;; stream), into a pathname in PATHNAME.
(defmacro with-pathname ((pathname pathname-designator) &body body)
  (once-only ((pathname-designator pathname-designator))
    `(let ((,pathname (etypecase ,pathname-designator
                        (pathname ,pathname-designator)
                        (string (parse-namestring ,pathname-designator))
                        ((or file-stream synonym-stream)
                         (stream-file-name-or-lose ,pathname-designator)))))
       ,@body)))

(defmacro with-native-pathname ((pathname pathname-designator) &body body)
  (once-only ((pathname-designator pathname-designator))
    `(let ((,pathname (etypecase ,pathname-designator
                        (pathname ,pathname-designator)
                        (string (parse-native-namestring ,pathname-designator))
                        ;; FIXME
                        #+nil
                        (file-stream (file-name ,pathname-designator)))))
       ,@body)))

(defmacro with-host ((host host-designator) &body body)
  ;; Generally, redundant specification of information in software,
  ;; whether in code or in comments, is bad. However, the ANSI spec
  ;; for this is messy enough that it's hard to hold in short-term
  ;; memory, so I've recorded these redundant notes on the
  ;; implications of the ANSI spec.
  ;;
  ;; According to the ANSI spec, HOST can be a valid pathname host, or
  ;; a logical host, or NIL.
  ;;
  ;; A valid pathname host can be a valid physical pathname host or a
  ;; valid logical pathname host.
  ;;
  ;; A valid physical pathname host is "any of a string, a list of
  ;; strings, or the symbol :UNSPECIFIC, that is recognized by the
  ;; implementation as the name of a host". In SBCL as of 0.6.9.8,
  ;; that means :UNSPECIFIC: though someday we might want to
  ;; generalize it to allow strings like "RTFM.MIT.EDU" or lists like
  ;; '("RTFM" "MIT" "EDU"), that's not supported now.
  ;;
  ;; A valid logical pathname host is a string which has been defined as
  ;; the name of a logical host, as with LOAD-LOGICAL-PATHNAME-TRANSLATIONS.
  ;;
  ;; A logical host is an object of implementation-dependent nature. In
  ;; SBCL, it's a member of the HOST class (a subclass of STRUCTURE-OBJECT).
  (once-only ((host-designator host-designator))
    `(let ((,host (etypecase ,host-designator
                    ((string 0)
                     ;; This is a special host. It's not valid as a
                     ;; logical host, so it is a sensible thing to
                     ;; designate the physical host object. So we do
                     ;; that.
                     *physical-host*)
                    (string
                     ;; In general ANSI-compliant Common Lisps, a
                     ;; string might also be a physical pathname
                     ;; host, but ANSI leaves this up to the
                     ;; implementor, and in SBCL we don't do it, so
                     ;; it must be a logical host.
                     (find-logical-host ,host-designator))
                    (absent-pathname-component
                     ;; CLHS says that HOST=:UNSPECIFIC has
                     ;; implementation-defined behavior. We
                     ;; just turn it into NIL.
                     nil)
                    (list
                     ;; ANSI also allows LISTs to designate hosts,
                     ;; but leaves its interpretation
                     ;; implementation-defined. Our interpretation
                     ;; is that it's unsupported.:-|
                     (error "A LIST representing a pathname host is not ~
                              supported in this implementation:~%  ~S"
                            ,host-designator))
                    (host ,host-designator))))
       ,@body)))

(defun find-host (host-designator &optional (errorp t))
  (with-host (host host-designator)
    (when (and errorp (not host))
      (error "Couldn't find host: ~S" host-designator))
    host))

(defun pathname (pathspec)
  "Convert PATHSPEC (a pathname designator) into a pathname."
  (declare (type pathname-designator pathspec))
  (with-pathname (pathname pathspec)
    pathname))

(defun native-pathname (pathspec)
  "Convert PATHSPEC (a pathname designator) into a pathname, assuming
the operating system native pathname conventions."
  (with-native-pathname (pathname pathspec)
    pathname))

;;; Recursively (e.g. for the directory component) change the case of
;;; the pathname component THING.
(declaim (ftype (sfunction ((or symbol integer string pattern list))
                           (or symbol integer string pattern list))
                diddle-case))
(defun diddle-case (thing)
  (labels ((check-for (pred in)
             (typecase in
               (pattern
                (some (lambda (piece)
                        (typecase piece
                          (simple-string
                           (check-for pred piece))
                          ((cons (eql :character-set))
                           (check-for pred (cdr piece)))))
                      (pattern-pieces in)))
               (simple-string
                (some pred in))))
           (diddle-with (fun thing)
             (typecase thing
               (pattern
                (make-pattern
                 (mapcar (lambda (piece)
                           (typecase piece
                             (simple-string
                              (funcall fun piece))
                             ((cons (eql :character-set))
                              (funcall fun (cdr piece)))
                             (t
                              piece)))
                         (pattern-pieces thing))))
               (simple-string
                (funcall fun thing))
               (t
                thing)))
           (maybe-diddle-part (thing)
             (if (listp thing)
                 (mapcar #'maybe-diddle-part thing)
                 (let ((any-uppers (check-for #'upper-case-p thing))
                       (any-lowers (check-for #'lower-case-p thing)))
                   (cond ((and any-uppers any-lowers) ; mixed case, stays the same
                          thing)
                         (any-uppers ; all uppercase, becomes all lower case
                          (diddle-with 'string-downcase thing))
                         (any-lowers ; all lowercase, becomes all upper case
                          (diddle-with 'string-upcase thing))
                         (t ; no letters?  I guess just leave it.
                          thing))))))
    (if (not (or (symbolp thing) (integerp thing)))
        (maybe-diddle-part thing)
        thing)))

(declaim (inline maybe-diddle-case))
(defun maybe-diddle-case (thing diddle-p)
  (if diddle-p
      (diddle-case thing)
      thing))

(defun merge-directories (dir1 dir2 diddle-case)
  (if (or (eq (car dir1) :absolute)
          (null dir2))
      dir1
      (let ((results nil))
        (flet ((add (dir)
                 (if (and (eq dir :back)
                          results
                          (typep (car results) '(or string pattern
                                                 (member :wild :wild-inferiors))))
                     (pop results)
                     (push dir results))))
          (dolist (dir (maybe-diddle-case dir2 diddle-case))
            (add dir))
          (dolist (dir (cdr dir1))
            (add dir)))
        (reverse results))))

(defun merge-pathnames (pathname
                        &optional
                        (defaults *default-pathname-defaults*)
                        (default-version :newest))
  "Construct a filled in pathname by completing the unspecified components
   from the defaults."
  (declare (type pathname-designator pathname)
           (type pathname-designator defaults)
           (values pathname))
  (with-pathname (defaults defaults)
    (let* ((pathname (let ((*default-pathname-defaults* defaults))
                       (pathname pathname)))
           (default-host (%pathname-host defaults))
           (pathname-host (%pathname-host pathname))
           (diddle-case
             (and default-host pathname-host
                  (not (eq (host-customary-case default-host)
                           (host-customary-case pathname-host)))))
           (directory (merge-directories (%pathname-directory pathname)
                                         (%pathname-directory defaults)
                                         diddle-case)))
      (macrolet ((merged-component (component)
                   `(or (,component pathname)
                        (let ((default (,component defaults)))
                          (if diddle-case
                              (diddle-case default)
                              default)))))
        (intern-pathname
         (or pathname-host default-host)
         ;; The device of ~/ shouldn't be merged, because the
         ;; expansion may have a different device
         (unless (typep directory '(cons (eql :absolute) (cons (eql :home))))
           (merged-component %pathname-device))
         directory
         (merged-component %pathname-name)
         (merged-component %pathname-type)
         (or (%pathname-version pathname)
             (and (not (%pathname-name pathname)) (%pathname-version defaults))
             default-version))))))

(defun import-directory (directory diddle-case)
  (etypecase directory
    (null nil)
    ((member :wild) '(:absolute :wild-inferiors))
    ((member :unspecific) '(:relative))
    (list
     (let ((root (pop directory))
           results)
       (if (member root '(:relative :absolute))
           (push root results)
           (error "List of directory components must start with ~S or ~S."
                  :absolute :relative))
       (when directory
         (let ((next (car directory)))
           (when (or (eq :home next)
                     (typep next '(cons (eql :home) (cons string null))))
             (push (pop directory) results)))
         (dolist (piece directory)
           (typecase piece
             ((member :wild :wild-inferiors :up)
              (push piece results))
             ((member :back)
              (if (typep (car results) '(or string pattern
                                         (member :wild :wild-inferiors)))
                  (pop results)
                  (push piece results)))
             ((or string pattern)
              (when (typep piece '(and string (not simple-array)))
                (setq piece (coerce piece 'simple-string)))
              ;; Unix namestrings allow embedded "//" within them. Consecutive
              ;; slashes are treated as one, which is weird but often convenient.
              ;; However, preserving empty directory components:
              ;; - is unaesthetic
              ;; - makes (NAMESTRING (MAKE-PATHNAME :DIRECTORY '(:RELATIVE "" "d")))
              ;;   visually indistinguishable from the absolute pathname "/d/"
              ;; - can causes a pathname equality test to return NIL
              ;;   on semantically equivalent pathnames. This can happen for
              ;;   other reasons, but fewer false negatives is better.
              (unless (and (stringp piece) (zerop (length piece)))
                (push (maybe-diddle-case piece diddle-case) results)))
             (t
              (error "~S is not allowed as a directory component." piece)))))
       (nreverse results)))
    (string
     (cond ((zerop (length directory)) `(:absolute))
           (t
            (when (typep directory '(not simple-array))
              (setq directory (coerce directory 'simple-string)))
            `(:absolute ,(maybe-diddle-case directory diddle-case)))))))

(defun make-pathname (&key host
                           (device nil devp)
                           (directory nil dirp)
                           (name nil namep)
                           (type nil typep)
                           (version nil versionp)
                           defaults
                           (case :local))
  "Makes a new pathname from the component arguments. Note that host is
a host-structure or string."
  (declare (type (or string host pathname-component-tokens) host)
           (type (or string pathname-component-tokens) device)
           (type (or list string pattern pathname-component-tokens) directory)
           (type (or string pattern pathname-component-tokens) name type)
           (type (or integer pathname-component-tokens (member :newest))
                 version)
           (type (or pathname-designator null) defaults)
           (type pathname-component-case case))
  (let* ((defaults (when defaults
                     (with-pathname (defaults defaults) defaults)))
         (default-host (if defaults
                           (%pathname-host defaults)
                           (pathname-host *default-pathname-defaults*)))
         ;; Raymond Toy writes: CLHS says make-pathname can take a
         ;; string (as a logical-host) for the host part. We map that
         ;; string into the corresponding logical host structure.
         ;;
         ;; Paul Werkowski writes:
         ;; HyperSpec says for the arg to MAKE-PATHNAME;
         ;; "host---a valid physical pathname host. ..."
         ;; where it probably means -- a valid pathname host.
         ;; "valid pathname host n. a valid physical pathname host or
         ;; a valid logical pathname host."
         ;; and defines
         ;; "valid physical pathname host n. any of a string,
         ;; a list of strings, or the symbol :unspecific,
         ;; that is recognized by the implementation as the name of a host."
         ;; "valid logical pathname host n. a string that has been defined
         ;; as the name of a logical host. ..."
         ;; HS is silent on what happens if the :HOST arg is NOT one of these.
         ;; It seems an error message is appropriate.
         (host (or (find-host host nil) default-host))
         (diddle-args (and (eq (host-customary-case host) :lower)
                           (eq case :common)))
         (diddle-defaults
          (not (eq (host-customary-case host)
                   (host-customary-case default-host))))
         (dir (import-directory directory diddle-args))
         (ver (cond
               (versionp version)
               (defaults (%pathname-version defaults))
               (t nil))))
    (when (and defaults (not dirp))
      (setf dir
            (merge-directories dir
                               (%pathname-directory defaults)
                               diddle-defaults)))

    (macrolet ((pick (var varp field)
                 `(cond ((or (simple-string-p ,var)
                             (pattern-p ,var))
                         (maybe-diddle-case ,var diddle-args))
                        ((stringp ,var)
                         (maybe-diddle-case (coerce ,var 'simple-string)
                                            diddle-args))
                        (,varp
                         (maybe-diddle-case ,var diddle-args))
                        (defaults
                         (maybe-diddle-case (,field defaults)
                                            diddle-defaults))
                        (t
                         nil))))
      (intern-pathname
       host
       (pick device devp %pathname-device) ; forced to :UNSPECIFIC when logical
       dir
       (pick name namep %pathname-name)
       (pick type typep %pathname-type)
       ver))))

(defun pathname-host (pathname &key (case :local))
  "Return PATHNAME's host."
  (declare (ignore case))
  (with-pathname (pathname pathname)
    (%pathname-host pathname)))

(macrolet ((frob (name component docstring)
             `(defun ,name (pathname &key (case :local))
                ,docstring
                (with-pathname (pathname pathname)
                  (let ((effective-case (and (eq case :common)
                                             (eq (host-customary-case
                                                  (%pathname-host pathname))
                                                 :lower))))
                    (maybe-diddle-case (,component pathname) effective-case))))))

  (frob pathname-device    %pathname-device    "Return PATHNAME's device.")
  (frob pathname-directory %pathname-directory "Return PATHNAME's directory.")
  (frob pathname-name      %pathname-name      "Return PATHNAME's name.")
  (frob pathname-type      %pathname-type      "Return PATHNAME's type."))

(defun pathname-version (pathname)
  "Return PATHNAME's version."
  (with-pathname (pathname pathname)
    (%pathname-version pathname)))

;;;; namestrings

;;; Handle the case for PARSE-NAMESTRING parsing a potentially
;;; syntactically valid logical namestring with an explicit host.
;;;
;;; This then isn't fully general -- we are relying on the fact that
;;; we will only pass to parse-namestring namestring with an explicit
;;; logical host, so that we can pass the host return from
;;; parse-logical-namestring through to %PARSE-NAMESTRING as a truth
;;; value. Yeah, this is probably a KLUDGE - CSR, 2002-04-18
(defun parseable-logical-namestring-p (namestr start end)
  (and (parse-potential-logical-host namestr start end)
       (handler-case
           (let ((result (parse-logical-namestring namestr start end)))
             ;; if we got this far, we should have an explicit host
             ;; (first return value of parse-logical-namestring)
             (aver result)
             result)
         ((or simple-type-error namestring-parse-error) ()
           nil))))

(defun parse-potential-logical-host (namestr &optional (start 0) end)
  (handler-case
    (let ((colon (position #\: namestr :start start :end end)))
      (when colon
        (let ((potential-host
               (logical-word-or-lose (subseq namestr start colon))))
          (values potential-host colon))))
    (namestring-parse-error () nil)))

;;; Handle the case where PARSE-NAMESTRING is actually parsing a
;;; namestring. We pick off the :JUNK-ALLOWED case then find a host to
;;; use for parsing, call the parser, then check whether the host matches.
(defun %parse-namestring (namestr host defaults start end junk-allowed)
  (declare (type (or host null) host)
           (type string namestr)
           (type index start)
           (type (or index null) end))
  (cond
    (junk-allowed
     (handler-case
         (%parse-namestring namestr host defaults start end nil)
       (namestring-parse-error (condition)
         (values nil (namestring-parse-error-offset condition)))))
    (t
     (let ((end (%check-vector-sequence-bounds namestr start end)))
       (multiple-value-bind (new-host device directory file type version)
           ;; Comments below are quotes from the HyperSpec
           ;; PARSE-NAMESTRING entry, reproduced here to demonstrate
           ;; that we actually have to do things this way rather than
           ;; some possibly more logical way. - CSR, 2002-04-18
           (cond
             ;; "If host is a logical host then thing is parsed as a
             ;; logical pathname namestring on the host."
             (host (funcall (host-parse host) namestr start end))
             ;; "If host is nil and thing is a syntactically valid
             ;; logical pathname namestring containing an explicit
             ;; host, then it is parsed as a logical pathname
             ;; namestring."
             ((parseable-logical-namestring-p namestr start end)
              (parse-logical-namestring namestr start end))
             ;; "If host is nil, default-pathname is a logical
             ;; pathname, and thing is a syntactically valid logical
             ;; pathname namestring without an explicit host, then it
             ;; is parsed as a logical pathname namestring on the
             ;; host that is the host component of default-pathname."
             ;;
             ;; "Otherwise, the parsing of thing is
             ;; implementation-defined."
             ;;
             ;; Both clauses are handled here, as the default
             ;; *DEFAULT-PATHNAME-DEFAULTS* has a SB-IMPL::UNIX-HOST
             ;; for a host.
             ((pathname-host defaults)
              (funcall (host-parse (pathname-host defaults))
                       namestr
                       start
                       end))
             ;; I don't think we should ever get here, as the default
             ;; host will always have a non-null HOST, given that we
             ;; can't create a new pathname without going through
             ;; *DEFAULT-PATHNAME-DEFAULTS*, which has a non-null
             ;; host...
             (t (bug "Fallen through COND in %PARSE-NAMESTRING")))
         (when (and host new-host (not (eq new-host host)))
           (error 'simple-type-error
                  :datum new-host
                  ;; Note: ANSI requires that this be a TYPE-ERROR,
                  ;; but there seems to be no completely correct
                  ;; value to use for TYPE-ERROR-EXPECTED-TYPE.
                  ;; Instead, we return a sort of "type error allowed
                  ;; type", trying to say "it would be OK if you
                  ;; passed NIL as the host value" but not mentioning
                  ;; that a matching string would be OK too.
                  :expected-type 'null
                  :format-control
                  "The host in the namestring, ~S,~@
                   does not match the explicit HOST argument, ~S."
                  :format-arguments (list new-host host)))
         (let ((pn-host (or new-host host (pathname-host defaults))))
           (values (intern-pathname pn-host device directory file type version)
                   end)))))))

(defun parse-namestring (thing
                         &optional
                         host
                         (defaults *default-pathname-defaults*)
                         &key (start 0) end junk-allowed)
  (declare (ftype (function * (values (or null pathname) (or null index)))
                  %parse-namestring))
  (with-host (found-host host)
    (let (;; According to ANSI defaults may be any valid pathname designator
          (defaults (etypecase defaults
                      (pathname
                       defaults)
                      (string
                       (aver (pathnamep *default-pathname-defaults*))
                       (parse-namestring defaults))
                      (stream
                       (truename defaults)))))
      (declare (type pathname defaults))
      (etypecase thing
        (string
         (with-array-data ((thing thing) (start start) (end end)
                           :check-fill-pointer t)
           (multiple-value-bind (pathname position)
               (%parse-namestring thing found-host defaults start end junk-allowed)
             (values pathname (- position start)))))
        (pathname
         (let ((defaulted-host (or found-host (%pathname-host defaults))))
           (declare (type host defaulted-host))
           (unless (eq defaulted-host (%pathname-host thing))
             (error "The HOST argument doesn't match the pathname host:~%  ~
                    ~S and ~S."
                    defaulted-host (%pathname-host thing))))
         (values thing start))
        ((or file-stream synonym-stream)
         (values (stream-file-name-or-lose thing) nil))))))

(defun %parse-native-namestring (namestr host defaults start end junk-allowed
                                 as-directory)
  (declare (type (or host null) host)
           (type string namestr)
           (type index start)
           (type (or index null) end))
  (cond
    (junk-allowed
     (handler-case
         (%parse-native-namestring namestr host defaults start end nil as-directory)
       (namestring-parse-error (condition)
         (values nil (namestring-parse-error-offset condition)))))
    (t
     (let* ((end (%check-vector-sequence-bounds namestr start end)))
       (multiple-value-bind (new-host device directory file type version)
           (cond
             (host
              (funcall (host-parse-native host) namestr start end as-directory))
             ((pathname-host defaults)
              (funcall (host-parse-native (pathname-host defaults))
                       namestr
                       start
                       end
                       as-directory))
             ;; I don't think we should ever get here, as the default
             ;; host will always have a non-null HOST, given that we
             ;; can't create a new pathname without going through
             ;; *DEFAULT-PATHNAME-DEFAULTS*, which has a non-null
             ;; host...
             (t (bug "Fallen through COND in %PARSE-NAMESTRING")))
         (when (and host new-host (not (eq new-host host)))
           (error 'simple-type-error
                  :datum new-host
                  :expected-type `(or null (eql ,host))
                  :format-control
                  "The host in the namestring, ~S,~@
                   does not match the explicit HOST argument, ~S."
                  :format-arguments (list new-host host)))
         (let ((pn-host (or new-host host (pathname-host defaults))))
           (values (intern-pathname pn-host device directory file type version)
                   end)))))))

(defun parse-native-namestring (thing
                                &optional
                                host
                                (defaults *default-pathname-defaults*)
                                &key (start 0) end junk-allowed
                                as-directory)
  "Convert THING into a pathname, using the native conventions
appropriate for the pathname host HOST, or if not specified the
host of DEFAULTS.  If THING is a string, the parse is bounded by
START and END, and error behaviour is controlled by JUNK-ALLOWED,
as with PARSE-NAMESTRING.  For file systems whose native
conventions allow directories to be indicated as files, if
AS-DIRECTORY is true, return a pathname denoting THING as a
directory."
  (declare (type pathname-designator thing defaults)
           (type (or list host string (member :unspecific)) host)
           (type index start)
           (type (or index null) end)
           (type (or t null) junk-allowed)
           (values (or null pathname) (or null index)))
  (declare (ftype (function * (values (or null pathname) (or null index)))
                  %parse-native-namestring))
  (with-host (found-host host)
    (let ((defaults (etypecase defaults
                      (pathname
                       defaults)
                      (string
                       (aver (pathnamep *default-pathname-defaults*))
                       (parse-native-namestring defaults))
                      (stream
                       (truename defaults)))))
      (declare (type pathname defaults))
      (etypecase thing
        (string
         (with-array-data ((thing thing) (start start) (end end)
                           :check-fill-pointer t)
           (multiple-value-bind (pathname position)
               (%parse-native-namestring thing
                                         found-host defaults start end junk-allowed
                                         as-directory)
             (values pathname (- position start)))))
        (pathname
         (let ((defaulted-host (or found-host (%pathname-host defaults))))
           (declare (type host defaulted-host))
           (unless (eq defaulted-host (%pathname-host thing))
             (error "The HOST argument doesn't match the pathname host:~%  ~
                     ~S and ~S."
                    defaulted-host (%pathname-host thing))))
         (values thing start))
        ((or file-stream synonym-stream)
         (values (stream-file-name-or-lose thing) nil))))))

(defun native-namestring (pathname &key as-file)
  "Construct the full native (name)string form of PATHNAME.  For
file systems whose native conventions allow directories to be
indicated as files, if AS-FILE is true and the name, type, and
version components of PATHNAME are all NIL or :UNSPECIFIC,
construct a string that names the directory according to the file
system's syntax for files."
  (declare (type pathname-designator pathname))
  (with-native-pathname (pathname pathname)
    (when pathname
      (let ((host (or (%pathname-host pathname)
                      (no-native-namestring-error
                       pathname "there is no ~S component." :host))))
        (funcall (host-unparse-native host) pathname as-file)))))

(flet ((pathname-host-or-no-namestring (pathname)
         (or (%pathname-host pathname)
             (no-namestring-error
              pathname "there is no ~S component." :host))))

  (defun namestring (pathname)
    "Construct the full (name)string form PATHNAME."
    (with-pathname (pathname pathname)
      (when pathname
        (or (%pathname-namestring pathname)
            (let ((host (pathname-host-or-no-namestring pathname)))
              (setf (%pathname-namestring pathname)
                    (logically-readonlyize
                     (possibly-base-stringize-to-heap
                      (funcall (host-unparse host) pathname)))))))))

  (defun host-namestring (pathname)
    "Return a string representation of the name of the host in PATHNAME."
    (with-pathname (pathname pathname)
      (let ((host (pathname-host-or-no-namestring pathname)))
        (funcall (host-unparse-host host) pathname))))

  (defun directory-namestring (pathname)
    "Return a string representation of the directory in PATHNAME."
    (with-pathname (pathname pathname)
      (let ((host (pathname-host-or-no-namestring pathname)))
        (funcall (host-unparse-directory host) pathname))))

  (defun file-namestring (pathname)
    "Return a string representation of the name in PATHNAME."
    (with-pathname (pathname pathname)
      (let ((host (pathname-host-or-no-namestring pathname)))
        (funcall (host-unparse-file host) pathname))))

  (defun enough-namestring (pathname
                            &optional
                            (defaults *default-pathname-defaults*))
    "Return an abbreviated pathname sufficient to identify PATHNAME
relative to DEFAULTS."
    (with-pathname (pathname pathname)
      (let ((host (pathname-host-or-no-namestring pathname)))
        (with-pathname (defaults defaults)
          (funcall (host-unparse-enough host) pathname defaults))))))

;;;; wild pathnames

(defun wild-pathname-p (pathname &optional field-key)
  "Predicate for determining whether pathname contains any wildcards."
  (declare (type pathname-designator pathname)
           (type (member nil :host :device :directory :name :type :version)
                 field-key))
  (with-pathname (pathname pathname)
    (labels ((wildp (x)
               (or (pattern-p x) (if (member x '(:wild :wild-inferiors)) t nil)))
             (test (field)
               (wildp
                (case field
                  (:host (%pathname-host pathname)) ; always NIL
                  (:device (%pathname-device pathname))
                  (:directory
                   (return-from test (some #'wildp (%pathname-directory pathname))))
                  (:name (%pathname-name pathname))
                  (:type (%pathname-type pathname))
                  (:version (%pathname-version pathname))))))
      (if (not field-key)
          ;; SBCL does not allow :WILD in the host
          (or (test :directory) (test :directory) (test :name) (test :type) (test :version))
          (test field-key)))))

(defun pathname-match-p (in-pathname in-wildname)
  "Pathname matches the wildname template?"
  (declare (type pathname-designator in-pathname))
  (with-pathname (pathname in-pathname)
    (with-pathname (wildname in-wildname)
      (macrolet ((frob (field &optional (op 'components-match))
                   `(or (null (,field wildname))
                        (,op (,field pathname) (,field wildname)))))
        (and (or (null (%pathname-host wildname))
                 (eq (%pathname-host wildname) (%pathname-host pathname)))
             (frob %pathname-device)
             (frob %pathname-directory directory-components-match)
             (frob %pathname-name)
             (frob %pathname-type)
             (or (eq (%pathname-host wildname) *physical-host*)
                 (frob %pathname-version)))))))

;;; Place the substitutions into the pattern and return the string or pattern
;;; that results. If DIDDLE-CASE is true, we diddle the result case as well,
;;; in case we are translating between hosts with difference conventional case.
;;; The second value is the tail of subs with all of the values that we used up
;;; stripped off. Note that PATTERN-MATCHES matches all consecutive wildcards
;;; as a single string, so we ignore subsequent contiguous wildcards.
(defun substitute-into (pattern subs diddle-case)
  (declare (type pattern pattern)
           (type list subs)
           (values (or simple-string pattern) list))
  (let ((in-wildcard nil)
        (pieces nil)
        (strings nil))
    (dolist (piece (pattern-pieces pattern))
      (cond ((simple-string-p piece)
             (push piece strings)
             (setf in-wildcard nil))
            (in-wildcard)
            (t
             (setf in-wildcard t)
             (unless subs
               (error "not enough wildcards in FROM pattern to match ~
                       TO pattern:~%  ~S"
                      pattern))
             (let ((sub (pop subs)))
               (typecase sub
                 (pattern
                  (when strings
                    (push (apply #'concatenate 'simple-string
                                 (nreverse strings))
                          pieces))
                  (dolist (piece (pattern-pieces sub))
                    (push piece pieces)))
                 (simple-string
                  (push sub strings))
                 (t
                  (error "can't substitute this into the middle of a word:~
                          ~%  ~S"
                         sub)))))))

    (when strings
      (push (apply #'concatenate 'simple-string (nreverse strings))
            pieces))
    (values
     (maybe-diddle-case
      (if (and pieces (simple-string-p (car pieces)) (null (cdr pieces)))
          (car pieces)
          (make-pattern (nreverse pieces)))
      diddle-case)
     subs)))

;;; Called when we can't see how source and from matched.
(defun didnt-match-error (source from)
  (error "Pathname components from SOURCE and FROM args to TRANSLATE-PATHNAME~@
          did not match:~%  ~S ~S"
         source from))

;;; Do TRANSLATE-COMPONENT for all components except host, directory
;;; and version.
(defun translate-component (source from to diddle-case)
  (typecase to
    (pattern
     (typecase from
       (pattern
        (typecase source
          (pattern
           (if (pattern= from source)
               source
               (didnt-match-error source from)))
          (simple-string
           (multiple-value-bind (won subs) (pattern-matches from source)
             (if won
                 (values (substitute-into to subs diddle-case))
                 (didnt-match-error source from))))
          (t
           (maybe-diddle-case source diddle-case))))
       ((member :wild)
        (values (substitute-into to (list source) diddle-case)))
       (t
        (if (components-match source from)
            (maybe-diddle-case source diddle-case)
            (didnt-match-error source from)))))
    ((member nil :wild)
     (maybe-diddle-case source diddle-case))
    (t
     (if (components-match source from)
         to
         (didnt-match-error source from)))))

;;; Return a list of all the things that we want to substitute into the TO
;;; pattern (the things matched by from on source.)  When From contains
;;; :WILD-INFERIORS, the result contains a sublist of the matched source
;;; subdirectories.
(defun compute-directory-substitutions (orig-source orig-from)
  (let ((source orig-source)
        (from orig-from))
    (collect ((subs))
      (loop
        (unless source
          (unless (every (lambda (x) (eq x :wild-inferiors)) from)
            (didnt-match-error orig-source orig-from))
          (subs ())
          (return))
        (unless from (didnt-match-error orig-source orig-from))
        (let ((from-part (pop from))
              (source-part (pop source)))
          (typecase from-part
            (pattern
             (typecase source-part
               (pattern
                (if (pattern= from-part source-part)
                    (subs source-part)
                    (didnt-match-error orig-source orig-from)))
               (simple-string
                (multiple-value-bind (won new-subs)
                    (pattern-matches from-part source-part)
                  (if won
                      (dolist (sub new-subs)
                        (subs sub))
                      (didnt-match-error orig-source orig-from))))
               (t
                (didnt-match-error orig-source orig-from))))
            ((member :wild)
             (subs source-part))
            ((member :wild-inferiors)
             (let ((remaining-source (cons source-part source)))
               (collect ((res))
                 (loop
                   (when (directory-components-match remaining-source from)
                     (return))
                   (unless remaining-source
                     (didnt-match-error orig-source orig-from))
                   (res (pop remaining-source)))
                 (subs (res))
                 (setq source remaining-source))))
            (simple-string
             (unless (and (simple-string-p source-part)
                          (string= from-part source-part))
               (didnt-match-error orig-source orig-from)))
            (t
             (didnt-match-error orig-source orig-from)))))
      (subs))))

;;; This is called by TRANSLATE-PATHNAME on the directory components
;;; of its argument pathnames to produce the result directory
;;; component. If this leaves the directory NIL, we return the source
;;; directory. The :RELATIVE or :ABSOLUTE is taken from the source
;;; directory, except if TO is :ABSOLUTE, in which case the result
;;; will be :ABSOLUTE.
(defun translate-directories (source from to diddle-case)
  (if (not (and source to from))
      (or (and to (null source) (remove :wild-inferiors to))
          (mapcar (lambda (x) (maybe-diddle-case x diddle-case)) source))
      (collect ((res))
               ;; If TO is :ABSOLUTE, the result should still be :ABSOLUTE.
               (res (if (eq (first to) :absolute)
                 :absolute
                 (first source)))
        (let ((subs-left (compute-directory-substitutions (rest source)
                                                          (rest from))))
          (dolist (to-part (rest to))
            (typecase to-part
              ((member :wild)
               (aver subs-left)
               (let ((match (pop subs-left)))
                 (when (listp match)
                   (error ":WILD-INFERIORS is not paired in from and to ~
                           patterns:~%  ~S ~S" from to))
                 (res (maybe-diddle-case match diddle-case))))
              ((member :wild-inferiors)
               (aver subs-left)
               (let ((match (pop subs-left)))
                 (unless (listp match)
                   (error ":WILD-INFERIORS not paired in from and to ~
                           patterns:~%  ~S ~S" from to))
                 (dolist (x match)
                   (res (maybe-diddle-case x diddle-case)))))
              (pattern
               (multiple-value-bind
                   (new new-subs-left)
                   (substitute-into to-part subs-left diddle-case)
                 (setf subs-left new-subs-left)
                 (res new)))
              (t (res to-part)))))
        (res))))

(defun translate-pathname (source from-wildname to-wildname &key)
  "Use the source pathname to translate the from-wildname's wild and
unspecified elements into a completed to-pathname based on the to-wildname."
  (declare (type pathname-designator source from-wildname to-wildname))
  (with-pathname (source source)
    (with-pathname (from from-wildname)
      (with-pathname (to to-wildname)
          (let* ((source-host (%pathname-host source))
                 (from-host (%pathname-host from))
                 (to-host (%pathname-host to))
                 (diddle-case
                  (and source-host to-host
                       (not (eq (host-customary-case source-host)
                                (host-customary-case to-host))))))
            (macrolet ((frob (field &optional (op 'translate-component))
                         `(let ((result (,op (,field source)
                                             (,field from)
                                             (,field to)
                                             diddle-case)))
                            (if (eq result :error)
                                (error "~S doesn't match ~S." source from)
                                result))))
              (intern-pathname
               (or to-host source-host)
               (frob %pathname-device)
               (frob %pathname-directory translate-directories)
               (frob %pathname-name)
               (frob %pathname-type)
               (if (eq from-host *physical-host*)
                   (if (or (eq (%pathname-version to) :wild)
                           (eq (%pathname-version to) nil))
                       (%pathname-version source)
                       (%pathname-version to))
                   (frob %pathname-version)))))))))

;;;;  logical pathname support. ANSI 92-102 specification.
;;;;
;;;;  As logical-pathname translations are loaded they are
;;;;  canonicalized as patterns to enable rapid efficient translation
;;;;  into physical pathnames.

;;;; utilities

;;; Access *DEFAULT-PATHNAME-DEFAULTS*, issuing a warning if its value
;;; is silly. (Unlike the vaguely-analogous SANE-PACKAGE, we don't
;;; actually need to reset the variable when it's silly, since even
;;; crazy values of *DEFAULT-PATHNAME-DEFAULTS* don't leave the system
;;; in a state where it's hard to recover interactively.)
(defun sane-default-pathname-defaults ()
  (let* ((dfd *default-pathname-defaults*)
         (dfd-dir (pathname-directory dfd)))
    ;; It's generally not good to use a relative pathname for
    ;; *DEFAULT-PATHNAME-DEFAULTS*, since relative pathnames
    ;; are defined by merging into a default pathname (which is,
    ;; by default, *DEFAULT-PATHNAME-DEFAULTS*).
    (when (and (consp dfd-dir)
               (eql (first dfd-dir) :relative))
      (warn
       "~@<~S is a relative pathname. (But we'll try using it anyway.)~@:>"
       '*default-pathname-defaults*))
    dfd))

(defun simplify-namestring (namestring &optional host)
  (funcall (host-simplify-namestring
            (or host
                (pathname-host (sane-default-pathname-defaults))))
           namestring))

(defun lpn-word-char-p (char)
  ;; This predicate is just {alpha|digit|dash} but by expressing it as
  ;; range comparison we can - I hope - avoid cross-compiling many of
  ;; the Unicode tables and particularly MISC-INDEX. Taking them out of
  ;; make-host-2 removes some hassle around dumping specialized vectors.
  (and (typep (truly-the character char) 'base-char)
       (let ((code (char-code char)))
         (or (<= (char-code #\a) code (char-code #\z))
             (<= (char-code #\A) code (char-code #\Z))
             (<= (char-code #\0) code (char-code #\9))
             (= code (char-code #\-))))))

;;; Canonicalize a logical pathname word by uppercasing it checking that it
;;; contains only legal characters.
(defun logical-word-or-lose (word)
  (declare (string word))
  ;; Maybe this function used to be called only on the HOST part of a namestring,
  ;; and so the error message about an empty string made sense in that it mentioned
  ;; "logical host", but this is also called by UPCASE-MAYBE via INTERN-PATHNAME
  ;; on every part - name, type, and directory.
  ;; Maybe INTERN-PATHNAME is the one that's wrong?
  (when (string= word "")
    ;; https://www.lispworks.com/documentation/HyperSpec/Body/19_cbb.htm
    (error 'namestring-parse-error
           :complaint "A string of length 0 is not a valid value for any ~
                       component of a logical pathname"
           :args (list word)
           :namestring word :offset 0))
  (dotimes (i (length word) (string-upcase word))
    ;; um, how do we know it's SIMPLE-STRING when the decl at the top
    ;; only says STRING?
    (let ((ch (schar word i)))
      (unless (lpn-word-char-p ch)
          (error 'namestring-parse-error
                 :complaint "logical namestring character which ~
                             is not alphanumeric or hyphen:~%  ~S"
                 :args (list ch)
                 :namestring word :offset i)))))

;;; Given a logical host or string, return a logical host. If ERROR-P
;;; is NIL, then return NIL when no such host exists.
(defun find-logical-host (thing &optional (errorp t))
  (etypecase thing
    (string
     (let* ((name (logical-word-or-lose thing))
            (hash (sxhash name))
            ;; Can do better here: binary search, since we maintain sorted order.
            (found (dovector (x *logical-hosts*)
                     (when (and (eq (logical-host-name-hash x) hash)
                                (string= (logical-host-name x) name))
                       (return x)))))
       (if (or found (not errorp))
           found
           ;; This is the error signalled from e.g.
           ;; LOGICAL-PATHNAME-TRANSLATIONS when host is not a defined
           ;; host, and ANSI specifies that that's a TYPE-ERROR.
           (error 'simple-type-error
                  :datum thing
                  ;; God only knows what ANSI expects us to use for
                  ;; the EXPECTED-TYPE here. Maybe this will be OK..
                  :expected-type
                  '(and string (satisfies logical-pathname-translations))
                  :format-control "logical host not yet defined: ~S"
                  :format-arguments (list thing)))))
    (logical-host thing)))

;;; Given a logical host name or host, return a logical host, creating
;;; a new one if necessary.
(defun intern-logical-host (thing &aux name host)
  (loop
    (awhen (find-logical-host thing nil) (return it))
    (unless name
      (setq name (logical-word-or-lose thing)
            host (make-logical-host :name name :name-hash (sxhash name))))
    (let* ((old *logical-hosts*)
           (new (merge 'vector old (list host) #'string< :key #'logical-host-name)))
      (when (eq (cas *logical-hosts* old new) old)
        (return host)))))

;;;; logical pathname parsing

;;; Deal with multi-char wildcards in a logical pathname token.
(defun maybe-make-logical-pattern (namestring chunks)
  (let ((chunk (caar chunks)))
    (collect ((pattern))
      (let ((last-pos 0)
            (len (length chunk)))
        (declare (fixnum last-pos))
        (loop
          (when (= last-pos len) (return))
          (let ((pos (or (position #\* chunk :start last-pos) len)))
            (if (= pos last-pos)
                (when (pattern)
                  (error 'namestring-parse-error
                         :complaint "double asterisk inside of logical ~
                                     word: ~S"
                         :args (list chunk)
                         :namestring namestring
                         :offset (+ (cdar chunks) pos)))
                (pattern (subseq chunk last-pos pos)))
            (if (= pos len)
                (return)
                (pattern :multi-char-wild))
            (setq last-pos (1+ pos)))))
        (aver (pattern))
        (if (cdr (pattern))
            (make-pattern (pattern))
            (let ((x (car (pattern))))
              (if (eq x :multi-char-wild)
                  :wild
                  x))))))

;;; Return a list of conses where the CDR is the start position and
;;; the CAR is a string (token) or character (punctuation.)
(defun logical-chunkify (namestr start end)
  (collect ((chunks))
    (do ((i start (1+ i))
         (prev start))
        ((= i end)
         (when (> end prev)
            (chunks (cons (nstring-upcase (subseq namestr prev end)) prev))))
      (let ((ch (schar namestr i)))
        (unless (or (lpn-word-char-p ch) (char= ch #\*))
          (when (> i prev)
            (chunks (cons (nstring-upcase (subseq namestr prev i)) prev)))
          (setq prev (1+ i))
          (unless (member ch '(#\; #\: #\.))
            (error 'namestring-parse-error
                   :complaint "illegal character for logical pathname:~%  ~S"
                   :args (list ch)
                   :namestring namestr
                   :offset i))
          (chunks (cons ch i)))))
    (chunks)))

;;; Break up a logical-namestring, always a string, into its
;;; constituent parts.
(defun parse-logical-namestring (namestr start end)
  (declare (type simple-string namestr)
           (type index start end))
  (collect ((directory))
    (let ((host nil)
          (name nil)
          (type nil)
          (version nil))
      (labels ((expecting (what chunks)
                 (unless (and chunks (simple-string-p (caar chunks)))
                   (error 'namestring-parse-error
                          :complaint "expecting ~A, got ~:[nothing~;~S~]."
                          :args (list what (caar chunks) (caar chunks))
                          :namestring namestr
                          :offset (if chunks (cdar chunks) end)))
                 (caar chunks))
               (parse-host (chunks)
                 (case (caadr chunks)
                   (#\:
                    (setq host
                          (find-logical-host (expecting "a host name" chunks)))
                    (parse-relative (cddr chunks)))
                   (t
                    (parse-relative chunks))))
               (parse-relative (chunks)
                 (case (caar chunks)
                   (#\;
                    (directory :relative)
                    (parse-directory (cdr chunks)))
                   (t
                    (directory :absolute) ; Assumption! Maybe revoked later.
                    (parse-directory chunks))))
               (parse-directory (chunks)
                 (case (caadr chunks)
                   (#\;
                    (directory
                     (let ((res (expecting "a directory name" chunks)))
                       (cond ((string= res "..") :up)
                             ((string= res "**") :wild-inferiors)
                             (t
                              (maybe-make-logical-pattern namestr chunks)))))
                    (parse-directory (cddr chunks)))
                   (t
                    (parse-name chunks))))
               (parse-name (chunks)
                 (when chunks
                   (expecting "a file name" chunks)
                   (setq name (maybe-make-logical-pattern namestr chunks))
                   (expecting-dot (cdr chunks))))
               (expecting-dot (chunks)
                 (when chunks
                   (unless (eql (caar chunks) #\.)
                     (error 'namestring-parse-error
                            :complaint "expecting a dot, got ~S."
                            :args (list (caar chunks))
                            :namestring namestr
                            :offset (cdar chunks)))
                   (if type
                       (parse-version (cdr chunks))
                       (parse-type (cdr chunks)))))
               (parse-type (chunks)
                 (expecting "a file type" chunks)
                 (setq type (maybe-make-logical-pattern namestr chunks))
                 (expecting-dot (cdr chunks)))
               (parse-version (chunks)
                 (let ((str (expecting "a positive integer, * or NEWEST"
                                       chunks)))
                   (cond
                    ((string= str "*") (setq version :wild))
                    ((string= str "NEWEST") (setq version :newest))
                    (t
                     (multiple-value-bind (res pos)
                         (parse-integer str :junk-allowed t)
                       (unless (and res (plusp res))
                         (error 'namestring-parse-error
                                :complaint "expected a positive integer, ~
                                            got ~S"
                                :args (list str)
                                :namestring namestr
                                :offset (+ pos (cdar chunks))))
                       (setq version res)))))
                 (when (cdr chunks)
                   (error 'namestring-parse-error
                          :complaint "extra stuff after end of file name"
                          :namestring namestr
                          :offset (cdadr chunks)))))
        (parse-host (logical-chunkify namestr start end)))
      (values host :unspecific (directory) name type version))))

;;; Return a value suitable, e.g., for preinitializing
;;; *DEFAULT-PATHNAME-DEFAULTS* before *DEFAULT-PATHNAME-DEFAULTS* is
;;; initialized (at which time we can't safely call e.g. #'PATHNAME).
(defun make-trivial-default-logical-pathname ()
  (intern-pathname (load-time-value (make-logical-host :name "") t)
                   :unspecific nil nil nil nil))

(defun logical-namestring-p (x)
  (and (stringp x)
       (ignore-errors
         (typep (pathname x) 'logical-pathname))))

(deftype logical-namestring ()
  `(satisfies logical-namestring-p))

(defun logical-pathname (pathspec)
  "Converts the pathspec argument to a logical-pathname and returns it."
  (declare (type (or logical-pathname string stream) pathspec)
           (values logical-pathname))
  (if (typep pathspec 'logical-pathname)
      pathspec
      (flet ((oops (problem)
               (error 'simple-type-error
                      :datum pathspec
                      :expected-type 'logical-namestring
                      :format-control "~S is not a valid logical namestring:~%  ~A"
                      :format-arguments (list pathspec problem))))
        (handler-case
            (if (streamp pathspec)
                (pathname pathspec)
                (let ((potential-host (parse-potential-logical-host pathspec)))
                  (if potential-host
                      (values (parse-namestring
                               pathspec (find-logical-host potential-host)))
                      (error "no host specified"))))
          (error (e) (oops e))))))


;;;; logical pathname unparsing

(defun unparse-logical-directory (pathname)
  (declare (type pathname pathname))
  (collect ((pieces))
    (let ((directory (%pathname-directory pathname)))
      (when directory
        (ecase (pop directory)
          (:absolute) ; nothing special
          (:relative (pieces ";")))
        (dolist (dir directory)
          (cond ((or (stringp dir) (pattern-p dir))
                 (pieces (unparse-logical-piece dir))
                 (pieces ";"))
                ((eq dir :wild)
                 (pieces "*;"))
                ((eq dir :wild-inferiors)
                 (pieces "**;"))
                (t
                 (error "invalid directory component: ~S" dir))))))
    (apply #'concatenate 'simple-string (pieces))))

(defun unparse-logical-piece (thing)
  (etypecase thing
    ((member :wild) "*")
    (simple-string thing)
    (pattern
     (collect ((strings))
       (dolist (piece (pattern-pieces thing))
         (etypecase piece
           (simple-string (strings piece))
           (keyword
            (cond ((eq piece :wild-inferiors)
                   (strings "**"))
                  ((eq piece :multi-char-wild)
                   (strings "*"))
                  (t (error "invalid keyword: ~S" piece))))))
       (apply #'concatenate 'simple-string (strings))))))

(defun unparse-logical-file (pathname)
  (declare (type pathname pathname))
    (collect ((strings))
    (let* ((name (%pathname-name pathname))
           (type (%pathname-type pathname))
           (version (%pathname-version pathname))
           (type-supplied (pathname-component-present-p type))
           (version-supplied (pathname-component-present-p version)))
      (when name
        (when (and (null type)
                   (typep name 'string)
                   (position #\. name :start 1))
          (error "too many dots in the name: ~S" pathname))
        (strings (unparse-logical-piece name)))
      (when type-supplied
        (unless name
          (error "cannot specify the type without a file: ~S" pathname))
        (when (typep type 'string)
          (when (position #\. type)
            (error "type component can't have a #\. inside: ~S" pathname)))
        (strings ".")
        (strings (unparse-logical-piece type)))
      (when version-supplied
        (unless type-supplied
          (error "cannot specify the version without a type: ~S" pathname))
        (etypecase version
          ((member :newest) (strings ".NEWEST")) ; really? not in LPNIFY-NAMESTRING
          ((member :wild) (strings ".*"))
          (fixnum (strings ".") (strings (format nil "~D" version))))))
    (apply #'concatenate 'simple-string (strings))))

;;; Unparse a logical pathname string.
(defun unparse-enough-namestring (pathname defaults)
  (let* ((path-directory (pathname-directory pathname))
         (def-directory (pathname-directory defaults))
         (enough-directory
           ;; Go down the directory lists to see what matches.  What's
           ;; left is what we want, more or less.
           (cond ((and (eq (first path-directory) (first def-directory))
                       (eq (first path-directory) :absolute))
                   ;; Both paths are :ABSOLUTE, so find where the
                   ;; common parts end and return what's left
                   (do* ((p (rest path-directory) (rest p))
                         (d (rest def-directory) (rest d)))
                        ((or (endp p) (endp d)
                             (not (equal (first p) (first d))))
                         `(:relative ,@p))))
                 (t
                   ;; At least one path is :RELATIVE, so just return the
                   ;; original path.  If the original path is :RELATIVE,
                   ;; then that's the right one.  If PATH-DIRECTORY is
                   ;; :ABSOLUTE, we want to return that except when
                   ;; DEF-DIRECTORY is :ABSOLUTE, as handled above. so return
                   ;; the original directory.
                   path-directory))))
    (unparse-logical-namestring
     (make-pathname :host (pathname-host pathname)
                    :directory enough-directory
                    :name (pathname-name pathname)
                    :type (pathname-type pathname)
                    :version (pathname-version pathname)))))

(defun unparse-logical-namestring (pathname)
  (declare (type logical-pathname pathname))
  (concatenate 'simple-string
               (logical-host-name (%pathname-host pathname)) ":"
               (unparse-logical-directory pathname)
               (unparse-logical-file pathname)))

;;;; logical pathname translations

;;; Verify that the list of translations consists of lists and prepare
;;; canonical translations. (Parse pathnames and expand out wildcards
;;; into patterns.)
(defun canonicalize-logical-pathname-translations (translation-list host)
  (declare (type list translation-list) (type host host)
           (values list))
  (mapcar (lambda (translation)
            (destructuring-bind (from to) translation
              (list (if (typep from 'logical-pathname)
                        from
                        (parse-namestring from host))
                    (pathname to))))
          translation-list))

(defun logical-pathname-translations (host)
  "Return the (logical) host object argument's list of translations."
  (declare (type (or string logical-host) host)
           (values list))
  (logical-host-translations (find-logical-host host)))

(defun (setf logical-pathname-translations) (translations host)
  "Set the translations list for the logical host argument."
  (declare (type (or string logical-host) host)
           (type list translations)
           (values list))
  (let ((host (intern-logical-host host)))
    (setf (logical-host-canon-transls host)
          (canonicalize-logical-pathname-translations translations host))
    (setf (logical-host-translations host) translations)))

(defun translate-logical-pathname (pathname &key)
  "Translate PATHNAME to a physical pathname, which is returned."
  (declare (type pathname-designator pathname)
           (values (or null pathname)))
  (typecase pathname
    (logical-pathname
     (dolist (x (logical-host-canon-transls (%pathname-host pathname))
                (error 'simple-file-error
                       :pathname pathname
                       :format-control "no translation for ~S"
                       :format-arguments (list pathname)))
       (destructuring-bind (from to) x
         (when (pathname-match-p pathname from)
           (return (translate-logical-pathname
                    (translate-pathname pathname from to)))))))
    (pathname pathname)
    (t (translate-logical-pathname (pathname pathname)))))

;;; Given a pathname, return a corresponding physical pathname.
(defun physicalize-pathname (possibly-logical-pathname)
  (if (typep possibly-logical-pathname 'logical-pathname)
      (translate-logical-pathname possibly-logical-pathname)
      possibly-logical-pathname))

(defun load-logical-pathname-translations (host)
  "Reads logical pathname translations from SYS:SITE;HOST.TRANSLATIONS.NEWEST,
with HOST replaced by the supplied parameter. Returns T on success.

If HOST is already defined as logical pathname host, no file is loaded and NIL
is returned.

The file should contain a single form, suitable for use with
\(SETF LOGICAL-PATHNAME-TRANSLATIONS).

Note: behaviour of this function is highly implementation dependent, and
historically it used to be a no-op in SBCL -- the current approach is somewhat
experimental and subject to change."
  (declare (type string host)
           (values (member t nil)))
  (if (find-logical-host host nil)
      ;; This host is already defined, all is well and good.
      nil
      ;; ANSI: "The specific nature of the search is
      ;; implementation-defined."
      (prog1 t
        (setf (logical-pathname-translations host)
              (with-open-file (lpt (make-pathname :host "SYS"
                                                  :directory '(:absolute "SITE")
                                                  :name host
                                                  :type "TRANSLATIONS"
                                                  :version :newest))
                (read lpt))))))

(defun !lpn-cold-init ()
  (let* ((sys *default-pathname-defaults*)
         (src
          (merge-pathnames
           (make-pathname :directory '(:relative "src" :wild-inferiors)
                          :name :wild :type :wild)
           sys))
         (contrib
          (merge-pathnames
           (make-pathname :directory '(:relative "contrib" :wild-inferiors)
                          :name :wild :type :wild)
           sys))
         (output
          (merge-pathnames
           (make-pathname :directory '(:relative "output" :wild-inferiors)
                          :name :wild :type :wild)
           sys)))
    (setf (logical-pathname-translations "SYS")
          `(("SYS:SRC;**;*.*.*" ,src)
            ("SYS:CONTRIB;**;*.*.*" ,contrib)
            ("SYS:OUTPUT;**;*.*.*" ,output)))))

(defun set-sbcl-source-location (pathname)
  "Initialize the SYS logical host based on PATHNAME, which should be
the top-level directory of the SBCL sources. This will replace any
existing translations for \"SYS:SRC;\", \"SYS:CONTRIB;\", and
\"SYS:OUTPUT;\". Other \"SYS:\" translations are preserved."
  (let ((truename (truename pathname))
        (current-translations
         (remove-if (lambda (translation)
                      (or (pathname-match-p "SYS:SRC;" translation)
                          (pathname-match-p "SYS:CONTRIB;" translation)
                          (pathname-match-p "SYS:OUTPUT;" translation)))
                    (logical-pathname-translations "SYS")
                    :key #'first)))
    (flet ((physical-target (component)
             (merge-pathnames
              (make-pathname :directory (list :relative component
                                              :wild-inferiors)
                             :name :wild
                             :type :wild)
              truename)))
      (setf (logical-pathname-translations "SYS")
            `(("SYS:SRC;**;*.*.*" ,(physical-target "src"))
              ("SYS:CONTRIB;**;*.*.*" ,(physical-target "contrib"))
              ("SYS:OUTPUT;**;*.*.*" ,(physical-target "output"))
              ,@current-translations)))))

(defmethod make-load-form ((pn pathname) &optional env)
  (declare (ignore env))
  (labels ((reconstruct (component)
             (cond ((pattern-p component) (patternify component))
                   ((and (listp component) (some #'pattern-p component))
                    (cons 'list (mapcar #'patternify component)))
                   (t `',component)))
           (patternify (subcomponent)
             (if (pattern-p subcomponent)
                 `(make-pattern ',(pattern-pieces subcomponent))
                 `',subcomponent)))
    (values `(intern-pathname
              ,(if (typep pn 'logical-pathname)
                   `(find-logical-host ',(logical-host-name (%pathname-host pn)))
                   '*physical-host*)
              ,(reconstruct (%pathname-device pn))
              ,(reconstruct (%pathname-directory pn))
              ,(reconstruct (%pathname-name pn))
              ,(reconstruct (%pathname-type pn))
              ,(reconstruct (%pathname-version pn))))))
