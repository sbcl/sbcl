;;;; that part of the CMU CL package.lisp file which can run on the
;;;; cross-compilation host

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;;; the PACKAGE-HASHTABLE structure

;;; comment from CMU CL:
;;;      Packages are implemented using a special kind of hashtable. It is
;;;   an open hashtable with a parallel 8-bit I-vector of hash-codes. The
;;;   primary purpose of the hash for each entry is to reduce paging by
;;;   allowing collisions and misses to be detected without paging in the
;;;   symbol and pname for an entry. If the hash for an entry doesn't
;;;   match that for the symbol that we are looking for, then we can
;;;   go on without touching the symbol, pname, or even hastable vector.
;;;      It turns out that, contrary to my expectations, paging is a very
;;;   important consideration the design of the package representation.
;;;   Using a similar scheme without the entry hash, the fasloader was
;;;   spending more than half its time paging in INTERN.
;;;      The hash code also indicates the status of an entry. If it zero,
;;;   the entry is unused. If it is one, then it is deleted.
;;;   Double-hashing is used for collision resolution.

(def!type hash-vector () '(simple-array (unsigned-byte 8) (*)))

(def!struct (package-hashtable
             (:constructor %make-package-hashtable
                           (table hash size &aux (free size)))
             (:copier nil))
  ;; These first two slots would be better named as 'sym-vec'
  ;; and 'hash-vec' which is how they're referred to in code.
  ;; The g-vector of symbols.
  (table (missing-arg) :type simple-vector)
  ;; The i-vector of pname hash values.
  (hash (missing-arg) :type hash-vector)
  ;; The total number of entries allowed before resizing.
  ;;
  ;; FIXME: CAPACITY would be a more descriptive name. (This is
  ;; related to but not quite the same as HASH-TABLE-SIZE, so calling
  ;; it SIZE seems somewhat misleading.)
  (size (missing-arg) :type index)
  ;; The remaining number of entries that can be made before we have to rehash.
  (free (missing-arg) :type index)
  ;; The number of deleted entries.
  (deleted 0 :type index))

;;;; the PACKAGE structure

;;; Meta: the original comment by WHN is partially untrue.
;;;  IN-PACKAGE references a string constant, not a package; and moreover,
;;;  xc never dumps constant package objects. If, as implied by the word
;;;  "nuisance" we would like this to be done differently, perhaps it could
;;;  be now, since the alleged rationale no longer exists.
;;;  I tested that it works to remove the :make-load-form-fun and instead
;;;  define an ordinary MAKE-LOAD-FORM method in PCL compilation.
;;; KLUDGE: We use DEF!STRUCT to define this not because we need to
;;; manipulate target package objects on the cross-compilation host,
;;; but only because its MAKE-LOAD-FORM function needs to be hooked
;;; into the pre-CLOS DEF!STRUCT MAKE-LOAD-FORM system so that we can
;;; compile things like IN-PACKAGE in warm init before CLOS is set up.
;;; The DEF!STRUCT side effect of defining a new PACKAGE type on the
;;; cross-compilation host is just a nuisance, and in order to avoid
;;; breaking the cross-compilation host, we need to work around it
;;; around by putting the new PACKAGE type (and the PACKAGEP predicate
;;; too..) into SB!XC. -- WHN 20000309
(def!struct (sb!xc:package
             (:constructor internal-make-package)
             (:make-load-form-fun (lambda (p)
                                    (values `(find-undeleted-package-or-lose
                                              ',(package-name p))
                                            nil)))
             (:predicate sb!xc:packagep))
  #!+sb-doc
  "the standard structure for the description of a package"
  ;; the name of the package, or NIL for a deleted package
  (%name nil :type (or simple-string null))
  ;; nickname strings
  (%nicknames () :type list)
  ;; packages used by this package
  (%use-list () :type list)
  ;; a simple-vector of the external symbol hashtables for used packages.
  ;; Derived from %USE-LIST, but maintained separately.
  (tables #() :type simple-vector)
  ;; index into TABLES of the table in which an inherited symbol was most
  ;; recently found. On the next FIND-SYMBOL* operation, the indexed table
  ;; is tested first.
  (mru-table-index 0 :type index)
  ;; packages that use this package
  (%used-by-list () :type list)
  ;; PACKAGE-HASHTABLEs of internal & external symbols
  (internal-symbols (missing-arg) :type package-hashtable)
  (external-symbols (missing-arg) :type package-hashtable)
  ;; shadowing symbols
  (%shadowing-symbols () :type list)
  ;; documentation string for this package
  (doc-string nil :type (or simple-string null))
  ;; package locking
  #!+sb-package-locks
  (lock nil :type boolean)
  #!+sb-package-locks
  (%implementation-packages nil :type list)
  ;; Definition source location
  (source-location nil :type (or null sb!c:definition-source-location))
  ;; Local package nicknames.
  (%local-nicknames nil :type list)
  (%locally-nicknamed-by nil :type list))

;;;; iteration macros

;; Concerning traversal, it's not clear that scanning the hash-vector makes
;; the code faster, it might even make it slower because assuming tables
;; are half full, it's 150% as many reads as just reading the symbols.
;; If uninterning stored 0 for the tombstone instead of NIL in the symbol
;; vector, we wouldn't mistake it for a genuinely present symbol.

(defmacro-mundanely do-symbols ((var &optional
                                     (package '*package*)
                                     result-form)
                                &body body-decls)
  #!+sb-doc
  "DO-SYMBOLS (VAR [PACKAGE [RESULT-FORM]]) {DECLARATION}* {TAG | FORM}*
   Executes the FORMs at least once for each symbol accessible in the given
   PACKAGE with VAR bound to the current symbol."
  (multiple-value-bind (body decls)
      (parse-body body-decls :doc-string-allowed nil)
    (let ((flet-name (sb!xc:gensym "DO-SYMBOLS-")))
      `(block nil
         (flet ((,flet-name (,var)
                  ,@decls
                  (tagbody ,@body)))
           (let* ((package (find-undeleted-package-or-lose ,package))
                  (shadows (package-%shadowing-symbols package)))
             (flet ((iterate-over-hash-table (table ignore)
                      (let ((hash-vec (package-hashtable-hash table))
                            (sym-vec (package-hashtable-table table)))
                        (dotimes (i (length sym-vec))
                          (when (>= (aref hash-vec i) 2)
                            (let ((sym (aref sym-vec i)))
                              (declare (inline member))
                              (unless (member sym ignore :test #'string=)
                                (,flet-name sym))))))))
               (iterate-over-hash-table (package-internal-symbols package) nil)
               (iterate-over-hash-table (package-external-symbols package) nil)
               (dolist (use (package-%use-list package))
                 (iterate-over-hash-table (package-external-symbols use)
                                          shadows)))))
         (let ((,var nil))
           (declare (ignorable ,var))
           ,@decls
           ,result-form)))))

(defmacro-mundanely do-external-symbols ((var &optional
                                              (package '*package*)
                                              result-form)
                                         &body body-decls)
  #!+sb-doc
  "DO-EXTERNAL-SYMBOLS (VAR [PACKAGE [RESULT-FORM]]) {DECL}* {TAG | FORM}*
   Executes the FORMs once for each external symbol in the given PACKAGE with
   VAR bound to the current symbol."
  (multiple-value-bind (body decls)
      (parse-body body-decls :doc-string-allowed nil)
    (let ((flet-name (sb!xc:gensym "DO-SYMBOLS-")))
      `(block nil
         (flet ((,flet-name (,var)
                  ,@decls
                  (tagbody ,@body)))
           (let* ((package (find-undeleted-package-or-lose ,package))
                  (table (package-external-symbols package))
                  (hash-vec (package-hashtable-hash table))
                  (sym-vec (package-hashtable-table table)))
             (dotimes (i (length sym-vec))
               (when (>= (aref hash-vec i) 2)
                 (,flet-name (aref sym-vec i))))))
         (let ((,var nil))
           (declare (ignorable ,var))
           ,@decls
           ,result-form)))))

(defmacro-mundanely do-all-symbols ((var &optional
                                         result-form)
                                    &body body-decls)
  #!+sb-doc
  "DO-ALL-SYMBOLS (VAR [RESULT-FORM]) {DECLARATION}* {TAG | FORM}*
   Executes the FORMs once for each symbol in every package with VAR bound
   to the current symbol."
  (multiple-value-bind (body decls)
      (parse-body body-decls :doc-string-allowed nil)
    (let ((flet-name (sb!xc:gensym "DO-SYMBOLS-")))
      `(block nil
         (flet ((,flet-name (,var)
                  ,@decls
                  (tagbody ,@body)))
           (dolist (package (list-all-packages))
             (flet ((iterate-over-hash-table (table)
                      (let ((hash-vec (package-hashtable-hash table))
                            (sym-vec (package-hashtable-table table)))
                        (dotimes (i (length sym-vec))
                          (when (>= (aref hash-vec i) 2)
                            (,flet-name (aref sym-vec i)))))))
               (iterate-over-hash-table (package-internal-symbols package))
               (iterate-over-hash-table (package-external-symbols package)))))
         (let ((,var nil))
           (declare (ignorable ,var))
           ,@decls
           ,result-form)))))

;;;; WITH-PACKAGE-ITERATOR

(defvar *empty-package-hashtable*
    (%make-package-hashtable
     #() (make-array 0 :element-type '(unsigned-byte 8)) 0))

(defun package-iter-init (access-types pkg-designator-list)
  (declare (type (integer 1 7) access-types)) ; a nonzero bitmask over types
  (values (logior (ash access-types 3) #b11) 0 *empty-package-hashtable*
          (package-listify pkg-designator-list)))

;; The STATE parameter is comprised of 4 packed fields
;;  [0:1] = substate {0=internal,1=external,2=inherited,3=initial}
;;  [2]   = package with inherited symbols has shadowing symbols
;;  [3:5] = enabling bits for {internal,external,inherited}
;;  [6:]  = index into 'package-tables'
;;
(defconstant +package-iter-check-shadows+  #b000100)

(defun package-iter-step (start-state index table pkglist)
  ;; the defknown isn't enough
  (declare (type fixnum start-state) (type index index)
           (type package-hashtable table) (type list pkglist))
  (declare (optimize speed))
  (labels
      ((advance (state) ; STATE is the one just completed
         (case (logand state #b11)
           ;; Test :INHERITED first because the state repeats for a package
           ;; as many times as there are packages it uses. There are enough
           ;; bits to count up to 2^23 packages if fixnums are 30 bits.
           (2
            (when (desired-state-p 2)
              (let* ((tables (package-tables (this-package)))
                     (next-state (the fixnum (+ state (ash 1 6))))
                     (table-idx (ash next-state -6)))
              (when (< table-idx (length tables))
                (return-from advance ; remain in state 2
                  (start next-state (svref tables table-idx))))))
            (pop pkglist)
            (advance 3)) ; start on next package
           (1 ; finished externals, switch to inherited if desired
            (when (desired-state-p 2)
              (let ((tables (package-tables (this-package))))
                (when (plusp (length tables)) ; inherited symbols
                  (return-from advance ; enter state 2
                    (start (if (package-%shadowing-symbols (this-package))
                               (logior 2 +package-iter-check-shadows+) 2)
                           (svref tables 0))))))
            (advance 2)) ; skip state 2
           (0 ; finished internals, switch to externals if desired
            (if (desired-state-p 1) ; enter state 1
                (start 1 (package-external-symbols (this-package)))
                (advance 1))) ; skip state 1
           (t ; initial state
            (cond ((endp pkglist) ; latch into returning NIL forever more
                   (values 0 0 *empty-package-hashtable* nil))
                  ((desired-state-p 0) ; enter state 0
                   (start 0 (package-internal-symbols (this-package))))
                  (t (advance 0)))))) ; skip state 0
       (desired-state-p (target-state)
         (logtest start-state (ash 1 (+ target-state 3))))
       (this-package ()
         (truly-the sb!xc:package (car pkglist)))
       (start (next-state new-table)
         (package-iter-step (logior (mask-field (byte 3 3) start-state)
                                    next-state)
                            (length (package-hashtable-table new-table))
                            new-table pkglist)))
    (declare (inline desired-state-p this-package))
    (if (zerop index)
        (advance start-state)
        (let ((hash-vec (package-hashtable-hash table)))
          ;; We could disable bounds-checking here, but if somebody manages
          ;; to shrink a package-hashtable while iterating, that would be bad.
          (macrolet ((scan (&optional (guard t))
                       `(loop
                         (when (and (>= (aref hash-vec (decf index)) 2) ,guard)
                           (return (values start-state index table pkglist)))
                         (when (zerop index)
                           (return (advance start-state))))))
            (if (logtest start-state +package-iter-check-shadows+)
                (let ((sym-vec (package-hashtable-table table))
                      (shadows (package-%shadowing-symbols (this-package))))
                  (scan (not (member (symbol-name (svref sym-vec index))
                                     shadows :test #'string=))))
                (scan)))))))

(defconstant-eqx +package-iter-access-kind+
    #(:internal :external :inherited)
  #'equalp)

(defmacro-mundanely with-package-iterator ((mname package-list
                                                  &rest symbol-types)
                                           &body body)
  #!+sb-doc
  "Within the lexical scope of the body forms, MNAME is defined via macrolet
such that successive invocations of (MNAME) will return the symbols, one by
one, from the packages in PACKAGE-LIST. SYMBOL-TYPES may be any
of :INHERITED :EXTERNAL :INTERNAL."
  ;; SYMBOL-TYPES should really be named ACCESSIBILITY-TYPES.
  (when (null symbol-types)
    (error 'simple-program-error
           :format-control
           "At least one of :INTERNAL, :EXTERNAL, or :INHERITED must be supplied."))
  (dolist (symbol symbol-types)
    (unless (member symbol '(:internal :external :inherited))
      (error 'simple-program-error
             :format-control
             "~S is not one of :INTERNAL, :EXTERNAL, or :INHERITED."
             :format-arguments (list symbol))))
  (with-unique-names (bits index table pkglist)
    (let ((state (list bits index table pkglist))
          (select (logior (if (member :internal  symbol-types) 1 0)
                          (if (member :external  symbol-types) 2 0)
                          (if (member :inherited symbol-types) 4 0))))
      `(multiple-value-bind ,state (package-iter-init ,select ,package-list)
         (macrolet
           ((,mname ()
              '(if (eql 0 (multiple-value-setq ,state
                            (package-iter-step ,@state)))
                   nil
                   (values
                    t
                    ;; If the iterator state held the symbol vector directly,
                    ;; we could safely elide bounds-checking. The fix mentioned
                    ;; in the comment above WITH-SYMBOL would also help.
                    (svref (package-hashtable-table ,table) ,index)
                    (svref +package-iter-access-kind+
                           (truly-the (mod 3) (logand ,bits #b11)))
                    (car ,pkglist)))))
           ,@body)))))

(defmacro-mundanely with-package-graph ((&key) &body forms)
  `(flet ((thunk () ,@forms))
     (declare (dynamic-extent #'thunk))
     (call-with-package-graph #'thunk)))
