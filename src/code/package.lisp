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

;; FIXME: too vaguely named. Should be PACKAGE-HASH-VECTOR.
(def!type hash-vector () '(simple-array (unsigned-byte 8) (*)))

(def!struct (package-hashtable
             (:constructor %make-package-hashtable
                           (table size &aux (free size)))
             (:copier nil))
  ;; The general-vector of symbols, with a hash-vector in its last cell.
  (table (missing-arg) :type simple-vector)
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

;;; Meta: IN-PACKAGE references a string constant, not a package.
;;; But we need this to be a DEF!STRUCT to emit 'package.h'
;;; during first genesis.
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
  ;; Todo: dynamically changeover to a PACKAGE-HASHTABLE if list gets long
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

(flet ((expand-iterator (range var body result-form)
         (multiple-value-bind (forms decls)
             (parse-body body :doc-string-allowed nil)
           (with-unique-names (iterator winp next)
             `(block nil
                (with-package-iterator (,iterator ,@range)
                 (tagbody
                  ,next
                  (multiple-value-bind (,winp ,var) (,iterator)
                    ;; I don't think this VAR is automatically ignorable.
                    ;; The user should use it, or declare IGNORE/IGNORABLE.
                    ,@decls
                    (if ,winp
                        (tagbody ,@forms (go ,next))
                        (return ,result-form))))))))))

(defmacro-mundanely do-symbols ((var &optional
                                     (package '*package*)
                                     result-form)
                                &body body-decls)
  #!+sb-doc
  "DO-SYMBOLS (VAR [PACKAGE [RESULT-FORM]]) {DECLARATION}* {TAG | FORM}*
   Executes the FORMs at least once for each symbol accessible in the given
   PACKAGE with VAR bound to the current symbol."
  (expand-iterator `((find-undeleted-package-or-lose ,package)
                     :internal :external :inherited)
                   var body-decls result-form))

(defmacro-mundanely do-external-symbols ((var &optional
                                              (package '*package*)
                                              result-form)
                                         &body body-decls)
  #!+sb-doc
  "DO-EXTERNAL-SYMBOLS (VAR [PACKAGE [RESULT-FORM]]) {DECL}* {TAG | FORM}*
   Executes the FORMs once for each external symbol in the given PACKAGE with
   VAR bound to the current symbol."
  (expand-iterator `((find-undeleted-package-or-lose ,package) :external)
                   var body-decls result-form))

(defmacro-mundanely do-all-symbols ((var &optional
                                         result-form)
                                    &body body-decls)
  #!+sb-doc
  "DO-ALL-SYMBOLS (VAR [RESULT-FORM]) {DECLARATION}* {TAG | FORM}*
   Executes the FORMs once for each symbol in every package with VAR bound
   to the current symbol."
  (expand-iterator '((list-all-packages) :internal :external)
                   var body-decls result-form)))


;;;; WITH-PACKAGE-ITERATOR

(defun package-iter-init (access-types pkg-designator-list)
  (declare (type (integer 1 7) access-types)) ; a nonzero bitmask over types
  (values (logior (ash access-types 3) #b11) 0 #()
          (package-listify pkg-designator-list)))

;; The STATE parameter is comprised of 4 packed fields
;;  [0:1] = substate {0=internal,1=external,2=inherited,3=initial}
;;  [2]   = package with inherited symbols has shadowing symbols
;;  [3:5] = enabling bits for {internal,external,inherited}
;;  [6:]  = index into 'package-tables'
;;
(defconstant +package-iter-check-shadows+  #b000100)

(defun package-iter-step (start-state index sym-vec pkglist)
  ;; the defknown isn't enough
  (declare (type fixnum start-state) (type index index)
           (type simple-vector sym-vec) (type list pkglist))
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
                   (values 0 0 #() '() nil nil))
                  ((desired-state-p 0) ; enter state 0
                   (start 0 (package-internal-symbols (this-package))))
                  (t (advance 0)))))) ; skip state 0
       (desired-state-p (target-state)
         (logtest start-state (ash 1 (+ target-state 3))))
       (this-package ()
         (truly-the sb!xc:package (car pkglist)))
       (start (next-state new-table)
         (let ((symbols (package-hashtable-table new-table)))
           (package-iter-step (logior (mask-field (byte 3 3) start-state)
                                      next-state)
                              ;; assert that physical length was nonzero
                              (the index (1- (length symbols)))
                              symbols pkglist))))
    (declare (inline desired-state-p this-package))
    (if (zerop index)
        (advance start-state)
        (macrolet ((scan (&optional (guard t))
                   `(loop
                     (let ((sym (aref sym-vec (decf index))))
                       (when (and (not (eql sym 0)) ,guard)
                         (return (values start-state index sym-vec pkglist sym
                                         (aref #(:internal :external :inherited)
                                               (logand start-state 3))))))
                     (when (zerop index)
                       (return (advance start-state))))))
          (declare #-sb-xc-host(optimize (sb!c::insert-array-bounds-checks 0)))
          (if (logtest start-state +package-iter-check-shadows+)
              (let ((shadows (package-%shadowing-symbols (this-package))))
                (scan (not (member sym shadows :test #'string=))))
              (scan))))))

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
  (with-unique-names (bits index sym-vec pkglist symbol kind)
    (let ((state (list bits index sym-vec pkglist))
          (select (logior (if (member :internal  symbol-types) 1 0)
                          (if (member :external  symbol-types) 2 0)
                          (if (member :inherited symbol-types) 4 0))))
      `(multiple-value-bind ,state (package-iter-init ,select ,package-list)
         (let (,symbol ,kind)
           (macrolet
               ((,mname ()
                   '(if (eql 0 (multiple-value-setq (,@state ,symbol ,kind)
                                 (package-iter-step ,@state)))
                        nil
                        (values t ,symbol ,kind
                                (car (truly-the list ,pkglist))))))
             ,@body))))))

(defmacro-mundanely with-package-graph ((&key) &body forms)
  `(flet ((thunk () ,@forms))
     (declare (dynamic-extent #'thunk))
     (call-with-package-graph #'thunk)))
