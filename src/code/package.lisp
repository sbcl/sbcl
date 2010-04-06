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
  ;; a list of all the hashtables for inherited symbols. This is
  ;; derived from %USE-LIST, but maintained separately from %USE-LIST
  ;; for some reason. (Perhaps the reason is that when FIND-SYMBOL*
  ;; hits an inherited symbol, it pulls it to the head of the list.)
  ;;
  ;; FIXME: This needs a more-descriptive name
  ;; (USED-PACKAGE-HASH-TABLES?). It also needs an explanation of why
  ;; the last entry is NIL. Perhaps it should even just go away and
  ;; let us do indirection on the fly through %USE-LIST. (If so,
  ;; benchmark to make sure that performance doesn't get stomped..)
  ;; (If benchmark performance is important, this should prob'ly
  ;; become a vector instead of a list.)
  (tables (list nil) :type list)
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
  (source-location nil :type (or null sb!c:definition-source-location)))

;;;; iteration macros

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

(defmacro-mundanely with-package-iterator ((mname package-list
                                                  &rest symbol-types)
                                           &body body)
  #!+sb-doc
  "Within the lexical scope of the body forms, MNAME is defined via macrolet
such that successive invocations of (MNAME) will return the symbols, one by
one, from the packages in PACKAGE-LIST. SYMBOL-TYPES may be any
of :INHERITED :EXTERNAL :INTERNAL."
  (with-unique-names (packages these-packages counter kind hash-vector vector
                      package-use-list init-macro end-test-macro real-symbol-p
                      inherited-symbol-p BLOCK)
    (let ((ordered-types (let ((res nil))
                           (dolist (kind '(:inherited :external :internal) res)
                             (when (member kind symbol-types)
                               (push kind res))))))  ; Order SYMBOL-TYPES.
      `(let* ((,these-packages ,package-list)
              (,packages `,(mapcar (lambda (package)
                                     (if (packagep package)
                                         package
                                         ;; Maybe FIND-PACKAGE-OR-DIE?
                                         (or (find-package package)
                                             (error 'simple-package-error
                                                    ;; could be a character
                                                    :package (string package)
                                                    :format-control "~@<~S does not name a package ~:>"
                                                    :format-arguments (list package)))))
                                   (if (consp ,these-packages)
                                       ,these-packages
                                       (list ,these-packages))))
              (,counter nil)
              (,kind (car ,packages))
              (,hash-vector nil)
              (,vector nil)
              (,package-use-list nil))
        ,(if (member :inherited ordered-types)
             `(setf ,package-use-list (package-%use-list (car ,packages)))
             `(declare (ignore ,package-use-list)))
        (macrolet ((,init-macro (next-kind)
                     (declare (optimize (inhibit-warnings 3)))
                     (let ((symbols (gensym)))
                       `(progn
                         (setf ,',kind ,next-kind)
                         (setf ,',counter nil)
                         ,(case next-kind
                                (:internal
                                 `(let ((,symbols (package-internal-symbols
                                                   (car ,',packages))))
                                   (when ,symbols
                                     (setf ,',vector (package-hashtable-table ,symbols))
                                     (setf ,',hash-vector
                                           (package-hashtable-hash ,symbols)))))
                                (:external
                                 `(let ((,symbols (package-external-symbols
                                                   (car ,',packages))))
                                   (when ,symbols
                                     (setf ,',vector (package-hashtable-table ,symbols))
                                     (setf ,',hash-vector
                                           (package-hashtable-hash ,symbols)))))
                                (:inherited
                                 `(let ((,symbols (and ,',package-use-list
                                                       (package-external-symbols
                                                        (car ,',package-use-list)))))
                                   (when ,symbols
                                     (setf ,',vector (package-hashtable-table ,symbols))
                                     (setf ,',hash-vector
                                           (package-hashtable-hash ,symbols)))))))))
                   (,end-test-macro (this-kind)
                     `,(let ((next-kind (cadr (member this-kind
                                                      ',ordered-types))))
                            (if next-kind
                                `(,',init-macro ,next-kind)
                                `(if (endp (setf ,',packages (cdr ,',packages)))
                                  (return-from ,',BLOCK)
                                  (,',init-macro ,(car ',ordered-types)))))))
          (when ,packages
            ,(when (null symbol-types)
                   (error 'simple-program-error
                          :format-control
                          "At least one of :INTERNAL, :EXTERNAL, or ~
                      :INHERITED must be supplied."))
            ,(dolist (symbol symbol-types)
                     (unless (member symbol '(:internal :external :inherited))
                       (error 'simple-program-error
                              :format-control
                              "~S is not one of :INTERNAL, :EXTERNAL, or :INHERITED."
                              :format-arguments (list symbol))))
            (,init-macro ,(car ordered-types))
            (flet ((,real-symbol-p (number)
                     (> number 1)))
              (macrolet ((,mname ()
                           (declare (optimize (inhibit-warnings 3)))
                           `(block ,',BLOCK
                             (loop
                              (case ,',kind
                                ,@(when (member :internal ',ordered-types)
                                        `((:internal
                                           (setf ,',counter
                                            (position-if #',',real-symbol-p
                                                         (the hash-vector ,',hash-vector)
                                                         :start (if ,',counter
                                                                    (1+ ,',counter)
                                                                    0)))
                                           (if ,',counter
                                               (return-from ,',BLOCK
                                                 (values t (svref ,',vector ,',counter)
                                                         ,',kind (car ,',packages)))
                                               (,',end-test-macro :internal)))))
                                ,@(when (member :external ',ordered-types)
                                        `((:external
                                           (setf ,',counter
                                            (position-if #',',real-symbol-p
                                                         (the hash-vector ,',hash-vector)
                                                         :start (if ,',counter
                                                                    (1+ ,',counter)
                                                                    0)))
                                           (if ,',counter
                                               (return-from ,',BLOCK
                                                 (values t (svref ,',vector ,',counter)
                                                         ,',kind (car ,',packages)))
                                               (,',end-test-macro :external)))))
                                ,@(when (member :inherited ',ordered-types)
                                        `((:inherited
                                           (flet ((,',inherited-symbol-p (number)
                                                    (when (,',real-symbol-p number)
                                                      (let* ((p (position
                                                                 number
                                                                 (the hash-vector
                                                                   ,',hash-vector)
                                                                 :start (if ,',counter
                                                                            (1+ ,',counter)
                                                                            0)))
                                                             (s (svref ,',vector p)))
                                                        (eql (nth-value
                                                              1 (find-symbol
                                                                 (symbol-name s)
                                                                 (car ,',packages)))
                                                             :inherited)))))
                                             (setf ,',counter
                                                   (when ,',hash-vector
                                                     (position-if #',',inherited-symbol-p
                                                                  (the hash-vector
                                                                    ,',hash-vector)
                                                                  :start (if ,',counter
                                                                             (1+ ,',counter)
                                                                             0)))))
                                           (cond (,',counter
                                                  (return-from
                                                   ,',BLOCK
                                                    (values t (svref ,',vector ,',counter)
                                                            ,',kind (car ,',packages))
                                                    ))
                                                 (t
                                                  (setf ,',package-use-list
                                                        (cdr ,',package-use-list))
                                                  (cond ((endp ,',package-use-list)
                                                         (setf ,',packages (cdr ,',packages))
                                                         (when (endp ,',packages)
                                                           (return-from ,',BLOCK))
                                                         (setf ,',package-use-list
                                                               (package-%use-list
                                                                (car ,',packages)))
                                                         (,',init-macro ,(car
                                                                          ',ordered-types)))
                                                        (t (,',init-macro :inherited)
                                                           (setf ,',counter nil)))))))))))))
                ,@body))))))))

(defmacro-mundanely with-package-graph ((&key) &body forms)
  `(flet ((thunk () ,@forms))
     (declare (dynamic-extent #'thunk))
     (call-with-package-graph #'thunk)))
