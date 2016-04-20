;;;; This file contains structures and functions for the maintenance of
;;;; basic information about defined types. Different object systems
;;;; can be supported simultaneously.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

(!begin-collecting-cold-init-forms)

;;;; the CLASSOID structure

;;; The CLASSOID structure is a supertype of all classoid types.
;;; Its definition occurs in 'early-classoid.lisp'

(defun classoid-make-load-form-fun (self)
  (let ((name (classoid-name self)))
    (if (and name (eq (find-classoid name nil) self))
        `(find-classoid ',name)
        (error "can't use anonymous or undefined class as constant:~%  ~S"
               self))))


;;;; basic LAYOUT stuff

;;; a list of conses, initialized by genesis
;;;
;;; In each cons, the car is the symbol naming the layout, and the
;;; cdr is the layout itself.
(defvar *!initial-layouts*)

;;; a table mapping class names to layouts for classes we have
;;; referenced but not yet loaded. This is initialized from an alist
;;; created by genesis describing the layouts that genesis created at
;;; cold-load time.
(defvar *forward-referenced-layouts*)
(!cold-init-forms
  ;; Protected by *WORLD-LOCK*
  (setq *forward-referenced-layouts* (make-hash-table :test 'equal))
  #-sb-xc-host (progn
                 (/show0 "processing *!INITIAL-LAYOUTS*")
                 (dolist (x *!initial-layouts*)
                   (setf (layout-clos-hash (cdr x)) (random-layout-clos-hash))
                   (setf (gethash (car x) *forward-referenced-layouts*)
                         (cdr x)))
                 (/show0 "done processing *!INITIAL-LAYOUTS*")))

;;; The LAYOUT structure itself is defined in 'early-classoid.lisp'

(declaim (inline layout-for-std-class-p))
(defun layout-for-std-class-p (x) (not (zerop (layout-%for-std-class-b x))))

(defmethod print-object ((layout layout) stream)
  (print-unreadable-object (layout stream :type t :identity t)
    (format stream
            "for ~S~@[, INVALID=~S~]"
            (layout-proper-name layout)
            (layout-invalid layout))))

(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  (defun layout-proper-name (layout)
    (classoid-proper-name (layout-classoid layout))))

;;;; support for the hash values used by CLOS when working with LAYOUTs

;;; a generator for random values suitable for the CLOS-HASH slots of
;;; LAYOUTs. We use our own RANDOM-STATE here because we'd like
;;; pseudo-random values to come the same way in the target even when
;;; we make minor changes to the system, in order to reduce the
;;; mysteriousness of possible CLOS bugs.
(defvar *layout-clos-hash-random-state*)
(defun random-layout-clos-hash ()
  ;; FIXME: I'm not sure why this expression is (1+ (RANDOM FOO)),
  ;; returning a strictly positive value. I copied it verbatim from
  ;; CMU CL INITIALIZE-LAYOUT-HASH, so presumably it works, but I
  ;; dunno whether the hash values are really supposed to be 1-based.
  ;; They're declared as INDEX.. Or is this a hack to try to avoid
  ;; having to use bignum arithmetic? Or what? An explanation would be
  ;; nice.
  ;;
  ;; an explanation is provided in Kiczales and Rodriguez, "Efficient
  ;; Method Dispatch in PCL", 1990.  -- CSR, 2005-11-30
  (1+ (random (1- layout-clos-hash-limit)
              (if (boundp '*layout-clos-hash-random-state*)
                  *layout-clos-hash-random-state*
                  (setf *layout-clos-hash-random-state*
                        (make-random-state))))))

;;; If we can't find any existing layout, then we create a new one
;;; storing it in *FORWARD-REFERENCED-LAYOUTS*. In classic CMU CL, we
;;; used to immediately check for compatibility, but for
;;; cross-compilability reasons (i.e. convenience of using this
;;; function in a MAKE-LOAD-FORM expression) that functionality has
;;; been split off into INIT-OR-CHECK-LAYOUT.
(declaim (ftype (sfunction (symbol) layout) find-layout))
;; The comment "This seems ..." is misleading but I don't have a better one.
;; FIND-LAYOUT is used by FIND-AND-INIT-OR-CHECK-LAYOUT which is used
;; by FOP-LAYOUT, so clearly it's used when reading fasl files.
(defun find-layout (name)
  ;; This seems to be currently used only from the compiler, but make
  ;; it thread-safe all the same. We need to lock *F-R-L* before doing
  ;; FIND-CLASSOID in case (SETF FIND-CLASSOID) happens in parallel.
  (let ((table *forward-referenced-layouts*))
    (with-world-lock ()
      (let ((classoid (find-classoid name nil)))
        (or (and classoid (classoid-layout classoid))
            (gethash name table)
            (setf (gethash name table)
                  (make-layout :classoid (or classoid (make-undefined-classoid name)))))))))

;;; If LAYOUT is uninitialized, initialize it with CLASSOID, LENGTH,
;;; INHERITS, DEPTHOID, and BITMAP.
;;; Otherwise require that it be consistent with the existing values.
;;;
;;; UNDEFINED-CLASS values are interpreted specially as "we don't know
;;; anything about the class", so if LAYOUT is initialized, any
;;; preexisting class slot value is OK, and if it's not initialized,
;;; its class slot value is set to an UNDEFINED-CLASS. -- FIXME: This
;;; is no longer true, :UNINITIALIZED used instead.
(declaim (ftype (function (layout classoid index simple-vector layout-depthoid
                                  unsigned-byte)
                          layout)
                %init-or-check-layout))
(defun %init-or-check-layout (layout classoid length inherits depthoid bitmap)
  (cond ((eq (layout-invalid layout) :uninitialized)
         ;; There was no layout before, we just created one which
         ;; we'll now initialize with our information.
         (setf (layout-length layout) length
               (layout-inherits layout) inherits
               (layout-depthoid layout) depthoid
               (layout-bitmap layout) bitmap
               (layout-classoid layout) classoid
               (layout-invalid layout) nil))
        ;; FIXME: Now that LAYOUTs are born :UNINITIALIZED, maybe this
        ;; clause is not needed?
        ((not *type-system-initialized*)
         (setf (layout-classoid layout) classoid))
        (t
         ;; There was an old layout already initialized with old
         ;; information, and we'll now check that old information
         ;; which was known with certainty is consistent with current
         ;; information which is known with certainty.
         (check-layout layout classoid length inherits depthoid bitmap)))
  layout)

;;; In code for the target Lisp, we don't dump LAYOUTs using the
;;; standard load form mechanism, we use special fops instead, in
;;; order to make cold load come out right. But when we're building
;;; the cross-compiler, we can't do that because we don't have access
;;; to special non-ANSI low-level things like special fops, and we
;;; don't need to do that anyway because our code isn't going to be
;;; cold loaded, so we use the ordinary load form system.
;;;
;;; KLUDGE: A special hack causes this not to be called when we are
;;; building code for the target Lisp. It would be tidier to just not
;;; have it in place when we're building the target Lisp, but it
;;; wasn't clear how to do that without rethinking DEF!STRUCT quite a
;;; bit, so I punted. -- WHN 19990914
#+sb-xc-host
(defun make-load-form-for-layout (layout &optional env)
  (declare (type layout layout))
  (declare (ignore env))
  (when (layout-invalid layout)
    (sb!c::compiler-error "can't dump reference to obsolete class: ~S"
                          (layout-classoid layout)))
  (let ((name (classoid-name (layout-classoid layout))))
    (unless name
      (sb!c::compiler-error "can't dump anonymous LAYOUT: ~S" layout))
    ;; Since LAYOUT refers to a class which refers back to the LAYOUT,
    ;; we have to do this in two stages, like the TREE-WITH-PARENT
    ;; example in the MAKE-LOAD-FORM entry in the ANSI spec.
    (values
     ;; "creation" form (which actually doesn't create a new LAYOUT if
     ;; there's a preexisting one with this name)
     `(find-layout ',name)
     ;; "initialization" form (which actually doesn't initialize
     ;; preexisting LAYOUTs, just checks that they're consistent).
     `(%init-or-check-layout ',layout
                             ',(layout-classoid layout)
                             ',(layout-length layout)
                             ',(layout-inherits layout)
                             ',(layout-depthoid layout)
                             ',(layout-bitmap layout)))))

;;; If LAYOUT's slot values differ from the specified slot values in
;;; any interesting way, then give a warning and return T.
(declaim (ftype (function (simple-string
                           layout
                           simple-string
                           index
                           simple-vector
                           layout-depthoid
                           unsigned-byte))
                redefine-layout-warning))
(defun redefine-layout-warning (old-context old-layout
                                context length inherits depthoid bitmap)
  (declare (type layout old-layout) (type simple-string old-context context))
  (let ((name (layout-proper-name old-layout))
        (old-inherits (layout-inherits old-layout)))
    (or (when (mismatch old-inherits inherits :key #'layout-proper-name)
          (warn "change in superclasses of class ~S:~%  ~
                       ~A superclasses: ~S~%  ~
                       ~A superclasses: ~S"
                      name
                      old-context
                      (map 'list #'layout-proper-name old-inherits)
                      context
                      (map 'list #'layout-proper-name inherits))
          t)
        (let ((diff (mismatch old-inherits inherits)))
          (when diff
            (warn "in class ~S:~%  ~
                    ~@(~A~) definition of superclass ~S is incompatible with~%  ~
                    ~A definition."
                   name
                   old-context
                   (layout-proper-name (svref old-inherits diff))
                   context)
            t))
        (let ((old-length (layout-length old-layout)))
          (unless (= old-length length)
            (warn "change in instance length of class ~S:~%  ~
                   ~A length: ~W~%  ~
                   ~A length: ~W"
                  name
                  old-context old-length
                  context length)
            t))
        (let ((old-bitmap (layout-bitmap old-layout)))
          (unless (= old-bitmap bitmap)
            (warn "change in placement of raw slots of class ~S ~
between the ~A definition and the ~A definition"
                  name old-context context)
            t))
        (unless (= (layout-depthoid old-layout) depthoid)
          (warn "change in the inheritance structure of class ~S~%  ~
                 between the ~A definition and the ~A definition"
                name old-context context)
          t))))

;;; Require that LAYOUT data be consistent with CLASSOID, LENGTH,
;;; INHERITS, DEPTHOID, and BITMAP.
(declaim (ftype (function (layout classoid index simple-vector layout-depthoid
                           unsigned-byte)) check-layout))
(defun check-layout (layout classoid length inherits depthoid bitmap)
  (aver (eq (layout-classoid layout) classoid))
  (when (redefine-layout-warning "current" layout
                                 "compile time" length inherits depthoid bitmap)
    ;; Classic CMU CL had more options here. There are several reasons
    ;; why they might want more options which are less appropriate for
    ;; us: (1) It's hard to fit the classic CMU CL flexible approach
    ;; into the ANSI-style MAKE-LOAD-FORM system, and having a
    ;; non-MAKE-LOAD-FORM-style system is painful when we're trying to
    ;; make the cross-compiler run under vanilla ANSI Common Lisp. (2)
    ;; We have CLOS now, and if you want to be able to flexibly
    ;; redefine classes without restarting the system, it'd make sense
    ;; to use that, so supporting complexity in order to allow
    ;; modifying DEFSTRUCTs without restarting the system is a low
    ;; priority. (3) We now have the ability to rebuild the SBCL
    ;; system from scratch, so we no longer need this functionality in
    ;; order to maintain the SBCL system by modifying running images.
    (error "The loaded code expects an incompatible layout for class ~S."
           (layout-proper-name layout)))
  (values))

;;; a common idiom (the same as CMU CL FIND-LAYOUT) rolled up into a
;;; single function call
;;;
;;; Used by the loader to forward-reference layouts for classes whose
;;; definitions may not have been loaded yet. This allows type tests
;;; to be loaded when the type definition hasn't been loaded yet.
(declaim (ftype (function (symbol index simple-vector layout-depthoid
                                  unsigned-byte) layout)
                find-and-init-or-check-layout))
(defun find-and-init-or-check-layout (name length inherits depthoid bitmap)
  (truly-the ; avoid an "assertion too complex to check" optimizer note
   (values layout &optional)
   (with-world-lock ()
    (let ((layout (find-layout name)))
      (%init-or-check-layout layout
                             (or (find-classoid name nil)
                                 (layout-classoid layout))
                             length
                             inherits
                             depthoid
                             bitmap)))))

;;; Record LAYOUT as the layout for its class, adding it as a subtype
;;; of all superclasses. This is the operation that "installs" a
;;; layout for a class in the type system, clobbering any old layout.
;;; However, this does not modify the class namespace; that is a
;;; separate operation (think anonymous classes.)
;;; -- If INVALIDATE, then all the layouts for any old definition
;;;    and subclasses are invalidated, and the SUBCLASSES slot is cleared.
;;; -- If DESTRUCT-LAYOUT, then this is some old layout, and is to be
;;;    destructively modified to hold the same type information.
(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
(defun register-layout (layout &key (invalidate t) destruct-layout)
  (declare (type layout layout) (type (or layout null) destruct-layout))
  (with-world-lock ()
    (let* ((classoid (layout-classoid layout))
           (classoid-layout (classoid-layout classoid))
           (subclasses (classoid-subclasses classoid)))

      ;; Attempting to register ourselves with a temporary undefined
      ;; class placeholder is almost certainly a programmer error. (I
      ;; should know, I did it.) -- WHN 19990927
      (aver (not (undefined-classoid-p classoid)))

      ;; This assertion dates from classic CMU CL. The rationale is
      ;; probably that calling REGISTER-LAYOUT more than once for the
      ;; same LAYOUT is almost certainly a programmer error.
      (aver (not (eq classoid-layout layout)))

      ;; Figure out what classes are affected by the change, and issue
      ;; appropriate warnings and invalidations.
      (when classoid-layout
        (%modify-classoid classoid)
        (when subclasses
          (dohash ((subclass subclass-layout) subclasses :locked t)
            (%modify-classoid subclass)
            (when invalidate
              (%invalidate-layout subclass-layout))))
        (when invalidate
          (%invalidate-layout classoid-layout)
          (setf (classoid-subclasses classoid) nil)))

      (if destruct-layout
          (setf (layout-invalid destruct-layout) nil
                (layout-inherits destruct-layout) (layout-inherits layout)
                (layout-depthoid destruct-layout) (layout-depthoid layout)
                (layout-length destruct-layout) (layout-length layout)
                (layout-bitmap destruct-layout) (layout-bitmap layout)
                (layout-info destruct-layout) (layout-info layout)
                (classoid-layout classoid) destruct-layout)
          (setf (layout-invalid layout) nil
                (classoid-layout classoid) layout))

      (dovector (super-layout (layout-inherits layout))
        (let* ((super (layout-classoid super-layout))
               (subclasses (or (classoid-subclasses super)
                               (setf (classoid-subclasses super)
                                     (make-hash-table :test 'eq
                                                      #-sb-xc-host #-sb-xc-host
                                                      :synchronized t)))))
          (when (and (eq (classoid-state super) :sealed)
                     (not (gethash classoid subclasses)))
            (warn "unsealing sealed class ~S in order to subclass it"
                  (classoid-name super))
            (setf (classoid-state super) :read-only))
          (setf (gethash classoid subclasses)
                (or destruct-layout layout))))))

  (values))
); EVAL-WHEN

;;; Arrange the inherited layouts to appear at their expected depth,
;;; ensuring that hierarchical type tests succeed. Layouts with
;;; DEPTHOID >= 0 (i.e. hierarchical classes) are placed first,
;;; at exactly that index in the INHERITS vector. Then, non-hierarchical
;;; layouts are placed in remaining elements. Then, any still-empty
;;; elements are filled with their successors, ensuring that each
;;; element contains a valid layout.
;;;
;;; This reordering may destroy CPL ordering, so the inherits should
;;; not be read as being in CPL order.
(defun order-layout-inherits (layouts)
  (declare (simple-vector layouts))
  (let ((length (length layouts))
        (max-depth -1))
    (dotimes (i length)
      (let ((depth (layout-depthoid (svref layouts i))))
        (when (> depth max-depth)
          (setf max-depth depth))))
    (let* ((new-length (max (1+ max-depth) length))
           ;; KLUDGE: 0 here is the "uninitialized" element.  We need
           ;; to specify it explicitly for portability purposes, as
           ;; elements can be read before being set [ see below, "(EQL
           ;; OLD-LAYOUT 0)" ].  -- CSR, 2002-04-20
           (inherits (make-array new-length :initial-element 0)))
      (dotimes (i length)
        (let* ((layout (svref layouts i))
               (depth (layout-depthoid layout)))
          (unless (eql depth -1)
            (let ((old-layout (svref inherits depth)))
              (unless (or (eql old-layout 0) (eq old-layout layout))
                (error "layout depth conflict: ~S~%" layouts)))
            (setf (svref inherits depth) layout))))
      (do ((i 0 (1+ i))
           (j 0))
          ((>= i length))
        (declare (type index i j))
        (let* ((layout (svref layouts i))
               (depth (layout-depthoid layout)))
          (when (eql depth -1)
            (loop (when (eql (svref inherits j) 0)
                    (return))
                  (incf j))
            (setf (svref inherits j) layout))))
      (do ((i (1- new-length) (1- i)))
          ((< i 0))
        (declare (type fixnum i))
        (when (eql (svref inherits i) 0)
          (setf (svref inherits i) (svref inherits (1+ i)))))
      inherits)))

;;;; class precedence lists

;;; Topologically sort the list of objects to meet a set of ordering
;;; constraints given by pairs (A . B) constraining A to precede B.
;;; When there are multiple objects to choose, the tie-breaker
;;; function is called with both the list of object to choose from and
;;; the reverse ordering built so far.
(defun topological-sort (objects constraints tie-breaker)
  (declare (list objects constraints)
           (function tie-breaker))
  (let ((obj-info (make-hash-table :size (length objects)))
        (free-objs nil)
        (result nil))
    (dolist (constraint constraints)
      (let ((obj1 (car constraint))
            (obj2 (cdr constraint)))
        (let ((info2 (gethash obj2 obj-info)))
          (if info2
              (incf (first info2))
              (setf (gethash obj2 obj-info) (list 1))))
        (let ((info1 (gethash obj1 obj-info)))
          (if info1
              (push obj2 (rest info1))
              (setf (gethash obj1 obj-info) (list 0 obj2))))))
    (dolist (obj objects)
      (let ((info (gethash obj obj-info)))
        (when (or (not info) (zerop (first info)))
          (push obj free-objs))))
    (loop
     (flet ((next-result (obj)
              (push obj result)
              (dolist (successor (rest (gethash obj obj-info)))
                (let* ((successor-info (gethash successor obj-info))
                       (count (1- (first successor-info))))
                  (setf (first successor-info) count)
                  (when (zerop count)
                    (push successor free-objs))))))
       (cond ((endp free-objs)
              (dohash ((obj info) obj-info)
                (unless (zerop (first info))
                  (error "Topological sort failed due to constraint on ~S."
                         obj)))
              (return (nreverse result)))
             ((endp (rest free-objs))
              (next-result (pop free-objs)))
             (t
              (let ((obj (funcall tie-breaker free-objs result)))
                (setf free-objs (remove obj free-objs))
                (next-result obj))))))))


;;; standard class precedence list computation
(defun std-compute-class-precedence-list (class)
  (let ((classes nil)
        (constraints nil))
    (labels ((note-class (class)
               (unless (member class classes)
                 (push class classes)
                 (let ((superclasses (classoid-direct-superclasses class)))
                   (do ((prev class)
                        (rest superclasses (rest rest)))
                       ((endp rest))
                     (let ((next (first rest)))
                       (push (cons prev next) constraints)
                       (setf prev next)))
                   (dolist (class superclasses)
                     (note-class class)))))
             (std-cpl-tie-breaker (free-classes rev-cpl)
               (dolist (class rev-cpl (first free-classes))
                 (let* ((superclasses (classoid-direct-superclasses class))
                        (intersection (intersection free-classes
                                                    superclasses)))
                   (when intersection
                     (return (first intersection)))))))
      (note-class class)
      (topological-sort classes constraints #'std-cpl-tie-breaker))))

;;;; object types to represent classes

;;; BUILT-IN-CLASS is used to represent the standard classes that
;;; aren't defined with DEFSTRUCT and other specially implemented
;;; primitive types whose only attribute is their name.
;;; It is defined in 'early-classoid.lisp'

;;; STRUCTURE-CLASS represents what we need to know about structure
;;; classes. Non-structure "typed" defstructs are a special case, and
;;; don't have a corresponding class.
(def!struct (structure-classoid (:include classoid)
                                (:constructor %make-structure-classoid)))
(defun make-structure-classoid (&key name)
  (mark-ctype-interned (%make-structure-classoid :name name)))

;;;; classoid namespace

(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  (defun (setf find-classoid) (new-value name)
    #-sb-xc (declare (type (or null classoid) new-value))
    (aver new-value)
    (let ((table *forward-referenced-layouts*))
      (with-world-lock ()
        (let ((cell (find-classoid-cell name :create t)))
          (ecase (info :type :kind name)
            ((nil))
            (:forthcoming-defclass-type
             ;; FIXME: Currently, nothing needs to be done in this case.
             ;; Later, when PCL is integrated tighter into SBCL, this
             ;; might need more work.
             nil)
            (:instance
             (aver cell)
             (let ((old-value (classoid-cell-classoid cell)))
               (aver old-value)
               ;; KLUDGE: The reason these clauses aren't directly
               ;; parallel is that we need to use the internal
               ;; CLASSOID structure ourselves, because we don't
               ;; have CLASSes to work with until PCL is built. In
               ;; the host, CLASSes have an approximately
               ;; one-to-one correspondence with the target
               ;; CLASSOIDs (as well as with the target CLASSes,
               ;; modulo potential differences with respect to
               ;; conditions).
               #+sb-xc-host
               (let ((old (class-of old-value))
                     (new (class-of new-value)))
                 (unless (eq old new)
                   (bug "Trying to change the metaclass of ~S from ~S to ~S in the ~
                            cross-compiler."
                        name (class-name old) (class-name new))))
               #-sb-xc-host
               (let ((old (classoid-of old-value))
                     (new (classoid-of new-value)))
                 (unless (eq old new)
                   (warn "Changing meta-class of ~S from ~S to ~S."
                         name (classoid-name old) (classoid-name new))))))
            (:primitive
             (error "Cannot redefine standard type ~S." name))
            (:defined
             (warn "redefining DEFTYPE type to be a class: ~
                    ~/sb-impl::print-symbol-with-prefix/" name)
             (clear-info :type :expander name)
             (clear-info :type :source-location name)))

          (remhash name table)
          (%note-type-defined name)
          ;; FIXME: I'm unconvinced of the need to handle either of these.
          ;; Package locks preclude the latter, and in the former case,
          ;; once you've made some random thing into a :PRIMITIVE kind of type,
          ;; you've painted yourself into a corner - those types
          ;; elicit vociferous complaints if you try to redefine them.
          ;;
          ;; we need to handle things like
          ;;   (setf (find-class 'foo) (find-class 'integer))
          ;; and
          ;;   (setf (find-class 'integer) (find-class 'integer))
          (cond ((built-in-classoid-p new-value)
                 ;; But I can't figure out how to get assertions to pass
                 ;; without violation what would otherwise be invariants
                 ;; of the internal representation of types. This sucks.
                 (setf (info :type :kind name)
                       (or (info :type :kind name) :defined)))
                (t
                 (setf (info :type :kind name) :instance)))
          (setf (classoid-cell-classoid cell) new-value)
          (unless (eq (info :type :compiler-layout name)
                      (classoid-layout new-value))
            (setf (info :type :compiler-layout name)
                  (classoid-layout new-value))))))
    new-value)

  (defun %clear-classoid (name cell)
    (ecase (info :type :kind name)
      ((nil))
      (:defined)
      (:primitive
       (error "Attempt to remove :PRIMITIVE type: ~S" name))
      ((:forthcoming-defclass-type :instance)
       (when cell
         ;; Note: We cannot remove the classoid cell from the table,
         ;; since compiled code may refer directly to the cell, and
         ;; getting a different cell for a classoid with the same name
         ;; just would not do.

         ;; Remove the proper name of the classoid, if this was it.
         (let* ((classoid (classoid-cell-classoid cell))
                (proper-name (classoid-name classoid)))
           (when (eq proper-name name)
             (setf (classoid-name classoid) nil)))

         ;; Clear the cell.
         (setf (classoid-cell-classoid cell) nil
               (classoid-cell-pcl-class cell) nil))
       (clear-info :type :kind name)
       (clear-info :type :documentation name)
       (clear-info :type :compiler-layout name)))))

;;; Called when we are about to define NAME as a class meeting some
;;; predicate (such as a meta-class type test.) The first result is
;;; always of the desired class. The second result is any existing
;;; LAYOUT for this name.
;;;
;;; Again, this should be compiler-only, but easier to make this
;;; thread-safe.
(defun insured-find-classoid (name predicate constructor)
  (declare (type function predicate constructor))
  (let ((table *forward-referenced-layouts*))
    (with-locked-system-table (table)
      (let* ((old (find-classoid name nil))
             (res (if (and old (funcall predicate old))
                      old
                      (funcall constructor :name name)))
             (found (or (gethash name table)
                        (when old (classoid-layout old)))))
        (when found
          (setf (layout-classoid found) res))
        (values res found)))))

;;; If the classoid has a proper name, return the name, otherwise return
;;; the classoid.
(defun classoid-proper-name (classoid)
  #-sb-xc (declare (type classoid classoid))
  (let ((name (classoid-name classoid)))
    (if (and name (eq (find-classoid name nil) classoid))
        name
        classoid)))

;;;; CLASS type operations

;; CLASSOID-ENUMERABLE-P is referenced during compile by !DEFINE-TYPE-CLASS.
;; But don't redefine it when building the target since we've already
;; got a perfectly good definition loaded for the host.
(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  ;; Actually this definition makes very little sense because
  ;;     (TYPE-ENUMERABLE (FIND-CLASSOID 'CHARACTER)) => T
  ;; but (TYPE-ENUMERABLE (SPECIFIER-TYPE 'CHARACTER)) => NIL.
  ;; You should never see the CLASSOID used as a type though,
  ;; at least not from parsing and set operations.
  ;; On a related note, (TYPE-ENUMERABLE (FIND-CLASSOID 'NULL))
  ;; should probably be T, but you'll never see that type either.
  ;; Perhaps a better definition of this function would be
  ;;   (if (classoid-translation x) (bug "enumerable-p classoid?") nil)
  (defun classoid-enumerable-p (x) (eq (classoid-name x) 'character)))
(!define-type-class classoid :enumerable #'classoid-enumerable-p
                    :might-contain-other-types nil)

;;; We might be passed classoids with invalid layouts; in any pairwise
;;; class comparison, we must ensure that both are valid before
;;; proceeding.
(defun %ensure-classoid-valid (classoid layout error-context)
  (declare (ignorable error-context)) ; not used on host
  (aver (eq classoid (layout-classoid layout)))
  (or (not (layout-invalid layout))
      ;; Avoid accidentally reaching code that can't work.
      #+sb-xc-host (bug "(TYPEP x 'STANDARD-CLASSOID) can't be tested")
      #-sb-xc-host
      (if (typep classoid 'standard-classoid)
          (let ((class (classoid-pcl-class classoid)))
            (cond
              ((sb!pcl:class-finalized-p class)
               (sb!pcl::%force-cache-flushes class)
               t)
              ((sb!pcl::class-has-a-forward-referenced-superclass-p class)
               (when error-context
                 (bug "~@<Invalid class ~S with forward-referenced superclass ~
                       ~S in ~A.~%~:@>"
                      class
                      (sb!pcl::class-has-a-forward-referenced-superclass-p class)
                      error-context))
               nil)
              (t
               (sb!pcl:finalize-inheritance class)
               t)))
          (bug "~@<Don't know how to ensure validity of ~S (not a STANDARD-CLASSOID) ~
                for ~A.~%~:@>"
               classoid (or error-context 'subtypep)))))

(defun %ensure-both-classoids-valid (class1 class2 &optional errorp)
  (do ((layout1 (classoid-layout class1) (classoid-layout class1))
       (layout2 (classoid-layout class2) (classoid-layout class2))
       (i 0 (+ i 1)))
      ((and (not (layout-invalid layout1)) (not (layout-invalid layout2)))
       t)
    (aver (< i 2))
    (unless (and (%ensure-classoid-valid class1 layout1 errorp)
                 (%ensure-classoid-valid class2 layout2 errorp))
      (return-from %ensure-both-classoids-valid nil))))

#-sb-xc-host ; No such thing as LAYOUT-OF, never mind the rest
(defun update-object-layout-or-invalid (object layout)
  ;; FIXME: explain why this isn't (LAYOUT-FOR-STD-CLASS-P LAYOUT).
  (if (layout-for-std-class-p (layout-of object))
      (sb!pcl::check-wrapper-validity object)
      (sb!c::%layout-invalid-error object layout)))

;;; Simple methods for TYPE= and SUBTYPEP should never be called when
;;; the two classes are equal, since there are EQ checks in those
;;; operations.
(!define-type-method (classoid :simple-=) (type1 type2)
  (aver (not (eq type1 type2)))
  (values nil t))

(!define-type-method (classoid :simple-subtypep) (class1 class2)
  (aver (not (eq class1 class2)))
  (with-world-lock ()
    (if (%ensure-both-classoids-valid class1 class2)
        (let ((subclasses2 (classoid-subclasses class2)))
          (if (and subclasses2 (gethash class1 subclasses2))
              (values t t)
              (if (and (typep class1 'standard-classoid)
                       (typep class2 'standard-classoid)
                       (or (sb!pcl::class-has-a-forward-referenced-superclass-p
                            (classoid-pcl-class class1))
                           (sb!pcl::class-has-a-forward-referenced-superclass-p
                            (classoid-pcl-class class2))))
                  ;; If there's a forward-referenced class involved we don't know for sure.
                  ;; (There are cases which we /could/ figure out, but that doesn't seem
                  ;; to be required or important, really.)
                  (values nil nil)
                  (values nil t))))
        (values nil nil))))

;;; When finding the intersection of a sealed class and some other
;;; class (not hierarchically related) the intersection is the union
;;; of the currently shared subclasses.
(defun sealed-class-intersection2 (sealed other)
  (declare (type classoid sealed other))
  (let ((s-sub (classoid-subclasses sealed))
        (o-sub (classoid-subclasses other)))
    (if (and s-sub o-sub)
        (collect ((res *empty-type* type-union))
          (dohash ((subclass layout) s-sub :locked t)
            (declare (ignore layout))
            (when (gethash subclass o-sub)
              (res (specifier-type subclass))))
          (res))
        *empty-type*)))

(!define-type-method (classoid :simple-intersection2) (class1 class2)
  (declare (type classoid class1 class2))
  (with-world-lock ()
    (%ensure-both-classoids-valid class1 class2 "type intersection")
    (cond ((eq class1 class2)
           class1)
          ;; If one is a subclass of the other, then that is the
          ;; intersection.
          ((let ((subclasses (classoid-subclasses class2)))
             (and subclasses (gethash class1 subclasses)))
           class1)
          ((let ((subclasses (classoid-subclasses class1)))
             (and subclasses (gethash class2 subclasses)))
           class2)
          ;; Otherwise, we can't in general be sure that the
          ;; intersection is empty, since a subclass of both might be
          ;; defined. But we can eliminate it for some special cases.
          ((or (structure-classoid-p class1)
               (structure-classoid-p class2))
           ;; No subclass of both can be defined.
           *empty-type*)
          ((eq (classoid-state class1) :sealed)
           ;; checking whether a subclass of both can be defined:
           (sealed-class-intersection2 class1 class2))
          ((eq (classoid-state class2) :sealed)
           ;; checking whether a subclass of both can be defined:
           (sealed-class-intersection2 class2 class1))
          (t
           ;; uncertain, since a subclass of both might be defined
           nil))))

;;; KLUDGE: we need this to deal with the special-case INSTANCE and
;;; FUNCALLABLE-INSTANCE types (which used to be CLASSOIDs until CSR
;;; discovered that this was incompatible with the MOP class
;;; hierarchy).  See NAMED :COMPLEX-SUBTYPEP-ARG2
(defvar *non-instance-classoid-types*
  '(symbol system-area-pointer weak-pointer code-component
    lra fdefn random-class))

;;; KLUDGE: we need this because of the need to represent
;;; intersections of two classes, even when empty at a given time, as
;;; uncanonicalized intersections because of the possibility of later
;;; defining a subclass of both classes.  The necessity for changing
;;; the default return value from SUBTYPEP to NIL, T if no alternate
;;; method is present comes about because, unlike the other places we
;;; use INVOKE-COMPLEX-SUBTYPEP-ARG1-METHOD, in HAIRY methods and the
;;; like, classes are in their own hierarchy with no possibility of
;;; mixtures with other type classes.
(!define-type-method (classoid :complex-subtypep-arg2) (type1 class2)
  (if (and (intersection-type-p type1)
           (> (count-if #'classoid-p (intersection-type-types type1)) 1))
      (values nil nil)
      (invoke-complex-subtypep-arg1-method type1 class2 nil t)))

(!define-type-method (classoid :negate) (type) (make-negation-type type))

(!define-type-method (classoid :unparse) (type)
  (classoid-proper-name type))

;;;; built-in classes

;;; The BUILT-IN-CLASSES list is a data structure which configures the
;;; creation of all the built-in classes. It contains all the info
;;; that we need to maintain the mapping between classes, compile-time
;;; types and run-time type codes. These options are defined:
;;;
;;; :TRANSLATION (default none)
;;;     When this class is "parsed" as a type specifier, it is
;;;     translated into the specified internal type representation,
;;;     rather than being left as a class. This is used for types
;;;     which we want to canonicalize to some other kind of type
;;;     object because in general we want to be able to include more
;;;     information than just the class (e.g. for numeric types.)
;;;
;;; :STATE (default :SEALED)
;;;     The value of CLASS-STATE which we want on completion,
;;;     indicating whether subclasses can be created at run-time.
;;;
;;; :HIERARCHICAL-P (default T unless any of the inherits are non-hierarchical)
;;;     True if we can assign this class a unique inheritance depth.
;;;
;;; :CODES (default none)
;;;     Run-time type codes which should be translated back to this
;;;     class by CLASS-OF. Unspecified for abstract classes.
;;;
;;; :INHERITS (default this class and T)
;;;     The class-precedence list for this class, with this class and
;;;     T implicit.
;;;
;;; :DIRECT-SUPERCLASSES (default to head of CPL)
;;;     List of the direct superclasses of this class.
;;;
;;; NB: not to be confused with SB-PCL::*BUILT-IN-CLASSES*
(!defvar *!built-in-classes*
   ;; To me these data would look nicer with commas instead of "#."
   '((t :state :read-only :translation t)
     (character :codes (#.sb!vm:character-widetag)
                :translation (character-set)
                :prototype-form (code-char 42))
     (symbol :codes (#.sb!vm:symbol-header-widetag)
             :prototype-form '#:mu)

     (system-area-pointer :codes (#.sb!vm:sap-widetag)
                          :prototype-form (int-sap 42))
     (weak-pointer :codes (#.sb!vm:weak-pointer-widetag)
      :prototype-form (make-weak-pointer (find-package "CL")))
     (code-component :codes (#.sb!vm:code-header-widetag))
     ;; should this be #!-(or x86 x86-64) ?
     (lra :codes (#.sb!vm:return-pc-header-widetag))
     (fdefn :codes (#.sb!vm:fdefn-widetag)
            :prototype-form (make-fdefn "42"))
     (random-class) ; used for unknown type codes

     (function
      :codes (#.sb!vm:closure-header-widetag
              #.sb!vm:simple-fun-header-widetag)
      :state :read-only
      :prototype-form (function (lambda () 42)))

     (number :translation number)
     (complex
      :translation complex
      :inherits (number)
      :codes (#.sb!vm:complex-widetag)
      :prototype-form (complex 42 42))
     (complex-single-float
      :translation (complex single-float)
      :inherits (complex number)
      :codes (#.sb!vm:complex-single-float-widetag)
      :prototype-form (complex 42f0 42f0))
     (complex-double-float
      :translation (complex double-float)
      :inherits (complex number)
      :codes (#.sb!vm:complex-double-float-widetag)
      :prototype-form (complex 42d0 42d0))
     #!+long-float
     (complex-long-float
      :translation (complex long-float)
      :inherits (complex number)
      :codes (#.sb!vm:complex-long-float-widetag)
      :prototype-form (complex 42l0 42l0))
     #!+sb-simd-pack
     (simd-pack
      :translation simd-pack
      :codes (#.sb!vm:simd-pack-widetag)
      :prototype-form (%make-simd-pack-ub64 42 42))
     (real :translation real :inherits (number))
     (float
      :translation float
      :inherits (real number))
     (single-float
      :translation single-float
      :inherits (float real number)
      :codes (#.sb!vm:single-float-widetag)
      :prototype-form 42f0)
     (double-float
      :translation double-float
      :inherits (float real number)
      :codes (#.sb!vm:double-float-widetag)
      :prototype-form 42d0)
     #!+long-float
     (long-float
      :translation long-float
      :inherits (float real number)
      :codes (#.sb!vm:long-float-widetag)
      :prototype-form 42l0)
     (rational
      :translation rational
      :inherits (real number))
     (ratio
      :translation (and rational (not integer))
      :inherits (rational real number)
      :codes (#.sb!vm:ratio-widetag)
      :prototype-form 1/42)
     (integer
      :translation integer
      :inherits (rational real number))
     (fixnum
      :translation (integer #.sb!xc:most-negative-fixnum
                    #.sb!xc:most-positive-fixnum)
      :inherits (integer rational real number)
      :codes #.(mapcar #'symbol-value sb!vm::fixnum-lowtags)
      :prototype-form 42)
     (bignum
      :translation (and integer (not fixnum))
      :inherits (integer rational real number)
      :codes (#.sb!vm:bignum-widetag)
      :prototype-form (expt 2 #.(* sb!vm:n-word-bits (/ 3 2))))

     (array :translation array :codes (#.sb!vm:complex-array-widetag)
            :hierarchical-p nil
            :prototype-form (make-array nil :adjustable t))
     (simple-array
      :translation simple-array :codes (#.sb!vm:simple-array-widetag)
      :inherits (array)
      :prototype-form (make-array nil))
     (sequence
      :translation (or cons (member nil) vector extended-sequence)
      :state :read-only
      :depth 2)
     (vector
      :translation vector :codes (#.sb!vm:complex-vector-widetag)
      :direct-superclasses (array sequence)
      :inherits (array sequence))
     (simple-vector
      :translation simple-vector :codes (#.sb!vm:simple-vector-widetag)
      :direct-superclasses (vector simple-array)
      :inherits (vector simple-array array sequence)
      :prototype-form (make-array 0))
     (bit-vector
      :translation bit-vector :codes (#.sb!vm:complex-bit-vector-widetag)
      :inherits (vector array sequence)
      :prototype-form (make-array 0 :element-type 'bit :fill-pointer t))
     (simple-bit-vector
      :translation simple-bit-vector :codes (#.sb!vm:simple-bit-vector-widetag)
      :direct-superclasses (bit-vector simple-array)
      :inherits (bit-vector vector simple-array
                 array sequence)
      :prototype-form (make-array 0 :element-type 'bit))
     (simple-array-unsigned-byte-2
      :translation (simple-array (unsigned-byte 2) (*))
      :codes (#.sb!vm:simple-array-unsigned-byte-2-widetag)
      :direct-superclasses (vector simple-array)
      :inherits (vector simple-array array sequence)
      :prototype-form (make-array 0 :element-type '(unsigned-byte 2)))
     (simple-array-unsigned-byte-4
      :translation (simple-array (unsigned-byte 4) (*))
      :codes (#.sb!vm:simple-array-unsigned-byte-4-widetag)
      :direct-superclasses (vector simple-array)
      :inherits (vector simple-array array sequence)
      :prototype-form (make-array 0 :element-type '(unsigned-byte 4)))
     (simple-array-unsigned-byte-7
      :translation (simple-array (unsigned-byte 7) (*))
      :codes (#.sb!vm:simple-array-unsigned-byte-7-widetag)
      :direct-superclasses (vector simple-array)
      :inherits (vector simple-array array sequence)
      :prototype-form (make-array 0 :element-type '(unsigned-byte 7)))
     (simple-array-unsigned-byte-8
      :translation (simple-array (unsigned-byte 8) (*))
      :codes (#.sb!vm:simple-array-unsigned-byte-8-widetag)
      :direct-superclasses (vector simple-array)
      :inherits (vector simple-array array sequence)
      :prototype-form (make-array 0 :element-type '(unsigned-byte 8)))
     (simple-array-unsigned-byte-15
      :translation (simple-array (unsigned-byte 15) (*))
      :codes (#.sb!vm:simple-array-unsigned-byte-15-widetag)
      :direct-superclasses (vector simple-array)
      :inherits (vector simple-array array sequence)
      :prototype-form (make-array 0 :element-type '(unsigned-byte 15)))
     (simple-array-unsigned-byte-16
      :translation (simple-array (unsigned-byte 16) (*))
      :codes (#.sb!vm:simple-array-unsigned-byte-16-widetag)
      :direct-superclasses (vector simple-array)
      :inherits (vector simple-array array sequence)
      :prototype-form (make-array 0 :element-type '(unsigned-byte 16)))

     (simple-array-unsigned-fixnum
      :translation (simple-array (unsigned-byte #.sb!vm:n-positive-fixnum-bits) (*))
      :codes (#.sb!vm:simple-array-unsigned-fixnum-widetag)
      :direct-superclasses (vector simple-array)
      :inherits (vector simple-array array sequence)
      :prototype-form (make-array 0
                       :element-type '(unsigned-byte #.sb!vm:n-positive-fixnum-bits)))

     (simple-array-unsigned-byte-31
      :translation (simple-array (unsigned-byte 31) (*))
      :codes (#.sb!vm:simple-array-unsigned-byte-31-widetag)
      :direct-superclasses (vector simple-array)
      :inherits (vector simple-array array sequence)
      :prototype-form (make-array 0 :element-type '(unsigned-byte 31)))
     (simple-array-unsigned-byte-32
      :translation (simple-array (unsigned-byte 32) (*))
      :codes (#.sb!vm:simple-array-unsigned-byte-32-widetag)
      :direct-superclasses (vector simple-array)
      :inherits (vector simple-array array sequence)
      :prototype-form (make-array 0 :element-type '(unsigned-byte 32)))
     #!+64-bit
     (simple-array-unsigned-byte-63
      :translation (simple-array (unsigned-byte 63) (*))
      :codes (#.sb!vm:simple-array-unsigned-byte-63-widetag)
      :direct-superclasses (vector simple-array)
      :inherits (vector simple-array array sequence)
      :prototype-form (make-array 0 :element-type '(unsigned-byte 63)))
     #!+64-bit
     (simple-array-unsigned-byte-64
      :translation (simple-array (unsigned-byte 64) (*))
      :codes (#.sb!vm:simple-array-unsigned-byte-64-widetag)
      :direct-superclasses (vector simple-array)
      :inherits (vector simple-array array sequence)
      :prototype-form (make-array 0 :element-type '(unsigned-byte 64)))
     (simple-array-signed-byte-8
      :translation (simple-array (signed-byte 8) (*))
      :codes (#.sb!vm:simple-array-signed-byte-8-widetag)
      :direct-superclasses (vector simple-array)
      :inherits (vector simple-array array sequence)
      :prototype-form (make-array 0 :element-type '(signed-byte 8)))
     (simple-array-signed-byte-16
      :translation (simple-array (signed-byte 16) (*))
      :codes (#.sb!vm:simple-array-signed-byte-16-widetag)
      :direct-superclasses (vector simple-array)
      :inherits (vector simple-array array sequence)
      :prototype-form (make-array 0 :element-type '(signed-byte 16)))

     (simple-array-fixnum
      :translation (simple-array (signed-byte #.sb!vm:n-fixnum-bits)
                    (*))
      :codes (#.sb!vm:simple-array-fixnum-widetag)
      :direct-superclasses (vector simple-array)
      :inherits (vector simple-array array sequence)
      :prototype-form (make-array 0
                       :element-type
                       '(signed-byte #.sb!vm:n-fixnum-bits)))

     (simple-array-signed-byte-32
      :translation (simple-array (signed-byte 32) (*))
      :codes (#.sb!vm:simple-array-signed-byte-32-widetag)
      :direct-superclasses (vector simple-array)
      :inherits (vector simple-array array sequence)
      :prototype-form (make-array 0 :element-type '(signed-byte 32)))
     #!+64-bit
     (simple-array-signed-byte-64
      :translation (simple-array (signed-byte 64) (*))
      :codes (#.sb!vm:simple-array-signed-byte-64-widetag)
      :direct-superclasses (vector simple-array)
      :inherits (vector simple-array array sequence)
      :prototype-form (make-array 0 :element-type '(signed-byte 64)))
     (simple-array-single-float
      :translation (simple-array single-float (*))
      :codes (#.sb!vm:simple-array-single-float-widetag)
      :direct-superclasses (vector simple-array)
      :inherits (vector simple-array array sequence)
      :prototype-form (make-array 0 :element-type 'single-float))
     (simple-array-double-float
      :translation (simple-array double-float (*))
      :codes (#.sb!vm:simple-array-double-float-widetag)
      :direct-superclasses (vector simple-array)
      :inherits (vector simple-array array sequence)
      :prototype-form (make-array 0 :element-type 'double-float))
     #!+long-float
     (simple-array-long-float
      :translation (simple-array long-float (*))
      :codes (#.sb!vm:simple-array-long-float-widetag)
      :direct-superclasses (vector simple-array)
      :inherits (vector simple-array array sequence)
      :prototype-form (make-array 0 :element-type 'long-float))
     (simple-array-complex-single-float
      :translation (simple-array (complex single-float) (*))
      :codes (#.sb!vm:simple-array-complex-single-float-widetag)
      :direct-superclasses (vector simple-array)
      :inherits (vector simple-array array sequence)
      :prototype-form (make-array 0 :element-type '(complex single-float)))
     (simple-array-complex-double-float
      :translation (simple-array (complex double-float) (*))
      :codes (#.sb!vm:simple-array-complex-double-float-widetag)
      :direct-superclasses (vector simple-array)
      :inherits (vector simple-array array sequence)
      :prototype-form (make-array 0 :element-type '(complex double-float)))
     #!+long-float
     (simple-array-complex-long-float
      :translation (simple-array (complex long-float) (*))
      :codes (#.sb!vm:simple-array-complex-long-float-widetag)
      :direct-superclasses (vector simple-array)
      :inherits (vector simple-array array sequence)
      :prototype-form (make-array 0 :element-type '(complex long-float)))
     (string
      :translation string
      :direct-superclasses (vector)
      :inherits (vector array sequence))
     (simple-string
      :translation simple-string
      :direct-superclasses (string simple-array)
      :inherits (string vector simple-array array sequence))
     (vector-nil
      :translation (vector nil)
      :codes (#.sb!vm:complex-vector-nil-widetag)
      :direct-superclasses (string)
      :inherits (string vector array sequence)
      :prototype-form (make-array 0 :element-type 'nil :fill-pointer t))
     (simple-array-nil
      :translation (simple-array nil (*))
      :codes (#.sb!vm:simple-array-nil-widetag)
      :direct-superclasses (vector-nil simple-string)
      :inherits (vector-nil simple-string string vector simple-array
                 array sequence)
      :prototype-form (make-array 0 :element-type 'nil))
     (base-string
      :translation base-string
      :codes (#.sb!vm:complex-base-string-widetag)
      :direct-superclasses (string)
      :inherits (string vector array sequence)
      :prototype-form (make-array 0 :element-type 'base-char :fill-pointer t))
     (simple-base-string
      :translation simple-base-string
      :codes (#.sb!vm:simple-base-string-widetag)
      :direct-superclasses (base-string simple-string)
      :inherits (base-string simple-string string vector simple-array
                 array sequence)
      :prototype-form (make-array 0 :element-type 'base-char))
     #!+sb-unicode
     (character-string
      :translation (vector character)
      :codes (#.sb!vm:complex-character-string-widetag)
      :direct-superclasses (string)
      :inherits (string vector array sequence)
      :prototype-form (make-array 0 :element-type 'character :fill-pointer t))
     #!+sb-unicode
     (simple-character-string
      :translation (simple-array character (*))
      :codes (#.sb!vm:simple-character-string-widetag)
      :direct-superclasses (character-string simple-string)
      :inherits (character-string simple-string string vector simple-array
                 array sequence)
      :prototype-form (make-array 0 :element-type 'character))
     (list
      :translation (or cons (member nil))
      :inherits (sequence))
     (cons
      :codes (#.sb!vm:list-pointer-lowtag)
      :translation cons
      :inherits (list sequence)
      :prototype-form (cons nil nil))
     (null
      :translation (member nil)
      :inherits (symbol list sequence)
      :direct-superclasses (symbol list)
      :prototype-form 'nil)
     (stream
      :state :read-only
      :depth 2)
     (file-stream
      :state :read-only
      :depth 4
      :inherits (stream))
     (string-stream
      :state :read-only
      :depth 4
      :inherits (stream))))

;;; See also src/code/class-init.lisp where we finish setting up the
;;; translations for built-in types.
(!cold-init-forms
  (dolist (x *!built-in-classes*)
    #-sb-xc-host (/show0 "at head of loop over *!BUILT-IN-CLASSES*")
    (destructuring-bind
        (name &key
              (translation nil trans-p)
              inherits
              codes
              state
              depth
              prototype-form
              (hierarchical-p t) ; might be modified below
              (direct-superclasses (if inherits
                                     (list (car inherits))
                                     '(t))))
        x
      (declare (ignore codes state translation prototype-form))
      (let ((inherits-list (if (eq name t)
                               ()
                               (cons t (reverse inherits))))
            (classoid
             (acond #+sb-xc ; genesis dumps some classoid literals
                    ((find-classoid name nil)
                     ;; Unseal it so that REGISTER-LAYOUT doesn't warn
                     (setf (classoid-state it) nil)
                     it)
                    (t
                     (setf (classoid-cell-classoid
                            (find-classoid-cell name :create t))
                           (mark-ctype-interned
                            (make-built-in-classoid
                             :name name
                             :translation (if trans-p :initializing nil)
                             :direct-superclasses
                             (if (eq name t)
                                 nil
                                 (mapcar #'find-classoid
                                         direct-superclasses)))))))))
        (setf (info :type :kind name) :primitive)
        (unless trans-p
          (setf (info :type :builtin name) classoid))
        (let* ((inherits-vector
                (map 'simple-vector
                     (lambda (x)
                       (let ((super-layout
                              (classoid-layout (find-classoid x))))
                         (when (minusp (layout-depthoid super-layout))
                           (setf hierarchical-p nil))
                         super-layout))
                     inherits-list))
               (depthoid (if hierarchical-p
                           (or depth (length inherits-vector))
                           -1)))
          (register-layout
           (find-and-init-or-check-layout name
                                          0
                                          inherits-vector
                                          depthoid
                                          0)
           :invalidate nil)))))
  (/show0 "done with loop over *!BUILT-IN-CLASSES*"))

;;; Define temporary PCL STANDARD-CLASSes. These will be set up
;;; correctly and the Lisp layout replaced by a PCL wrapper after PCL
;;; is loaded and the class defined.
(!cold-init-forms
  (/show0 "about to define temporary STANDARD-CLASSes")
  ;; You'd think with all the pedantic explanation in here it would at least
  ;; be right, but it isn't: layout-inherits for FUNDAMENTAL-STREAM
  ;; ends up as (T SLOT-OBJECT STREAM STANDARD-OBJECT)
  (dolist (x '(;; Why is STREAM duplicated in this list? Because, when
               ;; the inherits-vector of FUNDAMENTAL-STREAM is set up,
               ;; a vector containing the elements of the list below,
               ;; i.e. '(T STREAM STREAM), is created, and
               ;; this is what the function ORDER-LAYOUT-INHERITS
               ;; would do, too.
               ;;
               ;; So, the purpose is to guarantee a valid layout for
               ;; the FUNDAMENTAL-STREAM class, matching what
               ;; ORDER-LAYOUT-INHERITS would do.
               ;; ORDER-LAYOUT-INHERITS would place STREAM at index 2
               ;; in the INHERITS(-VECTOR). Index 1 would not be
               ;; filled, so STREAM is duplicated there (as
               ;; ORDER-LAYOUTS-INHERITS would do). Maybe the
               ;; duplicate definition could be removed (removing a
               ;; STREAM element), because FUNDAMENTAL-STREAM is
               ;; redefined after PCL is set up, anyway. But to play
               ;; it safely, we define the class with a valid INHERITS
               ;; vector.
               (fundamental-stream (t stream stream))))
    (/show0 "defining temporary STANDARD-CLASS")
    (let* ((name (first x))
           (inherits-list (second x))
           (classoid (make-standard-classoid :name name))
           (classoid-cell (find-classoid-cell name :create t)))
      ;; Needed to open-code the MAP, below
      (declare (type list inherits-list))
      (setf (classoid-cell-classoid classoid-cell) classoid
            (info :type :kind name) :instance)
      (let ((inherits (map 'simple-vector
                           (lambda (x)
                             (classoid-layout (find-classoid x)))
                           inherits-list)))
        #-sb-xc-host (/show0 "INHERITS=..") #-sb-xc-host (/hexstr inherits)
        (register-layout (find-and-init-or-check-layout name 0 inherits -1 0)
                         :invalidate nil))))
  (/show0 "done defining temporary STANDARD-CLASSes"))

;;; Now that we have set up the class heterarchy, seal the sealed
;;; classes. This must be done after the subclasses have been set up.
(!cold-init-forms
  (dolist (x *!built-in-classes*)
    (destructuring-bind (name &key (state :sealed) &allow-other-keys) x
      (setf (classoid-state (find-classoid name)) state))))

;;;; class definition/redefinition

;;; This is to be called whenever we are altering a class.
#+sb-xc-host
(defun %modify-classoid (classoid) (bug "MODIFY-CLASSOID ~S" classoid))
#-sb-xc-host
(defun %modify-classoid (classoid)
  (clear-type-caches)
  (awhen (classoid-state classoid)
    ;; FIXME: This should probably be CERROR.
    (warn "making ~(~A~) class ~S writable" it (classoid-name classoid))
    (setf (classoid-state classoid) nil)))

;;; Mark LAYOUT as invalid. Setting DEPTHOID -1 helps cause unsafe
;;; structure type tests to fail. Remove class from all superclasses
;;; too (might not be registered, so might not be in subclasses of the
;;; nominal superclasses.)  We set the layout-clos-hash slots to 0 to
;;; invalidate the wrappers for specialized dispatch functions, which
;;; use those slots as indexes into tables.
(defun %invalidate-layout (layout)
  (declare (type layout layout))
  (setf (layout-invalid layout) t
        (layout-depthoid layout) -1)
  (setf (layout-clos-hash layout) 0)
  (let ((inherits (layout-inherits layout))
        (classoid (layout-classoid layout)))
    (%modify-classoid classoid)
    (dovector (super inherits)
      (let ((subs (classoid-subclasses (layout-classoid super))))
        (when subs
          (remhash classoid subs)))))
  (values))

;;;; cold loading initializations

;;; FIXME: It would be good to arrange for this to be called when the
;;; cross-compiler is being built, not just when the target Lisp is
;;; being cold loaded. Perhaps this could be moved to its own file
;;; late in the build-order.lisp-expr sequence, and be put in
;;; !COLD-INIT-FORMS there?
(defun !class-finalize ()
  (dohash ((name layout) *forward-referenced-layouts*)
    (let ((class (find-classoid name nil)))
      (cond ((not class)
             (setf (layout-classoid layout) (make-undefined-classoid name)))
            ((eq (classoid-layout class) layout)
             (remhash name *forward-referenced-layouts*))
            (t
             (error "Something strange with forward layout for ~S:~%  ~S"
                    name layout))))))

(!cold-init-forms
  #-sb-xc-host (/show0 "about to set *BUILT-IN-CLASS-CODES*")
  (setq **built-in-class-codes**
        (let* ((initial-element (classoid-layout (find-classoid 'random-class)))
               (res (make-array 256 :initial-element initial-element)))
          (dolist (x *!built-in-classes* res)
            (destructuring-bind (name &key codes &allow-other-keys)
                                x
              (let ((layout (classoid-layout (find-classoid name))))
                (dolist (code codes)
                  (setf (svref res code) layout)))))))
  #-sb-xc-host (/show0 "done setting *BUILT-IN-CLASS-CODES*"))

(!defun-from-collected-cold-init-forms !classes-cold-init)
