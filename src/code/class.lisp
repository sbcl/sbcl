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

(in-package "SB-KERNEL")

(!begin-collecting-cold-init-forms)

;;;; the CLASSOID structure

;;; The CLASSOID structure is a supertype of all classoid types.
;;; Its definition occurs in 'early-classoid.lisp'
#+sb-xc-host
(defmethod sb-xc:make-load-form ((self classoid) &optional env)
  (declare (ignore env))
  `(find-classoid ',(classoid-name self)))


;;;; basic LAYOUT stuff

;;; a vector of conses, initialized by genesis
;;;
;;; In each cons, the car is the symbol naming the layout, and the
;;; cdr is the layout itself.
;;; If #+metaspace then the cdr is actually of type WRAPPER,
;;; and if #-metaspace then the wrapper is a LAYOUT.
(defvar *!initial-wrappers*)

;;; a table mapping class names to layouts for classes we have
;;; referenced but not yet loaded. This is initialized from an alist
;;; created by genesis describing the layouts that genesis created at
;;; cold-load time.
(define-load-time-global *forward-referenced-wrappers*
    ;; FIXME: why is the test EQUAL and not EQ? Aren't the keys all symbols?
    (make-hash-table :test 'equal))
#-sb-xc-host
(!cold-init-forms
  ;; *forward-referenced-wrappers* is protected by *WORLD-LOCK*
  ;; so it does not need a :synchronized option.
 (setq *forward-referenced-wrappers* (make-hash-table :test 'equal))
 (dovector (x *!initial-wrappers*)
   (let ((expected (hash-layout-name (car x)))
         (actual (layout-clos-hash (wrapper-friend (cdr x)))))
     (unless (= actual expected) (bug "XC layout hash calculation failed")))
   (setf (gethash (car x) *forward-referenced-wrappers*) (cdr x))))

;;; The LAYOUT structure itself is defined in 'early-classoid.lisp'

(defvar *print-layout-id* t)
(defmethod print-object ((layout layout) stream)
  (print-unreadable-object (layout stream :type t :identity t)
    (format stream
            #+sb-xc-host "for ~S~@[, INVALID=~S~]"
            #-sb-xc-host "~@[(ID=~d) ~]for ~S~@[, INVALID=~S~]"
            #-sb-xc-host (when *print-layout-id* (layout-id layout))
            (layout-proper-name layout)
            (layout-invalid layout))))

(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  (defun layout-proper-name (layout)
    (classoid-proper-name (layout-classoid layout))))

;;; Return the layout currently installed in the classoid named NAME.
;;; If there is none, then make a layout referring for an undefined classoid.
(declaim (ftype (sfunction (symbol) layout) find-layout))
(defun find-layout (name)
  (binding* ((classoid (find-classoid name nil) :exit-if-null) ; threadsafe
             (layout (classoid-layout classoid) :exit-if-null))
    (return-from find-layout layout))
  (let ((table *forward-referenced-wrappers*))
    (with-world-lock ()
      (let ((classoid (find-classoid name nil)))
        (or (and classoid (classoid-layout classoid))
            (acond ((gethash name table)
                    (wrapper-friend it))
                   (t
                    (let ((new (make-layout (hash-layout-name name)
                                            (or classoid (make-undefined-classoid name)))))
                      (setf (gethash name table) (layout-friend new))
                      new))))))))

;;; In code for the target Lisp, we don't dump LAYOUTs using the
;;; standard load form mechanism, we use special fops instead, in
;;; order to make cold load come out right. But when we're building
;;; the cross-compiler, we can't do that because we don't have access
;;; to special non-ANSI low-level things like special fops, and we
;;; don't need to do that anyway because our code isn't going to be
;;; cold loaded, so we use the ordinary load form system.
#+sb-xc-host
(progn
(defmethod make-load-form ((layout layout) &optional env)
  (declare (ignore env))
  (labels ((externalize (layout &aux (classoid (layout-classoid layout))
                                     (name (classoid-name classoid)))
             (when (layout-invalid layout)
               (sb-c:compiler-error "can't dump reference to obsolete class: ~S"
                                    (layout-classoid layout)))
             (unless name
               (sb-c:compiler-error "can't dump anonymous LAYOUT: ~S" layout))
             (aver (= (layout-flags layout)
                      (typecase classoid
                        (structure-classoid +structure-layout-flag+)
                        (condition-classoid +condition-layout-flag+)
                        (undefined-classoid
                         (bug "xc MAKE-LOAD-FORM on undefined layout"))
                        (t 0))))
             `(xc-load-layout ',name
                              ,(layout-depthoid layout)
                              (vector ,@(map 'list #'externalize (layout-inherits layout)))
                              ,(layout-length layout)
                              ,(layout-bitmap layout)
                              ,(layout-flags layout))))
      (externalize layout)))
(defun xc-load-layout (name depthoid inherits length bitmap flags)
  (let ((classoid (find-classoid name)))
    (aver (and classoid (not (undefined-classoid-p classoid))))
    (let ((layout (classoid-layout classoid)))
      (aver layout)
      (unless (and (= (layout-depthoid layout) depthoid)
                   (= (length (layout-inherits layout)) (length inherits))
                   (every #'eq (layout-inherits layout) inherits)
                   (= (layout-length layout) length)
                   (= (layout-bitmap layout) bitmap)
                   (= (layout-flags layout) flags))
        (error "XC can't reload layout for ~S with ~S vs ~A"
               name (list depthoid inherits length bitmap flags) layout))
      layout))))

;;; If LAYOUT's slot values differ from the specified slot values in
;;; any interesting way, then give a warning and return T.
(declaim (ftype (function (simple-string
                           layout
                           simple-string
                           index
                           simple-vector
                           layout-depthoid
                           layout-bitmap))
                warn-if-altered-layout))
(defun warn-if-altered-layout (old-context old-layout context
                               length inherits depthoid bitmap)
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

(defun load-layout (name depthoid inherits length bitmap flags)
  (let* ((layout
          (or (binding* ((classoid (find-classoid name nil) :exit-if-null))
                (classoid-layout classoid))
              (let ((table *forward-referenced-wrappers*))
                (with-world-lock ()
                 (let ((classoid (find-classoid name nil)))
                   (or (and classoid (classoid-layout classoid))
                       (let ((wrapper (gethash name table)))
                         (if wrapper
                             (wrapper-friend wrapper)
                             (let ((new (make-layout
                                         (hash-layout-name name)
                                         (or classoid (make-undefined-classoid name))
                                         :depthoid depthoid :inherits inherits
                                         :length length :bitmap bitmap :flags flags)))
                               (setf (gethash name table) (layout-friend new))
                               new)))))))))
         (classoid
          (or (find-classoid name nil) (layout-classoid layout))))
    (if (or (eq (layout-invalid layout) :uninitialized)
            (not *type-system-initialized*))
        (setf (layout-classoid layout) classoid)
        ;; There was an old layout already initialized with old
        ;; information, and we'll now check that old information
        ;; which was known with certainty is consistent with current
        ;; information which is known with certainty.
        (when (warn-if-altered-layout "current" layout "compile time"
                                    length inherits depthoid bitmap)
          (error "The loaded code expects an incompatible layout for class ~S."
                 (layout-proper-name layout))))
    layout))

;;; Record LAYOUT as the layout for its class, adding it as a subtype
;;; of all superclasses. This is the operation that "installs" a
;;; layout for a class in the type system, clobbering any old layout.
;;; However, this does not modify the class namespace; that is a
;;; separate operation (think anonymous classes.)
;;; -- If INVALIDATE, then all the layouts for any old definition
;;;    and subclasses are invalidated, and the SUBCLASSES slot is cleared.
;;; -- If DESTRUCT-LAYOUT, then this is some old layout, and is to be
;;;    destructively modified to hold the same type information.
(macrolet ((set-bitmap-from-layout (to-layout from-layout)
             `(let ((to-index
                     (+ (type-dd-length layout)
                        (calculate-extra-id-words (layout-depthoid ,to-layout))))
                    (from-index
                     (+ (type-dd-length layout)
                        (calculate-extra-id-words (layout-depthoid ,from-layout)))))
                (dotimes (i (layout-bitmap-words ,from-layout))
                  (setf (%raw-instance-ref/word ,to-layout (+ to-index i))
                        (%raw-instance-ref/word ,from-layout (+ from-index i)))))))
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
          #+sb-xc-host (error "Why mutate a layout in XC host?")
          #-sb-xc-host
          ;; Destructively modifying a layout is not threadsafe at all.
          ;; Use at your own risk (interactive use only).
          (let ((inherits (layout-inherits layout))
                (depthoid (layout-depthoid layout)) ; "new" depthoid
                (extra-id-words ; "old" extra words
                 (calculate-extra-id-words (layout-depthoid destruct-layout)))
                (id ; read my ID before screwing with the depthoid
                 (layout-id destruct-layout)))
            (aver (logtest +structure-layout-flag+ (layout-flags layout)))
            (aver (= (length inherits) depthoid))
            ;; DEPTHOID implies the number of words of "extra" IDs preceding the bitmap.
            ;; Layout alteration is forbidden if it would affect the number of such words.
            ;; So MUTABLE-LAYOUT-P should have checked that this is OK, but assert it
            ;; again to be certain. Heap corruption is the greater evil versus a minor
            ;; inconvenience of not offering the RECKLESSLY-CONTINUE restart.
            (aver (= (calculate-extra-id-words depthoid) extra-id-words))
            #-64-bit (setf (layout-depthoid destruct-layout) (layout-depthoid layout)
                           (layout-length destruct-layout) (layout-length layout))
            (setf (layout-flags destruct-layout) (layout-flags layout)
                  (layout-info destruct-layout) (layout-info layout))
            ;; Zero out the inherited ID values one word at a time.
            ;; This makes self-ID transiently disappear, but what else can we do?
            ;; It's may be in the wrong slot anyway, depending on whether depthoid changed.
            ;; The calculation of the min word count of 3 or 6 is done as
            ;;   (/ (- (1+ layout-id-vector-fixed-capacity) 2) number-of-ids-per-word)
            ;; which is surely more confusing than spelling it as 3 or 6.
            (dotimes (i (+ extra-id-words #+64-bit 3 #-64-bit 6))
              (setf (%raw-instance-ref/word destruct-layout
                                            (+ (get-dsd-index layout id-word0) i))
                    0))
            (set-layout-inherits destruct-layout inherits t id)
            (set-bitmap-from-layout destruct-layout layout)
            (setf (layout-invalid destruct-layout) nil
                  (classoid-wrapper classoid) (layout-friend destruct-layout)))
          (setf (layout-invalid layout) nil
                (classoid-wrapper classoid) (layout-friend layout)))

      (dovector (super-layout (layout-inherits layout))
        (let* ((super (layout-classoid super-layout))
               (subclasses
                 (or (classoid-subclasses super)
                     (setf (classoid-subclasses super)
                           #+sb-xc-host (make-hash-table :test 'eq)
                           ;; Might as well use CTYPE-HASH-VALUE as a
                           ;; stable hash since we have it.
                           #-sb-xc-host
                           (make-hash-table :hash-function #'type-hash-value
                                            :test 'eq
                                            :synchronized t)))))
          (when (and (eq (classoid-state super) :sealed)
                     (not (gethash classoid subclasses)))
            (warn "unsealing sealed class ~S in order to subclass it"
                  (classoid-name super))
            (setf (classoid-state super) :read-only))
          (setf (gethash classoid subclasses)
                (or destruct-layout layout))))))

  (values)))

;;; Arrange the inherited layouts to appear at their expected depth,
;;; ensuring that hierarchical type tests succeed. Layouts with
;;; DEPTHOID >= 0 (i.e. hierarchical classes) are placed first,
;;; at exactly that index in the INHERITS vector. Then, non-hierarchical
;;; layouts are placed in remaining elements. Then, any still-empty
;;; elements are filled with their successors, ensuring that each
;;; element contains a valid layout.
;;;
;;; *** FIXME *** the preceding comment seems dubious, and I'm not sure whether
;;; to fix the code or the comment or both. The code works as-is, but is too hairy.
;;; I fail to see how "still-empty" elements can exist after filling in mandatory
;;; elements. It seems to anticipate being able to create a type whose INHERITS vector
;;; length exceeds depthoid, or, say, a type at depthoid 5 which inherits STREAM but
;;; might lack an entry at depth index 1 for example. As to why I think the comment
;;; is false: FILE-STREAM and STRING-STREAM each have depthoid 4, but their INHERITS
;;; vector has length 2. So they don't store elements that would be at index 2 and 3.
;;; Length less than depthoid is opposite of what the fill-in logic supports.
;;; How, in practice, could a user achieve such weird states as need this logic?
;;; If impossible, then simplify it.

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
    (loop for (obj1 . obj2) in constraints do
       (incf (first (ensure-gethash obj2 obj-info (list 0))))
       (push obj2 (rest (ensure-gethash obj1 obj-info (list 0)))))
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
                                (:copier nil)
                                (:constructor make-structure-classoid
                                    (&key name &aux (%bits (pack-ctype-bits classoid name))))))

;;;; classoid namespace

(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  (defun (setf find-classoid) (new-value name)
    #-sb-xc (declare (type (or null classoid) new-value))
    (aver new-value)
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
               (let ((old (cl:class-of old-value))
                     (new (cl:class-of new-value)))
                 (unless (eq old new)
                   (bug "Trying to change the metaclass of ~S from ~S to ~S in the ~
                            cross-compiler."
                        name (cl:class-name old) (cl:class-name new))))
               #-sb-xc-host
               (let ((old (classoid-of old-value))
                     (new (classoid-of new-value)))
                 (unless (eq old new)
                   (warn "Changing meta-class of ~S from ~S to ~S."
                         name (classoid-name old) (classoid-name new))))))
            (:primitive
             (error "Cannot redefine standard type ~
                     ~/sb-impl:print-type-specifier/." name))
            (:defined
             (warn "redefining DEFTYPE type to be a class: ~
                    ~/sb-ext:print-symbol-with-prefix/" name)
             (clear-info :type :expander name)
             (clear-info :type :source-location name)))

          (remhash name *forward-referenced-wrappers*)
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
                  (classoid-layout new-value)))))
    new-value)

  (defun %clear-classoid (name cell)
    (ecase (info :type :kind name)
      ((nil))
      (:defined)
      (:primitive
       (error "Attempt to remove :PRIMITIVE type: ~
              ~/sb-impl:print-type-specifier/" name))
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
       (clear-info :type :compiler-layout name)
       (values-specifier-type-cache-clear)))))

;;; Called when we are about to define NAME as a class meeting some
;;; predicate (such as a meta-class type test.) The first result is
;;; always of the desired class. The second result is any existing
;;; LAYOUT for this name.
;;;
;;; Again, this should be compiler-only, but easier to make this
;;; thread-safe.
(defun insured-find-classoid (name predicate constructor)
  (declare (type function predicate)
           (type (or function symbol) constructor))
  (let ((table *forward-referenced-wrappers*))
    (with-system-mutex ((hash-table-lock table))
      (let* ((old (find-classoid name nil))
             (res (if (and old (funcall predicate old))
                      old
                      (funcall constructor :name name)))
             (old-wrapper (or (gethash name table)
                              (when old (classoid-wrapper old))))
             (found (when old-wrapper (wrapper-friend old-wrapper))))
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
(define-type-class classoid :enumerable #'classoid-enumerable-p
                    :might-contain-other-types nil)

(defun classoid-inherits-from (sub super-or-name)
  (declare (type classoid sub)
           (type (or symbol classoid) super-or-name))
  (let ((super (if (symbolp super-or-name)
                   (find-classoid super-or-name)
                   super-or-name)))
    (find (classoid-layout super)
          (layout-inherits (classoid-layout sub)))))

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
              ((sb-mop:class-finalized-p class)
               (sb-pcl::%force-cache-flushes class)
               t)
              ((sb-pcl::class-has-a-forward-referenced-superclass-p class)
               (when error-context
                 (bug "~@<Invalid class ~S with forward-referenced superclass ~
                       ~S in ~A.~%~:@>"
                      class
                      (sb-pcl::class-has-a-forward-referenced-superclass-p class)
                      error-context))
               nil)
              (t
               (sb-mop:finalize-inheritance class)
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

;;; Simple methods for TYPE= and SUBTYPEP should never be called when
;;; the two classes are equal, since there are EQ checks in those
;;; operations.
(define-type-method (classoid :simple-=) (type1 type2)
  (aver (not (eq type1 type2)))
  (values nil t))

(define-type-method (classoid :simple-subtypep) (class1 class2)
  (aver (not (eq class1 class2)))
  (with-world-lock ()
    (if (%ensure-both-classoids-valid class1 class2)
        (let ((subclasses2 (classoid-subclasses class2)))
          (if (and subclasses2 (gethash class1 subclasses2))
              (values t t)
              (if (and (typep class1 'standard-classoid)
                       (typep class2 'standard-classoid)
                       (or (sb-pcl::class-has-a-forward-referenced-superclass-p
                            (classoid-pcl-class class1))
                           (sb-pcl::class-has-a-forward-referenced-superclass-p
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

(define-type-method (classoid :simple-intersection2) (class1 class2)
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
          ;; If exactly one of CLASS{1,2} is a CONDITION-CLASSOID,
          ;; there can be no intersection: sub-/superclass relations
          ;; between CONDITION-CLASSOIDs and other CLASSOIDs are not
          ;; possible and a CONDITION-CLASSOIDs cannot be changed into
          ;; different CLASSOIDs.
          ((let ((c1 (condition-classoid-p class1))
                 (c2 (condition-classoid-p class2)))
             (or (and c1 (not c2)) (and (not c1) c2)))
           *empty-type*)
          (t
           ;; uncertain, since a subclass of both might be defined
           nil))))

;;; KLUDGE: we need this to deal with the special-case INSTANCE and
;;; FUNCALLABLE-INSTANCE types (which used to be CLASSOIDs until CSR
;;; discovered that this was incompatible with the MOP class
;;; hierarchy).  See NAMED :COMPLEX-SUBTYPEP-ARG2
(declaim (type cons **non-instance-classoid-types**))
(defglobal **non-instance-classoid-types**
  '(symbol system-area-pointer weak-pointer code-component
    #-(or x86 x86-64) lra
    fdefn random-class))

(defun classoid-non-instance-p (classoid)
  (declare (type classoid classoid))
  (member classoid **non-instance-classoid-types**
          :key #'find-classoid))

;;; KLUDGE: we need this because of the need to represent
;;; intersections of two classes, even when empty at a given time, as
;;; uncanonicalized intersections because of the possibility of later
;;; defining a subclass of both classes.  The necessity for changing
;;; the default return value from SUBTYPEP to NIL, T if no alternate
;;; method is present comes about because, unlike the other places we
;;; use INVOKE-COMPLEX-SUBTYPEP-ARG1-METHOD, in HAIRY methods and the
;;; like, classes are in their own hierarchy with no possibility of
;;; mixtures with other type classes.
(define-type-method (classoid :complex-subtypep-arg2) (type1 class2)
  (if (and (intersection-type-p type1)
           (> (count-if #'classoid-p (intersection-type-types type1)) 1))
      (values nil nil)
      (invoke-complex-subtypep-arg1-method type1 class2 nil t)))

(define-type-method (classoid :negate) (type) (make-negation-type type))

(define-type-method (classoid :unparse) (type)
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
(defconstant-eqx +!built-in-classes+
  ;; constant-quasiquote-form-p is not smart enough to notice
  ;; that this entire thing is constant, so we kind of have to force it.
'#.`((t :state :read-only :translation t)
     (character :codes (,sb-vm:character-widetag)
                :translation (character-set)
                :prototype-form (code-char 42))
     (symbol :codes (,sb-vm:symbol-widetag)
             :prototype-form '*)

     (system-area-pointer :codes (,sb-vm:sap-widetag)
                          :prototype-form (int-sap 0))
     (weak-pointer :codes (,sb-vm:weak-pointer-widetag)
      :prototype-form (make-weak-pointer 0))
     (code-component :codes (,sb-vm:code-header-widetag)
                     :prototype-form (fun-code-header #'identity))
     #-(or x86 x86-64) (lra :codes (,sb-vm:return-pc-widetag)
                            ;; Make the PROTOTYPE slot unbound.
                            :prototype-form sb-pcl:+slot-unbound+)
     (fdefn :codes (,sb-vm:fdefn-widetag)
            :prototype-form (find-or-create-fdefn 'sb-mop:class-prototype))
     (random-class ; used for unknown type codes
            ;; Make the PROTOTYPE slot unbound.
            :prototype-form sb-pcl:+slot-unbound+)
     (function
      :codes (,sb-vm:closure-widetag ,sb-vm:simple-fun-widetag)
      :state :read-only
      :prototype-form #'identity)

     (number :translation number :prototype-form 0)
     (complex
      :translation complex
      :inherits (number)
      :codes (,sb-vm:complex-widetag)
      :prototype-form ,(complex 0 1))
     (complex-single-float
      :translation (complex single-float)
      :inherits (complex number)
      :codes (,sb-vm:complex-single-float-widetag)
      :prototype-form ,(complex $0f0 $0f0))
     (complex-double-float
      :translation (complex double-float)
      :inherits (complex number)
      :codes (,sb-vm:complex-double-float-widetag)
      :prototype-form ,(complex $0d0 $0d0))
     #+long-float
     (complex-long-float
      :translation (complex long-float)
      :inherits (complex number)
      :codes (,sb-vm:complex-long-float-widetag)
      :prototype-form ,(complex $0L0 $0L0))
     #+sb-simd-pack
     (simd-pack
      :translation simd-pack
      :codes (,sb-vm:simd-pack-widetag)
      :prototype-form (%make-simd-pack-ub64 42 42))
     #+sb-simd-pack-256
     (simd-pack-256
      :translation simd-pack-256
      :codes (,sb-vm:simd-pack-256-widetag)
      :prototype-form
      ;; KLUDGE: doesn't work without AVX2 support from the CPU
      ;; (%make-simd-pack-256-ub64 42 42 42 42)
      sb-pcl:+slot-unbound+)
     (real :translation real :inherits (number) :prototype-form 0)
     (float :translation float :inherits (real number) :prototype-form $0f0)
     (single-float
      :translation single-float
      :inherits (float real number)
      :codes (,sb-vm:single-float-widetag)
      :prototype-form $0f0)
     (double-float
      :translation double-float
      :inherits (float real number)
      :codes (,sb-vm:double-float-widetag)
      :prototype-form $0d0)
     #+long-float
     (long-float
      :translation long-float
      :inherits (float real number)
      :codes (,sb-vm:long-float-widetag)
      :prototype-form $0L0)
     (rational
      :translation rational :inherits (real number) :prototype-form 0)
     (ratio
      :translation (and rational (not integer))
      :inherits (rational real number)
      :codes (,sb-vm:ratio-widetag)
      :prototype-form 1/42)
     (integer
      :translation integer :inherits (rational real number) :prototype-form 0)
     (fixnum
      :translation (integer ,most-negative-fixnum ,most-positive-fixnum)
      :inherits (integer rational real number)
      :codes ,(mapcar #'symbol-value sb-vm::fixnum-lowtags)
      :prototype-form 42)
     (bignum
      :translation (and integer (not fixnum))
      :inherits (integer rational real number)
      :codes (,sb-vm:bignum-widetag)
      :prototype-form ,(1+ most-positive-fixnum))

     (array :translation array :codes (,sb-vm:complex-array-widetag)
            :hierarchical-p nil
            :prototype-form (make-array nil :adjustable t))
     (simple-array
      :translation simple-array :codes (,sb-vm:simple-array-widetag)
      :inherits (array)
      :prototype-form (make-array nil))
     (sequence
      :translation (or cons (member nil) vector extended-sequence)
      :state :read-only
      :depth 1)
     (vector
      :translation vector :codes (,sb-vm:complex-vector-widetag)
      :direct-superclasses (array sequence)
      :inherits (array sequence)
      :prototype-form (make-array 0 :adjustable t))
     (simple-vector
      :translation simple-vector :codes (,sb-vm:simple-vector-widetag)
      :direct-superclasses (vector simple-array)
      :inherits (vector simple-array array sequence)
      :prototype-form (make-array 0))
     (bit-vector
      :translation bit-vector :codes (,sb-vm:complex-bit-vector-widetag)
      :inherits (vector array sequence)
      :prototype-form (make-array 0 :element-type 'bit :fill-pointer t))
     (simple-bit-vector
      :translation simple-bit-vector :codes (,sb-vm:simple-bit-vector-widetag)
      :direct-superclasses (bit-vector simple-array)
      :inherits (bit-vector vector simple-array
                 array sequence)
      :prototype-form #*)
     (string
      :translation string
      :direct-superclasses (vector)
      :inherits (vector array sequence)
      :prototype-form "")
     (simple-string
      :translation simple-string
      :direct-superclasses (string simple-array)
      :inherits (string vector simple-array array sequence)
      :prototype-form "")
     (vector-nil
      :translation (vector nil)
      :inherits (vector array sequence)
      :prototype-form (make-array 0 :element-type 'nil :fill-pointer t))
     ;; This name is imperfect. It should be SIMPLE-RANK1-ARRAY-NIL
     ;; to clearly convey that the dimensions are '(*) and not '*.
     (simple-array-nil
      :translation (simple-array nil (*))
      :codes (,sb-vm:simple-array-nil-widetag)
      :direct-superclasses (vector-nil)
      :inherits (vector-nil vector simple-array array sequence)
      :prototype-form (make-array 0 :element-type 'nil))
     (base-string
      :translation base-string
      :codes (,sb-vm:complex-base-string-widetag)
      :direct-superclasses (string)
      :inherits (string vector array sequence)
      :prototype-form (make-array 0 :element-type 'base-char :fill-pointer t))
     (simple-base-string
      :translation simple-base-string
      :codes (,sb-vm:simple-base-string-widetag)
      :direct-superclasses (base-string simple-string)
      :inherits (base-string simple-string string vector simple-array
                 array sequence)
      :prototype-form (make-array 0 :element-type 'base-char))
     #+sb-unicode
     (character-string
      :translation (vector character)
      :codes (,sb-vm:complex-character-string-widetag)
      :direct-superclasses (string)
      :inherits (string vector array sequence)
      :prototype-form (make-array 0 :element-type 'character :fill-pointer t))
     #+sb-unicode
     (simple-character-string
      :translation (simple-array character (*))
      :codes (,sb-vm:simple-character-string-widetag)
      :direct-superclasses (character-string simple-string)
      :inherits (character-string simple-string string vector simple-array
                 array sequence)
      :prototype-form (make-array 0 :element-type 'character))
     (list
      :translation (or cons (member nil))
      :inherits (sequence)
      :prototype-form 'nil)
     (cons
      :codes (,sb-vm:list-pointer-lowtag)
      :translation cons
      :inherits (list sequence)
      :prototype-form (cons nil nil))
     (null
      :translation (member nil)
      :inherits (symbol list sequence)
      :direct-superclasses (symbol list)
      :prototype-form 'nil)

     (sb-pcl::slot-object
      :translation (or structure-object standard-object condition)
      :hierarchical-p nil
      :state :read-only
      :prototype-form (make-defstruct-description t 'arbitrary))

     ;; KLUDGE: the length must match the subsequent defstruct.
     (pathname :depth 1
               :length ,(+ 7 sb-vm:instance-data-start)
               :prototype-form (make-pathname))
     (logical-pathname :depth 2
                       :length ,(+ 7 sb-vm:instance-data-start)
                       :prototype-form (make-pathname :host "SYS")
                       :inherits (pathname))

     ;; These last few are strange. STREAM has only T as an ancestor,
     ;; so you'd think it would be at depth 1. FILE- and STRING-STREAM
     ;; each have STREAM and T as ancestors, so you'd think they'd be at depth
     ;; 1 greater than STREAM, instead of 2 greater. But changing any of
     ;; these to the "obvious" value makes various type checks go wrong.
     ;;
     ;; Essentially the hardwiring corresponds to the indices of the
     ;; respective types in the inherits vector for FD-STREAM.
     ;;  * (layout-inherits (find-layout 'fd-stream))
     ;;  #(#<LAYOUT for T {50300003}>
     ;;    #<LAYOUT for STRUCTURE-OBJECT {50300103}>
     ;;    #<LAYOUT for STREAM {50301003}>
     ;;    #<LAYOUT for ANSI-STREAM {50301183}>
     ;;    #<LAYOUT for FILE-STREAM {50303303}>)

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
      :inherits (stream))
     ,@(loop for x across sb-vm:*specialized-array-element-type-properties*
             unless (member (sb-vm:saetp-specifier x) '(t character base-char nil bit))
             collect
             ;; I'm not sure if it's an accident that there are distinct SB-KERNEL
             ;; versus SB-VM symbols for the specialized arrays. The former are types
             ;; in the language, and the latter are primitive object types,
             ;; but istm they should be designated by the same symbols.
             `(,(intern (string (sb-vm:saetp-primitive-type-name x)) *package*)
               :translation (simple-array ,(sb-vm:saetp-specifier x) (*))
               :codes (,(sb-vm:saetp-typecode x))
               :direct-superclasses (vector simple-array)
               :inherits (vector simple-array array sequence)
               :prototype-form
               (logically-readonlyize
                (make-array 0 :element-type ',(sb-vm:saetp-specifier x))))))
  #'equal)

;;; See also src/code/class-init.lisp where we finish setting up the
;;; translations for built-in types.
(!cold-init-forms
  (dolist (x +!built-in-classes+)
    #-sb-xc-host (/show0 "at head of loop over +!BUILT-IN-CLASSES+")
    (destructuring-bind
        (name &key
              (translation nil trans-p)
              inherits
              codes
              state
              depth
              (length 0)
              prototype-form
              (hierarchical-p t) ; might be modified below
              (direct-superclasses (if inherits
                                     (list (car inherits))
                                     '(t))))
        x
      (declare (ignore codes state translation))
      ;; instance metatypes and T don't need a prototype, everything else does
      (unless (or prototype-form depth (eq name 't))
        (error "Missing prototype in ~S" x))
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
                           (!make-built-in-classoid
                             :%bits (pack-ctype-bits classoid name)
                             :name name
                             :translation (if trans-p :initializing nil)
                             :direct-superclasses
                             (if (eq name t)
                                 nil
                                 (mapcar #'find-classoid
                                         direct-superclasses))))))))
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
          (register-layout (load-layout name
                                        depthoid
                                        inherits-vector
                                        length
                                        +layout-all-tagged+
                                        0) ; flags
                           :invalidate nil)))))
  (/show0 "done with loop over +!BUILT-IN-CLASSES+"))

;;; Now that we have set up the class heterarchy, seal the sealed
;;; classes. This must be done after the subclasses have been set up.
(!cold-init-forms
  (dolist (x +!built-in-classes+)
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

;;; Mark LAYOUT as invalid. This is called only on CONDITION and STRUCTURE
;;; subtypes when redefining incompatibly. PCL objects use invalidate-wrapper.
;;; Remove class from all superclasses
;;; too (might not be registered, so might not be in subclasses of the
;;; nominal superclasses.)  We set the layout-clos-hash slots to 0 to
;;; invalidate the wrappers for specialized dispatch functions, which
;;; use those slots as indexes into tables.
(defun %invalidate-layout (layout)
  (declare (type layout layout))
  #+sb-xc-host (warn "Why are we invalidating layout ~S?" layout)
  (setf (layout-invalid layout) t)
  ;; Ensure that the INVALID slot conveying ancillary data describing the
  ;; invalidity reason is published before causing the invalid layout trap.
  (sb-thread:barrier (:write))
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
  (dohash ((name wrapper) *forward-referenced-wrappers*)
    (let ((class (find-classoid name nil)))
      (cond ((not class)
             (error "How is there no classoid for ~S ?" name))
            ((eq (classoid-wrapper class) wrapper)
             (remhash name *forward-referenced-wrappers*))
            (t
             (error "Something strange with forward layout for ~S:~%  ~S"
                    name wrapper))))))

(!defun-from-collected-cold-init-forms !classes-cold-init)
