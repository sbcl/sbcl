;;;; This file contains the optimization machinery for make-instance.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is derived from software originally released by
;;;; Gerd Moellmann.  Copyright and release statements follow.  Later
;;;; modifications to the software are in the public domain and are
;;;; provided with absolutely no warranty.  See the COPYING and
;;;; CREDITS files for more information.

;;; Copyright (C) 2002 Gerd Moellmann <gerd.moellmann@t-online.de>
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of the author may not be used to endorse or promote
;;;    products derived from this software without specific prior written
;;;    permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
;;; OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
;;; USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
;;; DAMAGE.

;;; ***************
;;; Overview  *****
;;; ***************
;;;
;;; Compiler macro for MAKE-INSTANCE, and load-time generation of
;;; optimized instance constructor functions.
;;;
;;; ********************
;;; Entry Points  ******
;;; ********************
;;;
;;; UPDATE-CTORS must be called when methods are added/removed,
;;; classes are changed, etc., which affect instance creation.
;;;
;;; PRECOMPILE-CTORS can be called to precompile constructor functions
;;; for classes whose definitions are known at the time the function
;;; is called.

(in-package "SB-PCL")

;;; ******************
;;; Utilities  *******
;;; ******************

(defun quote-plist-keys (plist)
  (loop for (key . more) on plist by #'cddr
        if (null more) do
          (error "Not a property list: ~S" plist)
        else
          collect `(quote ,key)
          and collect (car more)))

(defun plist-keys (plist &key test)
  (loop for (key . more) on plist by #'cddr
        if (null more) do
          (error "Not a property list: ~S" plist)
        else if (or (null test) (funcall test key))
          collect key))

(defun plist-values (plist &key test)
  (loop for (key . more) on plist by #'cddr
        if (null more) do
          (error "Not a property list: ~S" plist)
        else if (or (null test) (funcall test (car more)))
          collect (car more)))

(defun constant-class-arg-p (form)
  (and (constantp form)
       (let ((constant (constant-form-value form)))
         (or (and (symbolp constant)
                  (not (null (symbol-package constant))))
             (classp form)))))

(defun constant-symbol-p (form)
  (and (constantp form)
       (let ((constant (constant-form-value form)))
         (and (symbolp constant)
              (not (null (symbol-package constant)))))))

;;; Somewhat akin to DEFAULT-INITARGS, but just collecting the defaulted
;;; initargs for the call.
(defun ctor-default-initkeys (supplied-initargs class-default-initargs)
  (loop for (key) in class-default-initargs
        when (eq (getf supplied-initargs key '.not-there.) '.not-there.)
        collect key))

;;; Like DEFAULT-INITARGS, but return a list that can be spliced into source,
;;; instead of a list with values already evaluated.
(defun ctor-default-initargs (supplied-initargs class-default-initargs)
  (loop for (key form fun) in class-default-initargs
        when (eq (getf supplied-initargs key '.not-there.) '.not-there.)
        append (list key (if (constantp form) form `(funcall ,fun)))
          into default-initargs
        finally
          (return (append supplied-initargs default-initargs))))

;;; *****************
;;; CTORS   *********
;;; *****************
;;;
;;; Ctors are funcallable instances whose initial function is a
;;; function computing an optimized constructor function when called.
;;; When the optimized function is computed, the function of the
;;; funcallable instance is set to it.
;;;
(!defstruct-with-alternate-metaclass ctor
  :slot-names (function-name class-or-name class initargs state safe-p)
  :boa-constructor %make-ctor
  :superclass-name function
  :metaclass-name static-classoid
  :metaclass-constructor make-static-classoid
  :dd-type funcallable-structure
  :runtime-type-checks-p nil)

;;; List of all defined ctors.
(defvar *all-ctors* ())

(defun make-ctor-parameter-list (ctor)
  (plist-values (ctor-initargs ctor) :test (complement #'constantp)))

;;; Reset CTOR to use a default function that will compute an
;;; optimized constructor function when called.
(defun install-initial-constructor (ctor &key force-p)
  (when (or force-p (ctor-class ctor))
    (setf (ctor-class ctor) nil
          (ctor-state ctor) 'initial)
    (setf (funcallable-instance-fun ctor)
          #'(lambda (&rest args)
              (install-optimized-constructor ctor)
              (apply ctor args)))
    (setf (%funcallable-instance-info ctor 1)
          (ctor-function-name ctor))))

(defun make-ctor-function-name (class-name initargs safe-code-p)
  (labels ((arg-name (x)
             (typecase x
               ;; this list of types might look arbitrary but it is
               ;; exactly the set of types descended into by EQUAL,
               ;; which is the predicate used by globaldb to test for
               ;; name equality.
               (list (gensym "LIST-INITARG-"))
               (string (gensym "STRING-INITARG-"))
               (bit-vector (gensym "BIT-VECTOR-INITARG-"))
               (pathname (gensym "PATHNAME-INITARG-"))
               (t x)))
           (munge (list)
             (let ((*gensym-counter* 0))
               (mapcar #'arg-name list))))
    (list* 'ctor class-name safe-code-p (munge initargs))))

;;; Keep this a separate function for testing.
(defun ensure-ctor (function-name class-name initargs safe-code-p)
  (with-world-lock ()
    (if (fboundp function-name)
        (the ctor (fdefinition function-name))
        (make-ctor function-name class-name initargs safe-code-p))))

;;; Keep this a separate function for testing.
(defun make-ctor (function-name class-name initargs safe-p)
  (without-package-locks ; for (setf symbol-function)
    (let ((ctor (%make-ctor function-name class-name nil initargs nil safe-p)))
      (install-initial-constructor ctor :force-p t)
      (push ctor *all-ctors*)
      (setf (fdefinition function-name) ctor)
      ctor)))

;;; *****************
;;; Inline CTOR cache
;;; *****************
;;;
;;; The cache starts out as a list of CTORs, sorted with the most recently
;;; used CTORs near the head. If it expands too much, we switch to a vector
;;; with a simple hashing scheme.

;;; Find CTOR for KEY (which is a class or class name) in a list. If the CTOR
;;; is in the list but not one of the 4 first ones, return a new list with the
;;; found CTOR at the head. Thread-safe: the new list shares structure with
;;; the old, but is not desctructively modified. Returning the old list for
;;; hits close to the head reduces ping-ponging with multiple threads seeking
;;; the same list.
(defun find-ctor (key list)
  (labels ((walk (tail from-head depth)
             (declare (fixnum depth))
             (if tail
                 (let ((ctor (car tail)))
                   (if (eq (ctor-class-or-name ctor) key)
                       (if (> depth 3)
                           (values ctor
                                   (nconc (list ctor) (nreverse from-head) (cdr tail)))
                           (values ctor
                                   list))
                       (walk (cdr tail)
                             (cons ctor from-head)
                             (logand #xf (1+ depth)))))
                 (values nil list))))
    (walk list nil 0)))

(declaim (inline sxhash-symbol-or-class))
(defun sxhash-symbol-or-class (x)
  (cond ((symbolp x) (sxhash x))
        ((std-instance-p x) (sb-impl::std-instance-hash x))
        ((fsc-instance-p x) (sb-impl::fsc-instance-hash x))
        (t
         (bug "Something strange where symbol or class expected."))))

;;; Max number of CTORs kept in an inline list cache. Once this is
;;; exceeded we switch to a table.
(defconstant +ctor-list-max-size+ 12)
;;; Max table size for CTOR cache. If the table fills up at this size
;;; we keep the same size and drop 50% of the old entries.
(defconstant +ctor-table-max-size+ (expt 2 8))
;;; Even if there is space in the cache, if we cannot fit a new entry
;;; with max this number of collisions we expand the table (if possible)
;;; and rehash.
(defconstant +ctor-table-max-probe-depth+ 5)

(defun make-ctor-table (size)
  (declare (index size))
  (let ((real-size (power-of-two-ceiling size)))
    (if (< real-size +ctor-table-max-size+)
        (values (make-array real-size :initial-element nil) nil)
        (values (make-array +ctor-table-max-size+ :initial-element nil) t))))

(declaim (inline mix-ctor-hash))
(defun mix-ctor-hash (hash base)
  (logand most-positive-fixnum (+ hash base 1)))

(defun put-ctor (ctor table)
  (cond ((try-put-ctor ctor table)
         (values ctor table))
        (t
         (expand-ctor-table ctor table))))

;;; Thread-safe: if two threads write to the same index in parallel, the other
;;; result is just lost. This is not an issue as the CTORs are used as their
;;; own keys. If both were EQ, we're good. If non-EQ, the next time the other
;;; one is needed we just cache it again -- hopefully not getting stomped on
;;; that time.
(defun try-put-ctor (ctor table)
  (declare (simple-vector table) (optimize speed))
  (let* ((class (ctor-class-or-name ctor))
         (base (sxhash-symbol-or-class class))
         (hash base)
         (mask (1- (length table))))
    (declare (fixnum base hash mask))
    (loop repeat +ctor-table-max-probe-depth+
          do (let* ((index (logand mask hash))
                    (old (aref table index)))
               (cond ((and old (neq class (ctor-class-or-name old)))
                      (setf hash (mix-ctor-hash hash base)))
                     (t
                      (setf (aref table index) ctor)
                      (return-from try-put-ctor t)))))
    ;; Didn't fit, must expand
    nil))

(defun get-ctor (class table)
  (declare (simple-vector table) (optimize speed))
  (let* ((base (sxhash-symbol-or-class class))
         (hash base)
         (mask (1- (length table))))
    (declare (fixnum base hash mask))
    (loop repeat +ctor-table-max-probe-depth+
          do (let* ((index (logand mask hash))
                    (old (aref table index)))
               (if (and old (eq class (ctor-class-or-name old)))
                   (return-from get-ctor old)
                   (setf hash (mix-ctor-hash hash base)))))
    ;; Nothing.
    nil))

;;; Thread safe: the old table is read, but if another thread mutates
;;; it while we're reading we still get a sane result -- either the old
;;; or the new entry. The new table is locally allocated, so that's ok
;;; too.
(defun expand-ctor-table (ctor old)
  (declare (simple-vector old))
  (let* ((old-size (length old))
         (new-size (* 2 old-size))
         (drop-random-entries nil))
    (tagbody
     :again
       (multiple-value-bind (new max-size-p) (make-ctor-table new-size)
         (let ((action (if drop-random-entries
                           ;; Same logic as in method caches -- see comment
                           ;; there.
                           (randomly-punting-lambda (old-ctor)
                             (try-put-ctor old-ctor new))
                           (lambda (old-ctor)
                             (unless (try-put-ctor old-ctor new)
                               (if max-size-p
                                   (setf drop-random-entries t)
                                   (setf new-size (* 2 new-size)))
                               (go :again))))))
           (aver (try-put-ctor ctor new))
           (dotimes (i old-size)
             (let ((old-ctor (aref old i)))
               (when old-ctor
                 (funcall action old-ctor))))
           (return-from expand-ctor-table (values ctor new)))))))

(defun ctor-list-to-table (list)
  (let ((table (make-ctor-table (length list))))
    (dolist (ctor list)
      (setf table (nth-value 1 (put-ctor ctor table))))
    table))

(defun ensure-cached-ctor (class-name store initargs safe-code-p)
  (flet ((maybe-ctor-for-caching ()
           (if (typep class-name '(or symbol class))
               (let ((name (make-ctor-function-name class-name initargs safe-code-p)))
                 (ensure-ctor name class-name initargs safe-code-p))
               ;; Invalid first argument: let MAKE-INSTANCE worry about it.
               (return-from ensure-cached-ctor
                 (values (lambda (&rest ctor-parameters)
                           (let (mi-initargs)
                             (doplist (key value) initargs
                               (push key mi-initargs)
                               (push (if (constantp value)
                                         value
                                         (pop ctor-parameters))
                                     mi-initargs))
                             (apply #'make-instance class-name (nreverse mi-initargs))))
                         store)))))
    (if (listp store)
        (multiple-value-bind (ctor list) (find-ctor class-name store)
          (if ctor
              (values ctor list)
              (let ((ctor (maybe-ctor-for-caching)))
                (if (< (length list) +ctor-list-max-size+)
                    (values ctor (cons ctor list))
                    (values ctor (ctor-list-to-table list))))))
       (let ((ctor (get-ctor class-name store)))
         (if ctor
             (values ctor store)
             (put-ctor (maybe-ctor-for-caching) store))))))

;;; ***********************************************
;;; Compile-Time Expansion of MAKE-INSTANCE *******
;;; ***********************************************

(defvar *compiling-optimized-constructor* nil)

;;; There are some MAKE-INSTANCE calls compiled prior to this macro definition.
;;; While it would be trivial to move earlier, I'm not sure that it would
;;; actually work.
(define-compiler-macro make-instance (&whole form &rest args &environment env)
  (declare (ignore args))
  ;; Compiling an optimized constructor for a non-standard class means
  ;; compiling a lambda with (MAKE-INSTANCE #<SOME-CLASS X> ...) in it
  ;; -- need to make sure we don't recurse there.
  (or (unless *compiling-optimized-constructor*
        (make-instance->constructor-call form (safe-code-p env)))
      form))

(defun make-instance->constructor-call (form safe-code-p)
  (destructuring-bind (class-arg &rest args) (cdr form)
    (flet (;;
           ;; Return the name of parameter number I of a constructor
           ;; function.
           (parameter-name (i)
             (format-symbol *pcl-package* ".P~D." i))
           ;; Check if CLASS-ARG is a constant symbol.  Give up if
           ;; not.
           (constant-class-p ()
             (and class-arg (constant-class-arg-p class-arg)))
           ;; Check if ARGS are suitable for an optimized constructor.
           ;; Return NIL from the outer function if not.
           (check-args ()
             (loop for (key . more) on args by #'cddr do
                      (when (or (null more)
                                (not (constant-symbol-p key))
                                (eq :allow-other-keys (constant-form-value key)))
                        (return-from make-instance->constructor-call nil)))))
      (check-args)
      ;; Collect a plist of initargs and constant values/parameter names
      ;; in INITARGS.  Collect non-constant initialization forms in
      ;; VALUE-FORMS.
      (multiple-value-bind (initargs value-forms)
          (loop for (key value) on args by #'cddr and i from 0
                collect (constant-form-value key) into initargs
                if (constantp value)
                collect value into initargs
                else
                collect (parameter-name i) into initargs
                and collect value into value-forms
                finally
                (return (values initargs value-forms)))
        (if (constant-class-p)
            (let* ((class-or-name (constant-form-value class-arg))
                   (function-name (make-ctor-function-name class-or-name initargs
                                                           safe-code-p)))
              (sb-int:check-deprecated-type (if (classp class-or-name)
                                                (class-name class-or-name)
                                                class-or-name))
              ;; Prevent compiler warnings for calling the ctor.
              (proclaim-as-fun-name function-name)
              (note-name-defined function-name :function)
              (when (eq (info :function :where-from function-name) :assumed)
                (setf (info :function :where-from function-name) :defined)
                (when (info :function :assumed-type function-name)
                  (setf (info :function :assumed-type function-name) nil)))
              ;; Return code constructing a ctor at load time, which,
              ;; when called, will set its funcallable instance
              ;; function to an optimized constructor function.
              `(locally
                   (declare (disable-package-locks ,function-name))
                 (let ((.x. (load-time-value
                             (ensure-ctor ',function-name ',class-or-name ',initargs
                                          ',safe-code-p))))
                   (declare (ignore .x.))
                   ;; ??? check if this is worth it.
                   (declare
                    (ftype (or (function ,(make-list (length value-forms)
                                                     :initial-element t)
                                         t)
                               (function (&rest t) t))
                           ,function-name))
                   (funcall (function ,function-name) ,@value-forms))))
            (when (and class-arg (not (constantp class-arg)))
              ;; Build an inline cache: a CONS, with the actual cache
              ;; in the CDR.
              `(locally (declare (disable-package-locks .cache. .class-arg. .store. .fun.
                                                        make-instance))
                 (let* ((.cache. (load-time-value (cons 'ctor-cache nil)))
                        (.store. (cdr .cache.))
                        (.class-arg. ,class-arg))
                   (multiple-value-bind (.fun. .new-store.)
                       (ensure-cached-ctor .class-arg. .store. ',initargs ',safe-code-p)
                     ;; Thread safe: if multiple threads hit this in
                     ;; parallel, the update from the other one is
                     ;; just lost -- no harm done, except for the need
                     ;; to redo the work next time.
                     (unless (eq .store. .new-store.)
                       (setf (cdr .cache.) .new-store.))
                     (funcall (truly-the function .fun.) ,@value-forms))))))))))

;;; **************************************************
;;; Load-Time Constructor Function Generation  *******
;;; **************************************************

;;; The system-supplied primary INITIALIZE-INSTANCE and
;;; SHARED-INITIALIZE methods.  One cannot initialize these variables
;;; to the right values here because said functions don't exist yet
;;; when this file is first loaded.
(defvar *the-system-ii-method* nil)
(defvar *the-system-si-method* nil)

(defun install-optimized-constructor (ctor)
  (with-world-lock ()
    (let* ((class-or-name (ctor-class-or-name ctor))
           (class (ensure-class-finalized
                   (if (symbolp class-or-name)
                       (find-class class-or-name)
                       class-or-name))))
      ;; We can have a class with an invalid layout here.  Such a class
      ;; cannot have a LAYOUT-INVALID of (:FLUSH ...) or (:OBSOLETE
      ;; ...), because part of the deal is that those only happen from
      ;; FORCE-CACHE-FLUSHES, which create a new valid wrapper for the
      ;; class.  An invalid layout of T needs to be flushed, however.
      (when (eq (layout-invalid (class-wrapper class)) t)
        (%force-cache-flushes class))
      (setf (ctor-class ctor) class)
      (pushnew ctor (plist-value class 'ctors) :test #'eq)
      (multiple-value-bind (form locations names optimizedp)
          (constructor-function-form ctor)
        (setf (funcallable-instance-fun ctor)
              (apply
               (let ((*compiling-optimized-constructor* t))
                 (handler-bind ((compiler-note #'muffle-warning))
                   (compile nil `(lambda ,names ,form))))
               locations)
              (ctor-state ctor) (if optimizedp 'optimized 'fallback))))))

(defun constructor-function-form (ctor)
  (let* ((class (ctor-class ctor))
         (proto (class-prototype class))
         (make-instance-methods
          (compute-applicable-methods #'make-instance (list class)))
         (allocate-instance-methods
          (compute-applicable-methods #'allocate-instance (list class)))
         ;; I stared at this in confusion for a while, thinking
         ;; carefully about the possibility of the class prototype not
         ;; being of sufficient discrimiating power, given the
         ;; possibility of EQL-specialized methods on
         ;; INITIALIZE-INSTANCE or SHARED-INITIALIZE.  However, given
         ;; that this is a constructor optimization, the user doesn't
         ;; yet have the instance to create a method with such an EQL
         ;; specializer.
         ;;
         ;; There remains the (theoretical) possibility of someone
         ;; coming along with code of the form
         ;;
         ;; (defmethod initialize-instance :before ((o foo) ...)
         ;;   (eval `(defmethod shared-initialize :before ((o foo) ...) ...)))
         ;;
         ;; but probably we can afford not to worry about this too
         ;; much for now.  -- CSR, 2004-07-12
         (ii-methods
          (compute-applicable-methods #'initialize-instance (list proto)))
         (si-methods
          (compute-applicable-methods #'shared-initialize (list proto t)))
         (setf-svuc-slots
          (loop for slot in (class-slots class)
                when (cdr (compute-applicable-methods
                           #'(setf slot-value-using-class)
                           (list nil class proto slot)))
                collect slot))
         (sbuc-slots
          (loop for slot in (class-slots class)
                when (cdr (compute-applicable-methods
                           #'slot-boundp-using-class
                           (list class proto slot)))
                collect slot)))
    ;; Cannot initialize these variables earlier because the generic
    ;; functions don't exist when PCL is built.
    (when (null *the-system-si-method*)
      (setq *the-system-si-method*
            (find-method #'shared-initialize
                         () (list *the-class-slot-object* *the-class-t*)))
      (setq *the-system-ii-method*
            (find-method #'initialize-instance
                         () (list *the-class-slot-object*))))
    ;; Note that when there are user-defined applicable methods on
    ;; MAKE-INSTANCE and/or ALLOCATE-INSTANCE, these will show up
    ;; together with the system-defined ones in what
    ;; COMPUTE-APPLICABLE-METHODS returns.
    (let ((maybe-invalid-initargs
           (check-initargs-1
            class
            (append
             (ctor-default-initkeys
              (ctor-initargs ctor) (class-default-initargs class))
             (plist-keys (ctor-initargs ctor)))
            (append ii-methods si-methods) nil nil))
          (custom-make-instance
           (not (null (cdr make-instance-methods)))))
      (if (and (not (structure-class-p class))
               (not (condition-class-p class))
               (not custom-make-instance)
               (null (cdr allocate-instance-methods))
               (every (lambda (x)
                        (member (slot-definition-allocation x)
                                '(:instance :class)))
                      (class-slots class))
               (not maybe-invalid-initargs)
               (not (hairy-around-or-nonstandard-primary-method-p
                     ii-methods *the-system-ii-method*))
               (not (around-or-nonstandard-primary-method-p
                     si-methods *the-system-si-method*)))
          (optimizing-generator ctor ii-methods si-methods setf-svuc-slots sbuc-slots)
          (fallback-generator ctor ii-methods si-methods
                              (or maybe-invalid-initargs custom-make-instance))))))

(defun around-or-nonstandard-primary-method-p
    (methods &optional standard-method)
  (loop with primary-checked-p = nil
        for method in methods
        as qualifiers = (if (consp method)
                            (early-method-qualifiers method)
                            (safe-method-qualifiers method))
        when (or (eq :around (car qualifiers))
                 (and (null qualifiers)
                      (not primary-checked-p)
                      (not (null standard-method))
                      (not (eq standard-method method))))
          return t
        when (null qualifiers) do
          (setq primary-checked-p t)))

(defun hairy-around-or-nonstandard-primary-method-p
    (methods &optional standard-method)
  (loop with primary-checked-p = nil
        for method in methods
        as qualifiers = (if (consp method)
                            (early-method-qualifiers method)
                            (safe-method-qualifiers method))
        when (or (and (eq :around (car qualifiers))
                      (not (simple-next-method-call-p method)))
              (and (null qualifiers)
                      (not primary-checked-p)
                      (not (null standard-method))
                      (not (eq standard-method method))))
          return t
        when (null qualifiers) do
          (setq primary-checked-p t)))

(defun fallback-generator (ctor ii-methods si-methods use-make-instance)
  (declare (ignore ii-methods si-methods))
  (let ((class (ctor-class ctor))
        (lambda-list (make-ctor-parameter-list ctor))
        (initargs (ctor-initargs ctor)))
    (if use-make-instance
        `(lambda ,lambda-list
           (declare #.*optimize-speed*)
           ;; The CTOR MAKE-INSTANCE optimization checks for
           ;; *COMPILING-OPTIMIZED-CONSTRUCTOR* which is bound around
           ;; compilation of the constructor, hence avoiding the
           ;; possibility of endless recursion.
           (make-instance ,class ,@(quote-plist-keys initargs)))
        (let ((defaults (class-default-initargs class)))
          (when defaults
            (setf initargs (ctor-default-initargs initargs defaults)))
          `(lambda ,lambda-list
             (declare #.*optimize-speed*)
             (fast-make-instance ,class ,@(quote-plist-keys initargs)))))))

;;; Not as good as the real optimizing generator, but faster than going
;;; via MAKE-INSTANCE: 1 GF call less, and no need to check initargs.
(defun fast-make-instance (class &rest initargs)
  (declare #.*optimize-speed*)
  (declare (dynamic-extent initargs))
  (let ((.instance. (apply #'allocate-instance class initargs)))
    (apply #'initialize-instance .instance. initargs)
    .instance.))

(defun optimizing-generator
    (ctor ii-methods si-methods setf-svuc-slots sbuc-slots)
  (multiple-value-bind (locations names body early-unbound-markers-p)
      (fake-initialization-emf ctor ii-methods si-methods
                               setf-svuc-slots sbuc-slots)
    (let ((wrapper (class-wrapper (ctor-class ctor))))
      (values
       `(lambda ,(make-ctor-parameter-list ctor)
         (declare #.*optimize-speed*)
         (block nil
           (when (layout-invalid ,wrapper)
             (install-initial-constructor ,ctor)
             (return (funcall ,ctor ,@(make-ctor-parameter-list ctor))))
           ,(wrap-in-allocate-forms ctor body early-unbound-markers-p)))
       locations
       names
       t))))

;;; Return a form wrapped around BODY that allocates an instance constructed
;;; by CTOR. EARLY-UNBOUND-MARKERS-P means slots may be accessed before we
;;; have explicitly initialized them, requiring all slots to start as
;;; +SLOT-UNBOUND+. The resulting form binds the local variables .INSTANCE. to
;;; the instance, and .SLOTS. to the instance's slot vector around BODY.
(defun wrap-in-allocate-forms (ctor body early-unbound-markers-p)
  (let* ((class (ctor-class ctor))
         (wrapper (class-wrapper class))
         (allocation-function (raw-instance-allocator class))
         (slots-fetcher (slots-fetcher class)))
    (if (eq allocation-function 'allocate-standard-instance)
        `(let ((.instance. (%make-standard-instance nil 0))
               (.slots. (make-array
                         ,(layout-length wrapper)
                         ,@(when early-unbound-markers-p
                                 '(:initial-element +slot-unbound+)))))
           (setf (std-instance-wrapper .instance.) ,wrapper)
           (setf (std-instance-slots .instance.) .slots.)
           ,body
           .instance.)
        `(let* ((.instance. (,allocation-function ,wrapper))
                (.slots. (,slots-fetcher .instance.)))
           (declare (ignorable .slots.))
           ,body
           .instance.))))

;;; Return a form for invoking METHOD with arguments from ARGS.  As
;;; can be seen in METHOD-FUNCTION-FROM-FAST-FUNCTION, method
;;; functions look like (LAMBDA (ARGS NEXT-METHODS) ...).  We could
;;; call fast method functions directly here, but benchmarks show that
;;; there's no speed to gain, so lets avoid the hair here.
(defmacro invoke-method (method args &optional next-methods)
  `(funcall ,(the function (method-function method)) ,args ,next-methods))

;;; Return a form that is sort of an effective method comprising all
;;; calls to INITIALIZE-INSTANCE and SHARED-INITIALIZE that would
;;; normally have taken place when calling MAKE-INSTANCE.
(defun fake-initialization-emf
    (ctor ii-methods si-methods setf-svuc-slots sbuc-slots)
  (multiple-value-bind (ii-around ii-before ii-primary ii-after)
      (standard-sort-methods ii-methods)
    (declare (ignore ii-primary))
    (multiple-value-bind (si-around si-before si-primary si-after)
        (standard-sort-methods si-methods)
      (declare (ignore si-primary))
      (aver (null si-around))
      (let ((initargs (ctor-initargs ctor))
            ;; :BEFORE and :AROUND initialization methods, and SETF SVUC and
            ;; SBUC methods can cause slots to be accessed before the we have
            ;; touched them here, which requires the instance-vector to be
            ;; initialized with +SLOT-UNBOUND+ to start with.
            (early-unbound-markers-p (or ii-before si-before ii-around
                                         setf-svuc-slots sbuc-slots)))
        (multiple-value-bind
              (locations names bindings vars defaulting-initargs body)
            (slot-init-forms ctor
                             early-unbound-markers-p
                             setf-svuc-slots sbuc-slots)
        (values
         locations
         names
         `(let ,bindings
           (declare (ignorable ,@vars))
           (flet ((initialize-it (.ii-args. .next-methods.)
                    ;; This has all the :BEFORE and :AFTER methods,
                    ;; and BODY does what primary SI method would do.
                    (declare (ignore .next-methods.))
                    (let* ((.instance. (car .ii-args.))
                           ,@(when (or si-before si-after)
                                  `((.si-args.
                                     (list* .instance. t (cdr .ii-args.))))))
                      ,@(loop for method in ii-before
                              collect `(invoke-method ,method .ii-args.))
                      ,@(loop for method in si-before
                              collect `(invoke-method ,method .si-args.))
                      ,@body
                      ,@(loop for method in si-after
                              collect `(invoke-method ,method .si-args.))
                      ,@(loop for method in ii-after
                              collect `(invoke-method ,method .ii-args.))
                      .instance.)))
             (declare (dynamic-extent #'initialize-it))
             (let ((.ii-args.
                    ,@(if (or ii-before ii-after ii-around si-before si-after)
                          `((list .instance. ,@(quote-plist-keys initargs)
                                  ,@defaulting-initargs))
                          `((list .instance.)))))
               ,(if ii-around
                    ;; If there are :AROUND methods, call them first -- they get
                    ;; the normal chaining, with #'INITIALIZE-IT standing in for
                    ;; the rest.
                    `(let ((.next-methods.
                            (list ,@(cdr ii-around) #'initialize-it)))
                       (declare (dynamic-extent .next-methods.))
                       (invoke-method ,(car ii-around) .ii-args. .next-methods.))
                    ;; The simple case.
                    `(initialize-it .ii-args. nil)))))
         early-unbound-markers-p))))))

;;; Return four values from APPLICABLE-METHODS: around methods, before
;;; methods, the applicable primary method, and applicable after
;;; methods.  Before and after methods are sorted in the order they
;;; must be called.
(defun standard-sort-methods (applicable-methods)
  (loop for method in applicable-methods
        as qualifiers = (if (consp method)
                            (early-method-qualifiers method)
                            (safe-method-qualifiers method))
        if (null qualifiers)
          collect method into primary
        else if (eq :around (car qualifiers))
          collect method into around
        else if (eq :after (car qualifiers))
          collect method into after
        else if (eq :before (car qualifiers))
          collect method into before
        finally
          (return (values around before (first primary) (reverse after)))))

(defmacro with-type-checked ((type safe-p) &body body)
  (if safe-p
      ;; To handle FUNCTION types reasonable, we use SAFETY 3 and
      ;; THE instead of e.g. CHECK-TYPE.
      `(locally
           (declare (optimize (safety 3)))
         (the ,type (progn ,@body)))
      `(progn ,@body)))

;;; Return as multiple values bindings for default initialization arguments,
;;; variable names, defaulting initargs and a body for initializing instance
;;; and class slots of an object costructed by CTOR. The variable .SLOTS. is
;;; assumed to bound to the instance's slot vector. EARLY-UNBOUND-MARKERS-P
;;; means other code will initialize instance slots to +SLOT-UNBOUND+, and we
;;; have to check if something has already set slots before we initialize
;;; them.
(defun slot-init-forms (ctor early-unbound-markers-p setf-svuc-slots sbuc-slots)
  (let* ((class (ctor-class ctor))
         (initargs (ctor-initargs ctor))
         (initkeys (plist-keys initargs))
         (safe-p (ctor-safe-p ctor))
         (wrapper (class-wrapper class))
         (slot-vector
          (make-array (layout-length wrapper) :initial-element nil))
         (class-inits ())
         (default-inits ())
         (defaulting-initargs ())
         (default-initargs (class-default-initargs class))
         (initarg-locations
          (compute-initarg-locations
           class (append initkeys (mapcar #'car default-initargs)))))
    (labels ((initarg-locations (initarg)
               (cdr (assoc initarg initarg-locations :test #'eq)))
             (initializedp (location)
               (cond
                 ((consp location)
                  (assoc location class-inits :test #'eq))
                 ((integerp location)
                  (not (null (aref slot-vector location))))
                 (t (bug "Weird location in ~S" 'slot-init-forms))))
             (class-init (location kind val type slotd)
               (aver (consp location))
               (unless (initializedp location)
                 (push (list location kind val type slotd) class-inits)))
             (instance-init (location kind val type slotd)
               (aver (integerp location))
               (unless (initializedp location)
                 (setf (aref slot-vector location)
                       (list kind val type slotd))))
             (default-init-var-name (i)
               (format-symbol *pcl-package* ".D~D." i))
             (location-var-name (i)
               (format-symbol *pcl-package* ".L~D." i)))
      ;; Loop over supplied initargs and values and record which
      ;; instance and class slots they initialize.
      (loop for (key value) on initargs by #'cddr
            as kind = (if (constantp value) 'constant 'param)
            as locations = (initarg-locations key)
            do (loop for (location type slotd) in locations
                     do (if (consp location)
                            (class-init location kind value type slotd)
                            (instance-init location kind value type slotd))))
      ;; Loop over default initargs of the class, recording
      ;; initializations of slots that have not been initialized
      ;; above.  Default initargs which are not in the supplied
      ;; initargs are treated as if they were appended to supplied
      ;; initargs, that is, their values must be evaluated even
      ;; if not actually used for initializing a slot.
      (loop for (key initform initfn) in default-initargs and i from 0
            unless (member key initkeys :test #'eq)
            do (let* ((kind (if (constantp initform) 'constant 'var))
                      (init (if (eq kind 'var) initfn initform)))
                 (ecase kind
                   (constant
                    (push (list 'quote key) defaulting-initargs)
                    (push initform defaulting-initargs))
                   (var
                    (push (list 'quote key) defaulting-initargs)
                    (push (default-init-var-name i) defaulting-initargs)))
              (when (eq kind 'var)
                (let ((init-var (default-init-var-name i)))
                  (setq init init-var)
                  (push (cons init-var initfn) default-inits)))
              (loop for (location type slotd) in (initarg-locations key)
                    do (if (consp location)
                           (class-init location kind init type slotd)
                           (instance-init location kind init type slotd)))))
      ;; Loop over all slots of the class, filling in the rest from
      ;; slot initforms.
      (loop for slotd in (class-slots class)
            as location = (slot-definition-location slotd)
            as type = (slot-definition-type slotd)
            as allocation = (slot-definition-allocation slotd)
            as initfn = (slot-definition-initfunction slotd)
            as initform = (slot-definition-initform slotd) do
              (unless (or (eq allocation :class)
                          (null initfn)
                          (initializedp location))
                (if (constantp initform)
                    (instance-init location 'initform initform type slotd)
                    (instance-init location
                                   'initform/initfn initfn type slotd))))
      ;; Generate the forms for initializing instance and class slots.
      (let ((instance-init-forms
             (loop for slot-entry across slot-vector and i from 0
                   as (kind value type slotd) = slot-entry
                   collect
                      (flet ((setf-form (value-form)
                               (if (member slotd setf-svuc-slots :test #'eq)
                                   `(setf (slot-value-using-class
                                           ,class .instance. ,slotd)
                                          ,value-form)
                                   `(setf (clos-slots-ref .slots. ,i)
                                          (with-type-checked (,type ,safe-p)
                                            ,value-form))))
                             (not-boundp-form ()
                               (if (member slotd sbuc-slots :test #'eq)
                                   `(not (slot-boundp-using-class
                                          ,class .instance. ,slotd))
                                   `(eq (clos-slots-ref .slots. ,i)
                                        +slot-unbound+))))
                        (ecase kind
                          ((nil)
                           (unless early-unbound-markers-p
                             `(setf (clos-slots-ref .slots. ,i)
                                    +slot-unbound+)))
                          ((param var)
                           (setf-form value))
                          (initfn
                           (setf-form `(funcall ,value)))
                          (initform/initfn
                           (if early-unbound-markers-p
                               `(when ,(not-boundp-form)
                                  ,(setf-form `(funcall ,value)))
                               (setf-form `(funcall ,value))))
                          (initform
                           (if early-unbound-markers-p
                               `(when ,(not-boundp-form)
                                  ,(setf-form `',(constant-form-value value)))
                               (setf-form `',(constant-form-value value))))
                          (constant
                           (setf-form `',(constant-form-value value))))))))
        ;; we are not allowed to modify QUOTEd locations, so we can't
        ;; generate code like (setf (cdr ',location) arg).  Instead,
        ;; we have to do (setf (cdr .L0.) arg) and arrange for .L0. to
        ;; be bound to the location.
        (multiple-value-bind (names locations class-init-forms)
            (loop with names
                  with locations
                  with i = -1
                  for (location kind value type slotd) in class-inits
                  for init-form
                     = (case kind
                         (constant `',(constant-form-value value))
                         ((param var) `,value)
                         (initfn `(funcall ,value)))
                  when (member slotd setf-svuc-slots :test #'eq)
                  collect `(setf (slot-value-using-class
                                  ,class .instance. ,slotd)
                                 ,init-form)
                  into class-init-forms
                  else collect
                     (let ((name (location-var-name (incf i))))
                       (push name names)
                       (push location locations)
                       `(setf (cdr ,name)
                              (with-type-checked (,type ,safe-p)
                                ,init-form)))
                  into class-init-forms
                  finally (return (values (nreverse names)
                                          (nreverse locations)
                                          class-init-forms)))
          (multiple-value-bind (vars bindings)
              (loop for (var . initfn) in (nreverse default-inits)
                    collect var into vars
                    collect `(,var (funcall ,initfn)) into bindings
                    finally (return (values vars bindings)))
            (values locations names
                    bindings vars
                    (nreverse defaulting-initargs)
                    `(,@(delete nil instance-init-forms)
                      ,@class-init-forms))))))))

;;; Return an alist of lists (KEY (LOCATION . TYPE-SPECIFIER) ...)
;;; telling, for each key in INITKEYS, which locations the initarg
;;; initializes and the associated type with the location.  CLASS is
;;; the class of the instance being initialized.
(defun compute-initarg-locations (class initkeys)
  (loop with slots = (class-slots class)
        for key in initkeys collect
          (loop for slot in slots
                if (memq key (slot-definition-initargs slot))
                  collect (list (slot-definition-location slot)
                                (slot-definition-type slot)
                                slot)
                          into locations
                else
                  collect slot into remaining-slots
                finally
                  (setq slots remaining-slots)
                  (return (cons key locations)))))


;;; *******************************
;;; External Entry Points  ********
;;; *******************************

(defun update-ctors (reason &key class name generic-function method)
  (labels ((reset (class &optional initarg-caches-p (ctorsp t))
             (when ctorsp
               (dolist (ctor (plist-value class 'ctors))
                 (install-initial-constructor ctor)))
             (when initarg-caches-p
               (dolist (cache '(mi-initargs ri-initargs))
                 (setf (plist-value class cache) ())))
             (dolist (subclass (class-direct-subclasses class))
               (reset subclass initarg-caches-p ctorsp))))
    (ecase reason
      ;; CLASS must have been specified.
      (finalize-inheritance
       (reset class t))
      ;; NAME must have been specified.
      (setf-find-class
       (loop for ctor in *all-ctors*
             when (eq (ctor-class-or-name ctor) name) do
             (when (ctor-class ctor)
               (reset (ctor-class ctor)))
             (loop-finish)))
      ;; GENERIC-FUNCTION and METHOD must have been specified.
      ((add-method remove-method)
       (flet ((class-of-1st-method-param (method)
                (type-class (first (method-specializers method)))))
         (case (generic-function-name generic-function)
           ((make-instance allocate-instance)
            ;; FIXME: I can't see a way of working out which classes a
            ;; given metaclass specializer are applicable to short of
            ;; iterating and testing with class-of.  It would be good
            ;; to not invalidate caches of system classes at this
            ;; point (where it is not legal to define a method
            ;; applicable to them on system functions).  -- CSR,
            ;; 2010-07-13
            (reset (find-class 'standard-object) t t))
           ((initialize-instance shared-initialize)
            (reset (class-of-1st-method-param method) t t))
           ((reinitialize-instance)
            (reset (class-of-1st-method-param method) t nil))
           (t (when (or (eq (generic-function-name generic-function)
                            'slot-boundp-using-class)
                        (equal (generic-function-name generic-function)
                               '(setf slot-value-using-class)))
                ;; this looks awfully expensive, but given that one
                ;; can specialize on the SLOTD argument, nothing is
                ;; safe.  -- CSR, 2004-07-12
                (reset (find-class 'standard-object))))))))))

(defun precompile-ctors ()
  (dolist (ctor *all-ctors*)
    (when (null (ctor-class ctor))
      (let ((class (find-class (ctor-class-or-name ctor) nil)))
        (when (and class (class-finalized-p class))
          (install-optimized-constructor ctor))))))

(defun maybe-call-ctor (class initargs)
  (flet ((frob-initargs (ctor)
           (do ((ctail (ctor-initargs ctor))
                (itail initargs)
                (args nil))
               ((or (null ctail) (null itail))
                (values (nreverse args) (and (null ctail) (null itail))))
             (unless (eq (pop ctail) (pop itail))
               (return nil))
             (let ((cval (pop ctail))
                   (ival (pop itail)))
               (if (constantp cval)
                   (unless (eql cval ival)
                     (return nil))
                   (push ival args))))))
    (dolist (ctor (plist-value class 'ctors))
      (when (eq (ctor-state ctor) 'optimized)
        (multiple-value-bind (ctor-args matchp)
            (frob-initargs ctor)
          (when matchp
            (return (apply ctor ctor-args))))))))

;;; FIXME: CHECK-FOO-INITARGS share most of their bodies.
(defun check-mi-initargs (class initargs)
  (let* ((class-proto (class-prototype class))
         (keys (plist-keys initargs))
         (cache (plist-value class 'mi-initargs))
         (cached (assoc keys cache :test #'equal))
         (invalid-keys
          (if (consp cached)
              (cdr cached)
              (let ((invalid
                     (check-initargs-1
                      class initargs
                      (list (list* 'allocate-instance class initargs)
                            (list* 'initialize-instance class-proto initargs)
                            (list* 'shared-initialize class-proto t initargs))
                      t nil)))
                (setf (plist-value class 'mi-initargs)
                      (acons keys invalid cache))
                invalid))))
    (when invalid-keys
      ;; FIXME: should have an operation here, and maybe a set of
      ;; valid keys.
      (error 'initarg-error :class class :initargs invalid-keys))))

(defun check-ri-initargs (instance initargs)
  (let* ((class (class-of instance))
         (keys (plist-keys initargs))
         (cache (plist-value class 'ri-initargs))
         (cached (assoc keys cache :test #'equal))
         (invalid-keys
          (if (consp cached)
              (cdr cached)
              (let ((invalid
                     ;; FIXME: give CHECK-INITARGS-1 and friends a
                     ;; more mnemonic name and (possibly) a nicer,
                     ;; more orthogonal interface.
                     (check-initargs-1
                      class initargs
                      (list (list* 'reinitialize-instance instance initargs)
                            (list* 'shared-initialize instance nil initargs))
                      t nil)))
                (setf (plist-value class 'ri-initargs)
                      (acons keys invalid cache))
                invalid))))
    (when invalid-keys
      (error 'initarg-error :class class :initargs invalid-keys))))

;;; end of ctor.lisp
