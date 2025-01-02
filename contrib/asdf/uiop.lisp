;;; This is UIOP 3.3.1
;;;; ---------------------------------------------------------------------------
;;;; Handle ASDF package upgrade, including implementation-dependent magic.
;;
;; See https://bugs.launchpad.net/asdf/+bug/485687
;;

(defpackage :uiop/package
  ;; CAUTION: we must handle the first few packages specially for hot-upgrade.
  ;; This package definition MUST NOT change unless its name too changes;
  ;; if/when it changes, don't forget to add new functions missing from below.
  ;; Until then, uiop/package is frozen to forever
  ;; import and export the same exact symbols as for ASDF 2.27.
  ;; Any other symbol must be import-from'ed and re-export'ed in a different package.
  (:use :common-lisp)
  (:export
   #:find-package* #:find-symbol* #:symbol-call
   #:intern* #:export* #:import* #:shadowing-import* #:shadow* #:make-symbol* #:unintern*
   #:symbol-shadowing-p #:home-package-p
   #:symbol-package-name #:standard-common-lisp-symbol-p
   #:reify-package #:unreify-package #:reify-symbol #:unreify-symbol
   #:nuke-symbol-in-package #:nuke-symbol #:rehome-symbol
   #:ensure-package-unused #:delete-package*
   #:package-names #:packages-from-names #:fresh-package-name #:rename-package-away
   #:package-definition-form #:parse-define-package-form
   #:ensure-package #:define-package))

(in-package :uiop/package)

;;;; General purpose package utilities

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun find-package* (package-designator &optional (error t))
    (let ((package (find-package package-designator)))
      (cond
        (package package)
        (error (error "No package named ~S" (string package-designator)))
        (t nil))))
  (defun find-symbol* (name package-designator &optional (error t))
    "Find a symbol in a package of given string'ified NAME;
unlike CL:FIND-SYMBOL, work well with 'modern' case sensitive syntax
by letting you supply a symbol or keyword for the name;
also works well when the package is not present.
If optional ERROR argument is NIL, return NIL instead of an error
when the symbol is not found."
    (block nil
      (let ((package (find-package* package-designator error)))
        (when package ;; package error handled by find-package* already
          (multiple-value-bind (symbol status) (find-symbol (string name) package)
            (cond
              (status (return (values symbol status)))
              (error (error "There is no symbol ~S in package ~S" name (package-name package))))))
        (values nil nil))))
  (defun symbol-call (package name &rest args)
    "Call a function associated with symbol of given name in given package,
with given ARGS. Useful when the call is read before the package is loaded,
or when loading the package is optional."
    (apply (find-symbol* name package) args))
  (defun intern* (name package-designator &optional (error t))
    (intern (string name) (find-package* package-designator error)))
  (defun export* (name package-designator)
    (let* ((package (find-package* package-designator))
           (symbol (intern* name package)))
      (export (or symbol (list symbol)) package)))
  (defun import* (symbol package-designator)
    (import (or symbol (list symbol)) (find-package* package-designator)))
  (defun shadowing-import* (symbol package-designator)
    (shadowing-import (or symbol (list symbol)) (find-package* package-designator)))
  (defun shadow* (name package-designator)
    (shadow (list (string name)) (find-package* package-designator)))
  (defun make-symbol* (name)
    (etypecase name
      (string (make-symbol name))
      (symbol (copy-symbol name))))
  (defun unintern* (name package-designator &optional (error t))
    (block nil
      (let ((package (find-package* package-designator error)))
        (when package
          (multiple-value-bind (symbol status) (find-symbol* name package error)
            (cond
              (status (unintern symbol package)
                      (return (values symbol status)))
              (error (error "symbol ~A not present in package ~A"
                            (string symbol) (package-name package))))))
        (values nil nil))))
  (defun symbol-shadowing-p (symbol package)
    (and (member symbol (package-shadowing-symbols package)) t))
  (defun home-package-p (symbol package)
    (and package (let ((sp (symbol-package symbol)))
                   (and sp (let ((pp (find-package* package)))
                             (and pp (eq sp pp))))))))


(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun symbol-package-name (symbol)
    (let ((package (symbol-package symbol)))
      (and package (package-name package))))
  (defun standard-common-lisp-symbol-p (symbol)
    (multiple-value-bind (sym status) (find-symbol* symbol :common-lisp nil)
      (and (eq sym symbol) (eq status :external))))
  (defun reify-package (package &optional package-context)
    (if (eq package package-context) t
        (etypecase package
          (null nil)
          ((eql (find-package :cl)) :cl)
          (package (package-name package)))))
  (defun unreify-package (package &optional package-context)
    (etypecase package
      (null nil)
      ((eql t) package-context)
      ((or symbol string) (find-package package))))
  (defun reify-symbol (symbol &optional package-context)
    (etypecase symbol
      ((or keyword (satisfies standard-common-lisp-symbol-p)) symbol)
      (symbol (vector (symbol-name symbol)
                      (reify-package (symbol-package symbol) package-context)))))
  (defun unreify-symbol (symbol &optional package-context)
    (etypecase symbol
      (symbol symbol)
      ((simple-vector 2)
       (let* ((symbol-name (svref symbol 0))
              (package-foo (svref symbol 1))
              (package (unreify-package package-foo package-context)))
         (if package (intern* symbol-name package)
             (make-symbol* symbol-name)))))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defvar *all-package-happiness* '())
  (defvar *all-package-fishiness* (list t))
  (defun record-fishy (info)
    ;;(format t "~&FISHY: ~S~%" info)
    (push info *all-package-fishiness*))
  (defmacro when-package-fishiness (&body body)
    `(when *all-package-fishiness* ,@body))
  (defmacro note-package-fishiness (&rest info)
    `(when-package-fishiness (record-fishy (list ,@info)))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  #+(or clisp clozure)
  (defun get-setf-function-symbol (symbol)
    #+clisp (let ((sym (get symbol 'system::setf-function)))
              (if sym (values sym :setf-function)
                  (let ((sym (get symbol 'system::setf-expander)))
                    (if sym (values sym :setf-expander)
                        (values nil nil)))))
    #+clozure (gethash symbol ccl::%setf-function-names%))
  #+(or clisp clozure)
  (defun set-setf-function-symbol (new-setf-symbol symbol &optional kind)
    #+clisp (assert (member kind '(:setf-function :setf-expander)))
    #+clozure (assert (eq kind t))
    #+clisp
    (cond
      ((null new-setf-symbol)
       (remprop symbol 'system::setf-function)
       (remprop symbol 'system::setf-expander))
      ((eq kind :setf-function)
       (setf (get symbol 'system::setf-function) new-setf-symbol))
      ((eq kind :setf-expander)
       (setf (get symbol 'system::setf-expander) new-setf-symbol))
      (t (error "invalid kind of setf-function ~S for ~S to be set to ~S"
                kind symbol new-setf-symbol)))
    #+clozure
    (progn
      (gethash symbol ccl::%setf-function-names%) new-setf-symbol
      (gethash new-setf-symbol ccl::%setf-function-name-inverses%) symbol))
  #+(or clisp clozure)
  (defun create-setf-function-symbol (symbol)
    #+clisp (system::setf-symbol symbol)
    #+clozure (ccl::construct-setf-function-name symbol))
  (defun set-dummy-symbol (symbol reason other-symbol)
    (setf (get symbol 'dummy-symbol) (cons reason other-symbol)))
  (defun make-dummy-symbol (symbol)
    (let ((dummy (copy-symbol symbol)))
      (set-dummy-symbol dummy 'replacing symbol)
      (set-dummy-symbol symbol 'replaced-by dummy)
      dummy))
  (defun dummy-symbol (symbol)
    (get symbol 'dummy-symbol))
  (defun get-dummy-symbol (symbol)
    (let ((existing (dummy-symbol symbol)))
      (if existing (values (cdr existing) (car existing))
          (make-dummy-symbol symbol))))
  (defun nuke-symbol-in-package (symbol package-designator)
    (let ((package (find-package* package-designator))
          (name (symbol-name symbol)))
      (multiple-value-bind (sym stat) (find-symbol name package)
        (when (and (member stat '(:internal :external)) (eq symbol sym))
          (if (symbol-shadowing-p symbol package)
              (shadowing-import* (get-dummy-symbol symbol) package)
              (unintern* symbol package))))))
  (defun nuke-symbol (symbol &optional (packages (list-all-packages)))
    #+(or clisp clozure)
    (multiple-value-bind (setf-symbol kind)
        (get-setf-function-symbol symbol)
      (when kind (nuke-symbol setf-symbol)))
    (loop :for p :in packages :do (nuke-symbol-in-package symbol p)))
  (defun rehome-symbol (symbol package-designator)
    "Changes the home package of a symbol, also leaving it present in its old home if any"
    (let* ((name (symbol-name symbol))
           (package (find-package* package-designator))
           (old-package (symbol-package symbol))
           (old-status (and old-package (nth-value 1 (find-symbol name old-package))))
           (shadowing (and old-package (symbol-shadowing-p symbol old-package) (make-symbol name))))
      (multiple-value-bind (overwritten-symbol overwritten-symbol-status) (find-symbol name package)
        (unless (eq package old-package)
          (let ((overwritten-symbol-shadowing-p
                  (and overwritten-symbol-status
                       (symbol-shadowing-p overwritten-symbol package))))
            (note-package-fishiness
             :rehome-symbol name
             (when old-package (package-name old-package)) old-status (and shadowing t)
             (package-name package) overwritten-symbol-status overwritten-symbol-shadowing-p)
            (when old-package
              (if shadowing
                  (shadowing-import* shadowing old-package))
              (unintern* symbol old-package))
            (cond
              (overwritten-symbol-shadowing-p
               (shadowing-import* symbol package))
              (t
               (when overwritten-symbol-status
                 (unintern* overwritten-symbol package))
               (import* symbol package)))
            (if shadowing
                (shadowing-import* symbol old-package)
                (import* symbol old-package))
            #+(or clisp clozure)
            (multiple-value-bind (setf-symbol kind)
                (get-setf-function-symbol symbol)
              (when kind
                (let* ((setf-function (fdefinition setf-symbol))
                       (new-setf-symbol (create-setf-function-symbol symbol)))
                  (note-package-fishiness
                   :setf-function
                   name (package-name package)
                   (symbol-name setf-symbol) (symbol-package-name setf-symbol)
                   (symbol-name new-setf-symbol) (symbol-package-name new-setf-symbol))
                  (when (symbol-package setf-symbol)
                    (unintern* setf-symbol (symbol-package setf-symbol)))
                  (setf (fdefinition new-setf-symbol) setf-function)
                  (set-setf-function-symbol new-setf-symbol symbol kind))))
            #+(or clisp clozure)
            (multiple-value-bind (overwritten-setf foundp)
                (get-setf-function-symbol overwritten-symbol)
              (when foundp
                (unintern overwritten-setf)))
            (when (eq old-status :external)
              (export* symbol old-package))
            (when (eq overwritten-symbol-status :external)
              (export* symbol package))))
        (values overwritten-symbol overwritten-symbol-status))))
  (defun ensure-package-unused (package)
    (loop :for p :in (package-used-by-list package) :do
      (unuse-package package p)))
  (defun delete-package* (package &key nuke)
    (let ((p (find-package package)))
      (when p
        (when nuke (do-symbols (s p) (when (home-package-p s p) (nuke-symbol s))))
        (ensure-package-unused p)
        (delete-package package))))
  (defun package-names (package)
    (cons (package-name package) (package-nicknames package)))
  (defun packages-from-names (names)
    (remove-duplicates (remove nil (mapcar #'find-package names)) :from-end t))
  (defun fresh-package-name (&key (prefix :%TO-BE-DELETED)
                               separator
                               (index (random most-positive-fixnum)))
    (loop :for i :from index
          :for n = (format nil "~A~@[~A~D~]" prefix (and (plusp i) (or separator "")) i)
          :thereis (and (not (find-package n)) n)))
  (defun rename-package-away (p &rest keys &key prefix &allow-other-keys)
    (let ((new-name
            (apply 'fresh-package-name
                   :prefix (or prefix (format nil "__~A__" (package-name p))) keys)))
      (record-fishy (list :rename-away (package-names p) new-name))
      (rename-package p new-name))))


;;; Communicable representation of symbol and package information

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun package-definition-form (package-designator
                                  &key (nicknamesp t) (usep t)
                                    (shadowp t) (shadowing-import-p t)
                                    (exportp t) (importp t) internp (error t))
    (let* ((package (or (find-package* package-designator error)
                        (return-from package-definition-form nil)))
           (name (package-name package))
           (nicknames (package-nicknames package))
           (use (mapcar #'package-name (package-use-list package)))
           (shadow ())
           (shadowing-import (make-hash-table :test 'equal))
           (import (make-hash-table :test 'equal))
           (export ())
           (intern ()))
      (when package
        (loop :for sym :being :the :symbols :in package
              :for status = (nth-value 1 (find-symbol* sym package)) :do
                (ecase status
                  ((nil :inherited))
                  ((:internal :external)
                   (let* ((name (symbol-name sym))
                          (external (eq status :external))
                          (home (symbol-package sym))
                          (home-name (package-name home))
                          (imported (not (eq home package)))
                          (shadowing (symbol-shadowing-p sym package)))
                     (cond
                       ((and shadowing imported)
                        (push name (gethash home-name shadowing-import)))
                       (shadowing
                        (push name shadow))
                       (imported
                        (push name (gethash home-name import))))
                     (cond
                       (external
                        (push name export))
                       (imported)
                       (t (push name intern)))))))
        (labels ((sort-names (names)
                   (sort (copy-list names) #'string<))
                 (table-keys (table)
                   (loop :for k :being :the :hash-keys :of table :collect k))
                 (when-relevant (key value)
                   (when value (list (cons key value))))
                 (import-options (key table)
                   (loop :for i :in (sort-names (table-keys table))
                         :collect `(,key ,i ,@(sort-names (gethash i table))))))
          `(defpackage ,name
             ,@(when-relevant :nicknames (and nicknamesp (sort-names nicknames)))
             (:use ,@(and usep (sort-names use)))
             ,@(when-relevant :shadow (and shadowp (sort-names shadow)))
             ,@(import-options :shadowing-import-from (and shadowing-import-p shadowing-import))
             ,@(import-options :import-from (and importp import))
             ,@(when-relevant :export (and exportp (sort-names export)))
             ,@(when-relevant :intern (and internp (sort-names intern)))))))))


;;; ensure-package, define-package
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun ensure-shadowing-import (name to-package from-package shadowed imported)
    (check-type name string)
    (check-type to-package package)
    (check-type from-package package)
    (check-type shadowed hash-table)
    (check-type imported hash-table)
    (let ((import-me (find-symbol* name from-package)))
      (multiple-value-bind (existing status) (find-symbol name to-package)
        (cond
          ((gethash name shadowed)
           (unless (eq import-me existing)
             (error "Conflicting shadowings for ~A" name)))
          (t
           (setf (gethash name shadowed) t)
           (setf (gethash name imported) t)
           (unless (or (null status)
                       (and (member status '(:internal :external))
                            (eq existing import-me)
                            (symbol-shadowing-p existing to-package)))
             (note-package-fishiness
              :shadowing-import name
              (package-name from-package)
              (or (home-package-p import-me from-package) (symbol-package-name import-me))
              (package-name to-package) status
              (and status (or (home-package-p existing to-package) (symbol-package-name existing)))))
           (shadowing-import* import-me to-package))))))
  (defun ensure-imported (import-me into-package &optional from-package)
    (check-type import-me symbol)
    (check-type into-package package)
    (check-type from-package (or null package))
    (let ((name (symbol-name import-me)))
      (multiple-value-bind (existing status) (find-symbol name into-package)
        (cond
          ((not status)
           (import* import-me into-package))
          ((eq import-me existing))
          (t
           (let ((shadowing-p (symbol-shadowing-p existing into-package)))
             (note-package-fishiness
              :ensure-imported name
              (and from-package (package-name from-package))
              (or (home-package-p import-me from-package) (symbol-package-name import-me))
              (package-name into-package)
              status
              (and status (or (home-package-p existing into-package) (symbol-package-name existing)))
              shadowing-p)
             (cond
               ((or shadowing-p (eq status :inherited))
                (shadowing-import* import-me into-package))
               (t
                (unintern* existing into-package)
                (import* import-me into-package))))))))
    (values))
  (defun ensure-import (name to-package from-package shadowed imported)
    (check-type name string)
    (check-type to-package package)
    (check-type from-package package)
    (check-type shadowed hash-table)
    (check-type imported hash-table)
    (multiple-value-bind (import-me import-status) (find-symbol name from-package)
      (when (null import-status)
        (note-package-fishiness
         :import-uninterned name (package-name from-package) (package-name to-package))
        (setf import-me (intern* name from-package)))
      (multiple-value-bind (existing status) (find-symbol name to-package)
        (cond
          ((and imported (gethash name imported))
           (unless (and status (eq import-me existing))
             (error "Can't import ~S from both ~S and ~S"
                    name (package-name (symbol-package existing)) (package-name from-package))))
          ((gethash name shadowed)
           (error "Can't both shadow ~S and import it from ~S" name (package-name from-package)))
          (t
           (setf (gethash name imported) t))))
      (ensure-imported import-me to-package from-package)))
  (defun ensure-inherited (name symbol to-package from-package mixp shadowed imported inherited)
    (check-type name string)
    (check-type symbol symbol)
    (check-type to-package package)
    (check-type from-package package)
    (check-type mixp (member nil t)) ; no cl:boolean on Genera
    (check-type shadowed hash-table)
    (check-type imported hash-table)
    (check-type inherited hash-table)
    (multiple-value-bind (existing status) (find-symbol name to-package)
      (let* ((sp (symbol-package symbol))
             (in (gethash name inherited))
             (xp (and status (symbol-package existing))))
        (when (null sp)
          (note-package-fishiness
           :import-uninterned name
           (package-name from-package) (package-name to-package) mixp)
          (import* symbol from-package)
          (setf sp (package-name from-package)))
        (cond
          ((gethash name shadowed))
          (in
           (unless (equal sp (first in))
             (if mixp
                 (ensure-shadowing-import name to-package (second in) shadowed imported)
                 (error "Can't inherit ~S from ~S, it is inherited from ~S"
                        name (package-name sp) (package-name (first in))))))
          ((gethash name imported)
           (unless (eq symbol existing)
             (error "Can't inherit ~S from ~S, it is imported from ~S"
                    name (package-name sp) (package-name xp))))
          (t
           (setf (gethash name inherited) (list sp from-package))
           (when (and status (not (eq sp xp)))
             (let ((shadowing (symbol-shadowing-p existing to-package)))
               (note-package-fishiness
                :inherited name
                (package-name from-package)
                (or (home-package-p symbol from-package) (symbol-package-name symbol))
                (package-name to-package)
                (or (home-package-p existing to-package) (symbol-package-name existing)))
               (if shadowing (ensure-shadowing-import name to-package from-package shadowed imported)
                   (unintern* existing to-package)))))))))
  (defun ensure-mix (name symbol to-package from-package shadowed imported inherited)
    (check-type name string)
    (check-type symbol symbol)
    (check-type to-package package)
    (check-type from-package package)
    (check-type shadowed hash-table)
    (check-type imported hash-table)
    (check-type inherited hash-table)
    (unless (gethash name shadowed)
      (multiple-value-bind (existing status) (find-symbol name to-package)
        (let* ((sp (symbol-package symbol))
               (im (gethash name imported))
               (in (gethash name inherited)))
          (cond
            ((or (null status)
                 (and status (eq symbol existing))
                 (and in (eq sp (first in))))
             (ensure-inherited name symbol to-package from-package t shadowed imported inherited))
            (in
             (remhash name inherited)
             (ensure-shadowing-import name to-package (second in) shadowed imported))
            (im
             (error "Symbol ~S import from ~S~:[~; actually ~:[uninterned~;~:*from ~S~]~] conflicts with existing symbol in ~S~:[~; actually ~:[uninterned~;from ~:*~S~]~]"
                    name (package-name from-package)
                    (home-package-p symbol from-package) (symbol-package-name symbol)
                    (package-name to-package)
                    (home-package-p existing to-package) (symbol-package-name existing)))
            (t
             (ensure-inherited name symbol to-package from-package t shadowed imported inherited)))))))

  (defun recycle-symbol (name recycle exported)
    ;; Takes a symbol NAME (a string), a list of package designators for RECYCLE
    ;; packages, and a hash-table of names (strings) of symbols scheduled to be
    ;; EXPORTED from the package being defined. It returns two values, the
    ;; symbol found (if any, or else NIL), and a boolean flag indicating whether
    ;; a symbol was found. The caller (DEFINE-PACKAGE) will then do the
    ;; re-homing of the symbol, etc.
    (check-type name string)
    (check-type recycle list)
    (check-type exported hash-table)
    (when (gethash name exported) ;; don't bother recycling private symbols
      (let (recycled foundp)
        (dolist (r recycle (values recycled foundp))
          (multiple-value-bind (symbol status) (find-symbol name r)
            (when (and status (home-package-p symbol r))
              (cond
                (foundp
                 ;; (nuke-symbol symbol)) -- even simple variable names like O or C will do that.
                 (note-package-fishiness :recycled-duplicate name (package-name foundp) (package-name r)))
                (t
                 (setf recycled symbol foundp r)))))))))
  (defun symbol-recycled-p (sym recycle)
    (check-type sym symbol)
    (check-type recycle list)
    (and (member (symbol-package sym) recycle) t))
  (defun ensure-symbol (name package intern recycle shadowed imported inherited exported)
    (check-type name string)
    (check-type package package)
    (check-type intern (member nil t)) ; no cl:boolean on Genera
    (check-type shadowed hash-table)
    (check-type imported hash-table)
    (check-type inherited hash-table)
    (unless (or (gethash name shadowed)
                (gethash name imported)
                (gethash name inherited))
      (multiple-value-bind (existing status)
          (find-symbol name package)
        (multiple-value-bind (recycled previous) (recycle-symbol name recycle exported)
          (cond
            ((and status (eq existing recycled) (eq previous package)))
            (previous
             (rehome-symbol recycled package))
            ((and status (eq package (symbol-package existing))))
            (t
             (when status
               (note-package-fishiness
                :ensure-symbol name
                (reify-package (symbol-package existing) package)
                status intern)
               (unintern existing))
             (when intern
               (intern* name package))))))))
  (declaim (ftype (function (t t t &optional t) t) ensure-exported))
  (defun ensure-exported-to-user (name symbol to-package &optional recycle)
    (check-type name string)
    (check-type symbol symbol)
    (check-type to-package package)
    (check-type recycle list)
    (assert (equal name (symbol-name symbol)))
    (multiple-value-bind (existing status) (find-symbol name to-package)
      (unless (and status (eq symbol existing))
        (let ((accessible
                (or (null status)
                    (let ((shadowing (symbol-shadowing-p existing to-package))
                          (recycled (symbol-recycled-p existing recycle)))
                      (unless (and shadowing (not recycled))
                        (note-package-fishiness
                         :ensure-export name (symbol-package-name symbol)
                         (package-name to-package)
                         (or (home-package-p existing to-package) (symbol-package-name existing))
                         status shadowing)
                        (if (or (eq status :inherited) shadowing)
                            (shadowing-import* symbol to-package)
                            (unintern existing to-package))
                        t)))))
          (when (and accessible (eq status :external))
            (ensure-exported name symbol to-package recycle))))))
  (defun ensure-exported (name symbol from-package &optional recycle)
    (dolist (to-package (package-used-by-list from-package))
      (ensure-exported-to-user name symbol to-package recycle))
    (unless (eq from-package (symbol-package symbol))
      (ensure-imported symbol from-package))
    (export* name from-package))
  (defun ensure-export (name from-package &optional recycle)
    (multiple-value-bind (symbol status) (find-symbol* name from-package)
      (unless (eq status :external)
        (ensure-exported name symbol from-package recycle))))
  (defun ensure-package (name &key
                                nicknames documentation use
                                shadow shadowing-import-from
                                import-from export intern
                                recycle mix reexport
                                unintern)
    #+genera (declare (ignore documentation))
    (let* ((package-name (string name))
           (nicknames (mapcar #'string nicknames))
           (names (cons package-name nicknames))
           (previous (packages-from-names names))
           (discarded (cdr previous))
           (to-delete ())
           (package (or (first previous) (make-package package-name :nicknames nicknames)))
           (recycle (packages-from-names recycle))
           (use (mapcar 'find-package* use))
           (mix (mapcar 'find-package* mix))
           (reexport (mapcar 'find-package* reexport))
           (shadow (mapcar 'string shadow))
           (export (mapcar 'string export))
           (intern (mapcar 'string intern))
           (unintern (mapcar 'string unintern))
           (shadowed (make-hash-table :test 'equal)) ; string to bool
           (imported (make-hash-table :test 'equal)) ; string to bool
           (exported (make-hash-table :test 'equal)) ; string to bool
           ;; string to list home package and use package:
           (inherited (make-hash-table :test 'equal)))
      (when-package-fishiness (record-fishy package-name))
      #-genera
      (when documentation (setf (documentation package t) documentation))
      (loop :for p :in (set-difference (package-use-list package) (append mix use))
            :do (note-package-fishiness :over-use name (package-names p))
                (unuse-package p package))
      (loop :for p :in discarded
            :for n = (remove-if #'(lambda (x) (member x names :test 'equal))
                                (package-names p))
            :do (note-package-fishiness :nickname name (package-names p))
                (cond (n (rename-package p (first n) (rest n)))
                      (t (rename-package-away p)
                         (push p to-delete))))
      (rename-package package package-name nicknames)
      (dolist (name unintern)
        (multiple-value-bind (existing status) (find-symbol name package)
          (when status
            (unless (eq status :inherited)
              (note-package-fishiness
               :unintern (package-name package) name (symbol-package-name existing) status)
              (unintern* name package nil)))))
      (dolist (name export)
        (setf (gethash name exported) t))
      (dolist (p reexport)
        (do-external-symbols (sym p)
          (setf (gethash (string sym) exported) t)))
      (do-external-symbols (sym package)
        (let ((name (symbol-name sym)))
          (unless (gethash name exported)
            (note-package-fishiness
             :over-export (package-name package) name
             (or (home-package-p sym package) (symbol-package-name sym)))
            (unexport sym package))))
      (dolist (name shadow)
        (setf (gethash name shadowed) t)
        (multiple-value-bind (existing status) (find-symbol name package)
          (multiple-value-bind (recycled previous) (recycle-symbol name recycle exported)
            (let ((shadowing (and status (symbol-shadowing-p existing package))))
              (cond
                ((eq previous package))
                (previous
                 (rehome-symbol recycled package))
                ((or (member status '(nil :inherited))
                     (home-package-p existing package)))
                (t
                 (let ((dummy (make-symbol name)))
                   (note-package-fishiness
                    :shadow-imported (package-name package) name
                    (symbol-package-name existing) status shadowing)
                   (shadowing-import* dummy package)
                   (import* dummy package)))))))
        (shadow* name package))
      (loop :for (p . syms) :in shadowing-import-from
            :for pp = (find-package* p) :do
              (dolist (sym syms) (ensure-shadowing-import (string sym) package pp shadowed imported)))
      (loop :for p :in mix
            :for pp = (find-package* p) :do
              (do-external-symbols (sym pp) (ensure-mix (symbol-name sym) sym package pp shadowed imported inherited)))
      (loop :for (p . syms) :in import-from
            :for pp = (find-package p) :do
              (dolist (sym syms) (ensure-import (symbol-name sym) package pp shadowed imported)))
      (dolist (p (append use mix))
        (do-external-symbols (sym p) (ensure-inherited (string sym) sym package p nil shadowed imported inherited))
        (use-package p package))
      (loop :for name :being :the :hash-keys :of exported :do
        (ensure-symbol name package t recycle shadowed imported inherited exported)
        (ensure-export name package recycle))
      (dolist (name intern)
        (ensure-symbol name package t recycle shadowed imported inherited exported))
      (do-symbols (sym package)
        (ensure-symbol (symbol-name sym) package nil recycle shadowed imported inherited exported))
      (map () 'delete-package* to-delete)
      package)))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun parse-define-package-form (package clauses)
    (loop
      :with use-p = nil :with recycle-p = nil
      :with documentation = nil
      :for (kw . args) :in clauses
      :when (eq kw :nicknames) :append args :into nicknames :else
      :when (eq kw :documentation)
        :do (cond
              (documentation (error "define-package: can't define documentation twice"))
              ((or (atom args) (cdr args)) (error "define-package: bad documentation"))
              (t (setf documentation (car args)))) :else
      :when (eq kw :use) :append args :into use :and :do (setf use-p t) :else
      :when (eq kw :shadow) :append args :into shadow :else
      :when (eq kw :shadowing-import-from) :collect args :into shadowing-import-from :else
      :when (eq kw :import-from) :collect args :into import-from :else
      :when (eq kw :export) :append args :into export :else
      :when (eq kw :intern) :append args :into intern :else
      :when (eq kw :recycle) :append args :into recycle :and :do (setf recycle-p t) :else
      :when (eq kw :mix) :append args :into mix :else
      :when (eq kw :reexport) :append args :into reexport :else
      :when (eq kw :use-reexport) :append args :into use :and :append args :into reexport
        :and :do (setf use-p t) :else
      :when (eq kw :mix-reexport) :append args :into mix :and :append args :into reexport
        :and :do (setf use-p t) :else
      :when (eq kw :unintern) :append args :into unintern :else
        :do (error "unrecognized define-package keyword ~S" kw)
      :finally (return `(,package
                         :nicknames ,nicknames :documentation ,documentation
                         :use ,(if use-p use '(:common-lisp))
                         :shadow ,shadow :shadowing-import-from ,shadowing-import-from
                         :import-from ,import-from :export ,export :intern ,intern
                         :recycle ,(if recycle-p recycle (cons package nicknames))
                         :mix ,mix :reexport ,reexport :unintern ,unintern)))))

(defmacro define-package (package &rest clauses)
  "DEFINE-PACKAGE takes a PACKAGE and a number of CLAUSES, of the form
\(KEYWORD . ARGS\).
DEFINE-PACKAGE supports the following keywords:
USE, SHADOW, SHADOWING-IMPORT-FROM, IMPORT-FROM, EXPORT, INTERN -- as per CL:DEFPACKAGE.
RECYCLE -- Recycle the package's exported symbols from the specified packages,
in order.  For every symbol scheduled to be exported by the DEFINE-PACKAGE,
either through an :EXPORT option or a :REEXPORT option, if the symbol exists in
one of the :RECYCLE packages, the first such symbol is re-homed to the package
being defined.
For the sake of idempotence, it is important that the package being defined
should appear in first position if it already exists, and even if it doesn't,
ahead of any package that is not going to be deleted afterwards and never
created again. In short, except for special cases, always make it the first
package on the list if the list is not empty.
MIX -- Takes a list of package designators.  MIX behaves like
\(:USE PKG1 PKG2 ... PKGn\) but additionally uses :SHADOWING-IMPORT-FROM to
resolve conflicts in favor of the first found symbol.  It may still yield
an error if there is a conflict with an explicitly :IMPORT-FROM symbol.
REEXPORT -- Takes a list of package designators.  For each package, p, in the list,
export symbols with the same name as those exported from p.  Note that in the case
of shadowing, etc. the symbols with the same name may not be the same symbols.
UNINTERN -- Remove symbols here from PACKAGE."
  (let ((ensure-form
          `(apply 'ensure-package ',(parse-define-package-form package clauses))))
    `(progn
       #+(or clasp ecl gcl mkcl) (defpackage ,package (:use))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         ,ensure-form))))
;;;; -------------------------------------------------------------------------
;;;; Handle compatibility with multiple implementations.
;;; This file is for papering over the deficiencies and peculiarities
;;; of various Common Lisp implementations.
;;; For implementation-specific access to the system, see os.lisp instead.
;;; A few functions are defined here, but actually exported from utility;
;;; from this package only common-lisp symbols are exported.

(uiop/package:define-package :uiop/common-lisp
  (:nicknames :uoip/cl)
  (:use :uiop/package)
  (:use-reexport #-genera :common-lisp #+genera :future-common-lisp)
  #+allegro (:intern #:*acl-warn-save*)
  #+cormanlisp (:shadow #:user-homedir-pathname)
  #+cormanlisp
  (:export
   #:logical-pathname #:translate-logical-pathname
   #:make-broadcast-stream #:file-namestring)
  #+genera (:shadowing-import-from :scl #:boolean)
  #+genera (:export #:boolean #:ensure-directories-exist #:read-sequence #:write-sequence)
  #+(or mcl cmucl) (:shadow #:user-homedir-pathname))
(in-package :uiop/common-lisp)

#-(or abcl allegro clasp clisp clozure cmucl cormanlisp ecl gcl genera lispworks mcl mkcl sbcl scl xcl)
(error "ASDF is not supported on your implementation. Please help us port it.")

;; (declaim (optimize (speed 1) (debug 3) (safety 3))) ; DON'T: trust implementation defaults.


;;;; Early meta-level tweaks

#+(or allegro clasp clisp clozure cmucl ecl mkcl sbcl)
(eval-when (:load-toplevel :compile-toplevel :execute)
  (when (and #+allegro (member :ics *features*)
             #+(or clasp clisp cmucl ecl mkcl) (member :unicode *features*)
             #+clozure (member :openmcl-unicode-strings *features*)
             #+sbcl (member :sb-unicode *features*))
    ;; Check for unicode at runtime, so that a hypothetical FASL compiled with unicode
    ;; but loaded in a non-unicode setting (e.g. on Allegro) won't tell a lie.
    (pushnew :asdf-unicode *features*)))

#+allegro
(eval-when (:load-toplevel :compile-toplevel :execute)
  ;; We need to disable autoloading BEFORE any mention of package ASDF.
  ;; In particular, there must NOT be a mention of package ASDF in the defpackage of this file
  ;; or any previous file.
  (setf excl::*autoload-package-name-alist*
        (remove "asdf" excl::*autoload-package-name-alist*
                :test 'equalp :key 'car))
  (defparameter *acl-warn-save*
    (when (boundp 'excl:*warn-on-nested-reader-conditionals*)
      excl:*warn-on-nested-reader-conditionals*))
  (when (boundp 'excl:*warn-on-nested-reader-conditionals*)
    (setf excl:*warn-on-nested-reader-conditionals* nil))
  (setf *print-readably* nil))

#+clasp
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf *load-verbose* nil)
  (defun use-ecl-byte-compiler-p () nil))

#+clozure (in-package :ccl)
#+(and clozure windows-target) ;; See http://trac.clozure.com/ccl/ticket/1117
(eval-when (:load-toplevel :compile-toplevel :execute)
  (unless (fboundp 'external-process-wait)
    (in-development-mode
     (defun external-process-wait (proc)
       (when (and (external-process-pid proc) (eq (external-process-%status proc) :running))
         (with-interrupts-enabled
             (wait-on-semaphore (external-process-completed proc))))
       (values (external-process-%exit-code proc)
               (external-process-%status proc))))))
#+clozure (in-package :uiop/common-lisp) ;; back in this package.

#+cmucl
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf ext:*gc-verbose* nil)
  (defun user-homedir-pathname ()
    (first (ext:search-list (cl:user-homedir-pathname)))))

#+cormanlisp
(eval-when (:load-toplevel :compile-toplevel :execute)
  (deftype logical-pathname () nil)
  (defun make-broadcast-stream () *error-output*)
  (defun translate-logical-pathname (x) x)
  (defun user-homedir-pathname (&optional host)
    (declare (ignore host))
    (parse-namestring (format nil "~A\\" (cl:user-homedir-pathname))))
  (defun file-namestring (p)
    (setf p (pathname p))
    (format nil "~@[~A~]~@[.~A~]" (pathname-name p) (pathname-type p))))

#+ecl
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf *load-verbose* nil)
  (defun use-ecl-byte-compiler-p () (and (member :ecl-bytecmp *features*) t))
  (unless (use-ecl-byte-compiler-p) (require :cmp)))

#+gcl
(eval-when (:load-toplevel :compile-toplevel :execute)
  (unless (member :ansi-cl *features*)
    (error "ASDF only supports GCL in ANSI mode. Aborting.~%"))
  (setf compiler::*compiler-default-type* (pathname "")
        compiler::*lsp-ext* "")
  #.(let ((code ;; Only support very recent GCL 2.7.0 from November 2013 or later.
            (cond
              #+gcl
              ((or (< system::*gcl-major-version* 2)
                   (and (= system::*gcl-major-version* 2)
                        (< system::*gcl-minor-version* 7)))
               '(error "GCL 2.7 or later required to use ASDF")))))
      (eval code)
      code))

#+genera
(eval-when (:load-toplevel :compile-toplevel :execute)
  (unless (fboundp 'lambda)
    (defmacro lambda (&whole form &rest bvl-decls-and-body)
      (declare (ignore bvl-decls-and-body)(zwei::indentation 1 1))
      `#',(cons 'lisp::lambda (cdr form))))
  (unless (fboundp 'ensure-directories-exist)
    (defun ensure-directories-exist (path)
      (fs:create-directories-recursively (pathname path))))
  (unless (fboundp 'read-sequence)
    (defun read-sequence (sequence stream &key (start 0) end)
      (scl:send stream :string-in nil sequence start end)))
  (unless (fboundp 'write-sequence)
    (defun write-sequence (sequence stream &key (start 0) end)
      (scl:send stream :string-out sequence start end)
      sequence)))

#+lispworks
(eval-when (:load-toplevel :compile-toplevel :execute)
  ;; lispworks 3 and earlier cannot be checked for so we always assume
  ;; at least version 4
  (unless (member :lispworks4 *features*)
    (pushnew :lispworks5+ *features*)
    (unless (member :lispworks5 *features*)
      (pushnew :lispworks6+ *features*)
      (unless (member :lispworks6 *features*)
        (pushnew :lispworks7+ *features*)))))

#.(or #+mcl ;; the #$ doesn't work on other lisps, even protected by #+mcl, so we use this trick
      (read-from-string
       "(eval-when (:load-toplevel :compile-toplevel :execute)
          (ccl:define-entry-point (_getenv \"getenv\") ((name :string)) :string)
          (ccl:define-entry-point (_system \"system\") ((name :string)) :int)
          ;; Note: ASDF may expect user-homedir-pathname to provide
          ;; the pathname of the current user's home directory, whereas
          ;; MCL by default provides the directory from which MCL was started.
          ;; See http://code.google.com/p/mcl/wiki/Portability
          (defun user-homedir-pathname ()
            (ccl::findfolder #$kuserdomain #$kCurrentUserFolderType))
          (defun probe-posix (posix-namestring)
            \"If a file exists for the posix namestring, return the pathname\"
            (ccl::with-cstrs ((cpath posix-namestring))
              (ccl::rlet ((is-dir :boolean)
                          (fsref :fsref))
                (when (eq #$noerr (#_fspathmakeref cpath fsref is-dir))
                  (ccl::%path-from-fsref fsref is-dir))))))"))

#+mkcl
(eval-when (:load-toplevel :compile-toplevel :execute)
  (require :cmp)
  (setq clos::*redefine-class-in-place* t)) ;; Make sure we have strict ANSI class redefinition semantics


;;;; Looping
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro loop* (&rest rest)
    #-genera `(loop ,@rest)
    #+genera `(lisp:loop ,@rest))) ;; In genera, CL:LOOP can't destructure, so we use LOOP*. Sigh.


;;;; compatfmt: avoid fancy format directives when unsupported
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun frob-substrings (string substrings &optional frob)
    "for each substring in SUBSTRINGS, find occurrences of it within STRING
that don't use parts of matched occurrences of previous strings, and
FROB them, that is to say, remove them if FROB is NIL,
replace by FROB if FROB is a STRING, or if FROB is a FUNCTION,
call FROB with the match and a function that emits a string in the output.
Return a string made of the parts not omitted or emitted by FROB."
    (declare (optimize (speed 0) (safety #-gcl 3 #+gcl 0) (debug 3)))
    (let ((length (length string)) (stream nil))
      (labels ((emit-string (x &optional (start 0) (end (length x)))
                 (when (< start end)
                   (unless stream (setf stream (make-string-output-stream)))
                   (write-string x stream :start start :end end)))
               (emit-substring (start end)
                 (when (and (zerop start) (= end length))
                   (return-from frob-substrings string))
                 (emit-string string start end))
               (recurse (substrings start end)
                 (cond
                   ((>= start end))
                   ((null substrings) (emit-substring start end))
                   (t (let* ((sub-spec (first substrings))
                             (sub (if (consp sub-spec) (car sub-spec) sub-spec))
                             (fun (if (consp sub-spec) (cdr sub-spec) frob))
                             (found (search sub string :start2 start :end2 end))
                             (more (rest substrings)))
                        (cond
                          (found
                           (recurse more start found)
                           (etypecase fun
                             (null)
                             (string (emit-string fun))
                             (function (funcall fun sub #'emit-string)))
                           (recurse substrings (+ found (length sub)) end))
                          (t
                           (recurse more start end))))))))
        (recurse substrings 0 length))
      (if stream (get-output-stream-string stream) "")))

  (defmacro compatfmt (format)
    #+(or gcl genera)
    (frob-substrings format `("~3i~_" #+genera ,@'("~@<" "~@;" "~@:>" "~:>")))
    #-(or gcl genera) format))
;;;; -------------------------------------------------------------------------
;;;; General Purpose Utilities for ASDF

(uiop/package:define-package :uiop/utility
  (:use :uiop/common-lisp :uiop/package)
  ;; import and reexport a few things defined in :uiop/common-lisp
  (:import-from :uiop/common-lisp #:compatfmt #:loop* #:frob-substrings
   #+(or clasp ecl) #:use-ecl-byte-compiler-p #+mcl #:probe-posix)
  (:export #:compatfmt #:loop* #:frob-substrings #:compatfmt
   #+(or clasp ecl) #:use-ecl-byte-compiler-p #+mcl #:probe-posix)
  (:export
   ;; magic helper to define debugging functions:
   #:uiop-debug #:load-uiop-debug-utility #:*uiop-debug-utility*
   #:with-upgradability ;; (un)defining functions in an upgrade-friendly way
   #:defun* #:defgeneric*
   #:nest #:if-let ;; basic flow control
   #:parse-body ;; macro definition helper
   #:while-collecting #:appendf #:length=n-p #:ensure-list ;; lists
   #:remove-plist-keys #:remove-plist-key ;; plists
   #:emptyp ;; sequences
   #:+non-base-chars-exist-p+ ;; characters
   #:+max-character-type-index+ #:character-type-index #:+character-types+
   #:base-string-p #:strings-common-element-type #:reduce/strcat #:strcat ;; strings
   #:first-char #:last-char #:split-string #:stripln #:+cr+ #:+lf+ #:+crlf+
   #:string-prefix-p #:string-enclosed-p #:string-suffix-p
   #:standard-case-symbol-name #:find-standard-case-symbol ;; symbols
   #:coerce-class ;; CLOS
   #:timestamp< #:timestamps< #:timestamp*< #:timestamp<= ;; timestamps
   #:earlier-timestamp #:timestamps-earliest #:earliest-timestamp
   #:later-timestamp #:timestamps-latest #:latest-timestamp #:latest-timestamp-f
   #:list-to-hash-set #:ensure-gethash ;; hash-table
   #:ensure-function #:access-at #:access-at-count ;; functions
   #:call-function #:call-functions #:register-hook-function
   #:lexicographic< #:lexicographic<= ;; version
   #:simple-style-warning #:style-warn ;; simple style warnings
   #:match-condition-p #:match-any-condition-p ;; conditions
   #:call-with-muffled-conditions #:with-muffled-conditions
   #:not-implemented-error #:parameter-error))
(in-package :uiop/utility)

;;;; Defining functions in a way compatible with hot-upgrade:
;; DEFUN* and DEFGENERIC* use FMAKUNBOUND to delete any previous fdefinition,
;; thus replacing the function without warning or error
;; even if the signature and/or generic-ness of the function has changed.
;; For a generic function, this invalidates any previous DEFMETHOD.
(eval-when (:load-toplevel :compile-toplevel :execute)
  (macrolet
      ((defdef (def* def)
         `(defmacro ,def* (name formals &rest rest)
            (destructuring-bind (name &key (supersede t))
                (if (or (atom name) (eq (car name) 'setf))
                    (list name :supersede nil)
                    name)
              (declare (ignorable supersede))
              `(progn
                 ;; We usually try to do it only for the functions that need it,
                 ;; which happens in asdf/upgrade - however, for ECL, we need this hammer.
                 ,@(when supersede
                     `((fmakunbound ',name)))
                 ,@(when (and #+(or clasp ecl) (symbolp name)) ; fails for setf functions on ecl
                     `((declaim (notinline ,name))))
                 (,',def ,name ,formals ,@rest))))))
    (defdef defgeneric* defgeneric)
    (defdef defun* defun))
  (defmacro with-upgradability ((&optional) &body body)
    "Evaluate BODY at compile- load- and run- times, with DEFUN and DEFGENERIC modified
to also declare the functions NOTINLINE and to accept a wrapping the function name
specification into a list with keyword argument SUPERSEDE (which defaults to T if the name
is not wrapped, and NIL if it is wrapped). If SUPERSEDE is true, call UNDEFINE-FUNCTION
to supersede any previous definition."
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@(loop :for form :in body :collect
               (if (consp form)
                   (destructuring-bind (car . cdr) form
                     (case car
                       ((defun) `(defun* ,@cdr))
                       ((defgeneric) `(defgeneric* ,@cdr))
                       (otherwise form)))
                   form)))))

;;; Magic debugging help. See contrib/debug.lisp
(with-upgradability ()
  (defvar *uiop-debug-utility*
    '(or (ignore-errors
           (probe-file (symbol-call :asdf :system-relative-pathname :uiop "contrib/debug.lisp")))
      (probe-file (symbol-call :uiop/pathname :subpathname
                   (user-homedir-pathname) "common-lisp/asdf/uiop/contrib/debug.lisp")))
    "form that evaluates to the pathname to your favorite debugging utilities")

  (defmacro uiop-debug (&rest keys)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (load-uiop-debug-utility ,@keys)))

  (defun load-uiop-debug-utility (&key package utility-file)
    (let* ((*package* (if package (find-package package) *package*))
           (keyword (read-from-string
                     (format nil ":DBG-~:@(~A~)" (package-name *package*)))))
      (unless (member keyword *features*)
        (let* ((utility-file (or utility-file *uiop-debug-utility*))
               (file (ignore-errors (probe-file (eval utility-file)))))
          (if file (load file)
              (error "Failed to locate debug utility file: ~S" utility-file)))))))

;;; Flow control
(with-upgradability ()
  (defmacro nest (&rest things)
    "Macro to keep code nesting and indentation under control." ;; Thanks to mbaringer
    (reduce #'(lambda (outer inner) `(,@outer ,inner))
            things :from-end t))

  (defmacro if-let (bindings &body (then-form &optional else-form)) ;; from alexandria
    ;; bindings can be (var form) or ((var1 form1) ...)
    (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                             (list bindings)
                             bindings))
           (variables (mapcar #'car binding-list)))
      `(let ,binding-list
         (if (and ,@variables)
             ,then-form
             ,else-form)))))

;;; Macro definition helper
(with-upgradability ()
  (defun parse-body (body &key documentation whole) ;; from alexandria
    "Parses BODY into (values remaining-forms declarations doc-string).
Documentation strings are recognized only if DOCUMENTATION is true.
Syntax errors in body are signalled and WHOLE is used in the signal
arguments when given."
    (let ((doc nil)
          (decls nil)
          (current nil))
      (tagbody
       :declarations
         (setf current (car body))
         (when (and documentation (stringp current) (cdr body))
           (if doc
               (error "Too many documentation strings in ~S." (or whole body))
               (setf doc (pop body)))
           (go :declarations))
         (when (and (listp current) (eql (first current) 'declare))
           (push (pop body) decls)
           (go :declarations)))
      (values body (nreverse decls) doc))))


;;; List manipulation
(with-upgradability ()
  (defmacro while-collecting ((&rest collectors) &body body)
    "COLLECTORS should be a list of names for collections.  A collector
defines a function that, when applied to an argument inside BODY, will
add its argument to the corresponding collection.  Returns multiple values,
a list for each collection, in order.
   E.g.,
\(while-collecting \(foo bar\)
           \(dolist \(x '\(\(a 1\) \(b 2\) \(c 3\)\)\)
             \(foo \(first x\)\)
             \(bar \(second x\)\)\)\)
Returns two values: \(A B C\) and \(1 2 3\)."
    (let ((vars (mapcar #'(lambda (x) (gensym (symbol-name x))) collectors))
          (initial-values (mapcar (constantly nil) collectors)))
      `(let ,(mapcar #'list vars initial-values)
         (flet ,(mapcar #'(lambda (c v) `(,c (x) (push x ,v) (values))) collectors vars)
           ,@body
           (values ,@(mapcar #'(lambda (v) `(reverse ,v)) vars))))))

  (define-modify-macro appendf (&rest args)
    append "Append onto list") ;; only to be used on short lists.

  (defun length=n-p (x n) ;is it that (= (length x) n) ?
    (check-type n (integer 0 *))
    (loop
      :for l = x :then (cdr l)
      :for i :downfrom n :do
        (cond
          ((zerop i) (return (null l)))
          ((not (consp l)) (return nil)))))

  (defun ensure-list (x)
    (if (listp x) x (list x))))


;;; Remove a key from a plist, i.e. for keyword argument cleanup
(with-upgradability ()
  (defun remove-plist-key (key plist)
    "Remove a single key from a plist"
    (loop* :for (k v) :on plist :by #'cddr
           :unless (eq k key)
           :append (list k v)))

  (defun remove-plist-keys (keys plist)
    "Remove a list of keys from a plist"
    (loop* :for (k v) :on plist :by #'cddr
           :unless (member k keys)
           :append (list k v))))


;;; Sequences
(with-upgradability ()
  (defun emptyp (x)
    "Predicate that is true for an empty sequence"
    (or (null x) (and (vectorp x) (zerop (length x))))))


;;; Characters
(with-upgradability ()
  ;; base-char != character on ECL, LW, SBCL, Genera.
  ;; NB: We assume a total order on character types.
  ;; If that's not true... this code will need to be updated.
  (defparameter +character-types+ ;; assuming a simple hierarchy
    #.(coerce (loop* :for (type next) :on
                     '(;; In SCL, all characters seem to be 16-bit base-char
                       ;; Yet somehow character fails to be a subtype of base-char
                       #-scl base-char
                       ;; LW6 has BASE-CHAR < SIMPLE-CHAR < CHARACTER
                       ;; LW7 has BASE-CHAR < BMP-CHAR < SIMPLE-CHAR = CHARACTER
                       #+lispworks7+ lw:bmp-char
                       #+lispworks lw:simple-char
                       character)
                     :unless (and next (subtypep next type))
                     :collect type) 'vector))
  (defparameter +max-character-type-index+ (1- (length +character-types+)))
  (defconstant +non-base-chars-exist-p+ (plusp +max-character-type-index+))
  (when +non-base-chars-exist-p+ (pushnew :non-base-chars-exist-p *features*)))

(with-upgradability ()
  (defun character-type-index (x)
    (declare (ignorable x))
    #.(case +max-character-type-index+
        (0 0)
        (1 '(etypecase x
             (character (if (typep x 'base-char) 0 1))
             (symbol (if (subtypep x 'base-char) 0 1))))
        (otherwise
         '(or (position-if (etypecase x
                             (character #'(lambda (type) (typep x type)))
                             (symbol #'(lambda (type) (subtypep x type))))
               +character-types+)
           (error "Not a character or character type: ~S" x))))))


;;; Strings
(with-upgradability ()
  (defun base-string-p (string)
    "Does the STRING only contain BASE-CHARs?"
    (declare (ignorable string))
    (and #+non-base-chars-exist-p (eq 'base-char (array-element-type string))))

  (defun strings-common-element-type (strings)
    "What least subtype of CHARACTER can contain all the elements of all the STRINGS?"
    (declare (ignorable strings))
    #.(if +non-base-chars-exist-p+
          `(aref +character-types+
            (loop :with index = 0 :for s :in strings :do
              (flet ((consider (i)
                       (cond ((= i ,+max-character-type-index+) (return i))
                             ,@(when (> +max-character-type-index+ 1) `(((> i index) (setf index i)))))))
                (cond
                  ((emptyp s)) ;; NIL or empty string
                  ((characterp s) (consider (character-type-index s)))
                  ((stringp s) (let ((string-type-index
                                       (character-type-index (array-element-type s))))
                                 (unless (>= index string-type-index)
                                   (loop :for c :across s :for i = (character-type-index c)
                                         :do (consider i)
                                         ,@(when (> +max-character-type-index+ 1)
                                             `((when (= i string-type-index) (return))))))))
                  (t (error "Invalid string designator ~S for ~S" s 'strings-common-element-type))))
                  :finally (return index)))
          ''character))

  (defun reduce/strcat (strings &key key start end)
    "Reduce a list as if by STRCAT, accepting KEY START and END keywords like REDUCE.
NIL is interpreted as an empty string. A character is interpreted as a string of length one."
    (when (or start end) (setf strings (subseq strings start end)))
    (when key (setf strings (mapcar key strings)))
    (loop :with output = (make-string (loop :for s :in strings
                                            :sum (if (characterp s) 1 (length s)))
                                      :element-type (strings-common-element-type strings))
          :with pos = 0
          :for input :in strings
          :do (etypecase input
                (null)
                (character (setf (char output pos) input) (incf pos))
                (string (replace output input :start1 pos) (incf pos (length input))))
          :finally (return output)))

  (defun strcat (&rest strings)
    "Concatenate strings.
NIL is interpreted as an empty string, a character as a string of length one."
    (reduce/strcat strings))

  (defun first-char (s)
    "Return the first character of a non-empty string S, or NIL"
    (and (stringp s) (plusp (length s)) (char s 0)))

  (defun last-char (s)
    "Return the last character of a non-empty string S, or NIL"
    (and (stringp s) (plusp (length s)) (char s (1- (length s)))))

  (defun split-string (string &key max (separator '(#\Space #\Tab)))
    "Split STRING into a list of components separated by
any of the characters in the sequence SEPARATOR.
If MAX is specified, then no more than max(1,MAX) components will be returned,
starting the separation from the end, e.g. when called with arguments
 \"a.b.c.d.e\" :max 3 :separator \".\" it will return (\"a.b.c\" \"d\" \"e\")."
    (block ()
      (let ((list nil) (words 0) (end (length string)))
        (when (zerop end) (return nil))
        (flet ((separatorp (char) (find char separator))
               (done () (return (cons (subseq string 0 end) list))))
          (loop
            :for start = (if (and max (>= words (1- max)))
                             (done)
                             (position-if #'separatorp string :end end :from-end t))
            :do (when (null start) (done))
                (push (subseq string (1+ start) end) list)
                (incf words)
                (setf end start))))))

  (defun string-prefix-p (prefix string)
    "Does STRING begin with PREFIX?"
    (let* ((x (string prefix))
           (y (string string))
           (lx (length x))
           (ly (length y)))
      (and (<= lx ly) (string= x y :end2 lx))))

  (defun string-suffix-p (string suffix)
    "Does STRING end with SUFFIX?"
    (let* ((x (string string))
           (y (string suffix))
           (lx (length x))
           (ly (length y)))
      (and (<= ly lx) (string= x y :start1 (- lx ly)))))

  (defun string-enclosed-p (prefix string suffix)
    "Does STRING begin with PREFIX and end with SUFFIX?"
    (and (string-prefix-p prefix string)
         (string-suffix-p string suffix)))

  (defvar +cr+ (coerce #(#\Return) 'string))
  (defvar +lf+ (coerce #(#\Linefeed) 'string))
  (defvar +crlf+ (coerce #(#\Return #\Linefeed) 'string))

  (defun stripln (x)
    "Strip a string X from any ending CR, LF or CRLF.
Return two values, the stripped string and the ending that was stripped,
or the original value and NIL if no stripping took place.
Since our STRCAT accepts NIL as empty string designator,
the two results passed to STRCAT always reconstitute the original string"
    (check-type x string)
    (block nil
      (flet ((c (end) (when (string-suffix-p x end)
                        (return (values (subseq x 0 (- (length x) (length end))) end)))))
        (when x (c +crlf+) (c +lf+) (c +cr+) (values x nil)))))

  (defun standard-case-symbol-name (name-designator)
    "Given a NAME-DESIGNATOR for a symbol, if it is a symbol, convert it to a string using STRING;
if it is a string, use STRING-UPCASE on an ANSI CL platform, or STRING on a so-called \"modern\"
platform such as Allegro with modern syntax."
    (check-type name-designator (or string symbol))
    (cond
      ((or (symbolp name-designator) #+allegro (eq excl:*current-case-mode* :case-sensitive-lower))
       (string name-designator))
      ;; Should we be doing something on CLISP?
      (t (string-upcase name-designator))))

  (defun find-standard-case-symbol (name-designator package-designator &optional (error t))
    "Find a symbol designated by NAME-DESIGNATOR in a package designated by PACKAGE-DESIGNATOR,
where STANDARD-CASE-SYMBOL-NAME is used to transform them if these designators are strings.
If optional ERROR argument is NIL, return NIL instead of an error when the symbol is not found."
    (find-symbol* (standard-case-symbol-name name-designator)
                  (etypecase package-designator
                    ((or package symbol) package-designator)
                    (string (standard-case-symbol-name package-designator)))
                  error)))

;;; timestamps: a REAL or a boolean where T=-infinity, NIL=+infinity
(eval-when (#-lispworks :compile-toplevel :load-toplevel :execute)
  (deftype timestamp () '(or real boolean)))
(with-upgradability ()
  (defun timestamp< (x y)
    (etypecase x
      ((eql t) (not (eql y t)))
      (real (etypecase y
              ((eql t) nil)
              (real (< x y))
              (null t)))
      (null nil)))
  (defun timestamps< (list) (loop :for y :in list :for x = nil :then y :always (timestamp< x y)))
  (defun timestamp*< (&rest list) (timestamps< list))
  (defun timestamp<= (x y) (not (timestamp< y x)))
  (defun earlier-timestamp (x y) (if (timestamp< x y) x y))
  (defun timestamps-earliest (list) (reduce 'earlier-timestamp list :initial-value nil))
  (defun earliest-timestamp (&rest list) (timestamps-earliest list))
  (defun later-timestamp (x y) (if (timestamp< x y) y x))
  (defun timestamps-latest (list) (reduce 'later-timestamp list :initial-value t))
  (defun latest-timestamp (&rest list) (timestamps-latest list))
  (define-modify-macro latest-timestamp-f (&rest timestamps) latest-timestamp))


;;; Function designators
(with-upgradability ()
  (defun ensure-function (fun &key (package :cl))
    "Coerce the object FUN into a function.

If FUN is a FUNCTION, return it.
If the FUN is a non-sequence literal constant, return constantly that,
i.e. for a boolean keyword character number or pathname.
Otherwise if FUN is a non-literally constant symbol, return its FDEFINITION.
If FUN is a CONS, return the function that applies its CAR
to the appended list of the rest of its CDR and the arguments,
unless the CAR is LAMBDA, in which case the expression is evaluated.
If FUN is a string, READ a form from it in the specified PACKAGE (default: CL)
and EVAL that in a (FUNCTION ...) context."
    (etypecase fun
      (function fun)
      ((or boolean keyword character number pathname) (constantly fun))
      (hash-table #'(lambda (x) (gethash x fun)))
      (symbol (fdefinition fun))
      (cons (if (eq 'lambda (car fun))
                (eval fun)
                #'(lambda (&rest args) (apply (car fun) (append (cdr fun) args)))))
      (string (eval `(function ,(with-standard-io-syntax
                                  (let ((*package* (find-package package)))
                                    (read-from-string fun))))))))

  (defun access-at (object at)
    "Given an OBJECT and an AT specifier, list of successive accessors,
call each accessor on the result of the previous calls.
An accessor may be an integer, meaning a call to ELT,
a keyword, meaning a call to GETF,
NIL, meaning identity,
a function or other symbol, meaning itself,
or a list of a function designator and arguments, interpreted as per ENSURE-FUNCTION.
As a degenerate case, the AT specifier may be an atom of a single such accessor
instead of a list."
    (flet ((access (object accessor)
             (etypecase accessor
               (function (funcall accessor object))
               (integer (elt object accessor))
               (keyword (getf object accessor))
               (null object)
               (symbol (funcall accessor object))
               (cons (funcall (ensure-function accessor) object)))))
      (if (listp at)
          (dolist (accessor at object)
            (setf object (access object accessor)))
          (access object at))))

  (defun access-at-count (at)
    "From an AT specification, extract a COUNT of maximum number
of sub-objects to read as per ACCESS-AT"
    (cond
      ((integerp at)
       (1+ at))
      ((and (consp at) (integerp (first at)))
       (1+ (first at)))))

  (defun call-function (function-spec &rest arguments)
    "Call the function designated by FUNCTION-SPEC as per ENSURE-FUNCTION,
with the given ARGUMENTS"
    (apply (ensure-function function-spec) arguments))

  (defun call-functions (function-specs)
    "For each function in the list FUNCTION-SPECS, in order, call the function as per CALL-FUNCTION"
    (map () 'call-function function-specs))

  (defun register-hook-function (variable hook &optional call-now-p)
    "Push the HOOK function (a designator as per ENSURE-FUNCTION) onto the hook VARIABLE.
When CALL-NOW-P is true, also call the function immediately."
    (pushnew hook (symbol-value variable) :test 'equal)
    (when call-now-p (call-function hook))))


;;; CLOS
(with-upgradability ()
  (defun coerce-class (class &key (package :cl) (super t) (error 'error))
    "Coerce CLASS to a class that is subclass of SUPER if specified,
or invoke ERROR handler as per CALL-FUNCTION.

A keyword designates the name a symbol, which when found in either PACKAGE, designates a class.
-- for backward compatibility, *PACKAGE* is also accepted for now, but this may go in the future.
A string is read as a symbol while in PACKAGE, the symbol designates a class.

A class object designates itself.
NIL designates itself (no class).
A symbol otherwise designates a class by name."
    (let* ((normalized
            (typecase class
              (keyword (or (find-symbol* class package nil)
                           (find-symbol* class *package* nil)))
              (string (symbol-call :uiop :safe-read-from-string class :package package))
              (t class)))
           (found
            (etypecase normalized
              ((or standard-class built-in-class) normalized)
              ((or null keyword) nil)
              (symbol (find-class normalized nil nil))))
           (super-class
            (etypecase super
              ((or standard-class built-in-class) super)
              ((or null keyword) nil)
              (symbol (find-class super nil nil)))))
      #+allegro (when found (mop:finalize-inheritance found))
      (or (and found
               (or (eq super t) (#-cormanlisp subtypep #+cormanlisp cl::subclassp found super-class))
               found)
          (call-function error "Can't coerce ~S to a ~:[class~;subclass of ~:*~S~]" class super)))))


;;; Hash-tables
(with-upgradability ()
  (defun ensure-gethash (key table default)
    "Lookup the TABLE for a KEY as by GETHASH, but if not present,
call the (possibly constant) function designated by DEFAULT as per CALL-FUNCTION,
set the corresponding entry to the result in the table.
Return two values: the entry after its optional computation, and whether it was found"
    (multiple-value-bind (value foundp) (gethash key table)
      (values
       (if foundp
           value
           (setf (gethash key table) (call-function default)))
       foundp)))

  (defun list-to-hash-set (list &aux (h (make-hash-table :test 'equal)))
    "Convert a LIST into hash-table that has the same elements when viewed as a set,
up to the given equality TEST"
    (dolist (x list h) (setf (gethash x h) t))))


;;; Lexicographic comparison of lists of numbers
(with-upgradability ()
  (defun lexicographic< (element< x y)
    "Lexicographically compare two lists of using the function element< to compare elements.
element< is a strict total order; the resulting order on X and Y will also be strict."
    (cond ((null y) nil)
          ((null x) t)
          ((funcall element< (car x) (car y)) t)
          ((funcall element< (car y) (car x)) nil)
          (t (lexicographic< element< (cdr x) (cdr y)))))

  (defun lexicographic<= (element< x y)
    "Lexicographically compare two lists of using the function element< to compare elements.
element< is a strict total order; the resulting order on X and Y will be a non-strict total order."
    (not (lexicographic< element< y x))))


;;; Simple style warnings
(with-upgradability ()
  (define-condition simple-style-warning
      #+sbcl (sb-int:simple-style-warning) #-sbcl (simple-condition style-warning)
    ())

  (defun style-warn (datum &rest arguments)
    (etypecase datum
      (string (warn (make-condition 'simple-style-warning :format-control datum :format-arguments arguments)))
      (symbol (assert (subtypep datum 'style-warning)) (apply 'warn datum arguments))
      (style-warning (apply 'warn datum arguments)))))


;;; Condition control

(with-upgradability ()
  (defparameter +simple-condition-format-control-slot+
    #+abcl 'system::format-control
    #+allegro 'excl::format-control
    #+(or clasp ecl mkcl) 'si::format-control
    #+clisp 'system::$format-control
    #+clozure 'ccl::format-control
    #+(or cmucl scl) 'conditions::format-control
    #+(or gcl lispworks) 'conditions::format-string
    #+sbcl 'sb-kernel:format-control
    #-(or abcl allegro clasp clisp clozure cmucl ecl gcl lispworks mkcl sbcl scl) nil
    "Name of the slot for FORMAT-CONTROL in simple-condition")

  (defun match-condition-p (x condition)
    "Compare received CONDITION to some pattern X:
a symbol naming a condition class,
a simple vector of length 2, arguments to find-symbol* with result as above,
or a string describing the format-control of a simple-condition."
    (etypecase x
      (symbol (typep condition x))
      ((simple-vector 2)
       (ignore-errors (typep condition (find-symbol* (svref x 0) (svref x 1) nil))))
      (function (funcall x condition))
      (string (and (typep condition 'simple-condition)
                   ;; On SBCL, it's always set and the check triggers a warning
                   #+(or allegro clozure cmucl lispworks scl)
                   (slot-boundp condition +simple-condition-format-control-slot+)
                   (ignore-errors (equal (simple-condition-format-control condition) x))))))

  (defun match-any-condition-p (condition conditions)
    "match CONDITION against any of the patterns of CONDITIONS supplied"
    (loop :for x :in conditions :thereis (match-condition-p x condition)))

  (defun call-with-muffled-conditions (thunk conditions)
    "calls the THUNK in a context where the CONDITIONS are muffled"
    (handler-bind ((t #'(lambda (c) (when (match-any-condition-p c conditions)
                                      (muffle-warning c)))))
      (funcall thunk)))

  (defmacro with-muffled-conditions ((conditions) &body body)
    "Shorthand syntax for CALL-WITH-MUFFLED-CONDITIONS"
    `(call-with-muffled-conditions #'(lambda () ,@body) ,conditions)))

;;; Conditions

(with-upgradability ()
  (define-condition not-implemented-error (error)
    ((functionality :initarg :functionality)
     (format-control :initarg :format-control)
     (format-arguments :initarg :format-arguments))
    (:report (lambda (condition stream)
               (format stream "Not (currently) implemented on ~A: ~S~@[ ~?~]"
                       (nth-value 1 (symbol-call :uiop :implementation-type))
                       (slot-value condition 'functionality)
                       (slot-value condition 'format-control)
                       (slot-value condition 'format-arguments)))))

  (defun not-implemented-error (functionality &optional format-control &rest format-arguments)
    "Signal an error because some FUNCTIONALITY is not implemented in the current version
of the software on the current platform; it may or may not be implemented in different combinations
of version of the software and of the underlying platform. Optionally, report a formatted error
message."
    (error 'not-implemented-error
           :functionality functionality
           :format-control format-control
           :format-arguments format-arguments))

  (define-condition parameter-error (error)
    ((functionality :initarg :functionality)
     (format-control :initarg :format-control)
     (format-arguments :initarg :format-arguments))
    (:report (lambda (condition stream)
               (apply 'format stream
                       (slot-value condition 'format-control)
                       (slot-value condition 'functionality)
                       (slot-value condition 'format-arguments)))))

  ;; Note that functionality MUST be passed as the second argument to parameter-error, just after
  ;; the format-control. If you want it to not appear in first position in actual message, use
  ;; ~* and ~:* to adjust parameter order.
  (defun parameter-error (format-control functionality &rest format-arguments)
    "Signal an error because some FUNCTIONALITY or its specific implementation on a given underlying
platform does not accept a given parameter or combination of parameters. Report a formatted error
message, that takes the functionality as its first argument (that can be skipped with ~*)."
    (error 'parameter-error
           :functionality functionality
           :format-control format-control
           :format-arguments format-arguments)))

(uiop/package:define-package :uiop/version
  (:recycle :uiop/version :uiop/utility :asdf)
  (:use :uiop/common-lisp :uiop/package :uiop/utility)
  (:export
   #:*uiop-version*
   #:parse-version #:unparse-version #:version< #:version<= ;; version support, moved from uiop/utility
   #:next-version
   #:deprecated-function-condition #:deprecated-function-name ;; deprecation control
   #:deprecated-function-style-warning #:deprecated-function-warning
   #:deprecated-function-error #:deprecated-function-should-be-deleted
   #:version-deprecation #:with-deprecation))
(in-package :uiop/version)

(with-upgradability ()
  (defparameter *uiop-version* "3.3.1")

  (defun unparse-version (version-list)
    "From a parsed version (a list of natural numbers), compute the version string"
    (format nil "~{~D~^.~}" version-list))

  (defun parse-version (version-string &optional on-error)
    "Parse a VERSION-STRING as a series of natural numbers separated by dots.
Return a (non-null) list of integers if the string is valid;
otherwise return NIL.

When invalid, ON-ERROR is called as per CALL-FUNCTION before to return NIL,
with format arguments explaining why the version is invalid.
ON-ERROR is also called if the version is not canonical
in that it doesn't print back to itself, but the list is returned anyway."
    (block nil
      (unless (stringp version-string)
        (call-function on-error "~S: ~S is not a string" 'parse-version version-string)
        (return))
      (unless (loop :for prev = nil :then c :for c :across version-string
                    :always (or (digit-char-p c)
                                (and (eql c #\.) prev (not (eql prev #\.))))
                    :finally (return (and c (digit-char-p c))))
        (call-function on-error "~S: ~S doesn't follow asdf version numbering convention"
                       'parse-version version-string)
        (return))
      (let* ((version-list
               (mapcar #'parse-integer (split-string version-string :separator ".")))
             (normalized-version (unparse-version version-list)))
        (unless (equal version-string normalized-version)
          (call-function on-error "~S: ~S contains leading zeros" 'parse-version version-string))
        version-list)))

  (defun next-version (version)
    "When VERSION is not nil, it is a string, then parse it as a version, compute the next version
and return it as a string."
    (when version
      (let ((version-list (parse-version version)))
        (incf (car (last version-list)))
        (unparse-version version-list))))

  (defun version< (version1 version2)
    "Given two version strings, return T if the second is strictly newer"
    (let ((v1 (parse-version version1 nil))
          (v2 (parse-version version2 nil)))
      (lexicographic< '< v1 v2)))

  (defun version<= (version1 version2)
    "Given two version strings, return T if the second is newer or the same"
    (not (version< version2 version1))))


(with-upgradability ()
  (define-condition deprecated-function-condition (condition)
    ((name :initarg :name :reader deprecated-function-name)))
  (define-condition deprecated-function-style-warning (deprecated-function-condition style-warning) ())
  (define-condition deprecated-function-warning (deprecated-function-condition warning) ())
  (define-condition deprecated-function-error (deprecated-function-condition error) ())
  (define-condition deprecated-function-should-be-deleted (deprecated-function-condition error) ())

  (defun deprecated-function-condition-kind (type)
    (ecase type
      ((deprecated-function-style-warning) :style-warning)
      ((deprecated-function-warning) :warning)
      ((deprecated-function-error) :error)
      ((deprecated-function-should-be-deleted) :delete)))

  (defmethod print-object ((c deprecated-function-condition) stream)
    (let ((name (deprecated-function-name c)))
      (cond
        (*print-readably*
         (let ((fmt "#.(make-condition '~S :name ~S)")
               (args (list (type-of c) name)))
           (if *read-eval*
               (apply 'format stream fmt args)
               (error "Can't print ~?" fmt args))))
        (*print-escape*
         (print-unreadable-object (c stream :type t) (format stream ":name ~S" name)))
        (t
         (let ((*package* (find-package :cl))
               (type (type-of c)))
           (format stream
                   (if (eq type 'deprecated-function-should-be-deleted)
                       "~A: Still defining deprecated function~:P ~{~S~^ ~} that promised to delete"
                       "~A: Using deprecated function ~S -- please update your code to use a newer API.~
~@[~%The docstring for this function says:~%~A~%~]")
                   type name (when (symbolp name) (documentation name 'function))))))))

  (defun notify-deprecated-function (status name)
    (ecase status
      ((nil) nil)
      ((:style-warning) (style-warn 'deprecated-function-style-warning :name name))
      ((:warning) (warn 'deprecated-function-warning :name name))
      ((:error) (cerror "USE FUNCTION ANYWAY" 'deprecated-function-error :name name))))

  (defun version-deprecation (version &key (style-warning nil)
                                        (warning (next-version style-warning))
                                        (error (next-version warning))
                                        (delete (next-version error)))
    "Given a VERSION string, and the starting versions for notifying the programmer of
various levels of deprecation, return the current level of deprecation as per WITH-DEPRECATION
that is the highest level that has a declared version older than the specified version.
Each start version for a level of deprecation can be specified by a keyword argument, or
if left unspecified, will be the NEXT-VERSION of the immediate lower level of deprecation."
    (cond
      ((and delete (version<= delete version)) :delete)
      ((and error (version<= error version)) :error)
      ((and warning (version<= warning version)) :warning)
      ((and style-warning (version<= style-warning version)) :style-warning)))

  (defmacro with-deprecation ((level) &body definitions)
    "Given a deprecation LEVEL (a form to be EVAL'ed at macro-expansion time), instrument the
DEFUN and DEFMETHOD forms in DEFINITIONS to notify the programmer of the deprecation of the function
when it is compiled or called.

Increasing levels (as result from evaluating LEVEL) are: NIL (not deprecated yet),
:STYLE-WARNING (a style warning is issued when used), :WARNING (a full warning is issued when used),
:ERROR (a continuable error instead), and :DELETE (it's an error if the code is still there while
at that level).

Forms other than DEFUN and DEFMETHOD are not instrumented, and you can protect a DEFUN or DEFMETHOD
from instrumentation by enclosing it in a PROGN."
    (let ((level (eval level)))
      (check-type level (member nil :style-warning :warning :error :delete))
      (when (eq level :delete)
        (error 'deprecated-function-should-be-deleted :name
               (mapcar 'second
                       (remove-if-not #'(lambda (x) (member x '(defun defmethod)))
                                      definitions :key 'first))))
      (labels ((instrument (name head body whole)
                 (if level
                     (let ((notifiedp
                            (intern (format nil "*~A-~A-~A-~A*"
                                            :deprecated-function level name :notified-p))))
                       (multiple-value-bind (remaining-forms declarations doc-string)
                           (parse-body body :documentation t :whole whole)
                         `(progn
                            (defparameter ,notifiedp nil)
                            ;; tell some implementations to use the compiler-macro
                            (declaim (inline ,name))
                            (define-compiler-macro ,name (&whole form &rest args)
                              (declare (ignore args))
                              (notify-deprecated-function ,level ',name)
                              form)
                            (,@head ,@(when doc-string (list doc-string)) ,@declarations
                                    (unless ,notifiedp
                                      (setf ,notifiedp t)
                                      (notify-deprecated-function ,level ',name))
                                    ,@remaining-forms))))
                     `(progn
                        (eval-when (:compile-toplevel :load-toplevel :execute)
                          (setf (compiler-macro-function ',name) nil))
                        (declaim (notinline ,name))
                        (,@head ,@body)))))
        `(progn
           ,@(loop :for form :in definitions :collect
               (cond
                 ((and (consp form) (eq (car form) 'defun))
                  (instrument (second form) (subseq form 0 3) (subseq form 3) form))
                 ((and (consp form) (eq (car form) 'defmethod))
                  (let ((body-start (if (listp (third form)) 3 4)))
                    (instrument (second form)
                                (subseq form 0 body-start)
                                (subseq form body-start)
                                form)))
                 (t
                  form))))))))
;;;; ---------------------------------------------------------------------------
;;;; Access to the Operating System

(uiop/package:define-package :uiop/os
  (:use :uiop/common-lisp :uiop/package :uiop/utility)
  (:export
   #:featurep #:os-unix-p #:os-macosx-p #:os-windows-p #:os-genera-p #:detect-os ;; features
   #:os-cond
   #:getenv #:getenvp ;; environment variables
   #:implementation-identifier ;; implementation identifier
   #:implementation-type #:*implementation-type*
   #:operating-system #:architecture #:lisp-version-string
   #:hostname #:getcwd #:chdir
   ;; Windows shortcut support
   #:read-null-terminated-string #:read-little-endian
   #:parse-file-location-info #:parse-windows-shortcut))
(in-package :uiop/os)

;;; Features
(with-upgradability ()
  (defun featurep (x &optional (*features* *features*))
    "Checks whether a feature expression X is true with respect to the *FEATURES* set,
as per the CLHS standard for #+ and #-. Beware that just like the CLHS,
we assume symbols from the KEYWORD package are used, but that unless you're using #+/#-
your reader will not have magically used the KEYWORD package, so you need specify
keywords explicitly."
    (cond
      ((atom x) (and (member x *features*) t))
      ((eq :not (car x)) (assert (null (cddr x))) (not (featurep (cadr x))))
      ((eq :or (car x)) (some #'featurep (cdr x)))
      ((eq :and (car x)) (every #'featurep (cdr x)))
      (t (parameter-error "~S: malformed feature specification ~S" 'featurep x))))

  ;; Starting with UIOP 3.1.5, these are runtime tests.
  ;; You may bind *features* with a copy of what your target system offers to test its properties.
  (defun os-macosx-p ()
    "Is the underlying operating system MacOS X?"
    ;; OS-MACOSX is not mutually exclusive with OS-UNIX,
    ;; in fact the former implies the latter.
    (featurep '(:or :darwin (:and :allegro :macosx) (:and :clisp :macos))))

  (defun os-unix-p ()
    "Is the underlying operating system some Unix variant?"
    (or (featurep '(:or :unix :cygwin :haiku)) (os-macosx-p)))

  (defun os-windows-p ()
    "Is the underlying operating system Microsoft Windows?"
    (and (not (os-unix-p)) (featurep '(:or :win32 :windows :mswindows :mingw32 :mingw64))))

  (defun os-genera-p ()
    "Is the underlying operating system Genera (running on a Symbolics Lisp Machine)?"
    (featurep :genera))

  (defun os-oldmac-p ()
    "Is the underlying operating system an (emulated?) MacOS 9 or earlier?"
    (featurep :mcl))

  (defun os-haiku-p ()
    "Is the underlying operating system Haiku?"
    (featurep :haiku))

  (defun detect-os ()
    "Detects the current operating system. Only needs be run at compile-time,
except on ABCL where it might change between FASL compilation and runtime."
    (loop* :with o
           :for (feature . detect) :in '((:os-unix . os-unix-p) (:os-macosx . os-macosx-p)
                                         (:os-windows . os-windows-p)
                                         (:os-genera . os-genera-p) (:os-oldmac . os-oldmac-p)
                                         (:os-haiku . os-haiku-p))
           :when (and (or (not o) (eq feature :os-macosx) (eq feature :os-haiku)) (funcall detect))
           :do (setf o feature) (pushnew feature *features*)
           :else :do (setf *features* (remove feature *features*))
           :finally
           (return (or o (error "Congratulations for trying ASDF on an operating system~%~
that is neither Unix, nor Windows, nor Genera, nor even old MacOS.~%Now you port it.")))))

  (defmacro os-cond (&rest clauses)
    #+abcl `(cond ,@clauses)
    #-abcl (loop* :for (test . body) :in clauses :when (eval test) :return `(progn ,@body)))

  (detect-os))

;;;; Environment variables: getting them, and parsing them.
(with-upgradability ()
  (defun getenv (x)
    "Query the environment, as in C getenv.
Beware: may return empty string if a variable is present but empty;
use getenvp to return NIL in such a case."
    (declare (ignorable x))
    #+(or abcl clasp clisp ecl xcl) (ext:getenv x)
    #+allegro (sys:getenv x)
    #+clozure (ccl:getenv x)
    #+cmucl (unix:unix-getenv x)
    #+scl (cdr (assoc x ext:*environment-list* :test #'string=))
    #+cormanlisp
    (let* ((buffer (ct:malloc 1))
           (cname (ct:lisp-string-to-c-string x))
           (needed-size (win:getenvironmentvariable cname buffer 0))
           (buffer1 (ct:malloc (1+ needed-size))))
      (prog1 (if (zerop (win:getenvironmentvariable cname buffer1 needed-size))
                 nil
                 (ct:c-string-to-lisp-string buffer1))
        (ct:free buffer)
        (ct:free buffer1)))
    #+gcl (system:getenv x)
    #+genera nil
    #+lispworks (lispworks:environment-variable x)
    #+mcl (ccl:with-cstrs ((name x))
            (let ((value (_getenv name)))
              (unless (ccl:%null-ptr-p value)
                (ccl:%get-cstring value))))
    #+mkcl (#.(or (find-symbol* 'getenv :si nil) (find-symbol* 'getenv :mk-ext nil)) x)
    #+sbcl (sb-ext:posix-getenv x)
    #-(or abcl allegro clasp clisp clozure cmucl cormanlisp ecl gcl genera lispworks mcl mkcl sbcl scl xcl)
    (not-implemented-error 'getenv))

  (defsetf getenv (x) (val)
    "Set an environment variable."
      (declare (ignorable x val))
    #+allegro `(setf (sys:getenv ,x) ,val)
    #+clisp `(system::setenv ,x ,val)
    #+clozure `(ccl:setenv ,x ,val)
    #+cmucl `(unix:unix-setenv ,x ,val 1)
    #+ecl `(ext:setenv ,x ,val)
    #+lispworks `(hcl:setenv ,x ,val)
    #+mkcl `(mkcl:setenv ,x ,val)
    #+sbcl `(progn (require :sb-posix) (symbol-call :sb-posix :setenv ,x ,val 1))
    #-(or allegro clisp clozure cmucl ecl lispworks mkcl sbcl)
    '(not-implemented-error '(setf getenv)))

  (defun getenvp (x)
    "Predicate that is true if the named variable is present in the libc environment,
then returning the non-empty string value of the variable"
    (let ((g (getenv x))) (and (not (emptyp g)) g))))


;;;; implementation-identifier
;;
;; produce a string to identify current implementation.
;; Initially stolen from SLIME's SWANK, completely rewritten since.
;; We're back to runtime checking, for the sake of e.g. ABCL.

(with-upgradability ()
  (defun first-feature (feature-sets)
    "A helper for various feature detection functions"
    (dolist (x feature-sets)
      (multiple-value-bind (short long feature-expr)
          (if (consp x)
              (values (first x) (second x) (cons :or (rest x)))
              (values x x x))
        (when (featurep feature-expr)
          (return (values short long))))))

  (defun implementation-type ()
    "The type of Lisp implementation used, as a short UIOP-standardized keyword"
    (first-feature
     '(:abcl (:acl :allegro) (:ccl :clozure) :clisp (:corman :cormanlisp)
       (:cmu :cmucl :cmu) :clasp :ecl :gcl
       (:lwpe :lispworks-personal-edition) (:lw :lispworks)
       :mcl :mkcl :sbcl :scl (:smbx :symbolics) :xcl)))

  (defvar *implementation-type* (implementation-type)
    "The type of Lisp implementation used, as a short UIOP-standardized keyword")

  (defun operating-system ()
    "The operating system of the current host"
    (first-feature
     '(:cygwin
       (:win :windows :mswindows :win32 :mingw32) ;; try cygwin first!
       (:linux :linux :linux-target) ;; for GCL at least, must appear before :bsd
       (:macosx :macosx :darwin :darwin-target :apple) ; also before :bsd
       (:solaris :solaris :sunos)
       (:bsd :bsd :freebsd :netbsd :openbsd :dragonfly)
       :unix
       :genera)))

  (defun architecture ()
    "The CPU architecture of the current host"
    (first-feature
     '((:x64 :x86-64 :x86_64 :x8664-target :amd64 (:and :word-size=64 :pc386))
       (:x86 :x86 :i386 :i486 :i586 :i686 :pentium3 :pentium4 :pc386 :iapx386 :x8632-target)
       (:ppc64 :ppc64 :ppc64-target) (:ppc32 :ppc32 :ppc32-target :ppc :powerpc)
       :hppa64 :hppa :sparc64 (:sparc32 :sparc32 :sparc)
       :mipsel :mipseb :mips :alpha (:arm :arm :arm-target) :imach
       ;; Java comes last: if someone uses C via CFFI or otherwise JNA or JNI,
       ;; we may have to segregate the code still by architecture.
       (:java :java :java-1.4 :java-1.5 :java-1.6 :java-1.7))))

  #+clozure
  (defun ccl-fasl-version ()
    ;; the fasl version is target-dependent from CCL 1.8 on.
    (or (let ((s 'ccl::target-fasl-version))
          (and (fboundp s) (funcall s)))
        (and (boundp 'ccl::fasl-version)
             (symbol-value 'ccl::fasl-version))
        (error "Can't determine fasl version.")))

  (defun lisp-version-string ()
    "return a string that identifies the current Lisp implementation version"
    (let ((s (lisp-implementation-version)))
      (car ; as opposed to OR, this idiom prevents some unreachable code warning
       (list
        #+allegro
        (format nil "~A~@[~A~]~@[~A~]~@[~A~]"
                excl::*common-lisp-version-number*
                ;; M means "modern", as opposed to ANSI-compatible mode (which I consider default)
                (and (eq excl:*current-case-mode* :case-sensitive-lower) "M")
                ;; Note if not using International ACL
                ;; see http://www.franz.com/support/documentation/8.1/doc/operators/excl/ics-target-case.htm
                (excl:ics-target-case (:-ics "8"))
                (and (member :smp *features*) "S"))
        #+armedbear (format nil "~a-fasl~a" s system::*fasl-version*)
        #+clisp
        (subseq s 0 (position #\space s)) ; strip build information (date, etc.)
        #+clozure
        (format nil "~d.~d-f~d" ; shorten for windows
                ccl::*openmcl-major-version*
                ccl::*openmcl-minor-version*
                (logand (ccl-fasl-version) #xFF))
        #+cmucl (substitute #\- #\/ s)
        #+scl (format nil "~A~A" s
                      ;; ANSI upper case vs lower case.
                      (ecase ext:*case-mode* (:upper "") (:lower "l")))
        #+ecl (format nil "~A~@[-~A~]" s
                      (let ((vcs-id (ext:lisp-implementation-vcs-id)))
                        (unless (equal vcs-id "UNKNOWN")
                          (subseq vcs-id 0 (min (length vcs-id) 8)))))
        #+gcl (subseq s (1+ (position #\space s)))
        #+genera
        (multiple-value-bind (major minor) (sct:get-system-version "System")
          (format nil "~D.~D" major minor))
        #+mcl (subseq s 8) ; strip the leading "Version "
        ;; seems like there should be a shorter way to do this, like ACALL.
        #+mkcl (or
                (let ((fname (find-symbol* '#:git-describe-this-mkcl :mkcl nil)))
                  (when (and fname (fboundp fname))
                    (funcall fname)))
                s)
        s))))

  (defun implementation-identifier ()
    "Return a string that identifies the ABI of the current implementation,
suitable for use as a directory name to segregate Lisp FASLs, C dynamic libraries, etc."
    (substitute-if
     #\_ #'(lambda (x) (find x " /:;&^\\|?<>(){}[]$#`'\""))
     (format nil "~(~a~@{~@[-~a~]~}~)"
             (or (implementation-type) (lisp-implementation-type))
             (lisp-version-string)
             (or (operating-system) (software-type))
             (or (architecture) (machine-type))))))


;;;; Other system information

(with-upgradability ()
  (defun hostname ()
    "return the hostname of the current host"
    #+(or abcl clasp clozure cmucl ecl genera lispworks mcl mkcl sbcl scl xcl) (machine-instance)
    #+cormanlisp "localhost" ;; is there a better way? Does it matter?
    #+allegro (symbol-call :excl.osi :gethostname)
    #+clisp (first (split-string (machine-instance) :separator " "))
    #+gcl (system:gethostname)))


;;; Current directory
(with-upgradability ()

  #+cmucl
  (defun parse-unix-namestring* (unix-namestring)
    "variant of LISP::PARSE-UNIX-NAMESTRING that returns a pathname object"
    (multiple-value-bind (host device directory name type version)
        (lisp::parse-unix-namestring unix-namestring 0 (length unix-namestring))
      (make-pathname :host (or host lisp::*unix-host*) :device device
                     :directory directory :name name :type type :version version)))

  (defun getcwd ()
    "Get the current working directory as per POSIX getcwd(3), as a pathname object"
    (or #+(or abcl genera xcl) (truename *default-pathname-defaults*) ;; d-p-d is canonical!
        #+allegro (excl::current-directory)
        #+clisp (ext:default-directory)
        #+clozure (ccl:current-directory)
        #+(or cmucl scl) (#+cmucl parse-unix-namestring* #+scl lisp::parse-unix-namestring
                        (strcat (nth-value 1 (unix:unix-current-directory)) "/"))
        #+cormanlisp (pathname (pl::get-current-directory)) ;; Q: what type does it return?
        #+(or clasp ecl) (ext:getcwd)
        #+gcl (let ((*default-pathname-defaults* #p"")) (truename #p""))
        #+lispworks (hcl:get-working-directory)
        #+mkcl (mk-ext:getcwd)
        #+sbcl (sb-ext:parse-native-namestring (sb-unix:posix-getcwd/))
        #+xcl (extensions:current-directory)
        (not-implemented-error 'getcwd)))

  (defun chdir (x)
    "Change current directory, as per POSIX chdir(2), to a given pathname object"
    (if-let (x (pathname x))
      #+(or abcl genera xcl) (setf *default-pathname-defaults* (truename x)) ;; d-p-d is canonical!
      #+allegro (excl:chdir x)
      #+clisp (ext:cd x)
      #+clozure (setf (ccl:current-directory) x)
      #+(or cmucl scl) (unix:unix-chdir (ext:unix-namestring x))
      #+cormanlisp (unless (zerop (win32::_chdir (namestring x)))
                     (error "Could not set current directory to ~A" x))
      #+(or clasp ecl) (ext:chdir x)
      #+gcl (system:chdir x)
      #+lispworks (hcl:change-directory x)
      #+mkcl (mk-ext:chdir x)
      #+sbcl (progn (require :sb-posix) (symbol-call :sb-posix :chdir (sb-ext:native-namestring x)))
      #-(or abcl allegro clasp clisp clozure cmucl cormanlisp ecl gcl genera lispworks mkcl sbcl scl xcl)
      (not-implemented-error 'chdir))))


;;;; -----------------------------------------------------------------
;;;; Windows shortcut support.  Based on:
;;;;
;;;; Jesse Hager: The Windows Shortcut File Format.
;;;; http://www.wotsit.org/list.asp?fc=13

#-(or clisp genera) ; CLISP doesn't need it, and READ-SEQUENCE annoys old Genera that doesn't need it
(with-upgradability ()
  (defparameter *link-initial-dword* 76)
  (defparameter *link-guid* #(1 20 2 0 0 0 0 0 192 0 0 0 0 0 0 70))

  (defun read-null-terminated-string (s)
    "Read a null-terminated string from an octet stream S"
    ;; note: doesn't play well with UNICODE
    (with-output-to-string (out)
      (loop :for code = (read-byte s)
            :until (zerop code)
            :do (write-char (code-char code) out))))

  (defun read-little-endian (s &optional (bytes 4))
    "Read a number in little-endian format from an byte (octet) stream S,
the number having BYTES octets (defaulting to 4)."
    (loop :for i :from 0 :below bytes
          :sum (ash (read-byte s) (* 8 i))))

  (defun parse-file-location-info (s)
    "helper to parse-windows-shortcut"
    (let ((start (file-position s))
          (total-length (read-little-endian s))
          (end-of-header (read-little-endian s))
          (fli-flags (read-little-endian s))
          (local-volume-offset (read-little-endian s))
          (local-offset (read-little-endian s))
          (network-volume-offset (read-little-endian s))
          (remaining-offset (read-little-endian s)))
      (declare (ignore total-length end-of-header local-volume-offset))
      (unless (zerop fli-flags)
        (cond
          ((logbitp 0 fli-flags)
           (file-position s (+ start local-offset)))
          ((logbitp 1 fli-flags)
           (file-position s (+ start
                               network-volume-offset
                               #x14))))
        (strcat (read-null-terminated-string s)
                (progn
                  (file-position s (+ start remaining-offset))
                  (read-null-terminated-string s))))))

  (defun parse-windows-shortcut (pathname)
    "From a .lnk windows shortcut, extract the pathname linked to"
    ;; NB: doesn't do much checking & doesn't look like it will work well with UNICODE.
    (with-open-file (s pathname :element-type '(unsigned-byte 8))
      (handler-case
          (when (and (= (read-little-endian s) *link-initial-dword*)
                     (let ((header (make-array (length *link-guid*))))
                       (read-sequence header s)
                       (equalp header *link-guid*)))
            (let ((flags (read-little-endian s)))
              (file-position s 76)        ;skip rest of header
              (when (logbitp 0 flags)
                ;; skip shell item id list
                (let ((length (read-little-endian s 2)))
                  (file-position s (+ length (file-position s)))))
              (cond
                ((logbitp 1 flags)
                 (parse-file-location-info s))
                (t
                 (when (logbitp 2 flags)
                   ;; skip description string
                   (let ((length (read-little-endian s 2)))
                     (file-position s (+ length (file-position s)))))
                 (when (logbitp 3 flags)
                   ;; finally, our pathname
                   (let* ((length (read-little-endian s 2))
                          (buffer (make-array length)))
                     (read-sequence buffer s)
                     (map 'string #'code-char buffer)))))))
        (end-of-file (c)
          (declare (ignore c))
          nil)))))


;;;; -------------------------------------------------------------------------
;;;; Portability layer around Common Lisp pathnames
;; This layer allows for portable manipulation of pathname objects themselves,
;; which all is necessary prior to any access the filesystem or environment.

(uiop/package:define-package :uiop/pathname
  (:nicknames :asdf/pathname) ;; deprecated. Used by ceramic
  (:use :uiop/common-lisp :uiop/package :uiop/utility :uiop/os)
  (:export
   ;; Making and merging pathnames, portably
   #:normalize-pathname-directory-component #:denormalize-pathname-directory-component
   #:merge-pathname-directory-components #:*unspecific-pathname-type* #:make-pathname*
   #:make-pathname-component-logical #:make-pathname-logical
   #:merge-pathnames*
   #:nil-pathname #:*nil-pathname* #:with-pathname-defaults
   ;; Predicates
   #:pathname-equal #:logical-pathname-p #:physical-pathname-p #:physicalize-pathname
   #:absolute-pathname-p #:relative-pathname-p #:hidden-pathname-p #:file-pathname-p
   ;; Directories
   #:pathname-directory-pathname #:pathname-parent-directory-pathname
   #:directory-pathname-p #:ensure-directory-pathname
   ;; Parsing filenames
   #:split-name-type #:parse-unix-namestring #:unix-namestring
   #:split-unix-namestring-directory-components
   ;; Absolute and relative pathnames
   #:subpathname #:subpathname*
   #:ensure-absolute-pathname
   #:pathname-root #:pathname-host-pathname
   #:subpathp #:enough-pathname #:with-enough-pathname #:call-with-enough-pathname
   ;; Checking constraints
   #:ensure-pathname ;; implemented in filesystem.lisp to accommodate for existence constraints
   ;; Wildcard pathnames
   #:*wild* #:*wild-file* #:*wild-file-for-directory* #:*wild-directory*
   #:*wild-inferiors* #:*wild-path* #:wilden
   ;; Translate a pathname
   #:relativize-directory-component #:relativize-pathname-directory
   #:directory-separator-for-host #:directorize-pathname-host-device
   #:translate-pathname*
   #:*output-translation-function*))
(in-package :uiop/pathname)

;;; Normalizing pathnames across implementations

(with-upgradability ()
  (defun normalize-pathname-directory-component (directory)
    "Convert the DIRECTORY component from a format usable by the underlying
implementation's MAKE-PATHNAME and other primitives to a CLHS-standard format
that is a list and not a string."
    (cond
      #-(or cmucl sbcl scl) ;; these implementations already normalize directory components.
      ((stringp directory) `(:absolute ,directory))
      ((or (null directory)
           (and (consp directory) (member (first directory) '(:absolute :relative))))
       directory)
      #+gcl
      ((consp directory)
       (cons :relative directory))
      (t
       (parameter-error (compatfmt "~@<~S: Unrecognized pathname directory component ~S~@:>")
                        'normalize-pathname-directory-component directory))))

  (defun denormalize-pathname-directory-component (directory-component)
    "Convert the DIRECTORY-COMPONENT from a CLHS-standard format to a format usable
by the underlying implementation's MAKE-PATHNAME and other primitives"
    directory-component)

  (defun merge-pathname-directory-components (specified defaults)
    "Helper for MERGE-PATHNAMES* that handles directory components"
    (let ((directory (normalize-pathname-directory-component specified)))
      (ecase (first directory)
        ((nil) defaults)
        (:absolute specified)
        (:relative
         (let ((defdir (normalize-pathname-directory-component defaults))
               (reldir (cdr directory)))
           (cond
             ((null defdir)
              directory)
             ((not (eq :back (first reldir)))
              (append defdir reldir))
             (t
              (loop :with defabs = (first defdir)
                    :with defrev = (reverse (rest defdir))
                    :while (and (eq :back (car reldir))
                                (or (and (eq :absolute defabs) (null defrev))
                                    (stringp (car defrev))))
                    :do (pop reldir) (pop defrev)
                    :finally (return (cons defabs (append (reverse defrev) reldir)))))))))))

  ;; Giving :unspecific as :type argument to make-pathname is not portable.
  ;; See CLHS make-pathname and 19.2.2.2.3.
  ;; This will be :unspecific if supported, or NIL if not.
  (defparameter *unspecific-pathname-type*
    #+(or abcl allegro clozure cmucl genera lispworks sbcl scl) :unspecific
    #+(or clasp clisp ecl mkcl gcl xcl #|These haven't been tested:|# cormanlisp mcl) nil
    "Unspecific type component to use with the underlying implementation's MAKE-PATHNAME")

  (defun make-pathname* (&rest keys &key directory host device name type version defaults
                                      #+scl &allow-other-keys)
    "Takes arguments like CL:MAKE-PATHNAME in the CLHS, and
   tries hard to make a pathname that will actually behave as documented,
   despite the peculiarities of each implementation. DEPRECATED: just use MAKE-PATHNAME."
    (declare (ignore host device directory name type version defaults))
    (apply 'make-pathname keys))

  (defun make-pathname-component-logical (x)
    "Make a pathname component suitable for use in a logical-pathname"
    (typecase x
      ((eql :unspecific) nil)
      #+clisp (string (string-upcase x))
      #+clisp (cons (mapcar 'make-pathname-component-logical x))
      (t x)))

  (defun make-pathname-logical (pathname host)
    "Take a PATHNAME's directory, name, type and version components,
and make a new pathname with corresponding components and specified logical HOST"
    (make-pathname
     :host host
     :directory (make-pathname-component-logical (pathname-directory pathname))
     :name (make-pathname-component-logical (pathname-name pathname))
     :type (make-pathname-component-logical (pathname-type pathname))
     :version (make-pathname-component-logical (pathname-version pathname))))

  (defun merge-pathnames* (specified &optional (defaults *default-pathname-defaults*))
    "MERGE-PATHNAMES* is like MERGE-PATHNAMES except that
if the SPECIFIED pathname does not have an absolute directory,
then the HOST and DEVICE both come from the DEFAULTS, whereas
if the SPECIFIED pathname does have an absolute directory,
then the HOST and DEVICE both come from the SPECIFIED pathname.
This is what users want on a modern Unix or Windows operating system,
unlike the MERGE-PATHNAMES behavior.
Also, if either argument is NIL, then the other argument is returned unmodified;
this is unlike MERGE-PATHNAMES which always merges with a pathname,
by default *DEFAULT-PATHNAME-DEFAULTS*, which cannot be NIL."
    (when (null specified) (return-from merge-pathnames* defaults))
    (when (null defaults) (return-from merge-pathnames* specified))
    #+scl
    (ext:resolve-pathname specified defaults)
    #-scl
    (let* ((specified (pathname specified))
           (defaults (pathname defaults))
           (directory (normalize-pathname-directory-component (pathname-directory specified)))
           (name (or (pathname-name specified) (pathname-name defaults)))
           (type (or (pathname-type specified) (pathname-type defaults)))
           (version (or (pathname-version specified) (pathname-version defaults))))
      (labels ((unspecific-handler (p)
                 (if (typep p 'logical-pathname) #'make-pathname-component-logical #'identity)))
        (multiple-value-bind (host device directory unspecific-handler)
            (ecase (first directory)
              ((:absolute)
               (values (pathname-host specified)
                       (pathname-device specified)
                       directory
                       (unspecific-handler specified)))
              ((nil :relative)
               (values (pathname-host defaults)
                       (pathname-device defaults)
                       (merge-pathname-directory-components directory (pathname-directory defaults))
                       (unspecific-handler defaults))))
          (make-pathname :host host :device device :directory directory
                         :name (funcall unspecific-handler name)
                         :type (funcall unspecific-handler type)
                         :version (funcall unspecific-handler version))))))

  (defun logical-pathname-p (x)
    "is X a logical-pathname?"
    (typep x 'logical-pathname))

  (defun physical-pathname-p (x)
    "is X a pathname that is not a logical-pathname?"
    (and (pathnamep x) (not (logical-pathname-p x))))

  (defun physicalize-pathname (x)
    "if X is a logical pathname, use translate-logical-pathname on it."
    ;; Ought to be the same as translate-logical-pathname, except the latter borks on CLISP
    (let ((p (when x (pathname x))))
      (if (logical-pathname-p p) (translate-logical-pathname p) p)))

  (defun nil-pathname (&optional (defaults *default-pathname-defaults*))
    "A pathname that is as neutral as possible for use as defaults
when merging, making or parsing pathnames"
    ;; 19.2.2.2.1 says a NIL host can mean a default host;
    ;; see also "valid physical pathname host" in the CLHS glossary, that suggests
    ;; strings and lists of strings or :unspecific
    ;; But CMUCL decides to die on NIL.
    ;; MCL has issues with make-pathname, nil and defaulting
    (declare (ignorable defaults))
    #.`(make-pathname :directory nil :name nil :type nil :version nil
                      :device (or #+(and mkcl os-unix) :unspecific)
                      :host (or #+cmucl lisp::*unix-host* #+(and mkcl os-unix) "localhost")
                      #+scl ,@'(:scheme nil :scheme-specific-part nil
                                :username nil :password nil :parameters nil :query nil :fragment nil)
                      ;; the default shouldn't matter, but we really want something physical
                      #-mcl ,@'(:defaults defaults)))

  (defvar *nil-pathname* (nil-pathname (physicalize-pathname (user-homedir-pathname)))
    "A pathname that is as neutral as possible for use as defaults
when merging, making or parsing pathnames")

  (defmacro with-pathname-defaults ((&optional defaults) &body body)
    "Execute BODY in a context where the *DEFAULT-PATHNAME-DEFAULTS* is as specified,
where leaving the defaults NIL or unspecified means a (NIL-PATHNAME), except
on ABCL, Genera and XCL, where it remains unchanged for it doubles as current-directory."
    `(let ((*default-pathname-defaults*
             ,(or defaults
                  #-(or abcl genera xcl) '*nil-pathname*
                  #+(or abcl genera xcl) '*default-pathname-defaults*)))
       ,@body)))


;;; Some pathname predicates
(with-upgradability ()
  (defun pathname-equal (p1 p2)
    "Are the two pathnames P1 and P2 reasonably equal in the paths they denote?"
    (when (stringp p1) (setf p1 (pathname p1)))
    (when (stringp p2) (setf p2 (pathname p2)))
    (flet ((normalize-component (x)
             (unless (member x '(nil :unspecific :newest (:relative)) :test 'equal)
               x)))
      (macrolet ((=? (&rest accessors)
                   (flet ((frob (x)
                            (reduce 'list (cons 'normalize-component accessors)
                                    :initial-value x :from-end t)))
                     `(equal ,(frob 'p1) ,(frob 'p2)))))
        (or (and (null p1) (null p2))
            (and (pathnamep p1) (pathnamep p2)
                 (and (=? pathname-host)
                      #-(and mkcl os-unix) (=? pathname-device)
                      (=? normalize-pathname-directory-component pathname-directory)
                      (=? pathname-name)
                      (=? pathname-type)
                      #-mkcl (=? pathname-version)))))))

  (defun absolute-pathname-p (pathspec)
    "If PATHSPEC is a pathname or namestring object that parses as a pathname
possessing an :ABSOLUTE directory component, return the (parsed) pathname.
Otherwise return NIL"
    (and pathspec
         (typep pathspec '(or null pathname string))
         (let ((pathname (pathname pathspec)))
           (and (eq :absolute (car (normalize-pathname-directory-component
                                    (pathname-directory pathname))))
                pathname))))

  (defun relative-pathname-p (pathspec)
    "If PATHSPEC is a pathname or namestring object that parses as a pathname
possessing a :RELATIVE or NIL directory component, return the (parsed) pathname.
Otherwise return NIL"
    (and pathspec
         (typep pathspec '(or null pathname string))
         (let* ((pathname (pathname pathspec))
                (directory (normalize-pathname-directory-component
                            (pathname-directory pathname))))
           (when (or (null directory) (eq :relative (car directory)))
             pathname))))

  (defun hidden-pathname-p (pathname)
    "Return a boolean that is true if the pathname is hidden as per Unix style,
i.e. its name starts with a dot."
    (and pathname (equal (first-char (pathname-name pathname)) #\.)))

  (defun file-pathname-p (pathname)
    "Does PATHNAME represent a file, i.e. has a non-null NAME component?

Accepts NIL, a string (converted through PARSE-NAMESTRING) or a PATHNAME.

Note that this does _not_ check to see that PATHNAME points to an
actually-existing file.

Returns the (parsed) PATHNAME when true"
    (when pathname
      (let ((pathname (pathname pathname)))
        (unless (and (member (pathname-name pathname) '(nil :unspecific "") :test 'equal)
                     (member (pathname-type pathname) '(nil :unspecific "") :test 'equal))
          pathname)))))


;;; Directory pathnames
(with-upgradability ()
  (defun pathname-directory-pathname (pathname)
    "Returns a new pathname with same HOST, DEVICE, DIRECTORY as PATHNAME,
and NIL NAME, TYPE and VERSION components"
    (when pathname
      (make-pathname :name nil :type nil :version nil :defaults pathname)))

  (defun pathname-parent-directory-pathname (pathname)
    "Returns a new pathname that corresponds to the parent of the current pathname's directory,
i.e. removing one level of depth in the DIRECTORY component. e.g. if pathname is
Unix pathname /foo/bar/baz/file.type then return /foo/bar/"
    (when pathname
      (make-pathname :name nil :type nil :version nil
                     :directory (merge-pathname-directory-components
                                 '(:relative :back) (pathname-directory pathname))
                     :defaults pathname)))

  (defun directory-pathname-p (pathname)
    "Does PATHNAME represent a directory?

A directory-pathname is a pathname _without_ a filename. The three
ways that the filename components can be missing are for it to be NIL,
:UNSPECIFIC or the empty string.

Note that this does _not_ check to see that PATHNAME points to an
actually-existing directory."
    (when pathname
      ;; I tried using Allegro's excl:file-directory-p, but this cannot be done,
      ;; because it rejects apparently legal pathnames as
      ;; ill-formed. [2014/02/10:rpg]
      (let ((pathname (pathname pathname)))
        (flet ((check-one (x)
                 (member x '(nil :unspecific) :test 'equal)))
          (and (not (wild-pathname-p pathname))
               (check-one (pathname-name pathname))
               (check-one (pathname-type pathname))
               t)))))

  (defun ensure-directory-pathname (pathspec &optional (on-error 'error))
    "Converts the non-wild pathname designator PATHSPEC to directory form."
    (cond
      ((stringp pathspec)
       (ensure-directory-pathname (pathname pathspec)))
      ((not (pathnamep pathspec))
       (call-function on-error (compatfmt "~@<Invalid pathname designator ~S~@:>") pathspec))
      ((wild-pathname-p pathspec)
       (call-function on-error (compatfmt "~@<Can't reliably convert wild pathname ~3i~_~S~@:>") pathspec))
      ((directory-pathname-p pathspec)
       pathspec)
      (t
       (handler-case
           (make-pathname :directory (append (or (normalize-pathname-directory-component
                                                  (pathname-directory pathspec))
                                                 (list :relative))
                                             (list (file-namestring pathspec)))
                          :name nil :type nil :version nil :defaults pathspec)
         (error (c) (call-function on-error (compatfmt "~@<error while trying to create a directory pathname for ~S: ~A~@:>") pathspec c)))))))


;;; Parsing filenames
(with-upgradability ()
  (declaim (ftype function ensure-pathname)) ; forward reference

  (defun split-unix-namestring-directory-components
      (unix-namestring &key ensure-directory dot-dot)
    "Splits the path string UNIX-NAMESTRING, returning four values:
A flag that is either :absolute or :relative, indicating
   how the rest of the values are to be interpreted.
A directory path --- a list of strings and keywords, suitable for
   use with MAKE-PATHNAME when prepended with the flag value.
   Directory components with an empty name or the name . are removed.
   Any directory named .. is read as DOT-DOT, or :BACK if it's NIL (not :UP).
A last-component, either a file-namestring including type extension,
   or NIL in the case of a directory pathname.
A flag that is true iff the unix-style-pathname was just
   a file-namestring without / path specification.
ENSURE-DIRECTORY forces the namestring to be interpreted as a directory pathname:
the third return value will be NIL, and final component of the namestring
will be treated as part of the directory path.

An empty string is thus read as meaning a pathname object with all fields nil.

Note that colon characters #\: will NOT be interpreted as host specification.
Absolute pathnames are only appropriate on Unix-style systems.

The intention of this function is to support structured component names,
e.g., \(:file \"foo/bar\"\), which will be unpacked to relative pathnames."
    (check-type unix-namestring string)
    (check-type dot-dot (member nil :back :up))
    (if (and (not (find #\/ unix-namestring)) (not ensure-directory)
             (plusp (length unix-namestring)))
        (values :relative () unix-namestring t)
        (let* ((components (split-string unix-namestring :separator "/"))
               (last-comp (car (last components))))
          (multiple-value-bind (relative components)
              (if (equal (first components) "")
                  (if (equal (first-char unix-namestring) #\/)
                      (values :absolute (cdr components))
                      (values :relative nil))
                  (values :relative components))
            (setf components (remove-if #'(lambda (x) (member x '("" ".") :test #'equal))
                                        components))
            (setf components (substitute (or dot-dot :back) ".." components :test #'equal))
            (cond
              ((equal last-comp "")
               (values relative components nil nil)) ; "" already removed from components
              (ensure-directory
               (values relative components nil nil))
              (t
               (values relative (butlast components) last-comp nil)))))))

  (defun split-name-type (filename)
    "Split a filename into two values NAME and TYPE that are returned.
We assume filename has no directory component.
The last . if any separates name and type from from type,
except that if there is only one . and it is in first position,
the whole filename is the NAME with an empty type.
NAME is always a string.
For an empty type, *UNSPECIFIC-PATHNAME-TYPE* is returned."
    (check-type filename string)
    (assert (plusp (length filename)))
    (destructuring-bind (name &optional (type *unspecific-pathname-type*))
        (split-string filename :max 2 :separator ".")
      (if (equal name "")
          (values filename *unspecific-pathname-type*)
          (values name type))))

  (defun parse-unix-namestring (name &rest keys &key type defaults dot-dot ensure-directory
                                &allow-other-keys)
    "Coerce NAME into a PATHNAME using standard Unix syntax.

Unix syntax is used whether or not the underlying system is Unix;
on such non-Unix systems it is reliably usable only for relative pathnames.
This function is especially useful to manipulate relative pathnames portably,
where it is of crucial to possess a portable pathname syntax independent of the underlying OS.
This is what PARSE-UNIX-NAMESTRING provides, and why we use it in ASDF.

When given a PATHNAME object, just return it untouched.
When given NIL, just return NIL.
When given a non-null SYMBOL, first downcase its name and treat it as a string.
When given a STRING, portably decompose it into a pathname as below.

#\\/ separates directory components.

The last #\\/-separated substring is interpreted as follows:
1- If TYPE is :DIRECTORY or ENSURE-DIRECTORY is true,
 the string is made the last directory component, and NAME and TYPE are NIL.
 if the string is empty, it's the empty pathname with all slots NIL.
2- If TYPE is NIL, the substring is a file-namestring, and its NAME and TYPE
 are separated by SPLIT-NAME-TYPE.
3- If TYPE is a string, it is the given TYPE, and the whole string is the NAME.

Directory components with an empty name or the name \".\" are removed.
Any directory named \"..\" is read as DOT-DOT,
which must be one of :BACK or :UP and defaults to :BACK.

HOST, DEVICE and VERSION components are taken from DEFAULTS,
which itself defaults to *NIL-PATHNAME*, also used if DEFAULTS is NIL.
No host or device can be specified in the string itself,
which makes it unsuitable for absolute pathnames outside Unix.

For relative pathnames, these components (and hence the defaults) won't matter
if you use MERGE-PATHNAMES* but will matter if you use MERGE-PATHNAMES,
which is an important reason to always use MERGE-PATHNAMES*.

Arbitrary keys are accepted, and the parse result is passed to ENSURE-PATHNAME
with those keys, removing TYPE DEFAULTS and DOT-DOT.
When you're manipulating pathnames that are supposed to make sense portably
even though the OS may not be Unixish, we recommend you use :WANT-RELATIVE T
to throw an error if the pathname is absolute"
    (block nil
      (check-type type (or null string (eql :directory)))
      (when ensure-directory
        (setf type :directory))
      (etypecase name
        ((or null pathname) (return name))
        (symbol
         (setf name (string-downcase name)))
        (string))
      (multiple-value-bind (relative path filename file-only)
          (split-unix-namestring-directory-components
           name :dot-dot dot-dot :ensure-directory (eq type :directory))
        (multiple-value-bind (name type)
            (cond
              ((or (eq type :directory) (null filename))
               (values nil nil))
              (type
               (values filename type))
              (t
               (split-name-type filename)))
          (apply 'ensure-pathname
                 (make-pathname
                  :directory (unless file-only (cons relative path))
                  :name name :type type
                  :defaults (or #-mcl defaults *nil-pathname*))
                 (remove-plist-keys '(:type :dot-dot :defaults) keys))))))

  (defun unix-namestring (pathname)
    "Given a non-wild PATHNAME, return a Unix-style namestring for it.
If the PATHNAME is NIL or a STRING, return it unchanged.

This only considers the DIRECTORY, NAME and TYPE components of the pathname.
This is a portable solution for representing relative pathnames,
But unless you are running on a Unix system, it is not a general solution
to representing native pathnames.

An error is signaled if the argument is not NULL, a STRING or a PATHNAME,
or if it is a PATHNAME but some of its components are not recognized."
    (etypecase pathname
      ((or null string) pathname)
      (pathname
       (with-output-to-string (s)
         (flet ((err () (parameter-error "~S: invalid unix-namestring ~S"
                                         'unix-namestring pathname)))
           (let* ((dir (normalize-pathname-directory-component (pathname-directory pathname)))
                  (name (pathname-name pathname))
                  (name (and (not (eq name :unspecific)) name))
                  (type (pathname-type pathname))
                  (type (and (not (eq type :unspecific)) type)))
             (cond
               ((member dir '(nil :unspecific)))
               ((eq dir '(:relative)) (princ "./" s))
               ((consp dir)
                (destructuring-bind (relabs &rest dirs) dir
                  (or (member relabs '(:relative :absolute)) (err))
                  (when (eq relabs :absolute) (princ #\/ s))
                  (loop :for x :in dirs :do
                    (cond
                      ((member x '(:back :up)) (princ "../" s))
                      ((equal x "") (err))
                      ;;((member x '("." "..") :test 'equal) (err))
                      ((stringp x) (format s "~A/" x))
                      (t (err))))))
               (t (err)))
             (cond
               (name
                (unless (and (stringp name) (or (null type) (stringp type))) (err))
                (format s "~A~@[.~A~]" name type))
               (t
                (or (null type) (err)))))))))))

;;; Absolute and relative pathnames
(with-upgradability ()
  (defun subpathname (pathname subpath &key type)
    "This function takes a PATHNAME and a SUBPATH and a TYPE.
If SUBPATH is already a PATHNAME object (not namestring),
and is an absolute pathname at that, it is returned unchanged;
otherwise, SUBPATH is turned into a relative pathname with given TYPE
as per PARSE-UNIX-NAMESTRING with :WANT-RELATIVE T :TYPE TYPE,
then it is merged with the PATHNAME-DIRECTORY-PATHNAME of PATHNAME."
    (or (and (pathnamep subpath) (absolute-pathname-p subpath))
        (merge-pathnames* (parse-unix-namestring subpath :type type :want-relative t)
                          (pathname-directory-pathname pathname))))

  (defun subpathname* (pathname subpath &key type)
    "returns NIL if the base pathname is NIL, otherwise like SUBPATHNAME."
    (and pathname
         (subpathname (ensure-directory-pathname pathname) subpath :type type)))

  (defun pathname-root (pathname)
    "return the root directory for the host and device of given PATHNAME"
    (make-pathname :directory '(:absolute)
                   :name nil :type nil :version nil
                   :defaults pathname ;; host device, and on scl, *some*
                   ;; scheme-specific parts: port username password, not others:
                   . #.(or #+scl '(:parameters nil :query nil :fragment nil))))

  (defun pathname-host-pathname (pathname)
    "return a pathname with the same host as given PATHNAME, and all other fields NIL"
    (make-pathname :directory nil
                   :name nil :type nil :version nil :device nil
                   :defaults pathname ;; host device, and on scl, *some*
                   ;; scheme-specific parts: port username password, not others:
                   . #.(or #+scl '(:parameters nil :query nil :fragment nil))))

  (defun ensure-absolute-pathname (path &optional defaults (on-error 'error))
    "Given a pathname designator PATH, return an absolute pathname as specified by PATH
considering the DEFAULTS, or, if not possible, use CALL-FUNCTION on the specified ON-ERROR behavior,
with a format control-string and other arguments as arguments"
    (cond
      ((absolute-pathname-p path))
      ((stringp path) (ensure-absolute-pathname (pathname path) defaults on-error))
      ((not (pathnamep path)) (call-function on-error "not a valid pathname designator ~S" path))
      ((let ((default-pathname (if (pathnamep defaults) defaults (call-function defaults))))
         (or (if (absolute-pathname-p default-pathname)
                 (absolute-pathname-p (merge-pathnames* path default-pathname))
                 (call-function on-error "Default pathname ~S is not an absolute pathname"
                                default-pathname))
             (call-function on-error "Failed to merge ~S with ~S into an absolute pathname"
                            path default-pathname))))
      (t (call-function on-error
                        "Cannot ensure ~S is evaluated as an absolute pathname with defaults ~S"
                        path defaults))))

  (defun subpathp (maybe-subpath base-pathname)
    "if MAYBE-SUBPATH is a pathname that is under BASE-PATHNAME, return a pathname object that
when used with MERGE-PATHNAMES* with defaults BASE-PATHNAME, returns MAYBE-SUBPATH."
    (and (pathnamep maybe-subpath) (pathnamep base-pathname)
         (absolute-pathname-p maybe-subpath) (absolute-pathname-p base-pathname)
         (directory-pathname-p base-pathname) (not (wild-pathname-p base-pathname))
         (pathname-equal (pathname-root maybe-subpath) (pathname-root base-pathname))
         (with-pathname-defaults (*nil-pathname*)
           (let ((enough (enough-namestring maybe-subpath base-pathname)))
             (and (relative-pathname-p enough) (pathname enough))))))

  (defun enough-pathname (maybe-subpath base-pathname)
    "if MAYBE-SUBPATH is a pathname that is under BASE-PATHNAME, return a pathname object that
when used with MERGE-PATHNAMES* with defaults BASE-PATHNAME, returns MAYBE-SUBPATH."
    (let ((sub (when maybe-subpath (pathname maybe-subpath)))
          (base (when base-pathname (ensure-absolute-pathname (pathname base-pathname)))))
      (or (and base (subpathp sub base)) sub)))

  (defun call-with-enough-pathname (maybe-subpath defaults-pathname thunk)
    "In a context where *DEFAULT-PATHNAME-DEFAULTS* is bound to DEFAULTS-PATHNAME (if not null,
or else to its current value), call THUNK with ENOUGH-PATHNAME for MAYBE-SUBPATH
given DEFAULTS-PATHNAME as a base pathname."
    (let ((enough (enough-pathname maybe-subpath defaults-pathname))
          (*default-pathname-defaults* (or defaults-pathname *default-pathname-defaults*)))
      (funcall thunk enough)))

  (defmacro with-enough-pathname ((pathname-var &key (pathname pathname-var)
                                                  (defaults *default-pathname-defaults*))
                                  &body body)
    "Shorthand syntax for CALL-WITH-ENOUGH-PATHNAME"
    `(call-with-enough-pathname ,pathname ,defaults #'(lambda (,pathname-var) ,@body))))


;;; Wildcard pathnames
(with-upgradability ()
  (defparameter *wild* (or #+cormanlisp "*" :wild)
    "Wild component for use with MAKE-PATHNAME")
  (defparameter *wild-directory-component* (or :wild)
    "Wild directory component for use with MAKE-PATHNAME")
  (defparameter *wild-inferiors-component* (or :wild-inferiors)
    "Wild-inferiors directory component for use with MAKE-PATHNAME")
  (defparameter *wild-file*
    (make-pathname :directory nil :name *wild* :type *wild*
                   :version (or #-(or allegro abcl xcl) *wild*))
    "A pathname object with wildcards for matching any file with TRANSLATE-PATHNAME")
  (defparameter *wild-file-for-directory*
    (make-pathname :directory nil :name *wild* :type (or #-(or clisp gcl) *wild*)
                   :version (or #-(or allegro abcl clisp gcl xcl) *wild*))
    "A pathname object with wildcards for matching any file with DIRECTORY")
  (defparameter *wild-directory*
    (make-pathname :directory `(:relative ,*wild-directory-component*)
                   :name nil :type nil :version nil)
    "A pathname object with wildcards for matching any subdirectory")
  (defparameter *wild-inferiors*
    (make-pathname :directory `(:relative ,*wild-inferiors-component*)
                   :name nil :type nil :version nil)
    "A pathname object with wildcards for matching any recursive subdirectory")
  (defparameter *wild-path*
    (merge-pathnames* *wild-file* *wild-inferiors*)
    "A pathname object with wildcards for matching any file in any recursive subdirectory")

  (defun wilden (path)
    "From a pathname, return a wildcard pathname matching any file in any subdirectory of given pathname's directory"
    (merge-pathnames* *wild-path* path)))


;;; Translate a pathname
(with-upgradability ()
  (defun relativize-directory-component (directory-component)
    "Given the DIRECTORY-COMPONENT of a pathname, return an otherwise similar relative directory component"
    (let ((directory (normalize-pathname-directory-component directory-component)))
      (cond
        ((stringp directory)
         (list :relative directory))
        ((eq (car directory) :absolute)
         (cons :relative (cdr directory)))
        (t
         directory))))

  (defun relativize-pathname-directory (pathspec)
    "Given a PATHNAME, return a relative pathname with otherwise the same components"
    (let ((p (pathname pathspec)))
      (make-pathname
       :directory (relativize-directory-component (pathname-directory p))
       :defaults p)))

  (defun directory-separator-for-host (&optional (pathname *default-pathname-defaults*))
    "Given a PATHNAME, return the character used to delimit directory names on this host and device."
    (let ((foo (make-pathname :directory '(:absolute "FOO") :defaults pathname)))
      (last-char (namestring foo))))

  #-scl
  (defun directorize-pathname-host-device (pathname)
    "Given a PATHNAME, return a pathname that has representations of its HOST and DEVICE components
added to its DIRECTORY component. This is useful for output translations."
    (os-cond
     ((os-unix-p)
      (when (physical-pathname-p pathname)
        (return-from directorize-pathname-host-device pathname))))
    (let* ((root (pathname-root pathname))
           (wild-root (wilden root))
           (absolute-pathname (merge-pathnames* pathname root))
           (separator (directory-separator-for-host root))
           (root-namestring (namestring root))
           (root-string
             (substitute-if #\/
                            #'(lambda (x) (or (eql x #\:)
                                              (eql x separator)))
                            root-namestring)))
      (multiple-value-bind (relative path filename)
          (split-unix-namestring-directory-components root-string :ensure-directory t)
        (declare (ignore relative filename))
        (let ((new-base (make-pathname :defaults root :directory `(:absolute ,@path))))
          (translate-pathname absolute-pathname wild-root (wilden new-base))))))

  #+scl
  (defun directorize-pathname-host-device (pathname)
    (let ((scheme (ext:pathname-scheme pathname))
          (host (pathname-host pathname))
          (port (ext:pathname-port pathname))
          (directory (pathname-directory pathname)))
      (flet ((specificp (x) (and x (not (eq x :unspecific)))))
        (if (or (specificp port)
                (and (specificp host) (plusp (length host)))
                (specificp scheme))
            (let ((prefix ""))
              (when (specificp port)
                (setf prefix (format nil ":~D" port)))
              (when (and (specificp host) (plusp (length host)))
                (setf prefix (strcat host prefix)))
              (setf prefix (strcat ":" prefix))
              (when (specificp scheme)
                (setf prefix (strcat scheme prefix)))
              (assert (and directory (eq (first directory) :absolute)))
              (make-pathname :directory `(:absolute ,prefix ,@(rest directory))
                             :defaults pathname)))
        pathname)))

  (defun* (translate-pathname*) (path absolute-source destination &optional root source)
    "A wrapper around TRANSLATE-PATHNAME to be used by the ASDF output-translations facility.
PATH is the pathname to be translated.
ABSOLUTE-SOURCE is an absolute pathname to use as source for translate-pathname,
DESTINATION is either a function, to be called with PATH and ABSOLUTE-SOURCE,
or a relative pathname, to be merged with ROOT and used as destination for translate-pathname
or an absolute pathname, to be used as destination for translate-pathname.
In that last case, if ROOT is non-NIL, PATH is first transformated by DIRECTORIZE-PATHNAME-HOST-DEVICE."
    (declare (ignore source))
    (cond
      ((functionp destination)
       (funcall destination path absolute-source))
      ((eq destination t)
       path)
      ((not (pathnamep destination))
       (parameter-error "~S: Invalid destination" 'translate-pathname*))
      ((not (absolute-pathname-p destination))
       (translate-pathname path absolute-source (merge-pathnames* destination root)))
      (root
       (translate-pathname (directorize-pathname-host-device path) absolute-source destination))
      (t
       (translate-pathname path absolute-source destination))))

  (defvar *output-translation-function* 'identity
    "Hook for output translations.

This function needs to be idempotent, so that actions can work
whether their inputs were translated or not,
which they will be if we are composing operations. e.g. if some
create-lisp-op creates a lisp file from some higher-level input,
you need to still be able to use compile-op on that lisp file."))
;;;; -------------------------------------------------------------------------
;;;; Portability layer around Common Lisp filesystem access

(uiop/package:define-package :uiop/filesystem
  (:use :uiop/common-lisp :uiop/package :uiop/utility :uiop/os :uiop/pathname)
  (:export
   ;; Native namestrings
   #:native-namestring #:parse-native-namestring
   ;; Probing the filesystem
   #:truename* #:safe-file-write-date #:probe-file* #:directory-exists-p #:file-exists-p
   #:directory* #:filter-logical-directory-results #:directory-files #:subdirectories
   #:collect-sub*directories
   ;; Resolving symlinks somewhat
   #:truenamize #:resolve-symlinks #:*resolve-symlinks* #:resolve-symlinks*
   ;; merging with cwd
   #:get-pathname-defaults #:call-with-current-directory #:with-current-directory
   ;; Environment pathnames
   #:inter-directory-separator #:split-native-pathnames-string
   #:getenv-pathname #:getenv-pathnames
   #:getenv-absolute-directory #:getenv-absolute-directories
   #:lisp-implementation-directory #:lisp-implementation-pathname-p
   ;; Simple filesystem operations
   #:ensure-all-directories-exist
   #:rename-file-overwriting-target
   #:delete-file-if-exists #:delete-empty-directory #:delete-directory-tree))
(in-package :uiop/filesystem)

;;; Native namestrings, as seen by the operating system calls rather than Lisp
(with-upgradability ()
  (defun native-namestring (x)
    "From a non-wildcard CL pathname, a return namestring suitable for passing to the operating system"
    (when x
      (let ((p (pathname x)))
        #+clozure (with-pathname-defaults () (ccl:native-translated-namestring p)) ; see ccl bug 978
        #+(or cmucl scl) (ext:unix-namestring p nil)
        #+sbcl (sb-ext:native-namestring p)
        #-(or clozure cmucl sbcl scl)
        (os-cond
         ((os-unix-p) (unix-namestring p))
         (t (namestring p))))))

  (defun parse-native-namestring (string &rest constraints &key ensure-directory &allow-other-keys)
    "From a native namestring suitable for use by the operating system, return
a CL pathname satisfying all the specified constraints as per ENSURE-PATHNAME"
    (check-type string (or string null))
    (let* ((pathname
             (when string
               (with-pathname-defaults ()
                 #+clozure (ccl:native-to-pathname string)
                 #+cmucl (uiop/os::parse-unix-namestring* string)
                 #+sbcl (sb-ext:parse-native-namestring string)
                 #+scl (lisp::parse-unix-namestring string)
                 #-(or clozure cmucl sbcl scl)
                 (os-cond
                  ((os-unix-p) (parse-unix-namestring string :ensure-directory ensure-directory))
                  (t (parse-namestring string))))))
           (pathname
             (if ensure-directory
                 (and pathname (ensure-directory-pathname pathname))
                 pathname)))
      (apply 'ensure-pathname pathname constraints))))


;;; Probing the filesystem
(with-upgradability ()
  (defun truename* (p)
    "Nicer variant of TRUENAME that plays well with NIL, avoids logical pathname contexts, and tries both files and directories"
    (when p
      (when (stringp p) (setf p (with-pathname-defaults () (parse-namestring p))))
      (values
       (or (ignore-errors (truename p))
           ;; this is here because trying to find the truename of a directory pathname WITHOUT supplying
           ;; a trailing directory separator, causes an error on some lisps.
           #+(or clisp gcl) (if-let (d (ensure-directory-pathname p nil)) (ignore-errors (truename d)))))))

  (defun safe-file-write-date (pathname)
    "Safe variant of FILE-WRITE-DATE that may return NIL rather than raise an error."
    ;; If FILE-WRITE-DATE returns NIL, it's possible that
    ;; the user or some other agent has deleted an input file.
    ;; Also, generated files will not exist at the time planning is done
    ;; and calls compute-action-stamp which calls safe-file-write-date.
    ;; So it is very possible that we can't get a valid file-write-date,
    ;; and we can survive and we will continue the planning
    ;; as if the file were very old.
    ;; (or should we treat the case in a different, special way?)
    (and pathname
         (handler-case (file-write-date (physicalize-pathname pathname))
           (file-error () nil))))

  (defun probe-file* (p &key truename)
    "when given a pathname P (designated by a string as per PARSE-NAMESTRING),
probes the filesystem for a file or directory with given pathname.
If it exists, return its truename if TRUENAME is true,
or the original (parsed) pathname if it is false (the default)."
    (values
     (ignore-errors
      (setf p (funcall 'ensure-pathname p
                       :namestring :lisp
                       :ensure-physical t
                       :ensure-absolute t :defaults 'get-pathname-defaults
                       :want-non-wild t
                       :on-error nil))
      (when p
        #+allegro
        (probe-file p :follow-symlinks truename)
        #+gcl
        (if truename
            (truename* p)
            (let ((kind (car (si::stat p))))
              (when (eq kind :link)
                (setf kind (ignore-errors (car (si::stat (truename* p))))))
              (ecase kind
                ((nil) nil)
                ((:file :link)
                 (cond
                   ((file-pathname-p p) p)
                   ((directory-pathname-p p)
                    (subpathname p (car (last (pathname-directory p)))))))
                (:directory (ensure-directory-pathname p)))))
        #+clisp
        #.(let* ((fs (or #-os-windows (find-symbol* '#:file-stat :posix nil)))
                 (pp (find-symbol* '#:probe-pathname :ext nil)))
            `(if truename
                 ,(if pp
                      `(values (,pp p))
                      '(or (truename* p)
                        (truename* (ignore-errors (ensure-directory-pathname p)))))
                 ,(cond
                    (fs `(and (,fs p) p))
                    (pp `(nth-value 1 (,pp p)))
                    (t '(or (and (truename* p) p)
                         (if-let (d (ensure-directory-pathname p))
                          (and (truename* d) d)))))))
        #-(or allegro clisp gcl)
        (if truename
            (probe-file p)
            (and
             #+(or cmucl scl) (unix:unix-stat (ext:unix-namestring p))
             #+(and lispworks os-unix) (system:get-file-stat p)
             #+sbcl (sb-unix:unix-stat (sb-ext:native-namestring p))
             #-(or cmucl (and lispworks os-unix) sbcl scl) (file-write-date p)
             p))))))

  (defun directory-exists-p (x)
    "Is X the name of a directory that exists on the filesystem?"
    #+allegro
    (excl:probe-directory x)
    #+clisp
    (handler-case (ext:probe-directory x)
           (sys::simple-file-error ()
             nil))
    #-(or allegro clisp)
    (let ((p (probe-file* x :truename t)))
      (and (directory-pathname-p p) p)))

  (defun file-exists-p (x)
    "Is X the name of a file that exists on the filesystem?"
    (let ((p (probe-file* x :truename t)))
      (and (file-pathname-p p) p)))

  (defun directory* (pathname-spec &rest keys &key &allow-other-keys)
    "Return a list of the entries in a directory by calling DIRECTORY.
Try to override the defaults to not resolving symlinks, if implementation allows."
    (apply 'directory pathname-spec
           (append keys '#.(or #+allegro '(:directories-are-files nil :follow-symbolic-links nil)
                               #+(or clozure digitool) '(:follow-links nil)
                               #+clisp '(:circle t :if-does-not-exist :ignore)
                               #+(or cmucl scl) '(:follow-links nil :truenamep nil)
                               #+lispworks '(:link-transparency nil)
                               #+sbcl (when (find-symbol* :resolve-symlinks '#:sb-impl nil)
                                        '(:resolve-symlinks nil))))))

  (defun filter-logical-directory-results (directory entries merger)
    "If DIRECTORY isn't a logical pathname, return ENTRIES. If it is,
given ENTRIES in the DIRECTORY, remove the entries which are physical yet
when transformed by MERGER have a different TRUENAME.
Also remove duplicates as may appear with some translation rules.
This function is used as a helper to DIRECTORY-FILES to avoid invalid entries
when using logical-pathnames."
    (if (logical-pathname-p directory)
        (remove-duplicates ;; on CLISP, querying ~/ will return duplicates
         ;; Try hard to not resolve logical-pathname into physical pathnames;
         ;; otherwise logical-pathname users/lovers will be disappointed.
         ;; If directory* could use some implementation-dependent magic,
         ;; we will have logical pathnames already; otherwise,
         ;; we only keep pathnames for which specifying the name and
         ;; translating the LPN commute.
         (loop :for f :in entries
               :for p = (or (and (logical-pathname-p f) f)
                            (let* ((u (ignore-errors (call-function merger f))))
                              ;; The first u avoids a cumbersome (truename u) error.
                              ;; At this point f should already be a truename,
                              ;; but isn't quite in CLISP, for it doesn't have :version :newest
                              (and u (equal (truename* u) (truename* f)) u)))
           :when p :collect p)
         :test 'pathname-equal)
        entries))

  (defun directory-files (directory &optional (pattern *wild-file-for-directory*))
    "Return a list of the files in a directory according to the PATTERN.
Subdirectories should NOT be returned.
  PATTERN defaults to a pattern carefully chosen based on the implementation;
override the default at your own risk.
  DIRECTORY-FILES tries NOT to resolve symlinks if the implementation permits this,
but the behavior in presence of symlinks is not portable. Use IOlib to handle such situations."
    (let ((dir (pathname directory)))
      (when (logical-pathname-p dir)
        ;; Because of the filtering we do below,
        ;; logical pathnames have restrictions on wild patterns.
        ;; Not that the results are very portable when you use these patterns on physical pathnames.
        (when (wild-pathname-p dir)
          (parameter-error "~S: Invalid wild pattern in logical directory ~S"
                           'directory-files directory))
        (unless (member (pathname-directory pattern) '(() (:relative)) :test 'equal)
          (parameter-error "~S: Invalid file pattern ~S for logical directory ~S" 'directory-files pattern directory))
        (setf pattern (make-pathname-logical pattern (pathname-host dir))))
      (let* ((pat (merge-pathnames* pattern dir))
             (entries (ignore-errors (directory* pat))))
        (remove-if 'directory-pathname-p
                   (filter-logical-directory-results
                    directory entries
                    #'(lambda (f)
                        (make-pathname :defaults dir
                                       :name (make-pathname-component-logical (pathname-name f))
                                       :type (make-pathname-component-logical (pathname-type f))
                                       :version (make-pathname-component-logical (pathname-version f)))))))))

  (defun subdirectories (directory)
    "Given a DIRECTORY pathname designator, return a list of the subdirectories under it.
The behavior in presence of symlinks is not portable. Use IOlib to handle such situations."
    (let* ((directory (ensure-directory-pathname directory))
           #-(or abcl cormanlisp genera xcl)
           (wild (merge-pathnames*
                  #-(or abcl allegro cmucl lispworks sbcl scl xcl)
                  *wild-directory*
                  #+(or abcl allegro cmucl lispworks sbcl scl xcl) "*.*"
                  directory))
           (dirs
             #-(or abcl cormanlisp genera xcl)
             (ignore-errors
              (directory* wild . #.(or #+clozure '(:directories t :files nil)
                                       #+mcl '(:directories t))))
             #+(or abcl xcl) (system:list-directory directory)
             #+cormanlisp (cl::directory-subdirs directory)
             #+genera (handler-case (fs:directory-list directory) (fs:directory-not-found () nil)))
           #+(or abcl allegro cmucl genera lispworks sbcl scl xcl)
           (dirs (loop :for x :in dirs
                       :for d = #+(or abcl xcl) (extensions:probe-directory x)
                       #+allegro (excl:probe-directory x)
                       #+(or cmucl sbcl scl) (directory-pathname-p x)
                       #+genera (getf (cdr x) :directory)
                       #+lispworks (lw:file-directory-p x)
                       :when d :collect #+(or abcl allegro xcl) (ensure-directory-pathname d)
                         #+genera (ensure-directory-pathname (first x))
                       #+(or cmucl lispworks sbcl scl) x)))
      (filter-logical-directory-results
       directory dirs
       (let ((prefix (or (normalize-pathname-directory-component (pathname-directory directory))
                         '(:absolute)))) ; because allegro returns NIL for #p"FOO:"
         #'(lambda (d)
             (let ((dir (normalize-pathname-directory-component (pathname-directory d))))
               (and (consp dir) (consp (cdr dir))
                    (make-pathname
                     :defaults directory :name nil :type nil :version nil
                     :directory (append prefix (make-pathname-component-logical (last dir)))))))))))

  (defun collect-sub*directories (directory collectp recursep collector)
    "Given a DIRECTORY, when COLLECTP returns true when CALL-FUNCTION'ed with the directory,
call-function the COLLECTOR function designator on the directory,
and recurse each of its subdirectories on which the RECURSEP returns true when CALL-FUNCTION'ed with them.
This function will thus let you traverse a filesystem hierarchy,
superseding the functionality of CL-FAD:WALK-DIRECTORY.
The behavior in presence of symlinks is not portable. Use IOlib to handle such situations."
    (when (call-function collectp directory)
      (call-function collector directory)
      (dolist (subdir (subdirectories directory))
        (when (call-function recursep subdir)
          (collect-sub*directories subdir collectp recursep collector))))))

;;; Resolving symlinks somewhat
(with-upgradability ()
  (defun truenamize (pathname)
    "Resolve as much of a pathname as possible"
    (block nil
      (when (typep pathname '(or null logical-pathname)) (return pathname))
      (let ((p pathname))
        (unless (absolute-pathname-p p)
          (setf p (or (absolute-pathname-p (ensure-absolute-pathname p 'get-pathname-defaults nil))
                      (return p))))
        (when (logical-pathname-p p) (return p))
        (let ((found (probe-file* p :truename t)))
          (when found (return found)))
        (let* ((directory (normalize-pathname-directory-component (pathname-directory p)))
               (up-components (reverse (rest directory)))
               (down-components ()))
          (assert (eq :absolute (first directory)))
          (loop :while up-components :do
            (if-let (parent
                     (ignore-errors
                      (probe-file* (make-pathname :directory `(:absolute ,@(reverse up-components))
                                                  :name nil :type nil :version nil :defaults p))))
              (if-let (simplified
                       (ignore-errors
                        (merge-pathnames*
                         (make-pathname :directory `(:relative ,@down-components)
                                        :defaults p)
                         (ensure-directory-pathname parent))))
                (return simplified)))
            (push (pop up-components) down-components)
            :finally (return p))))))

  (defun resolve-symlinks (path)
    "Do a best effort at resolving symlinks in PATH, returning a partially or totally resolved PATH."
    #-allegro (truenamize path)
    #+allegro
    (if (physical-pathname-p path)
        (or (ignore-errors (excl:pathname-resolve-symbolic-links path)) path)
        path))

  (defvar *resolve-symlinks* t
    "Determine whether or not ASDF resolves symlinks when defining systems.
Defaults to T.")

  (defun resolve-symlinks* (path)
    "RESOLVE-SYMLINKS in PATH iff *RESOLVE-SYMLINKS* is T (the default)."
    (if *resolve-symlinks*
        (and path (resolve-symlinks path))
        path)))


;;; Check pathname constraints
(with-upgradability ()
  (defun ensure-pathname
      (pathname &key
                  on-error
                  defaults type dot-dot namestring
                  empty-is-nil
                  want-pathname
                  want-logical want-physical ensure-physical
                  want-relative want-absolute ensure-absolute ensure-subpath
                  want-non-wild want-wild wilden
                  want-file want-directory ensure-directory
                  want-existing ensure-directories-exist
                  truename resolve-symlinks truenamize
       &aux (p pathname)) ;; mutable working copy, preserve original
    "Coerces its argument into a PATHNAME,
optionally doing some transformations and checking specified constraints.

If the argument is NIL, then NIL is returned unless the WANT-PATHNAME constraint is specified.

If the argument is a STRING, it is first converted to a pathname via
PARSE-UNIX-NAMESTRING, PARSE-NAMESTRING or PARSE-NATIVE-NAMESTRING respectively
depending on the NAMESTRING argument being :UNIX, :LISP or :NATIVE respectively,
or else by using CALL-FUNCTION on the NAMESTRING argument;
if :UNIX is specified (or NIL, the default, which specifies the same thing),
then PARSE-UNIX-NAMESTRING it is called with the keywords
DEFAULTS TYPE DOT-DOT ENSURE-DIRECTORY WANT-RELATIVE, and
the result is optionally merged into the DEFAULTS if ENSURE-ABSOLUTE is true.

The pathname passed or resulting from parsing the string
is then subjected to all the checks and transformations below are run.

Each non-nil constraint argument can be one of the symbols T, ERROR, CERROR or IGNORE.
The boolean T is an alias for ERROR.
ERROR means that an error will be raised if the constraint is not satisfied.
CERROR means that an continuable error will be raised if the constraint is not satisfied.
IGNORE means just return NIL instead of the pathname.

The ON-ERROR argument, if not NIL, is a function designator (as per CALL-FUNCTION)
that will be called with the the following arguments:
a generic format string for ensure pathname, the pathname,
the keyword argument corresponding to the failed check or transformation,
a format string for the reason ENSURE-PATHNAME failed,
and a list with arguments to that format string.
If ON-ERROR is NIL, ERROR is used instead, which does the right thing.
You could also pass (CERROR \"CONTINUE DESPITE FAILED CHECK\").

The transformations and constraint checks are done in this order,
which is also the order in the lambda-list:

EMPTY-IS-NIL returns NIL if the argument is an empty string.
WANT-PATHNAME checks that pathname (after parsing if needed) is not null.
Otherwise, if the pathname is NIL, ensure-pathname returns NIL.
WANT-LOGICAL checks that pathname is a LOGICAL-PATHNAME
WANT-PHYSICAL checks that pathname is not a LOGICAL-PATHNAME
ENSURE-PHYSICAL ensures that pathname is physical via TRANSLATE-LOGICAL-PATHNAME
WANT-RELATIVE checks that pathname has a relative directory component
WANT-ABSOLUTE checks that pathname does have an absolute directory component
ENSURE-ABSOLUTE merges with the DEFAULTS, then checks again
that the result absolute is an absolute pathname indeed.
ENSURE-SUBPATH checks that the pathname is a subpath of the DEFAULTS.
WANT-FILE checks that pathname has a non-nil FILE component
WANT-DIRECTORY checks that pathname has nil FILE and TYPE components
ENSURE-DIRECTORY uses ENSURE-DIRECTORY-PATHNAME to interpret
any file and type components as being actually a last directory component.
WANT-NON-WILD checks that pathname is not a wild pathname
WANT-WILD checks that pathname is a wild pathname
WILDEN merges the pathname with **/*.*.* if it is not wild
WANT-EXISTING checks that a file (or directory) exists with that pathname.
ENSURE-DIRECTORIES-EXIST creates any parent directory with ENSURE-DIRECTORIES-EXIST.
TRUENAME replaces the pathname by its truename, or errors if not possible.
RESOLVE-SYMLINKS replaces the pathname by a variant with symlinks resolved by RESOLVE-SYMLINKS.
TRUENAMIZE uses TRUENAMIZE to resolve as many symlinks as possible."
    (block nil
      (flet ((report-error (keyword description &rest arguments)
               (call-function (or on-error 'error)
                              "Invalid pathname ~S: ~*~?"
                              pathname keyword description arguments)))
        (macrolet ((err (constraint &rest arguments)
                     `(report-error ',(intern* constraint :keyword) ,@arguments))
                   (check (constraint condition &rest arguments)
                     `(when ,constraint
                        (unless ,condition (err ,constraint ,@arguments))))
                   (transform (transform condition expr)
                     `(when ,transform
                        (,@(if condition `(when ,condition) '(progn))
                         (setf p ,expr)))))
          (etypecase p
            ((or null pathname))
            (string
             (when (and (emptyp p) empty-is-nil)
               (return-from ensure-pathname nil))
             (setf p (case namestring
                       ((:unix nil)
                        (parse-unix-namestring
                         p :defaults defaults :type type :dot-dot dot-dot
                           :ensure-directory ensure-directory :want-relative want-relative))
                       ((:native)
                        (parse-native-namestring p))
                       ((:lisp)
                        (parse-namestring p))
                       (t
                        (call-function namestring p))))))
          (etypecase p
            (pathname)
            (null
             (check want-pathname (pathnamep p) "Expected a pathname, not NIL")
             (return nil)))
          (check want-logical (logical-pathname-p p) "Expected a logical pathname")
          (check want-physical (physical-pathname-p p) "Expected a physical pathname")
          (transform ensure-physical () (physicalize-pathname p))
          (check ensure-physical (physical-pathname-p p) "Could not translate to a physical pathname")
          (check want-relative (relative-pathname-p p) "Expected a relative pathname")
          (check want-absolute (absolute-pathname-p p) "Expected an absolute pathname")
          (transform ensure-absolute (not (absolute-pathname-p p))
                     (ensure-absolute-pathname p defaults (list #'report-error :ensure-absolute "~@?")))
          (check ensure-absolute (absolute-pathname-p p)
                 "Could not make into an absolute pathname even after merging with ~S" defaults)
          (check ensure-subpath (absolute-pathname-p defaults)
                 "cannot be checked to be a subpath of non-absolute pathname ~S" defaults)
          (check ensure-subpath (subpathp p defaults) "is not a sub pathname of ~S" defaults)
          (check want-file (file-pathname-p p) "Expected a file pathname")
          (check want-directory (directory-pathname-p p) "Expected a directory pathname")
          (transform ensure-directory (not (directory-pathname-p p)) (ensure-directory-pathname p))
          (check want-non-wild (not (wild-pathname-p p)) "Expected a non-wildcard pathname")
          (check want-wild (wild-pathname-p p) "Expected a wildcard pathname")
          (transform wilden (not (wild-pathname-p p)) (wilden p))
          (when want-existing
            (let ((existing (probe-file* p :truename truename)))
              (if existing
                  (when truename
                    (return existing))
                  (err want-existing "Expected an existing pathname"))))
          (when ensure-directories-exist (ensure-directories-exist p))
          (when truename
            (let ((truename (truename* p)))
              (if truename
                  (return truename)
                  (err truename "Can't get a truename for pathname"))))
          (transform resolve-symlinks () (resolve-symlinks p))
          (transform truenamize () (truenamize p))
          p)))))


;;; Pathname defaults
(with-upgradability ()
  (defun get-pathname-defaults (&optional (defaults *default-pathname-defaults*))
    "Find the actual DEFAULTS to use for pathnames, including
resolving them with respect to GETCWD if the DEFAULTS were relative"
    (or (absolute-pathname-p defaults)
        (merge-pathnames* defaults (getcwd))))

  (defun call-with-current-directory (dir thunk)
    "call the THUNK in a context where the current directory was changed to DIR, if not NIL.
Note that this operation is usually NOT thread-safe."
    (if dir
        (let* ((dir (resolve-symlinks* (get-pathname-defaults (pathname-directory-pathname dir))))
               (cwd (getcwd))
               (*default-pathname-defaults* dir))
          (chdir dir)
          (unwind-protect
               (funcall thunk)
            (chdir cwd)))
        (funcall thunk)))

  (defmacro with-current-directory ((&optional dir) &body body)
    "Call BODY while the POSIX current working directory is set to DIR"
    `(call-with-current-directory ,dir #'(lambda () ,@body))))


;;; Environment pathnames
(with-upgradability ()
  (defun inter-directory-separator ()
    "What character does the current OS conventionally uses to separate directories?"
    (os-cond ((os-unix-p) #\:) (t #\;)))

  (defun split-native-pathnames-string (string &rest constraints &key &allow-other-keys)
    "Given a string of pathnames specified in native OS syntax, separate them in a list,
check constraints and normalize each one as per ENSURE-PATHNAME,
where an empty string denotes NIL."
    (loop :for namestring :in (split-string string :separator (string (inter-directory-separator)))
          :collect (unless (emptyp namestring) (apply 'parse-native-namestring namestring constraints))))

  (defun getenv-pathname (x &rest constraints &key ensure-directory want-directory on-error &allow-other-keys)
    "Extract a pathname from a user-configured environment variable, as per native OS,
check constraints and normalize as per ENSURE-PATHNAME."
    ;; For backward compatibility with ASDF 2, want-directory implies ensure-directory
    (apply 'parse-native-namestring (getenvp x)
           :ensure-directory (or ensure-directory want-directory)
           :on-error (or on-error
                         `(error "In (~S ~S), invalid pathname ~*~S: ~*~?" getenv-pathname ,x))
           constraints))
  (defun getenv-pathnames (x &rest constraints &key on-error &allow-other-keys)
    "Extract a list of pathname from a user-configured environment variable, as per native OS,
check constraints and normalize each one as per ENSURE-PATHNAME.
       Any empty entries in the environment variable X will be returned as NILs."
    (unless (getf constraints :empty-is-nil t)
      (parameter-error "Cannot have EMPTY-IS-NIL false for ~S" 'getenv-pathnames))
    (apply 'split-native-pathnames-string (getenvp x)
           :on-error (or on-error
                         `(error "In (~S ~S), invalid pathname ~*~S: ~*~?" getenv-pathnames ,x))
           :empty-is-nil t
           constraints))
  (defun getenv-absolute-directory (x)
    "Extract an absolute directory pathname from a user-configured environment variable,
as per native OS"
    (getenv-pathname x :want-absolute t :ensure-directory t))
  (defun getenv-absolute-directories (x)
    "Extract a list of absolute directories from a user-configured environment variable,
as per native OS.  Any empty entries in the environment variable X will be returned as
NILs."
    (getenv-pathnames x :want-absolute t :ensure-directory t))

  (defun lisp-implementation-directory (&key truename)
    "Where are the system files of the current installation of the CL implementation?"
    (declare (ignorable truename))
    (let ((dir
            #+abcl extensions:*lisp-home*
            #+(or allegro clasp ecl mkcl) #p"SYS:"
            #+clisp custom:*lib-directory*
            #+clozure #p"ccl:"
            #+cmucl (ignore-errors (pathname-parent-directory-pathname (truename #p"modules:")))
            #+gcl system::*system-directory*
            #+lispworks lispworks:*lispworks-directory*
            #+sbcl (if-let (it (find-symbol* :sbcl-homedir-pathname :sb-int nil))
                     (funcall it)
                     (getenv-pathname "SBCL_HOME" :ensure-directory t))
            #+scl (ignore-errors (pathname-parent-directory-pathname (truename #p"file://modules/")))
            #+xcl ext:*xcl-home*))
      (if (and dir truename)
          (truename* dir)
          dir)))

  (defun lisp-implementation-pathname-p (pathname)
    "Is the PATHNAME under the current installation of the CL implementation?"
    ;; Other builtin systems are those under the implementation directory
    (and (when pathname
           (if-let (impdir (lisp-implementation-directory))
             (or (subpathp pathname impdir)
                 (when *resolve-symlinks*
                   (if-let (truename (truename* pathname))
                     (if-let (trueimpdir (truename* impdir))
                       (subpathp truename trueimpdir)))))))
         t)))


;;; Simple filesystem operations
(with-upgradability ()
  (defun ensure-all-directories-exist (pathnames)
    "Ensure that for every pathname in PATHNAMES, we ensure its directories exist"
    (dolist (pathname pathnames)
      (when pathname
        (ensure-directories-exist (physicalize-pathname pathname)))))

  (defun delete-file-if-exists (x)
    "Delete a file X if it already exists"
    (when x (handler-case (delete-file x) (file-error () nil))))

  (defun rename-file-overwriting-target (source target)
    "Rename a file, overwriting any previous file with the TARGET name,
in an atomic way if the implementation allows."
    (let ((source (ensure-pathname source :namestring :lisp :ensure-physical t :want-file t))
          (target (ensure-pathname target :namestring :lisp :ensure-physical t :want-file t)))
      #+clisp ;; in recent enough versions of CLISP, :if-exists :overwrite would make it atomic
      (progn (funcall 'require "syscalls")
             (symbol-call :posix :copy-file source target :method :rename))
      #+(and sbcl os-windows) (delete-file-if-exists target) ;; not atomic
      #-clisp
      (rename-file source target
                   #+(or clasp clozure ecl) :if-exists
                   #+clozure :rename-and-delete #+(or clasp ecl) t)))

  (defun delete-empty-directory (directory-pathname)
    "Delete an empty directory"
    #+(or abcl digitool gcl) (delete-file directory-pathname)
    #+allegro (excl:delete-directory directory-pathname)
    #+clisp (ext:delete-directory directory-pathname)
    #+clozure (ccl::delete-empty-directory directory-pathname)
    #+(or cmucl scl) (multiple-value-bind (ok errno)
                       (unix:unix-rmdir (native-namestring directory-pathname))
                     (unless ok
                       #+cmucl (error "Error number ~A when trying to delete directory ~A"
                                    errno directory-pathname)
                       #+scl (error "~@<Error deleting ~S: ~A~@:>"
                                    directory-pathname (unix:get-unix-error-msg errno))))
    #+cormanlisp (win32:delete-directory directory-pathname)
    #+(or clasp ecl) (si:rmdir directory-pathname)
    #+genera (fs:delete-directory directory-pathname)
    #+lispworks (lw:delete-directory directory-pathname)
    #+mkcl (mkcl:rmdir directory-pathname)
    #+sbcl #.(if-let (dd (find-symbol* :delete-directory :sb-ext nil))
               `(,dd directory-pathname) ;; requires SBCL 1.0.44 or later
               `(progn (require :sb-posix) (symbol-call :sb-posix :rmdir directory-pathname)))
    #+xcl (symbol-call :uiop :run-program `("rmdir" ,(native-namestring directory-pathname)))
    #-(or abcl allegro clasp clisp clozure cmucl cormanlisp digitool ecl gcl genera lispworks mkcl sbcl scl xcl)
    (not-implemented-error 'delete-empty-directory "(on your platform)")) ; genera

  (defun delete-directory-tree (directory-pathname &key (validate nil validatep) (if-does-not-exist :error))
    "Delete a directory including all its recursive contents, aka rm -rf.

To reduce the risk of infortunate mistakes, DIRECTORY-PATHNAME must be
a physical non-wildcard directory pathname (not namestring).

If the directory does not exist, the IF-DOES-NOT-EXIST argument specifies what happens:
if it is :ERROR (the default), an error is signaled, whereas if it is :IGNORE, nothing is done.

Furthermore, before any deletion is attempted, the DIRECTORY-PATHNAME must pass
the validation function designated (as per ENSURE-FUNCTION) by the VALIDATE keyword argument
which in practice is thus compulsory, and validates by returning a non-NIL result.
If you're suicidal or extremely confident, just use :VALIDATE T."
    (check-type if-does-not-exist (member :error :ignore))
    (cond
      ((not (and (pathnamep directory-pathname) (directory-pathname-p directory-pathname)
                 (physical-pathname-p directory-pathname) (not (wild-pathname-p directory-pathname))))
       (parameter-error "~S was asked to delete ~S but it is not a physical non-wildcard directory pathname"
              'delete-directory-tree directory-pathname))
      ((not validatep)
       (parameter-error "~S was asked to delete ~S but was not provided a validation predicate"
              'delete-directory-tree directory-pathname))
      ((not (call-function validate directory-pathname))
       (parameter-error "~S was asked to delete ~S but it is not valid ~@[according to ~S~]"
              'delete-directory-tree directory-pathname validate))
      ((not (directory-exists-p directory-pathname))
       (ecase if-does-not-exist
         (:error
          (error "~S was asked to delete ~S but the directory does not exist"
              'delete-directory-tree directory-pathname))
         (:ignore nil)))
      #-(or allegro cmucl clozure genera sbcl scl)
      ((os-unix-p) ;; On Unix, don't recursively walk the directory and delete everything in Lisp,
       ;; except on implementations where we can prevent DIRECTORY from following symlinks;
       ;; instead spawn a standard external program to do the dirty work.
       (symbol-call :uiop :run-program `("rm" "-rf" ,(native-namestring directory-pathname))))
      (t
       ;; On supported implementation, call supported system functions
       #+allegro (symbol-call :excl.osi :delete-directory-and-files
                              directory-pathname :if-does-not-exist if-does-not-exist)
       #+clozure (ccl:delete-directory directory-pathname)
       #+genera (fs:delete-directory directory-pathname :confirm nil)
       #+sbcl #.(if-let (dd (find-symbol* :delete-directory :sb-ext nil))
                  `(,dd directory-pathname :recursive t) ;; requires SBCL 1.0.44 or later
                  '(error "~S requires SBCL 1.0.44 or later" 'delete-directory-tree))
       ;; Outside Unix or on CMUCL and SCL that can avoid following symlinks,
       ;; do things the hard way.
       #-(or allegro clozure genera sbcl)
       (let ((sub*directories
               (while-collecting (c)
                 (collect-sub*directories directory-pathname t t #'c))))
             (dolist (d (nreverse sub*directories))
               (map () 'delete-file (directory-files d))
               (delete-empty-directory d)))))))
;;;; ---------------------------------------------------------------------------
;;;; Utilities related to streams

(uiop/package:define-package :uiop/stream
  (:use :uiop/common-lisp :uiop/package :uiop/utility :uiop/os :uiop/pathname :uiop/filesystem)
  (:export
   #:*default-stream-element-type*
   #:*stdin* #:setup-stdin #:*stdout* #:setup-stdout #:*stderr* #:setup-stderr
   #:detect-encoding #:*encoding-detection-hook* #:always-default-encoding
   #:encoding-external-format #:*encoding-external-format-hook* #:default-encoding-external-format
   #:*default-encoding* #:*utf-8-external-format*
   #:with-safe-io-syntax #:call-with-safe-io-syntax #:safe-read-from-string
   #:with-output #:output-string #:with-input #:input-string
   #:with-input-file #:call-with-input-file #:with-output-file #:call-with-output-file
   #:null-device-pathname #:call-with-null-input #:with-null-input
   #:call-with-null-output #:with-null-output
   #:finish-outputs #:format! #:safe-format!
   #:copy-stream-to-stream #:concatenate-files #:copy-file
   #:slurp-stream-string #:slurp-stream-lines #:slurp-stream-line
   #:slurp-stream-forms #:slurp-stream-form
   #:read-file-string #:read-file-line #:read-file-lines #:safe-read-file-line
   #:read-file-forms #:read-file-form #:safe-read-file-form
   #:eval-input #:eval-thunk #:standard-eval-thunk
   #:println #:writeln
   #:file-stream-p #:file-or-synonym-stream-p
   ;; Temporary files
   #:*temporary-directory* #:temporary-directory #:default-temporary-directory
   #:setup-temporary-directory
   #:call-with-temporary-file #:with-temporary-file
   #:add-pathname-suffix #:tmpize-pathname
   #:call-with-staging-pathname #:with-staging-pathname))
(in-package :uiop/stream)

(with-upgradability ()
  (defvar *default-stream-element-type*
    (or #+(or abcl cmucl cormanlisp scl xcl) 'character
        #+lispworks 'lw:simple-char
        :default)
    "default element-type for open (depends on the current CL implementation)")

  (defvar *stdin* *standard-input*
    "the original standard input stream at startup")

  (defun setup-stdin ()
    (setf *stdin*
          #.(or #+clozure 'ccl::*stdin*
                #+(or cmucl scl) 'system:*stdin*
                #+(or clasp ecl) 'ext::+process-standard-input+
                #+sbcl 'sb-sys:*stdin*
                '*standard-input*)))

  (defvar *stdout* *standard-output*
    "the original standard output stream at startup")

  (defun setup-stdout ()
    (setf *stdout*
          #.(or #+clozure 'ccl::*stdout*
                #+(or cmucl scl) 'system:*stdout*
                #+(or clasp ecl) 'ext::+process-standard-output+
                #+sbcl 'sb-sys:*stdout*
                '*standard-output*)))

  (defvar *stderr* *error-output*
    "the original error output stream at startup")

  (defun setup-stderr ()
    (setf *stderr*
          #.(or #+allegro 'excl::*stderr*
                #+clozure 'ccl::*stderr*
                #+(or cmucl scl) 'system:*stderr*
                #+(or clasp ecl) 'ext::+process-error-output+
                #+sbcl 'sb-sys:*stderr*
                '*error-output*)))

  ;; Run them now. In image.lisp, we'll register them to be run at image restart.
  (setup-stdin) (setup-stdout) (setup-stderr))


;;; Encodings (mostly hooks only; full support requires asdf-encodings)
(with-upgradability ()
  (defparameter *default-encoding*
    ;; preserve explicit user changes to something other than the legacy default :default
    (or (if-let (previous (and (boundp '*default-encoding*) (symbol-value '*default-encoding*)))
          (unless (eq previous :default) previous))
        :utf-8)
    "Default encoding for source files.
The default value :utf-8 is the portable thing.
The legacy behavior was :default.
If you (asdf:load-system :asdf-encodings) then
you will have autodetection via *encoding-detection-hook* below,
reading emacs-style -*- coding: utf-8 -*- specifications,
and falling back to utf-8 or latin1 if nothing is specified.")

  (defparameter *utf-8-external-format*
    (if (featurep :asdf-unicode)
        (or #+clisp charset:utf-8 :utf-8)
        :default)
    "Default :external-format argument to pass to CL:OPEN and also
CL:LOAD or CL:COMPILE-FILE to best process a UTF-8 encoded file.
On modern implementations, this will decode UTF-8 code points as CL characters.
On legacy implementations, it may fall back on some 8-bit encoding,
with non-ASCII code points being read as several CL characters;
hopefully, if done consistently, that won't affect program behavior too much.")

  (defun always-default-encoding (pathname)
    "Trivial function to use as *encoding-detection-hook*,
always 'detects' the *default-encoding*"
    (declare (ignore pathname))
    *default-encoding*)

  (defvar *encoding-detection-hook* #'always-default-encoding
    "Hook for an extension to define a function to automatically detect a file's encoding")

  (defun detect-encoding (pathname)
    "Detects the encoding of a specified file, going through user-configurable hooks"
    (if (and pathname (not (directory-pathname-p pathname)) (probe-file* pathname))
        (funcall *encoding-detection-hook* pathname)
        *default-encoding*))

  (defun default-encoding-external-format (encoding)
    "Default, ignorant, function to transform a character ENCODING as a
portable keyword to an implementation-dependent EXTERNAL-FORMAT specification.
Load system ASDF-ENCODINGS to hook in a better one."
    (case encoding
      (:default :default) ;; for backward-compatibility only. Explicit usage discouraged.
      (:utf-8 *utf-8-external-format*)
      (otherwise
       (cerror "Continue using :external-format :default" (compatfmt "~@<Your ASDF component is using encoding ~S but it isn't recognized. Your system should :defsystem-depends-on (:asdf-encodings).~:>") encoding)
       :default)))

  (defvar *encoding-external-format-hook*
    #'default-encoding-external-format
    "Hook for an extension (e.g. ASDF-ENCODINGS) to define a better mapping
from non-default encodings to and implementation-defined external-format's")

  (defun encoding-external-format (encoding)
    "Transform a portable ENCODING keyword to an implementation-dependent EXTERNAL-FORMAT,
going through all the proper hooks."
    (funcall *encoding-external-format-hook* (or encoding *default-encoding*))))


;;; Safe syntax
(with-upgradability ()
  (defvar *standard-readtable* (with-standard-io-syntax *readtable*)
    "The standard readtable, implementing the syntax specified by the CLHS.
It must never be modified, though only good implementations will even enforce that.")

  (defmacro with-safe-io-syntax ((&key (package :cl)) &body body)
    "Establish safe CL reader options around the evaluation of BODY"
    `(call-with-safe-io-syntax #'(lambda () (let ((*package* (find-package ,package))) ,@body))))

  (defun call-with-safe-io-syntax (thunk &key (package :cl))
    (with-standard-io-syntax
      (let ((*package* (find-package package))
            (*read-default-float-format* 'double-float)
            (*print-readably* nil)
            (*read-eval* nil))
        (funcall thunk))))

  (defun safe-read-from-string (string &key (package :cl) (eof-error-p t) eof-value (start 0) end preserve-whitespace)
    "Read from STRING using a safe syntax, as per WITH-SAFE-IO-SYNTAX"
    (with-safe-io-syntax (:package package)
      (read-from-string string eof-error-p eof-value :start start :end end :preserve-whitespace preserve-whitespace))))

;;; Output helpers
(with-upgradability ()
  (defun call-with-output-file (pathname thunk
                                &key
                                  (element-type *default-stream-element-type*)
                                  (external-format *utf-8-external-format*)
                                  (if-exists :error)
                                  (if-does-not-exist :create))
    "Open FILE for input with given recognizes options, call THUNK with the resulting stream.
Other keys are accepted but discarded."
    (with-open-file (s pathname :direction :output
                                :element-type element-type
                                :external-format external-format
                                :if-exists if-exists
                                :if-does-not-exist if-does-not-exist)
      (funcall thunk s)))

  (defmacro with-output-file ((var pathname &rest keys
                               &key element-type external-format if-exists if-does-not-exist)
                              &body body)
    (declare (ignore element-type external-format if-exists if-does-not-exist))
    `(call-with-output-file ,pathname #'(lambda (,var) ,@body) ,@keys))

  (defun call-with-output (output function &key keys)
    "Calls FUNCTION with an actual stream argument,
behaving like FORMAT with respect to how stream designators are interpreted:
If OUTPUT is a STREAM, use it as the stream.
If OUTPUT is NIL, use a STRING-OUTPUT-STREAM as the stream, and return the resulting string.
If OUTPUT is T, use *STANDARD-OUTPUT* as the stream.
If OUTPUT is a STRING with a fill-pointer, use it as a string-output-stream.
If OUTPUT is a PATHNAME, open the file and write to it, passing KEYS to WITH-OUTPUT-FILE
-- this latter as an extension since ASDF 3.1.
Otherwise, signal an error."
    (etypecase output
      (null
       (with-output-to-string (stream) (funcall function stream)))
      ((eql t)
       (funcall function *standard-output*))
      (stream
       (funcall function output))
      (string
       (assert (fill-pointer output))
       (with-output-to-string (stream output) (funcall function stream)))
      (pathname
       (apply 'call-with-output-file output function keys))))

  (defmacro with-output ((output-var &optional (value output-var)) &body body)
    "Bind OUTPUT-VAR to an output stream, coercing VALUE (default: previous binding of OUTPUT-VAR)
as per FORMAT, and evaluate BODY within the scope of this binding."
    `(call-with-output ,value #'(lambda (,output-var) ,@body)))

  (defun output-string (string &optional output)
    "If the desired OUTPUT is not NIL, print the string to the output; otherwise return the string"
    (if output
        (with-output (output) (princ string output))
        string)))


;;; Input helpers
(with-upgradability ()
  (defun call-with-input-file (pathname thunk
                               &key
                                 (element-type *default-stream-element-type*)
                                 (external-format *utf-8-external-format*)
                                 (if-does-not-exist :error))
    "Open FILE for input with given recognizes options, call THUNK with the resulting stream.
Other keys are accepted but discarded."
    (with-open-file (s pathname :direction :input
                                :element-type element-type
                                :external-format external-format
                                :if-does-not-exist if-does-not-exist)
      (funcall thunk s)))

  (defmacro with-input-file ((var pathname &rest keys
                              &key element-type external-format if-does-not-exist)
                             &body body)
    (declare (ignore element-type external-format if-does-not-exist))
    `(call-with-input-file ,pathname #'(lambda (,var) ,@body) ,@keys))

  (defun call-with-input (input function &key keys)
    "Calls FUNCTION with an actual stream argument, interpreting
stream designators like READ, but also coercing strings to STRING-INPUT-STREAM,
and PATHNAME to FILE-STREAM.
If INPUT is a STREAM, use it as the stream.
If INPUT is NIL, use a *STANDARD-INPUT* as the stream.
If INPUT is T, use *TERMINAL-IO* as the stream.
If INPUT is a STRING, use it as a string-input-stream.
If INPUT is a PATHNAME, open it, passing KEYS to WITH-INPUT-FILE
-- the latter is an extension since ASDF 3.1.
Otherwise, signal an error."
    (etypecase input
      (null (funcall function *standard-input*))
      ((eql t) (funcall function *terminal-io*))
      (stream (funcall function input))
      (string (with-input-from-string (stream input) (funcall function stream)))
      (pathname (apply 'call-with-input-file input function keys))))

  (defmacro with-input ((input-var &optional (value input-var)) &body body)
    "Bind INPUT-VAR to an input stream, coercing VALUE (default: previous binding of INPUT-VAR)
as per CALL-WITH-INPUT, and evaluate BODY within the scope of this binding."
    `(call-with-input ,value #'(lambda (,input-var) ,@body)))

  (defun input-string (&optional input)
    "If the desired INPUT is a string, return that string; otherwise slurp the INPUT into a string
and return that"
    (if (stringp input)
        input
        (with-input (input) (funcall 'slurp-stream-string input)))))

;;; Null device
(with-upgradability ()
  (defun null-device-pathname ()
    "Pathname to a bit bucket device that discards any information written to it
and always returns EOF when read from"
    (os-cond
      ((os-unix-p) #p"/dev/null")
      ((os-windows-p) #p"NUL") ;; Q: how many Lisps accept the #p"NUL:" syntax?
      (t (error "No /dev/null on your OS"))))
  (defun call-with-null-input (fun &rest keys &key element-type external-format if-does-not-exist)
    "Call FUN with an input stream from the null device; pass keyword arguments to OPEN."
    (declare (ignore element-type external-format if-does-not-exist))
    (apply 'call-with-input-file (null-device-pathname) fun keys))
  (defmacro with-null-input ((var &rest keys
                              &key element-type external-format if-does-not-exist)
                             &body body)
    (declare (ignore element-type external-format if-does-not-exist))
    "Evaluate BODY in a context when VAR is bound to an input stream accessing the null device.
Pass keyword arguments to OPEN."
    `(call-with-null-input #'(lambda (,var) ,@body) ,@keys))
  (defun call-with-null-output (fun
                                &key (element-type *default-stream-element-type*)
                                  (external-format *utf-8-external-format*)
                                  (if-exists :overwrite)
                                  (if-does-not-exist :error))
    "Call FUN with an output stream to the null device; pass keyword arguments to OPEN."
    (call-with-output-file
     (null-device-pathname) fun
     :element-type element-type :external-format external-format
     :if-exists if-exists :if-does-not-exist if-does-not-exist))
  (defmacro with-null-output ((var &rest keys
                              &key element-type external-format if-does-not-exist if-exists)
                              &body body)
    "Evaluate BODY in a context when VAR is bound to an output stream accessing the null device.
Pass keyword arguments to OPEN."
    (declare (ignore element-type external-format if-exists if-does-not-exist))
    `(call-with-null-output #'(lambda (,var) ,@body) ,@keys)))

;;; Ensure output buffers are flushed
(with-upgradability ()
  (defun finish-outputs (&rest streams)
    "Finish output on the main output streams as well as any specified one.
Useful for portably flushing I/O before user input or program exit."
    ;; CCL notably buffers its stream output by default.
    (dolist (s (append streams
                       (list *stdout* *stderr* *error-output* *standard-output* *trace-output*
                             *debug-io* *terminal-io* *query-io*)))
      (ignore-errors (finish-output s)))
    (values))

  (defun format! (stream format &rest args)
    "Just like format, but call finish-outputs before and after the output."
    (finish-outputs stream)
    (apply 'format stream format args)
    (finish-outputs stream))

  (defun safe-format! (stream format &rest args)
    "Variant of FORMAT that is safe against both
dangerous syntax configuration and errors while printing."
    (with-safe-io-syntax ()
      (ignore-errors (apply 'format! stream format args))
      (finish-outputs stream)))) ; just in case format failed


;;; Simple Whole-Stream processing
(with-upgradability ()
  (defun copy-stream-to-stream (input output &key element-type buffer-size linewise prefix)
    "Copy the contents of the INPUT stream into the OUTPUT stream.
If LINEWISE is true, then read and copy the stream line by line, with an optional PREFIX.
Otherwise, using WRITE-SEQUENCE using a buffer of size BUFFER-SIZE."
    (with-open-stream (input input)
      (if linewise
          (loop* :for (line eof) = (multiple-value-list (read-line input nil nil))
                 :while line :do
                 (when prefix (princ prefix output))
                 (princ line output)
                 (unless eof (terpri output))
                 (finish-output output)
                 (when eof (return)))
          (loop
            :with buffer-size = (or buffer-size 8192)
            :with buffer = (make-array (list buffer-size) :element-type (or element-type 'character))
            :for end = (read-sequence buffer input)
            :until (zerop end)
            :do (write-sequence buffer output :end end)
                (when (< end buffer-size) (return))))))

  (defun concatenate-files (inputs output)
    "create a new OUTPUT file the contents of which a the concatenate of the INPUTS files."
    (with-open-file (o output :element-type '(unsigned-byte 8)
                              :direction :output :if-exists :rename-and-delete)
      (dolist (input inputs)
        (with-open-file (i input :element-type '(unsigned-byte 8)
                                 :direction :input :if-does-not-exist :error)
          (copy-stream-to-stream i o :element-type '(unsigned-byte 8))))))

  (defun copy-file (input output)
    "Copy contents of the INPUT file to the OUTPUT file"
    ;; Not available on LW personal edition or LW 6.0 on Mac: (lispworks:copy-file i f)
    #+allegro
    (excl.osi:copy-file input output)
    #+ecl
    (ext:copy-file input output)
    #-(or allegro ecl)
    (concatenate-files (list input) output))

  (defun slurp-stream-string (input &key (element-type 'character) stripped)
    "Read the contents of the INPUT stream as a string"
    (let ((string
            (with-open-stream (input input)
              (with-output-to-string (output)
                (copy-stream-to-stream input output :element-type element-type)))))
      (if stripped (stripln string) string)))

  (defun slurp-stream-lines (input &key count)
    "Read the contents of the INPUT stream as a list of lines, return those lines.

Note: relies on the Lisp's READ-LINE, but additionally removes any remaining CR
from the line-ending if the file or stream had CR+LF but Lisp only removed LF.

Read no more than COUNT lines."
    (check-type count (or null integer))
    (with-open-stream (input input)
      (loop :for n :from 0
            :for l = (and (or (not count) (< n count))
                          (read-line input nil nil))
            ;; stripln: to remove CR when the OS sends CRLF and Lisp only remove LF
            :while l :collect (stripln l))))

  (defun slurp-stream-line (input &key (at 0))
    "Read the contents of the INPUT stream as a list of lines,
then return the ACCESS-AT of that list of lines using the AT specifier.
PATH defaults to 0, i.e. return the first line.
PATH is typically an integer, or a list of an integer and a function.
If PATH is NIL, it will return all the lines in the file.

The stream will not be read beyond the Nth lines,
where N is the index specified by path
if path is either an integer or a list that starts with an integer."
    (access-at (slurp-stream-lines input :count (access-at-count at)) at))

  (defun slurp-stream-forms (input &key count)
    "Read the contents of the INPUT stream as a list of forms,
and return those forms.

If COUNT is null, read to the end of the stream;
if COUNT is an integer, stop after COUNT forms were read.

BEWARE: be sure to use WITH-SAFE-IO-SYNTAX, or some variant thereof"
    (check-type count (or null integer))
    (loop :with eof = '#:eof
          :for n :from 0
          :for form = (if (and count (>= n count))
                          eof
                          (read-preserving-whitespace input nil eof))
          :until (eq form eof) :collect form))

  (defun slurp-stream-form (input &key (at 0))
    "Read the contents of the INPUT stream as a list of forms,
then return the ACCESS-AT of these forms following the AT.
AT defaults to 0, i.e. return the first form.
AT is typically a list of integers.
If AT is NIL, it will return all the forms in the file.

The stream will not be read beyond the Nth form,
where N is the index specified by path,
if path is either an integer or a list that starts with an integer.

BEWARE: be sure to use WITH-SAFE-IO-SYNTAX, or some variant thereof"
    (access-at (slurp-stream-forms input :count (access-at-count at)) at))

  (defun read-file-string (file &rest keys)
    "Open FILE with option KEYS, read its contents as a string"
    (apply 'call-with-input-file file 'slurp-stream-string keys))

  (defun read-file-lines (file &rest keys)
    "Open FILE with option KEYS, read its contents as a list of lines
BEWARE: be sure to use WITH-SAFE-IO-SYNTAX, or some variant thereof"
    (apply 'call-with-input-file file 'slurp-stream-lines keys))

  (defun read-file-line (file &rest keys &key (at 0) &allow-other-keys)
    "Open input FILE with option KEYS (except AT),
and read its contents as per SLURP-STREAM-LINE with given AT specifier.
BEWARE: be sure to use WITH-SAFE-IO-SYNTAX, or some variant thereof"
    (apply 'call-with-input-file file
           #'(lambda (input) (slurp-stream-line input :at at))
           (remove-plist-key :at keys)))

  (defun read-file-forms (file &rest keys &key count &allow-other-keys)
    "Open input FILE with option KEYS (except COUNT),
and read its contents as per SLURP-STREAM-FORMS with given COUNT.
BEWARE: be sure to use WITH-SAFE-IO-SYNTAX, or some variant thereof"
    (apply 'call-with-input-file file
           #'(lambda (input) (slurp-stream-forms input :count count))
           (remove-plist-key :count keys)))

  (defun read-file-form (file &rest keys &key (at 0) &allow-other-keys)
    "Open input FILE with option KEYS (except AT),
and read its contents as per SLURP-STREAM-FORM with given AT specifier.
BEWARE: be sure to use WITH-SAFE-IO-SYNTAX, or some variant thereof"
    (apply 'call-with-input-file file
           #'(lambda (input) (slurp-stream-form input :at at))
           (remove-plist-key :at keys)))

  (defun safe-read-file-line (pathname &rest keys &key (package :cl) &allow-other-keys)
    "Reads the specified line from the top of a file using a safe standardized syntax.
Extracts the line using READ-FILE-LINE,
within an WITH-SAFE-IO-SYNTAX using the specified PACKAGE."
    (with-safe-io-syntax (:package package)
      (apply 'read-file-line pathname (remove-plist-key :package keys))))

  (defun safe-read-file-form (pathname &rest keys &key (package :cl) &allow-other-keys)
    "Reads the specified form from the top of a file using a safe standardized syntax.
Extracts the form using READ-FILE-FORM,
within an WITH-SAFE-IO-SYNTAX using the specified PACKAGE."
    (with-safe-io-syntax (:package package)
      (apply 'read-file-form pathname (remove-plist-key :package keys))))

  (defun eval-input (input)
    "Portably read and evaluate forms from INPUT, return the last values."
    (with-input (input)
      (loop :with results :with eof ='#:eof
            :for form = (read input nil eof)
            :until (eq form eof)
            :do (setf results (multiple-value-list (eval form)))
            :finally (return (values-list results)))))

  (defun eval-thunk (thunk)
    "Evaluate a THUNK of code:
If a function, FUNCALL it without arguments.
If a constant literal and not a sequence, return it.
If a cons or a symbol, EVAL it.
If a string, repeatedly read and evaluate from it, returning the last values."
    (etypecase thunk
      ((or boolean keyword number character pathname) thunk)
      ((or cons symbol) (eval thunk))
      (function (funcall thunk))
      (string (eval-input thunk))))

  (defun standard-eval-thunk (thunk &key (package :cl))
    "Like EVAL-THUNK, but in a more standardized evaluation context."
    ;; Note: it's "standard-" not "safe-", because evaluation is never safe.
    (when thunk
      (with-safe-io-syntax (:package package)
        (let ((*read-eval* t))
          (eval-thunk thunk))))))

(with-upgradability ()
  (defun println (x &optional (stream *standard-output*))
    "Variant of PRINC that also calls TERPRI afterwards"
    (princ x stream) (terpri stream) (finish-output stream) (values))

  (defun writeln (x &rest keys &key (stream *standard-output*) &allow-other-keys)
    "Variant of WRITE that also calls TERPRI afterwards"
    (apply 'write x keys) (terpri stream) (finish-output stream) (values)))


;;; Using temporary files
(with-upgradability ()
  (defun default-temporary-directory ()
    "Return a default directory to use for temporary files"
    (os-cond
      ((os-unix-p)
       (or (getenv-pathname "TMPDIR" :ensure-directory t)
           (parse-native-namestring "/tmp/")))
      ((os-windows-p)
       (getenv-pathname "TEMP" :ensure-directory t))
      (t (subpathname (user-homedir-pathname) "tmp/"))))

  (defvar *temporary-directory* nil "User-configurable location for temporary files")

  (defun temporary-directory ()
    "Return a directory to use for temporary files"
    (or *temporary-directory* (default-temporary-directory)))

  (defun setup-temporary-directory ()
    "Configure a default temporary directory to use."
    (setf *temporary-directory* (default-temporary-directory))
    #+gcl (setf system::*tmp-dir* *temporary-directory*))

  (defun call-with-temporary-file
      (thunk &key
               (want-stream-p t) (want-pathname-p t) (direction :io) keep after
               directory (type "tmp" typep) prefix (suffix (when typep "-tmp"))
               (element-type *default-stream-element-type*)
               (external-format *utf-8-external-format*))
    "Call a THUNK with stream and/or pathname arguments identifying a temporary file.

The temporary file's pathname will be based on concatenating
PREFIX (or \"tmp\" if it's NIL), a random alphanumeric string,
and optional SUFFIX (defaults to \"-tmp\" if a type was provided)
and TYPE (defaults to \"tmp\", using a dot as separator if not NIL),
within DIRECTORY (defaulting to the TEMPORARY-DIRECTORY) if the PREFIX isn't absolute.

The file will be open with specified DIRECTION (defaults to :IO),
ELEMENT-TYPE (defaults to *DEFAULT-STREAM-ELEMENT-TYPE*) and
EXTERNAL-FORMAT (defaults to *UTF-8-EXTERNAL-FORMAT*).
If WANT-STREAM-P is true (the defaults to T), then THUNK will then be CALL-FUNCTION'ed
with the stream and the pathname (if WANT-PATHNAME-P is true, defaults to T),
and stream will be closed after the THUNK exits (either normally or abnormally).
If WANT-STREAM-P is false, then WANT-PATHAME-P must be true, and then
THUNK is only CALL-FUNCTION'ed after the stream is closed, with the pathname as argument.
Upon exit of THUNK, the AFTER thunk if defined is CALL-FUNCTION'ed with the pathname as argument.
If AFTER is defined, its results are returned, otherwise, the results of THUNK are returned.
Finally, the file will be deleted, unless the KEEP argument when CALL-FUNCTION'ed returns true."
    #+xcl (declare (ignorable typep))
    (check-type direction (member :output :io))
    (assert (or want-stream-p want-pathname-p))
    (loop
      :with prefix-pn = (ensure-absolute-pathname
                         (or prefix "tmp")
                         (or (ensure-pathname
                              directory
                              :namestring :native
                              :ensure-directory t
                              :ensure-physical t)
                             #'temporary-directory))
      :with prefix-nns = (native-namestring prefix-pn)
      :with results = (progn (ensure-directories-exist prefix-pn)
                             ())
      :for counter :from (random (expt 36 #-gcl 8 #+gcl 5))
      :for pathname = (parse-native-namestring
                       (format nil "~A~36R~@[~A~]~@[.~A~]"
                               prefix-nns counter suffix (unless (eq type :unspecific) type)))
      :for okp = nil :do
        ;; TODO: on Unix, do something about umask
        ;; TODO: on Unix, audit the code so we make sure it uses O_CREAT|O_EXCL
        ;; TODO: on Unix, use CFFI and mkstemp --
        ;; except UIOP is precisely meant to not depend on CFFI or on anything! Grrrr.
        ;; Can we at least design some hook?
        (unwind-protect
             (progn
               (ensure-directories-exist pathname)
               (with-open-file (stream pathname
                                       :direction direction
                                       :element-type element-type
                                       :external-format external-format
                                       :if-exists nil :if-does-not-exist :create)
                 (when stream
                   (setf okp pathname)
                   (when want-stream-p
                     ;; Note: can't return directly from within with-open-file
                     ;; or the non-local return causes the file creation to be undone.
                     (setf results (multiple-value-list
                                    (if want-pathname-p
                                        (funcall thunk stream pathname)
                                        (funcall thunk stream)))))))
               (cond
                 ((not okp) nil)
                 (after (return (call-function after okp)))
                 ((and want-pathname-p (not want-stream-p)) (return (call-function thunk okp)))
                 (t (return (values-list results)))))
          (when (and okp (not (call-function keep)))
            (ignore-errors (delete-file-if-exists okp))))))

  (defmacro with-temporary-file ((&key (stream (gensym "STREAM") streamp)
                                    (pathname (gensym "PATHNAME") pathnamep)
                                    directory prefix suffix type
                                    keep direction element-type external-format)
                                 &body body)
    "Evaluate BODY where the symbols specified by keyword arguments
STREAM and PATHNAME (if respectively specified) are bound corresponding
to a newly created temporary file ready for I/O, as per CALL-WITH-TEMPORARY-FILE.
At least one of STREAM or PATHNAME must be specified.
If the STREAM is not specified, it will be closed before the BODY is evaluated.
If STREAM is specified, then the :CLOSE-STREAM label if it appears in the BODY,
separates forms run before and after the stream is closed.
The values of the last form of the BODY (not counting the separating :CLOSE-STREAM) are returned.
Upon success, the KEEP form is evaluated and the file is is deleted unless it evaluates to TRUE."
    (check-type stream symbol)
    (check-type pathname symbol)
    (assert (or streamp pathnamep))
    (let* ((afterp (position :close-stream body))
           (before (if afterp (subseq body 0 afterp) body))
           (after (when afterp (subseq body (1+ afterp))))
           (beforef (gensym "BEFORE"))
           (afterf (gensym "AFTER")))
      `(flet (,@(when before
                  `((,beforef (,@(when streamp `(,stream)) ,@(when pathnamep `(,pathname)))
                       ,@(when after `((declare (ignorable ,pathname))))
                       ,@before)))
              ,@(when after
                  (assert pathnamep)
                  `((,afterf (,pathname) ,@after))))
         #-gcl (declare (dynamic-extent ,@(when before `(#',beforef)) ,@(when after `(#',afterf))))
         (call-with-temporary-file
          ,(when before `#',beforef)
          :want-stream-p ,streamp
          :want-pathname-p ,pathnamep
          ,@(when direction `(:direction ,direction))
          ,@(when directory `(:directory ,directory))
          ,@(when prefix `(:prefix ,prefix))
          ,@(when suffix `(:suffix ,suffix))
          ,@(when type `(:type ,type))
          ,@(when keep `(:keep ,keep))
          ,@(when after `(:after #',afterf))
          ,@(when element-type `(:element-type ,element-type))
          ,@(when external-format `(:external-format ,external-format))))))

  (defun get-temporary-file (&key directory prefix suffix type)
    (with-temporary-file (:pathname pn :keep t
                          :directory directory :prefix prefix :suffix suffix :type type)
      pn))

  ;; Temporary pathnames in simple cases where no contention is assumed
  (defun add-pathname-suffix (pathname suffix &rest keys)
    "Add a SUFFIX to the name of a PATHNAME, return a new pathname.
Further KEYS can be passed to MAKE-PATHNAME."
    (apply 'make-pathname :name (strcat (pathname-name pathname) suffix)
                          :defaults pathname keys))

  (defun tmpize-pathname (x)
    "Return a new pathname modified from X by adding a trivial random suffix.
A new empty file with said temporary pathname is created, to ensure there is no
clash with any concurrent process attempting the same thing."
    (let* ((px (ensure-pathname x :ensure-physical t))
           (prefix (if-let (n (pathname-name px)) (strcat n "-tmp") "tmp"))
           (directory (pathname-directory-pathname px)))
      (get-temporary-file :directory directory :prefix prefix :type (pathname-type px))))

  (defun call-with-staging-pathname (pathname fun)
    "Calls FUN with a staging pathname, and atomically
renames the staging pathname to the PATHNAME in the end.
NB: this protects only against failure of the program, not against concurrent attempts.
For the latter case, we ought pick a random suffix and atomically open it."
    (let* ((pathname (pathname pathname))
           (staging (tmpize-pathname pathname)))
      (unwind-protect
           (multiple-value-prog1
               (funcall fun staging)
             (rename-file-overwriting-target staging pathname))
        (delete-file-if-exists staging))))

  (defmacro with-staging-pathname ((pathname-var &optional (pathname-value pathname-var)) &body body)
    "Trivial syntax wrapper for CALL-WITH-STAGING-PATHNAME"
    `(call-with-staging-pathname ,pathname-value #'(lambda (,pathname-var) ,@body))))

(with-upgradability ()
  (defun file-stream-p (stream)
    (typep stream 'file-stream))
  (defun file-or-synonym-stream-p (stream)
    (or (file-stream-p stream)
        (and (typep stream 'synonym-stream)
             (file-or-synonym-stream-p
              (symbol-value (synonym-stream-symbol stream)))))))
;;;; -------------------------------------------------------------------------
;;;; Starting, Stopping, Dumping a Lisp image

(uiop/package:define-package :uiop/image
  (:use :uiop/common-lisp :uiop/package :uiop/utility :uiop/pathname :uiop/stream :uiop/os)
  (:export
   #:*image-dumped-p* #:raw-command-line-arguments #:*command-line-arguments*
   #:command-line-arguments #:raw-command-line-arguments #:setup-command-line-arguments #:argv0
   #:*lisp-interaction*
   #:fatal-condition #:fatal-condition-p
   #:handle-fatal-condition
   #:call-with-fatal-condition-handler #:with-fatal-condition-handler
   #:*image-restore-hook* #:*image-prelude* #:*image-entry-point*
   #:*image-postlude* #:*image-dump-hook*
   #:quit #:die #:raw-print-backtrace #:print-backtrace #:print-condition-backtrace
   #:shell-boolean-exit
   #:register-image-restore-hook #:register-image-dump-hook
   #:call-image-restore-hook #:call-image-dump-hook
   #:restore-image #:dump-image #:create-image
))
(in-package :uiop/image)

(with-upgradability ()
  (defvar *lisp-interaction* t
    "Is this an interactive Lisp environment, or is it batch processing?")

  (defvar *command-line-arguments* nil
    "Command-line arguments")

  (defvar *image-dumped-p* nil ; may matter as to how to get to command-line-arguments
    "Is this a dumped image? As a standalone executable?")

  (defvar *image-restore-hook* nil
    "Functions to call (in reverse order) when the image is restored")

  (defvar *image-restored-p* nil
    "Has the image been restored? A boolean, or :in-progress while restoring, :in-regress while dumping")

  (defvar *image-prelude* nil
    "a form to evaluate, or string containing forms to read and evaluate
when the image is restarted, but before the entry point is called.")

  (defvar *image-entry-point* nil
    "a function with which to restart the dumped image when execution is restored from it.")

  (defvar *image-postlude* nil
    "a form to evaluate, or string containing forms to read and evaluate
before the image dump hooks are called and before the image is dumped.")

  (defvar *image-dump-hook* nil
    "Functions to call (in order) when before an image is dumped"))

(eval-when (#-lispworks :compile-toplevel :load-toplevel :execute)
  (deftype fatal-condition ()
    `(and serious-condition #+clozure (not ccl:process-reset))))

;;; Exiting properly or im-
(with-upgradability ()
  (defun quit (&optional (code 0) (finish-output t))
    "Quits from the Lisp world, with the given exit status if provided.
This is designed to abstract away the implementation specific quit forms."
    (when finish-output ;; essential, for ClozureCL, and for standard compliance.
      (finish-outputs))
    #+(or abcl xcl) (ext:quit :status code)
    #+allegro (excl:exit code :quiet t)
    #+(or clasp ecl) (si:quit code)
    #+clisp (ext:quit code)
    #+clozure (ccl:quit code)
    #+cormanlisp (win32:exitprocess code)
    #+(or cmucl scl) (unix:unix-exit code)
    #+gcl (system:quit code)
    #+genera (error "~S: You probably don't want to Halt Genera. (code: ~S)" 'quit code)
    #+lispworks (lispworks:quit :status code :confirm nil :return nil :ignore-errors-p t)
    #+mcl (progn code (ccl:quit)) ;; or should we use FFI to call libc's exit(3) ?
    #+mkcl (mk-ext:quit :exit-code code)
    #+sbcl #.(let ((exit (find-symbol* :exit :sb-ext nil))
                   (quit (find-symbol* :quit :sb-ext nil)))
               (cond
                 (exit `(,exit :code code :abort (not finish-output)))
                 (quit `(,quit :unix-status code :recklessly-p (not finish-output)))))
    #-(or abcl allegro clasp clisp clozure cmucl ecl gcl genera lispworks mcl mkcl sbcl scl xcl)
    (not-implemented-error 'quit "(called with exit code ~S)" code))

  (defun die (code format &rest arguments)
    "Die in error with some error message"
    (with-safe-io-syntax ()
      (ignore-errors
       (format! *stderr* "~&~?~&" format arguments)))
    (quit code))

  (defun raw-print-backtrace (&key (stream *debug-io*) count condition)
    "Print a backtrace, directly accessing the implementation"
    (declare (ignorable stream count condition))
    #+abcl
    (loop :for i :from 0
          :for frame :in (sys:backtrace (or count most-positive-fixnum)) :do
            (safe-format! stream "~&~D: ~A~%" i frame))
    #+allegro
    (let ((*terminal-io* stream)
          (*standard-output* stream)
          (tpl:*zoom-print-circle* *print-circle*)
          (tpl:*zoom-print-level* *print-level*)
          (tpl:*zoom-print-length* *print-length*))
      (tpl:do-command "zoom"
        :from-read-eval-print-loop nil
        :count (or count t)
        :all t))
    #+(or clasp ecl mkcl)
    (let* ((top (si:ihs-top))
           (repeats (if count (min top count) top))
           (backtrace (loop :for ihs :from 0 :below top
                            :collect (list (si::ihs-fun ihs)
                                           (si::ihs-env ihs)))))
      (loop :for i :from 0 :below repeats
            :for frame :in (nreverse backtrace) :do
              (safe-format! stream "~&~D: ~S~%" i frame)))
    #+clisp
    (system::print-backtrace :out stream :limit count)
    #+(or clozure mcl)
    (let ((*debug-io* stream))
      #+clozure (ccl:print-call-history :count count :start-frame-number 1)
      #+mcl (ccl:print-call-history :detailed-p nil)
      (finish-output stream))
    #+(or cmucl scl)
    (let ((debug:*debug-print-level* *print-level*)
          (debug:*debug-print-length* *print-length*))
      (debug:backtrace (or count most-positive-fixnum) stream))
    #+gcl
    (let ((*debug-io* stream))
      (ignore-errors
       (with-safe-io-syntax ()
         (if condition
             (conditions::condition-backtrace condition)
             (system::simple-backtrace)))))
    #+lispworks
    (let ((dbg::*debugger-stack*
            (dbg::grab-stack nil :how-many (or count most-positive-fixnum)))
          (*debug-io* stream)
          (dbg:*debug-print-level* *print-level*)
          (dbg:*debug-print-length* *print-length*))
      (dbg:bug-backtrace nil))
    #+sbcl
    (sb-debug:print-backtrace :stream stream :count (or count most-positive-fixnum))
    #+xcl
    (loop :for i :from 0 :below (or count most-positive-fixnum)
          :for frame :in (extensions:backtrace-as-list) :do
            (safe-format! stream "~&~D: ~S~%" i frame)))

  (defun print-backtrace (&rest keys &key stream count condition)
    "Print a backtrace"
    (declare (ignore stream count condition))
    (with-safe-io-syntax (:package :cl)
      (let ((*print-readably* nil)
            (*print-circle* t)
            (*print-miser-width* 75)
            (*print-length* nil)
            (*print-level* nil)
            (*print-pretty* t))
        (ignore-errors (apply 'raw-print-backtrace keys)))))

  (defun print-condition-backtrace (condition &key (stream *stderr*) count)
    "Print a condition after a backtrace triggered by that condition"
    ;; We print the condition *after* the backtrace,
    ;; for the sake of who sees the backtrace at a terminal.
    ;; It is up to the caller to print the condition *before*, with some context.
    (print-backtrace :stream stream :count count :condition condition)
    (when condition
      (safe-format! stream "~&Above backtrace due to this condition:~%~A~&"
                    condition)))

  (defun fatal-condition-p (condition)
    "Is the CONDITION fatal?"
    (typep condition 'fatal-condition))

  (defun handle-fatal-condition (condition)
    "Handle a fatal CONDITION:
depending on whether *LISP-INTERACTION* is set, enter debugger or die"
    (cond
      (*lisp-interaction*
       (invoke-debugger condition))
      (t
       (safe-format! *stderr* "~&Fatal condition:~%~A~%" condition)
       (print-condition-backtrace condition :stream *stderr*)
       (die 99 "~A" condition))))

  (defun call-with-fatal-condition-handler (thunk)
    "Call THUNK in a context where fatal conditions are appropriately handled"
    (handler-bind ((fatal-condition #'handle-fatal-condition))
      (funcall thunk)))

  (defmacro with-fatal-condition-handler ((&optional) &body body)
    "Execute BODY in a context where fatal conditions are appropriately handled"
    `(call-with-fatal-condition-handler #'(lambda () ,@body)))

  (defun shell-boolean-exit (x)
    "Quit with a return code that is 0 iff argument X is true"
    (quit (if x 0 1))))


;;; Using image hooks
(with-upgradability ()
  (defun register-image-restore-hook (hook &optional (call-now-p t))
    "Regiter a hook function to be run when restoring a dumped image"
    (register-hook-function '*image-restore-hook* hook call-now-p))

  (defun register-image-dump-hook (hook &optional (call-now-p nil))
    "Register a the hook function to be run before to dump an image"
    (register-hook-function '*image-dump-hook* hook call-now-p))

  (defun call-image-restore-hook ()
    "Call the hook functions registered to be run when restoring a dumped image"
    (call-functions (reverse *image-restore-hook*)))

  (defun call-image-dump-hook ()
    "Call the hook functions registered to be run before to dump an image"
    (call-functions *image-dump-hook*)))


;;; Proper command-line arguments
(with-upgradability ()
  (defun raw-command-line-arguments ()
    "Find what the actual command line for this process was."
    #+abcl ext:*command-line-argument-list* ; Use 1.0.0 or later!
    #+allegro (sys:command-line-arguments) ; default: :application t
    #+(or clasp ecl) (loop :for i :from 0 :below (si:argc) :collect (si:argv i))
    #+clisp (coerce (ext:argv) 'list)
    #+clozure ccl:*command-line-argument-list*
    #+(or cmucl scl) extensions:*command-line-strings*
    #+gcl si:*command-args*
    #+(or genera mcl) nil
    #+lispworks sys:*line-arguments-list*
    #+mkcl (loop :for i :from 0 :below (mkcl:argc) :collect (mkcl:argv i))
    #+sbcl sb-ext:*posix-argv*
    #+xcl system:*argv*
    #-(or abcl allegro clasp clisp clozure cmucl ecl gcl genera lispworks mcl mkcl sbcl scl xcl)
    (not-implemented-error 'raw-command-line-arguments))

  (defun command-line-arguments (&optional (arguments (raw-command-line-arguments)))
    "Extract user arguments from command-line invocation of current process.
Assume the calling conventions of a generated script that uses --
if we are not called from a directly executable image."
    (block nil
      #+abcl (return arguments)
      ;; SBCL and Allegro already separate user arguments from implementation arguments.
      #-(or sbcl allegro)
      (unless (eq *image-dumped-p* :executable)
        ;; LispWorks command-line processing isn't transparent to the user
        ;; unless you create a standalone executable; in that case,
        ;; we rely on cl-launch or some other script to set the arguments for us.
        #+lispworks (return *command-line-arguments*)
        ;; On other implementations, on non-standalone executables,
        ;; we trust cl-launch or whichever script starts the program
        ;; to use -- as a delimiter between implementation arguments and user arguments.
        #-lispworks (setf arguments (member "--" arguments :test 'string-equal)))
      (rest arguments)))

  (defun argv0 ()
    "On supported implementations (most that matter), or when invoked by a proper wrapper script,
return a string that for the name with which the program was invoked, i.e. argv[0] in C.
Otherwise, return NIL."
    (cond
      ((eq *image-dumped-p* :executable) ; yes, this ARGV0 is our argv0 !
       ;; NB: not currently available on ABCL, Corman, Genera, MCL
       (or #+(or allegro clisp clozure cmucl gcl lispworks sbcl scl xcl)
           (first (raw-command-line-arguments))
           #+(or clasp ecl) (si:argv 0) #+mkcl (mkcl:argv 0)))
      (t ;; argv[0] is the name of the interpreter.
       ;; The wrapper script can export __CL_ARGV0. cl-launch does as of 4.0.1.8.
       (getenvp "__CL_ARGV0"))))

  (defun setup-command-line-arguments ()
    (setf *command-line-arguments* (command-line-arguments)))

  (defun restore-image (&key
                          (lisp-interaction *lisp-interaction*)
                          (restore-hook *image-restore-hook*)
                          (prelude *image-prelude*)
                          (entry-point *image-entry-point*)
                          (if-already-restored '(cerror "RUN RESTORE-IMAGE ANYWAY")))
    "From a freshly restarted Lisp image, restore the saved Lisp environment
by setting appropriate variables, running various hooks, and calling any specified entry point.

If the image has already been restored or is already being restored, as per *IMAGE-RESTORED-P*,
call the IF-ALREADY-RESTORED error handler (by default, a continuable error), and do return
immediately to the surrounding restore process if allowed to continue.

Then, comes the restore process itself:
First, call each function in the RESTORE-HOOK,
in the order they were registered with REGISTER-IMAGE-RESTORE-HOOK.
Second, evaluate the prelude, which is often Lisp text that is read,
as per EVAL-INPUT.
Third, call the ENTRY-POINT function, if any is specified, with no argument.

The restore process happens in a WITH-FATAL-CONDITION-HANDLER, so that if LISP-INTERACTION is NIL,
any unhandled error leads to a backtrace and an exit with an error status.
If LISP-INTERACTION is NIL, the process also exits when no error occurs:
if neither restart nor entry function is provided, the program will exit with status 0 (success);
if a function was provided, the program will exit after the function returns (if it returns),
with status 0 if and only if the primary return value of result is generalized boolean true,
and with status 1 if this value is NIL.

If LISP-INTERACTION is true, unhandled errors will take you to the debugger, and the result
of the function will be returned rather than interpreted as a boolean designating an exit code."
    (when *image-restored-p*
      (if if-already-restored
          (call-function if-already-restored "Image already ~:[being ~;~]restored"
                         (eq *image-restored-p* t))
          (return-from restore-image)))
    (with-fatal-condition-handler ()
      (setf *lisp-interaction* lisp-interaction)
      (setf *image-restore-hook* restore-hook)
      (setf *image-prelude* prelude)
      (setf *image-restored-p* :in-progress)
      (call-image-restore-hook)
      (standard-eval-thunk prelude)
      (setf *image-restored-p* t)
      (let ((results (multiple-value-list
                      (if entry-point
                          (call-function entry-point)
                          t))))
        (if lisp-interaction
            (values-list results)
            (shell-boolean-exit (first results)))))))


;;; Dumping an image

(with-upgradability ()
  (defun dump-image (filename &key output-name executable
                                (postlude *image-postlude*)
                                (dump-hook *image-dump-hook*)
                                #+clozure prepend-symbols #+clozure (purify t)
                                #+sbcl compression
                                #+(and sbcl os-windows) application-type)
    "Dump an image of the current Lisp environment at pathname FILENAME, with various options.

First, finalize the image, by evaluating the POSTLUDE as per EVAL-INPUT, then calling each of
 the functions in DUMP-HOOK, in reverse order of registration by REGISTER-DUMP-HOOK.

If EXECUTABLE is true, create an standalone executable program that calls RESTORE-IMAGE on startup.

Pass various implementation-defined options, such as PREPEND-SYMBOLS and PURITY on CCL,
or COMPRESSION on SBCL, and APPLICATION-TYPE on SBCL/Windows."
    ;; Note: at least SBCL saves only global values of variables in the heap image,
    ;; so make sure things you want to dump are NOT just local bindings shadowing the global values.
    (declare (ignorable filename output-name executable))
    (setf *image-dumped-p* (if executable :executable t))
    (setf *image-restored-p* :in-regress)
    (setf *image-postlude* postlude)
    (standard-eval-thunk *image-postlude*)
    (setf *image-dump-hook* dump-hook)
    (call-image-dump-hook)
    (setf *image-restored-p* nil)
    #-(or clisp clozure (and cmucl executable) lispworks sbcl scl)
    (when executable
      (not-implemented-error 'dump-image "dumping an executable"))
    #+allegro
    (progn
      (sys:resize-areas :global-gc t :pack-heap t :sift-old-areas t :tenure t) ; :new 5000000
      (excl:dumplisp :name filename :suppress-allegro-cl-banner t))
    #+clisp
    (apply #'ext:saveinitmem filename
           :quiet t
           :start-package *package*
           :keep-global-handlers nil
           :executable (if executable 0 t) ;--- requires clisp 2.48 or later, still catches --clisp-x
           (when executable
             (list
              ;; :parse-options nil ;--- requires a non-standard patch to clisp.
              :norc t :script nil :init-function #'restore-image)))
    #+clozure
    (flet ((dump (prepend-kernel)
             (ccl:save-application filename :prepend-kernel prepend-kernel :purify purify
                                            :toplevel-function (when executable #'restore-image))))
      ;;(setf ccl::*application* (make-instance 'ccl::lisp-development-system))
      (if prepend-symbols
          (with-temporary-file (:prefix "ccl-symbols-" :direction :output :pathname path)
            (require 'elf)
            (funcall (fdefinition 'ccl::write-elf-symbols-to-file) path)
            (dump path))
          (dump t)))
    #+(or cmucl scl)
    (progn
      (ext:gc :full t)
      (setf ext:*batch-mode* nil)
      (setf ext::*gc-run-time* 0)
      (apply 'ext:save-lisp filename
             :allow-other-keys t ;; hush SCL and old versions of CMUCL
             #+(and cmucl executable) :executable #+(and cmucl executable) t
             (when executable '(:init-function restore-image :process-command-line nil
                                :quiet t :load-init-file nil :site-init nil))))
    #+gcl
    (progn
      (si::set-hole-size 500) (si::gbc nil) (si::sgc-on t)
      (si::save-system filename))
    #+lispworks
    (if executable
        (lispworks:deliver 'restore-image filename 0 :interface nil)
        (hcl:save-image filename :environment nil))
    #+sbcl
    (progn
      ;;(sb-pcl::precompile-random-code-segments) ;--- it is ugly slow at compile-time (!) when the initial core is a big CLOS program. If you want it, do it yourself
      (setf sb-ext::*gc-run-time* 0)
      (apply 'sb-ext:save-lisp-and-die filename
             :executable t ;--- always include the runtime that goes with the core
             (append
              (when compression (list :compression compression))
              ;;--- only save runtime-options for standalone executables
              (when executable (list :toplevel #'restore-image :save-runtime-options t))
              #+(and sbcl os-windows) ;; passing :application-type :gui will disable the console window.
              ;; the default is :console - only works with SBCL 1.1.15 or later.
              (when application-type (list :application-type application-type)))))
    #-(or allegro clisp clozure cmucl gcl lispworks sbcl scl)
    (not-implemented-error 'dump-image))

  (defun create-image (destination lisp-object-files
                       &key kind output-name prologue-code epilogue-code extra-object-files
                         (prelude () preludep) (postlude () postludep)
                         (entry-point () entry-point-p) build-args no-uiop)
    (declare (ignorable destination lisp-object-files extra-object-files kind output-name
                        prologue-code epilogue-code prelude preludep postlude postludep
                        entry-point entry-point-p build-args no-uiop))
    "On ECL, create an executable at pathname DESTINATION from the specified OBJECT-FILES and options"
    ;; Is it meaningful to run these in the current environment?
    ;; only if we also track the object files that constitute the "current" image,
    ;; and otherwise simulate dump-image, including quitting at the end.
    #-(or clasp ecl mkcl) (not-implemented-error 'create-image)
    #+(or clasp ecl mkcl)
    (let ((epilogue-code
           (if no-uiop
               epilogue-code
               (let ((forms
                      (append
                       (when epilogue-code `(,epilogue-code))
                       (when postludep `((setf *image-postlude* ',postlude)))
                       (when preludep `((setf *image-prelude* ',prelude)))
                       (when entry-point-p `((setf *image-entry-point* ',entry-point)))
                       (case kind
                         ((:image)
                          (setf kind :program) ;; to ECL, it's just another program.
                          `((setf *image-dumped-p* t)
                            (si::top-level #+(or clasp ecl) t) (quit)))
                         ((:program)
                          `((setf *image-dumped-p* :executable)
                            (shell-boolean-exit
                             (restore-image))))))))
                 (when forms `(progn ,@forms))))))
      #+(or clasp ecl mkcl)
      (check-type kind (member :dll :shared-library :lib :static-library
                               :fasl :fasb :program))
      (apply #+clasp 'cmp:builder #+clasp kind
             #+(or ecl mkcl)
             (ecase kind
               ((:dll :shared-library)
                #+ecl 'c::build-shared-library #+mkcl 'compiler:build-shared-library)
               ((:lib :static-library)
                #+ecl 'c::build-static-library #+mkcl 'compiler:build-static-library)
               ((:fasl #+ecl :fasb)
                #+ecl 'c::build-fasl #+mkcl 'compiler:build-fasl)
               #+mkcl ((:fasb) 'compiler:build-bundle)
               ((:program)
                #+ecl 'c::build-program #+mkcl 'compiler:build-program))
             (pathname destination)
             #+(or clasp ecl) :lisp-files #+mkcl :lisp-object-files
             (append lisp-object-files #+(or clasp ecl) extra-object-files)
             #+ecl :init-name
             #+ecl (getf build-args :init-name)
             (append
              (when prologue-code `(:prologue-code ,prologue-code))
              (when epilogue-code `(:epilogue-code ,epilogue-code))
              #+mkcl (when extra-object-files `(:object-files ,extra-object-files))
              build-args)))))


;;; Some universal image restore hooks
(with-upgradability ()
  (map () 'register-image-restore-hook
       '(setup-stdin setup-stdout setup-stderr
         setup-command-line-arguments setup-temporary-directory
         #+abcl detect-os)))
;;;; -------------------------------------------------------------------------
;;;; Support to build (compile and load) Lisp files

(uiop/package:define-package :uiop/lisp-build
  (:nicknames :asdf/lisp-build) ;; OBSOLETE, used by slime/contrib/swank-asdf.lisp
  (:use :uiop/common-lisp :uiop/package :uiop/utility
   :uiop/os :uiop/pathname :uiop/filesystem :uiop/stream :uiop/image)
  (:export
   ;; Variables
   #:*compile-file-warnings-behaviour* #:*compile-file-failure-behaviour*
   #:*output-translation-function*
   #:*optimization-settings* #:*previous-optimization-settings*
   #:*base-build-directory*
   #:compile-condition #:compile-file-error #:compile-warned-error #:compile-failed-error
   #:compile-warned-warning #:compile-failed-warning
   #:check-lisp-compile-results #:check-lisp-compile-warnings
   #:*uninteresting-conditions* #:*usual-uninteresting-conditions*
   #:*uninteresting-compiler-conditions* #:*uninteresting-loader-conditions*
   ;; Types
   #+sbcl #:sb-grovel-unknown-constant-condition
   ;; Functions & Macros
   #:get-optimization-settings #:proclaim-optimization-settings #:with-optimization-settings
   #:call-with-muffled-compiler-conditions #:with-muffled-compiler-conditions
   #:call-with-muffled-loader-conditions #:with-muffled-loader-conditions
   #:reify-simple-sexp #:unreify-simple-sexp
   #:reify-deferred-warnings #:unreify-deferred-warnings
   #:reset-deferred-warnings #:save-deferred-warnings #:check-deferred-warnings
   #:with-saved-deferred-warnings #:warnings-file-p #:warnings-file-type #:*warnings-file-type*
   #:enable-deferred-warnings-check #:disable-deferred-warnings-check
   #:current-lisp-file-pathname #:load-pathname
   #:lispize-pathname #:compile-file-type #:call-around-hook
   #:compile-file* #:compile-file-pathname* #:*compile-check*
   #:load* #:load-from-string #:combine-fasls)
  (:intern #:defaults #:failure-p #:warnings-p #:s #:y #:body))
(in-package :uiop/lisp-build)

(with-upgradability ()
  (defvar *compile-file-warnings-behaviour*
    (or #+clisp :ignore :warn)
    "How should ASDF react if it encounters a warning when compiling a file?
Valid values are :error, :warn, and :ignore.")

  (defvar *compile-file-failure-behaviour*
    (or #+(or mkcl sbcl) :error #+clisp :ignore :warn)
    "How should ASDF react if it encounters a failure (per the ANSI spec of COMPILE-FILE)
when compiling a file, which includes any non-style-warning warning.
Valid values are :error, :warn, and :ignore.
Note that ASDF ALWAYS raises an error if it fails to create an output file when compiling.")

  (defvar *base-build-directory* nil
    "When set to a non-null value, it should be an absolute directory pathname,
which will serve as the *DEFAULT-PATHNAME-DEFAULTS* around a COMPILE-FILE,
what more while the input-file is shortened if possible to ENOUGH-PATHNAME relative to it.
This can help you produce more deterministic output for FASLs."))

;;; Optimization settings
(with-upgradability ()
  (defvar *optimization-settings* nil
    "Optimization settings to be used by PROCLAIM-OPTIMIZATION-SETTINGS")
  (defvar *previous-optimization-settings* nil
    "Optimization settings saved by PROCLAIM-OPTIMIZATION-SETTINGS")
  (defparameter +optimization-variables+
    ;; TODO: allegro genera corman mcl
    (or #+(or abcl xcl) '(system::*speed* system::*space* system::*safety* system::*debug*)
        #+clisp '() ;; system::*optimize* is a constant hash-table! (with non-constant contents)
        #+clozure '(ccl::*nx-speed* ccl::*nx-space* ccl::*nx-safety*
                    ccl::*nx-debug* ccl::*nx-cspeed*)
        #+(or cmucl scl) '(c::*default-cookie*)
        #+clasp '()
        #+ecl (unless (use-ecl-byte-compiler-p) '(c::*speed* c::*space* c::*safety* c::*debug*))
        #+gcl '(compiler::*speed* compiler::*space* compiler::*compiler-new-safety* compiler::*debug*)
        #+lispworks '(compiler::*optimization-level*)
        #+mkcl '(si::*speed* si::*space* si::*safety* si::*debug*)
        #+sbcl '(sb-c::*policy*)))
  (defun get-optimization-settings ()
    "Get current compiler optimization settings, ready to PROCLAIM again"
    #-(or abcl allegro clasp clisp clozure cmucl ecl lispworks mkcl sbcl scl xcl)
    (warn "~S does not support ~S. Please help me fix that."
          'get-optimization-settings (implementation-type))
    #+(or abcl allegro clasp clisp clozure cmucl ecl lispworks mkcl sbcl scl xcl)
    (let ((settings '(speed space safety debug compilation-speed #+(or cmucl scl) c::brevity)))
      #.`(loop #+(or allegro clozure)
               ,@'(:with info = #+allegro (sys:declaration-information 'optimize)
                   #+clozure (ccl:declaration-information 'optimize nil))
               :for x :in settings
               ,@(or #+(or abcl clasp ecl gcl mkcl xcl) '(:for v :in +optimization-variables+))
               :for y = (or #+(or allegro clozure) (second (assoc x info)) ; normalize order
                            #+clisp (gethash x system::*optimize* 1)
                            #+(or abcl clasp ecl mkcl xcl) (symbol-value v)
                            #+(or cmucl scl) (slot-value c::*default-cookie*
                                                       (case x (compilation-speed 'c::cspeed)
                                                             (otherwise x)))
                            #+lispworks (slot-value compiler::*optimization-level* x)
                            #+sbcl (sb-c::policy-quality sb-c::*policy* x))
               :when y :collect (list x y))))
  (defun proclaim-optimization-settings ()
    "Proclaim the optimization settings in *OPTIMIZATION-SETTINGS*"
    (proclaim `(optimize ,@*optimization-settings*))
    (let ((settings (get-optimization-settings)))
      (unless (equal *previous-optimization-settings* settings)
        (setf *previous-optimization-settings* settings))))
  (defmacro with-optimization-settings ((&optional (settings *optimization-settings*)) &body body)
    #+(or allegro clisp)
    (let ((previous-settings (gensym "PREVIOUS-SETTINGS")))
      `(let ((,previous-settings (get-optimization-settings)))
         ,@(when settings `((proclaim `(optimize ,@,settings))))
         (unwind-protect (progn ,@body)
           (proclaim `(optimize ,@,previous-settings)))))
    #-(or allegro clisp)
    `(let ,(loop :for v :in +optimization-variables+ :collect `(,v ,v))
       ,@(when settings `((proclaim `(optimize ,@,settings))))
       ,@body)))


;;; Condition control
(with-upgradability ()
  #+sbcl
  (progn
    (defun sb-grovel-unknown-constant-condition-p (c)
      "Detect SB-GROVEL unknown-constant conditions on older versions of SBCL"
      (and (typep c 'sb-int:simple-style-warning)
           (stringp (simple-condition-format-control c))
           (string-enclosed-p
            "Couldn't grovel for "
            (simple-condition-format-control c)
            " (unknown to the C compiler).")))
    (deftype sb-grovel-unknown-constant-condition ()
      '(and style-warning (satisfies sb-grovel-unknown-constant-condition-p))))

  (defvar *usual-uninteresting-conditions*
    (append
     ;;#+clozure '(ccl:compiler-warning)
     #+cmucl '("Deleting unreachable code.")
     #+lispworks '("~S being redefined in ~A (previously in ~A)."
                   "~S defined more than once in ~A.") ;; lispworks gets confused by eval-when.
     #+sbcl
     '(sb-c::simple-compiler-note
       "&OPTIONAL and &KEY found in the same lambda list: ~S"
       sb-kernel:lexical-environment-too-complex
       sb-kernel:undefined-alien-style-warning
       sb-grovel-unknown-constant-condition ; defined above.
       sb-ext:implicit-generic-function-warning ;; Controversial.
       sb-int:package-at-variance
       sb-kernel:uninteresting-redefinition
       ;; BEWARE: the below four are controversial to include here.
       sb-kernel:redefinition-with-defun
       sb-kernel:redefinition-with-defgeneric
       sb-kernel:redefinition-with-defmethod
       sb-kernel::redefinition-with-defmacro) ; not exported by old SBCLs
     '("No generic function ~S present when encountering macroexpansion of defmethod. Assuming it will be an instance of standard-generic-function.")) ;; from closer2mop
    "A suggested value to which to set or bind *uninteresting-conditions*.")

  (defvar *uninteresting-conditions* '()
    "Conditions that may be skipped while compiling or loading Lisp code.")
  (defvar *uninteresting-compiler-conditions* '()
    "Additional conditions that may be skipped while compiling Lisp code.")
  (defvar *uninteresting-loader-conditions*
    (append
     '("Overwriting already existing readtable ~S." ;; from named-readtables
       #(#:finalizers-off-warning :asdf-finalizers)) ;; from asdf-finalizers
     #+clisp '(clos::simple-gf-replacing-method-warning))
    "Additional conditions that may be skipped while loading Lisp code."))

;;;; ----- Filtering conditions while building -----
(with-upgradability ()
  (defun call-with-muffled-compiler-conditions (thunk)
    "Call given THUNK in a context where uninteresting conditions and compiler conditions are muffled"
    (call-with-muffled-conditions
     thunk (append *uninteresting-conditions* *uninteresting-compiler-conditions*)))
  (defmacro with-muffled-compiler-conditions ((&optional) &body body)
    "Trivial syntax for CALL-WITH-MUFFLED-COMPILER-CONDITIONS"
    `(call-with-muffled-compiler-conditions #'(lambda () ,@body)))
  (defun call-with-muffled-loader-conditions (thunk)
    "Call given THUNK in a context where uninteresting conditions and loader conditions are muffled"
    (call-with-muffled-conditions
     thunk (append *uninteresting-conditions* *uninteresting-loader-conditions*)))
  (defmacro with-muffled-loader-conditions ((&optional) &body body)
    "Trivial syntax for CALL-WITH-MUFFLED-LOADER-CONDITIONS"
    `(call-with-muffled-loader-conditions #'(lambda () ,@body))))


;;;; Handle warnings and failures
(with-upgradability ()
  (define-condition compile-condition (condition)
    ((context-format
      :initform nil :reader compile-condition-context-format :initarg :context-format)
     (context-arguments
      :initform nil :reader compile-condition-context-arguments :initarg :context-arguments)
     (description
      :initform nil :reader compile-condition-description :initarg :description))
    (:report (lambda (c s)
               (format s (compatfmt "~@<~A~@[ while ~?~]~@:>")
                       (or (compile-condition-description c) (type-of c))
                       (compile-condition-context-format c)
                       (compile-condition-context-arguments c)))))
  (define-condition compile-file-error (compile-condition error) ())
  (define-condition compile-warned-warning (compile-condition warning) ())
  (define-condition compile-warned-error (compile-condition error) ())
  (define-condition compile-failed-warning (compile-condition warning) ())
  (define-condition compile-failed-error (compile-condition error) ())

  (defun check-lisp-compile-warnings (warnings-p failure-p
                                                  &optional context-format context-arguments)
    "Given the warnings or failures as resulted from COMPILE-FILE or checking deferred warnings,
raise an error or warning as appropriate"
    (when failure-p
      (case *compile-file-failure-behaviour*
        (:warn (warn 'compile-failed-warning
                     :description "Lisp compilation failed"
                     :context-format context-format
                     :context-arguments context-arguments))
        (:error (error 'compile-failed-error
                       :description "Lisp compilation failed"
                       :context-format context-format
                       :context-arguments context-arguments))
        (:ignore nil)))
    (when warnings-p
      (case *compile-file-warnings-behaviour*
        (:warn (warn 'compile-warned-warning
                     :description "Lisp compilation had style-warnings"
                     :context-format context-format
                     :context-arguments context-arguments))
        (:error (error 'compile-warned-error
                       :description "Lisp compilation had style-warnings"
                       :context-format context-format
                       :context-arguments context-arguments))
        (:ignore nil))))

  (defun check-lisp-compile-results (output warnings-p failure-p
                                             &optional context-format context-arguments)
    "Given the results of COMPILE-FILE, raise an error or warning as appropriate"
    (unless output
      (error 'compile-file-error :context-format context-format :context-arguments context-arguments))
    (check-lisp-compile-warnings warnings-p failure-p context-format context-arguments)))


;;;; Deferred-warnings treatment, originally implemented by Douglas Katzman.
;;;
;;; To support an implementation, three functions must be implemented:
;;; reify-deferred-warnings unreify-deferred-warnings reset-deferred-warnings
;;; See their respective docstrings.
(with-upgradability ()
  (defun reify-simple-sexp (sexp)
    "Given a simple SEXP, return a representation of it as a portable SEXP.
Simple means made of symbols, numbers, characters, simple-strings, pathnames, cons cells."
    (etypecase sexp
      (symbol (reify-symbol sexp))
      ((or number character simple-string pathname) sexp)
      (cons (cons (reify-simple-sexp (car sexp)) (reify-simple-sexp (cdr sexp))))
      (simple-vector (vector (mapcar 'reify-simple-sexp (coerce sexp 'list))))))

  (defun unreify-simple-sexp (sexp)
    "Given the portable output of REIFY-SIMPLE-SEXP, return the simple SEXP it represents"
    (etypecase sexp
      ((or symbol number character simple-string pathname) sexp)
      (cons (cons (unreify-simple-sexp (car sexp)) (unreify-simple-sexp (cdr sexp))))
      ((simple-vector 2) (unreify-symbol sexp))
      ((simple-vector 1) (coerce (mapcar 'unreify-simple-sexp (aref sexp 0)) 'vector))))

  #+clozure
  (progn
    (defun reify-source-note (source-note)
      (when source-note
        (with-accessors ((source ccl::source-note-source) (filename ccl:source-note-filename)
                         (start-pos ccl:source-note-start-pos) (end-pos ccl:source-note-end-pos)) source-note
          (declare (ignorable source))
          (list :filename filename :start-pos start-pos :end-pos end-pos
                #|:source (reify-source-note source)|#))))
    (defun unreify-source-note (source-note)
      (when source-note
        (destructuring-bind (&key filename start-pos end-pos source) source-note
          (ccl::make-source-note :filename filename :start-pos start-pos :end-pos end-pos
                                 :source (unreify-source-note source)))))
    (defun unsymbolify-function-name (name)
      (if-let (setfed (gethash name ccl::%setf-function-name-inverses%))
        `(setf ,setfed)
        name))
    (defun symbolify-function-name (name)
      (if (and (consp name) (eq (first name) 'setf))
          (let ((setfed (second name)))
            (gethash setfed ccl::%setf-function-names%))
          name))
    (defun reify-function-name (function-name)
      (let ((name (or (first function-name) ;; defun: extract the name
                      (let ((sec (second function-name)))
                        (or (and (atom sec) sec) ; scoped method: drop scope
                            (first sec)))))) ; method: keep gf name, drop method specializers
        (list name)))
    (defun unreify-function-name (function-name)
      function-name)
    (defun nullify-non-literals (sexp)
      (typecase sexp
        ((or number character simple-string symbol pathname) sexp)
        (cons (cons (nullify-non-literals (car sexp))
                    (nullify-non-literals (cdr sexp))))
        (t nil)))
    (defun reify-deferred-warning (deferred-warning)
      (with-accessors ((warning-type ccl::compiler-warning-warning-type)
                       (args ccl::compiler-warning-args)
                       (source-note ccl:compiler-warning-source-note)
                       (function-name ccl:compiler-warning-function-name)) deferred-warning
        (list :warning-type warning-type :function-name (reify-function-name function-name)
              :source-note (reify-source-note source-note)
              :args (destructuring-bind (fun &rest more)
                        args
                      (cons (unsymbolify-function-name fun)
                            (nullify-non-literals more))))))
    (defun unreify-deferred-warning (reified-deferred-warning)
      (destructuring-bind (&key warning-type function-name source-note args)
          reified-deferred-warning
        (make-condition (or (cdr (ccl::assq warning-type ccl::*compiler-whining-conditions*))
                            'ccl::compiler-warning)
                        :function-name (unreify-function-name function-name)
                        :source-note (unreify-source-note source-note)
                        :warning-type warning-type
                        :args (destructuring-bind (fun . more) args
                                (cons (symbolify-function-name fun) more))))))
  #+(or cmucl scl)
  (defun reify-undefined-warning (warning)
    ;; Extracting undefined-warnings from the compilation-unit
    ;; To be passed through the above reify/unreify link, it must be a "simple-sexp"
    (list*
     (c::undefined-warning-kind warning)
     (c::undefined-warning-name warning)
     (c::undefined-warning-count warning)
     (mapcar
      #'(lambda (frob)
          ;; the lexenv slot can be ignored for reporting purposes
          `(:enclosing-source ,(c::compiler-error-context-enclosing-source frob)
            :source ,(c::compiler-error-context-source frob)
            :original-source ,(c::compiler-error-context-original-source frob)
            :context ,(c::compiler-error-context-context frob)
            :file-name ,(c::compiler-error-context-file-name frob) ; a pathname
            :file-position ,(c::compiler-error-context-file-position frob) ; an integer
            :original-source-path ,(c::compiler-error-context-original-source-path frob)))
      (c::undefined-warning-warnings warning))))

  #+sbcl
  (defun reify-undefined-warning (warning)
    ;; Extracting undefined-warnings from the compilation-unit
    ;; To be passed through the above reify/unreify link, it must be a "simple-sexp"
    (list*
     (sb-c::undefined-warning-kind warning)
     (sb-c::undefined-warning-name warning)
     (sb-c::undefined-warning-count warning)
     (mapcar
      #'(lambda (frob)
          ;; the lexenv slot can be ignored for reporting purposes
          `(:enclosing-source ,(sb-c::compiler-error-context-enclosing-source frob)
            :source ,(sb-c::compiler-error-context-source frob)
            :original-source ,(sb-c::compiler-error-context-original-source frob)
            :context ,(sb-c::compiler-error-context-context frob)
            :file-name ,(sb-c::compiler-error-context-file-name frob) ; a pathname
            :file-position ,(sb-c::compiler-error-context-file-position frob) ; an integer
            :original-source-path ,(sb-c::compiler-error-context-original-source-path frob)))
      (sb-c::undefined-warning-warnings warning))))

  (defun reify-deferred-warnings ()
    "return a portable S-expression, portably readable and writeable in any Common Lisp implementation
using READ within a WITH-SAFE-IO-SYNTAX, that represents the warnings currently deferred by
WITH-COMPILATION-UNIT. One of three functions required for deferred-warnings support in ASDF."
    #+allegro
    (list :functions-defined excl::.functions-defined.
          :functions-called excl::.functions-called.)
    #+clozure
    (mapcar 'reify-deferred-warning
            (if-let (dw ccl::*outstanding-deferred-warnings*)
              (let ((mdw (ccl::ensure-merged-deferred-warnings dw)))
                (ccl::deferred-warnings.warnings mdw))))
    #+(or cmucl scl)
    (when lisp::*in-compilation-unit*
      ;; Try to send nothing through the pipe if nothing needs to be accumulated
      `(,@(when c::*undefined-warnings*
            `((c::*undefined-warnings*
               ,@(mapcar #'reify-undefined-warning c::*undefined-warnings*))))
        ,@(loop :for what :in '(c::*compiler-error-count*
                                c::*compiler-warning-count*
                                c::*compiler-note-count*)
                :for value = (symbol-value what)
                :when (plusp value)
                  :collect `(,what . ,value))))
    #+sbcl
    (when sb-c::*in-compilation-unit*
      ;; Try to send nothing through the pipe if nothing needs to be accumulated
      `(,@(when sb-c::*undefined-warnings*
            `((sb-c::*undefined-warnings*
               ,@(mapcar #'reify-undefined-warning sb-c::*undefined-warnings*))))
        ,@(loop :for what :in '(sb-c::*aborted-compilation-unit-count*
                                sb-c::*compiler-error-count*
                                sb-c::*compiler-warning-count*
                                sb-c::*compiler-style-warning-count*
                                sb-c::*compiler-note-count*)
                :for value = (symbol-value what)
                :when (plusp value)
                  :collect `(,what . ,value)))))

  (defun unreify-deferred-warnings (reified-deferred-warnings)
    "given a S-expression created by REIFY-DEFERRED-WARNINGS, reinstantiate the corresponding
deferred warnings as to be handled at the end of the current WITH-COMPILATION-UNIT.
Handle any warning that has been resolved already,
such as an undefined function that has been defined since.
One of three functions required for deferred-warnings support in ASDF."
    (declare (ignorable reified-deferred-warnings))
    #+allegro
    (destructuring-bind (&key functions-defined functions-called)
        reified-deferred-warnings
      (setf excl::.functions-defined.
            (append functions-defined excl::.functions-defined.)
            excl::.functions-called.
            (append functions-called excl::.functions-called.)))
    #+clozure
    (let ((dw (or ccl::*outstanding-deferred-warnings*
                  (setf ccl::*outstanding-deferred-warnings* (ccl::%defer-warnings t)))))
      (appendf (ccl::deferred-warnings.warnings dw)
               (mapcar 'unreify-deferred-warning reified-deferred-warnings)))
    #+(or cmucl scl)
    (dolist (item reified-deferred-warnings)
      ;; Each item is (symbol . adjustment) where the adjustment depends on the symbol.
      ;; For *undefined-warnings*, the adjustment is a list of initargs.
      ;; For everything else, it's an integer.
      (destructuring-bind (symbol . adjustment) item
        (case symbol
          ((c::*undefined-warnings*)
           (setf c::*undefined-warnings*
                 (nconc (mapcan
                         #'(lambda (stuff)
                             (destructuring-bind (kind name count . rest) stuff
                               (unless (case kind (:function (fboundp name)))
                                 (list
                                  (c::make-undefined-warning
                                   :name name
                                   :kind kind
                                   :count count
                                   :warnings
                                   (mapcar #'(lambda (x)
                                               (apply #'c::make-compiler-error-context x))
                                           rest))))))
                         adjustment)
                        c::*undefined-warnings*)))
          (otherwise
           (set symbol (+ (symbol-value symbol) adjustment))))))
    #+sbcl
    (dolist (item reified-deferred-warnings)
      ;; Each item is (symbol . adjustment) where the adjustment depends on the symbol.
      ;; For *undefined-warnings*, the adjustment is a list of initargs.
      ;; For everything else, it's an integer.
      (destructuring-bind (symbol . adjustment) item
        (case symbol
          ((sb-c::*undefined-warnings*)
           (setf sb-c::*undefined-warnings*
                 (nconc (mapcan
                         #'(lambda (stuff)
                             (destructuring-bind (kind name count . rest) stuff
                               (unless (case kind (:function (fboundp name)))
                                 (list
                                  (sb-c::make-undefined-warning
                                   :name name
                                   :kind kind
                                   :count count
                                   :warnings
                                   (mapcar #'(lambda (x)
                                               (apply #'sb-c::make-compiler-error-context x))
                                           rest))))))
                         adjustment)
                        sb-c::*undefined-warnings*)))
          (otherwise
           (set symbol (+ (symbol-value symbol) adjustment)))))))

  (defun reset-deferred-warnings ()
    "Reset the set of deferred warnings to be handled at the end of the current WITH-COMPILATION-UNIT.
One of three functions required for deferred-warnings support in ASDF."
    #+allegro
    (setf excl::.functions-defined. nil
          excl::.functions-called. nil)
    #+clozure
    (if-let (dw ccl::*outstanding-deferred-warnings*)
      (let ((mdw (ccl::ensure-merged-deferred-warnings dw)))
        (setf (ccl::deferred-warnings.warnings mdw) nil)))
    #+(or cmucl scl)
    (when lisp::*in-compilation-unit*
      (setf c::*undefined-warnings* nil
            c::*compiler-error-count* 0
            c::*compiler-warning-count* 0
            c::*compiler-note-count* 0))
    #+sbcl
    (when sb-c::*in-compilation-unit*
      (setf sb-c::*undefined-warnings* nil
            sb-c::*aborted-compilation-unit-count* 0
            sb-c::*compiler-error-count* 0
            sb-c::*compiler-warning-count* 0
            sb-c::*compiler-style-warning-count* 0
            sb-c::*compiler-note-count* 0)))

  (defun save-deferred-warnings (warnings-file)
    "Save forward reference conditions so they may be issued at a latter time,
possibly in a different process."
    (with-open-file (s warnings-file :direction :output :if-exists :supersede
                       :element-type *default-stream-element-type*
                       :external-format *utf-8-external-format*)
      (with-safe-io-syntax ()
        (let ((*read-eval* t))
          (write (reify-deferred-warnings) :stream s :pretty t :readably t))
        (terpri s))))

  (defun warnings-file-type (&optional implementation-type)
    "The pathname type for warnings files on given IMPLEMENTATION-TYPE,
where NIL designates the current one"
    (case (or implementation-type *implementation-type*)
      ((:acl :allegro) "allegro-warnings")
      ;;((:clisp) "clisp-warnings")
      ((:cmu :cmucl) "cmucl-warnings")
      ((:sbcl) "sbcl-warnings")
      ((:clozure :ccl) "ccl-warnings")
      ((:scl) "scl-warnings")))

  (defvar *warnings-file-type* nil
    "Pathname type for warnings files, or NIL if disabled")

  (defun enable-deferred-warnings-check ()
    "Enable the saving of deferred warnings"
    (setf *warnings-file-type* (warnings-file-type)))

  (defun disable-deferred-warnings-check ()
    "Disable the saving of deferred warnings"
    (setf *warnings-file-type* nil))

  (defun warnings-file-p (file &optional implementation-type)
    "Is FILE a saved warnings file for the given IMPLEMENTATION-TYPE?
If that given type is NIL, use the currently configured *WARNINGS-FILE-TYPE* instead."
    (if-let (type (if implementation-type
                      (warnings-file-type implementation-type)
                      *warnings-file-type*))
      (equal (pathname-type file) type)))

  (defun check-deferred-warnings (files &optional context-format context-arguments)
    "Given a list of FILES containing deferred warnings saved by CALL-WITH-SAVED-DEFERRED-WARNINGS,
re-intern and raise any warnings that are still meaningful."
    (let ((file-errors nil)
          (failure-p nil)
          (warnings-p nil))
      (handler-bind
          ((warning #'(lambda (c)
                        (setf warnings-p t)
                        (unless (typep c 'style-warning)
                          (setf failure-p t)))))
        (with-compilation-unit (:override t)
          (reset-deferred-warnings)
          (dolist (file files)
            (unreify-deferred-warnings
             (handler-case
                 (with-safe-io-syntax ()
                   (let ((*read-eval* t))
                     (read-file-form file)))
               (error (c)
                 ;;(delete-file-if-exists file) ;; deleting forces rebuild but prevents debugging
                 (push c file-errors)
                 nil))))))
      (dolist (error file-errors) (error error))
      (check-lisp-compile-warnings
       (or failure-p warnings-p) failure-p context-format context-arguments)))

  #|
  Mini-guide to adding support for deferred warnings on an implementation.

  First, look at what such a warning looks like:

  (describe
  (handler-case
  (and (eval '(lambda () (some-undefined-function))) nil)
  (t (c) c)))

  Then you can grep for the condition type in your compiler sources
  and see how to catch those that have been deferred,
  and/or read, clear and restore the deferred list.

  Also look at
  (macroexpand-1 '(with-compilation-unit () foo))
  |#

  (defun call-with-saved-deferred-warnings (thunk warnings-file &key source-namestring)
    "If WARNINGS-FILE is not nil, record the deferred-warnings around a call to THUNK
and save those warnings to the given file for latter use,
possibly in a different process. Otherwise just call THUNK."
    (declare (ignorable source-namestring))
    (if warnings-file
        (with-compilation-unit (:override t #+sbcl :source-namestring #+sbcl source-namestring)
          (unwind-protect
               (let (#+sbcl (sb-c::*undefined-warnings* nil))
                 (multiple-value-prog1
                     (funcall thunk)
                   (save-deferred-warnings warnings-file)))
            (reset-deferred-warnings)))
        (funcall thunk)))

  (defmacro with-saved-deferred-warnings ((warnings-file &key source-namestring) &body body)
    "Trivial syntax for CALL-WITH-SAVED-DEFERRED-WARNINGS"
    `(call-with-saved-deferred-warnings
      #'(lambda () ,@body) ,warnings-file :source-namestring ,source-namestring)))


;;; from ASDF
(with-upgradability ()
  (defun current-lisp-file-pathname ()
    "Portably return the PATHNAME of the current Lisp source file being compiled or loaded"
    (or *compile-file-pathname* *load-pathname*))

  (defun load-pathname ()
    "Portably return the LOAD-PATHNAME of the current source file or fasl"
    *load-pathname*) ;; magic no longer needed for GCL.

  (defun lispize-pathname (input-file)
    "From a INPUT-FILE pathname, return a corresponding .lisp source pathname"
    (make-pathname :type "lisp" :defaults input-file))

  (defun compile-file-type (&rest keys)
    "pathname TYPE for lisp FASt Loading files"
    (declare (ignorable keys))
    #-(or clasp ecl mkcl) (load-time-value (pathname-type (compile-file-pathname "foo.lisp")))
    #+(or clasp ecl mkcl) (pathname-type (apply 'compile-file-pathname "foo" keys)))

  (defun call-around-hook (hook function)
    "Call a HOOK around the execution of FUNCTION"
    (call-function (or hook 'funcall) function))

  (defun compile-file-pathname* (input-file &rest keys &key output-file &allow-other-keys)
    "Variant of COMPILE-FILE-PATHNAME that works well with COMPILE-FILE*"
    (let* ((keys
             (remove-plist-keys `(#+(or (and allegro (not (version>= 8 2)))) :external-format
                                    ,@(unless output-file '(:output-file))) keys)))
      (if (absolute-pathname-p output-file)
          ;; what cfp should be doing, w/ mp* instead of mp
          (let* ((type (pathname-type (apply 'compile-file-type keys)))
                 (defaults (make-pathname
                            :type type :defaults (merge-pathnames* input-file))))
            (merge-pathnames* output-file defaults))
          (funcall *output-translation-function*
                   (apply 'compile-file-pathname input-file keys)))))

  (defvar *compile-check* nil
    "A hook for user-defined compile-time invariants")

  (defun* (compile-file*) (input-file &rest keys
                                      &key (compile-check *compile-check*) output-file warnings-file
                                      #+clisp lib-file #+(or clasp ecl mkcl) object-file #+sbcl emit-cfasl
                                      &allow-other-keys)
    "This function provides a portable wrapper around COMPILE-FILE.
It ensures that the OUTPUT-FILE value is only returned and
the file only actually created if the compilation was successful,
even though your implementation may not do that. It also checks an optional
user-provided consistency function COMPILE-CHECK to determine success;
it will call this function if not NIL at the end of the compilation
with the arguments sent to COMPILE-FILE*, except with :OUTPUT-FILE TMP-FILE
where TMP-FILE is the name of a temporary output-file.
It also checks two flags (with legacy british spelling from ASDF1),
*COMPILE-FILE-FAILURE-BEHAVIOUR* and *COMPILE-FILE-WARNINGS-BEHAVIOUR*
with appropriate implementation-dependent defaults,
and if a failure (respectively warnings) are reported by COMPILE-FILE,
it will consider that an error unless the respective behaviour flag
is one of :SUCCESS :WARN :IGNORE.
If WARNINGS-FILE is defined, deferred warnings are saved to that file.
On ECL or MKCL, it creates both the linkable object and loadable fasl files.
On implementations that erroneously do not recognize standard keyword arguments,
it will filter them appropriately."
    #+(or clasp ecl)
    (when (and object-file (equal (compile-file-type) (pathname object-file)))
      (format t "Whoa, some funky ASDF upgrade switched ~S calling convention for ~S and ~S~%"
              'compile-file* output-file object-file)
      (rotatef output-file object-file))
    (let* ((keywords (remove-plist-keys
                      `(:output-file :compile-check :warnings-file
                                     #+clisp :lib-file #+(or clasp ecl mkcl) :object-file) keys))
           (output-file
             (or output-file
                 (apply 'compile-file-pathname* input-file :output-file output-file keywords)))
           (physical-output-file (physicalize-pathname output-file))
           #+(or clasp ecl)
           (object-file
             (unless (use-ecl-byte-compiler-p)
               (or object-file
                   #+ecl (compile-file-pathname output-file :type :object)
                   #+clasp (compile-file-pathname output-file :output-type :object))))
           #+mkcl
           (object-file
             (or object-file
                 (compile-file-pathname output-file :fasl-p nil)))
           (tmp-file (tmpize-pathname physical-output-file))
           #+sbcl
           (cfasl-file (etypecase emit-cfasl
                         (null nil)
                         ((eql t) (make-pathname :type "cfasl" :defaults physical-output-file))
                         (string (parse-namestring emit-cfasl))
                         (pathname emit-cfasl)))
           #+sbcl
           (tmp-cfasl (when cfasl-file (make-pathname :type "cfasl" :defaults tmp-file)))
           #+clisp
           (tmp-lib (make-pathname :type "lib" :defaults tmp-file)))
      (multiple-value-bind (output-truename warnings-p failure-p)
          (with-enough-pathname (input-file :defaults *base-build-directory*)
            (with-saved-deferred-warnings (warnings-file :source-namestring (namestring input-file))
              (with-muffled-compiler-conditions ()
                (or #-(or clasp ecl mkcl)
                    (apply 'compile-file input-file :output-file tmp-file
                           #+sbcl (if emit-cfasl (list* :emit-cfasl tmp-cfasl keywords) keywords)
                           #-sbcl keywords)
                    #+ecl (apply 'compile-file input-file :output-file
                                (if object-file
                                    (list* object-file :system-p t keywords)
                                    (list* tmp-file keywords)))
                    #+clasp (apply 'compile-file input-file :output-file
                                  (if object-file
                                      (list* object-file :output-type :object #|:system-p t|# keywords)
                                      (list* tmp-file keywords)))
                    #+mkcl (apply 'compile-file input-file
                                  :output-file object-file :fasl-p nil keywords)))))
        (cond
          ((and output-truename
                (flet ((check-flag (flag behaviour)
                         (or (not flag) (member behaviour '(:success :warn :ignore)))))
                  (and (check-flag failure-p *compile-file-failure-behaviour*)
                       (check-flag warnings-p *compile-file-warnings-behaviour*)))
                (progn
                  #+(or clasp ecl mkcl)
                  (when (and #+(or clasp ecl) object-file)
                    (setf output-truename
                          (compiler::build-fasl tmp-file
                           #+(or clasp ecl) :lisp-files #+mkcl :lisp-object-files (list object-file))))
                  (or (not compile-check)
                      (apply compile-check input-file
                             :output-file output-truename
                             keywords))))
           (delete-file-if-exists physical-output-file)
           (when output-truename
             #+clasp (when output-truename (rename-file-overwriting-target tmp-file output-truename))
             ;; see CLISP bug 677
             #+clisp
             (progn
               (setf tmp-lib (make-pathname :type "lib" :defaults output-truename))
               (unless lib-file (setf lib-file (make-pathname :type "lib" :defaults physical-output-file)))
               (rename-file-overwriting-target tmp-lib lib-file))
             #+sbcl (when cfasl-file (rename-file-overwriting-target tmp-cfasl cfasl-file))
             (rename-file-overwriting-target output-truename physical-output-file)
             (setf output-truename (truename physical-output-file)))
           #+clasp (delete-file-if-exists tmp-file)
           #+clisp (progn (delete-file-if-exists tmp-file) ;; this one works around clisp BUG 677
                          (delete-file-if-exists tmp-lib))) ;; this one is "normal" defensive cleanup
          (t ;; error or failed check
           (delete-file-if-exists output-truename)
           #+clisp (delete-file-if-exists tmp-lib)
           #+sbcl (delete-file-if-exists tmp-cfasl)
           (setf output-truename nil)))
        (values output-truename warnings-p failure-p))))

  (defun load* (x &rest keys &key &allow-other-keys)
    "Portable wrapper around LOAD that properly handles loading from a stream."
    (with-muffled-loader-conditions ()
      (etypecase x
        ((or pathname string #-(or allegro clozure genera) stream #+clozure file-stream)
         (apply 'load x keys))
        ;; Genera can't load from a string-input-stream
        ;; ClozureCL 1.6 can only load from file input stream
        ;; Allegro 5, I don't remember but it must have been broken when I tested.
        #+(or allegro clozure genera)
        (stream ;; make do this way
         (let ((*package* *package*)
               (*readtable* *readtable*)
               (*load-pathname* nil)
               (*load-truename* nil))
           (eval-input x))))))

  (defun load-from-string (string)
    "Portably read and evaluate forms from a STRING."
    (with-input-from-string (s string) (load* s))))

;;; Links FASLs together
(with-upgradability ()
  (defun combine-fasls (inputs output)
    "Combine a list of FASLs INPUTS into a single FASL OUTPUT"
    #-(or abcl allegro clisp clozure cmucl lispworks sbcl scl xcl)
    (not-implemented-error 'combine-fasls "~%inputs: ~S~%output: ~S" inputs output)
    #+abcl (funcall 'sys::concatenate-fasls inputs output) ; requires ABCL 1.2.0
    #+(or allegro clisp cmucl sbcl scl xcl) (concatenate-files inputs output)
    #+clozure (ccl:fasl-concatenate output inputs :if-exists :supersede)
    #+lispworks
    (let (fasls)
      (unwind-protect
           (progn
             (loop :for i :in inputs
                   :for n :from 1
                   :for f = (add-pathname-suffix
                             output (format nil "-FASL~D" n))
                   :do (copy-file i f)
                       (push f fasls))
             (ignore-errors (lispworks:delete-system :fasls-to-concatenate))
             (eval `(scm:defsystem :fasls-to-concatenate
                      (:default-pathname ,(pathname-directory-pathname output))
                      :members
                      ,(loop :for f :in (reverse fasls)
                             :collect `(,(namestring f) :load-only t))))
             (scm:concatenate-system output :fasls-to-concatenate :force t))
        (loop :for f :in fasls :do (ignore-errors (delete-file f)))
        (ignore-errors (lispworks:delete-system :fasls-to-concatenate))))))
;;;; -------------------------------------------------------------------------
;;;; launch-program - semi-portably spawn asynchronous subprocesses

(uiop/package:define-package :uiop/launch-program
  (:use :uiop/common-lisp :uiop/package :uiop/utility
   :uiop/pathname :uiop/os :uiop/filesystem :uiop/stream)
  (:export
   ;;; Escaping the command invocation madness
   #:easy-sh-character-p #:escape-sh-token #:escape-sh-command
   #:escape-windows-token #:escape-windows-command
   #:escape-shell-token #:escape-shell-command
   #:escape-token #:escape-command

   ;;; launch-program
   #:launch-program
   #:close-streams #:process-alive-p #:terminate-process #:wait-process
   #:process-info-error-output #:process-info-input #:process-info-output #:process-info-pid))
(in-package :uiop/launch-program)

;;;; ----- Escaping strings for the shell -----
(with-upgradability ()
  (defun requires-escaping-p (token &key good-chars bad-chars)
    "Does this token require escaping, given the specification of
either good chars that don't need escaping or bad chars that do need escaping,
as either a recognizing function or a sequence of characters."
    (some
     (cond
       ((and good-chars bad-chars)
        (parameter-error "~S: only one of good-chars and bad-chars can be provided"
                         'requires-escaping-p))
       ((typep good-chars 'function)
        (complement good-chars))
       ((typep bad-chars 'function)
        bad-chars)
       ((and good-chars (typep good-chars 'sequence))
        #'(lambda (c) (not (find c good-chars))))
       ((and bad-chars (typep bad-chars 'sequence))
        #'(lambda (c) (find c bad-chars)))
       (t (parameter-error "~S: no good-char criterion" 'requires-escaping-p)))
     token))

  (defun escape-token (token &key stream quote good-chars bad-chars escaper)
    "Call the ESCAPER function on TOKEN string if it needs escaping as per
REQUIRES-ESCAPING-P using GOOD-CHARS and BAD-CHARS, otherwise output TOKEN,
using STREAM as output (or returning result as a string if NIL)"
    (if (requires-escaping-p token :good-chars good-chars :bad-chars bad-chars)
        (with-output (stream)
          (apply escaper token stream (when quote `(:quote ,quote))))
        (output-string token stream)))

  (defun escape-windows-token-within-double-quotes (x &optional s)
    "Escape a string token X within double-quotes
for use within a MS Windows command-line, outputing to S."
    (labels ((issue (c) (princ c s))
             (issue-backslash (n) (loop :repeat n :do (issue #\\))))
      (loop
        :initially (issue #\") :finally (issue #\")
        :with l = (length x) :with i = 0
        :for i+1 = (1+ i) :while (< i l) :do
          (case (char x i)
            ((#\") (issue-backslash 1) (issue #\") (setf i i+1))
            ((#\\)
             (let* ((j (and (< i+1 l) (position-if-not
                                       #'(lambda (c) (eql c #\\)) x :start i+1)))
                    (n (- (or j l) i)))
               (cond
                 ((null j)
                  (issue-backslash (* 2 n)) (setf i l))
                 ((and (< j l) (eql (char x j) #\"))
                  (issue-backslash (1+ (* 2 n))) (issue #\") (setf i (1+ j)))
                 (t
                  (issue-backslash n) (setf i j)))))
            (otherwise
             (issue (char x i)) (setf i i+1))))))

  (defun easy-windows-character-p (x)
    "Is X an \"easy\" character that does not require quoting by the shell?"
    (or (alphanumericp x) (find x "+-_.,@:/=")))

  (defun escape-windows-token (token &optional s)
    "Escape a string TOKEN within double-quotes if needed
for use within a MS Windows command-line, outputing to S."
    (escape-token token :stream s :good-chars #'easy-windows-character-p :quote nil
                        :escaper 'escape-windows-token-within-double-quotes))

  (defun escape-sh-token-within-double-quotes (x s &key (quote t))
    "Escape a string TOKEN within double-quotes
for use within a POSIX Bourne shell, outputing to S;
omit the outer double-quotes if key argument :QUOTE is NIL"
    (when quote (princ #\" s))
    (loop :for c :across x :do
      (when (find c "$`\\\"") (princ #\\ s))
      (princ c s))
    (when quote (princ #\" s)))

  (defun easy-sh-character-p (x)
    "Is X an \"easy\" character that does not require quoting by the shell?"
    (or (alphanumericp x) (find x "+-_.,%@:/=")))

  (defun escape-sh-token (token &optional s)
    "Escape a string TOKEN within double-quotes if needed
for use within a POSIX Bourne shell, outputing to S."
    (escape-token token :stream s :quote #\" :good-chars #'easy-sh-character-p
                        :escaper 'escape-sh-token-within-double-quotes))

  (defun escape-shell-token (token &optional s)
    "Escape a token for the current operating system shell"
    (os-cond
      ((os-unix-p) (escape-sh-token token s))
      ((os-windows-p) (escape-windows-token token s))))

  (defun escape-command (command &optional s
                                  (escaper 'escape-shell-token))
    "Given a COMMAND as a list of tokens, return a string of the
spaced, escaped tokens, using ESCAPER to escape."
    (etypecase command
      (string (output-string command s))
      (list (with-output (s)
              (loop :for first = t :then nil :for token :in command :do
                (unless first (princ #\space s))
                (funcall escaper token s))))))

  (defun escape-windows-command (command &optional s)
    "Escape a list of command-line arguments into a string suitable for parsing
by CommandLineToArgv in MS Windows"
    ;; http://msdn.microsoft.com/en-us/library/bb776391(v=vs.85).aspx
    ;; http://msdn.microsoft.com/en-us/library/17w5ykft(v=vs.85).aspx
    (escape-command command s 'escape-windows-token))

  (defun escape-sh-command (command &optional s)
    "Escape a list of command-line arguments into a string suitable for parsing
by /bin/sh in POSIX"
    (escape-command command s 'escape-sh-token))

  (defun escape-shell-command (command &optional stream)
    "Escape a command for the current operating system's shell"
    (escape-command command stream 'escape-shell-token)))


(with-upgradability ()
  ;;; Internal helpers for run-program
  (defun %normalize-io-specifier (specifier &optional role)
    "Normalizes a portable I/O specifier for LAUNCH-PROGRAM into an implementation-dependent
argument to pass to the internal RUN-PROGRAM"
    (declare (ignorable role))
    (typecase specifier
      (null (or #+(or allegro lispworks) (null-device-pathname)))
      (string (parse-native-namestring specifier))
      (pathname specifier)
      (stream specifier)
      ((eql :stream) :stream)
      ((eql :interactive)
       #+(or allegro lispworks) nil
       #+clisp :terminal
       #+(or abcl clozure cmucl ecl mkcl sbcl scl) t
       #-(or abcl clozure cmucl ecl mkcl sbcl scl allegro lispworks clisp)
       (not-implemented-error :interactive-output
                              "On this lisp implementation, cannot interpret ~a value of ~a"
                              specifier role))
      ((eql :output)
       (cond ((eq role :error-output)
              #+(or abcl allegro clozure cmucl ecl lispworks mkcl sbcl scl)
              :output
              #-(or abcl allegro clozure cmucl ecl lispworks mkcl sbcl scl)
              (not-implemented-error :error-output-redirect
                                     "Can't send ~a to ~a on this lisp implementation."
                                     role specifier))
             (t (parameter-error "~S IO specifier invalid for ~S" specifier role))))
      (otherwise
       (parameter-error "Incorrect I/O specifier ~S for ~S"
                        specifier role))))

  (defun %interactivep (input output error-output)
    (member :interactive (list input output error-output)))

  (defun %signal-to-exit-code (signum)
    (+ 128 signum))

  (defun %code-to-status (exit-code signal-code)
    (cond ((null exit-code) :running)
          ((null signal-code) (values :exited exit-code))
          (t (values :signaled signal-code))))

  #+mkcl
  (defun %mkcl-signal-to-number (signal)
    (require :mk-unix)
    (symbol-value (find-symbol signal :mk-unix)))

  (defclass process-info ()
    (;; The process field is highly platform-, implementation-, and
     ;; even version-dependent.
     ;; Prior to LispWorks 7, the only information that
     ;; `sys:run-shell-command` with `:wait nil` was certain to return
     ;; is a PID (e.g. when all streams are nil), hence we stored it
     ;; and used `sys:pid-exit-status` to obtain an exit status
     ;; later. That is still what we do.
     ;; From LispWorks 7 on, if `sys:run-shell-command` does not
     ;; return a proper stream, we are instead given a dummy stream.
     ;; We can thus always store a stream and use
     ;; `sys:pipe-exit-status` to obtain an exit status later.
     ;; The advantage of dealing with streams instead of PID is the
     ;; availability of functions like `sys:pipe-kill-process`.
     (process :initform nil)
     (input-stream :initform nil)
     (output-stream :initform nil)
     (bidir-stream :initform nil)
     (error-output-stream :initform nil)
     ;; For backward-compatibility, to maintain the property (zerop
     ;; exit-code) <-> success, an exit in response to a signal is
     ;; encoded as 128+signum.
     (exit-code :initform nil)
     ;; If the platform allows it, distinguish exiting with a code
     ;; >128 from exiting in response to a signal by setting this code
     (signal-code :initform nil)))

;;;---------------------------------------------------------------------------
;;; The following two helper functions take care of handling the IF-EXISTS and
;;; IF-DOES-NOT-EXIST arguments for RUN-PROGRAM. In particular, they process the
;;; :ERROR, :APPEND, and :SUPERSEDE arguments *here*, allowing the master
;;; function to treat input and output files unconditionally for reading and
;;; writing.
;;;---------------------------------------------------------------------------

  (defun %handle-if-exists (file if-exists)
    (when (or (stringp file) (pathnamep file))
      (ecase if-exists
        ((:append :supersede :error)
         (with-open-file (dummy file :direction :output :if-exists if-exists)
           (declare (ignorable dummy)))))))

  (defun %handle-if-does-not-exist (file if-does-not-exist)
    (when (or (stringp file) (pathnamep file))
      (ecase if-does-not-exist
        ((:create :error)
         (with-open-file (dummy file :direction :probe
                                :if-does-not-exist if-does-not-exist)
           (declare (ignorable dummy)))))))

  (defun process-info-error-output (process-info)
    (slot-value process-info 'error-output-stream))
  (defun process-info-input (process-info)
    (or (slot-value process-info 'bidir-stream)
        (slot-value process-info 'input-stream)))
  (defun process-info-output (process-info)
    (or (slot-value process-info 'bidir-stream)
        (slot-value process-info 'output-stream)))

  (defun process-info-pid (process-info)
    (let ((process (slot-value process-info 'process)))
      (declare (ignorable process))
      #+abcl (symbol-call :sys :process-pid process)
      #+allegro process
      #+clozure (ccl:external-process-id process)
      #+ecl (ext:external-process-pid process)
      #+(or cmucl scl) (ext:process-pid process)
      #+lispworks7+ (sys:pipe-pid process)
      #+(and lispworks (not lispworks7+)) process
      #+mkcl (mkcl:process-id process)
      #+sbcl (sb-ext:process-pid process)
      #-(or abcl allegro clozure cmucl ecl mkcl lispworks sbcl scl)
      (not-implemented-error 'process-info-pid)))

  (defun %process-status (process-info)
    (if-let (exit-code (slot-value process-info 'exit-code))
      (return-from %process-status
        (if-let (signal-code (slot-value process-info 'signal-code))
          (values :signaled signal-code)
          (values :exited exit-code))))
    #-(or allegro clozure cmucl ecl lispworks mkcl sbcl scl)
    (not-implemented-error '%process-status)
    (if-let (process (slot-value process-info 'process))
      (multiple-value-bind (status code)
          (progn
            #+allegro (multiple-value-bind (exit-code pid signal-code)
                          (sys:reap-os-subprocess :pid process :wait nil)
                        (assert pid)
                        (%code-to-status exit-code signal-code))
            #+clozure (ccl:external-process-status process)
            #+(or cmucl scl) (let ((status (ext:process-status process)))
                               (if (member status '(:exited :signaled))
                                   ;; Calling ext:process-exit-code on
                                   ;; processes that are still alive
                                   ;; yields an undefined result
                                   (values status (ext:process-exit-code process))
                                   status))
            #+ecl (ext:external-process-status process)
            #+lispworks
            ;; a signal is only returned on LispWorks 7+
            (multiple-value-bind (exit-code signal-code)
                (symbol-call :sys
                             #+lispworks7+ :pipe-exit-status
                             #-lispworks7+ :pid-exit-status
                             process :wait nil)
              (%code-to-status exit-code signal-code))
            #+mkcl (let ((status (mk-ext:process-status process)))
                     (if (eq status :exited)
                         ;; Only call mk-ext:process-exit-code when
                         ;; necessary since it leads to another waitpid()
                         (let ((code (mk-ext:process-exit-code process)))
                           (if (stringp code)
                               (values :signaled (%mkcl-signal-to-number code))
                               (values :exited code)))
                         status))
            #+sbcl (let ((status (sb-ext:process-status process)))
                     (if (eq status :running)
                         :running
                         ;; sb-ext:process-exit-code can also be
                         ;; called for stopped processes to determine
                         ;; the signal that stopped them
                         (values status (sb-ext:process-exit-code process)))))
        (case status
          (:exited (setf (slot-value process-info 'exit-code) code))
          (:signaled (let ((%code (%signal-to-exit-code code)))
                       (setf (slot-value process-info 'exit-code) %code
                             (slot-value process-info 'signal-code) code))))
        (if code
            (values status code)
            status))))

  (defun process-alive-p (process-info)
    "Check if a process has yet to exit."
    (unless (slot-value process-info 'exit-code)
      #+abcl (sys:process-alive-p (slot-value process-info 'process))
      #+(or cmucl scl) (ext:process-alive-p (slot-value process-info 'process))
      #+sbcl (sb-ext:process-alive-p (slot-value process-info 'process))
      #-(or abcl cmucl sbcl scl) (find (%process-status process-info)
                                       '(:running :stopped :continued :resumed))))

  (defun wait-process (process-info)
    "Wait for the process to terminate, if it is still running.
Otherwise, return immediately. An exit code (a number) will be
returned, with 0 indicating success, and anything else indicating
failure. If the process exits after receiving a signal, the exit code
will be the sum of 128 and the (positive) numeric signal code. A second
value may be returned in this case: the numeric signal code itself.
Any asynchronously spawned process requires this function to be run
before it is garbage-collected in order to free up resources that
might otherwise be irrevocably lost."
    (if-let (exit-code (slot-value process-info 'exit-code))
      (if-let (signal-code (slot-value process-info 'signal-code))
        (values exit-code signal-code)
        exit-code)
      (let ((process (slot-value process-info 'process)))
        #-(or abcl allegro clozure cmucl ecl lispworks mkcl sbcl scl)
        (not-implemented-error 'wait-process)
        (when process
          ;; 1- wait
          #+clozure (ccl::external-process-wait process)
          #+(or cmucl scl) (ext:process-wait process)
          #+sbcl (sb-ext:process-wait process)
          ;; 2- extract result
          (multiple-value-bind (exit-code signal-code)
              (progn
                #+abcl (sys:process-wait process)
                #+allegro (multiple-value-bind (exit-code pid signal)
                              (sys:reap-os-subprocess :pid process :wait t)
                            (assert pid)
                            (values exit-code signal))
                #+clozure (multiple-value-bind (status code)
                              (ccl:external-process-status process)
                            (if (eq status :signaled)
                                (values nil code)
                                code))
                #+(or cmucl scl) (let ((status (ext:process-status process))
                                       (code (ext:process-exit-code process)))
                                   (if (eq status :signaled)
                                       (values nil code)
                                       code))
                #+ecl (multiple-value-bind (status code)
                          (ext:external-process-wait process t)
                        (if (eq status :signaled)
                            (values nil code)
                            code))
                #+lispworks (symbol-call :sys
                                         #+lispworks7+ :pipe-exit-status
                                         #-lispworks7+ :pid-exit-status
                                         process :wait t)
                #+mkcl (let ((code (mkcl:join-process process)))
                         (if (stringp code)
                             (values nil (%mkcl-signal-to-number code))
                             code))
                #+sbcl (let ((status (sb-ext:process-status process))
                             (code (sb-ext:process-exit-code process)))
                         (if (eq status :signaled)
                             (values nil code)
                             code)))
            (if signal-code
                (let ((%exit-code (%signal-to-exit-code signal-code)))
                  (setf (slot-value process-info 'exit-code) %exit-code
                        (slot-value process-info 'signal-code) signal-code)
                  (values %exit-code signal-code))
                (progn (setf (slot-value process-info 'exit-code) exit-code)
                       exit-code)))))))

  ;; WARNING: For signals other than SIGTERM and SIGKILL this may not
  ;; do what you expect it to. Sending SIGSTOP to a process spawned
  ;; via LAUNCH-PROGRAM, e.g., will stop the shell /bin/sh that is used
  ;; to run the command (via `sh -c command`) but not the actual
  ;; command.
  #+os-unix
  (defun %posix-send-signal (process-info signal)
    #+allegro (excl.osi:kill (slot-value process-info 'process) signal)
    #+clozure (ccl:signal-external-process (slot-value process-info 'process)
                                           signal :error-if-exited nil)
    #+(or cmucl scl) (ext:process-kill (slot-value process-info 'process) signal)
    #+sbcl (sb-ext:process-kill (slot-value process-info 'process) signal)
    #-(or allegro clozure cmucl sbcl scl)
    (if-let (pid (process-info-pid process-info))
      (symbol-call :uiop :run-program
                   (format nil "kill -~a ~a" signal pid) :ignore-error-status t)))

  ;;; this function never gets called on Windows, but the compiler cannot tell
  ;;; that. [2016/09/25:rpg]
  #+os-windows
  (defun %posix-send-signal (process-info signal)
    (declare (ignore process-info signal))
    (values))

  (defun terminate-process (process-info &key urgent)
    "Cause the process to exit. To that end, the process may or may
not be sent a signal, which it will find harder (or even impossible)
to ignore if URGENT is T. On some platforms, it may also be subject to
race conditions."
    (declare (ignorable urgent))
    #+abcl (sys:process-kill (slot-value process-info 'process))
    ;; On ECL, this will only work on versions later than 2016-09-06,
    ;; but we still want to compile on earlier versions, so we use symbol-call
    #+ecl (symbol-call :ext :terminate-process (slot-value process-info 'process) urgent)
    #+lispworks7+ (sys:pipe-kill-process (slot-value process-info 'process))
    #+mkcl (mk-ext:terminate-process (slot-value process-info 'process)
                                     :force urgent)
    #-(or abcl ecl lispworks7+ mkcl)
    (os-cond
     ((os-unix-p) (%posix-send-signal process-info (if urgent 9 15)))
     ((os-windows-p) (if-let (pid (process-info-pid process-info))
                       (symbol-call :uiop :run-program
                                    (format nil "taskkill ~:[~;/f ~]/pid ~a" urgent pid)
                                    :ignore-error-status t)))
     (t (not-implemented-error 'terminate-process))))

  (defun close-streams (process-info)
    "Close any stream that the process might own. Needs to be run
whenever streams were requested by passing :stream to :input, :output,
or :error-output."
    (dolist (stream
              (cons (slot-value process-info 'error-output-stream)
                    (if-let (bidir-stream (slot-value process-info 'bidir-stream))
                      (list bidir-stream)
                      (list (slot-value process-info 'input-stream)
                            (slot-value process-info 'output-stream)))))
      (when stream (close stream))))

  (defun launch-program (command &rest keys
                         &key
                           input (if-input-does-not-exist :error)
                           output (if-output-exists :supersede)
                           error-output (if-error-output-exists :supersede)
                           (element-type #-clozure *default-stream-element-type*
                                         #+clozure 'character)
                           (external-format *utf-8-external-format*)
                           directory
                           #+allegro separate-streams
                           &allow-other-keys)
    "Launch program specified by COMMAND,
either a list of strings specifying a program and list of arguments,
or a string specifying a shell command (/bin/sh on Unix, CMD.EXE on
Windows) _asynchronously_.

If OUTPUT is a pathname, a string designating a pathname, or NIL (the
default) designating the null device, the file at that path is used as
output.
If it's :INTERACTIVE, output is inherited from the current process;
beware that this may be different from your *STANDARD-OUTPUT*, and
under SLIME will be on your *inferior-lisp* buffer.  If it's T, output
goes to your current *STANDARD-OUTPUT* stream.  If it's :STREAM, a new
stream will be made available that can be accessed via
PROCESS-INFO-OUTPUT and read from. Otherwise, OUTPUT should be a value
that the underlying lisp implementation knows how to handle.

IF-OUTPUT-EXISTS, which is only meaningful if OUTPUT is a string or a
pathname, can take the values :ERROR, :APPEND, and :SUPERSEDE (the
default). The meaning of these values and their effect on the case
where OUTPUT does not exist, is analogous to the IF-EXISTS parameter
to OPEN with :DIRECTION :OUTPUT.

ERROR-OUTPUT is similar to OUTPUT. T designates the *ERROR-OUTPUT*,
:OUTPUT means redirecting the error output to the output stream,
and :STREAM causes a stream to be made available via
PROCESS-INFO-ERROR-OUTPUT.

IF-ERROR-OUTPUT-EXISTS is similar to IF-OUTPUT-EXIST, except that it
affects ERROR-OUTPUT rather than OUTPUT.

INPUT is similar to OUTPUT, except that T designates the
*STANDARD-INPUT* and a stream requested through the :STREAM keyword
would be available through PROCESS-INFO-INPUT.

IF-INPUT-DOES-NOT-EXIST, which is only meaningful if INPUT is a string
or a pathname, can take the values :CREATE and :ERROR (the
default). The meaning of these values is analogous to the
IF-DOES-NOT-EXIST parameter to OPEN with :DIRECTION :INPUT.

ELEMENT-TYPE and EXTERNAL-FORMAT are passed on to your Lisp
implementation, when applicable, for creation of the output stream.

LAUNCH-PROGRAM returns a PROCESS-INFO object."
    #-(or abcl allegro clozure cmucl ecl (and lispworks os-unix) mkcl sbcl scl)
    (progn command keys input output error-output directory element-type external-format
           if-input-does-not-exist if-output-exists if-error-output-exists ;; ignore
           (not-implemented-error 'launch-program))
    #+allegro
    (when (some #'(lambda (stream)
                    (and (streamp stream)
                         (not (file-stream-p stream))))
                (list input output error-output))
      (parameter-error "~S: Streams passed as I/O parameters need to be file streams on this lisp"
                       'launch-program))
    #+(or abcl clisp lispworks)
    (when (some #'streamp (list input output error-output))
      (parameter-error "~S: I/O parameters cannot be foreign streams on this lisp"
                       'launch-program))
    #+clisp
    (unless (eq error-output :interactive)
      (parameter-error "~S: The only admissible value for ~S is ~S on this lisp"
                       'launch-program :error-output :interactive))
    #+ecl
    (when (some #'(lambda (stream)
                    (and (streamp stream)
                         (not (file-or-synonym-stream-p stream))))
                (list input output error-output))
      (parameter-error "~S: Streams passed as I/O parameters need to be (synonymous with) file streams on this lisp"
                       'launch-program))
    #+(or abcl allegro clozure cmucl ecl (and lispworks os-unix) mkcl sbcl scl)
    (nest
     (progn ;; see comments for these functions
       (%handle-if-does-not-exist input if-input-does-not-exist)
       (%handle-if-exists output if-output-exists)
       (%handle-if-exists error-output if-error-output-exists))
     #+ecl (let ((*standard-input* *stdin*)
                 (*standard-output* *stdout*)
                 (*error-output* *stderr*)))
     (let ((process-info (make-instance 'process-info))
           (input (%normalize-io-specifier input :input))
           (output (%normalize-io-specifier output :output))
           (error-output (%normalize-io-specifier error-output :error-output))
           #+(and allegro os-windows) (interactive (%interactivep input output error-output))
           (command
            (etypecase command
              #+os-unix (string `("/bin/sh" "-c" ,command))
              #+os-unix (list command)
              #+os-windows
              (string
               ;; NB: On other Windows implementations, this is utterly bogus
               ;; except in the most trivial cases where no quoting is needed.
               ;; Use at your own risk.
               #-(or allegro clisp clozure ecl)
               (nest
                #+(or ecl sbcl) (unless (find-symbol* :escape-arguments #+ecl :ext #+sbcl :sb-impl nil))
                (parameter-error "~S doesn't support string commands on Windows on this Lisp"
                                 'launch-program command))
               ;; NB: We add cmd /c here. Behavior without going through cmd is not well specified
               ;; when the command contains spaces or special characters:
               ;; IIUC, the system will use space as a separator,
               ;; but the C++ argv-decoding libraries won't, and
               ;; you're supposed to use an extra argument to CreateProcess to bridge the gap,
               ;; yet neither allegro nor clisp provide access to that argument.
               #+(or allegro clisp) (strcat "cmd /c " command)
               ;; On ClozureCL for Windows, we assume you are using
               ;; r15398 or later in 1.9 or later,
               ;; so that bug 858 is fixed http://trac.clozure.com/ccl/ticket/858
               ;; On ECL, commit 2040629 https://gitlab.com/embeddable-common-lisp/ecl/issues/304
               ;; On SBCL, we assume the patch from fcae0fd (to be part of SBCL 1.3.13)
               #+(or clozure ecl sbcl) (cons "cmd" (strcat "/c " command)))
              #+os-windows
              (list
               #+allegro (escape-windows-command command)
               #-allegro command)))))
     #+(or abcl (and allegro os-unix) clozure cmucl ecl mkcl sbcl)
     (let ((program (car command))
           #-allegro (arguments (cdr command))))
     #+(and (or ecl sbcl) os-windows)
     (multiple-value-bind (arguments escape-arguments)
         (if (listp arguments)
             (values arguments t)
             (values (list arguments) nil)))
     #-(or allegro mkcl sbcl) (with-current-directory (directory))
     (multiple-value-bind
       #+(or abcl clozure cmucl sbcl scl) (process)
       #+allegro (in-or-io out-or-err err-or-pid pid-or-nil)
       #+ecl (stream code process)
       #+lispworks (io-or-pid err-or-nil #-lispworks7+ pid-or-nil)
       #+mkcl (stream process code)
       #.`(apply
           #+abcl 'sys:run-program
           #+allegro ,@'('excl:run-shell-command
                         #+os-unix (coerce (cons program command) 'vector)
                         #+os-windows command)
           #+clozure 'ccl:run-program
           #+(or cmucl ecl scl) 'ext:run-program
           #+lispworks ,@'('system:run-shell-command `("/usr/bin/env" ,@command)) ; full path needed
           #+mkcl 'mk-ext:run-program
           #+sbcl 'sb-ext:run-program
           #+(or abcl clozure cmucl ecl mkcl sbcl) ,@'(program arguments)
           #+(and (or ecl sbcl) os-windows) ,@'(:escape-arguments escape-arguments)
           :input input :if-input-does-not-exist :error
           :output output :if-output-exists :append
           ,(or #+(or allegro lispworks) :error-output :error) error-output
           ,(or #+(or allegro lispworks) :if-error-output-exists :if-error-exists) :append
           :wait nil :element-type element-type :external-format external-format
           :allow-other-keys t
           #+allegro ,@`(:directory directory
                         #+os-windows ,@'(:show-window (if interactive nil :hide)))
           #+lispworks ,@'(:save-exit-status t)
           #+mkcl ,@'(:directory (native-namestring directory))
           #-sbcl keys ;; on SBCL, don't pass :directory nil but remove it from the keys
           #+sbcl ,@'(:search t (if directory keys (remove-plist-key :directory keys)))))
     (labels ((prop (key value) (setf (slot-value process-info key) value)))
       #+allegro
       (cond
         (separate-streams
          (prop 'process pid-or-nil)
          (when (eq input :stream) (prop 'input-stream in-or-io))
          (when (eq output :stream) (prop 'output-stream out-or-err))
          (when (eq error-output :stream) (prop 'error-stream err-or-pid)))
         (t
          (prop 'process err-or-pid)
          (ecase (+ (if (eq input :stream) 1 0) (if (eq output :stream) 2 0))
            (0)
            (1 (prop 'input-stream in-or-io))
            (2 (prop 'output-stream in-or-io))
            (3 (prop 'bidir-stream in-or-io)))
          (when (eq error-output :stream)
            (prop 'error-stream out-or-err))))
       #+(or abcl clozure cmucl sbcl scl)
       (progn
         (prop 'process process)
         (when (eq input :stream)
           (nest
            (prop 'input-stream)
            #+abcl (symbol-call :sys :process-input)
            #+clozure (ccl:external-process-input-stream)
            #+(or cmucl scl) (ext:process-input)
            #+sbcl (sb-ext:process-input)
            process))
         (when (eq output :stream)
           (nest
            (prop 'output-stream)
            #+abcl (symbol-call :sys :process-output)
            #+clozure (ccl:external-process-output-stream)
            #+(or cmucl scl) (ext:process-output)
            #+sbcl (sb-ext:process-output)
            process))
         (when (eq error-output :stream)
           (nest
            (prop 'error-output-stream)
            #+abcl (symbol-call :sys :process-error)
            #+clozure (ccl:external-process-error-stream)
            #+(or cmucl scl) (ext:process-error)
            #+sbcl (sb-ext:process-error)
            process)))
       #+(or ecl mkcl)
       (let ((mode (+ (if (eq input :stream) 1 0) (if (eq output :stream) 2 0))))
         code ;; ignore
         (unless (zerop mode)
           (prop (case mode (1 'input-stream) (2 'output-stream) (3 'bidir-stream)) stream))
         (prop 'process process))
       #+lispworks
       ;; See also the comments on the process-info class
       (let ((mode (+ (if (eq input :stream) 1 0) (if (eq output :stream) 2 0))))
         (cond
           ((or (plusp mode) (eq error-output :stream))
            (prop 'process #+lispworks7+ io-or-pid #-lispworks7+ pid-or-nil)
            (when (plusp mode)
              (prop (ecase mode (1 'input-stream) (2 'output-stream) (3 'bidir-stream))
                    io-or-pid))
            (when (eq error-output :stream)
              (prop 'error-stream err-or-nil)))
           ;; Prior to Lispworks 7, this returned (pid); now it
           ;; returns (io err pid) of which we keep io.
           (t (prop 'process io-or-pid)))))
     process-info)))

;;;; -------------------------------------------------------------------------
;;;; run-program initially from xcvb-driver.

(uiop/package:define-package :uiop/run-program
  (:nicknames :asdf/run-program) ; OBSOLETE. Used by cl-sane, printv.
  (:use :uiop/common-lisp :uiop/package :uiop/utility :uiop/version
   :uiop/pathname :uiop/os :uiop/filesystem :uiop/stream :uiop/launch-program)
  (:export
   #:run-program
   #:slurp-input-stream #:vomit-output-stream
   #:subprocess-error
   #:subprocess-error-code #:subprocess-error-command #:subprocess-error-process)
  (:import-from :uiop/launch-program
   #:%handle-if-does-not-exist #:%handle-if-exists #:%interactivep
   #:input-stream #:output-stream #:error-output-stream))
(in-package :uiop/run-program)

;;;; Slurping a stream, typically the output of another program
(with-upgradability ()
  (defun call-stream-processor (fun processor stream)
    "Given FUN (typically SLURP-INPUT-STREAM or VOMIT-OUTPUT-STREAM,
a PROCESSOR specification which is either an atom or a list specifying
a processor an keyword arguments, call the specified processor with
the given STREAM as input"
    (if (consp processor)
        (apply fun (first processor) stream (rest processor))
        (funcall fun processor stream)))

  (defgeneric slurp-input-stream (processor input-stream &key)
    (:documentation
     "SLURP-INPUT-STREAM is a generic function with two positional arguments
PROCESSOR and INPUT-STREAM and additional keyword arguments, that consumes (slurps)
the contents of the INPUT-STREAM and processes them according to a method
specified by PROCESSOR.

Built-in methods include the following:
* if PROCESSOR is a function, it is called with the INPUT-STREAM as its argument
* if PROCESSOR is a list, its first element should be a function.  It will be applied to a cons of the
  INPUT-STREAM and the rest of the list.  That is (x . y) will be treated as
    \(APPLY x <stream> y\)
* if PROCESSOR is an output-stream, the contents of INPUT-STREAM is copied to the output-stream,
  per copy-stream-to-stream, with appropriate keyword arguments.
* if PROCESSOR is the symbol CL:STRING or the keyword :STRING, then the contents of INPUT-STREAM
  are returned as a string, as per SLURP-STREAM-STRING.
* if PROCESSOR is the keyword :LINES then the INPUT-STREAM will be handled by SLURP-STREAM-LINES.
* if PROCESSOR is the keyword :LINE then the INPUT-STREAM will be handled by SLURP-STREAM-LINE.
* if PROCESSOR is the keyword :FORMS then the INPUT-STREAM will be handled by SLURP-STREAM-FORMS.
* if PROCESSOR is the keyword :FORM then the INPUT-STREAM will be handled by SLURP-STREAM-FORM.
* if PROCESSOR is T, it is treated the same as *standard-output*. If it is NIL, NIL is returned.

Programmers are encouraged to define their own methods for this generic function."))

  #-genera
  (defmethod slurp-input-stream ((function function) input-stream &key)
    (funcall function input-stream))

  (defmethod slurp-input-stream ((list cons) input-stream &key)
    (apply (first list) input-stream (rest list)))

  #-genera
  (defmethod slurp-input-stream ((output-stream stream) input-stream
                                 &key linewise prefix (element-type 'character) buffer-size)
    (copy-stream-to-stream
     input-stream output-stream
     :linewise linewise :prefix prefix :element-type element-type :buffer-size buffer-size))

  (defmethod slurp-input-stream ((x (eql 'string)) stream &key stripped)
    (slurp-stream-string stream :stripped stripped))

  (defmethod slurp-input-stream ((x (eql :string)) stream &key stripped)
    (slurp-stream-string stream :stripped stripped))

  (defmethod slurp-input-stream ((x (eql :lines)) stream &key count)
    (slurp-stream-lines stream :count count))

  (defmethod slurp-input-stream ((x (eql :line)) stream &key (at 0))
    (slurp-stream-line stream :at at))

  (defmethod slurp-input-stream ((x (eql :forms)) stream &key count)
    (slurp-stream-forms stream :count count))

  (defmethod slurp-input-stream ((x (eql :form)) stream &key (at 0))
    (slurp-stream-form stream :at at))

  (defmethod slurp-input-stream ((x (eql t)) stream &rest keys &key &allow-other-keys)
    (apply 'slurp-input-stream *standard-output* stream keys))

  (defmethod slurp-input-stream ((x null) (stream t) &key)
    nil)

  (defmethod slurp-input-stream ((pathname pathname) input
                                 &key
                                   (element-type *default-stream-element-type*)
                                   (external-format *utf-8-external-format*)
                                   (if-exists :rename-and-delete)
                                   (if-does-not-exist :create)
                                   buffer-size
                                   linewise)
    (with-output-file (output pathname
                              :element-type element-type
                              :external-format external-format
                              :if-exists if-exists
                              :if-does-not-exist if-does-not-exist)
      (copy-stream-to-stream
       input output
       :element-type element-type :buffer-size buffer-size :linewise linewise)))

  (defmethod slurp-input-stream (x stream
                                 &key linewise prefix (element-type 'character) buffer-size)
    (declare (ignorable stream linewise prefix element-type buffer-size))
    (cond
      #+genera
      ((functionp x) (funcall x stream))
      #+genera
      ((output-stream-p x)
       (copy-stream-to-stream
        stream x
        :linewise linewise :prefix prefix :element-type element-type :buffer-size buffer-size))
      (t
       (parameter-error "Invalid ~S destination ~S" 'slurp-input-stream x)))))

;;;; Vomiting a stream, typically into the input of another program.
(with-upgradability ()
  (defgeneric vomit-output-stream (processor output-stream &key)
    (:documentation
     "VOMIT-OUTPUT-STREAM is a generic function with two positional arguments
PROCESSOR and OUTPUT-STREAM and additional keyword arguments, that produces (vomits)
some content onto the OUTPUT-STREAM, according to a method specified by PROCESSOR.

Built-in methods include the following:
* if PROCESSOR is a function, it is called with the OUTPUT-STREAM as its argument
* if PROCESSOR is a list, its first element should be a function.
  It will be applied to a cons of the OUTPUT-STREAM and the rest of the list.
  That is (x . y) will be treated as \(APPLY x <stream> y\)
* if PROCESSOR is an input-stream, its contents will be copied the OUTPUT-STREAM,
  per copy-stream-to-stream, with appropriate keyword arguments.
* if PROCESSOR is a string, its contents will be printed to the OUTPUT-STREAM.
* if PROCESSOR is T, it is treated the same as *standard-input*. If it is NIL, nothing is done.

Programmers are encouraged to define their own methods for this generic function."))

  #-genera
  (defmethod vomit-output-stream ((function function) output-stream &key)
    (funcall function output-stream))

  (defmethod vomit-output-stream ((list cons) output-stream &key)
    (apply (first list) output-stream (rest list)))

  #-genera
  (defmethod vomit-output-stream ((input-stream stream) output-stream
                                 &key linewise prefix (element-type 'character) buffer-size)
    (copy-stream-to-stream
     input-stream output-stream
     :linewise linewise :prefix prefix :element-type element-type :buffer-size buffer-size))

  (defmethod vomit-output-stream ((x string) stream &key fresh-line terpri)
    (princ x stream)
    (when fresh-line (fresh-line stream))
    (when terpri (terpri stream))
    (values))

  (defmethod vomit-output-stream ((x (eql t)) stream &rest keys &key &allow-other-keys)
    (apply 'vomit-output-stream *standard-input* stream keys))

  (defmethod vomit-output-stream ((x null) (stream t) &key)
    (values))

  (defmethod vomit-output-stream ((pathname pathname) input
                                 &key
                                   (element-type *default-stream-element-type*)
                                   (external-format *utf-8-external-format*)
                                   (if-exists :rename-and-delete)
                                   (if-does-not-exist :create)
                                   buffer-size
                                   linewise)
    (with-output-file (output pathname
                              :element-type element-type
                              :external-format external-format
                              :if-exists if-exists
                              :if-does-not-exist if-does-not-exist)
      (copy-stream-to-stream
       input output
       :element-type element-type :buffer-size buffer-size :linewise linewise)))

  (defmethod vomit-output-stream (x stream
                                 &key linewise prefix (element-type 'character) buffer-size)
    (declare (ignorable stream linewise prefix element-type buffer-size))
    (cond
      #+genera
      ((functionp x) (funcall x stream))
      #+genera
      ((input-stream-p x)
       (copy-stream-to-stream
        x stream
        :linewise linewise :prefix prefix :element-type element-type :buffer-size buffer-size))
      (t
       (parameter-error "Invalid ~S source ~S" 'vomit-output-stream x)))))


;;;; Run-program: synchronously run a program in a subprocess, handling input, output and error-output.
(with-upgradability ()
  (define-condition subprocess-error (error)
    ((code :initform nil :initarg :code :reader subprocess-error-code)
     (command :initform nil :initarg :command :reader subprocess-error-command)
     (process :initform nil :initarg :process :reader subprocess-error-process))
    (:report (lambda (condition stream)
               (format stream "Subprocess ~@[~S~% ~]~@[with command ~S~% ~]exited with error~@[ code ~D~]"
                       (subprocess-error-process condition)
                       (subprocess-error-command condition)
                       (subprocess-error-code condition)))))

  (defun %check-result (exit-code &key command process ignore-error-status)
    (unless ignore-error-status
      (unless (eql exit-code 0)
        (cerror "IGNORE-ERROR-STATUS"
                'subprocess-error :command command :code exit-code :process process)))
    exit-code)

  (defun %active-io-specifier-p (specifier)
    "Determines whether a run-program I/O specifier requires Lisp-side processing
via SLURP-INPUT-STREAM or VOMIT-OUTPUT-STREAM (return T),
or whether it's already taken care of by the implementation's underlying run-program."
    (not (typep specifier '(or null string pathname (member :interactive :output)
                            #+(or cmucl (and sbcl os-unix) scl) (or stream (eql t))
                            #+lispworks file-stream))))

  (defun %run-program (command &rest keys &key &allow-other-keys)
    "DEPRECATED. Use LAUNCH-PROGRAM instead."
    (apply 'launch-program command keys))

  (defun %call-with-program-io (gf tval stream-easy-p fun direction spec activep returner
                                &key
                                  (element-type #-clozure *default-stream-element-type* #+clozure 'character)
                                  (external-format *utf-8-external-format*) &allow-other-keys)
    ;; handle redirection for run-program and system
    ;; SPEC is the specification for the subprocess's input or output or error-output
    ;; TVAL is the value used if the spec is T
    ;; GF is the generic function to call to handle arbitrary values of SPEC
    ;; STREAM-EASY-P is T if we're going to use a RUN-PROGRAM that copies streams in the background
    ;; (it's only meaningful on CMUCL, SBCL, SCL that actually do it)
    ;; DIRECTION is :INPUT, :OUTPUT or :ERROR-OUTPUT for the direction of this io argument
    ;; FUN is a function of the new reduced spec and an activity function to call with a stream
    ;; when the subprocess is active and communicating through that stream.
    ;; ACTIVEP is a boolean true if we will get to run code while the process is running
    ;; ELEMENT-TYPE and EXTERNAL-FORMAT control what kind of temporary file we may open.
    ;; RETURNER is a function called with the value of the activity.
    ;; --- TODO (fare@tunes.org): handle if-output-exists and such when doing it the hard way.
    (declare (ignorable stream-easy-p))
    (let* ((actual-spec (if (eq spec t) tval spec))
           (activity-spec (if (eq actual-spec :output)
                              (ecase direction
                                ((:input :output)
                                 (parameter-error "~S does not allow ~S as a ~S spec"
                                                  'run-program :output direction))
                                ((:error-output)
                                 nil))
                              actual-spec)))
      (labels ((activity (stream)
                 (call-function returner (call-stream-processor gf activity-spec stream)))
               (easy-case ()
                 (funcall fun actual-spec nil))
               (hard-case ()
                 (if activep
                     (funcall fun :stream #'activity)
                     (with-temporary-file (:pathname tmp)
                       (ecase direction
                         (:input
                          (with-output-file (s tmp :if-exists :overwrite
                                               :external-format external-format
                                               :element-type element-type)
                            (activity s))
                          (funcall fun tmp nil))
                         ((:output :error-output)
                          (multiple-value-prog1 (funcall fun tmp nil)
                            (with-input-file (s tmp
                                               :external-format external-format
                                               :element-type element-type)
                              (activity s)))))))))
        (typecase activity-spec
          ((or null string pathname (eql :interactive))
           (easy-case))
          #+(or cmucl (and sbcl os-unix) scl) ;; streams are only easy on implementations that try very hard
          (stream
           (if stream-easy-p (easy-case) (hard-case)))
          (t
           (hard-case))))))

  (defmacro place-setter (place)
    (when place
      (let ((value (gensym)))
        `#'(lambda (,value) (setf ,place ,value)))))

  (defmacro with-program-input (((reduced-input-var
                                  &optional (input-activity-var (gensym) iavp))
                                 input-form &key setf stream-easy-p active keys) &body body)
    `(apply '%call-with-program-io 'vomit-output-stream *standard-input* ,stream-easy-p
            #'(lambda (,reduced-input-var ,input-activity-var)
                ,@(unless iavp `((declare (ignore ,input-activity-var))))
                ,@body)
            :input ,input-form ,active (place-setter ,setf) ,keys))

  (defmacro with-program-output (((reduced-output-var
                                  &optional (output-activity-var (gensym) oavp))
                                  output-form &key setf stream-easy-p active keys) &body body)
    `(apply '%call-with-program-io 'slurp-input-stream *standard-output* ,stream-easy-p
            #'(lambda (,reduced-output-var ,output-activity-var)
                ,@(unless oavp `((declare (ignore ,output-activity-var))))
                ,@body)
            :output ,output-form ,active (place-setter ,setf) ,keys))

  (defmacro with-program-error-output (((reduced-error-output-var
                                         &optional (error-output-activity-var (gensym) eoavp))
                                        error-output-form &key setf stream-easy-p active keys)
                                       &body body)
    `(apply '%call-with-program-io 'slurp-input-stream *error-output* ,stream-easy-p
            #'(lambda (,reduced-error-output-var ,error-output-activity-var)
                ,@(unless eoavp `((declare (ignore ,error-output-activity-var))))
                ,@body)
            :error-output ,error-output-form ,active (place-setter ,setf) ,keys))

  (defun %use-launch-program (command &rest keys
                           &key input output error-output ignore-error-status &allow-other-keys)
    ;; helper for RUN-PROGRAM when using LAUNCH-PROGRAM
    #+(or cormanlisp gcl (and lispworks os-windows) mcl xcl)
    (progn
      command keys input output error-output ignore-error-status ;; ignore
      (not-implemented-error '%use-launch-program))
    (when (member :stream (list input output error-output))
      (parameter-error "~S: ~S is not allowed as synchronous I/O redirection argument"
                       'run-program :stream))
    (let* ((active-input-p (%active-io-specifier-p input))
           (active-output-p (%active-io-specifier-p output))
           (active-error-output-p (%active-io-specifier-p error-output))
           (activity
             (cond
               (active-output-p :output)
               (active-input-p :input)
               (active-error-output-p :error-output)
               (t nil)))
           output-result error-output-result exit-code process-info)
      (with-program-output ((reduced-output output-activity)
                            output :keys keys :setf output-result
                            :stream-easy-p t :active (eq activity :output))
        (with-program-error-output ((reduced-error-output error-output-activity)
                                    error-output :keys keys :setf error-output-result
                                    :stream-easy-p t :active (eq activity :error-output))
          (with-program-input ((reduced-input input-activity)
                               input :keys keys
                               :stream-easy-p t :active (eq activity :input))
            (setf process-info
                  (apply 'launch-program command
                         :input reduced-input :output reduced-output
                         :error-output (if (eq error-output :output) :output reduced-error-output)
                         keys))
            (labels ((get-stream (stream-name &optional fallbackp)
                       (or (slot-value process-info stream-name)
                           (when fallbackp
                             (slot-value process-info 'bidir-stream))))
                     (run-activity (activity stream-name &optional fallbackp)
                       (if-let (stream (get-stream stream-name fallbackp))
                         (funcall activity stream)
                         (error 'subprocess-error
                                :code `(:missing ,stream-name)
                                :command command :process process-info))))
              (unwind-protect
                   (ecase activity
                     ((nil))
                     (:input (run-activity input-activity 'input-stream t))
                     (:output (run-activity output-activity 'output-stream t))
                     (:error-output (run-activity error-output-activity 'error-output-stream)))
                (close-streams process-info)
                (setf exit-code (wait-process process-info)))))))
      (%check-result exit-code
                     :command command :process process-info
                     :ignore-error-status ignore-error-status)
      (values output-result error-output-result exit-code)))

  (defun %normalize-system-command (command) ;; helper for %USE-SYSTEM
    (etypecase command
      (string command)
      (list (escape-shell-command
             (os-cond
              ((os-unix-p) (cons "exec" command))
              (t command))))))

  (defun %redirected-system-command (command in out err directory) ;; helper for %USE-SYSTEM
    (flet ((redirect (spec operator)
             (let ((pathname
                     (typecase spec
                       (null (null-device-pathname))
                       (string (parse-native-namestring spec))
                       (pathname spec)
                       ((eql :output)
                        (unless (equal operator " 2>>")
                          (parameter-error "~S: only the ~S argument can be ~S"
                                           'run-program :error-output :output))
                        (return-from redirect '(" 2>&1"))))))
               (when pathname
                 (list operator " "
                       (escape-shell-token (native-namestring pathname)))))))
      (let* ((redirections (append (redirect in " <") (redirect out " >>") (redirect err " 2>>")))
             (normalized (%normalize-system-command command))
             (directory (or directory #+(or abcl xcl) (getcwd)))
             (chdir (when directory
                      (let ((dir-arg (escape-shell-token (native-namestring directory))))
                        (os-cond
                         ((os-unix-p) `("cd " ,dir-arg " ; "))
                         ((os-windows-p) `("cd /d " ,dir-arg " & ")))))))
        (reduce/strcat
         (os-cond
          ((os-unix-p) `(,@(when redirections `("exec " ,@redirections " ; ")) ,@chdir ,normalized))
          ((os-windows-p) `(,@redirections " (" ,@chdir ,normalized ")")))))))

  (defun %system (command &rest keys &key directory
                                       input (if-input-does-not-exist :error)
                                       output (if-output-exists :supersede)
                                       error-output (if-error-output-exists :supersede)
                                       &allow-other-keys)
    "A portable abstraction of a low-level call to libc's system()."
    (declare (ignorable keys directory input if-input-does-not-exist output
                        if-output-exists error-output if-error-output-exists))
    (when (member :stream (list input output error-output))
      (parameter-error "~S: ~S is not allowed as synchronous I/O redirection argument"
                       'run-program :stream))
    #+(or abcl allegro clozure cmucl ecl (and lispworks os-unix) mkcl sbcl scl)
    (let (#+(or abcl ecl mkcl)
            (version (parse-version
                      #-abcl
                      (lisp-implementation-version)
                      #+abcl
                      (second (split-string (implementation-identifier) :separator '(#\-))))))
      (nest
       #+abcl (unless (lexicographic< '< version '(1 4 0)))
       #+ecl (unless (lexicographic<= '< version '(16 0 0)))
       #+mkcl (unless (lexicographic<= '< version '(1 1 9)))
       (return-from %system
         (wait-process
          (apply 'launch-program (%normalize-system-command command) keys)))))
    #+(or abcl clasp clisp cormanlisp ecl gcl genera (and lispworks os-windows) mkcl xcl)
    (let ((%command (%redirected-system-command command input output error-output directory)))
      ;; see comments for these functions
      (%handle-if-does-not-exist input if-input-does-not-exist)
      (%handle-if-exists output if-output-exists)
      (%handle-if-exists error-output if-error-output-exists)
      #+abcl (ext:run-shell-command %command)
      #+(or clasp ecl) (let ((*standard-input* *stdin*)
                             (*standard-output* *stdout*)
                             (*error-output* *stderr*))
                         (ext:system %command))
      #+clisp
      (let ((raw-exit-code
             (or
              #.`(#+os-windows ,@'(ext:run-shell-command %command)
                  #+os-unix ,@'(ext:run-program "/bin/sh" :arguments `("-c" ,%command))
                  :wait t :input :terminal :output :terminal)
              0)))
        (if (minusp raw-exit-code)
            (- 128 raw-exit-code)
            raw-exit-code))
      #+cormanlisp (win32:system %command)
      #+gcl (system:system %command)
      #+genera (not-implemented-error '%system)
      #+(and lispworks os-windows)
      (system:call-system %command :current-directory directory :wait t)
      #+mcl (ccl::with-cstrs ((%%command %command)) (_system %%command))
      #+mkcl (mkcl:system %command)
      #+xcl (system:%run-shell-command %command)))

  (defun %use-system (command &rest keys
                      &key input output error-output ignore-error-status &allow-other-keys)
    ;; helper for RUN-PROGRAM when using %system
    (let (output-result error-output-result exit-code)
      (with-program-output ((reduced-output)
                            output :keys keys :setf output-result)
        (with-program-error-output ((reduced-error-output)
                                    error-output :keys keys :setf error-output-result)
          (with-program-input ((reduced-input) input :keys keys)
            (setf exit-code (apply '%system command
                                   :input reduced-input :output reduced-output
                                   :error-output reduced-error-output keys)))))
      (%check-result exit-code
                     :command command
                     :ignore-error-status ignore-error-status)
      (values output-result error-output-result exit-code)))

  (defun run-program (command &rest keys
                       &key ignore-error-status (force-shell nil force-shell-suppliedp)
                         input (if-input-does-not-exist :error)
                         output (if-output-exists :supersede)
                         error-output (if-error-output-exists :supersede)
                         (element-type #-clozure *default-stream-element-type* #+clozure 'character)
                         (external-format *utf-8-external-format*)
                       &allow-other-keys)
    "Run program specified by COMMAND,
either a list of strings specifying a program and list of arguments,
or a string specifying a shell command (/bin/sh on Unix, CMD.EXE on Windows);
_synchronously_ process its output as specified and return the processing results
when the program and its output processing are complete.

Always call a shell (rather than directly execute the command when possible)
if FORCE-SHELL is specified.  Similarly, never call a shell if FORCE-SHELL is
specified to be NIL.

Signal a continuable SUBPROCESS-ERROR if the process wasn't successful (exit-code 0),
unless IGNORE-ERROR-STATUS is specified.

If OUTPUT is a pathname, a string designating a pathname, or NIL (the default)
designating the null device, the file at that path is used as output.
If it's :INTERACTIVE, output is inherited from the current process;
beware that this may be different from your *STANDARD-OUTPUT*,
and under SLIME will be on your *inferior-lisp* buffer.
If it's T, output goes to your current *STANDARD-OUTPUT* stream.
Otherwise, OUTPUT should be a value that is a suitable first argument to
SLURP-INPUT-STREAM (qv.), or a list of such a value and keyword arguments.
In this case, RUN-PROGRAM will create a temporary stream for the program output;
the program output, in that stream, will be processed by a call to SLURP-INPUT-STREAM,
using OUTPUT as the first argument (or the first element of OUTPUT, and the rest as keywords).
The primary value resulting from that call (or NIL if no call was needed)
will be the first value returned by RUN-PROGRAM.
E.g., using :OUTPUT :STRING will have it return the entire output stream as a string.
And using :OUTPUT '(:STRING :STRIPPED T) will have it return the same string
stripped of any ending newline.

IF-OUTPUT-EXISTS, which is only meaningful if OUTPUT is a string or a
pathname, can take the values :ERROR, :APPEND, and :SUPERSEDE (the
default). The meaning of these values and their effect on the case
where OUTPUT does not exist, is analogous to the IF-EXISTS parameter
to OPEN with :DIRECTION :OUTPUT.

ERROR-OUTPUT is similar to OUTPUT, except that the resulting value is returned
as the second value of RUN-PROGRAM. T designates the *ERROR-OUTPUT*.
Also :OUTPUT means redirecting the error output to the output stream,
in which case NIL is returned.

IF-ERROR-OUTPUT-EXISTS is similar to IF-OUTPUT-EXIST, except that it
affects ERROR-OUTPUT rather than OUTPUT.

INPUT is similar to OUTPUT, except that VOMIT-OUTPUT-STREAM is used,
no value is returned, and T designates the *STANDARD-INPUT*.

IF-INPUT-DOES-NOT-EXIST, which is only meaningful if INPUT is a string
or a pathname, can take the values :CREATE and :ERROR (the
default). The meaning of these values is analogous to the
IF-DOES-NOT-EXIST parameter to OPEN with :DIRECTION :INPUT.

ELEMENT-TYPE and EXTERNAL-FORMAT are passed on
to your Lisp implementation, when applicable, for creation of the output stream.

One and only one of the stream slurping or vomiting may or may not happen
in parallel in parallel with the subprocess,
depending on options and implementation,
and with priority being given to output processing.
Other streams are completely produced or consumed
before or after the subprocess is spawned, using temporary files.

RUN-PROGRAM returns 3 values:
0- the result of the OUTPUT slurping if any, or NIL
1- the result of the ERROR-OUTPUT slurping if any, or NIL
2- either 0 if the subprocess exited with success status,
or an indication of failure via the EXIT-CODE of the process"
    (declare (ignorable input output error-output if-input-does-not-exist if-output-exists
                        if-error-output-exists element-type external-format ignore-error-status))
    #-(or abcl allegro clasp clisp clozure cmucl cormanlisp ecl gcl lispworks mcl mkcl sbcl scl xcl)
    (not-implemented-error 'run-program)
    (apply (if (or force-shell
                   ;; Per doc string, set FORCE-SHELL to T if we get command as a string.
                   ;; But don't override user's specified preference. [2015/06/29:rpg]
                   (and (stringp command)
                        (or (not force-shell-suppliedp)
                            #-(or allegro clisp clozure sbcl) (os-cond ((os-windows-p) t))))
                   #+(or clasp clisp cormanlisp gcl (and lispworks os-windows) mcl xcl) t
                   ;; A race condition in ECL <= 16.0.0 prevents using ext:run-program
                   #+ecl #.(if-let (ver (parse-version (lisp-implementation-version)))
                                   (lexicographic<= '< ver '(16 0 0)))
                   #+(and lispworks os-unix) (%interactivep input output error-output))
               '%use-system '%use-launch-program)
           command keys)))

;;;; ---------------------------------------------------------------------------
;;;; Generic support for configuration files

(uiop/package:define-package :uiop/configuration
  (:recycle :uiop/configuration :asdf/configuration) ;; necessary to upgrade from 2.27.
  (:use :uiop/common-lisp :uiop/utility
   :uiop/os :uiop/pathname :uiop/filesystem :uiop/stream :uiop/image :uiop/lisp-build)
  (:export
   #:user-configuration-directories #:system-configuration-directories ;; implemented in backward-driver
   #:in-first-directory #:in-user-configuration-directory #:in-system-configuration-directory ;; idem
   #:get-folder-path
   #:xdg-data-home #:xdg-config-home #:xdg-data-dirs #:xdg-config-dirs
   #:xdg-cache-home #:xdg-runtime-dir #:system-config-pathnames
   #:filter-pathname-set #:xdg-data-pathnames #:xdg-config-pathnames
   #:find-preferred-file #:xdg-data-pathname #:xdg-config-pathname
   #:validate-configuration-form #:validate-configuration-file #:validate-configuration-directory
   #:configuration-inheritance-directive-p
   #:report-invalid-form #:invalid-configuration #:*ignored-configuration-form* #:*user-cache*
   #:*clear-configuration-hook* #:clear-configuration #:register-clear-configuration-hook
   #:resolve-location #:location-designator-p #:location-function-p #:*here-directory*
   #:resolve-relative-location #:resolve-absolute-location #:upgrade-configuration))
(in-package :uiop/configuration)

(with-upgradability ()
  (define-condition invalid-configuration ()
    ((form :reader condition-form :initarg :form)
     (location :reader condition-location :initarg :location)
     (format :reader condition-format :initarg :format)
     (arguments :reader condition-arguments :initarg :arguments :initform nil))
    (:report (lambda (c s)
               (format s (compatfmt "~@<~? (will be skipped)~@:>")
                       (condition-format c)
                       (list* (condition-form c) (condition-location c)
                              (condition-arguments c))))))

  (defun configuration-inheritance-directive-p (x)
    "Is X a configuration inheritance directive?"
    (let ((kw '(:inherit-configuration :ignore-inherited-configuration)))
      (or (member x kw)
          (and (length=n-p x 1) (member (car x) kw)))))

  (defun report-invalid-form (reporter &rest args)
    "Report an invalid form according to REPORTER and various ARGS"
    (etypecase reporter
      (null
       (apply 'error 'invalid-configuration args))
      (function
       (apply reporter args))
      ((or symbol string)
       (apply 'error reporter args))
      (cons
       (apply 'apply (append reporter args)))))

  (defvar *ignored-configuration-form* nil
    "Have configuration forms been ignored while parsing the configuration?")

  (defun validate-configuration-form (form tag directive-validator
                                            &key location invalid-form-reporter)
    "Validate a configuration FORM. By default it will raise an error if the
FORM is not valid.  Otherwise it will return the validated form.
     Arguments control the behavior:
     The configuration FORM should be of the form (TAG . <rest>)
     Each element of <rest> will be checked by first seeing if it's a configuration inheritance
directive (see CONFIGURATION-INHERITANCE-DIRECTIVE-P) then invoking DIRECTIVE-VALIDATOR
on it.
     In the event of an invalid form, INVALID-FORM-REPORTER will be used to control
reporting (see REPORT-INVALID-FORM) with LOCATION providing information about where
the configuration form appeared."
    (unless (and (consp form) (eq (car form) tag))
      (setf *ignored-configuration-form* t)
      (report-invalid-form invalid-form-reporter :form form :location location)
      (return-from validate-configuration-form nil))
    (loop :with inherit = 0 :with ignore-invalid-p = nil :with x = (list tag)
          :for directive :in (cdr form)
          :when (cond
                  ((configuration-inheritance-directive-p directive)
                   (incf inherit) t)
                  ((eq directive :ignore-invalid-entries)
                   (setf ignore-invalid-p t) t)
                  ((funcall directive-validator directive)
                   t)
                  (ignore-invalid-p
                   nil)
                  (t
                   (setf *ignored-configuration-form* t)
                   (report-invalid-form invalid-form-reporter :form directive :location location)
                   nil))
            :do (push directive x)
          :finally
             (unless (= inherit 1)
               (report-invalid-form invalid-form-reporter
                                    :form form :location location
                                    ;; we throw away the form and location arguments, hence the ~2*
                                    ;; this is necessary because of the report in INVALID-CONFIGURATION
                                    :format (compatfmt "~@<Invalid source registry ~S~@[ in ~S~]. ~
                                                        One and only one of ~S or ~S is required.~@:>")
                                    :arguments '(:inherit-configuration :ignore-inherited-configuration)))
             (return (nreverse x))))

  (defun validate-configuration-file (file validator &key description)
    "Validate a configuration FILE.  The configuration file should have only one s-expression
in it, which will be checked with the VALIDATOR FORM.  DESCRIPTION argument used for error
reporting."
    (let ((forms (read-file-forms file)))
      (unless (length=n-p forms 1)
        (error (compatfmt "~@<One and only one form allowed for ~A. Got: ~3i~_~S~@:>~%")
               description forms))
      (funcall validator (car forms) :location file)))

  (defun validate-configuration-directory (directory tag validator &key invalid-form-reporter)
    "Map the VALIDATOR across the .conf files in DIRECTORY, the TAG will
be applied to the results to yield a configuration form.  Current
values of TAG include :source-registry and :output-translations."
    (let ((files (sort (ignore-errors ;; SORT w/o COPY-LIST is OK: DIRECTORY returns a fresh list
                        (remove-if
                         'hidden-pathname-p
                         (directory* (make-pathname :name *wild* :type "conf" :defaults directory))))
                       #'string< :key #'namestring)))
      `(,tag
        ,@(loop :for file :in files :append
                                    (loop :with ignore-invalid-p = nil
                                          :for form :in (read-file-forms file)
                                          :when (eq form :ignore-invalid-entries)
                                            :do (setf ignore-invalid-p t)
                                          :else
                                            :when (funcall validator form)
                                              :collect form
                                          :else
                                            :when ignore-invalid-p
                                              :do (setf *ignored-configuration-form* t)
                                          :else
                                            :do (report-invalid-form invalid-form-reporter :form form :location file)))
        :inherit-configuration)))

  (defun resolve-relative-location (x &key ensure-directory wilden)
    "Given a designator X for an relative location, resolve it to a pathname."
    (ensure-pathname
     (etypecase x
       (null nil)
       (pathname x)
       (string (parse-unix-namestring
                x :ensure-directory ensure-directory))
       (cons
        (if (null (cdr x))
            (resolve-relative-location
             (car x) :ensure-directory ensure-directory :wilden wilden)
            (let* ((car (resolve-relative-location
                         (car x) :ensure-directory t :wilden nil)))
              (merge-pathnames*
               (resolve-relative-location
                (cdr x) :ensure-directory ensure-directory :wilden wilden)
               car))))
       ((eql :*/) *wild-directory*)
       ((eql :**/) *wild-inferiors*)
       ((eql :*.*.*) *wild-file*)
       ((eql :implementation)
        (parse-unix-namestring
         (implementation-identifier) :ensure-directory t))
       ((eql :implementation-type)
        (parse-unix-namestring
         (string-downcase (implementation-type)) :ensure-directory t))
       ((eql :hostname)
        (parse-unix-namestring (hostname) :ensure-directory t)))
     :wilden (and wilden (not (pathnamep x)) (not (member x '(:*/ :**/ :*.*.*))))
     :want-relative t))

  (defvar *here-directory* nil
    "This special variable is bound to the currect directory during calls to
PROCESS-SOURCE-REGISTRY in order that we be able to interpret the :here
directive.")

  (defvar *user-cache* nil
    "A specification as per RESOLVE-LOCATION of where the user keeps his FASL cache")

  (defun resolve-absolute-location (x &key ensure-directory wilden)
    "Given a designator X for an absolute location, resolve it to a pathname"
    (ensure-pathname
     (etypecase x
       (null nil)
       (pathname x)
       (string
        (let ((p #-mcl (parse-namestring x)
                 #+mcl (probe-posix x)))
          #+mcl (unless p (error "POSIX pathname ~S does not exist" x))
          (if ensure-directory (ensure-directory-pathname p) p)))
       (cons
        (return-from resolve-absolute-location
          (if (null (cdr x))
              (resolve-absolute-location
               (car x) :ensure-directory ensure-directory :wilden wilden)
              (merge-pathnames*
               (resolve-relative-location
                (cdr x) :ensure-directory ensure-directory :wilden wilden)
               (resolve-absolute-location
                (car x) :ensure-directory t :wilden nil)))))
       ((eql :root)
        ;; special magic! we return a relative pathname,
        ;; but what it means to the output-translations is
        ;; "relative to the root of the source pathname's host and device".
        (return-from resolve-absolute-location
          (let ((p (make-pathname :directory '(:relative))))
            (if wilden (wilden p) p))))
       ((eql :home) (user-homedir-pathname))
       ((eql :here) (resolve-absolute-location
                     (or *here-directory* (pathname-directory-pathname (load-pathname)))
                     :ensure-directory t :wilden nil))
       ((eql :user-cache) (resolve-absolute-location
                           *user-cache* :ensure-directory t :wilden nil)))
     :wilden (and wilden (not (pathnamep x)))
     :resolve-symlinks *resolve-symlinks*
     :want-absolute t))

  ;; Try to override declaration in previous versions of ASDF.
  (declaim (ftype (function (t &key (:directory boolean) (:wilden boolean)
                               (:ensure-directory boolean)) t) resolve-location))

  (defun* (resolve-location) (x &key ensure-directory wilden directory)
    "Resolve location designator X into a PATHNAME"
    ;; :directory backward compatibility, until 2014-01-16: accept directory as well as ensure-directory
    (loop* :with dirp = (or directory ensure-directory)
           :with (first . rest) = (if (atom x) (list x) x)
           :with path = (or (resolve-absolute-location
                             first :ensure-directory (and (or dirp rest) t)
                                   :wilden (and wilden (null rest)))
                            (return nil))
           :for (element . morep) :on rest
           :for dir = (and (or morep dirp) t)
           :for wild = (and wilden (not morep))
           :for sub = (merge-pathnames*
                       (resolve-relative-location
                        element :ensure-directory dir :wilden wild)
                       path)
           :do (setf path (if (absolute-pathname-p sub) (resolve-symlinks* sub) sub))
           :finally (return path)))

  (defun location-designator-p (x)
    "Is X a designator for a location?"
    ;; NIL means "skip this entry", or as an output translation, same as translation input.
    ;; T means "any input" for a translation, or as output, same as translation input.
    (flet ((absolute-component-p (c)
             (typep c '(or string pathname
                        (member :root :home :here :user-cache))))
           (relative-component-p (c)
             (typep c '(or string pathname
                        (member :*/ :**/ :*.*.* :implementation :implementation-type)))))
      (or (typep x 'boolean)
          (absolute-component-p x)
          (and (consp x) (absolute-component-p (first x)) (every #'relative-component-p (rest x))))))

  (defun location-function-p (x)
    "Is X the specification of a location function?"
    ;; Location functions are allowed in output translations, and notably used by ABCL for JAR file support.
    (and (length=n-p x 2) (eq (car x) :function)))

  (defvar *clear-configuration-hook* '())

  (defun register-clear-configuration-hook (hook-function &optional call-now-p)
    "Register a function to be called when clearing configuration"
    (register-hook-function '*clear-configuration-hook* hook-function call-now-p))

  (defun clear-configuration ()
    "Call the functions in *CLEAR-CONFIGURATION-HOOK*"
    (call-functions *clear-configuration-hook*))

  (register-image-dump-hook 'clear-configuration)

  (defun upgrade-configuration ()
    "If a previous version of ASDF failed to read some configuration, try again now."
    (when *ignored-configuration-form*
      (clear-configuration)
      (setf *ignored-configuration-form* nil)))


  (defun get-folder-path (folder)
    "Semi-portable implementation of a subset of LispWorks' sys:get-folder-path,
this function tries to locate the Windows FOLDER for one of
:LOCAL-APPDATA, :APPDATA or :COMMON-APPDATA.
     Returns NIL when the folder is not defined (e.g., not on Windows)."
    (or #+(and lispworks os-windows) (sys:get-folder-path folder)
        ;; read-windows-registry HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\User Shell Folders\AppData
        (ecase folder
          (:local-appdata (or (getenv-absolute-directory "LOCALAPPDATA")
                              (subpathname* (get-folder-path :appdata) "Local")))
          (:appdata (getenv-absolute-directory "APPDATA"))
          (:common-appdata (or (getenv-absolute-directory "ALLUSERSAPPDATA")
                               (subpathname* (getenv-absolute-directory "ALLUSERSPROFILE") "Application Data/"))))))


  ;; Support for the XDG Base Directory Specification
  (defun xdg-data-home (&rest more)
    "Returns an absolute pathname for the directory containing user-specific data files.
MORE may contain specifications for a subpath relative to this directory: a
subpathname specification and keyword arguments as per RESOLVE-LOCATION \(see
also \"Configuration DSL\"\) in the ASDF manual."
    (resolve-absolute-location
     `(,(or (getenv-absolute-directory "XDG_DATA_HOME")
            (os-cond
             ((os-windows-p) (get-folder-path :local-appdata))
             (t (subpathname (user-homedir-pathname) ".local/share/"))))
       ,more)))

  (defun xdg-config-home (&rest more)
    "Returns a pathname for the directory containing user-specific configuration files.
MORE may contain specifications for a subpath relative to this directory: a
subpathname specification and keyword arguments as per RESOLVE-LOCATION \(see
also \"Configuration DSL\"\) in the ASDF manual."
    (resolve-absolute-location
     `(,(or (getenv-absolute-directory "XDG_CONFIG_HOME")
            (os-cond
             ((os-windows-p) (xdg-data-home "config/"))
             (t (subpathname (user-homedir-pathname) ".config/"))))
       ,more)))

  (defun xdg-data-dirs (&rest more)
    "The preference-ordered set of additional paths to search for data files.
Returns a list of absolute directory pathnames.
MORE may contain specifications for a subpath relative to these directories: a
subpathname specification and keyword arguments as per RESOLVE-LOCATION \(see
also \"Configuration DSL\"\) in the ASDF manual."
    (mapcar #'(lambda (d) (resolve-location `(,d ,more)))
            (or (remove nil (getenv-absolute-directories "XDG_DATA_DIRS"))
                (os-cond
                 ((os-windows-p) (mapcar 'get-folder-path '(:appdata :common-appdata)))
                 (t (mapcar 'parse-unix-namestring '("/usr/local/share/" "/usr/share/")))))))

  (defun xdg-config-dirs (&rest more)
    "The preference-ordered set of additional base paths to search for configuration files.
Returns a list of absolute directory pathnames.
MORE may contain specifications for a subpath relative to these directories:
subpathname specification and keyword arguments as per RESOLVE-LOCATION \(see
also \"Configuration DSL\"\) in the ASDF manual."
    (mapcar #'(lambda (d) (resolve-location `(,d ,more)))
            (or (remove nil (getenv-absolute-directories "XDG_CONFIG_DIRS"))
                (os-cond
                 ((os-windows-p) (xdg-data-dirs "config/"))
                 (t (mapcar 'parse-unix-namestring '("/etc/xdg/")))))))

  (defun xdg-cache-home (&rest more)
    "The base directory relative to which user specific non-essential data files should be stored.
Returns an absolute directory pathname.
MORE may contain specifications for a subpath relative to this directory: a
subpathname specification and keyword arguments as per RESOLVE-LOCATION \(see
also \"Configuration DSL\"\) in the ASDF manual."
    (resolve-absolute-location
     `(,(or (getenv-absolute-directory "XDG_CACHE_HOME")
            (os-cond
             ((os-windows-p) (xdg-data-home "cache/"))
             (t (subpathname* (user-homedir-pathname) ".cache/"))))
       ,more)))

  (defun xdg-runtime-dir (&rest more)
    "Pathname for user-specific non-essential runtime files and other file objects,
such as sockets, named pipes, etc.
Returns an absolute directory pathname.
MORE may contain specifications for a subpath relative to this directory: a
subpathname specification and keyword arguments as per RESOLVE-LOCATION \(see
also \"Configuration DSL\"\) in the ASDF manual."
    ;; The XDG spec says that if not provided by the login system, the application should
    ;; issue a warning and provide a replacement. UIOP is not equipped to do that and returns NIL.
    (resolve-absolute-location `(,(getenv-absolute-directory "XDG_RUNTIME_DIR") ,more)))

  ;;; NOTE: modified the docstring because "system user configuration
  ;;; directories" seems self-contradictory. I'm not sure my wording is right.
  (defun system-config-pathnames (&rest more)
    "Return a list of directories where are stored the system's default user configuration information.
MORE may contain specifications for a subpath relative to these directories: a
subpathname specification and keyword arguments as per RESOLVE-LOCATION \(see
also \"Configuration DSL\"\) in the ASDF manual."
    (declare (ignorable more))
    (os-cond
     ((os-unix-p) (list (resolve-absolute-location `(,(parse-unix-namestring "/etc/") ,more))))))

  (defun filter-pathname-set (dirs)
    "Parse strings as unix namestrings and remove duplicates and non absolute-pathnames in a list."
    (remove-duplicates (remove-if-not #'absolute-pathname-p dirs) :from-end t :test 'equal))

  (defun xdg-data-pathnames (&rest more)
    "Return a list of absolute pathnames for application data directories.  With APP,
returns directory for data for that application, without APP, returns the set of directories
for storing all application configurations.
MORE may contain specifications for a subpath relative to these directories: a
subpathname specification and keyword arguments as per RESOLVE-LOCATION \(see
also \"Configuration DSL\"\) in the ASDF manual."
    (filter-pathname-set
     `(,(xdg-data-home more)
       ,@(xdg-data-dirs more))))

  (defun xdg-config-pathnames (&rest more)
    "Return a list of pathnames for application configuration.
MORE may contain specifications for a subpath relative to these directories: a
subpathname specification and keyword arguments as per RESOLVE-LOCATION \(see
also \"Configuration DSL\"\) in the ASDF manual."
    (filter-pathname-set
     `(,(xdg-config-home more)
       ,@(xdg-config-dirs more))))

  (defun find-preferred-file (files &key (direction :input))
    "Find first file in the list of FILES that exists (for direction :input or :probe)
or just the first one (for direction :output or :io).
    Note that when we say \"file\" here, the files in question may be directories."
    (find-if (ecase direction ((:probe :input) 'probe-file*) ((:output :io) 'identity)) files))

  (defun xdg-data-pathname (&optional more (direction :input))
    (find-preferred-file (xdg-data-pathnames more) :direction direction))

  (defun xdg-config-pathname (&optional more (direction :input))
    (find-preferred-file (xdg-config-pathnames more) :direction direction))

  (defun compute-user-cache ()
    "Compute (and return) the location of the default user-cache for translate-output
objects. Side-effects for cached file location computation."
    (setf *user-cache* (xdg-cache-home "common-lisp" :implementation)))
  (register-image-restore-hook 'compute-user-cache))
;;; -------------------------------------------------------------------------
;;; Hacks for backward-compatibility with older versions of UIOP

(uiop/package:define-package :uiop/backward-driver
  (:recycle :uiop/backward-driver :asdf/backward-driver :uiop)
  (:use :uiop/common-lisp :uiop/package :uiop/utility :uiop/version
   :uiop/pathname :uiop/stream :uiop/os :uiop/image
   :uiop/run-program :uiop/lisp-build :uiop/configuration)
  (:export
   #:coerce-pathname
   #:user-configuration-directories #:system-configuration-directories
   #:in-first-directory #:in-user-configuration-directory #:in-system-configuration-directory
   #:version-compatible-p))
(in-package :uiop/backward-driver)

(eval-when (:compile-toplevel :load-toplevel :execute)
(with-deprecation ((version-deprecation *uiop-version* :style-warning "3.2" :warning "3.4"))
  ;; Backward compatibility with ASDF 2.000 to 2.26

  ;; For backward-compatibility only, for people using internals
  ;; Reported users in quicklisp 2015-11: hu.dwim.asdf (removed in next release)
  ;; Will be removed after 2015-12.
  (defun coerce-pathname (name &key type defaults)
    "DEPRECATED. Please use UIOP:PARSE-UNIX-NAMESTRING instead."
    (parse-unix-namestring name :type type :defaults defaults))

  ;; Backward compatibility for ASDF 2.27 to 3.1.4
  (defun user-configuration-directories ()
    "Return the current user's list of user configuration directories
for configuring common-lisp.
DEPRECATED. Use UIOP:XDG-CONFIG-PATHNAMES instead."
    (xdg-config-pathnames "common-lisp"))
  (defun system-configuration-directories ()
    "Return the list of system configuration directories for common-lisp.
DEPRECATED. Use UIOP:CONFIG-SYSTEM-PATHNAMES instead."
    (system-config-pathnames "common-lisp"))
  (defun in-first-directory (dirs x &key (direction :input))
    "Finds the first appropriate file named X in the list of DIRS for I/O
in DIRECTION \(which may be :INPUT, :OUTPUT, :IO, or :PROBE).
If direction is :INPUT or :PROBE, will return the first extant file named
X in one of the DIRS.
If direction is :OUTPUT or :IO, will simply return the file named X in the
first element of DIRS that exists. DEPRECATED."
    (find-preferred-file
     (mapcar #'(lambda (dir) (subpathname (ensure-directory-pathname dir) x)) dirs)
     :direction direction))
  (defun in-user-configuration-directory (x &key (direction :input))
    "Return the file named X in the user configuration directory for common-lisp.
DEPRECATED."
    (xdg-config-pathname `("common-lisp" ,x) direction))
  (defun in-system-configuration-directory (x &key (direction :input))
    "Return the pathname for the file named X under the system configuration directory
for common-lisp. DEPRECATED."
    (find-preferred-file (system-config-pathnames "common-lisp" x) :direction direction))


  ;; Backward compatibility with ASDF 1 to ASDF 2.32

  (defun version-compatible-p (provided-version required-version)
    "Is the provided version a compatible substitution for the required-version?
If major versions differ, it's not compatible.
If they are equal, then any later version is compatible,
with later being determined by a lexicographical comparison of minor numbers.
DEPRECATED."
    (let ((x (parse-version provided-version nil))
          (y (parse-version required-version nil)))
      (and x y (= (car x) (car y)) (lexicographic<= '< (cdr y) (cdr x)))))))

;;;; ---------------------------------------------------------------------------
;;;; Re-export all the functionality in UIOP

(uiop/package:define-package :uiop/driver
  (:nicknames :uiop :asdf/driver) ;; asdf/driver is obsolete (uiop isn't);
  ;; but asdf/driver is still used by swap-bytes, static-vectors.
  (:use :uiop/common-lisp)
   ;; NB: not reexporting uiop/common-lisp
   ;; which include all of CL with compatibility modifications on select platforms,
   ;; that could cause potential conflicts for packages that would :use (cl uiop)
   ;; or :use (closer-common-lisp uiop), etc.
  (:use-reexport
   :uiop/package :uiop/utility :uiop/version
   :uiop/os :uiop/pathname :uiop/filesystem :uiop/stream :uiop/image
   :uiop/launch-program :uiop/run-program
   :uiop/lisp-build :uiop/configuration :uiop/backward-driver))

;; Provide both lowercase and uppercase, to satisfy more people.
(provide "uiop") (provide "UIOP")
(provide "UIOP")
(provide "uiop")
