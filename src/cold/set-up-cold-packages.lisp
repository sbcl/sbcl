;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-COLD")

;;; an entry in the table which describes the non-standard part (i.e. not
;;; CL/CL-USER/KEYWORD) of the package structure of the SBCL system
;;;
;;; We make no attempt to be fully general; our table doesn't need to be
;;; able to express features which we don't happen to use.
(export '(package-data
          make-package-data
          package-data-name
          package-data-export
          package-data-reexport
          package-data-import-from
          package-data-use))
(defstruct package-data
  ;; a string designator for the package name
  (name (error "missing PACKAGE-DATA-NAME datum"))
  ;; a doc string
  (doc (error "missing PACKAGE-DOC datum"))
  ;; a tree containing names for exported symbols which'll be set up at package
  ;; creation time, and NILs, which are ignored. (This is a tree in order to
  ;; allow constructs like '("ENOSPC" #!+LINUX ("EDQUOT" "EISNAM" "ENAVAIL"
  ;; "EREMOTEIO")) to be used in initialization. NIL entries in the tree are
  ;; ignored for the same reason of notational convenience.)
  export
  ;; a list of string designators for exported symbols which don't necessarily
  ;; originate in this package (so their EXPORT operations should be handled
  ;; after USE operations have been done, so that duplicates aren't created)
  reexport
  ;; a list of sublists describing imports. Each sublist has the format as an
  ;; IMPORT-FROM list in DEFPACKAGE: the first element is the name of the
  ;; package to import from, and the remaining elements are the names of
  ;; symbols to import.
  import-from
  ;; a tree of string designators for package names of other packages
  ;; which this package uses
  use)

(let ((package-data-list (read-from-file "package-data-list.lisp-expr")))
  (labels ((flatten (tree)
             (mapcan (lambda (x) (if (listp x) (flatten x) (list x)))
                     tree)))

    ;; Build all packages that we need, and initialize them as far as we
    ;; can without referring to any other packages.
    (dolist (package-data package-data-list)
      (let* ((package (make-package
                       (package-data-name package-data)
                       ;; Note: As of 0.7.0, the only nicknames we use
                       ;; for our implementation packages are hacks
                       ;; not needed at cross-compile time (e.g. the
                       ;; deprecated SB-C-CALL nickname for SB-ALIEN).
                       ;; So support for nicknaming during xc is gone,
                       ;; since any nicknames are hacked in during
                       ;; cold init.
                       :nicknames nil
                       :use nil)))
        ;; Walk the tree of exported names, exporting each name.
        (dolist (string (flatten (package-data-export package-data)))
          (export (intern string package) package))))

    ;; Now that all packages exist, we can set up package-package
    ;; references.
    (dolist (package-data package-data-list)
      (use-package (package-data-use package-data)
                   (package-data-name package-data))
      (dolist (sublist (package-data-import-from package-data))
        (let* ((from-package (first sublist))
               (symbol-names (rest sublist))
               (symbols (mapcar (lambda (name)
                                  ;; old way, broke for importing symbols
                                  ;; like SB!C::DEBUG-SOURCE-FORM into
                                  ;; SB!DI -- WHN 19990714
                                  #+nil
                                  (let ((s (find-symbol name from-package)))
                                    (unless s
                                      (error "can't find ~S in ~S"
                                             name
                                             from-package))
                                    s)
                                  ;; new way, works for SB!DI stuff
                                  ;; -- WHN 19990714
                                  (intern name from-package))
                                (flatten symbol-names))))
          (import symbols (package-data-name package-data)))))

    ;; Now that all package-package references exist, we can handle
    ;; REEXPORT operations. (We have to wait until now because they
    ;; interact with USE operations.)  This code handles dependencies
    ;; properly, but is somewhat ugly.
    (let (done)
      (labels
          ((reexport (package-data)
             (let ((package (find-package (package-data-name package-data))))
               (cond
                 ((member package done))
                 ((null (package-data-reexport package-data))
                  (push package done))
                 (t
                  (mapcar #'reexport
                          (remove-if-not
                           (lambda (x)
                             (member x (package-data-use package-data)
                                     :test #'string=))
                           package-data-list
                           :key #'package-data-name))
                  (dolist (symbol-name
                           (flatten (package-data-reexport package-data)))
                    (multiple-value-bind (symbol status)
                        (find-symbol symbol-name package)
                      (unless status
                        (error "No symbol named ~S is accessible in ~S."
                               symbol-name package))
                      (when (eq (symbol-package symbol) package)
                        (error
                         "~S is not inherited/imported, but native to ~S."
                         symbol-name package))
                      (export symbol package)))
                  (push package done))))))
        (dolist (x package-data-list)
          (reexport x))
        (assert (= (length done) (length package-data-list)))))))

;; Each backend should have a different package for its instruction set
;; so that they can co-exist.
(make-assembler-package (backend-asm-package-name))

(defun package-list-for-genesis ()
  (append (sb-cold:read-from-file "package-data-list.lisp-expr")
          (let ((asm-package (backend-asm-package-name)))
            (list (make-package-data
                   :name asm-package
                   :use (mapcar 'package-name
                                (package-use-list asm-package))
                   :doc nil)))))
