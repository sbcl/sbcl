;;;; that part of DEFTYPE which runs within the compiler itself

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(/show0 "compiler-deftype.lisp 14")

(defun %compiler-deftype (name lambda-list expander doc source-location)
  (with-single-package-locked-error
      (:symbol name "defining ~A as a type specifier"))
  (ecase (info :type :kind name)
    (:primitive
     (when *type-system-initialized*
       (error "illegal to redefine standard type: ~S" name)))
    (:instance
     (warn "The class ~S is being redefined to be a DEFTYPE." name)
     (undeclare-structure (find-classoid name) t)
     ;; FIXME: shouldn't this happen only at eval-time?
     (setf (classoid-cell-classoid (find-classoid-cell name :create t)) nil)
     (setf (info :type :compiler-layout name) nil)
     (setf (info :type :kind name) :defined))
    (:defined
     ;; Note: It would be nice to warn here when a type is being
     ;; incompatibly redefined, but it's hard to tell, since type
     ;; expanders are often function objects which can't easily be
     ;; compared for equivalence. And just warning on redefinition
     ;; isn't good, since DEFTYPE necessarily does its thing once at
     ;; compile time and again at load time, so that it's very common
     ;; and normal for types to be defined twice. So since there
     ;; doesn't seem to be anything simple and obvious to do, and
     ;; since mistakenly redefining a type isn't a common error
     ;; anyway, we just don't worry about trying to warn about it.
     )
    ((nil :forthcoming-defclass-type)
     (setf (info :type :kind name) :defined)))
  (setf (info :type :expander name) expander
        (info :type :lambda-list name) lambda-list)
  (sb!c:with-source-location (source-location)
    (setf (info :type :source-location name) source-location))
  (when doc
    (setf (fdocumentation name 'type) doc))
  ;; ### Bootstrap hack -- we need to define types before %NOTE-TYPE-DEFINED
  ;; is defined. (FIXME: Do we still need to do this? -- WHN 19990310)
  (if (fboundp 'sb!c::%note-type-defined)
      (sb!c::%note-type-defined name)
      (warn "defining type before %NOTE-TYPE-DEFINED is defined"))
  name)

(/show0 "compiler-deftype.lisp end of file")
