(in-package :sb-manual)

;;;; Determining the package for parsing docstrings
;;;;
;;;; The package for parsing is the package that was in effect when
;;;; the docstring of the definition was read. This is basically the
;;;; nearest IN-PACKAGE above the definition in the sources.
;;;;
;;;; With these semantics, when editing a docstring in Slime, if M-.
;;;; works on an uppercase symbol name, then you can expect it to be
;;;; codified by MARKDOWN-TO-TEXINFO. For symbols without a
;;;; definition, you can use TAB-completion to check, but it's better
;;;; to actually load PAX and check (see
;;;; PAX::@BROWSING-LIVE-DOCUMENTATION).

;;; To avoid conflicting with PAX's heuristics, DOCSTRING-PACKAGE
;;; always returns a non-NIL package. We use a reimplementation of
;;; DREF-EXT:DEFINITION-PROPERTY for DREF-EXT:DOCSTRING*, which we
;;; feed to DOCSTRING-PACKAGE-OVERRIDES-TO-PAX in USE-PAX.

;;; These map the SYMBOL-PACKAGE of a definition's XREF-NAME to the
;;; docstring package.
(defparameter *package-to-docstring-package*
  '(("COMMON-LISP" "SB-IMPL")
    ("SB-ACLREPL" "SB-ACLREPL")
    ("SB-ALIEN" "SB-ALIEN")
    ("SB-BSD-SOCKETS" "SB-BSD-SOCKETS")
    ("SB-CONCURRENCY" "SB-CONCURRENCY")
    ("SB-COVER" "SB-COVER")
    ("SB-DEBUG" "SB-DEBUG")
    ("SB-EXT" "SB-IMPL")
    ("SB-GRAY" "SB-GRAY")
    ("SB-GROVEL" "SB-GROVEL")
    ("SB-INTROSPECT" "SB-INTROSPECT")
    ("SB-MANUAL" "SB-MANUAL")
    ("SB-MD5" "SB-MD5")
    ("SB-POSIX" "SB-POSIX")
    ("SB-SEQUENCE" "SB-IMPL")
    ("SB-PROFILE" "SB-PROFILE")
    ("SB-ROTATE-BYTE" "SB-ROTATE-BYTE")
    ("SB-UNICODE" "SB-UNICODE")
    ("SB-SPROF" "SB-SPROF")
    ("SB-SYS" "SB-IMPL")
    ("SB-THREAD" "SB-THREAD")))

;;; The package-wide docstring packages are almost correct, but there
;;; are a couple of definitions in random files.
(defparameter *definition-to-docstring-package*
  '(((with-compilation-unit macro) "SB-C")
    ((sb-ext:restrict-compiler-policy function) "SB-C")
    ((trace macro) "SB-DEBUG")))

;;; For when this file is recompiled in interactive development after
;;; a later, explicit call to USE-PAX
(eval-when (:load-toplevel :execute)
  (when *using-pax*
    (convert-docstring-package-overrides-to-pax)))

(defun docstring-package (xref)
  (let* ((name (xref-name xref))
         (key (list name (xref-locative xref))))
    (or (find-package
         (or (second (find key *definition-to-docstring-package*
                           :key #'first :test #'equal))
             (when (symbolp name)
               (second (find (package-name (symbol-package name))
                             *package-to-docstring-package*
                             :key #'first :test #'equal)))))
        (assert nil () "Cannot determine package of the docstring of ~S."
                xref))))
