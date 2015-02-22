;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!FORMAT")

(defparameter *format-whitespace-chars*
  (vector #\space
          #\newline
          ;; We leave out this non-STANDARD-CHARACTER entry from this table
          ;; when we're running in the cross-compilation host, since ANSI
          ;; doesn't require the cross-compilation host to know what a tab is.
          #-sb-xc-host (code-char tab-char-code)))

(defvar *format-directive-expanders*
  (make-array base-char-code-limit :initial-element nil))
(defvar *format-directive-interpreters*
  (make-array base-char-code-limit :initial-element nil))

(defvar *default-format-error-control-string* nil)
(defvar *default-format-error-offset* nil)

;;;; specials used to communicate information

;;; Used both by the expansion stuff and the interpreter stuff. When it is
;;; non-NIL, up-up-and-out (~:^) is allowed. Otherwise, ~:^ isn't allowed.
(defvar *up-up-and-out-allowed* nil)

;;; Used by the interpreter stuff. When it's non-NIL, it's a function
;;; that will invoke PPRINT-POP in the right lexical environemnt.
(declaim (type (or null function) *logical-block-popper*))
(defvar *logical-block-popper* nil)

;;; Used by the expander stuff. This is bindable so that ~<...~:>
;;; can change it.
(defvar *expander-next-arg-macro* 'expander-next-arg)

;;; Used by the expander stuff. Initially starts as T, and gets set to NIL
;;; if someone needs to do something strange with the arg list (like use
;;; the rest, or something).
(defvar *only-simple-args*)

;;; Used by the expander stuff. We do an initial pass with this as NIL.
;;; If someone doesn't like this, they (THROW 'NEED-ORIG-ARGS NIL) and we try
;;; again with it bound to T. If this is T, we don't try to do anything
;;; fancy with args.
(defvar *orig-args-available* nil)

;;; Used by the expander stuff. List of (symbol . offset) for simple args.
(defvar *simple-args*)
