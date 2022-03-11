;;;; the known-to-the-cross-compiler part of PATHNAME logic

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;;; data types used by pathnames

;;; The HOST structure holds the functions that both parse the
;;; pathname information into structure slot entries, and after
;;; translation the inverse (unparse) functions.
(defstruct (host (:constructor nil)
                 (:copier nil)
                 (:print-object
                  (lambda (host stream)
                    (print-unreadable-object (host stream :type t :identity t)))))
  (parse (missing-arg) :type function :read-only t)
  (parse-native (missing-arg) :type function :read-only t)
  (unparse (missing-arg) :type function :read-only t)
  (unparse-native (missing-arg) :type function :read-only t)
  (unparse-host (missing-arg) :type function :read-only t)
  (unparse-directory (missing-arg) :type function :read-only t)
  (unparse-file (missing-arg) :type function :read-only t)
  (unparse-enough (missing-arg) :type function :read-only t)
  (unparse-directory-separator (missing-arg) :type simple-string :read-only t)
  (simplify-namestring (missing-arg) :type function :read-only t)
  (customary-case (missing-arg) :type (member :upper :lower) :read-only t))

;;; A PATTERN is a list of entries and wildcards used for pattern
;;; matches of translations.
(defstruct (pattern (:constructor %make-pattern (hash pieces))
                    (:copier nil))
  (hash 0 :type fixnum :read-only t)
  (pieces nil :type list :read-only t))

;;;; PATHNAME structures

;;; the various magic tokens that are allowed to appear in pretty much
;;; all pathname components
(deftype pathname-component-tokens ()
  '(member nil :unspecific :wild :unc))

;;; This definition relies on compiler magic to turn the metclass
;;; into BUILT-IN-CLASSOID. Same for LOGICAL-PATHNAME.
(defstruct (pathname (:conc-name %pathname-)
                     (:copier nil)
                     (:constructor !allocate-pathname
                         (host device dir+hash name type version))
                     (:predicate pathnamep))
  (namestring nil) ; computed on demand
  ;; the host (at present either a UNIX or logical host)
  ;; Host and device could be reduced to small integers and packed in one slot
  ;; by keeping tables of the observed values.
  (host nil :type (or host null) :read-only t)
  ;; the name of a logical or physical device holding files
  (device nil :type (or simple-string pathname-component-tokens) :read-only t)
  ;; an interned list of strings headed by :ABSOLUTE or :RELATIVE
  ;; comprising the path, or NIL.
  ;; if the list is non-NIL, it's a cons of the list and a numeric hash.
  (dir+hash nil :type list :read-only t)
  ;; the filename
  (name nil :type (or simple-string pattern pathname-component-tokens) :read-only t)
  ;; the type extension of the file
  (type nil :type (or simple-string pattern pathname-component-tokens) :read-only t)
  ;; the version number of the file, a positive integer (not supported
  ;; on standard Unix filesystems)
  (version nil :type (or integer pathname-component-tokens (member :newest))
               :read-only t))

(let ((to (find-layout 'logical-pathname))
      (from (find-layout 'pathname)))
  (setf (wrapper-info to) (wrapper-info from)
        (wrapper-slot-table to) (wrapper-slot-table from)))
(declaim (inline logical-pathname-p))
(defun logical-pathname-p (x) (typep x 'logical-pathname))
