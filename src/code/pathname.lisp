;;;; the known-to-the-cross-compiler part of PATHNAME logic

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;;; data types used by pathnames

;;; The HOST structure holds the functions that both parse the
;;; pathname information into structure slot entries, and after
;;; translation the inverse (unparse) functions.
(sb!xc:defstruct (host (:constructor nil)
                       (:copier nil)
                       (:print-object
                        (lambda (host stream)
                          (print-unreadable-object
                           (host stream :type t :identity t)))))
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
(sb!xc:defstruct (pattern (:constructor make-pattern (pieces)) (:copier nil))
  (pieces nil :type list))

;;;; PATHNAME structures

;;; the various magic tokens that are allowed to appear in pretty much
;;; all pathname components
(sb!xc:deftype pathname-component-tokens ()
  '(member nil :unspecific :wild :unc))

(sb!xc:defstruct (pathname (:conc-name %pathname-)
                           (:copier nil)
                           (:constructor %%make-pathname
                               (host device directory name type version
                                &aux (dir-hash (pathname-dir-hash directory))
                                     (stem-hash (mix (sxhash name) (sxhash type)))))
                           (:predicate pathnamep))
  (namestring nil) ; computed on demand
  ;; support for pathname interning and hashing.
  (dir-hash nil :type fixnum :read-only t)
  (stem-hash nil :type fixnum :read-only t) ; name hash and type hash mixed
  ;; the host (at present either a UNIX or logical host)
  (host nil :type (or host null) :read-only t)
  ;; the name of a logical or physical device holding files
  (device nil :type (or simple-string pathname-component-tokens) :read-only t)
  ;; a list of strings that are the component subdirectory components
  (directory nil :type list :read-only t)
  ;; the filename
  (name nil :type (or simple-string pattern pathname-component-tokens) :read-only t)
  ;; the type extension of the file
  (type nil :type (or simple-string pattern pathname-component-tokens) :read-only t)
  ;; the version number of the file, a positive integer (not supported
  ;; on standard Unix filesystems)
  (version nil :type (or integer pathname-component-tokens (member :newest))
               :read-only t))

