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
(def!struct (host (:constructor nil))
  (parse (missing-arg) :type function)
  (unparse (missing-arg) :type function)
  (unparse-host (missing-arg) :type function)
  (unparse-directory (missing-arg) :type function)
  (unparse-file (missing-arg) :type function)
  (unparse-enough (missing-arg) :type function)
  (customary-case (missing-arg) :type (member :upper :lower)))

(def!method print-object ((host host) stream)
  (print-unreadable-object (host stream :type t :identity t)))

(def!struct (logical-host
	     (:make-load-form-fun make-logical-host-load-form-fun)
	     (:include host
		       (parse #'parse-logical-namestring)
		       (unparse #'unparse-logical-namestring)
		       (unparse-host
			(lambda (x)
			  (logical-host-name (%pathname-host x))))
		       (unparse-directory #'unparse-logical-directory)
		       (unparse-file #'unparse-unix-file)
		       (unparse-enough #'unparse-enough-namestring)
		       (customary-case :upper)))
  (name "" :type simple-base-string)
  (translations nil :type list)
  (canon-transls nil :type list))

(def!method print-object ((logical-host logical-host) stream)
  (print-unreadable-object (logical-host stream :type t)
    (prin1 (logical-host-name logical-host) stream)))

(defun make-logical-host-load-form-fun (logical-host)
  (values `(find-logical-host ',(logical-host-name logical-host))
	  nil))

;;; A PATTERN is a list of entries and wildcards used for pattern
;;; matches of translations.
(def!struct (pattern (:constructor make-pattern (pieces)))
  (pieces nil :type list))

;;;; PATHNAME structures

;;; the various magic tokens that are allowed to appear in pretty much
;;; all pathname components
(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  (def!type pathname-component-tokens ()
    '(member nil :unspecific :wild)))

(sb!xc:defstruct (pathname (:conc-name %pathname-)
			   (:constructor %make-pathname (host
							 device
							 directory
							 name
							 type
							 version))
			   (:predicate pathnamep))
  ;; the host (at present either a UNIX or logical host)
  (host nil :type (or host null))
  ;; the name of a logical or physical device holding files
  (device nil :type (or simple-string pathname-component-tokens))
  ;; a list of strings that are the component subdirectory components
  (directory nil :type list)
  ;; the filename
  (name nil :type (or simple-string pattern pathname-component-tokens))
  ;; the type extension of the file
  (type nil :type (or simple-string pattern pathname-component-tokens))
  ;; the version number of the file, a positive integer (not supported
  ;; on standard Unix filesystems)
  (version nil :type (or integer pathname-component-tokens (member :newest))))

;;; Logical pathnames have the following format:
;;;
;;; logical-namestring ::=
;;;	 [host ":"] [";"] {directory ";"}* [name] ["." type ["." version]]
;;;
;;; host ::= word
;;; directory ::= word | wildcard-word | **
;;; name ::= word | wildcard-word
;;; type ::= word | wildcard-word
;;; version ::= pos-int | newest | NEWEST | *
;;; word ::= {uppercase-letter | digit | -}+
;;; wildcard-word ::= [word] '* {word '*}* [word]
;;; pos-int ::= integer > 0
;;;
;;; Physical pathnames include all these slots and a device slot.

;;; Logical pathnames are a subclass of PATHNAME. Their class
;;; relations are mimicked using structures for efficiency.
(sb!xc:defstruct (logical-pathname (:conc-name %logical-pathname-)
				   (:include pathname)
				   (:constructor %make-logical-pathname
						 (host
						  device
						  directory
						  name
						  type
						  version))))
