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

(sb!xc:defstruct
            (logical-host
             (:copier nil)
             (:print-object
              (lambda (logical-host stream)
                (print-unreadable-object (logical-host stream :type t)
                  (prin1 (logical-host-name logical-host) stream))))
             (:include host
                       (parse #'parse-logical-namestring)
                       (parse-native
                        (lambda (&rest x)
                          (error "called PARSE-NATIVE-NAMESTRING using a ~
                                  logical host: ~S" (first x))))
                       (unparse #'unparse-logical-namestring)
                       (unparse-native
                        (lambda (&rest x)
                          (error "called NATIVE-NAMESTRING using a ~
                                  logical host: ~S" (first x))))
                       (unparse-host
                        (lambda (x)
                          (logical-host-name (%pathname-host x))))
                       (unparse-directory #'unparse-logical-directory)
                       (unparse-file #'unparse-logical-file)
                       (unparse-enough #'unparse-enough-namestring)
                       (unparse-directory-separator ";")
                       (simplify-namestring #'identity)
                       (customary-case :upper)))
  (name "" :type simple-string :read-only t)
  (translations nil :type list)
  (canon-transls nil :type list))

#-sb-xc-host
(defmethod make-load-form ((logical-host logical-host) &optional env)
  (declare (ignore env))
  (values `(find-logical-host ',(logical-host-name logical-host))
          nil))

;;; A PATTERN is a list of entries and wildcards used for pattern
;;; matches of translations.
(sb!xc:defstruct (pattern (:constructor make-pattern (pieces)) (:copier nil))
  (pieces nil :type list))

;;;; PATHNAME structures

;;; the various magic tokens that are allowed to appear in pretty much
;;; all pathname components
(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  (sb!xc:deftype pathname-component-tokens ()
    '(member nil :unspecific :wild :unc)))

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

;;; Logical pathnames have the following format:
;;;
;;; logical-namestring ::=
;;;      [host ":"] [";"] {directory ";"}* [name] ["." type ["." version]]
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
                                   (:copier nil)
                                   (:constructor %make-logical-pathname
                                    (host device directory name type version
                                     &aux (dir-hash (pathname-dir-hash directory))
                                          (stem-hash (mix (sxhash name) (sxhash type)))))))

;;; Utility functions

(declaim (inline pathname-component-present-p))
(defun pathname-component-present-p (component)
  (not (typep component '(or null (eql :unspecific)))))

;;; The following functions are used both for Unix and Windows: while
;;; we accept both \ and / as directory separators on Windows, we
;;; print our own always with /, which is much less confusing what
;;; with being \ needing to be escaped.
#-sb-xc-host ; %PATHNAME-DIRECTORY is target-only
(defun unparse-physical-directory (pathname escape-char)
  (declare (pathname pathname))
  (unparse-physical-directory-list (%pathname-directory pathname) escape-char))

#-sb-xc-host
(defun unparse-physical-directory-list (directory escape-char)
  (declare (list directory))
  (collect ((pieces))
    (when directory
      (ecase (pop directory)
       (:absolute
        (let ((next (pop directory)))
          (cond ((eq :home next)
                 (pieces "~"))
                ((and (consp next) (eq :home (car next)))
                 (pieces "~")
                 (pieces (second next)))
                ((and (plusp (length next)) (char= #\~ (char next 0)))
                 ;; The only place we need to escape the tilde.
                 (pieces "\\")
                 (pieces next))
                (next
                 (push next directory)))
          (pieces "/")))
        (:relative))
      (dolist (dir directory)
        (typecase dir
         ((member :up)
          (pieces "../"))
         ((member :back)
          (error ":BACK cannot be represented in namestrings."))
         ((member :wild-inferiors)
          (pieces "**/"))
         ((or simple-string pattern (member :wild))
          (pieces (unparse-physical-piece dir escape-char))
          (pieces "/"))
         (t
          (error "invalid directory component: ~S" dir)))))
    (apply #'concatenate 'simple-string (pieces))))

#-sb-xc-host
(defun unparse-physical-file (pathname escape-char)
  (declare (type pathname pathname))
  (let ((name (%pathname-name pathname))
        (type (%pathname-type pathname)))
    (collect ((fragments))
      ;; Note: by ANSI 19.3.1.1.5, we ignore the version slot when
      ;; translating logical pathnames to a filesystem without
      ;; versions (like Unix and Win32).
      (when name
        (when (and (null type)
                   (typep name 'string)
                   (> (length name) 0)
                   (position #\. name :start 1))
          (no-namestring-error
           pathname
           "there are too many dots in the ~S component ~S." :name name))
        (when (and (typep name 'string)
                   (string= name ""))
          (no-namestring-error
           pathname "the ~S component ~S is of length 0" :name name))
        (fragments (unparse-physical-piece name escape-char)))
      (when (pathname-component-present-p type)
        (unless name
          (no-namestring-error
           pathname
           "there is a ~S component but no ~S component" :type :name))
        (when (typep type 'simple-string)
          (when (position #\. type)
            (no-namestring-error
             pathname "the ~S component contains a ~S" :type #\.)))
        (fragments ".")
        (fragments (unparse-physical-piece type escape-char)))
      (apply #'concatenate 'simple-string (fragments)))))

#-sb-xc-host
(defun unparse-native-physical-file (pathname)
  (let ((name (pathname-name pathname))
        (type (pathname-type pathname)))
    (collect ((fragments))
      (cond
        ((pathname-component-present-p name)
         (unless (stringp name)         ; some kind of wild field
           (no-native-namestring-error
            pathname "of the ~S component ~S." :name name))
         (fragments name)
         (when (pathname-component-present-p type)
           (unless (stringp type)       ; some kind of wild field
             (no-native-namestring-error
              pathname "of the :~S component ~S" :type type))
           (fragments ".")
           (fragments type)))
        ((pathname-component-present-p type) ; type without a name
         (no-native-namestring-error
          pathname
          "there is a ~S component but no ~S component" :type :name)))
      (apply #'concatenate 'simple-string (fragments)))))
