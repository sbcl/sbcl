(in-package :sb-manual)

(defsection @pathnames (:title "Pathnames")
  (@lisp-pathnames section)
  (@native-filenames section))

(defsection @lisp-pathnames (:title "Lisp Pathnames")
  "There are many aspects of ANSI Common Lisp's pathname support
  which are implementation-defined and so need documentation."
  (@home-directory-specifiers section)
  (@the-sys-logical-pathname-host section))

;; FIXME: as a matter of ANSI conformance, we are required to document
;; implementation-defined stuff, which for pathnames (chapter 19 of CLtS)
;; includes:
;;
;; * Otherwise, the parsing of thing is implementation-defined.
;;   (PARSE-NAMESTRING)
;;
;; * If thing contains an explicit host name and no explicit device name,
;;   then it is implementation-defined whether parse-namestring will supply
;;   the standard default device for that host as the device component of
;;   the resulting pathname.  (PARSE-NAMESTRING)
;;
;; * The specific nature of the search is implementation-defined.
;;   (LOAD-LOGICAL-PATHNAME-TRANSLATIONS)
;;
;; * Any additional elements are implementation-defined.
;;   (LOGICAL-PATHNAME-TRANSLATIONS)
;;
;; * The matching rules are implementation-defined but should be consistent
;;   with directory.  (PATHNAME-MATCH-P)
;;
;; * Any such additional translations are implementation-defined.
;;   (TRANSLATE-LOGICAL-PATHNAMES)
;;
;; * ...or an implementation-defined portion of a component...
;;   (TRANSLATE-PATHNAME)
;;
;; * The portion of source that is copied into the resulting pathname is
;;   implementation-defined.  (TRANSLATE-PATHNAME)
;;
;; * During the copying of a portion of source into the resulting
;;   pathname, additional implementation-defined translations of case or
;;   file naming conventions might occur.  (TRANSLATE-PATHNAME)
;;
;; * In general, the syntax of namestrings involves the use of
;;   implementation-defined conventions.  (19.1.1)
;;
;; * The nature of the mapping between structure imposed by pathnames and
;;   the structure, if any, that is used by the underlying file system is
;;   implementation-defined.  (19.1.2)
;;
;; * The mapping of the pathname components into the concepts peculiar to
;;   each file system is implementation-defined.  (19.1.2)
;;
;; * Whether separator characters are permitted as part of a string in a
;;   pathname component is implementation-defined;  (19.2.2.1.1)
;;
;; * Whether a value of :unspecific is permitted for any component on any
;;   given file system accessible to the implementation is
;;   implementation-defined.  (19.2.2.2.3)
;;
;; * Other symbols and integers have implementation-defined meaning.
;;   (19.2.2.4.6)

(defsection @home-directory-specifiers (:title "Home Directory Specifiers")
  "SBCL accepts the keyword :HOME and a list of the form
  `(:HOME` `\"username\")` as a directory component immediately
  following :ABSOLUTE.

  :HOME is represented in namestrings by `~/` and `(:HOME`
  `\"username\")` by `~username/` at the start of the namestring.
  Tilde-characters elsewhere in namestrings represent themselves.

  Home directory specifiers are resolved to home directory of the
  current or specified user by SB-EXT:NATIVE-NAMESTRING, which is used
  by the implementation to translate pathnames before passing them on
  to operating system specific routines.

  Using `(:HOME` `\"user\")` form on Windows signals an error.")

(defsection @the-sys-logical-pathname-host
    (:title "The SYS Logical Pathname Host")
  ;; The existence and meaning of SYS: logical pathnames is
  ;; implementation-defined (CLHS 19.3.1.1.1).
  "The logical pathname host named by `\"SYS\"` exists in SBCL.
  Its LOGICAL-PATHNAME-TRANSLATIONS may be set by the site or the user
  applicable to point to the locations of the system's sources; in
  particular, the core system's source files match the logical
  pathname `\"SYS:SRC;**;*.*.*\"`, and the contributed modules' source
  files match `\"SYS:CONTRIB;**;*.*.*\"`."
  (sb-ext:set-sbcl-source-location function))

(defsection @native-filenames (:title "Native Filenames")
  "In some circumstances, what is wanted is a Lisp pathname object which
  corresponds to a string produced by the Operating System. In this
  case, some of the default parsing rules are inappropriate: most
  filesystems do not have a native understanding of wild pathnames;
  such functionality is often provided by shells above the OS, often
  in mutually-incompatible ways.

  To allow the user to deal with this, the following functions are
  provided: SB-EXT:PARSE-NATIVE-NAMESTRING and SB-EXT:NATIVE-PATHNAME
  return the closest equivalent Lisp pathname to a given string
  (appropriate for the Operating System), while
  SB-EXT:NATIVE-NAMESTRING converts a non-wild pathname designator to
  the equivalent native namestring, if possible. Some Lisp pathname
  concepts (such as the :BACK directory component) have no direct
  equivalents in most Operating Systems; the behaviour of
  SB-EXT:NATIVE-NAMESTRING is unspecified if an inappropriate pathname
  designator is passed to it. Additionally, note that conversion from
  pathname to native filename and back to pathname should not be
  expected to preserve equivalence under EQUAL."
  (sb-ext:parse-native-namestring function)
  (sb-ext:native-pathname function)
  (sb-ext:native-namestring function)
  "Because some file systems permit the names of directories to be
  expressed in multiple ways, it is occasionally necessary to parse a
  native file name as a directory name or to produce a native file
  name that names a directory as a file. For these cases,
  PARSE-NATIVE-NAMESTRING accepts the keyword argument
  :AS-DIRECTORY to force a filename to parse as a directory, and
  SB-EXT:NATIVE-NAMESTRING accepts the keyword argument :AS-FILE
  to force a pathname to unparse as a file. For example,

      ; On Unix, the directory \"/tmp/\" can be denoted by \"/tmp/\" or \"/tmp\".
      ; Under the default rules for native filenames, these parse and
      ; unparse differently.
      (defvar *p*)
      (setf *p* (parse-native-namestring \"/tmp/\")) => #P\"/tmp/\"
      (pathname-name *p*) => NIL
      (pathname-directory *p*) => (:ABSOLUTE \"tmp\")
      (native-namestring *p*) => \"/tmp/\"

      (setf *p* (parse-native-namestring \"/tmp\")) => #P\"/tmp\"
      (pathname-name *p*) => \"tmp\"
      (pathname-directory *p*) => (:ABSOLUTE)
      (native-namestring *p*) => \"/tmp\"

      ; A non-NIL AS-DIRECTORY argument to PARSE-NATIVE-NAMESTRING forces
      ; both the second string to parse the way the first does.
      (setf *p* (parse-native-namestring \"/tmp\"
                                         nil *default-pathname-defaults*
                                         :as-directory t)) => #P\"/tmp/\"
      (pathname-name *p*) => NIL
      (pathname-directory *p*) => (:ABSOLUTE \"tmp\")

      ; A non-NIL AS-FILE argument to NATIVE-NAMESTRING forces the pathname
      ; parsed from the first string to unparse as the second string.
      (setf *p* (parse-native-namestring \"/tmp/\")) => #P\"/tmp/\"
      (native-namestring *p* :as-file t) => \"/tmp\"")
