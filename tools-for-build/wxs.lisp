;;;; Generate WiX XML Source, from which we eventually generate the .MSI

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;;; XML generation

(defvar *indent-level* 0)

(defvar *sbcl-source-root*
  (truename
   (merge-pathnames (make-pathname :directory (list :relative :up))
                    (make-pathname :name nil :type nil :defaults *load-truename*))))

(defun print-xml (sexp &optional (stream *standard-output*))
  (destructuring-bind (tag &optional attributes &body children) sexp
    (when attributes (assert (evenp (length attributes))))
    (format stream "~VT<~A~{ ~A='~A'~}~@[/~]>~%"
            *indent-level* tag attributes (not children))
      (let ((*indent-level* (+ *indent-level* 3)))
        (dolist (child children)
          (unless (listp child)
            (error "Malformed child: ~S in ~S" child children))
          (print-xml child stream)))
      (when children
        (format stream "~VT</~A>~%" *indent-level* tag))))

(defun xml-1.0 (pathname sexp)
  (with-open-file (xml pathname :direction :output :if-exists :supersede
                       :external-format :ascii)
     (format xml "<?xml version='1.0'?>")
     (print-xml sexp xml)))

(defun application-name ()
  (format nil "SBCL ~A" (lisp-implementation-version)))

;;;; GUID generation
;;;;
;;;; Apparently this willy-nilly regeneration of GUIDs is a bad thing, and
;;;; we should probably have a single GUID per release / Component, so
;;;; that no matter by whom the .MSI is built the GUIDs are the same.
;;;;
;;;; Something to twiddle on a rainy day, I think.

(load-shared-object "OLE32.DLL")

(define-alien-type uuid
    (struct uuid
            (data1 unsigned-long)
            (data2 unsigned-short)
            (data3 unsigned-short)
            (data4 (array unsigned-char 8))))

(define-alien-routine ("CoCreateGuid" co-create-guid) int (guid (* uuid)))

(defun uuid-string (uuid)
  (declare (type (alien (* uuid)) uuid))
  (let ((data4 (slot uuid 'data4)))
    (format nil "~8,'0X-~4,'0X-~4,'0X-~2,'0X~2,'0X-~{~2,'0X~}"
            (slot uuid 'data1)
            (slot uuid 'data2)
            (slot uuid 'data3)
            (deref data4 0)
            (deref data4 1)
            (loop for i from 2 upto 7 collect (deref data4 i)))))

(defun make-guid ()
  (let (guid)
    (unwind-protect
         (progn
           (setf guid (make-alien (struct uuid)))
           (co-create-guid guid)
           (uuid-string guid))
      (free-alien guid))))

(defun list-all-contribs ()
  (loop for flag in (directory "../contrib/*/test-passed")
        collect (car (last (pathname-directory flag)))))

(defvar *id-char-substitutions* '((#\\ . #\_)
                                  (#\/ . #\_)
                                  (#\: . #\.)
                                  (#\- . #\.)))

(defun id (string)
  ;; Mangle a string till it can be used as an Id. A-Z, a-z, 0-9, and
  ;; _ are ok, nothing else is.
  (map 'string (lambda (c)
                 (or (cdr (assoc c *id-char-substitutions*))
                     c))
       string))

(defun directory-id (name)
  (id (format nil "Directory_~A" (enough-namestring name *sbcl-source-root*))))

(defun directory-names (pathname)
  (let ((name (car (last (pathname-directory pathname)))))
    (if (< 8 (length name))
        (list "Name" (subseq name 0 8)
              "LongName" name)
        (list "Name" name))))

(defun file-id (pathname)
  (id (format nil "File_~A" (enough-namestring pathname *sbcl-source-root*))))

(defparameter *ignored-directories* '("CVS" ".svn"))

(defparameter *pathname-type-abbrevs*
  '(("lisp" . "lsp")
    ("fasl" . "fas")
    ("SBCL" . "txt") ; README.SBCL -> README.txt
    ("texinfo" . "tfo")
    ("lisp-temp" . "lmp")
    ("html" . "htm")))

(defun file-names (pathname)
  (if (or (< 8 (length (pathname-name pathname)))
          (< 3 (length (pathname-type pathname))))
      (let ((short-name (let ((name (pathname-name pathname)))
                          (if (< 8 (length name))
                              (subseq name 0 8)
                              name)))
            (short-type (let ((type (pathname-type pathname)))
                          (if (< 3 (length type))
                              (or (cdr (assoc type *pathname-type-abbrevs* :test #'equalp))
                                  (error "No abbreviation for type: ~A" type))
                              type))))
        (list "Name" (if short-type
                         (format nil "~A.~A" short-name short-type)
                         short-name)
              "LongName" (file-namestring pathname)))
      (list "Name" (file-namestring pathname))))

(defparameter *components* nil)

(defun component-id (pathname)
  (let ((id (id (format nil "Contrib_~A" (enough-namestring pathname *sbcl-source-root*)))))
    (push id *components*)
    id))

(defun ref-all-components ()
  (prog1
      (mapcar (lambda (id)
                `("ComponentRef" ("Id" ,id)))
              *components*)
    (setf *components* nil)))

(defun collect-1-component (root)
  `("Directory" ("Id" ,(directory-id root)
                 ,@(directory-names root))
    ("Component" ("Id" ,(component-id root)
                  "Guid" ,(make-guid)
                  "DiskId" 1)
     ,@(loop for file in (directory
                          (make-pathname :name :wild :type :wild :defaults root))
             when (or (pathname-name file) (pathname-type file))
             collect `("File" ("Id" ,(file-id file)
                               ,@(file-names file)
                               "Source" ,(enough-namestring file)))))))

(defun collect-components (root)
  (cons (collect-1-component root)
        (loop for directory in
              (directory
               (merge-pathnames (make-pathname
                                 :directory '(:relative :wild)
                                 :name nil :type nil)
                                root))
              unless (member (car (last (pathname-directory directory)))
                             *ignored-directories* :test #'equal)
              append (collect-components directory))))

(defun collect-contrib-components ()
  (loop for contrib in (directory "../contrib/*/test-passed")
        append (collect-components (make-pathname :name nil
                                                  :type nil
                                                  :version nil
                                                  :defaults contrib))))

(defun make-extension (type mime)
  `("Extension" ("Id" ,type "ContentType" ,mime)
    ("Verb" ("Id" ,(format nil "load_~A" type)
             "Argument" "--core \"[#sbcl.core]\" --load \"%1\""
             "Command" "Load with SBCL"
             "Target" "[#sbcl.exe]"))))

(defun write-wxs (pathname)
  ;; both :INVERT and :PRESERVE could be used here, but this seemed
  ;; better at the time
  (xml-1.0
   pathname
   `("Wix" ("xmlns" "http://schemas.microsoft.com/wix/2003/01/wi")
     ("Product" ("Id" "????????-????-????-????-????????????"
                 "Name" ,(application-name)
                 "Version" ,(lisp-implementation-version)
                 "Manufacturer" "http://www.sbcl.org"
                 "Language" 1033)
      ("Package" ("Id" "????????-????-????-????-????????????"
                  "Manufacturer" "http://www.sbcl.org"
                  "InstallerVersion" 200
                  "Compressed" "yes"))
      ("Media" ("Id" 1
                "Cabinet" "sbcl.cab"
                "EmbedCab" "yes"))
      ("Directory" ("Id" "TARGETDIR"
                    "Name" "SourceDir")
       ("Directory" ("Id" "ProgramMenuFolder"
                     "Name" "PMFolder"))
       ("Directory" ("Id" "ProgramFilesFolder"
                     "Name" "PFiles")
        ("Directory" ("Id" "BaseFolder"
                      "Name" "SBCL"
                      "LongName" "Steel Bank Common Lisp")
         ("Directory" ("Id" "VersionFolder"
                       "Name" ,(lisp-implementation-version))
          ("Directory" ("Id" "INSTALLDIR")
           ("Component" ("Id" "SBCL_Base"
                         "Guid" ,(make-guid)
                         "DiskId" 1)
            ("Environment" ("Id" "Env_SBCL_HOME"
                            "Action" "set"
                            "Name" "SBCL_HOME"
                            "Part" "all"
                            "Value" "[INSTALLDIR]"))
            ("Environment" ("Id" "Env_PATH"
                            "Action" "set"
                            "Name" "PATH"
                            "Part" "first"
                            "Value" "[INSTALLDIR]"))
            ;; If we want to associate files with SBCL, this
            ;; is how it's done -- but doing this by default
            ;; and without asking the user for permission Is
            ;; Bad. Before this is enabled we need to figure out
            ;; how to make WiX ask for permission for this...
            ;; ,(make-extension "fasl" "application/x-lisp-fasl")
            ;; ,(make-extension "lisp" "text/x-lisp-source")
            ("File" ("Id" "sbcl.exe"
                     "Name" "sbcl.exe"
                     "Source" "../src/runtime/sbcl.exe")
             ("Shortcut" ("Id" "sbcl.lnk"
                          "Directory" "ProgramMenuFolder"
                          "Name" "SBCL"
                          "LongName" ,(application-name)
                          "Arguments" "--core \"[#sbcl.core]\"")))
            ("File" ("Id" "sbcl.core"
                     "Name" "sbcl.cre"
                     "LongName" "sbcl.core"
                     "Source" "sbcl.core")))
           ,@(collect-contrib-components))))))
      ("Feature" ("Id" "Minimal"
                  "ConfigurableDirectory" "INSTALLDIR"
                  "Level" 1)
       ("ComponentRef" ("Id" "SBCL_Base"))
       ,@(ref-all-components))
      ("Property" ("Id" "WIXUI_INSTALLDIR" "Value" "INSTALLDIR"))
      ("UIRef" ("Id" "WixUI_InstallDir"))))))
