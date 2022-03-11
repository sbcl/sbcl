;;;; REQUIRE, PROVIDE, and friends
;;;;
;;;; Officially these are deprecated, but in practice they're probably
;;;; even less likely to actually go away than there is to ever be
;;;; another revision of the standard.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;;; exported specials

(defvar *modules* ()
  "This is a list of module names that have been loaded into Lisp so far.
   It is used by PROVIDE and REQUIRE.")

(defvar *module-provider-functions* (list 'module-provide-contrib)
  "See function documentation for REQUIRE.")

;;;; PROVIDE and REQUIRE

(defun provide (module-name)
  "Adds a new module name to *MODULES* indicating that it has been loaded.
   Module-name is a string designator"
  (pushnew (string module-name) *modules* :test #'string=)
  t)

(defvar *requiring* nil)

(defun require-error (control &rest arguments)
  (declare (optimize allow-non-returning-tail-call))
  (error 'extension-failure
         :format-control control
         :format-arguments arguments
         :references
         (list
          '(:sbcl :variable *module-provider-functions*)
          '(:sbcl :function require))))

(defun require (module-name &optional pathnames)
  "Loads a module, unless it already has been loaded. PATHNAMES, if supplied,
   is a designator for a list of pathnames to be loaded if the module
   needs to be. If PATHNAMES is not supplied, functions from the list
   *MODULE-PROVIDER-FUNCTIONS* are called in order with MODULE-NAME
   as an argument, until one of them returns non-NIL.  User code is
   responsible for calling PROVIDE to indicate a successful load of the
   module."
  (let ((name (string module-name)))
    (when (member name *requiring* :test #'string=)
      (require-error "~@<Could not ~S ~A: circularity detected. Please check ~
                     your configuration.~:@>" 'require module-name))
    (let ((saved-modules (copy-list *modules*))
          (*requiring* (cons name *requiring*)))
      (unless (member name *modules* :test #'string=)
        (cond (pathnames
               ;; ambiguity in standard: should we try all pathnames in the
               ;; list, or should we stop as soon as one of them calls PROVIDE?
               (dolist (ele (ensure-list pathnames) t)
                 (load ele)))
              (t
               (unless (some (lambda (p) (funcall p module-name))
                             *module-provider-functions*)
                 (require-error "Don't know how to ~S ~A."
                                'require module-name)))))
      (set-difference *modules* saved-modules))))


;;;; miscellany

(defun module-provide-contrib (name)
  "Stringify and downcase NAME, then attempt to load the file
   $SBCL_HOME/name/name"
  (let* ((filesys-name (string-downcase name))
         (unadorned-path
          (merge-pathnames
           (make-pathname :directory (list :relative "contrib")
                          :name filesys-name)
           ;; DO NOT CALL TRUENAME HERE. YOU SHOULD NOT NEED THAT.
           (or (sbcl-homedir-pathname)
               (return-from module-provide-contrib nil))))
         (fasl-path (merge-pathnames
                     (make-pathname :type *fasl-file-type*)
                     unadorned-path))
         (lisp-path (merge-pathnames (make-pathname :type "lisp")
                                     unadorned-path)))
    ;; KLUDGE: there's a race condition here; the file we probe could
    ;; be removed by the time we get round to trying to load it.
    ;; Maybe factor out the logic in the LOAD guesser as to which file
    ;; was meant, so that we can use it here on open streams instead?
    ;;
    ;; Prefer the untruename as the argument to give to LOAD,
    ;; because users need to see (in backtraces and similar) the pathnames
    ;; actually handed off to the filesystem calls, like:
    ;;    ... #P"blaze-out/k8-fastbuild/bin/third_party/lisp/sbcl/binary-distribution/k8/sbcl/bin/../lib/sbcl/contrib/sb-md5.fasl"
    ;; and not names that have been obtained through readlink like
    ;;    ... #P"/build/cas/081/081f9f05e4af917b63b6ccf7453e4565fc66587a093a45fb1c6dbe64e8c8ad58_0100b65b"
    ;; as the latter can not be reverse-engineered to a source file
    ;; except possibly by much trial and error.
    ;;
    ;; If the idea of using PROBE-FILE was to get the newest version
    ;; on a versioned file system, I don't see how it helps to truenameize early,
    ;; because as per the "possible race" cited above, there may be an even newer version
    ;; when you call LOAD, or it may go away. Aside from ensuring existence,
    ;; was there any benefit to using the result of PROBE-FILE?
    (let ((file (cond ((probe-file fasl-path) fasl-path)
                      ((probe-file unadorned-path) unadorned-path)
                      ((probe-file lisp-path) lisp-path))))
      (when file
        (handler-bind
            (((or style-warning package-at-variance) #'muffle-warning))
          (load file))
        t))))
