;;;; the known-to-the-cross-compiler part of PATHNAME logic

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMUCL system, which was
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
(defstruct (pattern (:constructor %make-pattern (pieces))
                    (:copier nil))
  (pieces nil :type list :read-only t))

;;;; PATHNAME structures

;;; This definition relies on compiler magic to turn the metclass
;;; into BUILT-IN-CLASSOID. Same for LOGICAL-PATHNAME.
(defstruct (pathname (:conc-name %pathname-)
                     (:copier nil)
                     (:constructor !allocate-pathname
                         (host-or-device dir+hash name type version sxhash))
                     (:predicate pathnamep))
  (namestring nil) ; computed on demand
  ;; Either the host or the device can be stored, no both because:
  ;; - logical pathnames always have :UNSPECIFIC as the device
  ;; - physical pathnames always have *PHYSICAL-HOST* as the host
  (host-or-device nil :type (or %pathname-host %pathname-device) :read-only t)
  ;; an interned list of strings headed by :ABSOLUTE or :RELATIVE
  ;; comprising the path, or NIL.
  ;; if the list is non-NIL, it's a cons of the list and a numeric hash.
  (dir+hash nil :type list :read-only t)
  ;; the filename
  (name nil :type %pathname-name :read-only t)
  ;; the type extension of the file
  (type nil :type %pathname-name :read-only t)
  ;; the version number of the file, a positive integer (not supported
  ;; on standard Unix filesystems)
  (version nil :type %pathname-version :read-only t)
  ;; SXHASH must be last because INSTANCE-SXHASH reads the slot whose index
  ;; is %INSTANCE-LENGTH. The stored length will get decresed by 1 after
  ;; allocation to make the access come out right.
  (sxhash 0))

(let ((to (find-layout 'logical-pathname))
      (from (find-layout 'pathname)))
  (setf (layout-info to) (layout-info from)
        (layout-slot-table to) (layout-slot-table from)))
(declaim (inline logical-pathname-p))
(defun logical-pathname-p (x) (typep x 'logical-pathname))

(declaim (inline %pathname-host %pathname-device %pathname-directory))

(define-load-time-global *physical-host* nil) ; initialized in {unix|win32}-pathname.lisp
(defun %pathname-host (pathname)
  ;; Using layout-depthoid to test for logical-pathname avoids needing to
  ;; compare to #<SB-KERNEL:LAYOUT LOGICAL-PATHNAME>
  (if (> (layout-depthoid (%instance-layout pathname)) sb-kernel::pathname-layout-depthoid)
      (%pathname-host-or-device pathname)
      *physical-host*))

(defun %pathname-device (pathname)
  (if (> (layout-depthoid (%instance-layout pathname)) sb-kernel::pathname-layout-depthoid)
      :unspecific
      (%pathname-host-or-device pathname)))

(defun %pathname-directory (pathname) (truly-the list (car (%pathname-dir+hash pathname))))
