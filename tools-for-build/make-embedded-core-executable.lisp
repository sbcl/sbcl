;;;; Wrap a bare SBCL core as an executable that carries the core in
;;;; a proper Mach-O or PE section, so the result is code-signable.
;;;;
;;;; Usage:
;;;;   sbcl --script make-embedded-core-executable.lisp -- \
;;;;     --core <core> --output <executable> \
;;;;     [--sbcl-home <dir>] \
;;;;     [--application-type console|gui]    ; Windows only
;;;;
;;;; SBCL_HOME is resolved from --sbcl-home, or the script's own
;;;; directory if sbcl.mk sits next to it, or sb-int:sbcl-homedir-pathname.

(in-package "CL-USER")

(defvar *sbcl-home*)

(defun fail (control &rest arguments)
  (error "~?" control arguments))

(defun sbcl.mk-path ()
  (let ((path (merge-pathnames "sbcl.mk" *sbcl-home*)))
    (or (probe-file path)
        (fail "Embedded-core executable helper requires ~A."
              (sb-ext:native-namestring path :as-file t)))))

(defun read-runtime-variable (name)
  (with-open-file (stream (sbcl.mk-path) :external-format :ascii)
    (loop with prefix = (concatenate 'string name "=")
          for line = (read-line stream nil)
          while line
          when (and (<= (length prefix) (length line))
                    (string= prefix line :end2 (length prefix)))
            return (subseq line (length prefix)))))

(defun required-runtime-variable (name)
  (let ((v (read-runtime-variable name)))
    (if (and v (not (string= v "")))
        v
        (fail "Embedded-core executable helper requires a ~A entry in ~A.~@
Rebuild SBCL with :SB-LINKABLE-RUNTIME and reinstall it."
              name (sb-ext:native-namestring (sbcl.mk-path) :as-file t)))))

(defun split-words (string)
  (when (and string (plusp (length string)))
    (flet ((spacep (c) (or (char= c #\Space) (char= c #\Tab))))
      (loop with len = (length string)
            for start = (position-if-not #'spacep string)
              then (position-if-not #'spacep string :start end)
            while start
            for end = (or (position-if #'spacep string :start start) len)
            collect (subseq string start end)))))

(defun absolute-runtime-file (name)
  (sb-ext:native-namestring
   (sb-ext:parse-native-namestring name nil *sbcl-home*)
   :as-file t))

(defun absolutize-libsbcl-arg (arg libsbcl)
  (cond ((member arg libsbcl :test #'string=)
         (absolute-runtime-file arg))
        ((and (> (length arg) 4) (string= arg "-Wl," :end1 4)
              (member (subseq arg 4) libsbcl :test #'string=))
         (format nil "-Wl,~A" (absolute-runtime-file (subseq arg 4))))
        (t arg)))

(defun run (command)
  (let* ((process (sb-ext:run-program (car command) (cdr command)
                                      :search t :input nil
                                      :output *standard-output*
                                      :error *error-output* :wait t))
         (code (sb-ext:process-exit-code process)))
    (unless (and (eq :exited (sb-ext:process-status process)) (zerop code))
      (fail "~A exited with status ~S while building embedded-core executable."
            (car command) code))))

(defun cc-link (extra-args)
  (let* ((cc (split-words (required-runtime-variable "CC")))
         (linkflags (split-words (read-runtime-variable "LINKFLAGS")))
         (libs (split-words (read-runtime-variable "LIBS")))
         (libsbcl (split-words (required-runtime-variable "LIBSBCL")))
         (use-libsbcl (mapcar (lambda (a) (absolutize-libsbcl-arg a libsbcl))
                              (split-words (required-runtime-variable "USE_LIBSBCL")))))
    (dolist (file libsbcl)
      (unless (probe-file (absolute-runtime-file file))
        (fail "Embedded-core executable helper requires installed runtime asset ~A."
              file)))
    (run (append cc linkflags extra-args use-libsbcl libs))))

#+win32
(defun random-tmp-name (type)
  (sb-ext:native-namestring
   (merge-pathnames
    (make-pathname :name (format nil "sbcl-embedded-core-~36,8,'0R"
                                 (random (expt 36 8)))
                   :type type)
    (sb-ext:parse-native-namestring
     (or (sb-ext:posix-getenv "TEMP") "C:/Windows/Temp") nil nil
     :as-directory t))
   :as-file t))

#+darwin
(defun build-embedded-core-executable (core output app-type)
  (declare (ignore app-type))
  (cc-link (list "-o" output
                 (format nil "-Wl,-sectcreate,__SBCL,__core,~A" core))))

#+win32
(defun build-embedded-core-executable (core output app-type)
  (let ((asm (random-tmp-name "s"))
        (obj (random-tmp-name "obj"))
        (subsystem (ecase app-type
                     (:console '("-Wl,--subsystem,console"))
                     (:gui '("-Wl,--subsystem,windows")))))
    (unwind-protect
         (progn
           (with-open-file (s asm :direction :output :external-format :ascii
                                  :if-exists :supersede :if-does-not-exist :create)
             (format s ".section .sbclcr,\"dr\"~%.incbin ~S~%"
                     (substitute #\/ #\\ core)))
           (run (append (split-words (required-runtime-variable "CC"))
                        (list "-c" "-o" obj asm)))
           (cc-link (append subsystem (list "-o" output obj))))
      (ignore-errors (delete-file asm))
      (ignore-errors (delete-file obj)))))

#-(or darwin win32)
(defun build-embedded-core-executable (core output app-type)
  (declare (ignore core output app-type))
  (fail "Embedded-core executable output is supported only on Darwin and Windows."))

(defun parse-application-type (v)
  (cond ((string-equal v "console") :console)
        ((string-equal v "gui") :gui)
        (t (fail "Unknown application type ~S (use console or gui)." v))))

(defun parse-arguments (args)
  (when (and args (string= (car args) "--")) (pop args))
  (let (core output sbcl-home (app-type :console))
    (declare (ignorable app-type))
    (flet ((next (name)
             (or (pop args) (fail "Missing value for ~A." name))))
      (loop while args do
        (let ((a (pop args)))
          (cond ((member a '("-h" "--help") :test #'string=)
                 (format t "usage: sbcl --script make-embedded-core-executable.lisp -- ~
--core <core> --output <output>~@
~8@T[--sbcl-home <dir>]~@
~8@T[--application-type console|gui]  (Windows only)~%")
                 (sb-ext:exit :code 0))
                ((string= a "--core") (setf core (next "--core")))
                ((string= a "--output") (setf output (next "--output")))
                ((string= a "--sbcl-home") (setf sbcl-home (next "--sbcl-home")))
                #+win32
                ((string= a "--application-type")
                 (setf app-type (parse-application-type (next "--application-type"))))
                (t (fail "Unknown option ~S." a))))))
    (values (or core (fail "Missing required --core option."))
            (or output (fail "Missing required --output option."))
            sbcl-home app-type)))

(defun resolve-sbcl-home (override)
  (or (and override (sb-ext:parse-native-namestring
                     override nil *default-pathname-defaults*
                     :as-directory t))
      (and *load-pathname*
           (let ((dir (make-pathname :name nil :type nil
                                     :defaults *load-pathname*)))
             (when (probe-file (merge-pathnames "sbcl.mk" dir)) dir)))
      (sb-int:sbcl-homedir-pathname)
      (fail "Cannot determine SBCL_HOME; pass --sbcl-home.")))

(defun main (&optional (args (cdr sb-ext:*posix-argv*)))
  (multiple-value-bind (core-arg output-arg sbcl-home-arg app-type)
      (parse-arguments args)
    (let* ((*sbcl-home* (resolve-sbcl-home sbcl-home-arg))
           (core (sb-ext:native-namestring
                  (truename (sb-ext:parse-native-namestring core-arg))
                  :as-file t))
           (output (sb-ext:native-namestring
                    (sb-ext:parse-native-namestring output-arg) :as-file t)))
      (build-embedded-core-executable core output app-type))))

(eval-when (:execute)
  (main))
