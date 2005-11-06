;;;; Source location tracking.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

(def!struct (definition-source-location
             (:make-load-form-fun sb!kernel:just-dump-it-normally))
  ;; Namestring of the source file that the definition was compiled from.
  ;; This is null if the definition was not compiled from a file.
  (namestring
   (when (and (boundp '*source-info*)
              *source-info*)
     (make-file-info-namestring *compile-file-pathname*
                                (source-info-file-info *source-info*)))
   :type (or string null))
  ;; Toplevel form index
  (toplevel-form-number
   (when (boundp '*current-path*)
     (source-path-tlf-number *current-path*))
   :type (or fixnum null))
  ;; plist from WITH-COMPILATION-UNIT
  (plist *source-plist*))

(defun make-file-info-namestring (name file-info)
  (let* ((untruename (file-info-untruename file-info))
         (dir (and untruename (pathname-directory untruename))))
    #+sb-xc-host
    (let ((src (position "src" dir :test #'string=
                         :from-end t)))
      (if src
          (format nil "SYS:~{~:@(~A~);~}~:@(~A~).LISP"
                  (subseq dir src) (pathname-name untruename))
          ;; FIXME: just output/stuff-groveled-from-headers.lisp
          (namestring untruename)))
    #-sb-xc-host
    (if (and dir (eq (first dir) :absolute))
        (namestring untruename)
        (if name
            (namestring name)
            nil))))

#!+sb-source-locations
(define-compiler-macro source-location (&environment env)
  #-sb-xc-host
  (unless (policy env (and (> space 1)
                           (> space debug)))
    (make-definition-source-location)))

(/show0 "/Processing source location thunks")
#!+sb-source-locations
(dolist (fun *source-location-thunks*)
  (/show0 ".")
  (funcall fun))
;; Unbind the symbol to ensure that we detect any attempts to add new
;; thunks after this.
(makunbound '*source-location-thunks*)
(/show0 "/Done with source location thunks")
