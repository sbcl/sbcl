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
             (:constructor %make-definition-source-location
                           (namestring toplevel-form-number))
             (:copier nil)
             (:make-load-form-fun just-dump-it-normally))
  ;; Namestring of the source file that the definition was compiled from.
  ;; This is null if the definition was not compiled from a file.
  (namestring nil :type (or string null) :read-only t)
  ;; Toplevel form index
  (toplevel-form-number nil :type (or fixnum null) :read-only t)
  ;; plist from WITH-COMPILATION-UNIT
  (plist *source-plist* :read-only t))

(defun make-definition-source-location ()
  (let* ((source-info (and (boundp '*source-info*) *source-info*))
         (namestring
          (or *source-namestring*
              (when source-info
                (make-file-info-namestring
                 *compile-file-pathname*
                 (get-toplevelish-file-info *source-info*)))))
         (tlf-num (acond ((boundp '*current-path*)
                          (source-path-tlf-number *current-path*))
                         ((and source-info (source-info-file-info source-info))
                          (1- (fill-pointer (file-info-forms it))))))
         (last (and source-info
                    (source-info-last-defn-source-loc source-info))))
    (if (and last
             (eql (definition-source-location-toplevel-form-number last) tlf-num)
             (string= (definition-source-location-namestring last) namestring)
             (equal (definition-source-location-plist last) *source-plist*))
        last
        (let ((new (%make-definition-source-location namestring tlf-num)))
          (when source-info
            (setf (source-info-last-defn-source-loc source-info) new))
          new))))

#+sb-xc-host
(defun lpnify-namestring (untruename dir type)
  (let ((src (position "src" dir :test #'string= :from-end t)))
    (cond
     ((and src (not (string= (car (last dir)) "output")))
      (format nil "SYS:~{~:@(~A~);~}~:@(~A~).~:@(~A~)"
              (subseq dir src) (pathname-name untruename) type))
     (t (aver (string-equal (car (last dir)) "output"))
        (aver (string-equal (pathname-name untruename) "stuff-groveled-from-headers"))
        (format nil "SYS:OUTPUT;STUFF-GROVELED-FROM-HEADERS.~:@(~A~)" type)))))

(defun make-file-info-namestring (name file-info)
  #+sb-xc-host (declare (ignore name))
  (let* ((untruename (file-info-untruename file-info))
         (dir (and untruename (pathname-directory untruename))))
    #+sb-xc-host
    (lpnify-namestring untruename dir (pathname-type untruename))
    #-sb-xc-host
    (if (and dir (eq (first dir) :absolute))
        (namestring untruename)
        (if name
            (namestring name)
            nil))))
