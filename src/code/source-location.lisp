;;;; Source location tracking.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;; Some uses of source locations want absolute filenames that were
;;; (implicitly) involved at compile- or load-time, some want relative
;;; filenames, and some want absolute filenames derived from runtime
;;; state. In order to keep the DEFINITION-SOURCE-LOCATION structure
;;; small, we separate out the filename pieces to its own record.
(def!struct (definition-source-location-filenames
                (:constructor %make-definition-source-location-filenames
                    (namestring-1 namestring-2)))
  ;; Namestring (often not always relative) of the source file that the
  ;; definition was compiled from.  This is null if the definition was
  ;; not compiled from a file.
  (namestring-1 nil :type (or string null) :read-only t)
  ;; Either null or the namestring relative to which NAMESTRING-1 named
  ;; a file at compile-file-time.
  (namestring-2 nil :type (or string null) :read-only t))
(!set-load-form-method definition-source-location-filenames  (:xc :target))

;;; A DEFINITION-SOURCE-LOCATION contains two packed fixnums in the INDICES slot,
;;; and unless there is a non-nil plist, does not store the plist.
;;; Packed representation is: header + layout, namestring, indices, (padding)
(def!struct (definition-source-location
             (:constructor %make-basic-definition-source-location
                           (filenames indices))
             (:copier nil))
  (filenames nil :type (or null definition-source-location-filenames) :read-only t)
  (indices 0 :type integer :read-only t))
(!set-load-form-method definition-source-location  (:xc :target))
(def!struct (definition-source-location+plist
             (:include definition-source-location)
             (:constructor %make-full-definition-source-location
                           (filenames indices plist))
             (:copier nil))
  (plist nil :read-only t))

(declaim (inline definition-source-location-toplevel-form-number
                 definition-source-location-form-number
                 definition-source-location-plist))
;;; Toplevel form index
(defun definition-source-location-toplevel-form-number (source-loc)
  (let ((val (ash (definition-source-location-indices source-loc) -15)))
    (cond ((plusp val) (1- val))
          ((minusp val) val))))
;; DFO form number within the top-level form
(defun definition-source-location-form-number (source-loc)
  (let ((val (ldb (byte 15 0) (definition-source-location-indices source-loc))))
    (if (plusp val) (1- val))))
;; plist from WITH-COMPILATION-UNIT
(defun definition-source-location-plist (source-loc)
  (when (typep (the definition-source-location source-loc)
               'definition-source-location+plist)
    (definition-source-location+plist-plist source-loc)))

;; This used to be an accessor, but now it obeys
;; *SOURCE-NAMESTRING-DEFAULTING*.
(defun definition-source-location-namestring (source-loc)
  (let* ((filenames (definition-source-location-filenames source-loc))
         (namestring-1 (definition-source-location-filenames-namestring-1 filenames))
         (namestring-2 (definition-source-location-filenames-namestring-2 filenames)))
    (maybe-reconstitute-namestring namestring-1 namestring-2)))

(defun %make-definition-source-location (namestring-1 namestring-2 tlf-num subform-num)
  (declare (type (or null (integer -1 *)) tlf-num)
           (type (or null unsigned-byte) subform-num))
  (let* ((plist *source-plist*)
         ;; Use 15 bits for subform#, and all other bits (including sign) for TLF#.
         ;; Map 0 to NIL, 1 to 0, 2 to 1, etc; but -1 remains itself.
         (indices
          (logior (ash (cond ((eql tlf-num -1) -1)
                             (tlf-num (1+ tlf-num))
                             (t 0))
                       15)
                  ;; If subform-num exceeds 32766 just drop it.
                  (if (and subform-num (< subform-num 32767))
                      (1+ subform-num)
                      0)))
         (source-info (and (boundp '*source-info*) *source-info*))
         (last (and source-info
                    (source-info-last-defn-source-loc source-info)))
         (last-filenames (if last (definition-source-location-filenames last)))
         (filenames (if (and last
                             (equal (definition-source-location-filenames-namestring-1
                                     last-filenames)
                                    namestring-1)
                             (equal (definition-source-location-filenames-namestring-2
                                      last-filenames)
                                    namestring-2))
                        last-filenames
                        (%make-definition-source-location-filenames
                         namestring-1 namestring-2))))
    (if (and last
             (eql (definition-source-location-indices last) indices)
             (eql last-filenames filenames)
             (equal (definition-source-location-plist last) plist))
        last
        (let ((new (if plist
                       (%make-full-definition-source-location filenames indices plist)
                       (%make-basic-definition-source-location filenames indices))))
          (when source-info
            (setf (source-info-last-defn-source-loc source-info) new))
          new))))

(defun make-definition-source-location ()
  (let ((source-info (and (boundp '*source-info*) *source-info*))
         tlf-number
         form-number)
    (multiple-value-bind (namestring-1 namestring-2)
        (or *source-namestring*
            (when source-info
              (make-file-info-namestrings
               cl:*compile-file-pathname*
               (get-toplevelish-file-info source-info))))
      (acond ((boundp '*current-path*)
              (setf tlf-number (source-path-tlf-number *current-path*)
                    form-number (source-path-form-number *current-path*)))
             ((and source-info (source-info-file-info source-info))
              (setf tlf-number (1- (fill-pointer (file-info-forms it))))))
      (%make-definition-source-location namestring-1 namestring-2 tlf-number form-number))))

(defun make-file-info-namestrings (name file-info)
  (let* ((pathname-1 (file-info-pathname-1 file-info))
         (dir (and pathname-1 (pathname-directory pathname-1)))
         (pathname-2 (file-info-pathname-2 file-info)))
    (cond ((and dir (eq (first dir) :absolute))
           (values (namestring pathname-1) nil))
          ((and pathname-1 pathname-2)
           (values (namestring pathname-1) (namestring pathname-2)))
          (t (if name
                 (namestring name)
                 nil)))))

(in-package "SB-IMPL")

(defvar *eval-source-context* nil)
(defvar *eval-tlf-index* nil)
(defvar *eval-source-info* nil)
