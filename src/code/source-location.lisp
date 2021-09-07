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

;;; A DEFINITION-SOURCE-LOCATION contains two packed fixnums in the INDICES slot,
;;; and unless there is a non-nil plist, does not store the plist.
;;; Packed representation is: header + layout, namestring, indices, (padding)
(def!struct (definition-source-location
             (:constructor %make-basic-definition-source-location
                           (namestring indices))
             (:copier nil))
  ;; Namestring of the source file that the definition was compiled from.
  ;; This is null if the definition was not compiled from a file.
  (namestring nil :type (or string null) :read-only t)
  (indices 0 :type integer :read-only t))
(!set-load-form-method definition-source-location  (:xc :target))
(def!struct (definition-source-location+plist
             (:include definition-source-location)
             (:constructor %make-full-definition-source-location
                           (namestring indices plist))
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

(defun %make-definition-source-location (namestring tlf-num subform-num)
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
                    (source-info-last-defn-source-loc source-info))))
    (if (and last
             (eql (definition-source-location-indices last) indices)
             (string= (definition-source-location-namestring last) namestring)
             (equal (definition-source-location-plist last) plist))
        last
        (let ((new (if plist
                       (%make-full-definition-source-location namestring indices plist)
                       (%make-basic-definition-source-location namestring indices))))
          (when source-info
            (setf (source-info-last-defn-source-loc source-info) new))
          new))))

(defun make-definition-source-location ()
  (let* ((source-info (and (boundp '*source-info*) *source-info*))
         (namestring
          (or *source-namestring*
              (when source-info
                (make-file-info-namestring
                 cl:*compile-file-pathname*
                 (get-toplevelish-file-info source-info)))))
         tlf-number
         form-number)
    (acond ((boundp '*current-path*)
            (setf tlf-number (source-path-tlf-number *current-path*)
                  form-number (source-path-form-number *current-path*)))
           ((and source-info (source-info-file-info source-info))
            (setf tlf-number (1- (fill-pointer (file-info-forms it))))))
    (%make-definition-source-location namestring tlf-number form-number)))

(defun make-file-info-namestring (name file-info)
  (let* ((pathname (file-info-pathname file-info))
         (dir (and pathname (pathname-directory pathname))))
    (if (and dir (eq (first dir) :absolute))
        (namestring pathname)
        (if name
            (namestring name)
            nil))))

#+sb-source-locations
(progn
  (define-source-transform source-location ()
    (make-definition-source-location))
  ;; We need a regular definition of SOURCE-LOCATION for calls processed
  ;; during LOAD on a source file while *EVALUATOR-MODE* is :INTERPRET.
  #-sb-xc-host
  (defun source-location ()
    (make-definition-source-location)))

#-sb-source-locations
(defun source-location () nil)

(in-package "SB-IMPL")

(defvar *eval-source-context* nil)
(defvar *eval-tlf-index* nil)
(defvar *eval-source-info* nil)
