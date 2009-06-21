;;;; code to manipulate symbols (but not packages, which are handled
;;;; elsewhere)
;;;;
;;;; Many of these definitions are trivial interpreter entries to
;;;; functions open-coded by the compiler.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(declaim (maybe-inline get get2 get3 %put getf remprop %putf get-properties keywordp))

(defun symbol-value (symbol)
  #!+sb-doc
  "Return SYMBOL's current bound value."
  (declare (optimize (safety 1)))
  (symbol-value symbol))

(defun boundp (symbol)
  #!+sb-doc
  "Return non-NIL if SYMBOL is bound to a value."
  (boundp symbol))

(defun set (symbol new-value)
  #!+sb-doc
  "Set SYMBOL's value cell to NEW-VALUE."
  (declare (type symbol symbol))
  (about-to-modify-symbol-value symbol 'set new-value)
  (%set-symbol-value symbol new-value))

(defun %set-symbol-value (symbol new-value)
  (%set-symbol-value symbol new-value))

(defun symbol-global-value (symbol)
  #!+sb-doc
  "Return the SYMBOL's current global value. Identical to SYMBOL-VALUE,
in single-threaded builds: in multithreaded builds bound values are
distinct from the global value. Can also be SETF."
  (declare (optimize (safety 1)))
  (symbol-global-value symbol))

(defun set-symbol-global-value (symbol new-value)
  (about-to-modify-symbol-value symbol 'set new-value)
  (sb!kernel:%set-symbol-global-value symbol new-value))

(declaim (inline %makunbound))
(defun %makunbound (symbol)
  (%set-symbol-value symbol (%primitive sb!c:make-other-immediate-type
                                        0 sb!vm:unbound-marker-widetag)))

(defun makunbound (symbol)
  #!+sb-doc
  "Make SYMBOL unbound, removing any value it may currently have."
  (with-single-package-locked-error (:symbol symbol "unbinding the symbol ~A")
    (when (and (info :variable :always-bound symbol))
      (error "Can't make ~A variable unbound: ~S" 'always-bound symbol))
    (about-to-modify-symbol-value symbol 'makunbound)
    (%makunbound symbol)
    symbol))

;;; Return the built-in hash value for SYMBOL.
(defun symbol-hash (symbol)
  (symbol-hash symbol))

(defun symbol-function (symbol)
  #!+sb-doc
  "Return SYMBOL's current function definition. Settable with SETF."
  (%coerce-name-to-fun symbol))

(defun (setf symbol-function) (new-value symbol)
  (declare (type symbol symbol) (type function new-value))
  (with-single-package-locked-error
      (:symbol symbol "setting the symbol-function of ~A")
    (setf (%coerce-name-to-fun symbol) new-value)))

(defun symbol-plist (symbol)
  #!+sb-doc
  "Return SYMBOL's property list."
  (symbol-plist symbol))

(defun %set-symbol-plist (symbol new-value)
  (setf (symbol-plist symbol) new-value))

(defun symbol-name (symbol)
  #!+sb-doc
  "Return SYMBOL's name as a string."
  (symbol-name symbol))

(defun symbol-package (symbol)
  #!+sb-doc
  "Return the package SYMBOL was interned in, or NIL if none."
  (symbol-package symbol))

(defun %set-symbol-package (symbol package)
  (declare (type symbol symbol))
  (%set-symbol-package symbol package))

(defun make-symbol (string)
  #!+sb-doc
  "Make and return a new symbol with the STRING as its print name."
  (declare (type string string))
  (%make-symbol (if (simple-string-p string)
                    string
                    (subseq string 0))))

(defun get (symbol indicator &optional (default nil))
  #!+sb-doc
  "Look on the property list of SYMBOL for the specified INDICATOR. If this
  is found, return the associated value, else return DEFAULT."
  (get3 symbol indicator default))

(defun get2 (symbol indicator)
  (get3 symbol indicator nil))
#|
  (let (cdr-pl)
    (do ((pl (symbol-plist symbol) (cdr cdr-pl)))
        ((atom pl) nil)
      (setf cdr-pl (cdr pl))
      (cond ((atom cdr-pl)
             (error "~S has an odd number of items in its property list."
                    symbol))
            ((eq (car pl) indicator)
             (return (car cdr-pl)))))))
|#

(defun get3 (symbol indicator default)
  (let (cdr-pl)
    (do ((pl (symbol-plist symbol) (cdr cdr-pl)))
        ((atom pl) default)
      (setq cdr-pl (cdr pl))
      (cond ((atom cdr-pl)
             (error "~S has an odd number of items in its property list."
                    symbol))
            ((eq (car pl) indicator)
             (return (car cdr-pl)))))))

(defun %put (symbol indicator value)
  #!+sb-doc
  "The VALUE is added as a property of SYMBOL under the specified INDICATOR.
  Returns VALUE."
  (do ((pl (symbol-plist symbol) (cddr pl)))
      ((endp pl)
       (setf (symbol-plist symbol)
             (list* indicator value (symbol-plist symbol)))
       value)
    (cond ((endp (cdr pl))
           (error "~S has an odd number of items in its property list."
                  symbol))
          ((eq (car pl) indicator)
           (rplaca (cdr pl) value)
           (return value)))))

(defun remprop (symbol indicator)
  #!+sb-doc
  "Look on property list of SYMBOL for property with specified
  INDICATOR. If found, splice this indicator and its value out of
  the plist, and return the tail of the original list starting with
  INDICATOR. If not found, return () with no side effects.

  NOTE: The ANSI specification requires REMPROP to return true (not false)
  or false (the symbol NIL). Portable code should not rely on any other value."
  (do ((pl (symbol-plist symbol) (cddr pl))
       (prev nil pl))
      ((atom pl) nil)
    (cond ((atom (cdr pl))
           (error "~S has an odd number of items in its property list."
                  symbol))
          ((eq (car pl) indicator)
           (cond (prev (rplacd (cdr prev) (cddr pl)))
                 (t
                  (setf (symbol-plist symbol) (cddr pl))))
           (return pl)))))

(defun getf (place indicator &optional (default ()))
  #!+sb-doc
  "Search the property list stored in Place for an indicator EQ to INDICATOR.
  If one is found, return the corresponding value, else return DEFAULT."
  (do ((plist place (cddr plist)))
      ((null plist) default)
    (cond ((atom (cdr plist))
           (error 'simple-type-error
                  :format-control "malformed property list: ~S."
                  :format-arguments (list place)
                  :datum (cdr plist)
                  :expected-type 'cons))
          ((eq (car plist) indicator)
           (return (cadr plist))))))

(defun %putf (place property new-value)
  (declare (type list place))
  (do ((plist place (cddr plist)))
      ((endp plist) (list* property new-value place))
    (declare (type list plist))
    (when (eq (car plist) property)
      (setf (cadr plist) new-value)
      (return place))))

(defun get-properties (place indicator-list)
  #!+sb-doc
  "Like GETF, except that INDICATOR-LIST is a list of indicators which will
  be looked for in the property list stored in PLACE. Three values are
  returned, see manual for details."
  (do ((plist place (cddr plist)))
      ((null plist) (values nil nil nil))
    (cond ((atom (cdr plist))
           (error 'simple-type-error
                  :format-control "malformed property list: ~S."
                  :format-arguments (list place)
                  :datum (cdr plist)
                  :expected-type 'cons))
          ((memq (car plist) indicator-list)
           (return (values (car plist) (cadr plist) plist))))))

(defun copy-symbol (symbol &optional (copy-props nil) &aux new-symbol)
  #!+sb-doc
  "Make and return a new uninterned symbol with the same print name
  as SYMBOL. If COPY-PROPS is false, the new symbol is neither bound
  nor fbound and has no properties, else it has a copy of SYMBOL's
  function, value and property list."
  (declare (type symbol symbol))
  (setq new-symbol (make-symbol (symbol-name symbol)))
  (when copy-props
    (%set-symbol-value new-symbol
                       (%primitive sb!c:fast-symbol-value symbol))
    (setf (symbol-plist new-symbol)
          (copy-list (symbol-plist symbol)))
    (when (fboundp symbol)
      (setf (symbol-function new-symbol) (symbol-function symbol))))
  new-symbol)

;;; FIXME: This declaration should be redundant.
(declaim (special *keyword-package*))

(defun keywordp (object)
  #!+sb-doc
  "Return true if Object is a symbol in the \"KEYWORD\" package."
  (and (symbolp object)
       (eq (symbol-package object) *keyword-package*)))

;;;; GENSYM and friends

(defun %make-symbol-name (prefix counter)
  (with-output-to-string (s)
    (write-string prefix s)
    (%output-integer-in-base counter 10 s)))

(defvar *gensym-counter* 0
  #!+sb-doc
  "counter for generating unique GENSYM symbols")
(declaim (type unsigned-byte *gensym-counter*))

(defun gensym (&optional (thing "G"))
  #!+sb-doc
  "Creates a new uninterned symbol whose name is a prefix string (defaults
   to \"G\"), followed by a decimal number. Thing, when supplied, will
   alter the prefix if it is a string, or be used for the decimal number
   if it is a number, of this symbol. The default value of the number is
   the current value of *gensym-counter* which is incremented each time
   it is used."
  (let ((old *gensym-counter*))
    (unless (numberp thing)
      (let ((new (etypecase old
                   (index (1+ old))
                   (unsigned-byte (1+ old)))))
        (declare (optimize (speed 3) (safety 0) (inhibit-warnings 3)))
        (setq *gensym-counter* new)))
    (multiple-value-bind (prefix int)
        (etypecase thing
          (simple-string (values thing old))
          (fixnum (values "G" thing))
          (string (values (coerce thing 'simple-string) old)))
      (declare (simple-string prefix))
      (make-symbol (%make-symbol-name prefix int)))))

(defvar *gentemp-counter* 0)
(declaim (type unsigned-byte *gentemp-counter*))

(defun gentemp (&optional (prefix "T") (package (sane-package)))
  #!+sb-doc
  "Creates a new symbol interned in package PACKAGE with the given PREFIX."
  (declare (type string prefix))
  (loop for name = (%make-symbol-name prefix (incf *gentemp-counter*))
        while (nth-value 1 (find-symbol name package))
        finally (return (values (intern name package)))))
