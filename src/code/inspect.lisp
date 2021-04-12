;;;; the CL:INSPECT function

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

(defparameter *inspect-length* 10)

(defun inspector (object input-stream output-stream)
  (declare (ignore input-stream))
  (catch 'quit-inspect
    (%inspect object output-stream))
  (values))

(defvar *inspect-fun* #'inspector
  "A function of three arguments OBJECT, INPUT, and OUTPUT which starts an interactive inspector.")

(defvar *inspected*)

(setf (documentation '*inspected* 'variable)
      "the value currently being inspected in CL:INSPECT")

(defun inspect (object)
  (funcall *inspect-fun* object *standard-input* *standard-output*))

(defconstant-eqx +help-for-inspect+
  "
help for INSPECT:
  Q, E        -  Quit the inspector.
  <integer>   -  Inspect the numbered slot.
  R           -  Redisplay current inspected object.
  U           -  Move upward/backward to previous inspected object.
  ?, H, Help  -  Show this help.
  <other>     -  Evaluate the input as an expression.
Within the inspector, the special variable SB-EXT:*INSPECTED* is bound
to the current inspected object, so that it can be referred to in
evaluated expressions.
" #'equal)

(defun %inspect (*inspected* s)
  (named-let redisplay () ; "LAMBDA, the ultimate GOTO":-|
    (multiple-value-bind (description named-p elements)
        (inspected-parts *inspected*)
      (tty-display-inspected-parts description named-p elements s)
      (named-let reread ()
        (format s "~&> ")
        (force-output)
        (let* (;; newly-consed object for hermetic protection against
               ;; mischievous input like #.*EOF-OBJECT*:
               (eof (cons *eof-object* nil))
               (command (read *standard-input* nil eof)))
          (when (eq command eof)
             ;; currently-undocumented feature: EOF is handled as Q.
             ;; If there's ever consensus that this is *the* right
             ;; thing to do (as opposed to e.g. handling it as U), we
             ;; could document it. Meanwhile, it seems more Unix-y to
             ;; do this than to signal an error.
             (/show0 "THROWing QUIT-INSPECT for EOF")
             (throw 'quit-inspect nil))
          (typecase command
            (integer
             (let ((elements-length (length elements)))
               (cond ((< -1 command elements-length)
                      (let* ((element (nth command elements))
                             (value (if named-p (cdr element) element)))
                        (cond ((eq value sb-pcl:+slot-unbound+)
                               (format s "~%That slot is unbound.~%")
                               (return-from %inspect (reread)))
                              (t
                               (%inspect value s)
                               ;; If we ever return, then we should be
                               ;; looking at *INSPECTED* again.
                               (return-from %inspect (redisplay))))))
                     ((zerop elements-length)
                      (format s "~%The object contains nothing to inspect.~%")
                      (return-from %inspect (reread)))
                     (t
                      (format s "~%Enter a valid index (~:[0-~W~;0~]).~%"
                              (= elements-length 1) (1- elements-length))
                      (return-from %inspect (reread))))))
            (symbol
             (case (find-symbol (symbol-name command) *keyword-package*)
               ((:q :e)
                (/show0 "THROWing QUIT-INSPECT for :Q or :E")
                (throw 'quit-inspect nil))
               (:u
                (return-from %inspect))
               (:r
                (return-from %inspect (redisplay)))
               ((:h :? :help)
                (write-string +help-for-inspect+ s)
                (return-from %inspect (reread)))
               (t
                (eval-for-inspect command s)
                (return-from %inspect (reread)))))
            (t
             (eval-for-inspect command s)
             (return-from %inspect (reread)))))))))

(defun eval-for-inspect (command stream)
  (let ((result-list (restart-case
                         (multiple-value-list (interactive-eval command))
                       (nil () :report "Return to the inspector."
                          (format stream "~%returning to the inspector~%")
                          (return-from eval-for-inspect nil)))))
    (format stream "~&~{~S~%~}" result-list)))

(defun tty-display-inspected-parts (description named-p elements stream)
  (let ((*suppress-print-errors*
         (if (subtypep 'serious-condition *suppress-print-errors*)
             *suppress-print-errors*
             'serious-condition)))
    (format stream "~%~A" description)
    (loop for element in elements
       for index from 0
       do (multiple-value-bind (value name)
              (if named-p
                  (values (cdr element) (car element))
                  element)
            (format stream "~W. ~@[~A: ~]~W~%"
                    index name (if (eq value sb-pcl:+slot-unbound+)
                                   "unbound"
                                   value))))))

;;;; INSPECTED-PARTS

;;; Destructure an object for inspection, returning
;;;   (VALUES DESCRIPTION NAMED-P ELEMENTS),
;;; where..
;;;
;;;   DESCRIPTION is a summary description of the destructured object,
;;;   e.g. "The object is a CONS.~%".
;;;
;;;   NAMED-P determines what representation is used for elements
;;;   of ELEMENTS. If NAMED-P is true, then each element is
;;;   (CONS NAME VALUE); otherwise each element is just VALUE.
;;;
;;;   ELEMENTS is a list of the component parts of OBJECT (whose
;;;   representation is determined by NAMED-P).
;;;
;;; (The NAMED-P dichotomy is useful because symbols and instances
;;; need to display both a slot name and a value, while lists and
;;; vectors need only display a value.)
(defgeneric inspected-parts (object))

(defmethod inspected-parts ((object symbol))
  (values (format nil "The object is a SYMBOL.~%")
          t
          (list (cons "Name" (symbol-name object))
                (cons "Package" (symbol-package object))
                (cons "Value" (if (boundp object)
                                  (symbol-value object)
                                  sb-pcl:+slot-unbound+))
                (cons "Function" (if (fboundp object)
                                     (symbol-function object)
                                     sb-pcl:+slot-unbound+))
                (cons "Plist" (symbol-plist object)))))

(defun inspected-structure-elements (object)
  (let ((parts-list '())
        (info (wrapper-info (sb-kernel:wrapper-of object))))
    (when (sb-kernel::defstruct-description-p info)
      (dolist (dd-slot (dd-slots info) (nreverse parts-list))
        (push (cons (dsd-name dd-slot)
                    (funcall (dsd-accessor-name dd-slot) object))
              parts-list)))))

(defmethod inspected-parts ((object structure-object))
  (values (format nil "The object is a STRUCTURE-OBJECT of type ~S.~%"
                  (type-of object))
          t
          (inspected-structure-elements object)))

(defmethod inspected-parts ((object pathname))
  (values (format nil "The object is a ~S.~%" (type-of object))
          t
          (inspected-structure-elements object)))

(defun inspected-standard-object-elements (object)
  (let ((reversed-elements nil)
        (class-slots (sb-pcl::class-slots (class-of object))))
    (dolist (class-slot class-slots (nreverse reversed-elements))
      (let* ((slot-name (slot-value class-slot 'sb-pcl::name))
             (slot-value (if (slot-boundp object slot-name)
                             (slot-value object slot-name)
                             sb-pcl:+slot-unbound+)))
        (push (cons slot-name slot-value) reversed-elements)))))

(defmethod inspected-parts ((object standard-object))
  (values (format nil "The object is a STANDARD-OBJECT of type ~S.~%"
                  (type-of object))
          t
          (inspected-standard-object-elements object)))

(defmethod inspected-parts ((object sb-mop:funcallable-standard-object))
  (values (format nil "The object is a ~S of type ~S.~%"
                  'sb-mop:funcallable-standard-object (type-of object))
          t
          (inspected-standard-object-elements object)))

(defmethod inspected-parts ((object condition))
  (values (format nil "The object is a CONDITION of type ~S.~%"
                  (type-of object))
          t
          (inspected-standard-object-elements object)))

(defmethod inspected-parts ((object function))
  (values (format nil "The object is a ~A named ~S.~%"
                  (if (closurep object) 'closure 'function)
                  (nth-value 2 (function-lambda-expression object)))
          t
          ;; Defined-from stuff used to be here. Someone took
          ;; it out. FIXME: We should make it easy to get
          ;; to DESCRIBE from the inspector.
          (list*
           (cons "Lambda-list" (%fun-lambda-list object))
           (cons "Ftype" (%fun-ftype object))
           (when (closurep object)
             (list
              (cons "Closed over values" (%closure-values object)))))))

#+(or sb-eval sb-fasteval)
(defmethod inspected-parts ((object interpreted-function))
  (multiple-value-bind (defn closurep name) (function-lambda-expression object)
    (declare (ignore closurep))
    (values (format nil "The object is an interpreted function named ~S.~%" name)
            t
            ;; Defined-from stuff used to be here. Someone took
            ;; it out. FIXME: We should make it easy to get
            ;; to DESCRIBE from the inspector.
            (list
             (cons "Lambda-list" (%fun-lambda-list object))
             (cons "Definition" defn)
             (cons "Documentation" (documentation object t))))))

(defmethod inspected-parts ((object vector))
  (let ((length (min (length object) *inspect-length*)))
    (values (format nil
                    "The object is a ~:[~;displaced ~]VECTOR of length ~W.~%"
                    (and (array-header-p object)
                         (%array-displaced-p object))
                    (length object))
            nil
            (coerce (subseq object 0 length) 'list))))

(defun inspected-index-string (index rev-dimensions)
  (if (null rev-dimensions)
      "[]"
      (let ((list nil))
        (dolist (dim rev-dimensions)
          (multiple-value-bind (q r) (floor index dim)
            (setq index q)
            (push r list)))
        (format nil "[~W~{,~W~}]" (car list) (cdr list)))))

(defmethod inspected-parts ((object array))
  (let* ((length (min (array-total-size object) *inspect-length*))
         (reference-array (make-array length
                                      :element-type (array-element-type object)
                                      :displaced-to object))
         (dimensions (array-dimensions object))
         (reversed-elements nil))
    (dotimes (i length)
      (push (cons (format nil
                          "~A "
                          (inspected-index-string i (reverse dimensions)))
                  (aref reference-array i))
            reversed-elements))
    (values (format nil "The object is ~:[an~;a displaced~] ARRAY of ~A.~%~
                         Its dimensions are ~:S.~%"
                    (and (array-header-p object)
                         (%array-displaced-p object))
                    (array-element-type object)
                    dimensions)
            t
            (nreverse reversed-elements))))

(defmethod inspected-parts ((object cons))
  (if (consp (cdr object))
      (inspected-parts-of-nontrivial-list object)
      (inspected-parts-of-simple-cons object)))

(defun inspected-parts-of-simple-cons (object)
  (values "The object is a CONS.
"
          t
          (list (cons 'car (car object))
                (cons 'cdr (cdr object)))))

(defun inspected-parts-of-nontrivial-list (object)
  (let ((length 0)
        (in-list object)
        (reversed-elements nil))
    (flet ((done (description-format)
             (return-from inspected-parts-of-nontrivial-list
               (values (format nil description-format length)
                       t
                       (nreverse reversed-elements)))))
      (loop
       (cond ((null in-list)
              (done "The object is a proper list of length ~S.~%"))
             ((>= length *inspect-length*)
              (push (cons 'rest in-list) reversed-elements)
              (done "The object is a long list (more than ~S elements).~%"))
             ((consp in-list)
              (push (cons length (pop in-list)) reversed-elements)
              (incf length))
             (t
              (push (cons 'rest in-list) reversed-elements)
              (done "The object is an improper list of length ~S.~%")))))))

(defmethod inspected-parts ((object t))
  (values (format nil "The object is an ATOM:~%  ~W~%" object) nil nil))
