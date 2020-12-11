(in-package :sb-grovel)

;;;; The macros defined here are called from #:Gconstants.lisp, which was
;;;; generated from constants.lisp by the C compiler as driven by that
;;;; wacky def-to-lisp thing.

;;; (def-foreign-routine ("stat" STAT ) (INTEGER 32) (FILE-NAME
;;; C-CALL:C-STRING) (BUF (* T)) )

;;; I can't help thinking this was originally going to do something a
;;; lot more complex
(defmacro define-foreign-routine
  (&whole it (c-name lisp-name) return-type &rest args)
  (declare (ignorable c-name lisp-name return-type args))
  `(define-alien-routine ,@(cdr it)))



;;; strctures

#| C structs need: the with-... interface.
|#

;;; global XXXs:
#|
 XXX: :distrust-length t fields are dangerous. they should only be at
      the end of the structure (they mess up offset/size calculations)
|#

(defun reintern (symbol &optional (package *package*))
  (if (symbolp symbol)
      (intern (symbol-name symbol) package)
      symbol))

(defparameter alien-type-table (make-hash-table :test 'eql))
(defparameter lisp-type-table (make-hash-table :test 'eql))

(macrolet ((define-alien-types ((type size) &rest defns)
               `(progn
                  ,@(loop for defn in defns
                          collect (destructuring-bind (expected-type c-type lisp-type) defn
                                    `(progn
                                       (setf (gethash ',expected-type alien-type-table)
                                             (lambda (,type ,size)
                                               (declare (ignorable type size))
                                               ,c-type))
                                       (setf (gethash ',expected-type lisp-type-table)
                                             (lambda (,type ,size)
                                               (declare (ignorable type size))
                                               ,lisp-type))))))))
  (define-alien-types (type size)
      (integer (or (gethash size (symbol-value (intern "*INTEGER-SIZES*")))
                   `(integer ,(* 8 size)))
               `(unsigned-byte ,(* 8 size)))
      (unsigned `(unsigned ,(* 8 size))
                `(unsigned-byte ,(* 8 size)))
      (signed `(signed ,(* 8 size))
              `(signed-byte ,(* 8 size)))
      (c-string `(array char ,size) 'cl:simple-string)
      (c-string-pointer 'c-string 'cl:simple-string)
      ;; TODO: multi-dimensional arrays, if they are ever needed.
      (array (destructuring-bind (array-tag elt-type &optional array-size) type
               (declare (ignore array-tag))
               ;; XXX: use of EVAL.  alien-size is a macro,
               ;; unfortunately; and it will only accept unquoted type
               ;; forms.
               `(sb-alien:array ,elt-type ,(or array-size
                                               (/ size (eval `(sb-alien:alien-size ,elt-type :bytes))))))
             t)))

(defun retrieve-type-for (type size table)
  (multiple-value-bind (type-fn found)
      (gethash (reintern (typecase type
                           (list (first type))
                           (t    type))
                         (find-package '#:sb-grovel))
               table)
    (values
     (if found
         (funcall (the function type-fn) type size)
         type)
     found)))

(defun alien-type-for (type size)
  (reintern (retrieve-type-for type size alien-type-table)))

(defun lisp-type-for (type size)
  (multiple-value-bind (val found)
      (retrieve-type-for type size lisp-type-table)
    (if found
        val
        t)))


(defun mk-padding (len offset)
  (make-instance 'padding
                 :type `(array char ,len)
                 :offset offset
                 :size len
                 :name (intern (format nil "PADDING-~D-~D" len offset))))
(defun mk-struct (offset &rest children)
  (make-instance 'struct :name (gentemp "STRUCT")
                 :children (remove nil children)
                 :offset offset))
(defun mk-union (offset &rest children)
  (make-instance 'union :name (gentemp "UNION")
                 :children (remove nil children)
                 :offset offset))
(defun mk-val (name type h-type offset size)
  (declare (ignore h-type))
  (make-instance 'value-slot :name name
                 :size size
                 :offset offset
                 :type type))

;;; struct tree classes

(defclass slot ()
  ((offset :initarg :offset :reader offset)
   (name :initarg :name :reader name)))

(defclass structured-type (slot)
  ((children :initarg :children :accessor children)))

(defclass union (structured-type)
  ())

(defclass struct (structured-type)
  ())

(defclass value-slot (slot)
  ((size :initarg :size :reader size)
   (type :initarg :type :reader type)))

(defclass padding (value-slot)
  ())

(defmethod print-object ((o value-slot) s)
  (print-unreadable-object (o s :type t)
    (format s "~S ~A+~A=~A" (name o) (offset o) (size o) (slot-end o))))

(defmethod print-object ((o structured-type) s)
  (print-unreadable-object (o s :type t)
    (format s "~S ~A" (name o) (children o))))

(defmethod size ((slot structured-type))
  (let ((min-offset (offset slot)))
    (if (null (children slot))
        0
        (reduce #'max (mapcar (lambda (child)
                                (+ (- (offset child) min-offset) (size child)))
                              (children slot))
                :initial-value 0))))

(defgeneric slot-end (slot))
(defmethod slot-end ((slot slot))
  (+ (offset slot) (size slot)))

(defun overlap-p (elt1 elt2)
  (unless (or (zerop (size elt1))
              (zerop (size elt2)))
    (or
     (and (<= (offset elt1)
              (offset elt2))
          (< (offset elt2)
             (slot-end elt1)))
     (and (<= (offset elt2)
              (offset elt1))
          (< (offset elt1)
             (slot-end elt2))))))

(defgeneric find-overlaps (root new-element))
(defmethod find-overlaps ((root structured-type) new-element)
  (when (overlap-p root new-element)
    (let ((overlapping-elts (loop for child in (children root)
                                  for overlap = (find-overlaps child new-element)
                                  when overlap
                                     return overlap)))
      (cons root overlapping-elts))))

(defmethod find-overlaps ((root value-slot) new-element)
  (when (overlap-p root new-element)
    (list root)))

(defgeneric pad-to-offset-of (to-pad parent))
  (macrolet ((skel (end-form)
             `(let* ((end ,end-form)
                     (len (abs (- (offset to-pad) end))))
                (cond
                  ((= end (offset to-pad)) ; we are at the right offset.
                   nil)
                  (t                    ; we have to pad between the
                                        ; old slot's end and the new
                                        ; slot's offset
                   (mk-padding len end))))))

  (defmethod pad-to-offset-of (to-pad (parent struct))
    (skel (if (null (children parent))
              0
              (+ (size parent) (offset parent)))))
  (defmethod pad-to-offset-of (to-pad (parent union))
    (skel (if (null (children parent))
              (offset to-pad)
              (offset parent)))))

(defgeneric replace-by-union (in-st element new-element))
(defmethod replace-by-union ((in-st struct) elt new-elt)
  (setf (children in-st) (remove elt (children in-st)))
  (let ((padding (pad-to-offset-of new-elt in-st)))
    (setf (children in-st)
          (nconc (children in-st)
                 (list (mk-union (offset elt)
                                 elt
                                 (if padding
                                     (mk-struct (offset elt)
                                                padding
                                                new-elt)
                                     new-elt)))))))

(defmethod replace-by-union ((in-st union) elt new-elt)
  (let ((padding (pad-to-offset-of new-elt in-st)))
    (setf (children in-st)
          (nconc (children in-st)
                 (list (if padding
                           (mk-struct (offset in-st)
                                      padding
                                      new-elt)
                           new-elt))))))

(defgeneric insert-element (root new-elt))
(defmethod insert-element ((root struct) (new-elt slot))
  (let ((overlaps (find-overlaps root new-elt)))
    (cond
      (overlaps (let ((last-structure (first (last overlaps 2)))
                      (last-val (first (last overlaps))))
                  (replace-by-union last-structure last-val new-elt)
                  root))
      (t
       (let ((padding (pad-to-offset-of new-elt root)))
         (setf (children root)
               (nconc (children root)
                      (when padding (list padding))
                      (list new-elt)))))))
  root)

(defun sane-slot (alien-var &rest slots)
  "Emulates the SB-ALIEN:SLOT interface, with useful argument order for
deeply nested structures."
  (labels ((rewriter (slots)
             (if (null slots)
                 alien-var
                 `(sb-alien:slot ,(rewriter (rest slots))
                                 ',(first slots)))))
    (rewriter slots)))

(defgeneric accessor-modifier-for (element-type accessor-type))

(defmethod accessor-modifier-for (element-type (accessor-type (eql :getter)))
  nil)
(defmethod accessor-modifier-for (element-type (accessor-type (eql :setter)))
  nil)

(defmethod accessor-modifier-for ((element-type (eql 'C-STRING))
                                  (accessor-type (eql :getter)))
  'c-string-reader[1])

(defmethod accessor-modifier-for ((element-type (eql 'C-STRING))
                                  (accessor-type (eql :setter)))
  'c-string-writer)

;; The "[1]" in the name c-string-reader[1] refers to the CLHS
;; glossary entry definitions for "reader".
(defun c-string-reader[1] (place &optional limit)
  (declare (ignore limit))
  `(cast ,place c-string))

(defun c-string-writer (string alien &optional limit)
  (sb-int:with-unique-names
      (stringvar upper-bound last-elt octets alien-ptr index)
    `(let* ((,stringvar ,string)
            (,upper-bound (or ,limit (1+ (length ,stringvar))))
            (,last-elt (min (1- ,upper-bound) (length ,stringvar)))
            (,octets (sb-ext:string-to-octets ,stringvar :end ,last-elt
                                              :null-terminate t))
            (,alien-ptr (cast ,alien (* unsigned-char))))
       (declare (cl:type (simple-array (unsigned-byte 8) (*)) ,octets))
       (declare (cl:type sb-int:index ,last-elt))
       (dotimes (,index ,last-elt)
         (setf (deref ,alien-ptr ,index) (aref ,octets ,index)))
       (subseq ,stringvar 0 ,last-elt))))

(defgeneric accessors-for (struct-name element path))
(defmethod accessors-for (struct-name (root structured-type) path)
  nil)


(defmethod accessors-for (struct-name (root value-slot) path)
  (let* ((rpath (reverse path))
         (accessor-name (intern
                         (format nil "~A-~A"
                                 (symbol-name struct-name)
                                 (symbol-name (name root)))))
         (offset-constant-name (intern
                                (format nil "OFFSET-OF-~A" accessor-name)))
         (var (gensym "VAR-"))
         (place (apply #'sane-slot 'struct
                       (mapcar 'name (append (rest rpath) (list root)))))
         (reader (let ((reader (accessor-modifier-for
                                (reintern (type root)
                                          (find-package :sb-grovel))
                                :getter)))
                   (if reader
                       (funcall reader place (size root))
                       place)))
         (writer (let ((writer (accessor-modifier-for
                                (reintern (type root)
                                          (find-package :sb-grovel))
                                :setter)))
                   (if writer
                       (funcall writer var place (size root))
                       `(setf ,place ,var)))))
    `((declaim (inline ,accessor-name (setf ,accessor-name)))
      (defun ,accessor-name (struct)
        (declare (cl:type (alien (* ,struct-name)) struct)
                 (optimize (speed 3)))
        ,reader)
      (defun (setf ,accessor-name) (,var struct)
        (declare (cl:type (alien (* ,struct-name)) struct)
                 (cl:type ,(lisp-type-for (type root) (size root)) ,var)
                 (optimize (speed 3)))
        ,writer)
      (defconstant ,offset-constant-name
                   ,(offset root)))))



(defmethod accessors-for (struct (root padding) path)
  nil)

(defgeneric generate-struct-definition (struct-name root path))
(defmethod generate-struct-definition (struct-name (root structured-type) path)
  (let ((naccessors (accessors-for struct-name root path))
        (nslots nil))
    (dolist (child (children root))
      (multiple-value-bind (slots accessors)
          (generate-struct-definition struct-name child (cons root path))
        (setf nslots (nconc nslots slots))
        (setf naccessors (nconc naccessors accessors))))
    (values `((,(name root) (,(type-of root) ,(name root) ,@nslots)))
            naccessors)))

(defmethod generate-struct-definition (struct-name (root value-slot) path)
  (values `((,(name root) ,(alien-type-for (type root) (size root))))
          (accessors-for struct-name root path)))

(defmacro define-c-struct (name size &rest elements)
  (multiple-value-bind (struct-elements accessors)
      (let* ((root (make-instance 'struct :name name :children nil :offset 0)))
        (loop for e in (sort (copy-list elements) #'< :key #'fourth)
              do (insert-element root (apply 'mk-val e))
              finally (return root))
        (setf (children root)
              (nconc (children root)
                     (list
                      (mk-padding (max 0 (- size
                                            (size root)))
                                  (size root)))))
        (generate-struct-definition name root nil))
    (sb-int:with-unique-names (var field-values body field-name pair
                                   object c-object index)
      (let ((with (intern (format nil "WITH-~A" name)))
            (allocate (intern (format nil "ALLOCATE-~A" name)))
            (size-of (intern (format nil "SIZE-OF-~A" name))))
        `(progn
           (sb-alien:define-alien-type ,@(first struct-elements))
           ,@accessors
           (defmacro ,with (,var (&rest ,field-values) &body ,body)
             (labels ((,field-name (,var)
                        (intern
                         (format nil ,(format nil "~A-~~A" (symbol-name name))
                                 (symbol-name ,var))
                         ,(symbol-package name))))
               `(sb-alien:with-alien ((,,var (* ,',name) ,'(,allocate)))
                  (unwind-protect
                      (progn
                        (setf ,@(mapcan
                                 (lambda (,pair)
                                   `((,(,field-name (first ,pair)) ,,var)
                                     ,(second ,pair)))
                                 ,field-values))
                        ,@,body)
                    (sb-alien:free-alien ,,var)))))
           (defconstant ,size-of ,size)
           (defun ,allocate ()
             (let* ((,object (sb-alien:make-alien ,name))
                    (,c-object (cast ,object (* (unsigned 8)))))
               ;; we have to initialize the object to all-0 before we can
               ;; expect to make sensible use of it - the object returned
               ;; by make-alien is initialized to all-D0 bytes.

               ;; FIXME: This should be fixed in sb-alien, where better
               ;; optimizations might be possible.
               (dotimes (,index ,size)
                 (setf (deref ,c-object ,index) 0))
               ,object)))))))

;; FIXME: Nothing in SBCL uses this, but kept it around in case there
;; are third-party sb-grovel clients.  It should go away eventually,
;; on the principle that sb-grovel should only have to be loaded in
;; order to do an actual groveling run.
(defun foreign-nullp (c)
  (null-alien c))

(declaim (sb-ext:deprecated
          :late ("SBCL" "1.2.15")
          (function foreign-nullp :replacement sb-alien:null-alien)))
