
(in-package :sb-c)

(defvar *struct-flags* nil)

(defun get-flag-info (type)
  (cdr (assoc type *struct-flags*)))

(defun set-flag-info (type info)
  (setf *struct-flags*
        (acons type info (remove type *struct-flags* :key #'car))))

(defun parse-flag-slot (spec)
  (let* ((spec (ensure-list spec))
         (name (first spec)))
    (check-type name symbol)
    (destructuring-bind (&optional default &rest rest) (rest spec)
      ;; How do you like them &optional+&key warnings
      (destructuring-bind (&key (type 'boolean)) rest
        (unless (typep default type)
          (error "Default value ~s not of type ~s" default type))
        (cond ((eq type 'boolean)
               (list :name name
                     :kind :boolean
                     :size 1
                     :default default
                     :values '(nil t)))
              ((typep type '(cons (eql member) cons))
               (let* ((values (rest type))
                      (size (max 1 (integer-length (1- (length values))))))
                 (list :name name
                       :kind :member
                       :size size
                       :default default
                       :values values)))
              (t
               (error "Only boolean and member types are supported: ~s" type)))))))

(defun all-flag-slots (type)
  (let ((info (get-flag-info type)))
    (unless info (error "Unknown flag type ~S" type))
    (let ((parent (getf info :parent)))
      (append (when parent (all-flag-slots parent))
              (getf info :slots)))))

(defmacro def-struct-flags (type-spec &body slots)
  (destructuring-bind (type &optional include) (ensure-list type-spec)
    (let* ((parent (when include
                          (or (get-flag-info include)
                              (error "Unknown flag type: ~s" include))))
           (start-offset (getf parent :offset 0))
           (flags-accessor (or (getf parent :flags-accessor)
                               (symbolicate type '-flags)))
           (offset start-offset)
           (slots
             (loop for slot in slots
                   for parsed = (parse-flag-slot slot)
                   for size = (getf parsed :size)
                   do
                   (setf (getf parsed :offset) offset)
                   (incf offset size)
                   collect parsed))
           (info `(:offset ,offset
                   :flags-accessor ,flags-accessor
                   :parent ,include
                   :slots ,slots)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (set-flag-info ',type ',info)))))

(defmacro flags (type &rest overrides)
  (let ((slots (all-flag-slots type))
        (int 0))
    (dolist (slot slots)
      (let* ((name (getf slot :name))
             (size (getf slot :size))
             (offset (getf slot :offset))
             (kw (intern (symbol-name name) :keyword))
             (value (if (member kw overrides)
                      (let ((raw (getf overrides kw)))
                        (if (and (consp raw) (eq (first raw) 'quote))
                            (second raw)
                            raw))
                      (getf slot :default))))
        (case (getf slot :kind)
          (:boolean
           (when value
             (setf int (dpb 1 (byte 1 offset) int))))
          (:member
           (let ((code (position value (getf slot :values) :test #'eql)))
             (unless code
               (error "Invalid value ~S for ~S. Must be one of ~S."
                      value name (getf slot :values)))
             (setf (ldb (byte size offset) int) code))))))
    int))

(defmacro def-struct-flag-accessors (type &key inherited
                                               (prefix type))
  (let* ((info (or (get-flag-info type)
                   (error "Unknown flag type: ~S" type)))
         (flags-accessor (getf info :flags-accessor))
         (slots (if inherited
                    (all-flag-slots type)
                    (getf info :slots))))
    `(progn
       ,@(loop for slot in slots
               append
               (let* ((name (getf slot :name))
                      (offset (getf slot :offset))
                      (size (getf slot :size))
                      (kind (getf slot :kind))
                      (values (getf slot :values))
                      (accessor (symbolicate prefix '- name)))

                 (if (eq kind :boolean)
                     `((declaim (inline ,accessor (setf ,accessor))
                                (ftype (function (,type) boolean) ,accessor)
                                (ftype (function (t ,type) t) (setf ,accessor)))

                       (defun ,accessor (instance)
                         (logbitp ,offset (,flags-accessor instance)))

                       (defun (setf ,accessor) (value instance)
                         (setf (ldb (byte 1 ,offset) (,flags-accessor instance))
                               (if value 1 0))
                         value)

                       (define-compiler-macro (setf ,accessor) (&whole whole value instance)
                         (if (constantp value)
                             (let* ((value (eval value))
                                    (bit (if value 1 0)))
                               `(progn
                                  (setf (ldb (byte 1 ,',offset) (,',flags-accessor ,instance))
                                        ,bit)
                                  ',value))
                             whole)))
                     (let ((val-vec (coerce values 'vector))
                           (member-type `(member ,@values)))
                       `((declaim (inline ,accessor (setf ,accessor))
                                  (ftype (function (,type) ,member-type) ,accessor)
                                  (ftype (function (,member-type ,type) ,member-type) (setf ,accessor)))

                         (defun ,accessor (instance)
                           (svref ,val-vec
                                  (ldb (byte ,size ,offset) (,flags-accessor instance))))

                         (defun (setf ,accessor) (value instance)
                           (let ((i (position value ',values :test #'eql)))
                             (unless i
                               (error "~s is not of type (MEMBER ~s)" value ',values))
                             (setf (ldb (byte ,size ,offset) (,flags-accessor instance))
                                   i)
                             value))

                         (define-compiler-macro (setf ,accessor) (&whole whole value instance)
                           (if (constantp value)
                               (let* ((value (eval value))
                                      (i (position value ',values :test #'eql)))
                                 (if i
                                     `(progn (setf (ldb (byte ,',size ,',offset) (,',flags-accessor ,instance))
                                                   ,i)
                                             ',value)
                                     (progn
                                       (warn "Invalid constant value ~S for ~S. Expected one of ~S."
                                             value ',accessor ',values)
                                       whole)))
                               whole))))))))))

(defmacro construct-flags (type-spec &rest args)
  (let* ((type (if (consp type-spec) (first type-spec) type-spec))
         (type-overrides (if (consp type-spec) (rest type-spec) nil))
         (slots (all-flag-slots type))
         (slot-map nil)
         pos-args
         overrides)

    (let ((kw-pos (position-if #'keywordp args)))
      (if kw-pos
          (setf pos-args (subseq args 0 kw-pos)
                overrides (subseq args kw-pos))
          (setf pos-args args
                overrides nil)))

    (let ((all-overrides (append type-overrides overrides)))
      (loop for (kw val) on all-overrides by #'cddr do
            (let ((slot (find (symbol-name kw) slots
                              :key (lambda (s) (symbol-name (getf s :name)))
                              :test #'string-equal)))
              (if slot
                  (push (cons (getf slot :name) val) slot-map)
                  (error "Unknown flag keyword ~S for type ~S." kw type)))))

    (dolist (arg pos-args)
      (let ((named-slot (and (symbolp arg)
                             (find (symbol-name arg) slots
                                   :key (lambda (s) (symbol-name (getf s :name)))
                                   :test #'string-equal))))
        (if (and named-slot (not (assoc (getf named-slot :name) slot-map)))
            (push (cons (getf named-slot :name) arg) slot-map)
            (let ((first-unbound (find-if (lambda (s)
                                            (not (assoc (getf s :name) slot-map)))
                                          slots)))
              (if first-unbound
                  (push (cons (getf first-unbound :name) arg) slot-map)
                  (error "Too many positional flag arguments for ~S." type))))))

    (let ((const-mask 0)
          (runtime-forms nil))
      (dolist (slot slots)
        (let* ((name (getf slot :name))
               (kind (getf slot :kind))
               (offset (getf slot :offset))
               (default (getf slot :default))
               (values (getf slot :values))
               (entry (assoc name slot-map))
               (value (if entry (cdr entry) default)))

          (if (cl:constantp value)
              (let* ((val (eval value))
                     (code (if (eq kind :boolean)
                               (if val 1 0)
                               (position val values :test #'eql))))
                (unless code
                  (error "Invalid constant value ~S for flag ~S. Expected one of ~S."
                         val name values))
                (setf const-mask (logior const-mask (ash code offset))))

                (if (eq kind :boolean)
                    (push `(if ,value ,(ash 1 offset) 0) runtime-forms)
                    (push `(ash (case ,value
                                  ,@(loop for v in values
                                          for i from 0
                                          collect `((,v) ,i))
                                  (otherwise
                                   (error "Invalid value ~S for flag ~S. Expected one of ~S."
                                          ,value ',name ',values)))
                                ,offset)
                          runtime-forms)))))

      (cond
        ((and (zerop const-mask) (null runtime-forms)) 0)
        ((null runtime-forms) const-mask)
        ((zerop const-mask) `(logior ,@runtime-forms))
        (t `(logior ,const-mask ,@runtime-forms))))))
