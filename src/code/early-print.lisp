;;;; printer stuff which has to be defined early (e.g. DEFMACROs)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;;; level and length abbreviations

;;; The current level we are printing at, to be compared against
;;; *PRINT-LEVEL*. See the macro DESCEND-INTO for a handy interface to
;;; depth abbreviation.
(declaim (index *current-level-in-print*))
(!defvar *current-level-in-print* 0)

;;; Automatically handle *PRINT-LEVEL* abbreviation. If we are too
;;; deep, then a #\# is printed to STREAM and BODY is ignored.
(defmacro descend-into ((stream) &body body)
  (let ((flet-name (sb!xc:gensym "DESCEND")))
    `(flet ((,flet-name ()
              ,@body))
       (cond ((and (null *print-readably*)
                   (let ((level *print-level*))
                     (and level (>= *current-level-in-print* level))))
              (write-char #\# ,stream))
             (t
              (let ((*current-level-in-print* (1+ *current-level-in-print*)))
                (,flet-name)))))))

;;; Punt if INDEX is equal or larger then *PRINT-LENGTH* (and
;;; *PRINT-READABLY* is NIL) by outputting \"...\" and returning from
;;; the block named NIL.
(defmacro punt-print-if-too-long (index stream)
  `(when (and (not *print-readably*)
              (let ((len *print-length*))
                (and len (>= ,index len))))
     (write-string "..." ,stream)
     (return)))


;;;; circularity detection stuff

;;; When *PRINT-CIRCLE* is T, this gets bound to a hash table that
;;; (eventually) ends up with entries for every object printed. When
;;; we are initially looking for circularities, we enter a T when we
;;; find an object for the first time, and a 0 when we encounter an
;;; object a second time around. When we are actually printing, the 0
;;; entries get changed to the actual marker value when they are first
;;; printed.
(defvar *circularity-hash-table* nil)

;;; When NIL, we are just looking for circularities. After we have
;;; found them all, this gets bound to 0. Then whenever we need a new
;;; marker, it is incremented.
(defvar *circularity-counter* nil)

;;; Check to see whether OBJECT is a circular reference, and return
;;; something non-NIL if it is. If ASSIGN is true, reference
;;; bookkeeping will only be done for existing entries, no new
;;; references will be recorded. If ASSIGN is true, then the number to
;;; use in the #n= and #n# noise is assigned at this time.
;;;
;;; Note: CHECK-FOR-CIRCULARITY must be called *exactly* once with
;;; ASSIGN true, or the circularity detection noise will get confused
;;; about when to use #n= and when to use #n#. If this returns non-NIL
;;; when ASSIGN is true, then you must call HANDLE-CIRCULARITY on it.
;;; If CHECK-FOR-CIRCULARITY returns :INITIATE as the second value,
;;; you need to initiate the circularity detection noise, e.g. bind
;;; *CIRCULARITY-HASH-TABLE* and *CIRCULARITY-COUNTER* to suitable values
;;; (see #'OUTPUT-OBJECT for an example).
;;;
;;; Circularity detection is done in two places, OUTPUT-OBJECT and
;;; WITH-CIRCULARITY-DETECTION (which is used from PPRINT-LOGICAL-BLOCK).
;;; These checks aren't really redundant (at least I can't really see
;;; a clean way of getting by with the checks in only one of the places).
;;; This causes problems when mixed with pprint-dispatching; an object is
;;; marked as visited in OUTPUT-OBJECT, dispatched to a pretty printer
;;; that uses PPRINT-LOGICAL-BLOCK (directly or indirectly), leading to
;;; output like #1=#1#. The MODE parameter is used for detecting and
;;; correcting this problem.
(defun check-for-circularity (object &optional assign (mode t))
  (when (null *print-circle*)
         ;; Don't bother, nobody cares.
    (return-from check-for-circularity nil))
  (let ((circularity-hash-table *circularity-hash-table*))
    (cond
        ((null circularity-hash-table)
          (values nil :initiate))
        ((null *circularity-counter*)
         (ecase (gethash object circularity-hash-table)
           ((nil)
            ;; first encounter
            (setf (gethash object circularity-hash-table) mode)
            ;; We need to keep looking.
            nil)
           ((:logical-block)
            (setf (gethash object circularity-hash-table)
                  :logical-block-circular)
            t)
           ((t)
            (cond ((eq mode :logical-block)
                   ;; We've seen the object before in output-object, and now
                   ;; a second time in a PPRINT-LOGICAL-BLOCK (for example
                   ;; via pprint-dispatch). Don't mark it as circular yet.
                   (setf (gethash object circularity-hash-table)
                         :logical-block)
                   nil)
                  (t
                   ;; second encounter
                   (setf (gethash object circularity-hash-table) 0)
                   ;; It's a circular reference.
                   t)))
           ((0 :logical-block-circular)
            ;; It's a circular reference.
            t)))
        (t
         (let ((value (gethash object circularity-hash-table)))
           (case value
             ((nil t :logical-block)
              ;; If NIL, we found an object that wasn't there the
              ;; first time around. If T or :LOGICAL-BLOCK, this
              ;; object appears exactly once. Either way, just print
              ;; the thing without any special processing. Note: you
              ;; might argue that finding a new object means that
              ;; something is broken, but this can happen. If someone
              ;; uses the ~@<...~:> format directive, it conses a new
              ;; list each time though format (i.e. the &REST list),
              ;; so we will have different cdrs.
              nil)
             ;; A circular reference to something that will be printed
             ;; as a logical block. Wait until we're called from
             ;; PPRINT-LOGICAL-BLOCK with ASSIGN true before assigning the
             ;; number.
             ;;
             ;; If mode is :LOGICAL-BLOCK and assign is false, return true
             ;; to indicate that this object is circular, but don't assign
             ;; it a number yet. This is neccessary for cases like
             ;; #1=(#2=(#2# . #3=(#1# . #3#))))).
             (:logical-block-circular
              (cond ((and (not assign)
                          (eq mode :logical-block))
                     t)
                    ((and assign
                          (eq mode :logical-block))
                     (let ((value (incf *circularity-counter*)))
                       ;; first occurrence of this object: Set the counter.
                       (setf (gethash object circularity-hash-table) value)
                       value))
                    (t
                     nil)))
             (0
              (if (eq assign t)
                  (let ((value (incf *circularity-counter*)))
                    ;; first occurrence of this object: Set the counter.
                    (setf (gethash object circularity-hash-table) value)
                    value)
                  t))
             (t
              ;; second or later occurrence
              (- value))))))))

;;; Handle the results of CHECK-FOR-CIRCULARITY. If this returns T then
;;; you should go ahead and print the object. If it returns NIL, then
;;; you should blow it off.
(defun handle-circularity (marker stream)
  (case marker
    (:initiate
     ;; Someone forgot to initiate circularity detection.
     (let ((*print-circle* nil))
       (error "trying to use CHECK-FOR-CIRCULARITY when ~
               circularity checking isn't initiated")))
    ((t :logical-block)
     ;; It's a second (or later) reference to the object while we are
     ;; just looking. So don't bother groveling it again.
     nil)
    (t
     (write-char #\# stream)
     (let ((*print-base* 10) (*print-radix* nil))
       (cond ((minusp marker)
              (output-integer (- marker) stream)
              (write-char #\# stream)
              nil)
             (t
              (output-integer marker stream)
              (write-char #\= stream)
              t))))))

(defmacro with-circularity-detection ((object stream) &body body)
  (with-unique-names (marker body-name)
    `(labels ((,body-name ()
               ,@body))
      (cond ((not *print-circle*) (,body-name))
            (*circularity-hash-table*
             (let ((,marker (check-for-circularity ,object t :logical-block)))
               (if ,marker
                   (when (handle-circularity ,marker ,stream)
                    (,body-name))
                  (,body-name))))
            (t
             (let ((*circularity-hash-table* (make-hash-table :test 'eq)))
               (output-object ,object (make-broadcast-stream))
               (let ((*circularity-counter* 0))
                 (let ((,marker (check-for-circularity ,object t
                                                       :logical-block)))
                   (when ,marker
                     (handle-circularity ,marker ,stream)))
                (,body-name))))))))
