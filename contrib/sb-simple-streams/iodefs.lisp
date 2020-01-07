;;; -*- lisp -*-
;;;
;;; **********************************************************************
;;; This code was written by Paul Foley and has been placed in the public
;;; domain.
;;;

;;; Sbcl port by Rudi Schlatte.

;;;
;;; **********************************************************************
;;;
;;; Macros needed by the simple-streams implementation

(in-package "SB-SIMPLE-STREAMS")

(defun %file-namestring (pathname)
  (sb-ext:native-namestring (sb-int:physicalize-pathname pathname) :as-file t))

(defmacro def-stream-class (name superclasses slots &rest options)
  `(defclass ,name ,superclasses ,slots ,@options))


;; All known stream flags.  Note that the position in the constant
;; list is significant (cf. %flags below).
(sb-int:defconstant-eqx +flag-bits+
                        '(:simple       ; instance is valid
                          :input :output ; direction
                          :dual :string ; type of stream
                          :eof          ; latched EOF
                          :dirty        ; output buffer needs write
                          :interactive) ; interactive stream
                        #'equal)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %flags (flags)
    (loop for flag in flags
          as pos = (position flag +flag-bits+)
        when (eq flag :gray) do
          (error "Gray streams not supported.")
        if pos
          sum (ash 1 pos) into bits
        else
          collect flag into unused
      finally (when unused
                (warn "Invalid stream instance flag~P: ~{~S~^, ~}"
                      (length unused) unused))
              (return bits))))

;;; Setup an environment where sm, funcall-stm-handler and
;;; funcall-stm-handler-2 are valid and efficient for a stream of type
;;; class-name or for the stream argument (in which case the
;;; class-name argument is ignored).  In nested with-stream-class
;;; forms, the inner with-stream-class form must specify a stream
;;; argument if the outer one specifies one, or the wrong object will
;;; be accessed.

(defmacro with-stream-class ((class-name &optional stream) &body body)
  (if stream
    (let ((stm (gensym "STREAM")))
      `(let* ((,stm ,stream))
         (declare (type ,class-name ,stm))
         (declare (ignorable ,stm))
         (macrolet ((sm (slot-name stream)
                      (declare (ignore stream))
                      #-count-sm
                      `(slot-value ,',stm ',slot-name)
                      #+count-sm
                      `(%sm ',slot-name ,',stm))
                    (add-stream-instance-flags (stream &rest flags)
                      (declare (ignore stream))
                      `(setf (sm %flags ,',stm) (logior (the fixnum (sm %flags ,',stm))
                                                        ,(%flags flags))))
                    (remove-stream-instance-flags (stream &rest flags)
                      (declare (ignore stream))
                      `(setf (sm %flags ,',stm) (logandc2 (the fixnum (sm %flags ,',stm))
                                                          ,(%flags flags))))
                    (any-stream-instance-flags (stream &rest flags)
                      (declare (ignore stream))
                      `(not (zerop (logand (the fixnum (sm %flags ,',stm))
                                           ,(%flags flags))))))
           ,@body)))
    `(macrolet ((sm (slot-name stream)
                  #-count-sm
                  `(slot-value ,stream ',slot-name)
                  #+count-sm
                  `(%sm ',slot-name ,stream)))
       ,@body)))

(defmacro sm (slot-name stream)
  "Access the named slot in Stream."
  (warn "Using ~S macro outside ~S." 'sm 'with-stream-class)
  `(slot-value ,stream ',slot-name))

(defmacro funcall-stm-handler (slot-name stream &rest args)
  "Call the strategy function named by Slot-Name on Stream."
  (let ((s (gensym)))
    `(let ((,s ,stream))
       (funcall (sm ,slot-name ,s) ,s ,@args))))

(defmacro funcall-stm-handler-2 (slot-name arg1 stream &rest args)
  "Call the strategy function named by Slot-Name on Stream."
  (let ((s (gensym)))
    `(let ((,s ,stream))
       (funcall (sm ,slot-name ,s) ,arg1 ,s ,@args))))

(defmacro add-stream-instance-flags (stream &rest flags)
  "Set the given Flags in Stream."
  (let ((s (gensym "STREAM")))
    `(let ((,s ,stream))
       (with-stream-class (simple-stream ,s)
         (add-stream-instance-flags ,s ,@flags)))))

(defmacro remove-stream-instance-flags (stream &rest flags)
  "Clear the given Flags in Stream."
  (let ((s (gensym "STREAM")))
    `(let ((,s ,stream))
       (with-stream-class (simple-stream ,s)
         (remove-stream-instance-flags ,s ,@flags)))))

(defmacro any-stream-instance-flags (stream &rest flags)
  "Determine whether any one of the Flags is set in Stream."
  (let ((s (gensym "STREAM")))
    `(let ((,s ,stream))
       (with-stream-class (simple-stream ,s)
         (any-stream-instance-flags ,s ,@flags)))))

(defmacro simple-stream-dispatch (stream single dual string)
  (let ((s (gensym "STREAM")))
    `(let ((,s ,stream))
       (with-stream-class (simple-stream ,s)
         (let ((%flags (sm %flags ,s)))
           (cond ((zerop (logand %flags ,(%flags '(:string :dual))))
                  ,single)
                 ((zerop (logand %flags ,(%flags '(:string))))
                  ,dual)
                 (t
                  ,string)))))))

(defmacro simple-stream-dispatch-2 (stream non-string string)
  (let ((s (gensym "STREAM")))
    `(let ((,s ,stream))
       (with-stream-class (simple-stream ,s)
         (let ((%flags (sm %flags ,s)))
           (cond ((zerop (logand %flags ,(%flags '(:string))))
                  ,non-string)
                 (t
                  ,string)))))))


;;;; The following two forms are for Franz source-compatibility,
;;;; disabled at the moment.

#+nil
(defpackage "EXCL"
  (:use "SB-SIMPLE-STREAMS")
  (:import-from "SB-SIMPLE-STREAMS"
        "BUFFER" "BUFFPOS" "BUFFER-PTR"
        "OUT-BUFFER" "MAX-OUT-POS"
        "INPUT-HANDLE" "OUTPUT-HANDLE"
        "MELDED-STREAM"
        "J-READ-CHARS"))

#+nil
(provide :iodefs)
