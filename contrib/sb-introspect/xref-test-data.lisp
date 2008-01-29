(defvar *a* nil)
(defconstant +z+ 'zzz)

(defun foo () 1)
(defun bar (x) x)

;; Should:
;;   reference *a*
;;   call bar
;;   not call foo
;;   not call xref/2
(defun xref/1 ()
  (flet ((foo ()
           (bar *a*)))
    (flet ((xref/2 ()
             1))
      (foo)
      (xref/2))))

;; Should:
;;   reference *a*, set *a*, bind *a*
;;   call xref/1
;;   not bind b
(defun xref/2 ()
  (setf *a* *a*)
  (let* ((b 1)
         (*a* b))
    (when nil
      (xref/1))))

(let ((x 1))
  ;; Should:
  ;;   call bar
  ;;   not reference *a*
  (defun xref/3 ()
    (bar x))
  ;; Should:
  ;;   not call bar
  ;;   reference *a*
  (defun xref/4 ()
    (setf x *a*)))


(flet ((z ()
         (xref/2)))
  ;; Should:
  ;;   call xref/2
  ;;   not call z
  (defun xref/5 ()
    (z))
  ;; Should:
  ;;   call xref/2
  ;;   not call z
  (defun xref/6 ()
    (z)))

(defun xref/7 ()
  (flet ((a ()
           (xref/6)))
    #'a))

;; call xref/2
(let ((a 1))
  (defvar *b* (or (xref/2) a)))

;; call xref/6
(defvar *c* (xref/6))

;; call xref/2 twice (not three times)
(defun xref/8 ()
  (flet ((a ()
           (xref/2)))
    (a)
    (a)
    (xref/2)))

;; Methods work, even ones with lots of arguments.
(defmethod xref/10 (a b c d e f g h (i fixnum))
  (xref/2))

;; Separate methods are indeed separate
(defmethod xref/11 ((a fixnum))
  (declare (ignore a))
  (xref/2))

(defmethod xref/11 ((a float))
  (declare (ignore a))
  (xref/3))

(declaim (inline inline/1))
(defun inline/1 ()
  (xref/3)
  (values +z+ *a*))

(eval-when (:compile-toplevel :load-toplevel)
  (defun xref/12 ()
    (flet ((a ()
             ;; Counts as calling xref/2
             (xref/2)))
      (declare (inline a))
      (a)
      ;; Doesn't count as calling xref/3, or referring to +z+ / *a*
      (inline/1))))

;; last node of block should also be taken into account
(defun xref/13 (x)
  (setf *a* x))

(defun xref/14 ()
  *a*)

;; calling a function in a macro body
(defmacro macro/1 ()
  (when nil
    (xref/12))
  nil)

;; expanding a macro
(defun macro-use/1 ()
  (macro/1))

;; expanding a macro in an flet/labels
(defun macro-use/2 ()
  (flet ((inner-flet ()
           (macro/1)))
    (inner-flet)))

;; expanding a macro in an toplevel flet/labels
(flet ((outer-flet ()
         (macro/1)))
  (defun macro-use/3 ()
    (outer-flet)))

;; expanding a macro in an inlined flet/labels
(defun macro-use/4 ()
  (flet ((inner-flet ()
           (macro/1)))
    (declare (inline inner-flet))
    (inner-flet)))

(declaim (inline inline/2))
(defun inline/2 ()
  (macro/1))

;; Inlining inline/3 doesn't count as macroexpanding macro/1
(defun macro-use/5 ()
  (inline/2))

;;; Code in the macrolet definition bodies is currently not considered
;;; at all for XREF. Maybe it should be, but it's slightly tricky to
;;; implement.
#+nil
(progn
  (defun macrolet/1 ()
    (macrolet ((a ()
                 (inline/2)
               1))
      (a)))
  (defun macrolet/2 ()
    (macrolet ((inner-m ()
                 (macro/1)))
      (inner-m))))

;;; Test references to / from compiler-macros
