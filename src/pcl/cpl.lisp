;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

;;;; copyright information from original PCL sources:
;;;;
;;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;;; All rights reserved.
;;;;
;;;; Use and copying of this software and preparation of derivative works based
;;;; upon this software are permitted. Any distribution of this software or
;;;; derivative works must comply with all applicable United States export
;;;; control laws.
;;;;
;;;; This software is made available AS IS, and Xerox Corporation makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.

(in-package "SB-PCL")

;;;; COMPUTE-CLASS-PRECEDENCE-LIST and friends

;;; Knuth section 2.2.3 has some interesting notes on this.
;;;
;;; What appears here is basically the algorithm presented there.
;;;
;;; The key idea is that we use class-precedence-description (CPD) structures
;;; to store the precedence information as we proceed. The CPD structure for
;;; a class stores two critical pieces of information:
;;;
;;;  - a count of the number of "reasons" why the class can't go
;;;    into the class precedence list yet.
;;;
;;;  - a list of the "reasons" this class prevents others from
;;;    going in until after it
;;
;;; A "reason" is essentially a single local precedence constraint. If a
;;; constraint between two classes arises more than once it generates more
;;; than one reason. This makes things simpler, linear, and isn't a problem
;;; as long as we make sure to keep track of each instance of a "reason".
;;;
;;; This code is divided into three phases.
;;;
;;;  - the first phase simply generates the CPD's for each of the class
;;;    and its superclasses. The remainder of the code will manipulate
;;;    these CPDs rather than the class objects themselves. At the end
;;;    of this pass, the CPD-SUPERS field of a CPD is a list of the CPDs
;;;    of the direct superclasses of the class.
;;;
;;;  - the second phase folds all the local constraints into the CPD
;;;    structure. The CPD-COUNT of each CPD is built up, and the
;;;    CPD-AFTER fields are augmented to include precedence constraints
;;;    from the CPD-SUPERS field and from the order of classes in other
;;;    CPD-SUPERS fields.
;;;
;;;    After this phase, the CPD-AFTER field of a class includes all the
;;;    direct superclasses of the class plus any class that immediately
;;;    follows the class in the direct superclasses of another. There
;;;    can be duplicates in this list. The CPD-COUNT field is equal to
;;;    the number of times this class appears in the CPD-AFTER field of
;;;    all the other CPDs.
;;;
;;;  - In the third phase, classes are put into the precedence list one
;;;    at a time, with only those classes with a CPD-COUNT of 0 being
;;;    candidates for insertion. When a class is inserted , every CPD
;;;    in its CPD-AFTER field has its count decremented.
;;;
;;;    In the usual case, there is only one candidate for insertion at
;;;    any point. If there is more than one, the specified tiebreaker
;;;    rule is used to choose among them.

(defmethod compute-class-precedence-list ((root class))
  (compute-std-cpl root (class-direct-superclasses root)))

(defstruct (class-precedence-description
            (:conc-name nil)
            (:constructor make-cpd ())
            (:copier nil))
  (cpd-class  nil)
  (cpd-supers ())
  (cpd-after  ())
  (cpd-count  0))

(defun compute-std-cpl (class supers)
  (cond
    ;; the first two branches of this COND are implementing an
    ;; optimization for single inheritance.
    ((and (null supers)
          (not (forward-referenced-class-p class)))
     (list class))
    ((and (car supers)
          (null (cdr supers))
          (not (forward-referenced-class-p (car supers))))
     (cons class
           (compute-std-cpl (car supers)
                            (class-direct-superclasses (car supers)))))
    (t
     (multiple-value-bind (all-cpds nclasses)
         (compute-std-cpl-phase-1 class supers)
       (compute-std-cpl-phase-2 all-cpds)
       (compute-std-cpl-phase-3 class all-cpds nclasses)))))

(defvar *compute-std-cpl-class->entry-table-size* 60)

(defun compute-std-cpl-phase-1 (class supers)
  (let ((nclasses 0)
        (all-cpds ())
        (table (make-hash-table :size *compute-std-cpl-class->entry-table-size*
                                :test #'eq)))
    (declare (fixnum nclasses))
    (labels ((get-cpd (c)
               (ensure-gethash c table (make-cpd)))
             (walk (c supers)
               (declare (special *allow-forward-referenced-classes-in-cpl-p*))
               (if (and (forward-referenced-class-p c)
                        (not *allow-forward-referenced-classes-in-cpl-p*))
                   (cpl-forward-referenced-class-error class c)
                   (let ((cpd (get-cpd c)))
                     (unless (cpd-class cpd)    ;If we have already done this
                                                ;class before, we can quit.
                       (setf (cpd-class cpd) c)
                       (incf nclasses)
                       (push cpd all-cpds)
                       (setf (cpd-supers cpd) (mapcar #'get-cpd supers))
                       (dolist (super supers)
                         (walk super (class-direct-superclasses super))))))))
      (walk class supers)
      (values all-cpds nclasses))))

(defun compute-std-cpl-phase-2 (all-cpds)
  (dolist (cpd all-cpds)
    (let ((supers (cpd-supers cpd)))
      (when supers
        (setf (cpd-after cpd) (nconc (cpd-after cpd) supers))
        (incf (cpd-count (car supers)) 1)
        (do* ((t1 supers t2)
              (t2 (cdr t1) (cdr t1)))
             ((null t2))
          (incf (cpd-count (car t2)) 2)
          (push (car t2) (cpd-after (car t1))))))))

(defun compute-std-cpl-phase-3 (class all-cpds nclasses)
  (let ((candidates ())
        (next-cpd nil)
        (rcpl ()))

    ;; We have to bootstrap the collection of those CPD's that
    ;; have a zero count. Once we get going, we will maintain
    ;; this list incrementally.
    (dolist (cpd all-cpds)
      (when (zerop (cpd-count cpd)) (push cpd candidates)))

    (loop
      (when (null candidates)

        ;; If there are no candidates, and enough classes have been put
        ;; into the precedence list, then we are all done. Otherwise
        ;; it means there is a consistency problem.
        (if (zerop nclasses)
            (return (reverse rcpl))
            (cpl-inconsistent-error class all-cpds)))

      ;; Try to find the next class to put in from among the candidates.
      ;; If there is only one, its easy, otherwise we have to use the
      ;; famous RPG tiebreaker rule. There is some hair here to avoid
      ;; having to call DELETE on the list of candidates. I dunno if
      ;; its worth it but what the hell.
      (setq next-cpd
            (if (null (cdr candidates))
                (prog1 (car candidates)
                       (setq candidates ()))
                (block tie-breaker
                  (dolist (c rcpl)
                    (let ((supers (class-direct-superclasses c)))
                      (if (memq (cpd-class (car candidates)) supers)
                          (return-from tie-breaker (pop candidates))
                          (do ((loc candidates (cdr loc)))
                              ((null (cdr loc)))
                            (let ((cpd (cadr loc)))
                              (when (memq (cpd-class cpd) supers)
                                (setf (cdr loc) (cddr loc))
                                (return-from tie-breaker cpd))))))))))
      (decf nclasses)
      (push (cpd-class next-cpd) rcpl)
      (dolist (after (cpd-after next-cpd))
        (when (zerop (decf (cpd-count after)))
          (push after candidates))))))

;;;; support code for signalling nice error messages

(defun cpl-error (class format-string &rest format-args)
  (error "While computing the class precedence list of the class ~A.~%~A"
          (if (class-name class)
              (format nil "named ~/sb-ext:print-symbol-with-prefix/"
                      (class-name class))
              class)
          (apply #'format nil format-string format-args)))

(defun cpl-forward-referenced-class-error (class forward-class)
  (flet ((class-or-name (class)
           (if (class-name class)
               (format nil "named ~/sb-ext:print-symbol-with-prefix/"
                       (class-name class))
               class)))
    (if (eq class forward-class)
        (cpl-error class
                   "The class ~A is a forward referenced class."
                   (class-or-name class))
        (let ((names (mapcar #'class-or-name
                             (cdr (find-superclass-chain class forward-class)))))
          (cpl-error class
                     "The class ~A is a forward referenced class.~@
                      The class ~A is ~A."
                     (class-or-name forward-class)
                     (class-or-name forward-class)
                     (if (null (cdr names))
                         (format nil
                                 "a direct superclass of the class ~A"
                                 (class-or-name class))
                         (format nil
                                 "reached from the class ~A by following~@
                              the direct superclass chain through: ~A~
                              ~%  ending at the class ~A"
                                 (class-or-name class)
                                 (format nil
                                         "~{~%  the class ~A,~}"
                                         (butlast names))
                                 (car (last names)))))))))

(defun find-superclass-chain (bottom top)
  (labels ((walk (c chain)
             (if (eq c top)
                 (return-from find-superclass-chain (nreverse chain))
                 (dolist (super (class-direct-superclasses c))
                   (walk super (cons super chain))))))
    (walk bottom (list bottom))))

(defun cpl-inconsistent-error (class all-cpds)
  (let ((reasons (find-cycle-reasons all-cpds)))
    (cpl-error class
      "It is not possible to compute the class precedence list because~@
       there ~A in the local precedence relations.~@
       ~A because:~{~%  ~A~}."
      (if (cdr reasons) "are circularities" "is a circularity")
      (if (cdr reasons) "These arise" "This arises")
      (format-cycle-reasons (apply #'append reasons)))))

(defun format-cycle-reasons (reasons)
  (flet ((class-or-name (cpd)
           (let ((class (cpd-class cpd)))
             (if (class-name class)
                 (format nil "named ~/sb-ext:print-symbol-with-prefix/"
                         (class-name class))
                 class))))
    (mapcar
      (lambda (reason)
        (ecase (caddr reason)
          (:super
           (format
            nil
            "The class ~A appears in the supers of the class ~A."
            (class-or-name (cadr reason))
            (class-or-name (car reason))))
          (:in-supers
           (format
            nil
            "The class ~A follows the class ~A in the supers of the class ~A."
            (class-or-name (cadr reason))
            (class-or-name (car reason))
            (class-or-name (cadddr reason))))))
      reasons)))

(defun find-cycle-reasons (all-cpds)
  (let ((been-here ())     ; list of classes we have visited
        (cycle-reasons ()))

    (labels ((chase (path)
               (if (memq (car path) (cdr path))
                   (record-cycle (memq (car path) (nreverse path)))
                   (unless (memq (car path) been-here)
                     (push (car path) been-here)
                     (dolist (after (cpd-after (car path)))
                       (chase (cons after path))))))
             (record-cycle (cycle)
               (let ((reasons ()))
                 (do* ((t1 cycle t2)
                       (t2 (cdr t1) (cdr t1)))
                      ((null t2))
                   (let ((c1 (car t1))
                         (c2 (car t2)))
                     (if (memq c2 (cpd-supers c1))
                         (push (list c1 c2 :super) reasons)
                         (dolist (cpd all-cpds)
                           (when (memq c2 (memq c1 (cpd-supers cpd)))
                             (return
                               (push (list c1 c2 :in-supers cpd) reasons)))))))
                 (push (nreverse reasons) cycle-reasons))))

      (dolist (cpd all-cpds)
        (unless (zerop (cpd-count cpd))
          (chase (list cpd))))

      cycle-reasons)))
