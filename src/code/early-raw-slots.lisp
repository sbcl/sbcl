;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;; This file has to be loaded during cold-init as early as you'd like to
;;; have any defstructs that use raw slots.  %COMPILER-DEFSTRUCT needs the
;;; raw-slot-data-list both at compile-time and load-time.

;; To utilize a word-sized slot in a defstruct without having to resort to
;; writing (myslot :type (unsigned-byte #.sb!vm:n-word-bits)), or even
;; worse (:type #+sb-xc-host <sometype> #-sb-xc-host <othertype>),
;; these abstractions are provided as soon as the raw slots defs are.
;; 'signed-word' is here for companionship - slots of that type are not raw.
(def!type sb!vm:word () `(unsigned-byte ,sb!vm:n-word-bits))
(def!type sb!vm:signed-word () `(signed-byte ,sb!vm:n-word-bits))

;; information about how a slot of a given DSD-RAW-TYPE is to be accessed
(eval-when (:compile-toplevel :load-toplevel :execute)
(defstruct (raw-slot-data
            (:copier nil)
            (:predicate nil))
  ;; the raw slot type, or T for a non-raw slot
  ;;
  ;; (Non-raw slots are in the ordinary place you'd expect, directly
  ;; indexed off the instance pointer.  Raw slots are indexed from the end
  ;; of the instance and skipped by GC.)
  (raw-type (missing-arg) :type (or symbol cons) :read-only t)
  ;; What operator is used to access a slot of this type?
  (accessor-name (missing-arg) :type symbol :read-only t)
  (init-vop (missing-arg) :type symbol :read-only t)
  ;; How many words are each value of this type?
  (n-words (missing-arg) :type (and index (integer 1)) :read-only t)
  ;; Necessary alignment in units of words.  Note that instances
  ;; themselves are aligned by exactly two words, so specifying more
  ;; than two words here would not work.
  (alignment 1 :type (integer 1 2) :read-only t)
  (comparer (missing-arg) :type function :read-only t)))

#!-sb-fluid (declaim (freeze-type raw-slot-data))

(defglobal *raw-slot-data-list*
  (macrolet ((make-comparer (accessor-name)
               `(lambda (index x y)
                  (declare (optimize speed (safety 0)))
                  (= (,accessor-name x index)
                     (,accessor-name y index)))))
    (let ((double-float-alignment
            ;; white list of architectures that can load unaligned doubles:
            #!+(or x86 x86-64 ppc) 1
            ;; at least sparc, mips and alpha can't:
            #!-(or x86 x86-64 ppc) 2))
      (list
       (make-raw-slot-data :raw-type 'sb!vm:word
                           :accessor-name '%raw-instance-ref/word
                           :init-vop 'sb!vm::raw-instance-init/word
                           :n-words 1
                           :comparer (make-comparer %raw-instance-ref/word))
       (make-raw-slot-data :raw-type 'single-float
                           :accessor-name '%raw-instance-ref/single
                           :init-vop 'sb!vm::raw-instance-init/single
                           ;; KLUDGE: On 64 bit architectures, we
                           ;; could pack two SINGLE-FLOATs into the
                           ;; same word if raw slots were indexed
                           ;; using bytes instead of words.  However,
                           ;; I don't personally find optimizing
                           ;; SINGLE-FLOAT memory usage worthwile
                           ;; enough.  And the other datatype that
                           ;; would really benefit is (UNSIGNED-BYTE
                           ;; 32), but that is a subtype of FIXNUM, so
                           ;; we store it unraw anyway.  :-( -- DFL
                           :n-words 1
                           :comparer (make-comparer %raw-instance-ref/single))
       (make-raw-slot-data :raw-type 'double-float
                           :accessor-name '%raw-instance-ref/double
                           :init-vop 'sb!vm::raw-instance-init/double
                           :alignment double-float-alignment
                           :n-words (/ 8 sb!vm:n-word-bytes)
                           :comparer (make-comparer %raw-instance-ref/double))
       (make-raw-slot-data :raw-type 'complex-single-float
                           :accessor-name '%raw-instance-ref/complex-single
                           :init-vop 'sb!vm::raw-instance-init/complex-single
                           :n-words (/ 8 sb!vm:n-word-bytes)
                           :comparer (make-comparer %raw-instance-ref/complex-single))
       (make-raw-slot-data :raw-type 'complex-double-float
                           :accessor-name '%raw-instance-ref/complex-double
                           :init-vop 'sb!vm::raw-instance-init/complex-double
                           :alignment double-float-alignment
                           :n-words (/ 16 sb!vm:n-word-bytes)
                           :comparer (make-comparer %raw-instance-ref/complex-double))
       #!+long-float
       (make-raw-slot-data :raw-type long-float
                           :accessor-name '%raw-instance-ref/long
                           :init-vop 'sb!vm::raw-instance-init/long
                           :n-words #!+x86 3 #!+sparc 4
                           :comparer (make-comparer %raw-instance-ref/long))
       #!+long-float
       (make-raw-slot-data :raw-type complex-long-float
                           :accessor-name '%raw-instance-ref/complex-long
                           :init-vop 'sb!vm::raw-instance-init/complex-long
                           :n-words #!+x86 6 #!+sparc 8
                           :comparer (make-comparer %raw-instance-ref/complex-long))))))

(declaim (ftype (sfunction (symbol) raw-slot-data) raw-slot-data-or-lose))
(defun raw-slot-data-or-lose (type)
  (or (car (member type *raw-slot-data-list* :key #'raw-slot-data-raw-type))
      (error "Invalid raw slot type: ~S" type)))

(defun raw-slot-words (type)
  (raw-slot-data-n-words (raw-slot-data-or-lose type)))
