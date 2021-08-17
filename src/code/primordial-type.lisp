;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

(!begin-collecting-cold-init-forms)

;; %DEFCONSTANT needs this to be bound.
(!cold-init-forms
 (setq sb-c::*compile-time-eval* nil))

(define-type-class named :enumerable nil :might-contain-other-types nil)

(macrolet ((frob (type global-sym)
             (let* ((name-hash (%sxhash-simple-string (string type)))
                    ;; Toggle some bits so that the hash is not equal to the hash
                    ;; for a classoid of this name (relevant for named type T only)
                    (perturbed-bit-string
                      (let ((string (format nil "~32,'0b" name-hash)))
                        (concatenate 'string
                                     (subseq string 0 22) (reverse (subseq string 22)))))
                    (bits `(pack-interned-ctype-bits
                            'named
                            ,(parse-integer perturbed-bit-string :radix 2)
                            ,(case type
                               ((*) 31)
                               ((nil t) (sb-vm::saetp-index-or-lose type))
                               (t nil)))))
               (declare (ignorable bits)) ; not used in XC
               `(progn
                  #+sb-xc-host
                  (progn (defvar ,global-sym (!make-named-type ,bits ',type))
                         ;; Make it known as a constant in the cross-compiler.
                         (setf (info :variable :kind ',global-sym) :constant))
                  (!cold-init-forms
                   #+sb-xc (sb-impl::%defconstant ',global-sym ,global-sym
                                                  (sb-c:source-location))
                   (setf (info :type :builtin ',type) ,global-sym
                         (info :type :kind ',type) :primitive))))))
  ;; KLUDGE: In ANSI, * isn't really the name of a type, it's just a
  ;; special symbol which can be stuck in some places where an
  ;; ordinary type can go, e.g. (ARRAY * 1) instead of (ARRAY T 1).
  ;; In SBCL it also used to denote universal VALUES type.
  (frob * *wild-type*)
  (frob nil *empty-type*)
  (frob t *universal-type*)
  ;; new in sbcl-0.9.5: these used to be CLASSOID types, but that
  ;; view of them was incompatible with requirements on the MOP
  ;; metaobject class hierarchy: the INSTANCE and
  ;; FUNCALLABLE-INSTANCE types are disjoint (instances have
  ;; instance-pointer-lowtag; funcallable-instances have
  ;; fun-pointer-lowtag), while FUNCALLABLE-STANDARD-OBJECT is
  ;; required to be a subclass of STANDARD-OBJECT.  -- CSR,
  ;; 2005-09-09
  (frob instance *instance-type*)
  (frob funcallable-instance *funcallable-instance-type*)
  ;; new in sbcl-1.0.3.3: necessary to act as a join point for the
  ;; extended sequence hierarchy.  (Might be removed later if we use
  ;; a dedicated FUNDAMENTAL-SEQUENCE class for this.)
  (frob extended-sequence *extended-sequence-type*))

#-sb-xc-host
(progn
;;; a vector that maps widetags to layouts, used for quickly finding
;;; the layouts of built-in classes
(define-load-time-global **primitive-object-layouts** nil)
(declaim (type simple-vector **primitive-object-layouts**)))

#-sb-xc-host
(!cold-init-forms

;; This vector is allocated in immobile space when possible. There isn't
;; a way to do that from lisp, so it's special-cased in genesis.
#-immobile-space (setq **primitive-object-layouts** (make-array 256))
;; If #+metaspace, we can't generally store layouts in heap objects except in
;; the instance header, but this vector can because it too will go in metaspace.
(map-into **primitive-object-layouts**
          (lambda (name) (wrapper-friend (classoid-wrapper (find-classoid name))))
          #.(let ((table (make-array 256 :initial-element 'sb-kernel::random-class)))
              (dolist (x sb-kernel::*builtin-classoids*)
                (destructuring-bind (name &key codes &allow-other-keys) x
                  (dolist (code codes)
                    (setf (svref table code) name))))
              (loop for i from sb-vm:list-pointer-lowtag by (* 2 sb-vm:n-word-bytes)
                      below 256
                    do (setf (aref table i) 'cons))
              (loop for i from sb-vm:even-fixnum-lowtag by (ash 1 sb-vm:n-fixnum-tag-bits)
                      below 256
                    do (setf (aref table i) 'fixnum))
              table)))

(!defun-from-collected-cold-init-forms !primordial-type-cold-init)
