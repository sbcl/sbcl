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

(define-type-class named :enumerable nil :might-contain-other-types nil)

(macrolet ((frob (type global-sym)
             (let* ((name-hash (%sxhash-simple-string (string type)))
                    ;; Toggle some bits so that the hash is not equal to the hash
                    ;; for a classoid of this name (relevant for named type T only)
                    (perturbed-bit-string
                      (let ((string (format nil "~32,'0b" name-hash)))
                        (concatenate 'string
                                     (subseq string 0 22) (reverse (subseq string 22)))))
                    (bits `(make-ctype-bits
                            'named
                            ,(parse-integer perturbed-bit-string :radix 2))))
               (declare (ignorable bits)) ; not used in XC
               `(progn
                  #+sb-xc-host
                  (progn (defvar ,global-sym (!make-named-type ,bits ',type))
                         ;; Make it known as a constant in the cross-compiler.
                         (setf (info :variable :kind ',global-sym) :constant))
                  (!cold-init-forms
                   #+sb-xc (sb-impl::%defconstant ',global-sym ,(symbol-value global-sym)
                                                  (sb-c:source-location))
                   (setf (info :type :builtin ',type) #+sb-xc-host ,global-sym #-sb-xc-host ,(symbol-value global-sym)
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

;;; Nothing can see element 1 of **PRIMITIVE-OBJECT-LAYOUTS** except the special case
;;; in x86-64 LAYOUT-OF. How do we know that? Because any object header word which has
;;; a 1 in its least-significant-byte represents a GC forwarding pointer, and would
;;; indicate heap corruption if you could read said word from user code.
(defconstant index-of-layout-for-null 1)
#-sb-xc-host
(!cold-init-forms
;; Genesis allocates this vector in static space for #+x86-64
#-x86-64 (setq **primitive-object-layouts** (make-array 256))
(map-into **primitive-object-layouts**
          (lambda (name) (classoid-layout (find-classoid name)))
          #.(let ((table (make-array 256 :initial-element 'sb-kernel::random-class)))
              (dolist (x sb-kernel::*builtin-classoids*)
                (destructuring-bind (name &key codes &allow-other-keys) x
                  (dolist (code codes)
                    (setf (svref table code) name))))
              ;; widetag-of can return n-widetag-bits-long result for immediates/conses/functions.
              (loop for i from sb-vm:list-pointer-lowtag by (* 2 sb-vm:n-word-bytes)
                    below 256
                    do (setf (aref table i) 'cons))
              (loop for i from sb-vm:fun-pointer-lowtag by (* 2 sb-vm:n-word-bytes)
                    below 256
                    do (setf (aref table i) 'function))
              (loop for i from sb-vm:even-fixnum-lowtag by (ash 1 sb-vm:n-fixnum-tag-bits)
                    below 256
                    do (setf (aref table i) 'fixnum))
              (setf (aref table index-of-layout-for-null) 'null)
              table)))

(!defun-from-collected-cold-init-forms !primordial-type-cold-init)
