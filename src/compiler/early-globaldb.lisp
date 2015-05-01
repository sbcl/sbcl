;;;; This file contains stuff that was split out from 'globaldb.lisp'
;;;; to satisfy build-order constraints.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;; Given the presence of docstrings and source locations,
;;; this logic arguably belongs to the runtime kernel, not the compiler,
;;; but such nuance isn't hugely important.
(in-package "SB!C")

;;; At run time, we represent the type of a piece of INFO in the globaldb
;;; by a small integer between 1 and 63.  [0 is reserved for internal use.]
(defconstant info-number-bits 6)
(deftype info-number () `(unsigned-byte ,info-number-bits))

;;; A map from info-number to its META-INFO object.
;;; The reverse mapping is obtained by reading the META-INFO.
(declaim (type (simple-vector #.(ash 1 info-number-bits)) *info-types*))
(!defglobal *info-types*
            (make-array (ash 1 info-number-bits) :initial-element nil))

;;; Compiler macros for INFO functions.
;;;
;;; These are defined ASAP so that when building the cross-compiler, all calls
;;; occurring after compilation of "globaldb" (for known constant meta-info)
;;; are transformed; and when executing the cross-compiler, *all* inlineable
;;; calls for known constant meta-info are transformed;
;;; and when running target code, calls with legal constants for the first two
;;; arguments are transformed.
(macrolet ((def (name lambda-list form)
             (assert (and (member 'category lambda-list)
                          (member 'kind lambda-list)))
             `(define-compiler-macro ,name ,(append '(&whole .whole.) lambda-list)
                (if (and (keywordp category) (keywordp kind))
                    ;; In the target Lisp, it's a STYLE-WARNING if this macro
                    ;; defers to a full call to #'INFO.
                    ;; If the cross-compilation host, if any info-type is
                    ;; defined, then it's an error not to find the meta-info.
                    ;; If no info-types are defined, silently defer.
                    (let ((meta-info
                           (and #+sb-xc-host (find-if #'identity *info-types*)
                                (meta-info category kind #-sb-xc-host nil))))
                      (if meta-info
                          ,form
                          (progn
                            #-sb-xc-host
                            (style-warn "(INFO ~S ~S) will fail at runtime."
                                        category kind)
                            .whole.)))
                    .whole.))))

  (def info (category kind name)
    `(truly-the (values ,(meta-info-type-spec meta-info) boolean)
                (get-info-value ,name ,(meta-info-number meta-info))))

  (def (setf info) (new-value category kind name)
    (let* (#+sb-xc-host (sb!xc:*gensym-counter* sb!xc:*gensym-counter*)
           (tin (meta-info-number meta-info)) ; info-type id number
           (type-spec (meta-info-type-spec meta-info))
           (check
            (when (meta-info-validate-function meta-info)
              ;; is (or ... null), but non-null at macroexpansion time
              ;; implies non-null at runtime.
              `(truly-the function
                (meta-info-validate-function
                 (truly-the meta-info (svref *info-types* ,tin)))))))
      (with-unique-names (new)
        `(let ((,new ,new-value))
           ;; enforce type-correctness regardless of enclosing policy
           (let ((,new (locally (declare (optimize (safety 3)))
                         (the ,type-spec ,new))))
             ,@(when check
                 `((funcall ,check ,name ,new)))
             (set-info-value ,name ,tin ,new))))))

  (def clear-info (category kind name)
    `(clear-info-values ,name '(,(meta-info-number meta-info)))))
