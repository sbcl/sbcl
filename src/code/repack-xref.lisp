;;;; Repacking xref information exploiting occurrence frequencies

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;;; Since the most frequently referenced names (even just within SBCL
;;;; itself) are not known until the system is built, the computing
;;;; the frequencies has to be done in warm load. However, all xref
;;;; information has already been collected and stored by that
;;;; time. Therefore, after the frequencies have been computed, all
;;;; xref information is unpacked and repacked, this time exploiting
;;;; the compact encoding for the most frequent names.

(in-package "SB-C")

(labels ((functoid-simple-fun (functoid)
           (typecase functoid
             (fdefn
              (functoid-simple-fun (fdefn-fun functoid)))
             (closure
              (let ((fun (%closure-fun functoid)))
                (if (and (eq (%fun-name fun) 'sb-impl::encapsulation))
                    (functoid-simple-fun
                     (sb-impl::encapsulation-info-definition
                      (sb-impl::encapsulation-info functoid)))
                    fun)))
             ((and function (not funcallable-instance))
              (%fun-fun functoid)))))

  ;;; Note that this function is used by sb-introspect.
  (defun map-simple-funs (function)
    (let ((function (%coerce-callable-to-fun function)))
      (labels ((process (name value)
                 (awhen (functoid-simple-fun value)
                   (funcall function name it))))
        (call-with-each-globaldb-name
         (lambda (name)
           (awhen (or (and (symbolp name) (macro-function name))
                      (and (legal-fun-name-p name) (find-fdefn name)))
             (cond
               ((and (fdefn-p it)
                     (typep (fdefn-fun it) 'generic-function))
                (loop for method in (sb-mop:generic-function-methods (fdefn-fun it))
                      for fun = (sb-pcl::safe-method-fast-function method)
                      when fun do (process (sb-kernel:%fun-name fun) fun)))
               ;; Methods are already processed above
               ((and (fdefn-p it)
                     (typep (fdefn-name it)
                            '(cons (member sb-pcl::slow-method
                                    sb-pcl::fast-method)))))
               (t
                (process name it))))
           #+sb-xref-for-internals
           (let ((info (info :function :info name)))
             (when info
               (loop for transform in (fun-info-transforms info)
                     for fun = (transform-function transform)
                     ;; Defined using :defun-only and a later %deftransform.
                     unless (symbolp fun)
                     do (process transform fun))))))
        #+sb-xref-for-internals
        (dohash ((name vop) *backend-template-names*)
          (declare (ignore name))
          (let ((fun (vop-info-generator-function vop)))
            (when fun
              (process vop fun))))))))

;;; Repack all xref data vectors in the system, potentially making
;;; them compact, but without changing their meaning:
;;;
;;; 1. Go through all xref data vectors, unpacking their contents
;;;    (using the current values of
;;;    **MOST-COMMON-XREF-NAMES-BY-{INDEX,NAME}**) and determining the
;;;    COMPACT-NAME-COUNT most frequently referenced names.
;;;
;;; 2. Update **MOST-COMMON-XREF-NAMES-BY-{INDEX,NAME}** with this
;;;    information.
;;;
;;; 3. Repack all xref data vectors using the updated
;;;    **MOST-COMMON-XREF-NAMES-BY-{INDEX,NAME}**.
(defun repack-xref (&key (compact-name-count 256) verbose)
  (let ((verbose (ecase verbose
                   ((nil) 0)
                   ((1 2) verbose)
                   ((t)   2)))
        (counts '())
        (counts-by-name (make-hash-table :test #'equal))
        (all-unpacked '())
        (old-size 0)
        (new-size 0))
    (flet ((xref-size (xref)
             ;; Disregarding overhead for array headers, required
             ;; space is number of octets in nested octet-vector plus
             ;; one word for each element in outer vector.
             (+ (* sb-vm:n-word-bytes (length xref))
                (length (aref xref 0)))))

      ;; Unpack (using old values of
      ;; **MOST-COMMON-XREF-NAMES-BY-{INDEX,NAME}**) xref data and count
      ;; occurrence frequencies of names.
      (map-simple-funs
       (lambda (name fun)
         (declare (ignore name))
         (binding* ((xrefs (%simple-fun-xrefs fun) :exit-if-null)
                    (seen (make-hash-table :test #'equal))
                    (unpacked '()))
           ;; Record size of the xref data for this simple fun.
           (incf old-size (xref-size xrefs))
           (map-packed-xref-data
            (lambda (kind name number)
              ;; Count NAME, but only once for each FUN.
              (unless (gethash name seen)
                (setf (gethash name seen) t)
                (incf (cdr (ensure-gethash name counts-by-name
                                           (let ((cell (cons name 0)))
                                             (push cell counts)
                                             cell)))))
              ;; Store (KIND NAME NUMBER) tuple for repacking.
              (setf (getf unpacked kind) (nconc (getf unpacked kind)
                                                (list (cons name number)))))
            xrefs)
           (unless unpacked (break))
           ;; Store FUN and UNPACKED for repacking.
           (push (cons fun unpacked) all-unpacked))))

      ;; Update **MOST-COMMON-XREF-NAMES-BY-{INDEX,NAME}**.
      (let* ((sorted-names (mapcar #'car (stable-sort counts #'> :key #'cdr)))
             (new-names (subseq sorted-names 0 (min (length sorted-names)
                                                    compact-name-count))))
        (when (>= verbose 2)
          (format t "; Updating most frequently cross-referenced names~%")
          (pprint-logical-block (*standard-output* new-names :per-line-prefix ";   ")
            (format t "~:[no cross references~;~:*~
                       ~{~/sb-ext:print-symbol-with-prefix/~^ ~:_~}~]"
                    (coerce new-names 'list)))
          (terpri))
        (setf **most-common-xref-names-by-index** (coerce new-names 'vector))
        (let ((table **most-common-xref-names-by-name**))
          (clrhash table)
          (loop for name in new-names
             for i from 0
             do (setf (gethash name table) i))))

      ;; Repack with updated **MOST-COMMON-XREF-NAMES-BY-{INDEX,NAME}**.
      (when (>= verbose 1)
        (format t "; Repacking xref information~%"))
      (loop for (fun . unpacked) in all-unpacked do
           (let ((new-xrefs (pack-xref-data unpacked)))
             (incf new-size (xref-size new-xrefs))
             (aver (vectorp new-xrefs))
             (let ((info (%simple-fun-info fun)))
               (if (typep info '(cons t simple-vector))
                   (rplacd info new-xrefs)
                   (setf (%simple-fun-info fun) new-xrefs))))))

    (when (>= verbose 1)
      (format t ";   Old xref size ~11:D byte~:P~@
                 ;   New xref size ~11:D byte~:P~%"
              old-size new-size))))
