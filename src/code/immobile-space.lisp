;;;; Reorganization of immobile code space.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

(defun order-by-in-degree ()
  (let ((compiler-stuff (make-hash-table :test 'eq))
        (other-stuff (make-hash-table :test 'eq)))
    (flet ((pick-table (fun-name)
             (if (symbolp fun-name)
                 (let ((package (symbol-package fun-name)))
                   (if (member package
                               (load-time-value
                                (cons sb-assem::*backend-instruction-set-package*
                                      (mapcar 'find-package
                                              '("SB-C" "SB-VM" "SB-FASL"
                                                "SB-ASSEM" "SB-DISASSEM"
                                                "SB-REGALLOC")))
                                t))
                       compiler-stuff
                       other-stuff))
                 other-stuff))
           (hashtable-keys-sorted (table)
             (mapcar #'car
              (sort (%hash-table-alist table)
                    (lambda (a b)
                      (cond ((> (cdr a) (cdr b)) t) ; higher in-degree
                            ((< (cdr a) (cdr b)) nil) ; lower in-degree
                            ;; break ties by name, and failing that,
                            ;; by address (which = random)
                            (t
                             (let ((name1
                                    (%simple-fun-name (%code-entry-point (car a) 0)))
                                   (name2
                                    (%simple-fun-name (%code-entry-point (car b) 0))))
                               (if (and (symbolp name1) (symbol-package name1)
                                        (symbolp name2) (symbol-package name2))
                                   (let ((p1 (package-name (symbol-package name1)))
                                         (p2 (package-name (symbol-package name2))))
                                     (cond ((string< p1 p2) t)
                                           ((string> p1 p2) nil)
                                           ((string< name1 name2))))
                                   (< (get-lisp-obj-address (car a))
                                      (get-lisp-obj-address (car b))))))))))))
      (sb-vm::map-allocated-objects
       (lambda (obj type size)
         size
         (when (= type sb-vm:code-header-widetag)
           (loop for i from sb-vm:code-constants-offset
                 below (code-header-words obj)
                 do (let ((ref (code-header-ref obj i))
                          (fun))
                      (when (and (fdefn-p ref)
                                 (simple-fun-p (setq fun (fdefn-fun ref)))
                                 (immobile-space-obj-p fun))
                        (let* ((code (fun-code-header fun))
                               (ht (pick-table (%simple-fun-name
                                                (%code-entry-point code 0)))))
                          (incf (gethash code ht 0))))))))
       :immobile)
      (append (hashtable-keys-sorted other-stuff)
              (hashtable-keys-sorted compiler-stuff)))))

;;; Passing your own toplevel functions as the root set
;;; will encourage the defrag procedure to place them early
;;; in the space, which should be better than leaving the
;;; organization to random chance.
;;; Note that these aren't roots in the GC sense, just a locality sense.
(defun choose-code-component-order (&optional roots)
  (let ((ordering (make-array 10000 :adjustable t :fill-pointer 0))
        (hashset (make-hash-table :test 'eq)))

    ;; Place assembler routines first.
    (let ((code sb-fasl:*assembler-routines*))
      (setf (gethash code hashset) t)
      (vector-push-extend code ordering))

    (labels ((visit (thing)
               (typecase thing
                 (code-component (visit-code thing))
                 (simple-fun (visit-code (fun-code-header thing)))
                 (closure (visit (%closure-fun thing)))
                 (symbol (when (and (fboundp thing)
                                    (not (special-operator-p thing))
                                    (not (macro-function thing)))
                           (visit (symbol-function thing))))))
             (visit-code (code-component)
               (when (or (not (immobile-space-obj-p code-component))
                         (gethash code-component hashset))
                 (return-from visit-code))
               (setf (gethash code-component hashset) t)
               (vector-push-extend code-component ordering)
               (loop for i from sb-vm:code-constants-offset
                     below (code-header-words code-component)
                     do (let ((obj (code-header-ref code-component i)))
                          (typecase obj
                            (fdefn  (awhen (fdefn-fun obj) (visit it)))
                            (symbol (visit obj))
                            (vector (map nil #'visit obj)))))))
      (mapc #'visit
            (mapcan (lambda (x)
                      (let ((f (coerce x 'function)))
                        (when (simple-fun-p f)
                          (list (fun-code-header f)))))
                    (or roots '(read eval print compile)))))

    (dolist (code (order-by-in-degree))
      (unless (gethash code hashset)
        (setf (gethash code hashset) t)
        (vector-push-extend code ordering)))

    (sb-vm::map-allocated-objects
     (lambda (obj type size)
       (declare (ignore size))
       (when (and (= type sb-vm:code-header-widetag)
                  (not (typep (%code-debug-info obj) 'function))
                  (not (gethash obj hashset)))
         (setf (gethash obj hashset) t)
         (vector-push-extend obj ordering)))
     :immobile)

    (let* ((n (length ordering))
           (array (make-alien int (1+ (* n 2)))))
      (loop for i below n
            do (setf (deref array (* i 2))
                     (get-lisp-obj-address (aref ordering i))))
      (setf (deref array (* n 2)) 0) ; null-terminate the array
      (setf (extern-alien "code_component_order" unsigned)
            (sap-int (alien-value-sap array)))))

  (multiple-value-bind (index relocs) (sb-vm::collect-immobile-code-relocs)
    (let* ((n (length index))
           (array (make-alien int n)))
      (dotimes (i n) (setf (deref array i) (aref index i)))
      (setf (extern-alien "immobile_space_reloc_index" unsigned)
            (sap-int (alien-value-sap array))))
    (let* ((n (length relocs))
           (array (make-alien int n)))
      (dotimes (i n) (setf (deref array i) (aref relocs i)))
      (setf (extern-alien "immobile_space_relocs" unsigned)
            (sap-int (alien-value-sap array))))))
