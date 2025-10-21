;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

;;;; SB-DI::FORM-NUMBER-TRANSLATIONS and SB-C::SUB-FIND-SOURCE-PATHS
;;;; are coupled and must agree, but the lack of coupling is mostly
;;;; invisible to the system, affecting primarily developer tools
;;;; which map code to locations in sources.  This file provides some
;;;; assurance that we don't change one of the implementations without
;;;; changing the other.

(defun translations (form)
  (sb-di::form-number-translations form 0))

(defun source-paths (form)
  (let ((sb-c::*source-paths* (make-hash-table :test 'eq))
        (sb-c::*current-form-number* 0))
    (sb-c::sub-find-source-paths form (list 0))
    (let (result)
      (sb-int:dohash ((k v) sb-c::*source-paths* :result result)
        (declare (ignore k))
        (push (cdr v) result)))))

;;; It's not actually clear to me what the coupling /should/ be.  The
;;; source paths contain extra entries compared with the form number
;;; translations, but somewhat bizarrely those extra entries are
;;; associated with a form number that is one above what I would
;;; expect (that is, they seem logically attached to the "next"
;;; depth-first number rather than the "current" one).  What does seem
;;; to be necessary is that all entries in TRANSLATIONS should have a
;;; corresponding entry in SOURCE-PATHS.

(defun find-unfound-translations (translations source-paths)
  (let ((unfound-translations nil))
    (sb-int:dovector (tr translations)
      (unless (find tr source-paths :test #'equal)
        (push tr unfound-translations)))
    unfound-translations))

(defun check-consistency (form)
  (let ((translations (translations form))
        (source-paths (source-paths form)))
    (assert (null (find-unfound-translations translations source-paths)))))

(with-test (:name (:static macrolet :check-consistency))
  (check-consistency '(macrolet ((def (x y) `(defun ,x (1+ ,y)))) (def ffloor) (def fceiling))))

(with-test (:name (:static :once-only :check-consistency))
  (check-consistency '(defmacro once-only (specs &body body)
                       (named-let frob ((specs specs)
                                        (body body))
                         (if (null specs)
                             `(progn ,@body)
                             (let ((spec (first specs)))
                               ;; FIXME: should just be DESTRUCTURING-BIND of SPEC
                               (unless (proper-list-of-length-p spec 2)
                                 (error "malformed ONCE-ONLY binding spec: ~S" spec))
                               (let* ((name (first spec))
                                      (exp-temp (gensym "ONCE-ONLY")))
                                 `(let ((,exp-temp ,(second spec))
                                        (,name (gensym ,(symbol-name name))))
                                    `(let ((,,name ,,exp-temp))
                                       ,,(frob (rest specs) body))))))))))

(with-test (:name (:static :aver :check-consistency))
  (check-consistency '(defmacro aver (expr)
                       ;; Don't hold on to symbols, helping shake-packages.
                       (labels ((replace-symbols (expr)
                                  (typecase expr
                                    (null expr)
                                    (symbol
                                     (symbol-name expr))
                                    (cons
                                     (cons (replace-symbols (car expr))
                                           (replace-symbols (cdr expr))))
                                    (t
                                     expr))))
                         `(unless ,expr
                            (%failed-aver ',(replace-symbols expr)))))))
