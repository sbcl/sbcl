;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;; Verify on startup that some constants were dumped reflecting the correct
;;; action of our vanilla-host-compatible functions.
;;; For now, just SXHASH is checked.

;;; Parallelized build doesn't get the full set of data because the side effect
;;; of data recording when invoking compile-time functions aren't propagated
;;; back to process that forked the children doing the grunt work.
(defvar sb-c::*sxhash-crosscheck*
  '#.(let (pairs)
       ;; De-duplicate, which reduces the list from ~8000 entries to ~1000 entries.
       ;; But make sure that any key which is repeated has the same value
       ;; at each repetition.
       (dolist (pair sb-c::*sxhash-crosscheck* (coerce pairs 'simple-vector))
         (let ((found (assoc (car pair) pairs)))
           (if found
               (aver (= (cdr found) (cdr pair)))
               (push pair pairs))))))
(defun check-compile-time-sxhashes ()
  (loop for (object . hash) across sb-c::*sxhash-crosscheck*
        unless (= (sxhash object) hash)
        do (error "SXHASH computed wrong answer for ~S. Got ~x should be ~x"
                  object hash (sxhash object))))
(check-compile-time-sxhashes)

(eval-when (:compile-toplevel) ;; Inform genesis of all defstructs
  (with-open-file (output (sb-cold:stem-object-path "defstructs.lisp-expr"
                                                    '(:extra-artifact) :target-compile)
                          :direction :output :if-exists :supersede)
    (dolist (root '(structure-object function))
      (dolist (pair (sort (%hash-table-alist
                           (classoid-subclasses (find-classoid root)))
                          #'string<
                          ;; pair = (#<classoid> . #<layout>)
                          :key (lambda (pair) (classoid-name (car pair)))))
        (let* ((layout (cdr pair))
               (dd (layout-info layout)))
          (cond
           (dd
            (let* ((*print-pretty* nil) ; output should be insensitive to host pprint
                   (*print-readably* t)
                   (classoid-name (classoid-name (car pair)))
                   (*package* (cl:symbol-package classoid-name)))
              (format output "~/sb-ext:print-symbol-with-prefix/ ~S (~%"
                      classoid-name
                      (list* (the (unsigned-byte 16) (layout-flags layout))
                             (layout-depthoid layout)
                             (map 'list #'layout-classoid-name
                                  (layout-inherits layout))))
              (dolist (dsd (dd-slots dd) (format output ")~%"))
                (format output "  (~d ~S ~S)~%"
                        (sb-kernel::dsd-bits dsd)
                        (dsd-name dsd)
                        (dsd-accessor-name dsd)))))
           (t
            (error "Missing DD for ~S" pair))))))
    (format output ";; EOF~%")))
