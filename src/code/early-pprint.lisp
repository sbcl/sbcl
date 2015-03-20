;;;; pretty printer stuff which has to be defined early (e.g. DEFMACROs)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!PRETTY")

;;;; user interface to the pretty printer

(defmacro pprint-logical-block ((stream-symbol
                                 object
                                 &rest keys
                                 &key (prefix nil prefixp)
                                      (per-line-prefix nil per-line-prefix-p)
                                      (suffix ""))
                                &body body)
  #!+sb-doc
  "Group some output into a logical block. STREAM-SYMBOL should be either a
   stream, T (for *TERMINAL-IO*), or NIL (for *STANDARD-OUTPUT*). The printer
   control variable *PRINT-LEVEL* is automatically handled."
  (let ((prefix (cond ((and prefixp per-line-prefix-p)
                       (error "cannot specify values for both PREFIX and PER-LINE-PREFIX."))
                      (prefixp prefix)
                      (per-line-prefix-p per-line-prefix)))
        (proc (make-symbol "PPRINT-BLOCK"))
        (list (and object (make-symbol "LIST")))
        (state (make-symbol "STATE"))
        (stream-var (case stream-symbol
                      ((nil) '*standard-output*)
                      ((t) '*terminal-io*)
                      (t stream-symbol)))
        (bindings))
    ;; This is not a function, but to the degree possible should have usual
    ;; evaluation order. No bothering with duplicated keyword args,
    ;; or :allow-other-keys nonsense.
    (unless (and (constantp prefix) (constantp suffix))
      (loop (multiple-value-bind (indicator value tail)
                (get-properties keys '(:prefix :per-line-prefix :suffix))
              (if (not indicator) (return))
              (setq keys (cddr tail))
              (unless (assoc indicator bindings :test 'string=) ; dup
                (let ((tmp (copy-symbol indicator)))
                  (setq bindings (nconc bindings (list (list tmp value))))
                  (if (eq indicator :suffix)
                      (setq suffix tmp)
                      (setq prefix tmp))))))
      (when object
        (let ((tmp (make-symbol "OBJ")))
          (setq bindings (acons tmp (list object) bindings) object tmp))))
    `(dx-flet ((,proc (,@(and list (list list)) ,state ,stream-var)
                 (declare (ignorable ,@(and list (list list))
                                      ,state ,stream-var))
                 (declare (disable-package-locks pprint-exit-if-list-exhausted
                                                 pprint-pop))
                 (macrolet ,(if object
                                `((pprint-exit-if-list-exhausted ()
                                    '(when (null ,list) (return-from ,proc)))
                                  (pprint-pop ()
                                    '(if (pprint-length-check ,list ,state)
                                         (pop ,list)
                                         (return-from ,proc))))
                                `((pprint-exit-if-list-exhausted ()
                                    '(return-from ,proc))
                                  (pprint-pop ()
                                    '(if (pprint-length-check* ,state)
                                         nil
                                         (return-from ,proc)))))
                   (declare (enable-package-locks pprint-exit-if-list-exhausted
                                                  pprint-pop))
                   ,@body)))
       (let ,bindings
         (call-logical-block-printer #',proc ,stream-symbol
                                     ,prefix ,per-line-prefix-p ,suffix
                                     ,@(if object (list object)))))))

(defmacro pprint-exit-if-list-exhausted ()
  #!+sb-doc
  "Cause the closest enclosing use of PPRINT-LOGICAL-BLOCK to return
   if its list argument is exhausted. Can only be used inside
   PPRINT-LOGICAL-BLOCK, and only when the LIST argument to
   PPRINT-LOGICAL-BLOCK is supplied."
  (error "PPRINT-EXIT-IF-LIST-EXHAUSTED must be lexically inside ~
          PPRINT-LOGICAL-BLOCK."))

(defmacro pprint-pop ()
  #!+sb-doc
  "Return the next element from LIST argument to the closest enclosing
   use of PPRINT-LOGICAL-BLOCK, automatically handling *PRINT-LENGTH*
   and *PRINT-CIRCLE*. Can only be used inside PPRINT-LOGICAL-BLOCK.
   If the LIST argument to PPRINT-LOGICAL-BLOCK was NIL, then nothing
   is popped, but the *PRINT-LENGTH* testing still happens."
  (error "PPRINT-POP must be lexically inside PPRINT-LOGICAL-BLOCK."))
