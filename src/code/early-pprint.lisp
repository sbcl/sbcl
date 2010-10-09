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

;;;; utilities

(defmacro with-pretty-stream ((stream-var
                               &optional (stream-expression stream-var))
                              &body body)
  (let ((flet-name (sb!xc:gensym "WITH-PRETTY-STREAM")))
    `(flet ((,flet-name (,stream-var)
              ,@body))
       (let ((stream ,stream-expression))
         (if (pretty-stream-p stream)
             (,flet-name stream)
             (catch 'line-limit-abbreviation-happened
               (let ((stream (make-pretty-stream stream)))
                 (,flet-name stream)
                 (force-pretty-output stream)))))
       nil)))

;;;; user interface to the pretty printer

(defmacro pprint-logical-block ((stream-symbol
                                 object
                                 &key
                                 (prefix nil prefixp)
                                 (per-line-prefix nil per-line-prefix-p)
                                 (suffix "" suffixp))
                                &body body)
  #!+sb-doc
  "Group some output into a logical block. STREAM-SYMBOL should be either a
   stream, T (for *TERMINAL-IO*), or NIL (for *STANDARD-OUTPUT*). The printer
   control variable *PRINT-LEVEL* is automatically handled."
  (let ((prefix (cond ((and prefixp per-line-prefix-p)
                       (error "cannot specify values for both PREFIX and PER-LINE-PREFIX."))
                      (prefixp prefix)
                      (per-line-prefix-p per-line-prefix))))
    (let ((object-var (if object (gensym) nil)))
      (once-only ((prefix-var prefix) (suffix-var suffix))
        (multiple-value-bind (stream-var stream-expression)
            (case stream-symbol
              ((nil)
               (values '*standard-output* '*standard-output*))
              ((t)
               (values '*terminal-io* '*terminal-io*))
              (t
               (values stream-symbol
                       (once-only ((stream stream-symbol))
                         `(case ,stream
                            ((nil) *standard-output*)
                            ((t) *terminal-io*)
                            (t ,stream))))))
          (let* ((block-name (sb!xc:gensym "PPRINT-LOGICAL-BLOCK-"))
                 (count-name (gensym "PPRINT-LOGICAL-BLOCK-LENGTH-"))
                 (pp-pop-name (sb!xc:gensym "PPRINT-POP-"))
                 (body
                  ;; FIXME: It looks as though PPRINT-LOGICAL-BLOCK might
                  ;; expand into a boatload of code, since DESCEND-INTO is a
                  ;; macro too. It might be worth looking at this to make
                  ;; sure it's not too bloated, since PPRINT-LOGICAL-BLOCK
                  ;; is called many times from system pretty-printing code.
                  ;;
                  ;; FIXME: I think pprint-logical-block is broken wrt
                  ;; argument order, multiple evaluation, etc. of its
                  ;; keyword (:PREFIX, :PER-LINE-PREFIX and :SUFFIX)
                  ;; arguments.  Dunno if that's legal.
                  `(descend-into (,stream-var)
                     (let ((,count-name 0))
                       (declare (type index ,count-name) (ignorable ,count-name))
                       ,@(when (or prefixp per-line-prefix-p)
                               `((declare (string ,prefix-var))))
                       ,@(when (and suffixp)
                               `((declare (string ,suffix-var))))
                       (start-logical-block ,stream-var
                                            ,prefix-var
                                            ,(if per-line-prefix-p t nil)
                                            ,suffix-var)
                       (block ,block-name
                         (flet ((,pp-pop-name ()
                                  ,@(when object
                                          `((unless (listp ,object-var)
                                              (write-string ". " ,stream-var)
                                              (output-object ,object-var ,stream-var)
                                              (return-from ,block-name nil))))
                                  (when (and (not *print-readably*)
                                             (eql ,count-name *print-length*))
                                    (write-string "..." ,stream-var)
                                    (return-from ,block-name nil))
                                  ,@(when object
                                          `((when (and ,object-var
                                                       (plusp ,count-name)
                                                       (check-for-circularity
                                                        ,object-var
                                                        nil
                                                        :logical-block))
                                              (write-string ". " ,stream-var)
                                              (output-object ,object-var ,stream-var)
                                              (return-from ,block-name nil))))
                                  (incf ,count-name)
                                  ,@(if object
                                        `((pop ,object-var))
                                        `(nil))))
                           (declare (ignorable (function ,pp-pop-name)))
                           (locally
                               (declare (disable-package-locks
                                         pprint-pop pprint-exit-if-list-exhausted))
                             (macrolet ((pprint-pop ()
                                          '(,pp-pop-name))
                                        (pprint-exit-if-list-exhausted ()
                                          ,(if object
                                               `'(when (null ,object-var)
                                                  (return-from ,block-name nil))
                                               `'(return-from ,block-name nil))))
                               (declare (enable-package-locks
                                         pprint-pop pprint-exit-if-list-exhausted))
                               ,@body))))
                       ;; FIXME: Don't we need UNWIND-PROTECT to ensure this
                       ;; always gets executed?
                       (end-logical-block ,stream-var)))))
            (when object
              (setf body
                    `(let ((,object-var ,object))
                       (if (listp ,object-var)
                           (with-circularity-detection (,object-var ,stream-var)
                             ,body)
                           (output-object ,object-var ,stream-var)))))
            `(with-pretty-stream (,stream-var ,stream-expression)
               ,body)))))))

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
