;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;;; quantifiers

;;; We borrow the logic from (MAP NIL ..) to handle iteration over
;;; arbitrary sequence arguments, both in the full call case and in
;;; the open code case.
(flet ((expand (pred sequences test found-result unfound-result)
         (unless (proper-list-of-length-p sequences 1 call-arguments-limit)
           (return-from expand (values nil t))) ; give up
         (binding* ((elements (make-gensym-list (length sequences)))
                    ((bind-fun call-it) (funarg-bind/call-forms pred elements))
                    (blockname (sb!xc:gensym "BLOCK"))
                    (wrapper (sb!xc:gensym "WRAPPER"))
                    (value (sb!xc:gensym "VAL")))
             (let ((form
                    `(block ,blockname
                       ;; Does DX actually help? INLINE should win anyway.
                       (dx-flet ((,wrapper (,@elements)
                                  (declare (optimize
                                            (sb!c::check-tag-existence 0)))
                                  (let ((,value ,call-it))
                                    (,test ,value
                                      (return-from ,blockname
                                       ,(if (eq found-result :value)
                                            value
                                            found-result))))))
                         (declare (inline ,wrapper))
                         (%map nil #',wrapper ,@sequences)
                         ,unfound-result))))
               (values (if bind-fun `(let ,bind-fun ,form) form) nil)))))
  (macrolet ((defquantifier (name found-test found-result
                                  &key doc (unfound-result (not found-result)))
               (declare (ignorable doc))
               `(progn
                ;; KLUDGE: It would be really nice if we could simply
                ;; do something like this
                ;;  (declaim (inline ,name))
                ;;  (defun ,name (pred first-seq &rest more-seqs)
                ;;    ,doc
                ;;    (flet ((map-me (&rest rest)
                ;;             (let ((pred-value (apply pred rest)))
                ;;               (,found-test pred-value
                ;;                 (return-from ,name
                ;;                   ,found-result)))))
                ;;      (declare (inline map-me))
                ;;      (apply #'map nil #'map-me first-seq more-seqs)
                ;;      ,unfound-result))
                ;; but Python doesn't seem to be smart enough about
                ;; inlining and APPLY to recognize that it can use
                ;; the DEFTRANSFORM for MAP in the resulting inline
                ;; expansion. I don't have any appetite for deep
                ;; compiler hacking right now, so I'll just work
                ;; around the apparent problem by using a compiler
                ;; macro instead. -- WHN 20000410
                  (sb!c:define-source-transform ,name (pred &rest sequences)
                    (expand pred sequences
                            ',found-test ',found-result ',unfound-result))
                  #-sb-xc-host ; don't redefine CL builtins!
                  (defun ,name (pred first-seq &rest more-seqs)
                    #!+sb-doc ,doc
                    (flet ((map-me (&rest rest)
                             (let ((value (apply pred rest)))
                               (,found-test value
                                 (return-from ,name
                                   ,(if (eq found-result :value)
                                        'value
                                        found-result))))))
                      (declare (inline map-me))
                      (apply #'%map nil #'map-me first-seq more-seqs)
                      ,unfound-result)))))

  (defquantifier some when :value :unfound-result nil
   :doc "Apply PREDICATE to the 0-indexed elements of the sequences, then
   possibly to those with index 1, and so on. Return the first
   non-NIL value encountered, or NIL if the end of any sequence is reached.")
  (defquantifier every unless nil
   :doc "Apply PREDICATE to the 0-indexed elements of the sequences, then
   possibly to those with index 1, and so on. Return NIL as soon
   as any invocation of PREDICATE returns NIL, or T if every invocation
   is non-NIL.")
  (defquantifier notany when nil
   :doc "Apply PREDICATE to the 0-indexed elements of the sequences, then
   possibly to those with index 1, and so on. Return NIL as soon
   as any invocation of PREDICATE returns a non-NIL value, or T if the end
   of any sequence is reached.")
  (defquantifier notevery unless t
   :doc "Apply PREDICATE to 0-indexed elements of the sequences, then
   possibly to those with index 1, and so on. Return T as soon
   as any invocation of PREDICATE returns NIL, or NIL if every invocation
   is non-NIL.")))
