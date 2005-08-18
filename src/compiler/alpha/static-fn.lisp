;;;; VOPs and macro magic for calling static functions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

(define-vop (static-fun-template)
  (:save-p t)
  (:policy :safe)
  (:variant-vars symbol)
  (:vop-var vop)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg)) move-temp)
  (:temporary (:sc descriptor-reg :offset lra-offset) lra)
  (:temporary (:sc interior-reg :offset lip-offset) entry-point)
  (:temporary (:sc any-reg :offset nargs-offset) nargs)
  (:temporary (:sc any-reg :offset ocfp-offset) ocfp)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save))

(eval-when  (:compile-toplevel :load-toplevel :execute)

(defun static-fun-template-name (num-args num-results)
  (intern (format nil "~:@(~R-arg-~R-result-static-fun~)"
                  num-args num-results)))

(defun moves (src dst)
  (collect ((moves))
    (do ((dst dst (cdr dst))
         (src src (cdr src)))
        ((or (null dst) (null src)))
      (moves `(move ,(car src) ,(car dst))))
    (moves)))

(defun static-fun-template-vop (num-args num-results)
  (unless (and (<= num-args register-arg-count)
               (<= num-results register-arg-count))
    (error "either too many args (~W) or too many results (~W); max = ~W"
           num-args num-results register-arg-count))
  (let ((num-temps (max num-args num-results)))
    (collect ((temp-names) (temps) (arg-names) (args) (result-names) (results))
      (dotimes (i num-results)
        (let ((result-name (intern (format nil "RESULT-~D" i))))
          (result-names result-name)
          (results `(,result-name :scs (any-reg descriptor-reg)))))
      (dotimes (i num-temps)
        (let ((temp-name (intern (format nil "TEMP-~D" i))))
          (temp-names temp-name)
          (temps `(:temporary (:sc descriptor-reg
                               :offset ,(nth i *register-arg-offsets*)
                               ,@(when (< i num-args)
                                   `(:from (:argument ,i)))
                               ,@(when (< i num-results)
                                   `(:to (:result ,i)
                                     :target ,(nth i (result-names)))))
                              ,temp-name))))
      (dotimes (i num-args)
        (let ((arg-name (intern (format nil "ARG-~D" i))))
          (arg-names arg-name)
          (args `(,arg-name
                  :scs (any-reg descriptor-reg null zero)
                  :target ,(nth i (temp-names))))))
      `(define-vop (,(static-fun-template-name num-args num-results)
                    static-fun-template)
         (:args ,@(args))
         ,@(temps)
         (:results ,@(results))
         (:generator ,(+ 50 num-args num-results)
           (let ((lra-label (gen-label))
                 (cur-nfp (current-nfp-tn vop)))
             ,@(moves (arg-names) (temp-names))
             (inst li (fixnumize ,num-args) nargs)
             (inst ldl entry-point (static-fun-offset symbol) null-tn)
             (when cur-nfp
               (store-stack-tn nfp-save cur-nfp))
             (inst move cfp-tn ocfp)
             (inst compute-lra-from-code lra code-tn lra-label temp)
             (note-this-location vop :call-site)
             (inst move csp-tn cfp-tn)
             (inst jsr zero-tn entry-point)
             (emit-return-pc lra-label)
             ,(collect ((bindings) (links))
                (do ((temp (temp-names) (cdr temp))
                     (name 'values (gensym))
                     (prev nil name)
                     (i 0 (1+ i)))
                    ((= i num-results))
                  (bindings `(,name
                              (make-tn-ref ,(car temp) nil)))
                  (when prev
                    (links `(setf (tn-ref-across ,prev) ,name))))
                `(let ,(bindings)
                   ,@(links)
                   (default-unknown-values vop
                       ,(if (zerop num-results) nil 'values)
                       ,num-results move-temp temp lra-label)))
             (when cur-nfp
               (maybe-load-stack-nfp-tn cur-nfp nfp-save temp))
             ,@(moves (temp-names) (result-names))))))))

) ; EVAL-WHEN

(expand
 (collect ((templates (list 'progn)))
   (dotimes (i register-arg-count)
     (templates (static-fun-template-vop i 1)))
   (templates)))

(defmacro define-static-fun (name args &key (results '(x)) translate
                                       policy cost arg-types result-types)
  `(define-vop (,name
                ,(static-fun-template-name (length args)
                                           (length results)))
     (:variant ',name)
     (:note ,(format nil "static-fun ~@(~S~)" name))
     ,@(when translate
         `((:translate ,translate)))
     ,@(when policy
         `((:policy ,policy)))
     ,@(when cost
         `((:generator-cost ,cost)))
     ,@(when arg-types
         `((:arg-types ,@arg-types)))
     ,@(when result-types
         `((:result-types ,@result-types)))))
