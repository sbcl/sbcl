(in-package "SB!VM")



(define-vop (static-fun-template)
  (:save-p t)
  (:policy :safe)
  (:variant-vars symbol)
  (:vop-var vop)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg)) move-temp)
  (:temporary (:sc descriptor-reg :offset lra-offset) lra)
  (:temporary (:sc interior-reg) entry-point)
  (:temporary (:sc any-reg :offset nargs-offset) nargs)
  (:temporary (:sc any-reg :offset ocfp-offset) ocfp)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save))


(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)

(defun static-fun-template-name (num-args num-results)
  (intern (format nil "~:@(~R-arg-~R-result-static-fun~)"
                  num-args num-results)))

(defun moves (dst src)
  (collect ((moves))
    (do ((dst dst (cdr dst))
         (src src (cdr src)))
        ((or (null dst) (null src)))
      (moves `(move ,(car dst) ,(car src))))
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
             ,@(moves (temp-names) (arg-names))
             (inst li nargs (fixnumize ,num-args))
             (inst lw entry-point null-tn (static-fun-offset symbol))
             (when cur-nfp
               (store-stack-tn nfp-save cur-nfp))
             (move ocfp cfp-tn)
             (inst compute-lra-from-code lra code-tn lra-label temp)
             (note-this-location vop :call-site)
             (inst j entry-point)
             (move cfp-tn csp-tn t)
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
               (load-stack-tn cur-nfp nfp-save))
             ,@(moves (result-names) (temp-names))))))))


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
