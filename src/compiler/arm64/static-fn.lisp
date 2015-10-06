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
  (:temporary (:sc any-reg :offset nargs-offset) nargs)
  (:temporary (:sc interior-reg) lip)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save))


(eval-when (:compile-toplevel :load-toplevel :execute)

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
                  :scs (any-reg descriptor-reg)
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
             (inst mov nargs (fixnumize ,num-args))
             (when cur-nfp
               (store-stack-tn nfp-save cur-nfp))
             ;; This is a somewhat ideosyncratic way to build a new
             ;; stack frame, pushing a value and updating CSP, finding
             ;; the new CFP, then pushing another value on CSP, but it
             ;; works for this situation.
             (inst compute-lra lip lip lra-label)

             (inst add csp-tn csp-tn 16)
             (inst str cfp-tn (@ csp-tn -16))
             (inst str lip (@ csp-tn -8))
             (inst sub cfp-tn csp-tn 16)
             (note-this-location vop :call-site)
             (inst ldr lip (@ null-tn (load-store-offset (static-fun-offset symbol))))
             (inst br lip)

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
                       ,num-results move-temp temp lip lra-label)))
             (when cur-nfp
               (load-stack-tn cur-nfp nfp-save))
             ,@(moves (result-names) (temp-names))))))))


) ; EVAL-WHEN


(macrolet ((frob (num-args num-res)
             (static-fun-template-vop (eval num-args) (eval num-res))))
  ;; Other backends cover options from zero through
  ;; register-arg-count.  It turns out, however, that only the 1 and 2
  ;; arg cases are actually used.
  (frob 1 1)
  (frob 2 1))

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
