;;;; the VOPs and macro magic required to call static functions

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
  (:variant-vars function)
  (:vop-var vop)
  (:temporary (:sc unsigned-reg :offset ecx-offset
                   :from (:eval 0) :to (:eval 2)) ecx))

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
  (let ((num-temps (max num-args num-results))
        (node (gensym "NODE-")))
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
                               :from ,(if (< i num-args)
                                          `(:argument ,i)
                                          '(:eval 1))
                               :to ,(if (< i num-results)
                                        `(:result ,i)
                                        '(:eval 1))
                               ,@(when (< i num-results)
                                   `(:target ,(nth i (result-names)))))
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
        (:temporary (:sc unsigned-reg) call-target)
        (:results ,@(results))
        (:node-var ,node)
        (:generator ,(+ 50 num-args num-results)
         ,@(moves (temp-names) (arg-names))

         ;; If speed is at least as important as size, duplicate the
         ;; effect of the ENTER with discrete instructions. Takes
         ;; 3+4+4=11 bytes as opposed to 1+4=5 bytes.
         (cond ((policy ,node (>= speed space))
                (inst sub rsp-tn (fixnumize 3))
                (inst mov (make-ea :qword :base rsp-tn
                                   :disp (frame-byte-offset
                                          (+ sp->fp-offset
                                             -3
                                             ocfp-save-offset)))
                      rbp-tn)
                (inst lea rbp-tn (make-ea :qword :base rsp-tn
                                          :disp (frame-byte-offset
                                                 (+ sp->fp-offset
                                                    -3
                                                    ocfp-save-offset)))))
               (t
                ;; Dummy for return address.
                (inst push rbp-tn)
                (inst enter (fixnumize 1))))

         ,(if (zerop num-args)
              '(inst xor ecx ecx)
              `(inst mov ecx (fixnumize ,num-args)))

         (note-this-location vop :call-site)
         ;; Old CMU CL comment:
         ;;   STATIC-FUN-OFFSET gives the offset from the start of
         ;;   the NIL object to the static function FDEFN and has the
         ;;   low tag of 1 added. When the NIL symbol value with its
         ;;   low tag of 3 is added the resulting value points to the
         ;;   raw address slot of the fdefn (at +4).
         ;; FIXME: Since the fork from CMU CL, we've swapped
         ;; FUN-POINTER-LOWTAG and INSTANCE-POINTER-LOWTAG, so the
         ;; text above is no longer right. Mysteriously, things still
         ;; work. It would be good to explain why. (Is this code no
         ;; longer executed? Does it not depend on the
         ;; 1+3=4=fdefn_raw_address_offset relationship above?
         ;; Is something else going on?)

         ;; Need to load the target address into a register, since
         ;; immediate call arguments are just a 32-bit displacement,
         ;; which obviously can't work with >4G spaces.
         (inst mov call-target
               (make-ea :qword
                        :disp (+ nil-value (static-fun-offset function))))
         (inst call call-target)
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
                     (default-unknown-values
                         vop
                         ,(if (zerop num-results) nil 'values)
                       ,num-results
                       ,node)))
         ,@(moves (result-names) (temp-names)))))))

) ; EVAL-WHEN

(macrolet ((frob (num-args num-res)
             (static-fun-template-vop (eval num-args) (eval num-res))))
  (frob 0 1)
  (frob 1 1)
  (frob 2 1)
  (frob 3 1))

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
