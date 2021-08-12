;;;; optimizations for SAP operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;;; DEFKNOWNs

(deftransform foreign-symbol-address ((symbol &optional datap)
                                      ((constant-arg simple-string)
                                       &optional (constant-arg boolean)))
  (if (and datap (lvar-value datap))
      (give-up-ir1-transform)
      `(values (sap-int (foreign-symbol-sap symbol datap)) t)))

(deftransform foreign-symbol-sap ((symbol &optional datap)
                                  (simple-string &optional boolean))
  (if (and (constant-lvar-p symbol) (and datap (constant-lvar-p datap)))
      (if (lvar-value datap)
          `(foreign-symbol-dataref-sap symbol)
          `(foreign-symbol-sap symbol))
      (give-up-ir1-transform)))

(defknown (sap< sap<= sap= sap>= sap>)
          (system-area-pointer system-area-pointer) boolean
  (movable flushable))

(defknown sap+ (system-area-pointer sb-vm:signed-word) system-area-pointer
  (movable flushable))
(defknown sap- (system-area-pointer system-area-pointer) sb-vm:signed-word
  (movable flushable))

(defknown sap-int (system-area-pointer)
  (unsigned-byte #.sb-vm:n-machine-word-bits)
  (movable flushable foldable))
(defknown int-sap ((unsigned-byte #.sb-vm:n-machine-word-bits))
  system-area-pointer (movable))

(macrolet ((defsapref (fun value-type
                 &aux (setter (symbolicate "%SET-" fun))
                      (setter-translatable
                       (unless (member fun #-64-bit '(sap-ref-64 signed-sap-ref-64)
                                           #+64-bit nil)
                         '(always-translatable))))
             `(progn
                ;; Callable definitions of these are are defined in src/code/stubs
                (defknown ,fun (system-area-pointer fixnum) ,value-type
                    (flushable always-translatable))
                (defknown (setf ,fun) (,value-type system-area-pointer fixnum)
                    ,value-type ())
                (defknown ,setter (,value-type system-area-pointer fixnum) (values)
                    (,@setter-translatable))
                ;; word-sized integers can be treated as signed or unsigned for CAS,
                ;; but sub-word will require an explicit sign-extension step,
                ;; either in the vop or in Lisp.
                ,@(when (member fun '(sap-ref-8 sap-ref-16
                                      sap-ref-32 #-64-bit signed-sap-ref-32
                                      #+64-bit sap-ref-64
                                      #+64-bit signed-sap-ref-64
                                      sap-ref-sap sap-ref-lispobj))
                    `((defknown (cas ,fun) (,value-type ,value-type system-area-pointer fixnum)
                       ,value-type (always-translatable))))
                ,@(when setter-translatable
                    ;; Unlike macros, source-transforms work on (funcall #'(setf name) ...)
                    ;; If this is a 64-bit sizes on a 32-bit machines,
                    ;; then the source-transform is defined at the end of this file.
                    `((define-source-transform (setf ,fun) (value sap offset)
                        `(let ((result ,value))
                           (,',setter result ,sap ,offset) ; vop-translated
                           result))))
                ;; FIXME: I think these transform are the opposite of good.
                ;; They can take arithmetic which is perfectly well-defined in the
                ;; pointer domain (modular addition on unsigned words),
                ;; and potentially turn it into a call to GENERIC+.
                (deftransform ,fun ((sap offset))
                  (splice-fun-args sap 'sap+ 2)
                  `(lambda (sap offset1 offset2)
                     (,',fun sap (+ offset1 offset2))))
                (deftransform ,setter ((value sap offset))
                  (splice-fun-args sap 'sap+ 2)
                  `(lambda (value sap offset1 offset2)
                     (,',setter value sap (+ offset1 offset2)))))))
  (defsapref sap-ref-8          (unsigned-byte 8))
  (defsapref signed-sap-ref-8   (signed-byte 8))
  (defsapref sap-ref-16         (unsigned-byte 16))
  (defsapref signed-sap-ref-16  (signed-byte 16))
  (defsapref sap-ref-32         (unsigned-byte 32))
  (defsapref signed-sap-ref-32  (signed-byte 32))
  (defsapref sap-ref-64         (unsigned-byte 64))
  (defsapref signed-sap-ref-64  (signed-byte 64))
  (defsapref sap-ref-sap        system-area-pointer)
  (defsapref sap-ref-lispobj    t)
  (defsapref sap-ref-single     single-float)
  (defsapref sap-ref-double     double-float)
  (defsapref sap-ref-long       long-float) ; actually DOUBLE-FLOAT
) ; MACROLET


;;;; transforms for converting sap relation operators

(macrolet ((def (sap-fun int-fun)
             `(deftransform ,sap-fun ((x y) * *)
                `(,',int-fun (sap-int x) (sap-int y)))))
  (def sap< <)
  (def sap<= <=)
  (def sap= =)
  (def sap>= >=)
  (def sap> >))

;;;; transforms for optimizing SAP+

(deftransform sap+ ((sap offset))
  (cond ((and (constant-lvar-p offset)
              (eql (lvar-value offset) 0))
         'sap)
        (t
         (splice-fun-args sap 'sap+ 2)
         '(lambda (sap offset1 offset2)
            (sap+ sap (+ offset1 offset2))))))


;;; [%SET-][SIGNED-]SAP-REF-WORD gets a defknown but no IR transforms,
;;; just source transforms.
(macrolet ((def (alias value-type)
             (let* ((str (string alias))
                    (prefix (subseq str 0 (- (length str) 4))) ; remove -WORD
                    (fun (symbolicate prefix (write-to-string sb-vm:n-word-bits))))
             `(progn
                (defknown ,alias (system-area-pointer fixnum) ,value-type (flushable))
                (defknown (setf ,alias) (,value-type system-area-pointer fixnum)
                  (,value-type) ())
                (define-source-transform ,alias (sap offset)
                  `(,',fun ,sap ,offset))
                (define-source-transform (setf ,alias) (value sap offset)
                  `(funcall #'(setf ,',fun) ,value ,sap ,offset))))))
  (def sap-ref-word word)
  (def signed-sap-ref-word sb-vm:signed-word))

;;; Transforms for 64-bit SAP accessors on 32-bit platforms.

#-(or 64-bit 64-bit-registers)
(progn
#+little-endian
(progn
  (define-source-transform sap-ref-64 (sap offset)
    `(let ((sap ,sap) (offset ,offset))
       (logior (sap-ref-32 sap offset)
               (ash (sap-ref-32 sap (+ offset 4)) 32))))

  (define-source-transform signed-sap-ref-64 (sap offset)
    `(let ((sap ,sap) (offset ,offset))
       (logior (sap-ref-32 sap offset)
               (ash (signed-sap-ref-32 sap (+ offset 4)) 32))))

  (define-source-transform (setf sap-ref-64) (value sap offset)
    `(let ((value ,value) (sap ,sap) (offset ,offset))
       (%set-sap-ref-32 (logand value #xffffffff) sap offset)
       (%set-sap-ref-32 (ash value -32) sap (+ offset 4))
       value))

  (define-source-transform (setf signed-sap-ref-64) (value sap offset)
    `(let ((value ,value) (sap ,sap) (offset ,offset))
       (%set-sap-ref-32 (logand value #xffffffff) sap offset)
       (%set-signed-sap-ref-32 (ash value -32) sap (+ offset 4))
       value)))

#+big-endian
(progn
  (define-source-transform sap-ref-64 (sap offset)
    `(let ((sap ,sap) (offset ,offset))
       (logior (ash (sap-ref-32 sap offset) 32)
               (sap-ref-32 sap (+ offset 4)))))

  (define-source-transform signed-sap-ref-64 (sap offset)
    `(let ((sap ,sap) (offset ,offset))
       (logior (ash (signed-sap-ref-32 sap offset) 32)
               (sap-ref-32 sap (+ 4 offset)))))

  (define-source-transform (setf sap-ref-64) (value sap offset)
    `(let ((value ,value) (sap ,sap) (offset ,offset))
       (%set-sap-ref-32 (ash value -32) sap offset)
       (%set-sap-ref-32 (logand value #xffffffff) sap (+ offset 4))
       value))

  (define-source-transform (setf signed-sap-ref-64) (value sap offset)
    `(let ((value ,value) (sap ,sap) (offset ,offset))
       (%set-signed-sap-ref-32 (ash value -32) sap offset)
       (%set-sap-ref-32 (logand value #xffffffff) sap (+ 4 offset))
       value)))
) ; (= 32 SB-VM:N-MACHINE-WORD-BITS)
