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

;;; FIXME: presumably the x86-64 backend could benefit from the -WITH-OFFSET
;;; transforms.  Perhaps measure to find out whether the transform ever fires.
(macrolet ((defsapref (fun value-type)
             (let (#+x86
                   (with-offset-fun (intern (format nil "~A-WITH-OFFSET" fun)))
                   (set-fun (intern (format nil "%SET-~A" fun)))
                   #+x86
                   (set-with-offset-fun (intern (format nil "%SET-~A-WITH-OFFSET" fun))))
               `(progn
                  (defknown ,fun (system-area-pointer fixnum) ,value-type
                    (flushable))
                  #+x86
                  (defknown ,with-offset-fun (system-area-pointer fixnum fixnum) ,value-type
                    (flushable always-translatable))
                  (defknown ,set-fun (system-area-pointer fixnum ,value-type) ,value-type
                    ())
                  #+x86
                  (defknown ,set-with-offset-fun (system-area-pointer fixnum fixnum ,value-type) ,value-type
                    (always-translatable))))))
  (defsapref sap-ref-8 (unsigned-byte 8))
  (defsapref sap-ref-16 (unsigned-byte 16))
  (defsapref sap-ref-32 (unsigned-byte 32))
  (defsapref sap-ref-64 (unsigned-byte 64))
  (defsapref signed-sap-ref-8 (signed-byte 8))
  (defsapref signed-sap-ref-16 (signed-byte 16))
  (defsapref signed-sap-ref-32 (signed-byte 32))
  (defsapref signed-sap-ref-64 (signed-byte 64))
  (defsapref sap-ref-sap system-area-pointer)
  (defsapref sap-ref-lispobj t)
  (defsapref sap-ref-single single-float)
  (defsapref sap-ref-double double-float)
  (defsapref sap-ref-long long-float)
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

(macrolet ((def (fun &optional setp value-type)
             (declare (ignorable value-type))
             `(progn
                (deftransform ,fun ((sap offset ,@(when setp `(new-value))) * *)
                  (splice-fun-args sap 'sap+ 2)
                  `(lambda (sap offset1 offset2 ,@',(when setp `(new-value)))
                     (,',fun sap (+ offset1 offset2) ,@',(when setp `(new-value)))))
                ;; Avoid defining WITH-OFFSET transforms for accessors whose
                ;; sizes are larger than the word size; they'd probably be
                ;; pointless to optimize anyway and tricky to boot.
                ,(unless (and (listp value-type)
                              (or (eq (first value-type) 'unsigned-byte)
                                  (eq (first value-type) 'signed-byte))
                              (> (second value-type) sb-vm:n-word-bits))
                   #+x86
                   (let ((with-offset-fun (intern (format nil "~A-WITH-OFFSET" fun))))
                     `(progn
                        ,(cond
                          (setp
                           `(deftransform ,fun ((sap offset new-value)
                                                (system-area-pointer fixnum ,value-type) *)
                             `(,',with-offset-fun sap (truly-the fixnum offset) 0 new-value)))
                          (t
                           `(deftransform ,fun ((sap offset) (system-area-pointer fixnum) *)
                              `(,',with-offset-fun sap (truly-the fixnum offset) 0))))
                        (deftransform ,with-offset-fun ((sap offset disp
                                                             ,@(when setp `(new-value))) * *)
                          (fold-index-addressing ',with-offset-fun
                                                 8 ; all sap-offsets are in bytes
                                                 0 ; lowtag
                                                 0 ; data offset
                                                 offset disp ,setp))))))))
  (def sap-ref-8)
  (def %set-sap-ref-8 t (unsigned-byte 8))
  (def signed-sap-ref-8)
  (def %set-signed-sap-ref-8 t (signed-byte 8))
  (def sap-ref-16)
  (def %set-sap-ref-16 t (unsigned-byte 16))
  (def signed-sap-ref-16)
  (def %set-signed-sap-ref-16 t (signed-byte 16))
  (def sap-ref-32)
  (def %set-sap-ref-32 t (unsigned-byte 32))
  (def signed-sap-ref-32)
  (def %set-signed-sap-ref-32 t (signed-byte 32))
  (def sap-ref-64)
  (def %set-sap-ref-64 t (unsigned-byte 64))
  (def signed-sap-ref-64)
  (def %set-signed-sap-ref-64 t (signed-byte 64))
  (def sap-ref-sap)
  (def %set-sap-ref-sap t system-area-pointer)
  (def sap-ref-lispobj)
  (def %set-sap-ref-lispobj t t)
  (def sap-ref-single)
  (def %set-sap-ref-single t single-float)
  (def sap-ref-double)
  (def %set-sap-ref-double t double-float)
  #+long-float (def sap-ref-long)
  #+long-float (def %set-sap-ref-long t long-float))

;;; [%SET-][SIGNED-]SAP-REF-WORD gets a defknown but no IR transforms,
;;; just source transforms.
(macrolet ((def (alias args value-type)
             (let* ((str (string alias))
                    (prefix (subseq str 0 (- (length str) 4))) ; remove -WORD
                    (fun (symbolicate prefix (write-to-string sb-vm:n-word-bits)))
                    (setp (string= prefix "%SET-" :end1 5)))
             `(progn
                (defknown ,alias (system-area-pointer fixnum ,@(if setp (list value-type)))
                  ,value-type (flushable))
                (define-source-transform ,alias ,args `(,',fun ,,@args))))))
  (def sap-ref-word (sap offset) word)
  (def signed-sap-ref-word (sap offset) sb-vm:signed-word)
  (def %set-sap-ref-word (sap offset value) word)
  (def %set-signed-sap-ref-word (sap offset value) sb-vm:signed-word))

;;; Transforms for 64-bit SAP accessors on 32-bit platforms.

#-(or 64-bit 64-bit-registers)
(progn
#+little-endian
(progn
  (deftransform sap-ref-64 ((sap offset) (* *))
    '(logior (sap-ref-32 sap offset)
             (ash (sap-ref-32 sap (+ offset 4)) 32)))

  (deftransform signed-sap-ref-64 ((sap offset) (* *))
    '(logior (sap-ref-32 sap offset)
             (ash (signed-sap-ref-32 sap (+ offset 4)) 32)))

  (deftransform %set-sap-ref-64 ((sap offset value) (* * *))
    '(progn
       (%set-sap-ref-32 sap offset (logand value #xffffffff))
       (%set-sap-ref-32 sap (+ offset 4) (ash value -32))))

  (deftransform %set-signed-sap-ref-64 ((sap offset value) (* * *))
    '(progn
       (%set-sap-ref-32 sap offset (logand value #xffffffff))
       (%set-signed-sap-ref-32 sap (+ offset 4) (ash value -32)))))

#+big-endian
(progn
  (deftransform sap-ref-64 ((sap offset) (* *))
    '(logior (ash (sap-ref-32 sap offset) 32)
      (sap-ref-32 sap (+ offset 4))))

  (deftransform signed-sap-ref-64 ((sap offset) (* *))
    '(logior (ash (signed-sap-ref-32 sap offset) 32)
      (sap-ref-32 sap (+ 4 offset))))

  (deftransform %set-sap-ref-64 ((sap offset value) (* * *))
    '(progn
      (%set-sap-ref-32 sap offset (ash value -32))
      (%set-sap-ref-32 sap (+ offset 4) (logand value #xffffffff))))

  (deftransform %set-signed-sap-ref-64 ((sap offset value) (* * *))
    '(progn
      (%set-signed-sap-ref-32 sap offset (ash value -32))
      (%set-sap-ref-32 sap (+ 4 offset) (logand value #xffffffff)))))
) ; (= 32 SB-VM:N-MACHINE-WORD-BITS)
