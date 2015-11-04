;;;; optimizations for SAP operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;;; DEFKNOWNs

#!+linkage-table
(deftransform foreign-symbol-address ((symbol &optional datap) (simple-string boolean)
                                      * :important t :policy :fast-safe)
  (if (and (constant-lvar-p symbol)
           (constant-lvar-p datap)
           #!+sb-dynamic-core (not (lvar-value datap)))
      `(values (sap-int (foreign-symbol-sap symbol datap))
               (or #!+sb-dynamic-core t))
      (give-up-ir1-transform)))

(deftransform foreign-symbol-sap ((symbol &optional datap)
                                      (simple-string &optional boolean))
    #!-linkage-table
    (if (null datap)
        (give-up-ir1-transform)
        `(foreign-symbol-sap symbol))
    #!+linkage-table
    (if (and (constant-lvar-p symbol) (constant-lvar-p datap))
        (let (#!-sb-dynamic-core (name (lvar-value symbol))
              (datap (lvar-value datap)))
          #!-sb-dynamic-core
          (if (or #+sb-xc-host t ; only static symbols on host
                  (not datap)
                  (find-foreign-symbol-in-table name *static-foreign-symbols*))
              `(foreign-symbol-sap ,name) ; VOP
              `(foreign-symbol-dataref-sap ,name)) ; VOP
          #!+sb-dynamic-core
          (if datap
              `(foreign-symbol-dataref-sap symbol)
              `(foreign-symbol-sap symbol)))
        (give-up-ir1-transform)))

(defknown (sap< sap<= sap= sap>= sap>)
          (system-area-pointer system-area-pointer) boolean
  (movable flushable))

(defknown sap+ (system-area-pointer integer) system-area-pointer
  (movable flushable))
(defknown sap- (system-area-pointer system-area-pointer)
               (signed-byte #.sb!vm::n-word-bits)
  (movable flushable))

(defknown sap-int (system-area-pointer)
  (unsigned-byte #.sb!vm::n-machine-word-bits)
  (movable flushable foldable))
(defknown int-sap ((unsigned-byte #.sb!vm::n-machine-word-bits))
  system-area-pointer (movable))

(macrolet ((defsapref (fun value-type)
             (let (#!+x86
                   (with-offset-fun (intern (format nil "~A-WITH-OFFSET" fun)))
                   (set-fun (intern (format nil "%SET-~A" fun)))
                   #!+x86
                   (set-with-offset-fun (intern (format nil "%SET-~A-WITH-OFFSET" fun))))
               `(progn
                  (defknown ,fun (system-area-pointer fixnum) ,value-type
                    (flushable))
                  #!+x86
                  (defknown ,with-offset-fun (system-area-pointer fixnum fixnum) ,value-type
                    (flushable always-translatable))
                  (defknown ,set-fun (system-area-pointer fixnum ,value-type) ,value-type
                    ())
                  #!+x86
                  (defknown ,set-with-offset-fun (system-area-pointer fixnum fixnum ,value-type) ,value-type
                    (always-translatable))))))
  (defsapref sap-ref-8 (unsigned-byte 8))
  (defsapref sap-ref-16 (unsigned-byte 16))
  (defsapref sap-ref-32 (unsigned-byte 32))
  (defsapref sap-ref-64 (unsigned-byte 64))
  (defsapref sap-ref-word (unsigned-byte #.sb!vm:n-word-bits))
  (defsapref signed-sap-ref-8 (signed-byte 8))
  (defsapref signed-sap-ref-16 (signed-byte 16))
  (defsapref signed-sap-ref-32 (signed-byte 32))
  (defsapref signed-sap-ref-64 (signed-byte 64))
  (defsapref signed-sap-ref-word (signed-byte #.sb!vm:n-word-bits))
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
                              (> (second value-type) sb!vm:n-word-bits))
                   #!+x86
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
  #!+long-float (def sap-ref-long)
  #!+long-float (def %set-sap-ref-long t long-float))

(macrolet ((def (fun args 32-bit 64-bit)
               `(deftransform ,fun (,args)
                  (ecase sb!vm::n-word-bits
                    (32 '(,32-bit ,@args))
                    (64 '(,64-bit ,@args))))))
  (def sap-ref-word (sap offset) sap-ref-32 sap-ref-64)
  (def signed-sap-ref-word (sap offset) signed-sap-ref-32 signed-sap-ref-64)
  (def %set-sap-ref-word (sap offset value)
    %set-sap-ref-32 %set-sap-ref-64)
  (def %set-signed-sap-ref-word (sap offset value)
    %set-signed-sap-ref-32 %set-signed-sap-ref-64))

;;; Transforms for 64-bit SAP accessors on 32-bit platforms.

#!-64-bit-registers
(progn
#!+#.(cl:if (cl:eq :little-endian sb!c:*backend-byte-order*) '(and) '(or))
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

#!+#.(cl:if (cl:eq :big-endian sb!c:*backend-byte-order*) '(and) '(or))
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
) ; (= 32 SB!VM:N-MACHINE-WORD-BITS)
