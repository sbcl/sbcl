;;;; cross-compile-time-only replacements for modular functions;
;;;; needed for constant-folding

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

(defun mask-signed-field (size integer)
  (cond ((zerop size)
         0)
        ((logbitp (1- size) integer)
         (dpb integer (byte size 0) -1))
        (t
         (ldb (byte size 0) integer))))

#.
(collect ((forms))
  (flet ((unsigned-definition (name lambda-list prototype width)
           `(defun ,name ,lambda-list
              (ldb (byte ,width 0) (,prototype ,@lambda-list))))
         (signed-definition (name lambda-list prototype width)
           `(defun ,name ,lambda-list
              (mask-signed-field ,width (,prototype ,@lambda-list)))))
    (flet ((do-mfuns (class)
             (loop for infos being each hash-value of (modular-class-funs class) using (hash-key prototype)
                   when (listp infos)
                   do (loop for info in infos
                            for name = (modular-fun-info-name info)
                            and width = (modular-fun-info-width info)
                            and signedp = (modular-fun-info-signedp info)
                            and lambda-list = (modular-fun-info-lambda-list info)
                            if signedp
                            do (forms (signed-definition name lambda-list prototype width))
                            else
                            do (forms (unsigned-definition name lambda-list prototype width))))))
      (do-mfuns *untagged-unsigned-modular-class*)
      (do-mfuns *untagged-signed-modular-class*)
      (do-mfuns *tagged-modular-class*)))
  `(progn ,@(forms)))

#!+#.(cl:if (cl:= sb!vm:n-machine-word-bits 32) '(and) '(or))
(defun sb!vm::ash-left-mod32 (integer amount)
  (ldb (byte 32 0) (ash integer amount)))
#!+#.(cl:if (cl:= sb!vm:n-machine-word-bits 64) '(and) '(or))
(defun sb!vm::ash-left-mod64 (integer amount)
  (ldb (byte 64 0) (ash integer amount)))
#!+x86
(defun sb!vm::ash-left-smod30 (integer amount)
  (mask-signed-field 30 (ash integer amount)))
#!+x86-64
(defun sb!vm::ash-left-smod61 (integer amount)
  (mask-signed-field 61 (ash integer amount)))

