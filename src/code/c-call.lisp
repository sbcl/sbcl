;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-ALIEN")

;;;; C string support.

(defun load-alien-c-string-type (element-type external-format not-null)
  (make-alien-c-string-type
   :to (parse-alien-type 'char (sb-kernel:make-null-lexenv))
   :element-type element-type
   :external-format external-format
   :not-null not-null))

(define-alien-type-translator c-string
    (&key (external-format :default)
          (element-type 'character)
          (not-null nil))
  (load-alien-c-string-type element-type external-format not-null))

(defun c-string-external-format (type)
  (let ((external-format (alien-c-string-type-external-format type)))
    (if (eq external-format :default)
        #+sb-xc-host (bug "No default c-string-external-format")
        #-sb-xc-host (default-c-string-external-format)
        external-format)))

(define-alien-type-method (c-string :unparse) (type state)
  (let* ((external-format (alien-c-string-type-external-format type))
         (element-type (alien-c-string-type-element-type type))
         (not-null (alien-c-string-type-not-null type))
         (tail
          (append (unless (eq :default external-format)
                    (list :external-format external-format))
                  (unless (eq 'character element-type)
                    (list :element-type element-type))
                  (when not-null
                    (list :not-null t)))))
    (if tail
        (cons 'c-string tail)
        'c-string)))

(define-alien-type-method (c-string :lisp-rep) (type)
  (let ((possibilities '(simple-string (alien (* char)) (simple-array (unsigned-byte 8)))))
    (if (alien-c-string-type-not-null type)
        `(or ,@possibilities)
        `(or null ,@possibilities))))

(define-alien-type-method (c-string :deport-pin-p) (type)
  (declare (ignore type))
  t)

(defun c-string-needs-conversion-p (type)
  #+sb-xc-host
  (declare (ignore type))
  #+sb-xc-host
  t
  #-sb-xc-host
  (let ((external-format (sb-impl::get-external-format
                          ;; Can't use C-STRING-EXTERNAL-FORMAT here,
                          ;; since the meaning of :DEFAULT can change
                          ;; when *DEFAULT-C-STRING-EXTERNAL-FORMAT*
                          ;; changes.
                          (alien-c-string-type-external-format type))))
    (not (and external-format
              (or (eq (first (sb-impl::ef-names external-format)) :ascii)
                  ;; On non-SB-UNICODE all latin-1 codepoints will fit
                  ;; into a base-char, on SB-UNICODE they won't.
                  #-sb-unicode
                  (eq (first (sb-impl::ef-names external-format)) :latin-1))))))

(declaim (ftype (sfunction (t) nil) null-error))
(sb-kernel:define-error-wrapper null-error (type)
  (aver (alien-c-string-type-not-null type))
  (error 'type-error
         :expected-type `(alien ,(unparse-alien-type type))
         :datum nil))

(define-alien-type-method (c-string :naturalize-gen) (type alien)
  `(if (zerop (sap-int ,alien))
       ,(if (alien-c-string-type-not-null type)
            `(null-error ',type)
            nil)
       ;; Check whether we need to do a full external-format
       ;; conversion, or whether we can just do a cheap byte-by-byte
       ;; copy of the c-string data.
       ;;
       ;; On SB-UNICODE we can never do the cheap copy, even if the
       ;; external format and element-type are suitable, since
       ;; simple-base-strings may not contain ISO-8859-1 characters.
       ;; If we need to check for non-ascii data in the input, we
       ;; might as well go through the usual external-format machinery
       ;; instead of rewriting another version of it.
       ,(if #+sb-unicode t
            #-sb-unicode (c-string-needs-conversion-p type)
            `(c-string-to-string ,alien
                                 (c-string-external-format ,type)
                                 (alien-c-string-type-element-type
                                  ,type))
            `(%naturalize-c-string ,alien))))

(define-alien-type-method (c-string :deport-gen) (type value)
  ;; This SAP taking is safe as DEPORT callers pin the VALUE when
  ;; necessary.
  `(etypecase ,value
     (null
      ,(if (alien-c-string-type-not-null type)
           `(null-error ',type)
           `(int-sap 0)))
     ((alien (* char)) (alien-sap ,value))
     (vector (vector-sap ,value))))

(define-alien-type-method (c-string :deport-alloc-gen) (type value)
  `(etypecase ,value
     (null
      ,(if (alien-c-string-type-not-null type)
           `(null-error ',type)
           nil))
     ((alien (* char)) ,value)
     ;; If the alien type is not ascii-compatible (+SB-UNICODE)
     ;; or latin-1-compatible (-SB-UNICODE), we need to do
     ;; external format conversion.
     ,@(if (c-string-needs-conversion-p type)
           `((t
              (string-to-c-string ,value
                                  (c-string-external-format ,type))))
           `((simple-base-string
              ,value)
             (simple-string
              (string-to-c-string ,value
                                  (c-string-external-format ,type)))))))

;;;; Struct Return-by-Value Support

;;; Classification categories for struct fields/eightbytes per ABI.
;;; These values have architecture-specific semantics:
;;;
;;; :integer - Pass/return in general-purpose registers (RAX/RDX on x86-64,
;;;            x0/x1 on ARM64). Used for integer, pointer, and mixed types.
;;;
;;; :single  - ARM64 HFA (Homogeneous Floating-point Aggregate) only.
;;;            Pass/return in single-precision FP registers (s0-s3).
;;;            x86-64 never uses this; single-floats become :double (SSE class).
;;;
;;; :double  - Pass/return in floating-point/SSE registers.
;;;            On x86-64: SSE class (XMM0/XMM1) for both single and double floats.
;;;            On ARM64: HFA double-precision (d0-d3).
;;;
;;; :memory  - Struct too large for registers; pass/return via hidden pointer.
;;;            x86-64: hidden pointer in RDI, returned in RAX.
;;;            ARM64: hidden pointer in x8.
(deftype struct-class () '(member :integer :single :double :memory))

(defstruct (struct-classification (:copier nil))
  ;; List of register slot classifications
  ;; Each element represents one register's worth of data
  (register-slots nil :type list)
  ;; Total size in bytes
  (size 0 :type (unsigned-byte 32))
  ;; Required alignment
  (alignment 1 :type (unsigned-byte 16))
  ;; Whether this struct must be returned via hidden pointer
  (memory-p nil :type boolean))

;;; Main entry point: classify a struct type for ABI compliance
;;; Returns: (values in-registers-p register-slots size)
;;;   in-registers-p - T if struct can be returned in registers
;;;   register-slots - list of slot classes for each register
;;;   size - total size in bytes (NIL if not a struct)
(defun struct-return-info (alien-type)
  "Classify how a struct should be returned according to platform ABI.
   Returns (values in-registers-p register-slots size) or (values nil nil nil) for non-structs."
  (declare (ignorable alien-type))
  #+(and (or arm64 x86-64) (not sb-xc-host))
  (progn
    (unless (alien-record-type-p alien-type)
      (return-from struct-return-info (values nil nil nil)))
    (let ((classification (sb-vm::classify-struct alien-type)))
      (when classification
        (values (not (struct-classification-memory-p classification))
                (struct-classification-register-slots classification)
                (struct-classification-size classification))))))

;;; Methods for struct by value - platform-specific implementations in
;;; compiler/{arch}/c-call.lisp define record-arg-tn and record-result-tn.
#+(and (or x86-64 arm64) (not sb-xc-host))
(progn
  (declaim (ftype (function (t t) t) sb-vm::record-arg-tn sb-vm::record-result-tn))
  (define-alien-type-method (record :arg-tn) (type state)
    (sb-vm::record-arg-tn type state))
  (define-alien-type-method (record :result-tn) (type state)
    (sb-vm::record-result-tn type state)))

#-(and (or x86-64 arm64) (not sb-xc-host))
(progn
  (define-alien-type-method (record :arg-tn) (type state)
    (declare (ignore type state))
    (error "Passing structs by value is unsupported on this platform."))
  (define-alien-type-method (record :result-tn) (type state)
    (declare (ignore type state))
    (error "Returning structs by value is unsupported on this platform.")))

