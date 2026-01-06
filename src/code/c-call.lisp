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

;;; Classification categories for struct fields
;;; :integer - pass/return in general-purpose registers
;;; :sse-single - pass/return in floating-point registers (single precision)
;;; :sse-double - pass/return in floating-point registers (double precision)
;;; :memory - pass/return via hidden pointer (struct too large for registers)
;;; :no-class - padding or empty (x86-64 specific)
(deftype struct-class () '(member :integer :sse :sse-single :sse-double :memory :no-class))

(defstruct (struct-classification (:copier nil))
  ;; List of register slot classifications (:integer, :sse-single, :sse-double, :memory)
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
#-sb-xc-host
(defun struct-return-info (alien-type)
  "Classify how a struct should be returned according to platform ABI.
   Returns (values in-registers-p register-slots size) or (values nil nil nil) for non-structs."
  (unless (alien-record-type-p alien-type)
    (return-from struct-return-info (values nil nil nil)))
  (let ((classification
          #+arm64 (sb-vm::classify-struct-arm64 alien-type)
          #+(or x86-64 x86) (sb-vm::classify-struct-x86-64 alien-type)
          #-(or arm64 x86-64 x86) nil))
    (when classification
      (values (not (struct-classification-memory-p classification))
              (struct-classification-register-slots classification)
              (struct-classification-size classification)))))

#+sb-xc-host
(defun struct-return-info (alien-type)
  (declare (ignore alien-type))
  (error "struct-return-info not available during cross-compilation"))

;;; Methods for struct by value
;;; During cross-compilation, these throw errors.
;;; At runtime in target, they dispatch to platform-specific helpers.

;;; Forward declarations for platform-specific helpers (defined in compiler/{arch}/c-call.lisp)
;;; Note: These are in SB-VM package, not SB-C
#-sb-xc-host
(progn
  #+(or x86-64 x86)
  (declaim (ftype (function (t t) t) sb-vm::record-arg-tn-x86-64 sb-vm::record-result-tn-x86-64))
  #+arm64
  (declaim (ftype (function (t t) t) sb-vm::record-arg-tn-arm64 sb-vm::record-result-tn-arm64)))

#-sb-xc-host
(define-alien-type-method (record :arg-tn) (type state)
  #+(or x86-64 x86) (sb-vm::record-arg-tn-x86-64 type state)
  #+arm64 (sb-vm::record-arg-tn-arm64 type state)
  #-(or x86-64 x86 arm64) (error "Passing structs by value: ~S ~S" type state))
#+sb-xc-host
(define-alien-type-method (record :arg-tn) (type state)
  (error "Passing structs by value: ~S ~S" type state))

#-sb-xc-host
(define-alien-type-method (record :result-tn) (type state)
  #+(or x86-64 x86) (sb-vm::record-result-tn-x86-64 type state)
  #+arm64 (sb-vm::record-result-tn-arm64 type state)
  #-(or x86-64 x86 arm64) (error "Returning structs by value: ~S ~S" type state))
#+sb-xc-host
(define-alien-type-method (record :result-tn) (type state)
  (error "Returning structs by value: ~S ~S" type state))

