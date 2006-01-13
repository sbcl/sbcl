;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!ALIEN")

(/show0 "host-c-call.lisp 12")

(define-alien-type-class (c-string :include pointer :include-args (to)))

(define-alien-type-translator c-string ()
  (make-alien-c-string-type
   :to (parse-alien-type 'char (sb!kernel:make-null-lexenv))))

(define-alien-type-method (c-string :unparse) (type)
  (declare (ignore type))
  'c-string)

(define-alien-type-method (c-string :lisp-rep) (type)
  (declare (ignore type))
  '(or simple-string null (alien (* char))))

(define-alien-type-method (c-string :naturalize-gen) (type alien)
  (declare (ignore type))
  `(if (zerop (sap-int ,alien))
       nil
       (%naturalize-c-string ,alien)))

(define-alien-type-method (c-string :deport-gen) (type value)
  (declare (ignore type))
  `(etypecase ,value
     (null (int-sap 0))
     ((alien (* char)) (alien-sap ,value))
     ;; FIXME: GC safety alert! These SAPs are not safe, since the
     ;; Lisp string can move. This is not hard to arrange, for example
     ;; the following will fail very quickly on a SB-UNICODE build:
     ;;
     ;;   (setf (bytes-consed-between-gcs) 4096)
     ;;   (define-alien-routine "strcmp" int (s1 c-string) (s2 c-string))
     ;;
     ;;   (loop
     ;;     (let ((string "hello, world"))
     ;;       (assert (zerop (strcmp string string)))))
     ;;
     ;; (This will appear to work on post-0.9.8.19 GENCGC, since
     ;;  the GC no longer zeroes memory immediately after releasing
     ;;  it after a minor GC. Either enabling the READ_PROTECT_FREE_PAGES
     ;;  #define in gencgc.c or modifying the example so that a major
     ;;  GC will occasionally be triggered would unmask the bug).
     ;;
     ;; The SIMPLE-BASE-STRING case will generally be very hard to
     ;; trigger on GENCGC (even when threaded) thanks to GC
     ;; conservativeness. It's mostly a problem on cheneygc.
     ;; -- JES, 2006-01-13
     (simple-base-string (vector-sap ,value))
     ;; This case, on the other hand, will cause trouble on GENCGC, since
     ;; we're taking the SAP of a immediately discarded temporary -> the
     ;; conservativeness doesn't protect us.
     ;; -- JES, 2006-01-13
     (simple-string (vector-sap (coerce ,value 'simple-base-string)))))

(/show0 "host-c-call.lisp 42")

(define-alien-type-class (utf8-string :include pointer :include-args (to)))

(define-alien-type-translator utf8-string ()
  (make-alien-utf8-string-type
   :to (parse-alien-type 'char (sb!kernel:make-null-lexenv))))

(define-alien-type-method (utf8-string :unparse) (type)
  (declare (ignore type))
  'utf8-string)

(define-alien-type-method (utf8-string :lisp-rep) (type)
  (declare (ignore type))
  '(or simple-string null (alien (* char))))

(define-alien-type-method (utf8-string :naturalize-gen) (type alien)
  (declare (ignore type))
  `(if (zerop (sap-int ,alien))
       nil
       (%naturalize-utf8-string ,alien)))

(define-alien-type-method (utf8-string :deport-gen) (type value)
  (declare (ignore type))
  `(etypecase ,value
     (null (int-sap 0))
     ((alien (* char)) (alien-sap ,value))
     ;; See the C-STRING :DEPORT-GEN comments for GC safety issues.
     (simple-base-string (vector-sap ,value))
     (simple-string (vector-sap (%deport-utf8-string ,value)))))

(/show0 "host-c-call.lisp end of file")
