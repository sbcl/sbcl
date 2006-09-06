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

(define-alien-type-class (c-string :include pointer :include-args (to))
  (external-format :default :type keyword)
  (element-type 'character :type (member character base-char)))

(define-alien-type-translator c-string
    (&key (external-format :default)
          (element-type 'character))
  (make-alien-c-string-type
   :to (parse-alien-type 'char (sb!kernel:make-null-lexenv))
   :element-type element-type
   :external-format external-format))

(defun c-string-external-format (type)
  (let ((external-format (alien-c-string-type-external-format type)))
    (if (eq external-format :default)
        (default-c-string-external-format)
        external-format)))

(define-alien-type-method (c-string :unparse) (type)
  (list 'c-string
        :external-format (alien-c-string-type-external-format type)
        :element-type (alien-c-string-type-element-type type)))

(define-alien-type-method (c-string :lisp-rep) (type)
  (declare (ignore type))
  '(or simple-string null (alien (* char))))

(defun c-string-needs-conversion-p (type)
  #+sb-xc-host
  t
  #-sb-xc-host
  (let ((external-format (sb!impl::get-external-format
                          ;; Can't use C-STRING-EXTERNAL-FORMAT here,
                          ;; since the meaning of :DEFAULT can change
                          ;; when *DEFAULT-C-STRING-EXTERNAL-FORMAT*
                          ;; changes.
                          (alien-c-string-type-external-format type))))
    (not (and external-format
              (or (eq (caar external-format) :ascii)
                  ;; On non-SB-UNICODE all latin-1 codepoints will fit
                  ;; into a base-char, on SB-UNICODE they won't.
                  #!-sb-unicode
                  (eq (caar external-format) :latin-1))))))

(define-alien-type-method (c-string :naturalize-gen) (type alien)
  `(if (zerop (sap-int ,alien))
       nil
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
       ,(if #!+sb-unicode t
            #!-sb-unicode (c-string-needs-conversion-p type)
            `(sb!alien::c-string-to-string ,alien
                                           (c-string-external-format ,type)
                                           (alien-c-string-type-element-type
                                            ,type))
            `(%naturalize-c-string ,alien))))

(define-alien-type-method (c-string :deport-gen) (type value)
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
     ;; The pure VECTOR-SAP branch for the SIMPLE-BASE-STRING case
     ;; will generally be very hard to trigger on GENCGC (even when
     ;; threaded) thanks to GC conservativeness. It's mostly a problem
     ;; on cheneygc.  -- JES, 2006-01-13
     (simple-base-string
      ,(if (c-string-needs-conversion-p type)
           ;; If the alien type is not ascii-compatible (+SB-UNICODE)
           ;; or latin-1-compatible (-SB-UNICODE), we need to do
           ;; external format conversion.
           `(vector-sap (string-to-c-string ,value
                                            (c-string-external-format ,type)))
           ;; Otherwise we can just pass it uncopied.
           `(vector-sap ,value)))
     ;; This case, on the other hand, will cause trouble on GENCGC, since
     ;; we're taking the SAP of a immediately discarded temporary -> the
     ;; conservativeness doesn't protect us.
     ;; -- JES, 2006-01-13
     (simple-string
      (vector-sap (string-to-c-string ,value
                                      (c-string-external-format ,type))))))

(/show0 "host-c-call.lisp end of file")
