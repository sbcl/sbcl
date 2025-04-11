;;; This is needed by genesis and SB-EDITCORE
(defpackage "SB-COREFILE"
  (:use "CL")
  (:export #:core-magic
           #:runtime-options-magic
           #:build-id-core-entry-type-code
           #:directory-core-entry-type-code
           #:initial-fun-core-entry-type-code
           #:static-constants-core-entry-type-code
           #:page-table-core-entry-type-code
           #:alien-linkage-table-core-entry-type-code
           #:lisp-linkage-space-core-entry-type-code
           #:end-core-entry-type-code
           ;;
           #:read-only-core-space-id
           #:static-core-space-id
           #:static-code-core-space-id
           #:dynamic-core-space-id
           #:immobile-fixedobj-core-space-id
           #:immobile-text-core-space-id
           #:permgen-core-space-id
           #:alien-linkage-table-core-space-id
           #:thread-struct-core-space-id
           #:deflated-core-space-id-flag))

(in-package "SB-COREFILE")

;;; This constant is computed as
;;;  (logior (ash (char-code #\S) 24)
;;;          (ash (char-code #\B) 16)
;;;          (ash (char-code #\C) 8)
;;;          (char-code #\L))
;;; But it needs to use SB-XC:CHAR-CODE when cross-compiling,
;;; so it's not easily shared between genesis and editcore
;;; without a bit of computation by hand.
(defconstant core-magic #x5342434C)
(defconstant runtime-options-magic #x31EBF355)

;;; magic numbers to identify entries in a core file
;;;
;;; These are arbitrary words, tested not for being in a particular range,
;;; but just for equality. However, if you ever need to look at a .core file
;;; and figure out what's going on, it's slightly convenient that they're
;;; all in an easily recognizable range, and displacing the range away from
;;; zero seems likely to reduce the chance that random garbage will be
;;; misinterpreted as a .core file.)
(defconstant build-id-core-entry-type-code 3860)
(defconstant directory-core-entry-type-code 3861)
(defconstant initial-fun-core-entry-type-code 3863)
(defconstant page-table-core-entry-type-code 3880)
(defconstant alien-linkage-table-core-entry-type-code 3881)
(defconstant lisp-linkage-space-core-entry-type-code 3882)
(defconstant static-constants-core-entry-type-code 3883)
(defconstant end-core-entry-type-code 3840)

(defconstant dynamic-core-space-id 1)
(defconstant static-core-space-id 2)
(defconstant read-only-core-space-id 3)
(defconstant immobile-fixedobj-core-space-id 4)
(defconstant immobile-text-core-space-id 5)
(defconstant permgen-core-space-id 6)
(defconstant static-code-core-space-id 4)
(defconstant deflated-core-space-id-flag 8)
;;; These space-IDs are not present in a core file, but
;;; we need unique values for supplying to os_alloc_gc_space
(defconstant alien-linkage-table-core-space-id 101)
(defconstant thread-struct-core-space-id 102)
