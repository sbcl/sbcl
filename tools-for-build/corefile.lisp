
;;; This package name does not persist after the build is complete,
;;; so it does not have an "!" in it.
;;; It it needed by genesis and SB-EDITCORE
(defpackage "SB-COREFILE"
  (:use "CL")
  (:export #:core-magic
           #:build-id-core-entry-type-code
           #:directory-core-entry-type-code
           #:initial-fun-core-entry-type-code
           #:page-table-core-entry-type-code
           #:linkage-table-core-entry-type-code
           #:end-core-entry-type-code
           #:max-core-space-id
           ;;
           #:read-only-core-space-id
           #:static-core-space-id
           #:static-code-core-space-id
           #:dynamic-core-space-id
           #:immobile-fixedobj-core-space-id
           #:immobile-varyobj-core-space-id
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
(defconstant linkage-table-core-entry-type-code 3881)
(defconstant end-core-entry-type-code 3840)

(defconstant dynamic-core-space-id 1)
(defconstant static-core-space-id 2)
(defconstant read-only-core-space-id 3)
(defconstant immobile-fixedobj-core-space-id 4)
(defconstant immobile-varyobj-core-space-id 5)
(defconstant static-code-core-space-id 4)
(defconstant deflated-core-space-id-flag 8)
