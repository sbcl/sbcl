(cl:in-package :cl-user)
(defpackage :asdf-install
  (:use "CL" "SB-EXT"  "SB-BSD-SOCKETS")
  (:export
   ;; customizable variables
   #:*proxy* #:*cclan-mirror* #:*sbcl-home*
   #:*verify-gpg-signatures* #:*locations*
   #:*safe-url-prefixes*
   ;; entry point
   #:install))

(defpackage :asdf-install-customize
  (:use "CL" "SB-EXT"  "SB-BSD-SOCKETS" "ASDF-INSTALL"))
