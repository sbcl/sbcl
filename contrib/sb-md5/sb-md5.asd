;;;; MD5 --- RFC 1321 The MD5 Message-Digest Algorithm

;;;; %File Description:
;;;;
;;;; This file contains the system definition form for the MD5
;;;; Library.  System definitions use the ASDF system definition
;;;; facility.
;;;;

(error "Can't build contribs with ASDF")

(defsystem "sb-md5"
  :description "The MD5 Message-Digest Algorithm RFC 1321"
  :author "Pierre R. Mai <pmai@pmsf.de>"
  :maintainer "Pierre R. Mai <pmai@pmsf.de>"
  :licence "CC0"
  :version "2.0.4"
  :depends-on (#-(or :cmu :sbcl
                     (and :lispworks (not :lispworks4))
                     :ccl :allegro)
               "flexi-streams")
  :components ((:file "md5")))
