(in-package :cl-user)
(require :sb-sprof)

#-(or win32 darwin)                    ;not yet
(sb-sprof::test)
#-(or win32 darwin)                    ;not yet
(sb-sprof::consing-test)

;; For debugging purposes, print output for visual inspection to see if
;; the allocation sequence gets hit in the right places (i.e. not at all
;; in traditional builds, and everywhere if SB-SAFEPOINT-STRICTLY is
;; enabled.)
(disassemble #'sb-sprof::consalot)
