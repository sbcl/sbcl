;;; Fortunately, dynamic linking is already working at this stage. On
;;; win32, however, dynamic foreign symbols for link-time dependencies
;;; are not available before explicit LoadLibrary on them.

(in-package "SB-IMPL")
#+win32
(progn
  (load-shared-object "kernel32.dll")
  (load-shared-object #+ucrt "ucrtbase.dll" #-ucrt "msvcrt.dll")
  (load-shared-object "advapi32.dll")
  (load-shared-object "ws2_32.dll")
  (load-shared-object "shell32.dll"))
