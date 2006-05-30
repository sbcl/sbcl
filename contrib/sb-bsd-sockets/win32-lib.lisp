(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-alien:load-shared-object "ws2_32.dll")
  (sb-alien:load-shared-object "msvcrt.dll"))
