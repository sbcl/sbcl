;;; -*- Lisp -*-

("fiber.h" "stddef.h")

((:integer-no-check +fiber-new+      "FIBER_NEW"      nil t)
 (:integer-no-check +fiber-runnable+ "FIBER_RUNNABLE" nil t)
 (:integer-no-check +fiber-running+  "FIBER_RUNNING"  nil t)
 (:integer-no-check +fiber-dead+     "FIBER_DEAD"     nil t)

 ;; (only map the fields the Lisp shim actually touches)
 (:structure sb-fiber-c ("struct sb_fiber"
                         ((signed 32) state "int" "state")
                         (integer owner "void *" "owner")
                         (integer return-fiber "void *" "return_fiber")
                         (integer catch "lispobj" "current_catch_block")
                         (integer unwind "lispobj" "current_unwind_protect_block")
                         (integer bsp "lispobj *" "binding_stack_pointer")
                         (integer bs-start "lispobj *" "binding_stack_start_for_thread"))))
