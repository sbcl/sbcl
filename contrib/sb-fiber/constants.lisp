;;; -*- Lisp -*-

("fiber.h" "stddef.h")

((:integer-no-check +fiber-new+      "FIBER_NEW")
 (:integer-no-check +fiber-runnable+ "FIBER_RUNNABLE")
 (:integer-no-check +fiber-running+  "FIBER_RUNNING")
 (:integer-no-check +fiber-dead+     "FIBER_DEAD")

 ;; (only map the fields the Lisp shim actually touches)
 (:structure fiber-ctx ("struct sb_fiber_ctx"
                         ((signed 32) state "int" "state")
                         (integer owner "void *" "owner")
                         (integer return-fiber "void *" "return_fiber")
                         (integer catch "lispobj" "current_catch_block")
                         (integer unwind "lispobj" "current_unwind_protect_block")
                         (integer bsp "lispobj *" "binding_stack_pointer")
                         (integer bs-start "lispobj *" "binding_stack_start_for_thread")
                         (integer bs-base "lispobj *" "binding_stack_base")
                         (integer bs-end "lispobj *" "binding_stack_end"))))
