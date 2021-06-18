(in-package "SB-C")

(!begin-collecting-cold-init-forms)

(define-load-time-global *!initial-parsed-types* nil)
(!cold-init-forms
 (dovector (saetp sb-vm:*specialized-array-element-type-properties*)
   (setf (sb-vm:saetp-ctype saetp) (specifier-type (sb-vm:saetp-specifier saetp))))
  ;; This seems so weird and random. I really wanted to remove it, but
  ;; adding the :BUILTIN property makes sense because without it, the type
  ;; does not have a unique parse. Probably a missing entry in one of the
  ;; pieces of type machinery voodoo.
  ;; Unfortunately, there are lots of other non-unique parses amongst the
  ;; standardized specifiers, most notably LIST which can be internalized
  ;; as either permutation of (OR (MEMBER NIL) CONS), and ATOM which
  ;; might create a new instance of (NOT CONS) on each parse.
  ;;
  ;; Changing the :KIND from :DEFINED to :PRIMITIVE makes sense too so that
  ;; it can't be redefined. (Package lock on CL prevents that anyway)
  ;; But some of of these properties are seemingly redundant with each other-
  ;; :BUILTIN essentially means :PRIMITIVE in this case.
 (let ((spec 'compiled-function))
   (setf (info :type :builtin spec) (specifier-type spec)
         (info :type :kind spec) :primitive))
 #-sb-xc-host
 (dovector (pair *!initial-parsed-types*)
   (destructuring-bind (spec . parse) pair
     (drop-all-hash-caches) ; start from a relative vacuum
     (unless (sb-kernel::brute-force-type-specifier-equalp (type-specifier parse) spec)
       (let ((*print-length* nil))
         (write-string "parse/unparse: ")
         (terpri)
         (write spec)
         (terpri)
         (write parse)
         (terpri)
         (write (type-specifier parse))
         (terpri)
         (bug "type parsed->unparse round-trip fail"))))))

(!defun-from-collected-cold-init-forms !fixup-type-cold-init)
