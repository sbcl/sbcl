;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

(!defstruct-with-alternate-metaclass condition
  :slot-names (actual-initargs assigned-slots)
  :boa-constructor %make-condition-object
  :superclass-name t
  :metaclass-name condition-classoid
  :metaclass-constructor make-condition-classoid
  :dd-type structure)

;;; Define just enough condition-classoids to get TYPEP
;;; to optimize into classoid-typep.
(macrolet ((def (name direct-supers &rest inherits)
             `(eval-when (:compile-toplevel :execute)
                ;; INHERITS is a list of symbols, unevaluated,
                ;; excluding T which is assumed.
                (%compiler-define-condition
                 ',name ',direct-supers
                 ,(make-layout
                   :classoid (make-undefined-classoid name)
                   :inherits (map 'vector #'find-layout (cons t inherits))
                   :depthoid -1
                   :length 3)
                 nil nil))))
  ;; These are grotesquely OAOO-violating, but on the bright side,
  ;; compilation will fail if the subsequent real definition differs,
  ;; so there's a built-in safety net.
  ;; As has been suggested in 'cross-condition', a DEF!CONDITION macro
  ;; might help, but still that's possibly not a complete solution,
  ;; because DEBUG-CONDITION is defined in a file with the :NOT-HOST flag.
  (def warning (condition) condition)
  (def style-warning (warning) condition warning)
  (def compiler-note (condition) condition)
  (def parse-unknown-type (condition) condition)
  (def parse-deprecated-type (condition) condition)
  (def serious-condition (condition) condition)
  (def error (serious-condition) condition serious-condition)
  (def sb!di:debug-condition (serious-condition) condition serious-condition)
  (def stream-error (error) condition serious-condition error)
  (def reference-condition (condition) condition)
  )
