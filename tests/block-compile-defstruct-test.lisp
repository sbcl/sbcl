;;; Prior to the change that allowed forward-references to slot types,
;;; the MAKE-S1 constructor would have used a "cached typep" placeholder
;;; for structure types S2 and S3; and MAKE-S2 would have used one for S3.
;;; The placeholder lazily figures out that a symbol references a now-defined
;;; defstruct, and it tries to precompute a way to as-efficiently-as-possible
;;; test for that type, given that it couldn't wire in the test to start with.
;;; Only S3 would have been compiled correctly from the outset because it
;;; makes backwards references and no forward references.
;;;
;;; But now with the DEFSTRUCT improvements, the type checks for the slot
;;; named A all compile to basically the same thing in each MAKE- function,
;;; without use of placeholders nor just-in-time optimization attempts.
;;;
(defstruct s1 (a nil :type (or s1 s2 s3 null)))
(defstruct s2 (a nil :type (or s1 s2 s3 null)))
(defstruct s3 (a nil :type (or s1 s2 s3 null)))
