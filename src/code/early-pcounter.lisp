;;;; PCOUNTERs
;;;;
;;;; a PCOUNTER is used to represent an unsigned integer quantity which
;;;; can grow bigger than a fixnum, but typically does so, if at all,
;;;; in many small steps, where we don't want to cons on every step.
;;;; Such quantities typically arise in profiling, e.g. 
;;;; total system consing, time spent in a profiled function, and
;;;; bytes consed in a profiled function are all examples of such
;;;; quantities. The name is an abbreviation for "Profiling COUNTER".

;;; This stuff is implemented in the SB!PROFILE package because the
;;; profiling code is currently the only code which wants to poke
;;; around in the implementation details. This needs to be done on the
;;; host for type information.

(in-package "SB!PROFILE")

(def!struct (pcounter (:copier nil))
  (integer 0 :type unsigned-byte)
  (fixnum 0 :type (and fixnum unsigned-byte)))

