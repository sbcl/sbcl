;;;; ANSI requires CONS be supported as a compound type. The CMU CL
;;;; version which SBCL was forked from didn't support this, but
;;;; various patches made around May 2000 added support for this to
;;;; CMU CL. This file contains tests of their functionality.

(cl:in-package :cl-user)

(declaim (optimize (debug 3) (speed 2) (space 1)))

;;; None of this is going to work until SBCL is patched.
#|
(assert (not (typep 11 'cons)))
(assert (not (typep 11 '(cons *))))
(assert (not (typep 11 '(cons t t))))

(assert (not (typep '() 'cons)))
(assert (typep '(100) 'cons))
(assert (typep '(100) '(cons t)))
(assert (typep '(100) '(cons number)))
(assert (not (typep '(100) '(cons character))))
(assert (typep '(100) '(cons number t)))
(assert (typep '(100) '(cons number null)))
(assert (not (typep '(100) '(cons number string))))

(assert (typep '("yes" no) '(cons string symbol)))
(assert (not (typep '(yes no) '(cons string symbol))))
(assert (not (typep '(yes "no") '(cons string symbol))))
(assert (typep '(yes "no") '(cons symbol)))
(assert (typep '(yes "no") '(cons symbol t)))
(assert (typep '(yes "no") '(cons t string)))
(assert (not (typep '(yes "no") '(cons t null))))

(assert (subtypep '(cons t) 'cons))
(assert (subtypep 'cons '(cons t) ))
(assert (subtypep '(cons t *) 'cons))
(assert (subtypep 'cons '(cons t *) ))
(assert (subtypep '(cons * *) 'cons))
(assert (subtypep 'cons '(cons * *) ))

(assert (subtypep '(cons number *) 'cons ))
(assert (not (subtypep 'cons '(cons number *))))
(assert (subtypep '(cons * number) 'cons ))
(assert (not (subtypep 'cons '(cons * number))))
(assert (subtypep '(cons structure-object number) 'cons ))
(assert (not (subtypep 'cons '(cons structure-object number))))

(assert (subtypep '(cons null fixnum) (type-of '(nil 44))))
|#

(sb-ext:quit :unix-status 104) ; success
