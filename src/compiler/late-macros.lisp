;;;; macros which use GET-SETF-EXPANSION in their macroexpander code,
;;;; and hence need special treatment. Currently (19990806) this
;;;; special treatment involves bare calls to SB!XC:DEFMACRO or
;;;; DEFMACRO-MUNDANELY and so this code can't appear in the build
;;;; sequence until after xc DEFMACRO machinery has been set up, and
;;;; so this stuff is separated out of the main compiler/macros.lisp
;;;; file (which has to appear earlier).

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

#+sb-xc-host
(sb!xc:defmacro deletef-in (next place item &environment env)
  (multiple-value-bind (temps vals stores store access)
      (sb!xc:get-setf-expansion place env)
    (when (cdr stores)
      (error "multiple store variables for ~S" place))
    (let ((n-item (gensym))
          (n-place (gensym))
          (n-current (gensym))
          (n-prev (gensym)))
      `(let* (,@(mapcar #'list temps vals)
              (,n-place ,access)
              (,n-item ,item))
         (if (eq ,n-place ,n-item)
             (let ((,(first stores) (,next ,n-place)))
               ,store)
             (do ((,n-prev ,n-place ,n-current)
                  (,n-current (,next ,n-place)
                              (,next ,n-current)))
                 ((eq ,n-current ,n-item)
                  (setf (,next ,n-prev)
                        (,next ,n-current)))))
         (values)))))

;;; Push ITEM onto a list linked by the accessor function NEXT that is
;;; stored in PLACE.
#+sb-xc-host
(sb!xc:defmacro push-in (next item place &environment env)
  (multiple-value-bind (temps vals stores store access)
      (sb!xc:get-setf-expansion place env)
    (when (cdr stores)
      (error "multiple store variables for ~S" place))
    `(let (,@(mapcar #'list temps vals)
           (,(first stores) ,item))
       (setf (,next ,(first stores)) ,access)
       ,store
       (values))))

;;; the target-code case of setting boolean attributes
#+sb-xc-host
(defmacro-mundanely !def-boolean-attribute-setter (test-name
                                                   translations-name
                                                   &rest attribute-names)
  (guts-of-!def-boolean-attribute-setter test-name
                                         translations-name
                                         attribute-names
                                         'sb!xc:get-setf-expansion))
