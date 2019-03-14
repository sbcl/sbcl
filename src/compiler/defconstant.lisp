;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

(declaim (ftype (sfunction (symbol t &optional t t) null)
                about-to-modify-symbol-value))
;;; the guts of DEFCONSTANT

(defun sb-c::%defconstant (name value source-location &optional (doc nil docp))
  #+sb-xc-host (declare (ignore doc docp))
  (unless (symbolp name)
    (error "The constant name is not a symbol: ~S" name))
  (with-single-package-locked-error (:symbol name
                                             "defining ~s as a constant")
   (when (looks-like-name-of-special-var-p name)
     (style-warn 'asterisks-around-constant-variable-name
                 :format-control "Defining ~S as a constant"
                 :format-arguments (list name)))
   (when source-location
     (setf (info :source-location :constant name) source-location))
   (let ((kind (info :variable :kind name)))
     (case kind
       (:constant
        ;; Note: This behavior (discouraging any non-EQL modification)
        ;; is unpopular, but it is specified by ANSI (i.e. ANSI says a
        ;; non-EQL change has undefined consequences). If people really
        ;; want bindings which are constant in some sense other than
        ;; EQL, I suggest either just using DEFVAR (which is usually
        ;; appropriate, despite the un-mnemonic name), or defining
        ;; something like the DEFCONSTANT-EQX macro used in SBCL (which
        ;; is occasionally more appropriate). -- WHN 2001-12-21
        (if (boundp name)
            (if (typep name '(or boolean keyword))
                ;; Non-continuable error.
                (about-to-modify-symbol-value name 'defconstant)
                (let ((old (symbol-value name)))
                  (unless (eql value old)
                    (multiple-value-bind (ignore aborted)
                        (with-simple-restart (abort "Keep the old value.")
                          (cerror "Go ahead and change the value."
                                  'defconstant-uneql
                                  :name name
                                  :old-value old
                                  :new-value value))
                      (declare (ignore ignore))
                      (when aborted
                        (return-from sb-c::%defconstant name))))))
            (warn "redefining a MAKUNBOUND constant: ~S" name)))
       (:unknown
        ;; (This is OK -- undefined variables are of this kind. So we
        ;; don't warn or error or anything, just fall through.)
        )
       (t (warn "redefining ~(~A~) ~S to be a constant" kind name)))))
  ;; We ought to be consistent in treating any change of :VARIABLE :KIND
  ;; as a continuable error. The above CASE expression pre-dates the
  ;; existence of symbol-macros (I believe), but at a bare minimum,
  ;; INFO should return NIL for its second value if requesting the
  ;; :macro-expansion of something that is getting defined as constant.
  (clear-info :variable :macro-expansion name)
  (clear-info :source-location :symbol-macro name)
  #-sb-xc-host
  (progn
    (when docp
      (setf (documentation name 'variable) doc))
    (%set-symbol-value name value))
  (setf (info :variable :kind name) :constant)
  name)
