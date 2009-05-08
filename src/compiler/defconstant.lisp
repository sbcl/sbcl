;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(def!macro sb!xc:defconstant (name value &optional documentation)
  #!+sb-doc
  "Define a global constant, saying that the value is constant and may be
  compiled into code. If the variable already has a value, and this is not
  EQL to the new value, the code is not portable (undefined behavior). The
  third argument is an optional documentation string for the variable."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (sb!c::%defconstant ',name ,value ',documentation
      (sb!c:source-location))))

;;; the guts of DEFCONSTANT
(defun sb!c::%defconstant (name value doc source-location)
  (unless (symbolp name)
    (error "The constant name is not a symbol: ~S" name))
  (when (looks-like-name-of-special-var-p name)
    (style-warn 'sb!kernel:asterisks-around-constant-variable-name
                :format-control "defining ~S as a constant"
                :format-arguments (list name)))
  (sb!c:with-source-location (source-location)
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
                       (return-from sb!c::%defconstant name))))))
           (warn "redefining a MAKUNBOUND constant: ~S" name)))
      (:unknown
       ;; (This is OK -- undefined variables are of this kind. So we
       ;; don't warn or error or anything, just fall through.)
       )
      (t (warn "redefining ~(~A~) ~S to be a constant" kind name))))
  (when doc
    (setf (fdocumentation name 'variable) doc))
  #-sb-xc-host
  (%set-symbol-value name value)
  #+sb-xc-host
  (progn
    ;; Redefining our cross-compilation host's CL symbols would be poor form.
    ;;
    ;; FIXME: Having to check this and then not treat it as a fatal error
    ;; seems like a symptom of things being pretty broken. It's also a problem
    ;; in and of itself, since it makes it too easy for cases of using the
    ;; cross-compilation host Lisp's CL constant values in the target Lisp to
    ;; slip by. I got backed into this because the cross-compiler translates
    ;; DEFCONSTANT SB!XC:FOO into DEFCONSTANT CL:FOO. It would be good to
    ;; unscrew the cross-compilation package hacks so that that translation
    ;; doesn't happen. Perhaps: * Replace SB-XC with SB-CL. SB-CL exports all
    ;; the symbols which ANSI requires to be exported from CL. * Make a
    ;; nickname SB!CL which behaves like SB!XC. * Go through the
    ;; loaded-on-the-host code making every target definition be in SB-CL.
    ;; E.g. DEFMACRO-MUNDANELY DEFCONSTANT becomes DEFMACRO-MUNDANELY
    ;; SB!CL:DEFCONSTANT. * Make IN-TARGET-COMPILATION-MODE do UNUSE-PACKAGE
    ;; CL and USE-PACKAGE SB-CL in each of the target packages (then undo it
    ;; on exit). * Make the cross-compiler's implementation of EVAL-WHEN
    ;; (:COMPILE-TOPLEVEL) do UNCROSS. (This may not require any change.) *
    ;; Hack GENESIS as necessary so that it outputs SB-CL stuff as COMMON-LISP
    ;; stuff. * Now the code here can assert that the symbol being defined
    ;; isn't in the cross-compilation host's CL package.
    (unless (eql (find-symbol (symbol-name name) :cl) name)
      ;; KLUDGE: In the cross-compiler, we use the cross-compilation host's
      ;; DEFCONSTANT macro instead of just (SETF SYMBOL-VALUE), in order to
      ;; get whatever blessing the cross-compilation host may expect for a
      ;; global (SETF SYMBOL-VALUE). (CMU CL, at least around 2.4.19,
      ;; generated full WARNINGs for code -- e.g. DEFTYPE expanders -- which
      ;; referred to symbols which had been set by (SETF SYMBOL-VALUE). I
      ;; doubt such warnings are ANSI-compliant, but I'm not sure, so I've
      ;; written this in a way that CMU CL will tolerate and which ought to
      ;; work elsewhere too.) -- WHN 2001-03-24
      (eval `(defconstant ,name ',value)))
    ;; It would certainly be awesome if this was only needed for symbols
    ;; in CL. Unfortunately, that is not the case. Maybe some are moved
    ;; back in CL later on?
    (setf (info :variable :xc-constant-value name) value))
  (setf (info :variable :kind name) :constant)
  name)
