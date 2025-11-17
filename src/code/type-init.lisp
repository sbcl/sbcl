;;;; When this file's top level forms are run, it precomputes the
;;;; translations for commonly used type specifiers. This stuff is
;;;; split off from the other type stuff to get around problems with
;;;; everything needing to be loaded before everything else. This file
;;;; is the first to exercise the type machinery. This stuff is also
;;;; somewhat implementation-dependent in that implementations may
;;;; want to precompute other types which are important to them.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

;;; built-in classes
(/show0 "beginning type-init.lisp")

;; We want to assign SAETP-CTYPE for each SAETP instance, but we can't do it
;; just yet because CHARACTER and FIXNUM are not acceptable to SPECIFIER-TYPE.
;; The reason they're not is that each is a builtin classoid with a translation
;; that can be given to SPECIFIER-TYPE as it has a translator:
;;  CHARACTER =  (CHARACTER-SET)
;;  FIXNUM = `(INTEGER ,MOST-NEGATIVE-FIXNUM ,MOST-POSITIVE-FIXNUM)
;; but the classoid's TRANSLATION slot isn't set yet.
;; So ideally we could commence with the loop over *builtin-classoids*
;; assigning each their translation, and then come back to filling in SAETP-CTYPE.
;; That's where we hit a circularity: You can't parse BIT-VECTOR because while we
;; know that it's (ARRAY BIT (*)), in order to parse (ARRAY <x>) you have to call
;; %UPGRADED-ARRAY-ELEMENT-TYPE with the parse of <x>, looking for the first SAETP
;; whosed -CTYPE is a supertype of <x>. But the SAETP-CTYPEs aren't filled in!
;; The solution I favor is to process CHARACTER and FIXNUM in *builtin-classoids*,
;; then assign all SAETP-CTYPES, then do the rest of *builtin-classoids*.
(dolist (spec '(character fixnum))
  (let* ((bic-entry (assoc spec *builtin-classoids*))
         (translation (getf (cdr bic-entry) :translation))
         (classoid (find-classoid spec)))
    (aver classoid)
    ;; In cold-init, the TRANSLATION is already in its internal representation
    ;; as though SPECIFIER-TYPE were called. This is extremely convenient
    ;; because parsing doesn't work. See the call to !MAKE-BUILT-IN-CLASSOID
    ;; in class.lisp - it sets :TRANSLATION to :INITIALIZING only in the host.
    (let ((ctype #+sb-xc-host (specifier-type translation)
                 #-sb-xc-host translation))
      (aver (ctype-p ctype))
      (aver (not (unknown-type-p ctype)))
      (setf (built-in-classoid-translation classoid) ctype)
      (setf (info :type :builtin spec) ctype))))

(dovector (saetp sb-vm:*specialized-array-element-type-properties*)
  (let* ((spec (sb-vm:saetp-specifier saetp))
         (ctype (specifier-type spec)))
    (aver (not (unknown-type-p ctype)))
    (setf (sb-vm:saetp-ctype saetp) ctype)))

(setf (info :type :kind 'number) :primitive
      (info :type :builtin 'number)
      #+sb-xc-host
      (specifier-type '(or real complex))
      #-sb-xc-host (specifier-type 'number))

#+sb-xc-host
(dolist (x *builtin-classoids*)
  (destructuring-bind (name &key (translation nil trans-p) &allow-other-keys)
      x
    (/show "doing class with" name)
    (when trans-p
      (let ((classoid (classoid-cell-classoid (find-classoid-cell name :create t)))
            (type (specifier-type translation)))
        (assert (not (unknown-type-p type)) (type) "Unknown type ~a" type)
        (when (typep (built-in-classoid-translation classoid) 'ctype)
          (aver (eq (built-in-classoid-translation classoid) type)))
        (setf (built-in-classoid-translation classoid) type)
        (setf (info :type :builtin name) type)))))

;;; the Common Lisp defined type spec symbols
(defconstant-eqx +!standard-type-names+
  '(array atom bignum bit bit-vector character compiled-function
    complex cons double-float extended-char fixnum float function
    hash-table integer keyword list long-float nil null number package
    pathname random-state ratio rational real readtable sequence
    short-float simple-array simple-bit-vector simple-string simple-vector
    single-float standard-char stream string base-char symbol t vector)
  #'equal)

;;; built-in symbol type specifiers

;;; Predefined types that are of kind :INSTANCE can't have their
;;; :BUILTIN property set, so we cull them out. This used to operate
;;; on all +!STANDARD-TYPE-NAMES+ because !PRECOMPUTE-TYPES was ok to
;;; call on unknown types. This relied upon the knowledge that
;;; VALUES-SPECIFIER-TYPE avoided signaling a PARSE-UNKNOWN-TYPE
;;; condition while in cold-init. This is terrible! It means that
;;; (a) we have to know that something wouldn't signal
;;;     when it otherwise should, and
;;; (b) we can call that thing when the very data that it depends on
;;;     are actually wrong.
;;; Well, in as much as we have to do this suspicious action,
;;; at least let's not get into a state where we *know*
;;; that it ought to signal.

(/show0 "precomputing built-in symbol type specifiers")
(!precompute-types
 (remove-if (lambda (x)
              (memq x '(compiled-function hash-table package pathname
                        random-state readtable)))
            +!standard-type-names+))

#+sb-xc-host (setf *type-system-initialized* t)

(/show0 "done with type-init.lisp")
