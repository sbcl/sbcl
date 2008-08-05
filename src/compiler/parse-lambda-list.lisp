;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

(/show0 "parse-lambda-list.lisp 12")

;;; Break something like a lambda list (but not necessarily actually a
;;; lambda list, e.g. the representation of argument types which is
;;; used within an FTYPE specification) into its component parts. We
;;; return twelve values:
;;;  1. a list of the required args;
;;;  2. a list of the &OPTIONAL arg specs;
;;;  3. true if a &REST arg was specified;
;;;  4. the &REST arg;
;;;  5. true if &KEY args are present;
;;;  6. a list of the &KEY arg specs;
;;;  7. true if &ALLOW-OTHER-KEYS was specified.;
;;;  8. true if any &AUX is present (new in SBCL vs. CMU CL);
;;;  9. a list of the &AUX specifiers;
;;; 10. true if a &MORE arg was specified;
;;; 11. the &MORE context var;
;;; 12. the &MORE count var;
;;; 13. true if any lambda list keyword is present (only for
;;;     PARSE-LAMBDA-LIST-LIKE-THING).
;;;
;;; The top level lambda list syntax is checked for validity, but the
;;; arg specifiers are just passed through untouched. If something is
;;; wrong, we use COMPILER-ERROR, aborting compilation to the last
;;; recovery point.
(declaim (ftype (sfunction (list &key (:silent boolean))
                           (values list list boolean t boolean list boolean
                                   boolean list boolean t t boolean))
                parse-lambda-list-like-thing))
(declaim (ftype (sfunction (list)
                           (values list list boolean t boolean list boolean
                                   boolean list boolean t t))
                parse-lambda-list))
(defun parse-lambda-list-like-thing (list &key silent)
  (collect ((required)
            (optional)
            (keys)
            (aux))
    (let ((restp nil)
          (rest nil)
          (morep nil)
          (more-context nil)
          (more-count nil)
          (keyp nil)
          (auxp nil)
          (allowp nil)
          (state :required))
      (declare (type (member :allow-other-keys :aux
                             :key
                             :more-context :more-count
                             :optional
                             :post-more :post-rest
                             :required :rest)
                     state))
      (dolist (arg list)
        (if (member arg sb!xc:lambda-list-keywords)
            (case arg
              (&optional
               (unless (eq state :required)
                 (compiler-error "misplaced &OPTIONAL in lambda list: ~S"
                                 list))
               (setq state :optional))
              (&rest
               (unless (member state '(:required :optional))
                 (compiler-error "misplaced &REST in lambda list: ~S" list))
               (setq state :rest))
              (&more
               (unless (member state '(:required :optional))
                 (compiler-error "misplaced &MORE in lambda list: ~S" list))
               (setq morep t
                     state :more-context))
              (&key
               (unless (member state
                               '(:required :optional :post-rest :post-more))
                 (compiler-error "misplaced &KEY in lambda list: ~S" list))
               #-sb-xc-host
               (when (optional)
                 (unless silent
                   (compiler-style-warn
                    "&OPTIONAL and &KEY found in the same lambda list: ~S" list)))
               (setq keyp t
                     state :key))
              (&allow-other-keys
               (unless (eq state ':key)
                 (compiler-error "misplaced &ALLOW-OTHER-KEYS in ~
                                  lambda list: ~S"
                                 list))
               (setq allowp t
                     state :allow-other-keys))
              (&aux
               (when (member state '(:rest :more-context :more-count))
                 (compiler-error "misplaced &AUX in lambda list: ~S" list))
               (when auxp
                 (compiler-error "multiple &AUX in lambda list: ~S" list))
               (setq auxp t
                     state :aux))
              (t
               ;; It could be argued that &WHOLE and friends would be
               ;; just ordinary variables in an ordinary lambda-list,
               ;; but since (1) that seem exceedingly to have been the
               ;; programmers intent and (2) the spec can be
               ;; interpreted as giving as licence to signal an
               ;; error[*] that is what we do.
               ;;
               ;; [* All lambda list keywords used in the
               ;; implementation appear in LAMBDA-LIST-KEYWORDS. Each
               ;; member of a lambda list is either a parameter
               ;; specifier ot a lambda list keyword. Ergo, symbols
               ;; appearing in LAMBDA-LIST-KEYWORDS cannot be
               ;; parameter specifiers.]
               (compiler-error 'simple-program-error
                               :format-control "Bad lambda list keyword ~S in: ~S"
                               :format-arguments (list arg list))))
            (progn
              (when (symbolp arg)
                (let ((name (symbol-name arg)))
                  (when (and (plusp (length name))
                             (char= (char name 0) #\&))
                    ;; Should this be COMPILER-STYLE-WARN?
                    (unless silent
                      (style-warn
                       "suspicious variable in lambda list: ~S." arg)))))
              (case state
                (:required (required arg))
                (:optional (optional arg))
                (:rest
                 (setq restp t
                       rest arg
                       state :post-rest))
                (:more-context
                 (setq more-context arg
                       state :more-count))
                (:more-count
                 (setq more-count arg
                       state :post-more))
                (:key (keys arg))
                (:aux (aux arg))
                (t
                 (compiler-error "found garbage in lambda list when expecting ~
                                  a keyword: ~S"
                                 arg))))))
      (when (eq state :rest)
        (compiler-error "&REST without rest variable"))

      (values (required) (optional) restp rest keyp (keys) allowp auxp (aux)
              morep more-context more-count
              (neq state :required)))))

;;; like PARSE-LAMBDA-LIST-LIKE-THING, except our LAMBDA-LIST argument
;;; really *is* a lambda list, not just a "lambda-list-like thing", so
;;; can barf on things which're illegal as arguments in lambda lists
;;; even if they could conceivably be legal in not-quite-a-lambda-list
;;; weirdosities
(defun parse-lambda-list (lambda-list)
  ;; Classify parameters without checking their validity individually.
  (multiple-value-bind (required optional restp rest keyp keys allowp auxp aux
                        morep more-context more-count)
      (parse-lambda-list-like-thing lambda-list)

    ;; Check validity of parameters.
    (flet ((need-symbol (x why)
             (unless (symbolp x)
               (compiler-error "~A is not a symbol: ~S" why x))))
      (dolist (i required)
        (need-symbol i "Required argument"))
      (dolist (i optional)
        (typecase i
          (symbol)
          (cons
           (destructuring-bind (var &optional init-form supplied-p) i
             (declare (ignore init-form supplied-p))
             (need-symbol var "&OPTIONAL parameter name")))
          (t
           (compiler-error "&OPTIONAL parameter is not a symbol or cons: ~S"
                           i))))
      (when restp
        (need-symbol rest "&REST argument"))
      (when keyp
        (dolist (i keys)
          (typecase i
            (symbol)
            (cons
             (destructuring-bind (var-or-kv &optional init-form supplied-p) i
               (declare (ignore init-form supplied-p))
               (if (consp var-or-kv)
                   (destructuring-bind (keyword-name var) var-or-kv
                     (declare (ignore keyword-name))
                     (need-symbol var "&KEY parameter name"))
                   (need-symbol var-or-kv "&KEY parameter name"))))
            (t
             (compiler-error "&KEY parameter is not a symbol or cons: ~S"
                             i))))))

    ;; Voila.
    (values required optional restp rest keyp keys allowp auxp aux
            morep more-context more-count)))

(/show0 "parse-lambda-list.lisp end of file")
