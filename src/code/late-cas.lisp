(in-package "SB!IMPL")

(defcas car (cons) %compare-and-swap-car)
(defcas cdr (cons) %compare-and-swap-cdr)
(defcas first (cons) %compare-and-swap-car)
(defcas rest (cons) %compare-and-swap-cdr)
(defcas symbol-plist (symbol) %compare-and-swap-symbol-plist)

(define-cas-expander symbol-value (name &environment env)
  (multiple-value-bind (tmp val cname)
      (if (sb!xc:constantp name env)
          (values nil nil (constant-form-value name env))
          (values (gensymify name) name nil))
    (with-unique-names (old new)
      (values (when tmp (list tmp))
              (when val (list val))
              old
              new
              (let ((slow
                      `(locally
                           (declare (symbol ,tmp))
                         (about-to-modify-symbol-value ,tmp 'compare-and-swap ,new)
                         (%compare-and-swap-symbol-value ,tmp ,old ,new))))
                (if cname
                    (if (member (info :variable :kind cname) '(:special :global))
                        ;; We can generate the type-check reasonably.
                        `(%compare-and-swap-symbol-value
                          ',cname ,old (the ,(info :variable :type cname) ,new))
                        slow)
                    slow))
              `(symbol-global-value ,(or tmp `',cname))))))

(define-cas-expander svref (vector index)
  (with-unique-names (v i old new)
    (values (list v i)
            (list vector index)
            old
            new
            `(locally (declare (simple-vector ,v))
               (%compare-and-swap-svref ,v (%check-bound ,v (length ,v) ,i) ,old ,new))
            `(svref ,v ,i))))
