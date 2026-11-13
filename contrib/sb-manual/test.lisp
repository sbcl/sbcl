;;;; Test that everything is documented in public packages

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-manual))

(in-package :sb-manual)

(defun sb-introspect-type-to-locative (type)
  (case type
    ((:macro) (dummy 'macro))
    ((:constant) (dummy 'constant))
    ((:symbol-macro) (dummy 'symbol-macro))
    (t
     (let ((sym (find-symbol (symbol-name type) :cl)))
       (assert sym () "Unexpected sb-introspect definition type ~S." type)
       sym))))

(defun sort-xrefs (xrefs)
  (flet ((xref< (xref-1 xref-2)
           (string< (format nil "~S ~S" (xref-name xref-1)
                            (xref-locative xref-1))
                    (format nil "~S ~S" (xref-name xref-2)
                            (xref-locative xref-2)))))
    (sort (copy-seq xrefs) #'xref<)))

(defun dref-apropos* (package)
  ;; This function reimplements
  #+nil
  (dref:dref-apropos nil :package package :external-only t
                     :dtype '(not (or method dref:unknown compiler-macro
                                   (and setf (not dref:structure-accessor)))))
  (let ((drefs ()))
    (do-external-symbols (sym package)
      ;; We don't document :COMPILER-MACRO, :METHOD, :PACKAGE,
      ;; :ALIEN-TYPE, :ALIEN-CALLBACK, :SETF-EXPANDER, (we usually say
      ;; "SETFable" elsewhere), and the internal definition types.
      (dolist (type '(:class :condition :constant :function :generic-function
                      :macro :method-combination :structure :symbol-macro
                      :type :variable))
        (when (and (sb-introspect:find-definition-sources-by-name sym type)
                   ;; For primitive types, F-D-S-B-N breaks its
                   ;; contract and finds the :TYPE even if there is a
                   ;; :CLASS.
                   (or (not (eq type :type))
                       (null (sb-introspect:find-definition-sources-by-name
                              sym :class))))
          (push (xref sym (sb-introspect-type-to-locative type))
                drefs)))
      ;; For SB-INTROSPECT:FIND-DEFINITION-SOURCES-BY-NAME,
      ;; :DECLARATION means a declaration instance (e.g. (DECLAIM
      ;; SPECIAL *X*)). We are only interested in declaration types
      ;; (e.g. (DECLAIM DECLARATION MY-DECL)).
      (when (sb-int:info :declaration :known sym)
        (push (xref sym 'declaration) drefs)))
    (sort-xrefs drefs)))

(defun undocumentedp (dref)
  (let ((name (xref-name dref))
        (home-section (home-section dref)))
    (cond (home-section
           (values nil (format nil "in ~S" (section-name home-section))))
          ((case (xref-locative-type dref)
             ((function generic-function macro)
              (sb-int:info :function :deprecated name))
             ((constant variable)
              (sb-int:info :variable :deprecated name))
             ((type class structure)
              (sb-int:info :type :deprecated name)))
           (values nil "deprecated"))
          (t
           t))))

(defun list-undocumented-in-package (package)
  (loop for dref in (dref-apropos* package)
        when (undocumentedp dref)
          collect dref))

;;; Public packages and documentation statuses
;;;
;;; Format: (PACKAGE-NAME &KEY NOT-NEEDED MISSING)
;;;
;;; - :NOT-NEEDED: definitions that need not be documented. For
;;;   example, SB-ALIEN exports CL:*, but (* FUNCTION) and (*
;;;   VARIABLE) are not to be documented in the manual.
;;;
;;; - :MISSING: definitions that currently aren't but should be in the
;;;   user manual.
;;;
;;; If you're about to add to these lists, consider whether an
;;; undocumented new export in a public package is the way to go. It
;;; likely isn't.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *public-packages*
    '((sb-debug
       :missing
       ((sb-debug:*debug-condition* variable)
        (sb-debug:*debug-help-string* variable)
        (sb-debug:*debug-readtable* variable)
        (sb-debug:*flush-debug-errors* variable)
        (sb-debug:*in-the-debugger* variable)
        (sb-debug:*stack-top-hint* variable)
        (sb-debug:backtrace function)
        (sb-debug:backtrace-as-list function)
        (sb-debug:frame-has-debug-tag-p function)
        (sb-debug:internal-debug function)
        (sb-debug:unwind-to-frame-and-call function)))
      (sb-ext
       :missing
       (#+sb-simd-pack-256
        (sb-ext:%make-simd-pack-256-double function)
        #+sb-simd-pack-256
        (sb-ext:%make-simd-pack-256-single function)
        #+sb-simd-pack-256
        (sb-ext:%make-simd-pack-256-ub32 function)
        #+sb-simd-pack-256
        (sb-ext:%make-simd-pack-256-ub64 function)
        #+sb-simd-pack-512
        (sb-ext:%make-simd-pack-512-double function)
        #+sb-simd-pack-512
        (sb-ext:%make-simd-pack-512-mask function)
        #+sb-simd-pack-512
        (sb-ext:%make-simd-pack-512-single function)
        #+sb-simd-pack-512
        (sb-ext:%make-simd-pack-512-ub32 function)
        #+sb-simd-pack-512
        (sb-ext:%make-simd-pack-512-ub64 function)
        #+sb-simd-pack
        (sb-ext:%make-simd-pack-double function)
        #+sb-simd-pack
        (sb-ext:%make-simd-pack-single function)
        #+sb-simd-pack
        (sb-ext:%make-simd-pack-ub32 function)
        #+sb-simd-pack
        (sb-ext:%make-simd-pack-ub64 function)
        (sb-ext:*disassemble-annotate* variable)
        (sb-ext:*exit-timeout* variable)
        (sb-ext:*forcibly-terminate-threads-on-exit* variable)
        (sb-ext:*inspected* variable)
        (sb-ext:*print-circle-not-shared* variable)
        (sb-ext:*print-vector-length* variable)
        (sb-ext:*runtime-pathname* variable)
        (sb-ext:*stepper-hook* variable)
        (sb-ext:*suppress-print-errors* variable)
        (sb-ext:call-with-timing function)
        (sb-ext:decimal-with-grouped-digits-width function)
        (sb-ext:define-load-time-global macro)
        (sb-ext:defined-type-name-p function)
        (sb-ext:double-float-negative-infinity constant)
        (sb-ext:double-float-positive-infinity constant)
        (sb-ext:eval-tlf function)
        (sb-ext:float-denormalized-p function)
        (sb-ext:float-infinity-p function)
        (sb-ext:float-nan-p function)
        (sb-ext:float-trapping-nan-p function)
        (sb-ext:fold-identical-code function)
        (sb-ext:heap-allocated-p function)
        (sb-ext:interactive-eval function)
        (sb-ext:long-float-negative-infinity constant)
        (sb-ext:long-float-positive-infinity constant)
        (sb-ext:most-positive-word constant)
        (sb-ext:primitive-object-size function)
        (sb-ext:print-symbol-with-prefix function)
        (sb-ext:print-unreadably function)
        (sb-ext:readtable-base-char-preference function)
        (sb-ext:set-macro-policy function)
        (sb-ext:short-float-negative-infinity constant)
        (sb-ext:short-float-positive-infinity constant)
        #+sb-simd-pack
        (sb-ext:simd-pack class)
        #+sb-simd-pack-256
        (sb-ext:simd-pack-256 class)
        #+sb-simd-pack-256
        (sb-ext:simd-pack-256-p function)
        #+sb-simd-pack-512
        (sb-ext:simd-pack-512 class)
        #+sb-simd-pack-512
        (sb-ext:simd-pack-512-mask class)
        #+sb-simd-pack-512
        (sb-ext:simd-pack-512-mask-p function)
        #+sb-simd-pack-512
        (sb-ext:simd-pack-512-p function)
        #+sb-simd-pack
        (sb-ext:simd-pack-p function)
        (sb-ext:single-float-negative-infinity constant)
        (sb-ext:single-float-positive-infinity constant)
        (sb-ext:spin-loop-hint function)
        (sb-ext:stack-allocated-p function)
        (sb-ext:step-condition condition)
        (sb-ext:step-condition-args function)
        (sb-ext:step-condition-form function)
        (sb-ext:step-condition-result function)
        (sb-ext:step-continue function)
        (sb-ext:step-finished-condition condition)
        (sb-ext:step-form-condition condition)
        (sb-ext:step-into function)
        (sb-ext:step-next function)
        (sb-ext:step-values-condition condition)
        (sb-ext:symbol-global-value function)
        (sb-ext:truly-the macro)
        (sb-ext:typexpand function)
        (sb-ext:typexpand-1 function)
        (sb-ext:typexpand-all function)
        (sb-ext:valid-type-specifier-p function)
        (sb-ext:with-current-source-form macro)
        (sb-ext:word type)
        (sb-ext:signed-word type)))
      (sb-alien
       :not-needed
       (;; These are symbols in the CL package that we use as e.g.
        ;; foreign type specifiers. Obviously, we don't want to
        ;; document their normal meaning.
        (* function)
        (* variable)
        (array class)
        (boolean type)
        (char function)
        (double-float class)
        (float class)
        (float function)
        (integer class)
        (long-float type)
        (single-float class)
        (union function)
        (values function)
        (function class)
        ;; Similar to above
        (sb-ext:word type)
        (sb-ext:signed-word type)
        ;; Exposed because of (SB-ALIEN:CAST MACRO). Should be
        ;; renamed.
        (sb-alien:cast structure)
        ;; Exposed because of SB-ALIEN:SLOT function). Should be
        ;; renamed.
        (sb-alien:slot class)
        ;; Exposed due to the foreign type specifier SB-ALIEN:STRUCT.
        ;; Should be renamed.
        (sb-alien:struct class)))
      (sb-sequence
       :missing
       ((sb-sequence:canonize-key function)
        (sb-sequence:canonize-test function)))
      (sb-sprof)
      (sb-profile)
      #+(and x86-64 sb-thread)
      (sb-aprof
       :missing
       ((sb-aprof:aprof-reset function)
        (sb-aprof:aprof-run function)
        (sb-aprof:aprof-show function)
        (sb-aprof:aprof-start function)
        (sb-aprof:aprof-stop function)
        (sb-aprof:patch-all-code function)))
      (sb-thread
       :missing
       ((sb-thread:*interrupt-handler* variable)
        (sb-thread:foreign-thread structure)
        (sb-thread:holding-mutex-p function)
        (sb-thread:join-thread-problem function)
        ("sb-thread:make-rw-spinlock" function)
        ("sb-thread:make-rwlock" function)
        ("sb-thread:rwlock-rdlock" function)
        ("sb-thread:rwlock-unlock" function)
        ("sb-thread:rwlock-wrlock" function)
        ("sb-thread:rwspinlock-rdlock" function)
        ("sb-thread:rwspinlock-rdunlock" function)
        ("sb-thread:rwspinlock-wrlock" function)
        ("sb-thread:rwspinlock-wrunlock" function)
        (sb-thread:thread-deadlock condition)
        (sb-thread:thread-deadlock-cycle function)
        (sb-thread:thread-ephemeral-p function)
        (sb-thread:thread-os-tid function)))
      (sb-unicode
       :missing
       ((sb-unicode:scalar-p function)
        (sb-unicode:utf8-decode-from-octets function)
        (sb-unicode:utf8-decode-from-sap function)))
      (sb-gray)
      ;; Entirely undocumented. Maybe we need not document all definitions,
      ;; only the idiosyncrasies and departures from AMOP.
      (sb-mop
       :not-needed
       ((add-method generic-function)
        (allocate-instance generic-function)
        (built-in-class class)
        (class class)
        (class-name generic-function)
        (compute-applicable-methods generic-function)
        (ensure-generic-function function)
        (function class)
        (generic-function class)
        (make-instance generic-function)
        (method class)
        (method-combination class)
        (method-qualifiers generic-function)
        (remove-method generic-function)
        (standard-class class)
        (standard-generic-function class)
        (standard-method class)
        (standard-object class)
        (t class)
        (t constant))
       :missing
       ((sb-mop:accessor-method-slot-definition generic-function)
        (sb-mop:add-dependent generic-function)
        (sb-mop:add-direct-method generic-function)
        (sb-mop:add-direct-subclass generic-function)
        (sb-mop:class-default-initargs generic-function)
        (sb-mop:class-direct-default-initargs generic-function)
        (sb-mop:class-direct-slots generic-function)
        (sb-mop:class-direct-subclasses generic-function)
        (sb-mop:class-direct-superclasses generic-function)
        (sb-mop:class-finalized-p generic-function)
        (sb-mop:class-precedence-list generic-function)
        (sb-mop:class-prototype generic-function)
        (sb-mop:class-slots generic-function)
        (sb-mop:compute-applicable-methods-using-classes generic-function)
        (sb-mop:compute-class-precedence-list generic-function)
        (sb-mop:compute-default-initargs generic-function)
        (sb-mop:compute-discriminating-function generic-function)
        (sb-mop:compute-effective-method generic-function)
        (sb-mop:compute-effective-slot-definition generic-function)
        (sb-mop:compute-slots generic-function)
        (sb-mop:direct-slot-definition class)
        (sb-mop:direct-slot-definition-class generic-function)
        (sb-mop:effective-slot-definition class)
        (sb-mop:effective-slot-definition-class generic-function)
        (sb-mop:ensure-class function)
        (sb-mop:ensure-class-using-class generic-function)
        (sb-mop:ensure-generic-function-using-class generic-function)
        (sb-mop:eql-specializer class)
        (sb-mop:eql-specializer-object generic-function)
        (sb-mop:extract-lambda-list function)
        (sb-mop:extract-specializer-names function)
        (sb-mop:finalize-inheritance generic-function)
        (sb-mop:find-method-combination generic-function)
        (sb-mop:forward-referenced-class class)
        (sb-mop:funcallable-standard-class class)
        (sb-mop:funcallable-standard-instance-access function)
        (sb-mop:funcallable-standard-object class)
        (sb-mop:generic-function-argument-precedence-order generic-function)
        (sb-mop:generic-function-declarations generic-function)
        (sb-mop:generic-function-lambda-list generic-function)
        (sb-mop:generic-function-method-class generic-function)
        (sb-mop:generic-function-method-combination generic-function)
        (sb-mop:generic-function-methods generic-function)
        (sb-mop:generic-function-name generic-function)
        (sb-mop:intern-eql-specializer function)
        (sb-mop:make-method-lambda generic-function)
        (sb-mop:map-dependents generic-function)
        (sb-mop:metaobject class)
        (sb-mop:method-function generic-function)
        (sb-mop:method-generic-function generic-function)
        (sb-mop:method-lambda-list generic-function)
        (sb-mop:method-specializers generic-function)
        (sb-mop:reader-method-class generic-function)
        (sb-mop:remove-dependent generic-function)
        (sb-mop:remove-direct-method generic-function)
        (sb-mop:remove-direct-subclass generic-function)
        (sb-mop:set-funcallable-instance-function function)
        (sb-mop:slot-boundp-using-class generic-function)
        (sb-mop:slot-definition class)
        (sb-mop:slot-definition-allocation generic-function)
        (sb-mop:slot-definition-initargs generic-function)
        (sb-mop:slot-definition-initform generic-function)
        (sb-mop:slot-definition-initfunction generic-function)
        (sb-mop:slot-definition-location generic-function)
        (sb-mop:slot-definition-name generic-function)
        (sb-mop:slot-definition-readers generic-function)
        (sb-mop:slot-definition-type generic-function)
        (sb-mop:slot-definition-writers generic-function)
        (sb-mop:slot-makunbound-using-class generic-function)
        (sb-mop:slot-value-using-class generic-function)
        (sb-mop:specializer class)
        (sb-mop:specializer-direct-generic-functions generic-function)
        (sb-mop:specializer-direct-methods generic-function)
        (sb-mop:standard-accessor-method class)
        (sb-mop:standard-direct-slot-definition class)
        (sb-mop:standard-effective-slot-definition class)
        (sb-mop:standard-instance-access function)
        (sb-mop:standard-reader-method class)
        (sb-mop:standard-slot-definition class)
        (sb-mop:standard-writer-method class)
        (sb-mop:update-dependent generic-function)
        (sb-mop:validate-superclass generic-function)
        (sb-mop:writer-method-class generic-function)))
      ;; This is "semi-public" in exports.lisp. What's that?
      (sb-pcl
       :missing
       ((sb-pcl:+ctor-list-max-size+ constant)
        (sb-pcl:+ctor-table-max-size+ constant)
        (sb-pcl:+slot-unbound+ symbol-macro)
        (sb-pcl:class-not-found-error condition)
        (sb-pcl:compute-cache-index function)
        (sb-pcl:ensure-class-finalized function)
        (sb-pcl:make-method-lambda-using-specializers generic-function)
        (sb-pcl:make-method-specializers-form generic-function)
        (sb-pcl:make-specializer-form-using-class generic-function)
        (sb-pcl:no-primary-method generic-function)
        (sb-pcl:parse-specializer-using-class generic-function)
        (sb-pcl:specializer-name-syntax-error condition)
        (sb-pcl:specializer-name-syntax-error function)
        (sb-pcl:specializer-type-specifier generic-function)
        (sb-pcl:system-class class)
        (sb-pcl:unparse-specializer-using-class generic-function)))
      ;; This is currently too messy a mix of public and private to
      ;; comprehend.
      (sb-sys
       :not-needed
       (;; Dummy global definitions for local macros in
        ;; SB-SYS:WITH-INTERRUPTS
        (sb-sys:with-local-interrupts macro)
        (sb-sys:allow-with-interrupts macro)
        ;; We should deprecate this, but it's used internally.
        (sb-sys:without-gcing macro)
        ;; We should get rid of SERVE-EVENT.
        (sb-sys:*periodic-polling-function* variable)
        (sb-sys:*periodic-polling-period* variable)
        (sb-sys:add-fd-handler function)
        (sb-sys:invalidate-descriptor function)
        (sb-sys:remove-fd-handler function)
        (sb-sys:serve-all-events function)
        (sb-sys:serve-event function)
        (sb-sys:wait-until-fd-usable function)
        (sb-sys:with-fd-handler macro))
       :missing
       ((sb-sys:%standard-char-p function)
        (sb-sys:*allow-with-interrupts* variable)
        (sb-sys:*exit-error-handler* variable)
        (sb-sys:*exit-in-progress* variable)
        (sb-sys:*interrupt-pending* variable)
        (sb-sys:*interrupts-enabled* variable)
        (sb-sys:*linkage-info* variable)
        (sb-sys:*long-site-name* variable)
        (sb-sys:*machine-version* variable)
        (sb-sys:*runtime-dlhandle* variable)
        (sb-sys:*shared-objects* variable)
        (sb-sys:*short-site-name* variable)
        (sb-sys:*stderr* variable)
        (sb-sys:*stdin* variable)
        (sb-sys:*stdout* variable)
        #+sb-safepoint
        (sb-sys:*thruption-pending* variable)
        (sb-sys:*tty* variable)
        (sb-sys:allocate-system-memory function)
        (sb-sys:beep function)
        (sb-sys:breakpoint-error condition)
        (sb-sys:close-shared-objects function)
        (sb-sys:deallocate-system-memory function)
        (sb-sys:dlopen-or-lose function)
        (sb-sys:elapsed-cycles function)
        (sb-sys:exit-code type)
        (sb-sys:extern-alien-name function)
        (sb-sys:find-dynamic-foreign-symbol-address function)
        (sb-sys:find-foreign-symbol-address function)
        (sb-sys:foreign-symbol-address function)
        (sb-sys:foreign-symbol-sap function)
        #+(and win32 x86-64)
        (sb-sys:foreign-heap-corruption condition)
        (sb-sys:get-machine-version function)
        (sb-sys:get-system-info function)
        (sb-sys:in-interruption macro)
        (sb-sys:invoke-interruption function)
        (sb-sys:memmove function)
        (sb-sys:memory-fault-error condition)
        #-win32
        (sb-sys:memory-fault-error function)
        (sb-sys:os-cold-init-or-reinit function)
        (sb-sys:os-deinit function)
        (sb-sys:os-exit function)
        (sb-sys:read-cycle-counter function)
        (sb-sys:read-n-bytes function)
        #+win32
        (sb-sys:reinit-internal-real-time function)
        (sb-sys:reopen-shared-objects function)
        (sb-sys:sap-foreign-symbol function)
        (sb-sys:scrub-control-stack function)
        (sb-sys:system-condition condition)
        (sb-sys:system-condition-address function)
        (sb-sys:system-condition-context function)
        (sb-sys:system-internal-run-time function)
        (sb-sys:update-alien-linkage-table function)
        (sb-sys:with-code-pages-pinned macro))))))

(defun ->xrefs (refs)
  (let ((xrefs ()))
    (dolist (ref refs)
      (let ((name (first ref)))
        (if (stringp name)
            (multiple-value-bind (symbol errorp)
                (ignore-errors (values (read-from-string name)))
              (unless errorp
                (push (xref symbol (second ref)) xrefs)))
            (push (xref (first ref) (second ref)) xrefs))))
    (nreverse xrefs)))

(defun check-documented (package-name not-needed missing)
  (let* ((not-needed (->xrefs not-needed))
         (missing (->xrefs missing))
         (undocumented
           (set-difference (list-undocumented-in-package package-name)
                           not-needed :test #'xref=))
         (not-missing
           (set-difference missing undocumented :test #'xref=))
         (undeclared-missing
           (set-difference undocumented missing :test #'xref=))
         (package (find-package package-name))
         (*package* (find-package :cl)))
    (dolist (dref (sort-xrefs not-missing))
      (format t "NOT MISSING: (~S ~S) is ~A.~%"
              (xref-name dref) (xref-locative dref)
              (or (nth-value 1 (undocumentedp dref))
                  "not defined")))
    (dolist (dref (sort-xrefs undeclared-missing))
      (let ((name (xref-name dref)))
        (format t "UNDECLARED MISSING: (~S ~S) is not in the manual.~%"
                name (xref-locative dref))
        (unless (eq (symbol-package name) package)
          (format t "  (exported from ~S)~%"
                  (package-name package)))))
    (assert (zerop (length undeclared-missing)))
    (assert (zerop (length not-missing)))))

#.(cons 'progn
        (loop
          for entry in *public-packages*
          collect (destructuring-bind (package-name &key not-needed missing)
                      entry
                    `(test-util:with-test (:name (:documented-public-package
                                                  ,package-name))
                       (check-documented ',package-name ',not-needed
                                         ',missing)))))
