(load "src/cold/shared.lisp")
(in-package "SB-COLD")
(setf *host-obj-prefix* "obj/from-host/"
      *target-obj-prefix* "obj/from-xc/")
(load "src/cold/set-up-cold-packages.lisp")
(load "src/cold/defun-load-or-cload-xcompiler.lisp")
(load-or-cload-xcompiler #'host-load-stem)
(load "tools-for-build/corefile.lisp" :verbose nil)
(host-cload-stem "src/compiler/generic/genesis" nil)

(genesis :object-file-names (let (list)
                              (do-stems-and-flags (stem flags 2)
                                (unless (member :not-target flags)
                                  (push (stem-object-path stem flags :target-compile)
                                        list)))
                              (nreverse list))
         :defstruct-descriptions (find-bootstrap-file "output/defstructs.lisp-expr" t)
         :tls-init (read-from-file "output/tls-init.lisp-expr" :build-dependent t)
         :c-header-dir-name "output/genesis-2"
         :symbol-table-file-name "src/runtime/sbcl.nm"
         :core-file-name "output/cold-sbcl.core"
         ;; The map file is not needed by the system, but can be
         ;; very handy when debugging cold init problems.
         :map-file-name "output/cold-sbcl.map")

(when sb-c::*track-full-called-fnames*
  (let (possibly-suspicious likely-suspicious)
    (sb-int:dohash ((name cell) sb-c::*emitted-full-calls*)
       (let* ((inlinep (eq (sb-int:info :function :inlinep name) 'inline))
              (source-xform (sb-int:info :function :source-transform name))
              (info (sb-int:info :function :info name)))
         (when (and cell
                    (or inlinep
                        source-xform
                        (and info (sb-c::fun-info-templates info))
                        (sb-int:info :function :compiler-macro-function name)))
             (cond (inlinep
                    ;; A full call to an inline function almost always indicates
                    ;; an out-of-order definition. If not an inline function,
                    ;; the call could be due to an inapplicable transformation.
                    (push (list name cell) likely-suspicious))
                   ;; structure constructors aren't inlined by default,
                   ;; though we have a source-xform.
                   ((and (listp source-xform) (eq :constructor (cdr source-xform))))
                   (t
                    (push (list name cell) possibly-suspicious))))))
    (flet ((show (label list)
             (when list
               (format t "~%~A suspicious calls:~:{~%~4d ~S~@{~%     ~S~}~}~%"
                       label
                       (mapcar (lambda (x) (list* (ash (cadr x) -2) (car x) (cddr x)))
                               (sort list #'> :key #'cadr))))))
      ;; Called inlines not in the presence of a declaration to the contrary
      ;; indicate that perhaps the function definition appeared too late.
      (show "Likely" likely-suspicious)
      ;; Failed transforms are considered not quite as suspicious
      ;; because it could either be too late, or that the transform failed.
      (show "Possibly" possibly-suspicious))
    ;; As each platform's build becomes warning-free,
    ;; it should be added to the list here to prevent regresssions.
    (when (and likely-suspicious
               (target-featurep '(:and (:or :x86 :x86-64) (:or :linux :darwin))))
      (warn "Expected zero inlinining failures"))))

#+cmu (ext:quit)
#+clisp (ext:quit)
#+abcl (ext:quit)
