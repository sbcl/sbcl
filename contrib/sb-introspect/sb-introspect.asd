;;; -*-  Lisp -*-

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(defpackage :sb-introspect-system
  (:use :asdf :cl))

(in-package :sb-introspect-system)

(defsystem :sb-introspect
  :components ((:file "introspect")))

(defmethod perform :after ((o load-op) (c (eql (find-system :sb-introspect))))
  (provide 'sb-introspect))

(defmethod perform ((o test-op) (c (eql (find-system :sb-introspect))))
  (operate 'load-op :sb-introspect-tests)
  (operate 'test-op :sb-introspect-tests))

(defclass plist-file (cl-source-file)
  ((source-plist
    :initform nil
    :initarg :source-plist
    :reader plist-file-source-plist)))

(defmethod perform ((op compile-op) (com plist-file))
  (with-compilation-unit (:source-plist (plist-file-source-plist com))
    (call-next-method)))

(defmethod perform ((op load-op) (com plist-file))
  (with-compilation-unit (:source-plist (plist-file-source-plist com))
    (call-next-method)))

(defclass source-only-file (cl-source-file)
  ())

(defmethod perform ((op compile-op) (com source-only-file)))

(defmethod output-files ((op compile-op) (com source-only-file))
  (list (component-pathname com)))

(defsystem :sb-introspect-tests
  :depends-on (:sb-introspect :sb-rt)
  :components ((:file "xref-test-data")
               (:file "xref-test" :depends-on ("xref-test-data"))
               (:plist-file "test" :source-plist (:test-outer "OUT"))
               (:source-only-file "load-test")
               (:file "test-driver" :depends-on ("test" "load-test"))))

(defmethod perform ((op test-op) (com (eql (find-system :sb-introspect-tests))))
  ;; N.b. At least DEFINITION-SOURCE-PLIST.1 assumes that CWD is the
  ;; contrib/sb-introspect directory which is true for when this is
  ;; implicitly run via make-target-contribs.sh -- but not when this
  ;; is executed manually.
  (let ((*default-pathname-defaults*
         (make-pathname :directory (pathname-directory
                                    '#.(or *compile-file-pathname*
                                           *load-pathname*)))))
    (multiple-value-bind (soft strict pending)
        (funcall (intern "DO-TESTS" (find-package "SB-RT")))
      (fresh-line)
      (unless strict
        #+sb-testing-contrib
        ;; We create TEST-PASSED from a shell script if tests passed.  But
        ;; since the shell script only `touch'es it, we can actually create
        ;; it ahead of time -- as long as we're certain that tests truly
        ;; passed, hence the check for SOFT.
        (when soft
          (with-open-file (s #p"SYS:CONTRIB;SB-INTROSPECT;TEST-PASSED"
                             :direction :output)
            (dolist (pend pending)
              (format s "Expected failure: ~A~%" pend))))
        (warn "ignoring expected failures in test-op"))
      (unless soft
        (error "test-op failed with unexpected failures")))))
