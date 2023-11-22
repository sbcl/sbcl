
;;; This script can be used to help diagnose failure in rebuild-pathname-cache
;;; by dumping the cache as-is from any core for offline examination.
;;; Usage:
;;; * (load "tools-for-build/readcore")
;;; * (sb-editcore:dump-pathname-hashsets "/path/to/your.core"))
;;;

(load (merge-pathnames "editcore.lisp" *load-pathname*))

(in-package sb-editcore)

;; layout IDs will be the same as the host's, assuming you're using the same SBCL git revision
(import 'sb-kernel::layout-id)
(defconstant pathname-layout-id (layout-id (find-layout 'pathname)))
(defconstant lpn-layout-id (layout-id (find-layout 'logical-pathname)))
(defconstant pattern-layout-id (layout-id (find-layout 'sb-impl::pattern)))
(defconstant physhost-layout-id (layout-id
                                 (find-layout #+unix 'sb-impl::unix-host
                                              #+win32 'sb-impl::win32-host)))
(defconstant loghost-layout-id (layout-id (find-layout 'sb-impl::logical-host)))

(defconstant keyword-package-id (sb-impl::package-id (find-package 'keyword)))

(defun convert-to-host-object (x spacemap)
  (let ((nil-object (compute-nil-object spacemap)))
    (labels ((recurse (x)
               (cond ((not (is-lisp-pointer (get-lisp-obj-address x)))
                      x)
                     ((eq x nil-object) nil)
                     (t
                      (let ((x (translate x spacemap)))
                        (ecase (lowtag-of x)
                          (#.list-pointer-lowtag
                           (cons (recurse (car x)) (recurse (cdr x))))
                          (#.instance-pointer-lowtag
                           ;; pathnames and patterns have only tagged slots
                           (let ((length (%instance-length x))
                                 (layout-id
                                  (sb-kernel::layout-id
                                   (translate (%instance-layout x) spacemap))))
                             (case layout-id
                               ;; don't display all slots in an instance of either subtype of HOST
                               (#.physhost-layout-id "#<physical-host>")
                               (#.loghost-layout-id
                                (list ':logical-host
                                      (the simple-string
                                           (translate (sb-impl::logical-host-name x) spacemap))))
                               (t
                                (list (ecase layout-id
                                        (#.pathname-layout-id :pathname)
                                        (#.lpn-layout-id :logical-pathname)
                                        (#.pattern-layout-id :pattern))
                                      (loop for i from instance-data-start below length
                                            collect (recurse (%instance-ref x i))))))))
                          (#.other-pointer-lowtag
                           (cond ((simple-string-p x) x)
                                 ((symbolp x)
                                  (aver (= (symbol-package-id x) keyword-package-id))
                                  (intern(translate (symbol-name x) spacemap)
                                         'keyword))
                                 (t (bug "What? ~x ~x"
                                         (get-lisp-obj-address x)
                                         (%other-pointer-widetag x)))))))))))
      (recurse x))))

(defun show-robinhood-hashset (symbol spacemap)
  (format t "~A~%" (translate (symbol-name symbol) spacemap))
  (let* ((hashset (translate (symbol-global-value symbol) spacemap))
         (storage (translate (sb-impl::hashset-storage hashset) spacemap))
         (cells (translate (sb-impl::hss-cells storage) spacemap))
         (n (length (the simple-vector cells)))
         (*print-pretty* nil)
         (*print-length* nil)
         (*print-level* nil))
    (dotimes (i n)
      (let ((elt (svref cells i)))
        ;; skip unused cells, but do show NIL and unbound-maker
        (unless (eql elt 0) ; unused
          (format t " [~4d]=~S~%" i (convert-to-host-object elt spacemap)))))))

(defun dump-pathname-hashsets (corefile-name)
  (with-open-file (input corefile-name :element-type '(unsigned-byte 8))
    (binding* ((core-header (make-array +backend-page-bytes+ :element-type '(unsigned-byte 8)))
               (core-offset (read-core-header input core-header t))
               ((npages space-list card-mask-nbits core-dir-start initfun)
                (parse-core-header input core-header)))
        (declare (ignore card-mask-nbits core-dir-start initfun))
        (with-mapped-core (sap core-offset npages input)
          (let* ((spacemap (cons sap (sort (copy-list space-list) #'> :key #'space-addr)))
                 (sb-impl-pkgid (symbol-package-id 'sb-impl::*pn-dir-table*))
                 (pn-table (find-target-symbol sb-impl-pkgid "*PN-DIR-TABLE*" spacemap))
                 (dir-table (find-target-symbol sb-impl-pkgid "*PN-TABLE*" spacemap)))
            (show-robinhood-hashset dir-table spacemap)
            (show-robinhood-hashset pn-table spacemap))))))
