;;; -*-  Lisp -*-

(defsystem sb-cover
    #+sb-building-contrib :pathname
    #+sb-building-contrib #p"SYS:CONTRIB;SB-COVER;"
    :depends-on (sb-md5)
    :components ((:file "cover")))

(defsystem sb-cover-tests
    :components ((:file "tests")))

(defmethod perform :after ((o load-op) (c (eql (find-system :sb-cover))))
  (provide 'sb-cover))

(defmethod perform ((o test-op) (c (eql (find-system :sb-cover))))
  (operate 'load-op 'sb-cover-tests)
  (operate 'test-op 'sb-cover-tests))
