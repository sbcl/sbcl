(defpackage :sb-introspect-test/xref
  (:use "SB-INTROSPECT" "CL"))

(in-package :sb-introspect-test/xref)

(load (compile-file (merge-pathnames "xref-test-data.lisp" *load-pathname*)))

(labels ((natural< (a b)
           (string< (princ-to-string a) (princ-to-string b))))
  (let ((tests '(((sb-introspect::who-calls 'foo) ())
                 ((sb-introspect::who-calls 'bar) (xref/1 xref/3))
                 ((sb-introspect::who-calls 'xref/1) (xref/2))
                 ((sb-introspect::who-calls 'xref/2)
                  (xref/5 xref/6 xref/8 xref/8 xref/12
                   (sb-pcl::fast-method xref/10
                                        (t t t t t t t t fixnum))
                   (sb-pcl::fast-method xref/11 (fixnum))))
                 ((sb-introspect::who-calls 'xref/3)
                  (inline/1 (sb-pcl::fast-method xref/11 (float))))
                 ((sb-introspect::who-calls 'xref/4) ())
                 ((sb-introspect::who-calls 'xref/5) ())
                 ((sb-introspect::who-calls 'xref/6) (xref/7))
                 ((sb-introspect::who-calls 'xref/7) ())
                 ((sb-introspect::who-calls 'xref/8) ())
                 ((sb-introspect::who-calls 'xref/10) ())
                 ((sb-introspect::who-calls 'xref/11) ())
                 ((sb-introspect::who-calls 'inline/1) (xref/12))
                 ((sb-introspect::who-calls 'xref/12) (macro/1))
                 ((sb-introspect::who-macroexpands 'macro/1)
                  (macro-use/1 macro-use/2 macro-use/3 macro-use/4 inline/2))
                 ((sb-introspect::who-binds '*a*) (xref/2))
                 ((sb-introspect::who-sets '*a*) (xref/2))
                 ((sb-introspect::who-references '*a*)
                  (xref/1 xref/2 xref/4 inline/1))
                 ((sb-introspect::who-references '+z+)
                  (inline/1)))))
    (loop for x in tests
          for form = (first x)
          for wanted = (sort (second x) #'natural<)
          for result = (sort (loop for name in (eval form)
                                   collect (car name))
                             #'natural<)
          do (assert (equalp wanted result)
                     nil
                     "form=~a~%wanted=~a~%result=~a~%" form wanted result))))

