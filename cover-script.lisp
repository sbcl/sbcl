(require :sb-cover)
(load "src/cold/chill.lisp")
(require :sb-grovel) ; hack
(let* ((src (translate-logical-pathname "SYS:SRC;"))
       (obj (merge-pathnames (make-pathname :directory '(:relative :back "obj" :wild-inferiors)
                                            :name :wild :type :wild)
                             src)))
  (push (list "SYS:OBJ;**;*.*.*" obj) ; hack
        (logical-pathname-translations "SYS")))
(push :sb-bsd-sockets-addrinfo *features*) ; hack
(sb-cover:reset-coverage)
(map nil 'sb-cover:merge-coverage-from-file (directory "tests/*.coverage"))
(sb-cover:save-coverage-in-file "all-tests.coverage")
(sb-cover:report "/tmp/s/")
