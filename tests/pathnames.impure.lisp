;;;; miscellaneous tests of pathname-related stuff

;;;; This file is naturally impure because we mess with
;;;; LOGICAL-PATHNAME-TRANSLATIONS.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(load "assertoid.lisp")
(use-package "ASSERTOID")

(load "test-util.lisp")
(use-package "TEST-UTIL")

(setf (logical-pathname-translations "demo0")
      '(("**;*.*.*" "/tmp/")))

;;; In case of a parse error we want to get a condition of type TYPE-ERROR,
;;; because ANSI says so. (This used to be PARSE-ERROR.)
(assert
  (typep (grab-condition (logical-pathname "demo0::bla;file.lisp"))
         'type-error))

;;; some things SBCL-0.6.9 used not to parse correctly:
;;;
;;; SBCL used to throw an error saying there's no translation.
(with-test (:name (:logical-pathname 1))
  (assert (equal (namestring (translate-logical-pathname "demo0:file.lisp"))
                 "/tmp/file.lisp")))

;;; We do not match a null directory to every wild path:
(with-test (:name (:logical-pathname 2))
  (assert (not (pathname-match-p "demo0:file.lisp"
                                 (logical-pathname "demo0:tmp;**;*.*.*")))))

;;; Remove "**" from our resulting pathname when the source-dir is NIL:
(with-test (:name (:logical-pathname 3))
  (setf (logical-pathname-translations "demo1")
        '(("**;*.*.*" "/tmp/**/*.*") (";**;*.*.*" "/tmp/rel/**/*.*")))
  (assert (not (equal (namestring (translate-logical-pathname "demo1:foo.lisp"))
                    "/tmp/**/foo.lisp"))))

;;; That should be correct:
(with-test (:name (:logical-pathname 4))
  (assert (equal (namestring (translate-logical-pathname "demo1:foo.lisp"))
                 "/tmp/foo.lisp")))

;;; Check for absolute/relative path confusion:
(with-test (:name (:logical-pathname 5))
  (assert (not (equal (namestring (translate-logical-pathname "demo1:;foo.lisp"))
                      "tmp/rel/foo.lisp")))
  (assert (equal (namestring (translate-logical-pathname "demo1:;foo.lisp"))
                 "/tmp/rel/foo.lisp")))

;;; Under SBCL: new function #'UNPARSE-ENOUGH-NAMESTRING, to
;;; handle the following case exactly (otherwise we get an error:
;;; "#'IDENTITY CALLED WITH 2 ARGS."
(with-test (:name (:logical-pathname 6))
  (setf (logical-pathname-translations "demo2")
        '(("test;**;*.*" "/tmp/demo2/test")))
  (enough-namestring "demo2:test;foo.lisp"))

;;; When a pathname comes from a logical host, it should be in upper
;;; case. (This doesn't seem to be specifically required in the ANSI
;;; spec, but it's left up to the implementors, and the arguments made
;;; in the cleanup issue PATHNAME-LOGICAL:ADD seem to be a pretty
;;; compelling reason for the implementors to choose case
;;; insensitivity and a canonical case.)
(with-test (:name (:logical-pathname 7))
  (setf (logical-pathname-translations "FOO")
        '(("**;*.*.*" "/full/path/to/foo/**/*.*")))
  (let* ((pn1 (make-pathname :host "FOO" :directory "etc" :name "INETD"
                             :type "conf"))
         (pn2 (make-pathname :host "foo" :directory "ETC" :name "inetd"
                             :type "CONF"))
         (pn3 (read-from-string (prin1-to-string pn1))))
    (assert (equal pn1 pn2))
    (assert (equal pn1 pn3))))

;;; In addition to the upper-case constraint above, if the logical-pathname
;;; contains a string component in e.g. the directory, name and type slot,
;;; these should be valid "WORDS", according to CLHS 19.3.1.
;;; FIXME: currently SBCL throws NAMESTRING-PARSE-ERROR: should this be
;;; a TYPE-ERROR?
(with-test (:name (:logical-pathname 8))
  (locally
    ;; MAKE-PATHNAME is UNSAFELY-FLUSHABLE
    (declare (optimize safety))

    (assert (not (ignore-errors
                  (make-pathname :host "FOO" :directory "!bla" :name "bar"))))

    ;; error: name-component not valid
    (assert (not (ignore-errors
                  (make-pathname :host "FOO" :directory "bla" :name "!bar"))))

    ;; error: type-component not valid.
    (assert (not (ignore-errors
                  (make-pathname :host "FOO" :directory "bla" :name "bar"
                                 :type "&baz"))))))

;;; We may need to parse the host as a LOGICAL-NAMESTRING HOST. The
;;; HOST in PARSE-NAMESTRING can be either a string or :UNSPECIFIC
;;; without actually requiring the system to signal an error (apart
;;; from host mismatches).
(with-test (:name (:logical-pathname 9))
  (assert (equal (namestring (parse-namestring "" "FOO")) "FOO:"))
  (assert (equal (namestring (parse-namestring "" :unspecific)) "")))

;;; The third would work if the call were (and it should continue to
;;; work ...)
(with-test (:name (:logical-pathname 10))
  (parse-namestring ""
                    (pathname-host
                     (translate-logical-pathname
                      "FOO:"))))

;;; ANSI says PARSE-NAMESTRING returns TYPE-ERROR on host mismatch.
(with-test (:name (:logical-pathname 11))
  (let ((cond (grab-condition (parse-namestring "foo:jeamland" "demo2"))))
    (assert (typep cond 'type-error))))

;;; turning one logical pathname into another:
(with-test (:name (:logical-pathname 12))
  (setf (logical-pathname-translations "foo")
        '(("todemo;*.*.*" "demo0:*.*.*")))
  (assert (equal (namestring (translate-logical-pathname "foo:todemo;x.y"))
                 (namestring (translate-logical-pathname "demo0:x.y")))))

;;; ANSI, in its wisdom, specifies that it's an error (specifically a
;;; TYPE-ERROR) to query the system about the translations of a string
;;; which doesn't have any translations. It's not clear why we don't
;;; just return NIL in that case, but they make the rules..
(with-test (:name (:logical-pathname 13))
  (let ((cond (grab-condition (logical-pathname-translations "unregistered-host"))))
    (assert (typep cond 'type-error)))

  (assert (not (string-equal (host-namestring (parse-namestring "OTHER-HOST:ILLEGAL/LPN")) "OTHER-HOST")))
  (assert (string-equal (pathname-name (parse-namestring "OTHER-HOST:ILLEGAL/LPN")) "LPN")))

;;; FIXME: A comment on this section up to sbcl-0.6.11.30 or so said
;;;   examples from CLHS: Section 19.4, LOGICAL-PATHNAME-TRANSLATIONS
;;;   (sometimes converted to the Un*x way of things)
;;; but when I looked it up I didn't see the connection. Presumably
;;; there's some code in this section which should be attributed
;;; to something in the ANSI spec, but I don't know what code it is
;;; or what section of the specification has the related code.
(with-test (:name (:logical-pathname 14))
  (setf (logical-pathname-translations "test0")
        '(("**;*.*.*"              "/library/foo/**/")))
  (assert (equal (namestring (translate-logical-pathname
                              "test0:foo;bar;baz;mum.quux"))
                 "/library/foo/foo/bar/baz/mum.quux"))
  (setf (logical-pathname-translations "prog")
        '(("RELEASED;*.*.*"        "MY-UNIX:/sys/bin/my-prog/")
          ("RELEASED;*;*.*.*"      "MY-UNIX:/sys/bin/my-prog/*/")
          ("EXPERIMENTAL;*.*.*"    "MY-UNIX:/usr/Joe/development/prog/")
          ("EXPERIMENTAL;*;*.*.*"  "MY-UNIX:/usr/Joe/development/prog/*/")))
  (setf (logical-pathname-translations "prog")
        '(("CODE;*.*.*"             "/lib/prog/")))
  (assert (equal (namestring (translate-logical-pathname
                              "prog:code;documentation.lisp"))
                 "/lib/prog/documentation.lisp"))
  (setf (logical-pathname-translations "prog")
        '(("CODE;DOCUMENTATION.*.*" "/lib/prog/docum.*")
          ("CODE;*.*.*"             "/lib/prog/")))
  (assert (equal (namestring (translate-logical-pathname
                              "prog:code;documentation.lisp"))
                 "/lib/prog/docum.lisp")))

;;; ANSI section 19.3.1.1.5 specifies that translation to a filesystem
;;; which doesn't have versions should ignore the version slot. CMU CL
;;; didn't ignore this as it should, but we do.
(with-test (:name (:logical-pathname 15))
  (assert (equal (namestring (translate-logical-pathname
                              "test0:foo;bar;baz;mum.quux.3"))
                 "/library/foo/foo/bar/baz/mum.quux")))


;;;; MERGE-PATHNAME tests
;;;;
;;;; There are some things we don't bother testing, just because they're
;;;; not meaningful on the underlying filesystem anyway.
;;;;
;;;; Mostly that means that we don't do devices, we don't do versions
;;;; except minimally in LPNs (they get lost in the translation to
;;;; physical hosts, so it's not much of an issue), and we don't do
;;;; hosts except for LPN hosts
;;;;
;;;; Although these tests could conceivably be useful in principle for
;;;; other implementations, they depend quite heavily on the rules for
;;;; namestring parsing, which are implementation-specific. So, success
;;;; or failure in these tests doesn't tell you anything about
;;;; ANSI-compliance unless your PARSE-NAMESTRING works like ours.

;;; Needs to be done at compile time, so that the #p"" read-macro
;;; correctly parses things as logical pathnames. This is not a
;;; problem as was, as this is an impure file and so gets loaded in,
;;; but just for future proofing...
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (logical-pathname-translations "scratch")
        '(("**;*.*.*" "/usr/local/doc/**/*"))))

(with-test (:name (:merge-pathname 1))
  (loop for (expected-result . params) in
        `( ;; trivial merge
          (#P"/usr/local/doc/foo" #p"foo" #p"/usr/local/doc/")
          ;; If pathname does not specify a host, device, directory,
          ;; name, or type, each such component is copied from
          ;; default-pathname.
          ;; 1) no name, no type
          (#p"/supplied-dir/name.type" #p"/supplied-dir/" #p"/dir/name.type")
          ;; 2) no directory, no type
          (#p"/dir/supplied-name.type" #p"supplied-name" #p"/dir/name.type")
          ;; 3) no name, no dir (must use make-pathname as ".foo" is parsed
          ;; as a name)
          (#p"/dir/name.supplied-type"
             ,(make-pathname :type "supplied-type")
             #p"/dir/name.type")
          ;; If (pathname-directory pathname) is a list whose car is
          ;; :relative, and (pathname-directory default-pathname) is a
          ;; list, then the merged directory is [...]
          (#p"/aaa/bbb/ccc/ddd/qqq/www" #p"qqq/www" #p"/aaa/bbb/ccc/ddd/eee")
          ;; except that if the resulting list contains a string or
          ;; :wild immediately followed by :back, both of them are
          ;; removed.
          (#P"/aaa/bbb/ccc/blah/eee"
             ;; "../" in a namestring is parsed as :up not :back, so make-pathname
             ,(make-pathname :directory '(:relative :back "blah"))
             #p"/aaa/bbb/ccc/ddd/eee")
          ;; If (pathname-directory default-pathname) is not a list or
          ;; (pathname-directory pathname) is not a list whose car is
          ;; :relative, the merged directory is (or (pathname-directory
          ;; pathname) (pathname-directory default-pathname))
          (#P"/absolute/path/name.type"
             #p"/absolute/path/name"
             #p"/dir/default-name.type")
          ;; === logical pathnames ===
          ;; recognizes a logical pathname namestring when
          ;; default-pathname is a logical pathname
          ;; FIXME: 0.6.12.23 fails this one.
          ;;
          ;; And, as it happens, it's right to fail it. Because
          ;; #p"name1" is read in with the ambient *d-p-d* value, which
          ;; has a physical (Unix) host; therefore, the host of the
          ;; default-pathname argument to merge-pathnames is
          ;; irrelevant. The result is (correctly) different if
          ;; '#p"name1"' is replaced by "name1", below, though it's
          ;; still not what one might expect... -- CSR, 2002-05-09
          #+nil (#P"scratch:foo;name1" #p"name1" #p"scratch:foo;")
          ;; or when the namestring begins with the name of a defined
          ;; logical host followed by a colon [I assume that refers to pathname
          ;; rather than default-pathname]
          (#p"SCRATCH:FOO;NAME2" #p"scratch:;name2" #p"scratch:foo;")
          ;; conduct the previous set of tests again, with a lpn first argument
          (#P"SCRATCH:USR;LOCAL;DOC;FOO" #p"scratch:;foo" #p"/usr/local/doc/")
          (#p"SCRATCH:SUPPLIED-DIR;NAME.TYPE"
             #p"scratch:supplied-dir;"
             #p"/dir/name.type")
          (#p"SCRATCH:DIR;SUPPLIED-NAME.TYPE"
             #p"scratch:;supplied-name"
             #p"/dir/name.type")
          (#p"SCRATCH:DIR;NAME.SUPPLIED-TYPE"
             ,(make-pathname :host "scratch" :type "supplied-type")
             #p"/dir/name.type")
          (#p"SCRATCH:AAA;BBB;CCC;DDD;FOO;BAR"
             ,(make-pathname :host "scratch"
                             :directory '(:relative "foo")
                             :name "bar")
             #p"/aaa/bbb/ccc/ddd/eee")
          (#p"SCRATCH:AAA;BBB;CCC;FOO;BAR"
             ,(make-pathname :host "scratch"
                             :directory '(:relative :back "foo")
                             :name "bar")
             #p"/aaa/bbb/ccc/ddd/eee")
          (#p"SCRATCH:ABSOLUTE;PATH;NAME.TYPE"
             #p"scratch:absolute;path;name" #p"/dir/default-name.type")

          ;; FIXME: test version handling in LPNs
          )
        do (let ((result (apply #'merge-pathnames params)))
             (macrolet ((frob (op)
                              `(assert (equal (,op result) (,op expected-result)))))
               (frob pathname-host)
               (frob pathname-directory)
               (frob pathname-name)
               (frob pathname-type)))))

;;; host-namestring testing
(with-test (:name :host-namestring)
  (assert (string=
           (namestring (parse-namestring "/foo" (host-namestring #p"/bar")))
           "/foo"))
  (assert (string=
           (namestring (parse-namestring "FOO" (host-namestring #p"SCRATCH:BAR")))
           "SCRATCH:FOO"))
  (assert (raises-error?
           (setf (logical-pathname-translations "")
                 (list '("**;*.*.*" "/**/*.*"))))))

;;; Bug 200: translate-logical-pathname is according to the spec supposed
;;; not to give errors if asked to translate a namestring for a valid
;;; physical pathname.  Failed in 0.7.7.28 and before
(with-test (:name (:logical-pathname 16))
  (assert (string= (namestring (translate-logical-pathname "/")) "/")))


;;; Not strictly pathname logic testing, but until sbcl-0.7.6.19 we
;;; had difficulty with non-FILE-STREAM stream arguments to pathname
;;; functions (they would cause memory protection errors).  Make sure
;;; that those errors are gone:
(with-test (:name (:string-streams-as-pathnames 1))
  (assert (raises-error? (pathname (make-string-input-stream "FOO"))
                         type-error))
  (assert (raises-error? (merge-pathnames (make-string-output-stream))
                         type-error)))

;;; ensure read/print consistency (or print-not-readable-error) on
;;; pathnames:
(with-test (:name :print/read-consistency)
  (let ((pathnames (list
                    (make-pathname :name "foo" :type "txt" :version :newest)
                    (make-pathname :name "foo" :type "txt" :version 1)
                    (make-pathname :name "foo" :type ".txt")
                    (make-pathname :name "foo." :type "txt")
                    (parse-namestring "SCRATCH:FOO.TXT.1")
                    (parse-namestring "SCRATCH:FOO.TXT.NEWEST")
                    (parse-namestring "SCRATCH:FOO.TXT"))))
    (dolist (p pathnames)
      (print p)
      (handler-case
         (let* ((*print-readably* t)
                (new (read-from-string (format nil "~S" p))))
           (unless (equal new p)
             (let ((*print-readably* nil))
               (error "oops: host:~S device:~S dir:~S version:~S~% ->~%~
                             host:~S device:~S dir:~S version:~S"
                      (pathname-host p) (pathname-device p)
                      (pathname-directory p) (pathname-version p)
                      (pathname-host new) (pathname-device new)
                      (pathname-directory new) (pathname-version new)))))
       (print-not-readable ()
          nil)))))

;;; BUG 330: "PARSE-NAMESTRING should accept namestrings as the
;;; default argument" ...and streams as well
(with-test (:name :parse-namestring/stream)
  (assert (equal (parse-namestring "foo" nil "/")
                 (parse-namestring "foo" nil #P"/")))
  (let ((test "parse-namestring-test.tmp"))
    (unwind-protect
        (with-open-file (f test :direction :output)
          ;; FIXME: This test is a bit flaky, since we only check that
          ;; no error is signalled. The dilemma here is "what is the
          ;; correct result when defaults is a _file_, not a
          ;; directory". Currently (0.8.10.73) we get #P"foo" here (as
          ;; opposed to eg. #P"/path/to/current/foo"), which is
          ;; possibly mildly surprising but probably conformant.
          (assert (parse-namestring "foo" nil f)))
      (when (probe-file test)
        (delete-file test)))))

;;; ENOUGH-NAMESTRING should probably not fail when the namestring in
;;; question has a :RELATIVE pathname.
(with-test (:name :enough-namestring)
  (assert (equal (enough-namestring #p"foo" #p"./") "foo")))

;;; bug reported by Artem V. Andreev: :WILD not handled in unparsing
;;; directory lists.
(with-test (:name :unparse-wild)
  (assert (equal (namestring #p"/tmp/*/") "/tmp/*/")))

;;; Printing of pathnames; see CLHS 22.1.3.1. This section was started
;;; to confirm that pathnames are printed as their namestrings under
;;; :escape nil :readably nil.
(with-test (:name :print-as-namestrings)
  (loop for (pathname expected . vars) in
        `((#p"/foo" "#P\"/foo\"")
          (#p"/foo" "#P\"/foo\"" :readably nil)
          (#p"/foo" "#P\"/foo\"" :escape nil)
          (#p"/foo" "/foo"       :readably nil :escape nil))
        for actual = (with-standard-io-syntax
                      (apply #'write-to-string pathname vars))
        do (assert (string= expected actual)
                   ()
                   "~S should be ~S, was ~S"
                   (list* 'write-to-string pathname vars)
                   expected
                   actual)))

;;; we got (truename "/") wrong for about 6 months.  Check that it's
;;; still right.
(with-test (:name :root-truename)
  (let ((pathname (truename "/")))
    (assert (equalp pathname #p"/"))
    (assert (equal (pathname-directory pathname) '(:absolute)))))

;;; we failed to unparse logical pathnames with :NAME :WILD :TYPE NIL.
;;; (Reported by Pascal Bourguignon.
(with-test (:name :unparse-logical-wild)
  (let ((pathname (make-pathname :host "SYS" :directory '(:absolute :wild-inferiors)
                                 :name :wild :type nil)))
    (assert (string= (namestring pathname) "SYS:**;*"))
    (assert (string= (write-to-string pathname :readably t) "#P\"SYS:**;*\""))))

;;; reported by James Y Knight on sbcl-devel 2006-05-17
(with-test (:name :merge-back)
  (let ((p1 (make-pathname :directory '(:relative "bar")))
        (p2 (make-pathname :directory '(:relative :back "foo"))))
    (assert (equal (merge-pathnames p1 p2)
                   (make-pathname :directory '(:relative :back "foo" "bar"))))))

;;; construct native namestrings even if the directory is empty (means
;;; that same as if (:relative))
(with-test (:name :native-namestring)
  (assert (equal (sb-ext:native-namestring (make-pathname :directory '(:relative)
                                                          :name "foo"
                                                          :type "txt"))
                 (sb-ext:native-namestring (let ((p (make-pathname :directory nil
                                                                   :name "foo"
                                                                   :type "txt")))
                                             (assert (not (pathname-directory p)))
                                             p)))))

;;; reported by Richard Kreuter: PATHNAME and MERGE-PATHNAMES used to
;;; be unsafely-flushable. Since they are known to return non-nil values
;;; only, the test-node of the IF is flushed, and since the function
;;; is unsafely-flushable, out it goes, and bad pathname designators
;;; breeze through.
;;;
;;; These tests rely on using a stream that appears as a file-stream
;;; but isn't a valid pathname-designator.
(with-test (:name :dont-flush-pathnames)
  (assert (eq :false
              (if (ignore-errors (pathname sb-sys::*tty*)) :true :false)))
  (assert (eq :false
              (if (ignore-errors (merge-pathnames sb-sys::*tty*)) :true :false))))

;;; This used to return "quux/bar.lisp"
(with-test (:name :dpd-output-file)
  (assert (equal #p"quux/bar.fasl"
                 (let ((*default-pathname-defaults* #p"quux/"))
                   (compile-file-pathname "foo.lisp" :output-file "bar"))))
  (assert (equal #p"quux/bar.fasl"
                 (let ((*default-pathname-defaults* #p"quux/"))
                   (compile-file-pathname "bar.lisp")))))

(with-test (:name :wild-enough)
  (enough-namestring #p".a*"))


(with-test (:name :translated-wild-version)
  (assert (eq 99
            (pathname-version
             (translate-pathname
              (make-pathname :name "foo" :type "bar" :version 99)
              (make-pathname :name :wild :type :wild :version :wild)
              (make-pathname :name :wild :type :wild :version :wild)))))

  (assert (eq 99
              (pathname-version
               (translate-pathname
                (make-pathname :name "foo" :type "bar" :version 99)
                (make-pathname :name :wild :type :wild :version :wild)
                (make-pathname :name :wild :type :wild :version nil))))))

;;; enough-namestring relative to root
(with-test (:name :enough-relative-to-root)
  (assert (equal "foo" (enough-namestring "/foo" "/"))))

;;; Check the handling of NIL, :UNSPECIFIC, the empty string, and
;;; non-NIL strings in NATIVE-NAMESTRING implementations.  Revised by
;;; RMK 2007-11-28, attempting to preserve the apparent intended
;;; denotation of SBCL's then-current pathname implementation.
(with-test (:name (:native-namestring 2))
  (assert (equal
           (loop with components = (list nil :unspecific "" "a")
                 for name in components
                 appending (loop for type in components
                                 as pathname = (make-pathname
                                                #+win32 :device #+win32 "C"
                                                :directory '(:absolute "tmp")
                                                :name name :type type)
                                 collect (ignore-errors
                                          (sb-ext:native-namestring pathname))))
           #-win32
              #|type  NIL       :UNSPECIFIC   ""        "a"         |#
  #|name       |#
  #|NIL        |#   '("/tmp/"   "/tmp/"       NIL       NIL
  #|:UNSPECIFIC|#     "/tmp/"   "/tmp/"       NIL       NIL
  #|""         |#     "/tmp/"   "/tmp/"       "/tmp/."  "/tmp/.a"
  #|"a"        |#     "/tmp/a"  "/tmp/a"      "/tmp/a." "/tmp/a.a")

           #+win32
              #|type  NIL           :UNSPECIFIC   ""            "a"     |#
  #|name       |#
  #|NIL        |#   '("C:\\tmp\\"   "C:\\tmp\\"   NIL           NIL
  #|:UNSPECIFIC|#     "C:\\tmp\\"   "C:\\tmp\\"   NIL           NIL
  #|""         |#     "C:\\tmp\\"   "C:\\tmp\\"   "C:\\tmp\\."  "C:\\tmp\\.a"
  #|"a"        |#     "C:\\tmp\\a"  "C:\\tmp\\a"  "C:\\tmp\\a." "C:\\tmp\\a.a"))))

(with-test (:name :delete-file-logical-pathname)
  (setf (logical-pathname-translations "SB-TEST")
        (list (list "**;*.*.*" (make-pathname :name :wild
                                              :type :wild
                                              :defaults (truename ".")))))
  (let ((test (pathname "SB-TEST:delete-logical-pathname.tmp")))
    (assert (typep test 'logical-pathname))
    (with-open-file (f test :direction :output)
      (write-line "delete me!" f))
    (assert (probe-file test))
    (assert (delete-file test))
    (assert (not (probe-file test)))))

(with-test (:name :logical-pathname-type-error)
  (assert (eq :type-error-ok
              (handler-case (logical-pathname "FOO.txt")
                (type-error () :type-error-ok))))
  (assert (eq :type-error-ok
              (handler-case (logical-pathname "SYS:%")
                (type-error () :type-error-ok)))))

;;; Reported by Willem Broekema: Reading #p"\\\\" caused an error due
;;; to insufficient sanity in input testing in EXTRACT-DEVICE (in
;;; src;code;win32-pathname).
#+win32
(with-test (:name :bug-489698)
  (assert (equal (make-pathname :directory '(:absolute))
                 (read-from-string "#p\"\\\\\\\\\""))))

(with-test (:name :load-logical-pathname-translations)
  (let* ((cwd (truename "."))
         (foo (merge-pathnames "llpnt-foo.translations" cwd))
         (bar (merge-pathnames "llpnt-bar.translations" cwd))
         (translations (logical-pathname-translations "SYS")))
    (unwind-protect
         (progn
           (with-open-file (f foo :direction :output)
             (prin1 (list (list "*.TEXT" (make-pathname
                                          :directory '(:absolute "my" "foo")
                                          :name :wild :type "txt")))
                    f))
           (with-open-file (f bar :direction :output)
             (prin1 (list (list "*.CL" (make-pathname
                                        :directory '(:absolute "my" "bar")
                                        :name :wild :type "lisp"))) f))
           (setf (logical-pathname-translations "SYS")
                 (list* (list "SITE;LLPNT-FOO.TRANSLATIONS.NEWEST" foo)
                        (list "SITE;LLPNT-BAR.TRANSLATIONS.NEWEST" bar)
                        translations))
           (assert (load-logical-pathname-translations "LLPNT-FOO"))
           (assert (load-logical-pathname-translations "LLPNT-BAR"))
           (assert
            (and
             (equal "/my/bar/quux.lisp"
                    (namestring (translate-logical-pathname "LLPNT-BAR:QUUX.CL")))
             (equal "/my/foo/quux.txt"
                    (namestring (translate-logical-pathname "LLPNT-FOO:QUUX.TEXT"))))))
      (ignore-errors (delete-file foo))
      (ignore-errors (delete-file bar))
      (setf (logical-pathname-translations "SYS") translations))))

;;;; success
