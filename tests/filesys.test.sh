#!/bin/sh

# This software is part of the SBCL system. See the README file for
# more information.
#
# While most of SBCL is derived from the CMU CL system, the test
# files (like this one) were written from scratch after the fork
# from CMU CL.
#
# This software is in the public domain and is provided with
# absolutely no warranty. See the COPYING and CREDITS files for
# more information.

. ./subr.sh

run_sbcl --eval '(exit :code (or #+unix 0 2))'
if [ $? -eq 2 ] ; then echo $0: SKIPPING ; exit $EXIT_TEST_WIN ; fi

use_test_subdirectory
testdir="`pwd -P`" # resolve symbolic links in the directory.

set -f # disable filename expansion in the shell.

# Test DIRECTORY and TRUENAME.
echo this is a test > test-1.tmp
echo this is a test > test-2.tmp
echo this is a test > wild?test.tmp

ln -s "$testdir" dirlinktest
ln -s test-1.tmp link-1
ln -s "$testdir/test-2.tmp" link-2
ln -s i-do-not-exist link-3
ln -s link-4 link-4
ln -s link-5 link-6
ln -s "$testdir/link-6" link-5
expected_truenames=`cat<<EOF
(list #p"$testdir/"
      #p"$testdir/link-3"
      #p"$testdir/link-4"
      #p"$testdir/link-5"
      #p"$testdir/link-6"
      #p"$testdir/test-1.tmp"
      #p"$testdir/test-2.tmp"
      #p"$testdir/wild\\\\\?test.tmp")
EOF
`
# FIXME: the following tests probably can't succeed at all if the
# testdir name contains wildcard characters or quotes.
run_sbcl <<EOF
  (in-package :cl-user)
  (let* ((directory (directory "./*.*"))
         (truenames (sort directory #'string< :key #'pathname-name)))
    ;(format t "~&TRUENAMES=~S~%" truenames)
    (finish-output)
    (assert (equal truenames $expected_truenames)))
  (assert (equal (truename "dirlinktest") #p"$testdir/"))
  (assert (equal (truename "dirlinktest/") #p"$testdir/"))
  (assert (equal (truename "test-1.tmp") #p"$testdir/test-1.tmp"))
  (assert (equal (truename "link-1")     #p"$testdir/test-1.tmp"))
  (assert (equal (truename "link-2")     #p"$testdir/test-2.tmp"))
  (assert (equal (truename "link-3")     #p"$testdir/link-3"))
  (assert (equal (truename "link-3/")    #p"$testdir/link-3"))
  (assert (equal (truename "link-4")     #p"$testdir/link-4"))
  (assert (equal (truename "link-5")     #p"$testdir/link-5"))
  (assert (equal (truename "link-6")     #p"$testdir/link-6"))
  (sb-ext:exit :code $EXIT_LISP_WIN)
EOF
check_status_maybe_lose "DIRECTORY/TRUENAME part 1" $?

cd "$SBCL_PWD"
run_sbcl <<EOF
  (in-package :cl-user)
  (let* ((directory (directory "$testdir/*.*"))
         (truenames (sort directory #'string< :key #'pathname-name)))
    ;(format t "~&TRUENAMES=~S~%" truenames)
    (finish-output)
    (assert (equal truenames $expected_truenames)))
  (assert (equal (truename "$testdir/test-1.tmp") #p"$testdir/test-1.tmp"))
  (assert (equal (truename "$testdir/link-1")     #p"$testdir/test-1.tmp"))
  (assert (equal (truename "$testdir/link-2")     #p"$testdir/test-2.tmp"))
  (assert (equal (truename "$testdir/link-3")     #p"$testdir/link-3"))
  (assert (equal (truename "$testdir/link-4")     #p"$testdir/link-4"))
  (assert (equal (truename "$testdir/link-5")     #p"$testdir/link-5"))
  (assert (equal (truename "$testdir/link-6")     #p"$testdir/link-6"))
  (sb-ext:exit :code $EXIT_LISP_WIN)
EOF
check_status_maybe_lose "DIRECTORY/TRUENAME part 2" $?
cleanup_test_subdirectory

# Test DIRECTORY on a tree structure of directories.
use_test_subdirectory

touch water dirt
mkdir animal plant
mkdir animal/vertebrate animal/invertebrate
mkdir animal/vertebrate/mammal
mkdir animal/vertebrate/snake
mkdir animal/vertebrate/bird
mkdir animal/vertebrate/mammal/bear
mkdir animal/vertebrate/mammal/mythical
mkdir animal/vertebrate/mammal/rodent
mkdir animal/vertebrate/mammal/ruminant
touch animal/vertebrate/mammal/platypus
touch animal/vertebrate/mammal/walrus
touch animal/vertebrate/mammal/bear/grizzly
touch animal/vertebrate/mammal/mythical/mermaid
touch animal/vertebrate/mammal/mythical/unicorn
touch animal/vertebrate/mammal/rodent/beaver
touch animal/vertebrate/mammal/rodent/mouse
touch animal/vertebrate/mammal/rodent/rabbit
touch animal/vertebrate/mammal/rodent/rat
touch animal/vertebrate/mammal/ruminant/cow
touch animal/vertebrate/snake/python
touch plant/kingsfoil plant/pipeweed
run_sbcl <<EOF
(in-package :cl-user)
(defun absolutify (pathname)
  "Convert a possibly-relative pathname to absolute."
  (merge-pathnames pathname
                   (make-pathname :directory
                                  (pathname-directory
                                   *default-pathname-defaults*))))
(defun sorted-truenamestrings (pathname-designators)
  "Convert a collection of pathname designators into canonical form
using TRUENAME, NAMESTRING, and SORT."
  (sort (mapcar #'namestring
                (mapcar #'truename
                        pathname-designators))
        #'string<))
(defun need-match-1 (directory-pathname result-sorted-truenamestrings)
  "guts of NEED-MATCH"
  (let ((directory-sorted-truenamestrings (sorted-truenamestrings
                                           (directory directory-pathname))))
    (unless (equal directory-sorted-truenamestrings
                   result-sorted-truenamestrings)
      (format t "~&~@<DIRECTORY argument = ~_~2I~S~:>~%"
              directory-pathname)
      (format t "~&~@<DIRECTORY result = ~_~2I~S~:>~%"
              directory-sorted-truenamestrings)
      (format t "~&~@<expected result = ~_~2I~S.~:>~%"
              result-sorted-truenamestrings)
      (error "mismatch between DIRECTORY and expected result"))))
(defun need-match (directory-pathname result-pathnames)
  "Require that (DIRECTORY DIRECTORY-PATHNAME) return RESULT-PATHNAMES
(modulo TRUENAME and NAMESTRING applied to each RESULT-PATHNAME for
convenience in e.g. converting Unix filename syntax idiosyncrasies to
Lisp filename syntax idiosyncrasies)."
  (let ((sorted-result-truenamestrings (sorted-truenamestrings
                                        result-pathnames)))
  ;; Relative and absolute pathnames should give the same result.
  (need-match-1 directory-pathname
                sorted-result-truenamestrings)
  (need-match-1 (absolutify directory-pathname)
                sorted-result-truenamestrings)))
(defun need-matches ()
  "lotso calls to NEED-MATCH"
  ;; FIXME: As discussed on sbcl-devel ca. 2001-01-01, DIRECTORY should
  ;; report Unix directory files contained within its output as e.g.
  ;; "/usr/bin" instead of the CMU-CL-style "/usr/bin/". In that case,
  ;; s:/":": in most or all the NEED-MATCHes here.
  (need-match "./*.*" '("animal/" "dirt" "plant/" "water"))
  (need-match "*.*" '("animal/" "dirt" "plant/" "water"))
  (need-match "animal" '("animal/"))
  (need-match "./animal" '("animal/"))
  (need-match "animal/*.*" '("animal/invertebrate/" "animal/vertebrate/"))
  (need-match "animal/*/*.*"
              '("animal/vertebrate/bird/"
                "animal/vertebrate/mammal/"
                "animal/vertebrate/snake/"))
  (need-match "plant/*.*" '("plant/kingsfoil" "plant/pipeweed"))
  (need-match "plant/**/*.*" '("plant/kingsfoil" "plant/pipeweed"))
  (need-match "plant/**/**/*.*" '("plant/kingsfoil" "plant/pipeweed"))
  (let ((vertebrates (mapcar (lambda (stem)
                               (concatenate 'string
                                            "animal/vertebrate/"
                                            stem))
                             '("bird/"
                               "mammal/"
                               "mammal/bear/" "mammal/bear/grizzly"
                               "mammal/mythical/" "mammal/mythical/mermaid"
                               "mammal/mythical/unicorn"
                               "mammal/platypus"
                               "mammal/rodent/" "mammal/rodent/beaver"
                               "mammal/rodent/mouse" "mammal/rodent/rabbit"
                               "mammal/rodent/rat"
                               "mammal/ruminant/" "mammal/ruminant/cow"
                               "mammal/walrus"
                               "snake/" "snake/python"))))
    (need-match "animal/vertebrate/**/*.*" vertebrates)
    (need-match "animal/vertebrate/mammal/../**/*.*" vertebrates)
    (need-match "animal/vertebrate/mammal/../**/**/*.*" vertebrates)
    #+nil
    (need-match "animal/vertebrate/mammal/mythical/../**/../**/*.*"
                vertebrates))
  (need-match "animal/vertebrate/**/robot.*" nil)
  (need-match "animal/vertebrate/mammal/../**/*.robot" nil)
  (need-match "animal/vertebrate/mammal/../**/robot/*.*" nil)
  #+nil
  (need-match "animal/vertebrate/mammal/robot/../**/../**/*.*" nil))
(need-matches)
(sb-ext:exit :code $EXIT_LISP_WIN)
EOF
check_status_maybe_lose "DIRECTORY/TRUENAME part 3" $?
cleanup_test_subdirectory

# DIRECTORY pattern matching
use_test_subdirectory

mkdir foo
touch foo/aa.txt
touch foo/aa.tmp
mkdir foo/x

mkdir far
touch far/ab.txt
touch far/ab.tmp
mkdir far/x
mkdir far/y
mkdir far/y/x
mkdir far/x/x

mkdir qar
touch qar/ac.txt
touch qar/ac.tmp

mkdir foo.moose
touch foo.bar

mkdir -p a/z c
touch a/z/foo.bar
touch a/z/foo.dummy
ln -s ../a/z c/z

run_sbcl <<EOF
(setf (logical-pathname-translations "foo")
      (list (list "**;*.txt.*" (merge-pathnames "foo/**/*.txt"))
            (list "**;*.*.*" (merge-pathnames "**/*.*"))))

(defun test (pattern &rest expected)
  (let ((wanted (sort (mapcar #'truename expected) #'string< :key #'namestring))
        (got (sort (directory pattern) #'string< :key #'namestring)))
    (unless (equal wanted got)
      (error "wanted:~%  ~S~%got:~%  ~S" wanted got))))
(test "*/a*.txt" "foo/aa.txt" "far/ab.txt" "qar/ac.txt")
(test "fo*/a*.t*" "foo/aa.txt" "foo/aa.tmp")
(test "*/*b.*" "far/ab.txt" "far/ab.tmp")
(test "*a*/*.txt" "far/ab.txt" "qar/ac.txt")
(test "*ar/*.txt" "far/ab.txt" "qar/ac.txt")
(test "f*.*" "far/" "foo/" "foo.moose/" "foo.bar")
(test "f*" "far/" "foo/")
(test "*r" "far/" "qar/")
(test "*r.*" "far/" "qar/")
(test "f*.[mb]*" "foo.moose/" "foo.bar")
(test "f*.m*.*")
(test "f*.b*.*")
(test "*/x" "foo/x/" "far/x/")
(test "far/*/x" "far/y/x/" "far/x/x/")
(test "**/x/" "foo/x/" "far/x/" "far/x/x" "far/y/x/")
(test "foo:*.txt" "foo/aa.txt")
(test "foo:far;*.txt" "far/ab.txt")
(test "foo:foo;*.txt" "foo/aa.txt")
(test "foo:**;*.tmp" "foo/aa.tmp" "far/ab.tmp" "qar/ac.tmp")
(test "foo:foo;*.tmp" "foo/aa.tmp")
(test "c/*/*.bar" "a/z/foo.bar")
(exit :code $EXIT_LISP_WIN)
EOF
check_status_maybe_lose "DIRECTORY/PATTERNS" $?

# Test whether ENSURE-DIRECTORIES-EXIST can create a directory whose
# name contains a wildcard character (it used to get itself confused
# internally).
run_sbcl --eval '(ensure-directories-exist "foo\\*bar/baz.txt")' --eval '(sb-ext:exit)'
test -d foo*bar
check_status_maybe_lose "ENSURE-DIRECTORIES-EXIST part 1" $? \
    0 "(directory exists)"

run_sbcl --eval '(ensure-directories-exist "foo\\?bar/baz.txt")' --eval '(sb-ext:exit)'
test -d foo?bar
check_status_maybe_lose "ENSURE-DIRECTORIES-EXIST part 2" $? \
    0 "(directory exists)"

# DELETE-FILE
use_test_subdirectory
mkdir    sub
touch    deltest
touch    sub/deltest
run_sbcl --eval '(let ((*default-pathname-defaults* (truename "sub")))
                   (delete-file "deltest")
                   (sb-ext:exit))'
test -f deltest && test ! -f sub/deltest
check_status_maybe_lose "delete-file via d-p-d" $? \
  0 "ok"

# RENAME-FILE
use_test_subdirectory
touch one
mkdir sub
touch sub/one
touch foo
ln -s foo link
run_sbcl --eval '(let ((*default-pathname-defaults* (truename "sub")))
                   (rename-file "one" "two"))' \
         --eval '(rename-file "one" "three")' \
         --eval '(rename-file "link" "bar")'
test -f three
check_status_maybe_lose "rename-file" $? \
    0 "ok"
test -f sub/two
check_status_maybe_lose "rename-file via d-p-d" $? \
    0 "ok"
test -f foo && test -L bar
check_status_maybe_lose "rename-file + symlink" $? \
    0 "ok"

# DELETE-DIRECTORY
use_test_subdirectory
mkdir    dont_delete_me
touch    me_neither
mkdir    simple_test_subdir1
mkdir    simple_test_subdir2
mkdir -p deep/1/2/
touch    deep/a
touch    deep/b
touch    deep/1/c
touch    deep/1/d
touch    deep/1/2/e
touch    deep/1/2/f
ln -s    `pwd`/dont_delete_me deep/linky
ln -s    `pwd`/me_neither deep/1/another_linky
mkdir -p one/one
touch    one/one/two
touch    one/two
ln -s dont_delete_me will_fail

run_sbcl --eval '(sb-ext:delete-directory "simple_test_subdir1")' \
         --eval '(sb-ext:delete-directory "simple_test_subdir2/")' \
         --eval '(sb-ext:delete-directory "deep" :recursive t)' \
         --eval '(let ((*default-pathname-defaults* (truename "one")))
                   (delete-directory "one" :recursive t))' \
         --eval '(handler-case (delete-directory "will_fail")
                   (file-error ())
                   (:no-error (x) (declare (ignore x)) (sb-ext:exit :code 1)))' \
         --eval '(sb-ext:exit)'
check_status_maybe_lose "delete-directory symlink" $? \
  0 "ok"
test -L will_fail && test -d dont_delete_me
check_status_maybe_lose "delete-directory symlink 2" $? \
  0 "ok"

test -d simple_test_subdir1
check_status_maybe_lose "delete-directory 1" $? \
  1 "deleted"

test -d simple_test_subdir2
check_status_maybe_lose "delete-directory 2" $? \
  1 "deleted"

test -d deep
check_status_maybe_lose "delete-directory 3" $? \
  1 "deleted"

test -d dont_delete_me
check_status_maybe_lose "delete-directory 4" $? \
  0 "didn't follow link"

test -f me_neither
check_status_maybe_lose "delete-directory 5" $? \
  0 "didn't follow link"

test -f one/two && test -d one && test ! -d one/one
check_status_maybe_lose "delete-directory via d-p-d" $? \
  0 "ok"

# success convention for script
exit $EXIT_TEST_WIN
