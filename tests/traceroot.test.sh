#!/bin/sh

# tests related to 'traceroot'

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

use_test_subdirectory

run_sbcl --eval "(sb-ext:exit :code (or #+(and sb-thread sb-traceroot) 0 7))"
test $? = 7 && exit $EXIT_TEST_WIN # Pass if feature is absent or not fully working

tmpfilename="$TEST_FILESTEM.out"

run_sbcl <<EOF >$tmpfilename
(setq sb-ext:*evaluator-mode* :compile)
(defvar *fred*)
(defstruct foo a)
;; CLEARSTUFF zeroes out a bunch of words below the stack pointer,
;; in case OBJ was spilled to a stack slot that randomly is left untouched
;; afterward, thereby making the test spuriously fail.
(defun clearstuff () (sb-int:dx-let ((b (make-array 20))) (eval b)))
(defun test1 (wp obj root)
  (let ((*fred* (list (make-foo :a (list (vector #xfeefa (list obj)))))))
    (setq obj nil) ; so OBJ is not found as a stack reference
    (ecase root
      (:tls
       (clearstuff)
       (sb-ext::gc-and-search-roots wp))
      (:bindings ; bind *FRED* again so the old value is on the binding stack
       (let ((*fred* 1))
         (clearstuff)
         (sb-ext::gc-and-search-roots wp)))
      (:stack
       ; put the OBJ back on the control stack
       ; and also ensure that *FRED* is not a root.
       (setq obj *fred* *fred* nil)
       (clearstuff)
       (sb-ext::gc-and-search-roots wp)))))

(let ((wp (make-weak-pointer (list 1 2 3 4))))
  (test1 wp (weak-pointer-value wp) :stack)
  (test1 wp (weak-pointer-value wp) :tls)
  (test1 wp (weak-pointer-value wp) :bindings)
  nil)
EOF

# In a typical test run the outputs would resemble as follows.
# Each group of data contains (gen, class, descriptor, wordindex) where:
#  * "gen"        = generation number, or 'S' for static
#  * "class"      = name of an instance or the name of the widetag
#  * "descriptor" = Lisp pointer to the containing object
#  * "wordindex"  = index into that object from the GC's point-of-view.
#
# (1) {"main thread":C stack:fun=0x1002e347db=TEST1}->(g5,cons)0x1002e76fd7[0]
#      ->(g5,FOO)0x1002c623d3[1]->(g5,cons)0x1002c632a7[0]
#      ->(g5,simple vector)0x1002c63bbf[3]->(g5,cons)0x1002c64077[0]->0x1002c64807.
# (2) {"main thread":TLS:*FRED*}->(g5,cons)0x1002c881f7[0]
#      ->(g5,FOO)0x1002c8a673[1]->(g5,cons)0x1002c8b4d7[0]
#      ->(g5,simple vector)0x1002c8bc2f[3]->(g5,cons)0x1002c8c457[0]->0x1002c8c927.
# (3) {"main thread":bindings:*FRED*}->(g5,cons)0x1002c80107[0]
#      ->(g5,FOO)0x1002c825d3[1]->(g5,cons)0x1002c83427[0]
#      ->(g5,simple vector)0x1002c83b9f[3]->(g5,cons)0x1002c84377[0]->0x1002c848c7.

# Should be able to identify a specific Lisp thread
# May or may not work for other than x86-64
thread=`run_sbcl --eval '(or #+(and sb-thread x86-64) (princ "\"main thread\":"))' --quit`

win1=`awk '/C stack.+TEST1.+cons.+FOO.+cons.+vector.+cons/{print "win\n"}' $tmpfilename`
win2=`awk '/'"${thread}"'TLS:\*FRED/{print "win\n"}' $tmpfilename`
win3=`awk '/bindings:\*FRED/{print "win\n"}' $tmpfilename`

test z$win1 = zwin -a z$win2 = zwin -a z$win3 = zwin && exit $EXIT_TEST_WIN
