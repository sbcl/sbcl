(require :sb-concurrency)
(load "../contrib/sb-concurrency/tests/package.lisp")
(load "../contrib/sb-concurrency/tests/test-utils.lisp")
#-interpreter (load "../contrib/sb-concurrency/tests/test-frlock.lisp")
;; This passes with sb-fasteval but not sb-eval
#-interpreter (load "../contrib/sb-concurrency/tests/test-queue.lisp")
(load "../contrib/sb-concurrency/tests/test-mailbox.lisp")
#-interpreter (load "../contrib/sb-concurrency/tests/test-gate.lisp")
