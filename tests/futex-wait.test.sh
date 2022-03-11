#!/bin/sh

. ./subr.sh

run_sbcl <<EOF
 ;; The desired output differs by many factors, but an arbitrary slop allowance
 ;; could let the old bug creep back in.
 #+(and linux sb-thread) (exit :code #+(or x86 x86-64) 45
                                     #-(or x86 x86-64) 100)
 ;; can't run the test
 (exit :code 0)
EOF
expect_test_outcome=$?
if [ $expect_test_outcome -eq 0 ] ; then # test can't be executed
    # we don't have a way to exit shell tests with "inapplicable" as the result
    exit $EXIT_TEST_WIN
fi

# Some distributions do not have strace by default or restrict the
# ptrace system call.
if which strace > /dev/null && ! strace ls > /dev/null 2>&1 ; then
    exit $EXIT_TEST_WIN
fi

set -e

tracelog=`mktemp ${TMPDIR:-/tmp}/$TEST_FILESTEM.XXXXXX`

strace -f -e futex -e signal=\!sigsegv -o $tracelog \
  $SBCL_RUNTIME --core "$SBCL_CORE" $SBCL_ARGS <<EOF
(defvar *m* (sb-thread:make-mutex :name "stupid"))
(defvar *thr*)

(sb-thread:grab-mutex *m*)

;; We need to simulate some thread observing the mutex in a contested state, while
;; the thread that currently holds it releases and re-grabs it again quickly enough
;; to place it in state 1, not state 2 (but having correctly notified waiters).
;; It's also possible to have more than 2 threads involved because any number
;; of threads can be woken by futex_wait even with a parameter of N=1.
;; Due to a flaw in our rendition of Mutex Algorithm 2 from the reference paper,
;; when this happened, any of the waiters might never get put into a sleep
;; in its futex wait - it would get EWOULDBLOCK every time, which is acceptable
;; in that the algorithm wasn't broken, but it was highly inefficient.

;; There's no way to force this situation to arise other than by poking at the
;; state of the mutex. So we want to simulate this sequence of events:
;;;   step 1: set state 0 (available)  |
;;;   step 2: futex wake               | from one thread
;;;   step 3: set state 1 (taken)      |
;;; But we're actually going to do this instead:
;;;   step 1: set state 1 (taken)
;;;   step 2: futex wake
;;; By doing this, the awakened waiter is sure to observe state 1 and only state 1.
;;;
;;; Our bug turned the futex algorithm into the world's most expensive spinlock.
;;; Each "spin" on the lock bit would require a system call and return.

(defun test ()
(setf (sb-thread::mutex-state *m*) 2) ; contested state
(setq *thr*
      (sb-thread:make-thread
       (lambda ()
         ;; thr will see the mutex in state 2
         (format t "~&thread ~s started~%" sb-thread:*current-thread*)
         (sb-thread:grab-mutex *m*)
         (format t "~&thread gets mutex~%"))))

;; Give the thread some time to enter its futex wait the normal way, but observing
;; that the initial state is already contested. Therefore it avoids doing one compare-and-swap
;; to set it to contested. And due to the bug, it was treating the state as *always*
;; contested even when it was not. And the optimization to avoid changing the state
;; to contested therefore caused every futex_wait call to return immediately.
(sleep .025)
;; Now pretend this thread grabbed/notified/re-grabbed sooner than the other thread
;; could execute enough of its retry loop to win the grab. i.e. it's going to sleep
;; again on the futex word, but where it would incorrectly think the mutex is already
;; in state 2 and would not change it, thus failing the match condition in futex_wait.
(setf (sb-thread::mutex-state *m*) 1)
(sb-sys:with-pinned-objects (*m*)
  (let ((disp
         (- (* (+ sb-vm:instance-slots-offset
                  (sb-kernel:get-dsd-index sb-thread::mutex sb-thread::state))
               sb-vm:n-word-bytes)
            sb-vm:instance-pointer-lowtag)))
    (sb-thread::futex-wake (+ (sb-kernel:get-lisp-obj-address *m*) disp
                              #+(and 64-bit big-endian) 4)
                           1)))

(format t "~&looks like mutex state is ~d~%" (sb-thread::mutex-state *m*))
(sleep .05) ; let that thread lose for a while
(sb-thread:release-mutex *m*)
(sb-thread:join-thread *thr*))
(compile 'test)
(test)
EOF

n=`awk 'END{print NR}' < $tracelog`
rm $tracelog

if [ $n -le $expect_test_outcome ]
then
  exit $EXIT_TEST_WIN
fi
# This test was seeing >1500 calls to futex_wait due to the bug
echo N was $n
exit 0

# SBCL had a bug in %%WAIT-FOR-MUTEX. This is an examination of why one should not
# endeavor to make the slightest alteration to researched and published code,
# potentially even so far as eschewing gratuitious abtractions such as introduction
# of symbolic constants that if anything make it difficult to compare against
# the reference algorithm.
#
# value of C as-is     STATE value
# from top of PROG     after futex-wait          outcome
# ================     ================          =======
#
# 1                    1                         same as reference algorithm
#    after wait: cmpxchg(val,0,2) fails
#      reference algorithm assigns C := 1 here. SBCL did not assign,
#      but the value of C is that anyway.
#    at retry  : cmpxchg(val,1,2) suceeds
#
# ----------------------------------------------------------------------------
#
# 2                    1                         extra futex_wait/wake cycles
#    after wait: cmpxchg(val,0,2) fails
#      reference algorithm assigns C := 1 here
#      SBCL incorrectly omits the next cmpxchg, which was needed.
#    at retry  : cmpxchg(val,1,2) succeeds ; SBCL omits by mistake
#
#    by omitting a required cmpxchg, the state is perceived as 2 when it isn't.
#    futex_wait() returns EWOULDBLOCK. This cycle continues forever
#    until the mutex is released.
#
# ----------------------------------------------------------------------------
#
# 1                    2                         one extra cmpxchg
#    after wait: cmpxchg(val,0,2) fails
#      reference algorithm assigns C := 2 here but SBCL algorithm did not,
#      therefore the reference algorithm does not attempt next cmpxchg.
#    at retry  : cmpxchg(val,1,2) fails   ;  this cmpxchg is not needed.
#
# ----------------------------------------------------------------------------
#
# 2                     2
#    after wait: cmpxchg(val,0,2) fails
#      reference algorithm assigns C := 2 here. SBCL did not assign,
#      but the value of C is that anyway.
#
