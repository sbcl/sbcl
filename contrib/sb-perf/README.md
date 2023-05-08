# SB-PERF


This contrib generates symbol files for use with Perf(https://perf.wiki.kernel.org/index.php/Main_Page).


## Usage

The typical workflow is:

1. Compile your application
1. Load `sb-perf` via `(load #P"SYS:CONTRIB;sb-perf;export-syms.lisp")`
1. Export symbols by running `(write-perfmap)`
1. While the code to be profiled is active, use eg. `perf record -g -p <pid of SBCL>`
1. Analyse the results (eg. as flamegraph, via `perf report`, etc.)

## Example

With this codeblock:

```lisp
(defun foo (x)
  (loop for i below x
        sum (sqrt i)))
(foo 1000000000)
```

run via `swank`, having `perf record -g -p <pid>` active for a second, then looking at `perf script`, an example output is


```
repl-thread 3593327 915377.321892:     923889 cycles:
                53abb7de FOO+0xce (/tmp/perf-3593296.map)
                 46dd65b [unknown] (...sbcl core...)
                 4597382 [unknown] (...sbcl core...)
                53a1458d SWANK::EVAL-REGION+0x17d (/tmp/perf-3593296.map)
                53a4c436 "DEFUN_REPL-EVAL"+0x306 (/tmp/perf-3593296.map)
                53a4c774 SWANK-REPL::TRACK-PACKAGE+0x9c (/tmp/perf-3593296.map)
                53a4c39a "DEFUN_REPL-EVAL"+0x26a (/tmp/perf-3593296.map)
                539f5810 "DEFUN_CALL-WITH-RETRY-RESTART"+0x198 (/tmp/perf-3593296.map)
                53a4c2b1 "DEFUN_REPL-EVAL"+0x181 (/tmp/perf-3593296.map)
                539c09d7 SWANK/SBCL::CALL-WITH-DEBOOTSTRAPPING+0xaf (/tmp/perf-3593296.map)
                53992e12 SWANK/BACKEND:CALL-WITH-SYNTAX-HOOKS+0xba (/tmp/perf-3593296.map)
                53a0f0fe SWANK::CALL-WITH-BUFFER-SYNTAX+0x23e (/tmp/perf-3593296.map)
                53a4c1f7 "DEFUN_REPL-EVAL"+0xc7 (/tmp/perf-3593296.map)
                53a4be92 SWANK-REPL:LISTENER-EVAL+0x2ba (/tmp/perf-3593296.map)
                 46dd65b [unknown] (...sbcl core...)
                 4597382 [unknown] (...sbcl core...)
                53a12369 "DEFUN_EVAL-FOR-EMACS"+0x411 (/tmp/perf-3593296.map)
                539fea62 SWANK::PROCESS-REQUESTS+0x39a (/tmp/perf-3593296.map)
                539fde4e "DEFUN_HANDLE-REQUESTS"+0xf7e (/tmp/perf-3593296.map)
                539fd765 "DEFUN_HANDLE-REQUESTS"+0x895 (/tmp/perf-3593296.map)
                539ccf7b SWANK/SBCL::CALL-WITH-BREAK-HOOK+0x9b (/tmp/perf-3593296.map)
                539cdd21 "DEFIMPLEMENTATION_CALL-WITH-DEBUGGER-HOOK"+0x141 (/tmp/perf-3593296.map)
                5399969e SWANK/BACKEND:CALL-WITH-DEBUGGER-HOOK+0xe6 (/tmp/perf-3593296.map)
                539fe537 "DEFUN_HANDLE-REQUESTS"+0x1667 (/tmp/perf-3593296.map)
                539e635d SWANK::CALL-WITH-BINDINGS+0x3bd (/tmp/perf-3593296.map)
                539fd2f2 "DEFUN_HANDLE-REQUESTS"+0x422 (/tmp/perf-3593296.map)
                53a4a7ce SWANK-REPL::REPL-LOOP+0x56 (/tmp/perf-3593296.map)
                53a4a6b5 "DEFUN_SPAWN-REPL-THREAD"+0x24d (/tmp/perf-3593296.map)
                539e6010 SWANK::CALL-WITH-BINDINGS+0x70 (/tmp/perf-3593296.map)
                53a4a616 "DEFUN_SPAWN-REPL-THREAD"+0x1ae (/tmp/perf-3593296.map)
                 47491bc [unknown] (...sbcl core...)
                 4749a86 [unknown] (...sbcl core...)
                 4748d53 [unknown] (...sbcl core...)
                 4749586 [unknown] (...sbcl core...)
                 4748b10 [unknown] (...sbcl core...)
                   4b9f1 Lcall+0xe (.../src/runtime/sbcl)
                   1675a funcall1+0x1a (.../src/runtime/sbcl)
                   2e62f new_thread_trampoline+0x10f (.../src/runtime/sbcl)
                   88fd4 start_thread+0x2c4 (/usr/lib/x86_64-linux-gnu/libc.so.6)
```
