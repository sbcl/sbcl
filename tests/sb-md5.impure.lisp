(require :sb-md5)
(setq run-tests::*allowed-inputs* :any) ; makes random pathnames without aid of WITH-SCRATCH-FILE
(load "../contrib/sb-md5/md5-tests.lisp")
