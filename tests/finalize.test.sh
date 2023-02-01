. ./subr.sh

# This was failing with:
# Verify after GC(6) [immobile]
# Ptr 0x1001100c9f @ 503c1028 (lispobj 503c1003,pg-1) sees junk
# Ptr 0x1001100ebf @ 503c10a8 (lispobj 503c1083,pg-1) sees junk
# Ptr 0x100110102f @ 503c1128 (lispobj 503c1103,pg-1) sees junk
# ...
run_sbcl <<EOF
  (setf (extern-alien "verify_gens" char) 0)
  ;; simple-streams causes invalidation of many layouts
  ;; that are at low addresses, like for FUNDAMENTAL-STREAM
  (require :sb-simple-streams)
  (defun foo ()
    ;; we need a *lot* of finalizers for cull_weak_hash_tables
    ;; to use up so much dynamic space that the mark bit
    ;; calculation would go wrong
    (dotimes (i 50000)
      (finalize (cons i i) (lambda ()))))
  (compile 'foo)
  (foo)
  (gc :gen 7)
EOF

exit $EXIT_TEST_WIN
