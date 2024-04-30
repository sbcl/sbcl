#!/bin/bash -ex

DIR="$HOME/src"

function bench_one {
  sbcl_dir="$1"
  type="$2"
  alloc="$3"
  ht_args="$4"
  table_file="$5"
  time this-sbcl ${sbcl_dir} --noinform --disable-ldb \
       --dynamic-space-size 16384 --disable-debugger --quit \
       --eval '(load (compile-file "benchmark-sbcl-hash-tables.lisp"))' \
       --eval "(bench-hash :start 1 :end ${max_keys} :type '${type} :alloc '${alloc} :ht-args '${ht_args} :table-file \"${table_file}\")"
}

function bench_one_float {
  sbcl_name="$1"
  sbcl_dir="$2"
  bench_one $sbcl_dir "float" "(:prog 1)" "(:test eq)" "float-prog-1-${sbcl_name}.tbl"
}

function bench_one_eq {
  sbcl_name="$1"
  sbcl_dir="$2"
  bench_one $sbcl_dir "fixnum" "(:prog 1)" "(:test eq)" "fixnum-prog-1-${sbcl_name}.tbl"
  bench_one $sbcl_dir "fixnum" "(:prog 12)" "(:test eq)" "fixnum-prog-12-${sbcl_name}.tbl"
  bench_one $sbcl_dir "fixnum" "(:rnd 6)" "(:test eq)" "fixnum-rnd-6-${sbcl_name}.tbl"
  bench_one $sbcl_dir "cons" "(:rnd 6)" "(:test eq)" "cons-rnd-6-${sbcl_name}.tbl"
  bench_one $sbcl_dir "symbol" ":existing" "(:test eq)" "symbol-existing-${sbcl_name}.tbl"
}

function bench_one_equal {
  sbcl_name="$1"
  sbcl_dir="$2"
  bench_one $sbcl_dir "string" ":existing-symbol-name" "(:test equal)" "string-existing-symbol-name-${sbcl_name}.tbl"
  bench_one $sbcl_dir "string" ":existing" "(:test equal)" "string-existing-${sbcl_name}.tbl"
}

function bench_float {
  max_keys="(expt 2 11)"
  bench_one_float "prefuzz" "$DIR/sbcl.1"
  bench_one_float "flat" "$DIR/sbcl.f"
  max_keys="(expt 2 23)"
  bench_one_float "murmur" "$DIR/sbcl.u"
  bench_one_float "flat-safe" "$DIR/sbcl.s"
  bench_one_float "flat-safe-small" "$DIR/sbcl.2"
}

function bench_eq {
  max_keys="(expt 2 24)"
  bench_one_eq "prefuzz" "$DIR/sbcl.1"
  bench_one_eq "murmur" "$DIR/sbcl.u"
  bench_one_eq "flat" "$DIR/sbcl.f"
  bench_one_eq "flat-safe" "$DIR/sbcl.s"
  bench_one_eq "flat-safe-small" "$DIR/sbcl.2"
}

function bench_equal {
  max_keys="(expt 2 24)"
  bench_one_equal "sbcl" "$DIR/sbcl.2"
  bench_one_equal "adaptive" "$DIR/sbcl.3"
}

function bench_all {
  bench_float
  bench_eq
  bench_equal
}

bench_all | tee -a microbenchmark.log
