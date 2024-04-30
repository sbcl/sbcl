#!/bin/bash -eu

# TODO:
#
# - add --sbcl-args
#
# - add --sbcl-tests (corresponding to bench_run_tests)
#
# - add --sbcl-xxx (corresponding to bench_binary)

base=`dirname "$0"`

# SBCL runtime and toplevel options go before and after this,
# respectively.
common_args=("--disable-ldb" "--noinform" "--end-runtime-options"
             "--disable-debugger" "--noprint")

# Pass Deltist options on
deltist_args=()
while true; do
    if [ "$1" = "--" ]; then
        shift
        break
    fi
    if [[ "$1" == --* ]]; then
        deltist_args+=("$1" "$2")
        shift
        shift
    else
        break
    fi
done
deltist_args+=("--warmup" "1"
               "--shuffle" "1"
               "--runs" "10"
               "--max-runs" "20"
               "--max-rse" "0.003"
               "--benchmark-file" ""
               "--shuffle-benchmarks" "0"
               "--skip-high-rse" "1")

sbcl_args=(${common_args[@]} "--quit")

sbcl_test_args=(${common_args[@]} "--no-sysinit" "--no-userinit")

# Print relevant information about the benchmarking environment.
echo "Benchmarking environment"
echo "------------------------"
if which taskset 2>&1 > /dev/null; then
    x=`taskset -c -p $$ | cut -d : -f 2 | xargs`
    echo CPU affinity: $x
    if [ ${#x} != 1 ]; then
        echo "WARNING: CPU affinity is not restricted to a single CPU."
        sleep 0.5
    fi
fi
echo Priority: `ps h -o ni $$` || true
if [ -f /sys/firmware/acpi/platform_profile ]; then
    echo platform_profile: `cat /sys/firmware/acpi/platform_profile`
fi
if [ -f /proc/sys/kernel/randomize_va_space ]; then
    echo "randomize_va_space:" `cat /proc/sys/kernel/randomize_va_space`
fi
if [ -f /sys/devices/system/cpu/intel_pstate/no_turbo ]; then
    x=`cat /sys/devices/system/cpu/intel_pstate/no_turbo`
    if [ "$x" != 1 ]; then
        echo "WARNING: /sys/devices/system/cpu/intel_pstate/no_turbo is $x,"\
             "which increases the variance of measurements."
        sleep 0.5
    fi
fi
if [ -f /sys/devices/platform/thinkpad_acpi/dytc_lapmode ]; then
    x=`cat /sys/devices/platform/thinkpad_acpi/dytc_lapmode`
    if [ "$x" != 0 ]; then
        echo "WARNING: /sys/devices/platform/thinkpad_acpi/dytc_lapmode is $x,"\
             "which increases the variance of measurements."
        sleep 0.5
    fi
fi

# Describe the SBCL versions being compared
echo
if [ $# = 0 ]; then
    echo "No SBCLs to compare. Usage:"
    echo "$0 [ <deltist-options>* -- ] <sbcl-binary-or-repository-dir>*"
    exit 0
fi
sbcls=()
all_sbcls_have_tests=1
echo "SBCLs to compare:"
for sbcl in "$@"; do
    if [ -d "${sbcl}" ]; then
        if [ -x "${sbcl}/run-sbcl.sh" ]; then
            sbcl="${sbcl}/run-sbcl.sh"
        fi
    fi
    echo -n "- ${sbcl}: version: "
    "${sbcl}" --version
    # Print the uncommitted changes in the checkout
    dir=`dirname "${sbcl}"`
    if [ -d "${dir}" -a -d "${dir}/.git/" ]; then
        # Exclude xfloat-math.lisp-expr, xperfecthash63.lisp-expr and
        # similar.
        git -C "${dir}" --no-pager diff HEAD -- . ':(exclude)x*.lisp-expr'
        echo
    fi
    # Note if there are SBCLs to compare without a tests/ dir.
    if [ ! -d "${dir}/tests/" ]; then
        all_sbcls_have_tests=0
    fi
    sbcls+=("${sbcl}")
done

if [ "${all_sbcls_have_tests}" != 1 ]; then
    echo "Not all SBCLs have a tests/ directory."\
         "Skipping run-test.sh based tests."
fi

bench_binary () {
    for sbcl in ${sbcls[@]}; do
        echo "'${sbcl}' ${sbcl_args[@]} $@"
    done
    echo
}

bench_run_tests () {
    if [ "${all_sbcls_have_tests}" = 1 ]; then
        # FIXME: We run the test in each dir, which may be different
        # or may not even exist. The alternative of running the same
        # test file with different SBCLs may fail because these are
        # testing internals. Still, it's useful to compare very
        # similar SBCLs this way.
        for sbcl in ${sbcls[@]}; do
            dir=`dirname "${sbcl}"`
            echo "sh -c 'cd \"${dir}/tests/\" &&"\
                 "env \"TEST_SBCL_ARGS=${sbcl_test_args[@]}\""\
                 "sh run-tests.sh $1'"
        done
        echo
    fi
}

# Pipe benchmarks definitions (one command per line, separated by
# blank lines) to Deltist (--benchmark-file "" means stdin).
(
    a_test_dir="$(dirname "${sbcls[0]}")/tests/"
    # Exclude some tests that hang, are SLEEP-heavy or very noisy
    # (from scheduling and heap layout), or change between versions.
    for i in `ls -1 ${a_test_dir}/*.pure.lisp | xargs -n 1 basename`; do
        if [ "$i" != "gethash-concurrency.pure.lisp" -a \
             "$i" != "hash.pure.lisp" ]; then
            bench_run_tests "$i"
        fi
    done
    for i in `ls -1 ${a_test_dir}/*.impure.lisp | xargs -n 1 basename`; do
        if [ "$i" != "kill-non-lisp-thread.impure.lisp" -a \
             "$i" != "deadline.impure.lisp" -a \
             "$i" != "finalize.impure.lisp" -a \
             "$i" != "finalize-2.impure.lisp" ]; then
            bench_run_tests "$i"
        fi
    done
    for i in `ls -1 ${a_test_dir}/*.sh | xargs -n 1 basename`; do
        bench_run_tests "$i"
    done
) | "${base}/deltist.sh" "${deltist_args[@]}"

# Benchmark with ASDF:LOAD-SYSTEM
(
    bench_binary "--load ${base}/bench-load-system.lisp" "3bmd"
    bench_binary "--load ${base}/bench-load-system.lisp" "alexandria"
    bench_binary "--load ${base}/bench-load-system.lisp" "babel"
    bench_binary "--load ${base}/bench-load-system.lisp" "cl-ppcre"
    bench_binary "--load ${base}/bench-load-system.lisp" "esrap"
    bench_binary "--load ${base}/bench-load-system.lisp" "flexi-streams"
    bench_binary "--load ${base}/bench-load-system.lisp" "jonathan"
    bench_binary "--load ${base}/bench-load-system.lisp" "journal"
    bench_binary "--load ${base}/bench-load-system.lisp" "lmdb"
    bench_binary "--load ${base}/bench-load-system.lisp" "local-time"
    bench_binary "--load ${base}/bench-load-system.lisp" "dref"
    bench_binary "--load ${base}/bench-load-system.lisp" "mgl-pax/full"
    bench_binary "--load ${base}/bench-load-system.lisp" "micmac"
    bench_binary "--load ${base}/bench-load-system.lisp" "osicat"
    bench_binary "--load ${base}/bench-load-system.lisp" "serapeum"
    bench_binary "--load ${base}/bench-load-system.lisp" "try"
) | "${base}/deltist.sh" "${deltist_args[@]}"


# Benchmark with ASDF:TEST-SYSTEM
(
    bench_binary "--load ${base}/bench-test-system.lisp" "3bmd"
    bench_binary "--load ${base}/bench-test-system.lisp" "alexandria"
    bench_binary "--load ${base}/bench-test-system.lisp" "babel"
    bench_binary "--load ${base}/bench-test-system.lisp" "cl-ppcre"
    bench_binary "--load ${base}/bench-test-system.lisp" "esrap"
    bench_binary "--load ${base}/bench-test-system.lisp" "flexi-streams"
    bench_binary "--load ${base}/bench-test-system.lisp" "jonathan"
    bench_binary "--load ${base}/bench-test-system.lisp" "journal"
    bench_binary "--load ${base}/bench-test-system.lisp" "lmdb"
    bench_binary "--load ${base}/bench-test-system.lisp" "local-time"
    bench_binary "--load ${base}/bench-test-system.lisp" "dref"
    bench_binary "--load ${base}/bench-test-system.lisp" "mgl-pax/full"
    bench_binary "--load ${base}/bench-test-system.lisp" "micmac"
    bench_binary "--load ${base}/bench-test-system.lisp" "osicat"
    bench_binary "--load ${base}/bench-test-system.lisp" "serapeum"
    bench_binary "--load ${base}/bench-test-system.lisp" "try"
) | "${base}/deltist.sh" "${deltist_args[@]}"
