OUT=$(mktemp -d)
for arch in x86 x86-64; do
    for host in sbcl ccl32 ccl64 clisp; do
        mkdir -p $OUT/$arch/$host
    done
done

for arch in x86 x86-64; do
    for host in sbcl ccl32 ccl64 clisp; do
        case $host in
            sbcl) xc_host=sbcl;;
            ccl32) xc_host='lx86cl -b';;
            ccl64) xc_host='lx86cl64 -b';;
            clisp) xc_host='clisp -ansi -on-error abort';;
        esac
        ./make.sh --arch=$arch --xc-host="$xc_host" "$@" && tar cf - run-sbcl.sh src/runtime/sbcl obj/from-xc output | tar -C $OUT/$arch/$host -xf -
    done
done

echo done: cd $OUT
