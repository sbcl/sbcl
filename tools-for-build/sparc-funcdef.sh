cd ./tools-for-build

TMP=sparc-funcdef.S

SUN_FUNCDEF="#define FUNCDEF(x) .type x, #function"
GNU_FUNCDEF="#define FUNCDEF(x) .type x,@function"

echo $SUN_FUNCDEF > $TMP
# cribbed from ldso_stubs, just "some code"
echo "
.globl foo_stub__printf ;
        FUNCDEF(foo_stub__printf) ;
foo_stub__printf: ;
        sethi %hi(printf),%g1 ;
        jmpl %g1+%lo(printf),%g0 ;
        nop /* delay slot*/ ;
.Lprintfe1: ;
        .size    foo_stub__printf,.Lprintfe1-foo_stub__printf ;" >> $TMP

if $GNUMAKE sparc-funcdef.o > /dev/null 2>&1 ; then
    echo $SUN_FUNCDEF
else
    echo $GNU_FUNCDEF
fi
rm -f $TMP
