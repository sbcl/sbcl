cd ./tools-for-build

base=openbsd-sigcontext

# check for sf_fpstate in struct sigframe (ie: pre July 2010)
rm -f "${base}.c"
cat > "${base}.c" <<EOF
#include <stddef.h>
#include <stdio.h>
#include <machine/frame.h>
int
main()
{
    printf("it works: %d\n", offsetof(struct sigframe, sf_fpstate));
    return (0);
}
EOF
if $GNUMAKE "${base}" > /dev/null 2>&1 && "./${base}" > /dev/null 2>&1
then
    echo '#define OS_OPENBSD_FPSTATE_IN_SIGFRAME'
fi

# check for sc_fpstate in struct sigcontext (ie: July 2010 and later)
rm -f "${base}.c"
cat > "${base}.c" <<EOF
#include <stddef.h>
#include <stdio.h>
#include <signal.h>
int
main()
{
    printf("it works: %d\n", offsetof(struct sigcontext, sc_fpstate));
    return (0);
}
EOF
if $GNUMAKE "${base}" > /dev/null 2>&1 && "./${base}" > /dev/null 2>&1
then
    echo '#define OS_OPENBSD_FPSTATE_IN_SIGCONTEXT'
fi

rm -f "${base}.c" "${base}"
