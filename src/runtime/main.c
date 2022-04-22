#include <interr.h>
#if defined LISP_FEATURE_SB_DEVEL && defined LISP_FEATURE_LINUX
#include <sys/prctl.h>
#endif

int main(int argc, char *argv[], char *envp[])
{
#if defined LISP_FEATURE_SB_DEVEL && defined LISP_FEATURE_LINUX
    prctl(PR_SET_PTRACER, PR_SET_PTRACER_ANY);
#endif
    extern int initialize_lisp(int argc, char *argv[], char *envp[]);
    initialize_lisp(argc, argv, envp);
    lose("unexpected return from initial thread in main()");
    return 0;
}
