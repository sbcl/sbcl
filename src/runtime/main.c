#include "interr.h"

int use_smlgc;

int main(int argc, char *argv[], char *envp[])
{
    if (getenv("SMLGC")) use_smlgc = 1;
    extern int initialize_lisp(int argc, char *argv[], char *envp[]);

    initialize_lisp(argc, argv, envp);
    lose("unexpected return from initial thread in main()");
    return 0;
}
