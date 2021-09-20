#include <interr.h>

int main(int argc, char *argv[], char *envp[])
{
    extern int initialize_lisp(int argc, char *argv[], char *envp[]);
    initialize_lisp(argc, argv, envp);
    lose("unexpected return from initial thread in main()");
    return 0;
}
