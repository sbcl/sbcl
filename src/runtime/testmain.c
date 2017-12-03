#include <stdio.h>
#include <dlfcn.h>

int main(int argc, char *argv[], char *envp[])
{
    void* lib = dlopen("./libsbcl.so", RTLD_NOW);
    if (!lib) {
        fprintf(stderr, "Can't open shared lib\n");
        return 1;
    }
    int (*sbcl_main)() = dlsym(lib, "main");
    if (!sbcl_main) {
        fprintf(stderr, "Can't find main\n");
        return 1;
    }
    return (*sbcl_main)(argc, argv, envp);
}
