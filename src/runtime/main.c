int main(int argc, char *argv[], char *envp[])
{
    extern int sbcl_main(int argc, char *argv[], char *envp[]);
    return sbcl_main(argc, argv, envp);
}
