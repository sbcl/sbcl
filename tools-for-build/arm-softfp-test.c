int main ()
{
#if defined __arm__ && defined __SOFTFP__
    return 104;
#else
    return 0;
#endif
}
