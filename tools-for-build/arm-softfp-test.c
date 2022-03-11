int main ()
{
/*
   The C compiler defines __ARM_PCS_VFP when the hard float ABI is in
   use and floats/doubles are passed to and returned from functions
   via VFP registers. Otherwise, floats are treated as integers for
   the purpose of function calls.

   Confusingly, __SOFTFP__ is defined only when software floating
   point emulation is used within a function, and doesn't reveal which
   function calling convention is used. It will be defined with
   -mfloat-abi=soft but not -mfloat-abi=softfp
*/
#if defined __arm__ && !defined __ARM_PCS_VFP
    return 104;
#else
    return 0;
#endif
}
