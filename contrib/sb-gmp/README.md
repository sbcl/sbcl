SB-GMP
======

GMP integration for SBCL

This contrib enables the use of bignum computation routines from the
GMP library for SBCL internal bignum calculations.

Some functions can be transparently replaced within SBCL, namely:

 - sb-bignum:multiply-bignum
 - sb-bignum:bignum-truncate
 - sb-bignum:bignum-gcd
 - sb-kernel::two-arg-lcm
 - cl:isqrt

and for making use of the GMP rational arithmetic:

 - sb-kernel::two-arg-+
 - sb-kernel::two-arg--
 - sb-kernel::two-arg-*
 - sb-kernel::two-arg-/

Most of the other SBCL bignum routines rely on these functions for the
heavy computational lifting.

However, SB-GMP also provides easy and transparent access to several
other functions of the GMP library and may be able to replace other
parts of the SBCL bignum machinery in the future. Refer to the GMP
documentation for the respective function specification.

The transparent SBCL integration is automatically activated by loading
SB-GMP.  Some care is taken to behave correctly across core saves and
loads; however, if this does not work correctly, `SB-GMP:UNLOAD-GMP`
and `SB-GMP:LOAD-GMP` can be called to explicitly unload and reload
libgmp and uninstall/reinstall the hooks.  The hooks can also be
directly uninstalled and installed with `SB-GMP:UNINSTALL-GMP-FUNS`
and `SB-GMP:INSTALL-GMP-FUNS`.  However, in most cases, binding
`SB-GMP:*GMP-DISABLED*` to `T` should suffice.
