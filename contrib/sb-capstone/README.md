SB-CAPSTONE
=======
Capstone integration for SBCL

Capstone [1] is a lightweight multi-platform, multi-architecture disassembly framework.
Capstone is based off the LLVM project, and can target a variety of architectures, including but not limiting to
PowerPC, x86 and ARM.
Capstone is implemented in C [2], and has several bindings into other languages.
A binding into Common Lisp was not previously available.

Binding
--------

We exported constants and routines from Capstone main header file [3] using the sb-alien SBCL package.

Requirements
--------

SB-CAPSTONE requires having the Capstone library installed, as a shared object.
See the Capstone file COMPLILE.TXT for instructions, and then install file "libcapstone.so" where the
load-shared-object function can find it.
You can set up the LD_LIBRARY_PATH environment variable if necessary.

Examples
--------

An example on using the bindings can be found on the tests. We rely on sb-alien heavily for passing system addresses to Capstone (where pointers were used in the C analog)

Example: using Capstone's cs_open to create a handle

    (require :sb-capstone)

    (defun get-handle ()
      (multiple-value-bind (return-code handle) (sb-capstone:cs-open-for-target '(:x86-64 :little-endian))
        handle))



[1] http://www.capstone-engine.org
[2] https://github.com/aquynh/capstone
[3] https://github.com/aquynh/capstone/blob/master/include/capstone.h
