# TLSF-BSD

Two Level Segregated Fit memory allocator implementation with O(1)
time complexity, distributed under the BSD License.

## Features

* O(1) cost for malloc, free, realloc, memalign
* Extremely low overhead per allocation (4 bytes)
* Low overhead per TLSF management of pools (~3kB)
* Low fragmentation
* Compiles to only a few kB of code and data
* Support for adding and removing memory pool regions on the fly

## API Usage

You can essentially pass a memory pool for TLSF to manage.
```C
enum { STATIC_POOL_SIZE = 1 << 20 };
static char s_pool[STATIC_POOL_SIZE];

tlsf_t instance = tlsf_create_with_pool(s_pool, STATIC_POOL_SIZE);

void *data = tlsf_malloc(instance, 64);
tlsf_free(instance, data);
```

## Caveats

* Currently, assumes architecture can make 4-byte aligned accesses
* Not designed to be thread safe; the user must provide this

## Notes
This code was based on the TLSF 1.4 spec and documentation found at:
    http://www.gii.upv.es/tlsf/main/docs

## Reference

M. Masmano, I. Ripoll, A. Crespo, and J. Real.
TLSF: a new dynamic memory allocator for real-time systems.
In Proc. ECRTS (2004), IEEE Computer Society, pp. 79-86.

This implementation was written to the specification of the document,
therefore no GPL restrictions apply.

It also leverages the TLSF 2.0 improvement to shrink the per-block overhead
from 8 to 4 bytes.

## Known Issues

* Due to the internal block structure size and the implementation
details of `tlsf_memalign`, there is worst-case behavior when requesting
small (<16 byte) blocks aligned to 8-byte boundaries. Overuse of memalign
will generally increase fragmentation, but this particular case will leave
lots of unusable "holes" in the pool. The solution would be to internally
align all blocks to 8 bytes, but this will require significantl changes
to the implementation.

## Licensing

TLSF-BSD is freely redistributable under the two-clause BSD License.
Use of this source code is governed by a BSD-style license that can be found
in the `LICENSE` file.
