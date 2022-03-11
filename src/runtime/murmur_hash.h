/*
 *
 * Copyright 2015 gRPC authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#ifndef _MURMUR_HASH_H_
#define _MURMUR_HASH_H_

#include <stdint.h>
#include <stddef.h>

/* compute the hash of key (length len) */
uint32_t gpr_murmur_hash3(const void* key, size_t len, uint32_t seed);
uint32_t murmur3_fmix32(uint32_t);
#ifdef LISP_FEATURE_64_BIT
uint64_t murmur3_fmix64(uint64_t);
#endif

#define FMIX32(h)    \
  (h) ^= (h) >> 16;  \
  (h) *= 0x85ebca6b; \
  (h) ^= (h) >> 13;  \
  (h) *= 0xc2b2ae35; \
  (h) ^= (h) >> 16

#define FMIX64(h) \
  (h) ^= (h) >> 33;             \
  (h) *= 0xff51afd7ed558ccdULL; \
  (h) ^= (h) >> 33;             \
  (h) *= 0xc4ceb9fe1a85ec53ULL; \
  (h) ^= (h) >> 33

#endif /* _MURMUR_HASH_H_ */
