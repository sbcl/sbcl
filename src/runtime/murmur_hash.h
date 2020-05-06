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

#ifndef GRPC_CORE_LIB_SUPPORT_MURMUR_HASH_H
#define GRPC_CORE_LIB_SUPPORT_MURMUR_HASH_H

#include <stdint.h>
#include <stddef.h>

/* compute the hash of key (length len) */
uint32_t gpr_murmur_hash3(const void* key, size_t len, uint32_t seed);
uint32_t murmur3_fmix32(uint32_t);
#ifdef LISP_FEATURE_64_BIT
uint64_t murmur3_fmix64(uint64_t);
#endif

#endif /* GRPC_CORE_LIB_SUPPORT_MURMUR_HASH_H */
