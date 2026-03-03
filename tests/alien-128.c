#include <stdint.h>

uint64_t uint128_low_64(unsigned __int128 x) { return (uint64_t)x; }
uint64_t uint128_high_64(unsigned __int128 x) { return (uint64_t)(x >> 64); }

uint64_t int128_low_64( __int128 x) { return (uint64_t)x; }
int64_t int128_high_64(__int128 x) { return (int64_t)(x >> 64); }
