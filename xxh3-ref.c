/*
   xxHash - Extremely Fast Hash algorithm
   Development source file for `xxh3`
   Copyright (C) 2019-present, Yann Collet.

   BSD 2-Clause License (http://www.opensource.org/licenses/bsd-license.php)

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are
   met:

       * Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
       * Redistributions in binary form must reproduce the above
   copyright notice, this list of conditions and the following disclaimer
   in the documentation and/or other materials provided with the
   distribution.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   input, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

   You can contact the author at :
   - xxHash source repository : https://github.com/Cyan4973/xxHash
*/

/* A compact, 100% standalone reference XXH3 implementation that focuses on clarity.
 * correctness, and portability, instead of dirty speed hacks.
 *
 * This file aims to be 100% compatible with C90 and C++ with the additional requirement of
 * stdint.h and long long.
 *
 * TODO: There are a lot of things that can be cleaner, and it may not be accurate as the
 * XXH3 hash is still being polished.
 *
 * There is no self test at the moment as the official test values have not been updated
 * for the new algorithm. */

#include <stddef.h>   /* size_t */
#include <stdint.h>   /* uint8_t, uint32_t, uint64_t */

#if defined(__GNUC__) || defined(__TINYC__)
#  define ALIGN(n)       __attribute__ ((__aligned__(n)))
#elif defined(_MSC_VER)
#  define ALIGN(n)      __declspec(align(n))
#elif __cplusplus >= 201103L
#  define ALIGN(n) alignas(n)
#else
#  define ALIGN(n)   /* disabled */
#endif


typedef uint64_t XXH64_hash_t;

typedef struct {
    XXH64_hash_t low64;
    XXH64_hash_t high64;
} XXH128_hash_t;

#ifdef __cplusplus
extern "C" {
#endif

XXH64_hash_t XXH3_64bits_withSeed(void const *const input, size_t const length, XXH64_hash_t const seed);
XXH64_hash_t XXH3_64bits(void const *const input, size_t const length);
XXH128_hash_t XXH3_128bits_withSeed(void const *const input, size_t const length, XXH64_hash_t const seed);
XXH128_hash_t XXH3_128bits(void const *const input, size_t const length);
XXH128_hash_t XXH128(void const *const input, size_t const length, XXH64_hash_t const seed);

#ifdef __cplusplus
}
#endif

static uint32_t const PRIME32_1 = 0x9E3779B1U;   /* 0b10011110001101110111100110110001 */

static uint64_t const PRIME64_1 = 11400714785074694791ULL;   /* 0b1001111000110111011110011011000110000101111010111100101010000111 */
static uint64_t const PRIME64_2 = 14029467366897019727ULL;   /* 0b1100001010110010101011100011110100100111110101001110101101001111 */
static uint64_t const PRIME64_3 =  1609587929392839161ULL;   /* 0b0001011001010110011001111011000110011110001101110111100111111001 */
static uint64_t const PRIME64_4 =  9650029242287828579ULL;   /* 0b1000010111101011110010100111011111000010101100101010111001100011 */
static uint64_t const PRIME64_5 =  2870177450012600261ULL;   /* 0b0010011111010100111010110010111100010110010101100110011111000101 */

/* Portably reads a 32-bit little endian integer from p. */
static uint32_t XXH_read32(uint8_t const *const p)
{
    return (uint32_t)p[0]
        | ((uint32_t)p[1] << 8)
        | ((uint32_t)p[2] << 16)
        | ((uint32_t)p[3] << 24);
}


/* Portably reads a 32-bit little endian integer from p. */
static uint64_t XXH_read64(uint8_t const *const p)
{
    return (uint64_t)p[0]
        | ((uint64_t)p[1] << 8)
        | ((uint64_t)p[2] << 16)
        | ((uint64_t)p[3] << 24)
        | ((uint64_t)p[4] << 32)
        | ((uint64_t)p[5] << 40)
        | ((uint64_t)p[6] << 48)
        | ((uint64_t)p[7] << 56);
}

#define KEYSET_DEFAULT_SIZE 48   /* minimum 32 */

ALIGN(64) static const uint32_t kKey[KEYSET_DEFAULT_SIZE] = {
    0xb8fe6c39U,0x23a44bbeU,0x7c01812cU,0xf721ad1cU,
    0xded46de9U,0x839097dbU,0x7240a4a4U,0xb7b3671fU,
    0xcb79e64eU,0xccc0e578U,0x825ad07dU,0xccff7221U,
    0xb8084674U,0xf743248eU,0xe03590e6U,0x813a264cU,
    0x3c2852bbU,0x91c300cbU,0x88d0658bU,0x1b532ea3U,
    0x71644897U,0xa20df94eU,0x3819ef46U,0xa9deacd8U,
    0xa8fa763fU,0xe39c343fU,0xf9dcbbc7U,0xc70b4f1dU,
    0x8a51e04bU,0xcdb45931U,0xc89f7ec9U,0xd9787364U,

    0xeac5ac83U,0x34d3ebc3U,0xc581a0ffU,0xfa1363ebU,
    0x170ddd51U,0xb7f0da49U,0xd3165526U,0x29d4689eU,
    0x2b16be58U,0x7d47a1fcU,0x8ff8b8d1U,0x7ad031ceU,
    0x45cb3a8fU,0x95160428U,0xafd7fbcaU,0xbb4b407eU,
};

/* Does a 64-bit to 128-bit multiply, then xor's the low bits of the product with
 * the high bit for a 64-bit result.
 *
 * The GCC family has a handy __uint128_t type which makes this super easy,
 * however, it is not defined on a 32-bit target, mainly because it is really complex,
 * likely as a sign to not use 128-bit integers on 32-bit.
 *
 * Thankfully, we never use this more than once in each hash. */
static uint64_t XXH3_mul128_fold64(uint64_t const lhs, uint64_t const rhs)
{
#if defined(__SIZEOF_INT128__) || (defined(_INTEGRAL_MAX_BITS) && _INTEGRAL_MAX_BITS >= 128)

    __uint128_t product = (__uint128_t) lhs * (__uint128_t) rhs;
    return (uint64_t) (product & 0xFFFFFFFFFFFFFFFFULL) ^ (uint64_t) (product >> 64);

    /* There are other platform-specific versions in the official repo.
     * They would all be left out in favor of the code above, but it is not
     * portable, so we keep the generic version. */

#else /* Portable scalar version */

    /* Hacker's Delight version, simplified, unrolled, and using 32->64 multiplies instead of 16->32. */
    uint64_t const lo_lo      = ((lhs & 0xFFFFFFFF) * (rhs & 0xFFFFFFFF));
    uint64_t const hi_lo      = ((lhs >> 32)        * (rhs & 0xFFFFFFFF)) + (lo_lo >> 32);
    uint64_t const lo_hi      = ((lhs & 0xFFFFFFFF) * (rhs >> 32))        + (hi_lo & 0xFFFFFFFF);
    uint64_t const product_hi = ((lhs >> 32)        * (rhs >> 32))        + (lo_hi >> 32) + (hi_lo >> 32);
    uint64_t const product_lo = (lo_lo & 0xFFFFFFFF) | (lo_hi << 32);

    return product_hi ^ product_lo;

#endif
}

#define STRIPE_LEN 64
#define STRIPE_ELTS (STRIPE_LEN / sizeof(uint32_t))
#define ACC_NB (STRIPE_LEN / sizeof(uint64_t))

static XXH64_hash_t XXH3_avalanche(uint64_t hash)
{
    hash ^= hash >> 29;
    hash *= PRIME64_3;
    hash ^= hash >> 32;
    return hash;
}

static uint64_t XXH3_readKey64(uint32_t const *const ptr)
{
    return (uint64_t) ptr[0] | ((uint64_t) ptr[1] << 32);
}


/* ==========================================
 * Short keys
 * ========================================== */

/* Hashes short keys from 1 to 3 bytes. */
static XXH64_hash_t XXH3_len_1to3_64b(uint8_t  const *const input, size_t const length,
                                      uint32_t const *const key,  XXH64_hash_t const seed)
{
    uint8_t  const byte1 = input[0];
    uint8_t  const byte2 = (length > 1) ? input[1] : input[0];
    uint8_t  const byte3 = input[length - 1];
    uint32_t const lane1 = (uint32_t)  byte1 | ((uint32_t) byte2 << 8);
    uint32_t const lane2 = (uint32_t) length + ((uint32_t) byte3 << 2);
    uint32_t const low   = lane1 + (uint32_t) (seed & 0xFFFFFFFF) + key[0];
    uint32_t const high  = lane2 + (uint32_t) (seed >> 32)        + key[1];
    uint64_t const product = (uint64_t) low * (uint64_t) high;
    return XXH3_avalanche(product);
}

/* Hashes short keys from 4 to 8 bytes. */
static XXH64_hash_t XXH3_len_4to8_64b(uint8_t  const *const input, size_t const length,
                                      uint32_t const *const key,  XXH64_hash_t const seed)
{
    uint32_t const input_lo = XXH_read32(input);
    uint32_t const input_hi = XXH_read32(input + length - 4);
    uint64_t const input64 = input_lo | ((uint64_t) input_hi << 32);
    uint64_t const keyed = input64 ^ (XXH3_readKey64(key) + seed);
    uint64_t const mixed = (uint64_t) length + XXH3_mul128_fold64(keyed, PRIME64_1);
    return XXH3_avalanche(mixed);
}

/* Hashes short keys from 9 to 16 bytes. */
static XXH64_hash_t XXH3_len_9to16_64b(uint8_t  const *const input, size_t const length,
                                       uint32_t const *const key,  XXH64_hash_t const seed)
{
   uint64_t const input1 = XXH_read64(input)              ^ (XXH3_readKey64(key)     + seed);
   uint64_t const input2 = XXH_read64(input + length - 8) ^ (XXH3_readKey64(key + 2) - seed);
   uint64_t const acc = (uint64_t) length + input1 + input2 + XXH3_mul128_fold64(input1, input2);
   return XXH3_avalanche(acc);
}

/* Hashes short keys that are less than or equal to 16 bytes. */
static XXH64_hash_t XXH3_len_0to16_64b(uint8_t const *const input, size_t const length,
                                       XXH64_hash_t const seed)
{
    if (length > 8)
        return XXH3_len_9to16_64b(input, length, kKey, seed);
    else if (length >= 4)
        return XXH3_len_4to8_64b(input, length, kKey, seed);
    else if (length != 0)
        return XXH3_len_1to3_64b(input, length, kKey, seed);
    return seed;
}

/* This is the main loop. This is usually written in SIMD code. */
static void XXH3_accumulate_512(uint64_t *const acc, uint8_t const *const input,
                                uint32_t const *const key)
{
    size_t i;
    for (i = 0; i < ACC_NB; i++) {
        uint64_t const data_val = XXH_read64(input + (8 * i));
        uint64_t const key_val  = XXH3_readKey64(key + (2 * i));
        uint64_t const data_key = data_val ^ key_val;
        acc[i] += (data_key & 0xFFFFFFFF) * (data_key >> 32);
        acc[i] += data_val;
    }
}

/* Scrambles input. This is usually written in SIMD code, as it is usually part of the main loop. */
static void XXH3_scrambleAcc(uint64_t *const acc, uint32_t const *const key)
{
    size_t i;
    for (i = 0; i < ACC_NB; i++) {
        uint64_t const key_val = XXH3_readKey64(key + (2 * i));
        uint64_t acc_val = acc[i];
        acc_val ^= acc_val >> 47;
        acc_val ^= key_val;
        acc_val *= PRIME32_1;
        acc[i] = acc_val;
    }
}

/* Processes a full block. */
static void XXH3_accumulate(uint64_t *const acc, uint8_t const *const input,
                            uint32_t const *key, size_t const nb_stripes)
{
    size_t n;
    for (n = 0; n < nb_stripes; n++) {
        XXH3_accumulate_512(acc, input + n * STRIPE_LEN, key);
        key += 2;
    }
}

/* Controls the long hash function. This is used for both XXH3_64 and XXH3_128. */
static void XXH3_hashLong(uint64_t *const acc, uint8_t const *const input, size_t const length)
{
    #define NB_KEYS (KEYSET_DEFAULT_SIZE - STRIPE_ELTS) / 2

    size_t const block_len = STRIPE_LEN * NB_KEYS;
    size_t const nb_blocks = length / block_len;
    size_t const nb_stripes = (length % block_len) / STRIPE_LEN;
    size_t n;

    for (n = 0; n < nb_blocks; n++) {
        XXH3_accumulate(acc, input + n * block_len, kKey, NB_KEYS);
        XXH3_scrambleAcc(acc, kKey + (KEYSET_DEFAULT_SIZE - STRIPE_ELTS));
    }

    /* last partial block */
    XXH3_accumulate(acc, input + nb_blocks * block_len, kKey, nb_stripes);

    /* last stripe */
    if ((length & (STRIPE_LEN - 1)) != 0) {
        XXH3_accumulate_512(acc, input + length - STRIPE_LEN, kKey + nb_stripes * 2);
    }
}

/* Mixes 16 bytes */
static uint64_t XXH3_mix16B(uint8_t const *const input, uint32_t const *const key)
{
    return XXH3_mul128_fold64(
               XXH_read64(input) ^ XXH3_readKey64(key),
               XXH_read64(input + 8) ^ XXH3_readKey64(key + 2) );
}

/* Combines two accumulators with two keys */
static uint64_t XXH3_mix2Accs(uint64_t const *const acc, uint32_t const *const key)
{
    return XXH3_mul128_fold64(
               acc[0] ^ XXH3_readKey64(key),
               acc[1] ^ XXH3_readKey64(key + 2) );
}

/* Combines 8 accumulators with keys into 1 finalized 64-bit hash. */
static XXH64_hash_t XXH3_mergeAccs(uint64_t const *const acc, uint32_t const *const key,
                                   uint64_t const start)
{
    uint64_t result64 = start;

    result64 += XXH3_mix2Accs(acc + 0, key + 0);
    result64 += XXH3_mix2Accs(acc + 2, key + 4);
    result64 += XXH3_mix2Accs(acc + 4, key + 8);
    result64 += XXH3_mix2Accs(acc + 6, key + 12);

    return XXH3_avalanche(result64);
}

/* Hashes a long input, > 128 bytes */
static XXH64_hash_t XXH3_hashLong_64b(uint8_t const *const input, size_t const length,
                                      XXH64_hash_t const seed)
{
    ALIGN(64) uint64_t acc[ACC_NB] = { 0, PRIME64_1, PRIME64_2, PRIME64_3, PRIME64_4, PRIME64_5, 0, 0 };
    acc[0] = seed;
    acc[6] = 0ULL - seed;
    XXH3_hashLong(acc, input, length);

    /* converge into final hash */
    return XXH3_mergeAccs(acc, kKey, (uint64_t) length * PRIME64_1);
}

/* The XXH3_64 seeded hash function.
 * input: The data to hash.
 * length:  The length of input. It is undefined behavior to have length larger than the
 *          capacity of input.
 * seed:    A 64-bit value to seed the hash with.
 * returns: The 64-bit calculated hash value. */
XXH64_hash_t XXH3_64bits_withSeed(void const *const input, size_t const length,
                                  XXH64_hash_t const seed)
{

    uint8_t const *const p = (uint8_t const *) input;
    uint64_t acc = PRIME64_1 * (length + seed);

    if (length <= 16) {
        return XXH3_len_0to16_64b(p, length, seed);
    }
    if (length > 128) {
        return XXH3_hashLong_64b(p, length, seed);
    }

    if (length > 96) {
        acc += XXH3_mix16B(p + 48, kKey + 24);
        acc += XXH3_mix16B(p + length - 64, kKey + 28);
    }
    if (length > 64) {
        acc += XXH3_mix16B(p + 32, kKey + 16);
        acc += XXH3_mix16B(p + length - 48, kKey + 20);
    }
    if (length > 32) {
        acc += XXH3_mix16B(p + 16, kKey + 8);
        acc += XXH3_mix16B(p + length - 32, kKey + 12);
    }
    acc += XXH3_mix16B(p + 0, kKey + 0);
    acc += XXH3_mix16B(p + length - 16, kKey + 4);

    return XXH3_avalanche(acc);
}

/* The XXH3_64 non-seeded hash function.
 * input: The data to hash.
 * length:  The length of input. It is undefined behavior to have length larger than the
 *          capacity of input.
 * returns: The 64-bit calculated hash value. */
XXH64_hash_t XXH3_64bits(void const *const input, size_t const length)
{
    return XXH3_64bits_withSeed(input, length, 0);
}


/* ==========================================
 * XXH3 128 bits (=> XXH128)
 * ========================================== */

/* TODO: clean this up once XXH128 approaches completion. */

static XXH128_hash_t XXH3_len_1to3_128b(uint8_t  const *const input, size_t const length,
                                        uint32_t const *const key,  XXH64_hash_t const seed)
{
    uint8_t  const c1 = input[0];
    uint8_t  const c2 = input[length / 2];
    uint8_t  const c3 = input[length - 1];
    uint32_t const l1 = (uint32_t)  c1 + ((uint32_t) c2 << 8);
    uint32_t const l2 = (uint32_t) length + ((uint32_t) c3 << 2);
    uint64_t const ll11 = ((l1 + seed + key[0]) & 0xFFFFFFFF) * (uint64_t) (l2 + key[1]);
    uint64_t const ll12 = ((l2 - seed + key[3]) & 0xFFFFFFFF) * (uint64_t) (l1 + key[2]);
    XXH128_hash_t ret;

    ret.low64 = XXH3_avalanche(ll11);
    ret.high64 = XXH3_avalanche(ll12);
    return ret;
}


static XXH128_hash_t XXH3_len_4to8_128b(uint8_t  const *const input, size_t const length,
                                        uint32_t const *const key,  XXH64_hash_t const seed)
{
    uint64_t acc1 = PRIME64_1 * ((uint64_t) length + seed);
    uint64_t acc2 = PRIME64_2 * ((uint64_t) length - seed);
    uint32_t const l1 = XXH_read32(input);
    uint32_t const l2 = XXH_read32(input + length - 4);
    XXH128_hash_t ret;

    acc1 += (uint64_t) (l1 + key[0]) * (uint64_t) (l2 + key[1]);
    acc2 += (uint64_t) (l1 - key[2]) * (uint64_t) (l2 + key[3]);

    ret.low64 = XXH3_avalanche(acc1);
    ret.high64 = XXH3_avalanche(acc2);
    return ret;
}

static XXH128_hash_t XXH3_len_9to16_128b(uint8_t  const *const input, size_t const length,
                                         uint32_t const *const key,  XXH64_hash_t const seed)
{
    uint64_t acc1 = PRIME64_1 * ((uint64_t) length + seed);
    uint64_t acc2 = PRIME64_2 * ((uint64_t) length - seed);
    uint64_t const ll1 = XXH_read64(input);
    uint64_t const ll2 = XXH_read64(input + length - 8);
    XXH128_hash_t ret;

    acc1 += XXH3_mul128_fold64(ll1 + XXH3_readKey64(key + 0), ll2 + XXH3_readKey64(key + 2));
    acc2 += XXH3_mul128_fold64(ll1 + XXH3_readKey64(key + 4), ll2 + XXH3_readKey64(key + 6));

    ret.low64 = XXH3_avalanche(acc1);
    ret.high64 = XXH3_avalanche(acc2);
    return ret;
}

static XXH128_hash_t XXH3_len_0to16_128b(uint8_t const *const input, size_t const length,
                                         XXH64_hash_t const seed)
{
    XXH128_hash_t ret;

    if (length > 8)
        return XXH3_len_9to16_128b(input, length, kKey, seed);
    if (length >= 4)
        return XXH3_len_4to8_128b(input, length, kKey, seed);
    if (length != 0)
        return XXH3_len_1to3_128b(input, length, kKey, seed);

    ret.low64 = seed;
    ret.high64 = 0ULL - seed;
    return ret;
}

static XXH128_hash_t XXH3_hashLong_128b(uint8_t const *const input, size_t const length,
                                        XXH64_hash_t const seed)
{
    ALIGN(64) uint64_t acc[ACC_NB] = { 0, PRIME64_1, PRIME64_2, PRIME64_3, PRIME64_4, PRIME64_5, 0, 0 };
    acc[0] = seed;
    acc[6] = 0ULL - seed;
    XXH3_hashLong(acc, input, length);

    /* converge into final hash */
    {
        uint64_t const low64  = XXH3_mergeAccs(acc,      kKey,  (uint64_t)length      * PRIME64_1);
        uint64_t const high64 = XXH3_mergeAccs(acc, kKey + 16, ((uint64_t)length + 1) * PRIME64_2);
        XXH128_hash_t ret;
        ret.low64 = low64;
        ret.high64 = high64;
        return ret;
    }
}

XXH128_hash_t XXH3_128bits_withSeed(void const *const input, size_t const length,
                                    XXH64_hash_t const seed)
{
    uint8_t const *const p = (uint8_t const*)input;

    uint64_t acc1 = PRIME64_1 * (length + seed);
    uint64_t acc2 = 0;

    if (length <= 16)
        return XXH3_len_0to16_128b(p, length, seed);

    if (length > 128)
        return XXH3_hashLong_128b(p, length, seed);

    /* TODO: reroll */
    if (length > 96) {
        acc1 += XXH3_mix16B(p + 48, kKey + 24);
        acc2 += XXH3_mix16B(p + length - 64, kKey + 28);
    }
    if (length > 64) {
        acc1 += XXH3_mix16B(p + 32, kKey + 16);
        acc2 += XXH3_mix16B(p + length - 48, kKey + 20);
    }
    if (length > 32) {
        acc1 += XXH3_mix16B(p + 16, kKey + 8);
        acc2 += XXH3_mix16B(p + length - 32, kKey + 12);
    }
    acc1 += XXH3_mix16B(p + 0, kKey + 0);
    acc2 += XXH3_mix16B(p + length - 16, kKey + 4);

    {
        uint64_t const part1 = acc1 + acc2;
        uint64_t const part2 = (acc1 * PRIME64_3) + (acc2 * PRIME64_4) + ((length - seed) * PRIME64_2);
        XXH128_hash_t ret;
        ret.low64 = XXH3_avalanche(part1);
        ret.high64 = 0ULL - XXH3_avalanche(part2);
        return ret;
    }
}


XXH128_hash_t XXH3_128bits(void const *const input, size_t const length)
{
    return XXH3_128bits_withSeed(input, length, 0);
}

XXH128_hash_t XXH128(void const *const input, size_t const length, XXH64_hash_t const seed)
{
    return XXH3_128bits_withSeed(input, length, seed);
}

