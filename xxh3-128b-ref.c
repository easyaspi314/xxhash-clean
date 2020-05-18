/*
 * xxHash - Extremely Fast Hash algorithm
 * Copyright (C) 2019-2020 Yann Collet
 * Copyright (C) 2019-2020 Devin Hussey (easyaspi314)
 *
 * BSD 2-Clause License (https://www.opensource.org/licenses/bsd-license.php)
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *    * Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    * Redistributions in binary form must reproduce the above
 *      copyright notice, this list of conditions and the following disclaimer
 *      in the documentation and/or other materials provided with the
 *      distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * You can contact the author at:
 *   - xxHash homepage: https://www.xxhash.com
 *   - xxHash source repository: https://github.com/Cyan4973/xxHash
 */

/* A compact, 100% standalone reference XXH3_128bits implementation that focuses on clarity.
 * correctness, and portability, instead of dirty speed hacks.
 *
 * This file aims to be 100% compatible with C90 and C++ with the additional requirement of
 * stdint.h and long long.
 *
 * There is no self test at the moment as the official test values have not been updated
 * for the new algorithm. */

#include <stddef.h>   /* size_t */
#include <stdint.h>   /* uint8_t, uint32_t, uint64_t */
#include <string.h>   /* memcpy */

typedef uint64_t XXH64_hash_t;
typedef struct {
    XXH64_hash_t low64;
    XXH64_hash_t high64;
} XXH128_hash_t;

#ifdef __cplusplus
extern "C" {
#endif
#define XXH3_SECRET_SIZE_MIN 136

XXH128_hash_t XXH3_128bits(void const *const input, size_t const length);
XXH128_hash_t XXH3_128bits_withSeed(void const *const input, size_t const length, XXH64_hash_t const seed);
XXH128_hash_t XXH3_128bits_withSecret(void const *const input, size_t const length, void const *const secret, size_t secret_size);
XXH128_hash_t XXH128(void const *const input, size_t const length, XXH64_hash_t const seed);

#ifdef __cplusplus
}
#endif

static uint32_t const PRIME32_1 = 0x9E3779B1U;   /* 0b10011110001101110111100110110001 */
static uint32_t const PRIME32_2 = 0x85EBCA77U;   /* 0b10000101111010111100101001110111 */
static uint32_t const PRIME32_3 = 0xC2B2AE3DU;   /* 0b11000010101100101010111000111101 */

static uint64_t const PRIME64_1 = 0x9E3779B185EBCA87ULL;   /* 0b1001111000110111011110011011000110000101111010111100101010000111 */
static uint64_t const PRIME64_2 = 0xC2B2AE3D27D4EB4FULL;   /* 0b1100001010110010101011100011110100100111110101001110101101001111 */
static uint64_t const PRIME64_3 = 0x165667B19E3779F9ULL;   /* 0b0001011001010110011001111011000110011110001101110111100111111001 */
static uint64_t const PRIME64_4 = 0x85EBCA77C2B2AE63ULL;   /* 0b1000010111101011110010100111011111000010101100101010111001100011 */
static uint64_t const PRIME64_5 = 0x27D4EB2F165667C5ULL;   /* 0b0010011111010100111010110010111100010110010101100110011111000101 */

/* Portably reads a 32-bit little endian integer from p. */
static uint32_t XXH_read32(uint8_t const *const p)
{
#if defined(_WIN32) || (defined(__BYTE_ORDER__) && __BYTE_ORDER__==__ORDER_LITTLE_ENDIAN__)
    uint32_t ret;
    memcpy(&ret, p, sizeof(uint32_t));
    return ret;
#else
    return (uint32_t)p[0]
        | ((uint32_t)p[1] << 8)
        | ((uint32_t)p[2] << 16)
        | ((uint32_t)p[3] << 24);
#endif
}

/* Portably reads a 64-bit little endian integer from p. */
static uint64_t XXH_read64(uint8_t const *const p)
{
#if defined(_WIN32) || (defined(__BYTE_ORDER__) && __BYTE_ORDER__==__ORDER_LITTLE_ENDIAN__)
    uint64_t ret;
    memcpy(&ret, p, sizeof(uint64_t));
    return ret;
#else
    return (uint64_t)p[0]
        | ((uint64_t)p[1] << 8)
        | ((uint64_t)p[2] << 16)
        | ((uint64_t)p[3] << 24)
        | ((uint64_t)p[4] << 32)
        | ((uint64_t)p[5] << 40)
        | ((uint64_t)p[6] << 48)
        | ((uint64_t)p[7] << 56);
#endif
}

/* Portably writes a 64-bit little endian integer to p. */
static void XXH_write64(uint8_t *const p, uint64_t val)
{
#if defined(_WIN32) || (defined(__BYTE_ORDER__) && __BYTE_ORDER__==__ORDER_LITTLE_ENDIAN__)
    memcpy(p, &val, sizeof(uint64_t));
#else
    p[0] = (uint8_t)(val >> 0);
    p[1] = (uint8_t)(val >> 8);
    p[2] = (uint8_t)(val >> 16);
    p[3] = (uint8_t)(val >> 24);
    p[4] = (uint8_t)(val >> 32);
    p[5] = (uint8_t)(val >> 40);
    p[6] = (uint8_t)(val >> 48);
    p[7] = (uint8_t)(val >> 56);
#endif
}

/* 32-bit byteswap */
static uint32_t XXH_swap32(uint32_t const x)
{
    return ((x << 24) & 0xff000000)
         | ((x <<  8) & 0x00ff0000)
         | ((x >>  8) & 0x0000ff00)
         | ((x >> 24) & 0x000000ff);
}

/* 32-bit byteswap */
static uint64_t XXH_swap64(uint64_t const x)
{
    return ((x << 56) & 0xff00000000000000ULL)
         | ((x << 40) & 0x00ff000000000000ULL)
         | ((x << 24) & 0x0000ff0000000000ULL)
         | ((x << 8)  & 0x000000ff00000000ULL)
         | ((x >> 8)  & 0x00000000ff000000ULL)
         | ((x >> 24) & 0x0000000000ff0000ULL)
         | ((x >> 40) & 0x000000000000ff00ULL)
         | ((x >> 56) & 0x00000000000000ffULL);
}

static uint32_t XXH_rotl32(uint32_t const val, unsigned const amt)
{
    return (val << (amt % 32)) | (val >> (32 - amt % 32));
}

#define XXH_SECRET_DEFAULT_SIZE 192   /* minimum XXH3_SECRET_SIZE_MIN */

static uint8_t const kSecret[XXH_SECRET_DEFAULT_SIZE] = {
    0xb8, 0xfe, 0x6c, 0x39, 0x23, 0xa4, 0x4b, 0xbe, 0x7c, 0x01, 0x81, 0x2c, 0xf7, 0x21, 0xad, 0x1c,
    0xde, 0xd4, 0x6d, 0xe9, 0x83, 0x90, 0x97, 0xdb, 0x72, 0x40, 0xa4, 0xa4, 0xb7, 0xb3, 0x67, 0x1f,
    0xcb, 0x79, 0xe6, 0x4e, 0xcc, 0xc0, 0xe5, 0x78, 0x82, 0x5a, 0xd0, 0x7d, 0xcc, 0xff, 0x72, 0x21,
    0xb8, 0x08, 0x46, 0x74, 0xf7, 0x43, 0x24, 0x8e, 0xe0, 0x35, 0x90, 0xe6, 0x81, 0x3a, 0x26, 0x4c,
    0x3c, 0x28, 0x52, 0xbb, 0x91, 0xc3, 0x00, 0xcb, 0x88, 0xd0, 0x65, 0x8b, 0x1b, 0x53, 0x2e, 0xa3,
    0x71, 0x64, 0x48, 0x97, 0xa2, 0x0d, 0xf9, 0x4e, 0x38, 0x19, 0xef, 0x46, 0xa9, 0xde, 0xac, 0xd8,
    0xa8, 0xfa, 0x76, 0x3f, 0xe3, 0x9c, 0x34, 0x3f, 0xf9, 0xdc, 0xbb, 0xc7, 0xc7, 0x0b, 0x4f, 0x1d,
    0x8a, 0x51, 0xe0, 0x4b, 0xcd, 0xb4, 0x59, 0x31, 0xc8, 0x9f, 0x7e, 0xc9, 0xd9, 0x78, 0x73, 0x64,

    0xea, 0xc5, 0xac, 0x83, 0x34, 0xd3, 0xeb, 0xc3, 0xc5, 0x81, 0xa0, 0xff, 0xfa, 0x13, 0x63, 0xeb,
    0x17, 0x0d, 0xdd, 0x51, 0xb7, 0xf0, 0xda, 0x49, 0xd3, 0x16, 0x55, 0x26, 0x29, 0xd4, 0x68, 0x9e,
    0x2b, 0x16, 0xbe, 0x58, 0x7d, 0x47, 0xa1, 0xfc, 0x8f, 0xf8, 0xb8, 0xd1, 0x7a, 0xd0, 0x31, 0xce,
    0x45, 0xcb, 0x3a, 0x8f, 0x95, 0x16, 0x04, 0x28, 0xaf, 0xd7, 0xfb, 0xca, 0xbb, 0x4b, 0x40, 0x7e,
};

/* Calculates a 64-bit to 128-bit unsigned multiply */
static XXH128_hash_t XXH_mult64to128(uint64_t const lhs, uint64_t const rhs)
{
#if defined(__SIZEOF_INT128__) || (defined(_INTEGRAL_MAX_BITS) && _INTEGRAL_MAX_BITS >= 128)
    __uint128_t product = (__uint128_t) lhs * (__uint128_t) rhs;
    XXH128_hash_t ret;
    ret.low64 = (XXH64_hash_t)product;
    ret.high64 = (XXH64_hash_t)(product >> 64);
    return ret;

    /* There are other platform-specific versions in the official repo.
     * They would all be left out in favor of the code above, but it is not
     * portable, so we keep the generic version. */

#else /* Portable scalar version */
    /* First calculate all of the cross products. */
    uint64_t const lo_lo = (lhs & 0xFFFFFFFF) * (rhs & 0xFFFFFFFF);
    uint64_t const hi_lo = (lhs >> 32)        * (rhs & 0xFFFFFFFF);
    uint64_t const lo_hi = (lhs & 0xFFFFFFFF) * (rhs >> 32);
    uint64_t const hi_hi = (lhs >> 32)        * (rhs >> 32);

    /* Now add the products together. These will never overflow. */
    uint64_t const cross = (lo_lo >> 32) + (hi_lo & 0xFFFFFFFF) + lo_hi;
    uint64_t const upper = (hi_lo >> 32) + (cross >> 32)        + hi_hi;
    uint64_t const lower = (cross << 32) | (lo_lo & 0xFFFFFFFF);

    XXH128_hash_t ret;
    ret.low64 = lower;
    ret.high64 = upper;
    return ret;
#endif
}

/* Calculates a 64-bit to 128-bit unsigned multiply, then xor's the low bits of the product with
 * the high bits for a 64-bit result. */
static uint64_t XXH3_mul128_fold64(uint64_t const lhs, uint64_t const rhs)
{
    XXH128_hash_t product = XXH_mult64to128(lhs, rhs);
    return product.low64 ^ product.high64;
}

#define STRIPE_LEN 64
#define XXH_SECRET_CONSUME_RATE 8   /* nb of secret bytes consumed at each accumulation */
#define ACC_NB (STRIPE_LEN / sizeof(uint64_t))

/* Mixes up the hash to finalize */
static XXH64_hash_t XXH3_avalanche(uint64_t hash)
{
    hash ^= hash >> 37;
    hash *= 0x165667919E3779F9ULL;
    hash ^= hash >> 32;
    return hash;
}

/* ==========================================
 * Short keys
 * ========================================== */

/* Hashes zero-length keys */
static XXH128_hash_t XXH3_len_0_128b(uint8_t const *const secret, XXH64_hash_t const seed)
{
    XXH128_hash_t acc;
    acc.low64   = PRIME64_1 + seed;
    acc.low64  ^= XXH_read64(secret + 64);
    acc.low64  ^= XXH_read64(secret + 72);
    acc.low64   = XXH3_avalanche(acc.low64);

    acc.high64  = PRIME64_2 - seed;
    acc.high64 ^= XXH_read64(secret + 80);
    acc.high64 ^= XXH_read64(secret + 88);
    acc.high64  = XXH3_avalanche(acc.high64);

    return acc;
}

/* Hashes short keys from 1 to 3 bytes. */
static XXH128_hash_t XXH3_len_1to3_128b(uint8_t const *const input,
                                        size_t  const length,
                                        uint8_t const *const secret,
                                        XXH64_hash_t const seed)
{
    uint8_t  const byte1 = input[0];
    uint8_t  const byte2 = (length > 1) ? input[1] : input[0];
    uint8_t  const byte3 = input[length - 1];

    uint32_t const combined_lo = ((uint32_t)byte1  << 16)
                               | ((uint32_t)byte2  << 24)
                               | ((uint32_t)byte3  <<  0)
                               | ((uint32_t)length <<  8);
    uint32_t const swapped = XXH_swap32(combined_lo);
    uint32_t const combined_hi = XXH_rotl32(swapped, 13);
    XXH128_hash_t acc;
    acc.low64   = (uint64_t)(XXH_read32(secret)     ^ XXH_read32(secret + 4));
    acc.low64  += seed;
    acc.low64  ^= (uint64_t)combined_lo;
    acc.low64  *= PRIME64_1;
    acc.low64   = XXH3_avalanche(acc.low64);

    acc.high64  = (uint64_t)(XXH_read32(secret + 8) ^ XXH_read32(secret + 12));
    acc.high64 -= seed;
    acc.high64 ^= (uint64_t)combined_hi;
    acc.high64 *= PRIME64_5;
    acc.high64  = XXH3_avalanche(acc.high64);

    return acc;
}

/* Hashes short keys from 4 to 8 bytes. */
static XXH128_hash_t XXH3_len_4to8_128b(uint8_t const *const input,
                                        size_t  const length,
                                        uint8_t const *const secret,
                                        XXH64_hash_t seed)
{
    uint32_t const input_lo = XXH_read32(input);
    uint32_t const input_hi = XXH_read32(input + length - 4);
    uint64_t const input_64 = (uint64_t)input_lo | ((uint64_t)input_hi << 32);
    uint64_t acc = XXH_read64(secret + 16) ^ XXH_read64(secret + 24);
    XXH128_hash_t m128;
    seed ^= (uint64_t)XXH_swap32(seed & 0xFFFFFFFF) << 32;
    acc += seed;
    acc ^= input_64;
    m128 = XXH_mult64to128(acc, PRIME64_1 + (length << 2));

    m128.high64 += (m128.low64  << 1);
    m128.low64  ^= (m128.high64 >> 3);

    m128.low64  ^= (m128.low64 >> 35);
    m128.low64  *= 0x9FB21C651E98DF25ULL;
    m128.low64  ^= (m128.low64 >> 28);

    m128.high64  = XXH3_avalanche(m128.high64);
    return m128;
}

/* Hashes short keys from 9 to 16 bytes. */
static XXH128_hash_t XXH3_len_9to16_128b(uint8_t const *const input,
                                         size_t  const length,
                                         uint8_t const *const secret,
                                         XXH64_hash_t const seed)
{
    uint64_t acc_lo   = XXH_read64(secret+32) ^ XXH_read64(secret+40);
    uint64_t acc_hi   = XXH_read64(secret+48) ^ XXH_read64(secret+56);
    uint64_t input_lo = XXH_read64(input);
    uint64_t input_hi = XXH_read64(input + length - 8);
    XXH128_hash_t m128, ret;
    acc_lo -= seed;
    acc_lo ^= input_lo;
    acc_lo ^= input_hi;

    acc_hi += seed;
    acc_hi ^= input_hi;

    m128 = XXH_mult64to128(acc_lo, PRIME64_1);
    m128.low64  += (uint64_t)(length - 1) << 54;
    m128.high64 += (acc_hi & 0xFFFFFFFF00000000) + ((acc_hi & 0xFFFFFFFF) * PRIME32_2);

    m128.low64  ^= XXH_swap64(m128.high64);

    /* m128 * PRIME64_2 */
    ret = XXH_mult64to128(m128.low64, PRIME64_2);
    ret.high64 += m128.high64 * PRIME64_2;

    ret.low64   = XXH3_avalanche(ret.low64);
    ret.high64  = XXH3_avalanche(ret.high64);
    return ret;
}

/* Hashes short keys that are less than or equal to 16 bytes. */
static XXH128_hash_t XXH3_len_0to16_128b(uint8_t const *const input,
                                         size_t const length,
                                         uint8_t const *const secret,
                                         XXH64_hash_t const seed)
{
    if (length > 8)
        return XXH3_len_9to16_128b(input, length, secret, seed);
    else if (length >= 4)
        return XXH3_len_4to8_128b(input, length, secret, seed);
    else if (length != 0)
        return XXH3_len_1to3_128b(input, length, secret, seed);
    return XXH3_len_0_128b(secret, seed);
}

/* A mixer for the midsize hashes */
static uint64_t XXH3_mix16B(uint8_t const *const input,
                            uint8_t const *const secret,
                            XXH64_hash_t seed)
{
    uint64_t lhs = seed;
    uint64_t rhs = 0U - seed;
    lhs += XXH_read64(secret);
    rhs += XXH_read64(secret + 8);
    lhs ^= XXH_read64(input);
    rhs ^= XXH_read64(input + 8);
    return XXH3_mul128_fold64(lhs, rhs);
}

/* The primary mixer for the midsize hashes */
static XXH128_hash_t XXH128_mix32B(XXH128_hash_t acc,
                                   uint8_t const *const input_1,
                                   uint8_t const *const input_2,
                                   uint8_t const *const secret,
                                   XXH64_hash_t seed)
{
    acc.low64  += XXH3_mix16B (input_1, secret+0, seed);
    acc.low64  ^= XXH_read64  (input_2) + XXH_read64(input_2 + 8);
    acc.high64 += XXH3_mix16B (input_2, secret+16, seed);
    acc.high64 ^= XXH_read64  (input_1) + XXH_read64(input_1 + 8);
    return acc;
}

/* Common avalanche code for midrange XXH128 hashes */
static XXH128_hash_t XXH128_midrange_avalanche(XXH128_hash_t acc,
                                               size_t const length,
                                               XXH64_hash_t const seed)
{
    XXH128_hash_t h128;
    h128.low64   = acc.low64 + acc.high64;
    h128.low64   = XXH3_avalanche(h128.low64);

    h128.high64  = acc.high64 * PRIME64_4;
    h128.high64 += acc.low64  * PRIME64_1;
    h128.high64 += ((uint64_t)length - seed) * PRIME64_2;
    h128.high64  = 0ull - XXH3_avalanche(h128.high64);

    return h128;
}

/* Hashes midsize keys from 17 to 128 bytes */
static XXH128_hash_t XXH3_len_17to128_128b(uint8_t const *const input,
                                           size_t const length,
                                           uint8_t const *const secret,
                                           XXH64_hash_t const seed)
{
    int i = (int)((length - 1) / 32);

    XXH128_hash_t acc;
    acc.low64 = length * PRIME64_1;
    acc.high64 = 0;

    for (; i >= 0; --i) {
        acc = XXH128_mix32B(acc,
                            input + (16 * i),
                            input + length - (16 * (i + 1)),
                            secret + (32 * i),
                            seed);
    }
    return XXH128_midrange_avalanche(acc, length, seed);
}


#define XXH3_MIDSIZE_MAX 240

/* Hashes midsize keys from 129 to 240 bytes */
static XXH128_hash_t XXH3_len_129to240_128b(uint8_t const *const input,
                                            size_t const length,
                                            uint8_t const *const secret,
                                            XXH64_hash_t const seed)
{

    #define XXH3_MIDSIZE_STARTOFFSET 3
    #define XXH3_MIDSIZE_LASTOFFSET  17

    XXH128_hash_t acc;
    int const nb_rounds = (int)length / 32;
    int i;
    acc.low64 = length * PRIME64_1;
    acc.high64 = 0;
    for (i = 0; i < 4; i++) {
        acc = XXH128_mix32B(acc,
                            input  + (32 * i),
                            input  + (32 * i) + 16,
                            secret + (32 * i),
                            seed);
    }
    acc.low64 = XXH3_avalanche(acc.low64);
    acc.high64 = XXH3_avalanche(acc.high64);
    for (i = 4 ; i < nb_rounds; i++) {
        acc = XXH128_mix32B(acc,
                            input + (32 * i),
                            input + (32 * i) + 16,
                            secret + XXH3_MIDSIZE_STARTOFFSET + (32 * (i - 4)),
                            seed);
    }
    /* last bytes */
    acc = XXH128_mix32B(acc,
                        input + length - 16,
                        input + length - 32,
                        secret + XXH3_SECRET_SIZE_MIN - XXH3_MIDSIZE_LASTOFFSET - 16,
                        0ULL - seed);
    return XXH128_midrange_avalanche(acc, length, seed);
}

/* Hashes a short input, < 240 bytes */
static XXH128_hash_t XXH3_hashShort_128b(uint8_t const *const input,
                                         size_t const length,
                                         uint8_t const *const secret,
                                         XXH64_hash_t const seed)
{
    if (length <= 16)
        return XXH3_len_0to16_128b(input, length, secret, seed);
    if (length <= 128)
        return XXH3_len_17to128_128b(input, length, secret, seed);
    return XXH3_len_129to240_128b(input, length, secret, seed);
}


/* This is the main loop. This is usually written in SIMD code. */
static void XXH3_accumulate_512_128b(uint64_t *const acc,
                                    uint8_t const *const input,
                                    uint8_t const *const secret)
{
    size_t i;
    for (i = 0; i < ACC_NB; i++) {
        uint64_t input_val = XXH_read64(input  + (8 * i));
        acc[i ^ 1] += input_val; /* swap even/odd lanes */
        input_val  ^= XXH_read64(secret + (8 * i));
        acc[i]     += (uint32_t)input_val * (input_val >> 32);
    }
}

/* Scrambles input. This is usually written in SIMD code, as it is usually part of the main loop. */
static void XXH3_scrambleAcc(uint64_t *const acc, uint8_t const *const secret)
{
    size_t i;
    for (i = 0; i < ACC_NB; i++) {
        acc[i] ^= acc[i] >> 47;
        acc[i] ^= XXH_read64(secret + (8 * i));
        acc[i] *= PRIME32_1;
    }
}

/* Processes a full block. */
static void XXH3_accumulate_128b(uint64_t *const acc,
                                 uint8_t const *const input,
                                 uint8_t const *const secret,
                                 size_t const nb_stripes)
{
    size_t n;
    for (n = 0; n < nb_stripes; n++) {
        XXH3_accumulate_512_128b(acc, input + n * STRIPE_LEN, secret + (8 * n));
    }
}

/* Combines two accumulators with two keys */
static uint64_t XXH3_mix2Accs(uint64_t const *const acc, uint8_t const *const secret)
{
    return XXH3_mul128_fold64(
               acc[0] ^ XXH_read64(secret),
               acc[1] ^ XXH_read64(secret + 8));
}

/* Combines 8 accumulators with keys into 1 finalized 64-bit hash. */
static XXH64_hash_t XXH3_mergeAccs(uint64_t const *const acc, uint8_t const *const key,
                                   uint64_t const start)
{
    uint64_t result64 = start;
    size_t i = 0;
    for (i = 0; i < 4; i++)
         result64 += XXH3_mix2Accs(acc + 2 * i, key + 16 * i);

    return XXH3_avalanche(result64);
}

/* Controls the long hash function. This is used for both XXH3_64 and XXH3_128. */
static XXH128_hash_t XXH3_hashLong_128b(uint8_t const *const input,
                                        size_t const length,
                                        uint8_t const *const secret,
                                        size_t const secret_size)
{
    size_t const nb_rounds = (secret_size - STRIPE_LEN) / XXH_SECRET_CONSUME_RATE;
    size_t const block_len = STRIPE_LEN * nb_rounds;
    size_t const nb_blocks = length / block_len;
    size_t const nb_stripes = (length - (block_len * nb_blocks)) / STRIPE_LEN;
    size_t n;
    uint64_t acc[ACC_NB];

    acc[0] = PRIME32_3;
    acc[1] = PRIME64_1;
    acc[2] = PRIME64_2;
    acc[3] = PRIME64_3;
    acc[4] = PRIME64_4;
    acc[5] = PRIME32_2;
    acc[6] = PRIME64_5;
    acc[7] = PRIME32_1;

    for (n = 0; n < nb_blocks; n++) {
        XXH3_accumulate_128b(acc, input + n * block_len, secret, nb_rounds);
        XXH3_scrambleAcc(acc, secret + secret_size - STRIPE_LEN);
    }

    /* last partial block */
    XXH3_accumulate_128b(acc, input + nb_blocks * block_len, secret, nb_stripes);

    /* last stripe */
    if (length % STRIPE_LEN != 0) {
        uint8_t const *const p = input + length - STRIPE_LEN;
        /* Do not align on 8, so that the secret is different from the scrambler */
#define XXH_SECRET_LASTACC_START 7
        XXH3_accumulate_512_128b(acc, p, secret + secret_size - STRIPE_LEN - XXH_SECRET_LASTACC_START);
    }

#define XXH_SECRET_MERGEACCS_START 11

    /* converge into final hash */
    {
        XXH128_hash_t h128;
        h128.low64 = XXH3_mergeAccs(acc,
                                        secret + XXH_SECRET_MERGEACCS_START,
                                        (uint64_t)length * PRIME64_1);
        h128.high64 = XXH3_mergeAccs(acc,
                                         secret + secret_size
                                                - sizeof(acc) - XXH_SECRET_MERGEACCS_START,
                                         ~((uint64_t)length * PRIME64_2));
        return h128;
    }
}

/* Hashes a long input, > 240 bytes */
static XXH128_hash_t XXH3_hashLong_128b_withSeed(uint8_t const *const input,
                                                 size_t const length,
                                                 XXH64_hash_t const seed)
{
    uint8_t secret[XXH_SECRET_DEFAULT_SIZE];
    size_t i;

    for (i = 0; i < XXH_SECRET_DEFAULT_SIZE / 16; i++) {
        XXH_write64(secret + (16 * i),     XXH_read64(kSecret + (16 * i))     + seed);
        XXH_write64(secret + (16 * i) + 8, XXH_read64(kSecret + (16 * i) + 8) - seed);
    }
    return XXH3_hashLong_128b(input, length, secret, sizeof(secret));
}

/* The XXH3_128 seeded hash function.
 * input: The data to hash.
 * length:  The length of input. It is undefined behavior to have length larger than the
 *          capacity of input.
 * seed:    A 64-bit value to seed the hash with.
 * returns: The 128-bit calculated hash value. */
XXH128_hash_t XXH3_128bits_withSeed(void const *const input, size_t const length, XXH64_hash_t const seed)
{
    if (length <= XXH3_MIDSIZE_MAX)
        return XXH3_hashShort_128b((uint8_t const *)input, length, kSecret, seed);
    return XXH3_hashLong_128b_withSeed((uint8_t const *)input, length, seed);
}

/* The XXH3_128 non-seeded hash function.
 * input: The data to hash.
 * length:  The length of input. It is undefined behavior to have length larger than the
 *          capacity of input.
 * returns: The 128-bit calculated hash value. */
XXH128_hash_t XXH3_128bits(void const *const input, size_t const length)
{
    return XXH3_128bits_withSeed(input, length, 0);
}

/* The XXH3_128 hash function with custom secrets.
 * input: The data to hash.
 * length:  The length of input. It is undefined behavior to have length larger than the
 *          capacity of input.
 * seed:    A 64-bit value to seed the hash with.
 * returns: The 128-bit calculated hash value. */
XXH128_hash_t XXH3_128bits_withSecret(void const *const input, size_t const length,
                                      void const *const secret, size_t const secret_size)
{
    if (length <= XXH3_MIDSIZE_MAX)
        return XXH3_hashShort_128b((uint8_t const *)input, length, (uint8_t const *)secret, 0);
    return XXH3_hashLong_128b((uint8_t const *)input, length, (uint8_t const *)secret, secret_size);
}

/* An alias for XXH3_128bits_withSeed. */
XXH128_hash_t XXH128(void const *const input, size_t const length, XXH64_hash_t const seed)
{
    return XXH3_128bits_withSeed(input, length, seed);
}

#ifdef XXH_SELFTEST
#include <stdio.h>  /* fprintf, puts */
#include <stdlib.h> /* exit */

#define TEST_DATA_SIZE 2243
static int test_num = 0;

static void verify(char const *const test_name, XXH128_hash_t const expected, XXH128_hash_t result)
{
    if (result.low64 != expected.low64 || result.high64 != expected.high64) {
        fprintf(stderr, "Error: Test %i: %s test failed!\n", ++test_num, test_name);
        fprintf(stderr, "Got { 0x%08X%08XULL, 0x%08X%08XULL }, expected { 0x%08X%08XULL, 0x%08X%08XULL } \n",
                (unsigned)(result.low64>>32), (unsigned)result.low64,
                (unsigned)(result.high64>>32), (unsigned)result.high64,
                (unsigned)(expected.low64>>32), (unsigned)expected.low64,
                (unsigned)(expected.high64>>32), (unsigned)expected.high64);
        exit(1);
    }
}
/* Checks a hash value. */
static void test_sequence(uint8_t const *const test_data, size_t const length,
                          uint64_t const seed, XXH128_hash_t const expected)
{
    XXH128_hash_t result = XXH3_128bits_withSeed(test_data, length, seed);
    verify("XXH3_128bits_withSeed", expected, result);
    if (seed == 0) {
        result = XXH3_128bits(test_data, length);
        verify("XXH3_128bits", expected, result);
    }
}


static const uint64_t PRIME32 = 0x9E3779B1U;
static const uint64_t PRIME64 = 0x9E3779B185EBCA8DULL;
int main(void)
{
    uint8_t test_data[TEST_DATA_SIZE];
    uint64_t byte_gen = PRIME32;
    size_t i = 0;

    /* Fill in the test_data buffer with "random" data. */
    for (; i < TEST_DATA_SIZE; ++i) {
        test_data[i] = (uint8_t) (byte_gen >> 56);
        byte_gen *= PRIME64;
    }

    /* xxhsum verification values */
    {   XXH128_hash_t const expected = { 0x1F17545BCE1061F1ULL, 0x07FD4E968E916AE1ULL };
        test_sequence(NULL,          0, 0,     expected);         /* empty string */
    }
    {   XXH128_hash_t const expected = { 0x7282E631387D51ACULL, 0x8743B0A8131AB9E6ULL };
        test_sequence(NULL,        0, PRIME32, expected);
    }
    {   XXH128_hash_t const expected = { 0xB936EBAE24CB01C5ULL, 0x2554B05763A71A05ULL };
        test_sequence(test_data,   1, 0,       expected);       /* 1-3 */
    }
    {   XXH128_hash_t const expected = { 0xCA57C628C04B45B8ULL, 0x916831F4DCD21CF9ULL };
        test_sequence(test_data,   1, PRIME32, expected);       /* 1-3 */
    }
    {   XXH128_hash_t const expected = { 0x3E7039BDDA43CFC6ULL, 0x082AFE0B8162D12AULL };
        test_sequence(test_data,   6, 0,       expected);       /* 4-8 */
    }
    {   XXH128_hash_t const expected = { 0x269D8F70BE98856EULL, 0x5A865B5389ABD2B1ULL };
        test_sequence(test_data,   6, PRIME32, expected);       /* 4-8 */
    }
    {   XXH128_hash_t const expected = { 0x061A192713F69AD9ULL, 0x6E3EFD8FC7802B18ULL };
        test_sequence(test_data,  12, 0,       expected);       /* 9-16 */
    }
    {   XXH128_hash_t const expected = { 0x9BE9F9A67F3C7DFBULL, 0xD7E09D518A3405D3ULL };
        test_sequence(test_data,  12, PRIME32, expected);       /* 9-16 */
    }
    {   XXH128_hash_t const expected = { 0x1E7044D28B1B901DULL, 0x0CE966E4678D3761ULL };
        test_sequence(test_data,  24, 0,       expected);       /* 17-32 */
    }
    {   XXH128_hash_t const expected = { 0xD7304C54EBAD40A9ULL, 0x3162026714A6A243ULL };
        test_sequence(test_data,  24, PRIME32, expected);       /* 17-32 */
    }
    {   XXH128_hash_t const expected = { 0xF942219AED80F67BULL, 0xA002AC4E5478227EULL };
        test_sequence(test_data,  48, 0,       expected);       /* 33-64 */
    }
    {   XXH128_hash_t const expected = { 0x7BA3C3E453A1934EULL, 0x163ADDE36C072295ULL };
        test_sequence(test_data,  48, PRIME32, expected);       /* 33-64 */
    }
    {   XXH128_hash_t const expected = { 0x5E8BAFB9F95FB803ULL, 0x4952F58181AB0042ULL };
        test_sequence(test_data,  81, 0,       expected);       /* 65-96 */
    }
    {   XXH128_hash_t const expected = { 0x703FBB3D7A5F755CULL, 0x2724EC7ADC750FB6ULL };
        test_sequence(test_data,  81, PRIME32, expected);       /* 65-96 */
    }
    {   XXH128_hash_t const expected = { 0xF1AEBD597CEC6B3AULL, 0x337E09641B948717ULL };
        test_sequence(test_data, 222, 0,       expected);       /* 129-240 */
    }
    {   XXH128_hash_t const expected = { 0xAE995BB8AF917A8DULL, 0x91820016621E97F1ULL };
        test_sequence(test_data, 222, PRIME32, expected);       /* 129-240 */
    }
    {   XXH128_hash_t const expected = { 0xCDEB804D65C6DEA4ULL, 0x1B6DE21E332DD73DULL };
        test_sequence(test_data, 403, 0,       expected);       /* one block, last stripe is overlapping */
    }
    {   XXH128_hash_t const expected = { 0x6259F6ECFD6443FDULL, 0xBED311971E0BE8F2ULL };
        test_sequence(test_data, 403, PRIME64, expected);       /* one block, last stripe is overlapping */
    }
    {   XXH128_hash_t const expected = { 0x1443B8153EBEE367ULL, 0x98EC7E48CD872997ULL };
        test_sequence(test_data, 512, 0,       expected);       /* one block, finishing at stripe boundary */
    }
    {   XXH128_hash_t const expected = { 0x43FDC6823A52F1F2ULL, 0x2F748A4F194E1EF0ULL };
        test_sequence(test_data, 512, PRIME64, expected);       /* one block, finishing at stripe boundary */
    }
    {   XXH128_hash_t const expected = { 0xF4258501BE8E0623ULL, 0x6930A2267A755B20ULL };
        test_sequence(test_data,2048, 0,       expected);       /* two blocks, finishing at block boundary */
    }
    {   XXH128_hash_t const expected = { 0x10CC56C2FA0AD9ACULL, 0xD0D7A3C2EEF2D892ULL };
        test_sequence(test_data,2048, PRIME32, expected);       /* two blocks, finishing at block boundary */
    }
    {   XXH128_hash_t const expected = { 0x5890AE7ACBB84A7EULL, 0x85C327B377AA7E62ULL };
        test_sequence(test_data,2240, 0,       expected);      /* two blocks, ends at stripe boundary */
    }
    {   XXH128_hash_t const expected = { 0x205E6D72DCCBD2AAULL, 0x62B70214DB075235ULL };
        test_sequence(test_data,2240, PRIME32, expected);       /* two blocks, ends at stripe boundary */
    }
    {   XXH128_hash_t const expected = { 0xF403CEA1763CD9CCULL, 0x0CDABF3F3C98B371ULL };
        test_sequence(test_data,2237, 0,       expected);       /* two blocks, last stripe is overlapping */
    }
    {   XXH128_hash_t const expected = { 0xF3824EE446018851ULL, 0xC81B751764BD53C5ULL };
        test_sequence(test_data,2237, PRIME32, expected);       /* two blocks, last stripe is overlapping */
    }

    puts("XXH3_128bits reference implementation: OK");

    return 0;
}
#endif /* XXH_SELFTEST */
