/*
 *  xxHash - Fast Hash algorithm
 *  Copyright (C) 2012-2019, Yann Collet
 *  Copyright (C) 2019, easyaspi314 (Devin)
 *
 *  BSD 2-Clause License (http://www.opensource.org/licenses/bsd-license.php)
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *
 *  * Redistributions of source code must retain the above copyright
 *  notice, this list of conditions and the following disclaimer.
 *  * Redistributions in binary form must reproduce the above
 *  copyright notice, this list of conditions and the following disclaimer
 *  in the documentation and/or other materials provided with the
 *  distribution.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 *  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 *  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 *  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 *  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *  You can contact the author at :
 *  - xxHash homepage: http://www.xxhash.com
 *  - xxHash source repository : https://github.com/Cyan4973/xxHash */

/* This is a compact, 100% standalone reference XXH64 streaming implementation.
 * Instead of focusing on performance hacks, this focuses on cleanliness,
 * conformance, portability and simplicity.
 *
 * This file aims to be 100% compatible with C90/C++98, with the additional
 * requirement of stdint.h and long long. Unlike the single-run implementation,
 * this uses malloc, free, memset, and memcpy. */

#include <stddef.h> /* size_t, NULL */
#include <stdlib.h> /* malloc, free */
#include <stdint.h> /* uint8_t, uint32_t, uint64_t */
#include <string.h> /* memset, memcpy */

typedef enum {
    FALSE, TRUE
} XXH_bool;
#ifdef __cplusplus
extern "C" {
#endif

/* Note: This is an opaque structure. The layout differs from the official implementation,
 * it is more compact and orders are different. */
typedef struct XXH64_state_s {
   uint64_t lane1;             /* Our lanes. */
   uint64_t lane2;
   uint64_t lane3;
   uint64_t lane4;
   uint8_t  temp_buffer[32];   /* Leftover data from a previous update */
   uint32_t temp_buffer_size;  /* how much data is in the temp buffer */
   XXH_bool has_large_len;     /* Whether we had enough to do full rounds. */
   uint64_t total_len_64;      /* The length of the data truncated to 32 bits. */
} XXH64_state_t;

typedef enum {
    XXH_OK = 0,
    XXH_ERROR = 1
} XXH_errorcode;

/*======   Streaming   ======*/
XXH64_state_t *XXH64_createState(void);
XXH_errorcode XXH64_freeState(XXH64_state_t *const state);
void XXH64_copyState(XXH64_state_t *const dest, XXH64_state_t const *const src);

XXH_errorcode XXH64_reset(XXH64_state_t *const statePtr, uint64_t const seed);
XXH_errorcode XXH64_update(XXH64_state_t *const statePtr, void const *const input, size_t const length);
uint64_t XXH64_digest(XXH64_state_t const *const statePtr);

#ifdef __cplusplus
}
#endif

static uint64_t const PRIME64_1 = 0x9E3779B185EBCA87ULL;   /* 0b1001111000110111011110011011000110000101111010111100101010000111 */
static uint64_t const PRIME64_2 = 0xC2B2AE3D27D4EB4FULL;   /* 0b1100001010110010101011100011110100100111110101001110101101001111 */
static uint64_t const PRIME64_3 = 0x165667B19E3779F9ULL;   /* 0b0001011001010110011001111011000110011110001101110111100111111001 */
static uint64_t const PRIME64_4 = 0x85EBCA77C2B2AE63ULL;   /* 0b1000010111101011110010100111011111000010101100101010111001100011 */
static uint64_t const PRIME64_5 = 0x27D4EB2F165667C5ULL;   /* 0b0010011111010100111010110010111100010110010101100110011111000101 */

/* Rotates value left by amount. */
static uint64_t XXH_rotl64(uint64_t const value, uint32_t const amount)
{
    return (value << amount) | (value >> (64 - amount));
}

/* Portably reads a 32-bit little endian integer from data at the given offset. */
static uint32_t XXH_read32(uint8_t const *const data, size_t const offset)
{
    return (uint32_t) data[offset + 0]
        | ((uint32_t) data[offset + 1] << 8)
        | ((uint32_t) data[offset + 2] << 16)
        | ((uint32_t) data[offset + 3] << 24);
}

/* Portably reads a 64-bit little endian integer from data at the given offset. */
static uint64_t XXH_read64(uint8_t const *const data, size_t const offset)
{
    return (uint64_t) data[offset + 0]
        | ((uint64_t) data[offset + 1] <<  8)
        | ((uint64_t) data[offset + 2] << 16)
        | ((uint64_t) data[offset + 3] << 24)
        | ((uint64_t) data[offset + 4] << 32)
        | ((uint64_t) data[offset + 5] << 40)
        | ((uint64_t) data[offset + 6] << 48)
        | ((uint64_t) data[offset + 7] << 56);
}

/* Mixes input into lane. */
static uint64_t XXH64_round(uint64_t lane, uint64_t const input)
{
    lane += input * PRIME64_2;
    lane  = XXH_rotl64(lane, 31);
    lane *= PRIME64_1;
    return lane;
}

/* Merges lane into hash to finalize */
static uint64_t XXH64_mergeRound(uint64_t hash, uint64_t const lane)
{
    hash ^= XXH64_round(0, lane);
    hash *= PRIME64_1;
    hash += PRIME64_4;
    return hash;
}

/* Mixes all bits to finalize the hash. */
static uint64_t XXH64_avalanche(uint64_t hash)
{
    hash ^= hash >> 33;
    hash *= PRIME64_2;
    hash ^= hash >> 29;
    hash *= PRIME64_3;
    hash ^= hash >> 32;
    return hash;
}

/* Dynamically allocates XXH64_state_t. It is expected to free this with
 * XXH64_freeState.
 * returns: A pointer to an XXH64_state_t. This may be NULL. */
XXH64_state_t *XXH64_createState(void)
{
    return (XXH64_state_t *) malloc(sizeof(XXH64_state_t));
}

/* Frees an XXH64_state_t.
 * state:   The state to free.
 * returns: XXH_OK on success, XXH_ERROR on error. */
XXH_errorcode XXH64_freeState(XXH64_state_t *const state)
{
    free(state);
    return XXH_OK;
}


/* Copies one XXH64_state_t to another.
 * dest:  The state to copy to. It is undefined behavior for dest to overlap with
 *        src.
 * src:   The state to copy from. It is undefined behavior for src to overlap with
 *        dest. */
void XXH64_copyState(XXH64_state_t *const dest, XXH64_state_t const *const src)
{
    memcpy(dest, src, sizeof(XXH64_state_t));
}

/* Resets an XXH64_state_t.
 * state:   The state to reset.
 * seed:    The seed to use.
 * returns: XXH_OK on success, XXH_ERROR on error. */
XXH_errorcode XXH64_reset(XXH64_state_t *const state, uint64_t const seed)
{
    /* Don't write into a null pointer. The official implementation doesn't check
     * for this. */
    if (state == NULL) {
        return XXH_ERROR;
    }

    memset(state, 0, sizeof(XXH64_state_t));

    state->lane1 = seed + PRIME64_1 + PRIME64_2;
    state->lane2 = seed + PRIME64_2;
    state->lane3 = seed + 0;
    state->lane4 = seed - PRIME64_1;
    return XXH_OK;
}

/* The XXH64 hash function update loop.
 * state:   The current state. It is undefined behavior to overlap with input.
 * input:   The data to hash. It is undefined behavior to overlap with state.
 * length:  The length of input. It is undefined behavior to have length larger than the
 *          capacity of input.
 * returns: XXH_OK on success, XXH_ERROR on failure. */
XXH_errorcode XXH64_update(XXH64_state_t *const state, void const *const input, size_t const length)
{
    uint8_t const *const data = (uint8_t const *) input;
    size_t remaining;
    size_t offset = 0;

    /* Don't dereference a null pointer. The reference implementation notably doesn't
     * check for this by default. */
    if (state == NULL || input == NULL) {
        return XXH_ERROR;
    }

    state->total_len_64 += (uint64_t) length;

    if (state->has_large_len == FALSE && (length >= 32 || state->total_len_64 >= 32)) {
        state->has_large_len = TRUE;
    }

    if (state->temp_buffer_size + length < 32)  {
        /* We don't have a full buffer, so we just copy the data over and return. */
        memcpy(&state->temp_buffer[state->temp_buffer_size], input, length);
        state->temp_buffer_size += (uint64_t) length;
        return XXH_OK;
    }

    remaining = state->temp_buffer_size + length;

     while (remaining >= 32) {
        /* fill up our temp buffer */
        memcpy(&state->temp_buffer[state->temp_buffer_size], &data[offset], 32 - state->temp_buffer_size);

        /* do our rounds */
        state->lane1 = XXH64_round(state->lane1, XXH_read64(state->temp_buffer, 0));
        state->lane2 = XXH64_round(state->lane2, XXH_read64(state->temp_buffer, 8));
        state->lane3 = XXH64_round(state->lane3, XXH_read64(state->temp_buffer, 16));
        state->lane4 = XXH64_round(state->lane4, XXH_read64(state->temp_buffer, 24));

        /* done with the rounds */
        state->temp_buffer_size = 0;
        remaining -= 32;
        offset += 32;
    }

    if (remaining != 0) {
        memcpy(state->temp_buffer, &data[offset], remaining);
        state->temp_buffer_size = (uint32_t) remaining;
    }

    return XXH_OK;
}

/* Finalizes an XXH64_state_t and returns the seed.
 * state:   The state to finalize. This is not modified.
 * returns: The calculated 64-bit hash. */
uint64_t XXH64_digest(XXH64_state_t const *const state)
{
    uint64_t hash;
    uint64_t remaining = state->temp_buffer_size;
    uint64_t offset = 0;

    if (state->has_large_len == TRUE) {
        hash = XXH_rotl64(state->lane1, 1)
             + XXH_rotl64(state->lane2, 7)
             + XXH_rotl64(state->lane3, 12)
             + XXH_rotl64(state->lane4, 18);

        hash = XXH64_mergeRound(hash, state->lane1);
        hash = XXH64_mergeRound(hash, state->lane2);
        hash = XXH64_mergeRound(hash, state->lane3);
        hash = XXH64_mergeRound(hash, state->lane4);
    } else {
        /* Not enough data for the main loop, put something in there instead. */
        hash = state->lane3 /* will be seed because of the + 0 */ + PRIME64_5;
    }

    hash += state->total_len_64;

    /* Process the remaining data. */
    while (remaining >= 8) {
        hash ^= XXH64_round(0, XXH_read64(state->temp_buffer, offset));
        hash  = XXH_rotl64(hash, 27);
        hash *= PRIME64_1;
        hash += PRIME64_4;
        offset += 8;
        remaining -= 8;
    }

    if (remaining >= 4) {
        hash ^= (uint64_t) XXH_read32(state->temp_buffer, offset) * PRIME64_1;
        hash  = XXH_rotl64(hash, 23);
        hash *= PRIME64_2;
        hash += PRIME64_3;
        offset += 4;
        remaining -= 4;
    }

    while (remaining != 0) {
        hash ^= (uint64_t) state->temp_buffer[offset] * PRIME64_5;
        hash  = XXH_rotl64(hash, 11);
        hash *= PRIME64_1;
        ++offset;
        --remaining;
    }

    return XXH64_avalanche(hash);
}

#ifdef XXH_SELFTEST
#include <stdio.h>  /* fprintf, puts */
#include <stdlib.h> /* exit */

#define TEST_DATA_SIZE 101
static int test_num = 0;
static uint32_t const PRIME32_1 = 0x9E3779B1U; /* 0b10011110001101110111100110110001 */

/* Checks a hash value. */
static void test_sequence(const uint8_t *const test_data, size_t const length,
                          uint64_t const seed, uint64_t const expected)
{
    XXH64_state_t *state = XXH64_createState();
    uint64_t result;
    size_t i;

    XXH64_reset(state, seed);
    XXH64_update(state, test_data, length);
    result = XXH64_digest(state);

    if (result != expected) {
        fprintf(stderr, "Error: Test %i: XXH64 test failed!\n", ++test_num);
        fprintf(stderr, "\rExpected value: 0x%08X%08X. Actual value: 0x%08X%08X.\n",
                (uint32_t) (expected >> 32), (uint32_t) expected, (uint32_t) (result >> 32), (uint32_t) result);
        exit(1);
    }

    XXH64_reset(state, seed);
    for (i = 0; i < length; i++) {
        XXH64_update(state, &test_data[i], 1);
    }
    result = XXH64_digest(state);

    if (result != expected) {
        fprintf(stderr, "Error: Test %i: XXH64 test failed!\n", ++test_num);
        fprintf(stderr, "\rExpected value: 0x%08X%08X. Actual value: 0x%08X%08X.\n",
                (uint32_t) (expected >> 32), (uint32_t) expected, (uint32_t) (result >> 32), (uint32_t) result);
        exit(1);
    }
    XXH64_freeState(state);
}

int main(void)
{
    const uint32_t prime = PRIME32_1;
    uint8_t test_data[TEST_DATA_SIZE] = {0};
    uint32_t byte_gen = prime;
    int i = 0;

    /* Fill the test_data buffer with "random" data */
    for (; i < TEST_DATA_SIZE; i++) {
        test_data[i] = (uint8_t) (byte_gen >> 24);
        byte_gen *= byte_gen;
    }

    test_sequence(NULL     ,  0            , 0    , 0xEF46DB3751D8E999ULL);
    test_sequence(NULL     ,  0            , prime, 0xAC75FDA2929B17EFULL);
    test_sequence(test_data,  1            , 0    , 0x4FCE394CC88952D8ULL);
    test_sequence(test_data,  1            , prime, 0x739840CB819FA723ULL);
    test_sequence(test_data, 14            , 0    , 0xCFFA8DB881BC3A3DULL);
    test_sequence(test_data, 14            , prime, 0x5B9611585EFCC9CBULL);
    test_sequence(test_data, TEST_DATA_SIZE, 0    , 0x0EAB543384F878ADULL);
    test_sequence(test_data, TEST_DATA_SIZE, prime, 0xCAA65939306F1E21ULL);

    puts("XXH64 reference implementation: OK");

    return 0;
}

#endif /* XXH_SELFTEST */

