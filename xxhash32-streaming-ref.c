/*
 *  xxHash - Fast Hash algorithm
 *  Copyright (C) 2012-2020 Yann Collet
 *  Copyright (C) 2019-2020 Devin Hussey (easyaspi314)
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

/* This is a compact, 100% standalone reference XXH32 streaming implementation.
 * Instead of focusing on performance hacks, this focuses on cleanliness,
 * conformance, portability and simplicity.
 *
 * This file aims to be 100% compatible with C90/C++98, with the additional
 * requirement of stdint.h. Unlike the single-run implementation, this uses
 * malloc, free, memset, and memcpy. */

#include <stddef.h> /* size_t, NULL */
#include <stdlib.h> /* malloc, free */
#include <stdint.h> /* uint8_t, uint32_t */
#include <string.h> /* memset, memcpy */

typedef enum {
    FALSE, TRUE
} XXH_bool;
#ifdef __cplusplus
extern "C" {
#endif

/* Note: This is an opaque structure. The layout differs from the official implementation,
 * it is more compact and orders are different. */
typedef struct XXH32_state_s {
   uint32_t lane1;             /* Our lanes. */
   uint32_t lane2;
   uint32_t lane3;
   uint32_t lane4;
   uint8_t  temp_buffer[16];   /* Leftover data from a previous update */
   uint32_t temp_buffer_size;  /* how much data is in the temp buffer */
   XXH_bool has_large_len;     /* Whether we had enough to do full rounds. */
   uint32_t total_len_32;      /* The length of the data truncated to 32 bits. */
} XXH32_state_t;

typedef enum {
    XXH_OK = 0,
    XXH_ERROR = 1
} XXH_errorcode;

/*======   Streaming   ======*/
XXH32_state_t *XXH32_createState(void);
XXH_errorcode XXH32_freeState(XXH32_state_t *const state);
void XXH32_copyState(XXH32_state_t *const dest, XXH32_state_t const *const src);

XXH_errorcode XXH32_reset(XXH32_state_t *const state, uint32_t const seed);
XXH_errorcode XXH32_update(XXH32_state_t *const state, void const *const input, size_t const length);
uint32_t XXH32_digest(XXH32_state_t const *const state);

#ifdef __cplusplus
}
#endif

static uint32_t const PRIME32_1 = 0x9E3779B1U;   /* 0b10011110001101110111100110110001 */
static uint32_t const PRIME32_2 = 0x85EBCA77U;   /* 0b10000101111010111100101001110111 */
static uint32_t const PRIME32_3 = 0xC2B2AE3DU;   /* 0b11000010101100101010111000111101 */
static uint32_t const PRIME32_4 = 0x27D4EB2FU;   /* 0b00100111110101001110101100101111 */
static uint32_t const PRIME32_5 = 0x165667B1U;   /* 0b00010110010101100110011110110001 */

/* Rotates value left by amt. */
static uint32_t XXH_rotl32(uint32_t const value, uint32_t const amt)
{
    return (value << (amt % 32)) | (value >> (32 - amt % 32));
}

/* Portably reads a 32-bit little endian integer from data at the given offset. */
static uint32_t XXH_read32(uint8_t const *const data, size_t const offset)
{
    return (uint32_t) data[offset + 0]
        | ((uint32_t) data[offset + 1] << 8)
        | ((uint32_t) data[offset + 2] << 16)
        | ((uint32_t) data[offset + 3] << 24);
}

/* Mixes input into lane. */
static uint32_t XXH32_round(uint32_t lane, uint32_t const input)
{
    lane += input * PRIME32_2;
    lane  = XXH_rotl32(lane, 13);
    lane *= PRIME32_1;
    return lane;
}

/* Mixes all bits to finalize the hash. */
static uint32_t XXH32_avalanche(uint32_t hash)
{
    hash ^= hash >> 15;
    hash *= PRIME32_2;
    hash ^= hash >> 13;
    hash *= PRIME32_3;
    hash ^= hash >> 16;
    return hash;
}


/* Dynamically allocates XXH32_state_t. It is expected to free this with
 * XXH32_freeState.
 * returns: A pointer to an XXH64_state_t. This may be NULL. */
XXH32_state_t *XXH32_createState(void)
{
    return (XXH32_state_t *) malloc(sizeof(XXH32_state_t));
}

/* Frees an XXH64_state_t.
 * state:   The state to free.
 * returns: XXH_OK on success, XXH_ERROR on error. */
XXH_errorcode XXH32_freeState(XXH32_state_t *const state)
{
    free(state);
    return XXH_OK;
}


/* Copies one XXH32_state_t to another.
 * dest:  The state to copy to. It is undefined behavior for dest to overlap with
 *        src.
 * src:   The state to copy from. It is undefined behavior for src to overlap with
 *        dest. */
void XXH32_copyState(XXH32_state_t *const dest, XXH32_state_t const *const src)
{
    memcpy(dest, src, sizeof(XXH32_state_t));
}


/* Resets an XXH64_state_t.
 * state:   The state to reset.
 * seed:    The seed to use.
 * returns: XXH_OK on success, XXH_ERROR on error. */
XXH_errorcode XXH32_reset(XXH32_state_t *const state, uint32_t const seed)
{
    /* Don't write into a null pointer. The official implementation doesn't check
     * for this. */
    if (state == NULL) {
        return XXH_ERROR;
    }

    memset(state, 0, sizeof(XXH32_state_t));

    state->lane1 = seed + PRIME32_1 + PRIME32_2;
    state->lane2 = seed + PRIME32_2;
    state->lane3 = seed + 0;
    state->lane4 = seed - PRIME32_1;
    return XXH_OK;
}

/* The XXH32 hash function update loop.
 * state:   The current state. It is undefined behavior to overlap with input.
 * input:   The data to hash. It is undefined behavior to overlap with state.
 * length:  The length of input. It is undefined behavior to have length larger than the
 *          capacity of input.
 * returns: XXH_OK on success, XXH_ERROR on failure. */
XXH_errorcode XXH32_update(XXH32_state_t *const state, void const *const input, size_t const length)
{
    uint8_t const *const data = (uint8_t const *) input;
    size_t remaining;
    size_t offset = 0;

    /* Don't dereference a null pointer. The reference implementation notably doesn't
     * check for this by default. */
    if (state == NULL || input == NULL) {
        return XXH_ERROR;
    }

    state->total_len_32 += (uint32_t) length;

    if (state->has_large_len == FALSE && (length >= 16 || state->total_len_32 >= 16)) {
        state->has_large_len = TRUE;
    }

    if (state->temp_buffer_size + length < 16)  {
        /* We don't have a full buffer, so we just copy the data over and return. */
        memcpy(&state->temp_buffer[state->temp_buffer_size], input, length);
        state->temp_buffer_size += (uint32_t) length;
        return XXH_OK;
    }

    remaining = state->temp_buffer_size + length;

     while (remaining >= 16) {
        /* fill up our temp buffer */
        memcpy(&state->temp_buffer[state->temp_buffer_size], &data[offset], 16 - state->temp_buffer_size);

        /* do our rounds */
        state->lane1 = XXH32_round(state->lane1, XXH_read32(state->temp_buffer, 0));
        state->lane2 = XXH32_round(state->lane2, XXH_read32(state->temp_buffer, 4));
        state->lane3 = XXH32_round(state->lane3, XXH_read32(state->temp_buffer, 8));
        state->lane4 = XXH32_round(state->lane4, XXH_read32(state->temp_buffer, 12));

        /* done with the rounds */
        offset += 16 - state->temp_buffer_size;
        remaining -= 16;
        state->temp_buffer_size = 0;
    }

    if (remaining != 0) {
        memcpy(state->temp_buffer, &data[offset], remaining);
        state->temp_buffer_size = (uint32_t) remaining;
    }

    return XXH_OK;
}

/* Finalizes an XXH32_state_t and returns the seed.
 * state:   The state to finalize. This is not modified.
 * returns: The calculated 32-bit hash. */
uint32_t XXH32_digest(XXH32_state_t const *const state)
{
    uint32_t hash;
    uint32_t remaining = state->temp_buffer_size;
    uint32_t offset = 0;

    if (state->has_large_len == TRUE) {
        hash = XXH_rotl32(state->lane1, 1)
             + XXH_rotl32(state->lane2, 7)
             + XXH_rotl32(state->lane3, 12)
             + XXH_rotl32(state->lane4, 18);
    } else {
        /* Not enough data for the main loop, put something in there instead. */
        hash = state->lane3 /* will be seed because of the + 0 */ + PRIME32_5;
    }

    hash += state->total_len_32;

    /* Process the remaining data. */
    while (remaining >= 4) {
        hash += XXH_read32(state->temp_buffer, offset) * PRIME32_3;
        hash  = XXH_rotl32(hash, 17);
        hash *= PRIME32_4;
        offset += 4;
        remaining -= 4;
    }

    while (remaining != 0) {
        hash += (uint32_t) state->temp_buffer[offset] * PRIME32_5;
        hash  = XXH_rotl32(hash, 11);
        hash *= PRIME32_1;
        --remaining;
        ++offset;
    }

    return XXH32_avalanche(hash);
}

#ifdef XXH_SELFTEST
#include <stdio.h>  /* fprintf, puts */
#include <stdlib.h> /* exit */

#define TEST_DATA_SIZE 101
static int test_num = 0;

/* Checks a hash value. */
static void test_sequence(uint8_t const *const test_data, size_t const length,
                          uint32_t const seed, uint32_t const expected)
{
    XXH32_state_t *state = XXH32_createState();
    uint32_t result;
    size_t i;

    XXH32_reset(state, seed);
    XXH32_update(state, test_data, length);
    result = XXH32_digest(state);

    if (result != expected) {
        fprintf(stderr, "Error: Test %i: XXH32 test failed!\n", ++test_num);
        fprintf(stderr, "Expected value: 0x%08X. Actual value: 0x%08X.\n", expected, result);
        exit(1);
    }

    XXH32_reset(state, seed);
    for (i = 0; i < length; i++) {
        XXH32_update(state, &test_data[i], 1);
    }
    result = XXH32_digest(state);

    if (result != expected) {
        fprintf(stderr, "Error: Test %i: XXH32 test failed!\n", ++test_num);
        fprintf(stderr, "Expected value: 0x%08X. Actual value: 0x%08X.\n", expected, result);
        exit(1);
    }
    XXH32_freeState(state);
}

int main(void)
{
    uint32_t const prime = PRIME32_1;
    uint8_t test_data[TEST_DATA_SIZE] = {0};
    uint32_t byte_gen = prime;
    int i = 0;

    /* Fill the test_data buffer with "random" data */
    for (; i < TEST_DATA_SIZE; i++) {
        test_data[i] = (uint8_t) (byte_gen >> 24);
        byte_gen *= byte_gen;
    }

    test_sequence(NULL     ,  0            , 0    , 0x02CC5D05U);
    test_sequence(NULL     ,  0            , prime, 0x36B78AE7U);
    test_sequence(test_data,  1            , 0    , 0xB85CBEE5U);
    test_sequence(test_data,  1            , prime, 0xD5845D64U);
    test_sequence(test_data, 14            , 0    , 0xE5AA0AB4U);
    test_sequence(test_data, 14            , prime, 0x4481951DU);
    test_sequence(test_data, TEST_DATA_SIZE, 0,     0x1F1AA412U);
    test_sequence(test_data, TEST_DATA_SIZE, prime, 0x498EC8E2U);

    puts("XXH32 reference implementation: OK");

    return 0;
}

#endif /* XXH_SELFTEST */

