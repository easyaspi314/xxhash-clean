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

#ifndef XXHASH_REF_H
#define XXHASH_REF_H 1

/* Headers for the xxhash reference implementation. */

#include <stddef.h> /* size_t */
#include <stdint.h> /* uint32_t, uint64_t */

#ifdef __cplusplus
extern "C" {
#endif
typedef enum {
    XXH_OK = 0,
    XXH_ERROR = 1
} XXH_errorcode;

typedef struct XXH32_state_s XXH32_state_t;
typedef struct XXH64_state_s XXH64_state_t;

uint32_t XXH32(void const *const input, size_t const length, uint32_t const seed);

/*======   Streaming   ======*/
XXH32_state_t *XXH32_createState(void);
XXH_errorcode XXH32_freeState(XXH32_state_t *const state);
void XXH32_copyState(XXH32_state_t *const dest, XXH32_state_t const *const src);

XXH_errorcode XXH32_reset(XXH32_state_t *const statePtr, uint32_t const seed);
XXH_errorcode XXH32_update(XXH32_state_t *const statePtr, void const *const input, size_t const length);
uint32_t XXH32_digest(XXH32_state_t const *const statePtr);

uint64_t XXH64(void const *const input, size_t const length, uint64_t const seed);

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

#endif /* XXHASH_REF_H */

