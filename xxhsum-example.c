/*
 *  xxhsum-example.c - xxHash example checksum utility
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
 *  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */

/* Note: This is completely different from xxhsum from xxHash's official
 * repository. It is released under a more permissive license, but it
 * has fewer features (i.e. it doesn't have -c or reading from stdin).
 *
 * It is mainly designed to show how to use the streaming functions. */

#include <stdio.h>  /* FILE, fread, fprintf, printf */
#include <string.h> /* strcmp, strerror */
#include <stddef.h> /* size_t, NULL */
#include <stdlib.h> /* exit */
#include <errno.h>  /* errno */

#include "xxhash-ref.h"

static char buffer[8192];

static XXH32_state_t *state_32;
static XXH64_state_t *state_64;

/* Just automates the fopen/error message */
static FILE *open_file(char const *const filename)
{
    FILE *file = fopen(filename, "rb");
    if (file == NULL) {
        fprintf(stderr, "Could not open '%s': %s.\n", filename, strerror(errno));
        exit(1);
    }
    return file;
}

/* Calculates and prints a 32-bit hash. */
static void hash_file_32(char const *const filename)
{
    size_t count;
    FILE *file = open_file(filename);

    if (XXH32_reset(state_32, 0) == XXH_ERROR) {
        fprintf(stderr, "Error resetting hash state.\n");
        exit(1);
    }

    /* Read in chunks. */
    while ((count = fread(buffer, 1, sizeof(buffer), file)) != 0) {
        if (ferror(file)) {
            fprintf(stderr, "Error reading file %s: %s.\n", filename, strerror(errno));
            exit(1);
        }
        if (XXH32_update(state_32, buffer, count) == XXH_ERROR) {
            fprintf(stderr, "Error hashing data.\n");
            exit(1);
        }
    }
    fclose(file);

    printf("%08x  %s\n", XXH32_digest(state_32), filename);
}

/* Calculates and prints a 64-bit hash. */
static void hash_file_64(char const *const filename)
{
    size_t count;
    FILE *file = open_file(filename);

    if (XXH64_reset(state_64, 0) == XXH_ERROR) {
        fprintf(stderr, "Error resetting hash state.\n");
        exit(1);
    }

    /* Read in chunks. */
    while ((count = fread(buffer, 1, sizeof(buffer), file)) != 0) {
        if (ferror(file)) {
            fprintf(stderr, "Error reading file '%s': %s.\n", filename, strerror(errno));
            exit(1);
        }
        if (XXH64_update(state_64, buffer, count) == XXH_ERROR) {
            fprintf(stderr, "Error hashing data.\n");
            exit(1);
        }
    }
    fclose(file);

    printf("%016llx  %s\n", (unsigned long long)XXH64_digest(state_64), filename);
}

#ifdef __GNUC__
__attribute__((__noreturn__))
#endif
static void usage(char const *const prog_name)
{
    /* TODO: read from stdin */
    fprintf(stderr, "Usage: %s [-H0|-H1] files...\n", prog_name);
    exit(1);
}

int main(int argc, char *argv[]) {
    /* So we don't have to double loops. */
    void (*hash_file)(char const *const filename) = &hash_file_64;
    int i = 1;

    if (argc == 1 || strcmp(argv[1], "--help") == 0 || strcmp(argv[1], "-h") == 0) {
        usage(argv[0]);
    }

    state_32 = XXH32_createState();
    state_64 = XXH64_createState();

    if (state_32 == NULL || state_64 == NULL) {
        fprintf(stderr, "Out of memory.\n");
        return 1;
    }

    if (strcmp(argv[1], "-H0") == 0) {
        ++i;
        /* change our callback */
        hash_file = &hash_file_32;
    } else if (strcmp(argv[1], "-H1") == 0) {
        ++i;
    }

    if (i == argc) {
        usage(argv[0]);
    }

    for (; i < argc; i++) {
        (*hash_file)(argv[i]);
    }

    XXH32_freeState(state_32);
    XXH64_freeState(state_64);

    return 0;
}
