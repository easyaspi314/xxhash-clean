# xxHash Clean C Reference Implementation

This is a cleaned up C reference implementation of `XXH32`, `XXH64`, `XXH3_64bits`, and `XXH3_128bits`, inspired by the [HighwayHash C implementation](https://github.com/google/highwayhash/blob/master/c/highwayhash.c).

**Warning: Performance is not the goal of this implementation.**

If you want a fast xxHash implementation, see the [official version](https://github.com/Cyan4973/xxHash)
by [Cyan4973](https://github.com/Cyan4973).

This version focuses on the following:
 - **Portability:** This code is all C90 and C++98 compatible with the additional requirement of `stdint.h` and `long long` (`XXH32` does not need `long long`).
 - **"Port-ability":** The single shot code only works with integers and arrays directly, not relying on any library functions like `memcpy`. The only `#include`s are for typedefs, so it would be very easy to port to another language. However, the streaming versions do rely on some functions, only because it is impossible or really ugly to do otherwise.
   - `memcpy`
   - `malloc`
   - `memset`
   - `free`
 - **Correctness:** Everything is (excessively?) const-correct, endian-independent, and standards compliant. Give me `-Weverything`, I can take it.
 - **Compactness:** The code is very short. Light documentation is added but the code only stands at about 240 lines for `XXH64` and 200 lines for `XXH32`, about 50 of which are from tests. `XXH3` squeezes to umder 800 lines.
    - The code also compiles very small.w
 - **Simplicity:** This implementation tries to make the code as clear as possible. Loops are rerolled, pointer arithmetic is removed in favor of counters, etc. Nothing is more complicated than it needs to be.
 - **Cleanliness:** No ugly macros, no `#ifdef` blocks (except for test data and `extern "C"`), no ugly SIMD intrinsics, no inline assembly hacks, nothing.

The streaming implementations are in separate files to keep things clean. Only `XXH32` and `XXH64` are implemented for now.

This does impact performance a lot, for example, Clang will autovectorize `XXH32` with a bunch of `pshufb` instructions when reading the data, resulting in stupid slowdowns.

This also provides `xxhsum-example.c` which shows how to use xxhash's streaming functions to read and checksum files. It isn't a fully-featured tool, and it shares no code with xxhsum.c from the official repository.

### License

```
xxHash Library
Copyright (c) 2012-2020, Yann Collet
Copyright (c) 2019-2020, Devin Hussey (easyaspi314)
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice, this
  list of conditions and the following disclaimer in the documentation and/or
  other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```
