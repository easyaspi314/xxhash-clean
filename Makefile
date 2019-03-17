CC := gcc
CXX := g++

# Fight me, Clang. Give me all you've got. You're no match for me!
ifneq (,$(filter clang,$(shell $(CC) -v 2>&1)))
EXTRA_CFLAGS := -Weverything
else
EXTRA_CFLAGS :=
endif
ifneq (,$(filter clang,$(shell $(CXX) -v 2>&1)))
EXTRA_CXXFLAGS := -Weverything -Wno-old-style-cast -Wno-zero-as-null-pointer-constant
                           # ^ All stupid C++-specific stuff that would break C compatibility.
else
EXTRA_CXXFLAGS :=
endif

CFLAGS :=   $(EXTRA_CFLAGS) -O3 -Wall -Wextra -Wcast-qual -Wcast-align -Wshadow \
            -Wstrict-aliasing=1 -Wswitch-enum -Wdeclaration-after-statement \
            -Wstrict-prototypes -Wundef -Wpointer-arith -Wformat-security \
            -Wvla -Wformat=2 -Winit-self -Wfloat-equal -Wwrite-strings \
            -Wredundant-decls -Wstrict-overflow=5 -Wpedantic -Wc++-compat \
            -std=c90 -Werror

CXXFLAGS := $(EXTRA_CXXFLAGS) -O3 -Wall -Wextra -Wcast-qual -Wcast-align -Wshadow \
            -Wstrict-aliasing=1 -Wswitch-enum -Wundef -Wpointer-arith \
            -Wformat-security -Wvla -Wformat=2 -Winit-self -Wfloat-equal \
            -Wwrite-strings -Wredundant-decls -Wstrict-overflow=5 -Wpedantic \
            -std=c++98 -Werror

# Define *.exe as extension for Windows systems
ifneq (,$(filter Windows%,$(OS)))
EXT =.exe
else
EXT =
endif

# We do use long long in xxh64, but everything else must be ansi.
xxhash64-ref$(EXT): CFLAGS += -Wno-long-long
xxhash64-ref-cxx$(EXT): CXXFLAGS += -Wno-long-long -Wno-c++98-compat-pedantic

all: xxhash64-ref$(EXT) xxhash32-ref$(EXT) xxhash32-ref-cxx$(EXT) xxhash64-ref-cxx$(EXT)
cxx: xxhash32-ref-cxx$(EXT) xxhash64-ref-cxx$(EXT)

xxhash32-ref$(EXT): xxhash32-ref.c
	$(CC) $(CFLAGS) -DXXH_SELFTEST $< -o $@

xxhash64-ref$(EXT): xxhash64-ref.c
	$(CC) $(CFLAGS) -DXXH_SELFTEST $< -o $@

xxhash32-ref-cxx$(EXT): xxhash32-ref.c
	$(CXX) -x c++ $(CXXFLAGS) -DXXH_SELFTEST $< -o $@

xxhash64-ref-cxx$(EXT): xxhash64-ref.c
	$(CXX) -x c++ $(CXXFLAGS) -DXXH_SELFTEST $< -o $@

clean:
	$(RM) xxhash32-ref$(EXT) xxhash64-ref$(EXT) xxhash32-ref-cxx$(EXT) xxhash64-ref-cxx$(EXT)

.PHONY: all clean cxx
