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

XXHSUM_EXAMPLE_OBJS := xxhash32-streaming-ref.o xxhash64-streaming-ref.o xxhsum-example.o
# Define *.exe as extension for Windows systems
ifneq (,$(filter Windows%,$(OS)))
EXT =.exe
else
EXT =
endif

# We do use long long in xxh64 and xxhsum, but everything else must be ansi.
xxhash64-ref$(EXT): CFLAGS += -Wno-long-long
xxhash64-ref-cxx$(EXT): CXXFLAGS += -Wno-long-long -Wno-c++98-compat-pedantic
xxhash64-streaming-ref$(EXT): CFLAGS += -Wno-long-long
xxhash64-streaming-ref.o: CFLAGS += -Wno-long-long
xxhash64-streaming-ref-cxx$(EXT): CXXFLAGS += -Wno-long-long -Wno-c++98-compat-pedantic
xxh3-64b-ref$(EXT): CFLAGS += -Wno-long-long
xxh3-64b-ref-cxx$(EXT): CXXFLAGS += -Wno-long-long -Wno-c++98-compat-pedantic
xxh3-128b-ref$(EXT): CFLAGS += -Wno-long-long
xxh3-128b-ref-cxx$(EXT): CXXFLAGS += -Wno-long-long -Wno-c++98-compat-pedantic

xxhsum-example.o: CFLAGS += -Wno-long-long

all: xxhsum-example$(EXT) xxhash64-ref$(EXT) xxhash32-ref$(EXT) xxhash32-ref-cxx$(EXT) xxhash64-ref-cxx$(EXT) \
     xxhash32-streaming-ref$(EXT)  xxhash64-streaming-ref$(EXT) xxhash32-streaming-ref-cxx$(EXT) \
     xxhash64-streaming-ref-cxx$(EXT) xxh3-64b-ref$(EXT) xxh3-128b-ref$(EXT) xxh3-64b-ref-cxx$(EXT) xxh3-128b-ref-cxx$(EXT)
cxx: xxhash32-ref-cxx$(EXT) xxhash64-ref-cxx$(EXT) xxh3-64b-ref-cxx$(EXT) xxh3-128b-ref-cxx$(EXT)

xxhsum-example$(EXT): $(XXHSUM_EXAMPLE_OBJS) xxhash-ref.h
	$(CC) $(CFLAGS) $(XXHSUM_EXAMPLE_OBJS) -o xxhsum-example$(EXT)

$(XXHSUM_EXAMPLE_OBJS): %.o: %.c
	$(CC) -c $(CFLAGS) $< -o $@

%ref$(EXT): %ref.c
	$(CC) $(CFLAGS) -DXXH_SELFTEST $< -o $@

%ref-cxx$(EXT): %ref.c
	$(CXX) $(CXXFLAGS) -DXXH_SELFTEST -x c++ $< -o $@

clean:
	$(RM) xxhash32-ref$(EXT) xxhash64-ref$(EXT) xxhash32-ref-cxx$(EXT) xxhash64-ref-cxx$(EXT) \
	$(RM) xxhash32-streaming-ref$(EXT) xxhash64-streaming-ref$(EXT) xxhash32-streaming-ref-cxx$(EXT) \
    xxhash64-streaming-ref-cxx$(EXT) xxhsum-example$(EXT) xxh3-64b-ref$(EXT) xxh3-64b-ref-cxx$(EXT) \
     xxh3-128b-ref$(EXT) xxh3-128b-ref-cxx$(EXT)  $(XXHSUM_EXAMPLE_OBJS)

.PHONY: all clean cxx
