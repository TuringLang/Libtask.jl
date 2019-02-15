JULIA_HOME ?= $(PWD)
target ?= x86_64-linux-gnu
prefix ?= $(PWD)
JULIA_VERSION ?= v1_1

INCLUDES = -I$(JULIA_HOME)/include/julia

ifneq (,$(findstring mingw32,$(target))) # mingw32
	OUTPUT_DIR = $(prefix)/bin
else
	OUTPUT_DIR = $(prefix)/lib
endif


ifneq (,$(findstring mingw32,$(target))) # mingw32
	LIBS = -L$(JULIA_HOME)/bin
else
	LIBS = -L$(JULIA_HOME)/lib -L$(JULIA_HOME)/lib/julia
endif


LDFLAGS = $(LIBS) -ljulia
ifneq (,$(findstring mingw32,$(target))) # mingw32
    LDFLAGS += -lopenlibm
endif

CFLAGS = -O2 -shared -std=gnu99 -DJULIA_ENABLE_THREADING=1 -fPIC
ifneq (,$(findstring i686,$(target))) # i686
	CFLAGS += -march=pentium4
endif
ifneq (,$(findstring linux,$(target))) # linux
	CFLAGS += -Wl,--export-dynamic
endif
ifneq (,$(findstring mingw32,$(target))) # mingw32
	CFLAGS += -Wl,--export-all-symbols
endif

ifneq (,$(findstring mingw32,$(target))) # mingw32
	DYEXT = dll
else ifneq (,$(findstring darwin,$(target))) # macOS
	DYEXT = dylib
else
	DYEXT = so
endif


all:
	mkdir -p $(OUTPUT_DIR)
	$(CC) $(CFLAGS) $(INCLUDES) $(LDFLAGS) task.c -o $(OUTPUT_DIR)/libtask_$(JULIA_VERSION).$(DYEXT)
