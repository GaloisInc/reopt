

SOURCES  := $(wildcard *.c)
ROOTS    := $(patsubst %.c,%,${SOURCES})
TARGETS  := ${foreach bin,${ROOTS},$(foreach lvl,O0 O1 O2,$(bin)_$(lvl).exe)}

CFLAGS += -std=c11

all: ${TARGETS}

# hack
%_O0.exe: %.c
	$(CC) $(CFLAGS) -O0 -o $@ $<

%_O1.exe: %.c
	$(CC) $(CFLAGS) -O1 -o $@ $<

%_O2.exe: %.c
	$(CC) $(CFLAGS) -O2 -o $@ $<



