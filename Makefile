CC = gcc

# TODO: add -Werror to CFLAGS later.
CFLAGS = -Wall -Wextra -Wpedantic -g

ifeq ($(OS), Windows_NT)
	CFLAGS += -Isrc\data_structures -Isrc\debug -Isrc\language -Itests\language
	CFLAGS += -Itests\unit -Itests\unit\lukip\include -Itests\unit\test_data_structures
else
	CFLAGS += -Isrc/data_structures -Isrc/debug -Isrc/language -Itests/language
	CFLAGS += -Itests/unit -Itests/unit/lukip/include -Itests/unit/test_data_structures
endif

SRC_DIR = src
TEST_DIR = tests
UNIT_DIR = tests/unit
LANG_DIR = tests/language
BIN_DIR = bin

LUKIP_DIR = $(UNIT_DIR)/lukip
LUKIP_LIB = $(LUKIP_DIR)/liblukip.a

ZYMUX_EXE = zymux
UNIT_EXE = unittest

SRCS := $(wildcard $(SRC_DIR)/*.c $(SRC_DIR)/*/*.c $(SRC_DIR)/*/*/*.c)

# Must manually add directories nested inside tests/unit so lukip's sources aren't included.
# TODO: move Lukip inside a lib directory flat on the project so we don't have to hardcore this.
UNIT_SRCS := $(wildcard $(UNIT_DIR)/*.c $(UNIT_DIR)/test_data_structures/*.c )
LANG_SRCS := $(wildcard $(LANG_DIR)/*.zmx $(LANG_DIR)/*/*.zmx $(LANG_DIR)/*/*/*.zmx)

ifeq ($(OS), Windows_NT)
	UNIT_DIR = tests\unit
	LANG_DIR = tests\language
	LUKIP_DIR = $(UNIT_DIR)\lukip
	LUKIP_LIB = $(LUKIP_DIR)\liblukip.a

	ZYMUX_EXE = zymux.exe
	UNIT_EXE = unittest.exe
	SRCS := $(subst /,\,$(SRCS))
	UNIT_SRCS := $(subst /,\,$(UNIT_SRCS))
	LANG := $(subst /,\,$(LANG))
endif

SRC_OBJS = $(SRCS:.c=.o)
UNIT_OBJS = $(UNIT_SRCS:.c=.o)

# Defines a variable "\n" which resolves to an actual escape newline sequence (hence endef is down).
define newline


endef

.PHONY: all tests langtest unittest clean

all: $(ZYMUX_EXE) 

tests: unittest langtest

langtest: $(BIN_DIR) $(ZYMUX_EXE)
ifeq ($(OS), Windows_NT)
	$(foreach file, $(LANG_SRCS), .\$<\$(ZYMUX_EXE) $(file) || exit 0$(newline)) 
else
	$(foreach file, $(LANG_SRCS), ./$</$(ZYMUX_EXE) $(file) || true$(newline))
endif

unittest: $(BIN_DIR) $(SRC_OBJS) $(UNIT_OBJS) $(LUKIP_LIB)
ifeq ($(OS), Windows_NT)
	$(CC) -o $<\$(UNIT_EXE) $(UNIT_OBJS) $(LUKIP_LIB)
	.\$(BIN_DIR)\$(UNIT_EXE)
else
	$(CC) -o $</$(UNIT_EXE) $(UNIT_OBJS) $(LUKIP_LIB)
	./$(BIN_DIR)/$(UNIT_EXE)
endif

$(LUKIP_LIB): 
	cd $(LUKIP_DIR) && $(MAKE)

$(ZYMUX_EXE): $(BIN_DIR) $(SRC_OBJS)
ifeq ($(OS), Windows_NT)
	$(CC) -o $<\$@ $(SRC_OBJS)
else
	$(CC) -o $</$@ $(SRC_OBJS)
endif

%.o: %.c
	$(CC) $(CFLAGS) -c $^ -o $@

$(BIN_DIR):
	mkdir $@

clean:
	cd $(LUKIP_DIR) && $(MAKE) clean
ifeq ($(OS), Windows_NT)
	$(foreach obj, $(SRC_OBJS) $(UNIT_OBJS), if exist $(obj) del /s /q $(obj) > NUL$(newline))
	if exist $(BIN_DIR) rmdir /s /q $(BIN_DIR)
else
	rm -rf $(SRC_OBJS) $(UNIT_OBJS) $(BIN_DIR)
endif
