CC = gcc

CFLAGS = -Werror -Wall -Wextra -Wpedantic -g
LDFLAGS = -lm

LIB_DIR = lib
SRC_DIR = src
TEST_DIR = tests
BIN_DIR = bin

UNIT_DIR = $(TEST_DIR)/unit
INTEGRATION_DIR = $(TEST_DIR)/integration

LUKIP_DIR = $(LIB_DIR)/lukip
LUKIP_LIB = $(LUKIP_DIR)/liblukip.a

ZYMUX_EXE = zymux
UNIT_EXE = unittest

SRCS := $(wildcard $(SRC_DIR)/*.c $(SRC_DIR)/*/*.c $(SRC_DIR)/*/*/*.c)
UNIT_SRCS := $(wildcard $(UNIT_DIR)/*.c $(UNIT_DIR)/*/*.c $(UNIT_DIR)/*/*/*.c)
INTEGRATION_SRCS := \
	$(wildcard $(INTEGRATION_DIR)/*.zmx $(INTEGRATION_DIR)/*/*.zmx $(INTEGRATION_DIR)/*/*/*.zmx)

ifeq ($(OS), Windows_NT)
	UNIT_DIR = $(TEST_DIR)\unit
	INTEGRATION_DIR = $(TEST_DIR)\integration
	LUKIP_DIR = $(LIB_DIR)\lukip
	LUKIP_LIB = $(LUKIP_DIR)\liblukip.a

	ZYMUX_EXE = zymux.exe
	UNIT_EXE = unittest.exe
	SRCS := $(subst /,\,$(SRCS))
	UNIT_SRCS := $(subst /,\,$(UNIT_SRCS))

	CFLAGS += -I$(SRC_DIR)\data_structures -I$(SRC_DIR)\debug -I$(SRC_DIR)\language
	CFLAGS += -I$(TEST_DIR)\integration -I$(TEST_DIR)\unit -I$(TEST_DIR)\unit\test_data_structures
	CFLAGS += -I$(LIB_DIR)\lukip\include
else
	CFLAGS += -I$(SRC_DIR)/data_structures -I$(SRC_DIR)/debug -I$(SRC_DIR)/language
	CFLAGS += -I$(TEST_DIR)/integration -I$(TEST_DIR)/unit -I$(TEST_DIR)/unit/test_data_structures
	CFLAGS += -I$(LIB_DIR)/lukip/include
endif

SRC_OBJS = $(SRCS:.c=.o)
UNIT_OBJS = $(UNIT_SRCS:.c=.o)

# Defines a newline var which resolves to an actual escaped (\n), hence endef is a line down.
define newline


endef

.PHONY: all test integration-test unit-test clean

all: $(ZYMUX_EXE) 

test: unit-test integration-test

integration-test: $(BIN_DIR) $(ZYMUX_EXE)
	@echo Integration tests started.
ifeq ($(OS), Windows_NT)
	@$(foreach file, $(INTEGRATION_SRCS), .\$<\$(ZYMUX_EXE) $(file) || exit 0$(newline)) 
else
	@$(foreach file, $(INTEGRATION_SRCS), ./$</$(ZYMUX_EXE) $(file) || true$(newline))
endif
	@echo Integration tests finished.

unit-test: $(BIN_DIR) $(SRC_OBJS) $(UNIT_OBJS) $(LUKIP_LIB)
ifeq ($(OS), Windows_NT)
	$(CC) -o $<\$(UNIT_EXE) $(UNIT_OBJS) $(LUKIP_LIB) $(LDFLAGS)
	.\$(BIN_DIR)\$(UNIT_EXE)
else
	$(CC) -o $</$(UNIT_EXE) $(UNIT_OBJS) $(LUKIP_LIB) $(LDFLAGS)
	./$(BIN_DIR)/$(UNIT_EXE)
endif

$(LUKIP_LIB): 
	cd $(LUKIP_DIR) && $(MAKE)

$(ZYMUX_EXE): $(BIN_DIR) $(SRC_OBJS)
ifeq ($(OS), Windows_NT)
	$(CC) -o $<\$@ $(SRC_OBJS) $(LDFLAGS)
else
	$(CC) -o $</$@ $(SRC_OBJS) $(LDFLAGS)
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
