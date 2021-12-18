# Based on https://github.com/stnolting/neorv32/blob/master/sw/common/common.mk


# -----------------------------------------------------------------------------
# User configuration
# -----------------------------------------------------------------------------

TOPDIR = $(realpath ..)

PROG := fpga_lb

FREERTOS_SRC := \
    FreeRTOS/portable/GCC/RISC-V/portASM.S \
	FreeRTOS/portable/GCC/RISC-V/port.c \
	FreeRTOS/queue.c \
	FreeRTOS/tasks.c \
	FreeRTOS/list.c \
	FreeRTOS/portable/MemMang/heap_4.c

APP_SRC := $(FREERTOS_SRC)
APP_SRC += src/main.c

APP_INC := \
	-I include \
	-I FreeRTOS/include \
	-I FreeRTOS/portable/GCC/RISC-V

ASM_INC := -I .

# Optimization
EFFORT := -O2

# Compiler toolchain
TOOLCHAIN_PATH :=
RISCV_PREFIX := riscv64-unknown-elf-

MARCH := rv32ic
MABI  := ilp32

# User flags for additional configuration (will be added to compiler flags)
USER_FLAGS :=


# -----------------------------------------------------------------------------
# General definitions
# -----------------------------------------------------------------------------

BUILD_DIR = build

PROG_BIN = $(BUILD_DIR)/$(PROG).bin
PROG_ELF = $(BUILD_DIR)/$(PROG).elf
PROG_HEX = $(BUILD_DIR)/$(PROG).hex
PROG_MIF = $(BUILD_DIR)/$(PROG).mif
PROG_ASM = $(BUILD_DIR)/$(PROG).asm

LINK_MAP = $(BUILD_DIR)/$(PROG).map

# -----------------------------------------------------------------------------
# Core libraries
# -----------------------------------------------------------------------------

CORELIB_COM_PATH = common
CORELIB_INC_PATH = lib/include
CORELIB_SRC_PATH = lib/src

CORE_SRC  = $(wildcard $(CORELIB_SRC_PATH)/*.c)
CORE_SRC += $(CORELIB_COM_PATH)/boot.S

LD_SCRIPT = common/link.lds


# -----------------------------------------------------------------------------
# Sources and objects
# -----------------------------------------------------------------------------

SRCS  = $(APP_SRC)
SRCS += $(CORE_SRC)

OBJS = $(SRCS:%=$(BUILD_DIR)/%.o)


# -----------------------------------------------------------------------------
# Tools and flags
# -----------------------------------------------------------------------------
# Compiler tools
CC      = $(TOOLCHAIN_PATH)$(RISCV_PREFIX)gcc
OBJCOPY = $(TOOLCHAIN_PATH)$(RISCV_PREFIX)objcopy
OBJDUMP = $(TOOLCHAIN_PATH)$(RISCV_PREFIX)objdump
AR      = $(TOOLCHAIN_PATH)$(RISCV_PREFIX)ar
LD      = $(TOOLCHAIN_PATH)$(RISCV_PREFIX)ld

# Utility tools
BIN2MIF = python3 $(TOPDIR)/tools/bin2mif.py

CPPFLAGS = \
	-D__riscv_float_abi_soft \
	-DportasmHANDLE_INTERRUPT=SystemIrqHandler \

CFLAGS = \
	$(EFFORT) \
	-march=$(MARCH) -mabi=$(MABI) -mcmodel=medany \
	-Wall \
	-fmessage-length=0 \
	-ffunction-sections \
	-fdata-sections \
	-fno-builtin-printf

# This accelerates instruction fetch after branches when C extension is enabled (irrelevant when C extension is disabled)
CFLAGS += -falign-functions=4 -falign-labels=4 -falign-loops=4 -falign-jumps=4
CFLAGS += $(USER_FLAGS)

ASFLAGS = -march=$(MARCH) -mabi=$(MABI) -mcmodel=medany
LDFLAGS = -march=$(MARCH) -mabi=$(MABI) -lgcc \
	-nostartfiles -ffreestanding -Wl,-Bstatic,--print-memory-usage

# -----------------------------------------------------------------------------
# Application output definitions
# -----------------------------------------------------------------------------
.PHONY: all
.DEFAULT_GOAL := all

ifneq ($(VERBOSE), 1)
.SILENT:
endif

all: $(PROG_BIN) $(PROG_ELF) $(PROG_HEX) $(PROG_MIF) $(PROG_ASM)

clean:
	rm -rf $(BUILD_DIR)

# -----------------------------------------------------------------------------
# General targets: Assemble, compile, link, dump
# -----------------------------------------------------------------------------

$(BUILD_DIR)/%.c.o: %.c
	@mkdir -p $(@D)
	@echo "[CC] $< -> $@"
	$(CC) $(CPPFLAGS) $(APP_INC) -I $(CORELIB_INC_PATH) $(CFLAGS) -c $< -o $@

$(BUILD_DIR)/%.S.o: %.S
	@mkdir -p $(@D)
	@echo "[AS] $< -> $@"
	$(CC) $(CPPFLAGS) $(APP_INC) -I $(CORELIB_INC_PATH) $(ASFLAGS) -c $< -o $@

$(PROG_ELF): $(OBJS) $(LD_SCRIPT)
	@echo "[LINK] $@"
	$(CC) $(LDFLAGS) -T $(LD_SCRIPT) -Wl,-Map=$(LINK_MAP) $(OBJS) -o $@

$(PROG_HEX): $(PROG_ELF)
	@echo "[OBJCOPY] $< -> $@"
	$(OBJCOPY) -O ihex $^ $@

$(PROG_BIN): $(PROG_ELF)
	@echo "[OBJCOPY] $< -> $@"
	$(OBJCOPY) -O binary $^ $@

$(PROG_ASM): $(PROG_ELF)
	@echo "[OBJDUMP] $< -> $@"
	$(OBJDUMP) -S -d $^ > $@

$(PROG_MIF): $(PROG_BIN)
	@echo "[BIN2MIF] $< -> $@"
	$(BIN2MIF) $< > $@

