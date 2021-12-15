TOOLCHAIN_PATH :=

TARGET  := riscv64-unknown-elf
BINARY  := $(TOOLCHAIN_PATH)$(TARGET)

CC      := $(BINARY)-gcc
OBJCOPY := $(BINARY)-objcopy
OBJDUMP := $(BINARY)-objdump
AR      := $(BINARY)-ar
BIN2MIF := python3 ../tools/bin2mif.py

MARCH := rv32ic
MABI  := ilp32

BUILD_DIR := build

FREERTOS_SRC := \
	FreeRTOS/queue.c \
	FreeRTOS/tasks.c \
	FreeRTOS/list.c \
	FreeRTOS/portable/MemMang/heap_4.c

PORT_SRC := FreeRTOS/portable/GCC/RISC-V/port.c
PORT_ASM := FreeRTOS/portable/GCC/RISC-V/portASM.S

PROG := fpga_lb

SRCS := src/main.c
ASMS := src/boot.S
LDSCRIPT := link.lds

CPPFLAGS = \
	-D__riscv_float_abi_soft \
	-DportasmHANDLE_INTERRUPT=handle_trap \
	-I . \
	-I ./include \
	-I FreeRTOS/include \
	-I FreeRTOS/portable/GCC/RISC-V

CFLAGS  = -march=$(MARCH) -mabi=$(MABI) -mcmodel=medany \
	-Wall \
	-fmessage-length=0 \
	-ffunction-sections \
	-fdata-sections \
	-fno-builtin-printf

ASFLAGS = -march=$(MARCH) -mabi=$(MABI) -mcmodel=medany
LDFLAGS = -march=$(MARCH) -mabi=$(MABI) -lgcc \
	-nostartfiles -ffreestanding -Wl,-Bstatic,-T,$(LDSCRIPT),-Map,$(BUILD_DIR)/$(PROG).map,--print-memory-usage

ifeq ($(DEBUG), 1)
    CFLAGS += -Og -ggdb3
else
    CFLAGS += -O2
endif

ifneq ($(VERBOSE), 1)
.SILENT:
endif

#
# Define all object files.
#
RTOS_OBJ = $(FREERTOS_SRC:%.c=$(BUILD_DIR)/%.o)
APP_OBJ  = $(SRCS:%.c=$(BUILD_DIR)/%.o)
PORT_OBJ = $(PORT_SRC:%.c=$(BUILD_DIR)/%.o)
PORT_ASM_OBJ = $(PORT_ASM:%.S=$(BUILD_DIR)/%.o)
ASM_OBJ = $(ASMS:%.S=$(BUILD_DIR)/%.o)
OBJS = $(ASM_OBJ) $(PORT_ASM_OBJ) $(PORT_OBJ) $(RTOS_OBJ) $(APP_OBJ)

all: $(BUILD_DIR)/$(PROG).elf $(BUILD_DIR)/$(PROG).hex $(BUILD_DIR)/$(PROG).bin $(BUILD_DIR)/$(PROG).asm $(BUILD_DIR)/$(PROG).mif

$(BUILD_DIR)/$(PROG).elf: $(OBJS) $(LDSCRIPT)
	@echo "[LINK] $@"
	$(CC) $(LDFLAGS) $(OBJS) -o $@

%.hex: %.elf
	@echo "[OBJCOPY] $< -> $@"
	$(OBJCOPY) -O ihex $^ $@

%.bin: %.elf
	@echo "[OBJCOPY] $< -> $@"
	$(OBJCOPY) -O binary $^ $@

%.asm: %.elf
	@echo "[OBJCOPY] $< -> $@"
	$(OBJDUMP) -S -d $^ > $@

%.mif: %.bin
	@echo "[BIN2MIF] $< -> $@"
	$(BIN2MIF) $< > $@

$(BUILD_DIR)/%.o: %.c
	@mkdir -p $(@D)
	@echo "[CC] $< -> $@"
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

$(BUILD_DIR)/%.o: %.S
	@mkdir -p $(@D)
	@echo "[CC] $< -> $@"
	$(CC) $(CPPFLAGS) $(ASFLAGS) -c $< -o $@

clean:
	rm -rf $(BUILD_DIR)

.PHONY: all clean
