# FPGA settings
FPGA_TOP = fpga_lb
BOARD = DE2-115

# QUARTUS_PATH: If empty then system path is searched. If set then requires trailling slash.
QUARTUS_PATH ?=

# Files for synthesis
SYN_FILES = rtl/fpga_lb.v
SYN_FILES += rtl/signal_sync.v

SYN_FILES += ip/pll/pll.qip

# QSF files
QSF_FILES = quartus/fpga_lb.qsf

# SDC files
SDC_FILES = quartus/fpga_lb.sdc

# QSYS
QSYS_SYSTEM := lb_system
QSYS_COMPONENTS := lb_dataplane VexRiscvCpu riscv_mtime gpio_ctrl

# Remote JTAG server
REMOTE_JTAG ?= 0
REMOTE_JTAG_SERVER_NAME = 10.10.0.4
REMOTE_JTAG_SERVER_PASS = pass

