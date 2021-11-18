# FPGA settings
FPGA_TOP = fpga_lb
BOARD = DE2-115

# Files for synthesis
SYN_FILES = rtl/fpga_lb.v

# QSF files
QSF_FILES = quartus/fpga_lb.qsf

# SDC files
SDC_FILES = quartus/fpga_lb.sdc

# Remote JTAG server
REMOTE_JTAG ?= 0
REMOTE_JTAG_SERVER_NAME = 10.10.0.4
REMOTE_JTAG_SERVER_PASS = pass
