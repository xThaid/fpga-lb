QUARTUS_MAP    = $(QUARTUS_PATH)bin/quartus_map
QUARTUS_FIT    = $(QUARTUS_PATH)bin/quartus_fit
QUARTUS_ASM    = $(QUARTUS_PATH)bin/quartus_asm
QUARTUS_STA    = $(QUARTUS_PATH)bin/quartus_sta
QUARTUS_SH     = $(QUARTUS_PATH)bin/quartus_sh
QUARTUS_PGM    = $(QUARTUS_PATH)bin/quartus_pgm
JTAGCONFIG     = $(QUARTUS_PATH)bin/jtagconfig
SYSTEM_CONSOLE = $(QUARTUS_PATH)sopc_builder/bin/system-console
QSYS_GEN       = $(QUARTUS_PATH)sopc_builder/bin/qsys-generate
QSYS_EDIT      = $(QUARTUS_PATH)sopc_builder/bin/qsys-edit

QUARTUS_ARGS = --no_banner

MAP_ARGS  = $(QUARTUS_ARGS) --family=$(FPGA_FAMILY)
FIT_ARGS  = $(QUARTUS_ARGS) --part=$(FPGA_DEVICE)
ASM_ARGS  = $(QUARTUS_ARGS) 
STA_ARGS  = $(QUARTUS_ARGS)
SH_ARGS   = $(QUARTUS_ARGS)
PGM_ARGS  = $(QUARTUS_ARGS)
QSYS_ARGS = --part=$(FPGA_DEVICE)