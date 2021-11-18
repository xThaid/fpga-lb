include ../config.mk

SYN_FILES_REL = $(patsubst %, ../../%, $(SYN_FILES))
QSF_FILES_REL = $(patsubst %, ../../%, $(QSF_FILES))
SDC_FILES_REL = $(patsubst %, ../../%, $(SDC_FILES))

ASSIGNMENT_FILES = $(FPGA_TOP).qpf $(FPGA_TOP).qsf

BITSTREAM_FILE := $(FPGA_TOP).sof
ifdef REV
BITSTREAM_FILE := ../rev/$(FPGA_TOP)_rev$(REV).sof
endif

ifeq ($(BOARD), DE1SoC)
include ../build/board.de1soc.mk
endif

ifeq ($(BOARD), DE2-115)
include ../build/board.de2-115.mk
endif

###################################################################
# Main Targets
###################################################################

all: fpga

quartus: $(FPGA_TOP).qpf
	quartus $(FPGA_TOP).qpf

fpga: $(FPGA_TOP).sof

program: fpga
ifeq ($(REMOTE_JTAG), 1)
	$(JTAGCONFIG) --addserver $(REMOTE_JTAG_SERVER_NAME) $(REMOTE_JTAG_SERVER_PASS)
endif
	$(QUARTUS_PGM) $(PGM_ARGS) --mode=jtag -o $(PROG_OP)

clean:
	rm -rf *.rpt *.summary *.smsg *.chg smart.log *.htm *.eqn *.pin *.sof *.pof *.qsf *.qpf *.jdi *.sld *.txt *.qws *.done *.cdf db incremental_db greybox_tmp reconfig_mif

map: smart.log $(FPGA_TOP).map.rpt
fit: smart.log $(FPGA_TOP).fit.rpt
asm: smart.log $(FPGA_TOP).asm.rpt
sta: smart.log $(FPGA_TOP).sta.rpt
smart: smart.log

.PHONY: all quartus fpga program clean map fit asm sta smart

###################################################################
# Executable Configuration
###################################################################

QUARTUS_MAP  = $(QUARTUS_PATH)quartus_map
QUARTUS_FIT  = $(QUARTUS_PATH)quartus_fit
QUARTUS_ASM  = $(QUARTUS_PATH)quartus_asm
QUARTUS_STA  = $(QUARTUS_PATH)quartus_sta
QUARTUS_SH   = $(QUARTUS_PATH)quartus_sh
QUARTUS_PGM  = $(QUARTUS_PATH)quartus_pgm
JTAGCONFIG   = $(QUARTUS_PATH)jtagconfig

QUARTUS_ARGS = --no_banner

MAP_ARGS = $(QUARTUS_ARGS) --family=$(FPGA_FAMILY)
FIT_ARGS = $(QUARTUS_ARGS) --part=$(FPGA_DEVICE)
ASM_ARGS = $(QUARTUS_ARGS) 
STA_ARGS = $(QUARTUS_ARGS)
SH_ARGS  = $(QUARTUS_ARGS)
PGM_ARGS = $(QUARTUS_ARGS)

###################################################################
# Target implementations
###################################################################

STAMP = echo done >

%.map.rpt: map.chg $(SYN_FILES_REL)
	$(QUARTUS_MAP) $(MAP_ARGS) $(FPGA_TOP)

%.fit.rpt: fit.chg %.map.rpt
	$(QUARTUS_FIT) $(FIT_ARGS) $(FPGA_TOP)

%.sta.rpt: sta.chg %.fit.rpt
	$(QUARTUS_STA) $(STA_ARGS) $(FPGA_TOP)

%.asm.rpt: asm.chg %.sta.rpt
	$(QUARTUS_ASM) $(ASM_ARGS) $(FPGA_TOP)
	mkdir -p ../rev
	EXT=sof; COUNT=1; \
	while [ -e ../rev/$*_rev$$COUNT.$$EXT ]; \
	do COUNT=$$((COUNT+1)); done; \
	cp $*.$$EXT ../rev/$*_rev$$COUNT.$$EXT; \
	echo "Output: ../rev/$*_rev$$COUNT.$$EXT";

%.sof: smart.log %.asm.rpt
	

smart.log: $(ASSIGNMENT_FILES)
	$(QUARTUS_SH) $(SH_ARGS) --determine_smart_action $(FPGA_TOP) > smart.log

.PRECIOUS: %.sof %.map.rpt %.fit.rpt %.asm.rpt %.sta.rpt

###################################################################
# Project initialization
###################################################################

$(ASSIGNMENT_FILES): $(QSF_FILES_REL) $(SYN_FILES_REL)
	rm -f $(FPGA_TOP).qsf
	$(QUARTUS_SH) $(SH_ARGS) --prepare -f $(FPGA_FAMILY) -d $(FPGA_DEVICE) -t $(FPGA_TOP) $(FPGA_TOP)
	echo >> $(FPGA_TOP).qsf
	echo >> $(FPGA_TOP).qsf
	echo "# Source files" >> $(FPGA_TOP).qsf
	for x in $(SYN_FILES_REL); do \
		case $${x##*.} in \
			v|V) echo set_global_assignment -name VERILOG_FILE $$x >> $(FPGA_TOP).qsf ;;\
			vhd|VHD) echo set_global_assignment -name VHDL_FILE $$x >> $(FPGA_TOP).qsf ;;\
			qip|QIP) echo set_global_assignment -name QIP_FILE $$x >> $(FPGA_TOP).qsf ;;\
			*) echo set_global_assignment -name SOURCE_FILE $$x >> $(FPGA_TOP).qsf ;;\
		esac; \
	done
	echo >> $(FPGA_TOP).qsf
	echo "# SDC files" >> $(FPGA_TOP).qsf
	for x in $(SDC_FILES_REL); do echo set_global_assignment -name SDC_FILE $$x >> $(FPGA_TOP).qsf; done
	for x in $(QSF_FILES_REL); do printf "\n#\n# Included QSF file $$x\n#\n" >> $(FPGA_TOP).qsf; cat $$x >> $(FPGA_TOP).qsf; done

map.chg:
	$(STAMP) map.chg
fit.chg:
	$(STAMP) fit.chg
sta.chg:
	$(STAMP) sta.chg
asm.chg:
	$(STAMP) asm.chg

