TOPDIR = $(realpath ../..)

include $(TOPDIR)/quartus/build/common.mk

SYN_FILES_REL = $(patsubst %, ../../%, $(SYN_FILES))
SYN_FILES_REL += ../qsys/build/$(QSYS_SYSTEM)/synthesis/$(QSYS_SYSTEM).qip
QSF_FILES_REL = $(patsubst %, ../../%, $(QSF_FILES))
SDC_FILES_REL = $(patsubst %, ../../%, $(SDC_FILES))

ASSIGNMENT_FILES = $(FPGA_TOP).qpf $(FPGA_TOP).qsf

BITSTREAM_FILE := $(FPGA_TOP).sof
ifdef REV
BITSTREAM_FILE := ../rev/$(FPGA_TOP)_rev$(REV).sof
endif

###################################################################
# Main Targets
###################################################################

all: fpga

quartus-edit: $(FPGA_TOP).qpf
	quartus $(FPGA_TOP).qpf &

fpga: $(FPGA_TOP).sof

program: fpga
ifeq ($(REMOTE_JTAG), 1)
	$(JTAGCONFIG) --addserver $(REMOTE_JTAG_SERVER_NAME) $(REMOTE_JTAG_SERVER_PASS)
endif
	$(QUARTUS_PGM) $(PGM_ARGS) --mode=jtag -o $(PROG_OP)

console:
	$(SYSTEM_CONSOLE) -cli --rc_script=../tcl/console.tcl

update_ram: sw
	$(QUARTUS_CDB) $(CDB_ARGS) $(FPGA_TOP) --update_mif
	$(STAMP) asm.chg

sw:
	$(MAKE) -C ../../sw

clean:
	rm -rf *.rpt *.summary *.smsg *.chg smart.log *.htm *.eqn *.pin *.sof *.pof *.qsf *.qpf *.jdi *.sld *.txt *.qws *.done *.cdf
	rm -rf db incremental_db greybox_tmp reconfig_mif .qsys_edit

map: smart.log $(FPGA_TOP).map.rpt
fit: smart.log $(FPGA_TOP).fit.rpt
asm: smart.log $(FPGA_TOP).asm.rpt
sta: smart.log $(FPGA_TOP).sta.rpt
smart: smart.log

.PHONY: all quartus-edit fpga program clean map fit asm sta smart update_ram sw

###################################################################
# Target implementations
###################################################################

STAMP = echo done >

%.map.rpt: map.chg $(SYN_FILES_REL)
	@echo "[QUARTUS_MAP] $@"
	$(QUARTUS_MAP) $(MAP_ARGS) $(FPGA_TOP)

%.fit.rpt: fit.chg %.map.rpt
	@echo "[QUARTUS_FIT] $@"
	$(QUARTUS_FIT) $(FIT_ARGS) $(FPGA_TOP)

%.sta.rpt: sta.chg %.fit.rpt
	@echo "[QUARTUS_STA] $@"
	$(QUARTUS_STA) $(STA_ARGS) $(FPGA_TOP)

%.asm.rpt: asm.chg %.sta.rpt
	@echo "[QUARTUS_ASM] $@"
	$(QUARTUS_ASM) $(ASM_ARGS) $(FPGA_TOP)
	if [ -f "$*_time_limited.sof" ]; then \
		mv $*_time_limited.sof $*.sof; \
	fi
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
	@echo "[PROJ_INIT] $@"
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

