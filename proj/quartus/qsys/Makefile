TOPDIR = $(realpath ../..)

include $(TOPDIR)/quartus/build/common.mk

HWCOMP-SOURCES := 

include $(patsubst %,components/%/comp.mk,$(QSYS_COMPONENTS))

QSYS_SOPCINFO_FILES = $(patsubst %, %.sopcinfo, $(QSYS_SYSTEM))
HWCOMP-FILES = $(foreach D,$(QSYS_COMPONENTS),components/$D/$D_hw.tcl)
HWCOMP-SOURCES_REL = $(patsubst %, $(TOPDIR)/%, $(HWCOMP-SOURCES))

all: qsys

qsys: $(QSYS_SOPCINFO_FILES)

qsys-edit: $(HWCOMP-FILES)
	$(QSYS_EDIT) $(QSYS_SYSTEM).qsys --search-path="$(TOPDIR)/quartus/qsys/components/**/*,$$" &

clean: $(QSYS_COMPONENTS:%=%-hw-clean)
	rm -rf *.sopcinfo
	rm -rf build .qsys_edit
	rm -rf *.tcl~

%.sopcinfo: $(HWCOMP-SOURCES_REL) $(HWCOMP-FILES) %.qsys
	@echo "[QSYS-GEN] $*.qsys"
	$(QSYS_GEN) $(QSYS_ARGS) --synthesis=VERILOG $*.qsys --output-directory=build/$* --search-path="$(TOPDIR)/quartus/qsys/components/**/*,$$"

$(HWCOMP-FILES): $(QSYS_COMPONENTS:%=%-hw-build)

%-hw-build:
	$(MAKE) -C components/$* build

%-hw-clean:
	$(MAKE) -C components/$* clean

.PHONY: all qsys qsys-edit clean
