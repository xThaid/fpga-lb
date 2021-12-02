include $(TOPDIR)/quartus/build/common.mk

HW-SOURCES_REL = $(patsubst %, $(TOPDIR)/%, $(HWCOMP-SOURCES))

build: $(HWCOMP-TOPLEVEL)_hw.tcl

clean:
	rm -rf *_hw.tcl *_hw.tcl~

%_hw.tcl: $(HW-SOURCES_REL) %.tcl
	@echo "[GEN] $@"
	rm -rf $@
	cp $*.tcl $@
	echo >> $@
	echo >> $@
	echo 'add_fileset QUARTUS_SYNTH QUARTUS_SYNTH "" ""' >> $@
	echo "set_fileset_property QUARTUS_SYNTH TOP_LEVEL $(HWCOMP-TOPLEVEL)" >> $@
	echo "set_fileset_property QUARTUS_SYNTH ENABLE_RELATIVE_INCLUDE_PATHS false" >> $@
	echo "set_fileset_property QUARTUS_SYNTH ENABLE_FILE_OVERWRITE_MODE true" >> $@
	for x in $(HW-SOURCES_REL); do \
		BASENAME=$$(basename $$x); \
		echo add_fileset_file $(HWCOMP-TOPLEVEL)/$$BASENAME VERILOG PATH $$x $$(if [ $$BASENAME = $(HWCOMP-TOPLEVEL).v ]; then echo TOP_LEVEL_FILE; fi) >> $@; \
	done


.PHONY: build clean
