TOPDIR = $(realpath ..)

include $(TOPDIR)/quartus/build/common.mk

all: fpga

qsys:
	$(MAKE) -C qsys qsys

fpga: qsys
	$(MAKE) -C project fpga

build: qsys
	$(MAKE) -C project map

program:
	$(MAKE) -C project program

quartus-edit: qsys
	$(MAKE) -C project quartus-edit

console:
	$(MAKE) -C project console

update_ram:
	$(MAKE) -C project update_ram

clean:
	$(MAKE) -C project clean
	$(MAKE) -C qsys clean

distclean: clean
	rm -rf rev

.PHONY: all qsys fpga build program quartus-edit console clean distclean update_ram
