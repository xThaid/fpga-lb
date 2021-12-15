TOPDIR = $(realpath ../../../..)

include comp.mk

include $(TOPDIR)/quartus/build/qsys-component.mk

vexriscv:
	$(MAKE) -C $(TOPDIR)/spinal

$(TOPDIR)/spinal/VexRiscvCpu.v: vexriscv

.PHONY: vexriscv
