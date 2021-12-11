VEXRISCV_REPO = https://github.com/SpinalHDL/VexRiscv.git
VEXRISCV_DIR = VexRiscv.local

VEXRISCV_TOP = VexRiscvCpu


all: vexriscv-rtl

vexriscv-rtl: $(VEXRISCV_TOP).v

vexriscv-repo: .stamp.vexriscv

$(VEXRISCV_TOP).v: .stamp.vexriscv src/main/scala/cpu/$(VEXRISCV_TOP).scala
	sbt "runMain cpu.$(VEXRISCV_TOP)"

.stamp.vexriscv:
	git clone $(VEXRISCV_REPO) $(VEXRISCV_DIR)
	touch .stamp.vexriscv

clean:
	rm -rf .stamp.*
	rm -rf $(VEXRISCV_DIR) target project/target project/project

distclean: clean
	rm -rf $(VEXRISCV_TOP).v cpu0.yaml $(VEXRISCV_TOP)_hw.tcl 

.PHONY: all vexriscv-rtl vexriscv-repo clean distclean

