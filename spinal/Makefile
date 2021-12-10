VEXRISCV_REPO = https://github.com/SpinalHDL/VexRiscv.git
VEXRISCV_DIR = VexRiscv.local

VEXRISCV_TOP = VexRiscvWithDebug

all:vexriscv-rtl

vexriscv-rtl: $(VEXRISCV_TOP).v

vexriscv-repo: .stamp.vexriscv

$(VEXRISCV_TOP).v: .stamp.vexriscv
	sbt "runMain cpu.VexRiscvWithDebug"

.stamp.vexriscv:
	git clone $(VEXRISCV_REPO) $(VEXRISCV_DIR)
	touch .stamp.vexriscv

clean:
	rm -rf .stamp.*
	rm -rf $(VEXRISCV_DIR) target project/target project/project

distclean: clean
	rm -rf $(VEXRISCV_TOP).v $(VEXRISCV_TOP).yaml

.PHONY: all vexriscv-rtl vexriscv-repo clean distclean

