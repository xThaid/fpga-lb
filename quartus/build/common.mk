include $(TOPDIR)/quartus/config.mk

ifeq ($(BOARD), DE1SoC)
include $(TOPDIR)/quartus/build/board.de1soc.mk
endif

ifeq ($(BOARD), DE2-115)
include $(TOPDIR)/quartus/build/board.de2-115.mk
endif

include $(TOPDIR)/quartus/build/tools.mk

# Pass "VERBOSE=1" at command line to display command being invoked by GNU Make
ifneq ($(VERBOSE), 1)
.SILENT:
endif
