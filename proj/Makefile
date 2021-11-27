# Subdirectories
SUBDIRS = quartus
SUBDIRS_CLEAN = $(patsubst %,%.clean,$(SUBDIRS))
SUBDIRS_DISTCLEAN = $(patsubst %,%.distclean,$(SUBDIRS))

# Rules
all: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

$(SUBDIRS_CLEAN):
	$(MAKE) -C $(@:.clean=) clean

$(SUBDIRS_DISTCLEAN):
	$(MAKE) -C $(@:.distclean=) distclean

clean: $(SUBDIRS_CLEAN)

distclean: $(SUBDIRS_DISTCLEAN)

program:
	$(MAKE) -C quartus program

.PHONY: all $(SUBDIRS) $(SUBDIRS_CLEAN) $(SUBDIRS_DISTCLEAN) clean program