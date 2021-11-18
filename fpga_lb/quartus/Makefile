all: build

build:
	$(MAKE) -C project fpga

program:
	$(MAKE) -C project program

quartus:
	$(MAKE) -C project quartus

clean:
	$(MAKE) -C project clean

distclean: clean
	rm -rf rev

.PHONY: all build program quartus clean distclean
