VERILOG_TOP = bin/dataplane.v

default: gen

gen: $(VERILOG_TOP)

tap: build
	sudo ip netns exec fpga_lb _build/default/test/tap/main.exe

$(VERILOG_TOP): build
	@mkdir -p bin 
	_build/default/main.exe

build:
	dune build

test:
	dune test

clean:
	rm -rf _build

distclean: clean
	rm -rf bin

.PHONY: default gen build test distclean clean gen tap
