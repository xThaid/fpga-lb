CXX := g++
CXXFLAGS := -std=c++11 -Wall -Og -g -faligned-new -fcf-protection=none

VERILATOR_ROOT := /usr/local/share/verilator
VERILATOR := verilator
VSRCS := verilated.cpp verilated_vcd_c.cpp
VOBJDIR := obj_dir
VOBJS := $(patsubst %.cpp, $(VOBJDIR)/%.o, $(VSRCS))

OBJDIR := objs
EXECUTABLE := $(OBJDIR)/$(MODULE)

VERILOG_SOURCES_ABS := $(patsubst %,$(TOPDIR)/%, $(VERILOG_SOURCES))
OBJS := $(patsubst %.cpp, $(OBJDIR)/%.o, $(SOURCES))

INCS := -I$(TOPDIR)/sim/cpp/common/lib/include -Iobj_dir -I$(VERILATOR_ROOT)/include

all: test

test: sim

sim: waveform.vcd

verilate: .stamp.verilate

build: $(EXECUTABLE)

waves: waveform.vcd
	@echo "[GTKWAVE]"
	gtkwave waveform.vcd $(MODULE).gtkw

waveform.vcd: $(EXECUTABLE)
	@echo "[SIMULATION] $(MODULE)"
	@./$(EXECUTABLE)

$(EXECUTABLE): $(OBJS) $(VOBJS) $(VOBJDIR)/V$(MODULE)__ALL.a
	$(CXX) $(FLAGS) $(INCS) $^ -o $@

$(VOBJDIR)/%.o: .stamp.verilate
	make -C obj_dir -f V$(MODULE).mk $*.o

$(OBJDIR)/%.o: %.cpp .stamp.verilate
	@mkdir -p $(OBJDIR)
	$(CXX) $(CXXFLAGS) $(INCS) $< -c -o $@

$(VOBJDIR)/V$(MODULE)__ALL.a: .stamp.verilate
	@echo "[BUILD-VLIB] $(MODULE)"
	make -C obj_dir -f V$(MODULE).mk

.stamp.verilate: $(VERILOG_SOURCES_ABS)
	@echo "[VERILATOR] $(MODULE)"
	verilator --trace -cc $^
	@touch .stamp.verilate

clean:
	rm -rf ./objs ./obj_dir
	rm -rf .stamp.*
	rm -rf waveform.vcd

.PHONY: all test sim verilate build waves clean
