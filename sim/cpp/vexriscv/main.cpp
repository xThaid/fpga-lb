#include <stdlib.h>
#include "VVexRiscvCpu.h"
#include <verilated_vcd_c.h>
#include "verilated.h"

#include "memory.h"
#include "memctrl.h"
#include "sim.h"

#include <iostream>
#include <vector>

class VexRiscv : public SimElement {
public:
    ForwardingOutputSignal<uint8_t> iBusRead;
    ForwardingOutputSignal<uint32_t> iBusAddress;
    ForwardingInputSignal<uint8_t> iBusDataValid;
    ForwardingInputSignal<uint32_t> iBusData;
    ForwardingInputSignal<uint8_t> iBusWaitRequestn;
    ForwardingInputSignal<uint8_t> iBusResponse;

    ForwardingOutputSignal<uint8_t> dBusRead;
    ForwardingOutputSignal<uint32_t> dBusAddress;
    ForwardingOutputSignal<uint8_t> dBusWrite;
    ForwardingOutputSignal<uint8_t> dBusByteEnable;
    ForwardingOutputSignal<uint32_t> dBusWriteData;
    ForwardingInputSignal<uint8_t> dBusDataValid;
    ForwardingInputSignal<uint32_t> dBusData;
    ForwardingInputSignal<uint8_t> dBusWaitRequestn;
    ForwardingInputSignal<uint8_t> dBusResponse;

    VexRiscv(VVexRiscvCpu* vex) :
        iBusRead(&vex->iBusAvalon_read),
        iBusAddress(&vex->iBusAvalon_address),
        iBusDataValid(&vex->iBusAvalon_readDataValid),
        iBusData(&vex->iBusAvalon_readData),
        iBusWaitRequestn(&vex->iBusAvalon_waitRequestn),
        iBusResponse(&vex->iBusAvalon_response),
        dBusRead(&vex->dBusAvalon_read),
        dBusAddress(&vex->dBusAvalon_address),
        dBusWrite(&vex->dBusAvalon_write),
        dBusByteEnable(&vex->dBusAvalon_byteEnable),
        dBusWriteData(&vex->dBusAvalon_writeData),
        dBusDataValid(&vex->dBusAvalon_readDataValid),
        dBusData(&vex->dBusAvalon_readData),
        dBusWaitRequestn(&vex->dBusAvalon_waitRequestn),
        dBusResponse(&vex->dBusAvalon_response),
        top(vex)
    {
        Verilated::traceEverOn(true);
        opentrace("waveform.vcd");

        inSig(&iBusDataValid);
        inSig(&iBusData);
        inSig(&iBusWaitRequestn);
        inSig(&iBusResponse);

        inSig(&dBusDataValid);
        inSig(&dBusData);
        inSig(&dBusWaitRequestn);
        inSig(&dBusResponse);
    }

    ~VexRiscv() {
        closetrace();
        delete top;
    }

    void reset() {
        top->clk = 0;
        top->eval();
        dumpVariables(0);
    }

    void cycle() {
        m_tickcount++;
        top->clk = 1;
        top->eval();
        dumpVariables((vluint64_t)(10 * m_tickcount));
    }

    void postCycle() {
        top->eval();
        dumpVariables((vluint64_t)(10 * m_tickcount + 1));
        top->clk = 0;
        top->eval();
        dumpVariables((vluint64_t)(10 * m_tickcount + 5));
    }

private:
    VVexRiscvCpu* top;
    VerilatedVcdC* m_trace;
    uint64_t m_tickcount;

    void opentrace(const char *vcdname) {
        if (!m_trace) {
            m_trace = new VerilatedVcdC();
            top->trace(m_trace, 99);
            m_trace->open(vcdname);
        }
    }

    void closetrace() {
        if (m_trace) {
            m_trace->close();
            delete m_trace;
            m_trace = NULL;
        }
    }

    void dumpVariables(vluint64_t time) {
        if (m_trace) {
            m_trace->dump(time);
        }
    }
};

class Simulation {
public:

    Simulation() {
        mem = new Memory();
        mem->loadHexFile("../../../sw/build/fpga_lb.hex");

        vex = new VexRiscv(new VVexRiscvCpu());
        ibus = new IBusCtrl(mem);
        dbus = new DBusCtrl(mem);

        ibus->avalonRead.assign(&vex->iBusRead);
        ibus->avalonAddress.assign(&vex->iBusAddress);

        dbus->avalonRead.assign(&vex->dBusRead);
        dbus->avalonAddress.assign(&vex->dBusAddress);
        dbus->avalonWrite.assign(&vex->dBusWrite);
        dbus->avalonWriteData.assign(&vex->dBusWriteData);
        dbus->avalonByteEnable.assign(&vex->dBusByteEnable);
        
        vex->iBusData.assign(&ibus->avalonData);
        vex->iBusDataValid.assign(&ibus->avalonDataValid);
        vex->iBusWaitRequestn.assign(&ibus->avalonWaitRequestn);
        vex->iBusResponse.assign(&ibus->avalonResponse);

        vex->dBusData.assign(&dbus->avalonData);
        vex->dBusDataValid.assign(&dbus->avalonDataValid);
        vex->dBusWaitRequestn.assign(&dbus->avalonWaitRequestn);
        vex->dBusResponse.assign(&dbus->avalonResponse);

        simElements.push_back(vex);
        simElements.push_back(ibus);
        simElements.push_back(dbus);

        propagateSignals();
    }

    ~Simulation() {
        delete vex;
        delete ibus;
    }

    void start() {
        for (auto& simel : simElements) {
            simel->reset();
        }
    }

    void step() {
        for (auto& simel : simElements) simel->cycle();
        propagateSignals();
        for (auto& simel : simElements) simel->postCycle();
        propagateSignals();
    }

    void propagateSignals() {
        for (auto& simel : simElements) {
            simel->propagateSignals();
        }
    }

    void step_many(int cnt) {
        for (int i = 0; i < cnt; i++)
            step();
    }

private:
    std::vector<SimElement*> simElements;

    Memory *mem;
    VexRiscv* vex;
    IBusCtrl *ibus;
    DBusCtrl *dbus;
};

int main(int argc, char **argv) {
	Verilated::commandArgs(argc, argv);

    Simulation* sim = new Simulation();
    sim->start();
    sim->step_many(150000);

    delete sim;
    
    return 0;
}
