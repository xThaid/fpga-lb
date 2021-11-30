#ifndef TESTB_H
#define TESTB_H

#include <stdio.h>
#include <stdint.h>
#include <verilated_vcd_c.h>

#define TBASSERT(TB,A) do { if (!(A)) { (TB).closetrace(); } assert(A); } while(0);

template <class Module>    
class TestBench {
public:
    Module *m_core;
    VerilatedVcdC* m_trace;
    uint64_t m_tickcount;

    TestBench() : m_trace(NULL), m_tickcount(0l) {
        m_core = new Module();
        Verilated::traceEverOn(true);
    }

    virtual ~TestBench() {
        closetrace();
        delete m_core;
        m_core = NULL;
    }

    virtual void start() {
        setclk(0);
        eval();
        dumpVariables(0);
        tick();
    }

    virtual void opentrace(const char *vcdname) {
        if (!m_trace) {
            m_trace = new VerilatedVcdC();
            m_core->trace(m_trace, 99);
            m_trace->open(vcdname);
        }
    }

    virtual void closetrace() {
        if (m_trace) {
            m_trace->close();
            delete m_trace;
            m_trace = NULL;
        }
    }

    virtual void eval() {
        m_core->eval();
    }

    void tick_many(int cnt) {
        for (int i = 0; i < cnt; i++)
            tick();
    }

    virtual void tick() {
        eval();
        dumpVariables((vluint64_t)(10 * m_tickcount + 1));
        setclk(0);
        eval();
        dumpVariables((vluint64_t)(10 * m_tickcount + 5));
        setclk(1);
        eval();
        dumpVariables((vluint64_t)(10 * m_tickcount + 10));

        m_tickcount++;
    }

    void dumpVariables(vluint64_t time) {
        if (m_trace) {
            m_trace->dump(time);
        }
    }

    virtual void setclk(int val) = 0;

    unsigned long tickcount() {
        return m_tickcount;
    }
};

#endif
