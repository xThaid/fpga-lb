#pragma once

#include <cstdint>
#include <queue>

#include "memory.h"
#include "sim.h"
#include "signal.h"

struct IBusAvalonRsp {
    uint32_t data;
    bool error;
};

class IBusCtrl : public SimElement {
public:
    InputSignal<uint8_t> avalonRead;
    InputSignal<uint32_t> avalonAddress;
    OutputSignal<uint8_t> avalonDataValid;
    OutputSignal<uint32_t> avalonData;
    OutputSignal<uint8_t> avalonWaitRequestn;
    OutputSignal<uint8_t> avalonResponse;

    IBusCtrl(Memory* mem);

    void reset();
    void cycle();
    void postCycle();

private:
    Memory* mem;
    std::queue<IBusAvalonRsp> respQ;
};

struct DBusAvalonRsp {
	uint32_t data;
	bool error;
};

class DBusCtrl : public SimElement {
public:
    InputSignal<uint8_t> avalonRead;
    InputSignal<uint8_t> avalonWrite;
    InputSignal<uint8_t> avalonByteEnable;
    InputSignal<uint32_t> avalonAddress;
    InputSignal<uint32_t> avalonWriteData;
    OutputSignal<uint8_t> avalonDataValid;
    OutputSignal<uint32_t> avalonData;
    OutputSignal<uint8_t> avalonWaitRequestn;
    OutputSignal<uint8_t> avalonResponse;

    DBusCtrl(Memory* mem);

    void reset();
    void cycle();
    void postCycle();

private:
    Memory* mem;
    std::queue<DBusAvalonRsp> respQ;
};