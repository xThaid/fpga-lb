#ifndef MEMORY_H
#define MEMORY_H

#include "bus.h"

#include <string>
#include <cstdint>

class Memory : public MemBusSlave {
public:
    uint8_t* mem[1 << 12];

    Memory(uint32_t baseAddr, uint32_t endAddr);
    ~Memory();

    uint32_t read(uint32_t addr);
    void write(uint32_t addr, uint32_t data, uint8_t mask);

    uint8_t* get(uint32_t address);
    void read(uint32_t address, uint32_t length, uint8_t *data);
    void write(uint32_t address, uint32_t length, uint8_t *data);
    void write(uint32_t address, uint8_t mask, uint8_t* data);
    uint8_t& operator [](uint32_t address);

    void loadHexFile(const std:: string& path);

private:

};

class JtagUART : public MemBusSlave {
public:
    JtagUART(uint32_t baseAddr, uint32_t endAddr);

    uint32_t read(uint32_t addr);
    void write(uint32_t addr, uint32_t data, uint8_t mask);
};

class MTimer : public MemBusSlave {
public:
    MTimer(uint32_t baseAddr, uint32_t endAddr);

    uint32_t read(uint32_t addr);
    void write(uint32_t addr, uint32_t data, uint8_t mask);
};

class GPIOControl : public MemBusSlave {
public:
    GPIOControl(uint32_t baseAddr, uint32_t endAddr);

    uint32_t read(uint32_t addr);
    void write(uint32_t addr, uint32_t data, uint8_t mask);
};

class TSE : public MemBusSlave {
public:
    TSE(uint32_t baseAddr, uint32_t endAddr);

    uint32_t read(uint32_t addr);
    void write(uint32_t addr, uint32_t data, uint8_t mask);
};


#endif