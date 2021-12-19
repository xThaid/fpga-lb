#ifndef BUS_H
#define BUS_H

#include <cstdint>
#include <vector>
#include <string>

class MemBusSlave {
public:
    MemBusSlave(const std::string& name, uint32_t baseAddr, uint32_t endAddr);

    virtual uint32_t read(uint32_t addr) = 0;
    virtual void write(uint32_t addr, uint32_t data, uint8_t mask) = 0;

    void setLogAccess(bool logAccess);
    bool getLogAccess();

    const std::string& getName();
    uint32_t getBaseAddr();
    uint32_t getEndAddr();

private:
    std::string name;
    uint32_t baseAddr, endAddr;
    bool logAccess;
};

class MemBusCtrl {
public:
    MemBusCtrl();

    uint32_t read(uint32_t addr);
    void write(uint32_t addr, uint8_t mask, uint32_t data);

    void addSlave(MemBusSlave* slave);

private:
    std::vector<MemBusSlave*> slaves;

    MemBusSlave* findSlave(uint32_t addr);
};

#endif