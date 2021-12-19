#include "bus.h"

#include <stdexcept>
#include <cstdio>

MemBusSlave::MemBusSlave(const std::string& name, uint32_t baseAddr, uint32_t endAddr) :
    name(name),
    baseAddr(baseAddr),
    endAddr(endAddr)
{
}

void MemBusSlave::setLogAccess(bool logAccess) {
    this->logAccess = logAccess;
}

bool MemBusSlave::getLogAccess() {
    return logAccess;
}

const std::string& MemBusSlave::getName() {
    return name;
}

uint32_t MemBusSlave::getBaseAddr() {
    return baseAddr;
}

uint32_t MemBusSlave::getEndAddr() {
    return endAddr;
}

MemBusCtrl::MemBusCtrl() {

}

uint32_t MemBusCtrl::read(uint32_t addr) {
    if (addr % 4 != 0)
        throw std::invalid_argument("address is not aligned to 4");

    MemBusSlave *slave = findSlave(addr);
    if (slave == nullptr)
        throw std::invalid_argument("can't find slave for the address");

    uint32_t ret = slave->read(addr);

    if (slave->getLogAccess())
        printf("[%s] Read request address: %08x, returned data: %08x\n", slave->getName().c_str(), addr, ret);

    return ret;
}

void MemBusCtrl::write(uint32_t addr, uint8_t mask, uint32_t data) {
    if (addr % 4 != 0)
        throw std::invalid_argument("address is not aligned to 4");

    MemBusSlave *slave = findSlave(addr);
    if (slave == nullptr)
        throw std::invalid_argument("can't find slave for the address");

    if (slave->getLogAccess())
        printf("[%s] Write request address: %08x, mask: %01x, data: %08x\n", slave->getName().c_str(), addr, mask, data);

    slave->write(addr, data, mask);
}

void MemBusCtrl::addSlave(MemBusSlave *slave) {
    slaves.push_back(slave);
}

MemBusSlave* MemBusCtrl::findSlave(uint32_t addr) {
    for (auto &slave : slaves) {
        if (slave->getBaseAddr() <= addr && addr <= slave->getEndAddr())
            return slave;
    }

    return nullptr;
}