#include "periph.h"

#include <cstdlib>
#include <fstream>

Memory::Memory(uint32_t baseAddr, uint32_t endAddr) :
    MemBusSlave("On-chip SRAM", baseAddr, endAddr)
{
    for(size_t i = 0; i < (1 << 12); i++)
        mem[i] = NULL;
}

Memory::~Memory(){
    for(size_t i = 0; i < (1 << 12); i++)
        if(mem[i]) delete [] mem[i];
}

uint32_t Memory::read(uint32_t addr) {
    uint32_t ret;
    uint8_t *data = (uint8_t *)&ret;
    for(size_t i = 0; i < 4;i++){
        data[i] = (*this)[addr + i];
    }
    return ret;
}

void Memory::write(uint32_t addr, uint32_t data, uint8_t mask) {
    for (size_t i = 0; i < 4; i++) {
        if ((1 << i) & mask)
            (*this)[addr + i] = ((uint8_t *)&data)[i];
    }
}

uint8_t* Memory::get(uint32_t address){
    if(mem[address >> 20] == NULL) {
        uint8_t* ptr = new uint8_t[1024*1024];
        for(size_t i = 0; i < 1024*1024;i+=4) {
            ptr[i + 0] = 0xFF;
            ptr[i + 1] = 0xFF;
            ptr[i + 2] = 0xFF;
            ptr[i + 3] = 0xFF;
        }
        mem[address >> 20] = ptr;
    }
    return &mem[address >> 20][address & 0xFFFFF];
}

void Memory::write(uint32_t address, uint32_t length, uint8_t *data){
    for(size_t i = 0; i < length;i++){
        (*this)[address + i] = data[i];
    }
}

uint8_t& Memory::operator [](uint32_t address) {
    return *get(address);
}

static uint32_t hti(char c) {
    if (c >= 'A' && c <= 'F')
        return c - 'A' + 10;
    if (c >= 'a' && c <= 'f')
        return c - 'a' + 10;
    return c - '0';
}

static uint32_t hToI(char *c, uint32_t size) {
    uint32_t value = 0;
    for (uint32_t i = 0; i < size; i++) {
        value += hti(c[i]) << ((size - i - 1) * 4);
    }
    return value;
}

void Memory::loadHexFile(const std:: string& path) {
    std::ifstream infile;
    infile.open(path, std::ios::binary);
    infile.seekg(0, std::ios::end);

    size_t file_size_in_byte = infile.tellg();
    std::vector<char> data;
    data.resize(file_size_in_byte);

    infile.seekg(0, std::ios::beg);
    infile.read(&data[0], file_size_in_byte);

    int offset = 0;
    char* line = &data[0];
    while (1) {
        if (line[0] == ':') {
            uint32_t byteCount = hToI(line + 1, 2);
            uint32_t nextAddr = hToI(line + 3, 4) + offset;
            uint32_t key = hToI(line + 7, 2);
            switch (key) {
            case 0:
                for (size_t i = 0; i < byteCount; i++) {
                    *(get(nextAddr + i)) = hToI(line + 9 + i * 2, 2);
                }
                break;
            case 2:
                offset = hToI(line + 9, 4) << 4;
                break;
            case 4:
                offset = hToI(line + 9, 4) << 16;
                break;
            default:
                break;
            }
        }

        while (*line != '\n' && file_size_in_byte != 0) {
            line++;
            file_size_in_byte--;
        }
        if (file_size_in_byte <= 1)
            break;
        line++;
        file_size_in_byte--;
    }
}

JtagUART::JtagUART(uint32_t baseAddr, uint32_t endAddr) :
    MemBusSlave("JTAG UART", baseAddr, endAddr)
{
}

uint32_t JtagUART::read(uint32_t addr) {
    return 0x00000000;
}

void JtagUART::write(uint32_t addr, uint32_t data, uint8_t mask) {
}

MTimer::MTimer(uint32_t baseAddr, uint32_t endAddr) :
    MemBusSlave("mtimer", baseAddr, endAddr)
{
}

uint32_t MTimer::read(uint32_t addr) {
    return 0x00000000;
}

void MTimer::write(uint32_t addr, uint32_t data, uint8_t mask) {
}

GPIOControl::GPIOControl(uint32_t baseAddr, uint32_t endAddr) :
    MemBusSlave("GPIO control", baseAddr, endAddr)
{
}

uint32_t GPIOControl::read(uint32_t addr) {
    return 0x00000000;
}

void GPIOControl::write(uint32_t addr, uint32_t data, uint8_t mask) {
}

TSE::TSE(uint32_t baseAddr, uint32_t endAddr) :
    MemBusSlave("TSE", baseAddr, endAddr)
{
}

uint32_t TSE::read(uint32_t addr) {
    return 0x00000000;
}

void TSE::write(uint32_t addr, uint32_t data, uint8_t mask) {
}

Dataplane::Dataplane(uint32_t baseAddr, uint32_t endAddr) :
    MemBusSlave("Dataplane", baseAddr, endAddr)
{
}

uint32_t Dataplane::read(uint32_t addr) {
    return 0x00000000;
}

void Dataplane::write(uint32_t addr, uint32_t data, uint8_t mask) {
}

LoadGenerator::LoadGenerator(uint32_t baseAddr, uint32_t endAddr) :
    MemBusSlave("Load generator", baseAddr, endAddr)
{
}

uint32_t LoadGenerator::read(uint32_t addr) {
    return 0x00000000;
}

void LoadGenerator::write(uint32_t addr, uint32_t data, uint8_t mask) {
}

