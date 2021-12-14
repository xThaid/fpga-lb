#ifndef MEMORY_H
#define MEMORY_H

#include <stdint.h>
#include <cstdlib>
#include <iostream>
#include <vector>
#include <fstream>

class Memory {
public:
    uint8_t* mem[1 << 12];

    Memory();
    ~Memory();

    uint8_t* get(uint32_t address);
    void read(uint32_t address, uint32_t length, uint8_t *data);
    void write(uint32_t address, uint32_t length, uint8_t *data);
    void write(uint32_t address, uint8_t mask, uint8_t* data);
    uint8_t& operator [](uint32_t address);

    void loadHexFile(const std:: string& path);

private:

};



#endif