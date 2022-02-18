#ifndef CONFIG_H
#define CONFIG_H

#include <stdint.h>

const uint64_t MAC0_ADDR = 0x0F00DEADBEEFLL;
const uint64_t MAC1_ADDR = 0x0F01DEADBEEFLL;

const uint8_t VIPS[8][4] = {
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {192, 168, 0, 151},
    {192, 168, 0, 150},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0}
};

const uint8_t HASHRING[8][8] = {
    {0, 0, 0, 0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0, 0, 0, 0},
    {1, 1, 1, 1, 1, 1, 1, 1},
    {10, 11, 12, 13, 14, 15, 16, 17},
    {0, 0, 0, 0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0, 0, 0, 0},
    {0, 0, 0, 0, 0, 0, 0, 0},
};

const uint8_t REALS[32][4] = {
    {0, 0, 0, 0},
    {192, 168, 0, 100},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},

    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {192, 168, 0, 230},
    {192, 168, 0, 231},
    {192, 168, 0, 232},
    {192, 168, 0, 233},
    {192, 168, 0, 234},
    {192, 168, 0, 235},
    
    {192, 168, 0, 236},
    {192, 168, 0, 237},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},

    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0},
    {0, 0, 0, 0}
};


const uint64_t LG_MAC_ADDR = MAC1_ADDR;
const uint8_t LG_SRC_IP[4] = {192, 168, 0, 175};
const uint8_t LG_DST_IP[4] = {192, 168, 0, 150};
const uint16_t LG_SRC_PORT = 41231;
const uint16_t LG_DST_PORT = 33333;

#endif