#ifndef LIB_DATAPLANE_H
#define LIB_DATAPLANE_H

#include <stdint.h>

#define HASH_RING_SIZE 8
#define MAX_REALS_CNT 32

void dataplane_add_vip(uint32_t vip, uint32_t idx);
void dataplane_update_real(uint32_t idx, uint32_t ip);
void dataplane_update_hashring(uint32_t vip_idx, int hashring[HASH_RING_SIZE]);

#endif