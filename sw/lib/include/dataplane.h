#ifndef LIB_DATAPLANE_H
#define LIB_DATAPLANE_H

#include <stdint.h>

#define MAX_VIPS_CNT 8
#define HASH_RING_SIZE 8
#define MAX_REALS_CNT 32

typedef struct {
    uint32_t pkt_cnt;
    uint64_t bytes_cnt;
} dataplane_stats_t;

void dataplane_set_mac_addr(uint64_t mac);

void dataplane_add_vip(uint32_t vip, uint32_t idx);
void dataplane_update_real(uint32_t idx, uint32_t ip);
void dataplane_update_hashring(uint32_t vip_idx, const uint8_t* hashring);

dataplane_stats_t dataplane_get_stats(int real_idx);

#endif
