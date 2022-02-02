#include "dataplane.h"

#include "io.h"

void dataplane_add_vip(uint32_t vip, uint32_t idx) {
    DATAPLANE.balancer.vip_map_vip = vip;
    asm volatile("" ::: "memory");
    DATAPLANE.balancer.vip_map_idx = idx;
}

void dataplane_update_real(uint32_t idx, uint32_t ip) {
    DATAPLANE.balancer.reals_map_data = ip;
    asm volatile("" ::: "memory");
    DATAPLANE.balancer.reals_map_addr = idx;
}

void dataplane_update_hashring(uint32_t vip_idx, const uint8_t* hashring) {
    for (int i = 0; i < HASH_RING_SIZE; i++) {
        DATAPLANE.balancer.hash_ring_data = hashring[i];
        asm volatile("" ::: "memory");
        DATAPLANE.balancer.hash_ring_addr = vip_idx * HASH_RING_SIZE + i;
    }
}

dataplane_stats_t dataplane_get_stats(int real_idx) {
    volatile pkt_stats* stats;
    if (real_idx == -1)
        stats = &DATAPLANE.balancer.stats;
    else
        stats = &DATAPLANE.balancer.per_real_stats[real_idx];

    dataplane_stats_t ret;
    ret.pkt_cnt = stats->pkt_cnt;
    ret.bytes_cnt = (((uint64_t) stats->bytes_cnt_hi) << 32) | stats->bytes_cnt_lo;

    return ret;
}

