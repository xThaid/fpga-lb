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

void dataplane_update_hashring(uint32_t vip_idx, int hashring[HASH_RING_SIZE]) {
    for (int i = 0; i < HASH_RING_SIZE; i++) {
        DATAPLANE.balancer.hash_ring_data = hashring[i];
        asm volatile("" ::: "memory");
        DATAPLANE.balancer.hash_ring_addr = vip_idx * HASH_RING_SIZE + i;
    }
}

