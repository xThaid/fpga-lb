#ifndef LIB_LOAD_GENERATOR_H
#define LIB_LOAD_GENERATOR_H

#include <stdint.h>

typedef struct {
    uint32_t pkt_cnt;
    uint64_t bytes_cnt;
} load_generator_stats_t;

void load_generator_init(uint64_t src_mac, uint32_t src_ip, uint32_t dst_ip, uint16_t src_port, uint16_t dst_port);

void load_generator_set_payload_len(uint16_t len);
void load_generator_set_tx_period(uint32_t period);

void load_generator_set_enabled(int enabled);

load_generator_stats_t load_generator_get_stats(void);

#endif
