#include "load_generator.h"

#include "io.h"

void load_generator_init(uint64_t src_mac, uint32_t src_ip, uint32_t dst_ip, uint16_t src_port, uint16_t dst_port) {
    LOAD_GENERATOR.config.mac_addr_lo = src_mac;
    LOAD_GENERATOR.config.mac_addr_hi = src_mac >> 32;

    LOAD_GENERATOR.config.src_ip = src_ip;
    LOAD_GENERATOR.config.dst_ip = dst_ip;
    LOAD_GENERATOR.config.ports = (((uint32_t) dst_port) << 16) | src_port;
}

void load_generator_set_payload_len(uint16_t len) {
    LOAD_GENERATOR.config.payload_len = len;
}

void load_generator_set_tx_period(uint32_t period) {
    LOAD_GENERATOR.config.tx_period = period;
}

void load_generator_set_enabled(int enabled) {
    if (enabled)
        LOAD_GENERATOR.config.ctrl_flags = 0x00000001;
    else
        LOAD_GENERATOR.config.ctrl_flags = 0x00000000;
}

load_generator_stats_t load_generator_get_stats(void) {
    load_generator_stats_t ret;
    ret.pkt_cnt = LOAD_GENERATOR.stats.pkt_cnt;
    ret.bytes_cnt = (((uint64_t) LOAD_GENERATOR.stats.bytes_cnt_hi) << 32) | LOAD_GENERATOR.stats.bytes_cnt_lo;

    return ret;
}

