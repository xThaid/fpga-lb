#include <stdlib.h>
#include "Varp_cache.h"
#include "verilated.h"

#include "testb.h"

class ArpCacheTB : public TestBench<Varp_cache> {
public:
    void setclk(int val) {
        m_core->clk = val;
    }

    void sendQuery(uint32_t ip) {
        m_core->query_ip_i = ip;
        m_core->query_req_valid_i = 1;
        tick();
        m_core->query_ip_i = 0;
        m_core->query_req_valid_i = 0;
    }

    void assertResponse(uint64_t mac, int error) {
        if (!error)
            assert(m_core->query_mac_o == mac);
        assert(m_core->query_err_o == error);
        assert(m_core->query_resp_valid_o);
    }

    void writeCache(uint32_t ip, uint64_t mac) {
        m_core->write_ip_i = ip;
        m_core->write_mac_i = mac;
        m_core->write_valid_i = 1;
        tick();
        m_core->write_ip_i = 0;
        m_core->write_mac_i = 0;
        m_core->write_valid_i = 0;
    }

    void test_simple_query() {
        m_core->query_resp_ready_i = 1;

        sendQuery(0x0002);
        tick();
        assertResponse(0x2000, 0);

        sendQuery(0xbbbb);
        tick();
        assertResponse(0x0000, 1);

        sendQuery(0xf0f0);
        tick();
        assertResponse(0xfafa, 0);
    }

    void test_pipeline_query() {
        m_core->query_resp_ready_i = 1;

        sendQuery(0x00ff);
        sendQuery(0xcccc);
        assertResponse(0x00ff, 0);
        sendQuery(0x0002);
        assertResponse(0x0000, 1);
        tick();
        assertResponse(0x2000, 0);
    }

    void test_pipeline_stall() {
        m_core->query_resp_ready_i = 0;

        sendQuery(0x00ff);
        sendQuery(0x0002);
        assert(!m_core->query_req_ready_o);
        tick_many(4);
        assert(!m_core->query_req_ready_o);

        m_core->query_resp_ready_i = 1;
        assertResponse(0x00ff, 0);
        sendQuery(0x0001);

        m_core->query_resp_ready_i = 0;
        tick_many(4);
        assertResponse(0x2000, 0);

        m_core->query_resp_ready_i = 1;
        tick();
        assertResponse(0x0001, 0);

        m_core->query_resp_ready_i = 1;
        tick();

        sendQuery(0x0001);
        m_core->query_resp_ready_i = 0;
        tick();

        assert(m_core->query_req_ready_o);
        sendQuery(0x0002);
        assert(!m_core->query_req_ready_o);
        m_core->query_resp_ready_i = 1;
        sendQuery(0x00ff);
        assertResponse(0x2000, 0);
        sendQuery(0xaaaa);
        assertResponse(0x00ff, 0);
        tick();
        assertResponse(0x0000, 1);
    }
};

int main(int argc, char **argv) {
	Verilated::commandArgs(argc, argv);

    ArpCacheTB* tb = new ArpCacheTB();
    tb->opentrace("waveform.vcd");
    tb->start();

    tb->writeCache(0x0001, 0x0001);
    tb->writeCache(0x0002, 0x2000);
    tb->writeCache(0x00ff, 0x00ff);
    tb->writeCache(0xf0f0, 0xfafa);

    tb->tick_many(2);

    tb->test_simple_query();
    tb->tick();

    tb->test_pipeline_query();
    tb->tick();

    tb->test_pipeline_stall();

    tb->tick_many(2);

    delete tb;
    
    return 0;
}
