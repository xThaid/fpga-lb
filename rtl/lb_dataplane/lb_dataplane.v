`timescale 1ns / 1ps
`default_nettype none

module lb_dataplane (
  input  [31:0] rx_avalonst_data,
  input         rx_avalonst_valid,
  input         rx_avalonst_endofpacket,
  input         rx_avalonst_startofpacket,
  input  [1:0]  rx_avalonst_empty,
  output        rx_avalonst_ready,
  input  [5:0]  rx_avalonst_error,
  output [31:0] tx_avalonst_data,
  output        tx_avalonst_endofpacket,
  output        tx_avalonst_startofpacket,
  output        tx_avalonst_valid,
  input         tx_avalonst_ready,
  output [1:0]  tx_avalonst_empty,
  output        tx_avalonst_error,
  input         clk,
  input         reset
);

  //assign rx_avalonst_ready = 1'b0;
  //assign tx_avalonst_valid = 1'b0;
  //assign tx_avalonst_data = 32'b00000000000000000000000000000000;
  assign tx_avalonst_startofpacket = 1'b0;
  assign tx_avalonst_endofpacket = 1'b0;
  assign tx_avalonst_empty = 2'b00;

  arp_cache arp_cache_0(
    .clk(clk),
    .rst(reset),
    .query_req_ready_o(rx_avalonst_ready),
    .query_req_valid_i(rx_avalonst_valid),
    .query_ip_i(rx_avalonst_data),
    .query_resp_ready_i(tx_avalonst_ready),
    .query_resp_valid_o(tx_avalonst_valid),
    .query_mac_o(tx_avalonst_data),
    .query_err_o(tx_avalonst_error)
);

endmodule
