`timescale 1ns / 1ps

module lb_dataplane (
  input  wire [31:0] rx_avalonst_data,
  input  wire        rx_avalonst_valid,
  input  wire        rx_avalonst_endofpacket,
  input  wire        rx_avalonst_startofpacket,
  input  wire [1:0]  rx_avalonst_empty,
  output wire        rx_avalonst_ready,
  input  wire [5:0]  rx_avalonst_error,
  output wire [31:0] tx_avalonst_data,
  output wire        tx_avalonst_endofpacket,
  output wire        tx_avalonst_startofpacket,
  output wire        tx_avalonst_valid,
  input  wire        tx_avalonst_ready,
  output wire [1:0]  tx_avalonst_empty,
  output wire        tx_avalonst_error,
  input  wire        clk,
  input  wire        reset
);

  assign rx_avalonst_ready = 1'b0;
  assign tx_avalonst_valid = 1'b0;
  assign tx_avalonst_data = 32'b00000000000000000000000000000000;
  assign tx_avalonst_startofpacket = 1'b0;
  assign tx_avalonst_endofpacket = 1'b0;
  assign tx_avalonst_empty = 2'b00;

endmodule
