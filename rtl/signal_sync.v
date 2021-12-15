`timescale 1ns / 1ps
`default_nettype none

module signal_sync #(
  parameter integer Width = 1,
  parameter integer Depth = 2
)(
  input          clk,
  input  [Width-1:0] sig,
  output [Width-1:0] sigout
);

  reg [Width-1:0] buffers[0:Depth-1];

  assign sigout = buffers[Depth-1];

  integer k;

  always @(posedge clk) begin
    buffers[0] <= sig;
    for (k = 1; k < Depth; k = k + 1) begin
      buffers[k] <= buffers[k - 1];
    end
  end

endmodule