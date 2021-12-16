`timescale 1ns / 1ps
`default_nettype none

module riscv_mtime (
  input  wire [31:0] avalon_writedata,
  input  wire  [1:0] avalon_address,
  output wire [31:0] avalon_readdata,
  input  wire        avalon_write,
  input  wire        avalon_read,
  input  wire        clk,
  input  wire        reset,
  output wire        irq
);

  reg [63:0] mtime, mtimecmp;
  reg [63:0] mtimecmp_next;
  wire [31:0] mtimecmp_lo, mtimecmp_hi;

  reg [31:0] readdata;

  assign mtimecmp_lo = mtimecmp[31:0];
  assign mtimecmp_hi = mtimecmp[63:32];

  assign avalon_readdata = readdata;

  assign irq = mtime >= mtimecmp;

  always @* begin
    mtimecmp_next = mtimecmp;
    if (avalon_write) begin
      if (avalon_address == 2'b10)
        mtimecmp_next = {mtimecmp_hi, avalon_writedata};
      else if (avalon_address == 2'b11)
        mtimecmp_next = {avalon_writedata, mtimecmp_lo};
    end
  end

  always @(posedge clk) begin
    if (!avalon_read)
      readdata <= 32'b0;
    else
      case (avalon_address)
        2'b00: readdata <= mtime[31:0];
        2'b01: readdata <= mtime[63:32];
        2'b10: readdata <= mtimecmp_lo;
        2'b11: readdata <= mtimecmp_hi;
      endcase
  end

  always @(posedge clk) begin
    if (reset) begin
      mtime <= 64'b0;
      mtimecmp <= 64'b0;
    end else begin
      mtime <= mtime + 64'b1;
      mtimecmp <= mtimecmp_next;
    end
  end

endmodule
