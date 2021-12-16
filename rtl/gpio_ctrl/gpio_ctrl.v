`timescale 1ns / 1ps
`default_nettype none

module gpio_ctrl (
  input  wire [31:0] avalon_writedata,
  output wire [31:0] avalon_readdata,
  input  wire        avalon_write,
  input  wire        avalon_read,
  input  wire        clk,
  input  wire        reset,
  output wire [17:0] ledr,
  output wire [8:0]  ledg
);

  reg [31:0] led, led_next;

  reg [31:0] readdata;

  assign ledr = led[17:0];
  assign ledg = led[26:18];
  assign avalon_readdata = readdata;

  always @* begin
    led_next = led;
    if (avalon_write) begin
      led_next = avalon_writedata;
    end
  end

  always @(posedge clk) begin
    if (!avalon_read)
      readdata <= 32'b0;
    else
      readdata <= led;
  end

  always @(posedge clk) begin
    if (reset) begin
      led <= 32'b0;
    end else begin
      led <= led_next;
    end
  end

endmodule
