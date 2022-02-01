`timescale 1ns / 1ps
`default_nettype none

module gpio_ctrl (
  input  wire  [3:0] avalon_address,
  input  wire [31:0] avalon_writedata,
  output wire [31:0] avalon_readdata,
  input  wire        avalon_write,
  input  wire        avalon_read,
  input  wire        clk,
  input  wire        reset,
  output wire [17:0] ledr,
  output wire [8:0]  ledg,
  output wire [55:0] seven_seg
);

  reg [31:0] led, led_next;
  reg [55:0] seven_seg_reg, seven_seg_next;

  reg [31:0] readdata;

  assign ledr = led[17:0];
  assign ledg = led[26:18];
  assign seven_seg = seven_seg_reg;
  assign avalon_readdata = readdata;

  always @* begin
    led_next = led;
    seven_seg_next = seven_seg_reg;
    if (avalon_write) begin
      case (avalon_address)
        4'd0 : led_next = avalon_writedata;
        4'd1 : seven_seg_next[6:0] = avalon_writedata[6:0];
        4'd2 : seven_seg_next[13:7] = avalon_writedata[6:0];
        4'd3 : seven_seg_next[20:14] = avalon_writedata[6:0];
        4'd4 : seven_seg_next[27:21] = avalon_writedata[6:0];
        4'd5 : seven_seg_next[34:28] = avalon_writedata[6:0];
        4'd6 : seven_seg_next[41:35] = avalon_writedata[6:0];
        4'd7 : seven_seg_next[48:42] = avalon_writedata[6:0];
        4'd8 : seven_seg_next[55:49] = avalon_writedata[6:0];
      endcase
    end
  end

  always @(posedge clk) begin
    if (!avalon_read)
      readdata <= 32'b0;
    else if (avalon_address == 0)
      readdata <= led;
  end

  always @(posedge clk) begin
    if (reset) begin
      led <= 32'b0;
    end else begin
      led <= led_next;
      seven_seg_reg <= seven_seg_next;
    end
  end

endmodule
