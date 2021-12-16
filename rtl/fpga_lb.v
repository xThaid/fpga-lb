`timescale 1ns / 1ps
`default_nettype none

module fpga_lb(

  //////////// CLOCK //////////
  input               CLOCK_50,

  //////////// LED //////////
  output       [8:0]  LEDG,
  output      [17:0]  LEDR,

  //////////// KEY //////////
  input        [3:0]  KEY,

  //////////// SW //////////
  input       [17:0]  SW,

  //////////// SEG7 //////////
  output       [6:0]  HEX0,
  output       [6:0]  HEX1,
  output       [6:0]  HEX2,
  output       [6:0]  HEX3,
  output       [6:0]  HEX4,
  output       [6:0]  HEX5,
  output       [6:0]  HEX6,
  output       [6:0]  HEX7,

  //////////// Ethernet 0 //////////
  output              ENET0_GTX_CLK,
  input               ENET0_INT_N,
  input               ENET0_LINK100,
  output              ENET0_MDC,
  inout               ENET0_MDIO,
  output              ENET0_RST_N,
  input               ENET0_RX_CLK,
  input        [3:0]  ENET0_RX_DATA,
  input               ENET0_RX_DV,
  output       [3:0]  ENET0_TX_DATA,
  output              ENET0_TX_EN,

  //////////// Ethernet 1 //////////
  output              ENET1_GTX_CLK,
  input               ENET1_INT_N,
  input               ENET1_LINK100,
  output              ENET1_MDC,
  inout               ENET1_MDIO,
  output              ENET1_RST_N,
  input               ENET1_RX_CLK,
  input        [3:0]  ENET1_RX_DATA,
  input               ENET1_RX_DV,
  output       [3:0]  ENET1_TX_DATA,
  output              ENET1_TX_EN
);

  wire sys_clk, clk_125, clk_25, clk_2m5;
  wire tx_clk;

  wire pll_rst = ~KEY[3];
  wire pll_rst_locked;
  wire sys_rst, sys_rst_n;

  wire mdio0_in, mdio0_out, mdio0_oen;

  assign tx_clk = clk_125;

  assign mdio0_in = ENET0_MDIO;
  assign ENET0_MDIO = mdio0_oen ? 1'bz : mdio0_out;
  
  assign ENET0_GTX_CLK = tx_clk;

  pll pll_0 (
    .areset (pll_rst),
    .inclk0 (CLOCK_50),
    .c0     (sys_clk),
    .c1     (clk_125),
    .c2     (clk_25),
    .c3     (clk_2m5),
    .locked (pll_rst_locked)
  );

  signal_sync #(
    .Depth(4)
  ) signal_sync_rst (
    .clk    (sys_clk),
    .sig    (pll_rst_locked),
    .sigout (sys_rst_n),
  );

  lb_system u0 (
    .clk_clk                (sys_clk),
    .reset_reset_n          (pll_rst_locked),
    .gpio_ledr              (LEDR),
    .gpio_ledg              (LEDG)
  );

endmodule
