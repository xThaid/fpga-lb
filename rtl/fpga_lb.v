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

  wire sys_clk, clk_125, dataplane_clk, clk_2m5;
  wire tx_clk;

  wire pll_rst = ~KEY[3];
  wire pll_rst_locked;
  wire sys_rst, sys_rst_n;

  wire mdio0_in, mdio0_out, mdio0_oen;
  wire mdio1_in, mdio1_out, mdio1_oen;

  assign tx_clk = clk_125;

  assign mdio0_in = ENET0_MDIO;
  assign ENET0_MDIO = mdio0_oen ? 1'bz : mdio0_out;
  
  assign ENET0_GTX_CLK = tx_clk;
  assign ENET0_RST_N = pll_rst_locked;

  assign mdio1_in = ENET1_MDIO;
  assign ENET1_MDIO = mdio1_oen ? 1'bz : mdio1_out;
  
  assign ENET1_GTX_CLK = tx_clk;
  assign ENET1_RST_N = pll_rst_locked;

  pll pll_0 (
    .areset (pll_rst),
    .inclk0 (CLOCK_50),
    .c0     (sys_clk),
    .c1     (clk_125),
    .c2     (dataplane_clk),
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
    .clk_clk                  (sys_clk),
    .reset_reset_n            (pll_rst_locked),

    .dataplane_clk_clk        (dataplane_clk),
    .dataplane_reset_reset_n  (pll_rst_locked),

    .mac_0_mdio_mdc           (ENET0_MDC),
    .mac_0_mdio_mdio_in       (mdio0_in),
    .mac_0_mdio_mdio_out      (mdio0_out),
    .mac_0_mdio_mdio_oen      (mdio0_oen),
    .mac_0_pcs_rx_clk_clk     (ENET0_RX_CLK),
    .mac_0_pcs_tx_clk_clk     (tx_clk),
    .mac_0_rgmii_rgmii_in     (ENET0_RX_DATA),
    .mac_0_rgmii_rgmii_out    (ENET0_TX_DATA),
    .mac_0_rgmii_rx_control   (ENET0_RX_DV),
    .mac_0_rgmii_tx_control   (ENET0_TX_EN),
    .mac_0_status_set_10      (1'b0),
    .mac_0_status_set_1000    (1'b0),
    .mac_0_status_eth_mode    (),
    .mac_0_status_ena_10      (),
    .mac_0_misc_ff_tx_crc_fwd (),
    .mac_0_misc_ff_tx_septy   (),
    .mac_0_misc_tx_ff_uflow   (),
    .mac_0_misc_ff_tx_a_full  (),
    .mac_0_misc_ff_tx_a_empty (),
    .mac_0_misc_rx_err_stat   (),
    .mac_0_misc_rx_frm_type   (),
    .mac_0_misc_ff_rx_dsav    (),
    .mac_0_misc_ff_rx_a_full  (),
    .mac_0_misc_ff_rx_a_empty (),

    .mac_1_mdio_mdc           (ENET1_MDC),
    .mac_1_mdio_mdio_in       (mdio1_in),
    .mac_1_mdio_mdio_out      (mdio1_out),
    .mac_1_mdio_mdio_oen      (mdio1_oen),
    .mac_1_pcs_rx_clk_clk     (ENET1_RX_CLK),
    .mac_1_pcs_tx_clk_clk     (tx_clk),
    .mac_1_rgmii_rgmii_in     (ENET1_RX_DATA),
    .mac_1_rgmii_rgmii_out    (ENET1_TX_DATA),
    .mac_1_rgmii_rx_control   (ENET1_RX_DV),
    .mac_1_rgmii_tx_control   (ENET1_TX_EN),
    .mac_1_status_set_10      (1'b0),
    .mac_1_status_set_1000    (1'b0),
    .mac_1_status_eth_mode    (),
    .mac_1_status_ena_10      (),
    .mac_1_misc_ff_tx_crc_fwd (),
    .mac_1_misc_ff_tx_septy   (),
    .mac_1_misc_tx_ff_uflow   (),
    .mac_1_misc_ff_tx_a_full  (),
    .mac_1_misc_ff_tx_a_empty (),
    .mac_1_misc_rx_err_stat   (),
    .mac_1_misc_rx_frm_type   (),
    .mac_1_misc_ff_rx_dsav    (),
    .mac_1_misc_ff_rx_a_full  (),
    .mac_1_misc_ff_rx_a_empty (),

    .gpio_ledr                (LEDR),
    .gpio_ledg                (LEDG),
    .gpio_seven_seg           ({HEX0, HEX1, HEX2, HEX3, HEX4, HEX5, HEX6, HEX7})
  );

endmodule
