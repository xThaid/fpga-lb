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

  wire sys_clk, clk125, clk25, clk2m5;
  wire tx_clk;

  wire pll_rst = ~KEY[3];
  wire pll_rst_locked;
  wire sys_rst, sys_rst_n;

  wire mdio0_in, mdio0_out, mdio0_oen;

  assign tx_clk = clk_125;

  assign sys_rst_n = !sys_rst;

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
    .sigout (sys_rst),
  );

  lb_system u0 (
    .clk_clk                (sys_clk),
    .mac_0_mdio_mdc         (ENET0_MDC),
    .mac_0_mdio_mdio_in     (mdio0_in),
    .mac_0_mdio_mdio_out    (mdio0_out),
    .mac_0_mdio_mdio_oen    (mdio0_oen),
    .mac_0_pcs_rx_clk_clk   (ENET0_RX_CLK),
    .mac_0_pcs_tx_clk_clk   (tx_clk),
    .mac_0_rgmii_rgmii_in   (ENET0_RX_DATA),
    .mac_0_rgmii_rgmii_out  (ENET0_TX_DATA),
    .mac_0_rgmii_rx_control (ENET0_RX_DV),
    .mac_0_rgmii_tx_control (ENET0_TX_EN),
    .mac_0_status_set_10    (1'b0),
    .mac_0_status_set_1000  (1'b0),
    .mac_0_status_eth_mode  (),
    .mac_0_status_ena_10    (),
    .reset_reset_n          (sys_rst_n)
);

endmodule
