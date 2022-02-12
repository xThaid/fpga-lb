create_clock -period 20.000ns -name {CLOCK_50} [get_ports CLOCK_50]
create_clock -period 20.000ns -name {CLOCK2_50} [get_ports CLOCK2_50]
create_clock -period 20.000ns -name {CLOCK3_50} [get_ports CLOCK3_50]
create_clock -period 40.000ns -name {ENETCLK_25} [get_ports {ENETCLK_25}]

# for enhancing USB BlasterII to be reliable, 25MHz
create_clock -name {altera_reserved_tck} -period 40.000ns {altera_reserved_tck}
set_input_delay -clock altera_reserved_tck -clock_fall 3 [get_ports altera_reserved_tdi]
set_input_delay -clock altera_reserved_tck -clock_fall 3 [get_ports altera_reserved_tms]
set_output_delay -clock altera_reserved_tck 3 [get_ports altera_reserved_tdo]

# Ethernet MDIO interface
set_output_delay  -clock [get_clocks CLOCK_50] 2   [get_ports ENET0_MDC]
set_input_delay   -clock [get_clocks CLOCK_50] 2   [get_ports ENET0_MDIO]
set_output_delay  -clock [get_clocks CLOCK_50] 2   [get_ports ENET0_MDIO]

set_output_delay  -clock [get_clocks CLOCK_50] 2   [get_ports ENET1_MDC]
set_input_delay   -clock [get_clocks CLOCK_50] 2   [get_ports ENET1_MDIO]
set_output_delay  -clock [get_clocks CLOCK_50] 2   [get_ports ENET1_MDIO]

set_false_path -from [get_ports ENET0_INT_N] -to *
set_false_path -from * -to [get_ports ENET0_RST_N]

set_false_path -from [get_ports ENET1_INT_N] -to *
set_false_path -from * -to [get_ports ENET1_RST_N]

set_false_path -from [get_ports SW[*]]
set_false_path -from [get_ports KEY[*]]
set_false_path -to [get_ports LEDR[*]]
set_false_path -to [get_ports LEDG[*]]
set_false_path -to [get_ports HEX0[*]]
set_false_path -to [get_ports HEX1[*]]
set_false_path -to [get_ports HEX2[*]]
set_false_path -to [get_ports HEX3[*]]
set_false_path -to [get_ports HEX4[*]]
set_false_path -to [get_ports HEX5[*]]
set_false_path -to [get_ports HEX6[*]]
set_false_path -to [get_ports HEX7[*]]

derive_pll_clocks
derive_clock_uncertainty

source ../dcs/rgmii_io.sdc

constrain_rgmii_input_pins "enet0" "ENET0_RX_CLK" "ENET0_RX_DV ENET0_RX_D*"
constrain_rgmii_output_pins "enet0" "pll_0|altpll_component|auto_generated|pll1|clk[1]" "ENET0_GTX_CLK" "ENET0_TX_EN ENET0_TX_D*"

constrain_rgmii_input_pins "enet1" "ENET1_RX_CLK" "ENET1_RX_DV ENET1_RX_D*"
constrain_rgmii_output_pins "enet1" "pll_0|altpll_component|auto_generated|pll1|clk[1]" "ENET1_GTX_CLK" "ENET1_TX_EN ENET1_TX_D*"
