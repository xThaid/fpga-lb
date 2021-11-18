#**************************************************************
# This .sdc file is created by Terasic Tool.
# Users are recommended to modify this file to match users logic.
#**************************************************************

#**************************************************************
# Create Clock
#**************************************************************
create_clock -period 20.000ns [get_ports CLOCK_50]
create_clock -period 20.000ns [get_ports CLOCK2_50]
create_clock -period 20.000ns [get_ports CLOCK3_50]

#**************************************************************
# Create Generated Clock
#**************************************************************
derive_pll_clocks



#**************************************************************
# Set Clock Latency
#**************************************************************



#**************************************************************
# Set Clock Uncertainty
#**************************************************************
derive_clock_uncertainty



#**************************************************************
# Set Input Delay
#**************************************************************



#**************************************************************
# Set Output Delay
#**************************************************************



#**************************************************************
# Set Clock Groups
#**************************************************************



#**************************************************************
# Set False Path
#**************************************************************

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
set_false_path -to [get_ports LCD_DATA[*]]
set_false_path -to [get_ports LCD_BLON[*]]
set_false_path -to [get_ports LCD_RW[*]]
set_false_path -to [get_ports LCD_EN[*]]
set_false_path -to [get_ports LCD_RS[*]]
set_false_path -to [get_ports LCD_ON[*]]

#**************************************************************
# Set Multicycle Path
#**************************************************************



#**************************************************************
# Set Maximum Delay
#**************************************************************



#**************************************************************
# Set Minimum Delay
#**************************************************************



#**************************************************************
# Set Input Transition
#**************************************************************



#**************************************************************
# Set Load
#**************************************************************


