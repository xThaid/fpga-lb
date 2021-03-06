# TCL File Generated by Component Editor 20.1
# Sat Nov 27 20:18:51 CET 2021
# DO NOT MODIFY


# 
# lb_dataplane "lb_dataplane" v1.0
#  2021.11.27.20:18:51
# 
# 

# 
# request TCL package from ACDS 16.1
# 
package require -exact qsys 16.1


# 
# module lb_dataplane
# 
set_module_property DESCRIPTION ""
set_module_property NAME lb_dataplane
set_module_property VERSION 1.0
set_module_property INTERNAL false
set_module_property OPAQUE_ADDRESS_MAP true
set_module_property AUTHOR ""
set_module_property DISPLAY_NAME lb_dataplane
set_module_property INSTANTIATE_IN_SYSTEM_MODULE true
set_module_property EDITABLE true
set_module_property REPORT_TO_TALKBACK false
set_module_property ALLOW_GREYBOX_GENERATION false
set_module_property REPORT_HIERARCHY false


# 
# parameters
# 


# 
# display items
# 


# 
# connection point rx_avalonst
# 
add_interface rx_avalonst avalon_streaming end
set_interface_property rx_avalonst associatedClock clk
set_interface_property rx_avalonst associatedReset reset
set_interface_property rx_avalonst dataBitsPerSymbol 8
set_interface_property rx_avalonst errorDescriptor ""
set_interface_property rx_avalonst firstSymbolInHighOrderBits true
set_interface_property rx_avalonst maxChannel 0
set_interface_property rx_avalonst readyLatency 0
set_interface_property rx_avalonst ENABLED true
set_interface_property rx_avalonst EXPORT_OF ""
set_interface_property rx_avalonst PORT_NAME_MAP ""
set_interface_property rx_avalonst CMSIS_SVD_VARIABLES ""
set_interface_property rx_avalonst SVD_ADDRESS_GROUP ""

add_interface_port rx_avalonst rx_data data Input 32
add_interface_port rx_avalonst rx_valid valid Input 1
add_interface_port rx_avalonst rx_endofpacket endofpacket Input 1
add_interface_port rx_avalonst rx_startofpacket startofpacket Input 1
add_interface_port rx_avalonst rx_empty empty Input 2
add_interface_port rx_avalonst rx_ready ready Output 1

# 
# connection point tx_avalonst
# 
add_interface tx_avalonst avalon_streaming start
set_interface_property tx_avalonst associatedClock clk
set_interface_property tx_avalonst associatedReset reset
set_interface_property tx_avalonst dataBitsPerSymbol 8
set_interface_property tx_avalonst errorDescriptor ""
set_interface_property tx_avalonst firstSymbolInHighOrderBits true
set_interface_property tx_avalonst maxChannel 0
set_interface_property tx_avalonst readyLatency 0
set_interface_property tx_avalonst ENABLED true
set_interface_property tx_avalonst EXPORT_OF ""
set_interface_property tx_avalonst PORT_NAME_MAP ""
set_interface_property tx_avalonst CMSIS_SVD_VARIABLES ""
set_interface_property tx_avalonst SVD_ADDRESS_GROUP ""

add_interface_port tx_avalonst tx_data data Output 32
add_interface_port tx_avalonst tx_endofpacket endofpacket Output 1
add_interface_port tx_avalonst tx_startofpacket startofpacket Output 1
add_interface_port tx_avalonst tx_valid valid Output 1
add_interface_port tx_avalonst tx_ready ready Input 1
add_interface_port tx_avalonst tx_empty empty Output 2


# 
# connection point clk
# 
add_interface clk clock end
set_interface_property clk clockRate 0
set_interface_property clk ENABLED true
set_interface_property clk EXPORT_OF ""
set_interface_property clk PORT_NAME_MAP ""
set_interface_property clk CMSIS_SVD_VARIABLES ""
set_interface_property clk SVD_ADDRESS_GROUP ""

add_interface_port clk clock clk Input 1


# 
# connection point reset
# 
add_interface reset reset end
set_interface_property reset associatedClock clk
set_interface_property reset synchronousEdges DEASSERT
set_interface_property reset ENABLED true
set_interface_property reset EXPORT_OF ""
set_interface_property reset PORT_NAME_MAP ""
set_interface_property reset CMSIS_SVD_VARIABLES ""
set_interface_property reset SVD_ADDRESS_GROUP ""

add_interface_port reset clear reset Input 1

# 
# connection point bus_agent
# 
add_interface bus_agent avalon end
set_interface_property bus_agent addressUnits WORDS
set_interface_property bus_agent associatedClock clk
set_interface_property bus_agent associatedReset reset
set_interface_property bus_agent bitsPerSymbol 8
set_interface_property bus_agent burstOnBurstBoundariesOnly false
set_interface_property bus_agent burstcountUnits WORDS
set_interface_property bus_agent explicitAddressSpan 0
set_interface_property bus_agent holdTime 0
set_interface_property bus_agent linewrapBursts false
set_interface_property bus_agent maximumPendingReadTransactions 0
set_interface_property bus_agent maximumPendingWriteTransactions 0
set_interface_property bus_agent readLatency 1
set_interface_property bus_agent readWaitTime 1
set_interface_property bus_agent setupTime 0
set_interface_property bus_agent timingUnits Cycles
set_interface_property bus_agent writeWaitTime 0
set_interface_property bus_agent ENABLED true
set_interface_property bus_agent EXPORT_OF ""
set_interface_property bus_agent PORT_NAME_MAP ""
set_interface_property bus_agent CMSIS_SVD_VARIABLES ""
set_interface_property bus_agent SVD_ADDRESS_GROUP ""

add_interface_port bus_agent bus_address address Input 8
add_interface_port bus_agent bus_read read Input 1
add_interface_port bus_agent bus_readdata readdata Output 32
add_interface_port bus_agent bus_waitrequest waitrequest Output 1
add_interface_port bus_agent bus_write write Input 1
add_interface_port bus_agent bus_writedata writedata Input 32
set_interface_assignment bus_agent embeddedsw.configuration.isFlash 0
set_interface_assignment bus_agent embeddedsw.configuration.isMemoryDevice 0
set_interface_assignment bus_agent embeddedsw.configuration.isNonVolatileStorage 0
set_interface_assignment bus_agent embeddedsw.configuration.isPrintableDevice 0
