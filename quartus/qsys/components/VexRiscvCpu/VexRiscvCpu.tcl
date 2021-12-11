
package require -exact qsys 13.1

#
# module def
#
set_module_property DESCRIPTION ""
set_module_property NAME VexRiscvCpu
set_module_property VERSION 1.0
set_module_property INTERNAL false
set_module_property OPAQUE_ADDRESS_MAP true
set_module_property AUTHOR ""
set_module_property DISPLAY_NAME VexRiscvCpu
set_module_property INSTANTIATE_IN_SYSTEM_MODULE true
set_module_property EDITABLE true
set_module_property REPORT_TO_TALKBACK false
set_module_property ALLOW_GREYBOX_GENERATION false
set_module_property REPORT_HIERARCHY false


#
# connection point iBusAvalon
#
add_interface iBusAvalon avalon start
set_interface_property iBusAvalon addressUnits SYMBOLS
set_interface_property iBusAvalon burstcountUnits WORDS
set_interface_property iBusAvalon burstOnBurstBoundariesOnly false
set_interface_property iBusAvalon constantBurstBehavior false
set_interface_property iBusAvalon holdTime 0
set_interface_property iBusAvalon linewrapBursts false
set_interface_property iBusAvalon maximumPendingReadTransactions 8
set_interface_property iBusAvalon maximumPendingWriteTransactions 0
set_interface_property iBusAvalon readLatency 0
set_interface_property iBusAvalon readWaitTime 0
set_interface_property iBusAvalon setupTime 0
set_interface_property iBusAvalon writeWaitTime 0
set_interface_property iBusAvalon holdTime 0

set_interface_property iBusAvalon associatedClock clk
set_interface_property iBusAvalon associatedReset reset
set_interface_property iBusAvalon bitsPerSymbol 8

set_interface_property iBusAvalon timingUnits Cycles
set_interface_property iBusAvalon ENABLED true
set_interface_property iBusAvalon EXPORT_OF ""
set_interface_property iBusAvalon PORT_NAME_MAP ""
set_interface_property iBusAvalon SVD_ADDRESS_GROUP ""

set_interface_property iBusAvalon doStreamReads false
set_interface_property iBusAvalon doStreamWrites false
add_interface_port iBusAvalon iBusAvalon_address address Output 32
add_interface_port iBusAvalon iBusAvalon_read read Output 1
add_interface_port iBusAvalon iBusAvalon_waitRequestn waitrequest_n Input 1
add_interface_port iBusAvalon iBusAvalon_response response Input 2
add_interface_port iBusAvalon iBusAvalon_readDataValid readdatavalid Input 1
add_interface_port iBusAvalon iBusAvalon_readData readdata Input 32

#
# connection point dBusAvalon
#
add_interface dBusAvalon avalon start
set_interface_property dBusAvalon addressUnits SYMBOLS
set_interface_property dBusAvalon burstcountUnits WORDS
set_interface_property dBusAvalon burstOnBurstBoundariesOnly false
set_interface_property dBusAvalon constantBurstBehavior false
set_interface_property dBusAvalon holdTime 0
set_interface_property dBusAvalon linewrapBursts false
set_interface_property dBusAvalon maximumPendingReadTransactions 1
set_interface_property dBusAvalon maximumPendingWriteTransactions 0
set_interface_property dBusAvalon readLatency 0
set_interface_property dBusAvalon readWaitTime 0
set_interface_property dBusAvalon setupTime 0
set_interface_property dBusAvalon writeWaitTime 0
set_interface_property dBusAvalon holdTime 0

set_interface_property dBusAvalon associatedClock clk
set_interface_property dBusAvalon associatedReset reset
set_interface_property dBusAvalon bitsPerSymbol 8

set_interface_property dBusAvalon timingUnits Cycles
set_interface_property dBusAvalon ENABLED true
set_interface_property dBusAvalon EXPORT_OF ""
set_interface_property dBusAvalon PORT_NAME_MAP ""
set_interface_property dBusAvalon SVD_ADDRESS_GROUP ""

set_interface_property dBusAvalon doStreamReads false
set_interface_property dBusAvalon doStreamWrites false
add_interface_port dBusAvalon dBusAvalon_address address Output 32
add_interface_port dBusAvalon dBusAvalon_read read Output 1
add_interface_port dBusAvalon dBusAvalon_write write Output 1
add_interface_port dBusAvalon dBusAvalon_waitRequestn waitrequest_n Input 1
add_interface_port dBusAvalon dBusAvalon_byteEnable byteenable Output 4
add_interface_port dBusAvalon dBusAvalon_writeData writedata Output 32
add_interface_port dBusAvalon dBusAvalon_response response Input 2
add_interface_port dBusAvalon dBusAvalon_readDataValid readdatavalid Input 1
add_interface_port dBusAvalon dBusAvalon_readData readdata Input 32

#
# connection point io_timerInterrupt
#
add_interface io_timerInterrupt conduit end
set_interface_property io_timerInterrupt associatedClock ""
set_interface_property io_timerInterrupt associatedReset ""
set_interface_property io_timerInterrupt ENABLED true
set_interface_property io_timerInterrupt EXPORT_OF ""
set_interface_property io_timerInterrupt PORT_NAME_MAP ""
set_interface_property io_timerInterrupt SVD_ADDRESS_GROUP ""
add_interface_port io_timerInterrupt io_timerInterrupt export Input 1



#
# connection point io_externalInterrupt
#
add_interface io_externalInterrupt conduit end
set_interface_property io_externalInterrupt associatedClock ""
set_interface_property io_externalInterrupt associatedReset ""
set_interface_property io_externalInterrupt ENABLED true
set_interface_property io_externalInterrupt EXPORT_OF ""
set_interface_property io_externalInterrupt PORT_NAME_MAP ""
set_interface_property io_externalInterrupt SVD_ADDRESS_GROUP ""
add_interface_port io_externalInterrupt io_externalInterrupt export Input 1



#
# connection point io_jtag
#
add_interface io_jtag conduit end
set_interface_property io_jtag associatedClock ""
set_interface_property io_jtag associatedReset ""
set_interface_property io_jtag ENABLED true
set_interface_property io_jtag EXPORT_OF ""
set_interface_property io_jtag PORT_NAME_MAP ""
set_interface_property io_jtag SVD_ADDRESS_GROUP ""
add_interface_port io_jtag io_jtag_tms export Input 1
add_interface_port io_jtag io_jtag_tdi export Input 1
add_interface_port io_jtag io_jtag_tdo export Output 1
add_interface_port io_jtag io_jtag_tck export Input 1



#
# connection point clk
#
add_interface clk clock end
set_interface_property clk clockRate 0
set_interface_property clk ENABLED true
set_interface_property clk EXPORT_OF ""
set_interface_property clk PORT_NAME_MAP ""
set_interface_property clk SVD_ADDRESS_GROUP ""

add_interface_port clk clk clk Input 1
             
#
# connection point reset
#
add_interface reset reset end
set_interface_property reset associatedClock clk
set_interface_property reset synchronousEdges DEASSERT
set_interface_property reset ENABLED true
set_interface_property reset EXPORT_OF ""
set_interface_property reset PORT_NAME_MAP ""
set_interface_property reset SVD_ADDRESS_GROUP ""

add_interface_port reset reset reset Input 1
             