#include "tse.h"

#include "io.h"

volatile int phyaddr;

void tse_setup(void) {
    // Initialize the MAC address
    TSE.base.mac0 = 0x116E6001;
    TSE.base.mac1 = 0x00000F02;

    // Specify the address of the PHY device to be accessed through MDIO interface
    TSE.base.mdio_addr0 = 0x10;

    // Write to register 16 of the PHY chip to enable automatic crossover for all modes
    TSE.mdio0.reg10 |= 0x0060;

    // Write to register 20 of the PHY chip to set up delay for input/output clk
    TSE.mdio0.reg14 |= 0x0082;

    // Software reset the PHY chip and wait
    TSE.mdio0.control |= 0x8000;
    while (TSE.mdio0.control & 0x8000);

    // Enable read and write transfers, gigabit Ethernet operation
    TSE.base.command_config |= TSE_CTRL_TX_ENA | TSE_CTRL_RX_ENA | TSE_CTRL_ETH_SPEED;
}
