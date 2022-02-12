#include "tse.h"

#include "io.h"

static void tse_setup(volatile tse_t *tse, uint64_t mac_addr, uint32_t mdio_addr) {
    // Initialize the MAC address
    tse->base.mac0 = mac_addr;
    tse->base.mac1 = mac_addr >> 32;

    // Specify the address of the PHY device to be accessed through MDIO interface
    tse->base.mdio_addr0 = mdio_addr;

    // Write to register 16 of the PHY chip to enable automatic crossover for all modes
    tse->mdio0.reg10 |= 0x0060;

    // Write to register 20 of the PHY chip to set up delay for input/output clk
    tse->mdio0.reg14 |= 0x0082;

    // Software reset the PHY chip and wait
    tse->mdio0.control |= 0x8000;
    while (tse->mdio0.control & 0x8000);

    // Enable read and write transfers, gigabit Ethernet operation
    tse->base.command_config |= TSE_CTRL_TX_ENA | TSE_CTRL_RX_ENA | TSE_CTRL_ETH_SPEED;
}

void tse0_setup(uint64_t mac_addr) {
    tse_setup(&TSE0, mac_addr, 0x10);
}

void tse1_setup(uint64_t mac_addr) {
    tse_setup(&TSE1, mac_addr, 0x11);
}