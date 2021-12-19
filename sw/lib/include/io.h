#ifndef LIB_IO_H
#define LIB_IO_H

#include <stdint.h>
#include <inttypes.h>
#include <limits.h>

#define FIELD_MASK(field_start, field_len) \
    ( ((1<<(field_len))-1) << (field_start))

//------------------------------------------------------------------------------
// JTAG UART
//------------------------------------------------------------------------------

typedef struct __attribute__((packed,aligned(4))) {
    uint32_t DATA;
    uint32_t CTRL;
} jtag_uart_t;

#define JTAG_UART (*((volatile jtag_uart_t*) (0x80000000UL)))

#define JTAG_UART_CTRL_RE 0
#define JTAG_UART_CTRL_RE_LEN 1

#define JTAG_UART_CTRL_WE 1
#define JTAG_UART_CTRL_WE_LEN 1

#define JTAG_UART_CTRL_RI 8
#define JTAG_UART_CTRL_RI_LEN 1

#define JTAG_UART_CTRL_WI 9
#define JTAG_UART_CTRL_WI_LEN 1

#define JTAG_UART_CTRL_AC 10
#define JTAG_UART_CTRL_AC_LEN 1

#define JTAG_UART_CTRL_WSPACE 16
#define JTAG_UART_CTRL_WSPACE_LEN 16
#define JTAG_UART_CTRL_WSPACE_MASK (FIELD_MASK(JTAG_UART_CTRL_WSPACE, JTAG_UART_CTRL_WSPACE_LEN))

#define JTAG_UART_DATA_DATA 0
#define JTAG_UART_DATA_DATA_LEN 8
#define JTAG_UART_DATA_DATA_MASK (FIELD_MASK(JTAG_UART_DATA_DATA, JTAG_UART_DATA_DATA_LEN))

#define JTAG_UART_DATA_RVALID 15
#define JTAG_UART_DATA_RVALID_LEN 1
#define JTAG_UART_DATA_RVALID_MASK (FIELD_MASK(JTAG_UART_DATA_RVALID, JTAG_UART_DATA_RVALID_LEN))

#define JTAG_UART_DATA_RAVAIL 16
#define JTAG_UART_DATA_RAVAIL_LEN 16
#define JTAG_UART_DATA_RAVAIL_MASK (FIELD_MASK(JTAG_UART_DATA_RAVAIL, JTAG_UART_DATA_RAVAIL_LEN))

//------------------------------------------------------------------------------
// GPIO
//------------------------------------------------------------------------------

typedef struct __attribute__((packed,aligned(4))) {
    uint32_t LED;
} gpio_t;

#define GPIO (*((volatile gpio_t*) (0x80002000UL)))

#define GPIO_LEDR(led) (1UL << led)
#define GPIO_LEDG(led) ((1UL << led) << 18)

//------------------------------------------------------------------------------
// Triple-Speed Ethernet
//------------------------------------------------------------------------------

typedef volatile struct __attribute__((packed)) {  
    uint32_t control;
    uint32_t status;
    uint32_t phy_id1;
    uint32_t phy_id2;
    uint32_t adv;
    uint32_t remadv;

    uint32_t reg6;
    uint32_t reg7;
    uint32_t reg8;
    uint32_t reg9;
    uint32_t rega;
    uint32_t regb;
    uint32_t regc;
    uint32_t regd;
    uint32_t rege;
    uint32_t regf;
    uint32_t reg10;
    uint32_t reg11;
    uint32_t reg12;
    uint32_t reg13;
    uint32_t reg14;
    uint32_t reg15;
    uint32_t reg16;
    uint32_t reg17;
    uint32_t reg18;
    uint32_t reg19;
    uint32_t reg1a;
    uint32_t reg1b;
    uint32_t reg1c;
    uint32_t reg1d;
    uint32_t reg1e;
    uint32_t reg1f;
} tse_mdio_t;

typedef volatile struct __attribute__((packed,aligned(4))) {
    struct __attribute__((packed)) {
        uint32_t rev;
        uint32_t scratch;
        uint32_t command_config;
        uint32_t mac0;
        uint32_t mac1;
        uint32_t frm_length;
        uint32_t pause_quant;
        uint32_t rx_section_empty;
        uint32_t rx_section_full;
        uint32_t tx_section_empty;
        uint32_t tx_section_full;
        uint32_t rx_almost_empty;
        uint32_t rx_almost_full;
        uint32_t tx_almost_empty;
        uint32_t tx_almost_full;
        uint32_t mdio_addr0;
        uint32_t mdio_addr1;
        uint32_t holdoff_quant;
        uint32_t _reserved[5];
        uint32_t tx_ipg_length;
    } base;

    uint32_t stats[34];
    uint32_t tx_cmd;
    uint32_t rx_cmd;
    uint32_t ext_stats[3];
    uint32_t _reserved;
    uint32_t hash_tb[64];

    tse_mdio_t mdio0;
    tse_mdio_t mdio1;
} tse_t;

#define TSE (*((volatile tse_t*) (0x80003000UL)))

enum TSE_CTRL_enum {
    TSE_CTRL_TX_ENA               = (1UL << 0),
    TSE_CTRL_RX_ENA               = (1UL << 1),
    TSE_CTRL_XON_GEN              = (1UL << 2),
    TSE_CTRL_ETH_SPEED            = (1UL << 3),
    TSE_CTRL_PROMIS_EN            = (1UL << 4),
    TSE_CTRL_PAD_EN               = (1UL << 5),
    TSE_CTRL_CRC_FWD              = (1UL << 6),
    TSE_CTRL_PAUSE_FWD            = (1UL << 7),
    TSE_CTRL_PAUSE_IGNORE         = (1UL << 8),
    TSE_CTRL_TX_ADDR_INS          = (1UL << 9),
    TSE_CTRL_HD_ENA               = (1UL << 10),
    TSE_CTRL_EXCESS_COL           = (1UL << 11),
    TSE_CTRL_LATE_COL             = (1UL << 12),
    TSE_CTRL_SW_RESET             = (1UL << 13),
    TSE_CTRL_MHASH_SEL            = (1UL << 14),
    TSE_CTRL_LOOP_ENA             = (1UL << 15),
    TSE_CTRL_TX_ADDR_SEL0         = (1UL << 16),
    TSE_CTRL_TX_ADDR_SEL1         = (1UL << 17),
    TSE_CTRL_TX_ADDR_SEL2         = (1UL << 18),
    TSE_CTRL_MAGIC_ENA            = (1UL << 19),
    TSE_CTRL_SLEEP                = (1UL << 20),
    TSE_CTRL_WAKEUP               = (1UL << 21),
    TSE_CTRL_XOFF_GEN             = (1UL << 22),
    TSE_CTRL_CNTL_FRM_ENA         = (1UL << 23),
    TSE_CTRL_NO_LGTH_CHECK        = (1UL << 24),
    TSE_CTRL_ENA_10               = (1UL << 25),
    TSE_CTRL_RX_ERR_DISC          = (1UL << 26),
    TSE_CTRL_DISABLE_READ_TIMEOUT = (1UL << 27),
    TSE_CTRL_CNT_RESET            = (1UL << 31)
};

#include "uart.h"
#include "gpio.h"
#include "tse.h"

#endif
