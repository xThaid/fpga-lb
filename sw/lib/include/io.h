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

#include "uart.h"
#include "gpio.h"

#endif
