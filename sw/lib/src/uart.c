#include "uart.h"

#include "io.h"

#include "mini-printf.h"

void jtag_uart_putc(const char c) {
    // Stall until there's space in the write FIFO
    while ((JTAG_UART.CTRL & JTAG_UART_CTRL_WSPACE_MASK) == 0);
    JTAG_UART.DATA = c;
}

void jtag_uart_puts(const char *s) {
    char c = 0;
    while ((c = *s++)) {
        jtag_uart_putc(c);
    }
}

static int _jtag_uart_puts(char *s, int len, void *buf) {
    for (int i = 0; i < len; i++)
        jtag_uart_putc(s[i]);
    return len;
}

int jtag_uart_printf(const char *fmt, ...) {
    int ret;
    va_list va;
    va_start(va, fmt);
    ret = mini_vpprintf(_jtag_uart_puts, (void*) 0, fmt, va);
    va_end(va);

    return ret;
}

char jtag_uart_getc(void) {
    uint32_t d = 0;
    while (1) {
        d = JTAG_UART.DATA;
        if (d & JTAG_UART_DATA_RVALID_MASK)
            return d & JTAG_UART_DATA_DATA_MASK;
    }
}

int jtag_uart_getc_safe(char *data) {
    uint32_t d = JTAG_UART.DATA;
    if (!(d & JTAG_UART_DATA_RVALID_MASK))
        return -1;

    *data = d & JTAG_UART_DATA_DATA_MASK;
    return 0;
}

int jtag_uart_readline(char *buf, int bufsize) {
    int n = 0;
    while(n < bufsize) {
        buf[n] = jtag_uart_getc();
        if (buf[n] == '\n') {
            buf[n] = 0;
            return n;
        }
        n++;
    }

    buf[n - 1] = 0;
    return n - 1;
}
