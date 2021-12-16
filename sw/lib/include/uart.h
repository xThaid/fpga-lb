#ifndef LIB_UART_H
#define LIB_UART_H

#include <stdarg.h>

void jtag_uart_putc(const char c);
void jtag_uart_print(const char *s);
char jtag_uart_getc(void);
int  jtag_uart_getc_safe(char *data);
int  jtag_uart_readline(char *buf, int bufsize);

#endif