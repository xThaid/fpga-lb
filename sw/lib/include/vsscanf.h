#ifndef _VSNPRINTF_H
#define _VSNPRINTF_H

#include <stddef.h>
#include <stdarg.h>

int vsscanf(char *str, const char *fmt, va_list ap);
int sscanf(char *str, const char *fmt, ...);

#endif

