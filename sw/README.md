# Software
This is the software running on the RISC-V soft-core. Its task is to configure the MAC and the data plane, but also to enable communication and monitoring through UART.

The system uses FreeRTOS kernel, which had to be slightly modified to work on this specific hardware.

With the toolchain in place, simply run `make` to build the code. It will produce the binary, the elf file, assembly and all that interesting stuff.