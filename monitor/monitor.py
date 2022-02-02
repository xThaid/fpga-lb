#! /usr/bin/env python3

import sys
import time
import curses
import intel_jtag_uart

REAL_NAMES = {
    10: "192.168.0.100",
    11: "192.168.100.101"
}

def conv_unit(val):
    if val >= 1000000000.0:
        return val / 1000000000, 'G'
    if val >= 1000000.0:
        return val / 1000000, 'M'
    if val >= 1000.0:
        return val / 1000, 'k'
    return val, ''

def main(stdscr):
    stdscr.clear()

    try:
        ju = intel_jtag_uart.intel_jtag_uart()

    except Exception as e:
        print(e)
        sys.exit(0)

    time.sleep(1)

    last_stats = None
    stats = {}
    last_read = time.time()

    while True:
        bytes_raw = ju.read()
        if len(bytes_raw) == 0:
            time.sleep(0.05)
            continue

        uart_str = bytes_raw.decode('utf-8')
        for entry in uart_str.split('\n'):
            comp = entry.split(' ')
            if len(comp) != 3:
                continue
            stats[int(comp[0])] = (int(comp[1]), int(comp[2]))

        if last_stats is None:
            last_stats = dict(stats)

        now = time.time()
        if now - last_read >= 0.5:
            pos = 0
            for real, real_name in REAL_NAMES.items():
                dt = now - last_read
                pps = (stats[real][0] - last_stats[real][0]) / dt
                bps = (stats[real][1] - last_stats[real][1]) / dt

                pps, pps_unit = conv_unit(pps)
                bps, bps_unit = conv_unit(bps * 8)

                p, p_unit = conv_unit(stats[real][0])
                b, b_unit = conv_unit(stats[real][1])

                line = '{: >15} - {:7.3f} {: >1}pps / {:7.3f} {: >1}p    {:7.3f} {: >1}bps / {:7.3f} {: >1}B'.format(
                    real_name, pps, pps_unit, p, p_unit, bps, bps_unit, b, b_unit)
                stdscr.addstr(pos, 0, line)
                pos += 1

            stdscr.refresh()

            last_read = now
            last_stats = dict(stats)

    ju.close()

curses.wrapper(main)